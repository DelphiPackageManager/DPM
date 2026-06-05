{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Console.Command.Spec;

interface

uses
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base;

type
  TSpecCommand = class(TBaseCommand)
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Common,
  DPM.Core.Options.Spec,
  DPM.Console.Prompts,
  DPM.Console.Command.Spec.Discovery,
  DPM.Console.Command.Spec.Writer,
  DPM.Console.Command.Spec.Scaffold;

function PromptMultiPackageMode(const packages : TLogicalPackages; out selectedIdx : integer;
  out cancelled : boolean) : integer;
//returns 0 for "scaffold all", 1 for "scaffold one" (selectedIdx set), -1 for "cancel"
var
  choices : TPromptChoices;
  topChoice : integer;
  i : integer;
  pkgChoices : TPromptChoices;
begin
  result := -1;
  selectedIdx := -1;
  SetLength(choices, 3);
  choices[0] := 'Scaffold one .dspec.yaml per package (' + IntToStr(Length(packages)) + ' files)';
  choices[1] := 'Scaffold a single .dspec.yaml (I''ll pick which package)';
  choices[2] := 'Cancel';
  topChoice := PromptChoice('Multiple package projects detected.', choices, 0, cancelled);
  if cancelled then
    exit;
  case topChoice of
    0 : result := 0;
    1 :
      begin
        SetLength(pkgChoices, Length(packages));
        for i := 0 to High(packages) do
          pkgChoices[i] := packages[i].Stem;
        selectedIdx := PromptChoice('Which package?', pkgChoices, 0, cancelled);
        if cancelled then
          exit;
        result := 1;
      end;
  else
    result := -1;
  end;
end;

{ TSpecCommand }

function TSpecCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TSpecOptions;
  cancelled : boolean;
  rootDir : string;
  havePackagePrompt : boolean;
  gitUrl : string;
  projectUrl : string;
  defaultId : string;
  packageId : string;
  description : string;
  author : string;
  version : string;
  license : string;
  existingSpec : TArray<string>;
  overwrite : boolean;
  configFile : string;
  ctx : TScaffoldContext;
  logicalPackages : TLogicalPackages;
  primaryIdx : integer;
  mode : integer;
  selectedIdx : integer;
  scaffold : TSpecScaffold;
  i : integer;
  writtenPath : string;
  effectiveId : string;
begin
  TSpecOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TSpecOptions.Default;
  ctx.NonInteractive := options.NonInteractive;

  rootDir := ExcludeTrailingPathDelimiter(GetCurrentDir);
  ctx.RootDir := rootDir;

  //1. overwrite guard
  existingSpec := TDirectory.GetFiles(rootDir, '*' + cPackageSpecExt);
  if (Length(existingSpec) > 0) and not options.Overwrite then
  begin
    if ctx.NonInteractive then
    begin
      Logger.Error('A ' + cPackageSpecExt + ' already exists in this folder - pass --overwrite to replace it.');
      exit(TExitCode.Error);
    end;
    overwrite := PromptYesNo('A ' + cPackageSpecExt + ' already exists. Overwrite?', false, cancelled);
    if cancelled or not overwrite then
    begin
      Logger.Information('Cancelled.');
      exit(TExitCode.OK);
    end;
  end;

  //2. source folder
  if not FindSourceFolder(rootDir, ctx.SourceFolder) then
  begin
    if ctx.NonInteractive then
    begin
      Logger.Error('Could not find a Source folder and no .pas files in current dir.');
      exit(TExitCode.MissingArg);
    end;
    ctx.SourceFolder := PromptLineRequired('Source folder (absolute or relative)', cancelled);
    if cancelled then exit(TExitCode.OK);
    if not TPath.IsPathRooted(ctx.SourceFolder) then
      ctx.SourceFolder := TPath.Combine(rootDir, ctx.SourceFolder);
  end;
  ctx.SourceRel := StringReplace(MakeRelative(rootDir, ctx.SourceFolder), './', '', []);

  //3. packages folder
  ctx.HasPackages := FindPackagesFolder(rootDir, ctx.PackagesFolder);
  if not ctx.HasPackages and not ctx.NonInteractive then
  begin
    havePackagePrompt := PromptYesNo('No Packages folder found. Does this project have package projects?', false, cancelled);
    if cancelled then exit(TExitCode.OK);
    if havePackagePrompt then
    begin
      ctx.PackagesFolder := PromptLineRequired('Packages folder (absolute or relative)', cancelled);
      if cancelled then exit(TExitCode.OK);
      if not TPath.IsPathRooted(ctx.PackagesFolder) then
        ctx.PackagesFolder := TPath.Combine(rootDir, ctx.PackagesFolder);
      ctx.HasPackages := TDirectory.Exists(ctx.PackagesFolder);
      if not ctx.HasPackages then
        Logger.Warning('Packages folder does not exist, skipping build/design entries.');
    end;
  end;
  if ctx.HasPackages then
    ctx.PackagesRel := StringReplace(MakeRelative(rootDir, ctx.PackagesFolder), './', '', [])
  else
    ctx.PackagesRel := '';

  //4. load config (needed before scanning - TProjectEditor requires it)
  ctx.Config := FConfigurationManager.LoadConfig(options.ConfigFile);
  configFile := options.ConfigFile;
  if configFile = '' then
    configFile := ctx.Config.FileName;

  //5. scan packages for compiler folders
  if ctx.HasPackages then
  begin
    ctx.CompilerFolders := ScanPackagesFolder(ctx.PackagesFolder, Logger, ctx.Config);
    if Length(ctx.CompilerFolders) = 0 then
    begin
      Logger.Warning('No recognisable compiler folders found under ' + ctx.PackagesFolder + ' - skipping build/design.');
      ctx.HasPackages := false;
    end;
  end;

  if cancellationToken.IsCancelled then
    exit(TExitCode.OK);

  //6. derive defaults from git
  gitUrl := '';
  projectUrl := '';
  defaultId := '';
  if DetectGitRemoteUrl(rootDir, gitUrl) then
  begin
    projectUrl := NormaliseGitUrl(gitUrl);
    defaultId := DerivePackageIdFromUrl(projectUrl);
  end;
  ctx.ProjectUrl := projectUrl;

  //7. detect logical packages from the primary compiler folder (if any)
  SetLength(logicalPackages, 0);
  if ctx.HasPackages then
  begin
    primaryIdx := ChoosePrimaryFolder(ctx.CompilerFolders);
    logicalPackages := GroupDProjsByStem(ctx.CompilerFolders[primaryIdx].DProjFiles);
  end;
  ctx.LogicalPackages := logicalPackages;
  //Default sibling ids to the stem. Scaffolding paths below may override when
  //the user passes --packageId or accepts a different default in a prompt.
  SetLength(ctx.PackageIds, Length(logicalPackages));
  for i := 0 to High(logicalPackages) do
    ctx.PackageIds[i] := logicalPackages[i].Stem;

  //8. shared metadata prompts (once, reused across every package)
  //   In multi-package mode PackageId is not prompted here - each logical package
  //   uses its stem as the id, prompted only when scaffolding a single package.
  if options.PackageId <> '' then
    packageId := options.PackageId
  else
    packageId := defaultId;

  if ctx.NonInteractive then
    description := ''
  else
  begin
    description := PromptLineRequired('Description', cancelled);
    if cancelled then exit(TExitCode.OK);
  end;
  ctx.Description := description;

  author := ctx.Config.Author;
  if author = '' then
  begin
    if ctx.NonInteractive then
      Logger.Warning('No author set in ' + configFile + ' - metadata will have an empty author.')
    else
    begin
      author := PromptLineRequired('Author name', cancelled);
      if cancelled then exit(TExitCode.OK);
      ctx.Config.Author := author;
      if not FConfigurationManager.SaveConfig(ctx.Config, configFile) then
        Logger.Warning('Could not save author to ' + configFile);
    end;
  end;
  ctx.Author := author;
  if author <> '' then
    ctx.Copyright := author + ' and contributors'
  else
    ctx.Copyright := '';

  if ctx.NonInteractive then
  begin
    version := '0.1.0';
    license := 'Apache-2.0';
  end
  else
  begin
    version := PromptLine('Version', '0.1.0', cancelled);
    if cancelled then exit(TExitCode.OK);
    license := PromptLine('License (SPDX id)', 'Apache-2.0', cancelled);
    if cancelled then exit(TExitCode.OK);
  end;
  ctx.Version := version;
  ctx.License := license;

  //9. scaffolding mode - single vs multi-package
  if Length(logicalPackages) <= 1 then
  begin
    //Single-package (or no-package) path - prompt for the package id as before.
    if options.PackageId = '' then
    begin
      if ctx.NonInteractive then
      begin
        if defaultId = '' then
        begin
          Logger.Error('No --packageId provided and no git remote from which to derive one.');
          exit(TExitCode.MissingArg);
        end;
        packageId := defaultId;
      end
      else
      begin
        if defaultId <> '' then
          packageId := PromptLine('Package id', defaultId, cancelled)
        else
          packageId := PromptLineRequired('Package id', cancelled);
        if cancelled then exit(TExitCode.OK);
        if Trim(packageId) = '' then
          packageId := defaultId;
      end;
    end;

    if Length(logicalPackages) = 1 then
    begin
      //Record the chosen id so BuildDependencies can detect self-references.
      ctx.PackageIds[0] := packageId;
      scaffold := BuildPackageScaffold(logicalPackages[0], ctx, packageId, Logger);
    end
    else
    begin
      //no packages - emit a scaffold with an empty dproj set and fallback source globs.
      scaffold := BuildPackageScaffold(Default(TLogicalPackage), ctx, packageId, Logger);
    end;

    if not WriteScaffoldFile(scaffold, rootDir, Logger, writtenPath) then
      exit(TExitCode.Error);
    Logger.Information('Review the generated file and fill in any remaining values.');
    exit(TExitCode.OK);
  end;

  //10. multi-package mode
  if ctx.NonInteractive then
  begin
    //default to writing every package when scripted
    mode := 0;
    selectedIdx := -1;
  end
  else
  begin
    mode := PromptMultiPackageMode(logicalPackages, selectedIdx, cancelled);
    if cancelled or (mode < 0) then
    begin
      Logger.Information('Cancelled.');
      exit(TExitCode.OK);
    end;
  end;

  if mode = 1 then
  begin
    //single selected package - still use its stem as the id default
    if options.PackageId <> '' then
      effectiveId := options.PackageId
    else
      effectiveId := logicalPackages[selectedIdx].Stem;
    ctx.PackageIds[selectedIdx] := effectiveId;
    scaffold := BuildPackageScaffold(logicalPackages[selectedIdx], ctx, effectiveId, Logger);
    if not WriteScaffoldFile(scaffold, rootDir, Logger, writtenPath) then
      exit(TExitCode.Error);
    Logger.Information('Review the generated file and fill in any remaining values.');
    exit(TExitCode.OK);
  end;

  //mode = 0: scaffold all
  for i := 0 to High(logicalPackages) do
  begin
    effectiveId := logicalPackages[i].Stem;
    scaffold := BuildPackageScaffold(logicalPackages[i], ctx, effectiveId, Logger);
    if not WriteScaffoldFile(scaffold, rootDir, Logger, writtenPath) then
      exit(TExitCode.Error);
    if cancellationToken.IsCancelled then
      exit(TExitCode.OK);
  end;
  Logger.Information('Wrote ' + IntToStr(Length(logicalPackages)) + ' spec file(s). Review and fill in remaining fields.');
  result := TExitCode.OK;
end;

end.
