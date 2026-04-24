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
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Common,
  DPM.Core.Options.Spec,
  DPM.Console.Prompts,
  DPM.Console.Command.Spec.Discovery,
  DPM.Console.Command.Spec.Writer;

type
  TClassifiedDProj = record
    FilePath : string;
    LeafName : string;
    Kind : TDProjKind;
  end;
  TClassifiedDProjs = TArray<TClassifiedDProj>;

function JoinSrc(const folder, glob : string) : string;
begin
  if folder = '' then
    result := glob
  else
    result := folder + '/' + glob;
end;

function ChoosePrimaryFolder(const folders : TCompilerFolders) : integer;
var
  i : integer;
begin
  //highest-numbered compiler wins
  result := 0;
  for i := 1 to High(folders) do
    if Ord(folders[i].Compiler) > Ord(folders[result].Compiler) then
      result := i;
end;

function ClassifyPrimaryDProjs(const primary : TCompilerFolder) : TClassifiedDProjs;
var
  i : integer;
  entry : TClassifiedDProj;
  classifiedList : TClassifiedDProjs;
begin
  SetLength(classifiedList, Length(primary.DProjFiles));
  for i := 0 to High(primary.DProjFiles) do
  begin
    entry.FilePath := primary.DProjFiles[i];
    entry.LeafName := ExtractFileName(primary.DProjFiles[i]);
    entry.Kind := ClassifyDProj(primary.DProjFiles[i]);
    classifiedList[i] := entry;
  end;
  result := classifiedList;
end;

function PromptDProjKind(const leafName : string; out cancelled : boolean) : TDProjKind;
var
  idx : integer;
  choices : TPromptChoices;
begin
  SetLength(choices, 3);
  choices[0] := 'Runtime (build)';
  choices[1] := 'Design-time (design)';
  choices[2] := 'Skip';
  idx := PromptChoice('How should ' + leafName + ' be treated?', choices, 0, cancelled);
  if cancelled then
  begin
    result := dkUnknown;
    exit;
  end;
  case idx of
    0 : result := dkRuntime;
    1 : result := dkDesign;
  else
    result := dkUnknown;
  end;
end;

function ResolveClassifications(const classified : TClassifiedDProjs; const nonInteractive : boolean;
  out cancelled : boolean) : TClassifiedDProjs;
var
  i : integer;
  current : TClassifiedDProj;
  resolvedList : TClassifiedDProjs;
begin
  cancelled := false;
  SetLength(resolvedList, Length(classified));
  for i := 0 to High(classified) do
  begin
    current := classified[i];
    if current.Kind = dkUnknown then
    begin
      if nonInteractive then
        current.Kind := dkRuntime
      else
      begin
        current.Kind := PromptDProjKind(current.LeafName, cancelled);
        if cancelled then
        begin
          result := resolvedList;
          exit;
        end;
      end;
    end;
    resolvedList[i] := current;
  end;
  result := resolvedList;
end;

procedure CollectBuildAndDesignNames(const classified : TClassifiedDProjs;
  out buildDProjs : TArray<string>; out designDProjs : TArray<string>);
var
  i : integer;
begin
  SetLength(buildDProjs, 0);
  SetLength(designDProjs, 0);
  for i := 0 to High(classified) do
  begin
    case classified[i].Kind of
      dkRuntime :
        begin
          SetLength(buildDProjs, Length(buildDProjs) + 1);
          buildDProjs[High(buildDProjs)] := classified[i].LeafName;
        end;
      dkDesign :
        begin
          SetLength(designDProjs, Length(designDProjs) + 1);
          designDProjs[High(designDProjs)] := classified[i].LeafName;
        end;
    end;
  end;
end;

function BuildSpecTargets(const folders : TCompilerFolders; const config : IConfiguration;
  const logger : ILogger) : TArray<TSpecTargetInfo>;
var
  i : integer;
  info : TSpecTargetInfo;
  list : TArray<TSpecTargetInfo>;
begin
  SetLength(list, Length(folders));
  for i := 0 to High(folders) do
  begin
    info.Compiler := folders[i].Compiler;
    info.Platforms := CollectPlatforms(folders[i].DProjFiles, logger, config);
    info.PackageSourceLiteral := folders[i].FolderName;
    info.PackageSourceTemplate := DerivePackageSourceTemplate(folders[i].Compiler, folders[i].FolderName);
    list[i] := info;
  end;
  result := list;
end;

procedure SortTargetsByCompiler(var targets : TArray<TSpecTargetInfo>);
var
  i, j : integer;
  tmp : TSpecTargetInfo;
begin
  //insertion sort - list is always short
  for i := 1 to High(targets) do
  begin
    j := i;
    while (j > 0) and (Ord(targets[j].Compiler) < Ord(targets[j - 1].Compiler)) do
    begin
      tmp := targets[j];
      targets[j] := targets[j - 1];
      targets[j - 1] := tmp;
      Dec(j);
    end;
  end;
end;

{ TSpecCommand }

function TSpecCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TSpecOptions;
  nonInteractive : boolean;
  cancelled : boolean;
  rootDir : string;
  sourceFolder : string;
  packagesFolder : string;
  hasPackages : boolean;
  havePackagePrompt : boolean;
  compilerFolders : TCompilerFolders;
  gitUrl : string;
  projectUrl : string;
  defaultId : string;
  packageId : string;
  description : string;
  author : string;
  version : string;
  license : string;
  primaryIdx : integer;
  classified : TClassifiedDProjs;
  resolved : TClassifiedDProjs;
  buildDProjs : TArray<string>;
  designDProjs : TArray<string>;
  scaffold : TSpecScaffold;
  yamlText : string;
  outputPath : string;
  existingSpec : TArray<string>;
  overwrite : boolean;
  config : IConfiguration;
  configFile : string;
  targets : TArray<TSpecTargetInfo>;
  sourceRel : string;
  packagesRel : string;
  sourceGlobs : TArray<string>;
  licenseLeaf : string;
  unionPlatforms : TDPMPlatforms;
  designPlatforms : TDPMPlatforms;
  targetIdx : integer;
begin
  TSpecOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TSpecOptions.Default;
  nonInteractive := options.NonInteractive;

  rootDir := ExcludeTrailingPathDelimiter(GetCurrentDir);

  //1. overwrite guard
  existingSpec := TDirectory.GetFiles(rootDir, '*' + cPackageSpecExt);
  if (Length(existingSpec) > 0) and not options.Overwrite then
  begin
    if nonInteractive then
    begin
      Logger.Error('A ' + cPackageSpecExt + ' already exists in this folder - pass --overwrite to replace it.');
      result := TExitCode.Error;
      exit;
    end;
    overwrite := PromptYesNo('A ' + cPackageSpecExt + ' already exists. Overwrite?', false, cancelled);
    if cancelled or not overwrite then
    begin
      Logger.Information('Cancelled.');
      result := TExitCode.OK;
      exit;
    end;
  end;

  //2. source folder
  if not FindSourceFolder(rootDir, sourceFolder) then
  begin
    if nonInteractive then
    begin
      Logger.Error('Could not find a Source folder and no .pas files in current dir.');
      result := TExitCode.MissingArg;
      exit;
    end;
    sourceFolder := PromptLineRequired('Source folder (absolute or relative)', cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
    if not TPath.IsPathRooted(sourceFolder) then
      sourceFolder := TPath.Combine(rootDir, sourceFolder);
  end;
  sourceRel := StringReplace(MakeRelative(rootDir, sourceFolder), './', '', []);

  //3. packages folder
  hasPackages := FindPackagesFolder(rootDir, packagesFolder);
  if not hasPackages and not nonInteractive then
  begin
    havePackagePrompt := PromptYesNo('No Packages folder found. Does this project have package projects?', false, cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
    if havePackagePrompt then
    begin
      packagesFolder := PromptLineRequired('Packages folder (absolute or relative)', cancelled);
      if cancelled then
      begin
        result := TExitCode.OK;
        exit;
      end;
      if not TPath.IsPathRooted(packagesFolder) then
        packagesFolder := TPath.Combine(rootDir, packagesFolder);
      hasPackages := TDirectory.Exists(packagesFolder);
      if not hasPackages then
        Logger.Warning('Packages folder does not exist, skipping build/design entries.');
    end;
  end;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  configFile := options.ConfigFile;
  if configFile = '' then
    configFile := config.FileName;

  //4. scan packages for compiler folders
  if hasPackages then
  begin
    compilerFolders := ScanPackagesFolder(packagesFolder, Logger, config);
    if Length(compilerFolders) = 0 then
    begin
      Logger.Warning('No recognisable compiler folders found under ' + packagesFolder + ' - skipping build/design.');
      hasPackages := false;
    end;
  end;

  if cancellationToken.IsCancelled then
  begin
    result := TExitCode.OK;
    exit;
  end;

  //5. derive defaults from git
  gitUrl := '';
  projectUrl := '';
  defaultId := '';
  if DetectGitRemoteUrl(rootDir, gitUrl) then
  begin
    projectUrl := NormaliseGitUrl(gitUrl);
    defaultId := DerivePackageIdFromUrl(projectUrl);
  end;

  //6. package id
  if options.PackageId <> '' then
    packageId := options.PackageId
  else
  begin
    if nonInteractive then
    begin
      if defaultId = '' then
      begin
        Logger.Error('No --packageId provided and no git remote from which to derive one.');
        result := TExitCode.MissingArg;
        exit;
      end;
      packageId := defaultId;
    end
    else
    begin
      if defaultId <> '' then
        packageId := PromptLine('Package id', defaultId, cancelled)
      else
        packageId := PromptLineRequired('Package id', cancelled);
      if cancelled then
      begin
        result := TExitCode.OK;
        exit;
      end;
      if Trim(packageId) = '' then
        packageId := defaultId;
    end;
  end;

  //7. description
  if nonInteractive then
    description := ''
  else
  begin
    description := PromptLineRequired('Description', cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
  end;

  //8. author
  author := config.Author;
  if author = '' then
  begin
    if nonInteractive then
      Logger.Warning('No author set in ' + configFile + ' - metadata will have an empty author.')
    else
    begin
      author := PromptLineRequired('Author name', cancelled);
      if cancelled then
      begin
        result := TExitCode.OK;
        exit;
      end;
      config.Author := author;
      if not FConfigurationManager.SaveConfig(config, configFile) then
        Logger.Warning('Could not save author to ' + configFile);
    end;
  end;

  //9. version + license
  if nonInteractive then
  begin
    version := '0.1.0';
    license := 'Apache-2.0';
  end
  else
  begin
    version := PromptLine('Version', '0.1.0', cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
    license := PromptLine('License (SPDX id)', 'Apache-2.0', cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
  end;

  //10. classify dprojs from the primary compiler folder
  SetLength(buildDProjs, 0);
  SetLength(designDProjs, 0);
  SetLength(targets, 0);
  packagesRel := '';
  if hasPackages then
  begin
    primaryIdx := ChoosePrimaryFolder(compilerFolders);
    classified := ClassifyPrimaryDProjs(compilerFolders[primaryIdx]);
    resolved := ResolveClassifications(classified, nonInteractive, cancelled);
    if cancelled then
    begin
      result := TExitCode.OK;
      exit;
    end;
    CollectBuildAndDesignNames(resolved, buildDProjs, designDProjs);
    targets := BuildSpecTargets(compilerFolders, config, Logger);
    SortTargetsByCompiler(targets);
    packagesRel := StringReplace(MakeRelative(rootDir, packagesFolder), './', '', []);
  end;

  //11. assemble scaffold
  scaffold.PackageId := packageId;
  scaffold.Version := version;
  scaffold.Description := description;
  scaffold.Author := author;
  if author <> '' then
    scaffold.Copyright := author + ' and contributors'
  else
    scaffold.Copyright := '';
  scaffold.License := license;
  scaffold.ProjectUrl := projectUrl;
  scaffold.RepositoryUrl := projectUrl;
  SetLength(scaffold.Tags, 0);

  //License file (if any) goes first so it shows up at the top of the source section.
  SetLength(sourceGlobs, 0);
  if FindLicenseFile(rootDir, licenseLeaf) then
  begin
    SetLength(sourceGlobs, 1);
    sourceGlobs[0] := licenseLeaf;
  end;
  SetLength(sourceGlobs, Length(sourceGlobs) + 2);
  sourceGlobs[High(sourceGlobs) - 1] := JoinSrc(sourceRel, '*.pas');
  sourceGlobs[High(sourceGlobs)] := JoinSrc(sourceRel, '*.inc');
  scaffold.SourceGlobs := sourceGlobs;

  scaffold.HasPackagesFolder := hasPackages;
  scaffold.PackagesFolderRel := packagesRel;
  scaffold.Targets := targets;
  scaffold.BuildDProjs := buildDProjs;
  scaffold.DesignDProjs := designDProjs;

  //Platforms for build = union of platforms across all target compilers.
  //Design platforms are IDE-specific - Delphi <12 only ship a Win32 IDE; 12+
  //also ship Win64. Use the existing DesignTimePlatforms helper to honour that
  //and union the result across every target compiler.
  unionPlatforms := [];
  designPlatforms := [];
  for targetIdx := 0 to High(targets) do
  begin
    unionPlatforms := unionPlatforms + targets[targetIdx].Platforms;
    designPlatforms := designPlatforms + DesignTimePlatforms(targets[targetIdx].Compiler);
  end;
  scaffold.BuildPlatforms := unionPlatforms;
  if (Length(designDProjs) > 0) and (designPlatforms = []) then
    designPlatforms := [TDPMPlatform.Win32];
  scaffold.DesignPlatforms := designPlatforms;

  //12. write output
  yamlText := BuildSpecYaml(scaffold);
  outputPath := TPath.Combine(rootDir, packageId + cPackageSpecExt);
  try
    TFile.WriteAllText(outputPath, yamlText, TEncoding.UTF8);
  except
    on e : Exception do
    begin
      Logger.Error('Failed to write ' + outputPath + ' : ' + e.Message);
      result := TExitCode.Error;
      exit;
    end;
  end;

  Logger.Information('Wrote ' + outputPath);
  Logger.Information('Review the generated file and fill in any remaining values.');

  result := TExitCode.OK;
end;

end.
