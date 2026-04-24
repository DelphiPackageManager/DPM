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
  DPM.Console.Command.Spec.Writer;

//Shared inputs the command collects once and reuses for every package we
//scaffold. Split out so the single-package path and the multi-package loop
//both consume the same data.
type
  TScaffoldContext = record
    RootDir : string;
    SourceFolder : string;      //absolute
    SourceRel : string;         //forward-slash, no leading './'
    PackagesFolder : string;    //absolute, '' when no packages
    PackagesRel : string;       //forward-slash, no leading './'
    HasPackages : boolean;
    CompilerFolders : TCompilerFolders;
    ProjectUrl : string;
    Description : string;
    Author : string;
    Copyright : string;
    License : string;
    Version : string;
    NonInteractive : boolean;
    Config : IConfiguration;
    //Logical packages discovered in the primary compiler folder. Used by
    //BuildPackageScaffold to classify `requires` entries as internal (a sibling
    //package we're also scaffolding) vs external (unknown external DPM dep).
    LogicalPackages : TLogicalPackages;
    //Parallel to LogicalPackages; the id each sibling will be scaffolded under,
    //so internal deps can reference the correct id (which may differ from the
    //stem if the user overrode via --packageId in single-package mode).
    PackageIds : TArray<string>;
  end;

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

procedure SortTargetsByCompiler(var targets : TArray<TSpecTargetInfo>);
var
  i, j : integer;
  tmp : TSpecTargetInfo;
begin
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

function FindDProjInFolder(const folder : TCompilerFolder; const leafName : string) : string;
var
  i : integer;
begin
  result := '';
  for i := 0 to High(folder.DProjFiles) do
    if SameText(ExtractFileName(folder.DProjFiles[i]), leafName) then
      exit(folder.DProjFiles[i]);
end;

function CollectPackagePlatforms(const compilerFolders : TCompilerFolders; const dprojLeaf : string;
  const logger : ILogger; const config : IConfiguration) : TArray<TSpecTargetInfo>;
var
  i : integer;
  dprojPath : string;
  info : TSpecTargetInfo;
  list : TArray<TSpecTargetInfo>;
  single : TArray<string>;
begin
  SetLength(list, 0);
  SetLength(single, 1);
  for i := 0 to High(compilerFolders) do
  begin
    dprojPath := FindDProjInFolder(compilerFolders[i], dprojLeaf);
    if dprojPath = '' then
      continue;
    single[0] := dprojPath;
    info.Compiler := compilerFolders[i].Compiler;
    info.Platforms := CollectPlatforms(single, logger, config);
    info.PackageSourceLiteral := compilerFolders[i].FolderName;
    info.PackageSourceTemplate := DerivePackageSourceTemplate(compilerFolders[i].Compiler, compilerFolders[i].FolderName);
    SetLength(list, Length(list) + 1);
    list[High(list)] := info;
  end;
  result := list;
end;

function BuildFallbackTargets(const folders : TCompilerFolders; const config : IConfiguration;
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

function DeriveSourceGlobsForPackage(const runtimeDProj : string; const ctx : TScaffoldContext) : TArray<string>;
var
  dpkPath : string;
  dpkUnits : TDpkUnits;
  unitPaths : TArray<string>;
  resolved : string;
  i : integer;
  containerDir : string;
  commonParent : string;
  rawExtracted : TArray<string>;
  globs : TArray<string>;
begin
  SetLength(globs, 0);
  SetLength(unitPaths, 0);
  if runtimeDProj = '' then
    exit(globs);
  containerDir := ExtractFilePath(runtimeDProj);

  //Prefer the .dpk (semantic). Fall back to the .dproj's DCCReference list.
  dpkPath := ChangeFileExt(runtimeDProj, '.dpk');
  if TFile.Exists(dpkPath) then
  begin
    dpkUnits := ParseDpkContains(dpkPath);
    for i := 0 to High(dpkUnits) do
    begin
      resolved := ResolveDpkUnitPath(containerDir, dpkUnits[i].RelPath, ctx.RootDir);
      if resolved <> '' then
      begin
        SetLength(unitPaths, Length(unitPaths) + 1);
        unitPaths[High(unitPaths)] := resolved;
      end;
    end;
  end;

  if Length(unitPaths) = 0 then
  begin
    rawExtracted := ExtractDprojUnits(runtimeDProj);
    for i := 0 to High(rawExtracted) do
    begin
      resolved := ResolveDpkUnitPath(containerDir, rawExtracted[i], ctx.RootDir);
      if resolved <> '' then
      begin
        SetLength(unitPaths, Length(unitPaths) + 1);
        unitPaths[High(unitPaths)] := resolved;
      end;
    end;
  end;

  if Length(unitPaths) = 0 then
    exit(globs);

  globs := DeriveSourceGlobs(unitPaths, ctx.RootDir);

  //Add a recursive .inc glob co-located with the derived .pas glob so packages
  //that ship private includes alongside their source get picked up.
  commonParent := CommonParentFolder(unitPaths);
  if commonParent <> '' then
  begin
    if TDirectory.Exists(TPath.Combine(ctx.RootDir,
       StringReplace(commonParent, '/', PathDelim, [rfReplaceAll]))) then
    begin
      SetLength(globs, Length(globs) + 1);
      globs[High(globs)] := commonParent + '/**.inc';
    end;
  end;

  result := globs;
end;

function PrependSharedIncludes(const globs : TArray<string>; const ctx : TScaffoldContext) : TArray<string>;
var
  sharedIncs : TArray<string>;
  i : integer;
  result_ : TArray<string>;
begin
  if ctx.SourceFolder = '' then
    exit(globs);
  try
    sharedIncs := TDirectory.GetFiles(ctx.SourceFolder, '*.inc');
  except
    sharedIncs := nil;
  end;
  if Length(sharedIncs) = 0 then
    exit(globs);
  //prepend '<sourceRel>/*.inc' so the shared Spring.inc / jedi.inc-style files
  //land in the package before package-specific source globs.
  SetLength(result_, Length(globs) + 1);
  result_[0] := JoinSrc(ctx.SourceRel, '*.inc');
  for i := 0 to High(globs) do
    result_[i + 1] := globs[i];
  result := result_;
end;

function FallbackSourceGlobs(const ctx : TScaffoldContext) : TArray<string>;
var
  list : TArray<string>;
begin
  SetLength(list, 2);
  list[0] := JoinSrc(ctx.SourceRel, '*.pas');
  list[1] := JoinSrc(ctx.SourceRel, '*.inc');
  result := list;
end;

function FindSiblingIdByStem(const ctx : TScaffoldContext; const stem : string) : string;
var
  i : integer;
begin
  result := '';
  for i := 0 to High(ctx.LogicalPackages) do
    if SameText(ctx.LogicalPackages[i].Stem, stem) then
    begin
      if i <= High(ctx.PackageIds) then
        result := ctx.PackageIds[i];
      exit;
    end;
end;

function DepAlreadyInList(const list : TSpecDependencies; const id : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(list) do
    if SameText(list[i].Id, id) then
      exit(true);
end;

function BuildDependencies(const logical : TLogicalPackage; const ctx : TScaffoldContext) : TSpecDependencies;
var
  dpkPath : string;
  requires : TArray<string>;
  rawEntry : string;
  i : integer;
  dep : TSpecDependency;
  siblingId : string;
  sourceDProj : string;
  selfId : string;
  list : TSpecDependencies;
begin
  SetLength(list, 0);
  sourceDProj := logical.RuntimeDProj;
  if sourceDProj = '' then
    sourceDProj := logical.DesignDProj;
  if sourceDProj = '' then
    exit(list);

  //Prefer the .dpk's requires clause (semantic). Fall back to DCP references
  //in the .dproj so we still get a result when the dpk is malformed or
  //missing.
  dpkPath := ChangeFileExt(sourceDProj, '.dpk');
  SetLength(requires, 0);
  if TFile.Exists(dpkPath) then
    requires := ParseDpkRequires(dpkPath);
  if Length(requires) = 0 then
    requires := ExtractDprojDcpReferences(sourceDProj);

  selfId := FindSiblingIdByStem(ctx, logical.Stem);

  for i := 0 to High(requires) do
  begin
    rawEntry := Trim(requires[i]);
    if rawEntry = '' then continue;
    //Resolve to a sibling id when one of the project's own packages shares
    //the stem - that way `requires Spring.Base;` becomes a dependency on the
    //sibling spec (which may carry a different id like Spring4D.Base).
    siblingId := FindSiblingIdByStem(ctx, rawEntry);
    if siblingId <> '' then
    begin
      //skip self-references
      if SameText(siblingId, selfId) then
        continue;
      dep.Id := siblingId;
      dep.Version := ctx.Version;
    end
    else
    begin
      dep.Id := rawEntry;
      dep.Version := '';
    end;
    if DepAlreadyInList(list, dep.Id) then
      continue;
    SetLength(list, Length(list) + 1);
    list[High(list)] := dep;
  end;
  result := list;
end;

function BuildPackageScaffold(const logical : TLogicalPackage; const ctx : TScaffoldContext;
  const packageId : string; const logger : ILogger) : TSpecScaffold;
var
  scaffold : TSpecScaffold;
  sourceGlobs : TArray<string>;
  licenseLeaf : string;
  licenseGlob : TArray<string>;
  merged : TArray<string>;
  i : integer;
  targets : TArray<TSpecTargetInfo>;
  unionPlatforms : TDPMPlatforms;
  designPlatforms : TDPMPlatforms;
  targetIdx : integer;
  buildDProjs : TArray<string>;
  designDProjs : TArray<string>;
begin
  scaffold.PackageId := packageId;
  scaffold.Version := ctx.Version;
  scaffold.Description := ctx.Description;
  scaffold.Author := ctx.Author;
  scaffold.Copyright := ctx.Copyright;
  scaffold.License := ctx.License;
  scaffold.ProjectUrl := ctx.ProjectUrl;
  scaffold.RepositoryUrl := ctx.ProjectUrl;
  SetLength(scaffold.Tags, 0);

  //1. Source globs - prefer dpk/dproj-derived, fall back to broad sourceRel/*.pas
  if logical.RuntimeDProj <> '' then
    sourceGlobs := DeriveSourceGlobsForPackage(logical.RuntimeDProj, ctx)
  else
    SetLength(sourceGlobs, 0);
  if Length(sourceGlobs) = 0 then
    sourceGlobs := FallbackSourceGlobs(ctx);
  sourceGlobs := PrependSharedIncludes(sourceGlobs, ctx);

  //2. LICENSE file at project root
  if FindLicenseFile(ctx.RootDir, licenseLeaf) then
  begin
    SetLength(licenseGlob, 1);
    licenseGlob[0] := licenseLeaf;
    SetLength(merged, Length(licenseGlob) + Length(sourceGlobs));
    merged[0] := licenseGlob[0];
    for i := 0 to High(sourceGlobs) do
      merged[i + 1] := sourceGlobs[i];
    sourceGlobs := merged;
  end;
  scaffold.SourceGlobs := sourceGlobs;

  //3. Build/Design dprojs (leaf names; writer joins with $packageSource$ folder)
  SetLength(buildDProjs, 0);
  SetLength(designDProjs, 0);
  if logical.RuntimeDProj <> '' then
  begin
    SetLength(buildDProjs, 1);
    buildDProjs[0] := ExtractFileName(logical.RuntimeDProj);
  end;
  if logical.DesignDProj <> '' then
  begin
    SetLength(designDProjs, 1);
    designDProjs[0] := ExtractFileName(logical.DesignDProj);
  end;
  scaffold.BuildDProjs := buildDProjs;
  scaffold.DesignDProjs := designDProjs;

  //4. Target platforms per logical package (collect from every compiler folder
  //that contains the runtime dproj's leaf name; fall back to the design dproj
  //when there's no runtime; finally fall back to the union across everything).
  if (logical.RuntimeDProj <> '') and (Length(ctx.CompilerFolders) > 0) then
    targets := CollectPackagePlatforms(ctx.CompilerFolders, ExtractFileName(logical.RuntimeDProj),
      logger, ctx.Config)
  else if (logical.DesignDProj <> '') and (Length(ctx.CompilerFolders) > 0) then
    targets := CollectPackagePlatforms(ctx.CompilerFolders, ExtractFileName(logical.DesignDProj),
      logger, ctx.Config)
  else
    targets := BuildFallbackTargets(ctx.CompilerFolders, ctx.Config, logger);
  SortTargetsByCompiler(targets);
  scaffold.Targets := targets;

  scaffold.HasPackagesFolder := ctx.HasPackages;
  scaffold.PackagesFolderRel := ctx.PackagesRel;

  //5. Build platforms = union across target compilers. Design platforms respect
  //   the IDE's supported bitness (Win32 always, Win64 from Delphi 12).
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

  //6. Dependencies - parse dpk/dproj requires, map to sibling ids where possible.
  scaffold.Dependencies := BuildDependencies(logical, ctx);

  result := scaffold;
end;

function WriteScaffoldFile(const scaffold : TSpecScaffold; const rootDir : string; const logger : ILogger) : TExitCode;
var
  outputPath : string;
  yamlText : string;
begin
  yamlText := BuildSpecYaml(scaffold);
  outputPath := TPath.Combine(rootDir, scaffold.PackageId + cPackageSpecExt);
  try
    TFile.WriteAllText(outputPath, yamlText, TEncoding.UTF8);
  except
    on e : Exception do
    begin
      logger.Error('Failed to write ' + outputPath + ' : ' + e.Message);
      exit(TExitCode.Error);
    end;
  end;
  logger.Information('Wrote ' + outputPath);
  result := TExitCode.OK;
end;

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
  exitCode : TExitCode;
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

    exitCode := WriteScaffoldFile(scaffold, rootDir, Logger);
    if exitCode <> TExitCode.OK then
      exit(exitCode);
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
    exitCode := WriteScaffoldFile(scaffold, rootDir, Logger);
    if exitCode <> TExitCode.OK then
      exit(exitCode);
    Logger.Information('Review the generated file and fill in any remaining values.');
    exit(TExitCode.OK);
  end;

  //mode = 0: scaffold all
  for i := 0 to High(logicalPackages) do
  begin
    effectiveId := logicalPackages[i].Stem;
    scaffold := BuildPackageScaffold(logicalPackages[i], ctx, effectiveId, Logger);
    exitCode := WriteScaffoldFile(scaffold, rootDir, Logger);
    if exitCode <> TExitCode.OK then
      exit(exitCode);
    if cancellationToken.IsCancelled then
      exit(TExitCode.OK);
  end;
  Logger.Information('Wrote ' + IntToStr(Length(logicalPackages)) + ' spec file(s). Review and fill in remaining fields.');
  result := TExitCode.OK;
end;

end.
