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

unit DPM.Console.Command.Spec.Scaffold;

//Pure scaffold-building helpers shared by the dpm `spec` command and the
//DSpecCreator GUI wizard. No console interaction here - callers (the command
//orchestrator or the wizard) collect the inputs into a TScaffoldContext via
//prompts/dialogs, then call BuildPackageScaffold + WriteScaffoldFile.

interface

uses
  System.SysUtils,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Console.Command.Spec.Discovery,
  DPM.Console.Command.Spec.Writer;

type
  //Shared inputs collected once and reused for every package we scaffold. Split
  //out so the single-package path and the multi-package loop both consume the
  //same data.
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
function ChoosePrimaryFolder(const folders : TCompilerFolders) : integer;
function CollectPackagePlatforms(const compilerFolders : TCompilerFolders; const dprojLeaf : string;
  const logger : ILogger; const config : IConfiguration) : TArray<TSpecTargetInfo>;
function BuildFallbackTargets(const folders : TCompilerFolders; const config : IConfiguration;
  const logger : ILogger) : TArray<TSpecTargetInfo>;
function DeriveSourceGlobsForPackage(const runtimeDProj : string; const ctx : TScaffoldContext) : TScaffoldSourceEntries;
function PrependSharedIncludes(const entries : TScaffoldSourceEntries; const ctx : TScaffoldContext) : TScaffoldSourceEntries;
function FallbackSourceGlobs(const ctx : TScaffoldContext) : TScaffoldSourceEntries;
function FindSiblingIdByStem(const ctx : TScaffoldContext; const stem : string) : string;
function BuildDependencies(const logical : TLogicalPackage; const ctx : TScaffoldContext) : TSpecDependencies;

function BuildPackageScaffold(const logical : TLogicalPackage; const ctx : TScaffoldContext;
  const packageId : string; const logger : ILogger) : TSpecScaffold;

/// <summary>
///  Builds a single scaffold that bundles several selected projects into one dspec:
///  every selected runtime dproj becomes a build entry and every selected design
///  dproj a design entry, with source globs, target platforms and dependencies
///  unioned across them. Requires pointing at another selected project are dropped
///  (internal to the merged package). Used by the "combine selected projects" mode
///  in both the spec command and the DSpecCreator wizard.
/// </summary>
function BuildMergedScaffold(const selected : TLogicalPackages; const ctx : TScaffoldContext;
  const packageId : string; const logger : ILogger) : TSpecScaffold;

/// <summary>
///  Generates the .dspec.yaml for the scaffold and writes it to
///  rootDir\<PackageId>.dspec.yaml. Returns false on write failure (and logs
///  the error). outputPath is always set to the intended path so callers can
///  open the file afterwards.
/// </summary>
function WriteScaffoldFile(const scaffold : TSpecScaffold; const rootDir : string;
  const logger : ILogger; out outputPath : string) : boolean;

implementation

uses
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Types,
  DPM.Core.Constants;

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

//Resolves the project-root-relative .pas unit paths a package contains, preferring
//the .dpk `contains` clause and falling back to the .dproj's DCCReference list.
function ResolvePackageUnitPaths(const runtimeDProj : string; const ctx : TScaffoldContext) : TArray<string>;
var
  dpkPath : string;
  dpkUnits : TDpkUnits;
  unitPaths : TArray<string>;
  resolved : string;
  i : integer;
  containerDir : string;
  rawExtracted : TArray<string>;
begin
  SetLength(unitPaths, 0);
  if runtimeDProj = '' then
    exit(unitPaths);
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

  result := unitPaths;
end;

function SrcEntry(const glob : string) : TScaffoldSourceEntry;
begin
  result.Glob := glob;
  SetLength(result.Exclude, 0);
end;

function EntryGlobExists(const entries : TScaffoldSourceEntries; const glob : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(entries) do
    if SameText(entries[i].Glob, glob) then
      exit(true);
end;

//Extracts just the glob/path strings of the entries (drops excludes). Used for a
//generated package definition's `files`, which is a plain string list.
function GlobsOf(const entries : TScaffoldSourceEntries) : TArray<string>;
var
  i : integer;
  list : TArray<string>;
begin
  SetLength(list, Length(entries));
  for i := 0 to High(entries) do
    list[i] := entries[i].Glob;
  result := list;
end;

//Prepends a plain source entry to the front of the list.
function PrependEntry(const entries : TScaffoldSourceEntries; const entry : TScaffoldSourceEntry) : TScaffoldSourceEntries;
var
  i : integer;
  list : TScaffoldSourceEntries;
begin
  SetLength(list, Length(entries) + 1);
  list[0] := entry;
  for i := 0 to High(entries) do
    list[i + 1] := entries[i];
  result := list;
end;

//Turns a set of resolved unit paths into source entries (filesystem-aware), plus a
//co-located recursive .inc glob when the units share a folder that exists on disk.
function DeriveSourceGlobsFromUnitPaths(const unitPaths : TArray<string>; const ctx : TScaffoldContext) : TScaffoldSourceEntries;
var
  entries : TScaffoldSourceEntries;
  commonParent : string;
  incGlob : string;
begin
  SetLength(entries, 0);
  if Length(unitPaths) = 0 then
    exit(entries);

  entries := DeriveSourceGlobs(unitPaths, ctx.RootDir);

  //Add a recursive .inc glob co-located with the derived .pas glob so packages
  //that ship private includes alongside their source get picked up.
  commonParent := CommonParentFolder(unitPaths);
  if commonParent <> '' then
  begin
    if TDirectory.Exists(TPath.Combine(ctx.RootDir,
       StringReplace(commonParent, '/', PathDelim, [rfReplaceAll]))) then
    begin
      incGlob := commonParent + '/**.inc';
      if not EntryGlobExists(entries, incGlob) then
      begin
        SetLength(entries, Length(entries) + 1);
        entries[High(entries)] := SrcEntry(incGlob);
      end;
    end;
  end;

  result := entries;
end;

function DeriveSourceGlobsForPackage(const runtimeDProj : string; const ctx : TScaffoldContext) : TScaffoldSourceEntries;
begin
  result := DeriveSourceGlobsFromUnitPaths(ResolvePackageUnitPaths(runtimeDProj, ctx), ctx);
end;

function PrependSharedIncludes(const entries : TScaffoldSourceEntries; const ctx : TScaffoldContext) : TScaffoldSourceEntries;
var
  sharedIncs : TArray<string>;
  i : integer;
  sharedGlob : string;
  result_ : TScaffoldSourceEntries;
begin
  if ctx.SourceFolder = '' then
    exit(entries);
  try
    sharedIncs := TDirectory.GetFiles(ctx.SourceFolder, '*.inc');
  except
    sharedIncs := nil;
  end;
  if Length(sharedIncs) = 0 then
    exit(entries);
  //A co-located '<sourceRel>/**.inc' already covers the root *.inc files, so don't
  //add a redundant '<sourceRel>/*.inc' on top of it.
  if EntryGlobExists(entries, JoinSrc(ctx.SourceRel, '**.inc')) then
    exit(entries);
  //prepend '<sourceRel>/*.inc' so the shared Spring.inc / jedi.inc-style files
  //land in the package before package-specific source globs.
  SetLength(result_, Length(entries) + 1);
  sharedGlob := JoinSrc(ctx.SourceRel, '*.inc');
  result_[0] := SrcEntry(sharedGlob);
  for i := 0 to High(entries) do
    result_[i + 1] := entries[i];
  result := result_;
end;

function FallbackSourceGlobs(const ctx : TScaffoldContext) : TScaffoldSourceEntries;
var
  list : TScaffoldSourceEntries;
begin
  SetLength(list, 2);
  list[0] := SrcEntry(JoinSrc(ctx.SourceRel, '*.pas'));
  list[1] := SrcEntry(JoinSrc(ctx.SourceRel, '*.inc'));
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

function StemInArray(const stems : TArray<string>; const stem : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(stems) do
    if SameText(stems[i], stem) then
      exit(true);
end;

//Parses one logical package's requires and appends them to `list`, de-duping
//against what's already there. Requires whose stem matches an entry in
//`excludeStems` are dropped - used by the merged scaffold so cross-references
//between projects bundled into the same dspec don't become dependencies.
procedure AppendDependenciesForLogical(var list : TSpecDependencies; const logical : TLogicalPackage;
  const ctx : TScaffoldContext; const excludeStems : TArray<string>);
var
  dpkPath : string;
  requires : TArray<string>;
  rawEntry : string;
  depName : string;
  depKind : TDProjKind;
  i : integer;
  dep : TSpecDependency;
  siblingId : string;
  sourceDProj : string;
  selfId : string;
begin
  sourceDProj := logical.RuntimeDProj;
  if sourceDProj = '' then
    sourceDProj := logical.DesignDProj;
  if sourceDProj = '' then
    exit;

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
    //Strip any runtime/design suffix (e.g. 'dmvcframeworkRT' -> 'dmvcframework')
    //so the dependency references the logical package id, not the dpk leaf, and
    //so the sibling lookup below matches the stem-keyed logical packages.
    depName := StripRuntimeDesignSuffix(rawEntry, depKind);
    //Drop requires that point at another project bundled into this same dspec -
    //they are internal to the merged package, not external dependencies.
    if StemInArray(excludeStems, depName) then
      continue;
    //Resolve to a sibling id when one of the project's own packages shares
    //the stem - that way `requires Spring.Base;` becomes a dependency on the
    //sibling spec (which may carry a different id like Spring4D.Base).
    siblingId := FindSiblingIdByStem(ctx, depName);
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
      dep.Id := depName;
      //The dpk requires clause carries no version, and we cannot infer one, so
      //default to 0.0.1 - a valid range that lets the package at least pack and
      //be tested. The user is expected to tighten this afterwards.
      dep.Version := '0.0.1';
    end;
    if DepAlreadyInList(list, dep.Id) then
      continue;
    SetLength(list, Length(list) + 1);
    list[High(list)] := dep;
  end;
end;

function BuildDependencies(const logical : TLogicalPackage; const ctx : TScaffoldContext) : TSpecDependencies;
var
  list : TSpecDependencies;
  noExclude : TArray<string>;
begin
  SetLength(list, 0);
  SetLength(noExclude, 0);
  AppendDependenciesForLogical(list, logical, ctx, noExclude);
  result := list;
end;

function BuildPackageScaffold(const logical : TLogicalPackage; const ctx : TScaffoldContext;
  const packageId : string; const logger : ILogger) : TSpecScaffold;
var
  scaffold : TSpecScaffold;
  sourceEntries : TScaffoldSourceEntries;
  packageDefFiles : TArray<string>;
  licenseLeaf : string;
  readmeLeaf : string;
  targets : TArray<TSpecTargetInfo>;
  unionPlatforms : TDPMPlatforms;
  designPlatforms : TDPMPlatforms;
  targetIdx : integer;
  buildDProjs : TArray<string>;
  designDProjs : TArray<string>;
  genProject : string;
  packageDef : TSpecPackageDef;
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

  //README at the project root - referenced via metadata.readme; the packer copies
  //it into the archive, so (unlike LICENSE) it does not go into the source globs.
  if FindReadmeFile(ctx.RootDir, readmeLeaf) then
    scaffold.Readme := readmeLeaf
  else
    scaffold.Readme := '';

  //1. Source entries - prefer dpk/dproj-derived, fall back to broad sourceRel/*.pas
  if logical.RuntimeDProj <> '' then
    sourceEntries := DeriveSourceGlobsForPackage(logical.RuntimeDProj, ctx)
  else
    SetLength(sourceEntries, 0);
  if Length(sourceEntries) = 0 then
    sourceEntries := FallbackSourceGlobs(ctx);
  sourceEntries := PrependSharedIncludes(sourceEntries, ctx);
  //capture the source-code globs (no LICENSE) for a generated package definition's files
  packageDefFiles := GlobsOf(sourceEntries);

  //2. LICENSE file at project root - prepended ahead of the source entries.
  if FindLicenseFile(ctx.RootDir, licenseLeaf) then
    sourceEntries := PrependEntry(sourceEntries, SrcEntry(licenseLeaf));
  scaffold.Sources := sourceEntries;

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

  //6. Source-only library (no package projects found): generate a runtime package
  //   definition from the source globs plus a matching build entry so the package
  //   actually compiles. The package-def project and the build project must be the
  //   same string so they resolve to the same generated dproj in the cache.
  if (Length(buildDProjs) = 0) and (Length(designDProjs) = 0) and (not ctx.HasPackages) then
  begin
    genProject := 'packages/' + packageId + 'R.dproj';

    packageDef.Project := genProject;
    packageDef.Kind := 'runtime';
    SetLength(packageDef.Requires, 1);
    packageDef.Requires[0] := 'rtl';
    packageDef.Files := packageDefFiles;
    SetLength(packageDef.Exclude, 0);
    packageDef.Platforms := []; //inherit the targetPlatform's platforms
    SetLength(scaffold.PackageDefs, 1);
    scaffold.PackageDefs[0] := packageDef;

    SetLength(buildDProjs, 1);
    buildDProjs[0] := genProject;
    scaffold.BuildDProjs := buildDProjs;
  end;

  //7. Dependencies - parse dpk/dproj requires, map to sibling ids where possible.
  scaffold.Dependencies := BuildDependencies(logical, ctx);

  result := scaffold;
end;

procedure AppendUniqueGlob(var list : TArray<string>; const value : string);
var
  i : integer;
begin
  for i := 0 to High(list) do
    if SameText(list[i], value) then
      exit;
  SetLength(list, Length(list) + 1);
  list[High(list)] := value;
end;

//Unions a target into the list, merging platforms when the compiler is already
//present (the same dproj leaf can appear in several selected projects' folders).
procedure MergeTargetInto(var list : TArray<TSpecTargetInfo>; const info : TSpecTargetInfo);
var
  i : integer;
begin
  for i := 0 to High(list) do
    if list[i].Compiler = info.Compiler then
    begin
      list[i].Platforms := list[i].Platforms + info.Platforms;
      if list[i].PackageSourceLiteral = '' then
        list[i].PackageSourceLiteral := info.PackageSourceLiteral;
      if list[i].PackageSourceTemplate = '' then
        list[i].PackageSourceTemplate := info.PackageSourceTemplate;
      exit;
    end;
  SetLength(list, Length(list) + 1);
  list[High(list)] := info;
end;

function BuildMergedScaffold(const selected : TLogicalPackages; const ctx : TScaffoldContext;
  const packageId : string; const logger : ILogger) : TSpecScaffold;
var
  scaffold : TSpecScaffold;
  sourceEntries : TScaffoldSourceEntries;
  unionUnitPaths : TArray<string>;
  perPkgUnits : TArray<string>;
  licenseLeaf : string;
  readmeLeaf : string;
  i, k : integer;
  targets : TArray<TSpecTargetInfo>;
  perTargets : TArray<TSpecTargetInfo>;
  unionPlatforms : TDPMPlatforms;
  designPlatforms : TDPMPlatforms;
  targetIdx : integer;
  buildDProjs : TArray<string>;
  designDProjs : TArray<string>;
  selectedStems : TArray<string>;
  deps : TSpecDependencies;
  primaryDProj : string;
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

  if FindReadmeFile(ctx.RootDir, readmeLeaf) then
    scaffold.Readme := readmeLeaf
  else
    scaffold.Readme := '';

  //1. Source globs - collect the union of every selected project's unit paths
  //   first, then derive globs once over the whole set. This is what lets the
  //   combined package collapse to a folder glob (e.g. ./Source/**.pas) when the
  //   selection owns the folder, while a single project (a subset of that folder)
  //   still lists just its own files.
  SetLength(unionUnitPaths, 0);
  for i := 0 to High(selected) do
  begin
    primaryDProj := selected[i].RuntimeDProj;
    if primaryDProj = '' then
      primaryDProj := selected[i].DesignDProj;
    perPkgUnits := ResolvePackageUnitPaths(primaryDProj, ctx);
    for k := 0 to High(perPkgUnits) do
      AppendUniqueGlob(unionUnitPaths, perPkgUnits[k]);
  end;
  sourceEntries := DeriveSourceGlobsFromUnitPaths(unionUnitPaths, ctx);
  if Length(sourceEntries) = 0 then
    sourceEntries := FallbackSourceGlobs(ctx);
  sourceEntries := PrependSharedIncludes(sourceEntries, ctx);

  //2. LICENSE file at project root - prepended ahead of the source entries.
  if FindLicenseFile(ctx.RootDir, licenseLeaf) then
    sourceEntries := PrependEntry(sourceEntries, SrcEntry(licenseLeaf));
  scaffold.Sources := sourceEntries;

  //3. Build/Design dprojs - every selected runtime leaf becomes a build entry,
  //   every selected design leaf a design entry. Collect target platforms per
  //   dproj and union them by compiler.
  SetLength(buildDProjs, 0);
  SetLength(designDProjs, 0);
  SetLength(targets, 0);
  SetLength(selectedStems, 0);
  for i := 0 to High(selected) do
  begin
    SetLength(selectedStems, Length(selectedStems) + 1);
    selectedStems[High(selectedStems)] := selected[i].Stem;

    if selected[i].RuntimeDProj <> '' then
    begin
      SetLength(buildDProjs, Length(buildDProjs) + 1);
      buildDProjs[High(buildDProjs)] := ExtractFileName(selected[i].RuntimeDProj);
      if Length(ctx.CompilerFolders) > 0 then
      begin
        perTargets := CollectPackagePlatforms(ctx.CompilerFolders,
          ExtractFileName(selected[i].RuntimeDProj), logger, ctx.Config);
        for k := 0 to High(perTargets) do
          MergeTargetInto(targets, perTargets[k]);
      end;
    end;
    if selected[i].DesignDProj <> '' then
    begin
      SetLength(designDProjs, Length(designDProjs) + 1);
      designDProjs[High(designDProjs)] := ExtractFileName(selected[i].DesignDProj);
      if Length(ctx.CompilerFolders) > 0 then
      begin
        perTargets := CollectPackagePlatforms(ctx.CompilerFolders,
          ExtractFileName(selected[i].DesignDProj), logger, ctx.Config);
        for k := 0 to High(perTargets) do
          MergeTargetInto(targets, perTargets[k]);
      end;
    end;
  end;
  scaffold.BuildDProjs := buildDProjs;
  scaffold.DesignDProjs := designDProjs;

  if Length(targets) = 0 then
    targets := BuildFallbackTargets(ctx.CompilerFolders, ctx.Config, logger);
  SortTargetsByCompiler(targets);
  scaffold.Targets := targets;

  scaffold.HasPackagesFolder := ctx.HasPackages;
  scaffold.PackagesFolderRel := ctx.PackagesRel;

  //4. Build platforms = union across target compilers. Design platforms respect
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

  //merged mode always has at least one selected dproj, so the source-only
  //synthetic package generation in BuildPackageScaffold doesn't apply here.
  SetLength(scaffold.PackageDefs, 0);

  //5. Dependencies - union across selected projects, dropping requires that
  //   resolve to another selected project (internal to this merged package).
  SetLength(deps, 0);
  for i := 0 to High(selected) do
    AppendDependenciesForLogical(deps, selected[i], ctx, selectedStems);
  scaffold.Dependencies := deps;

  result := scaffold;
end;

function WriteScaffoldFile(const scaffold : TSpecScaffold; const rootDir : string;
  const logger : ILogger; out outputPath : string) : boolean;
var
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
      exit(false);
    end;
  end;
  logger.Information('Wrote ' + outputPath);
  result := true;
end;

end.
