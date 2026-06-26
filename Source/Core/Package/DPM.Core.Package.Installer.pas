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

unit DPM.Core.Package.Installer;

interface

uses
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Search,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Project.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Project.PackageGenerator,
  DPM.Core.Compiler.Interfaces;

type
  TPackageInstaller = class(TInterfacedObject, IPackageInstaller)
  private
    FLogger: ILogger;
    FConfigurationManager: IConfigurationManager;
    FRepositoryManager: IPackageRepositoryManager;
    FPackageCache: IPackageCache;
    FDependencyResolver: IDependencyResolver;
    FContext: IPackageInstallerContext;
    FCompilerFactory: ICompilerFactory;
    FReceiptService : IReceiptService;
    FPackageGenerator : IPackageProjectGenerator;
    // P2 §2.6 — populate each top-level reference's ManifestHash from the
    // verification receipt the cache wrote during install. The editor writes
    // this back to the .dproj as `manifestHash="sha256:..."`.
    procedure PopulateManifestHashes(const graph : IPackageReference;
                                      const compilerVersion : TCompilerVersion);
    // Recursively clears the ManifestHash lock on every node in the graph. Used on a detected
    // compiler upgrade (in-place dproj opened in a newer IDE) so the per-compiler locks recorded
    // under the old compiler are discarded rather than failing validation against the new build.
    procedure ClearManifestHashes(const graph : IPackageReference);
    // Single-pass equivalent of ValidateLockedManifestHashes followed by
    // PopulateManifestHashes - reads each receipt at most once. Validation still
    // runs to completion and the refresh is only applied when validation passes,
    // so the abort-before-any-write semantics of the two-call sequence are kept.
    // The id of every package whose lock did not match is appended to mismatchedPackageIds
    // so the caller can report them and skip loading their design-time BPLs without aborting
    // the whole restore.
    function ValidateAndPopulateManifestHashes(const graph : IPackageReference;
                                               const compilerVersion : TCompilerVersion;
                                               const ignoreLocks : boolean;
                                               const mismatchedPackageIds : IList<string>) : boolean;
  protected
    function Init(const options : TSearchOptions) : IConfiguration;
    function GetPackageInfo(const cancellationToken: ICancellationToken; const packageId: IPackageIdentity): IPackageInfo;
    function CreateProjectRefs(const cancellationToken: ICancellationToken; const rootnode: IPackageReference; const projectReferences: IList<IPackageReference>): boolean;

    function CollectSearchPaths(const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                const compilerVersion: TCompilerVersion; const platform: TDPMPlatform;  const searchPaths: IList<string>): boolean;

    function DownloadPackages(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
    //Ensures a single package is present in the cache. Git registry packages are cloned
    //in place (clone + dspec); all others are downloaded as a .dpkg and extracted.
    function EnsurePackageInCache(const cancellationToken: ICancellationToken; const packageInfo: IPackageInfo): boolean;

    function CollectPlatformsFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function GetCompilerVersionFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration) : boolean;

    function CompilePackage(const cancellationToken: ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                            const packageSpec: IPackageSpec;  const force: boolean; const forceDebug : boolean): boolean;

    function BuildDependencies(const cancellationToken: ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                               const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                               const packageSpecs: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;

    //Design-time packages load into the IDE process, so the design BPL (and the runtime .dcp it links
    //against) must exist for the IDE host platform - Win32 for the classic IDE, Win64 for bds64. The
    //per-platform install/restore loop only compiles the project's target platforms, so a project that
    //does not target the host platform (eg a Win64-only project in the 32-bit IDE) never produces the
    //design BPL the IDE needs. This compiles the host platform into the cache so the BPL exists. It is
    //cache-only and IDE-only : the project (search paths, copy-local, dproj references) is untouched
    //because the host platform is not one of the project's targets, and it is a no-op in the CLI.
    function EnsureDesignHostPlatformCompiled(const cancellationToken: ICancellationToken; const options: TSearchOptions;
                                              const graph: IPackageReference; const installedPlatforms: TDPMPlatforms): boolean;

    //Returns the path to dpm.exe to record in the project's <DPMExe> for the copy-local targets to
    //prefer, or '' when the running process isn't dpm (e.g. the IDE plugin restoring) - in which case
    //the targets file falls back to 'dpm' on PATH.
    function GetDpmExePathForTargets : string;

    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>) : boolean;

    //Builds a {lowercase id -> spec} dict by walking the resolved graph and reading specs from the cache.
    //Used after the per-platform install/restore loop so design packages can be loaded once per project.
    function BuildPackageSpecsFromGraph(const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;

    //Restore fast path: validate the existing graph against each package's declared dependencies.
    //If every transient is present and within its parent's version range, the graph is good and we
    //can skip resolution. resolvedPackages is populated in topological order (deps before dependers).
    //Returns false if any violation is found - caller should fall back to a full re-resolve.
    function TryValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                     const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): boolean;

    //Looks up an IPackageInfo for (id, version, compiler), caching the result in the supplied dict.
    //Hits cache first, then repo. Pulled out as a method because Delphi anonymous methods cannot
    //capture nested functions (see TryValidateRestoreGraph).
    function GetOrLoadPackageInfo(const cancellationToken: ICancellationToken; const id: string; const version: TPackageVersion;
                                  const compilerVersion: TCompilerVersion; const cache: IDictionary<string, IPackageInfo>): IPackageInfo;

    //Builds a fresh root with the top-level references cloned (no transients). Used by the restore
    //slow path so the resolver derives transients fresh from each top-level's manifest, ignoring any
    //stale or out-of-range transient versions the user may have left in the dproj.
    function BuildTopLevelOnlyGraph(const fullGraph: IPackageReference; const compilerVersion: TCompilerVersion): IPackageReference;

    //Walks the project's recorded graph and returns a flat (id -> version) map of every package
    //currently recorded in it. Used by the restore slow path as a lock-file: when re-resolving,
    //the resolver prefers these versions over picking the latest in-range alternative. The dict is
    //keyed by lowercase id to match the resolver's id-keyed lookups.
    function CollectGraphVersions(const fullGraph: IPackageReference): IDictionary<string, TPackageVersion>;

    //Restore slow-path: iterate top-level packages and call ResolveForInstall for each, accumulating
    //resolved refs across iterations so shared transients are seen by later top-levels and not
    //double-resolved. Returns the final graph + flat resolvedPackages list ready for FinalizePackageConfiguration.
    function RestoreUsingInstallResolver(const cancellationToken: ICancellationToken; const options: TRestoreOptions; const projectFile: string;
                                         const topLevelGraph: IPackageReference; const preferredVersions: IDictionary<string, TPackageVersion>;
                                         out resultGraph: IPackageReference; out resolvedPackages: IList<IPackageInfo>): boolean;

    // Common configuration logic for install/restore - builds, collects search paths, copies local, installs design packages
    function DoConfigurePackageForPlatform(const cancellationToken: ICancellationToken; const options: TSearchOptions;
      const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
      const config: IConfiguration; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;

    // Resolves project files from path (handles .dproj, .groupproj, directory, or explicit list)
    function ResolveProjectFiles(const projectPath: string; const explicitProjects: TArray<string>;
      const projectList: IList<string>): boolean;

    // Validates and reconciles compiler version and platforms between options and project
    procedure ValidateAndSetCompilerPlatforms(const options: TSearchOptions; const projectEditor: IProjectEditor;
      out effectivePlatforms: TDPMPlatforms; out shouldSkip: boolean);

    function DoRestoreProjectForPlatform(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext;
                              out resultGraph: IPackageReference): boolean;

    function DoInstallPackageForPlatform(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFile: string; const projectEditor: IProjectEditor;
                              const platform: TDPMPlatform; const config: IConfiguration; const context: IPackageInstallerContext;
                              out resultGraph: IPackageReference): boolean;

    // Shared post-resolution logic: downloads packages, configures for platform, records graph
    function FinalizePackageConfiguration(const cancellationToken: ICancellationToken; const options: TSearchOptions;
      const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
      const config: IConfiguration; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; out resultGraph: IPackageReference): boolean;

    // Helper to create and load a project editor
    function LoadProjectEditor(const projectFile: string; const config: IConfiguration;
      const compilerVersion: TCompilerVersion; out projectEditor: IProjectEditor): boolean;

    function DoCachePackage(const cancellationToken: ICancellationToken; const Options: TCacheOptions): boolean;

    //After a package has been extracted into the cache by DoCachePackage, this resolves its dependency
    //graph and compiles the package (and its dependencies) into the cache for every platform the package
    //supports. Without this the cached entry only contains source - no lib\{platform} .dcu/.dcp or
    //bpl\{platform} .bpl - so it is incomplete (the IDE/restore would have to build it on first use, and
    //`dpm cache verify` would treat it as invalid). Mirrors the build half of DoConfigurePackageForPlatform
    //but writes only to the cache (no project search paths / copy-local / dproj edits).
    function DoBuildCachedPackage(const cancellationToken: ICancellationToken; const Options: TCacheOptions; const packageInfo: IPackageInfo): boolean;

    // works out what compiler/platform then calls DoInstallPackage
    function InstallPackage(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectEditor: IProjectEditor; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    // user specified a package file - will install for single compiler/platform - calls InstallPackageFromId
    function InstallPackageFromFile(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>; const config: IConfiguration;
                                    const context: IPackageInstallerContext): boolean;

    // resolves package from id - calls InstallPackage
    function InstallPackageFromId(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function UnInstallFromProject(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const projectFile: string; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function RestoreProject(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    // calls either InstallPackageFromId or InstallPackageFromFile depending on options.
    function Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;


    function Uninstall(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
    // calls restore project
    function Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;

    function Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;

    function Cache(const cancellationToken: ICancellationToken; const Options: TCacheOptions): boolean;

    function ProjectHasPackageReferences(const projectFile : string; const compilerVersion : TCompilerVersion) : boolean;

  public
    constructor Create(const logger: ILogger;
      const configurationManager: IConfigurationManager;
      const repositoryManager: IPackageRepositoryManager;
      const packageCache: IPackageCache;
      const dependencyResolver: IDependencyResolver;
      const context: IPackageInstallerContext;
      const compilerFactory: ICompilerFactory;
      const receiptService : IReceiptService;
      const packageGenerator : IPackageProjectGenerator);
  end;

implementation

uses
  System.IOUtils,
  System.Types,
  System.SysUtils,
  Spring,
  Spring.Collections.Extensions,
  VSoft.AntPatterns,
  DPM.Core.Constants,
  DPM.Core.Compiler.BOM,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Project.CopyLocalTargets,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Options.List,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Classes,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.InstallerContext;

{ TPackageInstaller }

function TPackageInstaller.Cache(const cancellationToken: ICancellationToken;  const Options: TCacheOptions): boolean;
var
  config: IConfiguration;
begin
  result := false;
  config := Init(Options);
  //logged in init
  if config = nil then
      exit;

  //Installs the package into the cache (the `dpm cache install` command): downloads and extracts
  //the package, resolves and downloads its dependencies, then compiles them all into the cache.
  //One .dpkg per compiler in the new model - it bundles every platform the package supports, so a
  //single download covers them all.
  result := DoCachePackage(cancellationToken, Options);
end;

function TPackageInstaller.ProjectHasPackageReferences(const projectFile : string; const compilerVersion : TCompilerVersion) : boolean;
var
  projectEditor : IProjectEditor;
  fullPath : string;
begin
  result := true; // if we can't load an existing project, let restore proceed so any problem is surfaced
  fullPath := projectFile;
  if TPathUtils.IsRelativePath(fullPath) then
  begin
    fullPath := TPath.Combine(GetCurrentDir, fullPath);
    fullPath := TPathUtils.CompressRelativePath(fullPath);
  end;
  // A project that isn't on disk yet (a new, unsaved project — or a transient
  // placeholder the IDE creates and removes during project creation) has no
  // package references. Treat "not on disk" as "no references" so the IDE skips
  // restore (and the log window) for new projects. The default-true above is
  // kept for projects that DO exist but fail to load, so genuine problems with
  // a real project are still surfaced.
  if not FileExists(fullPath) then
  begin
    result := false;
    exit;
  end;
  //Reading PackageReferences is pure xml - the config is only used by the search path code,
  //not when loading [PackageRefs], so we don't need a config here.
  projectEditor := TProjectEditor.Create(FLogger, nil, compilerVersion);
  if projectEditor.LoadProject(fullPath, [TProjectElement.PackageRefs]) then
    result := projectEditor.HasPackages;
end;

function TPackageInstaller.CollectPlatformsFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration): boolean;
var
  projectFile: string;
  projectEditor: IProjectEditor;
begin
  result := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
      Options.platforms := Options.platforms + projectEditor.platforms;
  end;

end;

function TPackageInstaller.CollectSearchPaths(const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                              const compilerVersion: TCompilerVersion; const platform: TDPMPlatform;  const searchPaths: IList<string>): boolean;
var
  packageInfo: IPackageInfo;
  packageSpec: IPackageSpec;
  template: ISpecTemplate;
  sourceEntry: ISpecSourceEntry;
  packageBasePath: string;
  destination: string;
  platformLibPath: string;
  seenPaths: IDictionary<string, boolean>;
  byId: IDictionary<string, IPackageInfo>;
  visited: IDictionary<string, boolean>;
  orderedPackages: IList<IPackageInfo>;
  sortedIds: IList<string>;
  id: string;
  isGitSource: boolean;
  addedLibPath: boolean;

  procedure AddUnique(const path: string);
  begin
    if not seenPaths.ContainsKey(LowerCase(path)) then
    begin
      seenPaths[LowerCase(path)] := true;
      searchPaths.Add(path);
    end;
  end;

  //Add every subfolder under absBase that actually contains .pas files. relSub is computed
  //relative to pkgAbs so the search path matches packageBasePath layout. Skips .git metadata.
  //Delphi search paths are not recursive, so each folder must be listed individually.
  procedure AddPasSubdirs(const pkgAbs, absBase, basePath: string);
  var
    subDir: string;
    relSub: string;
  begin
    if not DirectoryExists(absBase) then
      exit;
    for subDir in TDirectory.GetDirectories(absBase, '*', TSearchOption.soAllDirectories) do
    begin
      //skip the git metadata folder and anything under it
      if SameText(ExtractFileName(subDir), '.git') then
        continue;
      if Pos(PathDelim + '.git' + PathDelim, subDir + PathDelim) > 0 then
        continue;
      if Length(TDirectory.GetFiles(subDir, '*.pas')) = 0 then
        continue;
      relSub := Copy(subDir, Length(pkgAbs) + 1, MaxInt);
      AddUnique(basePath + relSub);
    end;
  end;

  //Adds search paths for a git source entry. The base folder of the src glob is
  //always added; for a recursive (**) pattern we also add every subfolder under it
  //that actually contains .pas files (the package is cloned in place, so they exist).
  procedure AddGitSearchPaths(const pkgInfo: IPackageInfo; const basePath: string; const src: string);
  var
    baseRel: string;
    recursive: boolean;
    pkgAbs: string;
    absBase: string;
  begin
    baseRel := TPathUtils.GlobBaseDir(src);
    recursive := Pos('**', src) > 0;
    pkgAbs := IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(pkgInfo));

    if baseRel <> '' then
      AddUnique(basePath + baseRel)
    else
      AddUnique(ExcludeTrailingPathDelimiter(basePath));

    if not recursive then
      exit;

    if baseRel <> '' then
      absBase := pkgAbs + baseRel
    else
      absBase := ExcludeTrailingPathDelimiter(pkgAbs);

    AddPasSubdirs(pkgAbs, absBase, basePath);
  end;

  //Adds search paths for a packed source entry. Files were copied under {dest} preserving their
  //subfolder structure, so always add the dest root; for a recursive (**) glob also add each
  //subfolder containing .pas files (they already exist in the cache - no clone step).
  procedure AddSourceSearchPaths(const pkgInfo: IPackageInfo; const basePath, dest, src: string);
  var
    pkgAbs: string;
  begin
    AddUnique(basePath + dest);
    if Pos('**', src) = 0 then
      exit;
    pkgAbs := IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(pkgInfo));
    AddPasSubdirs(pkgAbs, pkgAbs + dest, basePath);
  end;

  //Depth-first topological visit: a package is added only after all of its (resolved)
  //dependencies, so the resulting order is bottom up (dependencies before dependers).
  //Dependencies are visited in sorted id order so the output is stable regardless of the
  //order resolvedPackages happened to arrive in.
  procedure VisitPackage(const visitId: string);
  var
    info: IPackageInfo;
    dep: IPackageDependency;
    depIds: IList<string>;
    depId: string;
  begin
    if visited.ContainsKey(visitId) then
      exit;
    visited[visitId] := true;
    if not byId.TryGetValue(visitId, info) then
      exit; //a dependency excluded for this platform - nothing to order
    depIds := TCollections.CreateList<string>;
    for dep in info.Dependencies do
      depIds.Add(LowerCase(dep.Id));
    depIds.Sort;
    for depId in depIds do
      VisitPackage(depId);
    orderedPackages.Add(info);
  end;

begin
  result := true;
  seenPaths := TCollections.CreateDictionary<string, boolean>;

  // we need to apply usesource from the graph to the package info's
  packageGraph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      pkgInfo: IPackageInfo;
    begin
      // not the most efficient thing to do
      pkgInfo := resolvedPackages.FirstOrDefault(
        function(const pkg: IPackageInfo): boolean
        begin
          result := SameText(pkg.Id, node.Id);
        end);
      // A graph node may legitimately be absent from resolvedPackages when it has been
      // excluded for this platform (it doesn't support it) - nothing to propagate then.
      if pkgInfo = nil then
        exit;
      pkgInfo.UseSource := pkgInfo.UseSource or node.UseSource;
    end);

  //Build a deterministic, dependency-ordered (bottom up) list to drive search-path order.
  //We derive the order from each package's own dependency metadata via a topological sort rather
  //than from the order of resolvedPackages, which varies depending on which restore path produced
  //it (fast graph-validation walk vs slow re-resolution). A stable order avoids rewriting the
  //dproj's search paths when nothing has actually changed.
  byId := TCollections.CreateDictionary<string, IPackageInfo>;
  sortedIds := TCollections.CreateList<string>;
  for packageInfo in resolvedPackages do
  begin
    byId[LowerCase(packageInfo.Id)] := packageInfo;
    sortedIds.Add(LowerCase(packageInfo.Id));
  end;
  //seed the walk in sorted id order so sibling (independent) packages have a stable order too.
  sortedIds.Sort;

  visited := TCollections.CreateDictionary<string, boolean>;
  orderedPackages := TCollections.CreateList<IPackageInfo>;
  for id in sortedIds do
    VisitPackage(id);

  for packageInfo in orderedPackages do
  begin
    packageSpec := FPackageCache.GetPackageSpec(packageInfo);
    if packageSpec = nil then
    begin
      FLogger.Error('Unable to get spec for package ' + packageInfo.ToString);
      exit(false);
    end;

    if (packageSpec.TargetPlatform = nil) or (packageSpec.TargetPlatform.TemplateName = '') then
    begin
      FLogger.Warning('Package [' + packageInfo.Id + '] spec has no target platform / template - no search paths added');
      continue;
    end;

    template := packageSpec.FindTemplate(packageSpec.TargetPlatform.TemplateName);
    if template = nil then
    begin
      FLogger.Warning('Package [' + packageInfo.Id + '] template [' + packageSpec.TargetPlatform.TemplateName + '] not found - no search paths added');
      continue;
    end;

    packageBasePath := packageInfo.Id + PathDelim + packageInfo.Version.ToStringNoMeta + PathDelim;

    //git packages are cloned in place, so their source lives at the repo-relative
    //location given by each source entry's src glob (dest is meaningless - there is
    //no copy step). Detect via the marker the in-place install writes into the cache
    //folder - robust even when SourceName is not carried on the resolved-graph package.
    isGitSource := FileExists(IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(packageInfo)) + cGitPackageMarkerFile);

    //A compiled package contributes its per-platform lib folder (dcu/dcp). This covers packages with
    //build entries AND design-only packages (no build entry - e.g. a combined runtime+design package
    //like VSoft.Internal.Win7Components) - both produce output under lib\{platform}. Without the
    //design-entry case such packages fell through to the source branch and, having no source entries,
    //contributed nothing to the search path. Source consumption (UseSource) is handled by the source
    //branch below so the consumer compiles the .pas directly.
    addedLibPath := false;
    if ((template.BuildEntries.Count > 0) or (template.DesignEntries.Count > 0)) and (not packageInfo.UseSource) and compiledPackages.Contains(packageInfo) then
    begin
      //Win64/Win64x binaries are interchangeable - point at whichever the package actually built
      //to (e.g. a Win64-only package consumed by a Win64x project resolves to lib\Win64).
      platformLibPath := 'lib' + PathDelim + DPMPlatformToBDString(ResolveCompatiblePlatform(platform, packageSpec.TargetPlatform.Platforms));
      //design-only packages are only compiled for the IDE host platform(s), so the lib folder may not
      //exist for every declared target platform - only add it when it actually exists on disk.
      if DirectoryExists(IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(packageInfo)) + platformLibPath) then
      begin
        AddUnique(packageBasePath + platformLibPath);
        addedLibPath := true;
      end;
    end;

    //Fall back to source entries when no binary lib path was added - source-only / UseSource packages,
    //or a platform the package wasn't compiled for.
    if not addedLibPath then
    begin
      for sourceEntry in template.SourceEntries do
      begin
        if isGitSource then
          AddGitSearchPaths(packageInfo, packageBasePath, sourceEntry.Source)
        else
        begin
          destination := sourceEntry.Destination;
          if destination = '' then
            destination := 'src';
          AddSourceSearchPaths(packageInfo, packageBasePath, destination, sourceEntry.Source);
        end;
      end;
    end;
  end;
end;

function TPackageInstaller.CompilePackage(const cancellationToken : ICancellationToken; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                                          const packageSpec: IPackageSpec; const force: boolean; const forceDebug : boolean): boolean;
var
  template: ISpecTemplate;
  buildEntry: ISpecBuildEntry;
  designEntry: ISpecDesignEntry;
  packagePath: string;
  projectFile: string;
  bomFile: string;
  configuration: string;
  searchPaths: IList<string>;
  dependency: IPackageReference;
  childSearchPath: string;
  bomNode: IPackageReference;
  supportedByCompiler: TDPMPlatforms;
  candidates: TDPMPlatforms;
  designPlatforms: TDPMPlatforms;
  designProjectEditor: IProjectEditor;
  effectivePlatform: TDPMPlatform;

  //Build/design `project` paths are relative to the extracted package root, but are authored
  //with a leading separator (e.g. '/packages/..') and forward slashes. TPath.Combine treats a
  //leading separator as a rooted path and discards packagePath, so normalise the separators and
  //strip any leading one first - mirroring how TAntPattern.Combine handles source patterns.
  function ResolveProjectFile(const projectPath : string) : string;
  var
    relativePath : string;
  begin
    relativePath := StringReplace(projectPath, '/', PathDelim, [rfReplaceAll]);
    if (relativePath <> '') and (relativePath[1] = PathDelim) then
      Delete(relativePath, 1, 1);
    result := TPath.Combine(packagePath, relativePath);
    result := TPathUtils.CompressRelativePath('', result);
  end;

  //Copy the files matched by the template's copyToLib globs (archive-relative, e.g. 'Source/**/*.dfm')
  //into lib\{platform}, flattened - companion files like .dfm/.res that aren't in the precompiled
  //.dcu/.dcp but the consumer's compiler still needs on the search path. The matched files were
  //extracted from the package alongside the source, so we glob the package folder and copy each match.
  procedure CopyCopyToLibFiles;
  var
    copyEntry : ISpecSourceEntry;
    libAntPattern : IAntPattern;
    patterns : TArray<IFileSystemPattern>;
    pattern : IFileSystemPattern;
    sourceGlob : string;
    files : TStringDynArray;
    srcFile : string;
    targetFile : string;
  begin
    if not template.CopyToLibEntries.Any then
      exit;
    libAntPattern := TAntPattern.Create(packagePath);
    for copyEntry in template.CopyToLibEntries do
    begin
      sourceGlob := StringReplace(copyEntry.Source, '/', PathDelim, [rfReplaceAll]);
      if (sourceGlob <> '') and (sourceGlob[1] = PathDelim) then
        Delete(sourceGlob, 1, 1);
      patterns := libAntPattern.Expand(sourceGlob);
      for pattern in patterns do
      begin
        if not TDirectory.Exists(pattern.Directory) then
          continue;
        files := TDirectory.GetFiles(pattern.Directory, pattern.FileMask, TSearchOption.soTopDirectoryOnly);
        for srcFile in files do
        begin
          //Flatten - everything lands directly in lib\{platform} next to the .dcu/.dcp, no folder mirroring.
          targetFile := TPath.Combine(Compiler.LibOutputDir, ExtractFileName(srcFile));
          if FileExists(targetFile) and TFileUtils.AreSameFiles(srcFile, targetFile) then
            continue;
          try
            ForceDirectories(Compiler.LibOutputDir);
            TFile.Copy(srcFile, targetFile, true);
            FLogger.Debug('Copied [' + ExtractFileName(srcFile) + '] to lib for package [' + packageInfo.Id + '].');
          except
            on e : Exception do
              FLogger.Warning('Unable to copy [' + srcFile + '] to lib during install: ' + e.Message);
          end;
        end;
      end;
    end;
  end;

  //Copy the files matched by the template's copyToBin globs into bpl\{platform}, flattened - native
  //dlls a runtime/design BPL needs to load. Each entry carries the platform it applies to; we only
  //copy when that platform matches the platform currently being built (Win64/Win64x are interchangeable).
  procedure CopyCopyToBinFiles;
  var
    copyEntry : ISpecSourceEntry;
    binAntPattern : IAntPattern;
    patterns : TArray<IFileSystemPattern>;
    pattern : IFileSystemPattern;
    sourceGlob : string;
    files : TStringDynArray;
    srcFile : string;
    targetFile : string;
  begin
    if not template.CopyToBinEntries.Any then
      exit;
    binAntPattern := TAntPattern.Create(packagePath);
    for copyEntry in template.CopyToBinEntries do
    begin
      //Only copy entries whose platform applies to the platform being built right now.
      if not PlatformSatisfiedBy(effectivePlatform, [copyEntry.CopyToBin]) then
        continue;
      sourceGlob := StringReplace(copyEntry.Source, '/', PathDelim, [rfReplaceAll]);
      if (sourceGlob <> '') and (sourceGlob[1] = PathDelim) then
        Delete(sourceGlob, 1, 1);
      patterns := binAntPattern.Expand(sourceGlob);
      for pattern in patterns do
      begin
        if not TDirectory.Exists(pattern.Directory) then
          continue;
        files := TDirectory.GetFiles(pattern.Directory, pattern.FileMask, TSearchOption.soTopDirectoryOnly);
        for srcFile in files do
        begin
          //Flatten - everything lands directly in bpl\{platform} next to the .bpl, no folder mirroring.
          targetFile := TPath.Combine(Compiler.BPLOutputDir, ExtractFileName(srcFile));
          if FileExists(targetFile) and TFileUtils.AreSameFiles(srcFile, targetFile) then
            continue;
          try
            ForceDirectories(Compiler.BPLOutputDir);
            TFile.Copy(srcFile, targetFile, true);
            FLogger.Debug('Copied [' + ExtractFileName(srcFile) + '] to bpl for package [' + packageInfo.Id + '].');
          except
            on e : Exception do
              FLogger.Warning('Unable to copy [' + srcFile + '] to bpl during install: ' + e.Message);
          end;
        end;
      end;
    end;
  end;

begin
  result := true;
  packagePath := FPackageCache.GetPackagePath(packageInfo);

  //Win64 and Win64x produce interchangeable binaries. When this package doesn't itself declare the
  //requested platform but does declare the compatible counterpart, build (and lay out) against the
  //counterpart - e.g. a Win64-only package requested for a Win64x project compiles as Win64 (its
  //dproj has a Win64 config, not Win64x) and lands in lib\Win64. The consumer's search paths point
  //here while the dproj's DPMSearch condition keeps the project's own ($(Platform)=='Win64x') value.
  effectivePlatform := ResolveCompatiblePlatform(Compiler.Platform, packageSpec.TargetPlatform.Platforms);

  //BOM is per-platform: lib/{platform} folder is per-platform, so the compile-skip check must be too.
  //Otherwise a Win32 install leaves a BOM that incorrectly short-circuits a later Win64 install.
  bomFile := TPath.Combine(packagePath, 'package.' + DPMPlatformToBDString(effectivePlatform) + '.bom');

  // BOM optimization: skip if dependencies unchanged. A valid BOM means this package was already
  // compiled on a prior install - which means any `package definitions` were already generated
  // (that's what produced the compiled output), so there's no need to regenerate them here.
  if (not force) and FileExists(bomFile) then
  begin
    bomNode := TBOMFile.LoadFromFile(FLogger, bomFile);
    if (bomNode <> nil) and bomNode.AreEqual(packageReference) then
    begin
      FLogger.Information('Package [' + packageInfo.Id + '] [' + DPMPlatformToString(effectivePlatform) + '] - dependencies unchanged, skipping compilation.');
      exit;
    end;
  end;
  DeleteFile(bomFile); // Dependencies changed or no BOM, rebuild

  // Get template for this package
  template := packageSpec.FindTemplate(packageSpec.TargetPlatform.TemplateName);
  if template = nil then
    exit(true); // No template = nothing to build

  if (not template.BuildEntries.Any) and (not template.DesignEntries.Any) then
    exit(true); // Nothing to compile

  // Source-only libraries declare `package definitions` - generate their dpk/dproj into the cache
  // before building. This runs only on the compile path (the BOM short-circuit above already
  // returned when nothing changed), so we don't regenerate on every no-op install. The build/design
  // entry `project` paths must match the definition paths so both resolve to the same cache file.
  if (FPackageGenerator <> nil) and template.PackageDefinitions.Any then
  begin
    if not FPackageGenerator.Generate(cancellationToken, packageSpec, template, packagePath,
          Compiler.compilerVersion, packageSpec.TargetPlatform.Platforms) then
      exit(false);
  end;

  // Setup configuration
  if forceDebug then
    configuration := 'Debug'
  else
    configuration := 'Release';

  // Setup compiler output directories (standardized paths)
  Compiler.BPLOutputDir := TPath.Combine(packagePath, 'bpl' + PathDelim + DPMPlatformToBDString(effectivePlatform));
  Compiler.LibOutputDir := TPath.Combine(packagePath, 'lib' + PathDelim + DPMPlatformToBDString(effectivePlatform));
  Compiler.Configuration := configuration;

  // Set library paths from dependencies
  if packageReference.HasChildren then
  begin
    searchPaths := TCollections.CreateList<string>;
    for dependency in packageReference.Children do
    begin
      childSearchPath := FPackageCache.GetPackagePath(dependency.Id, dependency.Version.ToStringNoMeta, Compiler.compilerVersion);
      childSearchPath := TPath.Combine(childSearchPath, 'lib' + PathDelim + DPMPlatformToBDString(effectivePlatform));
      searchPaths.Add(childSearchPath);
    end;
    Compiler.SetSearchPaths(searchPaths);
  end
  else
    Compiler.SetSearchPaths(nil);

  // Compile build entries
  for buildEntry in template.BuildEntries do
  begin
    // Check platform filter if specified
    if (buildEntry.Platforms <> []) and (not (effectivePlatform in buildEntry.Platforms)) then
      continue;

    // Blank line so each project's build output is visually separated in the log.
    FLogger.NewLine;
    FLogger.Information('Building project: ' + buildEntry.Project);
    projectFile := ResolveProjectFile(buildEntry.Project);

    result := Compiler.BuildProject(cancellationToken, effectivePlatform, projectFile, configuration, packageInfo.Version, false);
    if result then
      FLogger.Success('Project [' + buildEntry.Project + '] build succeeded.')
    else
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Building project [' + buildEntry.Project + '] cancelled.')
      else
        FLogger.Error('Building project [' + buildEntry.Project + '] failed.');
      exit;
    end;
  end;

  // Compile design entries
  // A design BPL can only be built when the current runtime platform (Compiler.Platform) is
  // both a supported design host for this compiler version AND declared by the manifest
  // (if explicit) or by the design .dproj's enabled platforms (if manifest silent). The design
  // .dpk references the runtime .dcp that was just built in lib\{Compiler.Platform}, so design
  // and runtime platforms must match. The Win32/Win64 matrix is produced across the series of
  // per-platform install/restore calls - each call contributes its matching design BPL.
  for designEntry in template.DesignEntries do
  begin
    projectFile := ResolveProjectFile(designEntry.Project);

    supportedByCompiler := DesignTimePlatforms(Compiler.compilerVersion);

    if designEntry.Platforms <> [] then
      //manifest explicit - authoritative
      candidates := designEntry.Platforms
    else
    begin
      //defer to the design dproj - FConfig is only used by AddPackageReference which we don't call.
      //we only need the platform list, so only load that element - loading everything would let an
      //unrelated element (AppType, package refs, etc) fail the inspection and force the fallback.
      designProjectEditor := TProjectEditor.Create(FLogger, nil, Compiler.compilerVersion);
      if designProjectEditor.LoadProject(projectFile, [TProjectElement.Platforms]) then
      begin
        candidates := designProjectEditor.Platforms;
        if candidates = [] then
          FLogger.Warning('Design project [' + designEntry.Project + '] declares no platforms DPM supports - no design package will be built.');
      end
      else
      begin
        //LoadProject already logged the specific reason (eg file missing or invalid xml).
        FLogger.Warning('Could not inspect design project [' + designEntry.Project + '] - defaulting to Win32 only.');
        candidates := [TDPMPlatform.Win32];
      end;
    end;

    designPlatforms := supportedByCompiler * candidates;

    if not (effectivePlatform in designPlatforms) then
    begin
      FLogger.Debug('Skipping design package [' + designEntry.Project + '] - ' + DPMPlatformToString(effectivePlatform) + ' is not a supported design platform for this entry.');
      continue;
    end;

    // Blank line so each design package's build output is visually separated in the log.
    FLogger.NewLine;
    FLogger.Information('Building design package: ' + designEntry.Project + ' (' + DPMPlatformToString(effectivePlatform) + ')');

    //output dirs and search paths are already set for effectivePlatform by the runtime build above - nothing to change.
    result := Compiler.BuildProject(cancellationToken, effectivePlatform, projectFile, configuration, packageInfo.Version, true);
    if not result then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Building design package [' + designEntry.Project + '] cancelled.')
      else
        FLogger.Error('Building design package [' + designEntry.Project + '] failed.');
      exit;
    end;
    FLogger.Success('Design package [' + designEntry.Project + '] build succeeded.');
  end;

  // Copy any copyToLib companion files (.dfm/.res/etc) into lib\{platform} now that it has been built.
  CopyCopyToLibFiles;

  // Copy any copyToBin files (native dlls) into bpl\{platform} for the platform just built.
  CopyCopyToBinFiles;

  // Save the bill of materials file for future reference
  TBOMFile.SaveToFile(FLogger, bomFile, packageReference);
end;


function TPackageInstaller.GetDpmExePathForTargets : string;
begin
  result := ParamStr(0);
  //Only record the path when we are actually dpm.exe; otherwise leave it empty so the targets file
  //falls back to 'dpm' on PATH (the IDE plugin restoring would record bds.exe here otherwise).
  if not SameText(ExtractFileName(result), 'dpm.exe') then
    result := '';
end;

constructor TPackageInstaller.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager: IPackageRepositoryManager;
                                     const packageCache: IPackageCache; const dependencyResolver: IDependencyResolver; const context: IPackageInstallerContext;
                                     const compilerFactory: ICompilerFactory; const receiptService : IReceiptService;
                                     const packageGenerator : IPackageProjectGenerator);
begin
  FLogger := logger;
  FConfigurationManager := configurationManager;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FDependencyResolver := dependencyResolver;
  FContext := context;
  FCompilerFactory := compilerFactory;
  FReceiptService := receiptService;
  FPackageGenerator := packageGenerator;
end;

procedure TPackageInstaller.PopulateManifestHashes(const graph : IPackageReference;
                                                    const compilerVersion : TCompilerVersion);
var
  node : IPackageReference;
  packageFolder : string;
  receipt : TVerificationReceipt;
begin
  if (graph = nil) or (FReceiptService = nil) or (FPackageCache = nil) then
    exit;
  for node in graph.Children do
  begin
    packageFolder := FPackageCache.GetPackagePath(node.Id, node.Version.ToStringNoMeta, compilerVersion);
    if not DirectoryExists(packageFolder) then
      Continue;
    if FReceiptService.TryRead(packageFolder, receipt) and (receipt.ManifestHashHex <> '') then
      node.ManifestHash := 'sha256:' + receipt.ManifestHashHex
    else
      // No receipt — legacy package. Clear any stale lock attribute rather
      // than leave a wrong one in place; next install with the new tooling
      // will populate it.
      node.ManifestHash := '';
  end;
end;

procedure TPackageInstaller.ClearManifestHashes(const graph : IPackageReference);
var
  node : IPackageReference;
begin
  if graph = nil then
    exit;
  for node in graph.Children do
  begin
    node.ManifestHash := '';
    //locks are written recursively (WritePackageRef), so clear transients too.
    ClearManifestHashes(node);
  end;
end;


function TPackageInstaller.ValidateAndPopulateManifestHashes(const graph : IPackageReference;
                                                             const compilerVersion : TCompilerVersion;
                                                             const ignoreLocks : boolean;
                                                             const mismatchedPackageIds : IList<string>) : boolean;
var
  node : IPackageReference;
  packageFolder : string;
  receipt : TVerificationReceipt;
  expected, actual : string;
  folderExists : boolean;
  haveReceipt : boolean;
  // refresh is staged so it is applied only after the whole graph validates - this preserves
  // the original "ValidateLockedManifestHashes (abort on mismatch) then PopulateManifestHashes" order.
  refreshNodes : IList<IPackageReference>;
  refreshHashes : IList<string>;
  i : integer;
begin
  result := true;
  if (graph = nil) or (FReceiptService = nil) or (FPackageCache = nil) then
    exit;

  refreshNodes := TCollections.CreateList<IPackageReference>;
  refreshHashes := TCollections.CreateList<string>;

  for node in graph.Children do
  begin
    packageFolder := FPackageCache.GetPackagePath(node.Id, node.Version.ToStringNoMeta, compilerVersion);
    folderExists := DirectoryExists(packageFolder);
    haveReceipt := false;
    if folderExists then
      haveReceipt := FReceiptService.TryRead(packageFolder, receipt);   // read each receipt once

    // --- validation (mirrors ValidateLockedManifestHashes) ---
    // locked nodes whose folder is absent are skipped - the install path will produce them.
    // when ignoreLocks is set we skip validation entirely and just refresh the locks below.
    if (node.ManifestHash <> '') and folderExists and (not ignoreLocks) then
    begin
      if not haveReceipt then
      begin
        FLogger.Error(Format(
          '[Installer] Lock hash mismatch for [%s %s]: project pins %s but cache has no receipt.',
          [node.Id, node.Version.ToStringNoMeta, node.ManifestHash]));
        if mismatchedPackageIds <> nil then
          mismatchedPackageIds.Add(node.Id);
        result := false;
      end
      else
      begin
        expected := LowerCase(node.ManifestHash);
        if (Length(expected) > 7) and (Copy(expected, 1, 7) = 'sha256:') then
          expected := Copy(expected, 8, MaxInt);
        actual := LowerCase(receipt.ManifestHashHex);
        if expected <> actual then
        begin
          FLogger.Error(Format(
            '[Installer] Lock hash mismatch for [%s %s]: project pins %s but cache has sha256:%s. ' +
            'Edit the dproj''s PackageReference or remove the cached package to re-acquire.',
            [node.Id, node.Version.ToStringNoMeta, node.ManifestHash, actual]));
          if mismatchedPackageIds <> nil then
            mismatchedPackageIds.Add(node.Id);
          result := false;
        end;
      end;
    end;

    // --- stage the refresh (mirrors PopulateManifestHashes) ---
    // folder-absent nodes are left untouched, matching the original Continue.
    if folderExists then
    begin
      refreshNodes.Add(node);
      if haveReceipt and (receipt.ManifestHashHex <> '') then
        refreshHashes.Add('sha256:' + receipt.ManifestHashHex)
      else
        refreshHashes.Add('');
    end;
  end;

  // gate: a lock mismatch aborts before any hash is written, exactly as the two-call flow did.
  if not result then
    exit;

  for i := 0 to refreshNodes.Count - 1 do
    refreshNodes[i].ManifestHash := refreshHashes[i];
end;

function TPackageInstaller.GetPackageInfo(const cancellationToken: ICancellationToken; const packageId: IPackageIdentity): IPackageInfo;
begin
  result := FPackageCache.GetPackageInfo(cancellationToken, packageId);
  if result = nil then
    result := FRepositoryManager.GetPackageInfo(cancellationToken, packageId);
end;

function TPackageInstaller.DoCachePackage(const cancellationToken : ICancellationToken; const Options: TCacheOptions): boolean;
var
  packageIdentity: IPackageIdentity;
  packageInfo : IPackageInfo;
  packageFileName : string;
begin
  result := false;
  // When testing, skip the TOFU trust ratchets for the whole operation - not just
  // the initial file install. The build step re-checks the cached package via
  // EnsurePackage (which re-evaluates the ratchets from the receipt) and may
  // re-install it, so the skip has to cover the entire cache flow, not one call.
  // Reset in the finally below; pack/test/upload are mutually exclusive so the
  // singleton cache is never in skip-mode for an unrelated operation.
  FPackageCache.SetSkipTrustRatchets(Options.SkipTrustRatchets);
  try
  if Options.PackageFile <> '' then
  begin
    //Caching from a .dpkg file on disk (`dpm cache install <file.dpkg>`). Extract the file into the
    //cache first, then derive the identity from the file name (compiler + version are encoded there)
    //and read the package info back from the now-cached spec. The build step below still resolves and
    //compiles the dependency graph from the configured sources, exactly as the id-based path does.
    if not FPackageCache.InstallPackageFromFile(Options.PackageFile, Options.SkipTrustRatchets) then
    begin
      FLogger.Error('Failed to install package file [' + Options.PackageFile + '] into the cache');
      exit;
    end;
    packageFileName := ChangeFileExt(ExtractFileName(Options.PackageFile), '');
    if not TPackageIdentity.TryCreateFromString(FLogger, packageFileName, '', packageIdentity) then
      exit;
    //The id-based build path keys off Options.CompilerVersion / Version - populate them from the file
    //so DoBuildCachedPackage (and the resolver) target the right compiler.
    Options.compilerVersion := packageIdentity.CompilerVersion;
    Options.Version := packageIdentity.Version;
    packageInfo := FPackageCache.GetPackageInfo(cancellationToken, packageIdentity);
  end
  else if not Options.Version.IsEmpty then
  begin
    packageIdentity := TPackageIdentity.Create('', Options.packageId, Options.Version, Options.compilerVersion);
    // sourceName will be empty if we are installing the package from a file
    //try the cache first - avoids a network round-trip when we already have this exact id+version.
    packageInfo := FPackageCache.GetPackageInfo(cancellationToken, packageIdentity);
    if packageInfo = nil then
      packageInfo := FRepositoryManager.GetPackageInfo(cancellationToken, packageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty, Options.PreRelease, options.Sources);
  end;
  if packageInfo = nil then
  begin
    if Options.PackageFile <> '' then
      FLogger.Error('Unable to read package info for cached file [' + Options.PackageFile + ']')
    else
      FLogger.Error('Package [' + Options.packageId + '] for compiler [' + CompilerToString(Options.compilerVersion) + '] not found on any sources');
    exit;
  end;
  if packageInfo.IsSigned and (packageInfo.SignedBy <> '') then
    FLogger.Information('Caching package ' + packageInfo.ToString + ' (signed by ' + packageInfo.SignedBy + ')')
  else if packageInfo.IsSigned then
    FLogger.Information('Caching package ' + packageInfo.ToString + ' (signed)')
  else
    FLogger.Information('Caching package ' + packageInfo.ToString);

  if not EnsurePackageInCache(cancellationToken, packageInfo) then
    exit;

  //Extraction only lays down the source - compile the package (and its dependencies) into the cache so
  //the entry is actually usable, otherwise we leave behind an incomplete cache entry with no binaries.
  if not DoBuildCachedPackage(cancellationToken, Options, packageInfo) then
    exit;

  result := true;
  finally
    FPackageCache.SetSkipTrustRatchets(false);
  end;
end;

function TPackageInstaller.DoBuildCachedPackage(const cancellationToken: ICancellationToken; const Options: TCacheOptions; const packageInfo: IPackageInfo): boolean;
var
  projectReferences: IList<IPackageReference>;
  resolvedPackages: IList<IPackageInfo>;
  dependencyGraph: IPackageReference;
  packageSpecs: IDictionary<string, IPackageSpec>;
  spec: IPackageSpec;
  supportedPlatforms: TDPMPlatforms;
  platform: TDPMPlatform;
  packageCompiler: ICompiler;
  packagesToBuild: IList<IPackageInfo>;
  packagesToCompile: IList<IPackageInfo>;
  compiledPackages: IList<IPackageInfo>;
  pkgInfo: IPackageInfo;
const
  //There is no project in the cache flow - the resolver only uses this as a key for cross-project
  //conflict detection, and the cache command resolves a single package in isolation, so a synthetic
  //name is fine (and avoids colliding with any real project recorded in the installer context).
  cCachePseudoProject = ':cache:';
begin
  result := false;

  //Resolve the dependency graph so we know the full set of packages to build and the order to build
  //them in (a package's compile needs its dependencies' search paths). projectReferences is empty -
  //there is no existing project graph in the cache flow.
  projectReferences := TCollections.CreateList<IPackageReference>;
  if not FDependencyResolver.ResolveForInstall(cancellationToken, Options.CompilerVersion, cCachePseudoProject, Options, packageInfo, projectReferences, dependencyGraph, resolvedPackages) then
  begin
    FLogger.Error('Failed to resolve dependencies for package [' + packageInfo.ToString + ']');
    exit;
  end;

  if (resolvedPackages = nil) or (resolvedPackages.Count = 0) then
  begin
    FLogger.Error('Resolver returned no packages for [' + packageInfo.ToString + ']');
    exit;
  end;

  //Drop synthetic bundled no-ops (e.g. Indy) - they are IDE-provided, so there is nothing to
  //download or compile (and no .dpkg exists on the server). Same filter FinalizePackageConfiguration
  //applies before downloading/configuring on the normal install path.
  packagesToBuild := TCollections.CreateList<IPackageInfo>;
  for pkgInfo in resolvedPackages do
    if not IsBundledPackageInfo(pkgInfo) then
      packagesToBuild.Add(pkgInfo);

  if packagesToBuild.Count = 0 then
  begin
    //Everything resolved was bundled - nothing to cache/build, but that is not an error.
    result := true;
    exit;
  end;

  //Ensure every resolved package (dependencies included) is in the cache and load their specs - the
  //build needs the specs for templates, build entries and platform support.
  packageSpecs := TCollections.CreateDictionary<string, IPackageSpec>;
  if not DownloadPackages(cancellationToken, packagesToBuild, packageSpecs) then
    exit;

  //The .dpkg bundles every platform the package supports for this compiler. Build them all so the
  //cache entry is complete - the spec's declared platforms are the source of truth for what can be
  //built, falling back to the package info if the spec doesn't declare any.
  spec := packageSpecs[LowerCase(packageInfo.Id)];
  if (spec <> nil) and (spec.TargetPlatform <> nil) and (spec.TargetPlatform.Platforms <> []) then
    supportedPlatforms := spec.TargetPlatform.Platforms
  else
    supportedPlatforms := packageInfo.SupportedPlatforms;

  if supportedPlatforms = [] then
  begin
    FLogger.Error('Package [' + packageInfo.ToString + '] does not declare any supported platforms - nothing to build.');
    exit;
  end;

  for platform in supportedPlatforms do
  begin
    if cancellationToken.IsCancelled then
      exit;

    packageCompiler := FCompilerFactory.CreateCompiler(Options.CompilerVersion, platform);

    //BuildDependencies removes packages from this list as it compiles them, so it needs a fresh copy
    //per platform. Drop any dependency that doesn't support this platform - it can't be built for it
    //(mirrors the supported/unsupported partition in DoConfigurePackageForPlatform).
    packagesToCompile := TCollections.CreateList<IPackageInfo>;
    for pkgInfo in packagesToBuild do
    begin
      spec := packageSpecs[LowerCase(pkgInfo.Id)];
      if (spec <> nil) and (spec.TargetPlatform <> nil) and (spec.TargetPlatform.Platforms <> []) and
         (not PlatformSatisfiedBy(platform, spec.TargetPlatform.Platforms)) then
        continue;
      packagesToCompile.Add(pkgInfo);
    end;

    if packagesToCompile.Count = 0 then
      continue;

    compiledPackages := TCollections.CreateList<IPackageInfo>;
    FLogger.Information('Building package [' + packageInfo.Id + '] for platform [' + DPMPlatformToString(platform) + ']');
    if not BuildDependencies(cancellationToken, packageCompiler, dependencyGraph, packagesToCompile, compiledPackages, packageSpecs, Options) then
    begin
      FLogger.Error('Failed to build package [' + packageInfo.ToString + '] for platform [' + DPMPlatformToString(platform) + ']');
      exit;
    end;
  end;

  result := true;
end;

// convert the dependency graph to a flat list of IPackageinfo's (which has dependencies).
function TPackageInstaller.CreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference;  const projectReferences: IList<IPackageReference>): boolean;
var
  seenPackages: IDictionary<string, IPackageInfo>;
  packageRefLookup : IDictionary<string, IPackageReference>;

  function DoCreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference) : boolean;
  var
    dependency: IPackageReference;
    info: IPackageInfo;
    newPackageRef: IPackageReference;

  begin
    result := false;
    if cancellationToken.IsCancelled then
      exit;
    //breadth first is important here.. we want to process top level nodes first!
    for dependency in rootNode.Children do
    begin
      if cancellationToken.IsCancelled then
        exit;

      if seenPackages.TryGetValue(LowerCase(dependency.Id), info) then
      begin
        if not packageRefLookup.TryGetValue(LowerCase(dependency.Id), newPackageRef) then
          raise Exception.Create('Package reference not found in lookup!');
        if dependency.UseSource then
        begin
          newPackageRef.UseSource := true;
          newPackageRef.PackageInfo.UseSource := true;
        end;
      end
      else
      begin
        info := GetPackageInfo(cancellationToken, dependency);
        if info = nil then
        begin
          FLogger.Error('Unable to resolve package : ' + dependency.ToIdVersionString);
          exit;
        end;
        if cancellationToken.IsCancelled then
          exit;

        info.UseSource := dependency.UseSource;

        newPackageRef := TPackageReference.Create(rootnode, info.Id, info.Version, info.CompilerVersion, dependency.VersionRange, dependency.UseSource);
        newPackageRef.PackageInfo := info;

        if newPackageRef.VersionRange.IsEmpty then
          newPackageRef.VersionRange := TVersionRange.Create(info.Version);

        packageRefLookup[LowerCase(dependency.Id)] := newPackageRef;
        seenPackages[LowerCase(dependency.Id)] := info;
        projectReferences.Add(newPackageRef);
      end;
    end;
    result := true;
    for dependency in rootnode.Children do
    begin
      if dependency.HasChildren then
        result := DoCreateProjectRefs(cancellationToken, dependency);
      if not result then
        exit;
      if cancellationToken.IsCancelled then
        exit;
    end;

  end;



begin
  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  packageRefLookup := TCollections.CreateDictionary<string, IPackageReference>;
  result := DoCreateProjectRefs(cancellationToken,rootnode);
end;

function TPackageInstaller.DoInstallPackageForPlatform(const cancellationToken : ICancellationToken; const Options: TInstallOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext; out resultGraph: IPackageReference): boolean;
var
  newPackageIdentity: IPackageIdentity;
  packageInfo: IPackageInfo; // includes dependencies;
  existingPackageRef: IPackageReference;
  dependency : IPackageReference;
  projectPackageGraph: IPackageReference;

  projectReferences: IList<IPackageReference>;

  resolvedPackages: IList<IPackageInfo>;

  seenPackages: IDictionary<string, IPackageInfo>;


begin
  result := false;
  resultGraph := nil;

  projectPackageGraph := projectEditor.GetPackageReferences; // can return nil

  if projectPackageGraph = nil then
    projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion);

  //If the package is already in the project graph, remove it so the resolver re-resolves below.
  //Install is idempotent: repeats for an already-built platform are short-circuited by the
  //per-platform BOM check; new platforms get compiled fresh. -force is still honored downstream
  //by CompilePackage (forces a rebuild even when the BOM would otherwise skip).
  existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if existingPackageRef <> nil then
  begin
    projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
    existingPackageRef := nil;
  end;

  // We could have a transitive dependency that is being promoted.
  // Since we want to control what version is installed, we will remove
  // any transitive references to that package so the newly installed version
  // will take precedence when resolving.
  dependency := projectPackageGraph.FindFirstChild(Options.packageId);
  while dependency <> nil do
  begin
    projectPackageGraph.RemoveChild(dependency);
    dependency := projectPackageGraph.FindFirstChild(Options.packageId);
  end;

  // if the user specified a version, either the on the command line or via a file then we will use that
  if not Options.Version.IsEmpty then
  begin
    // sourceName will be empty if we are installing the package from a file
    newPackageIdentity := TPackageIdentity.Create(options.Sources, Options.packageId,  Options.Version, Options.compilerVersion);
    packageInfo := GetPackageInfo(cancellationToken, newPackageIdentity);
  end
  else
  begin
    // no version specified, so we need to get the latest version available;
    packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, options.PackageId, options.CompilerVersion, TPackageVersion.Empty,  Options.PreRelease, options.Sources);

  end;
  if packageInfo = nil then
  begin
    FLogger.Error('Package [' + Options.packageId + '] for platform [' + DPMPlatformToString(platform) + '] not found on any sources');
    exit;
  end;
  if packageInfo.IsSigned and (packageInfo.SignedBy <> '') then
    FLogger.Information('Installing package ' + packageInfo.ToString + ' (signed by ' + packageInfo.SignedBy + ')')
  else if packageInfo.IsSigned then
    FLogger.Information('Installing package ' + packageInfo.ToString + ' (signed)')
  else
    FLogger.Information('Installing package ' + packageInfo.ToString);

  if not EnsurePackageInCache(cancellationToken, packageInfo) then
    exit;

  packageInfo.UseSource := Options.UseSource;
  // we need this later when collecting search paths.

  seenPackages := TCollections.CreateDictionary<string, IPackageInfo>;
  projectReferences := TCollections.CreateList<IPackageReference>;

  //flatten and dedupe the graph to a list of package references.
  if not CreateProjectRefs(cancellationToken, projectPackageGraph, projectReferences) then
    exit;

  if not FDependencyResolver.ResolveForInstall(cancellationToken, Options.CompilerVersion, projectFile, Options, packageInfo, projectReferences, projectPackageGraph, resolvedPackages) then
  begin
    FLogger.Debug('ResolveForInstall failed');
    // we need to carry on here as resolution may have failed for another package
    // not sure what the best solution is here.. a better resolver perhaps.
     exit;
  end;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit(false);
  end;

  // Install-specific sanity check: verify the package we were installing is in the resolved list
  packageInfo := resolvedPackages.FirstOrDefault(
    function(const info: IPackageInfo): boolean
    begin
      result := SameText(info.Id, Options.packageId);
    end);

  if packageInfo = nil then
  begin
    FLogger.Error('Something went wrong, resolution did not return installed package!');
    exit(false);
  end;

  // Finalize configuration (download, build, search paths, design packages, etc.)
  result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
end;

function TPackageInstaller.DoRestoreProjectForPlatform(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext; out resultGraph: IPackageReference): boolean;
var
  projectPackageGraph: IPackageReference;
  projectReferences: IList<IPackageReference>;
  resolvedPackages: IList<IPackageInfo>;
  preferredVersions: IDictionary<string, TPackageVersion>;
begin
  result := false;
  resultGraph := nil;

  projectPackageGraph := projectEditor.GetPackageReferences;
  // can return nil
  // if there is no project package graph then there is nothing to do.
  if projectPackageGraph = nil then
    exit(true);

  //Fast path: validate the existing graph against each package's declared dependencies.
  //If it's intact, skip the resolver entirely and proceed straight to download+configure.
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  if TryValidateRestoreGraph(cancellationToken, projectPackageGraph, Options.CompilerVersion, resolvedPackages) then
  begin
    FLogger.Verbose('Restore: existing graph is consistent with package specs - skipping resolution');
    result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
    exit;
  end;

  //Slow path: graph has missing transients or out-of-range versions. Re-resolve from top-level
  //constraints by treating each top-level package as a fresh install, via ResolveForInstall.
  //The dproj acts as a lock file - collect every recorded (id, version) from the existing graph
  //so the resolver re-uses those exact versions where they still satisfy the parent's range, and
  //only picks a new version for genuinely missing transients.
  preferredVersions := CollectGraphVersions(projectPackageGraph);
  if not RestoreUsingInstallResolver(cancellationToken, Options, projectFile,
                                     BuildTopLevelOnlyGraph(projectPackageGraph, Options.CompilerVersion),
                                     preferredVersions,
                                     projectPackageGraph, resolvedPackages) then
    exit;

  projectReferences := nil;

  // Finalize configuration (download, build, search paths, design packages, etc.)
  result := FinalizePackageConfiguration(cancellationToken, Options, projectFile, projectEditor, platform, config, projectPackageGraph, resolvedPackages, resultGraph);
end;

function TPackageInstaller.BuildDependencies(const cancellationToken : ICancellationToken; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                                             const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                             const packageSpecs: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;

begin
  result := false;
  try
    // build the dependency graph in the correct order.
    projectPackageGraph.VisitDFS(
      procedure(const packageReference: IPackageReference)
      var
        pkgInfo: IPackageInfo;
        Spec: IPackageSpec;
        template: ISpecTemplate;
        forceCompile: boolean;
      begin
        Assert(packageReference.IsRoot = false, 'graph should not visit root node');

        pkgInfo := packagesToCompile.FirstOrDefault(
          function(const value: IPackageInfo): boolean
          begin
            result := SameText(value.Id, packageReference.Id);
          end);
        // if it's not found that means we have already processed the package elsewhere in the graph
        if pkgInfo = nil then
          exit;

        // removing it so we don't process it again
        packagesToCompile.Remove(pkgInfo);

        Spec := packageSpecs[LowerCase(packageReference.Id)];
        Assert(Spec <> nil);

        // Get the template for this package
        if Spec.TargetPlatform = nil then
          exit;
        template := Spec.FindTemplate(Spec.TargetPlatform.TemplateName);
        if template = nil then
          exit;

        // Check if package has build entries that require compilation
        if template.BuildEntries.Any or template.DesignEntries.Any then
        begin
          // Determine if force compilation needed (only for explicitly requested package)
          forceCompile := Options.force and SameText(pkgInfo.Id, Options.SearchTerms);

          if not CompilePackage(cancellationToken, packageCompiler, pkgInfo, packageReference, Spec, forceCompile, false) then
          begin
            if not cancellationToken.IsCancelled then
              raise Exception.Create('Compiling package [' + pkgInfo.ToIdVersionString + '] failed.');
            exit;
          end;

          compiledPackages.Add(pkgInfo);
        end;
      end);
    result := true;

  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      exit;
    end;
  end;

end;


function TPackageInstaller.EnsureDesignHostPlatformCompiled(const cancellationToken : ICancellationToken; const options : TSearchOptions;
                                                            const graph : IPackageReference; const installedPlatforms : TDPMPlatforms): boolean;
var
  hostPlatform : TDPMPlatform;
  packageCompiler : ICompiler;
  packageSpecs : IDictionary<string, IPackageSpec>;
  resolvedPackages : IList<IPackageInfo>;
  compiledPackages : IList<IPackageInfo>;
  infoCache : IDictionary<string, IPackageInfo>;
begin
  result := true;

  //CLI never loads design packages - InstallDesignPackages is a no-op there, so the host-platform
  //compile would be wasted work.
  if not TSystemUtils.IsIDEProcess then
    exit;

  //The plugin's own bitness tells us which BPL variant the IDE will load (the same decision made in
  //TDPMIDEPackageInstallerContext.InstallDesignPackages).
  if TSystemUtils.Is64BitIDE then
    hostPlatform := TDPMPlatform.Win64
  else
    hostPlatform := TDPMPlatform.Win32;

  //Already produced by the normal per-platform loop - nothing extra to do.
  if hostPlatform in installedPlatforms then
    exit;

  //Flatten the resolved graph to the set of packages to compile. Prefer the PackageInfo the resolver
  //already attached to each node, falling back to a cache/repo lookup if absent.
  infoCache := TCollections.CreateDictionary<string, IPackageInfo>;
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  graph.VisitDFS(
    procedure(const node : IPackageReference)
    var
      info : IPackageInfo;
      key : string;
    begin
      key := LowerCase(node.Id);
      if infoCache.ContainsKey(key) then
        exit;
      info := node.PackageInfo;
      if info = nil then
        info := GetOrLoadPackageInfo(cancellationToken, node.Id, node.Version, options.compilerVersion, infoCache)
      else
        infoCache[key] := info;
      //bundled no-ops (e.g. Indy) have no package to compile - the graph already excludes them,
      //this guards the fallback load path defensively.
      if (info <> nil) and (not IsBundledPackageInfo(info)) then
        resolvedPackages.Add(info);
    end);

  if resolvedPackages.Count = 0 then
    exit;

  packageSpecs := BuildPackageSpecsFromGraph(graph, options.compilerVersion);
  packageCompiler := FCompilerFactory.CreateCompiler(options.compilerVersion, hostPlatform);
  compiledPackages := TCollections.CreateList<IPackageInfo>;

  FLogger.Information('Compiling design-time packages for the IDE host platform [' + DPMPlatformToString(hostPlatform) + '].', true);
  //Cache-only : BuildDependencies populates bpl\{host} and lib\{host} in the cache. We deliberately do
  //not call CollectSearchPaths/CopyLocal/AddSearchPaths - the host platform is not a project target.
  result := BuildDependencies(cancellationToken, packageCompiler, graph, resolvedPackages, compiledPackages, packageSpecs, options);
end;

function TPackageInstaller.EnsurePackageInCache(const cancellationToken: ICancellationToken; const packageInfo: IPackageInfo): boolean;
var
  packageFileName: string;
  sourceType: TSourceType;
begin
  result := false;

  //git registry packages are installed in place (clone + dspec) rather than
  //downloaded as a .dpkg. The repository handles cache freshness (incl. HEAD
  //tracking for untagged repos) so we always call it.
  if FRepositoryManager.TryGetSourceType(packageInfo.SourceName, sourceType) and (sourceType = TSourceType.GitRegistry) then
  begin
    if not FRepositoryManager.InstallPackageInPlace(cancellationToken, packageInfo, FPackageCache.GetPackagePath(packageInfo)) then
    begin
      FLogger.Error('Failed to install git package [' + packageInfo.ToString + ']');
      exit;
    end;
    result := true;
    exit;
  end;

  if FPackageCache.EnsurePackage(packageInfo) then
    exit(true);

  // not in the cache, so we need to get it from the repository
  if not FRepositoryManager.DownloadPackage(cancellationToken, packageInfo, FPackageCache.GetPackageFileFolder(packageInfo), packageFileName) then
  begin
    if cancellationToken.IsCancelled then
      FLogger.Error('Downloading package [' + packageInfo.ToString + '] cancelled.')
    else
      FLogger.Error('Failed to download package [' + packageInfo.ToString + ']');
    exit;
  end;
  if not FPackageCache.InstallPackageFromFile(packageFileName) then
  begin
    FLogger.Error('Failed to install package file [' + packageFileName + '] into the cache');
    exit;
  end;
  result := true;
end;

function TPackageInstaller.DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
var
  packageInfo: IPackageInfo;
  spec: IPackageSpec;
begin
  result := false;
  //TODO : Download in parallel
  for packageInfo in resolvedPackages do
  begin
    if cancellationToken.IsCancelled then
      exit;

    if not EnsurePackageInCache(cancellationToken, packageInfo) then
      exit;
    if cancellationToken.IsCancelled then
      exit;
    if not packageSpecs.ContainsKey(LowerCase(packageInfo.Id)) then
    begin
      spec := FPackageCache.GetPackageSpec(packageInfo);
      Assert(spec <> nil);
      packageSpecs[LowerCase(packageInfo.Id)] := spec;
    end;

  end;
  result := true;

end;

function TPackageInstaller.DoConfigurePackageForPlatform(const cancellationToken: ICancellationToken; const options: TSearchOptions;
  const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
  const config: IConfiguration; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
var
  packagesToCompile: IList<IPackageInfo>;
  compiledPackages: IList<IPackageInfo>;
  packageSearchPaths: IList<string>;
  packageCompiler: ICompiler;
  supportedPackages: IList<IPackageInfo>;
  unsupportedPackageIds: IList<string>;
  packageInfo: IPackageInfo;
  spec: IPackageSpec;
  unsupportedList: string;
  id: string;
begin
  result := false;

  // Partition the resolved packages into those that support this platform and those that
  // don't. Rather than abandoning the whole platform when a single package lacks support,
  // we configure the supporting packages and report the rest so the user can fix them
  // (remove the package or pick a version that supports the platform) and keep working.
  supportedPackages := TCollections.CreateList<IPackageInfo>;
  unsupportedPackageIds := TCollections.CreateList<string>;
  for packageInfo in resolvedPackages do
  begin
    spec := packageSpecs[LowerCase(packageInfo.Id)];
    // Only treat as unsupported when the package actually declares platform support and
    // this platform isn't in it - a package that declares nothing is left to the existing
    // downstream handling rather than being excluded here.
    if (spec <> nil) and (spec.TargetPlatform <> nil) and (spec.TargetPlatform.Platforms <> []) and
       (not PlatformSatisfiedBy(platform, spec.TargetPlatform.Platforms)) then
      unsupportedPackageIds.Add(packageInfo.Id)
    else
      supportedPackages.Add(packageInfo);
  end;

  if unsupportedPackageIds.Count > 0 then
  begin
    unsupportedList := '';
    for id in unsupportedPackageIds do
    begin
      if unsupportedList <> '' then
        unsupportedList := unsupportedList + ', ';
      unsupportedList := unsupportedList + id;
    end;
    FLogger.Warning('Platform [' + DPMPlatformToString(platform) + '] not supported by package(s): ' + unsupportedList +
      ' - these packages are skipped for this platform. Remove them or select a version that supports [' +
      DPMPlatformToString(platform) + '].', true);
  end;

  // Nothing supports this platform - skip it entirely (not an error).
  if supportedPackages.Count = 0 then
  begin
    FLogger.Information('Platform [' + DPMPlatformToString(platform) + '] skipped - not supported by any packages', true);
    result := true;
    exit;
  end;

  compiledPackages := TCollections.CreateList<IPackageInfo>;
  packagesToCompile := TCollections.CreateList<IPackageInfo>(supportedPackages);
  packageSearchPaths := TCollections.CreateList<string>;
  packageCompiler := FCompilerFactory.CreateCompiler(options.compilerVersion, platform);

  if not BuildDependencies(cancellationToken, packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageSpecs, options) then
    exit;

  if not CollectSearchPaths(projectPackageGraph, supportedPackages, compiledPackages, projectEditor.compilerVersion, platform, packageSearchPaths) then
    exit;

  //Copy-local runs at build time (the output dir is only reliably known then), not here. At
  //restore/install we just ensure the targets file exists in the cache root and the project imports
  //it, so its DPMCopyLocal target (hooked into $(BuildDependsOn)) can invoke 'dpm copylocal'.
  EnsureCopyLocalTargets(config.PackageCacheLocation, FLogger);
  if not projectEditor.EnsureCopyLocalImport(GetDpmExePathForTargets) then
    exit;

  //Design packages are per-IDE/compiler, not per-platform - the call is hoisted out to the
  //post-platform-loop site in InstallPackage / RestoreProject.

  if not projectEditor.AddSearchPaths(platform, packageSearchPaths, config.PackageCacheLocation) then
    exit;

  result := true;
end;

function TPackageInstaller.FinalizePackageConfiguration(const cancellationToken: ICancellationToken; const options: TSearchOptions;
  const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
  const config: IConfiguration; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; out resultGraph: IPackageReference): boolean;
var
  packageSpecs: IDictionary<string, IPackageSpec>;
  packagesToConfigure: IList<IPackageInfo>;
  packageInfo: IPackageInfo;
begin
  result := false;
  resultGraph := nil;

  if resolvedPackages = nil then
  begin
    FLogger.Error('Resolver returned no packages!');
    exit;
  end;

  // Record the resolved package graph so we can detect conflicts between projects
  FContext.RecordGraph(projectFile, projectPackageGraph);

  //Drop synthetic bundled no-ops (e.g. Indy) - there is nothing to download or compile and they
  //contribute no search path (the IDE's library path already provides the bundled library). This
  //single filter covers DownloadPackages, DoConfigurePackageForPlatform and CollectSearchPaths.
  packagesToConfigure := TCollections.CreateList<IPackageInfo>;
  for packageInfo in resolvedPackages do
    if not IsBundledPackageInfo(packageInfo) then
      packagesToConfigure.Add(packageInfo);

  packageSpecs := TCollections.CreateDictionary<string, IPackageSpec>;
  // Downloads the package files to the cache if they are not already there and
  // returns the deserialized dspec as we need it for search paths and design
  if not DownloadPackages(cancellationToken, packagesToConfigure, packageSpecs) then
    exit;

  // Configure the package for this platform (build, search paths, copy local, design packages)
  if not DoConfigurePackageForPlatform(cancellationToken, options, projectFile, projectEditor, platform, config, projectPackageGraph, packagesToConfigure, packageSpecs) then
    exit;

  // Return the graph for caller to handle UpdatePackageReferences and SaveProject
  resultGraph := projectPackageGraph;
  result := true;
end;

function TPackageInstaller.LoadProjectEditor(const projectFile: string; const config: IConfiguration;
  const compilerVersion: TCompilerVersion; out projectEditor: IProjectEditor): boolean;
var
  fullPath: string;
begin
  result := false;
  projectEditor := nil;

  fullPath := projectFile;
  if TPathUtils.IsRelativePath(fullPath) then
  begin
    fullPath := TPath.Combine(GetCurrentDir, fullPath);
    fullPath := TPathUtils.CompressRelativePath(fullPath);
  end;

  projectEditor := TProjectEditor.Create(FLogger, config, compilerVersion);
  if not projectEditor.LoadProject(fullPath) then
  begin
    FLogger.Error('Unable to load project file [' + fullPath + '], cannot continue');
    exit;
  end;

  result := true;
end;

function TPackageInstaller.ResolveProjectFiles(const projectPath: string; const explicitProjects: TArray<string>;
  const projectList: IList<string>): boolean;
var
  groupProjReader: IGroupProjectReader;
  projectRoot: string;
  i: integer;
begin
  result := false;

  // If explicit projects were provided, use them
  if Length(explicitProjects) > 0 then
  begin
    for i := 0 to Length(explicitProjects) - 1 do
    begin
      if FileExists(explicitProjects[i]) then
        projectList.Add(explicitProjects[i])
      else
        FLogger.Warning('Project [' + explicitProjects[i] + '] does not exist', true);
    end;
    if projectList.Count = 0 then
    begin
      FLogger.Error('No valid project files found in the provided list');
      exit;
    end;
    result := true;
    exit;
  end;

  // Handle file path (single .dproj or .groupproj)
  if FileExists(projectPath) then
  begin
    if ExtractFileExt(projectPath) = '.groupproj' then
    begin
      groupProjReader := TGroupProjectReader.Create(FLogger);
      if not groupProjReader.LoadGroupProj(projectPath) then
        exit;
      if not groupProjReader.ExtractProjects(projectList) then
        exit;
      // Projects in a group are likely relative, so make them full paths
      projectRoot := ExtractFilePath(projectPath);
      for i := 0 to projectList.Count - 1 do
      begin
        if TPathUtils.IsRelativePath(projectList[i]) then
          projectList[i] := TPathUtils.CompressRelativePath(projectRoot, projectList[i]);
      end;
    end
    else
      projectList.Add(projectPath);
    result := true;
    exit;
  end;

  // Handle directory path
  if DirectoryExists(projectPath) then
  begin
    projectList.AddRange(TDirectory.GetFiles(projectPath, '*.dproj'));
    if projectList.Count = 0 then
    begin
      FLogger.Error('No dproj files found in projectPath : ' + projectPath);
      exit;
    end;
    FLogger.Information('Found ' + IntToStr(projectList.Count) + ' project file(s).');
    result := true;
    exit;
  end;

  FLogger.Error('The projectPath provided does not exist');
end;

procedure TPackageInstaller.ValidateAndSetCompilerPlatforms(const options: TSearchOptions; const projectEditor: IProjectEditor;
  out effectivePlatforms: TDPMPlatforms; out shouldSkip: boolean);
var
  ambiguousProjectVersion: boolean;
  ambiguousVersions: string;
begin
  shouldSkip := false;
  effectivePlatforms := [];

  ambiguousProjectVersion := IsAmbigousProjectVersion(projectEditor.ProjectVersion, ambiguousVersions) and (not projectEditor.HasDPM);

  if ambiguousProjectVersion and (options.compilerVersion = TCompilerVersion.UnknownVersion) then
    FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] is ambiguous (' + ambiguousVersions + '), recommend specifying compiler version on command line.');

  // If the compiler version was specified (either on the command line or through a package file)
  // then make sure our dproj is actually for that version.
  if options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning('ProjectVersion [' + projectEditor.ProjectVersion + '] does not match the compiler version.');
      projectEditor.compilerVersion := options.compilerVersion;
    end;
  end
  else
    options.compilerVersion := projectEditor.compilerVersion;

  // If the platform was specified (either on the command line or through a package file)
  // then make sure our dproj is actually for that platform.
  if options.platforms <> [] then
  begin
    effectivePlatforms := options.platforms * projectEditor.platforms; // get the intersection
    if effectivePlatforms = [] then
    begin
      FLogger.Warning('Skipping project file [' + projectEditor.ProjectFile + '] as it does not match specified platforms.');
      shouldSkip := true;
    end;
  end
  else
    effectivePlatforms := projectEditor.platforms;
end;

function TPackageInstaller.InstallPackage(const cancellationToken : ICancellationToken; const Options: TInstallOptions; const projectEditor: IProjectEditor;
                                          const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  platformResult: boolean;
  shouldSkip: boolean;
  platformOptions: TInstallOptions;
  erroredPlatforms : TDPMPlatforms;
  finalGraph: IPackageReference;
  platformGraph: IPackageReference;
begin
  result := false;
  finalGraph := nil;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

  result := true;
  erroredPlatforms := [];
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;

    // do not modify the passed in options!
    platformOptions := Options.Clone;
    platformOptions.platforms := [platform];

    FLogger.Information('Installing [' + platformOptions.SearchTerms + '-' +  DPMPlatformToString(platform) + '] into [' + projectEditor.ProjectFile + ']', true);
    platformResult := DoInstallPackageForPlatform(cancellationToken, platformOptions, projectEditor.projectFile, projectEditor, platform, config, context, platformGraph);

    if not platformResult then
    begin
      FLogger.Error('Install failed for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']');
      Include(erroredPlatforms, platform);
    end
    else
    begin
      FLogger.Success('Install succeeded for [' + platformOptions.SearchTerms + '-' + DPMPlatformToString(platform) + ']', true);
      // Capture the graph from successful platform configuration (graph is same for all platforms)
      if finalGraph = nil then
        finalGraph := platformGraph;
    end;

    result := platformResult and result;
    FLogger.Information('');
  end;

  if not result then
    FLogger.Error('Install failed for [' + Options.SearchTerms + '] on platforms [' + DPMPlatformsToString(erroredPlatforms) + ']')
  else if finalGraph <> nil then
  begin
    // P2 §2.6 — pull each cached package's manifest hash off its receipt
    // and lock it onto the PackageReference before we write the dproj.
    PopulateManifestHashes(finalGraph, Options.compilerVersion);
    // Update package references once using the new platform-independent format
    projectEditor.UpdatePackageReferences(finalGraph);
    result := projectEditor.SaveProject();
    //Load design-time packages once after all platforms are installed - design BPLs are per-IDE,
    //not per-platform, so this runs at the project level. CLI context is a no-op.
    if result then
    begin
      //Make sure the IDE host platform's design BPL exists before we try to load it - the install
      //loop above only built the project's target platforms. Non-fatal : the install already
      //succeeded, and a failure here just means the design components may not load.
      if not EnsureDesignHostPlatformCompiled(cancellationToken, Options, finalGraph, platforms) then
        FLogger.Warning('Failed to compile design-time packages for the IDE host platform - design components may not load.');
      InstallDesignPackages(cancellationToken, projectEditor.ProjectFile, BuildPackageSpecsFromGraph(finalGraph, Options.compilerVersion));
    end;
  end;

end;

function TPackageInstaller.GetCompilerVersionFromProjectFiles(const Options: TInstallOptions; const projectFiles: TArray<string>; const config: IConfiguration): boolean;
var
  projectFile: string;
  projectEditor: IProjectEditor;
  compilerVersion: TCompilerVersion;
  bFirst: boolean;
begin
  result := true;
  compilerVersion := TCompilerVersion.UnknownVersion;
  bFirst := true;
  for projectFile in projectFiles do
  begin
    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    result := result and projectEditor.LoadProject(projectFile);
    if result then
    begin
      if not bFirst then
      begin
        if projectEditor.compilerVersion <> compilerVersion then
        begin
          FLogger.Error('Projects are not all the for same compiler version.');
          result := false;
        end;
      end;
      compilerVersion := Options.compilerVersion;
      Options.compilerVersion := projectEditor.compilerVersion;
      bFirst := false;
    end;
  end;
end;

function TPackageInstaller.Init(const options: TSearchOptions): IConfiguration;
begin
  result := nil;
  if (not Options.Validated) and (not Options.Validate(FLogger)) then
    exit
  else if not Options.IsValid then
    exit;
  FConfigurationManager.EnsureDefaultConfig;
  result := FConfigurationManager.LoadConfig(Options.ConfigFile);
  if result = nil then
    exit;

  FPackageCache.Location := result.PackageCacheLocation;

  //also inits the repository manager.
  if not FDependencyResolver.Initialize(result) then
  begin
    FLogger.Error('Unable to initialize the dependency manager.');
    exit(nil);
  end;

  if not FRepositoryManager.HasSources then
  begin
    FLogger.Error
      ('No package sources are defined. Use `dpm sources add` command to add a package source.');
    exit(nil);
  end;


end;

function TPackageInstaller.Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;
var
  config: IConfiguration;
  projectList: IList<string>;
  isGroup: boolean;
begin
  result := false;
  try
    config := Init(Options);
    if config = nil then
      exit;

    projectList := TCollections.CreateList<string>;
    if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
      exit;

    isGroup := ExtractFileExt(Options.ProjectPath) = '.groupproj';

    if (Options.ProjectGroup <> '') and (not isGroup) then
    begin
      //TODO : load the project group and record the package graphs for the projects in the group
      //in the installer context so that package conflicts can be taken into account when installing
      //in a single project from the command line.

    end;

    if Options.PackageFile <> '' then
    begin
      if not FileExists(Options.PackageFile) then
      begin
        // should never happen if validation is called on the options.
        FLogger.Error('The specified packageFile [' + Options.PackageFile + '] does not exist.');
        exit;
      end;
      result := InstallPackageFromFile(cancellationToken, Options, projectList, config, context);
    end
    else
      result := InstallPackageFromId(cancellationToken, Options, projectList, config, context);
  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;

end;

function TPackageInstaller.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
begin
  //Note : we delegate this to the context as this is a no-op in the command line tool, the IDE plugin provides it's own context implementation.
  result := FContext.InstallDesignPackages(cancellationToken, projectFile, packageSpecs);
end;

function TPackageInstaller.BuildPackageSpecsFromGraph(const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;
var
  specs: IDictionary<string, IPackageSpec>;
begin
  specs := TCollections.CreateDictionary<string, IPackageSpec>;
  graph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      identity: IPackageIdentity;
      spec: IPackageSpec;
      key: string;
    begin
      key := LowerCase(node.Id);
      if specs.ContainsKey(key) then
        exit;
      identity := TPackageIdentity.Create('', node.Id, node.Version, compilerVersion);
      spec := FPackageCache.GetPackageSpec(identity);
      if spec <> nil then
        specs[key] := spec;
    end);
  result := specs;
end;

function TPackageInstaller.GetOrLoadPackageInfo(const cancellationToken: ICancellationToken; const id: string; const version: TPackageVersion;
                                                const compilerVersion: TCompilerVersion; const cache: IDictionary<string, IPackageInfo>): IPackageInfo;
var
  identity: IPackageIdentity;
  key: string;
begin
  key := LowerCase(id);
  if cache.TryGetValue(key, result) then
    exit;
  identity := TPackageIdentity.Create('', id, version, compilerVersion);
  result := GetPackageInfo(cancellationToken, identity); //tries cache then repo
  if result <> nil then
    cache[key] := result;
end;

function TPackageInstaller.BuildTopLevelOnlyGraph(const fullGraph: IPackageReference; const compilerVersion: TCompilerVersion): IPackageReference;
var
  child: IPackageReference;
  cloned: IPackageReference;
begin
  result := TPackageReference.CreateRoot(compilerVersion);
  for child in fullGraph.Children do
  begin
    cloned := child.Clone; //clone strips transient sub-children
    result.AddExistingChild(cloned.Id, cloned);
  end;
end;

function TPackageInstaller.CollectGraphVersions(const fullGraph: IPackageReference): IDictionary<string, TPackageVersion>;
var
  collected: IDictionary<string, TPackageVersion>;
begin
  collected := TCollections.CreateDictionary<string, TPackageVersion>;
  if fullGraph <> nil then
  begin
    fullGraph.VisitDFS(
      procedure(const packageReference: IPackageReference)
      var
        key: string;
      begin
        key := LowerCase(packageReference.Id);
        //In a valid graph an id appears once (transients are deduplicated during resolution).
        //If somehow the same id is recorded at different versions, first write wins - we don't
        //try to choose between them here.
        if not collected.ContainsKey(key) then
          collected.Add(key, packageReference.Version);
      end);
  end;
  result := collected;
end;

function TPackageInstaller.RestoreUsingInstallResolver(const cancellationToken: ICancellationToken; const options: TRestoreOptions; const projectFile: string;
                                                       const topLevelGraph: IPackageReference; const preferredVersions: IDictionary<string, TPackageVersion>;
                                                       out resultGraph: IPackageReference; out resolvedPackages: IList<IPackageInfo>): boolean;
var
  topLevelChild: IPackageReference;
  topLevelIdentity: IPackageIdentity;
  topLevelInfo: IPackageInfo;
  accumulatedRefs: IList<IPackageReference>;
  iterationGraph: IPackageReference;
  iterationResolved: IList<IPackageInfo>;
  sharedVersionCache: IDictionary<string, IList<IPackageInfo>>;
begin
  result := false;
  resultGraph := nil;
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  accumulatedRefs := TCollections.CreateList<IPackageReference>;
  //Shared across every ResolveForInstall iteration below so a transitive dep (e.g. Spring4D)
  //referenced by multiple top-levels is only queried from the repository once.
  sharedVersionCache := TCollections.CreateDictionary<string, IList<IPackageInfo>>;

  for topLevelChild in topLevelGraph.Children do
  begin
    if cancellationToken.IsCancelled then
      exit;

    //Load latest IPackageInfo for this top-level using its dproj-recorded version. The cache lookup
    //gives us deps populated from the manifest. If the user edited the top-level version to something
    //bogus, this returns nil and the restore can't proceed.
    topLevelIdentity := TPackageIdentity.Create('', topLevelChild.Id, topLevelChild.Version, options.CompilerVersion);
    topLevelInfo := GetPackageInfo(cancellationToken, topLevelIdentity);
    if topLevelInfo = nil then
    begin
      FLogger.Error('Restore: top-level package [' + topLevelChild.Id + '-' + topLevelChild.Version.ToStringNoMeta + '] not found in cache or repo');
      exit;
    end;

    if not FDependencyResolver.ResolveForInstall(cancellationToken, options.CompilerVersion, projectFile, options,
                                                 topLevelInfo, accumulatedRefs, iterationGraph, iterationResolved, sharedVersionCache, preferredVersions) then
    begin
      FLogger.Error('Restore: re-resolution failed for [' + topLevelChild.Id + ']');
      exit;
    end;

    //Carry the resolver's output forward as projectReferences for the next top-level so shared
    //transients are not re-resolved (and so prior top-levels stay in the final graph).
    resultGraph := iterationGraph;
    resolvedPackages := iterationResolved;
    accumulatedRefs := TCollections.CreateList<IPackageReference>;
    if not CreateProjectRefs(cancellationToken, iterationGraph, accumulatedRefs) then
      exit;
  end;

  result := resultGraph <> nil;
end;

function TPackageInstaller.TryValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                                   const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): boolean;
var
  packageInfos: IDictionary<string, IPackageInfo>;
  validationOk: boolean;
  topLevelChild: IPackageReference;

  //Recursively validates a node and its declared dependencies.
  //Order matters: we always check the *parent's* declared range against the graph child's recorded
  //version BEFORE attempting to load the child's own info. That way a stale/out-of-range transient
  //is reported as "out of range" rather than "not found in cache or repo".
  procedure ValidateNode(const node: IPackageReference);
  var
    info: IPackageInfo;
    dep: IPackageDependency;
    child: IPackageReference;
  begin
    if not validationOk then
      exit;
    if cancellationToken.IsCancelled then
    begin
      validationOk := false;
      exit;
    end;
    info := GetOrLoadPackageInfo(cancellationToken, node.Id, node.Version, compilerVersion, packageInfos);
    if info = nil then
    begin
      FLogger.Information('Restore: package [' + node.Id + '-' + node.Version.ToStringNoMeta + '] not found in cache or repo - re-resolving');
      validationOk := false;
      exit;
    end;
    //For every dependency this package declares, verify the graph has a child for it whose version
    //satisfies the declared range. Range check happens BEFORE recursing so we don't try to load info
    //for a stale child id/version pair that isn't in the cache or repo.
    for dep in info.Dependencies do
    begin
      //bundled deps (e.g. Indy) are satisfied by the IDE and deliberately not written to the
      //graph - their absence is expected, not a stale/missing transient. If the user added a real
      //package for the id it appears as its own top-level node and is validated independently.
      if dep.VersionRange.IsBundledSentinel then
        continue;
      child := node.FindFirstChild(dep.Id);
      if child = nil then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] requires [' + dep.Id + '] which is missing from the project graph - re-resolving');
        validationOk := false;
        exit;
      end;
      if not dep.VersionRange.IsSatisfiedBy(child.Version) then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] is outside required range [' + dep.VersionRange.ToString + '] - re-resolving');
        validationOk := false;
        exit;
      end;
      //Only recurse if the child passed the range check.
      ValidateNode(child);
      if not validationOk then
        exit;
    end;
  end;

begin
  result := false;
  validationOk := true;
  packageInfos := TCollections.CreateDictionary<string, IPackageInfo>;

  for topLevelChild in projectPackageGraph.Children do
  begin
    ValidateNode(topLevelChild);
    if not validationOk then
      exit;
  end;

  //DFS yields children-before-parents - that is the order resolvedPackages needs (deps before dependers).
  projectPackageGraph.VisitDFS(
    procedure(const node: IPackageReference)
    var
      info: IPackageInfo;
      key: string;
      i: integer;
      alreadyAdded: boolean;
    begin
      key := LowerCase(node.Id);
      if not packageInfos.TryGetValue(key, info) then
        exit;
      //skip duplicates - shared transients can appear under multiple parents
      alreadyAdded := false;
      for i := 0 to resolvedPackages.Count - 1 do
      begin
        if SameText(resolvedPackages[i].Id, info.Id) then
        begin
          alreadyAdded := true;
          break;
        end;
      end;
      if not alreadyAdded then
        resolvedPackages.Add(info);
    end);

  result := true;
end;

function TPackageInstaller.InstallPackageFromFile(const cancellationToken : ICancellationToken; const Options: TInstallOptions;const projectFiles: IList<string>;
                                                  const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  packageIdString: string;
  packageIdentity: IPackageIdentity;
  packageSpec: IPackageSpec;
begin
  // get the package into the cache first then just install as normal
  result := FPackageCache.InstallPackageFromFile(Options.PackageFile);
  if not result then
    exit;

  // get the identity so we can get the compiler version
  packageIdString := ExtractFileName(Options.PackageFile);
  packageIdString := ChangeFileExt(packageIdString, '');
  if not TPackageIdentity.TryCreateFromString(FLogger, packageIdString, '', packageIdentity) then
    exit;

  // update options so we can install from the packageid.
  Options.PackageFile := '';
  Options.packageId := packageIdentity.Id;
  Options.Version := packageIdentity.Version;
  Options.compilerVersion := packageIdentity.compilerVersion;

  //Restrict to the platforms the package actually supports - read from the now-cached spec.
  //ValidateAndSetCompilerPlatforms downstream intersects this with the project's enabled platforms.
  packageSpec := FPackageCache.GetPackageSpec(packageIdentity);
  if (packageSpec <> nil) and (packageSpec.TargetPlatform <> nil) then
    Options.platforms := packageSpec.TargetPlatform.Platforms
  else
  begin
    FLogger.Warning('Could not read spec for [' + packageIdentity.ToString + '] - install will run for all project platforms');
    Options.platforms := [];
  end;

  result := InstallPackageFromId(cancellationToken, options, projectFiles, config, context);
end;

function TPackageInstaller.InstallPackageFromId(const cancellationToken  : ICancellationToken; const Options: TInstallOptions; const projectFiles: IList<string>;
                                                const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  i: integer;
  projectEditor : IProjectEditor;
  projectEditors : IList<IProjectEditor>;
  existingPackageRef: IPackageReference;
  projectPackageGraph: IPackageReference;
  dependency : IPackageReference;
begin
  result := true;

  projectEditors := TCollections.CreateList<IProjectEditor>;

  for i := 0 to projectFiles.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;
    projectFile := projectFiles[i];
    if TPathUtils.IsRelativePath(projectFile) then
    begin
      projectFile := TPath.Combine(GetCurrentDir, projectFile);
      projectFile := TPathUtils.CompressRelativePath(projectFile);
    end;

    projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);
    if not projectEditor.LoadProject(projectFile) then
    begin
      FLogger.Error('Unable to load project file, cannot continue');
      exit;
    end;
    //Per-project platform reconciliation happens later in ValidateAndSetCompilerPlatforms (called from
    //InstallPackage). Mutating Options.platforms here would leak the first project's platforms into the
    //second project's call and narrow installs across the directory.
    projectEditors.Add(projectEditor);
  end;

  if cancellationToken.IsCancelled then
    exit;


  //if we are upgrading, remove package references and clean up the resolutions.
  for i := 0 to projectEditors.Count -1 do
  begin
    projectEditor := projectEditors[i];

   // CONTINUE WORKING HERE!!!!  Need to change to justr single package reference per package no platforms.

    projectPackageGraph := projectEditor.GetPackageReferences; // can return nil

    if projectPackageGraph = nil then
      projectPackageGraph := TPackageReference.CreateRoot(Options.compilerVersion);

    //Install is idempotent - if the package is already in the graph, remove it so the resolver
    //re-resolves below. Repeats for an already-built platform are short-circuited by the
    //per-platform BOM check; new platforms get compiled fresh.
    existingPackageRef := projectPackageGraph.FindTopLevelChild(Options.packageId);
    if existingPackageRef <> nil then
    begin
      projectPackageGraph.RemoveTopLevelChild(existingPackageRef.Id);
      //No call to RemoveResolution here - FindPackageResolution skips the current project, so re-resolving
      //in this project doesn't see its own stale resolution. Other projects' recorded resolutions reflect
      //their dprojs and shouldn't be retroactively mutated by an install in a sibling project.
      existingPackageRef := nil;
    end;

    // We could have a transitive dependency that is being promoted.
    // Since we want to control what version is installed, we will remove
    // any transitive references to that package so the newly installed version
    // will take precedence when resolving.
    dependency := projectPackageGraph.FindFirstChild(Options.packageId);
    while dependency <> nil do
    begin
      projectPackageGraph.RemoveChild(dependency);
      dependency := projectPackageGraph.FindFirstChild(Options.packageId);
    end;
  end;

  for i := 0 to projectEditors.Count -1 do
  begin
    //TODO : Change to pass editor in rather than projectfile.
    result := InstallPackage(cancellationToken, Options, projectEditors[i], config, context) and result;
    projectEditors[i] := nil;
  end;





end;

function TPackageInstaller.Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;
begin
  result := false;
end;

function TPackageInstaller.Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
  stopwatch: TStopwatch;
begin
  result := false;
  stopwatch := TStopwatch.Create;
  stopwatch.Start;
  try
    config := Init(Options);
    if config = nil then
      exit;

    projectList := TCollections.CreateList<string>;
    if not ResolveProjectFiles(Options.ProjectPath, nil, projectList) then
      exit;

    result := true;
    for i := 0 to projectList.Count - 1 do
    begin
      if cancellationToken.IsCancelled then
        exit;
      projectFile := projectList[i];

      if TPathUtils.IsRelativePath(projectFile) then
      begin
        projectFile := TPath.Combine(GetCurrentDir, projectFile);
        projectFile := TPathUtils.CompressRelativePath(projectFile);
      end;
      // order is important so we avoid shortcut boolean eval
      result := RestoreProject(cancellationToken, Options, projectFile, config, context) and result;
    end;
    stopwatch.Stop;
    if result then
      FLogger.Success('Restore succeeded in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms', true)
    else
      FLogger.Error('Restore failed in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms');


  except
    on e: Exception do
    begin
      FLogger.Error(e.Message);
      result := false;
    end;
  end;
end;

function TPackageInstaller.RestoreProject(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                                          const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  errorPlatforms : TDPMPlatforms;
  platformResult: boolean;
  shouldSkip: boolean;
  platformOptions: TRestoreOptions;
  finalGraph: IPackageReference;
  platformGraph: IPackageReference;
  storedCompiler : TCompilerVersion;
  mismatchedPackageIds : IList<string>;
  packageSpecs : IDictionary<string, IPackageSpec>;
  mismatchedId : string;
begin
  result := false;
  finalGraph := nil;

  // Load and validate the project file
  if not LoadProjectEditor(projectFile, config, Options.compilerVersion, projectEditor) then
    exit;

  if cancellationToken.IsCancelled then
    exit;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

  FLogger.Information('Restoring for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  result := true;
  errorPlatforms := [];
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    // do not modify options here!
    platformOptions := Options.Clone;
    platformOptions.platforms := [platform];
    FLogger.Information('Restoring project [' + projectFile + '] for [' + DPMPlatformToString(platform) + ']', true);
    platformResult := DoRestoreProjectForPlatform(cancellationToken, platformOptions, projectFile, projectEditor, platform, config, context, platformGraph);
    if not platformResult then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Restore Cancelled.')
      else
      begin
        FLogger.Error('Restore failed for ' + DPMPlatformToString(platform));
        Include(errorPlatforms, platform);
      end;
    end
    else
    begin
      FLogger.Success('Restore succeeded for ' + DPMPlatformToString(platform), true);
      // Capture the graph from successful platform configuration (graph is same for all platforms)
      if finalGraph = nil then
        finalGraph := platformGraph;
    end;
    result := platformResult and result;
    FLogger.Information('');
  end;

  if not result then
    FLogger.Error('Restore failed for [' + projectFile + '] on platforms [' + DPMPlatformsToString(errorPlatforms) + ']')
  else if finalGraph <> nil then
  begin
    // The manifest hash lock is per-compiler. When a project is opened in a newer IDE (in-place
    // dproj upgrade), the locks recorded under the old compiler cannot match the new compiler's
    // packages - that is a legitimate upgrade, not tampering. Detect it via the <DPMCompiler>
    // marker captured at load and clear the stale locks so they are simply re-established below
    // rather than failing validation.
    storedCompiler := projectEditor.DPMCompilerVersion;
    if (storedCompiler <> TCompilerVersion.UnknownVersion) and (storedCompiler <> Options.compilerVersion) then
    begin
      FLogger.Information(Format(
        'Project was last managed by DPM under [%s]; restoring under [%s] - ' +
        'clearing stale manifest hash locks and re-locking for the new compiler.',
        [CompilerToString(storedCompiler), CompilerToString(Options.compilerVersion)]), true);
      ClearManifestHashes(finalGraph);
    end;
    // P2 §2.6 — enforce existing PackageReference.ManifestHash lock values before we
    // overwrite them, then refresh from the receipts. Merged into a single pass so each
    // receipt is read only once.
    //
    // A lock mismatch is no longer a hard abort. Previously it failed the whole restore before
    // any design-time BPLs were loaded, so a single mismatched package stopped every other
    // package's design components from installing in the IDE. Now we report the problem (with
    // resolution guidance), leave the dproj untouched (so the recorded locks are preserved and
    // keep warning until the user resolves it), and still load the design-time BPLs for the
    // packages that DID validate. The mismatched packages are skipped - we will not load BPLs
    // whose cached content no longer matches what the project pinned. The restore result is
    // still marked failed so the CLI exits non-zero and the IDE flags the problem.
    mismatchedPackageIds := TCollections.CreateList<string>;
    if ValidateAndPopulateManifestHashes(finalGraph, Options.compilerVersion, Options.IgnoreHashLocks, mismatchedPackageIds) then
    begin
      // Update package references once using the new platform-independent format
      projectEditor.UpdatePackageReferences(finalGraph);
      result := projectEditor.SaveProject();
    end
    else
    begin
      FLogger.Warning('One or more manifest hash locks did not match the cached packages (see the errors above). ' +
        'The project file was left unchanged and the affected package(s) were skipped. To resolve: reinstall ' +
        'the affected package(s), delete them from the cache (' + FPackageCache.Location + ') and restore, or ' +
        'restore with --ignoreHashLocks (-ihl) to refresh the recorded locks from the cache.');
      result := false;
    end;
    //Load design-time packages once after all platforms are restored - design BPLs are per-IDE,
    //not per-platform, so this runs at the project level. CLI context is a no-op. This runs even
    //when a hash lock failed to validate (result is false) so one mismatched package does not block
    //the rest - only SaveProject failures (disk errors) suppress it.
    if result or (mismatchedPackageIds.Count > 0) then
    begin
      //Make sure the IDE host platform's design BPL exists before we try to load it - the restore
      //loop above only built the project's target platforms. Non-fatal : the restore already
      //succeeded, and a failure here just means the design components may not load.
      if not EnsureDesignHostPlatformCompiled(cancellationToken, Options, finalGraph, platforms) then
        FLogger.Warning('Failed to compile design-time packages for the IDE host platform - design components may not load.');
      packageSpecs := BuildPackageSpecsFromGraph(finalGraph, Options.compilerVersion);
      //Skip the design-time BPLs of any package whose lock did not validate - their cached content
      //no longer matches what the project pinned, so loading them could load unexpected code.
      for mismatchedId in mismatchedPackageIds do
        packageSpecs.Remove(LowerCase(mismatchedId));
      InstallDesignPackages(cancellationToken, projectFile, packageSpecs);
    end;
  end;

end;

function TPackageInstaller.Uninstall(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
begin
  result := false;
  // commandline would have validated already, but IDE probably not.
  if (not Options.Validated) and (not Options.Validate(FLogger)) then
    exit
  else if not Options.IsValid then
    exit;

  config := FConfigurationManager.LoadConfig(Options.ConfigFile);
  if config = nil then // no need to log, config manager will
    exit;

  FDependencyResolver.Initialize(config);

  FPackageCache.Location := config.PackageCacheLocation;
  if not FRepositoryManager.Initialize(config) then
  begin
    FLogger.Error('Unable to initialize the repository manager.');
    exit;
  end;

  projectList := TCollections.CreateList<string>;
  if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
    exit;

  result := true;
  for i := 0 to projectList.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;
    projectFile := projectList[i];
    result := UnInstallFromProject(cancellationToken, Options, projectFile, config, context) and result;
  end;

end;

function TPackageInstaller.UnInstallFromProject(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const projectFile: string;
                                                const config: IConfiguration; const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  shouldSkip: boolean;
  projectPackageGraph: IPackageReference;
  foundReference: IPackageReference;
  orphanedIds: IList<string>;
  orphanedId: string;
begin
  result := false;

  // make sure we can parse the dproj
  projectEditor := TProjectEditor.Create(FLogger, config, Options.compilerVersion);

  if not projectEditor.LoadProject(projectFile) then
  begin
    FLogger.Error('Unable to load project file, cannot continue');
    exit;
  end;

  if cancellationToken.IsCancelled then
    exit;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  if shouldSkip then
    exit;

  FLogger.Information('Uninstalling for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  //The package graph is project-level, not per-platform. Find the top-level reference once and bail
  //if it isn't there - no work needed.
  projectPackageGraph := projectEditor.GetPackageReferences;
  if projectPackageGraph = nil then
  begin
    FLogger.Information('Package [' + Options.packageId + '] is not referenced in project [' + projectFile + '] - nothing to do.');
    exit(true);
  end;

  foundReference := projectPackageGraph.FindTopLevelChild(Options.packageId);
  if foundReference = nil then
  begin
    FLogger.Information('Package [' + Options.packageId + '] is not referenced in project [' + projectFile + '] - nothing to do.');
    exit(true);
  end;

  //Remove the top-level node first so HasAnyChild reflects the post-uninstall graph below.
  projectPackageGraph.RemoveTopLevelChild(foundReference.Id);

  //Walk the removed subtree and collect ids whose subtree is no longer referenced anywhere in the
  //remaining graph - those are the ones we need to drop from per-platform search paths. Transients
  //still pulled in by another top-level stay.
  orphanedIds := TCollections.CreateList<string>;
  foundReference.VisitDFS(
    procedure(const node: IPackageReference)
    begin
      if not projectPackageGraph.HasAnyChild(node.Id) then
        orphanedIds.Add(node.Id);
    end);

  //Search paths ARE per-platform (each <DPMSearch> has a Condition on $(Platform)) so this part
  //runs once per platform.
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    FLogger.Information('Removing [' + Options.packageId + '] from search paths for [' + DPMPlatformToString(platform) + ']');
    for orphanedId in orphanedIds do
      projectEditor.RemoveFromSearchPath(platform, orphanedId);
  end;

  //Tell the context to unload design BPLs whose dpm package is no longer referenced by this project.
  if not context.UninstallDesignPackages(cancellationToken, projectFile, orphanedIds) then
    FLogger.Warning('Design-package unload reported failure - continuing.');

  //TODO : Tell context to upgrade graph (FContext.PackageGraphTrimmed once that exists).

  //Project-level: write the updated graph (single <Packages> node, no platform attribute) and save once.
  projectEditor.UpdatePackageReferences(projectPackageGraph);
  result := projectEditor.SaveProject();
  if result then
    FLogger.Success('Uninstall succeeded for [' + Options.packageId + ']', true);
end;

end.
