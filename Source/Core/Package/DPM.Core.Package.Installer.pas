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
  DPM.Core.Project.ConfigPatcher,
  DPM.Core.Compiler.Interfaces;

type
  //what the restore fast path found in a project's recorded graph : good as it is, needs re-resolving, or
  //can't be restored at all.
  TRestoreGraphState = (rgsValid, rgsNeedsResolve, rgsInvalid);

  ///<summary> One project's part in an install : what to apply to it, and what each phase produced.
  ///  Internal to TPackageInstaller. </summary>
  TProjectInstallWork = class
  private
    FProjectFile : string;
    FEditor : IProjectEditor;
    FOptions : TInstallOptions;
    FPlatforms : TDPMPlatforms;
    FPackages : IList<IPackageInfo>;
    FGraph : IPackageReference;
    FResolved : IList<IPackageInfo>;
  public
    constructor Create(const projectFile : string; const editor : IProjectEditor; const options : TInstallOptions; const platforms : TDPMPlatforms);
    destructor Destroy; override;
    property ProjectFile : string read FProjectFile;
    property Editor : IProjectEditor read FEditor;
    //a clone owned by the work item, set to the operation's compiler version.
    property Options : TInstallOptions read FOptions;
    property Platforms : TDPMPlatforms read FPlatforms;
    //the packages to apply to the project.
    property Packages : IList<IPackageInfo> read FPackages write FPackages;
    //what resolving the project produced.
    property Graph : IPackageReference read FGraph write FGraph;
    property Resolved : IList<IPackageInfo> read FResolved write FResolved;
  end;

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
    FProjectConfigPatcher : IProjectConfigPatcher;
    FInstallPlanner : IInstallPlanner;
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
    function CreateProjectRefs(const cancellationToken: ICancellationToken; const rootnode: IPackageReference; const projectReferences: IList<IPackageReference>): boolean; overload;
    //excludeIds (lowercase, may be nil) : packages to leave out, with anything only reachable through them.
    function CreateProjectRefs(const cancellationToken: ICancellationToken; const rootnode: IPackageReference; const excludeIds : ISet<string>; const projectReferences: IList<IPackageReference>): boolean; overload;

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
    //If every transient is present and within its parent's version range, the graph is good (rgsValid) and
    //we can skip resolution - resolvedPackages is then populated in topological order (deps before dependers).
    //A stale or missing transient gives rgsNeedsResolve - caller should fall back to a full re-resolve.
    //A top level reference wins over the range a package declares for it : newer on the same major is
    //accepted with a warning, anything else gives rgsInvalid and the restore fails.
    function ValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                  const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): TRestoreGraphState;

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

    // Records the graph and downloads the resolved packages (bundled no-ops left out) - the platform
    // independent half of FinalizePackageConfiguration.
    function PrepareProjectPackages(const cancellationToken: ICancellationToken; const projectFile: string; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; out packagesToConfigure: IList<IPackageInfo>; out packageSpecs: IDictionary<string, IPackageSpec>): boolean;

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

    // user specified a package file - puts it in the cache and points the options at the package it holds.
    function PreparePackageFileInstall(const Options: TInstallOptions): boolean;

    // Install = plan, then apply : see PlanInstall/ApplyInstallPlan.
    function DoPlanInstall(const cancellationToken: ICancellationToken; const options: TInstallOptions; out plan: IInstallPlan): boolean;
    function DoApplyInstallPlan(const cancellationToken: ICancellationToken; const options: TInstallOptions; const plan: IInstallPlan): boolean;

    // Loads the projects, settling the compiler version and each project's platforms. A project whose
    // platforms don't match the options is left out (not a failure); one that won't load is a failure.
    function LoadInstallWork(const cancellationToken: ICancellationToken; const options: TInstallOptions; const projectFiles: IList<string>;
                             const config: IConfiguration; const works: IList<TProjectInstallWork>; out compilerVersion: TCompilerVersion): boolean;
    // The requested packages at concrete versions - looked up once for the whole install.
    function LookupRequestedPackages(const cancellationToken: ICancellationToken; const options: TInstallOptions; const compilerVersion: TCompilerVersion;
                                     const requested: IList<IPackageInfo>): boolean;
    // Phase 1 : resolve a project with the packages being installed. Changes nothing.
    function ResolveProjectInstall(const cancellationToken: ICancellationToken; const work: TProjectInstallWork): boolean;
    // Phase 2 : download, compile and configure every platform, updating the dproj in memory only.
    function BuildProjectInstall(const cancellationToken: ICancellationToken; const config: IConfiguration; const work: TProjectInstallWork): boolean;
    // Phase 3 : save the dproj, commit its recorded resolutions, load design packages.
    function CommitProjectInstall(const cancellationToken: ICancellationToken; const work: TProjectInstallWork): boolean;
    // After a plain install failed to resolve : if upgrading other installed packages along with it would
    // have worked, say which.
    procedure HintCompanions(const cancellationToken: ICancellationToken; const config: IConfiguration; const options: TInstallOptions;
                             const plan: IInstallPlan; const works: IList<TProjectInstallWork>);
    procedure LogPlanErrors(const plan: IInstallPlan);
    procedure LogPlanSummary(const plan: IInstallPlan);

    function UnInstallFromProject(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const projectFile: string; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function RestoreProject(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    // plans the install, then applies the plan.
    function Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;
    function PlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan) : boolean;
    function ApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan; const context : IPackageInstallerContext) : boolean;


    function Uninstall(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
    // calls restore project
    function Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;

    function Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;

    function Cache(const cancellationToken: ICancellationToken; const Options: TCacheOptions): boolean;

    function ProjectHasPackageReferences(const projectFile : string; const compilerVersion : TCompilerVersion) : boolean;

    function RefreshProjectSearchPaths(const cancellationToken : ICancellationToken; const projectFile : string;
                                       const compilerVersion : TCompilerVersion; const configFile : string;
                                       const context : IPackageInstallerContext) : boolean;

  public
    constructor Create(const logger: ILogger;
      const configurationManager: IConfigurationManager;
      const repositoryManager: IPackageRepositoryManager;
      const packageCache: IPackageCache;
      const dependencyResolver: IDependencyResolver;
      const context: IPackageInstallerContext;
      const compilerFactory: ICompilerFactory;
      const receiptService : IReceiptService;
      const packageGenerator : IPackageProjectGenerator;
      const projectConfigPatcher : IProjectConfigPatcher;
      const installPlanner : IInstallPlanner);
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
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Project.BuildHookValidator,
  DPM.Core.Project.CopyLocalTargets,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Options.List,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Classes,
  DPM.Core.Package.InstallPlan,
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
    result := projectEditor.HasPackages
  else if not FileExists(fullPath) then
    //Gone between the check above and the load - a transient the IDE creates and removes while
    //creating a new project, not a real project. Nothing to restore, and letting the default-true
    //stand here is what used to pop the log window with a lone "does not exist" error.
    result := false;
end;

function TPackageInstaller.RefreshProjectSearchPaths(const cancellationToken : ICancellationToken; const projectFile : string;
                                                     const compilerVersion : TCompilerVersion; const configFile : string;
                                                     const context : IPackageInstallerContext) : boolean;
var
  config : IConfiguration;
  projectEditor : IProjectEditor;
  graph : IPackageReference;
  resolvedPackages : IList<IPackageInfo>;
  infoCache : IDictionary<string, IPackageInfo>;
  platforms : TDPMPlatforms;
  platform : TDPMPlatform;
  searchPaths : IList<string>;
begin
  result := false;

  //Minimal config load - just enough to know the cache location and load the project. We do NOT
  //initialize the resolver/repository (no resolve or download) - every package is already cached.
  FConfigurationManager.EnsureDefaultConfig;
  config := FConfigurationManager.LoadConfig(configFile);
  if config = nil then
    exit;
  FPackageCache.Location := config.PackageCacheLocation;

  if not LoadProjectEditor(projectFile, config, compilerVersion, projectEditor) then
    exit;

  graph := projectEditor.GetPackageReferences;
  if graph = nil then
    exit(true); //no packages installed in this project - nothing to do

  //Apply the current-session 'use source' choices onto the graph. Source flows down to a package's
  //dependencies via TPackageReference.GetUseSource (which ORs the parent's value).
  graph.VisitDFS(
    procedure(const node : IPackageReference)
    begin
      if context.GetUseSource(projectFile, node.Id) then
        node.UseSource := true;
    end);

  //Flatten the graph to the installed packages, reading each IPackageInfo from the cache (no
  //compile, no download). Same pattern as EnsureDesignHostPlatformCompiled.
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
        info := GetOrLoadPackageInfo(cancellationToken, node.Id, node.Version, compilerVersion, infoCache)
      else
        infoCache[key] := info;
      //bundled no-ops (e.g. Indy) contribute no search path.
      if (info <> nil) and (not IsBundledPackageInfo(info)) then
        resolvedPackages.Add(info);
    end);

  //All installed packages are already built in the cache, so pass them all as 'compiled' - the lib
  //branch in CollectSearchPaths still guards each lib\{platform} path with DirectoryExists, and
  //UseSource packages fall through to the source branch regardless.
  platforms := projectEditor.platforms;
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    searchPaths := TCollections.CreateList<string>;
    if not CollectSearchPaths(graph, resolvedPackages, resolvedPackages, compilerVersion, platform, searchPaths) then
      exit(false);
    if not projectEditor.AddSearchPaths(platform, searchPaths, config.PackageCacheLocation) then
      exit(false);
  end;

  //Write the dproj (search paths only - the package reference graph is unchanged and useSource is
  //never persisted). No design-time package load, no build.
  result := projectEditor.SaveProject;
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
  pkgAbsRoot: string;

  procedure AddUnique(const path: string);
  begin
    if not seenPaths.ContainsKey(LowerCase(path)) then
    begin
      seenPaths[LowerCase(path)] := true;
      searchPaths.Add(path);
    end;
  end;

  //True when dir directly contains a file the compiler/IDE needs on the search path:
  //source (.pas/.inc), form/frame definitions (.dfm/.fmx) or compiled resources (.res).
  function DirHasSourceFiles(const dir: string): boolean;
  const
    cSourceMasks: array[0..4] of string = ('*.pas', '*.inc', '*.dfm', '*.fmx', '*.res');
  var
    mask: string;
  begin
    result := false;
    for mask in cSourceMasks do
      if Length(TDirectory.GetFiles(dir, mask)) > 0 then
        exit(true);
  end;

  //Add every subfolder under absBase that contains .pas/.inc/.dfm/.fmx/.res files. relSub is
  //computed relative to pkgAbs so the search path matches packageBasePath layout. Skips .git
  //metadata. Delphi search paths are not recursive, so each folder must be listed individually.
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
      if not DirHasSourceFiles(subDir) then
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

  // IPackageInfo instances are memoized in the package cache (a process singleton), so UseSource
  // can carry over from a previous install/restore/refresh. Reset it here before we OR the current
  // graph's values on - otherwise a package that used source last time can never revert to lib
  // (the OR below would keep seeing the stale true), which is exactly what breaks un-ticking
  // 'use source' in the IDE.
  for packageInfo in resolvedPackages do
    packageInfo.UseSource := false;

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
      if template.SourceEntries.Count > 0 then
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
      end
      else if not isGitSource then
      begin
        //Packed packages have their source entries stripped from the manifest (see the packaging
        //writer), but the source files were still extracted into the cache. Add every subfolder that
        //contains source/form/resource files (.pas/.inc/.dfm/.fmx/.res) so a UseSource (or source-only)
        //package still contributes its source to the search path. The lib\ and bpl\ folders hold
        //dcu/dcp/bpl and none of those, so they are skipped.
        pkgAbsRoot := IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(packageInfo));
        AddPasSubdirs(pkgAbsRoot, ExcludeTrailingPathDelimiter(pkgAbsRoot), packageBasePath);
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
  seenDependencies: IDictionary<string, boolean>;
  bomNode: IPackageReference;
  supportedByCompiler: TDPMPlatforms;
  candidates: TDPMPlatforms;
  designPlatforms: TDPMPlatforms;
  designProjectEditor: IProjectEditor;
  effectivePlatform: TDPMPlatform;
  designPlatform: TDPMPlatform;
  prebuiltError: string;
  //design entry project path (lowercased) -> the platforms it declared BEFORE any dproj patching.
  designCandidates: IDictionary<string, TDPMPlatforms>;

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

  //Resolve an entry's additional searchPaths (authored relative to the package root, same convention
  //as `project`) against the extracted package folder, and combine them with the dependency lib
  //folders in `searchPaths`. A path that resolves outside the package folder is warned about and
  //skipped - it never reaches msbuild. An entry with no searchPaths just yields the dependency folders.
  function ResolveEntrySearchPaths(const entrySearchPaths : IList<string>) : IList<string>;
  var
    entryPath : string;
    resolvedPath : string;
    packageRoot : string;
  begin
    result := TCollections.CreateList<string>;
    result.AddRange(searchPaths);
    if (entrySearchPaths = nil) or (entrySearchPaths.Count = 0) then
      exit;
    packageRoot := IncludeTrailingPathDelimiter(packagePath);
    for entryPath in entrySearchPaths do
    begin
      resolvedPath := IncludeTrailingPathDelimiter(ResolveProjectFile(entryPath));
      //Trailing delimiter on both sides keeps the check from matching a sibling folder that merely
      //shares the package folder's name as a prefix, and allows the package root itself.
      if not SameText(Copy(resolvedPath, 1, Length(packageRoot)), packageRoot) then
      begin
        FLogger.Warning('Ignoring search path [' + entryPath + '] for package [' + packageInfo.Id + '] - it resolves outside the package folder.');
        continue;
      end;
      result.Add(resolvedPath);
    end;
  end;

  //Before handing a project to msbuild with /p:Config=<configuration> /p:Platform=<effectivePlatform>,
  //make sure the dproj actually declares that pair. A dspec can legitimately target platforms the
  //package's own dproj was never configured for, and msbuild applies NONE of the project's settings
  //when the config activator group is missing. The patch is written in place in the cache folder and
  //is idempotent - a project that already declares the pair is not rewritten at all.
  //Failure here is never fatal : we warn and let msbuild try anyway, so this can only ever fix
  //packages, never break one that builds today.
  procedure EnsureBuildTarget(const projectFile : string; const entryProject : string; const options : TProjectPatchOptions);
  var
    packageRoot : string;
    patchResult : TProjectPatchResult;
  begin
    if FProjectConfigPatcher = nil then
      exit;
    //Never mutate a file outside the package's own cache folder - `project` is author supplied and
    //ResolveProjectFile compresses '..' segments without a containment check. Same guard
    //ResolveEntrySearchPaths applies to search paths.
    packageRoot := IncludeTrailingPathDelimiter(packagePath);
    if not SameText(Copy(projectFile, 1, Length(packageRoot)), packageRoot) then
    begin
      FLogger.Warning('Not adjusting project [' + entryProject + '] for package [' + packageInfo.Id +
                      '] - it resolves outside the package folder.');
      exit;
    end;
    patchResult := FProjectConfigPatcher.EnsureBuildTarget(projectFile, effectivePlatform, configuration,
                                                          Compiler.compilerVersion, options);
    if patchResult = TProjectPatchResult.Failed then
      FLogger.Warning('Unable to add [' + configuration + '] / ' + DPMPlatformToString(effectivePlatform) +
                      ' to project [' + entryProject + '] - attempting the build anyway.');
  end;

  //DPM compiles a package's projects, it does not run them. TMSBuildCompiler passes the IDE level
  //hooks (PreBuildEvent, CustomToolCommand ...) empty on the msbuild command line so they cannot
  //fire, but a dproj is an msbuild file : a <Target> hung off Build with AfterTargets, a
  //<UsingTask>, or an <Import> of a targets file shipped in the package will still execute, and no
  //command line switch prevents that. So the project is inspected before msbuild ever sees it.
  //See DPM.Core.Project.BuildHookValidator. `dpm pack` rejects these outright, but nothing forces
  //a package to have been produced by `dpm pack` - this is the check that actually holds.
  function ProjectIsSafeToBuild(const projectFile : string; const entryProject : string) : boolean;
  var
    blocking : IList<string>;
    advisory : IList<string>;
    finding : string;
  begin
    blocking := TCollections.CreateList<string>;
    advisory := TCollections.CreateList<string>;
    if not TBuildHookValidator.TryScanFile(projectFile, blocking, advisory) then
    begin
      //Fail closed : if the project cannot be read we cannot say what it would do, and msbuild
      //is not going to make a better job of a file that would not parse at all.
      FLogger.Error('Package [' + packageInfo.Id + '] project [' + entryProject +
                    '] could not be inspected - refusing to build it.');
      exit(false);
    end;

    //Advisory findings are already inert - the author just expected a step that will never run.
    for finding in advisory do
      FLogger.Warning('Package [' + packageInfo.Id + '] : ' + finding +
                      ' - DPM blocks build events, so that step will not run.');

    result := not blocking.Any;
    if not result then
    begin
      FLogger.Error('Package [' + packageInfo.Id + '] project [' + entryProject +
                    '] would run commands during the build : ' + TStringUtils.Join('; ', blocking.ToArray));
      FLogger.Error('DPM compiles package projects, it does not execute them - refusing to build this package.');
    end;
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
      FLogger.Verbose('Package [' + packageInfo.Id + '] [' + DPMPlatformToString(effectivePlatform) + '] - dependencies unchanged, skipping compilation.');
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

  // Collect the base library paths from dependencies. These are combined per build/design entry with
  // that entry's own searchPaths (see ResolveEntrySearchPaths) and set on the compiler just before
  // each BuildProject call, so an entry can add package-local search paths without affecting others.
  //
  // This must be the TRANSITIVE closure of the subtree, not just packageReference.Children. Delphi
  // needs the .dcu of every unit reachable from the units it compiles - a unit we use pulls in the
  // units IT uses - so a dependency-of-a-dependency's lib folder has to be on the search path too.
  // Direct children only produced 'F1026 File not found: <transient>.dcu' (e.g. VSoft.SlackClient ->
  // VSoft.HttpClient -> VSoft.Uri). VisitDFS walks descendants then the node itself, so skip self;
  // dedupe by id since a shared dependency appears under every parent that references it.
  searchPaths := TCollections.CreateList<string>;
  if packageReference.HasChildren then
  begin
    seenDependencies := TCollections.CreateDictionary<string, boolean>;
    packageReference.VisitDFS(
      procedure(const dependencyReference : IPackageReference)
      var
        dependencyKey: string;
        dependencyLibPath: string;
      begin
        if dependencyReference = packageReference then
          exit;
        dependencyKey := LowerCase(dependencyReference.Id);
        if seenDependencies.ContainsKey(dependencyKey) then
          exit;
        seenDependencies[dependencyKey] := true;
        dependencyLibPath := FPackageCache.GetPackagePath(dependencyReference.Id, dependencyReference.Version.ToStringNoMeta, Compiler.compilerVersion);
        dependencyLibPath := TPath.Combine(dependencyLibPath, 'lib' + PathDelim + DPMPlatformToBDString(effectivePlatform));
        searchPaths.Add(dependencyLibPath);
      end);
  end;

  // Snapshot each design entry's candidate platforms BEFORE the build loop runs. The build loop
  // patches dprojs in place (including their <Platform> list), and a project that is both a build
  // entry and a design entry would otherwise have that list extended before we inspect it - making
  // a Win32-only design package look like it supports every platform we ever built it for.
  designCandidates := TCollections.CreateDictionary<string, TDPMPlatforms>;
  for designEntry in template.DesignEntries do
  begin
    projectFile := ResolveProjectFile(designEntry.Project);

    //Prebuilt entries are handled wholesale in the build loop below - they are never compiled, and
    //their platform is an IDE bitness rather than a compile target, so they take no part in the
    //effectivePlatform gate these candidates feed.
    if designEntry.IsPrebuilt then
      continue
    else if designEntry.Platforms <> [] then
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
    designCandidates[LowerCase(designEntry.Project)] := candidates;
  end;

  // Copy any copyToLib companion files (.dfm/.res/etc) into lib\{platform} BEFORE compiling, so the
  // companion files are already on the compiler's search path when the runtime/design projects build.
  CopyCopyToLibFiles;

  // Compile build entries
  for buildEntry in template.BuildEntries do
  begin
    // Check platform filter if specified
    if (buildEntry.Platforms <> []) and (not (effectivePlatform in buildEntry.Platforms)) then
      continue;

    FLogger.Information('Building project: ' + buildEntry.Project);
    projectFile := ResolveProjectFile(buildEntry.Project);

    if not ProjectIsSafeToBuild(projectFile, buildEntry.Project) then
    begin
      FLogger.NewLine;
      exit(false);
    end;

    EnsureBuildTarget(projectFile, buildEntry.Project, [TProjectPatchOption.UpdatePlatformList]);

    Compiler.SetSearchPaths(ResolveEntrySearchPaths(buildEntry.SearchPaths));
    result := Compiler.BuildProject(cancellationToken, effectivePlatform, projectFile, configuration, packageInfo.Version, false);
    if result then
      FLogger.Success('Project [' + buildEntry.Project + '] build succeeded.')
    else
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Building project [' + buildEntry.Project + '] cancelled.')
      else
        FLogger.Error('Building project [' + buildEntry.Project + '] failed.');
      // Trailing blank line so the caller's failure message doesn't run into this project's output.
      FLogger.NewLine;
      exit;
    end;
    // Trailing blank line so each project's output is a visually distinct block - the header for the
    // next project (or the next package/platform) then starts against white space rather than ending it.
    FLogger.NewLine;
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

    //Packages that ship precompiled design-time binaries point the entry at the .bpl itself rather
    //than a dproj. There is nothing to patch and nothing to compile - both steps below fail hard on
    //a PE file (msxml rejects it as invalid xml, msbuild can't load it as a project). All we owe the
    //caller is confirmation that the entry is well formed and the binary really is in the package.
    //Deliberately checked ahead of the effectivePlatform gate: the entry's platform is the bitness of
    //the IDE that can load the bpl, not a compile target, so a Win64 design bpl is just as relevant
    //to a Win32-only install. Which one the IDE actually loads is decided at load time.
    if designEntry.IsPrebuilt then
    begin
      if not designEntry.ResolvePrebuiltPlatform(designPlatform, prebuiltError) then
      begin
        FLogger.Error('Package [' + packageInfo.Id + '] : ' + prebuiltError);
        FLogger.NewLine;
        exit(false);
      end;
      if not FileExists(projectFile) then
      begin
        FLogger.Error('Prebuilt design package [' + designEntry.Project + '] for package [' + packageInfo.Id +
                      '] was not found in the package - check the dspec ships it via a source entry.');
        FLogger.NewLine;
        exit(false);
      end;
      FLogger.Information('Using prebuilt design package: ' + designEntry.Project + ' (' + DPMPlatformToString(designPlatform) + ' IDE)');
      FLogger.NewLine;
      continue;
    end;

    supportedByCompiler := DesignTimePlatforms(Compiler.compilerVersion);

    //candidates were captured above, before any dproj was patched.
    designPlatforms := supportedByCompiler * designCandidates[LowerCase(designEntry.Project)];

    if not (effectivePlatform in designPlatforms) then
    begin
      FLogger.Debug('Skipping design package [' + designEntry.Project + '] - ' + DPMPlatformToString(effectivePlatform) + ' is not a supported design platform for this entry.');
      continue;
    end;

    FLogger.Information('Building design package: ' + designEntry.Project + ' (' + DPMPlatformToString(effectivePlatform) + ')');

    if not ProjectIsSafeToBuild(projectFile, designEntry.Project) then
    begin
      FLogger.NewLine;
      exit(false);
    end;

    //note the empty option set - we never write to a design dproj's <Platform value=..> list,
    //because that list is exactly what the manifest-silent branch above reads to decide which
    //design platforms this entry supports, and the patch persists in the cache. msbuild ignores
    //ProjectExtensions anyway, so the design build loses nothing.
    EnsureBuildTarget(projectFile, designEntry.Project, []);

    //output dirs were set for effectivePlatform by the runtime build above; search paths are set per
    //entry so this design entry's own searchPaths (combined with the dependency lib folders) apply.
    Compiler.SetSearchPaths(ResolveEntrySearchPaths(designEntry.SearchPaths));
    result := Compiler.BuildProject(cancellationToken, effectivePlatform, projectFile, configuration, packageInfo.Version, true);
    if not result then
    begin
      if cancellationToken.IsCancelled then
        FLogger.Error('Building design package [' + designEntry.Project + '] cancelled.')
      else
        FLogger.Error('Building design package [' + designEntry.Project + '] failed.');
      FLogger.NewLine;
      exit;
    end;
    FLogger.Success('Design package [' + designEntry.Project + '] build succeeded.');
    FLogger.NewLine;
  end;

  // Copy any copyToBin files (native dlls) into bpl\{platform} for the platform just built.
  CopyCopyToBinFiles;

  // Save the bill of materials file for future reference
  TBOMFile.SaveToFile(FLogger, bomFile, packageReference);
end;


function TPackageInstaller.GetDpmExePathForTargets : string;
var
  candidate : string;
begin
  //When running as dpm.exe (CLI), ParamStr(0) is the exe to record directly.
  result := ParamStr(0);
  if SameText(ExtractFileName(result), 'dpm.exe') then
    exit;

  //Running inside the IDE (ParamStr(0) is bds.exe): this code executes from the DPM design-time
  //package, which is installed in the same folder as dpm.exe. Locate dpm.exe next to our own module
  //so the copy-local target uses a full path rather than relying on 'dpm' being on the PATH.
  result := '';
  candidate := TPath.Combine(ExtractFilePath(TSystemUtils.GetCurrentModuleFileName), 'dpm.exe');
  if TFile.Exists(candidate) then
    result := candidate;
end;

constructor TPackageInstaller.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager: IPackageRepositoryManager;
                                     const packageCache: IPackageCache; const dependencyResolver: IDependencyResolver; const context: IPackageInstallerContext;
                                     const compilerFactory: ICompilerFactory; const receiptService : IReceiptService;
                                     const packageGenerator : IPackageProjectGenerator;
                                     const projectConfigPatcher : IProjectConfigPatcher;
                                     const installPlanner : IInstallPlanner);
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
  FProjectConfigPatcher := projectConfigPatcher;
  FInstallPlanner := installPlanner;
end;

{ TProjectInstallWork }

constructor TProjectInstallWork.Create(const projectFile : string; const editor : IProjectEditor; const options : TInstallOptions; const platforms : TDPMPlatforms);
begin
  inherited Create;
  FProjectFile := projectFile;
  FEditor := editor;
  FOptions := options;
  FPlatforms := platforms;
  FPackages := TCollections.CreateList<IPackageInfo>;
end;

destructor TProjectInstallWork.Destroy;
begin
  FOptions.Free;
  inherited;
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

function TPackageInstaller.CreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference;  const projectReferences: IList<IPackageReference>): boolean;
begin
  result := CreateProjectRefs(cancellationToken, rootnode, nil, projectReferences);
end;

// convert the dependency graph to a flat list of IPackageinfo's (which has dependencies). Packages in
// excludeIds are left out along with anything only reachable through them - used for the packages being
// installed, which replace whatever the graph has for them. The graph itself is not modified.
function TPackageInstaller.CreateProjectRefs(const cancellationToken : ICancellationToken; const rootnode: IPackageReference; const excludeIds : ISet<string>;
                                             const projectReferences: IList<IPackageReference>): boolean;
var
  seenPackages: IDictionary<string, IPackageInfo>;
  packageRefLookup : IDictionary<string, IPackageReference>;

  function IsExcluded(const id : string) : boolean;
  begin
    result := (excludeIds <> nil) and excludeIds.Contains(LowerCase(id));
  end;

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
      if IsExcluded(dependency.Id) then
        continue;

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
      if IsExcluded(dependency.Id) then
        continue;
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

function TPackageInstaller.DoRestoreProjectForPlatform(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string;
                                            const projectEditor: IProjectEditor; const platform: TDPMPlatform; const config: IConfiguration;
                                            const context: IPackageInstallerContext; out resultGraph: IPackageReference): boolean;
var
  projectPackageGraph: IPackageReference;
  projectReferences: IList<IPackageReference>;
  resolvedPackages: IList<IPackageInfo>;
  preferredVersions: IDictionary<string, TPackageVersion>;
  graphState: TRestoreGraphState;
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
  graphState := ValidateRestoreGraph(cancellationToken, projectPackageGraph, Options.CompilerVersion, resolvedPackages);
  if graphState = rgsInvalid then
    exit;
  if graphState = rgsValid then
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

//True for a package the user asked to install - force only rebuilds those, not their dependencies.
function IsRequestedPackage(const options : TSearchOptions; const packageId : string) : boolean;
var
  request : TInstallPackageRequest;
begin
  if (options is TInstallOptions) and (Length(TInstallOptions(options).Packages) > 0) then
  begin
    for request in TInstallOptions(options).Packages do
    begin
      if SameText(request.PackageId, packageId) then
        exit(true);
    end;
    exit(false);
  end;
  result := SameText(packageId, options.SearchTerms);
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
          // Determine if force compilation needed (only for explicitly requested packages)
          forceCompile := Options.force and IsRequestedPackage(Options, pkgInfo.Id);

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

  //Overlay the IDE's current-session 'use source' choices onto the graph before we collect search
  //paths. This is the single source of truth for the source-vs-lib decision - it is never persisted
  //to the dproj and is always empty for the CLI (so command line installs use the precompiled lib).
  //A source choice on a node flows down to its dependencies via TPackageReference.GetUseSource.
  projectPackageGraph.VisitDFS(
    procedure(const node : IPackageReference)
    begin
      if FContext.GetUseSource(projectFile, node.Id) then
        node.UseSource := true;
    end);

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

function TPackageInstaller.PrepareProjectPackages(const cancellationToken: ICancellationToken; const projectFile: string; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; out packagesToConfigure: IList<IPackageInfo>; out packageSpecs: IDictionary<string, IPackageSpec>): boolean;
var
  packageInfo: IPackageInfo;
begin
  result := false;
  packagesToConfigure := nil;
  packageSpecs := nil;

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

  result := true;
end;

function TPackageInstaller.FinalizePackageConfiguration(const cancellationToken: ICancellationToken; const options: TSearchOptions;
  const projectFile: string; const projectEditor: IProjectEditor; const platform: TDPMPlatform;
  const config: IConfiguration; const projectPackageGraph: IPackageReference;
  const resolvedPackages: IList<IPackageInfo>; out resultGraph: IPackageReference): boolean;
var
  packageSpecs: IDictionary<string, IPackageSpec>;
  packagesToConfigure: IList<IPackageInfo>;
begin
  result := false;
  resultGraph := nil;

  if not PrepareProjectPackages(cancellationToken, projectFile, projectPackageGraph, resolvedPackages, packagesToConfigure, packageSpecs) then
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
  fullPath: string;
  explicitProject: string;
  i: integer;
begin
  result := false;

  //Normalise up front. Everything below - and everything downstream that takes these paths
  //apart again - relies on an absolute, PathDelim separated path. A path supplied with posix
  //separators ('dpm restore c:/src/all.groupproj') otherwise breaks the group member
  //resolution: ExtractFilePath only splits on PathDelim and DriveDelim, so the group root
  //comes out as 'c:' and every member dproj is looked for under the drive root.
  fullPath := TPathUtils.ToAbsolutePath(projectPath);

  // If explicit projects were provided, use them
  if Length(explicitProjects) > 0 then
  begin
    for i := 0 to Length(explicitProjects) - 1 do
    begin
      //normalised for the same reason as the project path - these end up in the same list.
      explicitProject := TPathUtils.ToAbsolutePath(explicitProjects[i]);
      if FileExists(explicitProject) then
        projectList.Add(explicitProject)
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
  if FileExists(fullPath) then
  begin
    if SameText(ExtractFileExt(fullPath), '.groupproj') then
    begin
      groupProjReader := TGroupProjectReader.Create(FLogger);
      if not groupProjReader.LoadGroupProj(fullPath) then
        exit;
      if not groupProjReader.ExtractProjects(projectList) then
        exit;
      // Projects in a group are likely relative, so make them full paths
      for i := 0 to projectList.Count - 1 do
        projectList[i] := TPathUtils.ResolveRelativeToFile(fullPath, projectList[i]);
    end
    else
      projectList.Add(fullPath);
    result := true;
    exit;
  end;

  // Handle directory path
  if DirectoryExists(fullPath) then
  begin
    projectList.AddRange(TDirectory.GetFiles(fullPath, '*.dproj'));
    if projectList.Count = 0 then
    begin
      FLogger.Error('No dproj files found in projectPath : ' + fullPath);
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
  plan : IInstallPlan;
begin
  result := false;
  //one request cache across planning and applying - see TRepositoryRequestCache.
  FRepositoryManager.BeginRequestCache;
  try
    try
      if not DoPlanInstall(cancellationToken, Options, plan) then
      begin
        LogPlanErrors(plan);
        exit;
      end;
      LogPlanSummary(plan);
      result := DoApplyInstallPlan(cancellationToken, Options, plan);
    except
      on e: Exception do
      begin
        FLogger.Error(e.Message);
        result := false;
      end;
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
end;

function TPackageInstaller.PlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan) : boolean;
begin
  plan := nil;
  FRepositoryManager.BeginRequestCache;
  try
    try
      result := DoPlanInstall(cancellationToken, options, plan);
    except
      on e: Exception do
      begin
        plan := CreateFailedPlan(e.Message);
        result := false;
      end;
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
end;

function TPackageInstaller.ApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan; const context : IPackageInstallerContext) : boolean;
begin
  FRepositoryManager.BeginRequestCache;
  try
    try
      result := DoApplyInstallPlan(cancellationToken, options, plan);
    except
      on e: Exception do
      begin
        FLogger.Error(e.Message);
        result := false;
      end;
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
end;

procedure TPackageInstaller.LogPlanErrors(const plan : IInstallPlan);
var
  error : string;
begin
  if plan = nil then
    exit;
  for error in plan.Errors do
    FLogger.Error(error);
end;

procedure TPackageInstaller.LogPlanSummary(const plan : IInstallPlan);
var
  line : string;
begin
  //companions are packages the user didn't ask for - make sure they are seen.
  for line in plan.SummaryLines do
    FLogger.Information(line, plan.HasCompanions);
end;

function TPackageInstaller.DoPlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan) : boolean;
var
  config : IConfiguration;
  projectFiles : IList<string>;
  works : IList<TProjectInstallWork>;
  work : TProjectInstallWork;
  compilerVersion : TCompilerVersion;
  requested : IList<IPackageInfo>;
  planProjects : IList<TInstallPlanProject>;
  planProject : TInstallPlanProject;
begin
  result := false;
  plan := nil;
  config := Init(options);
  if config = nil then
  begin
    plan := CreateFailedPlan('Unable to start the install - see the log for details.');
    exit;
  end;

  projectFiles := TCollections.CreateList<string>;
  if not ResolveProjectFiles(options.ProjectPath, options.Projects, projectFiles) then
  begin
    plan := CreateFailedPlan('No projects to install into.');
    exit;
  end;

  if options.PackageFile <> '' then
  begin
    if not FileExists(options.PackageFile) then
    begin
      // should never happen if validation is called on the options.
      plan := CreateFailedPlan('The specified packageFile [' + options.PackageFile + '] does not exist.');
      exit;
    end;
    if not PreparePackageFileInstall(options) then
    begin
      plan := CreateFailedPlan('Unable to install the package file [' + options.PackageFile + '].');
      exit;
    end;
  end;
  //callers that set PackageId/Version directly rather than a list install that one package.
  if (Length(options.Packages) = 0) and (options.PackageId <> '') then
    options.AddPackage(options.PackageId, options.Version);

  works := TCollections.CreateObjectList<TProjectInstallWork>(true);
  if not LoadInstallWork(cancellationToken, options, projectFiles, config, works, compilerVersion) then
  begin
    plan := CreateFailedPlan('Unable to load the project(s) - see the log for details.');
    exit;
  end;

  requested := TCollections.CreateList<IPackageInfo>;
  if not LookupRequestedPackages(cancellationToken, options, compilerVersion, requested) then
  begin
    plan := CreateFailedPlan('Unable to find the package(s) to install - see the log for details.');
    exit;
  end;

  planProjects := TCollections.CreateList<TInstallPlanProject>;
  for work in works do
  begin
    planProject.ProjectFile := work.ProjectFile;
    planProject.Graph := work.Editor.GetPackageReferences;
    planProjects.Add(planProject);
  end;

  plan := FInstallPlanner.Plan(cancellationToken, config, options, requested, planProjects, options.IsUpgrade);
  result := plan.Succeeded;
end;

function TPackageInstaller.LoadInstallWork(const cancellationToken : ICancellationToken; const options : TInstallOptions; const projectFiles : IList<string>;
                                           const config : IConfiguration; const works : IList<TProjectInstallWork>; out compilerVersion : TCompilerVersion) : boolean;
var
  i : integer;
  projectFile : string;
  projectEditor : IProjectEditor;
  workOptions : TInstallOptions;
  platforms : TDPMPlatforms;
  shouldSkip : boolean;
begin
  result := false;
  compilerVersion := options.CompilerVersion;
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

    projectEditor := TProjectEditor.Create(FLogger, config, compilerVersion);
    if not projectEditor.LoadProject(projectFile) then
    begin
      FLogger.Error('Unable to load project file [' + projectFile + '], cannot continue');
      exit;
    end;
    //when no compiler version was given the first project decides it - every project is installed for the same one.
    if compilerVersion = TCompilerVersion.UnknownVersion then
      compilerVersion := projectEditor.CompilerVersion;

    //each project gets its own options - reconciling the compiler/platforms mustn't leak between projects.
    workOptions := options.Clone;
    workOptions.CompilerVersion := compilerVersion;
    ValidateAndSetCompilerPlatforms(workOptions, projectEditor, platforms, shouldSkip);
    if shouldSkip then
    begin
      //nothing to do in a project that doesn't target the requested platforms - not a failure.
      workOptions.Free;
      continue;
    end;
    works.Add(TProjectInstallWork.Create(projectFile, projectEditor, workOptions, platforms));
  end;
  result := not cancellationToken.IsCancelled;
end;

function TPackageInstaller.LookupRequestedPackages(const cancellationToken : ICancellationToken; const options : TInstallOptions;
                                                   const compilerVersion : TCompilerVersion; const requested : IList<IPackageInfo>) : boolean;
var
  request : TInstallPackageRequest;
  packageIdentity : IPackageIdentity;
  packageInfo : IPackageInfo;
begin
  result := true;
  for request in options.Packages do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    // if the user specified a version, either on the command line or via a file then we will use that
    if not request.Version.IsEmpty then
    begin
      // sourceName will be empty if we are installing the package from a file
      packageIdentity := TPackageIdentity.Create(options.Sources, request.PackageId, request.Version, compilerVersion);
      packageInfo := GetPackageInfo(cancellationToken, packageIdentity);
    end
    else
      packageInfo := FRepositoryManager.FindLatestVersion(cancellationToken, request.PackageId, compilerVersion, TPackageVersion.Empty, options.PreRelease, options.Sources);

    if packageInfo = nil then
    begin
      FLogger.Error('Package [' + request.PackageId + '] not found on any sources');
      result := false;
      continue;
    end;
    if packageInfo.IsSigned and (packageInfo.SignedBy <> '') then
      FLogger.Information('Installing package ' + packageInfo.ToString + ' (signed by ' + packageInfo.SignedBy + ')')
    else if packageInfo.IsSigned then
      FLogger.Information('Installing package ' + packageInfo.ToString + ' (signed)')
    else
      FLogger.Information('Installing package ' + packageInfo.ToString);
    // we need this later when collecting search paths.
    packageInfo.UseSource := options.UseSource;
    requested.Add(packageInfo);
  end;
end;

function TPackageInstaller.DoApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan) : boolean;
var
  config : IConfiguration;
  works : IList<TProjectInstallWork>;
  work : TProjectInstallWork;
  compilerVersion : TCompilerVersion;
  projectFiles : IList<string>;
  failedProjects : string;
begin
  result := false;
  if (plan = nil) or (not plan.Succeeded) then
  begin
    LogPlanErrors(plan);
    exit;
  end;
  if not plan.Projects.Any then
  begin
    FLogger.Error('There is nothing to install - none of the projects need the package(s).');
    exit;
  end;

  config := Init(options);
  if config = nil then
    exit;

  works := TCollections.CreateObjectList<TProjectInstallWork>(true);
  if not LoadInstallWork(cancellationToken, options, plan.Projects, config, works, compilerVersion) then
    exit;
  projectFiles := TCollections.CreateList<string>;
  for work in works do
  begin
    work.Packages := plan.PackagesForProject(work.ProjectFile);
    projectFiles.Add(work.ProjectFile);
  end;

  //Projects in the operation don't block each other while they are resolved, and nothing they record
  //counts until their dproj is saved - see IPackageInstallerContext.BeginOperation.
  FContext.BeginOperation(projectFiles);
  try
    //Phase 1 : resolve every project before anything is changed. Carry on after a failure so every
    //problem is reported in one run.
    failedProjects := '';
    for work in works do
    begin
      if cancellationToken.IsCancelled then
        exit;
      if not ResolveProjectInstall(cancellationToken, work) then
      begin
        if failedProjects <> '' then
          failedProjects := failedProjects + ', ';
        failedProjects := failedProjects + ExtractFileName(work.ProjectFile);
      end;
    end;
    if failedProjects <> '' then
    begin
      FLogger.Error('Dependency resolution failed for [' + failedProjects + '] - no project was changed.');
      if not options.IsUpgrade then
        HintCompanions(cancellationToken, config, options, plan, works);
      exit;
    end;

    //Phase 2 : download, compile and configure every project - in memory, the dprojs are not saved yet.
    for work in works do
    begin
      if cancellationToken.IsCancelled then
        exit;
      if not BuildProjectInstall(cancellationToken, config, work) then
      begin
        FLogger.Error('Install failed for [' + work.ProjectFile + '] - no project was changed.');
        exit;
      end;
    end;

    //Phase 3 : save. Not interrupted by cancellation - stopping part way would leave the group half changed.
    result := true;
    for work in works do
      result := CommitProjectInstall(cancellationToken, work) and result;
  finally
    FContext.EndOperation;
  end;
end;

function TPackageInstaller.ResolveProjectInstall(const cancellationToken : ICancellationToken; const work : TProjectInstallWork) : boolean;
var
  projectGraph : IPackageReference;
  excludeIds : ISet<string>;
  packageInfo : IPackageInfo;
  projectReferences : IList<IPackageReference>;
  resolvedGraph : IPackageReference;
  resolved : IList<IPackageInfo>;
  ids : string;
begin
  result := false;
  ids := '';
  excludeIds := TCollections.CreateSet<string>;
  for packageInfo in work.Packages do
  begin
    excludeIds.Add(LowerCase(packageInfo.Id));
    if ids <> '' then
      ids := ids + ', ';
    ids := ids + packageInfo.Id + '-' + packageInfo.Version.ToStringNoMeta;
  end;
  FLogger.Information('Resolving [' + ids + '] for [' + work.ProjectFile + ']', true);

  projectGraph := work.Editor.GetPackageReferences; // can return nil
  if projectGraph = nil then
    projectGraph := TPackageReference.CreateRoot(work.Options.CompilerVersion);

  //The packages being installed replace any reference to them, top level or transitive, so they (and
  //anything only reachable through them) are left out of the references the resolver starts from.
  projectReferences := TCollections.CreateList<IPackageReference>;
  if not CreateProjectRefs(cancellationToken, projectGraph, excludeIds, projectReferences) then
    exit;

  if not FDependencyResolver.ResolveForInstall(cancellationToken, work.Options.CompilerVersion, work.ProjectFile, work.Options, work.Packages,
                                               projectReferences, resolvedGraph, resolved, nil, nil, nil) then
    exit;

  // Install-specific sanity check: every package being installed must be in the resolved list
  for packageInfo in work.Packages do
  begin
    if not resolved.Any(
      function(const info : IPackageInfo) : boolean
      begin
        result := SameText(info.Id, packageInfo.Id);
      end) then
    begin
      FLogger.Error('Something went wrong, resolution did not return installed package [' + packageInfo.Id + ']!');
      exit;
    end;
  end;

  work.Graph := resolvedGraph;
  work.Resolved := resolved;
  result := true;
end;

function TPackageInstaller.BuildProjectInstall(const cancellationToken : ICancellationToken; const config : IConfiguration; const work : TProjectInstallWork) : boolean;
var
  packagesToConfigure : IList<IPackageInfo>;
  packageSpecs : IDictionary<string, IPackageSpec>;
  platform : TDPMPlatform;
  platformOptions : TInstallOptions;
  platformResult : boolean;
begin
  result := false;
  if not PrepareProjectPackages(cancellationToken, work.ProjectFile, work.Graph, work.Resolved, packagesToConfigure, packageSpecs) then
    exit;

  for platform in work.Platforms do
  begin
    if cancellationToken.IsCancelled then
      exit;
    FLogger.Information('Installing into [' + work.ProjectFile + '] for platform [' + DPMPlatformToString(platform) + ']', true);
    // do not modify the work's options!
    platformOptions := work.Options.Clone;
    try
      platformOptions.Platforms := [platform];
      platformResult := DoConfigurePackageForPlatform(cancellationToken, platformOptions, work.ProjectFile, work.Editor, platform, config,
                                                      work.Graph, packagesToConfigure, packageSpecs);
    finally
      platformOptions.Free;
    end;
    if not platformResult then
    begin
      FLogger.Error('Install failed for [' + work.ProjectFile + '] on platform [' + DPMPlatformToString(platform) + ']');
      exit;
    end;
  end;

  //Make sure the IDE host platform's design BPL exists before design packages are loaded at commit -
  //the loop above only built the project's target platforms. Non-fatal : a failure here just means the
  //design components may not load.
  if not EnsureDesignHostPlatformCompiled(cancellationToken, work.Options, work.Graph, work.Platforms) then
    FLogger.Warning('Failed to compile design-time packages for the IDE host platform - design components may not load.');

  // P2 §2.6 — pull each cached package's manifest hash off its receipt
  // and lock it onto the PackageReference before we write the dproj.
  PopulateManifestHashes(work.Graph, work.Options.CompilerVersion);
  // Update package references once using the new platform-independent format
  work.Editor.UpdatePackageReferences(work.Graph);
  result := true;
end;

function TPackageInstaller.CommitProjectInstall(const cancellationToken : ICancellationToken; const work : TProjectInstallWork) : boolean;
begin
  result := work.Editor.SaveProject;
  if not result then
  begin
    FLogger.Error('Failed to save [' + work.ProjectFile + ']');
    exit;
  end;
  //the dproj now reflects the install, so its resolutions and graph count for the other projects.
  FContext.CommitProject(work.ProjectFile);
  //Load design-time packages once per project - design BPLs are per-IDE, not per-platform. CLI context is a no-op.
  InstallDesignPackages(cancellationToken, work.ProjectFile, BuildPackageSpecsFromGraph(work.Graph, work.Options.CompilerVersion));
  FLogger.Success('Install succeeded for [' + work.ProjectFile + ']', true);
end;

procedure TPackageInstaller.HintCompanions(const cancellationToken : ICancellationToken; const config : IConfiguration; const options : TInstallOptions;
                                           const plan : IInstallPlan; const works : IList<TProjectInstallWork>);
var
  requested : IList<IPackageInfo>;
  item : IInstallPlanItem;
  planProjects : IList<TInstallPlanProject>;
  planProject : TInstallPlanProject;
  work : TProjectInstallWork;
  upgradePlan : IInstallPlan;
begin
  requested := TCollections.CreateList<IPackageInfo>;
  for item in plan.Items do
    requested.Add(item.PackageInfo);
  //the editors' graphs are still as loaded - resolving doesn't change them.
  planProjects := TCollections.CreateList<TInstallPlanProject>;
  for work in works do
  begin
    planProject.ProjectFile := work.ProjectFile;
    planProject.Graph := work.Editor.GetPackageReferences;
    planProjects.Add(planProject);
  end;
  upgradePlan := FInstallPlanner.Plan(cancellationToken, config, options, requested, planProjects, true);
  if (not upgradePlan.Succeeded) or (not upgradePlan.HasCompanions) then
    exit;
  FLogger.Warning('Other installed packages need to move with this install :', true);
  for item in upgradePlan.Items do
  begin
    if item.IsCompanion then
      FLogger.Warning('  ' + item.PackageInfo.Id + ' ' + item.VersionChangeText + ' (' + item.Reason + ')', true);
  end;
  FLogger.Warning('Use Upgrade in the IDE, or --upgrade on the command line, to install them together.', true);
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

function TPackageInstaller.ValidateRestoreGraph(const cancellationToken: ICancellationToken; const projectPackageGraph: IPackageReference;
                                                const compilerVersion: TCompilerVersion; const resolvedPackages: IList<IPackageInfo>): TRestoreGraphState;
var
  packageInfos: IDictionary<string, IPackageInfo>;
  state: TRestoreGraphState;
  warned: ISet<string>;
  topLevelChild: IPackageReference;

  //A top level reference wins over the range a package declares for it, so it is the version that package
  //gets. Newer on the same major is accepted with a warning - DPM ranges only float the patch, so a top level
  //a minor ahead of a package's range is common, and by semver it is compatible. Older, or another major,
  //the package may not work with it - false, and the restore fails.
  function CheckTopLevelOverride(const node: IPackageReference; const dep: IPackageDependency; const topLevel: IPackageReference): boolean;
  var
    text: string;
    key: string;
  begin
    result := true;
    if dep.VersionRange.IsSatisfiedBy(topLevel.Version) then
      exit;
    text := 'Restore: ' + topLevel.Id + '-' + topLevel.Version.ToStringNoMeta + ' is a top level reference, but ' +
            node.Id + '-' + node.Version.ToStringNoMeta + ' requires ' + dep.Id + ' ' + dep.VersionRange.ToString;
    if not dep.VersionRange.IsCompatibleNewer(topLevel.Version) then
    begin
      if topLevel.Version.Major <> dep.VersionRange.MinVersion.Major then
        FLogger.Error(text + ' - a different major version is not compatible')
      else
        FLogger.Error(text + ' - an older version is not compatible');
      exit(false);
    end;
    //a shared package is visited once for every package that uses it - only say it once.
    key := LowerCase(node.Id + '>' + dep.Id);
    if warned.Contains(key) then
      exit;
    warned.Add(key);
    FLogger.Warning(text + ' - using ' + topLevel.Version.ToStringNoMeta + ' (same major version)');
  end;

  //Recursively validates a node and its declared dependencies. Carries on past anything that only needs a
  //re-resolve, so a top level reference that fails the restore is still found further on - the re-resolve
  //keeps the project's versions, so it wouldn't catch it.
  //Order matters: we always check the *parent's* declared range against the graph child's recorded
  //version BEFORE attempting to load the child's own info. That way a stale/out-of-range transient
  //is reported as "out of range" rather than "not found in cache or repo".
  procedure ValidateNode(const node: IPackageReference);
  var
    info: IPackageInfo;
    dep: IPackageDependency;
    child: IPackageReference;
    topLevel: IPackageReference;
  begin
    if state = rgsInvalid then
      exit;
    if cancellationToken.IsCancelled then
    begin
      state := rgsInvalid;
      exit;
    end;
    info := GetOrLoadPackageInfo(cancellationToken, node.Id, node.Version, compilerVersion, packageInfos);
    if info = nil then
    begin
      FLogger.Information('Restore: package [' + node.Id + '-' + node.Version.ToStringNoMeta + '] not found in cache or repo - re-resolving');
      state := rgsNeedsResolve;
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
      topLevel := projectPackageGraph.FindTopLevelChild(dep.Id);
      if (topLevel <> nil) and (not CheckTopLevelOverride(node, dep, topLevel)) then
      begin
        state := rgsInvalid;
        exit;
      end;
      child := node.FindFirstChild(dep.Id);
      if child = nil then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] requires [' + dep.Id + '] which is missing from the project graph - re-resolving');
        state := rgsNeedsResolve;
        continue;
      end;
      if topLevel <> nil then
      begin
        //the top level node is validated on its own - the graph only has to agree with it.
        if child.Version <> topLevel.Version then
        begin
          FLogger.Warning('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] differs from the top level reference [' +
                          topLevel.Id + '-' + topLevel.Version.ToStringNoMeta + '] - re-resolving');
          state := rgsNeedsResolve;
        end;
        continue;
      end;
      if not dep.VersionRange.IsSatisfiedBy(child.Version) then
      begin
        FLogger.Warning('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] is outside required range [' + dep.VersionRange.ToString + '] - re-resolving');
        state := rgsNeedsResolve;
        continue;
      end;
      //Only recurse if the child passed the range check.
      ValidateNode(child);
      if state = rgsInvalid then
        exit;
    end;
  end;

begin
  state := rgsValid;
  warned := TCollections.CreateSet<string>;
  packageInfos := TCollections.CreateDictionary<string, IPackageInfo>;

  for topLevelChild in projectPackageGraph.Children do
  begin
    ValidateNode(topLevelChild);
    if state = rgsInvalid then
      break;
  end;
  result := state;
  if result <> rgsValid then
    exit;

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
end;

function TPackageInstaller.PreparePackageFileInstall(const Options: TInstallOptions): boolean;
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
    exit(false);

  // update options so we can install from the packageid.
  Options.PackageFile := '';
  Options.AddPackage(packageIdentity.Id, packageIdentity.Version);
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
