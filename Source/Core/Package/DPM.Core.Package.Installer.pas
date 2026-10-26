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

  //the manifest hash a project recorded for a package, and the version (no metadata) it was recorded for.
  TRecordedLock = record
    Version : string;
    Hash : string;
  end;

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
    FLocks : IDictionary<string, TRecordedLock>;
    FFailedPlatforms : TDPMPlatforms;
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
    //the locks the project recorded for the packages it keeps (see CollectGraphLocks).
    property Locks : IDictionary<string, TRecordedLock> read FLocks write FLocks;
    //the platforms that didn't build - the project is still installed into the others.
    property FailedPlatforms : TDPMPlatforms read FFailedPlatforms write FFailedPlatforms;
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
    // Recursively clears the ManifestHash lock on every node in the graph. Used on a detected
    // compiler upgrade (in-place dproj opened in a newer IDE) so the per-compiler locks recorded
    // under the old compiler are discarded rather than failing validation against the new build.
    procedure ClearManifestHashes(const graph : IPackageReference);
    // P2 §2.6 — checks every locked node in the graph, transitive packages included, against the
    // verification receipt the cache wrote for it, then (only when they all match) sets each node's
    // ManifestHash from its receipt - the editor writes it to the .dproj as `manifestHash="sha256:..."`.
    // Each receipt is read once. The id of every package whose lock did not match is appended to
    // mismatchedPackageIds (when given) so the caller can skip loading its design-time BPLs.
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

    function CollectSearchPaths(const cancellationToken: ICancellationToken; const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                const compilerVersion: TCompilerVersion; const platform: TDPMPlatform;  const searchPaths: IList<string>): boolean;

    function DownloadPackages(const cancellationToken: ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
    //Ensures a single package is present in the cache. Git registry packages are cloned
    //in place (clone + dspec); all others are downloaded as a .dpkg and extracted.
    function EnsurePackageInCache(const cancellationToken: ICancellationToken; const packageInfo: IPackageInfo): boolean;

    //logger : what the build reports through - the operation's, or a TDemotingLogger for a build whose
    //failure is not the operation's (see EnsureDesignHostPlatformCompiled).
    function CompilePackage(const cancellationToken: ICancellationToken; const logger : ILogger; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
                            const packageSpec: IPackageSpec;  const force: boolean; const forceDebug : boolean): boolean;

    function BuildDependencies(const cancellationToken: ICancellationToken; const logger : ILogger; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                               const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                               const packageSpecs: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;

    //Design-time packages load into the IDE process, so the design BPL (and the runtime .dcp it links
    //against) must exist for the IDE host platform - Win32 for the classic IDE, Win64 for bds64. The
    //per-platform install/restore loop only compiles the project's target platforms, so a project that
    //does not target the host platform (eg a Win64-only project in the 32-bit IDE) never produces the
    //design BPL the IDE needs. This compiles the host platform into the cache so the BPL exists. It is
    //cache-only and IDE-only : the project (search paths, copy-local, dproj references) is untouched
    //because the host platform is not one of the project's targets, and it is a no-op in the CLI.
    //Not the operation's failure if it doesn't build - that is reported as one warning.
    procedure EnsureDesignHostPlatformCompiled(const cancellationToken: ICancellationToken; const options: TSearchOptions;
                                               const graph: IPackageReference; const installedPlatforms: TDPMPlatforms);

    //Loads the project's design-time packages, unless the build for the platform the IDE loads them for
    //(see DesignHostPlatform) is one of failedPlatforms - then there are no design BPLs to load.
    procedure LoadDesignPackages(const cancellationToken: ICancellationToken; const projectFile: string; const graph: IPackageReference;
                                 const compilerVersion: TCompilerVersion; const failedPlatforms: TDPMPlatforms);

    //Returns the path to dpm.exe to record in the copy-local targets file in the package cache, or ''
    //when it can't be found (the IDE plugin, with no dpm.exe beside it) - the file then keeps the path
    //it already has.
    function GetDpmExePathForTargets : string;

    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>) : boolean;

    //Builds a {lowercase id -> spec} dict by walking the resolved graph and reading specs from the cache.
    //Used after the per-platform install/restore loop so design packages can be loaded once per project.
    function BuildPackageSpecsFromGraph(const cancellationToken: ICancellationToken; const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;

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

    //The locks a project recorded, keyed by lowercase id - excludeIds (lowercase, may be nil) are left out.
    //Re-resolving builds new nodes, so ApplyGraphLocks carries these over to the ones still at the version
    //each was recorded for : without it a re-resolve dropped every lock and re-locked whatever the cache held.
    function CollectGraphLocks(const graph : IPackageReference; const excludeIds : ISet<string>) : IDictionary<string, TRecordedLock>;
    procedure ApplyGraphLocks(const graph : IPackageReference; const locks : IDictionary<string, TRecordedLock>);

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

    //The project's package graph, validated - or re-resolved when it is stale (see ValidateRestoreGraph).
    //projectGraph is nil when the project has no packages. The graph is the project's, not a platform's,
    //so restore does this once per project.
    function ResolveRestoreGraph(const cancellationToken: ICancellationToken; const options: TRestoreOptions; const projectFile: string;
                                 const projectEditor: IProjectEditor; out projectGraph: IPackageReference;
                                 out resolvedPackages: IList<IPackageInfo>): boolean;

    // Records the graph and downloads the resolved packages (bundled no-ops left out) - the platform
    // independent part of an install or restore.
    function PrepareProjectPackages(const cancellationToken: ICancellationToken; const projectFile: string; const projectPackageGraph: IPackageReference;
      const resolvedPackages: IList<IPackageInfo>; out packagesToConfigure: IList<IPackageInfo>; out packageSpecs: IDictionary<string, IPackageSpec>): boolean;

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
    function PreparePackageFileInstall(const cancellationToken : ICancellationToken; const Options: TInstallOptions): boolean;

    // Install = plan, then apply : see PlanInstall/ApplyInstallPlan. Planning hands back the configuration and the
    // projects it loaded, so Install can apply the plan to them rather than loading them again.
    function DoPlanInstall(const cancellationToken: ICancellationToken; const options: TInstallOptions; out plan: IInstallPlan;
                           out config: IConfiguration; out works: IList<TProjectInstallWork>): boolean;
    function DoApplyInstallPlan(const cancellationToken: ICancellationToken; const options: TInstallOptions; const plan: IInstallPlan): boolean;
    // false, having said why, when the plan failed or changes nothing.
    function CanApplyPlan(const plan: IInstallPlan): boolean;
    // resolves, builds and saves the projects in works - which are the plan's projects, already loaded.
    function ApplyInstallWork(const cancellationToken: ICancellationToken; const options: TInstallOptions; const plan: IInstallPlan;
                              const config: IConfiguration; const works: IList<TProjectInstallWork>): boolean;

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
    procedure LogPlanSummary(const plan: IInstallPlan);
    //a package that should exist couldn't be found - says whether a source that could not be used is why.
    procedure ReportNotFound(const description: string);

    function UnInstallFromProject(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const projectFile: string; const config: IConfiguration;
                                  const context: IPackageInstallerContext): boolean;

    function RestoreProject(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                            const context: IPackageInstallerContext): boolean;

    //the entry points' work - the entry points add the operation guard (see TOperationGuard).
    function DoRestore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;
    function DoUninstall(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
    function DoRefreshProjectSearchPaths(const cancellationToken : ICancellationToken; const projectFile : string;
                                         const compilerVersion : TCompilerVersion; const configFile : string;
                                         const context : IPackageInstallerContext) : boolean;

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
  WinApi.Windows,
  System.IOUtils,
  System.Types,
  System.SysUtils,
  Spring,
  Spring.Collections.Extensions,
  VSoft.AntPatterns,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
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

type
  //what ValidateAndPopulateManifestHashes read from a package's receipt.
  TReceiptLookup = record
    FolderExists : boolean;
    HaveReceipt : boolean;
    ManifestHashHex : string;
  end;

{ TPackageInstaller }

function TPackageInstaller.Cache(const cancellationToken: ICancellationToken;  const Options: TCacheOptions): boolean;
var
  config: IConfiguration;
  guard : TOperationGuard;
begin
  result := false;
  guard := TOperationGuard.Start(FLogger);
  try
    config := Init(Options);
    //Init has said why.
    if config <> nil then
      //Installs the package into the cache (the `dpm cache install` command): downloads and extracts
      //the package, resolves and downloads its dependencies, then compiles them all into the cache.
      //One .dpkg per compiler in the new model - it bundles every platform the package supports, so a
      //single download covers them all.
      result := DoCachePackage(cancellationToken, Options);
  except
    on e : Exception do
      result := guard.Failed(e);
  end;
  guard.Finish(result, cancellationToken.IsCancelled, 'Caching the package');
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
  guard : TOperationGuard;
begin
  guard := TOperationGuard.Start(FLogger);
  try
    result := DoRefreshProjectSearchPaths(cancellationToken, projectFile, compilerVersion, configFile, context);
  except
    on e : Exception do
      result := guard.Failed(e);
  end;
  guard.Finish(result, cancellationToken.IsCancelled, 'Refreshing the search paths');
end;

function TPackageInstaller.DoRefreshProjectSearchPaths(const cancellationToken : ICancellationToken; const projectFile : string;
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
    if not CollectSearchPaths(cancellationToken, graph, resolvedPackages, resolvedPackages, compilerVersion, platform, searchPaths) then
      exit(false);
    if not projectEditor.AddSearchPaths(platform, searchPaths, config.PackageCacheLocation) then
      exit(false);
  end;

  //Write the dproj (search paths only - the package reference graph is unchanged and useSource is
  //never persisted). No design-time package load, no build.
  result := projectEditor.SaveProject;
end;

function TPackageInstaller.CollectSearchPaths(const cancellationToken: ICancellationToken; const packageGraph: IPackageReference; const resolvedPackages: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
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
  errorMark: integer;

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

  //A source entry's folder (a packed entry's dest, a git entry's src base folder) is author supplied and
  //goes on the consumer's search path - one that is absolute or leads out of the package folder would
  //have the project compile whatever is there. Such a folder is warned about and never added.
  function SourceFolderInPackage(const pkgInfo: IPackageInfo; const folder: string): boolean;
  var
    resolvedFolder: string;
  begin
    result := TPathUtils.TryResolveInFolder(FPackageCache.GetPackagePath(pkgInfo), folder, resolvedFolder);
    if not result then
      FLogger.Warning(TWarningCode.W5012_PathOutsidePackage, 'Ignoring the source folder [' + folder + '] of [' + pkgInfo.Id + '] - it resolves outside the package folder.');
  end;

  //Add every subfolder under absBase that contains .pas/.inc/.dfm/.fmx/.res files. relSub is
  //computed relative to pkgAbs so the search path matches packageBasePath layout. Skips .git
  //metadata. Delphi search paths are not recursive, so each folder must be listed individually.
  //A junction or symbolic link is not followed - it can lead anywhere, out of the package folder (a
  //git package is a clone of someone else's repository).
  procedure AddPasSubdirs(const pkgAbs, absBase, basePath: string);
  var
    subDir: string;
    relSub: string;
  begin
    if not DirectoryExists(absBase) then
      exit;
    for subDir in TDirectory.GetDirectories(absBase, '*', TSearchOption.soTopDirectoryOnly) do
    begin
      //skip the git metadata folder and anything under it
      if SameText(ExtractFileName(subDir), '.git') then
        continue;
      if (GetFileAttributes(PChar(subDir)) and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then
        continue;
      if DirHasSourceFiles(subDir) then
      begin
        relSub := Copy(subDir, Length(pkgAbs) + 1, MaxInt);
        AddUnique(basePath + relSub);
      end;
      AddPasSubdirs(pkgAbs, subDir, basePath);
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
    if not SourceFolderInPackage(pkgInfo, baseRel) then
      exit;
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
    if not SourceFolderInPackage(pkgInfo, dest) then
      exit;
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
    errorMark := FLogger.ErrorCount;
    packageSpec := FPackageCache.GetPackageSpec(cancellationToken, packageInfo);
    if packageSpec = nil then
    begin
      //the cache (or the spec reader) may already have said why.
      if FLogger.ErrorCount = errorMark then
        FLogger.Error(TErrorCode.E3009_PackageSpecUnavailable, 'Unable to read the spec of [' + packageInfo.ToString + '] from the package cache.');
      exit(false);
    end;

    if (packageSpec.TargetPlatform = nil) or (packageSpec.TargetPlatform.TemplateName = '') then
    begin
      FLogger.Warning(TWarningCode.W3012_PackageHasNoSearchPaths, 'The spec of [' + packageInfo.Id + '] has no target platform or template - no search paths added.');
      continue;
    end;

    template := packageSpec.FindTemplate(packageSpec.TargetPlatform.TemplateName);
    if template = nil then
    begin
      FLogger.Warning(TWarningCode.W3012_PackageHasNoSearchPaths, 'The template [' + packageSpec.TargetPlatform.TemplateName + '] of [' + packageInfo.Id + '] was not found - no search paths added.');
      continue;
    end;

    packageBasePath := packageInfo.Id + PathDelim + packageInfo.Version.ToStringNoMeta + PathDelim;

    //git packages are cloned in place, so their source lives at the repo-relative
    //location given by each source entry's src glob (dest is meaningless - there is
    //no copy step). Detect via the marker the in-place install writes into the cache
    //folder - robust even when SourceName is not carried on the resolved-graph package.
    isGitSource := IsGitRegistryPackageFolder(FPackageCache.GetPackagePath(packageInfo));

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

function TPackageInstaller.CompilePackage(const cancellationToken : ICancellationToken; const logger : ILogger; const Compiler: ICompiler; const packageInfo: IPackageInfo; const packageReference: IPackageReference;
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

  //Build/design `project` paths are relative to the extracted package root, authored with a leading
  //separator (e.g. '/packages/..') and forward slashes. They are author supplied, so CheckProjectPaths has
  //already refused any that is absolute or leads out of the package folder - this only resolves them.
  function ResolveProjectFile(const projectPath : string) : string;
  begin
    if not TPathUtils.TryResolveInFolder(packagePath, projectPath, result) then
      result := '';
  end;

  //Every build and design project has to be inside the package folder - DPM reads, patches and builds
  //them. Checked once, before anything touches any of them, and fatal: a package that points its build
  //somewhere else is not one to half install.
  function CheckProjectPaths : boolean;
  var
    projects : IList<string>;
    buildItem : ISpecBuildEntry;
    designItem : ISpecDesignEntry;
    entryProject : string;
    resolvedFile : string;
  begin
    projects := TCollections.CreateList<string>;
    for buildItem in template.BuildEntries do
      projects.Add(buildItem.Project);
    for designItem in template.DesignEntries do
      projects.Add(designItem.Project);
    for entryProject in projects do
    begin
      if not TPathUtils.TryResolveInFolder(packagePath, entryProject, resolvedFile) then
      begin
        logger.Error(TErrorCode.E5019_ProjectOutsidePackage, 'The project [' + entryProject + '] of [' + packageInfo.Id +
                     '] is not inside the package folder - spec paths are relative to it, and DPM only builds what the package contains.');
        exit(false);
      end;
    end;
    result := true;
  end;

  //Resolve an entry's additional searchPaths (authored relative to the package root, same convention
  //as `project`) against the extracted package folder, and combine them with the dependency lib
  //folders in `searchPaths`. A path that resolves outside the package folder is warned about and
  //skipped - it never reaches msbuild. An entry with no searchPaths just yields the dependency folders.
  function ResolveEntrySearchPaths(const entrySearchPaths : IList<string>) : IList<string>;
  var
    entryPath : string;
    resolvedPath : string;
  begin
    result := TCollections.CreateList<string>;
    result.AddRange(searchPaths);
    if (entrySearchPaths = nil) or (entrySearchPaths.Count = 0) then
      exit;
    for entryPath in entrySearchPaths do
    begin
      if not TPathUtils.TryResolveInFolder(packagePath, entryPath, resolvedPath) then
      begin
        logger.Warning(TWarningCode.W5012_PathOutsidePackage, 'Ignoring the search path [' + entryPath + '] of [' + packageInfo.Id + '] - it resolves outside the package folder.');
        continue;
      end;
      result.Add(IncludeTrailingPathDelimiter(resolvedPath));
    end;
  end;

  //Before handing a project to msbuild with /p:Config=<configuration> /p:Platform=<effectivePlatform>,
  //make sure the dproj actually declares that pair. A dspec can legitimately target platforms the
  //package's own dproj was never configured for, and msbuild applies NONE of the project's settings
  //when the config activator group is missing. The patch is written in place in the cache folder and
  //is idempotent - a project that already declares the pair is not rewritten at all.
  //Failure here is never fatal : we warn and let msbuild try anyway, so this can only ever fix
  //packages, never break one that builds today.
  //The project is inside the package folder - CheckProjectPaths refused the package otherwise - so the
  //patch never touches a file the package doesn't own.
  procedure EnsureBuildTarget(const projectFile : string; const entryProject : string; const options : TProjectPatchOptions);
  var
    patchResult : TProjectPatchResult;
  begin
    if FProjectConfigPatcher = nil then
      exit;
    //a failure is reported by the patcher - the build is attempted anyway.
    patchResult := FProjectConfigPatcher.EnsureBuildTarget(projectFile, effectivePlatform, configuration,
                                                          Compiler.compilerVersion, options);
    if patchResult = TProjectPatchResult.Failed then
      logger.Debug('Unable to add [' + configuration + '] / ' + DPMPlatformToString(effectivePlatform) +
                   ' to the project [' + entryProject + '].');
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
      logger.Error(TErrorCode.E5004_BuildProjectUnreadable, 'The project [' + entryProject + '] of [' + packageInfo.Id +
                    '] could not be inspected - refusing to build it.');
      exit(false);
    end;

    //Advisory findings are already inert - the author just expected a step that will never run.
    for finding in advisory do
      logger.Warning(TWarningCode.W5013_BuildEventIgnored, 'Package [' + packageInfo.Id + '] : ' + finding +
                      ' - DPM blocks build events, so that step will not run.');

    result := not blocking.Any;
    if not result then
    begin
      logger.Error(TErrorCode.E5005_BuildProjectRunsCommands, 'The project [' + entryProject + '] of [' + packageInfo.Id +
                    '] would run commands during the build : ' + TStringUtils.Join('; ', blocking.ToArray) +
                    ' - DPM compiles package projects, it does not execute them, so it will not build this package.');
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
    warned : boolean;
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
      warned := false;
      for pattern in patterns do
      begin
        //only ever from inside the package - Expand keeps a rooted pattern as it is and follows '..', so a
        //pattern could otherwise copy any file on the machine into lib\{platform}.
        if not TPathUtils.IsPathInFolder(packagePath, pattern.Directory) then
        begin
          if not warned then
            logger.Warning(TWarningCode.W5012_PathOutsidePackage, 'Ignoring the copyToLib pattern [' + copyEntry.Source + '] of [' + packageInfo.Id + '] - it resolves outside the package folder.');
          warned := true;
          continue;
        end;
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
            logger.Debug('Copied [' + ExtractFileName(srcFile) + '] to lib for package [' + packageInfo.Id + '].');
          except
            on e : Exception do
              logger.Warning(TWarningCode.W5014_CopyFileFailed, 'Unable to copy [' + srcFile + '] to the lib folder : ' + e.Message);
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
    warned : boolean;
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
      warned := false;
      for pattern in patterns do
      begin
        //only ever from inside the package, as for copyToLib - and bpl\{platform} is on the IDE's PATH.
        if not TPathUtils.IsPathInFolder(packagePath, pattern.Directory) then
        begin
          if not warned then
            logger.Warning(TWarningCode.W5012_PathOutsidePackage, 'Ignoring the copyToBin pattern [' + copyEntry.Source + '] of [' + packageInfo.Id + '] - it resolves outside the package folder.');
          warned := true;
          continue;
        end;
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
            logger.Debug('Copied [' + ExtractFileName(srcFile) + '] to bpl for package [' + packageInfo.Id + '].');
          except
            on e : Exception do
              logger.Warning(TWarningCode.W5014_CopyFileFailed, 'Unable to copy [' + srcFile + '] to the bpl folder : ' + e.Message);
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
    bomNode := TBOMFile.LoadFromFile(logger, bomFile);
    if (bomNode <> nil) and bomNode.AreEqual(packageReference) then
    begin
      logger.Verbose('Package [' + packageInfo.Id + '] [' + DPMPlatformToString(effectivePlatform) + '] - dependencies unchanged, skipping compilation.');
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

  // Before anything reads, generates, patches or builds a project.
  if not CheckProjectPaths then
    exit(false);

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
      designProjectEditor := TProjectEditor.Create(TDemotingLogger.Create(logger), nil, Compiler.compilerVersion);
      if designProjectEditor.LoadProject(projectFile, [TProjectElement.Platforms]) then
      begin
        candidates := designProjectEditor.Platforms;
        if candidates = [] then
          logger.Warning(TWarningCode.W5015_DesignProjectHasNoPlatforms, 'The design project [' + designEntry.Project + '] declares no platforms DPM supports - no design package will be built.');
      end
      else
      begin
        //why is in the debug log (eg file missing or invalid xml).
        logger.Warning(TWarningCode.W5016_DesignProjectUnreadable, 'Unable to inspect the design project [' + designEntry.Project + '] - building it for Win32 only.');
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

    logger.Information('Building project: ' + buildEntry.Project);
    projectFile := ResolveProjectFile(buildEntry.Project);

    if not ProjectIsSafeToBuild(projectFile, buildEntry.Project) then
    begin
      logger.NewLine;
      exit(false);
    end;

    EnsureBuildTarget(projectFile, buildEntry.Project, [TProjectPatchOption.UpdatePlatformList]);

    Compiler.SetSearchPaths(ResolveEntrySearchPaths(buildEntry.SearchPaths));
    result := Compiler.BuildProject(cancellationToken, effectivePlatform, projectFile, configuration, packageInfo.Version, false);
    if result then
      logger.Success('Project [' + buildEntry.Project + '] build succeeded.')
    else
    begin
      //the compiler said why.
      logger.NewLine;
      exit;
    end;
    // Trailing blank line so each project's output is a visually distinct block - the header for the
    // next project (or the next package/platform) then starts against white space rather than ending it.
    logger.NewLine;
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
        logger.Error(TErrorCode.E5006_PrebuiltDesignPackageInvalid, 'Package [' + packageInfo.Id + '] : ' + prebuiltError);
        logger.NewLine;
        exit(false);
      end;
      if not FileExists(projectFile) then
      begin
        logger.Error(TErrorCode.E5007_PrebuiltDesignPackageMissing, 'The prebuilt design package [' + designEntry.Project + '] of [' + packageInfo.Id +
                      '] was not found in the package - check the dspec ships it via a source entry.');
        logger.NewLine;
        exit(false);
      end;
      logger.Information('Using prebuilt design package: ' + designEntry.Project + ' (' + DPMPlatformToString(designPlatform) + ' IDE)');
      logger.NewLine;
      continue;
    end;

    supportedByCompiler := DesignTimePlatforms(Compiler.compilerVersion);

    //candidates were captured above, before any dproj was patched.
    designPlatforms := supportedByCompiler * designCandidates[LowerCase(designEntry.Project)];

    if not (effectivePlatform in designPlatforms) then
    begin
      logger.Debug('Skipping design package [' + designEntry.Project + '] - ' + DPMPlatformToString(effectivePlatform) + ' is not a supported design platform for this entry.');
      continue;
    end;

    logger.Information('Building design package: ' + designEntry.Project + ' (' + DPMPlatformToString(effectivePlatform) + ')');

    if not ProjectIsSafeToBuild(projectFile, designEntry.Project) then
    begin
      logger.NewLine;
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
      //the compiler said why.
      logger.NewLine;
      exit;
    end;
    logger.Success('Design package [' + designEntry.Project + '] build succeeded.');
    logger.NewLine;
  end;

  // Copy any copyToBin files (native dlls) into bpl\{platform} for the platform just built.
  CopyCopyToBinFiles;

  // Save the bill of materials file for future reference
  TBOMFile.SaveToFile(logger, bomFile, packageReference);
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
  nodes : IList<IPackageReference>;
  node : IPackageReference;
  key : string;
  packageFolder : string;
  lookups : IDictionary<string, TReceiptLookup>;
  lookup : TReceiptLookup;
  receipt : TVerificationReceipt;
  reported : ISet<string>;
  expected : string;
  problem : string;
begin
  result := true;
  if (graph = nil) or (FReceiptService = nil) or (FPackageCache = nil) then
    exit;

  //every node - the project records (and locks) its transitive packages too. A package a diamond reaches
  //twice is one receipt and, with the same lock, one report.
  nodes := TCollections.CreateList<IPackageReference>;
  graph.VisitDFS(
    procedure(const packageReference : IPackageReference)
    begin
      nodes.Add(packageReference);
    end);
  lookups := TCollections.CreateDictionary<string, TReceiptLookup>;
  reported := TCollections.CreateSet<string>;

  for node in nodes do
  begin
    key := LowerCase(node.Id) + '|' + node.Version.ToStringNoMeta;
    if not lookups.TryGetValue(key, lookup) then
    begin
      packageFolder := FPackageCache.GetPackagePath(node.Id, node.Version.ToStringNoMeta, compilerVersion);
      lookup.FolderExists := DirectoryExists(packageFolder);
      lookup.HaveReceipt := lookup.FolderExists and FReceiptService.TryRead(packageFolder, receipt);
      if lookup.HaveReceipt then
        lookup.ManifestHashHex := LowerCase(receipt.ManifestHashHex)
      else
        lookup.ManifestHashHex := '';
      lookups[key] := lookup;
    end;

    // locked nodes whose folder is absent are skipped - the install path will produce them.
    // when ignoreLocks is set we skip validation entirely and just refresh the locks below.
    if (node.ManifestHash = '') or (not lookup.FolderExists) or ignoreLocks then
      Continue;
    expected := LowerCase(node.ManifestHash);
    if (Length(expected) > 7) and (Copy(expected, 1, 7) = 'sha256:') then
      expected := Copy(expected, 8, MaxInt);
    if lookup.HaveReceipt and (expected = lookup.ManifestHashHex) then
      Continue;

    result := false;
    if not reported.Add(key + '|' + expected) then
      Continue;
    if not lookup.HaveReceipt then
      problem := 'the package cache has no receipt for it.'
    else
      problem := 'the package cache has sha256:' + lookup.ManifestHashHex + '. Edit the dproj''s PackageReference or remove the cached package to re-acquire it.';
    FLogger.Error(TErrorCode.E3007_LockHashMismatch, Format('Lock hash mismatch for [%s %s] : the project pins %s but %s',
      [node.Id, node.Version.ToStringNoMeta, node.ManifestHash, problem]));
    if (mismatchedPackageIds <> nil) and (not mismatchedPackageIds.Contains(node.Id)) then
      mismatchedPackageIds.Add(node.Id);
  end;

  // a lock mismatch aborts before any hash is written.
  if not result then
    exit;

  // folder-absent nodes are left untouched. A package without a receipt (a git registry install) or one
  // that predates receipts gets no lock rather than a wrong one.
  for node in nodes do
  begin
    lookup := lookups[LowerCase(node.Id) + '|' + node.Version.ToStringNoMeta];
    if not lookup.FolderExists then
      Continue;
    if lookup.ManifestHashHex <> '' then
      node.ManifestHash := 'sha256:' + lookup.ManifestHashHex
    else
      node.ManifestHash := '';
  end;
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
    //the cache says why it couldn't.
    if not FPackageCache.InstallPackageFromFile(cancellationToken, Options.PackageFile, Options.SkipTrustRatchets) then
      exit;
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
    if cancellationToken.IsCancelled then
      exit;
    if Options.PackageFile <> '' then
      FLogger.Error(TErrorCode.E3009_PackageSpecUnavailable, 'Unable to read the package details of [' + Options.PackageFile + '] from the package cache.')
    else if not Options.Version.IsEmpty then
      ReportNotFound('The package [' + Options.packageId + '-' + Options.Version.ToStringNoMeta + '] was not found for compiler ' + CompilerToString(Options.compilerVersion))
    else
      ReportNotFound('The package [' + Options.packageId + '] was not found for compiler ' + CompilerToString(Options.compilerVersion));
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
  //the resolver reported the conflicts.
  if not FDependencyResolver.ResolveForInstall(cancellationToken, Options.CompilerVersion, cCachePseudoProject, Options, packageInfo, projectReferences, dependencyGraph, resolvedPackages) then
    exit;

  if (resolvedPackages = nil) or (resolvedPackages.Count = 0) then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'The resolver returned no packages for [' + packageInfo.ToString + '].');
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
    FLogger.Error(TErrorCode.E3006_PackageDeclaresNoPlatforms, 'The package [' + packageInfo.ToString + '] does not declare any supported platforms - nothing to build.');
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
    //the build said why.
    if not BuildDependencies(cancellationToken, FLogger, packageCompiler, dependencyGraph, packagesToCompile, compiledPackages, packageSpecs, Options) then
      exit;
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
          raise EDPMError.Create(TErrorCode.E0003_InternalError, 'The package reference [' + dependency.Id + '] was not found in the lookup.');
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
          if not cancellationToken.IsCancelled then
            ReportNotFound('The package [' + dependency.Id + '-' + dependency.Version.ToStringNoMeta + '] that the project references was not found in the package cache or on any source');
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

function TPackageInstaller.ResolveRestoreGraph(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const projectFile : string;
                                               const projectEditor : IProjectEditor; out projectGraph : IPackageReference;
                                               out resolvedPackages : IList<IPackageInfo>) : boolean;
var
  graphState : TRestoreGraphState;
  preferredVersions : IDictionary<string, TPackageVersion>;
  topLevelGraph : IPackageReference;
  locks : IDictionary<string, TRecordedLock>;
begin
  result := false;
  resolvedPackages := TCollections.CreateList<IPackageInfo>;
  projectGraph := projectEditor.GetPackageReferences;
  // can return nil - no packages, nothing to do.
  if projectGraph = nil then
    exit(true);

  //Fast path: validate the existing graph against each package's declared dependencies.
  //If it's intact, skip the resolver entirely and proceed straight to download+configure.
  graphState := ValidateRestoreGraph(cancellationToken, projectGraph, options.CompilerVersion, resolvedPackages);
  //ValidateRestoreGraph has said why (or the restore was cancelled).
  if graphState = rgsInvalid then
    exit;
  if graphState = rgsValid then
  begin
    FLogger.Verbose('Restore: existing graph is consistent with package specs - skipping resolution');
    exit(true);
  end;

  //Slow path: graph has missing transients or out-of-range versions. Re-resolve from top-level
  //constraints by treating each top-level package as a fresh install, via ResolveForInstall.
  //The dproj acts as a lock file - collect every recorded (id, version) from the existing graph
  //so the resolver re-uses those exact versions where they still satisfy the parent's range, and
  //only picks a new version for genuinely missing transients.
  preferredVersions := CollectGraphVersions(projectGraph);
  locks := CollectGraphLocks(projectGraph, nil);
  topLevelGraph := BuildTopLevelOnlyGraph(projectGraph, options.CompilerVersion);
  result := RestoreUsingInstallResolver(cancellationToken, options, projectFile, topLevelGraph, preferredVersions, projectGraph, resolvedPackages);
  if result then
    ApplyGraphLocks(projectGraph, locks);
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

function TPackageInstaller.BuildDependencies(const cancellationToken : ICancellationToken; const logger : ILogger; const packageCompiler: ICompiler; const projectPackageGraph: IPackageReference;
                                             const packagesToCompile: IList<IPackageInfo>; const compiledPackages: IList<IPackageInfo>;
                                             const packageSpecs: IDictionary<string, IPackageSpec>; const Options: TSearchOptions): boolean;
var
  failed : boolean;
begin
  //CompilePackage has said why a package didn't build - nothing more is built once one fails.
  failed := false;
  // build the dependency graph in the correct order.
  projectPackageGraph.VisitDFS(
    procedure(const packageReference: IPackageReference)
    var
      pkgInfo: IPackageInfo;
      Spec: IPackageSpec;
      template: ISpecTemplate;
      forceCompile: boolean;
    begin
      if failed or cancellationToken.IsCancelled then
        exit;
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

        if not CompilePackage(cancellationToken, logger, packageCompiler, pkgInfo, packageReference, Spec, forceCompile, false) then
        begin
          failed := true;
          exit;
        end;

        compiledPackages.Add(pkgInfo);
      end;
    end);
  result := (not failed) and (not cancellationToken.IsCancelled);
end;


//The platform the IDE loads design packages for - the plugin's own bitness tells us which BPL variant
//the IDE will load (the same decision made in TDPMIDEPackageInstallerContext.InstallDesignPackages).
function DesignHostPlatform : TDPMPlatform;
begin
  if TSystemUtils.Is64BitIDE then
    result := TDPMPlatform.Win64
  else
    result := TDPMPlatform.Win32;
end;

procedure TPackageInstaller.LoadDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const graph : IPackageReference;
                                               const compilerVersion : TCompilerVersion; const failedPlatforms : TDPMPlatforms);
begin
  //The design BPLs are built for the IDE's own platform - when that build failed there are none to load,
  //and trying would only add a 'not found' for each. The CLI loads none anyway.
  if TSystemUtils.IsIDEProcess and (DesignHostPlatform in failedPlatforms) then
  begin
    FLogger.Information('The design-time packages were not loaded - the [' + DPMPlatformToString(DesignHostPlatform) +
      '] build, which the IDE loads them from, failed.', true);
    exit;
  end;
  InstallDesignPackages(cancellationToken, projectFile, BuildPackageSpecsFromGraph(cancellationToken, graph, compilerVersion));
end;

procedure TPackageInstaller.EnsureDesignHostPlatformCompiled(const cancellationToken : ICancellationToken; const options : TSearchOptions;
                                                             const graph : IPackageReference; const installedPlatforms : TDPMPlatforms);
var
  hostPlatform : TDPMPlatform;
  quietLogger : IDemotingLogger;
  packageCompiler : ICompiler;
  packageSpecs : IDictionary<string, IPackageSpec>;
  resolvedPackages : IList<IPackageInfo>;
  compiledPackages : IList<IPackageInfo>;
  infoCache : IDictionary<string, IPackageInfo>;
begin
  //CLI never loads design packages - InstallDesignPackages is a no-op there, so the host-platform
  //compile would be wasted work.
  if not TSystemUtils.IsIDEProcess then
    exit;

  hostPlatform := DesignHostPlatform;

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

  packageSpecs := BuildPackageSpecsFromGraph(cancellationToken, graph, options.compilerVersion);
  //not the operation's failure : the project's own platforms are built, a failure here just means the
  //design components may not load - so its problems become one warning.
  quietLogger := TDemotingLogger.Create(FLogger);
  packageCompiler := FCompilerFactory.CreateCompiler(options.compilerVersion, hostPlatform, quietLogger);
  compiledPackages := TCollections.CreateList<IPackageInfo>;

  FLogger.Information('Compiling design-time packages for the IDE host platform [' + DPMPlatformToString(hostPlatform) + '].', true);
  //Cache-only : BuildDependencies populates bpl\{host} and lib\{host} in the cache. We deliberately do
  //not call CollectSearchPaths/CopyLocal/AddSearchPaths - the host platform is not a project target.
  if (not BuildDependencies(cancellationToken, quietLogger, packageCompiler, graph, resolvedPackages, compiledPackages, packageSpecs, options)) and
     (not cancellationToken.IsCancelled) then
    FLogger.Warning(TWarningCode.W5010_DesignHostCompileFailed, 'Unable to compile the design-time packages for the IDE host platform [' + DPMPlatformToString(hostPlatform) +
                    '] - design components may not load. The first problem was : ' + quietLogger.FirstProblem);
end;

function TPackageInstaller.EnsurePackageInCache(const cancellationToken: ICancellationToken; const packageInfo: IPackageInfo): boolean;
var
  packageFileName: string;
  sourceType: TSourceType;
  downloadInfo: IPackageInfo;
begin
  result := false;

  //git registry packages are installed in place (clone + dspec) rather than
  //downloaded as a .dpkg. The repository handles cache freshness (incl. HEAD
  //tracking for untagged repos) so we always call it.
  if FRepositoryManager.TryGetSourceType(packageInfo.SourceName, sourceType) and (sourceType = TSourceType.GitRegistry) then
  begin
    //the repository says why it couldn't.
    result := FRepositoryManager.InstallPackageInPlace(cancellationToken, packageInfo, FPackageCache.GetPackagePath(packageInfo));
    exit;
  end;

  if FPackageCache.EnsurePackage(cancellationToken, packageInfo) then
    exit(true);

  // not in the cache, so we need to get it from the repository - the repository manager says why it
  // couldn't, and the cache why it couldn't use the download.
  //
  // A download is checked against the hash its source publishes. A package info read from the cache (no
  // source name) carries the cache's own record of the hash instead - the very thing that just proved
  // unusable - so ask the sources for theirs.
  downloadInfo := packageInfo;
  if packageInfo.SourceName = '' then
  begin
    downloadInfo := FRepositoryManager.GetPackageInfo(cancellationToken, packageInfo);
    if cancellationToken.IsCancelled then
      exit;
    if downloadInfo = nil then
      downloadInfo := packageInfo;
  end;
  if not FRepositoryManager.DownloadPackage(cancellationToken, downloadInfo, FPackageCache.GetPackageFileFolder(packageInfo), packageFileName) then
    exit;
  result := FPackageCache.InstallPackageFromFile(cancellationToken, packageFileName);
end;

function TPackageInstaller.DownloadPackages(const cancellationToken : ICancellationToken; const resolvedPackages: IList<IPackageInfo>; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
var
  packageInfo: IPackageInfo;
  spec: IPackageSpec;
  errorMark: integer;
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
      errorMark := FLogger.ErrorCount;
      spec := FPackageCache.GetPackageSpec(cancellationToken, packageInfo);
      if spec = nil then
      begin
        //the cache (or the spec reader) may already have said why.
        if FLogger.ErrorCount = errorMark then
          FLogger.Error(TErrorCode.E3009_PackageSpecUnavailable, 'Unable to read the spec of [' + packageInfo.ToString + '] from the package cache.');
        exit;
      end;
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
    FLogger.Warning(TWarningCode.W3011_PlatformNotSupportedByPackages, 'Platform [' + DPMPlatformToString(platform) + '] not supported by package(s): ' + unsupportedList +
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

  if not BuildDependencies(cancellationToken, FLogger, packageCompiler, projectPackageGraph, packagesToCompile, compiledPackages, packageSpecs, options) then
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

  if not CollectSearchPaths(cancellationToken, projectPackageGraph, supportedPackages, compiledPackages, projectEditor.compilerVersion, platform, packageSearchPaths) then
    exit;

  //Copy-local runs at build time (the output dir is only reliably known then), not here. At
  //restore/install we just ensure the targets file exists in the cache root and the project imports
  //it, so its DPMCopyLocal target (hooked into $(BuildDependsOn)) can invoke 'dpm copylocal'.
  EnsureCopyLocalTargets(config.PackageCacheLocation, GetDpmExePathForTargets, FLogger);
  if not projectEditor.EnsureCopyLocalImport then
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
    FLogger.Error(TErrorCode.E0003_InternalError, 'The resolver returned no packages.');
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

  //the editor says why it couldn't.
  projectEditor := TProjectEditor.Create(FLogger, config, compilerVersion);
  if not projectEditor.LoadProject(fullPath) then
    exit;

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
        FLogger.Warning(TWarningCode.W4011_ProjectNotFoundInList, 'The project [' + explicitProjects[i] + '] does not exist.', true);
    end;
    if projectList.Count = 0 then
    begin
      FLogger.Error(TErrorCode.E4002_NoProjectsFound, 'None of the listed project files exist.');
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
      FLogger.Error(TErrorCode.E4002_NoProjectsFound, 'No project files (.dproj) were found in [' + fullPath + '].');
      exit;
    end;
    FLogger.Information('Found ' + IntToStr(projectList.Count) + ' project file(s).');
    result := true;
    exit;
  end;

  FLogger.Error(TErrorCode.E4001_ProjectNotFound, 'The project path [' + projectPath + '] does not exist.');
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
    FLogger.Warning(TWarningCode.W4009_ProjectVersionAmbiguous, 'The ProjectVersion [' + projectEditor.ProjectVersion + '] of [' + projectEditor.ProjectFile + '] is ambiguous (' + ambiguousVersions + ') - specify the compiler version on the command line.');

  // If the compiler version was specified (either on the command line or through a package file)
  // then make sure our dproj is actually for that version.
  if options.compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    if projectEditor.compilerVersion <> options.compilerVersion then
    begin
      if not ambiguousProjectVersion then
        FLogger.Warning(TWarningCode.W4010_ProjectVersionMismatch, 'The ProjectVersion [' + projectEditor.ProjectVersion + '] of [' + projectEditor.ProjectFile + '] does not match the compiler version ' + CompilerToString(options.compilerVersion) + '.');
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
      FLogger.Warning(TWarningCode.W4008_ProjectPlatformsNotTargeted, 'Skipping the project [' + projectEditor.ProjectFile + '] - it does not target the requested platforms.');
      shouldSkip := true;
    end;
  end
  else
    effectivePlatforms := projectEditor.platforms;
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

  //the repository manager says why a source isn't valid.
  if not FRepositoryManager.Initialize(result) then
    exit(nil);
  FDependencyResolver.Initialize(result);

  if not FRepositoryManager.HasSources then
  begin
    FLogger.Error(TErrorCode.E1001_NoSourcesConfigured, 'No package sources are configured. Use the `dpm sources add` command to add one.');
    exit(nil);
  end;


end;

function TPackageInstaller.Install(const cancellationToken: ICancellationToken; const Options: TInstallOptions; const context: IPackageInstallerContext): boolean;
var
  plan : IInstallPlan;
  config : IConfiguration;
  works : IList<TProjectInstallWork>;
  i : integer;
  workFile : string;
  guard : TOperationGuard;
begin
  result := false;
  guard := TOperationGuard.Start(FLogger);
  //one request cache across planning and applying - see TRepositoryRequestCache.
  FRepositoryManager.BeginRequestCache;
  try
    try
      if DoPlanInstall(cancellationToken, Options, plan, config, works) then
      begin
        LogPlanSummary(plan);
        if CanApplyPlan(plan) then
        begin
          //nothing can change between planning and applying here (the IDE asks first, so it loads them
          //again) - apply to the projects planning loaded, leaving out the ones the plan doesn't change.
          for i := works.Count - 1 downto 0 do
          begin
            workFile := works[i].ProjectFile;
            if not plan.Projects.Any(
              function(const projectFile : string) : boolean
              begin
                result := SameText(projectFile, workFile);
              end) then
              works.Delete(i);
          end;
          result := ApplyInstallWork(cancellationToken, Options, plan, config, works);
        end;
      end
      else
        LogPlanErrors(FLogger, plan);
    except
      on e: Exception do
        result := guard.Failed(e);
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
  guard.Finish(result, cancellationToken.IsCancelled, 'Install');
end;

function TPackageInstaller.PlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan) : boolean;
var
  guard : TOperationGuard;
  config : IConfiguration;
  works : IList<TProjectInstallWork>;
begin
  plan := nil;
  guard := TOperationGuard.Start(FLogger);
  FRepositoryManager.BeginRequestCache;
  try
    try
      //ApplyInstallPlan loads the projects again - the user may change things before confirming the plan.
      result := DoPlanInstall(cancellationToken, options, plan, config, works);
    except
      on e: Exception do
      begin
        LogException(FLogger, e);
        plan := CreateFailedPlan;
        result := false;
      end;
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
  //the plan's own errors are reported by whoever shows the plan (LogPlanErrors) - only a plan that failed
  //without one, or saying anything, is this guard's business.
  if (plan = nil) or (not plan.Errors.Any) then
    guard.Finish(result, cancellationToken.IsCancelled, 'Planning the install');
end;

function TPackageInstaller.ApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan; const context : IPackageInstallerContext) : boolean;
var
  guard : TOperationGuard;
begin
  guard := TOperationGuard.Start(FLogger);
  FRepositoryManager.BeginRequestCache;
  try
    try
      result := DoApplyInstallPlan(cancellationToken, options, plan);
    except
      on e: Exception do
        result := guard.Failed(e);
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
  guard.Finish(result, cancellationToken.IsCancelled, 'Install');
end;

procedure TPackageInstaller.ReportNotFound(const description : string);
begin
  if FRepositoryManager.UnavailableSources <> '' then
    FLogger.Error(TErrorCode.E1102_PackageNotFoundSourcesUnavailable, description + ' - the source(s) [' + FRepositoryManager.UnavailableSources + '] could not be used, see the warning(s) above.')
  else
    FLogger.Error(TErrorCode.E1101_PackageNotFound, description + '.');
end;

procedure TPackageInstaller.LogPlanSummary(const plan : IInstallPlan);
var
  line : string;
begin
  //companions are packages the user didn't ask for - make sure they are seen.
  for line in plan.SummaryLines do
    FLogger.Information(line, plan.HasCompanions);
end;

function TPackageInstaller.DoPlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan;
                                         out config : IConfiguration; out works : IList<TProjectInstallWork>) : boolean;
var
  projectFiles : IList<string>;
  work : TProjectInstallWork;
  compilerVersion : TCompilerVersion;
  requested : IList<IPackageInfo>;
  planProjects : IList<TInstallPlanProject>;
  planProject : TInstallPlanProject;
begin
  result := false;
  plan := nil;
  works := nil;
  //every step below that fails has said why - those plans carry no errors of their own.
  config := Init(options);
  if config = nil then
  begin
    plan := CreateFailedPlan;
    exit;
  end;

  projectFiles := TCollections.CreateList<string>;
  if not ResolveProjectFiles(options.ProjectPath, options.Projects, projectFiles) then
  begin
    plan := CreateFailedPlan;
    exit;
  end;

  if options.PackageFile <> '' then
  begin
    if not FileExists(options.PackageFile) then
    begin
      // should never happen if validation is called on the options.
      plan := CreateFailedPlan(TErrorCode.E3004_PackageFileNotFound, 'The package file [' + options.PackageFile + '] does not exist.');
      exit;
    end;
    if not PreparePackageFileInstall(cancellationToken, options) then
    begin
      plan := CreateFailedPlan;
      exit;
    end;
  end;
  //callers that set PackageId/Version directly rather than a list install that one package.
  if (Length(options.Packages) = 0) and (options.PackageId <> '') then
    options.AddPackage(options.PackageId, options.Version);

  works := TCollections.CreateObjectList<TProjectInstallWork>(true);
  if not LoadInstallWork(cancellationToken, options, projectFiles, config, works, compilerVersion) then
  begin
    plan := CreateFailedPlan;
    exit;
  end;

  requested := TCollections.CreateList<IPackageInfo>;
  if not LookupRequestedPackages(cancellationToken, options, compilerVersion, requested) then
  begin
    plan := CreateFailedPlan;
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

    //the editor says why it couldn't.
    projectEditor := TProjectEditor.Create(FLogger, config, compilerVersion);
    if not projectEditor.LoadProject(projectFile) then
      exit;
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
      if cancellationToken.IsCancelled then
        exit(false);
      if not request.Version.IsEmpty then
        ReportNotFound('The package [' + request.PackageId + '-' + request.Version.ToStringNoMeta + '] was not found for compiler ' + CompilerToString(compilerVersion))
      else
        ReportNotFound('The package [' + request.PackageId + '] was not found for compiler ' + CompilerToString(compilerVersion));
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

function TPackageInstaller.CanApplyPlan(const plan : IInstallPlan) : boolean;
begin
  result := false;
  if (plan = nil) or (not plan.Succeeded) then
  begin
    LogPlanErrors(FLogger, plan);
    exit;
  end;
  if not plan.Projects.Any then
  begin
    FLogger.Error(TErrorCode.E3001_NothingToInstall, 'There is nothing to install - none of the projects need the package(s).');
    exit;
  end;
  result := true;
end;

function TPackageInstaller.DoApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan) : boolean;
var
  config : IConfiguration;
  works : IList<TProjectInstallWork>;
  compilerVersion : TCompilerVersion;
begin
  result := false;
  if not CanApplyPlan(plan) then
    exit;

  config := Init(options);
  if config = nil then
    exit;

  works := TCollections.CreateObjectList<TProjectInstallWork>(true);
  if not LoadInstallWork(cancellationToken, options, plan.Projects, config, works, compilerVersion) then
    exit;
  result := ApplyInstallWork(cancellationToken, options, plan, config, works);
end;

function TPackageInstaller.ApplyInstallWork(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan;
                                            const config : IConfiguration; const works : IList<TProjectInstallWork>) : boolean;
var
  work : TProjectInstallWork;
  projectFiles : IList<string>;
  failedProjects : string;
begin
  result := false;
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
    //the resolver reported the conflicts - this is the outcome.
    if failedProjects <> '' then
    begin
      FLogger.Information('Dependency resolution failed for [' + failedProjects + '] - no project was changed.', true);
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
        //what went wrong was reported where it happened - this is the outcome.
        if not cancellationToken.IsCancelled then
          FLogger.Information('Install failed for [' + work.ProjectFile + '] - no project was changed.', true);
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

  //The project's locks on the packages it keeps still apply - the packages being installed get new ones.
  work.Locks := CollectGraphLocks(projectGraph, excludeIds);

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
      FLogger.Error(TErrorCode.E0003_InternalError, 'The resolver did not return the package being installed [' + packageInfo.Id + '].');
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
  builtPlatforms : TDPMPlatforms;
  failedPlatforms : TDPMPlatforms;
begin
  result := false;
  if not PrepareProjectPackages(cancellationToken, work.ProjectFile, work.Graph, work.Resolved, packagesToConfigure, packageSpecs) then
    exit;

  // P2 §2.6 — the packages are in the cache : what the project already pinned must be what is there before
  // any of it is built (a build runs the package's own project), and the rest are locked to the receipts.
  ApplyGraphLocks(work.Graph, work.Locks);
  if not ValidateAndPopulateManifestHashes(work.Graph, work.Options.CompilerVersion, false, nil) then
  begin
    FLogger.Information('Nothing was built and [' + work.ProjectFile + '] was not changed - the package cache no longer holds what the ' +
      'project pinned (see the errors above). Restore the project with --ignoreHashLocks (-ihl) to accept what the cache holds, ' +
      'or remove the package from the cache to fetch it again.', true);
    exit;
  end;

  builtPlatforms := [];
  failedPlatforms := [];
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
    //What went wrong was reported where it happened. A platform that can't be built (no SDK for it, say)
    //mustn't cost the others their install, so carry on - a failed platform's search path is only ever
    //written once it has built, so it is left as it was.
    if platformResult then
      Include(builtPlatforms, platform)
    else
    begin
      if cancellationToken.IsCancelled then
        exit;
      Include(failedPlatforms, platform);
    end;
  end;
  work.FailedPlatforms := failedPlatforms;
  //nothing built - nothing worth recording, so the project is left alone.
  if (failedPlatforms <> []) and (builtPlatforms = []) then
    exit;

  //Make sure the IDE host platform's design BPL exists before design packages are loaded at commit -
  //the loop above only built the project's target platforms. Non-fatal : a failure here just means the
  //design components may not load. (When the host platform was one of them and failed, it isn't tried
  //again - LoadDesignPackages leaves the design packages out.)
  EnsureDesignHostPlatformCompiled(cancellationToken, work.Options, work.Graph, work.Platforms);

  // Update package references once using the new platform-independent format
  work.Editor.UpdatePackageReferences(work.Graph);
  result := true;
end;

function TPackageInstaller.CommitProjectInstall(const cancellationToken : ICancellationToken; const work : TProjectInstallWork) : boolean;
begin
  //the editor says why it couldn't.
  result := work.Editor.SaveProject;
  if not result then
    exit;
  //the dproj now reflects the install, so its resolutions and graph count for the other projects.
  FContext.CommitProject(work.ProjectFile);
  //Load design-time packages once per project - design BPLs are per-IDE, not per-platform. CLI context is a no-op.
  LoadDesignPackages(cancellationToken, work.ProjectFile, work.Graph, work.Options.CompilerVersion, work.FailedPlatforms);
  if work.FailedPlatforms = [] then
  begin
    FLogger.Success('Install succeeded for [' + work.ProjectFile + ']', true);
    exit;
  end;
  //the failures were reported where they happened - this is the outcome.
  FLogger.Information('Install failed for [' + work.ProjectFile + '] on platforms [' + DPMPlatformsToString(work.FailedPlatforms) +
    '] - the project was changed for the platforms that built. Restore it once the problem is fixed to finish those platforms.', true);
  result := false;
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
  hint : string;
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
  //one warning - it is one piece of advice.
  hint := 'Other installed packages need to move with this install :';
  for item in upgradePlan.Items do
  begin
    if item.IsCompanion then
      hint := hint + sLineBreak + '  ' + item.PackageInfo.Id + ' ' + item.VersionChangeText + ' (' + item.Reason + ')';
  end;
  hint := hint + sLineBreak + 'Use Upgrade in the IDE, or --upgrade on the command line, to install them together.';
  FLogger.Warning(TWarningCode.W2012_CompanionsRequired, hint, true);
end;

function TPackageInstaller.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
begin
  //Note : we delegate this to the context as this is a no-op in the command line tool, the IDE plugin provides it's own context implementation.
  result := FContext.InstallDesignPackages(cancellationToken, projectFile, packageSpecs);
end;

function TPackageInstaller.BuildPackageSpecsFromGraph(const cancellationToken: ICancellationToken; const graph: IPackageReference; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageSpec>;
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
      spec := FPackageCache.GetPackageSpec(cancellationToken, identity);
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

function TPackageInstaller.CollectGraphLocks(const graph : IPackageReference; const excludeIds : ISet<string>) : IDictionary<string, TRecordedLock>;
var
  collected : IDictionary<string, TRecordedLock>;
  excluded : ISet<string>;
begin
  collected := TCollections.CreateDictionary<string, TRecordedLock>;
  excluded := excludeIds;
  if graph <> nil then
    graph.VisitDFS(
      procedure(const packageReference : IPackageReference)
      var
        key : string;
        recorded : TRecordedLock;
      begin
        key := LowerCase(packageReference.Id);
        if (packageReference.ManifestHash = '') or collected.ContainsKey(key) then
          exit;
        if (excluded <> nil) and excluded.Contains(key) then
          exit;
        recorded.Version := packageReference.Version.ToStringNoMeta;
        recorded.Hash := packageReference.ManifestHash;
        collected.Add(key, recorded);
      end);
  result := collected;
end;

procedure TPackageInstaller.ApplyGraphLocks(const graph : IPackageReference; const locks : IDictionary<string, TRecordedLock>);
var
  recordedLocks : IDictionary<string, TRecordedLock>;
  moved : ISet<string>;
begin
  if (graph = nil) or (locks = nil) or (locks.Count = 0) then
    exit;
  recordedLocks := locks;
  moved := TCollections.CreateSet<string>;
  graph.VisitDFS(
    procedure(const packageReference : IPackageReference)
    var
      recorded : TRecordedLock;
    begin
      if not recordedLocks.TryGetValue(LowerCase(packageReference.Id), recorded) then
        exit;
      if SameText(recorded.Version, packageReference.Version.ToStringNoMeta) then
        packageReference.ManifestHash := recorded.Hash
      else if moved.Add(LowerCase(packageReference.Id)) then
        FLogger.Information(Format('[%s] is now %s rather than %s - its manifest hash is locked afresh from the package cache.',
          [packageReference.Id, packageReference.Version.ToStringNoMeta, recorded.Version]));
    end);
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
      if not cancellationToken.IsCancelled then
        ReportNotFound('The top level package [' + topLevelChild.Id + '-' + topLevelChild.Version.ToStringNoMeta + '] was not found in the package cache or on any source');
      exit;
    end;

    //the resolver reported the conflicts.
    if not FDependencyResolver.ResolveForInstall(cancellationToken, options.CompilerVersion, projectFile, options,
                                                 topLevelInfo, accumulatedRefs, iterationGraph, iterationResolved, sharedVersionCache, preferredVersions) then
      exit;

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
        FLogger.Error(TErrorCode.E2006_RestoreTopLevelIncompatible, text + ' - a different major version is not compatible.')
      else
        FLogger.Error(TErrorCode.E2006_RestoreTopLevelIncompatible, text + ' - an older version is not compatible.');
      exit(false);
    end;
    //a shared package is visited once for every package that uses it - only say it once.
    key := LowerCase(node.Id + '>' + dep.Id);
    if warned.Contains(key) then
      exit;
    warned.Add(key);
    FLogger.Warning(TWarningCode.W2011_TopLevelVersionNewer, text + ' - using ' + topLevel.Version.ToStringNoMeta + ' (same major version).');
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
        FLogger.Information('Restore: package [' + node.Id + '] requires [' + dep.Id + '] which is missing from the project graph - re-resolving');
        state := rgsNeedsResolve;
        continue;
      end;
      if topLevel <> nil then
      begin
        //the top level node is validated on its own - the graph only has to agree with it.
        if child.Version <> topLevel.Version then
        begin
          FLogger.Information('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] differs from the top level reference [' +
                          topLevel.Id + '-' + topLevel.Version.ToStringNoMeta + '] - re-resolving');
          state := rgsNeedsResolve;
        end;
        continue;
      end;
      if not dep.VersionRange.IsSatisfiedBy(child.Version) then
      begin
        FLogger.Information('Restore: package [' + node.Id + '] dependency [' + child.Id + '-' + child.Version.ToStringNoMeta + '] is outside required range [' + dep.VersionRange.ToString + '] - re-resolving');
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

function TPackageInstaller.PreparePackageFileInstall(const cancellationToken : ICancellationToken; const Options: TInstallOptions): boolean;
var
  packageIdString: string;
  packageIdentity: IPackageIdentity;
  packageSpec: IPackageSpec;
begin
  // get the package into the cache first then just install as normal
  result := FPackageCache.InstallPackageFromFile(cancellationToken, Options.PackageFile);
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
  packageSpec := FPackageCache.GetPackageSpec(cancellationToken, packageIdentity);
  if (packageSpec <> nil) and (packageSpec.TargetPlatform <> nil) then
    Options.platforms := packageSpec.TargetPlatform.Platforms
  else
  begin
    FLogger.Warning(TWarningCode.W3013_PackageSpecUnreadable, 'Unable to read the spec of [' + packageIdentity.ToString + '] - installing for all the project platforms.');
    Options.platforms := [];
  end;
end;

function TPackageInstaller.Remove(const cancellationToken: ICancellationToken; const Options: TUnInstallOptions): boolean;
begin
  result := false;
end;

function TPackageInstaller.Restore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;
var
  guard : TOperationGuard;
  stopwatch : TStopwatch;
begin
  guard := TOperationGuard.Start(FLogger);
  stopwatch := TStopwatch.Create;
  stopwatch.Start;
  //one request cache across the projects - a group's projects look up the same packages.
  FRepositoryManager.BeginRequestCache;
  try
    try
      result := DoRestore(cancellationToken, Options, context);
    except
      on e: Exception do
        result := guard.Failed(e);
    end;
  finally
    FRepositoryManager.EndRequestCache;
  end;
  stopwatch.Stop;
  //what went wrong was reported where it happened - this is the outcome.
  if result then
    FLogger.Success('Restore succeeded in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms', true)
  else if not cancellationToken.IsCancelled then
    FLogger.Information('Restore failed in [' + IntToStr(stopwatch.ElapsedMilliseconds) + '] ms', true);
  guard.Finish(result, cancellationToken.IsCancelled, 'Restore');
end;

function TPackageInstaller.DoRestore(const cancellationToken: ICancellationToken; const Options: TRestoreOptions; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
  projectGuard : TOperationGuard;
  projectResult : boolean;
begin
  result := false;
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
      exit(false);
    projectFile := projectList[i];

    if TPathUtils.IsRelativePath(projectFile) then
    begin
      projectFile := TPath.Combine(GetCurrentDir, projectFile);
      projectFile := TPathUtils.CompressRelativePath(projectFile);
    end;
    //each project is an operation of its own - its errors show even when an earlier project had the same
    //ones, and a project that fails without saying why is still caught.
    projectGuard := TOperationGuard.Start(FLogger);
    projectResult := RestoreProject(cancellationToken, Options, projectFile, config, context);
    projectGuard.Finish(projectResult, cancellationToken.IsCancelled, 'Restoring [' + projectFile + ']');
    // order is important so we avoid shortcut boolean eval
    result := projectResult and result;
  end;
end;

function TPackageInstaller.RestoreProject(const cancellationToken : ICancellationToken; const Options: TRestoreOptions; const projectFile: string; const config: IConfiguration;
                                          const context: IPackageInstallerContext): boolean;
var
  projectEditor: IProjectEditor;
  platforms: TDPMPlatforms;
  platform: TDPMPlatform;
  errorPlatforms : TDPMPlatforms;
  builtPlatforms : TDPMPlatforms;
  platformResult: boolean;
  shouldSkip: boolean;
  platformOptions: TRestoreOptions;
  finalGraph: IPackageReference;
  resolvedPackages : IList<IPackageInfo>;
  packagesToConfigure : IList<IPackageInfo>;
  storedCompiler : TCompilerVersion;
  mismatchedPackageIds : IList<string>;
  packageSpecs : IDictionary<string, IPackageSpec>;
  mismatchedId : string;
begin
  result := false;

  // Load and validate the project file - the editor says why it couldn't.
  if not LoadProjectEditor(projectFile, config, Options.compilerVersion, projectEditor) then
    exit;

  if cancellationToken.IsCancelled then
    exit;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  //a project that doesn't target the requested platforms has nothing to restore - not a failure.
  if shouldSkip then
    exit(true);

  FLogger.Information('Restoring for compiler version  [' + CompilerToString(Options.compilerVersion) + '].', true);

  //The package graph is the project's, not a platform's - validate it (re-resolving when it is stale)
  //and download its packages once, then configure each platform.
  if not ResolveRestoreGraph(cancellationToken, Options, projectFile, projectEditor, finalGraph, resolvedPackages) then
    exit;
  //no packages - nothing to restore.
  if finalGraph = nil then
    exit(true);
  if not PrepareProjectPackages(cancellationToken, projectFile, finalGraph, resolvedPackages, packagesToConfigure, packageSpecs) then
    exit;

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
  // P2 §2.6 — the cached packages must be what the project pinned before any of them is built - a build
  // runs the package's own project. Checked for the whole graph, transitive packages included, then the
  // locks are refreshed from the receipts.
  //
  // A mismatch fails the restore and nothing is built or written - the recorded locks are kept, so it
  // keeps failing until the user resolves it. The design-time BPLs already built for the packages that
  // did validate are still loaded, so one mismatched package doesn't take every other package's design
  // components out of the IDE. The mismatched packages are skipped - their cached content no longer
  // matches what the project pinned.
  mismatchedPackageIds := TCollections.CreateList<string>;
  if not ValidateAndPopulateManifestHashes(finalGraph, Options.compilerVersion, Options.IgnoreHashLocks, mismatchedPackageIds) then
  begin
    FLogger.Information('One or more manifest hash locks did not match the cached packages (see the errors above). ' +
      'Nothing was built and the project file was left unchanged. To resolve: reinstall ' +
      'the affected package(s), delete them from the cache (' + FPackageCache.Location + ') and restore, or ' +
      'restore with --ignoreHashLocks (-ihl) to refresh the recorded locks from the cache.', true);
    packageSpecs := BuildPackageSpecsFromGraph(cancellationToken, finalGraph, Options.compilerVersion);
    for mismatchedId in mismatchedPackageIds do
      packageSpecs.Remove(LowerCase(mismatchedId));
    InstallDesignPackages(cancellationToken, projectFile, packageSpecs);
    exit(false);
  end;

  errorPlatforms := [];
  builtPlatforms := [];
  for platform in platforms do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    // do not modify options here!
    platformOptions := Options.Clone;
    try
      platformOptions.platforms := [platform];
      FLogger.Information('Restoring project [' + projectFile + '] for [' + DPMPlatformToString(platform) + ']', true);
      platformResult := DoConfigurePackageForPlatform(cancellationToken, platformOptions, projectFile, projectEditor, platform, config,
                                                      finalGraph, packagesToConfigure, packageSpecs);
    finally
      platformOptions.Free;
    end;
    if platformResult then
    begin
      Include(builtPlatforms, platform);
      FLogger.Success('Restore succeeded for ' + DPMPlatformToString(platform), true);
    end
    else if cancellationToken.IsCancelled then
      exit(false)
    else
      Include(errorPlatforms, platform);
    FLogger.Information('');
  end;

  //What went wrong was reported where it happened. A platform that can't be built mustn't cost the others
  //their restore - the failed ones' search paths are only ever written once they build, so they are left
  //as they were. Nothing built - nothing worth recording.
  if (errorPlatforms <> []) and (builtPlatforms = []) then
  begin
    FLogger.Information('Restore failed for [' + projectFile + '] on platforms [' + DPMPlatformsToString(errorPlatforms) + ']', true);
    exit;
  end;

  // Update package references once using the new platform-independent format
  projectEditor.UpdatePackageReferences(finalGraph);
  result := projectEditor.SaveProject();
  //Load design-time packages once after all platforms are restored - design BPLs are per-IDE,
  //not per-platform, so this runs at the project level. CLI context is a no-op. A SaveProject
  //failure (disk errors) suppresses it.
  if result then
  begin
    //Make sure the IDE host platform's design BPL exists before we try to load it - the restore
    //loop above only built the project's target platforms. Non-fatal : a failure here just means the
    //design components may not load. (When the host platform was one of them and failed, it isn't
    //tried again - LoadDesignPackages leaves the design packages out.)
    EnsureDesignHostPlatformCompiled(cancellationToken, Options, finalGraph, platforms);
    LoadDesignPackages(cancellationToken, projectFile, finalGraph, Options.compilerVersion, errorPlatforms);
  end;
  //this is the outcome.
  if errorPlatforms <> [] then
  begin
    FLogger.Information('Restore failed for [' + projectFile + '] on platforms [' + DPMPlatformsToString(errorPlatforms) +
      '] - the project was restored for the platforms that built.', true);
    result := false;
  end;
end;

function TPackageInstaller.Uninstall(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
var
  guard : TOperationGuard;
begin
  guard := TOperationGuard.Start(FLogger);
  try
    result := DoUninstall(cancellationToken, Options, context);
  except
    on e : Exception do
      result := guard.Failed(e);
  end;
  guard.Finish(result, cancellationToken.IsCancelled, 'Uninstall');
end;

function TPackageInstaller.DoUninstall(const cancellationToken : ICancellationToken; const Options: TUnInstallOptions; const context: IPackageInstallerContext): boolean;
var
  projectFile: string;
  config: IConfiguration;
  projectList: IList<string>;
  i: integer;
  projectGuard : TOperationGuard;
  projectResult : boolean;
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

  //uninstalling changes the projects and reads the package cache - it doesn't use the sources, so a
  //source that is down (or not valid) doesn't stop it.
  FPackageCache.Location := config.PackageCacheLocation;

  projectList := TCollections.CreateList<string>;
  if not ResolveProjectFiles(Options.ProjectPath, Options.Projects, projectList) then
    exit;

  result := true;
  for i := 0 to projectList.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    projectFile := projectList[i];
    //each project is an operation of its own - a project that fails without saying why is still caught.
    projectGuard := TOperationGuard.Start(FLogger);
    projectResult := UnInstallFromProject(cancellationToken, Options, projectFile, config, context);
    projectGuard.Finish(projectResult, cancellationToken.IsCancelled, 'Uninstalling from [' + projectFile + ']');
    result := projectResult and result;
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

  //the editor says why it couldn't.
  if not projectEditor.LoadProject(projectFile) then
    exit;

  if cancellationToken.IsCancelled then
    exit;

  ValidateAndSetCompilerPlatforms(Options, projectEditor, platforms, shouldSkip);
  //a project that doesn't target the requested platforms has nothing to uninstall - not a failure.
  if shouldSkip then
    exit(true);

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
    FLogger.Warning(TWarningCode.W3014_DesignPackageUnloadFailed, 'Unable to unload the design packages of the removed package(s) - continuing.');

  //TODO : Tell context to upgrade graph (FContext.PackageGraphTrimmed once that exists).

  //Project-level: write the updated graph (single <Packages> node, no platform attribute) and save once.
  projectEditor.UpdatePackageReferences(projectPackageGraph);
  result := projectEditor.SaveProject();
  if result then
    FLogger.Success('Uninstall succeeded for [' + Options.packageId + ']', true);
end;

end.
