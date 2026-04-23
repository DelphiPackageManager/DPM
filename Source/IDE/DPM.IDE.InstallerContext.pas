unit DPM.IDE.InstallerContext;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Spec.Interfaces,
  DPM.IDE.PathManager;

type
  TDPMLoadedBPL = record
    PackageId : string;
    BPLPath : string;
    BPLFolder : string;
    Referencers : IList<string>; //lowercase project paths
  end;

  TDPMProjectBPLRef = record
    PackageId : string; //lowercase
    BPLPath : string;   //lowercase
  end;

  //IDE-only extension of IPackageInstallerContext - methods that only make sense when the
  //Delphi IDE is hosting DPM (no equivalent in the CLI).
  IDPMIDEPackageInstallerContext = interface
    ['{D8A51C2F-4B97-48E6-9C3D-7B5A6F8D19E2}']
    procedure CleanupOrphanedDesignBPLs;
  end;

  TDPMIDEPackageInstallerContext = class(TCorePackageInstallerContext, IPackageInstallerContext, IDPMIDEPackageInstallerContext)
  private
    FPathManager : IDPMIDEPathManager;
    FPackageCache : IPackageCache;
    //key: lowercase bpl path. Record holds IList which is shared across copies - mutations via Referencers list are visible.
    FLoadedBPLs : IDictionary<string, TDPMLoadedBPL>;
    //key: lowercase project file. List preserves load order (post-order DFS across graph, spec order within a package).
    FProjectBPLs : IDictionary<string, IList<TDPMProjectBPLRef>>;
    function ResolveDesignBPLPath(const node : IPackageReference; const designEntry : ISpecDesignEntry; const loadPlatform : TDPMPlatform; out bplPath : string; out bplFolder : string) : boolean;
    function LoadBPLIfNeeded(const bplPath : string; const bplFolder : string; const packageId : string; const lcProject : string) : boolean;
    procedure ReleaseBPLRef(const lcBplPath : string; const lcProject : string);
    procedure ReleaseProjectBPLs(const lcProject : string);
    function IsAlreadyLoadedByIDE(const bplPath : string) : boolean;
  protected
    procedure Clear; override;
    procedure RemoveProject(const projectFile : string); override;
    function InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageManifests : IDictionary<string, IPackageSpec>) : boolean; override;
    function UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean; override;
  public
    constructor Create(const logger : ILogger; const pathManager : IDPMIDEPathManager; const packageCache : IPackageCache); reintroduce;
    procedure CleanupOrphanedDesignBPLs;
  end;

implementation

uses
  ToolsAPI,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.SysUtils,
  DPM.Core.Utils.System;

{ helpers }

procedure RunOnMainThread(const proc : TThreadProcedure);
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    proc()
  else
    TThread.Synchronize(nil, proc);
end;

{ TDPMIDEPackageInstallerContext }

constructor TDPMIDEPackageInstallerContext.Create(const logger : ILogger; const pathManager : IDPMIDEPathManager; const packageCache : IPackageCache);
begin
  inherited Create(logger);
  FPathManager := pathManager;
  FPackageCache := packageCache;
  FLoadedBPLs := TCollections.CreateDictionary<string, TDPMLoadedBPL>;
  FProjectBPLs := TCollections.CreateDictionary<string, IList<TDPMProjectBPLRef>>;
end;

function TDPMIDEPackageInstallerContext.ResolveDesignBPLPath(const node : IPackageReference; const designEntry : ISpecDesignEntry; const loadPlatform : TDPMPlatform; out bplPath : string; out bplFolder : string) : boolean;
var
  packagePath : string;
  projectBase : string;
  libSuffix : string;
  fileName : string;
begin
  result := false;
  bplPath := '';
  bplFolder := '';
  //{cache}\{compiler}\{id}\{version}\bpl\{platform}
  packagePath := FPackageCache.GetPackagePath(node);
  bplFolder := TPath.Combine(packagePath, 'bpl' + PathDelim + DPMPlatformToBDString(loadPlatform));

  //LibSuffix in the spec is optional - when absent, .dpk output uses the compiler's default ($LibSuffix)
  //eg '290' for Delphi 12. Same fallback so the heuristic finds the produced file.
  libSuffix := designEntry.LibSuffix;
  if libSuffix = '' then
    libSuffix := CompilerToLibSuffix(node.CompilerVersion);

  //heuristic: {LibPrefix}{ProjectBaseName}{LibSuffix}.bpl
  //designEntry.Project can use forward slashes - normalise so ExtractFileName works on Windows.
  projectBase := StringReplace(designEntry.Project, '/', PathDelim, [rfReplaceAll]);
  projectBase := ChangeFileExt(ExtractFileName(projectBase), '');
  fileName := designEntry.LibPrefix + projectBase + libSuffix + '.bpl';
  bplPath := TPath.Combine(bplFolder, fileName);

  if not TFile.Exists(bplPath) then
  begin
    FLogger.Warning('Could not locate design BPL for [' + node.Id + '] at [' + bplPath + '] - set LibPrefix/LibSuffix on the design entry to match the compiled .bpl name.');
    exit;
  end;
  result := true;
end;

function TDPMIDEPackageInstallerContext.IsAlreadyLoadedByIDE(const bplPath : string) : boolean;
var
  svc : IOTAPackageServices;
  i : integer;
  loadedName : string;
  lcBpl : string;
begin
  result := false;
  if not Supports(BorlandIDEServices, IOTAPackageServices, svc) then
    exit;
  lcBpl := LowerCase(bplPath);
  for i := 0 to svc.PackageCount - 1 do
  begin
    loadedName := svc.PackageNames[i];
    if SameText(loadedName, bplPath) or SameText(ExtractFileName(loadedName), ExtractFileName(bplPath)) then
    begin
      result := true;
      exit;
    end;
    //some Delphi versions return the package name rather than the filename - best-effort compare.
    if Pos(LowerCase(ExtractFileName(bplPath)), LowerCase(loadedName)) > 0 then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TDPMIDEPackageInstallerContext.LoadBPLIfNeeded(const bplPath : string; const bplFolder : string; const packageId : string; const lcProject : string) : boolean;
var
  lcBpl : string;
  loaded : TDPMLoadedBPL;
  installOk : boolean;
begin
  result := true;
  lcBpl := LowerCase(bplPath);

  if FLoadedBPLs.TryGetValue(lcBpl, loaded) then
  begin
    //Already loaded by us - add this project as a referencer if not already there.
    if loaded.Referencers.IndexOf(lcProject) < 0 then
      loaded.Referencers.Add(lcProject);
    exit;
  end;

  if IsAlreadyLoadedByIDE(bplPath) then
  begin
    FLogger.Information('Design BPL [' + ExtractFileName(bplPath) + '] already loaded by the IDE - not tracking.');
    exit;
  end;

  FPathManager.EnsurePath(bplFolder);

  installOk := false;
  RunOnMainThread(
    procedure
    var
      svc : IOTAPackageServices;
    begin
      try
        if Supports(BorlandIDEServices, IOTAPackageServices, svc) then
          installOk := svc.InstallPackage(bplPath);
      except
        on e : Exception do
        begin
          FLogger.Error('Exception loading design BPL [' + bplPath + '] : ' + e.Message);
          installOk := false;
        end;
      end;
    end);

  if not installOk then
  begin
    FLogger.Error('Failed to load design BPL [' + bplPath + ']');
    FPathManager.RemovePath(bplFolder);
    result := false;
    exit;
  end;

  loaded.PackageId := packageId;
  loaded.BPLPath := bplPath;
  loaded.BPLFolder := bplFolder;
  loaded.Referencers := TCollections.CreateList<string>;
  loaded.Referencers.Add(lcProject);
  FLoadedBPLs[lcBpl] := loaded;
  FLogger.Success('Loaded design BPL [' + ExtractFileName(bplPath) + ']');
end;

procedure TDPMIDEPackageInstallerContext.ReleaseBPLRef(const lcBplPath : string; const lcProject : string);
var
  loaded : TDPMLoadedBPL;
  uninstallOk : boolean;
begin
  if not FLoadedBPLs.TryGetValue(lcBplPath, loaded) then
    exit;

  loaded.Referencers.Remove(lcProject);
  if loaded.Referencers.Count > 0 then
    exit;

  uninstallOk := false;
  RunOnMainThread(
    procedure
    var
      svc : IOTAPackageServices;
    begin
      try
        if Supports(BorlandIDEServices, IOTAPackageServices, svc) then
          uninstallOk := svc.UninstallPackage(loaded.BPLPath);
      except
        on e : Exception do
        begin
          FLogger.Error('Exception unloading design BPL [' + loaded.BPLPath + '] : ' + e.Message);
          uninstallOk := false;
        end;
      end;
    end);

  if not uninstallOk then
    FLogger.Warning('IDE reported failure unloading design BPL [' + ExtractFileName(loaded.BPLPath) + '] - removing from tracking anyway.');

  FPathManager.RemovePath(loaded.BPLFolder);
  FLoadedBPLs.Remove(lcBplPath);
  FLogger.Success('Unloaded design BPL [' + ExtractFileName(loaded.BPLPath) + ']');
end;

procedure TDPMIDEPackageInstallerContext.ReleaseProjectBPLs(const lcProject : string);
var
  projectRefs : IList<TDPMProjectBPLRef>;
  i : integer;
begin
  if not FProjectBPLs.TryGetValue(lcProject, projectRefs) then
    exit;
  //unload in reverse load order so dependants go before their dependencies
  for i := projectRefs.Count - 1 downto 0 do
    ReleaseBPLRef(projectRefs[i].BPLPath, lcProject);
  projectRefs.Clear;
end;

function TDPMIDEPackageInstallerContext.InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageManifests : IDictionary<string, IPackageSpec>) : boolean;
var
  lcProject : string;
  loadPlatform : TDPMPlatform;
  graph : IPackageReference;
  projectRefs : IList<TDPMProjectBPLRef>;
  overallResult : boolean;
begin
  result := true;
  lcProject := LowerCase(projectFile);

  //The plugin's own bitness tells us which BPL variant to load - this is the one legitimate use of
  //Is64BitIDE (IDE-side load decision, not a compile decision).
  if TSystemUtils.Is64BitIDE then
    loadPlatform := TDPMPlatform.Win64
  else
    loadPlatform := TDPMPlatform.Win32;

  graph := GetProjectGraph(projectFile);
  if graph = nil then
  begin
    FLogger.Debug('InstallDesignPackages: no recorded graph for [' + projectFile + '] - nothing to load.');
    exit;
  end;

  //Restore semantics: release anything we were tracking for this project first so the rebuilt
  //list reflects the new graph exactly. BPLs that are still required will be re-added below.
  ReleaseProjectBPLs(lcProject);

  projectRefs := TCollections.CreateList<TDPMProjectBPLRef>;
  FProjectBPLs[lcProject] := projectRefs;

  overallResult := true;

  //Load order: VisitDFS is post-order (children before parent), spec order within a package.
  graph.VisitDFS(
    procedure(const node : IPackageReference)
    var
      spec : IPackageSpec;
      template : ISpecTemplate;
      designEntry : ISpecDesignEntry;
      bplPath : string;
      bplFolder : string;
      ref : TDPMProjectBPLRef;
    begin
      if cancellationToken.IsCancelled then
        exit;
      if not packageManifests.TryGetValue(LowerCase(node.Id), spec) then
        exit;
      if spec.TargetPlatform = nil then
        exit;
      template := spec.FindTemplate(spec.TargetPlatform.TemplateName);
      if template = nil then
        exit;

      for designEntry in template.DesignEntries do
      begin
        if not ResolveDesignBPLPath(node, designEntry, loadPlatform, bplPath, bplFolder) then
          continue;

        if not LoadBPLIfNeeded(bplPath, bplFolder, node.Id, lcProject) then
          overallResult := false;

        ref.PackageId := LowerCase(node.Id);
        ref.BPLPath := LowerCase(bplPath);
        projectRefs.Add(ref);
      end;
    end);

  result := overallResult and not cancellationToken.IsCancelled;
end;

function TDPMIDEPackageInstallerContext.UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean;
var
  lcProject : string;
  projectRefs : IList<TDPMProjectBPLRef>;
  orphanSet : IDictionary<string, integer>;
  orphanedId : string;
  i : integer;
  ref : TDPMProjectBPLRef;
begin
  result := true;
  lcProject := LowerCase(projectFile);

  if not FProjectBPLs.TryGetValue(lcProject, projectRefs) then
    exit;

  if (orphanedPackageIds = nil) or (orphanedPackageIds.Count = 0) then
    exit;

  orphanSet := TCollections.CreateDictionary<string, integer>;
  for orphanedId in orphanedPackageIds do
    orphanSet[LowerCase(orphanedId)] := 1;

  //Reverse load order: dependants before their dependencies, reverse spec order within a package.
  for i := projectRefs.Count - 1 downto 0 do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    ref := projectRefs[i];
    if not orphanSet.ContainsKey(ref.PackageId) then
      continue; //still referenced elsewhere in this project - keep it

    ReleaseBPLRef(ref.BPLPath, lcProject);
    projectRefs.Delete(i);
  end;
end;

procedure TDPMIDEPackageInstallerContext.RemoveProject(const projectFile : string);
var
  lcProject : string;
begin
  lcProject := LowerCase(projectFile);
  ReleaseProjectBPLs(lcProject);
  FProjectBPLs.Remove(lcProject);
  inherited;
end;

procedure TDPMIDEPackageInstallerContext.Clear;
var
  keys : TArray<string>;
  key : string;
begin
  //snapshot keys so we can release without invalidating enumeration
  keys := FProjectBPLs.Keys.ToArray;
  for key in keys do
    ReleaseProjectBPLs(key);
  FProjectBPLs.Clear;
  FLoadedBPLs.Clear; //defensive - should be empty by now
  inherited;
end;

procedure TDPMIDEPackageInstallerContext.CleanupOrphanedDesignBPLs;
//Called on a deferred / post-load trigger (see TDPMStartupCleanupNotifier in DPM.IDE.Wizard),
//NOT from the wizard constructor. By the time this runs, Delphi has already processed Known
//Packages from the registry and loaded the design BPLs - so IOTAPackageServices can see them.
//
//Anything the IDE has loaded from our package cache at this point can only be a leftover from
//a prior (crashed) session - DPM itself hasn't loaded anything yet this session, it only does so
//via LoadBPLIfNeeded when a project opens. IOTAPackageServices.UninstallPackage both unloads the
//BPL and removes it from the Known Packages registry list (see IOTAPackageInfo.SetLoaded docs).
var
  cacheRoot : string;
  cacheRootWithDelim : string;
  orphaned : TArray<string>;
  cleanedCount : integer;
begin
  cacheRoot := FPackageCache.PackagesFolder;
  if cacheRoot = '' then
    exit;
  cacheRootWithDelim := IncludeTrailingPathDelimiter(cacheRoot);
  orphaned := nil;

  //First pass: collect matching paths. Don't uninstall inside the loop - UninstallPackage mutates
  //PackageCount and invalidates the index-based walk.
  RunOnMainThread(
    procedure
    var
      svc : IOTAPackageServices;
      info : IOTAPackageInfo;
      i : integer;
      candidate : string;
      found : IList<string>;
    begin
      if not Supports(BorlandIDEServices, IOTAPackageServices, svc) then
        exit;
      found := TCollections.CreateList<string>;
      for i := 0 to svc.PackageCount - 1 do
      begin
        info := svc.Package[i];
        if info = nil then
          continue;
        //IDE packages are loaded by the IDE itself and cannot be uninstalled by the user - skip
        //defensively even though they won't match our cache path anyway.
        if info.IDEPackage then
          continue;
        candidate := info.FileName;
        if candidate = '' then
          continue;
        if StartsText(cacheRootWithDelim, candidate) then
          found.Add(candidate);
      end;
      orphaned := found.ToArray;
    end);

  if Length(orphaned) = 0 then
    exit;

  cleanedCount := 0;
  RunOnMainThread(
    procedure
    var
      svc : IOTAPackageServices;
      k : integer;
      uninstallOk : boolean;
    begin
      if not Supports(BorlandIDEServices, IOTAPackageServices, svc) then
        exit;
      for k := 0 to Length(orphaned) - 1 do
      begin
        uninstallOk := false;
        try
          uninstallOk := svc.UninstallPackage(orphaned[k]);
        except
          on e : Exception do
            FLogger.Error('Exception cleaning up orphaned design BPL [' + orphaned[k] + '] : ' + e.Message);
        end;
        if uninstallOk then
        begin
          FLogger.Information('Cleaned up orphaned design BPL [' + ExtractFileName(orphaned[k]) + ']');
          Inc(cleanedCount);
        end
        else
          FLogger.Warning('IDE reported failure unloading orphaned design BPL [' + ExtractFileName(orphaned[k]) + ']');
      end;
    end);

  if cleanedCount > 0 then
    FLogger.Success('Cleaned up ' + IntToStr(cleanedCount) + ' orphaned design BPL(s) from previous session');
end;

end.
