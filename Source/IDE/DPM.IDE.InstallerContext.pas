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
  DPM.IDE.PathManager,
  DPM.IDE.EnvironmentVariableManager;

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

  //carries full (original-case) load info through the differential update in InstallDesignPackages -
  //LoadBPLIfNeeded needs the original-case path/folder/id, which TDPMProjectBPLRef does not retain.
  TDPMDesignBPL = record
    PackageId : string;
    BPLPath : string;
    BPLFolder : string;
  end;

  TDPMProjectEnvVarRef = record
    PackageId : string; //lowercase
    Name : string;      //env var name as authored (PATH special-cased by the manager)
    Value : string;     //value with $packageDir$ already resolved
  end;

  TDPMIDEPackageInstallerContext = class(TCorePackageInstallerContext, IPackageInstallerContext)
  private
    FPathManager : IDPMIDEPathManager;
    FEnvVarManager : IDPMIDEEnvironmentVariableManager;
    FPackageCache : IPackageCache;
    //key: lowercase bpl path. Record holds IList which is shared across copies - mutations via Referencers list are visible.
    FLoadedBPLs : IDictionary<string, TDPMLoadedBPL>;
    //key: lowercase project file. List preserves load order (post-order DFS across graph, spec order within a package).
    FProjectBPLs : IDictionary<string, IList<TDPMProjectBPLRef>>;
    //key: lowercase project file. Env vars applied for this project, in apply order.
    FProjectEnvVars : IDictionary<string, IList<TDPMProjectEnvVarRef>>;
    function ResolveDesignBPLPath(const node : IPackageReference; const designEntry : ISpecDesignEntry; const loadPlatform : TDPMPlatform; out bplPath : string; out bplFolder : string) : boolean;
    function LoadBPLIfNeeded(const bplPath : string; const bplFolder : string; const packageId : string; const lcProject : string) : boolean;
    procedure ReleaseBPLRef(const lcBplPath : string; const lcProject : string);
    procedure ReleaseProjectBPLs(const lcProject : string);
    procedure ApplyEnvironmentVariables(const node : IPackageReference; const template : ISpecTemplate; const lcProject : string; const projectEnvVars : IList<TDPMProjectEnvVarRef>);
    procedure ReleaseProjectEnvVars(const lcProject : string);
    function FindLoadedBPL(const bplPath : string; out exactPath : boolean) : boolean;
  protected
    procedure Clear; override;
    procedure RemoveProject(const projectFile : string); override;
    function InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean; override;
    function UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean; override;
  public
    constructor Create(const logger : ILogger; const pathManager : IDPMIDEPathManager; const envVarManager : IDPMIDEEnvironmentVariableManager; const packageCache : IPackageCache); reintroduce;
  end;

implementation

uses
  ToolsAPI,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.SysUtils,
  Vcl.Forms,
  DPM.Core.Utils.Directory,
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

constructor TDPMIDEPackageInstallerContext.Create(const logger : ILogger; const pathManager : IDPMIDEPathManager; const envVarManager : IDPMIDEEnvironmentVariableManager; const packageCache : IPackageCache);
begin
  inherited Create(logger);
  FPathManager := pathManager;
  FEnvVarManager := envVarManager;
  FPackageCache := packageCache;
  FLoadedBPLs := TCollections.CreateDictionary<string, TDPMLoadedBPL>;
  FProjectBPLs := TCollections.CreateDictionary<string, IList<TDPMProjectBPLRef>>;
  FProjectEnvVars := TCollections.CreateDictionary<string, IList<TDPMProjectEnvVarRef>>;
end;

function TDPMIDEPackageInstallerContext.ResolveDesignBPLPath(const node : IPackageReference; const designEntry : ISpecDesignEntry; const loadPlatform : TDPMPlatform; out bplPath : string; out bplFolder : string) : boolean;
var
  packagePath : string;
  projectBase : string;
  libSuffix : string;
  fileName : string;
  exactPath : string;
  pattern : string;
  matches : IList<string>;
  candidates : string;
  i : integer;
begin
  result := false;
  bplPath := '';
  bplFolder := '';
  //{cache}\{compiler}\{id}\{version}\bpl\{platform}
  packagePath := FPackageCache.GetPackagePath(node);
  bplFolder := TPath.Combine(packagePath, 'bpl' + PathDelim + DPMPlatformToBDString(loadPlatform));

  //designEntry.Project can use forward slashes - normalise so ExtractFileName works on Windows.
  projectBase := StringReplace(designEntry.Project, '/', PathDelim, [rfReplaceAll]);
  projectBase := ChangeFileExt(ExtractFileName(projectBase), '');

  //Exact fast path: honour an author-specified LibSuffix, else the compiler's default ($LibSuffix)
  //eg '290' for Delphi 12. NOTE: LibVersion intentionally does NOT participate in the bpl filename -
  //in Delphi it sets the file version resource, not the output name. Don't append it here.
  libSuffix := designEntry.LibSuffix;
  if libSuffix = '' then
    libSuffix := CompilerToLibSuffix(node.CompilerVersion);

  //heuristic: {LibPrefix}{ProjectBaseName}{LibSuffix}.bpl
  fileName := designEntry.LibPrefix + projectBase + libSuffix + '.bpl';
  exactPath := TPath.Combine(bplFolder, fileName);
  if TFile.Exists(exactPath) then
  begin
    bplPath := exactPath;
    result := true;
    exit;
  end;

  //Discovery fallback: DPM controls the bpl output folder and produces exactly one bpl per design
  //project, but the suffix Delphi actually emitted may differ from our guess - a non-default
  //{$LIBSUFFIX} baked into the .dpk wins over the dproj/msbuild setting. Glob on the project base
  //name to find whatever was produced. Never glob with $(Auto)/AUTO - that's a build token, not an
  //on-disk string; the '*' covers any suffix.
  pattern := designEntry.LibPrefix + projectBase + '*.bpl';
  matches := TDirectoryUtils.GetFiles(bplFolder, pattern);

  if (matches = nil) or (matches.Count = 0) then
  begin
    FLogger.Warning('Could not locate design BPL for [' + node.Id + '] at [' + exactPath + '] - set LibPrefix/LibSuffix on the design entry to match the compiled .bpl name.');
    exit;
  end;

  if matches.Count = 1 then
  begin
    bplPath := matches[0];
    result := true;
    exit;
  end;

  //More than one match. Prefer the exact-name candidate if it's among them (defensive - the fast
  //path above already failed for it), otherwise refuse to guess: loading the wrong bpl into the
  //IDE is worse than not loading at all.
  for i := 0 to matches.Count - 1 do
  begin
    if SameText(ExtractFileName(matches[i]), fileName) then
    begin
      bplPath := matches[i];
      result := true;
      exit;
    end;
  end;

  candidates := '';
  for i := 0 to matches.Count - 1 do
  begin
    if candidates <> '' then
      candidates := candidates + ', ';
    candidates := candidates + ExtractFileName(matches[i]);
  end;
  FLogger.Warning('Multiple candidate design BPLs for [' + node.Id + '] in [' + bplFolder + '] : ' + candidates + ' - set LibPrefix/LibSuffix on the design entry to disambiguate.');
end;

//returns true if a loaded package shares this BPL's filename.
//exactPath is set true only when the full path matches (ie it is our cache BPL).
//Uses IOTAPackageInfo.FileName (the real on-disk path) rather than PackageNames (descriptions)
//so we can distinguish our cache BPL from a foreign package that happens to share a filename.
function TDPMIDEPackageInstallerContext.FindLoadedBPL(const bplPath : string; out exactPath : boolean) : boolean;
var
  svc : IOTAPackageServices;
  info : IOTAPackageInfo;
  targetName : string;
  nameMatch : boolean;
  i : integer;
begin
  result := false;
  exactPath := false;
  nameMatch := false;
  if not Supports(BorlandIDEServices, IOTAPackageServices, svc) then
    exit;
  targetName := ExtractFileName(bplPath);
  for i := 0 to svc.PackageCount - 1 do
  begin
    info := svc.Package[i];
    if (info = nil) or (info.FileName = '') then
      continue;
    if SameText(info.FileName, bplPath) then
    begin
      exactPath := true;
      exit(true);
    end;
    if SameText(ExtractFileName(info.FileName), targetName) then
      nameMatch := true;
  end;
  result := nameMatch;
end;

function TDPMIDEPackageInstallerContext.LoadBPLIfNeeded(const bplPath : string; const bplFolder : string; const packageId : string; const lcProject : string) : boolean;
var
  lcBpl : string;
  loaded : TDPMLoadedBPL;
  installOk : boolean;
  exactPath : boolean;
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

  if FindLoadedBPL(bplPath, exactPath) then
  begin
    if exactPath then
    begin
      //The IDE auto-loaded our cache BPL (persisted in Known Packages and loaded at startup).
      //Adopt it so we control its lifetime and unload it on project-group close. Do NOT call
      //InstallPackage - it is already loaded. EnsurePath so its dependent DLLs still resolve,
      //mirroring the normal load path (ReleaseBPLRef will RemovePath on unload).
      FPathManager.EnsurePath(bplFolder);
      loaded.PackageId := packageId;
      loaded.BPLPath := bplPath;
      loaded.BPLFolder := bplFolder;
      loaded.Referencers := TCollections.CreateList<string>;
      loaded.Referencers.Add(lcProject);
      FLoadedBPLs[lcBpl] := loaded;
      FLogger.Information('Design BPL [' + ExtractFileName(bplPath) + '] already loaded by the IDE - taking ownership for tracking.');
      exit;
    end;
    //Same filename, different location: a foreign (non-DPM) package. Leave it alone.
    FLogger.Warning('Design BPL [' + ExtractFileName(bplPath) + '] is already loaded by the IDE from another location - not tracking to avoid a conflict.');
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

  //During IDE shutdown, skip the UninstallPackage call - the IDE is tearing down its own
  //package state in parallel, and racing IOTAPackageServices.UninstallPackage against that
  //is the source of the generic shutdown AV when a project group is loaded (ofnBeginProjectGroupClose
  //fires Clear() which fans out to UninstallPackage for every design BPL). The IDE will unload
  //our BPLs as part of its normal package teardown; we just drop our tracking refs here.
  if Application.Terminated then
  begin
    FLoadedBPLs.Remove(lcBplPath);
    exit;
  end;

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

procedure TDPMIDEPackageInstallerContext.ApplyEnvironmentVariables(const node : IPackageReference; const template : ISpecTemplate; const lcProject : string; const projectEnvVars : IList<TDPMProjectEnvVarRef>);
var
  packagePath : string;
  platformFolder : string;
  keys : TArray<string>;
  i : integer;
  resolvedValue : string;
  ref : TDPMProjectEnvVarRef;
begin
  if template.EnvironmentVariables.Count = 0 then
    exit;
  //Two deferred tokens are resolved here:
  // $packageDir$ - this package's cache folder, so values can point at files shipped in the package.
  // $platform$   - the IDE *host* platform (Win32 for the classic IDE, Win64 for bds64), since env
  //                vars take effect in the IDE process - eg a native DLL loaded by a design component
  //                must match the IDE's bitness, not the project's target platform. Uses the same
  //                Is64BitIDE load decision as the design BPL selection above, and the same BD folder
  //                name the installer writes platform sub-folders with.
  //Everything else was expanded at pack time.
  packagePath := ExcludeTrailingPathDelimiter(FPackageCache.GetPackagePath(node));
  if TSystemUtils.Is64BitIDE then
    platformFolder := DPMPlatformToBDString(TDPMPlatform.Win64)
  else
    platformFolder := DPMPlatformToBDString(TDPMPlatform.Win32);
  keys := template.EnvironmentVariables.Keys.ToArray;
  for i := 0 to High(keys) do
  begin
    resolvedValue := StringReplace(template.EnvironmentVariables[keys[i]], '$packageDir$', packagePath, [rfReplaceAll, rfIgnoreCase]);
    resolvedValue := StringReplace(resolvedValue, '$platform$', platformFolder, [rfReplaceAll, rfIgnoreCase]);
    FEnvVarManager.SetVariable(keys[i], resolvedValue, lcProject, node.Id);
    ref.PackageId := LowerCase(node.Id);
    ref.Name := keys[i];
    ref.Value := resolvedValue;
    projectEnvVars.Add(ref);
  end;
end;

procedure TDPMIDEPackageInstallerContext.ReleaseProjectEnvVars(const lcProject : string);
var
  envVars : IList<TDPMProjectEnvVarRef>;
  i : integer;
begin
  if not FProjectEnvVars.TryGetValue(lcProject, envVars) then
    exit;
  //release in reverse apply order
  for i := envVars.Count - 1 downto 0 do
    FEnvVarManager.RemoveVariable(envVars[i].Name, envVars[i].Value, lcProject, envVars[i].PackageId);
  envVars.Clear;
end;

function TDPMIDEPackageInstallerContext.InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean;
var
  lcProject : string;
  loadPlatform : TDPMPlatform;
  graph : IPackageReference;
  oldRefs : IList<TDPMProjectBPLRef>;
  newRefs : IList<TDPMProjectBPLRef>;
  desired : IList<TDPMDesignBPL>;
  desiredSet : IDictionary<string, integer>;
  projectEnvVars : IList<TDPMProjectEnvVarRef>;
  overallResult : boolean;
  designBpl : TDPMDesignBPL;
  i : integer;
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

  //Differential update: rather than unload everything and reload (which needlessly cycles every
  //design BPL on every install/restore), resolve the desired BPL set from the new graph, then
  //unload only the orphans and load only the genuinely new ones. BPLs in both old and new sets are
  //left untouched - LoadBPLIfNeeded is already idempotent for those, and we never release them.

  //Env vars are cheap to re-apply and not the source of the churn - keep the simple rebuild here.
  ReleaseProjectEnvVars(lcProject);
  projectEnvVars := TCollections.CreateList<TDPMProjectEnvVarRef>;
  FProjectEnvVars[lcProject] := projectEnvVars;

  desired := TCollections.CreateList<TDPMDesignBPL>;
  desiredSet := TCollections.CreateDictionary<string, integer>;
  newRefs := TCollections.CreateList<TDPMProjectBPLRef>;
  overallResult := true;

  //Resolve phase: walk the graph (post-order DFS - children before parent, spec order within a
  //package) and collect the desired BPLs in load order without touching the IDE yet.
  graph.VisitDFS(
    procedure(const node : IPackageReference)
    var
      spec : IPackageSpec;
      template : ISpecTemplate;
      designEntry : ISpecDesignEntry;
      bplPath : string;
      bplFolder : string;
      entry : TDPMDesignBPL;
      ref : TDPMProjectBPLRef;
    begin
      if cancellationToken.IsCancelled then
        exit;
      if not packageSpecs.TryGetValue(LowerCase(node.Id), spec) then
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

        entry.PackageId := node.Id;
        entry.BPLPath := bplPath;
        entry.BPLFolder := bplFolder;
        desired.Add(entry);
        desiredSet[LowerCase(bplPath)] := 1;

        ref.PackageId := LowerCase(node.Id);
        ref.BPLPath := LowerCase(bplPath);
        newRefs.Add(ref);
      end;

      //Apply any IDE environment variables this package declares - independent of whether it has a
      //design BPL, so a runtime-only package can still expose e.g. a path to its bundled DLLs.
      ApplyEnvironmentVariables(node, template, lcProject, projectEnvVars);
    end);

  if cancellationToken.IsCancelled then
    exit(false);

  //Unload-orphans phase: release BPLs we were tracking for this project that are no longer wanted.
  //Reverse order (dependants before dependencies) matches ReleaseProjectBPLs. Done before loading
  //so an upgraded package's old/new same-filename BPL don't collide in the IDE.
  if FProjectBPLs.TryGetValue(lcProject, oldRefs) then
  begin
    for i := oldRefs.Count - 1 downto 0 do
      if not desiredSet.ContainsKey(oldRefs[i].BPLPath) then
        ReleaseBPLRef(oldRefs[i].BPLPath, lcProject);
  end;

  //Load-new phase: forward (post-order DFS) order. Already-loaded BPLs are no-ops in LoadBPLIfNeeded.
  for designBpl in desired do
  begin
    if cancellationToken.IsCancelled then
      exit(false);
    if not LoadBPLIfNeeded(designBpl.BPLPath, designBpl.BPLFolder, designBpl.PackageId, lcProject) then
      overallResult := false;
  end;

  FProjectBPLs[lcProject] := newRefs;

  result := overallResult and not cancellationToken.IsCancelled;
end;

function TDPMIDEPackageInstallerContext.UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean;
var
  lcProject : string;
  projectRefs : IList<TDPMProjectBPLRef>;
  projectEnvVars : IList<TDPMProjectEnvVarRef>;
  orphanSet : IDictionary<string, integer>;
  orphanedId : string;
  i : integer;
  ref : TDPMProjectBPLRef;
  envRef : TDPMProjectEnvVarRef;
begin
  result := true;
  lcProject := LowerCase(projectFile);

  if (orphanedPackageIds = nil) or (orphanedPackageIds.Count = 0) then
    exit;

  orphanSet := TCollections.CreateDictionary<string, integer>;
  for orphanedId in orphanedPackageIds do
    orphanSet[LowerCase(orphanedId)] := 1;

  //Environment variables for orphaned packages - release in reverse apply order.
  if FProjectEnvVars.TryGetValue(lcProject, projectEnvVars) then
  begin
    for i := projectEnvVars.Count - 1 downto 0 do
    begin
      envRef := projectEnvVars[i];
      if not orphanSet.ContainsKey(envRef.PackageId) then
        continue;
      FEnvVarManager.RemoveVariable(envRef.Name, envRef.Value, lcProject, envRef.PackageId);
      projectEnvVars.Delete(i);
    end;
  end;

  if not FProjectBPLs.TryGetValue(lcProject, projectRefs) then
    exit;

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
  ReleaseProjectEnvVars(lcProject);
  FProjectEnvVars.Remove(lcProject);
  inherited;
end;

procedure TDPMIDEPackageInstallerContext.Clear;
var
  keys : TArray<string>;
  key : string;
begin
  //During IDE shutdown, drop tracking without re-entering the IDE - the IDE will unload our
  //BPLs as part of its own package teardown. inherited is skipped because it can touch shared
  //state too. Normal (user-initiated) group close still goes through the full release path so
  //BPLs are properly removed for the next project group.
  if Application.Terminated then
  begin
    FProjectBPLs.Clear;
    FLoadedBPLs.Clear;
    FProjectEnvVars.Clear;
    exit;
  end;
  //snapshot keys so we can release without invalidating enumeration
  keys := FProjectBPLs.Keys.ToArray;
  for key in keys do
    ReleaseProjectBPLs(key);
  FProjectBPLs.Clear;
  FLoadedBPLs.Clear; //defensive - should be empty by now

  //restore any env vars we set, then drop tracking.
  keys := FProjectEnvVars.Keys.ToArray;
  for key in keys do
    ReleaseProjectEnvVars(key);
  FProjectEnvVars.Clear;
  inherited;
end;

end.
