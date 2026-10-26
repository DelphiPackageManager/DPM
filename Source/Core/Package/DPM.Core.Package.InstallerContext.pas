unit DPM.Core.Package.InstallerContext;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Spec.Interfaces;

type
  TCorePackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  protected
    FProjectGraphs : IDictionary<string, IPackageReference>;
    FProjectResolutions : IDictionary<string, IList<IResolvedPackage>>;
    //O(1) index over FProjectResolutions: outer key = lowercase projectFile, inner key =
    //lowercase packageId. Kept in sync by RecordResolutions / PackageGraphPruned / RemoveProject
    /// Clear so FindPackageResolution doesn't have to walk every project's resolution list on
    //every cross-project lookup miss.
    FResolutionIndex : IDictionary<string, IDictionary<string, IResolvedPackage>>;

    //Current-session 'use source' choices. Outer key = lowercase projectFile, inner key =
    //lowercase packageId. Never persisted - populated only by the IDE, empty for the CLI.
    FUseSourceState : IDictionary<string, IDictionary<string, boolean>>;

    //Multi-project install operation (BeginOperation .. EndOperation). Projects in the operation
    //record into the staged dictionaries until CommitProject, and their committed records are
    //hidden from FindPackageResolution since they are about to be replaced. Keys are lowercase.
    FInOperation : boolean;
    FOperationProjects : ISet<string>;
    FStagedGraphs : IDictionary<string, IPackageReference>;
    FStagedResolutions : IDictionary<string, IList<IResolvedPackage>>;
    FStagedIndex : IDictionary<string, IDictionary<string, IResolvedPackage>>;
    //whether the last operation committed a project - kept past EndOperation, reset by BeginOperation.
    FAnyCommitted : boolean;

    FLogger : ILogger;

    procedure ClearOperation;

    procedure PackageInstalled(const dpmPackageId : string; const packageVersion : TPackageVersion; const designFiles : TArray<string>);virtual;
    procedure PackageUninstalled(const dpmPackageId : string);virtual;

    procedure RemoveProject(const projectFile : string);virtual;

    function GetProjectGraph(const projectFile : string) : IPackageReference;

    procedure RecordGraph(const projectFile: string; const graph: IPackageReference);virtual;
    //called when package uninstalled
    procedure PackageGraphPruned(const projectFile : string; const graph : IPackageReference);virtual;

    //this is a no-op here, look at the IDE installer context to see how this is implemented.
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean;virtual;

    //this is a no-op here, look at the IDE installer context to see how this is implemented.
    function UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean;virtual;

    //record package resoltions for a project, so we can detect conflicts
    procedure RecordResolutions(const projectFile: string; const resolutions : TArray<IResolvedPackage>);
    //search other projects in the project group to see if they have resolved the package.
    function FindPackageResolution(const projectFile: string; const packageId : string ) : IResolvedPackage;

    //multi-project install operation - see interface for details.
    procedure BeginOperation(const projectFiles : IList<string>);
    procedure CommitProject(const projectFile : string);
    procedure EndOperation;
    function AnyProjectCommitted : boolean;

    //current-session 'use source' choices - see interface for details.
    procedure SetUseSource(const projectFile, packageId : string; const value : boolean);virtual;
    function GetUseSource(const projectFile, packageId : string) : boolean;virtual;
    procedure ClearUseSource(const projectFile, packageId : string);virtual;

    procedure Clear;virtual;

    property Logger : ILogger read FLogger;
  public
    constructor Create(const logger : ILogger);virtual;

  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Dependency.Resolution;


{ TPackageInstallerContext }

function TCorePackageInstallerContext.AnyProjectCommitted : boolean;
begin
  result := FAnyCommitted;
end;

procedure TCorePackageInstallerContext.BeginOperation(const projectFiles : IList<string>);
var
  projectFile : string;
begin
  if FInOperation then
    raise EInvalidOpException.Create('An install operation is already in progress');
  ClearOperation;
  FInOperation := true;
  FAnyCommitted := false;
  for projectFile in projectFiles do
    FOperationProjects.Add(LowerCase(projectFile));
end;

procedure TCorePackageInstallerContext.Clear;
begin
  //IDE will override this
  FProjectGraphs.Clear;
  FProjectResolutions.Clear;
  FResolutionIndex.Clear;
  FUseSourceState.Clear;
  ClearOperation;
end;

procedure TCorePackageInstallerContext.ClearOperation;
begin
  FInOperation := false;
  FOperationProjects.Clear;
  FStagedGraphs.Clear;
  FStagedResolutions.Clear;
  FStagedIndex.Clear;
end;

procedure TCorePackageInstallerContext.CommitProject(const projectFile : string);
var
  projKey : string;
  graph : IPackageReference;
  resolutions : IList<IResolvedPackage>;
  byPackage : IDictionary<string, IResolvedPackage>;
begin
  projKey := LowerCase(projectFile);
  if not FOperationProjects.Contains(projKey) then
    exit;
  FAnyCommitted := true;
  if FStagedGraphs.TryGetValue(projKey, graph) then
  begin
    FProjectGraphs[projKey] := graph;
    FStagedGraphs.Remove(projKey);
  end;
  if FStagedResolutions.TryGetValue(projKey, resolutions) then
  begin
    FProjectResolutions[projKey] := resolutions;
    FStagedResolutions.Remove(projKey);
  end;
  if FStagedIndex.TryGetValue(projKey, byPackage) then
  begin
    FResolutionIndex[projKey] := byPackage;
    FStagedIndex.Remove(projKey);
  end;
  FOperationProjects.Remove(projKey);
end;

constructor TCorePackageInstallerContext.Create(const logger : ILogger);
begin
  FLogger := Logger;
  FProjectGraphs := TCollections.CreateDictionary<string, IPackageReference>;
  FProjectResolutions := TCollections.CreateDictionary<string, IList<IResolvedPackage>>;
  FResolutionIndex := TCollections.CreateDictionary<string, IDictionary<string, IResolvedPackage>>;
  FUseSourceState := TCollections.CreateDictionary<string, IDictionary<string, boolean>>;
  FOperationProjects := TCollections.CreateSet<string>;
  FStagedGraphs := TCollections.CreateDictionary<string, IPackageReference>;
  FStagedResolutions := TCollections.CreateDictionary<string, IList<IResolvedPackage>>;
  FStagedIndex := TCollections.CreateDictionary<string, IDictionary<string, IResolvedPackage>>;
end;


procedure TCorePackageInstallerContext.EndOperation;
begin
  ClearOperation;
end;

function TCorePackageInstallerContext.FindPackageResolution(const projectFile : string; const packageId: string): IResolvedPackage;
var
  projKey : string;
  pkgKey : string;
  pair : TPair<string, IDictionary<string, IResolvedPackage>>;
  byPackage : IDictionary<string, IResolvedPackage>;
begin
  result := nil;
  projKey := LowerCase(projectFile);
  pkgKey := LowerCase(packageId);
  //Skip the current project (we only consult OTHER projects for cross-project lookups) and
  //ask the index directly - O(1) per project rather than scanning every resolution list.
  //Projects in an install operation are about to be replaced, so their committed records are
  //skipped too - one not yet processed must not block the project being processed.
  for pair in FResolutionIndex do
  begin
    if pair.Key = projKey then
      continue;
    if FOperationProjects.Contains(pair.Key) then
      continue;
    byPackage := pair.Value;
    if byPackage.TryGetValue(pkgKey, result) then
      exit;
  end;
  //Projects already processed in the operation are checked against their new resolutions.
  for pair in FStagedIndex do
  begin
    if pair.Key = projKey then
      continue;
    byPackage := pair.Value;
    if byPackage.TryGetValue(pkgKey, result) then
      exit;
  end;
  result := nil;
end;

function TCorePackageInstallerContext.GetProjectGraph(const projectFile: string): IPackageReference;
begin
  result := nil;
  FProjectGraphs.TryGetValue(LowerCase(projectFile), result);
end;

function TCorePackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>): boolean;
begin
  result := true; //this is only needed for the IDE context
end;

function TCorePackageInstallerContext.UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>): boolean;
begin
  result := true; //this is only needed for the IDE context
end;

procedure TCorePackageInstallerContext.PackageGraphPruned(const projectFile: string; const graph: IPackageReference);
var
  projKey : string;
  resolutionEntry : IList<IResolvedPackage>;
  resolutions : IDictionary<string, integer>;
  packageId  : string;

  prunedResolutions : IList<IResolvedPackage>;
  prunedIndex : IDictionary<string, IResolvedPackage>;
  resolution : IResolvedPackage;

begin
  projKey := LowerCase(projectFile);
  if not FProjectResolutions.TryGetValue(projKey, resolutionEntry) then
    exit;

  prunedResolutions := TCollections.CreateList<IResolvedPackage>;
  prunedIndex := TCollections.CreateDictionary<string, IResolvedPackage>;

  //flatten the graph so we can use it to prune resolutions.
  resolutions := TCollections.CreateDictionary<string,integer>;
  graph.VisitDFS(
    procedure(const packageReference: IPackageReference)
    begin
      resolutions[LowerCase(packageReference.Id)] := 1;
    end);

  for packageId in resolutions.Keys do
  begin
    resolution := resolutionEntry.FirstOrDefault(
      function(const res : IResolvedPackage) : boolean
      begin
        result := SameText(res.PackageInfo.Id, packageId);
      end);
    if resolution <> nil then
    begin
      prunedResolutions.Add(resolution);
      prunedIndex[LowerCase(resolution.PackageInfo.Id)] := resolution;
    end;
  end;

  FProjectResolutions[projKey] := prunedResolutions;
  FResolutionIndex[projKey] := prunedIndex;
end;

procedure TCorePackageInstallerContext.PackageInstalled(const dpmPackageId: string; const packageVersion : TPackageVersion; const designFiles: TArray<string>);
begin
  //do nothing, used in the IDE plugin
end;

procedure TCorePackageInstallerContext.PackageUninstalled(const dpmPackageId: string);
begin
  //do nothing, used in the IDE plugin
end;

procedure TCorePackageInstallerContext.RecordGraph(const projectFile: string; const graph: IPackageReference);
var
  projKey : string;
begin
  projKey := LowerCase(projectFile);
  if FOperationProjects.Contains(projKey) then
    FStagedGraphs[projKey] := graph
  else
    FProjectGraphs[projKey] := graph;
end;


procedure TCorePackageInstallerContext.RecordResolutions(const projectFile: string; const resolutions: TArray<IResolvedPackage>);
var
  projKey : string;
  owned : IList<IResolvedPackage>;
  byPackage : IDictionary<string, IResolvedPackage>;
  resolution : IResolvedPackage;
begin
  projKey := LowerCase(projectFile);
  //The resolver adopts resolutions from sibling projects, and those still name the sibling as
  //their project. Record them as this project's own - it uses that version too. Left as they
  //were, a sibling re-resolving would find its own stale resolution here (which skipping the
  //current project in FindPackageResolution is meant to prevent), treat it as local, and loop
  //removing and re-importing it.
  owned := TCollections.CreateList<IResolvedPackage>;
  for resolution in resolutions do
  begin
    if SameText(resolution.ProjectFile, projectFile) then
      owned.Add(resolution)
    else
      owned.Add(TResolvedPackage.Create(resolution.PackageInfo, resolution.VersionRange, resolution.ParentId, projectFile, resolution.IsBundled));
  end;
  //Rebuild the index entry for this project from the new resolution list.
  byPackage := TCollections.CreateDictionary<string, IResolvedPackage>;
  for resolution in owned do
    byPackage[LowerCase(resolution.PackageInfo.Id)] := resolution;

  if FOperationProjects.Contains(projKey) then
  begin
    FStagedResolutions[projKey] := owned;
    FStagedIndex[projKey] := byPackage;
  end
  else
  begin
    FProjectResolutions[projKey] := owned;
    FResolutionIndex[projKey] := byPackage;
  end;
end;

procedure TCorePackageInstallerContext.RemoveProject(const projectFile: string);
var
  projKey : string;
begin
  projKey := LowerCase(projectFile);
  if FProjectGraphs.ContainsKey(projKey) then
    FProjectGraphs.Remove(projKey);
  if FProjectResolutions.ContainsKey(projKey) then
    FProjectResolutions.Remove(projKey);
  if FResolutionIndex.ContainsKey(projKey) then
    FResolutionIndex.Remove(projKey);
  if FUseSourceState.ContainsKey(projKey) then
    FUseSourceState.Remove(projKey);
  if FOperationProjects.Contains(projKey) then
    FOperationProjects.Remove(projKey);
  if FStagedGraphs.ContainsKey(projKey) then
    FStagedGraphs.Remove(projKey);
  if FStagedResolutions.ContainsKey(projKey) then
    FStagedResolutions.Remove(projKey);
  if FStagedIndex.ContainsKey(projKey) then
    FStagedIndex.Remove(projKey);
end;

procedure TCorePackageInstallerContext.SetUseSource(const projectFile, packageId : string; const value : boolean);
var
  projKey : string;
  byPackage : IDictionary<string, boolean>;
begin
  projKey := LowerCase(projectFile);
  if not FUseSourceState.TryGetValue(projKey, byPackage) then
  begin
    byPackage := TCollections.CreateDictionary<string, boolean>;
    FUseSourceState[projKey] := byPackage;
  end;
  byPackage[LowerCase(packageId)] := value;
end;

function TCorePackageInstallerContext.GetUseSource(const projectFile, packageId : string) : boolean;
var
  byPackage : IDictionary<string, boolean>;
begin
  result := false;
  if FUseSourceState.TryGetValue(LowerCase(projectFile), byPackage) then
    byPackage.TryGetValue(LowerCase(packageId), result);
end;

procedure TCorePackageInstallerContext.ClearUseSource(const projectFile, packageId : string);
var
  byPackage : IDictionary<string, boolean>;
begin
  if FUseSourceState.TryGetValue(LowerCase(projectFile), byPackage) then
    byPackage.Remove(LowerCase(packageId));
end;



end.
