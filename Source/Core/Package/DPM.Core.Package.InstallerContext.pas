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

    FLogger : ILogger;

    procedure PackageInstalled(const dpmPackageId : string; const packageVersion : TPackageVersion; const designFiles : TArray<string>);virtual;
    procedure PackageUninstalled(const dpmPackageId : string);virtual;

    procedure RemoveProject(const projectFile : string);virtual;

    function GetProjectGraph(const projectFile : string) : IPackageReference;

    procedure RecordGraph(const projectFile: string; const graph: IPackageReference);virtual;
    //called when package uninstalled
    procedure PackageGraphPruned(const projectFile : string; const graph : IPackageReference);virtual;

    //this is a no-op here, look at the IDE installer context to see how this is implemented.
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests : IDictionary<string, IPackageSpec>) : boolean;virtual;

    //this is a no-op here, look at the IDE installer context to see how this is implemented.
    function UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean;virtual;

    //record package resoltions for a project, so we can detect conflicts
    procedure RecordResolutions(const projectFile: string; const resolutions : TArray<IResolvedPackage>);
    //search other projects in the project group to see if they have resolved the package.
    function FindPackageResolution(const projectFile: string; const packageId : string ) : IResolvedPackage;

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

procedure TCorePackageInstallerContext.Clear;
begin
  //IDE will override this
  FProjectGraphs.Clear;
  FProjectResolutions.Clear;
  FResolutionIndex.Clear;
end;

constructor TCorePackageInstallerContext.Create(const logger : ILogger);
begin
  FLogger := Logger;
  FProjectGraphs := TCollections.CreateDictionary<string, IPackageReference>;
  FProjectResolutions := TCollections.CreateDictionary<string, IList<IResolvedPackage>>;
  FResolutionIndex := TCollections.CreateDictionary<string, IDictionary<string, IResolvedPackage>>;
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
  for pair in FResolutionIndex do
  begin
    if pair.Key = projKey then
      continue;
    byPackage := pair.Value;
    if byPackage.TryGetValue(pkgKey, result) then
      exit;
  end;
end;

function TCorePackageInstallerContext.GetProjectGraph(const projectFile: string): IPackageReference;
begin
  result := nil;
  FProjectGraphs.TryGetValue(LowerCase(projectFile), result);
end;

function TCorePackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests : IDictionary<string, IPackageSpec>): boolean;
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
begin
  FProjectGraphs[LowerCase(projectFile)] := graph;
end;


procedure TCorePackageInstallerContext.RecordResolutions(const projectFile: string; const resolutions: TArray<IResolvedPackage>);
var
  projKey : string;
  byPackage : IDictionary<string, IResolvedPackage>;
  resolution : IResolvedPackage;
begin
  projKey := LowerCase(projectFile);
  FProjectResolutions[projKey] := TCollections.CreateList<IResolvedPackage>(resolutions);
  //Rebuild the index entry for this project from the new resolution list.
  byPackage := TCollections.CreateDictionary<string, IResolvedPackage>;
  for resolution in resolutions do
    byPackage[LowerCase(resolution.PackageInfo.Id)] := resolution;
  FResolutionIndex[projKey] := byPackage;
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
end;



end.
