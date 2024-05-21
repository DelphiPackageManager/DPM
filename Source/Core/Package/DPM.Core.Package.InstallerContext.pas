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
  DPM.Core.Manifest.Interfaces;

type
  TCorePackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  protected
    FProjectGraphs : IDictionary<string, IDictionary<TDPMPlatform, IPackageReference>>;
    FProjectResolutions : IDictionary<string, IDictionary<TDPMPlatform, IList<IResolvedPackage>>>;

    FLogger : ILogger;

    procedure PackageInstalled(const platform : TDPMPlatform; const dpmPackageId : string; const packageVersion : TPackageVersion; const designFiles : TArray<string>);virtual;
    procedure PackageUninstalled(const platform : TDPMPlatform; const dpmPackageId : string);virtual;

    procedure RemoveProject(const projectFile : string);virtual;

    function GetProjectGraph(const projectFile : string; const platform : TDPMPlatform) : IPackageReference;

    procedure RecordGraph(const projectFile: string; const platform : TDPMPlatform; const graph: IPackageReference);virtual;
    //called when package uninstalled
    procedure PackageGraphPruned(const projectFile : string; const platform : TDPMPlatform; const graph : IPackageReference);virtual;

    //this is a no-op here, look at the IDE installer context to see how this is implemented.
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageManifests : IDictionary<string, IPackageManifest>) : boolean;virtual;

    //record package resoltions for a project, so we can detect conflicts
    procedure RecordResolutions(const projectFile: string; const platform : TDPMPlatform; const resolutions : TArray<IResolvedPackage>);
    //search other projects in the project group to see if they have resolved the package.
    function FindPackageResolution(const projectFile: string; const platform : TDPMPlatform; const packageId : string ) : IResolvedPackage;

    procedure RemoveResolution(const platform : TDPMPlatform; const packageId : string);

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
end;

constructor TCorePackageInstallerContext.Create(const logger : ILogger);
begin
  FLogger := Logger;
  FProjectGraphs := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform, IPackageReference>>;
  FProjectResolutions := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform, IList<IResolvedPackage>>>;
end;


function TCorePackageInstallerContext.FindPackageResolution(const projectFile : string; const platform: TDPMPlatform; const packageId: string): IResolvedPackage;
var
  pair : TPair<string, IDictionary<TDPMPlatform, IList<IResolvedPackage>>>;
  resolutions :  IList<IResolvedPackage>;
begin
  result := nil;
  for pair in FProjectResolutions do
  begin
    //only check other projects which might have already resolved the package.
    if not SameText(projectFile, pair.Key) then
    begin
      if pair.Value.TryGetValue(platform, resolutions) then
      begin
        result := resolutions.FirstOrDefault(
          function (const resolution : IResolvedPackage) : boolean
          begin
            result := SameText(packageId, resolution.PackageInfo.Id);
          end);

        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TCorePackageInstallerContext.GetProjectGraph(const projectFile: string; const platform: TDPMPlatform): IPackageReference;
var
  projectPlatformEntry : IDictionary<TDPMPlatform, IPackageReference>;
begin
  result := nil;
  if not FProjectGraphs.TryGetValue(LowerCase(projectFile), projectPlatformEntry) then
    exit; //this will be a problem. how to deal with?
  projectPlatformEntry.TryExtract(platform, result);
end;

function TCorePackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageManifests : IDictionary<string, IPackageManifest>): boolean;
begin
  result := true; //this is only needed for the IDE context
end;

procedure TCorePackageInstallerContext.PackageGraphPruned(const projectFile: string; const platform: TDPMPlatform; const graph: IPackageReference);
var
  projectPlatformEntry : IDictionary<TDPMPlatform, IPackageReference>;
  resolutionPlatformEntry : IDictionary<TDPMPlatform, IList<IResolvedPackage>>;
  resolutions : IDictionary<string, integer>;
  packageId  : string;

  platformResolutions : IList<IResolvedPackage>;
  prunedResolutions : IList<IResolvedPackage>;
  resolution : IResolvedPackage;

begin
  if not FProjectGraphs.TryGetValue(LowerCase(projectFile), projectPlatformEntry) then
  begin
    projectPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IPackageReference>;
    FProjectGraphs[LowerCase(projectFile)] := projectPlatformEntry;
  end;
  projectPlatformEntry[platform] := graph;

  if not FProjectResolutions.TryGetValue(LowerCase(projectFile),resolutionPlatformEntry) then
    exit;


  if not resolutionPlatformEntry.TryGetValue(platform, platformResolutions) then
    exit;


  prunedResolutions := TCollections.CreateList<IResolvedPackage>;

  //flatten the graph so we can use it to prune resolutions.
  resolutions := TCollections.CreateDictionary<string,integer>;
  graph.VisitDFS(
    procedure(const packageReference: IPackageReference)
    begin
      resolutions[LowerCase(packageReference.Id)] := 1;
    end);

  for packageId in resolutions.Keys do
  begin
    resolution := platformResolutions.FirstOrDefault(
      function(const res : IResolvedPackage) : boolean
      begin
        result := SameText(res.PackageInfo.Id, packageId);
      end);
    if resolution <> nil then
      prunedResolutions.Add(resolution);
  end;

  resolutionPlatformEntry[platform] := prunedResolutions;


end;

procedure TCorePackageInstallerContext.PackageInstalled(const platform: TDPMPlatform; const dpmPackageId: string; const packageVersion : TPackageVersion; const designFiles: TArray<string>);
begin
  //do nothing, used in the IDE plugin
end;

procedure TCorePackageInstallerContext.PackageUninstalled(const platform: TDPMPlatform; const dpmPackageId: string);
begin
  //do nothing, used in the IDE plugin
end;

procedure TCorePackageInstallerContext.RecordGraph(const projectFile: string; const platform : TDPMPlatform; const graph: IPackageReference);
var
  projectPlatformEntry : IDictionary<TDPMPlatform, IPackageReference>;
begin
  if not FProjectGraphs.TryGetValue(LowerCase(projectFile), projectPlatformEntry) then
  begin
    projectPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IPackageReference>;
    FProjectGraphs[LowerCase(projectFile)] := projectPlatformEntry;
  end;
  projectPlatformEntry[platform] := graph;

end;


procedure TCorePackageInstallerContext.RecordResolutions(const projectFile: string; const platform: TDPMPlatform; const resolutions: TArray<IResolvedPackage>);
var
  resolutionPlatformEntry : IDictionary<TDPMPlatform, IList<IResolvedPackage>>;
begin
  if not FProjectResolutions.TryGetValue(LowerCase(projectFile),resolutionPlatformEntry) then
  begin
    resolutionPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IList<IResolvedPackage>>;
    FProjectResolutions[LowerCase(projectFile)] := resolutionPlatformEntry;
  end;
  resolutionPlatformEntry[platform] := TCollections.CreateList<IResolvedPackage>(resolutions);

end;

procedure TCorePackageInstallerContext.RemoveProject(const projectFile: string);
begin
  if FProjectGraphs.ContainsKey(LowerCase(projectFile)) then
    FProjectGraphs.Remove(LowerCase(projectFile));
  if FProjectResolutions.ContainsKey(LowerCase(projectFile)) then
    FProjectResolutions.Remove(LowerCase(projectFile));
end;



procedure TCorePackageInstallerContext.RemoveResolution(const platform: TDPMPlatform; const packageId: string);
var
  projectRes : IDictionary<TDPMPlatform, IList<IResolvedPackage>>;
  resList : IList<IResolvedPackage>;
  resolution : IResolvedPackage;
begin
  for projectRes  in FProjectResolutions.Values do
  begin
    resList := projectRes[platform];
    if resList <> nil then
    begin
      resolution := resList.FirstOrDefault(
        function(const value : IResolvedPackage) : boolean
        begin
             result := SameText(value.PackageInfo.Id, packageId);
        end);
      if resolution <> nil  then
        resList.Remove(resolution);
    end;

  end;
end;

end.
