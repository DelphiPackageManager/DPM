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
    FProjectGraphs : IDictionary<string, IDictionary<TDPMPlatform, IPackageReference>>;
    FProjectResolutions : IDictionary<string, IDictionary<TDPMPlatform, IList<IResolution>>>;

    FLogger : ILogger;

    procedure PackageInstalled(const platform : TDPMPlatform; const dpmPackageId : string; const packageVersion : TPackageVersion; const designFiles : TArray<string>);virtual;
    procedure PackageUninstalled(const platform : TDPMPlatform; const dpmPackageId : string);virtual;

    procedure RemoveProject(const projectFile : string);virtual;

    procedure RecordGraph(const projectFile: string; const platform : TDPMPlatform; const graph: IPackageReference);virtual;
    //called when package uninstalled
    procedure PackageGraphPruned(const projectFile : string; const platform : TDPMPlatform; const graph : IPackageReference);virtual;

    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>) : boolean;virtual;


    //search other projects in the project group to see if they have resolved the package.
    procedure RecordResolutions(const projectFile: string; const platform : TDPMPlatform; const resolutions : TArray<IResolution>);
    function FindPackageResolution(const projectFile: string; const platform : TDPMPlatform; const packageId : string ) : IResolution;

    procedure Clear;virtual;
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
  FProjectResolutions := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform, IList<IResolution>>>;
end;


function TCorePackageInstallerContext.FindPackageResolution(const projectFile : string; const platform: TDPMPlatform; const packageId: string): IResolution;
var
  pair : TPair<string, IDictionary<TDPMPlatform, IList<IResolution>>>;
  resolutions :  IList<IResolution>;
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
          function (const resolution : IResolution) : boolean
          begin
            result := SameText(packageId, resolution.Package.Id);
          end);

        if result <> nil then
          exit;
      end;
    end;
  end;
end;

function TCorePackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
begin
  result := true; //this is only needed for the IDE context
end;

procedure TCorePackageInstallerContext.PackageGraphPruned(const projectFile: string; const platform: TDPMPlatform; const graph: IPackageReference);
var
  projectPlatformEntry : IDictionary<TDPMPlatform, IPackageReference>;
//  resolutionPlatformEntry : IDictionary<TDPMPlatform, IList<IResolution>>;
  resolutions : IDictionary<string, integer>;
begin
  if not FProjectGraphs.TryGetValue(LowerCase(projectFile), projectPlatformEntry) then
  begin
    projectPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IPackageReference>;
    FProjectGraphs[LowerCase(projectFile)] := projectPlatformEntry;
  end;
  projectPlatformEntry[platform] := graph;

  resolutions := TCollections.CreateDictionary<string,integer>;
  graph.VisitDFS(
    procedure(const packageReference: IPackageReference)
    begin
      resolutions[packageReference.Id] := 1;
    end);





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


procedure TCorePackageInstallerContext.RecordResolutions(const projectFile: string; const platform: TDPMPlatform; const resolutions: TArray<IResolution>);
var
  resolutionPlatformEntry : IDictionary<TDPMPlatform, IList<IResolution>>;
begin
  if not FProjectResolutions.TryGetValue(LowerCase(projectFile),resolutionPlatformEntry) then
  begin
    resolutionPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IList<IResolution>>;
    FProjectResolutions[LowerCase(projectFile)] := resolutionPlatformEntry;
  end;
  resolutionPlatformEntry[platform] := TCollections.CreateList<IResolution>(resolutions);

end;

procedure TCorePackageInstallerContext.RemoveProject(const projectFile: string);
begin
  if FProjectGraphs.ContainsKey(LowerCase(projectFile)) then
    FProjectGraphs.Remove(LowerCase(projectFile));
  if FProjectResolutions.ContainsKey(LowerCase(projectFile)) then
    FProjectResolutions.Remove(LowerCase(projectFile));
end;



end.
