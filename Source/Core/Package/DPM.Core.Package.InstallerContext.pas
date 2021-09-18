unit DPM.Core.Package.InstallerContext;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Installer.Interfaces;

type
  TCorePackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  private

    FProjectGraphs : IDictionary<string, IDictionary<TDPMPlatform, IGraphNode>>;
    FProjectResolutions : IDictionary<string, IDictionary<TDPMPlatform, IList<IResolution>>>;
  protected
    FLogger : ILogger;
    procedure StartProject(const projectFile: string; const platform : TDPMPlatform);virtual;
    procedure EndProject(const projectFile: string; const platform : TDPMPlatform);virtual;


    function RegisterDesignPackage(const platform : TDPMPlatform; const packageFile : string; const dependsOn : IList<string>; out errorMessage : string) : boolean;virtual;
    function IsDesignPackageInstalled(const packageName : string; out platform : TDPMPlatform; out project : string) : boolean;virtual;

    procedure RemoveProject(const projectFile : string);virtual;

    procedure RecordGraph(const projectFile: string; const platform : TDPMPlatform; const graph: IGraphNode; const resolutions : TArray<IResolution>);virtual;

    //search other projects in the project group to see if they have resolved the package.
    function FindPackageResolution(const currentProjectFile: string; const packageId : string; const platform : TDPMPlatform) : IResolution;


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
  FProjectGraphs := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform, IGraphNode>>;
  FProjectResolutions := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform, IList<IResolution>>>;
end;

procedure TCorePackageInstallerContext.StartProject(const projectFile: string; const platform : TDPMPlatform);
begin

end;


procedure TCorePackageInstallerContext.EndProject(const projectFile: string; const platform : TDPMPlatform);
begin

end;


function TCorePackageInstallerContext.FindPackageResolution(const currentProjectFile, packageId: string; const platform: TDPMPlatform): IResolution;
var
  pair : TPair<string, IDictionary<TDPMPlatform, IList<IResolution>>>;
  resolutions :  IList<IResolution>;
begin
  result := nil;
  for pair in FProjectResolutions do
  begin
    if not SameText(currentProjectFile, pair.Key) then
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

function TCorePackageInstallerContext.IsDesignPackageInstalled(const packageName: string; out platform : TDPMPlatform; out project : string): Boolean;
begin
  result := false;
end;

procedure TCorePackageInstallerContext.RecordGraph(const projectFile: string; const platform : TDPMPlatform; const graph: IGraphNode; const resolutions : TArray<IResolution>);
var
  projectPlatformEntry : IDictionary<TDPMPlatform, IGraphNode>;
  resolutionPlatformEntry : IDictionary<TDPMPlatform, IList<IResolution>>;
begin
  if not FProjectGraphs.TryGetValue(LowerCase(projectFile), projectPlatformEntry) then
  begin
    projectPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IGraphNode>;
    FProjectGraphs[LowerCase(projectFile)] := projectPlatformEntry;
  end;
  projectPlatformEntry[platform] := graph;

  if not FProjectResolutions.TryGetValue(LowerCase(projectFile),resolutionPlatformEntry) then
  begin
    resolutionPlatformEntry := TCollections.CreateDictionary<TDPMPlatform, IList<IResolution>>;
    FProjectResolutions[LowerCase(projectFile)] := resolutionPlatformEntry;
  end;
  resolutionPlatformEntry[platform] := TCollections.CreateList<IResolution>(resolutions);

end;

function TCorePackageInstallerContext.RegisterDesignPackage(const platform : TDPMPlatform; const packageFile: string; const dependsOn: IList<string>; out errorMessage : string) : boolean;
begin
  result := true;
end;

procedure TCorePackageInstallerContext.RemoveProject(const projectFile: string);
begin
  if FProjectGraphs.ContainsKey(LowerCase(projectFile)) then
    FProjectGraphs.Remove(LowerCase(projectFile));
  if FProjectResolutions.ContainsKey(LowerCase(projectFile)) then
    FProjectResolutions.Remove(LowerCase(projectFile));
end;



end.
