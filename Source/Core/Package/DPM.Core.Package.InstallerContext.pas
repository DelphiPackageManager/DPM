unit DPM.Core.Package.InstallerContext;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces;

type
  TPackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  private
    FLogger : ILogger;

    FProjectGraphs : IDictionary<string, IDictionary<TDPMPlatform,IGraphNode>>;
    //FPackageGraphs : array[TDPMPlatform] of IGraphNode;

  protected
    procedure Reset;
  public
    constructor Create(const logger : ILogger);
    procedure EndProject(const projectFile: string; const platform : TDPMPlatform);
    procedure StartProject(const projectFile: string; const platform : TDPMPlatform);
    procedure RegisterDesignPackage(const platform : TDPMPlatform; const packageFile: string; const dependsOn: IList<string>);
    function IsDesignPackageInstalled(const packageName: string): Boolean;
  end;

implementation

{ TPackageInstallerContext }

constructor TPackageInstallerContext.Create(const logger: ILogger);
begin
  FLogger := logger;
  FProjectGraphs := TCollections.CreateDictionary<string, IDictionary<TDPMPlatform,IGraphNode>>;
end;

procedure TPackageInstallerContext.StartProject(const projectFile: string; const platform : TDPMPlatform);
begin

end;


procedure TPackageInstallerContext.EndProject(const projectFile: string; const platform : TDPMPlatform);
begin

end;

function TPackageInstallerContext.IsDesignPackageInstalled(const packageName: string): Boolean;
begin
  result := false;
end;

procedure TPackageInstallerContext.RegisterDesignPackage(const platform : TDPMPlatform; const packageFile: string; const dependsOn: IList<string>);
begin

end;

procedure TPackageInstallerContext.Reset;
begin

end;


end.
