unit DPM.IDE.InstallerContext;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.InstallerContext;
type

  TDPMIDEPackageInstallerContext = class(TCorePackageInstallerContext, IPackageInstallerContext)
  private
  protected
    procedure StartProject(const projectFile: string; const platform: TDPMPlatform);override;
    procedure EndProject(const projectFile: string; const platform: TDPMPlatform);override;
    function IsDesignPackageInstalled(const packageName: string): Boolean;override;
    function RegisterDesignPackage(const platform: TDPMPlatform; const packageFile: string; const dependsOn: IList<string>) : boolean;override;
    procedure Clear;override;
  public
    constructor Create(const logger : ILogger);override;
  end;

implementation

{ TDPMIDEPackageInstallerContext }

constructor TDPMIDEPackageInstallerContext.Create(const logger: ILogger);
begin
  inherited Create(logger);

end;

procedure TDPMIDEPackageInstallerContext.EndProject( const projectFile: string; const platform: TDPMPlatform);
begin

end;

function TDPMIDEPackageInstallerContext.IsDesignPackageInstalled(const packageName: string): Boolean;
begin
  result := false;
end;

function TDPMIDEPackageInstallerContext.RegisterDesignPackage(const platform: TDPMPlatform; const packageFile: string; const dependsOn: IList<string>) : boolean;
begin
  result := false;
end;

procedure TDPMIDEPackageInstallerContext.Clear;
begin
  inherited;
end;

procedure TDPMIDEPackageInstallerContext.StartProject(const projectFile: string; const platform: TDPMPlatform);
begin

end;

end.

