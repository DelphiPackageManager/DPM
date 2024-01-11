unit DPM.IDE.InstallerContext;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Spec.Interfaces,
  DPM.IDE.PathManager;

type
  TDPMIDEPackageInstallerContext = class(TCorePackageInstallerContext, IPackageInstallerContext)
  private
    FPathManager : IDPMIDEPathManager;
  protected
    procedure Clear;override;
    procedure RemoveProject(const projectFile : string);override;
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageSpecs: IDictionary<string, IPackageSpec>) : boolean;override;
  public
    constructor Create(const logger : ILogger; const pathManager : IDPMIDEPathManager);reintroduce;
  end;

implementation

uses
  System.SysUtils;

{ TDPMIDEPackageInstallerContext }

constructor TDPMIDEPackageInstallerContext.Create(const logger: ILogger; const pathManager : IDPMIDEPathManager);
begin
  inherited Create(logger);
  FPathManager := pathManager;

end;


function TDPMIDEPackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
var
  projectGraph : IPackageReference;
begin
  result := true;
  projectGraph := GetProjectGraph(projectFile, platform);
  if projectGraph = nil then
  begin
    Logger.Error(Format('No projectGraph recorded for : %s platform : %s',[projectFile,DPMPlatformToString(platform)]));


  end;



end;

procedure TDPMIDEPackageInstallerContext.RemoveProject(const projectFile: string);
begin
  inherited;
  //unstall any unused design time packages.
end;

procedure TDPMIDEPackageInstallerContext.Clear;
begin
  inherited;
  //uninstall any design time packages.
end;

end.

