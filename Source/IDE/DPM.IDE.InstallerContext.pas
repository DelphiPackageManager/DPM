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
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests: IDictionary<string, IPackageSpec>) : boolean;override;
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


function TDPMIDEPackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageManifests: IDictionary<string, IPackageSpec>): boolean;
begin
  //design-time package installation is not yet implemented on the single-package-per-compiler branch.
  //the core installer calls this once per project after all platforms have been processed.
  result := true;
  Logger.Debug('InstallDesignPackages: not implemented in current branch (project: ' + projectFile + ')');
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
