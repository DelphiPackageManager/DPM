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
  DPM.IDE.DesignManager;

type
  TDPMIDEPackageInstallerContext = class(TCorePackageInstallerContext, IPackageInstallerContext)
  private
    FDesignManager : IDPMIDEDesignManager;
  protected
    procedure Clear;override;
    procedure RemoveProject(const projectFile : string);override;
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>) : boolean;override;
  public
    constructor Create(const logger : ILogger; const designManager : IDPMIDEDesignManager);reintroduce;
  end;

implementation

{ TDPMIDEPackageInstallerContext }

constructor TDPMIDEPackageInstallerContext.Create(const logger: ILogger; const designManager : IDPMIDEDesignManager);
begin
  inherited Create(logger);
  FDesignManager := designManager;

end;


function TDPMIDEPackageInstallerContext.InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs: IDictionary<string, IPackageSpec>): boolean;
begin
  result := true;

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

