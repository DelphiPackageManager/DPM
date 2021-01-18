unit DPM.Core.Package.InstallerContext;

interface

uses
  Spring.Collections,
  DPM.Core.Package.Interfaces;

type
  TPackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  private

  protected
    procedure Reset;
  public
    procedure EndProject(const projectFile: string);
    procedure StartProject(const projectFile: string);
    procedure RegisterDesignPackage(const packageFile: string; const dependsOn: IList<string>);
    function IsDesignPackageInstalled(const packageName: string): Boolean;

  end;

implementation

{ TPackageInstallerContext }

procedure TPackageInstallerContext.EndProject(const projectFile: string);
begin

end;

function TPackageInstallerContext.IsDesignPackageInstalled(const packageName: string): Boolean;
begin
  result := false;
end;

procedure TPackageInstallerContext.RegisterDesignPackage(const packageFile: string; const dependsOn: IList<string>);
begin

end;

procedure TPackageInstallerContext.Reset;
begin

end;

procedure TPackageInstallerContext.StartProject(const projectFile: string);
begin

end;

end.
