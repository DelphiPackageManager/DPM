unit DPM.Core.Package.InstallerContext;

interface

uses
  DPM.Core.Package.Interfaces;

type
  TPackageInstallerContext = class(TInterfacedObject, IPackageInstallerContext)
  private

  protected
    procedure Reset;
  public

  end;

implementation

{ TPackageInstallerContext }

procedure TPackageInstallerContext.Reset;
begin

end;

end.
