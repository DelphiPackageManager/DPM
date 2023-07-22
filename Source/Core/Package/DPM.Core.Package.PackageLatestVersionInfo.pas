unit DPM.Core.Package.PackageLatestVersionInfo;

interface

uses
  DPM.Core.Types,
  DPM.Core.Package.Interfaces;

type
  TDPMPackageLatestVersionInfo = class(TInterfacedObject, IPackageLatestVersionInfo)
  private
    FId : string;
    FLatestStableVersion: TPackageVersion;
    FLatestVersion: TPackageVersion;
  protected
   function GetId: string;
   function GetLatestStableVersion: TPackageVersion;
   function GetLatestVersion: TPackageVersion;
   procedure SetLatestStableVersion(const value : TPackageVersion);
   procedure SetLatestVersion(const value : TPackageVersion);

  public
    constructor Create(const id : string; const latestStableVer : TPackageVersion; const latestVersion : TPackageVersion);
  end;


implementation

{ TDPMPackageLatestVersionInfo }

constructor TDPMPackageLatestVersionInfo.Create(const id: string; const latestStableVer, latestVersion: TPackageVersion);
begin
  FId := id;
  FLatestStableVersion := latestStableVer;
  FLatestVersion := latestVersion;
end;

function TDPMPackageLatestVersionInfo.GetId: string;
begin
  result := FId;
end;

function TDPMPackageLatestVersionInfo.GetLatestStableVersion: TPackageVersion;
begin
  result := FLatestStableVersion;
end;

function TDPMPackageLatestVersionInfo.GetLatestVersion: TPackageVersion;
begin
  result := FLatestVersion;
end;

procedure TDPMPackageLatestVersionInfo.SetLatestStableVersion(const value: TPackageVersion);
begin
  FLatestStableVersion := value;
end;

procedure TDPMPackageLatestVersionInfo.SetLatestVersion(const value: TPackageVersion);
begin
  FLatestVersion := value;
end;

end.
