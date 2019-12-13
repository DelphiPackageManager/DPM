unit DPM.Core.Repository.PackageInfo;

interface

uses
  DPM.Core.Types,
  DPM.Core.Repository.Interfaces;

type
  TPackageInfo = class(TInterfacedObject, IPackageInfo)
  private
    FCompilerVersion: TCompilerVersion;
    FId: string;
    FPlatform: TDPMPlatform;
    FSourceName: string;
    FVersion: TPackageVersion;
  protected
    function GetCompilerVersion: TCompilerVersion;
    function GetId: string;
    function GetPlatform: TDPMPlatform;
    function GetSourceName: string;
    function GetVersion: TPackageVersion;
  public
    function ToString : string;override;
    constructor Create(const id, source : string; const version : TPackageVersion; const compilerVersion: TCompilerVersion; const platform : TDPMPlatform);
  end;

//  TPackageInfoComparer = class

implementation

{ TPackageInfo }

constructor TPackageInfo.Create(const id, source : string; const version : TPackageVersion; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform);
begin
  FId := id;
  FVersion := version;
  FSourceName := source;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
end;

function TPackageInfo.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageInfo.GetId: string;
begin
  result := FId;
end;

function TPackageInfo.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageInfo.GetSourceName: string;
begin
  result := FSourceName;
end;

function TPackageInfo.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

function TPackageInfo.ToString: string;
begin
  result := FId +'-' + CompilerToString(FCompilerVersion) + '-' + DPMPlatformToString(FPlatform) + '-' + FVersion.ToString;
end;

end.
