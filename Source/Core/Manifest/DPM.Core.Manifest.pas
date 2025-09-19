unit DPM.Core.Manifest;

interface

uses
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Manifest.Interfaces;

type
  TPackageManifest = class(TInterfacedObject, IPackageManifest)
  private
    FLogger : ILogger;
    FMetaData : ISpecMetaData;
    FTargetPlatform : ISpecTargetPlatform;
    FIsValid : boolean;
    FFileName : string;
  protected
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetIsValid : boolean;
    function GetFileName : string;
    function LoadTargetPlatformFromYAML(const targetPlatforms : IYAMLSequence) : boolean;
    function LoadFromYAML(const yamlObj : IYAMLMapping) : boolean;
  public
    constructor Create(const logger : ILogger; const fileName : string; packageKind : TDPMPackageKind);
    destructor Destroy;override;
  end;

implementation

uses
  DPM.Core.Spec.MetaData,
  DPM.Core.Spec.TargetPlatform;

{ TPackageManifest }

constructor TPackageManifest.Create(const logger: ILogger; const fileName: string; packageKind : TDPMPackageKind);
begin
  inherited Create;
  FLogger := logger;
  FFileName := fileName;
  //FLogger.Debug('Creating manifest for : ' + FFileName);
  FMetaData := TSpecMetaData.Create(logger, packageKind) as ISpecMetaData;

end;

destructor TPackageManifest.Destroy;
begin
  FTargetPlatform := nil;
  //FLogger.Debug('destroying manifest for : ' + FFileName);
  FMetaData := nil;
  FLogger := nil;
  inherited;
end;

function TPackageManifest.GetFileName: string;
begin
  result := FFileName;
end;

function TPackageManifest.GetIsValid: boolean;
begin
  result := FIsValid;
end;

function TPackageManifest.GetMetaData: ISpecMetaData;
begin
  result :=  FMetaData;
end;

function TPackageManifest.GetTargetPlatform: ISpecTargetPlatform;
begin
  result := FTargetPlatform;
end;

function TPackageManifest.LoadFromYAML(const yamlObj : IYAMLMapping) : boolean;
var
  metaDataO : IYAMLMapping;
  targetPlatformsSeq : IYAMLSequence;
begin
  metaDataO := yamlObj.O['metadata'];
  FMetaData.LoadFromYAML(metaDataO);

  targetPlatformsSeq := yamlObj.A['targetPlatforms'];
  result := LoadTargetPlatformFromYAML(targetPlatformsSeq);
end;



function TPackageManifest.LoadTargetPlatformFromYAML(const targetPlatforms : IYAMLSequence): boolean;
begin
  if targetPlatforms.Count = 0 then
  begin
    FLogger.Error('No targetPlatforms found, at 1 is required');
    exit(false);
  end;
  if targetPlatforms.Count > 1 then
  begin
    FLogger.Error('More than one targetPlatform found, only 1 is allowed in a package manifest');
    exit(false);
  end;

  FTargetPlatform := TSpecTargetPlatform.Create(FLogger);
  result := FTargetPlatform.LoadFromYAML(targetPlatforms.O[0]);

end;

end.
