unit DPM.Core.Manifest;

interface

uses
  JsonDataObjects,
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
    function LoadTargetPlatformFromJson(const targetPlatformsArray : TJsonArray) : boolean;
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
  public
    constructor Create(const logger : ILogger; const fileName : string); reintroduce;
  end;

implementation

uses
  DPM.Core.Spec.MetaData,
  DPM.Core.Spec.TargetPlatform;

{ TPackageManifest }

constructor TPackageManifest.Create(const logger: ILogger; const fileName: string);
begin
  FLogger := logger;
  FFileName := fileName;
  FMetaData := TSpecMetaData.Create(logger);
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

function TPackageManifest.LoadFromJson(const jsonObject: TJsonObject): boolean;
var
  metaDataObj : TJsonObject;
  targetPlatformsArray : TJsonArray;
begin
  FIsValid := false;
  //Logger.Debug('Reading spec metadata');
  if not jsonObject.Contains('metadata') then
  begin
    FLogger.Error('Required element [metadata] not found!');
    result := false;
  end
  else
  begin
    metaDataObj := jsonObject.O['metadata'];
    result := FMetaData.LoadFromJson(metaDataObj)
  end;

  if not jsonObject.Contains('targetPlatforms') then
  begin
    FLogger.Error('Required element [targetPlatforms] not found!');
    result := false;
  end
  else
  begin
    //Logger.Debug('Reading spec targetPlatforms');
    targetPlatformsArray := jsonObject.A['targetPlatforms'];
    result := LoadTargetPlatformFromJson(targetPlatformsArray) and result;
  end;

  FIsValid := result;
end;


function TPackageManifest.LoadTargetPlatformFromJson(const targetPlatformsArray: TJsonArray): boolean;
begin
  if targetPlatformsArray.Count = 0 then
  begin
    FLogger.Error('No targetPlatforms found, at 1 is required');
    exit(false);
  end;
  if targetPlatformsArray.Count > 1 then
  begin
    FLogger.Error('More than one targetPlatform found, only 1 is allowed in a package manifest');
    exit(false);
  end;

  FTargetPlatform := TSpecTargetPlatform.Create(FLogger);
  result := FTargetPlatform.LoadFromJson(targetPlatformsArray.O[0]);

end;

end.
