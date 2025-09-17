unit DPM.Core.Manifest.Reader;

interface

uses
  JsonDataObjects,
  VSoft.YAML,
  DPM.Core.Logging,
  DPM.Core.Manifest.Interfaces;

type
  TPackageManifestReader = class(TInterfacedObject, IPackageManifestReader)
  private
    FLogger : ILogger;
  protected
    function InternalReadPackageSpecJson(const fileName : string; const jsonObject : TJsonObject) : IPackageManifest;
    function InternalReadPackageSpecYaml(const fileName : string; const yamlObject : IYamlMapping) : IPackageManifest;
    function ReadManifest(const fileName : string) : IPackageManifest;
    function ReadManifestString(const manifestString : string) : IPackageManifest;

  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Manifest;

{ TPackageManifestReader }

constructor TPackageManifestReader.Create(const logger: ILogger);
begin
  FLogger := logger;
end;

function TPackageManifestReader.InternalReadPackageSpecJson(const fileName: string; const jsonObject: TJsonObject): IPackageManifest;
begin
  result := nil;
  if not jsonObject.Contains('metadata') then
  begin
    FLogger.Error('json document does not have a metadata object, this is probably not a dspec file');
    exit;
  end;
  result := TPackageManifest.Create(FLogger, fileName, TDPMPackageKind.dpm);
  result.LoadFromJson(jsonObject);
end;

function TPackageManifestReader.InternalReadPackageSpecYaml(const fileName: string; const yamlObject: IYamlMapping): IPackageManifest;
begin

end;

function TPackageManifestReader.ReadManifest(const fileName: string): IPackageManifest;
var
  jsonObj : TJsonObject;
  ext : string;
  yamlDoc : IYAMLDocument;
begin
  result := nil;
  if not FileExists(fileName) then
  begin
    FLogger.Error('Pacakge manifest file : [' + filename + '] does not exist');
    exit;
  end;
  ext := ExtractFileExt(fileName);
  if SameText('.yaml', ext) then
  begin
    try
      yamlDoc := TYAML.LoadFromFile(fileName);
      result := InternalReadPackageSpecYaml(filename, yamlDoc.AsMapping);
    except
      on e : Exception do
      begin
        result := nil;
        FLogger.Error('Error parsing package manifest json : ' + e.Message);
        exit;
      end;
    end;
  end
  else
  begin
    try
      jsonObj := TJsonObject.ParseFromFile(fileName) as TJsonObject;
      try
        Result := InternalReadPackageSpecJson(fileName, jsonObj);
      finally
        jsonObj.Free;
      end;
    except
      on e : Exception do
      begin
        result := nil;
        FLogger.Error('Error parsing package manifest json : ' + e.Message);
        exit;
      end;
    end;
  end;

end;

function TPackageManifestReader.ReadManifestString(const manifestString: string): IPackageManifest;
var
  jsonObj : TJsonObject;
begin
  result := nil;
  if manifestString = '' then
  begin
    FLogger.Error('Manifest string is empty!');
    exit;
  end;

  try
    jsonObj := TJsonObject.Parse(manifestString) as TJsonObject;
    try
      Result := InternalReadPackageSpecJson('', jsonObj);
    finally
      jsonObj.Free;
    end;
  except
    on e : Exception do
    begin
      FLogger.Error('Error parsing package manifest json : ' + e.Message);
      exit;
    end;
  end;

end;

end.
