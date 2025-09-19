unit DPM.Core.Manifest.Reader;

interface

uses
  VSoft.YAML,
  DPM.Core.Logging,
  DPM.Core.Manifest.Interfaces;

type
  TPackageManifestReader = class(TInterfacedObject, IPackageManifestReader)
  private
    FLogger : ILogger;
  protected
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

function TPackageManifestReader.InternalReadPackageSpecYaml(const fileName: string; const yamlObject: IYamlMapping): IPackageManifest;
begin
  result := nil;
  if not yamlObject.Contains('metadata') then
  begin
    FLogger.Error('json document does not have a metadata object, this is probably not a dspec file');
    exit;
  end;
  result := TPackageManifest.Create(FLogger, fileName, TDPMPackageKind.dpm);
  result.LoadFromYAML(yamlObject);

end;

function TPackageManifestReader.ReadManifest(const fileName: string): IPackageManifest;
var
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
  end;
end;

function TPackageManifestReader.ReadManifestString(const manifestString: string): IPackageManifest;
var
  yamlDoc: IYAMLDocument;
begin
  result := nil;
  if manifestString = '' then
  begin
    FLogger.Error('Manifest string is empty!');
    exit;
  end;

  try
    yamlDoc := TYAML.LoadFromString(manifestString);
    Result := InternalReadPackageSpecYaml('', yamlDoc.Root.AsMapping);
  except
    on e : Exception do
    begin
      FLogger.Error('Error parsing package manifest json : ' + e.Message);
      exit;
    end;
  end;

end;

end.
