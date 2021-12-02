unit DPM.Core.Package.ListItem;

interface

uses
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces;

type
  TPackageListItem = class(TInterfacedObject, IPackageListItem)
  private
    FCompilerVersion: TCompilerVersion;
    FId: string;
    FPlatforms: string;
    FVersion: TPackageVersion;
  protected
    function GetCompilerVersion: TCompilerVersion;
    function GetId: string;
    function GetPlatforms: string;
    function GetVersion: TPackageVersion;
  public
    constructor Create(const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platforms : string);overload;
    constructor Create(const jsonObj : TJsonObject);overload;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject;   out listItem : IPackageListItem) : boolean;
  end;

implementation

uses
  System.SysUtils;

{ TPackageListItem }

constructor TPackageListItem.Create(const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platforms : string);
begin
  FId := id;
  FCompilerVersion := compilerVersion;
  FVersion := version;
  FPlatforms := platforms;
end;

constructor TPackageListItem.Create(const jsonObj: TJsonObject);
var
  id : string;
  platforms : string;
  stmp : string;
  cv : TCompilerVersion;
  packageVersion : TPackageVersion;
begin
  id := jsonObj.S['id'];
  stmp := jsonObj.S['compiler'];
  cv := StringToCompilerVersion(stmp);
  if cv = TCompilerVersion.UnknownVersion then
    raise Exception.Create('Compiler segment is not a valid version [' + stmp+ ']');
  platforms := jsonObj.S['platforms'];
  stmp := jsonObj.S['version'];
  if not TPackageVersion.TryParse(stmp, packageVersion) then
    raise Exception.Create('Version is not a valid version [' + stmp + ']');
  Create(id,cv, packageVersion, platforms);
end;

function TPackageListItem.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageListItem.GetId: string;
begin
  result := FId;
end;

function TPackageListItem.GetPlatforms: string;
begin
  result := FPlatforms;
end;

function TPackageListItem.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

class function TPackageListItem.TryLoadFromJson(const logger: ILogger; const jsonObj: TJsonObject; out listItem : IPackageListItem): boolean;
begin
  result := false;
  try
    listItem := TPackageListItem.Create(jsonObj);
  except
    on e : Exception do
    begin
      logger.Error(e.Message);
      exit
    end;
  end;
  result := true;

end;

end.
