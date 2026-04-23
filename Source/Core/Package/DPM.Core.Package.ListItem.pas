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
    FPlatforms: TDPMPlatforms;
    FVersion: TPackageVersion;
  protected
    function GetCompilerVersion: TCompilerVersion;
    function GetId: string;
    function GetPlatforms: TDPMPlatforms;
    function GetVersion: TPackageVersion;

    procedure SetPlatforms(const value : TDPMPlatforms);
    function IsSamePackageVersion(const item : IPackageListItem) : Boolean;
    function IsSamePackageId(const item : IPackageListItem) : boolean;
    function MergeWith(const item : IPackageListItem) : IPackageListItem;

  public
    constructor Create(const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platforms : TDPMPlatforms);overload;
    constructor Create(const jsonObj : TJsonObject);overload;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject;   out listItem : IPackageListItem) : boolean;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TPackageListItem }

constructor TPackageListItem.Create(const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platforms : TDPMPlatforms);
begin
  FId := id;
  FCompilerVersion := compilerVersion;
  FVersion := version;
  FPlatforms := platforms;
end;

constructor TPackageListItem.Create(const jsonObj: TJsonObject);
var
  id : string;
  platforms : TDPMPlatforms;
  stmp : string;
  cv : TCompilerVersion;
  packageVersion : TPackageVersion;
begin
  id := jsonObj.S['id'];
  stmp := jsonObj.S['compiler'];
  cv := StringToCompilerVersion(stmp);
  if cv = TCompilerVersion.UnknownVersion then
    raise Exception.Create('Compiler segment is not a valid version [' + stmp+ ']');
  platforms := StringToDPMPlatforms(jsonObj.S['platforms']);
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

function TPackageListItem.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TPackageListItem.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

function TPackageListItem.IsSamePackageId(const item: IPackageListItem): boolean;
begin
  result := (FCompilerVersion = item.CompilerVersion) and (FId = item.Id);
end;

function TPackageListItem.IsSamePackageVersion(const item: IPackageListItem): Boolean;
begin
  result := (FCompilerVersion = item.CompilerVersion) and (FId = item.Id) and (FVersion = item.Version);
end;

function TPackageListItem.MergeWith(const item: IPackageListItem): IPackageListItem;
begin
  Assert(IsSamePackageVersion(item));
  result := TPackageListItem.Create(FId, FCompilerVersion, FVersion, FPlatforms + item.Platforms);
end;

procedure TPackageListItem.SetPlatforms(const value: TDPMPlatforms);
begin
  FPlatforms := value;
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
