unit DPM.Creator.MRUService;

interface

uses
  System.Classes;

type
  IMRUSource = interface
  ['{B7CBD305-38AA-41A8-B08D-E600E406430A}']
    procedure MRUAdd(const filename : string);
    function MRURemove(const filename : string): boolean;
    procedure MRULoad(const list : TStrings);
    procedure MRUSave(const list : TStrings);
    function MRUCount: integer;
  end;

  IMRUListService = interface
  ['{DDFC7DBF-0A9D-4051-A9D9-6CFC978246AB}']
    procedure LoadMRU;
    procedure SaveMRU;
    procedure GetList(const list : TStrings);
    procedure Add(const filename : string);
    function Remove(const filename : string) : boolean;
    function GetItemCount: integer;
    procedure SetSource(const value : IMRUSource);
    function GetIniFilePath : string;
  end;

function MRUListService : IMRUListService;

implementation

uses
  System.IniFiles,
  System.SysUtils,
  System.IOUtils;

var
  _service : IMRUListService;

type
  TMRUListService = class(TInterfacedObject, IMRUListService)
  private
    FSource : IMRUSource;
    function GetIniFilePath : string;
  protected
    procedure LoadMRU;
    procedure SaveMRU;
    procedure GetList(const list : TStrings);
    procedure Add(const filename : string);
    function Remove(const filename : string) : boolean;
    function GetItemCount: integer;
    procedure SetSource(const value : IMRUSource);

  public
  end;

function MRUListService : IMRUListService;
begin
  if not Assigned(_service) then
    _service := TMRUListService.Create;
  result := _service;
end;

{ TMRUListService }

procedure TMRUListService.Add(const filename: string);
begin
  if Assigned(FSource) then
    FSource.MRUAdd(filename);
end;


function TMRUListService.GetIniFilePath: string;
begin
  result := TPath.GetHomePath;
  result := TPath.Combine(result,'.dpm\dspec-creator.ini')
end;

function TMRUListService.GetItemCount: integer;
begin
  Result := 0;
  if FSource <> nil then
    Result := FSource.MRUCount;
end;

procedure TMRUListService.GetList(const list: TStrings);
begin
  if FSource <> nil then
    FSource.MRUSave(list);
end;

procedure TMRUListService.LoadMRU;
var
  iniFilePath : string;
  iniFile : TIniFile;
  list : TStringList;
  names   : TStringList;
  i : integer;
  stmp : string;
begin
  if FSource <> nil then
  begin
    iniFilePath := GetIniFilePath;
    iniFile := TIniFile.Create(iniFilePath);
    try
      list := TStringList.Create;
      names := TStringList.Create;
      try
        iniFile.ReadSection('MRU', names);
        for i := 0 to names.Count - 1 do
        begin
          sTmp := iniFile.ReadString('MRU',names.Strings[i], '');
          if (sTmp <> '') and (FileExists(sTmp)) then
            list.Add(sTmp);
        end;
        FSource.MRULoad(list);
      finally
        list.Free;
        names.Free;
      end;
    finally
      iniFile.Free;
    end;
  end;
end;

function TMRUListService.Remove(const filename: string): boolean;
begin
  result := false;
  if FSource <> nil then
    result := FSource.MRURemove(filename);
end;

procedure TMRUListService.SaveMRU;
var
  iniFilePath : string;
  iniFile : TIniFile;
  list : TStringList;
  i : integer;
begin
  iniFilePath := GetIniFilePath;
  iniFile := TIniFile.Create(iniFilePath);
  try
     list := TStringList.Create;
    try
      FSource.MRUSave(list);
      for i := list.Count - 1 downto 0 do
        iniFile.WriteString('MRU', IntToStr(i), list.Strings[i]);
    finally
      list.Free;
    end;
  finally
    iniFile.Free;
  end;
end;

procedure TMRUListService.SetSource(const value: IMRUSource);
begin
  FSource := value;
end;

end.
