unit DPM.IDE.Options;

interface

uses
  JsonDataObjects,
  DPM.Core.Types;

type
  IDPMIDEOptions = interface
  ['{7DC7324F-781C-4400-AFB7-E68FB9066494}']
    function LoadFromFile(const fileName : string = '') : boolean;
    function SaveToFile(const fileName : string = '') : boolean;

    function GetOptionsFileName : string;

    function GetLogVerbosity : TVerbosity;
    procedure SetLogVebosity(const value : TVerbosity);

    function GetShowLogForRestore : boolean;
    procedure SetShowLogForRestore(const value : boolean);
    function GetShowLogForInstall : boolean;
    procedure SetShowLogForInstall(const value : boolean);
    function GetShowLogForUnInstall : boolean;
    procedure SetShowLogForUnInstall(const value : boolean);

    function GetAutoCloseLogOnSuccess : boolean;
    procedure SetAutoCloseLogOnSuccess(const value : boolean);
    function GetAutoCloseLogDelaySeconds : integer;
    procedure SetAutoCloseLogDelaySelcond(const value : integer);

    function GetAddDPMToProjectTree : boolean;
    procedure SetAddDPMToProjectTree(const value : boolean);

    function GetLogWindowWidth : integer;
    procedure SetLogWindowWidth(const value : integer);
    function GetLogWindowHeight : integer;
    procedure SetLogWindowHeight(const value : integer);

    property LogVerbosity : TVerbosity read GetLogVerbosity write SetLogVebosity;
    property LogWindowWidth : integer read GetLogWindowWidth write SetLogWindowWidth;
    property LogWindowHeight : integer read GetLogWindowHeight write SetLogWindowHeight;

    property ShowLogForRestore : boolean read GetShowLogForRestore write SetShowLogForRestore;
    property ShowLogForInstall : boolean read GetShowLogForInstall write SetShowLogForInstall;
    property ShowLogForUninstall : boolean read GetShowLogForUninstall write SetShowLogForUninstall;

    property AutoCloseLogOnSuccess : boolean read GetAutoCloseLogOnSuccess write SetAutoCloseLogOnSuccess;
    property AutoCloseLogDelaySeconds : integer read GetAutoCloseLogDelaySeconds write SetAutoCloseLogDelaySelcond;

    property AddDPMToProjectTree : boolean read GetAddDPMToProjectTree write SetAddDPMToProjectTree;

    property FileName : string read GetOptionsFileName;
  end;

  TDPMIDEOptions = class(TInterfacedObject, IDPMIDEOptions)
  private
    FFileName : string;
    FVerbosity : TVerbosity;
    FShowLogForRestore : boolean;
    FSHowLogForInstall : boolean;
    FShowLogForUninstall : boolean;
    FAutoCloseOnSuccess : boolean;
    FAutoCloseLogDelaySeconds : integer;
    FAddDPMToProjectTree : boolean;
    FLogWindowWidth : integer;
    FLogWindowHeight : integer;
  protected
    function LoadFromJson(const jsonObj : TJsonObject) : boolean;
    function SaveToJson(const parentObj : TJsonObject) : boolean;

    function LoadFromFile(const fileName: string = ''): Boolean;
    function SaveToFile(const fileName: string = ''): Boolean;

    function GetOptionsFileName : string;

    function GetLogVerbosity : TVerbosity;
    procedure SetLogVebosity(const value : TVerbosity);

    function GetShowLogForRestore : boolean;
    procedure SetShowLogForRestore(const value : boolean);

    function GetShowLogForInstall : boolean;
    procedure SetShowLogForInstall(const value : boolean);

    function GetShowLogForUnInstall : boolean;
    procedure SetShowLogForUnInstall(const value : boolean);

    function GetAutoCloseLogOnSuccess : boolean;
    procedure SetAutoCloseLogOnSuccess(const value : boolean);

    function GetAutoCloseLogDelaySeconds : integer;
    procedure SetAutoCloseLogDelaySelcond(const value : integer);

    function GetAddDPMToProjectTree : boolean;
    procedure SetAddDPMToProjectTree(const value : boolean);

    function GetLogWindowWidth : integer;
    procedure SetLogWindowWidth(const value : integer);
    function GetLogWindowHeight : integer;
    procedure SetLogWindowHeight(const value : integer);


  public
    constructor Create;
  end;


implementation

uses
  System.SysUtils,
  System.TypInfo,
  DPM.IDE.Types,
  DPM.Core.Utils.System;

{ TDPMIDEOptions }

constructor TDPMIDEOptions.Create;
begin
  FFileName := TSystemUtils.ExpandEnvironmentStrings(cDPMIDEDefaultOptionsFile);
  {$IFDEF DEBUG}
  FVerbosity := TVerbosity.Debug;
  {$ELSE}
  FVerbosity := TVerbosity.Normal;
  {$ENDIF}
  FShowLogForRestore := true;
  FShowLogForRestore := true;
  FShowLogForUninstall := true;
  FAutoCloseOnSuccess := true;
  FAutoCloseLogDelaySeconds := 3;
  FAddDPMToProjectTree := true;
  FLogWindowWidth := 800;
  FLogWindowHeight := 500;
end;

function TDPMIDEOptions.GetAddDPMToProjectTree: boolean;
begin
  result := FAddDPMToProjectTree;
end;

function TDPMIDEOptions.GetAutoCloseLogDelaySeconds: integer;
begin
  result := FAutoCloseLogDelaySeconds;
end;

function TDPMIDEOptions.GetAutoCloseLogOnSuccess: boolean;
begin
  result := FAutoCloseOnSuccess;
end;

function TDPMIDEOptions.GetLogVerbosity: TVerbosity;
begin
  result := FVerbosity;
end;


function TDPMIDEOptions.GetLogWindowHeight: integer;
begin
  result := FLogWindowHeight;
end;

function TDPMIDEOptions.GetLogWindowWidth: integer;
begin
  result := FLogWindowWidth
end;

function TDPMIDEOptions.GetOptionsFileName: string;
begin
  result := FFileName;
end;

function TDPMIDEOptions.GetShowLogForInstall: boolean;
begin
  result := FSHowLogForInstall;
end;

function TDPMIDEOptions.GetShowLogForRestore: boolean;
begin
  result := FShowLogForRestore;
end;

function TDPMIDEOptions.GetShowLogForUnInstall: boolean;
begin
  result := FShowLogForUninstall;
end;

function TDPMIDEOptions.LoadFromFile(const fileName: string): Boolean;
var
  jsonObj : TJsonObject;
  sFileName : string;
begin
  if fileName <> '' then
    sFileName := fileName
  else
    sFileName := FFileName;

  if sFileName = '' then
    raise Exception.Create('No filename set for config file, unable to load');


  try
    jsonObj := TJsonObject.ParseFromFile(sFileName) as TJsonObject;
    try
      Result := LoadFromJson(jsonObj);
      FFileName := sFileName;
    finally
      jsonObj.Free;
    end;
  except
    on e : Exception do
      raise Exception.Create('Exception while loading config file [' + fileName + ']' + #13#10 + e.Message);
  end;
end;

function TDPMIDEOptions.LoadFromJson(const jsonObj: TJsonObject): boolean;
var
  sVerbosity : string;
  iValue : integer;
begin
  result := false;
  sVerbosity := jsonObj.s['logverbosity'];
  if sVerbosity <> '' then
  begin
    iValue := GetEnumValue(TypeInfo(TVerbosity),sVerbosity);
    if iValue <> -1 then
      FVerbosity := TVerbosity(iValue);
  end;

  if jsonObj.Contains('showlogforrestore') then
    FShowLogForRestore := jsonObj.B['showlogforrestore'];

  if jsonObj.Contains('showlogforinstall') then
    FShowLogForInstall := jsonObj.B['showlogforinstall'];

  if jsonObj.Contains('showlogforuninstall') then
    FShowLogForInstall := jsonObj.B['showlogforuninstall'];

  if jsonObj.Contains('autocloselogonsuccess') then
    FAutoCloseOnSuccess := jsonObj.B['autocloselogonsuccess'];

  if jsonObj.Contains('autocloselogdelayseconds') then
    FAutoCloseLogDelaySeconds := jsonObj.I['autocloselogdelayseconds'];

  if jsonObj.Contains('addtoprojecttree') then
    FAddDPMToProjectTree := jsonObj.B['addtoprojecttree'];

  if jsonObj.Contains('logwindowwidth') then
    FLogWindowWidth := jsonObj.I['logwindowwidth'];

  if jsonObj.Contains('logwindowheight') then
    FLogWindowHeight := jsonObj.I['logwindowheight'];

end;

function TDPMIDEOptions.SaveToFile(const fileName: string): Boolean;
var
  sFileName : string;
  jsonObj : TJsonObject;
begin
  if fileName <> '' then
    sFileName := fileName
  else
    sFileName := FFileName;

  if sFileName = '' then
    raise Exception.Create('No filename set for config file, unable to save');

  jsonObj := TJsonObject.Create;
  try
    try
      result := SaveToJson(jsonObj);
      if result then
        jsonObj.SaveToFile(sFileName, false);
      FFileName := sFileName;
    except
      on e : Exception do
        raise Exception.Create('Exception while saving config to file [' + sFileName + ']' + #13#10 + e.Message);
    end;
  finally
    jsonObj.Free;
  end;
end;

function TDPMIDEOptions.SaveToJson(const parentObj: TJsonObject): boolean;
begin
  parentObj.S['logverbosity'] := GetEnumName(TypeInfo(TVerbosity), Ord(FVerbosity));
  parentObj.B['showlogforrestore'] := FShowLogForRestore;
  parentObj.B['showlogforinstall'] := FShowLogForInstall;
  parentObj.B['showlogforuninstall'] := FShowLogForUninstall;
  parentObj.B['autocloselogonsuccess'] := FAutoCloseOnSuccess;
  parentObj.I['autocloselogdelayseconds'] := FAutoCloseLogDelaySeconds;
  parentObj.B['addtoprojecttree'] := FAddDPMToProjectTree;
  parentObj.I['logwindowwidth'] := FLogWindowWidth;
  parentObj.I['logwindowheight'] := FLogWindowHeight;

  result := true;
end;

procedure TDPMIDEOptions.SetAddDPMToProjectTree(const value: boolean);
begin
  FAddDPMToProjectTree := value;
end;

procedure TDPMIDEOptions.SetAutoCloseLogDelaySelcond(const value: integer);
begin
  FAutoCloseLogDelaySeconds := value;
  if FAutoCloseLogDelaySeconds < 0 then
    FAutoCloseLogDelaySeconds := 0;
end;

procedure TDPMIDEOptions.SetAutoCloseLogOnSuccess(const value: boolean);
begin
  FAutoCloseOnSuccess := value;
end;

procedure TDPMIDEOptions.SetLogVebosity(const value: TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDPMIDEOptions.SetLogWindowHeight(const value: integer);
begin
  FLogWindowHeight := value;
end;

procedure TDPMIDEOptions.SetLogWindowWidth(const value: integer);
begin
  FLogWindowWidth := value;
end;

procedure TDPMIDEOptions.SetShowLogForInstall(const value: boolean);
begin
  FSHowLogForInstall := value;
end;

procedure TDPMIDEOptions.SetShowLogForRestore(const value: boolean);
begin
  FShowLogForRestore := value;
end;

procedure TDPMIDEOptions.SetShowLogForUnInstall(const value: boolean);
begin
  FShowLogForUninstall := value;
end;

end.
