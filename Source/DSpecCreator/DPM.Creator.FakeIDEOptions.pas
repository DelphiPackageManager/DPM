unit DPM.Creator.FakeIDEOptions;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.IDE.Options
  ;

type
  TFakeIDE = class(TInterfacedObject, IDPMIDEOptions)
  private
    FVerbosity: TVerbosity;
    Ffilename : string;
    FHeight: Integer;
    FWidth: Integer;
    FShowLogForInstall: Boolean;
    FShowLogForRestore: Boolean;
    FShowLogForUnInstall: Boolean;
    FAutoCloseLogOnSuccess: Boolean;
    FAddDPMToProjectTree: Boolean;
    FAutoCloseLogDelaySeconds : Integer;
  public
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
  end;


implementation

{ TFakeIDE }

function TFakeIDE.GetAddDPMToProjectTree: boolean;
begin
  Result := FAddDPMToProjectTree;
end;

function TFakeIDE.GetAutoCloseLogDelaySeconds: integer;
begin
  Result := FAutoCloseLogDelaySeconds;
end;

function TFakeIDE.GetAutoCloseLogOnSuccess: boolean;
begin
  Result := FAutoCloseLogOnSuccess;
end;

function TFakeIDE.GetLogVerbosity: TVerbosity;
begin
  Result := FVerbosity;
end;

function TFakeIDE.GetLogWindowHeight: integer;
begin
  Result := FHeight;
end;

function TFakeIDE.GetLogWindowWidth: integer;
begin
  Result := FWidth;
end;

function TFakeIDE.GetOptionsFileName: string;
begin
  Result := Ffilename;
end;

function TFakeIDE.GetShowLogForInstall: boolean;
begin
  Result := FShowLogForInstall;
end;

function TFakeIDE.GetShowLogForRestore: boolean;
begin
  Result := FShowLogForRestore;
end;

function TFakeIDE.GetShowLogForUnInstall: boolean;
begin
  Result := FShowLogForUnInstall;
end;

function TFakeIDE.LoadFromFile(const fileName: string): boolean;
begin

end;

function TFakeIDE.SaveToFile(const fileName: string): boolean;
begin

end;

procedure TFakeIDE.SetAddDPMToProjectTree(const value: boolean);
begin
  FAddDPMToProjectTree := value;
end;

procedure TFakeIDE.SetAutoCloseLogDelaySelcond(const value: integer);
begin
  FAutoCloseLogDelaySeconds := value;
end;

procedure TFakeIDE.SetAutoCloseLogOnSuccess(const value: boolean);
begin
  FAutoCloseLogOnSuccess := value;
end;

procedure TFakeIDE.SetLogVebosity(const value: TVerbosity);
begin
  FVerbosity := value;
end;

procedure TFakeIDE.SetLogWindowHeight(const value: integer);
begin
  FHeight := value;
end;

procedure TFakeIDE.SetLogWindowWidth(const value: integer);
begin
  FWidth := value;
end;

procedure TFakeIDE.SetShowLogForInstall(const value: boolean);
begin
  FShowLogForInstall := value;
end;

procedure TFakeIDE.SetShowLogForRestore(const value: boolean);
begin
  FShowLogForRestore := value;
end;

procedure TFakeIDE.SetShowLogForUnInstall(const value: boolean);
begin
  FShowLogForUnInstall := value;
end;

end.
