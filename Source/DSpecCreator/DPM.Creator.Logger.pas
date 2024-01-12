unit DPM.Creator.Logger;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging
  ;

type
  TDSpecLogger = class(TInterfacedObject, ILogger)
  private
    strList : TStrings;
    FVerbosity : TVerbosity;
  public
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false);
    procedure Error(const data : string);
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear; //not implemented in the console logger.
    procedure NewLine;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    constructor Create(sl: TStrings);
  end;

implementation

{ TDSpecLogger }

procedure TDSpecLogger.Clear;
begin

end;

constructor TDSpecLogger.Create(sl: TStrings);
begin
  strList := sl;
end;

procedure TDSpecLogger.Debug(const data: string);
begin
  strList.Add('DEBUG: ' + data);
end;

procedure TDSpecLogger.Error(const data: string);
begin
  strList.Add('ERROR: ' + data);
end;

function TDSpecLogger.GetVerbosity: TVerbosity;
begin
  Result := FVerbosity;
end;

procedure TDSpecLogger.Information(const data: string; const important: boolean);
begin
  strList.Add('INFORMATION: ' + data);
end;

procedure TDSpecLogger.NewLine;
begin
  strList.Add('');
end;

procedure TDSpecLogger.SetVerbosity(const value: TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDSpecLogger.Success(const data: string; const important: boolean);
begin
  strList.Add('SUCCESS: ' + data);
end;

procedure TDSpecLogger.Verbose(const data: string; const important: boolean);
begin
  strList.Add('VERBOSE: ' + data);
end;

procedure TDSpecLogger.Warning(const data: string; const important: boolean);
begin
  strList.Add('WARNING: ' + data);
end;

end.
