unit TestLogger;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics;

type
  //Captures everything logged so a test can assert that a message was - or, just as often, was
  //not - emitted. Hold the ILogger reference for the life of the assertions; the concrete
  //instance is reference counted like any other TInterfacedObject.
  TTestLogger = class(TBaseLogger)
  private
    FMessages : TStringList;
    FDiagnostics : TStringList;
    FWarningCount : integer;
  protected
    function IsEnabled(const level : TLogLevel; const important : boolean) : boolean; override;
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); override;
    procedure DiagnosticReported(const level : TLogLevel; const code : string; const data : string; const duplicate : boolean); override;
    procedure DoClear; override;
  public
    constructor Create;
    destructor Destroy; override;

    //Every message logged (duplicate warnings/errors only once - as the user would see them), one per
    //line, as '<level>: <text>' - eg 'error: DPME1101: Package [x] was not found.'
    function Messages : TStrings;
    //True when any message at this level contains text (case insensitive).
    function Logged(const level : string; const text : string) : boolean;
    //Every warning and error reported, duplicates included, as '<level> <code>' ('error DPME1101'),
    //'<level> -' for an uncoded one.
    function Diagnostics : TStrings;
    function HasError(const code : TErrorCode) : boolean;
    function HasWarning(const code : TWarningCode) : boolean;
    //warnings reported, duplicates included (ErrorCount counts errors).
    function WarningCount : integer;
  end;

implementation

uses
  System.SysUtils;

const
  cLevelNames : array[TLogLevel] of string = ('debug', 'verbose', 'information', 'success', 'warning', 'error');

{ TTestLogger }

constructor TTestLogger.Create;
begin
  inherited Create;
  FMessages := TStringList.Create;
  FDiagnostics := TStringList.Create;
end;

destructor TTestLogger.Destroy;
begin
  FMessages.Free;
  FDiagnostics.Free;
  inherited;
end;

function TTestLogger.IsEnabled(const level : TLogLevel; const important : boolean) : boolean;
begin
  //capture everything, whatever the verbosity.
  result := true;
end;

procedure TTestLogger.WriteMessage(const level : TLogLevel; const text : string; const important : boolean);
var
  data : string;
begin
  //warnings and errors arrive formatted - 'error DPME1101: text', or 'error: text' when uncoded. Keep
  //the '<level>: ' prefix the assertions match on, followed by the code if there is one.
  data := text;
  if level in [TLogLevel.Warning, TLogLevel.Error] then
  begin
    data := Copy(text, Length(cLevelNames[level]) + 1, MaxInt);
    if Pos(': ', data) = 1 then
      data := Copy(data, 3, MaxInt)
    else
      data := Copy(data, 2, MaxInt);
  end;
  FMessages.Add(cLevelNames[level] + ': ' + data);
end;

procedure TTestLogger.DiagnosticReported(const level : TLogLevel; const code : string; const data : string; const duplicate : boolean);
begin
  if level = TLogLevel.Warning then
    Inc(FWarningCount);
  if code <> '' then
    FDiagnostics.Add(cLevelNames[level] + ' ' + code)
  else
    FDiagnostics.Add(cLevelNames[level] + ' -');
end;

procedure TTestLogger.DoClear;
begin
  FMessages.Clear;
end;

function TTestLogger.Messages : TStrings;
begin
  result := FMessages;
end;

function TTestLogger.Diagnostics : TStrings;
begin
  result := FDiagnostics;
end;

function TTestLogger.HasError(const code : TErrorCode) : boolean;
begin
  result := FDiagnostics.IndexOf('error ' + ErrorCodeToString(code)) >= 0;
end;

function TTestLogger.HasWarning(const code : TWarningCode) : boolean;
begin
  result := FDiagnostics.IndexOf('warning ' + WarningCodeToString(code)) >= 0;
end;

function TTestLogger.WarningCount : integer;
begin
  result := FWarningCount;
end;

function TTestLogger.Logged(const level, text: string): boolean;
var
  i : integer;
  prefix : string;
  line : string;
begin
  result := false;
  prefix := LowerCase(level) + ': ';
  for i := 0 to FMessages.Count - 1 do
  begin
    line := LowerCase(FMessages.Strings[i]);
    if Pos(prefix, line) <> 1 then
      continue;
    if Pos(LowerCase(text), line) > 0 then
      exit(true);
  end;
end;

end.
