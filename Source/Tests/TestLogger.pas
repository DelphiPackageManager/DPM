unit TestLogger;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging;

type
  //Captures everything logged so a test can assert that a message was - or, just as often, was
  //not - emitted. Hold the ILogger reference for the life of the assertions; the concrete
  //instance is reference counted like any other TInterfacedObject.
  TTestLogger = class(TInterfacedObject, ILogger)
  private
    FMessages : TStringList;
  protected
    procedure Debug(const data: string);
    procedure Error(const data: string);
    procedure Information(const data: string; const important : boolean = false);
    procedure Success(const data: string; const important : boolean = false);
    procedure Verbose(const data: string; const important : boolean = false);
    procedure Warning(const data: string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);

    procedure Capture(const level : string; const data : string);
  public
    constructor Create;
    destructor Destroy; override;

    //Every message logged, one per line, as '<level>: <text>'.
    function Messages : TStrings;
    //True when any message at this level contains text (case insensitive).
    function Logged(const level : string; const text : string) : boolean;
  end;

implementation

uses
  System.SysUtils;

{ TTestLogger }

constructor TTestLogger.Create;
begin
  inherited Create;
  FMessages := TStringList.Create;
end;

destructor TTestLogger.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TTestLogger.Capture(const level, data: string);
begin
  FMessages.Add(level + ': ' + data);
end;

function TTestLogger.Messages: TStrings;
begin
  result := FMessages;
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

procedure TTestLogger.Clear;
begin
  FMessages.Clear;
end;

procedure TTestLogger.Debug(const data: string);
begin
  Capture('debug', data);
end;

procedure TTestLogger.Error(const data: string);
begin
  Capture('error', data);
end;

function TTestLogger.GetVerbosity: TVerbosity;
begin
  result := TVerbosity.Normal; //just to shut the compiler up.
end;

procedure TTestLogger.Information(const data: string; const important : boolean);
begin
  Capture('information', data);
end;

procedure TTestLogger.NewLine;
begin
///
end;

procedure TTestLogger.SetVerbosity(const value: TVerbosity);
begin

end;

procedure TTestLogger.Success(const data: string;  const important: boolean);
begin
  Capture('success', data);
end;

procedure TTestLogger.Verbose(const data: string; const important : boolean);
begin
  Capture('verbose', data);
end;

procedure TTestLogger.Warning(const data: string; const important : boolean);
begin
  Capture('warning', data);
end;

end.
