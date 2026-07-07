unit DPM.Creator.Logger;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Controls.LogMemo
  ;

type
  TDSpecLogger = class(TInterfacedObject, ILogger)
  private
    FTarget : TLogMemo;
    FVerbosity : TVerbosity;
  public
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false);
    procedure Error(const data : string);
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    constructor Create(const memo : TLogMemo);
  end;

  // Thread-safe ILogger used while packing/signing on a worker thread (the
  // VSoft.Awaitable work function runs off the UI thread). Each line is
  // marshalled onto the UI thread via TThread.Queue before touching the memo,
  // so log output appears live without cross-thread VCL access.
  TDSpecQueuedLogger = class(TInterfacedObject, ILogger)
  private
    FTarget : TLogMemo;
    FVerbosity : TVerbosity;
    procedure QueueLine(const data : string; const messageType : TLogMessageType);
  public
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Information(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false);
    procedure Error(const data : string);
    procedure Success(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;

    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    constructor Create(const memo : TLogMemo);
    // Retarget output to a different memo. Operations (pack/sign/upload) are
    // mutually exclusive, so the single shared logger can be pointed at the
    // active page's memo before each operation starts.
    procedure SetTarget(const memo : TLogMemo);
  end;

implementation

uses
  System.SysUtils;

{ TDSpecLogger }

procedure TDSpecLogger.Clear;
begin
  FTarget.Clear;
end;

constructor TDSpecLogger.Create(const memo : TLogMemo);
begin
  inherited Create;
  FTarget := memo;
  FVerbosity := TVerbosity.Normal;
end;

procedure TDSpecLogger.Debug(const data: string);
begin
  if FVerbosity < TVerbosity.Debug then
    exit;
  FTarget.AddRow(data, mtDebug);
  FTarget.Flush;
end;

procedure TDSpecLogger.Error(const data: string);
begin
  FTarget.AddRow(data, mtError);
  FTarget.Flush;
end;

function TDSpecLogger.GetVerbosity: TVerbosity;
begin
  Result := FVerbosity;
end;

procedure TDSpecLogger.Information(const data: string; const important: boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
  if important then
    FTarget.AddRow(data, mtImportantInformation)
  else
    FTarget.AddRow(data, mtInformation);
  FTarget.Flush;
end;

procedure TDSpecLogger.NewLine;
begin
  FTarget.AddRow('', mtInformation);
  FTarget.Flush;
end;

procedure TDSpecLogger.SetVerbosity(const value: TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDSpecLogger.Success(const data: string; const important: boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
  if important then
    FTarget.AddRow(data, mtImportantSuccess)
  else
    FTarget.AddRow(data, mtSuccess);
  FTarget.Flush;
end;

procedure TDSpecLogger.Verbose(const data: string; const important: boolean);
begin
  if FVerbosity < TVerbosity.Detailed then
    exit;
  if important then
    FTarget.AddRow(data, mtImportantVerbose)
  else
    FTarget.AddRow(data, mtVerbose);
  FTarget.Flush;
end;

procedure TDSpecLogger.Warning(const data: string; const important: boolean);
begin
  if important then
    FTarget.AddRow(data, mtImportantWarning)
  else
    FTarget.AddRow(data, mtWarning);
  FTarget.Flush;
end;

{ TDSpecQueuedLogger }

constructor TDSpecQueuedLogger.Create(const memo : TLogMemo);
begin
  inherited Create;
  FTarget := memo;
  FVerbosity := TVerbosity.Normal;
end;

procedure TDSpecQueuedLogger.SetTarget(const memo : TLogMemo);
begin
  FTarget := memo;
end;

procedure TDSpecQueuedLogger.QueueLine(const data : string; const messageType : TLogMessageType);
var
  line : string;
  memo : TLogMemo;
  msgType : TLogMessageType;
begin
  line := data;
  memo := FTarget;
  msgType := messageType;
  TThread.Queue(nil,
    procedure
    begin
      memo.AddRow(line, msgType);
      memo.Flush;
    end);
end;

procedure TDSpecQueuedLogger.Clear;
var
  memo : TLogMemo;
begin
  memo := FTarget;
  TThread.Queue(nil,
    procedure
    begin
      memo.Clear;
    end);
end;

procedure TDSpecQueuedLogger.Debug(const data : string);
begin
  if FVerbosity < TVerbosity.Debug then
    exit;
  QueueLine(data, mtDebug);
end;

procedure TDSpecQueuedLogger.Error(const data : string);
begin
  QueueLine(data, mtError);
end;

function TDSpecQueuedLogger.GetVerbosity : TVerbosity;
begin
  Result := FVerbosity;
end;

procedure TDSpecQueuedLogger.Information(const data : string; const important : boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
  if important then
    QueueLine(data, mtImportantInformation)
  else
    QueueLine(data, mtInformation);
end;

procedure TDSpecQueuedLogger.NewLine;
begin
  QueueLine('', mtInformation);
end;

procedure TDSpecQueuedLogger.SetVerbosity(const value : TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDSpecQueuedLogger.Success(const data : string; const important : boolean);
begin
  if (FVerbosity < TVerbosity.Normal) and (not important) then
    exit;
  if important then
    QueueLine(data, mtImportantSuccess)
  else
    QueueLine(data, mtSuccess);
end;

procedure TDSpecQueuedLogger.Verbose(const data : string; const important : boolean);
begin
  if FVerbosity < TVerbosity.Detailed then
    exit;
  if important then
    QueueLine(data, mtImportantVerbose)
  else
    QueueLine(data, mtVerbose);
end;

procedure TDSpecQueuedLogger.Warning(const data : string; const important : boolean);
begin
  if important then
    QueueLine(data, mtImportantWarning)
  else
    QueueLine(data, mtWarning);
end;

end.
