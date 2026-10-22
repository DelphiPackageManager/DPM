unit DPM.Creator.Logger;

interface

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Controls.LogMemo
  ;

type
  TDSpecLogger = class(TBaseLogger)
  private
    FTarget : TLogMemo;
  protected
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); override;
    procedure DoClear; override;
    procedure DoNewLine; override;
  public
    constructor Create(const memo : TLogMemo);
  end;

  //The logger the pack, sign, test and upload operations log through. They take turns (only one runs
  //at a time), each with its own page's memo.
  IDSpecOpsLogger = interface(ILogger)
    ['{3C0E8B1F-6A52-4D1B-9F3E-2B7A4C9D8E61}']
    //Points the logger at the operation's memo, clears it and starts a new duplicate scope - call it
    //before the operation logs anything, so nothing lands on the previous operation's page.
    procedure BeginOperation(const memo : TLogMemo);
  end;

  // Thread-safe ILogger used while packing/signing on a worker thread (the
  // VSoft.Awaitable work function runs off the UI thread). Each line is
  // marshalled onto the UI thread via TThread.Queue before touching the memo,
  // so log output appears live without cross-thread VCL access.
  TDSpecQueuedLogger = class(TBaseLogger, IDSpecOpsLogger)
  private
    FTarget : TLogMemo;
    procedure QueueLine(const data : string; const messageType : TLogMessageType);
  protected
    procedure WriteMessage(const level : TLogLevel; const text : string; const important : boolean); override;
    procedure DoClear; override;
    procedure DoNewLine; override;
    procedure BeginOperation(const memo : TLogMemo);
  public
    constructor Create(const memo : TLogMemo);
  end;

implementation

uses
  System.SysUtils;

function MessageTypeOf(const level : TLogLevel; const important : boolean) : TLogMessageType;
begin
  case level of
    TLogLevel.Debug : result := mtDebug;
    TLogLevel.Verbose :
    begin
      if important then
        result := mtImportantVerbose
      else
        result := mtVerbose;
    end;
    TLogLevel.Success :
    begin
      if important then
        result := mtImportantSuccess
      else
        result := mtSuccess;
    end;
    TLogLevel.Warning :
    begin
      if important then
        result := mtImportantWarning
      else
        result := mtWarning;
    end;
    TLogLevel.Error : result := mtError;
  else
    if important then
      result := mtImportantInformation
    else
      result := mtInformation;
  end;
end;

{ TDSpecLogger }

constructor TDSpecLogger.Create(const memo : TLogMemo);
begin
  inherited Create;
  FTarget := memo;
end;

procedure TDSpecLogger.DoClear;
begin
  FTarget.Clear;
end;

procedure TDSpecLogger.DoNewLine;
begin
  FTarget.AddRow('', mtInformation);
  FTarget.Flush;
end;

procedure TDSpecLogger.WriteMessage(const level : TLogLevel; const text : string; const important : boolean);
begin
  FTarget.AddRow(text, MessageTypeOf(level, important));
  FTarget.Flush;
end;

{ TDSpecQueuedLogger }

constructor TDSpecQueuedLogger.Create(const memo : TLogMemo);
begin
  inherited Create;
  FTarget := memo;
end;

procedure TDSpecQueuedLogger.BeginOperation(const memo : TLogMemo);
begin
  FTarget := memo;
  Clear;
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

procedure TDSpecQueuedLogger.DoClear;
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

procedure TDSpecQueuedLogger.DoNewLine;
begin
  QueueLine('', mtInformation);
end;

procedure TDSpecQueuedLogger.WriteMessage(const level : TLogLevel; const text : string; const important : boolean);
begin
  QueueLine(text, MessageTypeOf(level, important));
end;

end.
