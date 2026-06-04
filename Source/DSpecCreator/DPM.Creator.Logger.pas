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

  // Thread-safe ILogger used while packing/signing on a worker thread (the
  // VSoft.Awaitable work function runs off the UI thread). Each line is
  // marshalled onto the UI thread via TThread.Queue before touching the memo,
  // so log output appears live without cross-thread VCL access.
  TDSpecQueuedLogger = class(TInterfacedObject, ILogger)
  private
    FStrList : TStrings;
    FVerbosity : TVerbosity;
    procedure QueueLine(const data : string);
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
    constructor Create(const sl : TStrings);
    // Retarget output to a different memo. Operations (pack/sign/upload) are
    // mutually exclusive, so the single shared logger can be pointed at the
    // active page's memo before each operation starts.
    procedure SetTarget(const sl : TStrings);
  end;

implementation

uses
  System.SysUtils;

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

{ TDSpecQueuedLogger }

constructor TDSpecQueuedLogger.Create(const sl : TStrings);
begin
  inherited Create;
  FStrList := sl;
end;

procedure TDSpecQueuedLogger.SetTarget(const sl : TStrings);
begin
  FStrList := sl;
end;

procedure TDSpecQueuedLogger.QueueLine(const data : string);
var
  line : string;
  strList : TStrings;
begin
  line := data;
  strList := FStrList;
  TThread.Queue(nil,
    procedure
    begin
      strList.Add(line);
    end);
end;

procedure TDSpecQueuedLogger.Clear;
var
  strList : TStrings;
begin
  strList := FStrList;
  TThread.Queue(nil,
    procedure
    begin
      strList.Clear;
    end);
end;

procedure TDSpecQueuedLogger.Debug(const data : string);
begin
  QueueLine(data);
end;

procedure TDSpecQueuedLogger.Error(const data : string);
begin
  QueueLine(data);
end;

function TDSpecQueuedLogger.GetVerbosity : TVerbosity;
begin
  Result := FVerbosity;
end;

procedure TDSpecQueuedLogger.Information(const data : string; const important : boolean);
begin
  QueueLine(data);
end;

procedure TDSpecQueuedLogger.NewLine;
begin
  QueueLine('');
end;

procedure TDSpecQueuedLogger.SetVerbosity(const value : TVerbosity);
begin
  FVerbosity := value;
end;

procedure TDSpecQueuedLogger.Success(const data : string; const important : boolean);
begin
  QueueLine(data);
end;

procedure TDSpecQueuedLogger.Verbose(const data : string; const important : boolean);
begin
  QueueLine(data);
end;

procedure TDSpecQueuedLogger.Warning(const data : string; const important : boolean);
begin
  QueueLine(data);
end;

end.
