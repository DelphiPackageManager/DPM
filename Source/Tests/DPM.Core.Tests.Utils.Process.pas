unit DPM.Core.Tests.Utils.Process;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TProcessTests = class
  public
    //A compile runs as cmd.exe -> cmd -> msbuild. Cancelling it must stop the whole tree, or msbuild keeps building
    //and keeps its files open.
    [Test]
    procedure Cancel_Stops_The_Whole_Process_Tree;
    //loggers are not thread safe - every line is handed over on the thread that called Run.
    [Test]
    procedure Run_Delivers_Lines_On_The_Calling_Thread;
    //a compile's output is shown while it compiles, not once it has finished.
    [Test]
    procedure Run_Delivers_A_Line_While_The_Process_Runs;
    //stdout and stderr are read separately - an unfinished line on one is never joined to a line from the other.
    [Test]
    procedure Run_Keeps_Stdout_And_Stderr_Lines_Apart;
    //the last line of the output is delivered even though the process never ended it with a line end.
    [Test]
    procedure Run_Delivers_A_Last_Line_Without_A_Line_End;
    //a compile run on the IDE's main thread keeps the IDE responsive.
    [Test]
    procedure Run_Pumps_The_Host_On_The_Main_Thread;
    //a program that can't be started has no exit code - Run says why instead.
    [Test]
    procedure Run_Of_A_Program_That_Can_Not_Start_Returns_False;
    [Test]
    procedure Run_Stops_A_Process_That_Times_Out;
    //the process is not left running when the caller's line handler fails.
    [Test]
    procedure Run_Stops_The_Process_When_OnLine_Raises;
    //a wait on the IDE's main thread keeps it responsive - repaints, and its Cancel button working.
    [Test]
    procedure WaitForAnyHandle_PumpsTheHostOnTheMainThread;
    //only the main thread runs the host's message loop.
    [Test]
    procedure WaitForAnyHandle_DoesNotPumpOnAnotherThread;
  end;

implementation

uses
  WinApi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.IOUtils,
  System.Diagnostics,
  VSoft.CancellationToken,
  DPM.Core.Utils.Process,
  VSoft.Process;

type
  ETestLineError = class(Exception);

var
  PumpCount : integer;

//what the IDE installs - here it just drains the queue.
procedure CountingPump;
var
  msg : TMsg;
begin
  Inc(PumpCount);
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
    ;
end;

//cmd.exe by its full path, running command. /s /c "..." - cmd strips just the outer quotes.
function NewCmd(const command : string) : IProcess;
begin
  result := TProcess.Create;
  result.StartInfo.Executable := GetEnvironmentVariable('COMSPEC');
  result.CommandLine.Add('/s /c "' + command + '"', TQuoted.qtNone);
end;

procedure TProcessTests.Cancel_Stops_The_Whole_Process_Tree;
var
  logFile : string;
  cancellation : ICancellationTokenSource;
  process : IProcess;
  stopwatch : TStopwatch;
  errorMessage : string;
begin
  logFile := TPath.GetTempFileName;
  cancellation := TCancellationTokenSourceFactory.Create;
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000);
      cancellation.Cancel;
    end).Start;

  //the same shape as a compile - ping stands in for msbuild, a grandchild that holds the file open for 30 seconds
  //unless it is stopped.
  process := NewCmd('cmd /c ping -n 30 127.0.0.1 > "' + logFile + '"');
  stopwatch := TStopwatch.StartNew;
  Assert.IsTrue(TProcessRunner.Run(process, cancellation.Token, nil, nil, errorMessage), errorMessage);
  stopwatch.Stop;

  Assert.IsTrue(stopwatch.ElapsedMilliseconds < 15000, 'the process tree should have been stopped, it ran for ' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms');
  //nothing still running holds it.
  TFile.Delete(logFile);
  Assert.IsFalse(TFile.Exists(logFile));
end;

procedure TProcessTests.Run_Delivers_Lines_On_The_Calling_Thread;
var
  callerThread : Cardinal;
  otherThread : boolean;
  lines : TStringList;
  errorMessage : string;
begin
  callerThread := GetCurrentThreadId;
  otherThread := false;
  lines := TStringList.Create;
  try
    Assert.IsTrue(TProcessRunner.Run(NewCmd('echo one& echo two'), TCancellationTokenSourceFactory.Create.Token,
      procedure(const line : string)
      begin
        if GetCurrentThreadId <> callerThread then
          otherThread := true;
        lines.Add(line);
      end, TEncoding.Default, errorMessage), errorMessage);
    Assert.IsFalse(otherThread, 'a line was handed over on another thread');
    Assert.AreEqual(2, lines.Count, lines.Text);
    Assert.AreEqual('one', lines[0], false);
    Assert.AreEqual('two', lines[1], false);
  finally
    lines.Free;
  end;
end;

procedure TProcessTests.Run_Delivers_A_Line_While_The_Process_Runs;
var
  process : IProcess;
  runningAtFirstLine : boolean;
  lineCount : integer;
  errorMessage : string;
begin
  //the first line, then two seconds before the process ends.
  process := NewCmd('echo first& ping -n 3 127.0.0.1 >nul& echo last');
  runningAtFirstLine := false;
  lineCount := 0;
  Assert.IsTrue(TProcessRunner.Run(process, TCancellationTokenSourceFactory.Create.Token,
    procedure(const line : string)
    begin
      Inc(lineCount);
      if lineCount = 1 then
        runningAtFirstLine := process.Status = TProcessStatus.Running;
    end, TEncoding.Default, errorMessage), errorMessage);
  Assert.AreEqual(2, lineCount);
  Assert.IsTrue(runningAtFirstLine, 'the first line was only handed over once the process had finished');
end;

procedure TProcessTests.Run_Keeps_Stdout_And_Stderr_Lines_Apart;
var
  lines : TStringList;
  errorMessage : string;
begin
  lines := TStringList.Create;
  try
    //'partial' has no line end yet when 'err' is written to stderr - stdout's line is only finished by 'done'.
    Assert.IsTrue(TProcessRunner.Run(NewCmd('<nul set /p =partial& >&2 echo err& echo done'), TCancellationTokenSourceFactory.Create.Token,
      procedure(const line : string)
      begin
        lines.Add(Trim(line));
      end, TEncoding.Default, errorMessage), errorMessage);
    Assert.IsTrue(lines.IndexOf('partialdone') >= 0, lines.Text);
    Assert.IsTrue(lines.IndexOf('err') >= 0, lines.Text);
  finally
    lines.Free;
  end;
end;

procedure TProcessTests.Run_Delivers_A_Last_Line_Without_A_Line_End;
var
  lines : TStringList;
  errorMessage : string;
begin
  lines := TStringList.Create;
  try
    Assert.IsTrue(TProcessRunner.Run(NewCmd('echo first& <nul set /p =tail'), TCancellationTokenSourceFactory.Create.Token,
      procedure(const line : string)
      begin
        lines.Add(Trim(line));
      end, TEncoding.Default, errorMessage), errorMessage);
    Assert.IsTrue(lines.IndexOf('first') >= 0, lines.Text);
    Assert.IsTrue(lines.IndexOf('tail') >= 0, lines.Text);
  finally
    lines.Free;
  end;
end;

procedure TProcessTests.Run_Pumps_The_Host_On_The_Main_Thread;
var
  errorMessage : string;
begin
  PumpCount := 0;
  ProcessMessagePump := CountingPump;
  try
    //a message arrives while the process runs.
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(300);
        PostThreadMessage(MainThreadID, WM_NULL, 0, 0);
      end).Start;
    Assert.IsTrue(TProcessRunner.Run(NewCmd('ping -n 3 127.0.0.1 >nul'), TCancellationTokenSourceFactory.Create.Token,
      nil, nil, errorMessage), errorMessage);
  finally
    ProcessMessagePump := nil;
  end;
  Assert.IsTrue(PumpCount > 0, 'the host was not pumped while the process ran');
end;

procedure TProcessTests.Run_Of_A_Program_That_Can_Not_Start_Returns_False;
var
  process : IProcess;
  errorMessage : string;
begin
  process := TProcess.Create;
  process.StartInfo.Executable := TPath.Combine(TPath.GetTempPath, 'dpm-no-such-program-' + IntToStr(GetCurrentProcessId) + '.exe');
  Assert.IsFalse(TProcessRunner.Run(process, TCancellationTokenSourceFactory.Create.Token, nil, nil, errorMessage));
  Assert.IsTrue(errorMessage <> '', 'nothing says why it did not start');
end;

procedure TProcessTests.Run_Stops_A_Process_That_Times_Out;
var
  process : IProcess;
  stopwatch : TStopwatch;
  errorMessage : string;
begin
  process := NewCmd('ping -n 30 127.0.0.1 >nul');
  process.StartInfo.TimeoutInMSec := 1000;
  stopwatch := TStopwatch.StartNew;
  Assert.IsTrue(TProcessRunner.Run(process, TCancellationTokenSourceFactory.Create.Token, nil, nil, errorMessage), errorMessage);
  stopwatch.Stop;
  Assert.IsTrue(process.TimedOut, 'TimedOut');
  Assert.IsTrue(process.Status = TProcessStatus.TimedOut, 'Status');
  Assert.IsTrue(stopwatch.ElapsedMilliseconds < 15000, 'it ran for ' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms');
end;

procedure TProcessTests.Run_Stops_The_Process_When_OnLine_Raises;
var
  process : IProcess;
  stopwatch : TStopwatch;
begin
  process := NewCmd('echo first& ping -n 30 127.0.0.1 >nul');
  stopwatch := TStopwatch.StartNew;
  Assert.WillRaise(
    procedure
    var
      errorMessage : string;
    begin
      TProcessRunner.Run(process, TCancellationTokenSourceFactory.Create.Token,
        procedure(const line : string)
        begin
          raise ETestLineError.Create('the line handler failed');
        end, TEncoding.Default, errorMessage);
    end, ETestLineError);
  stopwatch.Stop;
  Assert.IsTrue(process.Status <> TProcessStatus.Running, 'the process was left running');
  Assert.IsTrue(stopwatch.ElapsedMilliseconds < 15000, 'it ran for ' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms');
end;

procedure TProcessTests.WaitForAnyHandle_PumpsTheHostOnTheMainThread;
var
  signal : TEvent;
  result_ : Cardinal;
begin
  signal := TEvent.Create(nil, true, false, '');
  try
    PumpCount := 0;
    ProcessMessagePump := CountingPump;
    try
      //a message arrives while it waits, then the handle is signalled.
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(100);
          PostThreadMessage(MainThreadID, WM_NULL, 0, 0);
          Sleep(100);
          signal.SetEvent;
        end).Start;
      result_ := WaitForAnyHandle([signal.Handle], 10000);
    finally
      ProcessMessagePump := nil;
    end;
    Assert.AreEqual(Cardinal(WAIT_OBJECT_0), result_);
    Assert.IsTrue(PumpCount > 0, 'the host was not pumped while waiting');
  finally
    signal.Free;
  end;
end;

procedure TProcessTests.WaitForAnyHandle_DoesNotPumpOnAnotherThread;
var
  signal : TEvent;
  worker : TThread;
  result_ : Cardinal;
begin
  signal := TEvent.Create(nil, true, false, '');
  try
    PumpCount := 0;
    result_ := WAIT_FAILED;
    ProcessMessagePump := CountingPump;
    try
      worker := TThread.CreateAnonymousThread(
        procedure
        var
          msg : TMsg;
        begin
          //give the worker a queue, and a message in it.
          PeekMessage(msg, 0, 0, 0, PM_NOREMOVE);
          PostThreadMessage(GetCurrentThreadId, WM_NULL, 0, 0);
          result_ := WaitForAnyHandle([signal.Handle], 10000);
        end);
      worker.FreeOnTerminate := false;
      try
        worker.Start;
        Sleep(200);
        signal.SetEvent;
        worker.WaitFor;
      finally
        worker.Free;
      end;
    finally
      ProcessMessagePump := nil;
    end;
    Assert.AreEqual(Cardinal(WAIT_OBJECT_0), result_);
    Assert.AreEqual(0, PumpCount, 'a worker thread must never run the host''s message loop');
  finally
    signal.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TProcessTests);

end.
