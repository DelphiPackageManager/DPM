unit DPM.Core.Tests.Utils.Process;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TProcessTests = class
  public
    //A compile runs as cmd.exe -> cmd -> msbuild, with the output redirected to a log file. Cancelling it
    //used to stop only the first process, so msbuild kept building and kept the log open.
    [Test]
    procedure Cancel_Stops_The_Whole_Process_Tree;
    //a wait on the IDE's main thread keeps it responsive - repaints, and its Cancel button working.
    [Test]
    procedure WaitForAnyHandle_PumpsTheHostOnTheMainThread;
    //only the main thread runs the host's message loop.
    [Test]
    procedure WaitForAnyHandle_DoesNotPumpOnAnotherThread;
    //a program that can't be started has no exit code - it used to report the Windows error as one.
    [Test]
    procedure Execute2_OfAProgramThatCanNotStart_Raises;

    //the value of a variable, by a name in any case - not NAME=value.
    [Test]
    procedure EnvironmentBlock_GetValue_Returns_The_Value;
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
  DPM.Core.Utils.Process;

procedure TProcessTests.Cancel_Stops_The_Whole_Process_Tree;
var
  logFile : string;
  cancellation : ICancellationTokenSource;
  stopwatch : TStopwatch;
begin
  logFile := TPath.GetTempFileName;
  cancellation := TCancellationTokenSourceFactory.Create;
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000);
      cancellation.Cancel;
    end).Start;

  stopwatch := TStopwatch.StartNew;
  //the same shape as TMSBuildCompiler's command line - ping stands in for msbuild, a grandchild that
  //holds the redirected output open for 30 seconds unless it is stopped.
  TProcess.Execute2(cancellation.Token, 'cmd.exe', ' cmd /c ping -n 30 127.0.0.1 > ' + logFile, '');
  stopwatch.Stop;

  Assert.IsTrue(stopwatch.ElapsedMilliseconds < 15000, 'the process tree should have been stopped, it ran for ' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms');
  //nothing still running holds it.
  TFile.Delete(logFile);
  Assert.IsFalse(TFile.Exists(logFile));
end;

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

procedure TProcessTests.Execute2_OfAProgramThatCanNotStart_Raises;
var
  missing : string;
begin
  missing := TPath.Combine(TPath.GetTempPath, 'dpm-no-such-program-' + IntToStr(GetCurrentProcessId) + '.exe');
  Assert.WillRaise(
    procedure
    begin
      TProcess.Execute2(TCancellationTokenSourceFactory.Create.Token, missing, '', '');
    end, EOSError);
end;

procedure TProcessTests.EnvironmentBlock_GetValue_Returns_The_Value;
var
  env : IEnvironmentBlock;
begin
  env := TEnvironmentBlockFactory.Create(nil, false);
  env.AddOrSet('DPM_TEST_VALUE', 'C:\some=path');
  Assert.AreEqual('C:\some=path', env.GetValue('dpm_test_value'), false);
  Assert.AreEqual('', env.GetValue('DPM_NOT_SET'));
end;

initialization
  TDUnitX.RegisterTestFixture(TProcessTests);

end.
