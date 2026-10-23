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
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
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

initialization
  TDUnitX.RegisterTestFixture(TProcessTests);

end.
