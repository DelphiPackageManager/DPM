unit DPM.Core.Tests.IDE.EnvironmentVariableManager;

// Headless tests for TDPMIDEEnvironmentVariableManager. The manager and path manager are pure
// (WinApi.Windows / Spring.Collections only - no ToolsAPI/VCL), so they can run against the test
// process's own environment. Focus: (project, package) reference counting so a variable survives
// while any loaded project still needs the declaring package, plus last-wins + conflict warning.

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TEnvironmentVariableManagerTests = class
  public
    [Teardown]
    procedure Teardown;
  published
    procedure SamePackageProject_RefCounts_NotClearedUntilLastPackageGone;
    procedure SameVar_TwoPackages_OneProject_SurvivesPartialUninstall;
    procedure SameVar_TwoProjects_SurvivesClosingOneProject;
    procedure ConflictingValues_LastWins_AndWarns;
    procedure ConflictingValues_RemoveLast_RevertsToSurvivor;
    procedure PreExistingValue_RestoredOnLastRelease;
    procedure NonExisting_ClearedOnLastRelease;
    procedure Path_TwoPackages_SameDir_RefCounted;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.IDE.PathManager,
  DPM.IDE.EnvironmentVariableManager;

const
  cFoo = 'DPM_TEST_FOO';
  cBar = 'DPM_TEST_BAR';
  cBaz = 'DPM_TEST_BAZ';
  cTestDir = 'C:\__dpm_env_test_dir__';
  cProjP = 'c:\group\projp.dproj';
  cProjQ = 'c:\group\projq.dproj';

type
  //Captures warnings so we can assert the conflict path fired.
  TCapturingLogger = class(TInterfacedObject, ILogger)
  public
    Warnings : string;
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : boolean = false);
    procedure Success(const data : string; const important : boolean = false);
    procedure Verbose(const data : string; const important : boolean = false);
    procedure Warning(const data : string; const important : boolean = false);
    procedure Clear;
    procedure NewLine;
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
  end;

procedure TCapturingLogger.Debug(const data : string); begin end;
procedure TCapturingLogger.Error(const data : string); begin end;
procedure TCapturingLogger.Information(const data : string; const important : boolean); begin end;
procedure TCapturingLogger.Success(const data : string; const important : boolean); begin end;
procedure TCapturingLogger.Verbose(const data : string; const important : boolean); begin end;
procedure TCapturingLogger.Warning(const data : string; const important : boolean); begin Warnings := Warnings + data + #13#10; end;
procedure TCapturingLogger.Clear; begin end;
procedure TCapturingLogger.NewLine; begin end;
function TCapturingLogger.GetVerbosity : TVerbosity; begin result := TVerbosity.Normal; end;
procedure TCapturingLogger.SetVerbosity(const value : TVerbosity); begin end;

{ helpers }

function NewManager(out logger : TCapturingLogger) : IDPMIDEEnvironmentVariableManager;
begin
  logger := TCapturingLogger.Create;
  //ILogger is reference counted - the manager holds it alive; the out param is a borrowed view.
  result := TDPMIDEEnvironmentVariableManager.Create(logger, TDPMIDEPathManager.Create);
end;

function GetEnv(const name : string) : string;
var
  len : DWORD;
begin
  result := '';
  len := WinApi.Windows.GetEnvironmentVariable(PChar(name), nil, 0);
  if len = 0 then
    exit;
  SetLength(result, len - 1);
  WinApi.Windows.GetEnvironmentVariable(PChar(name), PChar(result), len);
end;

{ TEnvironmentVariableManagerTests }

procedure TEnvironmentVariableManagerTests.Teardown;
begin
  //leave the test process env clean regardless of what a test did.
  WinApi.Windows.SetEnvironmentVariable(PChar(cFoo), nil);
  WinApi.Windows.SetEnvironmentVariable(PChar(cBar), nil);
  WinApi.Windows.SetEnvironmentVariable(PChar(cBaz), nil);
end;

procedure TEnvironmentVariableManagerTests.SamePackageProject_RefCounts_NotClearedUntilLastPackageGone;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable(cFoo, 'a', cProjP, 'pkgA');
  Assert.AreEqual('a', GetEnv(cFoo), 'set');
  //removing a package that never set it is a no-op
  mgr.RemoveVariable(cFoo, 'a', cProjP, 'pkgZ');
  Assert.AreEqual('a', GetEnv(cFoo), 'unrelated remove must not clear');
  mgr.RemoveVariable(cFoo, 'a', cProjP, 'pkgA');
  Assert.AreEqual('', GetEnv(cFoo), 'cleared after last referencer');
end;

procedure TEnvironmentVariableManagerTests.SameVar_TwoPackages_OneProject_SurvivesPartialUninstall;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  //THE REGRESSION: two packages in ONE project both set FOO. Uninstalling one must not clear it.
  mgr := NewManager(logger);
  mgr.SetVariable(cFoo, 'a', cProjP, 'pkgA');
  mgr.SetVariable(cFoo, 'a', cProjP, 'pkgB');
  mgr.RemoveVariable(cFoo, 'a', cProjP, 'pkgA');
  Assert.AreEqual('a', GetEnv(cFoo), 'pkgB still needs FOO');
  mgr.RemoveVariable(cFoo, 'a', cProjP, 'pkgB');
  Assert.AreEqual('', GetEnv(cFoo), 'cleared once both gone');
end;

procedure TEnvironmentVariableManagerTests.SameVar_TwoProjects_SurvivesClosingOneProject;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable(cFoo, 'a', cProjP, 'pkgA');
  mgr.SetVariable(cFoo, 'a', cProjQ, 'pkgA');
  mgr.RemoveVariable(cFoo, 'a', cProjP, 'pkgA');
  Assert.AreEqual('a', GetEnv(cFoo), 'projQ still loads pkgA');
  mgr.RemoveVariable(cFoo, 'a', cProjQ, 'pkgA');
  Assert.AreEqual('', GetEnv(cFoo), 'cleared once both projects closed');
end;

procedure TEnvironmentVariableManagerTests.ConflictingValues_LastWins_AndWarns;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable(cBar, 'x', cProjP, 'pkgA');
  mgr.SetVariable(cBar, 'y', cProjP, 'pkgB');
  Assert.AreEqual('y', GetEnv(cBar), 'last applied value wins');
  Assert.IsTrue(ContainsText(logger.Warnings, cBar), 'a conflict warning naming the variable is logged');
end;

procedure TEnvironmentVariableManagerTests.ConflictingValues_RemoveLast_RevertsToSurvivor;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable(cBar, 'x', cProjP, 'pkgA');
  mgr.SetVariable(cBar, 'y', cProjP, 'pkgB');
  mgr.RemoveVariable(cBar, 'y', cProjP, 'pkgB');
  Assert.AreEqual('x', GetEnv(cBar), 'surviving package value re-applied');
end;

procedure TEnvironmentVariableManagerTests.PreExistingValue_RestoredOnLastRelease;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  WinApi.Windows.SetEnvironmentVariable(PChar(cBaz), 'original');
  mgr := NewManager(logger);
  mgr.SetVariable(cBaz, 'override', cProjP, 'pkgA');
  Assert.AreEqual('override', GetEnv(cBaz), 'overridden');
  mgr.RemoveVariable(cBaz, 'override', cProjP, 'pkgA');
  Assert.AreEqual('original', GetEnv(cBaz), 'pre-existing value restored');
end;

procedure TEnvironmentVariableManagerTests.NonExisting_ClearedOnLastRelease;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  Assert.AreEqual('', GetEnv(cBaz), 'precondition: not set');
  mgr := NewManager(logger);
  mgr.SetVariable(cBaz, 'override', cProjP, 'pkgA');
  mgr.RemoveVariable(cBaz, 'override', cProjP, 'pkgA');
  Assert.AreEqual('', GetEnv(cBaz), 'cleared (did not exist before)');
end;

procedure TEnvironmentVariableManagerTests.Path_TwoPackages_SameDir_RefCounted;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TCapturingLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable('PATH', cTestDir, cProjP, 'pkgA');
  mgr.SetVariable('PATH', cTestDir, cProjP, 'pkgB');
  Assert.IsTrue(ContainsText(GetEnv('PATH'), cTestDir), 'dir on PATH');
  mgr.RemoveVariable('PATH', cTestDir, cProjP, 'pkgA');
  Assert.IsTrue(ContainsText(GetEnv('PATH'), cTestDir), 'still on PATH - pkgB needs it');
  mgr.RemoveVariable('PATH', cTestDir, cProjP, 'pkgB');
  Assert.IsFalse(ContainsText(GetEnv('PATH'), cTestDir), 'removed once both gone');
end;

initialization
  TDUnitX.RegisterTestFixture(TEnvironmentVariableManagerTests);

end.
