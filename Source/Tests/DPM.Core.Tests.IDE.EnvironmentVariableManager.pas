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
    //"dpm pack" refuses these names, but nothing forces a package to have been made by "dpm pack" - the
    //IDE has to refuse them too.
    procedure ReservedVariable_IsNotSet_AndWarns;
    procedure ReservedVariable_IsCaseInsensitive;
    procedure ReservedVariable_Remove_LeavesItAlone;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
  DPM.IDE.PathManager,
  DPM.IDE.EnvironmentVariableManager,
  TestLogger;

const
  cFoo = 'DPM_TEST_FOO';
  cBar = 'DPM_TEST_BAR';
  cBaz = 'DPM_TEST_BAZ';
  cTestDir = 'C:\__dpm_env_test_dir__';
  cProjP = 'c:\group\projp.dproj';
  cProjQ = 'c:\group\projq.dproj';

{ helpers }

function NewManager(out logger : TTestLogger) : IDPMIDEEnvironmentVariableManager;
begin
  logger := TTestLogger.Create;
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
  logger : TTestLogger;
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
  logger : TTestLogger;
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
  logger : TTestLogger;
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
  logger : TTestLogger;
begin
  mgr := NewManager(logger);
  mgr.SetVariable(cBar, 'x', cProjP, 'pkgA');
  mgr.SetVariable(cBar, 'y', cProjP, 'pkgB');
  Assert.AreEqual('y', GetEnv(cBar), 'last applied value wins');
  Assert.IsTrue(logger.Logged('warning', cBar), 'a conflict warning naming the variable is logged');
end;

procedure TEnvironmentVariableManagerTests.ConflictingValues_RemoveLast_RevertsToSurvivor;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TTestLogger;
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
  logger : TTestLogger;
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
  logger : TTestLogger;
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
  logger : TTestLogger;
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

procedure TEnvironmentVariableManagerTests.ReservedVariable_IsNotSet_AndWarns;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TTestLogger;
  comSpec : string;
begin
  comSpec := GetEnv('COMSPEC');
  try
    mgr := NewManager(logger);
    mgr.SetVariable('COMSPEC', 'C:\evil\cmd.exe', cProjP, 'Evil.Package');
    Assert.AreEqual(comSpec, GetEnv('COMSPEC'), false, 'a reserved variable must not be set');
    Assert.IsTrue(logger.HasWarning(TWarningCode.W9107_EnvironmentVariableNotAllowed), logger.Messages.Text);
    Assert.AreEqual(1, logger.WarningCount, logger.Messages.Text);
    Assert.IsTrue(logger.Logged('warning', 'Evil.Package'), 'the warning names the package : ' + logger.Messages.Text);
    Assert.IsTrue(logger.Logged('warning', '[COMSPEC]'), 'the warning names the variable : ' + logger.Messages.Text);
  finally
    WinApi.Windows.SetEnvironmentVariable('COMSPEC', PChar(comSpec));
  end;
end;

procedure TEnvironmentVariableManagerTests.ReservedVariable_IsCaseInsensitive;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TTestLogger;
  pathExt : string;
begin
  pathExt := GetEnv('PATHEXT');
  try
    mgr := NewManager(logger);
    mgr.SetVariable('PathExt', '.CMD;.BAT;.EXE', cProjP, 'Evil.Package');
    Assert.AreEqual(pathExt, GetEnv('PATHEXT'), false, 'PathExt is PATHEXT');
    Assert.IsTrue(logger.HasWarning(TWarningCode.W9107_EnvironmentVariableNotAllowed), logger.Messages.Text);
  finally
    WinApi.Windows.SetEnvironmentVariable('PATHEXT', PChar(pathExt));
  end;
end;

procedure TEnvironmentVariableManagerTests.ReservedVariable_Remove_LeavesItAlone;
var
  mgr : IDPMIDEEnvironmentVariableManager;
  logger : TTestLogger;
  comSpec : string;
begin
  comSpec := GetEnv('COMSPEC');
  try
    mgr := NewManager(logger);
    mgr.SetVariable('COMSPEC', 'C:\evil\cmd.exe', cProjP, 'Evil.Package');
    //the installer context still releases what it asked for when the project closes.
    mgr.RemoveVariable('COMSPEC', 'C:\evil\cmd.exe', cProjP, 'Evil.Package');
    Assert.AreEqual(comSpec, GetEnv('COMSPEC'), false, 'was never ours to restore or clear');
  finally
    WinApi.Windows.SetEnvironmentVariable('COMSPEC', PChar(comSpec));
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TEnvironmentVariableManagerTests);

end.
