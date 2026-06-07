unit DPM.Core.Tests.Packaging.EnvironmentVariableValidator;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TEnvironmentVariableValidatorTests = class
  published
    [Test]
    procedure CustomVariable_IsAllowed;
    [Test]
    procedure Path_IsAllowed_AndDetectedAsPath;
    [Test]
    procedure EmptyName_IsNotAllowed;

    [Test]
    [TestCase('SystemRoot', 'SystemRoot')]
    [TestCase('PathExt', 'PATHEXT')]
    [TestCase('ComSpec', 'ComSpec')]
    [TestCase('Windir', 'windir')]
    [TestCase('Temp', 'TEMP')]
    [TestCase('UserProfile', 'USERPROFILE')]
    [TestCase('ProgramFiles', 'ProgramFiles')]
    [TestCase('Bds', 'BDS')]
    [TestCase('BdsCommonDir', 'BDSCOMMONDIR')]
    [TestCase('Delphi', 'DELPHI')]
    procedure ReservedVariable_IsNotAllowed(const name : string);
  end;

implementation

uses
  DPM.Core.Packaging.EnvironmentVariableValidator;

procedure TEnvironmentVariableValidatorTests.CustomVariable_IsAllowed;
var
  reason : string;
begin
  Assert.IsTrue(TEnvironmentVariableValidator.IsAllowed('SKIADIR', reason), reason);
  Assert.AreEqual('', reason);
end;

procedure TEnvironmentVariableValidatorTests.Path_IsAllowed_AndDetectedAsPath;
var
  reason : string;
begin
  //PATH is allowed (append-only) even though it is reserved everywhere else.
  Assert.IsTrue(TEnvironmentVariableValidator.IsAllowed('PATH', reason), reason);
  Assert.IsTrue(TEnvironmentVariableValidator.IsPathVariable('path'), 'lowercase path should be detected');
  Assert.IsTrue(TEnvironmentVariableValidator.IsPathVariable(' PATH '), 'trimmed path should be detected');
  Assert.IsFalse(TEnvironmentVariableValidator.IsPathVariable('PATHEXT'), 'PATHEXT is not PATH');
end;

procedure TEnvironmentVariableValidatorTests.EmptyName_IsNotAllowed;
var
  reason : string;
begin
  Assert.IsFalse(TEnvironmentVariableValidator.IsAllowed('   ', reason));
  Assert.IsTrue(reason <> '', 'a reason should be supplied');
end;

procedure TEnvironmentVariableValidatorTests.ReservedVariable_IsNotAllowed(const name : string);
var
  reason : string;
begin
  Assert.IsFalse(TEnvironmentVariableValidator.IsAllowed(name, reason), name + ' should be banned');
  Assert.IsTrue(reason <> '', 'a reason should be supplied for ' + name);
end;

initialization
  TDUnitX.RegisterTestFixture(TEnvironmentVariableValidatorTests);

end.
