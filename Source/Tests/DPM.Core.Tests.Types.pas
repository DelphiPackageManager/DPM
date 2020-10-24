unit DPM.Core.Tests.Types;

interface

uses
  DPM.Core.Types,
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TCoreTypesTests = class

  public
//    [SetupFixture]
//    procedure FixtureSetup;
//
//    [TearDownFixture]
//    procedure FixtureTearDown;
    [Test]
    [TestCase('XE2','XE2')]
    [TestCase('XE3','XE3')]
    [TestCase('XE4','XE4')]
    [TestCase('XE5','XE5')]
    [TestCase('XE6','XE6')]
    [TestCase('XE7','XE7')]
    [TestCase('XE8','XE8')]
    [TestCase('10.0','10.0')]
    [TestCase('10.1','10.1')]
    [TestCase('10.2','10.2')]
    [TestCase('10.3','10.3')]
    [TestCase('10.4','10.4')]
    procedure TestStringToCompiler(const value : string);
  end;

implementation

{ TCoreTypesTests }

procedure TCoreTypesTests.TestStringToCompiler(const value: string);
var
  compilerVersion : TCompilerVersion;
begin
  compilerVersion := StringToCompilerVersion(value);
  Assert.AreEqual(value, CompilerToString(compilerVersion));
end;

initialization
 TDUnitX.RegisterTestFixture(TCoreTypesTests);

end.
