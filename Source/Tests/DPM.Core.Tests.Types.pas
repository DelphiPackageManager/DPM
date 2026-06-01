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
    [TestCase('XE2', 'XE2')]
    [TestCase('XE3', 'XE3')]
    [TestCase('XE4', 'XE4')]
    [TestCase('XE5', 'XE5')]
    [TestCase('XE6', 'XE6')]
    [TestCase('XE7', 'XE7')]
    [TestCase('XE8', 'XE8')]
    [TestCase('delphiXE8', 'XE8')]
    [TestCase('10.0', '10.0')]
    [TestCase('10.1', '10.1')]
    [TestCase('10.2', '10.2')]
    [TestCase('10.3', '10.3')]
    [TestCase('10.4', '10.4')]
    [TestCase('delphi10.4', '10.4')]
    procedure TestStringToCompiler(const value :string);

    [Test]
    [TestCase('11', '11,Delphi11_0')]
    [TestCase('12', '12,Delphi12_0')]
    [TestCase('13', '13,Delphi13_0')]
    [TestCase('10', '10,Delphi10_0')]
    [TestCase('10', '10,Delphi10_0')]
    [TestCase('delphi10', 'delphi10,Delphi10_0')]
    [TestCase('11.0', '11.0,Delphi11_0')]
    [TestCase('DELPHI12', 'DELPHI12,Delphi12_0')]
    [TestCase('12.3', '12.3,Delphi12_0')]
    [TestCase('11.2', '11.2,Delphi11_0')]
    [TestCase('13.1', '13.1,Delphi13_0')]
    procedure TestStringToCompilerMajorOnly(const value : string; const expected : string);
  end;

implementation

uses
  System.TypInfo;

{ TCoreTypesTests }

procedure TCoreTypesTests.TestStringToCompiler(const value :string);
var
  compilerVersion :TCompilerVersion;
begin
  //Round-trip via the short (user-facing) form. CompilerToString returns the prefixed
  //internal form ('delphixe2', 'delphi10.0') used for on-disk paths and is intentionally
  //asymmetric with the input; CompilerNoPrefix is the public-facing inverse.
  compilerVersion := StringToCompilerVersion(value);
  Assert.AreEqual(value, CompilerNoPrefix(compilerVersion));
end;

procedure TCoreTypesTests.TestStringToCompilerMajorOnly(const value : string; const expected : string);
var
  compilerVersion : TCompilerVersion;
  expectedVersion : TCompilerVersion;
begin
  //Lenient parsing: bare majors (11/12/13/10) and 11+ point/update forms (12.3 = 12.0 Update 3)
  //collapse to the major's _0 enum. CompilerNoPrefix can't round-trip these (it returns the
  //point form), so assert the resulting enum directly.
  expectedVersion := TCompilerVersion(GetEnumValue(TypeInfo(TCompilerVersion), expected));
  compilerVersion := StringToCompilerVersion(value);
  Assert.AreEqual(Ord(expectedVersion), Ord(compilerVersion));
end;

initialization
  TDUnitX.RegisterTestFixture(TCoreTypesTests);

end.

