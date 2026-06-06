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

    [Test]
    [TestCase('XE2', 'XE2,XE2')]
    [TestCase('10.0', '10.0,100')]
    [TestCase('10.1', '10.1,101')]
    [TestCase('10.2', '10.2,102')]
    [TestCase('10.3', '10.3,103')]
    [TestCase('10.4', '10.4,104')]
    [TestCase('11.0', '11.0,110')]
    [TestCase('12.0', '12.0,120')]
    [TestCase('13.0', '13.0,130')]
    procedure TestCompilerToShortVersion(const value : string; const expected : string);

    //Win64x ("Windows 64-bit (Modern)", 12.1+) - distinct platform, binary-compatible with Win64.
    [Test]
    procedure Win64x_Parses_To_Distinct_Platform;
    [Test]
    procedure Win64x_Round_Trips_Through_String;
    [Test]
    procedure Win64x_Is_64_Bit;
    [Test]
    procedure Win64x_Has_Display_String;
    [Test]
    [TestCase('Delphi12_0', 'Delphi12_0')]
    [TestCase('Delphi13_0', 'Delphi13_0')]
    procedure AllPlatforms_Includes_Win64x(const compiler : string);
    [Test]
    procedure Win64_Package_Satisfies_Win64x_Target;
    [Test]
    procedure Win64x_Package_Satisfies_Win64_Target;
    [Test]
    procedure ResolveCompatiblePlatform_Prefers_Direct_Match;
    [Test]
    procedure ResolveCompatiblePlatform_Falls_Back_To_Counterpart;
    [Test]
    procedure PlatformSatisfiedBy_False_When_No_Compatible_Platform;
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

procedure TCoreTypesTests.TestCompilerToShortVersion(const value : string; const expected : string);
var
  compilerVersion : TCompilerVersion;
begin
  //CompilerToShortVersion drops the prefix and the point: "10.4" -> "104", "11.0" -> "110",
  //"XE2" -> "XE2". Used by the $compilershortversion$ token for D100/D104/D110 folder schemes.
  compilerVersion := StringToCompilerVersion(value);
  Assert.AreEqual(expected, CompilerToShortVersion(compilerVersion));
end;

procedure TCoreTypesTests.Win64x_Parses_To_Distinct_Platform;
begin
  //Win64x must parse to its own enum value, not be collapsed to Win64 - the project's platform
  //identity has to survive so DPM writes the correct $(Platform)=='Win64x' search-path condition.
  Assert.AreEqual(Ord(TDPMPlatform.Win64x), Ord(StringToDPMPlatform('Win64x')));
  Assert.AreNotEqual(Ord(TDPMPlatform.Win64), Ord(StringToDPMPlatform('Win64x')));
end;

procedure TCoreTypesTests.Win64x_Round_Trips_Through_String;
begin
  Assert.AreEqual(Ord(TDPMPlatform.Win64x), Ord(StringToDPMPlatform(DPMPlatformToString(TDPMPlatform.Win64x))));
end;

procedure TCoreTypesTests.Win64x_Is_64_Bit;
begin
  Assert.AreEqual('64', DPMPlatformBitness(TDPMPlatform.Win64x));
end;

procedure TCoreTypesTests.Win64x_Has_Display_String;
begin
  //Must not raise (the case statement previously had no Win64x entry).
  Assert.AreEqual('Windows 64-bit (Modern)', DPMPlatformToDisplayString(TDPMPlatform.Win64x));
end;

procedure TCoreTypesTests.AllPlatforms_Includes_Win64x(const compiler : string);
var
  compilerVersion : TCompilerVersion;
begin
  compilerVersion := TCompilerVersion(GetEnumValue(TypeInfo(TCompilerVersion), compiler));
  Assert.IsTrue(TDPMPlatform.Win64x in AllPlatforms(compilerVersion));
end;

procedure TCoreTypesTests.Win64_Package_Satisfies_Win64x_Target;
begin
  //A Win64-only package can satisfy a Win64x project target (binaries are interchangeable).
  Assert.IsTrue(PlatformSatisfiedBy(TDPMPlatform.Win64x, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
end;

procedure TCoreTypesTests.Win64x_Package_Satisfies_Win64_Target;
begin
  Assert.IsTrue(PlatformSatisfiedBy(TDPMPlatform.Win64, [TDPMPlatform.Win32, TDPMPlatform.Win64x]));
end;

procedure TCoreTypesTests.ResolveCompatiblePlatform_Prefers_Direct_Match;
begin
  //When the wanted platform is directly available it must be returned as-is, not remapped.
  Assert.AreEqual(Ord(TDPMPlatform.Win64x),
    Ord(ResolveCompatiblePlatform(TDPMPlatform.Win64x, [TDPMPlatform.Win64, TDPMPlatform.Win64x])));
end;

procedure TCoreTypesTests.ResolveCompatiblePlatform_Falls_Back_To_Counterpart;
begin
  Assert.AreEqual(Ord(TDPMPlatform.Win64),
    Ord(ResolveCompatiblePlatform(TDPMPlatform.Win64x, [TDPMPlatform.Win32, TDPMPlatform.Win64])));
  Assert.AreEqual(Ord(TDPMPlatform.Win64x),
    Ord(ResolveCompatiblePlatform(TDPMPlatform.Win64, [TDPMPlatform.Win32, TDPMPlatform.Win64x])));
end;

procedure TCoreTypesTests.PlatformSatisfiedBy_False_When_No_Compatible_Platform;
begin
  //The compatibility rule is Win64<->Win64x only - it must not bridge unrelated platforms.
  Assert.IsFalse(PlatformSatisfiedBy(TDPMPlatform.Win64x, [TDPMPlatform.Win32, TDPMPlatform.Linux64]));
end;

initialization
  TDUnitX.RegisterTestFixture(TCoreTypesTests);

end.

