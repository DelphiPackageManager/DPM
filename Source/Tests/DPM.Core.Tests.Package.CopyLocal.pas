unit DPM.Core.Tests.Package.CopyLocal;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCopyLocalRuntimeTests = class
  public
    //StripLibSuffix - removes only a trailing run of digits.
    [Test]
    [TestCase('numeric suffix stripped', 'FooPkg290,FooPkg')]
    [TestCase('no suffix unchanged',     'FooPkg,FooPkg')]
    [TestCase('embedded digits kept',    'Foo123Bar,Foo123Bar')]
    [TestCase('dotted name',             'VSoft.Foo290,VSoft.Foo')]
    procedure StripLibSuffix_Strips_Trailing_Digits(const input : string; const expected : string);

    [Test]
    procedure StripLibSuffix_AllDigits_ReturnsEmpty;

    //PackageRuntimeLinked - exact match after stripping the numeric lib suffix from both sides.
    [Test]
    procedure RuntimeLinked_NumericSuffix_Matches;
    [Test]
    procedure RuntimeLinked_CaseInsensitive_Matches;
    [Test]
    procedure RuntimeLinked_NoTokenMatch_ReturnsFalse;
    [Test]
    procedure RuntimeLinked_EmptyRuntimePackages_ReturnsFalse;
    [Test]
    procedure RuntimeLinked_NonBplBinary_Ignored;

    //PackageRuntimeLinked - prefix fallback for non-numeric/custom lib suffixes.
    [Test]
    procedure RuntimeLinked_NonNumericSuffix_PrefixMatches;
    [Test]
    procedure RuntimeLinked_PrefixMatch_MinLengthBoundary;
    [Test]
    procedure RuntimeLinked_ShortToken_DoesNotPrefixMatch;
  end;

implementation

uses
  DPM.Core.Package.CopyLocal;

{ TCopyLocalRuntimeTests }

procedure TCopyLocalRuntimeTests.StripLibSuffix_Strips_Trailing_Digits(const input : string; const expected : string);
begin
  Assert.AreEqual(expected, StripLibSuffix(input));
end;

procedure TCopyLocalRuntimeTests.StripLibSuffix_AllDigits_ReturnsEmpty;
begin
  Assert.AreEqual('', StripLibSuffix('290'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NumericSuffix_Matches;
begin
  //the dcp token 'FooPkg' matches the bpl after the numeric suffix is stripped.
  Assert.IsTrue(PackageRuntimeLinked(['bpl/Win32/FooPkg290.bpl'], 'rtl;vcl;FooPkg'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_CaseInsensitive_Matches;
begin
  Assert.IsTrue(PackageRuntimeLinked(['bpl/Win32/VSoft.Foo290.bpl'], 'vsoft.foo'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NoTokenMatch_ReturnsFalse;
begin
  Assert.IsFalse(PackageRuntimeLinked(['bpl/Win32/FooPkg290.bpl'], 'rtl;vcl'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_EmptyRuntimePackages_ReturnsFalse;
begin
  Assert.IsFalse(PackageRuntimeLinked(['bpl/Win32/FooPkg290.bpl'], ''));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NonBplBinary_Ignored;
begin
  //only .bpl files participate - a dll with a matching name must not count.
  Assert.IsFalse(PackageRuntimeLinked(['lib/Win32/FooPkg.dll'], 'FooPkg'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NonNumericSuffix_PrefixMatches;
begin
  //custom (non-numeric) lib suffix: striplibsuffix can't normalise '_D12', so the prefix fallback
  //matches because the dcp token 'VSoft.Foo' starts the bpl name 'vsoft.foo_d12'.
  Assert.IsTrue(PackageRuntimeLinked(['bpl/Win32/VSoft.Foo_D12.bpl'], 'VSoft.Foo'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_PrefixMatch_MinLengthBoundary;
begin
  //a 4-char token is exactly at the minimum prefix length, so it matches.
  Assert.IsTrue(PackageRuntimeLinked(['bpl/Win32/Abcd_Custom.bpl'], 'Abcd'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_ShortToken_DoesNotPrefixMatch;
begin
  //'vcl' is below the 4-char minimum, so it must not prefix-match an unrelated 'vclXYZ' bpl.
  Assert.IsFalse(PackageRuntimeLinked(['bpl/Win32/vclXYZ.bpl'], 'vcl'));
end;

initialization
  TDUnitX.RegisterTestFixture(TCopyLocalRuntimeTests);

end.
