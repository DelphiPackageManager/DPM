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
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Template,
  DPM.Core.Package.CopyLocal;

function MakeTemplate(const bpls : array of string) : ISpecTemplate;
var
  logger : ILogger;
  i : integer;
begin
  logger := TTestLogger.Create;
  result := TSpecTemplate.Create(logger);
  for i := Low(bpls) to High(bpls) do
    result.PrecompiledBinaries.Add(bpls[i]);
end;

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
var
  template : ISpecTemplate;
begin
  template := MakeTemplate(['bpl/Win32/FooPkg290.bpl']);
  //the dcp token 'FooPkg' matches the bpl after the numeric suffix is stripped.
  Assert.IsTrue(PackageRuntimeLinked(template, 'rtl;vcl;FooPkg'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_CaseInsensitive_Matches;
var
  template : ISpecTemplate;
begin
  template := MakeTemplate(['bpl/Win32/VSoft.Foo290.bpl']);
  Assert.IsTrue(PackageRuntimeLinked(template, 'vsoft.foo'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NoTokenMatch_ReturnsFalse;
var
  template : ISpecTemplate;
begin
  template := MakeTemplate(['bpl/Win32/FooPkg290.bpl']);
  Assert.IsFalse(PackageRuntimeLinked(template, 'rtl;vcl'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_EmptyRuntimePackages_ReturnsFalse;
var
  template : ISpecTemplate;
begin
  template := MakeTemplate(['bpl/Win32/FooPkg290.bpl']);
  Assert.IsFalse(PackageRuntimeLinked(template, ''));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NonBplBinary_Ignored;
var
  template : ISpecTemplate;
begin
  //only .bpl binaries participate - a dll with a matching name must not count.
  template := MakeTemplate(['lib/Win32/FooPkg.dll']);
  Assert.IsFalse(PackageRuntimeLinked(template, 'FooPkg'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_NonNumericSuffix_PrefixMatches;
var
  template : ISpecTemplate;
begin
  //custom (non-numeric) lib suffix: striplibsuffix can't normalise '_D12', so the prefix fallback
  //matches because the dcp token 'VSoft.Foo' starts the bpl name 'vsoft.foo_d12'.
  template := MakeTemplate(['bpl/Win32/VSoft.Foo_D12.bpl']);
  Assert.IsTrue(PackageRuntimeLinked(template, 'VSoft.Foo'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_PrefixMatch_MinLengthBoundary;
var
  template : ISpecTemplate;
begin
  //a 4-char token is exactly at the minimum prefix length, so it matches.
  template := MakeTemplate(['bpl/Win32/Abcd_Custom.bpl']);
  Assert.IsTrue(PackageRuntimeLinked(template, 'Abcd'));
end;

procedure TCopyLocalRuntimeTests.RuntimeLinked_ShortToken_DoesNotPrefixMatch;
var
  template : ISpecTemplate;
begin
  //'vcl' is below the 4-char minimum, so it must not prefix-match an unrelated 'vclXYZ' bpl.
  template := MakeTemplate(['bpl/Win32/vclXYZ.bpl']);
  Assert.IsFalse(PackageRuntimeLinked(template, 'vcl'));
end;

initialization
  TDUnitX.RegisterTestFixture(TCopyLocalRuntimeTests);

end.
