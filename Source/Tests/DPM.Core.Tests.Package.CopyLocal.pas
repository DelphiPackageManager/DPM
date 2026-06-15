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

    //ResolveRuntimeBplNames - maps a $(DCC_UsePackage) token to the bpl paired with its .dcp.
    [Test]
    procedure Resolve_TokenEqualsDcp_NumericSuffixBpl;
    [Test]
    procedure Resolve_ExactBplBaseEqualsDcp;
    [Test]
    procedure Resolve_CaseInsensitive;
    [Test]
    procedure Resolve_TokenCarriesLibSuffix_StripsToDcp;
    [Test]
    procedure Resolve_CustomSuffixBpl_PrefixPairs;
    [Test]
    procedure Resolve_DesignBpl_NoReferencedDcp_Excluded;
    [Test]
    procedure Resolve_FooVsFoobar_Disambiguates;
    [Test]
    procedure Resolve_TokenNotADcp_ReturnsEmpty;
    [Test]
    procedure Resolve_EmptyRuntimePackages_ReturnsEmpty;
    [Test]
    procedure Resolve_DuplicateBplNames_Deduped;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Package.CopyLocal;

{ helpers }

function ContainsName(const arr : TArray<string>; const name : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := Low(arr) to High(arr) do
    if SameText(arr[i], name) then
    begin
      result := true;
      exit;
    end;
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

procedure TCopyLocalRuntimeTests.Resolve_TokenEqualsDcp_NumericSuffixBpl;
var
  res : TArray<string>;
begin
  //token 'Sempare.TemplateR' = dcp base; its paired bpl carries the numeric lib suffix.
  res := ResolveRuntimeBplNames(['Sempare.TemplateR'], ['Sempare.TemplateR370.bpl'], 'rtl;vcl;Sempare.TemplateR');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'Sempare.TemplateR370.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_ExactBplBaseEqualsDcp;
var
  res : TArray<string>;
begin
  //bpl with no lib suffix - base equals the dcp base exactly.
  res := ResolveRuntimeBplNames(['FooPkg'], ['FooPkg.bpl'], 'FooPkg');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'FooPkg.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_CaseInsensitive;
var
  res : TArray<string>;
begin
  res := ResolveRuntimeBplNames(['VSoft.Foo'], ['VSoft.Foo290.bpl'], 'vsoft.foo');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'VSoft.Foo290.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_TokenCarriesLibSuffix_StripsToDcp;
var
  res : TArray<string>;
begin
  //some projects record the suffixed name in DCC_UsePackage; it still stems to the dcp base.
  res := ResolveRuntimeBplNames(['Sempare.TemplateR'], ['Sempare.TemplateR370.bpl'], 'Sempare.TemplateR370');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'Sempare.TemplateR370.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_CustomSuffixBpl_PrefixPairs;
var
  res : TArray<string>;
begin
  //a non-numeric/custom lib suffix - the bpl base starts with the dcp base.
  res := ResolveRuntimeBplNames(['VSoft.Foo'], ['VSoft.Foo_D12.bpl'], 'VSoft.Foo');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'VSoft.Foo_D12.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_DesignBpl_NoReferencedDcp_Excluded;
var
  res : TArray<string>;
begin
  //the design bpl 'dclFoo290' has no matching dcp in the (runtime) dcp set, so it is excluded -
  //this is the key new behaviour: design bpls are never auto-copied.
  res := ResolveRuntimeBplNames(['Foo'], ['Foo290.bpl', 'dclFoo290.bpl'], 'Foo');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'Foo290.bpl'));
  Assert.IsFalse(ContainsName(res, 'dclFoo290.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_FooVsFoobar_Disambiguates;
var
  res : TArray<string>;
begin
  //token 'Foo' must not drag in 'Foobar290.bpl' (which belongs to dcp 'Foobar').
  res := ResolveRuntimeBplNames(['Foo', 'Foobar'], ['Foo290.bpl', 'Foobar290.bpl'], 'Foo');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'Foo290.bpl'));
  Assert.IsFalse(ContainsName(res, 'Foobar290.bpl'));
end;

procedure TCopyLocalRuntimeTests.Resolve_TokenNotADcp_ReturnsEmpty;
var
  res : TArray<string>;
begin
  res := ResolveRuntimeBplNames(['Foo'], ['Foo290.bpl'], 'rtl;vcl');
  Assert.AreEqual(0, Length(res));
end;

procedure TCopyLocalRuntimeTests.Resolve_EmptyRuntimePackages_ReturnsEmpty;
var
  res : TArray<string>;
begin
  res := ResolveRuntimeBplNames(['Foo'], ['Foo290.bpl'], '');
  Assert.AreEqual(0, Length(res));
end;

procedure TCopyLocalRuntimeTests.Resolve_DuplicateBplNames_Deduped;
var
  res : TArray<string>;
begin
  res := ResolveRuntimeBplNames(['Foo'], ['Foo290.bpl', 'Foo290.bpl'], 'Foo');
  Assert.AreEqual(1, Length(res));
  Assert.IsTrue(ContainsName(res, 'Foo290.bpl'));
end;

initialization
  TDUnitX.RegisterTestFixture(TCopyLocalRuntimeTests);

end.
