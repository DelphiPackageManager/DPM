unit DPM.Core.Tests.SBOM.FormatParser;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSBOMFormatParserTests = class
  public
    [Test]
    procedure SingleTokenCycloneDX;
    [Test]
    procedure SingleTokenHTML;
    [Test]
    procedure MultiTokenCombination;
    [Test]
    procedure BothAliasExpandsToCycloneDXPlusSPDX;
    [Test]
    procedure AllAliasExpandsToEveryFormat;
    [Test]
    procedure EmptyInputReturnsDefaults;
    [Test]
    procedure WhitespaceTolerated;
    [Test]
    procedure UnknownTokenRaises;
    [Test]
    procedure CaseInsensitive;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Options.SBOM;

procedure TSBOMFormatParserTests.SingleTokenCycloneDX;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('cyclonedx');
  Assert.IsTrue(formats = [TSBOMFormat.CycloneDX],
                'expected exactly [CycloneDX]');
end;

procedure TSBOMFormatParserTests.SingleTokenHTML;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('html');
  Assert.IsTrue(formats = [TSBOMFormat.HTML],
                'expected exactly [HTML]');
end;

procedure TSBOMFormatParserTests.MultiTokenCombination;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('cyclonedx,html,markdown');
  Assert.IsTrue(TSBOMFormat.CycloneDX in formats);
  Assert.IsTrue(TSBOMFormat.HTML in formats);
  Assert.IsTrue(TSBOMFormat.Markdown in formats);
  Assert.IsFalse(TSBOMFormat.SPDX in formats);
end;

procedure TSBOMFormatParserTests.BothAliasExpandsToCycloneDXPlusSPDX;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('both');
  Assert.IsTrue(formats = [TSBOMFormat.CycloneDX, TSBOMFormat.SPDX],
                'expected exactly [CycloneDX, SPDX]');
end;

procedure TSBOMFormatParserTests.AllAliasExpandsToEveryFormat;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('all');
  Assert.IsTrue(TSBOMFormat.CycloneDX in formats);
  Assert.IsTrue(TSBOMFormat.SPDX in formats);
  Assert.IsTrue(TSBOMFormat.HTML in formats);
  Assert.IsTrue(TSBOMFormat.Markdown in formats);
end;

procedure TSBOMFormatParserTests.EmptyInputReturnsDefaults;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('');
  //Default is CycloneDX only - changing this is a breaking change for callers
  //that rely on -format omission. Use 'both' to also emit SPDX.
  Assert.IsTrue(formats = [TSBOMFormat.CycloneDX],
                'default format should be CycloneDX');
end;

procedure TSBOMFormatParserTests.WhitespaceTolerated;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('  cyclonedx , html  ');
  Assert.IsTrue(TSBOMFormat.CycloneDX in formats);
  Assert.IsTrue(TSBOMFormat.HTML in formats);
end;

procedure TSBOMFormatParserTests.UnknownTokenRaises;
begin
  Assert.WillRaise(
    procedure
    begin
      StringToSBOMFormats('xml');
    end,
    EArgumentException);
end;

procedure TSBOMFormatParserTests.CaseInsensitive;
var
  formats : TSBOMFormats;
begin
  formats := StringToSBOMFormats('CycloneDX,SPDX,HTML,Markdown');
  Assert.IsTrue(TSBOMFormat.CycloneDX in formats);
  Assert.IsTrue(TSBOMFormat.SPDX in formats);
  Assert.IsTrue(TSBOMFormat.HTML in formats);
  Assert.IsTrue(TSBOMFormat.Markdown in formats);
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMFormatParserTests);

end.
