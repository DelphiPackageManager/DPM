unit DPM.Core.Tests.SBOM.Sidecar;

interface

uses
  DUnitX.TestFramework,
  DPM.Core.Logging,
  TestLogger,
  DPM.Core.SBOM.ComponentRules,
  DPM.Core.SBOM.ProductInfo;

type
  //SaveSBOMSidecar - what the IDE's SBOM dialog writes into <project>.sbom.yaml, read back the way
  //the generator reads it.
  [TestFixture]
  TSBOMSidecarWriteTests = class
  private
    FTempDir : string;
    FProjectPath : string;
    FSidecar : string;
    FLogger : TTestLogger;
    FLoggerRef : ILogger;
    function MakeInfo(const version, supplier, supplierUrl, license, sbomCreator : string) : TSBOMProductInfo;
    function ReadBack : TSBOMProductInfo; overload;
    function ReadBack(const rules : ISBOMComponentRules) : TSBOMProductInfo; overload;
    procedure WriteSidecar(const content : string);
    function SidecarText : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure NoSidecar_AllFieldsSet_CreatesAReadableFile;
    [Test]
    procedure NewSidecar_IsCRLFWithoutABOM;
    [Test]
    procedure NoSidecar_AllFieldsBlank_CreatesNoFile;
    [Test]
    procedure EmptySidecar_IsFilled;
    [Test]
    procedure ChangingSupplier_KeepsComponentsAndOwnUnits;
    [Test]
    procedure ChangingOneKey_LeavesTheOtherProductValues;
    [Test]
    procedure ClearingAnExistingKey_ReadsBackBlank;
    [Test]
    procedure ClearingAnAbsentKey_DoesNotAddIt;
    [Test]
    procedure SameValues_DoNotRewriteTheFile;
    [Test]
    procedure ExistingKeyInAnotherCase_IsUpdatedInPlace;
    [Test]
    [TestCase('url with fragment', 'https://x.example/a#b')]
    [TestCase('colon space', 'Acme: Tools')]
    [TestCase('leading hash', '#1 Ltd')]
    [TestCase('number', '2.10')]
    [TestCase('boolean', 'true')]
    [TestCase('null word', 'null')]
    [TestCase('tilde', '~')]
    [TestCase('leading dash', '- Acme')]
    [TestCase('single quote', 'O''Brien Ltd')]
    [TestCase('spdx expression', 'MIT OR Apache-2.0')]
    procedure ValueNeedingQuotes_RoundTrips(const value : string);
    [Test]
    procedure UnparsableSidecar_FailsAndLeavesTheFile;
    [Test]
    procedure SequenceRoot_FailsAndLeavesTheFile;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Diagnostics;

{ TSBOMSidecarWriteTests }

procedure TSBOMSidecarWriteTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FTempDir := TPath.Combine(TPath.GetTempPath, 'dpm-sidecar-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(FTempDir);
  //the project file itself is never opened - only its name places the sidecar
  FProjectPath := TPath.Combine(FTempDir, 'MyApp.dproj');
  FSidecar := SBOMSidecarFileName(FProjectPath);
  FLogger := TTestLogger.Create;
  FLoggerRef := FLogger;
end;

procedure TSBOMSidecarWriteTests.TearDown;
begin
  FLogger := nil;
  FLoggerRef := nil;
  if DirectoryExists(FTempDir) then
    TDirectory.Delete(FTempDir, true);
end;

function TSBOMSidecarWriteTests.MakeInfo(const version, supplier, supplierUrl, license, sbomCreator : string) : TSBOMProductInfo;
begin
  result.Version := version;
  result.Supplier := supplier;
  result.SupplierUrl := supplierUrl;
  result.License := license;
  result.SbomCreator := sbomCreator;
end;

function TSBOMSidecarWriteTests.ReadBack : TSBOMProductInfo;
begin
  result := ReadBack(nil);
end;

function TSBOMSidecarWriteTests.ReadBack(const rules : ISBOMComponentRules) : TSBOMProductInfo;
var
  ok : boolean;
begin
  ok := LoadSBOMSidecar(FLoggerRef, FProjectPath, result, rules);
  Assert.IsTrue(ok, 'the written sidecar could not be read back : ' + SidecarText);
end;

procedure TSBOMSidecarWriteTests.WriteSidecar(const content : string);
var
  bytes : TBytes;
begin
  //no BOM, as a hand-written file usually is
  bytes := TEncoding.UTF8.GetBytes(content);
  TFile.WriteAllBytes(FSidecar, bytes);
end;

function TSBOMSidecarWriteTests.SidecarText : string;
begin
  if FileExists(FSidecar) then
    result := TFile.ReadAllText(FSidecar, TEncoding.UTF8)
  else
    result := '';
end;

procedure TSBOMSidecarWriteTests.NoSidecar_AllFieldsSet_CreatesAReadableFile;
var
  ok : boolean;
  info : TSBOMProductInfo;
begin
  ok := SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('1.2.3', 'Acme Ltd', 'https://acme.example', 'MIT', 'sbom@acme.example'));

  Assert.IsTrue(ok);
  Assert.IsTrue(FileExists(FSidecar), 'no sidecar was written');
  info := ReadBack;
  Assert.AreEqual('1.2.3', info.Version, false);
  Assert.AreEqual('Acme Ltd', info.Supplier, false);
  Assert.AreEqual('https://acme.example', info.SupplierUrl, false);
  Assert.AreEqual('MIT', info.License, false);
  Assert.AreEqual('sbom@acme.example', info.SbomCreator, false);
end;

procedure TSBOMSidecarWriteTests.NewSidecar_IsCRLFWithoutABOM;
var
  bytes : TBytes;
  text : string;
begin
  SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'Acme Ltd', '', 'MIT', ''));

  bytes := TFile.ReadAllBytes(FSidecar);
  Assert.IsTrue(Length(bytes) > 3);
  Assert.IsFalse((bytes[0] = $EF) and (bytes[1] = $BB) and (bytes[2] = $BF), 'the sidecar has a BOM');
  text := TEncoding.UTF8.GetString(bytes);
  Assert.IsTrue(Pos(#13#10, text) > 0, 'the sidecar has no CRLF line breaks');
  Assert.AreEqual(0, Pos(#10, StringReplace(text, #13#10, '', [rfReplaceAll])), 'the sidecar has a bare LF');
end;

procedure TSBOMSidecarWriteTests.NoSidecar_AllFieldsBlank_CreatesNoFile;
var
  ok : boolean;
begin
  ok := SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', '', '', '', ''));

  Assert.IsTrue(ok);
  Assert.IsFalse(FileExists(FSidecar), 'an empty sidecar was created');
end;

procedure TSBOMSidecarWriteTests.EmptySidecar_IsFilled;
var
  ok : boolean;
begin
  WriteSidecar('');

  ok := SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'Acme Ltd', '', '', ''));

  Assert.IsTrue(ok);
  Assert.AreEqual('Acme Ltd', ReadBack.Supplier, false);
end;

procedure TSBOMSidecarWriteTests.ChangingSupplier_KeepsComponentsAndOwnUnits;
var
  ok : boolean;
  rules : ISBOMComponentRules;
  info : TSBOMProductInfo;
begin
  WriteSidecar('supplier: Old Co'#13#10 +
               'license: MIT'#13#10 +
               'ownUnits:'#13#10 +
               '  - MainForm'#13#10 +
               'ownUnitPrefixes: App.'#13#10 +
               'components:'#13#10 +
               '  - name: zlib'#13#10 +
               '    version: 1.3.1'#13#10 +
               '    supplier: Jean-loup Gailly'#13#10 +
               '    units:'#13#10 +
               '      - ZLib'#13#10 +
               '      - ZLibConst'#13#10 +
               '    unitPrefixes:'#13#10 +
               '      - zl'#13#10 +
               '  - name: sqlite'#13#10 +
               '    notes: loaded at runtime'#13#10);

  ok := SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'New Co', '', 'MIT', ''));

  Assert.IsTrue(ok);
  rules := TSBOMComponentRules.Create;
  info := ReadBack(rules);
  Assert.AreEqual('New Co', info.Supplier, false);
  Assert.AreEqual('MIT', info.License, false);
  Assert.AreEqual(2, rules.Count, SidecarText);
  Assert.AreEqual('zlib', rules.Rules[0].Name, false);
  Assert.AreEqual('1.3.1', rules.Rules[0].Version, false);
  Assert.AreEqual('Jean-loup Gailly', rules.Rules[0].Supplier, false);
  Assert.AreEqual(2, Length(rules.Rules[0].Units));
  Assert.AreEqual('ZLib', rules.Rules[0].Units[0], false);
  Assert.AreEqual('ZLibConst', rules.Rules[0].Units[1], false);
  Assert.AreEqual(1, Length(rules.Rules[0].UnitPrefixes));
  Assert.AreEqual('zl', rules.Rules[0].UnitPrefixes[0], false);
  Assert.AreEqual('sqlite', rules.Rules[1].Name, false);
  Assert.AreEqual(1, Length(rules.GetOwnUnits));
  Assert.AreEqual('MainForm', rules.GetOwnUnits[0], false);
  Assert.AreEqual(1, Length(rules.GetOwnUnitPrefixes));
  Assert.AreEqual('App.', rules.GetOwnUnitPrefixes[0], false);
end;

procedure TSBOMSidecarWriteTests.ChangingOneKey_LeavesTheOtherProductValues;
var
  info : TSBOMProductInfo;
begin
  WriteSidecar('version: 4.0.1'#13#10 +
               'supplier: Acme Ltd'#13#10 +
               'supplierUrl: security@acme.example'#13#10 +
               'license: MIT'#13#10 +
               'sbomCreator: https://acme.example/sbom'#13#10);

  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath,
                                MakeInfo('4.0.1', 'Acme Ltd', 'security@acme.example', 'Apache-2.0', 'https://acme.example/sbom')));

  info := ReadBack;
  Assert.AreEqual('4.0.1', info.Version, false);
  Assert.AreEqual('Acme Ltd', info.Supplier, false);
  Assert.AreEqual('security@acme.example', info.SupplierUrl, false);
  Assert.AreEqual('Apache-2.0', info.License, false);
  Assert.AreEqual('https://acme.example/sbom', info.SbomCreator, false);
end;

procedure TSBOMSidecarWriteTests.ClearingAnExistingKey_ReadsBackBlank;
var
  info : TSBOMProductInfo;
begin
  WriteSidecar('supplier: Acme Ltd'#13#10 +
               'license: MIT'#13#10);

  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', '', '', 'MIT', '')));

  info := ReadBack;
  Assert.AreEqual('', info.Supplier, false, SidecarText);
  Assert.AreEqual('MIT', info.License, false);
end;

procedure TSBOMSidecarWriteTests.ClearingAnAbsentKey_DoesNotAddIt;
begin
  WriteSidecar('license: MIT'#13#10);

  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'Acme Ltd', '', 'MIT', '')));

  Assert.AreEqual(0, Pos('version', LowerCase(SidecarText)), SidecarText);
  Assert.AreEqual(0, Pos('sbomcreator', LowerCase(SidecarText)), SidecarText);
end;

procedure TSBOMSidecarWriteTests.SameValues_DoNotRewriteTheFile;
var
  original : string;
  stamp : TDateTime;
begin
  //a version YAML reads as a float, and comments that a rewrite would move
  original := '# product metadata for the SBOM'#13#10 +
              'version: 2.1'#13#10 +
              'supplier: Acme Ltd   # the legal entity'#13#10 +
              #13#10 +
              '# the licence we ship under'#13#10 +
              'license: MIT'#13#10;
  WriteSidecar(original);
  stamp := EncodeDateTime(2020, 1, 2, 3, 4, 5, 0);
  TFile.SetLastWriteTime(FSidecar, stamp);

  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('2.1', 'Acme Ltd', '', 'MIT', '')));

  Assert.AreEqual(original, SidecarText, false);
  Assert.IsTrue(SameDateTime(stamp, TFile.GetLastWriteTime(FSidecar)), 'the sidecar was rewritten');
end;

procedure TSBOMSidecarWriteTests.ExistingKeyInAnotherCase_IsUpdatedInPlace;
var
  text : string;
begin
  WriteSidecar('Supplier: Old Co'#13#10);

  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'New Co', '', '', '')));

  Assert.AreEqual('New Co', ReadBack.Supplier, false);
  text := SidecarText;
  Assert.IsTrue(Pos('Supplier:', text) > 0, 'the key lost its spelling : ' + text);
  Assert.AreEqual(0, Pos('supplier:', text), 'the key was duplicated : ' + text);
end;

procedure TSBOMSidecarWriteTests.ValueNeedingQuotes_RoundTrips(const value : string);
begin
  Assert.IsTrue(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', value, '', '', '')));

  Assert.AreEqual(value, ReadBack.Supplier, false, SidecarText);
end;

procedure TSBOMSidecarWriteTests.UnparsableSidecar_FailsAndLeavesTheFile;
var
  original : string;
begin
  original := 'supplier: [Acme'#13#10;
  WriteSidecar(original);

  Assert.IsFalse(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'New Co', '', '', '')));

  Assert.IsTrue(FLogger.HasError(TErrorCode.E8102_SBOMProductInfoUnreadable));
  Assert.AreEqual(original, SidecarText, false);
end;

procedure TSBOMSidecarWriteTests.SequenceRoot_FailsAndLeavesTheFile;
var
  original : string;
begin
  original := '- a'#13#10 +
              '- b'#13#10;
  WriteSidecar(original);

  Assert.IsFalse(SaveSBOMSidecar(FLoggerRef, FProjectPath, MakeInfo('', 'New Co', '', '', '')));

  Assert.IsTrue(FLogger.HasError(TErrorCode.E8101_SBOMProductInfoInvalid));
  Assert.AreEqual(original, SidecarText, false);
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMSidecarWriteTests);

end.
