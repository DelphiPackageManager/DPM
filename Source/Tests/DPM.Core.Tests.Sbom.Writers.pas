unit DPM.Core.Tests.SBOM.Writers;

interface

uses
  DUnitX.TestFramework,
  JsonDataObjects,
  DPM.Core.SBOM.Types;

type
  [TestFixture]
  TSBOMWritersTests = class
  private
    function BuildSampleReport : TSBOMReport;
    function BuildReportWithLicense(const license : string) : TSBOMReport;
    function NewTempPath(const ext : string) : string;
    function ReadAllText(const fileName : string) : string;
    //writes the report with the CycloneDX writer and returns the parsed document - caller frees
    function WriteCycloneDXAndParse(const report : TSBOMReport) : TJsonObject;
    //round-trips a single licence string through the SPDX writer and returns the
    //licenseDeclared value it produced for the component carrying it.
    function SpdxLicenseDeclaredFor(const license : string) : string;
    //writes the report with the SPDX writer and returns the parsed document - caller frees
    function WriteSpdxAndParse(const report : TSBOMReport) : TJsonObject;
  public
    [Test]
    procedure CycloneDX_EmitsRequiredFields;
    [Test]
    procedure CycloneDX_EmitsHashesPurlAndDependencies;
    [Test]
    procedure CycloneDX_EmitsToolsAsComponentsObject;
    [Test]
    procedure CycloneDX_SpdxLicenseIdEmittedAsLicenseId;
    [Test]
    procedure CycloneDX_CompoundLicenseEmittedAsExpression;
    [Test]
    procedure CycloneDX_UnknownLicenseEmittedAsLicenseName;
    [Test]
    procedure SPDX_EmitsRequiredDocumentFields;
    [Test]
    procedure SPDX_EmitsDescribesRelationship;
    [Test]
    procedure SPDX_LooseLicenseBecomesNoAssertion;
    [Test]
    procedure CycloneDX_LicenseIdOutsideTheCommonSetIsStillRecognised;
    [Test]
    procedure SPDX_UnknownLicenseIdBecomesNoAssertion;
    [Test]
    procedure SPDX_SpdxLicenseIdIsDeclaredVerbatim;
    [Test]
    procedure SPDX_CompoundLicenseExpressionIsDeclared;
    [Test]
    procedure SPDX_PrimaryPackagePurposeReflectsComponentKind;
    [Test]
    procedure SPDX_ComponentKindIsAnnotated;
    [Test]
    procedure SPDX_DocumentAnnotatesPlatformCompilerAndMetaProperties;
    [Test]
    procedure SPDX_AuthorsBecomeOriginatorNotSupplier;
    [Test]
    procedure SPDX_SupplierIsUsedWhenPresent;
    [Test]
    procedure SPDX_RepositoryBecomesDownloadLocationWhenNoDownloadUrl;
    [Test]
    procedure SPDX_DownloadUrlWinsAndRepositoryGoesToSourceInfo;
    [Test]
    procedure SPDX_AnnotatorCarriesToolVersion;
    [Test]
    procedure SPDX_CreationInfoDeclaresLicenseListVersion;
    [Test]
    procedure SPDX_DocumentNamespaceIsUnderTheProjectDomain;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Utils.Spdx,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Writers;

function TSBOMWritersTests.NewTempPath(const ext : string) : string;
var
  g : TGUID;
  guidStr : string;
begin
  CreateGUID(g);
  guidStr := LowerCase(Copy(GUIDToString(g), 2, 36));
  result := TPath.Combine(TPath.GetTempPath, 'dpm-sbom-writer-' + guidStr + ext);
end;

function TSBOMWritersTests.ReadAllText(const fileName : string) : string;
var
  bytes : TBytes;
begin
  bytes := TFile.ReadAllBytes(fileName);
  result := TEncoding.UTF8.GetString(bytes);
end;

function TSBOMWritersTests.BuildSampleReport : TSBOMReport;
var
  report : TSBOMReport;
  comp : TSBOMComponent;
  runtime : TSBOMComponent;
begin
  report := TSBOMReport.Create;
  report.SerialNumber := 'urn:uuid:11111111-2222-3333-4444-555555555555';
  report.TimestampUtc := '2026-05-18T00:00:00Z';
  report.ToolName := 'dpm';
  report.ToolVersion := '1.0.0';
  report.ProjectName := 'SampleProject';
  report.ProjectVersion := '0.1.0';
  report.Platform := TDPMPlatform.Win64;
  report.CompilerVersion := TCompilerVersion.Delphi12_0;

  report.RootComponent.BomRef := 'project:SampleProject:win64';
  report.RootComponent.Id := 'SampleProject';
  report.RootComponent.Version := '0.1.0';

  comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'dpm:Spring.Base@2.0.2';
  comp.Id := 'Spring.Base';
  comp.Version := '2.0.2';
  comp.Description := 'Spring Base library';
  comp.License := 'Apache-2.0';
  comp.Copyright := '(c) Spring4D contributors';
  comp.RepositoryUrl := 'https://github.com/spring4d/spring4d';
  comp.RepositoryCommit := 'abc123';
  comp.HashAlgorithm := 'sha256';
  comp.HashValue := 'deadbeef';
  comp.Authors.Add('Spring4D Team');
  comp.Purl := 'pkg:generic/dpm/Spring.Base@2.0.2?checksum=sha256:deadbeef&vcs_url=https://github.com/spring4d/spring4d@abc123';
  comp.AddProperty('dpm:purl-type', 'dpm');
  report.AddRelationship(report.RootComponent.BomRef, comp.BomRef);

  runtime := report.AddComponent(TSBOMComponentKind.DelphiRuntime);
  runtime.BomRef := 'embarcadero:delphi@23.0';
  runtime.Id := 'Delphi RTL/VCL/FMX';
  runtime.Version := '23.0';
  runtime.Supplier := 'Embarcadero Technologies';
  runtime.Purl := 'pkg:generic/embarcadero/delphi@23.0';
  report.AddRelationship(report.RootComponent.BomRef, runtime.BomRef);

  result := report;
end;

function TSBOMWritersTests.BuildReportWithLicense(const license : string) : TSBOMReport;
var
  report : TSBOMReport;
  comp : TSBOMComponent;
begin
  report := TSBOMReport.Create;
  report.SerialNumber := 'urn:uuid:99999999-9999-9999-9999-999999999999';
  report.TimestampUtc := '2026-05-18T00:00:00Z';
  report.ToolName := 'dpm';
  report.ToolVersion := '1.0.0';
  report.ProjectName := 'LicenseProject';
  report.RootComponent.BomRef := 'root';
  report.RootComponent.Id := 'LicenseProject';
  report.RootComponent.Version := '1.0.0';

  comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'dpm:Licensed@1.0.0';
  comp.Id := 'Licensed';
  comp.Version := '1.0.0';
  comp.License := license;

  result := report;
end;

function TSBOMWritersTests.WriteCycloneDXAndParse(const report : TSBOMReport) : TJsonObject;
var
  writer : ISbomWriter;
  outPath : string;
begin
  writer := TCycloneDXWriter.Create;
  outPath := NewTempPath('.cdx.json');
  try
    writer.Write(report, outPath);
    result := TJsonObject.Parse(ReadAllText(outPath)) as TJsonObject;
  finally
    if FileExists(outPath) then
      DeleteFile(outPath);
  end;
end;

function TSBOMWritersTests.SpdxLicenseDeclaredFor(const license : string) : string;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  doc : TJsonObject;
  packages : TJsonArray;
  i : integer;
begin
  result := '';
  report := BuildReportWithLicense(license);
  try
    writer := TSPDXWriter.Create;
    outPath := NewTempPath('.spdx.json');
    try
      writer.Write(report, outPath);
      doc := TJsonObject.Parse(ReadAllText(outPath)) as TJsonObject;
      try
        packages := doc.A['packages'];
        for i := 0 to packages.Count - 1 do
          if packages.O[i].S['name'] = 'Licensed' then
            result := packages.O[i].S['licenseDeclared'];
      finally
        doc.Free;
      end;
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

function TSBOMWritersTests.WriteSpdxAndParse(const report : TSBOMReport) : TJsonObject;
var
  writer : ISbomWriter;
  outPath : string;
begin
  writer := TSPDXWriter.Create;
  outPath := NewTempPath('.spdx.json');
  try
    writer.Write(report, outPath);
    result := TJsonObject.Parse(ReadAllText(outPath)) as TJsonObject;
  finally
    if FileExists(outPath) then
      DeleteFile(outPath);
  end;
end;

//finds the packages[] entry with the given name, nil when absent.
function FindSpdxPackage(const doc : TJsonObject; const name : string) : TJsonObject;
var
  packages : TJsonArray;
  i : integer;
begin
  result := nil;
  packages := doc.A['packages'];
  for i := 0 to packages.Count - 1 do
    if packages.O[i].S['name'] = name then
      exit(packages.O[i]);
end;

//true when obj (a package or the document) carries an annotation with this comment.
function HasAnnotation(const obj : TJsonObject; const comment : string) : boolean;
var
  annotations : TJsonArray;
  i : integer;
begin
  result := false;
  if not obj.Contains('annotations') then
    exit;
  annotations := obj.A['annotations'];
  for i := 0 to annotations.Count - 1 do
    if annotations.O[i].S['comment'] = comment then
      exit(true);
end;

procedure TSBOMWritersTests.CycloneDX_EmitsRequiredFields;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
  doc : TJsonObject;
begin
  report := BuildSampleReport;
  try
    writer := TCycloneDXWriter.Create;
    outPath := NewTempPath('.cdx.json');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      doc := TJsonObject.Parse(text) as TJsonObject;
      try
        Assert.IsNotNull(doc, 'CycloneDX output should be valid JSON');
        Assert.AreEqual('CycloneDX', doc.S['bomFormat']);
        Assert.AreEqual('1.6', doc.S['specVersion']);
        Assert.IsTrue(doc.Contains('serialNumber'));
        Assert.IsTrue(doc.Contains('metadata'));
        Assert.IsTrue(doc.Contains('components'));
      finally
        doc.Free;
      end;
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_EmitsHashesPurlAndDependencies;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := TCycloneDXWriter.Create;
    outPath := NewTempPath('.cdx.json');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('"hashes"', text) > 0, 'should emit hashes section');
      Assert.IsTrue(Pos('"SHA-256"', text) > 0, 'CycloneDX hash alg should be SHA-256 (dashed)');
      Assert.IsTrue(Pos('pkg:generic/dpm/Spring.Base', text) > 0, 'should emit purl');
      Assert.IsTrue(Pos('"dependencies"', text) > 0, 'should emit dependencies section');
      Assert.IsTrue(Pos('"licenses"', text) > 0, 'should emit licenses section');
      Assert.IsTrue(Pos('"externalReferences"', text) > 0, 'should emit externalReferences');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_EmitsToolsAsComponentsObject;
var
  report : TSBOMReport;
  doc : TJsonObject;
  tools : TJsonObject;
  toolComp : TJsonObject;
begin
  //CycloneDX 1.6 deprecates the legacy metadata.tools array in favour of
  //metadata.tools.components[].
  report := BuildSampleReport;
  try
    doc := WriteCycloneDXAndParse(report);
    try
      Assert.IsTrue(doc.O['metadata'].Contains('tools'), 'expected metadata.tools');
      Assert.AreEqual(Ord(jdtObject), Ord(doc.O['metadata'].Types['tools']),
                      'metadata.tools should be an object, not the deprecated array');
      tools := doc.O['metadata'].O['tools'];
      Assert.IsTrue(tools.Contains('components'), 'expected metadata.tools.components');
      Assert.AreEqual<integer>(1, tools.A['components'].Count);
      toolComp := tools.A['components'].O[0];
      Assert.AreEqual('application', toolComp.S['type']);
      Assert.AreEqual('dpm', toolComp.S['name']);
      Assert.AreEqual('1.0.0', toolComp.S['version']);
      Assert.AreEqual('DPM', toolComp.S['publisher']);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_SpdxLicenseIdEmittedAsLicenseId;
var
  report : TSBOMReport;
  doc : TJsonObject;
  licWrap : TJsonObject;
begin
  //A known SPDX identifier belongs in licenses[].license.id - that field is a strict
  //enum in the schema.
  report := BuildReportWithLicense('Apache-2.0');
  try
    doc := WriteCycloneDXAndParse(report);
    try
      licWrap := doc.A['components'].O[0].A['licenses'].O[0];
      Assert.IsTrue(licWrap.Contains('license'), 'expected licenses[0].license');
      //license.id is a case sensitive enum, hence ignoreCase=false
      Assert.AreEqual('Apache-2.0', licWrap.O['license'].S['id'], false);
      Assert.IsFalse(licWrap.O['license'].Contains('name'), 'should not also emit a name');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_CompoundLicenseEmittedAsExpression;
var
  report : TSBOMReport;
  doc : TJsonObject;
  licArr : TJsonArray;
begin
  //Compound SPDX expressions are not valid license.id values - they go in
  //licenses[0].expression, and the schema allows only that single entry.
  report := BuildReportWithLicense('MIT OR Apache-2.0');
  try
    doc := WriteCycloneDXAndParse(report);
    try
      licArr := doc.A['components'].O[0].A['licenses'];
      Assert.AreEqual<integer>(1, licArr.Count, 'an expression must be the only licenses[] entry');
      Assert.AreEqual('MIT OR Apache-2.0', licArr.O[0].S['expression']);
      Assert.IsFalse(licArr.O[0].Contains('license'), 'expression and license must not be mixed');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_UnknownLicenseEmittedAsLicenseName;
var
  report : TSBOMReport;
  doc : TJsonObject;
  licWrap : TJsonObject;
begin
  //'Apache 2.0' is not an SPDX identifier (the identifier is 'Apache-2.0'), so emitting
  //it as license.id would fail schema validation. It has to fall back to license.name.
  report := BuildReportWithLicense('Apache 2.0');
  try
    doc := WriteCycloneDXAndParse(report);
    try
      licWrap := doc.A['components'].O[0].A['licenses'].O[0];
      Assert.IsTrue(licWrap.Contains('license'), 'expected licenses[0].license');
      Assert.AreEqual('Apache 2.0', licWrap.O['license'].S['name']);
      Assert.IsFalse(licWrap.O['license'].Contains('id'), 'unknown licenses must not be emitted as id');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_EmitsRequiredDocumentFields;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
  doc : TJsonObject;
  packages : TJsonArray;
  i : integer;
  pkg : TJsonObject;
begin
  report := BuildSampleReport;
  try
    writer := TSPDXWriter.Create;
    outPath := NewTempPath('.spdx.json');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      doc := TJsonObject.Parse(text) as TJsonObject;
      try
        Assert.IsNotNull(doc, 'SPDX output should be valid JSON');
        Assert.AreEqual('SPDX-2.3', doc.S['spdxVersion']);
        Assert.AreEqual('CC0-1.0', doc.S['dataLicense']);
        Assert.AreEqual('SPDXRef-DOCUMENT', doc.S['SPDXID']);
        Assert.IsTrue(doc.Contains('documentNamespace'));
        Assert.IsTrue(doc.Contains('creationInfo'));

        Assert.IsTrue(doc.Contains('packages'), 'SPDX output should have packages array');
        packages := doc.A['packages'];
        Assert.IsTrue(packages.Count >= 3, 'expected root + spring + runtime');
        for i := 0 to packages.Count - 1 do
        begin
          pkg := packages.O[i];
          Assert.IsTrue(pkg.Contains('SPDXID'), 'package needs SPDXID');
          Assert.IsTrue(pkg.Contains('downloadLocation'), 'package needs downloadLocation');
          Assert.IsTrue(pkg.Contains('licenseConcluded'), 'package needs licenseConcluded');
          Assert.IsTrue(pkg.Contains('licenseDeclared'), 'package needs licenseDeclared');
          Assert.IsTrue(pkg.Contains('filesAnalyzed'), 'package needs filesAnalyzed');
          Assert.IsFalse(pkg.B['filesAnalyzed'], 'filesAnalyzed should be false');
        end;
      finally
        doc.Free;
      end;
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_EmitsDescribesRelationship;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := TSPDXWriter.Create;
    outPath := NewTempPath('.spdx.json');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('"DESCRIBES"', text) > 0, 'expected DOCUMENT DESCRIBES root relationship');
      Assert.IsTrue(Pos('"DEPENDS_ON"', text) > 0, 'expected DEPENDS_ON relationships for dependencies');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_LooseLicenseBecomesNoAssertion;
var
  report : TSBOMReport;
  comp : TSBOMComponent;
  writer : ISbomWriter;
  outPath : string;
  doc : TJsonObject;
  packages : TJsonArray;
  i : integer;
  pkg : TJsonObject;
  weirdPkg : TJsonObject;
begin
  report := TSBOMReport.Create;
  try
    report.SerialNumber := 'urn:uuid:nooasertion';
    report.TimestampUtc := '2026-05-18T00:00:00Z';
    report.ProjectName := 'P';
    report.RootComponent.BomRef := 'p';
    report.RootComponent.Id := 'P';
    report.RootComponent.Version := '0';

    comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
    comp.BomRef := 'dpm:Weird@1';
    comp.Id := 'Weird';
    comp.Version := '1';
    //License with characters that aren't part of an SPDX expression (commas, slashes) should
    //force a NOASSERTION on licenseDeclared, not be passed through as-is.
    comp.License := 'See the COPYING file in the source tree, /etc.';

    writer := TSPDXWriter.Create;
    outPath := NewTempPath('.spdx.json');
    try
      writer.Write(report, outPath);
      doc := TJsonObject.Parse(ReadAllText(outPath)) as TJsonObject;
      try
        packages := doc.A['packages'];
        weirdPkg := nil;
        for i := 0 to packages.Count - 1 do
        begin
          pkg := packages.O[i];
          if pkg.S['name'] = 'Weird' then
          begin
            weirdPkg := pkg;
            break;
          end;
        end;
        Assert.IsNotNull(weirdPkg, 'expected Weird package in SPDX output');
        Assert.AreEqual('NOASSERTION', weirdPkg.S['licenseDeclared'],
                        'free-text license should fall back to NOASSERTION');
      finally
        doc.Free;
      end;
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.CycloneDX_LicenseIdOutsideTheCommonSetIsStillRecognised;
var
  report : TSBOMReport;
  doc : TJsonObject;
  licWrap : TJsonObject;
begin
  //Identifier recognition comes from the full SPDX list shipped in DPM_SPDX_LICENSES,
  //not a hand-picked shortlist - so a rarely-seen-but-valid id still emits as an id.
  report := BuildReportWithLicense('Sleepycat');
  try
    doc := WriteCycloneDXAndParse(report);
    try
      licWrap := doc.A['components'].O[0].A['licenses'].O[0];
      Assert.AreEqual('Sleepycat', licWrap.O['license'].S['id'], false);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_UnknownLicenseIdBecomesNoAssertion;
begin
  //'Apache 2.0' passes the loose character-set heuristic but is not an SPDX identifier,
  //so SPDX licenseDeclared has to be NOASSERTION rather than the raw string.
  Assert.AreEqual('NOASSERTION', SpdxLicenseDeclaredFor('Apache 2.0'));
  Assert.AreEqual('NOASSERTION', SpdxLicenseDeclaredFor('Free for non commercial use'));
end;

procedure TSBOMWritersTests.SPDX_SpdxLicenseIdIsDeclaredVerbatim;
begin
  Assert.AreEqual('Apache-2.0', SpdxLicenseDeclaredFor('Apache-2.0'), false);
  //canonical casing, matching what the CycloneDX writer emits. ignoreCase=false matters
  //here - DUnitX string comparisons ignore case by default.
  Assert.AreEqual('MIT', SpdxLicenseDeclaredFor('mit'), false);
end;

procedure TSBOMWritersTests.SPDX_CompoundLicenseExpressionIsDeclared;
begin
  Assert.AreEqual('MIT OR Apache-2.0', SpdxLicenseDeclaredFor('MIT OR Apache-2.0'));
end;

procedure TSBOMWritersTests.SPDX_PrimaryPackagePurposeReflectsComponentKind;
var
  report : TSBOMReport;
  doc : TJsonObject;
begin
  //SPDX 2.3 primaryPackagePurpose is the closest first-class equivalent of the CycloneDX
  //component type - without it an SPDX consumer cannot tell the Delphi runtime from a package.
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      Assert.AreEqual('APPLICATION', FindSpdxPackage(doc, 'SampleProject').S['primaryPackagePurpose'], false);
      Assert.AreEqual('LIBRARY', FindSpdxPackage(doc, 'Spring.Base').S['primaryPackagePurpose'], false);
      Assert.AreEqual('FRAMEWORK', FindSpdxPackage(doc, 'Delphi RTL/VCL/FMX').S['primaryPackagePurpose'], false);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_ComponentKindIsAnnotated;
var
  report : TSBOMReport;
  doc : TJsonObject;
begin
  //primaryPackagePurpose collapses dpm-package / third-party / unidentified into LIBRARY,
  //so the DPM kind rides along as an annotation - the same information CycloneDX keeps
  //in the dpm:component-kind property.
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      Assert.IsTrue(HasAnnotation(FindSpdxPackage(doc, 'Spring.Base'), 'dpm:component-kind=dpm-package'),
                    'expected dpm:component-kind annotation on the package');
      Assert.IsTrue(HasAnnotation(FindSpdxPackage(doc, 'Delphi RTL/VCL/FMX'), 'dpm:component-kind=delphi-runtime'),
                    'expected dpm:component-kind annotation on the runtime');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_DocumentAnnotatesPlatformCompilerAndMetaProperties;
var
  report : TSBOMReport;
  doc : TJsonObject;
begin
  //The CycloneDX writer puts these in metadata.properties[]. SPDX has no properties bag at
  //document level, so they become document annotations - otherwise the compiler version and
  //any evidence notes are lost entirely and the platform survives only in the document name.
  report := BuildSampleReport;
  try
    report.AddMetaProperty('dpm:evidence.note', 'map-file-no-source-paths');
    doc := WriteSpdxAndParse(report);
    try
      Assert.IsTrue(HasAnnotation(doc, 'dpm:platform=win64'), 'expected dpm:platform document annotation');
      Assert.IsTrue(HasAnnotation(doc, 'dpm:compilerVersion=delphi12.0'), 'expected dpm:compilerVersion document annotation');
      Assert.IsTrue(HasAnnotation(doc, 'dpm:evidence.note=map-file-no-source-paths'),
                    'expected report meta properties to reach the document');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_AuthorsBecomeOriginatorNotSupplier;
var
  report : TSBOMReport;
  doc : TJsonObject;
  pkg : TJsonObject;
begin
  //SPDX splits these: supplier is who distributed the package, originator is who created it.
  //Spring.Base in the sample has authors but no supplier.
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      pkg := FindSpdxPackage(doc, 'Spring.Base');
      Assert.AreEqual('Organization: Spring4D Team', pkg.S['originator']);
      Assert.AreEqual('NOASSERTION', pkg.S['supplier'], 'authors must not be asserted as the supplier');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_SupplierIsUsedWhenPresent;
var
  report : TSBOMReport;
  doc : TJsonObject;
  pkg : TJsonObject;
begin
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      //the runtime component carries a supplier and no authors
      pkg := FindSpdxPackage(doc, 'Delphi RTL/VCL/FMX');
      Assert.AreEqual('Organization: Embarcadero Technologies', pkg.S['supplier']);
      Assert.IsFalse(pkg.Contains('originator'), 'no authors means no originator');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_RepositoryBecomesDownloadLocationWhenNoDownloadUrl;
var
  report : TSBOMReport;
  doc : TJsonObject;
  pkg : TJsonObject;
begin
  //Spring.Base has a repository url + commit and no download url. SPDX accepts VCS syntax
  //in downloadLocation, so 'NOASSERTION' would be throwing away something we know.
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      pkg := FindSpdxPackage(doc, 'Spring.Base');
      Assert.AreEqual('git+https://github.com/spring4d/spring4d@abc123', pkg.S['downloadLocation']);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_DownloadUrlWinsAndRepositoryGoesToSourceInfo;
var
  report : TSBOMReport;
  doc : TJsonObject;
  pkg : TJsonObject;
begin
  report := BuildSampleReport;
  try
    report.Components[0].DownloadUrl := 'https://cdn.example.com/spring.base-2.0.2.dpkg';
    doc := WriteSpdxAndParse(report);
    try
      pkg := FindSpdxPackage(doc, 'Spring.Base');
      Assert.AreEqual('https://cdn.example.com/spring.base-2.0.2.dpkg', pkg.S['downloadLocation']);
      Assert.AreEqual('git+https://github.com/spring4d/spring4d@abc123', pkg.S['sourceInfo'],
                      'the repository location should still be recorded');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_AnnotatorCarriesToolVersion;
var
  report : TSBOMReport;
  doc : TJsonObject;
  pkg : TJsonObject;
begin
  //SPDX wants the same 'Tool: <id>-<version>' identifier used in creationInfo.creators.
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      pkg := FindSpdxPackage(doc, 'Spring.Base');
      Assert.AreEqual('Tool: dpm-1.0.0', pkg.A['annotations'].O[0].S['annotator']);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_CreationInfoDeclaresLicenseListVersion;
var
  report : TSBOMReport;
  doc : TJsonObject;
begin
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      Assert.AreEqual(TSpdxLicenses.LicenseListVersion, doc.O['creationInfo'].S['licenseListVersion'], false);
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMWritersTests.SPDX_DocumentNamespaceIsUnderTheProjectDomain;
var
  report : TSBOMReport;
  doc : TJsonObject;
  ns : string;
begin
  report := BuildSampleReport;
  try
    doc := WriteSpdxAndParse(report);
    try
      ns := doc.S['documentNamespace'];
      Assert.StartsWith('https://github.com/DelphiPackageManager/DPM/spdxdocs/', ns,
                        'the namespace should sit under a domain the project actually owns');
      //uniqueness still comes from the report serial number
      Assert.IsTrue(Pos('11111111-2222-3333-4444-555555555555', ns) > 0, 'expected the serial in the namespace');
      Assert.IsFalse(Pos('#', ns) > 0, 'SPDX forbids a fragment in documentNamespace');
    finally
      doc.Free;
    end;
  finally
    report.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMWritersTests);

end.
