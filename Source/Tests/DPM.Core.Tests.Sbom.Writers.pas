unit DPM.Core.Tests.SBOM.Writers;

interface

uses
  DUnitX.TestFramework,
  DPM.Core.SBOM.Types;

type
  [TestFixture]
  TSBOMWritersTests = class
  private
    function BuildSampleReport : TSBOMReport;
    function NewTempPath(const ext : string) : string;
    function ReadAllText(const fileName : string) : string;
  public
    [Test]
    procedure CycloneDX_EmitsRequiredFields;
    [Test]
    procedure CycloneDX_EmitsHashesPurlAndDependencies;
    [Test]
    procedure SPDX_EmitsRequiredDocumentFields;
    [Test]
    procedure SPDX_EmitsDescribesRelationship;
    [Test]
    procedure SPDX_LooseLicenseBecomesNoAssertion;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  JsonDataObjects,
  DPM.Core.Types,
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
        Assert.AreEqual('1.5', doc.S['specVersion']);
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

initialization
  TDUnitX.RegisterTestFixture(TSBOMWritersTests);

end.
