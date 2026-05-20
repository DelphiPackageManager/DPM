unit DPM.Core.Tests.Vuln.CycloneDX;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TCycloneDxVexWriterTests = class
  public
    [Test]
    procedure EmptyReportEmitsEmptyVulnerabilitiesArray;
    [Test]
    procedure SingleVulnHasAffectsRefMatchingSourceBomRef;
    [Test]
    procedure SeverityRendersAsCycloneDXEnum;
    [Test]
    procedure SourceObjectIsPickedFromIdPrefix;
    [Test]
    procedure CycloneDXBomFormatAndSpecVersionAreSet;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  JsonDataObjects,
  DPM.Core.SBOM.Types,
  DPM.Core.Vuln.Types,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Writer.CycloneDX;

function ReadVexJsonToObject(const fileName : string) : TJsonObject;
var
  raw : string;
begin
  raw := TFile.ReadAllText(fileName, TEncoding.UTF8);
  result := TJsonBaseObject.Parse(raw) as TJsonObject;
end;

function MakeSbom(out comp : TSBOMComponent) : TSBOMReport;
begin
  result := TSBOMReport.Create;
  result.SerialNumber := 'urn:uuid:33333333-3333-3333-3333-333333333333';
  result.ProjectName := 'MyApp';
  result.RootComponent.BomRef := 'root';
  result.RootComponent.Id := 'MyApp';
  comp := result.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'pkg-foo';
  comp.Id := 'foo';
  comp.Version := '1.0.0';
  comp.Purl := 'pkg:generic/dpm/foo@1.0.0';
end;

procedure TCycloneDxVexWriterTests.EmptyReportEmitsEmptyVulnerabilitiesArray;
var
  sbom : TSBOMReport;
  vuln : TVulnReport;
  comp : TSBOMComponent;
  writer : IVulnWriter;
  outFile : string;
  obj : TJsonObject;
begin
  sbom := MakeSbom(comp);
  try
    vuln := TVulnReport.Create;
    try
      vuln.SerialNumber := 'urn:uuid:00000000-0000-0000-0000-000000000000';
      vuln.TimestampUtc := '2026-05-19T12:00:00Z';
      vuln.ToolName := 'dpm-scan';
      vuln.ToolVersion := '1.0.0';
      vuln.SourceSbomSerial := sbom.SerialNumber;

      outFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'vex-empty.json';
      writer := TCycloneDxVexWriter.Create;
      writer.Write(sbom, vuln, outFile);
      try
        obj := ReadVexJsonToObject(outFile);
        try
          Assert.IsTrue(obj.Contains('vulnerabilities'));
          Assert.AreEqual<integer>(0, obj.A['vulnerabilities'].Count);
        finally
          obj.Free;
        end;
      finally
        if FileExists(outFile) then TFile.Delete(outFile);
      end;
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TCycloneDxVexWriterTests.SingleVulnHasAffectsRefMatchingSourceBomRef;
var
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  v : TVulnerability;
  affected : TVulnAffected;
  writer : IVulnWriter;
  outFile : string;
  obj : TJsonObject;
  vulns : TJsonArray;
  affects : TJsonArray;
begin
  sbom := MakeSbom(comp);
  try
    vuln := TVulnReport.Create;
    try
      vuln.SerialNumber := 'urn:uuid:00000000-0000-0000-0000-000000000000';
      vuln.TimestampUtc := '2026-05-19T12:00:00Z';
      vuln.ToolName := 'dpm-scan';
      vuln.ToolVersion := '1.0.0';
      vuln.SourceSbomSerial := sbom.SerialNumber;
      v := vuln.AddVulnerability('GHSA-test-1234');
      v.Severity := TSeverity.High;
      v.Summary := 'Test vuln summary';
      v.CvssScore := 7.5;
      v.CvssVector := 'CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H';
      affected.BomRef := comp.BomRef;
      affected.ComponentId := comp.Id;
      affected.ComponentVersion := comp.Version;
      affected.Purl := comp.Purl;
      affected.FixedInVersion := '1.0.1';
      v.AddAffected(affected);
      vuln.Finalize;

      outFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'vex-single.json';
      writer := TCycloneDxVexWriter.Create;
      writer.Write(sbom, vuln, outFile);
      try
        obj := ReadVexJsonToObject(outFile);
        try
          vulns := obj.A['vulnerabilities'];
          Assert.AreEqual<integer>(1, vulns.Count);
          Assert.AreEqual('GHSA-test-1234', vulns.O[0].S['id']);
          affects := vulns.O[0].A['affects'];
          Assert.AreEqual<integer>(1, affects.Count);
          Assert.AreEqual(comp.BomRef, affects.O[0].S['ref'],
                          'affects[].ref must point at the source SBOM bom-ref');
        finally
          obj.Free;
        end;
      finally
        if FileExists(outFile) then TFile.Delete(outFile);
      end;
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TCycloneDxVexWriterTests.SeverityRendersAsCycloneDXEnum;
var
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  v : TVulnerability;
  affected : TVulnAffected;
  writer : IVulnWriter;
  outFile : string;
  obj : TJsonObject;
  ratings : TJsonArray;
begin
  sbom := MakeSbom(comp);
  try
    vuln := TVulnReport.Create;
    try
      v := vuln.AddVulnerability('CVE-2024-9999');
      v.Severity := TSeverity.Critical;
      affected.BomRef := comp.BomRef;
      affected.ComponentId := comp.Id;
      affected.ComponentVersion := comp.Version;
      affected.Purl := comp.Purl;
      v.AddAffected(affected);

      outFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'vex-sev.json';
      writer := TCycloneDxVexWriter.Create;
      writer.Write(sbom, vuln, outFile);
      try
        obj := ReadVexJsonToObject(outFile);
        try
          ratings := obj.A['vulnerabilities'].O[0].A['ratings'];
          Assert.AreEqual<integer>(1, ratings.Count);
          Assert.AreEqual('critical', ratings.O[0].S['severity']);
        finally
          obj.Free;
        end;
      finally
        if FileExists(outFile) then TFile.Delete(outFile);
      end;
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TCycloneDxVexWriterTests.SourceObjectIsPickedFromIdPrefix;
var
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  v : TVulnerability;
  affected : TVulnAffected;
  writer : IVulnWriter;
  outFile : string;
  obj : TJsonObject;
  vulnObj : TJsonObject;
  sourceObj : TJsonObject;
begin
  sbom := MakeSbom(comp);
  try
    vuln := TVulnReport.Create;
    try
      v := vuln.AddVulnerability('GHSA-aaaa-bbbb-cccc');
      v.Severity := TSeverity.Low;
      affected.BomRef := comp.BomRef;
      affected.ComponentId := comp.Id;
      affected.ComponentVersion := comp.Version;
      affected.Purl := comp.Purl;
      v.AddAffected(affected);

      outFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'vex-source.json';
      writer := TCycloneDxVexWriter.Create;
      writer.Write(sbom, vuln, outFile);
      try
        obj := ReadVexJsonToObject(outFile);
        try
          vulnObj := obj.A['vulnerabilities'].O[0];
          sourceObj := vulnObj.O['source'];
          Assert.AreEqual('GitHub Advisory Database', sourceObj.S['name']);
          Assert.IsTrue(Pos('github.com/advisories/', sourceObj.S['url']) > 0);
        finally
          obj.Free;
        end;
      finally
        if FileExists(outFile) then TFile.Delete(outFile);
      end;
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TCycloneDxVexWriterTests.CycloneDXBomFormatAndSpecVersionAreSet;
var
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  writer : IVulnWriter;
  outFile : string;
  obj : TJsonObject;
begin
  sbom := MakeSbom(comp);
  try
    vuln := TVulnReport.Create;
    try
      outFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'vex-format.json';
      writer := TCycloneDxVexWriter.Create;
      writer.Write(sbom, vuln, outFile);
      try
        obj := ReadVexJsonToObject(outFile);
        try
          Assert.AreEqual('CycloneDX', obj.S['bomFormat']);
          Assert.AreEqual('1.5', obj.S['specVersion']);
        finally
          obj.Free;
        end;
      finally
        if FileExists(outFile) then TFile.Delete(outFile);
      end;
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCycloneDxVexWriterTests);

end.
