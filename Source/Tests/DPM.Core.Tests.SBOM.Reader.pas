unit DPM.Core.Tests.SBOM.Reader;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSBOMReaderTests = class
  public
    [Test]
    procedure ReadsBomFormatAndVersion;
    [Test]
    procedure RejectsNonCycloneDX;
    [Test]
    procedure ReadsRootComponentAndComponents;
    [Test]
    procedure ReadsDependencies;
    [Test]
    procedure RecoversKindFromDpmComponentKindProperty;
    [Test]
    procedure InfersKindFromTypeWhenPropertyMissing;
    [Test]
    procedure ReadsHashesAndPurl;
    [Test]
    procedure RoundTripsWriterOutput;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.SBOM.Types,
  DPM.Core.SBOM.Reader,
  DPM.Core.SBOM.Writers,
  DPM.Core.SBOM.Interfaces;

const
  cMinimalSbom =
    '{' +
    '"bomFormat":"CycloneDX",' +
    '"specVersion":"1.5",' +
    '"version":1,' +
    '"serialNumber":"urn:uuid:11111111-1111-1111-1111-111111111111",' +
    '"metadata":{' +
      '"timestamp":"2026-05-19T10:00:00Z",' +
      '"tools":[{"vendor":"DPM","name":"dpm","version":"1.0.0"}],' +
      '"component":{' +
        '"type":"application",' +
        '"bom-ref":"app-1",' +
        '"name":"MyApp","version":"1.2.3",' +
        '"properties":[{"name":"dpm:component-kind","value":"application"}]' +
      '}' +
    '},' +
    '"components":[' +
      '{"type":"library","bom-ref":"dpm-1","name":"VSoft.JsonDataObjects","version":"0.3.0",' +
        '"purl":"pkg:generic/dpm/VSoft.JsonDataObjects@0.3.0",' +
        '"hashes":[{"alg":"SHA-256","content":"abc123"}],' +
        '"properties":[{"name":"dpm:component-kind","value":"dpm-package"}]},' +
      '{"type":"framework","bom-ref":"rt-1","name":"Delphi RTL/VCL","version":"29.0.55362.2017",' +
        '"properties":[{"name":"dpm:component-kind","value":"delphi-runtime"}]},' +
      '{"type":"library","bom-ref":"tp-1","name":"third.party.unit","version":"0",' +
        '"properties":[{"name":"dpm:component-kind","value":"third-party"}]}' +
    '],' +
    '"dependencies":[' +
      '{"ref":"app-1","dependsOn":["dpm-1","rt-1","tp-1"]}' +
    ']' +
    '}';

procedure TSBOMReaderTests.ReadsBomFormatAndVersion;
var
  reader : ISBOMReader;
  report : TSBOMReport;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cMinimalSbom);
  try
    Assert.AreEqual('urn:uuid:11111111-1111-1111-1111-111111111111', report.SerialNumber);
    Assert.AreEqual('2026-05-19T10:00:00Z', report.TimestampUtc);
    Assert.AreEqual('dpm', report.ToolName);
    Assert.AreEqual('1.0.0', report.ToolVersion);
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.RejectsNonCycloneDX;
var
  reader : ISBOMReader;
begin
  reader := TCycloneDXReader.Create;
  Assert.WillRaise(
    procedure
    begin
      reader.ReadFromString('{"bomFormat":"SPDX","specVersion":"2.3"}').Free;
    end,
    EArgumentException);
end;

procedure TSBOMReaderTests.ReadsRootComponentAndComponents;
var
  reader : ISBOMReader;
  report : TSBOMReport;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cMinimalSbom);
  try
    Assert.AreEqual('MyApp', report.ProjectName);
    Assert.AreEqual('1.2.3', report.ProjectVersion);
    Assert.AreEqual('app-1', report.RootComponent.BomRef);
    Assert.AreEqual(3, report.Components.Count, 'expected 3 components (dpm + runtime + third-party)');
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.ReadsDependencies;
var
  reader : ISBOMReader;
  report : TSBOMReport;
  rel : TSBOMRelationship;
  countFromApp : integer;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cMinimalSbom);
  try
    Assert.AreEqual(3, report.Relationships.Count);
    countFromApp := 0;
    for rel in report.Relationships do
      if rel.ParentBomRef = 'app-1' then
        Inc(countFromApp);
    Assert.AreEqual(3, countFromApp, 'all relationships should fan out from the application root');
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.RecoversKindFromDpmComponentKindProperty;
var
  reader : ISBOMReader;
  report : TSBOMReport;
  comp : TSBOMComponent;
  dpmFound, runtimeFound, thirdPartyFound : boolean;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cMinimalSbom);
  try
    dpmFound := false; runtimeFound := false; thirdPartyFound := false;
    for comp in report.Components do
    begin
      case comp.Kind of
        TSBOMComponentKind.DpmPackage : dpmFound := true;
        TSBOMComponentKind.DelphiRuntime : runtimeFound := true;
        TSBOMComponentKind.ThirdParty : thirdPartyFound := true;
      end;
    end;
    Assert.IsTrue(dpmFound, 'expected at least one DpmPackage');
    Assert.IsTrue(runtimeFound, 'expected at least one DelphiRuntime');
    Assert.IsTrue(thirdPartyFound, 'expected at least one ThirdParty');
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.InfersKindFromTypeWhenPropertyMissing;
const
  cNoKindProps =
    '{"bomFormat":"CycloneDX","specVersion":"1.5","version":1,' +
    '"components":[' +
      '{"type":"library","bom-ref":"a","name":"foo","version":"1.0",' +
        '"purl":"pkg:generic/dpm/foo@1.0"},' +
      '{"type":"framework","bom-ref":"b","name":"rtl","version":"29.0"},' +
      '{"type":"library","bom-ref":"c","name":"thirdparty","version":"2.0",' +
        '"purl":"pkg:maven/org.apache/foo@1.0"}' +
    ']}';
var
  reader : ISBOMReader;
  report : TSBOMReport;
  kinds : array of TSBOMComponentKind;
  i : integer;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cNoKindProps);
  try
    SetLength(kinds, report.Components.Count);
    for i := 0 to report.Components.Count - 1 do
      kinds[i] := report.Components[i].Kind;
    //pkg:generic/dpm/... in a library -> DpmPackage
    Assert.AreEqual(Ord(TSBOMComponentKind.DpmPackage), Ord(kinds[0]));
    //framework type -> DelphiRuntime
    Assert.AreEqual(Ord(TSBOMComponentKind.DelphiRuntime), Ord(kinds[1]));
    //library with non-dpm purl -> ThirdParty
    Assert.AreEqual(Ord(TSBOMComponentKind.ThirdParty), Ord(kinds[2]));
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.ReadsHashesAndPurl;
var
  reader : ISBOMReader;
  report : TSBOMReport;
  dpmComp : TSBOMComponent;
begin
  reader := TCycloneDXReader.Create;
  report := reader.ReadFromString(cMinimalSbom);
  try
    dpmComp := report.FindComponentById('VSoft.JsonDataObjects');
    Assert.IsNotNull(dpmComp);
    Assert.AreEqual('pkg:generic/dpm/VSoft.JsonDataObjects@0.3.0', dpmComp.Purl);
    Assert.AreEqual('SHA-256', dpmComp.HashAlgorithm);
    Assert.AreEqual('abc123', dpmComp.HashValue);
  finally
    report.Free;
  end;
end;

procedure TSBOMReaderTests.RoundTripsWriterOutput;
var
  report : TSBOMReport;
  comp : TSBOMComponent;
  writer : ISbomWriter;
  reader : ISBOMReader;
  tempFile : string;
  roundTripped : TSBOMReport;
  recovered : TSBOMComponent;
begin
  //Build a report -> write to disk -> read back -> assert the load-bearing
  //fields round-trip cleanly.
  report := TSBOMReport.Create;
  try
    report.SerialNumber := 'urn:uuid:22222222-2222-2222-2222-222222222222';
    report.TimestampUtc := '2026-05-19T11:00:00Z';
    report.ToolName := 'dpm';
    report.ToolVersion := '1.0.0';
    report.ProjectName := 'RoundTripDemo';
    report.ProjectVersion := '0.1.0';
    report.Platform := TDPMPlatform.Win32;
    report.RootComponent.BomRef := 'root';
    report.RootComponent.Id := 'RoundTripDemo';
    report.RootComponent.Version := '0.1.0';

    comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
    comp.BomRef := 'pkg-1';
    comp.Id := 'foo';
    comp.Version := '1.0.0';
    comp.Purl := 'pkg:generic/dpm/foo@1.0.0';
    comp.HashAlgorithm := 'SHA-256';
    comp.HashValue := 'deadbeef';
    report.AddRelationship('root', 'pkg-1');

    tempFile := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'sbom-roundtrip.cdx.json';
    writer := TCycloneDXWriter.Create;
    writer.Write(report, tempFile);
    try
      reader := TCycloneDXReader.Create;
      roundTripped := reader.ReadFromFile(tempFile);
      try
        Assert.AreEqual(report.SerialNumber, roundTripped.SerialNumber);
        Assert.AreEqual(report.ProjectName, roundTripped.ProjectName);
        Assert.AreEqual(1, roundTripped.Components.Count);
        recovered := roundTripped.Components[0];
        Assert.AreEqual('pkg-1', recovered.BomRef);
        Assert.AreEqual('foo', recovered.Id);
        Assert.AreEqual('1.0.0', recovered.Version);
        Assert.AreEqual('pkg:generic/dpm/foo@1.0.0', recovered.Purl);
        Assert.AreEqual(Ord(TSBOMComponentKind.DpmPackage), Ord(recovered.Kind),
                        'kind must round-trip via dpm:component-kind property');
        Assert.AreEqual('SHA-256', recovered.HashAlgorithm);
        Assert.AreEqual('deadbeef', recovered.HashValue);
        Assert.AreEqual(1, roundTripped.Relationships.Count);
      finally
        roundTripped.Free;
      end;
    finally
      if FileExists(tempFile) then
        TFile.Delete(tempFile);
    end;
  finally
    report.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMReaderTests);

end.
