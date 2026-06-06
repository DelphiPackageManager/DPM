unit DPM.Core.Tests.Vuln.Scanner;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVulnScannerTests = class
  public
    [Test]
    procedure ZeroVulnsOnCleanSbom;
    [Test]
    procedure SinglePackageOneVulnIsReported;
    [Test]
    procedure DedupesSameAdvisoryAcrossComponents;
    [Test]
    procedure MaxSeverityIsHighestFound;
    [Test]
    procedure ComponentsWithoutPurlAreSkipped;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.SBOM.Types,
  DPM.Core.Vuln.Types,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Scanner;

type
  //Test double - implements IVulnDatabaseAdvisory with constructor-injected values.
  TFakeAdvisory = class(TInterfacedObject, IVulnDatabaseAdvisory)
  private
    FId : string;
    FSeverity : TSeverity;
    FFixedVersion : string;
    FAliases : IList<TVulnAlias>;
    FReferences : IList<string>;
  protected
    function GetId : string;
    function GetAliases : IList<TVulnAlias>;
    function GetSummary : string;
    function GetDetails : string;
    function GetSeverity : TSeverity;
    function GetCvssScore : Double;
    function GetCvssVector : string;
    function GetPublished : string;
    function GetModified : string;
    function GetReferences : IList<string>;
    function GetFixedVersion : string;
  public
    constructor Create(const id : string; const severity : TSeverity; const fixedVersion : string = '');
  end;

  //Test double - returns a fixed advisory mapping per purl.
  TFakeDatabase = class(TInterfacedObject, IVulnDatabase)
  private
    FMap : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
  protected
    function GetSourceName : string;
    function Query(const cancellationToken : ICancellationToken;
                   const purls : IList<string>) : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
  public
    constructor Create;
    procedure AddVuln(const purl : string; const advisory : IVulnDatabaseAdvisory);
  end;

  //Trivial silent logger so test output isn't polluted.
  TSilentLogger = class(TInterfacedObject, ILogger)
  protected
    function GetVerbosity : TVerbosity;
    procedure SetVerbosity(const value : TVerbosity);
    procedure Debug(const data : string);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure Error(const data : string);
    procedure Clear; //not implemented in the console logger.
    procedure NewLine;
    function GetUseColor : boolean;
    procedure SetUseColor(const value : boolean);

  end;

{ TFakeAdvisory }

constructor TFakeAdvisory.Create(const id : string; const severity : TSeverity; const fixedVersion : string);
begin
  inherited Create;
  FId := id;
  FSeverity := severity;
  FFixedVersion := fixedVersion;
  FAliases := TCollections.CreateList<TVulnAlias>;
  FReferences := TCollections.CreateList<string>;
end;

function TFakeAdvisory.GetId : string;          begin result := FId; end;
function TFakeAdvisory.GetAliases : IList<TVulnAlias>; begin result := FAliases; end;
function TFakeAdvisory.GetSummary : string;     begin result := ''; end;
function TFakeAdvisory.GetDetails : string;     begin result := ''; end;
function TFakeAdvisory.GetSeverity : TSeverity; begin result := FSeverity; end;
function TFakeAdvisory.GetCvssScore : Double;   begin result := 0.0; end;
function TFakeAdvisory.GetCvssVector : string;  begin result := ''; end;
function TFakeAdvisory.GetPublished : string;   begin result := ''; end;
function TFakeAdvisory.GetModified : string;    begin result := ''; end;
function TFakeAdvisory.GetReferences : IList<string>; begin result := FReferences; end;
function TFakeAdvisory.GetFixedVersion : string; begin result := FFixedVersion; end;

{ TFakeDatabase }

constructor TFakeDatabase.Create;
begin
  inherited Create;
  FMap := TCollections.CreateDictionary<string, IList<IVulnDatabaseAdvisory>>;
end;

function TFakeDatabase.GetSourceName : string;
begin
  result := 'fake';
end;

procedure TFakeDatabase.AddVuln(const purl : string; const advisory : IVulnDatabaseAdvisory);
var
  list : IList<IVulnDatabaseAdvisory>;
begin
  if not FMap.TryGetValue(purl, list) then
  begin
    list := TCollections.CreateList<IVulnDatabaseAdvisory>;
    FMap[purl] := list;
  end;
  list.Add(advisory);
end;

function TFakeDatabase.Query(const cancellationToken : ICancellationToken;
                             const purls : IList<string>) : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
var
  purl : string;
begin
  result := TCollections.CreateDictionary<string, IList<IVulnDatabaseAdvisory>>;
  for purl in purls do
    if FMap.ContainsKey(purl) then
      result[purl] := FMap[purl];
end;

{ TSilentLogger }

function TSilentLogger.GetVerbosity : TVerbosity;
begin
result := TVerbosity.Quiet;
end;
procedure TSilentLogger.SetVerbosity(const value : TVerbosity);
begin
end;
procedure TSilentLogger.Clear;
begin

end;

procedure TSilentLogger.Debug(const data : string);
begin
end;
procedure TSilentLogger.Verbose(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Information(const data : string; const important : Boolean = False); begin end;
procedure TSilentLogger.Success(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Warning(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Error(const data : string); begin end;
procedure TSilentLogger.NewLine; begin end;
function TSilentLogger.GetUseColor : boolean; begin result := false; end;
procedure TSilentLogger.SetUseColor(const value : boolean); begin end;

function MakeReportWithPackage(const purl, id, version : string) : TSBOMReport;
var
  comp : TSBOMComponent;
begin
  result := TSBOMReport.Create;
  result.ProjectName := 'TestApp';
  comp := result.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'pkg-1';
  comp.Id := id;
  comp.Version := version;
  comp.Purl := purl;
end;

{ TVulnScannerTests }

procedure TVulnScannerTests.ZeroVulnsOnCleanSbom;
var
  scanner : IVulnScanner;
  db : IVulnDatabase;
  sbom : TSBOMReport;
  vuln : TVulnReport;
  ct : ICancellationToken;
begin
  db := TFakeDatabase.Create;
  scanner := TVulnScanner.Create(TSilentLogger.Create, db);
  sbom := MakeReportWithPackage('pkg:generic/dpm/clean@1.0.0', 'clean', '1.0.0');
  try
    ct := TCancellationTokenSourceFactory.Create.Token;
    vuln := scanner.Scan(ct, sbom);
    try
      Assert.AreEqual(0, vuln.Vulnerabilities.Count);
      Assert.AreEqual(1, vuln.ComponentsScanned);
      Assert.AreEqual(0, vuln.ComponentsAffected);
      Assert.AreEqual(Ord(TSeverity.None), Ord(vuln.MaxSeverity));
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TVulnScannerTests.SinglePackageOneVulnIsReported;
var
  scanner : IVulnScanner;
  db : TFakeDatabase;
  sbom : TSBOMReport;
  vuln : TVulnReport;
  ct : ICancellationToken;
  advisory : IVulnDatabaseAdvisory;
begin
  db := TFakeDatabase.Create;
  advisory := TFakeAdvisory.Create('GHSA-aaaa-bbbb-cccc', TSeverity.High, '1.0.1');
  db.AddVuln('pkg:generic/dpm/bad@1.0.0', advisory);
  scanner := TVulnScanner.Create(TSilentLogger.Create, db);
  sbom := MakeReportWithPackage('pkg:generic/dpm/bad@1.0.0', 'bad', '1.0.0');
  try
    ct := TCancellationTokenSourceFactory.Create.Token;
    vuln := scanner.Scan(ct, sbom);
    try
      Assert.AreEqual(1, vuln.Vulnerabilities.Count);
      Assert.AreEqual('GHSA-aaaa-bbbb-cccc', vuln.Vulnerabilities[0].Id);
      Assert.AreEqual(1, vuln.ComponentsAffected);
      Assert.AreEqual(Ord(TSeverity.High), Ord(vuln.MaxSeverity));
      Assert.AreEqual('pkg-1', vuln.Vulnerabilities[0].Affects[0].BomRef);
      Assert.AreEqual('1.0.1', vuln.Vulnerabilities[0].Affects[0].FixedInVersion);
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TVulnScannerTests.DedupesSameAdvisoryAcrossComponents;
var
  scanner : IVulnScanner;
  db : TFakeDatabase;
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  shared : IVulnDatabaseAdvisory;
  vuln : TVulnReport;
  ct : ICancellationToken;
begin
  //Two different packages, same advisory id - should collapse into ONE vuln entry
  //with TWO affects[] entries.
  shared := TFakeAdvisory.Create('CVE-2024-9999', TSeverity.Critical);
  db := TFakeDatabase.Create;
  db.AddVuln('pkg:generic/dpm/a@1.0.0', shared);
  db.AddVuln('pkg:generic/dpm/b@2.0.0', shared);
  scanner := TVulnScanner.Create(TSilentLogger.Create, db);
  sbom := MakeReportWithPackage('pkg:generic/dpm/a@1.0.0', 'a', '1.0.0');
  comp := sbom.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'pkg-2';
  comp.Id := 'b';
  comp.Version := '2.0.0';
  comp.Purl := 'pkg:generic/dpm/b@2.0.0';
  try
    ct := TCancellationTokenSourceFactory.Create.Token;
    vuln := scanner.Scan(ct, sbom);
    try
      Assert.AreEqual(1, vuln.Vulnerabilities.Count, 'shared CVE should not be duplicated');
      Assert.AreEqual(2, vuln.Vulnerabilities[0].Affects.Count, 'both components should be in affects[]');
      Assert.AreEqual(2, vuln.ComponentsAffected);
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TVulnScannerTests.MaxSeverityIsHighestFound;
var
  scanner : IVulnScanner;
  db : TFakeDatabase;
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  ct : ICancellationToken;
  advisory : IVulnDatabaseAdvisory;
begin
  db := TFakeDatabase.Create;
  advisory := TFakeAdvisory.Create('LOW-1', TSeverity.Low);
  db.AddVuln('pkg:generic/dpm/low@1.0.0', advisory);
  advisory := TFakeAdvisory.Create('CRIT-1', TSeverity.Critical);
  db.AddVuln('pkg:generic/dpm/crit@1.0.0', advisory);
  advisory := TFakeAdvisory.Create('MED-1', TSeverity.Medium);
  db.AddVuln('pkg:generic/dpm/medium@1.0.0', advisory);
  scanner := TVulnScanner.Create(TSilentLogger.Create, db);
  sbom := MakeReportWithPackage('pkg:generic/dpm/low@1.0.0', 'low', '1.0.0');
  comp := sbom.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'pkg-2';
  comp.Id := 'crit';
  comp.Version := '1.0.0';
  comp.Purl := 'pkg:generic/dpm/crit@1.0.0';
  comp := sbom.AddComponent(TSBOMComponentKind.DpmPackage);
  comp.BomRef := 'pkg-3';
  comp.Id := 'medium';
  comp.Version := '1.0.0';
  comp.Purl := 'pkg:generic/dpm/medium@1.0.0';
  try
    ct := TCancellationTokenSourceFactory.Create.Token;
    vuln := scanner.Scan(ct, sbom);
    try
      Assert.AreEqual(Ord(TSeverity.Critical), Ord(vuln.MaxSeverity));
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

procedure TVulnScannerTests.ComponentsWithoutPurlAreSkipped;
var
  scanner : IVulnScanner;
  db : TFakeDatabase;
  sbom : TSBOMReport;
  comp : TSBOMComponent;
  vuln : TVulnReport;
  ct : ICancellationToken;
begin
  //An unidentified component (no purl) should be silently skipped - no query, no result.
  db := TFakeDatabase.Create;
  scanner := TVulnScanner.Create(TSilentLogger.Create, db);
  sbom := TSBOMReport.Create;
  comp := sbom.AddComponent(TSBOMComponentKind.Unidentified);
  comp.BomRef := 'unid-1';
  comp.Id := 'unknown.unit';
  comp.Version := '';
  comp.Purl := '';
  try
    ct := TCancellationTokenSourceFactory.Create.Token;
    vuln := scanner.Scan(ct, sbom);
    try
      Assert.AreEqual(0, vuln.ComponentsScanned);
      Assert.AreEqual(0, vuln.Vulnerabilities.Count);
    finally
      vuln.Free;
    end;
  finally
    sbom.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVulnScannerTests);

end.
