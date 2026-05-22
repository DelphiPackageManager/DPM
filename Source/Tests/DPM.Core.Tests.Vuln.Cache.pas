unit DPM.Core.Tests.Vuln.Cache;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TVulnCacheTests = class
  private
    FCacheDir : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure PutThenGetReturnsCachedJson;
    [Test]
    procedure MissReturnsFalse;
    [Test]
    procedure StaleEntryIsTreatedAsMiss;
    [Test]
    procedure BypassReadsForcesMiss;
    [Test]
    procedure BypassReadsStillWrites;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.DateUtils,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Utils.DateTime,
  DPM.Core.Logging,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Cache;

type
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
    procedure NewLine;
    procedure Clear;
    function GetUseColor : boolean;
    procedure SetUseColor(const value : boolean);
  end;

function TSilentLogger.GetVerbosity : TVerbosity;        begin result := TVerbosity.Quiet; end;
procedure TSilentLogger.SetVerbosity(const value : TVerbosity); begin end;
procedure TSilentLogger.Clear;
begin

end;

procedure TSilentLogger.Debug(const data : string);       begin end;
procedure TSilentLogger.Verbose(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Information(const data : string; const important : Boolean = False); begin end;
procedure TSilentLogger.Success(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Warning(const data : string; const important : Boolean = False);     begin end;
procedure TSilentLogger.Error(const data : string); begin end;
procedure TSilentLogger.NewLine; begin end;
function TSilentLogger.GetUseColor : boolean; begin result := false; end;
procedure TSilentLogger.SetUseColor(const value : boolean); begin end;

procedure TVulnCacheTests.Setup;
begin
  //Per-test cache dir under TEMP so concurrent runs don't collide and so
  //we never touch the user's real cache.
  FCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) +
               'dpm-vuln-test-' + IntToHex(Random(MaxInt), 8);
  if DirectoryExists(FCacheDir) then
    TDirectory.Delete(FCacheDir, true);
end;

procedure TVulnCacheTests.TearDown;
begin
  if DirectoryExists(FCacheDir) then
    TDirectory.Delete(FCacheDir, true);
end;

procedure TVulnCacheTests.PutThenGetReturnsCachedJson;
var
  cache : IVulnResponseCache;
  retrieved : string;
  payload : string;
begin
  cache := TVulnResponseCache.Create(TSilentLogger.Create, FCacheDir, 24);
  payload := '{"vulnIds":["GHSA-1","GHSA-2"]}';
  cache.Put('pkg:generic/dpm/foo@1.0.0', 'osv', payload);
  Assert.IsTrue(cache.TryGet('pkg:generic/dpm/foo@1.0.0', 'osv', retrieved));
  //The cache may pretty-format the JSON - we only assert the keys survived.
  Assert.IsTrue(Pos('vulnIds', retrieved) > 0);
  Assert.IsTrue(Pos('GHSA-1', retrieved) > 0);
  Assert.IsTrue(Pos('GHSA-2', retrieved) > 0);
end;

procedure TVulnCacheTests.MissReturnsFalse;
var
  cache : IVulnResponseCache;
  retrieved : string;
begin
  cache := TVulnResponseCache.Create(TSilentLogger.Create, FCacheDir, 24);
  Assert.IsFalse(cache.TryGet('pkg:generic/dpm/never-cached@1.0.0', 'osv', retrieved));
  Assert.AreEqual('', retrieved);
end;

procedure TVulnCacheTests.StaleEntryIsTreatedAsMiss;
var
  cache : IVulnResponseCache;
  retrieved : string;
  cacheFiles : TArray<string>;
  fileName : string;
  entry : TJsonObject;
  raw : string;
begin
  cache := TVulnResponseCache.Create(TSilentLogger.Create, FCacheDir, 24);
  cache.Put('pkg:generic/dpm/aged@1.0.0', 'osv', '{"vulnIds":[]}');

  //Tamper with the on-disk cachedAt to be 48h old.
  cacheFiles := TDirectory.GetFiles(FCacheDir, '*.json', TSearchOption.soAllDirectories);
  Assert.AreEqual<integer>(1, Length(cacheFiles));
  fileName := cacheFiles[0];
  raw := TFile.ReadAllText(fileName, TEncoding.UTF8);
  entry := TJsonBaseObject.Parse(raw) as TJsonObject;
  try
    entry.S['cachedAt'] := TDPMDateTimeUtils.DateToISO8601(IncHour(TTimeZone.Local.ToUniversalTime(Now), -48), true);
    TFile.WriteAllText(fileName, entry.ToJSON(false), TEncoding.UTF8);
  finally
    entry.Free;
  end;

  Assert.IsFalse(cache.TryGet('pkg:generic/dpm/aged@1.0.0', 'osv', retrieved),
                 'expected stale entry to be treated as a miss');
end;

procedure TVulnCacheTests.BypassReadsForcesMiss;
var
  cache : IVulnResponseCache;
  retrieved : string;
begin
  cache := TVulnResponseCache.Create(TSilentLogger.Create, FCacheDir, 24);
  cache.Put('pkg:generic/dpm/skip@1.0.0', 'osv', '{"vulnIds":[]}');
  cache.BypassReads := true;
  Assert.IsFalse(cache.TryGet('pkg:generic/dpm/skip@1.0.0', 'osv', retrieved));
  //Toggle back off and confirm the value is still there.
  cache.BypassReads := false;
  Assert.IsTrue(cache.TryGet('pkg:generic/dpm/skip@1.0.0', 'osv', retrieved));
end;

procedure TVulnCacheTests.BypassReadsStillWrites;
var
  cache : IVulnResponseCache;
  retrieved : string;
begin
  cache := TVulnResponseCache.Create(TSilentLogger.Create, FCacheDir, 24);
  cache.BypassReads := true;
  //Even with reads bypassed, Put should still persist - so that the NEXT run
  //(without -no-cache) finds fresh data instead of having to re-query.
  cache.Put('pkg:generic/dpm/writeOnly@1.0.0', 'osv', '{"vulnIds":["GHSA-X"]}');
  cache.BypassReads := false;
  Assert.IsTrue(cache.TryGet('pkg:generic/dpm/writeOnly@1.0.0', 'osv', retrieved));
  Assert.IsTrue(Pos('GHSA-X', retrieved) > 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TVulnCacheTests);

end.
