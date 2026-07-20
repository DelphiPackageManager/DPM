unit DPM.Core.Tests.Upgrade;

// Update check : channel selection rules and the IDE's 1 hour result cache.
//
// The selection tests go through TGithubUpgradeService.SelectBestRelease with a
// literal releases array, so they exercise the real rules with no network.

interface

uses
  DUnitX.TestFramework;

type
  // Guards the channel semantics. The one that is easiest to break by accident
  // is Beta_PicksNewerStable_OverOlderBeta : includePrerelease is a SUPERSET
  // switch (it stops prereleases being excluded), NOT a "betas only" switch.
  [TestFixture]
  TUpgradeChannelTests = class
  public
    [Test] procedure Stable_IgnoresPrereleases;
    [Test] procedure Stable_PicksNewestStable;
    [Test] procedure Beta_PicksNewerBeta_OverOlderStable;
    [Test] procedure Beta_PicksNewerStable_OverOlderBeta;
    [Test] procedure Beta_PrefersStable_OverItsOwnPrerelease;
    [Test] procedure UpToDate_WhenNothingNewer;
    [Test] procedure SkipsDrafts;
    [Test] procedure SkipsUnparsableTags;
    [Test] procedure ToleratesTagWithoutVPrefix;
    [Test] procedure Error_WhenNewestReleaseHasNoInstallerAsset;
  end;

  [TestFixture]
  TUpgradeCheckCacheTests = class
  private
    function TempFile : string;
  public
    [Test] procedure TryGet_ReturnsFalse_WhenNoFile;
    [Test] procedure Put_ThenTryGet_RoundTripsUpgradeAvailable;
    [Test] procedure Put_ThenTryGet_RoundTripsUpToDate;
    [Test] procedure TryGet_Misses_WhenChannelDiffers;
    [Test] procedure TryGet_Misses_WhenCurrentVersionDiffers;
    [Test] procedure TryGet_Misses_WhenExpired;
    [Test] procedure TryGet_Misses_WhenFileCorrupt;
    [Test] procedure Put_DoesNotCacheErrors;
    [Test] procedure Clear_RemovesEntry;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  VSoft.YAML,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Upgrade.Interfaces,
  DPM.Core.Upgrade.Cache,
  DPM.Core.Upgrade.Github;

type
  // SelectBestRelease is protected - a descendant is the least invasive way to
  // reach it without widening the public surface just for tests.
  TTestableUpgradeService = class(TGithubUpgradeService)
  public
    function SelectFrom(const json : string; const currentVersion : string; const includePrerelease : boolean;
                        out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;
  end;

function TTestableUpgradeService.SelectFrom(const json, currentVersion : string; const includePrerelease : boolean;
                                            out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;
var
  doc : IYAMLDocument;
  version : TPackageVersion;
begin
  //YAML is a superset of json, which is why the service can parse the github
  //response with the yaml parser - same trick here.
  doc := TYAML.LoadFromString(json);
  version := TPackageVersion.Parse(currentVersion);
  result := SelectBestRelease(doc.AsSequence, version, includePrerelease, upgradeInfo);
end;

function MakeService : TTestableUpgradeService;
begin
  //SelectBestRelease logs (unparsable tags, missing asset), so it needs a real
  //ILogger - a nil interface would AV on the first skipped release.
  result := TTestableUpgradeService.Create(TTestLogger.Create);
end;

// Builds one release entry. asset=true attaches a DPMSetup-*.exe.
function Release(const tag : string; const draft : boolean = false; const asset : boolean = true) : string;
var
  assets : string;
begin
  if asset then
    assets := '[{"name":"DPMSetup-' + tag + '.exe","browser_download_url":"https://example.test/' + tag + '.exe"}]'
  else
    assets := '[]';

  result := '{"tag_name":"' + tag + '",' +
            '"draft":' + LowerCase(BoolToStr(draft, true)) + ',' +
            //deliberately the OPPOSITE of the semver label, to prove the github
            //flag is ignored and only the version decides.
            '"prerelease":false,' +
            '"html_url":"https://example.test/releases/' + tag + '",' +
            '"assets":' + assets + '}';
end;

{ TUpgradeChannelTests }

procedure TUpgradeChannelTests.Stable_IgnoresPrereleases;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v0.9.281-beta') + ',' + Release('v0.9.280-beta') + ']',
                          '0.9.243-beta', false, info);
    //Every release is a prerelease, so a stable channel user has nothing to go to.
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpToDate), Ord(res));
    Assert.IsNull(info);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Stable_PicksNewestStable;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v1.0.0') + ',' + Release('v1.1.0') + ',' + Release('v1.0.5') + ']',
                          '0.9.243-beta', false, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.1.0', info.Version.ToStringNoMeta);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Beta_PicksNewerBeta_OverOlderStable;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v1.0.0') + ',' + Release('v1.1.0-beta') + ']',
                          '0.9.243-beta', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.1.0-beta', info.Version.ToStringNoMeta);
    Assert.IsTrue(info.IsPrerelease);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Beta_PicksNewerStable_OverOlderBeta;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    //THE regression guard : a beta channel user must still be offered a newer
    //stable release. Beta is a superset of stable, not a separate track.
    res := svc.SelectFrom('[' + Release('v0.9.281-beta') + ',' + Release('v1.0.0') + ']',
                          '0.9.243-beta', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.0.0', info.Version.ToStringNoMeta);
    Assert.IsFalse(info.IsPrerelease);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Beta_PrefersStable_OverItsOwnPrerelease;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    //semver : 1.0.0 > 1.0.0-beta.1
    res := svc.SelectFrom('[' + Release('v1.0.0-beta.1') + ',' + Release('v1.0.0') + ']',
                          '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.0.0', info.Version.ToStringNoMeta);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.UpToDate_WhenNothingNewer;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v1.0.0') + ']', '1.0.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpToDate), Ord(res));
    Assert.IsNull(info);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.SkipsDrafts;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v2.0.0', true) + ',' + Release('v1.0.0') + ']', '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.0.0', info.Version.ToStringNoMeta, 'the draft 2.0.0 should have been skipped');
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.SkipsUnparsableTags;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('nightly') + ',' + Release('v1.0.0') + ']', '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.0.0', info.Version.ToStringNoMeta);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.ToleratesTagWithoutVPrefix;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('1.2.3') + ']', '1.0.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('1.2.3', info.Version.ToStringNoMeta);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Error_WhenNewestReleaseHasNoInstallerAsset;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    //A source-only release. We do NOT silently fall back to an older release -
    //reporting an upgrade the user cannot install would be worse.
    res := svc.SelectFrom('[' + Release('v2.0.0', false, false) + ',' + Release('v1.0.0') + ']', '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.Error), Ord(res));
    Assert.IsNull(info);
  finally
    svc.Free;
  end;
end;

{ TUpgradeCheckCacheTests }

function TUpgradeCheckCacheTests.TempFile : string;
begin
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-update-check-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.json');
end;

procedure TUpgradeCheckCacheTests.TryGet_ReturnsFalse_WhenNoFile;
var
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  cache := TUpgradeCheckCache.Create(nil, TempFile, 1);
  Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
end;

procedure TUpgradeCheckCacheTests.Put_ThenTryGet_RoundTripsUpgradeAvailable;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
  stored : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    stored := TUpgradeInfo.Create(TPackageVersion.Parse('1.2.0-beta'), 'v1.2.0-beta', 'DPMSetup-1.2.0-beta.exe',
                                  'https://example.test/dl.exe', 'https://example.test/rel', true);
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpgradeAvailable, stored);

    //new instance - proves it round trips through the file, not memory.
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    Assert.IsTrue(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.IsNotNull(info);
    Assert.AreEqual('1.2.0-beta', info.Version.ToStringNoMeta);
    Assert.AreEqual('v1.2.0-beta', info.TagName);
    Assert.AreEqual('DPMSetup-1.2.0-beta.exe', info.AssetName);
    Assert.AreEqual('https://example.test/dl.exe', info.DownloadUrl);
    Assert.AreEqual('https://example.test/rel', info.ReleaseUrl);
    Assert.IsTrue(info.IsPrerelease);
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.Put_ThenTryGet_RoundTripsUpToDate;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    //The negative result must cache too, otherwise an up to date IDE re-checks
    //on every view open - which is the common case.
    cache.Put(TPackageVersion.Parse('1.0.0'), false, TUpgradeCheckResult.UpToDate, nil);

    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    Assert.IsTrue(cache.TryGet(TPackageVersion.Parse('1.0.0'), false, res, info));
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpToDate), Ord(res));
    Assert.IsNull(info);
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenChannelDiffers;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), false, TUpgradeCheckResult.UpToDate, nil);
    //switching Stable -> Beta must not serve the stable answer.
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenCurrentVersionDiffers;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpToDate, nil);
    //having upgraded since, the old answer is meaningless.
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.1.0'), true, res, info));
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenExpired;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
  doc : IYAMLDocument;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpToDate, nil);

    //age the entry by rewriting cachedAt two hours back.
    doc := TYAML.LoadFromString(TFile.ReadAllText(fileName));
    doc.AsMapping.S['cachedAt'] := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', IncHour(TTimeZone.Local.ToUniversalTime(Now), -2));
    TFile.WriteAllText(fileName, TYAML.WriteToJSONString(doc), TEncoding.UTF8);

    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenFileCorrupt;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    //a half written file must be a cache miss, never an exception.
    TFile.WriteAllText(fileName, '{"cachedAt": "not-a-date", "chan', TEncoding.UTF8);
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.Put_DoesNotCacheErrors;
var
  fileName : string;
  cache : IUpgradeCheckCache;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    //An offline machine must retry next time, not go quiet for an hour.
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.Error, nil);
    Assert.IsFalse(TFile.Exists(fileName));
  finally
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.Clear_RemovesEntry;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpToDate, nil);
    Assert.IsTrue(TFile.Exists(fileName));
    cache.Clear;
    Assert.IsFalse(TFile.Exists(fileName));
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
  finally
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUpgradeChannelTests);
  TDUnitX.RegisterTestFixture(TUpgradeCheckCacheTests);

end.
