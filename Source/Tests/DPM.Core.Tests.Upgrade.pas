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
    [Test] procedure Error_WhenInstallerUrlIsNotOnGithub;
    [Test] procedure Error_WhenInstallerNameIsNotSafe;
    [Test] procedure Digest_IsTakenFromTheAsset;
    [Test] procedure NoDigest_StillOffersTheUpgrade;
  end;

  // What dpm will download and run. The release list and update-check.json are both just data, so the
  // name, url and digest rules are what keep a path, another host or plain http out of the download.
  [TestFixture]
  TUpgradeAssetRulesTests = class
  public
    [Test]
    [TestCase('release', 'DPMSetup-0.9.326-beta.exe,true')]
    [TestCase('build metadata', 'DPMSetup-1.0.0+build.7.exe,true')]
    [TestCase('case', 'dpmsetup-1.0.0.EXE,true')]
    [TestCase('backslash', 'DPMSetup-1.0\..\..\evil.exe,false')]
    [TestCase('slash', 'DPMSetup-1.0/../evil.exe,false')]
    [TestCase('drive', 'DPMSetup-C:evil.exe,false')]
    [TestCase('space', 'DPMSetup-1.0 (x64).exe,false')]
    [TestCase('no version', 'DPMSetup-.exe,false')]
    [TestCase('stream', 'DPMSetup-1.0.exe:evil.exe,false')]
    [TestCase('other extension', 'DPMSetup-1.0.0.bat,false')]
    [TestCase('other prefix', 'Setup-1.0.0.exe,false')]
    procedure IsInstallerName(const assetName : string; const expected : boolean);

    [Test]
    [TestCase('github', 'https://github.com/DelphiPackageManager/DPM/releases/download/v0.9.326-beta/DPMSetup-0.9.326-beta.exe,true')]
    [TestCase('objects', 'https://objects.githubusercontent.com/github-production-release-asset-2e65be/123,true')]
    [TestCase('release-assets', 'https://release-assets.githubusercontent.com/github-production-release-asset/123,true')]
    [TestCase('case', 'HTTPS://GitHub.com/DelphiPackageManager/DPM/releases/download/v1/DPMSetup-1.exe,true')]
    [TestCase('http', 'http://github.com/DelphiPackageManager/DPM/releases/download/v1/DPMSetup-1.exe,false')]
    //on its own, the port check would refuse plain http on 80.
    [TestCase('http on 443', 'http://github.com:443/DelphiPackageManager/DPM/releases/download/v1/DPMSetup-1.exe,false')]
    [TestCase('lookalike host', 'https://github.com.evil.test/DPMSetup-1.exe,false')]
    [TestCase('github in the path', 'https://evil.test/github.com/DPMSetup-1.exe,false')]
    [TestCase('github as user info', 'https://github.com@evil.test/DPMSetup-1.exe,false')]
    [TestCase('credentials', 'https://user:pw@github.com/DPMSetup-1.exe,false')]
    [TestCase('port', 'https://github.com:8443/DPMSetup-1.exe,false')]
    [TestCase('file url', 'file:///c:/temp/DPMSetup-1.exe,false')]
    [TestCase('local path', 'c:\temp\DPMSetup-1.exe,false')]
    procedure TryParseDownloadUrl(const url : string; const expected : boolean);
    [Test] procedure TryParseDownloadUrl_Empty;

    [Test] procedure TryParseDigest_Sha256_IsLowercaseHex;
    [Test]
    [TestCase('other algorithm', 'sha512:1f819ea6413a3a8548091cd2613b2d1d769ed63923081ad4ae1b670fa6e5aaf4')]
    [TestCase('short', 'sha256:1f819ea6')]
    [TestCase('not hex', 'sha256:zz819ea6413a3a8548091cd2613b2d1d769ed63923081ad4ae1b670fa6e5aaf4')]
    [TestCase('bare hex', '1f819ea6413a3a8548091cd2613b2d1d769ed63923081ad4ae1b670fa6e5aaf4')]
    procedure TryParseDigest_Rejects(const digest : string);
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
    // the ttl is an hour - HoursBetween truncating made it nearly two.
    [Test]
    [TestCase('59 minutes', '59,true')]
    [TestCase('61 minutes', '61,false')]
    [TestCase('119 minutes', '119,false')]
    procedure TryGet_HonoursTheTtlToTheMinute(const ageMinutes : integer; const expectHit : boolean);
    [Test] procedure TryGet_Misses_WhenFileCorrupt;
    [Test] procedure TryGet_Misses_WhenCachedUrlIsNotOnGithub;
    [Test] procedure TryGet_Misses_WhenCachedAssetNameIsNotSafe;
    [Test] procedure Put_DoesNotCacheErrors;
    [Test] procedure Clear_RemovesEntry;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  VSoft.YAML,
  VSoft.Uri,
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

const
  cDigest = 'sha256:1F819EA6413A3A8548091CD2613B2D1D769ED63923081AD4AE1B670FA6E5AAF4';
  cDigestHex = '1f819ea6413a3a8548091cd2613b2d1d769ed63923081ad4ae1b670fa6e5aaf4';

function DownloadUrl(const tag : string) : string;
begin
  //the shape of a real browser_download_url.
  result := 'https://github.com/DelphiPackageManager/DPM/releases/download/' + tag + '/DPMSetup-' + tag + '.exe';
end;

// One release entry carrying assetsJson.
function ReleaseWithAssets(const tag : string; const assetsJson : string; const draft : boolean = false) : string;
begin
  result := '{"tag_name":"' + tag + '",' +
            '"draft":' + LowerCase(BoolToStr(draft, true)) + ',' +
            //deliberately the OPPOSITE of the semver label, to prove the github
            //flag is ignored and only the version decides.
            '"prerelease":false,' +
            '"html_url":"https://github.com/DelphiPackageManager/DPM/releases/tag/' + tag + '",' +
            '"assets":' + assetsJson + '}';
end;

// Builds one release entry. asset=true attaches a DPMSetup-*.exe.
function Release(const tag : string; const draft : boolean = false; const asset : boolean = true) : string;
var
  assets : string;
begin
  if asset then
    assets := '[{"name":"DPMSetup-' + tag + '.exe","browser_download_url":"' + DownloadUrl(tag) + '","digest":"' + cDigest + '"}]'
  else
    assets := '[]';
  result := ReleaseWithAssets(tag, assets, draft);
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
    //the caller reports it - an error for dpm upgrade, a warning for the IDE's background check.
    Assert.Contains(svc.LastError, 'v2.0.0');
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Error_WhenInstallerUrlIsNotOnGithub;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + ReleaseWithAssets('v2.0.0',
                                  '[{"name":"DPMSetup-2.0.0.exe","browser_download_url":"https://evil.test/DPMSetup-2.0.0.exe"}]') + ']',
                          '0.9.0', true, info);
    //the release check is where an installer from anywhere else stops - nothing is offered.
    Assert.AreEqual(Ord(TUpgradeCheckResult.Error), Ord(res));
    Assert.IsNull(info);
    Assert.Contains(svc.LastError, 'not one dpm will download');
    Assert.Contains(svc.LastError, 'https://evil.test/DPMSetup-2.0.0.exe');
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Error_WhenInstallerNameIsNotSafe;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    //the asset name becomes the file name in temp.
    res := svc.SelectFrom('[' + ReleaseWithAssets('v2.0.0',
                                  '[{"name":"DPMSetup-2.0\\..\\..\\evil.exe","browser_download_url":"' + DownloadUrl('v2.0.0') + '"}]') + ']',
                          '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.Error), Ord(res));
    Assert.IsNull(info);
    Assert.Contains(svc.LastError, 'not one dpm will download');
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.Digest_IsTakenFromTheAsset;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    res := svc.SelectFrom('[' + Release('v1.0.0') + ']', '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual(cDigestHex, info.Digest, false);
  finally
    svc.Free;
  end;
end;

procedure TUpgradeChannelTests.NoDigest_StillOffersTheUpgrade;
var
  svc : TTestableUpgradeService;
  info : IUpgradeInfo;
  res : TUpgradeCheckResult;
begin
  svc := MakeService;
  try
    //assets uploaded before github recorded digests have "digest": null - the signature check still applies.
    res := svc.SelectFrom('[' + ReleaseWithAssets('v1.0.0',
                                  '[{"name":"DPMSetup-1.0.0.exe","browser_download_url":"' + DownloadUrl('v1.0.0') + '","digest":null}]') + ']',
                          '0.9.0', true, info);
    Assert.AreEqual(Ord(TUpgradeCheckResult.UpgradeAvailable), Ord(res));
    Assert.AreEqual('', info.Digest);
  finally
    svc.Free;
  end;
end;

{ TUpgradeAssetRulesTests }

procedure TUpgradeAssetRulesTests.IsInstallerName(const assetName : string; const expected : boolean);
begin
  Assert.AreEqual(expected, TUpgradeAssetRules.IsInstallerName(assetName), assetName);
end;

procedure TUpgradeAssetRulesTests.TryParseDownloadUrl(const url : string; const expected : boolean);
var
  uri : IUri;
  reason : string;
  parsed : boolean;
begin
  //called first - the message argument would otherwise be built before reason is set.
  parsed := TUpgradeAssetRules.TryParseDownloadUrl(url, uri, reason);
  Assert.AreEqual(expected, parsed, url + ' : ' + reason);
  if expected then
  begin
    Assert.IsNotNull(uri);
    Assert.AreEqual('', reason);
  end
  else
  begin
    Assert.IsNull(uri, 'a refused url must not hand back something to download');
    Assert.AreNotEqual('', reason);
  end;
end;

procedure TUpgradeAssetRulesTests.TryParseDownloadUrl_Empty;
var
  uri : IUri;
  reason : string;
begin
  Assert.IsFalse(TUpgradeAssetRules.TryParseDownloadUrl('', uri, reason));
  Assert.IsNull(uri);
end;

procedure TUpgradeAssetRulesTests.TryParseDigest_Sha256_IsLowercaseHex;
var
  hex : string;
begin
  Assert.IsTrue(TUpgradeAssetRules.TryParseDigest(cDigest, hex));
  Assert.AreEqual(cDigestHex, hex, false);
end;

procedure TUpgradeAssetRulesTests.TryParseDigest_Rejects(const digest : string);
var
  hex : string;
begin
  Assert.IsFalse(TUpgradeAssetRules.TryParseDigest(digest, hex));
  Assert.AreEqual('', hex);
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
                                  DownloadUrl('v1.2.0-beta'), 'https://github.com/DelphiPackageManager/DPM/releases/tag/v1.2.0-beta',
                                  true, cDigestHex);
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
    Assert.AreEqual(DownloadUrl('v1.2.0-beta'), info.DownloadUrl);
    Assert.AreEqual('https://github.com/DelphiPackageManager/DPM/releases/tag/v1.2.0-beta', info.ReleaseUrl);
    Assert.IsTrue(info.IsPrerelease);
    //the download is checked against it, so it has to survive the cache.
    Assert.AreEqual(cDigestHex, info.Digest, false);
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

//rewrites one field of the cached entry - what anything with access to %APPDATA% could do.
procedure RewriteCachedField(const fileName, key, value : string);
var
  doc : IYAMLDocument;
begin
  doc := TYAML.LoadFromString(TFile.ReadAllText(fileName));
  doc.AsMapping.S[key] := value;
  TFile.WriteAllText(fileName, TYAML.WriteToJSONString(doc), TEncoding.UTF8);
end;

procedure TUpgradeCheckCacheTests.TryGet_HonoursTheTtlToTheMinute(const ageMinutes : integer; const expectHit : boolean);
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
    RewriteCachedField(fileName, 'cachedAt',
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', IncMinute(TTimeZone.Local.ToUniversalTime(Now), -ageMinutes)));

    Assert.AreEqual(expectHit, cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info),
                    'an entry ' + IntToStr(ageMinutes) + ' minutes old with a 1 hour ttl');
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenCachedUrlIsNotOnGithub;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
  stored : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    //a local - passed straight into Put's const parameter it would never be released.
    stored := TUpgradeInfo.Create(TPackageVersion.Parse('1.2.0'), 'v1.2.0', 'DPMSetup-1.2.0.exe', DownloadUrl('v1.2.0'), '', false, cDigestHex);
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpgradeAvailable, stored);
    //sanity : the untouched entry is served.
    Assert.IsTrue(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));

    RewriteCachedField(fileName, 'downloadUrl', 'http://evil.test/DPMSetup-1.2.0.exe');
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info), 'an entry pointing anywhere else must be re-checked');
    Assert.IsNull(info);
  finally
    TFile.Delete(fileName);
  end;
end;

procedure TUpgradeCheckCacheTests.TryGet_Misses_WhenCachedAssetNameIsNotSafe;
var
  fileName : string;
  cache : IUpgradeCheckCache;
  res : TUpgradeCheckResult;
  info : IUpgradeInfo;
  stored : IUpgradeInfo;
begin
  fileName := TempFile;
  try
    stored := TUpgradeInfo.Create(TPackageVersion.Parse('1.2.0'), 'v1.2.0', 'DPMSetup-1.2.0.exe', DownloadUrl('v1.2.0'), '', false, '');
    cache := TUpgradeCheckCache.Create(nil, fileName, 1);
    cache.Put(TPackageVersion.Parse('1.0.0'), true, TUpgradeCheckResult.UpgradeAvailable, stored);
    Assert.IsTrue(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));

    //the asset name is the file name the installer is saved as.
    RewriteCachedField(fileName, 'assetName', 'DPMSetup-1.2\..\..\..\Startup\evil.exe');
    Assert.IsFalse(cache.TryGet(TPackageVersion.Parse('1.0.0'), true, res, info));
    Assert.IsNull(info);
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
  TDUnitX.RegisterTestFixture(TUpgradeAssetRulesTests);
  TDUnitX.RegisterTestFixture(TUpgradeCheckCacheTests);

end.
