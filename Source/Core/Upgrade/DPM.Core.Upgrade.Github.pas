{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright (c) 2019 Vincent Parrett and contributors             }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Upgrade.Github;

interface

uses
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Upgrade.Interfaces;

type
  //TUpgradeInfo now lives in DPM.Core.Upgrade.Interfaces so the check cache can
  //rehydrate one without depending on this unit.
  TGithubUpgradeService = class(TInterfacedObject, IUpgradeService)
  private
    FLogger : ILogger;
    FLastError : string;
  protected
    function CheckForUpgrade(const cancellationToken : ICancellationToken; const currentVersion : TPackageVersion;
                             const includePrerelease : boolean; out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;
    function DownloadUpgrade(const cancellationToken : ICancellationToken; const upgradeInfo : IUpgradeInfo;
                             out installerFile : string) : boolean;
    function LaunchInstaller(const installerFile : string) : boolean;
    function LastError : string;

    /// <summary>
    ///  Fetches and parses the releases list. Returns nil (with LastError set) on any
    ///  transport or parse failure - callers cannot distinguish "no releases"
    ///  from "failed", which is why an empty repo also yields an error.
    /// </summary>
    function FetchReleases(const cancellationToken : ICancellationToken) : IYAMLSequence;

    //TPEUtils.VerifyAuthenticode (revocation checked), logging E0306 when the installer fails it.
    function VerifyInstallerSignature(const fileName : string; const displayName : string) : boolean;

    /// <summary>
    ///  Picks the newest usable release from an already parsed releases list.
    ///  Split out from CheckForUpgrade purely so the channel/version selection
    ///  rules can be tested without hitting the network - build the sequence
    ///  with TYAML.LoadFromString over a literal json array.
    /// </summary>
    /// <param name="includePrerelease">
    ///  False (the Stable channel) skips any release whose version carries a
    ///  prerelease label. True (Beta) is a SUPERSET - it excludes nothing, so a
    ///  newer stable release is still selected for a beta user. Semver ordering
    ///  does the rest : 1.0.0 > 1.0.0-beta.1 and 1.0.0 > 0.9.290-beta.
    /// </param>
    function SelectBestRelease(const releases : IYAMLSequence; const currentVersion : TPackageVersion;
                               const includePrerelease : boolean; out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;

    /// <summary>
    ///  Finds the DPMSetup-*.exe asset on a release. Returns false if the
    ///  release has no installer attached (eg a source-only release), or the
    ///  one it has is not one dpm will download (TUpgradeAssetRules) - problem
    ///  then says why.
    /// </summary>
    function FindInstallerAsset(const release : IYAMLMapping; out assetName : string; out downloadUrl : string;
                                out digest : string; out problem : string) : boolean;

    property Logger : ILogger read FLogger;
  public
    constructor Create(const logger : ILogger); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Diagnostics,
  VSoft.Uri,
  VSoft.HttpClient,
  DPM.Core.Constants,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.Hash,
  DPM.Core.Utils.PE,
  DPM.Core.Utils.Process,
  DPM.Core.Utils.Strings,
  DPM.Core.Diagnostics;

const
  //Note : we use the releases list rather than /releases/latest, because
  //'latest' excludes prereleases entirely - we need to do our own filtering.
  //100 is the github api maximum page size, and is far more than enough to find
  //the newest release.
  cReleasesPageSize = 100;

  //VSoft.HttpClient waits at most ResponseTimeout for the whole response, body included - its 60s default
  //failed the ~20MB installer on any connection slower than about 350KB/s. The same 5 minutes as a package
  //download; cancelling stops it sooner.
  cInstallerDownloadTimeoutMs = 5 * 60 * 1000;

{ TGithubUpgradeService }

constructor TGithubUpgradeService.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

function TGithubUpgradeService.LastError : string;
begin
  result := FLastError;
end;

function TGithubUpgradeService.FetchReleases(const cancellationToken : ICancellationToken) : IYAMLSequence;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  path : string;
  doc : IYAMLDocument;
begin
  result := nil;
  path := '/' + Format(cGithubReleases, [cDPMGithubRepo]);

  httpClient := THttpClientFactory.CreateClient(cGithubApiUrl);
  request := httpClient.CreateRequest(path)
                       .WithHeader(cUserAgentHeader, cDPMUserAgent)
                       .WithAccept(cGithubv3Accept)
                       .WithParameter('per_page', IntToStr(cReleasesPageSize));

  Logger.Verbose('GET ' + cGithubApiUrl + path);
  try
    response := httpClient.Get(request, cancellationToken);
  except
    on e : Exception do
    begin
      FLastError := 'Unable to contact github : ' + e.Message;
      exit;
    end;
  end;

  if cancellationToken.IsCancelled then
    exit;

  if response.StatusCode <> 200 then
  begin
    //403 here is almost always the unauthenticated rate limit, which is easy to
    //hit on a shared/office IP - call it out rather than just printing 403.
    if response.StatusCode = 403 then
      FLastError := 'Unable to get the DPM releases from github : rate limited by github (403). Please try again later.'
    else
      FLastError := 'Unable to get the DPM releases from github : ' + IntToStr(response.StatusCode) + ' ' + response.ErrorMessage;
    exit;
  end;

  //YAML is a superset of JSON, so the yaml parser handles the response directly
  //and gives us interfaces we don't have to free.
  try
    doc := TYAML.LoadFromString(response.Response);
  except
    on e : Exception do
    begin
      FLastError := 'Unable to read the response from github : ' + e.Message;
      exit;
    end;
  end;

  if not doc.IsSequence then
  begin
    FLastError := 'Unexpected response from github - expected a list of releases.';
    exit;
  end;

  result := doc.AsSequence;
end;

function TGithubUpgradeService.FindInstallerAsset(const release : IYAMLMapping; out assetName, downloadUrl, digest, problem : string) : boolean;
var
  assets : IYAMLSequence;
  asset : IYAMLMapping;
  i : integer;
  name : string;
  url : string;
  uri : IUri;
  urlProblem : string;
  publishedDigest : string;
begin
  result := false;
  assetName := '';
  downloadUrl := '';
  digest := '';
  problem := '';

  if not release.Contains('assets') then
    exit;

  assets := release.A['assets'];
  for i := 0 to assets.Count - 1 do
  begin
    asset := assets.O[i];
    name := asset.S['name'];
    //Match on prefix + extension rather than rebuilding the expected file name
    //from the version - that way a change in how the build renders the version
    //into the file name doesn't silently break upgrading.
    if TStringUtils.StartsWith(name, cDPMInstallerPrefix, true) and
       SameText(ExtractFileExt(name), cDPMInstallerExt) then
    begin
      url := asset.S['browser_download_url'];
      if url = '' then
        exit;
      //it looks like the installer - so anything wrong with it is a reason not to upgrade, not to look further.
      if not TUpgradeAssetRules.IsInstallerName(name) then
      begin
        problem := 'its name [' + name + '] is not ' + cDPMInstallerPrefix + '{version}' + cDPMInstallerExt;
        exit;
      end;
      if not TUpgradeAssetRules.TryParseDownloadUrl(url, uri, urlProblem) then
      begin
        problem := 'its download url [' + url + '] ' + urlProblem;
        exit;
      end;
      //assets uploaded before github recorded digests have none (json null) - the Authenticode check still applies.
      publishedDigest := asset.S['digest'];
      if (publishedDigest <> '') and not TUpgradeAssetRules.TryParseDigest(publishedDigest, digest) then
        Logger.Verbose('Ignoring the digest [' + publishedDigest + '] of ' + name + ' - it is not a sha256 digest');
      assetName := name;
      downloadUrl := url;
      exit(true);
    end;
  end;
end;

function TGithubUpgradeService.CheckForUpgrade(const cancellationToken : ICancellationToken; const currentVersion : TPackageVersion;
                                               const includePrerelease : boolean; out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;
var
  releases : IYAMLSequence;
begin
  upgradeInfo := nil;
  result := TUpgradeCheckResult.Error;
  FLastError := '';

  //A build that stamped an invalid cDPMSemVer would compare as Empty and report
  //every release as an upgrade - refuse rather than doing something surprising.
  if currentVersion.IsEmpty then
  begin
    FLastError := 'Unable to determine the current dpm version, so there is nothing to compare releases with.';
    exit;
  end;

  releases := FetchReleases(cancellationToken);
  if releases = nil then
    exit; //LastError says why

  result := SelectBestRelease(releases, currentVersion, includePrerelease, upgradeInfo);
end;

function TGithubUpgradeService.SelectBestRelease(const releases : IYAMLSequence; const currentVersion : TPackageVersion;
                                                 const includePrerelease : boolean; out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;
var
  release : IYAMLMapping;
  i : integer;
  tagName : string;
  versionString : string;
  version : TPackageVersion;
  isPrerelease : boolean;
  bestVersion : TPackageVersion;
  bestRelease : IYAMLMapping;
  bestTag : string;
  bestIsPrerelease : boolean;
  assetName : string;
  downloadUrl : string;
  digest : string;
  problem : string;
begin
  upgradeInfo := nil;
  result := TUpgradeCheckResult.Error;

  bestVersion := TPackageVersion.Empty;
  bestRelease := nil;
  bestTag := '';
  bestIsPrerelease := false;

  for i := 0 to releases.Count - 1 do
  begin
    release := releases.O[i];

    //drafts are not visible to users and have no usable assets.
    if release.B['draft'] then
      continue;

    tagName := release.S['tag_name'];
    if tagName = '' then
      continue;

    //tags are conventionally prefixed with 'v', but not always - tolerate both.
    versionString := tagName;
    if TStringUtils.StartsWith(versionString, 'v', true) then
      versionString := Copy(versionString, 2, Length(versionString) - 1);

    if not TPackageVersion.TryParse(versionString, version) then
    begin
      Logger.Verbose('Skipping release with unparsable tag [' + tagName + ']');
      continue;
    end;

    //The release VERSION decides whether this is a prerelease - github's own
    //'prerelease' flag is deliberately ignored. Every dpm release to date is
    //tagged -beta but published with that flag False, so honouring the flag
    //would make -pr meaningless.
    //Note this only ever EXCLUDES prereleases - includePrerelease is a superset
    //switch, so a beta channel user is still offered a newer stable release.
    isPrerelease := not version.IsStable;
    if isPrerelease and (not includePrerelease) then
      continue;

    //Don't trust the order github returns releases in - compare explicitly.
    if bestVersion.IsEmpty or (version > bestVersion) then
    begin
      bestVersion := version;
      bestRelease := release;
      bestTag := tagName;
      bestIsPrerelease := isPrerelease;
    end;
  end;

  if bestRelease = nil then
  begin
    //No usable release at all. Not an error - there is simply nothing newer we
    //are willing to consider.
    result := TUpgradeCheckResult.UpToDate;
    exit;
  end;

  if not (bestVersion > currentVersion) then
  begin
    result := TUpgradeCheckResult.UpToDate;
    exit;
  end;

  if not FindInstallerAsset(bestRelease, assetName, downloadUrl, digest, problem) then
  begin
    if problem <> '' then
      FLastError := 'The installer attached to the release [' + bestTag + '] is not one dpm will download - ' + problem + '.'
    else
      FLastError := 'The release [' + bestTag + '] does not have an installer (' + cDPMInstallerPrefix + '*' +
                    cDPMInstallerExt + ') attached, so it can not be installed.';
    exit;
  end;

  upgradeInfo := TUpgradeInfo.Create(bestVersion, bestTag, assetName, downloadUrl, bestRelease.S['html_url'], bestIsPrerelease, digest);
  result := TUpgradeCheckResult.UpgradeAvailable;
end;

function TGithubUpgradeService.DownloadUpgrade(const cancellationToken : ICancellationToken; const upgradeInfo : IUpgradeInfo;
                                               out installerFile : string) : boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  uri : IUri;
  problem : string;
  destFile : string;
  partialFile : string;
  hash : string;
  lastError : Cardinal;
  stopwatch : TStopwatch;
begin
  result := false;
  installerFile := '';

  //upgradeInfo may have come back from the IDE's update-check.json - check it again here, it decides what we
  //download and then run.
  if not TUpgradeAssetRules.IsInstallerName(upgradeInfo.AssetName) then
  begin
    Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'The installer name [' + upgradeInfo.AssetName + '] is not ' +
                 cDPMInstallerPrefix + '{version}' + cDPMInstallerExt + ', so it was not downloaded.');
    exit;
  end;
  if not TUpgradeAssetRules.TryParseDownloadUrl(upgradeInfo.DownloadUrl, uri, problem) then
  begin
    Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'The installer url [' + upgradeInfo.DownloadUrl + '] ' + problem +
                 ', so it was not downloaded.');
    exit;
  end;

  //Download into a dpm subfolder of temp rather than temp itself, so a failed
  //or cancelled download leaves something obviously ours behind.
  destFile := TPath.Combine(TPath.Combine(TPath.GetTempPath, 'dpm'), upgradeInfo.AssetName);
  try
    ForceDirectories(ExtractFilePath(destFile));
  except
    on e : Exception do
    begin
      Logger.Error(TErrorCode.E0005_FolderCreateFailed, 'Unable to create the download folder [' + ExtractFilePath(destFile) + '] : ' + e.Message);
      exit;
    end;
  end;

  //A leftover from a previous attempt would otherwise be launched as-is, and it
  //may well be a truncated file.
  if TFile.Exists(destFile) then
  begin
    try
      TFile.Delete(destFile);
    except
      on e : Exception do
      begin
        Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'Unable to overwrite [' + destFile + '] : ' + e.Message);
        exit;
      end;
    end;
  end;

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);
  //the request copies the client's timeouts when it is created.
  httpClient.ResponseTimeout := cInstallerDownloadTimeoutMs;
  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader, cDPMUserAgent);
  //browser_download_url redirects to objects.githubusercontent.com. The client
  //follows redirects by default, this just makes the requirement explicit.
  request.FollowRedirects := true;
  //The download goes to a file of its own, and only becomes the installer once it has arrived whole and
  //passed its checks - a failed, cancelled or tampered download leaves nothing behind that could be run.
  partialFile := TFileUtils.TempFileNameFor(destFile);
  request.SaveAsFile := partialFile;
  try
    Logger.Information('Downloading ' + upgradeInfo.AssetName + ' ...');
    stopwatch := TStopwatch.StartNew;
    try
      response := httpClient.Get(request, cancellationToken);
    except
      on e : Exception do
      begin
        Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'Unable to download the installer : ' + e.Message);
        exit;
      end;
    end;

    if cancellationToken.IsCancelled then
      exit;

    if response.StatusCode <> 200 then
    begin
      Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'Unable to download the installer : ' + IntToStr(response.StatusCode) + ' ' + response.ErrorMessage);
      exit;
    end;
    stopwatch.Stop;
    //the response holds the file it saved to open.
    response := nil;
    request := nil;

    if not TFile.Exists(partialFile) then
    begin
      Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'The installer download reported success but no file was written to [' + partialFile + '].');
      exit;
    end;

    if upgradeInfo.Digest <> '' then
    begin
      hash := THashSHA256.GetHashStringFromFile(partialFile);
      if not SameText(hash, upgradeInfo.Digest) then
      begin
        Logger.Error(TErrorCode.E0307_UpgradeInstallerHashMismatch, 'The downloaded ' + upgradeInfo.AssetName + ' does not match the hash github ' +
                     'published for it (sha256 ' + LowerCase(hash) + ', expected ' + upgradeInfo.Digest + '), so it was deleted.');
        exit;
      end;
    end
    else
      Logger.Verbose('github published no digest for ' + upgradeInfo.AssetName + ' - relying on its Authenticode signature.');

    if not VerifyInstallerSignature(partialFile, upgradeInfo.AssetName) then
      exit;

    if not TFileUtils.TryReplaceFile(partialFile, destFile, lastError) then
    begin
      Logger.Error(TErrorCode.E0303_UpgradeDownloadFailed, 'The installer was downloaded, but [' + destFile + '] could not be written : ' +
                   SysErrorMessage(lastError));
      exit;
    end;
  finally
    response := nil;
    request := nil;
    if FileExists(partialFile) then
      System.SysUtils.DeleteFile(partialFile);
  end;

  Logger.Information('Downloaded to ' + destFile + ' [' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms]');
  installerFile := destFile;
  result := true;
end;

function TGithubUpgradeService.VerifyInstallerSignature(const fileName : string; const displayName : string) : boolean;
var
  reason : string;
begin
  result := TPEUtils.VerifyAuthenticode(fileName, true, reason);
  if not result then
    Logger.Error(TErrorCode.E0306_UpgradeInstallerNotTrusted, 'dpm will not run the installer ' + displayName + ' - the file ' + reason + '.');
end;

function TGithubUpgradeService.LaunchInstaller(const installerFile : string) : boolean;
begin
  result := false;
  if not TFile.Exists(installerFile) then
  begin
    Logger.Error(TErrorCode.E0304_UpgradeInstallerFailed, 'The installer [' + installerFile + '] does not exist.');
    exit;
  end;

  //it has sat in temp since it was downloaded (in the IDE, while the user answered the save prompts).
  if not VerifyInstallerSignature(installerFile, '[' + installerFile + ']') then
  begin
    try
      TFile.Delete(installerFile);
    except
      //it isn't going to be run either way.
    end;
    exit;
  end;

  try
    //Must not wait - the installer cannot replace our exe until we have exited.
    TProcess.ExecuteNoWait(installerFile, '');
    result := true;
  except
    on e : Exception do
      Logger.Error(TErrorCode.E0304_UpgradeInstallerFailed, 'Unable to start the installer [' + installerFile + '] : ' + e.Message);
  end;
end;

end.
