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

unit DPM.Core.Upgrade.Interfaces;

interface

uses
  VSoft.CancellationToken,
  VSoft.Uri,
  DPM.Core.Types;

{$SCOPEDENUMS ON}

type
  TUpgradeCheckResult = (UpToDate, UpgradeAvailable, Error);

  /// <summary>
  ///  Describes a release on github that is newer than the version we are
  ///  running.
  /// </summary>
  IUpgradeInfo = interface
    ['{9F1C6A2E-4B7D-4E31-8C0A-5D2E7F1A8B34}']
    function GetVersion : TPackageVersion;
    function GetTagName : string;
    function GetAssetName : string;
    function GetDownloadUrl : string;
    function GetReleaseUrl : string;
    function GetIsPrerelease : boolean;
    function GetDigest : string;

    /// <summary>The semver parsed from the release tag.</summary>
    property Version : TPackageVersion read GetVersion;
    /// <summary>The raw github tag, eg 'v0.1.10-alpha'.</summary>
    property TagName : string read GetTagName;
    /// <summary>The installer asset file name, eg 'DPMSetup-0.1.10-alpha.exe'.</summary>
    property AssetName : string read GetAssetName;
    /// <summary>Direct download url for the installer asset.</summary>
    property DownloadUrl : string read GetDownloadUrl;
    /// <summary>The html url of the release, for display / release notes.</summary>
    property ReleaseUrl : string read GetReleaseUrl;
    property IsPrerelease : boolean read GetIsPrerelease;
    /// <summary>
    ///  The SHA-256 (lowercase hex) github published for the installer asset, '' when it published none -
    ///  assets uploaded before github started recording digests have none.
    /// </summary>
    property Digest : string read GetDigest;
  end;

  /// <summary>
  ///  Checks for, downloads and launches an upgrade of the dpm client itself.
  ///  Deliberately free of any UI concerns (no prompting, no console writing -
  ///  it only logs) so that both the command line and the IDE plugin can use it.
  /// </summary>
  IUpgradeService = interface
    ['{2E7A5C13-8D64-4F09-B1A7-6C3E9D0F4A82}']

    /// <summary>
    ///  Queries github releases for a version newer than currentVersion.
    /// </summary>
    /// <param name="currentVersion">
    ///  The version to compare against. Passed in rather than read internally
    ///  because the IDE plugin has its own version, distinct from the cli's.
    /// </param>
    /// <param name="includePrerelease">
    ///  When false, prerelease releases are ignored entirely.
    /// </param>
    /// <param name="upgradeInfo">
    ///  Set only when the result is UpgradeAvailable, nil otherwise.
    /// </param>
    function CheckForUpgrade(const cancellationToken : ICancellationToken; const currentVersion : TPackageVersion;
                             const includePrerelease : boolean; out upgradeInfo : IUpgradeInfo) : TUpgradeCheckResult;

    /// <summary>
    ///  Why the last CheckForUpgrade returned Error. The check does not log it - an upgrade the
    ///  user asked for fails with it, a background check only warns.
    /// </summary>
    function LastError : string;

    /// <summary>
    ///  Downloads the installer asset to the temp folder. Returns false and
    ///  logs on failure. Only an installer that matches the digest github
    ///  published for it and carries a valid Authenticode signature is kept -
    ///  any other download is deleted.
    /// </summary>
    /// <param name="installerFile">The full path to the downloaded installer.</param>
    function DownloadUpgrade(const cancellationToken : ICancellationToken; const upgradeInfo : IUpgradeInfo;
                             out installerFile : string) : boolean;

    /// <summary>
    ///  Starts the installer and returns immediately without waiting - the
    ///  installer cannot replace our exe until we have exited, so the caller
    ///  must exit promptly after this returns true. Checks the Authenticode
    ///  signature again first (it is a file in temp, it may have changed since
    ///  it was downloaded) and refuses, deleting it, when it is not valid.
    /// </summary>
    function LaunchInstaller(const installerFile : string) : boolean;
  end;

  /// <summary>
  ///  Plain carrier for IUpgradeInfo. Lives here rather than with the github
  ///  service so that the check cache can rehydrate one without depending on
  ///  the service implementation.
  /// </summary>
  TUpgradeInfo = class(TInterfacedObject, IUpgradeInfo)
  private
    FVersion : TPackageVersion;
    FTagName : string;
    FAssetName : string;
    FDownloadUrl : string;
    FReleaseUrl : string;
    FIsPrerelease : boolean;
    FDigest : string;
  protected
    function GetVersion : TPackageVersion;
    function GetTagName : string;
    function GetAssetName : string;
    function GetDownloadUrl : string;
    function GetReleaseUrl : string;
    function GetIsPrerelease : boolean;
    function GetDigest : string;
  public
    constructor Create(const version : TPackageVersion; const tagName : string; const assetName : string;
                       const downloadUrl : string; const releaseUrl : string; const isPrerelease : boolean;
                       const digest : string); reintroduce;
  end;

  /// <summary>
  ///  What dpm accepts as an upgrade installer. Checked when a release is
  ///  picked, when the IDE reads a cached check back, and again before the
  ///  download - the release list and update-check.json are both just data.
  /// </summary>
  TUpgradeAssetRules = class
  public
    /// <summary>
    ///  DPMSetup-{version}.exe, the version made only of letters, digits and
    ///  . + - (no path separators, drive letters or spaces), as the build names it.
    /// </summary>
    class function IsInstallerName(const assetName : string) : boolean; static;

    /// <summary>
    ///  Parses url and returns true when it is https (port 443, no user info)
    ///  on one of the hosts github serves release assets from. The download
    ///  must use the uri returned here, so what was checked is what is fetched.
    ///  Otherwise reason says why, in a form that follows "the url ...".
    /// </summary>
    class function TryParseDownloadUrl(const url : string; out uri : IUri; out reason : string) : boolean; static;

    /// <summary>
    ///  github's asset digest is 'sha256:{64 hex}'. Returns true with the hex
    ///  (lowercase) for that form, false for anything else.
    /// </summary>
    class function TryParseDigest(const digest : string; out sha256Hex : string) : boolean; static;
  end;

const
  //github repository that dpm releases are published to.
  cDPMGithubRepo = 'DelphiPackageManager/DPM';

  //These mirror the constants in DPM.Core.Repository.BaseGithub, which is NOT
  //compiled into dpm.dpr - that unit is an unfinished stub (its repository
  //methods are all commented out) and using it here would drag it into the
  //build just for three strings. Duplicated deliberately; if the github
  //repository support is ever finished, consolidate these.
  cGithubApiUrl = 'https://api.github.com';
  cGithubReleases = 'repos/%s/releases'; //repo
  cGithubv3Accept = 'application/vnd.github.v3+json';

  //The installer asset is named DPMSetup-{semver}.exe by the build (see
  //Build\BuildDPM.fbp8 - OutputFileName=DPMSetup-%SemVer%). We match on the
  //prefix/extension rather than reconstructing the whole name, so that a change
  //to how the version is rendered in the file name doesn't break upgrades.
  cDPMInstallerPrefix = 'DPMSetup-';
  cDPMInstallerExt = '.exe';

  //browser_download_url is on github.com, which redirects to one of the other two - VSoft.HttpClient follows
  //redirects but never from https to http.
  cGithubDownloadHosts : array[0..2] of string = ('github.com', 'objects.githubusercontent.com', 'release-assets.githubusercontent.com');

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Strings;

const
  cSha256DigestPrefix = 'sha256:';
  cHttpsPort = 443;

{ TUpgradeInfo }

constructor TUpgradeInfo.Create(const version : TPackageVersion; const tagName, assetName, downloadUrl, releaseUrl : string;
                                const isPrerelease : boolean; const digest : string);
begin
  inherited Create;
  FVersion := version;
  FTagName := tagName;
  FAssetName := assetName;
  FDownloadUrl := downloadUrl;
  FReleaseUrl := releaseUrl;
  FIsPrerelease := isPrerelease;
  FDigest := digest;
end;

function TUpgradeInfo.GetAssetName : string;
begin
  result := FAssetName;
end;

function TUpgradeInfo.GetDigest : string;
begin
  result := FDigest;
end;

function TUpgradeInfo.GetDownloadUrl : string;
begin
  result := FDownloadUrl;
end;

function TUpgradeInfo.GetIsPrerelease : boolean;
begin
  result := FIsPrerelease;
end;

function TUpgradeInfo.GetReleaseUrl : string;
begin
  result := FReleaseUrl;
end;

function TUpgradeInfo.GetTagName : string;
begin
  result := FTagName;
end;

function TUpgradeInfo.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;

{ TUpgradeAssetRules }

class function TUpgradeAssetRules.IsInstallerName(const assetName : string) : boolean;
var
  version : string;
  i : integer;
begin
  result := false;
  if Length(assetName) <= Length(cDPMInstallerPrefix) + Length(cDPMInstallerExt) then
    exit;
  if not TStringUtils.StartsWith(assetName, cDPMInstallerPrefix, true) then
    exit;
  if not SameText(Copy(assetName, Length(assetName) - Length(cDPMInstallerExt) + 1, Length(cDPMInstallerExt)), cDPMInstallerExt) then
    exit;
  version := Copy(assetName, Length(cDPMInstallerPrefix) + 1, Length(assetName) - Length(cDPMInstallerPrefix) - Length(cDPMInstallerExt));
  for i := 1 to Length(version) do
  begin
    case version[i] of
      '0'..'9', 'a'..'z', 'A'..'Z', '.', '+', '-' : ;
    else
      exit;
    end;
  end;
  result := true;
end;

class function TUpgradeAssetRules.TryParseDownloadUrl(const url : string; out uri : IUri; out reason : string) : boolean;
var
  i : integer;
  normalised : string;
begin
  result := false;
  reason := '';
  uri := nil;
  //schemes are case insensitive, but VSoft.Uri only knows https's default port when it is written in lowercase.
  normalised := url;
  i := Pos(':', normalised);
  if i > 0 then
    normalised := LowerCase(Copy(normalised, 1, i - 1)) + Copy(normalised, i, MaxInt);
  if (normalised = '') or not TUriFactory.TryParse(normalised, true, uri) then
  begin
    uri := nil;
    reason := 'is not a valid url';
    exit;
  end;
  if not SameText(uri.Scheme, 'https') then
    reason := 'is not an https url'
  else if (uri.UserName <> '') or (uri.Password <> '') then
    reason := 'has user information in it'
  else if uri.Port <> cHttpsPort then
    reason := 'is not on the https port'
  else
  begin
    for i := Low(cGithubDownloadHosts) to High(cGithubDownloadHosts) do
    begin
      if SameText(uri.Host, cGithubDownloadHosts[i]) then
        exit(true);
    end;
    reason := 'is not on a host github serves release downloads from';
  end;
  uri := nil;
end;

class function TUpgradeAssetRules.TryParseDigest(const digest : string; out sha256Hex : string) : boolean;
var
  hex : string;
  i : integer;
begin
  result := false;
  sha256Hex := '';
  if not TStringUtils.StartsWith(digest, cSha256DigestPrefix, true) then
    exit;
  hex := LowerCase(Copy(digest, Length(cSha256DigestPrefix) + 1, MaxInt));
  if Length(hex) <> 64 then
    exit;
  for i := 1 to Length(hex) do
  begin
    case hex[i] of
      '0'..'9', 'a'..'f' : ;
    else
      exit;
    end;
  end;
  sha256Hex := hex;
  result := true;
end;

end.
