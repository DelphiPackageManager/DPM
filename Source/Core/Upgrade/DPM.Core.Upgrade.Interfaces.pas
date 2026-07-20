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
    ///  Downloads the installer asset to the temp folder. Returns false and
    ///  logs on failure.
    /// </summary>
    /// <param name="installerFile">The full path to the downloaded installer.</param>
    function DownloadUpgrade(const cancellationToken : ICancellationToken; const upgradeInfo : IUpgradeInfo;
                             out installerFile : string) : boolean;

    /// <summary>
    ///  Starts the installer and returns immediately without waiting - the
    ///  installer cannot replace our exe until we have exited, so the caller
    ///  must exit promptly after this returns true.
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
  protected
    function GetVersion : TPackageVersion;
    function GetTagName : string;
    function GetAssetName : string;
    function GetDownloadUrl : string;
    function GetReleaseUrl : string;
    function GetIsPrerelease : boolean;
  public
    constructor Create(const version : TPackageVersion; const tagName : string; const assetName : string;
                       const downloadUrl : string; const releaseUrl : string; const isPrerelease : boolean); reintroduce;
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

implementation

{ TUpgradeInfo }

constructor TUpgradeInfo.Create(const version : TPackageVersion; const tagName, assetName, downloadUrl, releaseUrl : string;
                                const isPrerelease : boolean);
begin
  inherited Create;
  FVersion := version;
  FTagName := tagName;
  FAssetName := assetName;
  FDownloadUrl := downloadUrl;
  FReleaseUrl := releaseUrl;
  FIsPrerelease := isPrerelease;
end;

function TUpgradeInfo.GetAssetName : string;
begin
  result := FAssetName;
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

end.
