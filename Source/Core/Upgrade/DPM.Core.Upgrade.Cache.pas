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

//Caches the outcome of an upgrade check so that the IDE - which checks every
//time the DPM view is opened - does not hit the github api repeatedly. Stored
//in %APPDATA%\.dpm\update-check.json, TTL 1 hour by default.
//
//The CLI deliberately does NOT use this : `dpm upgrade` is an explicit user
//action, so it always does a live check.
//
//Both outcomes are cached, including UpToDate - otherwise an up to date IDE
//would re-check on every view open, which is the common case. Errors are never
//cached, so an offline machine retries next time rather than going quiet for an
//hour.

unit DPM.Core.Upgrade.Cache;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Upgrade.Interfaces;

type
  IUpgradeCheckCache = interface
    ['{6C4E8D91-3A7F-4B25-9E08-D1F5A63C7B40}']

    /// <summary>
    ///  Returns a cached outcome when one exists that is younger than the TTL
    ///  AND was recorded for the same channel and the same running version.
    ///  upgradeInfo is only set when checkResult is UpgradeAvailable.
    /// </summary>
    /// <param name="includePrerelease">
    ///  The channel the caller is about to check for. A cached stable-channel
    ///  answer must not be served to a beta-channel caller (or vice versa), so
    ///  this forms part of the cache key rather than just the value.
    /// </param>
    function TryGet(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                    out checkResult : TUpgradeCheckResult; out upgradeInfo : IUpgradeInfo) : boolean;

    /// <summary>
    ///  Records an outcome. Only UpToDate and UpgradeAvailable should be
    ///  stored - passing Error is ignored rather than persisted.
    /// </summary>
    procedure Put(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                  const checkResult : TUpgradeCheckResult; const upgradeInfo : IUpgradeInfo);

    /// <summary>Deletes the cache file. Never raises.</summary>
    procedure Clear;

    function GetFileName : string;
    property FileName : string read GetFileName;
  end;

  TUpgradeCheckCache = class(TInterfacedObject, IUpgradeCheckCache)
  private
    FLogger : ILogger;
    FFileName : string;
    FTTLHours : integer;
  protected
    function TryGet(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                    out checkResult : TUpgradeCheckResult; out upgradeInfo : IUpgradeInfo) : boolean;
    procedure Put(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                  const checkResult : TUpgradeCheckResult; const upgradeInfo : IUpgradeInfo);
    procedure Clear;
    function GetFileName : string;
  public
    //container ctor - real path, 1 hour ttl.
    constructor Create(const logger : ILogger); overload;
    //injectable for tests.
    constructor Create(const logger : ILogger; const fileName : string; const ttlHours : integer); overload;
  end;

const
  cUpdateCheckCacheFile = '%APPDATA%\.dpm\update-check.json';
  cUpdateCheckTTLHours = 1;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  VSoft.YAML,
  DPM.Core.Utils.System,
  DPM.Core.Utils.DateTime;

const
  //channel is stored as a readable string rather than the raw boolean so the
  //file is meaningful when someone opens it.
  cChannelStable = 'stable';
  cChannelBeta = 'beta';

  cResultUpToDate = 'UpToDate';
  cResultUpgradeAvailable = 'UpgradeAvailable';

function ChannelToString(const includePrerelease : boolean) : string;
begin
  if includePrerelease then
    result := cChannelBeta
  else
    result := cChannelStable;
end;

{ TUpgradeCheckCache }

constructor TUpgradeCheckCache.Create(const logger : ILogger);
begin
  Create(logger, TSystemUtils.ExpandEnvironmentStrings(cUpdateCheckCacheFile), cUpdateCheckTTLHours);
end;

constructor TUpgradeCheckCache.Create(const logger : ILogger; const fileName : string; const ttlHours : integer);
begin
  inherited Create;
  FLogger := logger;
  FFileName := fileName;
  if ttlHours <= 0 then
    FTTLHours := cUpdateCheckTTLHours
  else
    FTTLHours := ttlHours;
end;

function TUpgradeCheckCache.GetFileName : string;
begin
  result := FFileName;
end;

function TUpgradeCheckCache.TryGet(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                                   out checkResult : TUpgradeCheckResult; out upgradeInfo : IUpgradeInfo) : boolean;
var
  doc : IYAMLDocument;
  entry : IYAMLMapping;
  cachedAt : TDateTime;
  ageHours : Int64;
  sResult : string;
  version : TPackageVersion;
begin
  result := false;
  checkResult := TUpgradeCheckResult.Error;
  upgradeInfo := nil;

  if not TFile.Exists(FFileName) then
    exit;

  //Every failure below is a cache MISS, never an exception to the caller - a
  //corrupt or half written file must not break the update check.
  try
    doc := TYAML.LoadFromString(TFile.ReadAllText(FFileName));
  except
    on e : Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Verbose('Ignoring unreadable update check cache [' + FFileName + '] : ' + e.Message);
      exit;
    end;
  end;

  if (doc = nil) or not doc.IsMapping then
    exit;
  entry := doc.AsMapping;

  if not entry.Contains('cachedAt') then
    exit;
  if not TDPMDateTimeUtils.TryISO8601ToDate(entry.S['cachedAt'], cachedAt, true) then
    exit;

  //A cachedAt in the future (clock changed, file copied between machines) would
  //give a negative age and look permanently fresh - treat it as a miss.
  ageHours := HoursBetween(TTimeZone.Local.ToUniversalTime(Now), cachedAt);
  if (ageHours > FTTLHours) or (cachedAt > TTimeZone.Local.ToUniversalTime(Now)) then
    exit;

  //Channel and running version are part of the key : switching channel or
  //having upgraded since must not serve the old answer.
  if not SameText(entry.S['channel'], ChannelToString(includePrerelease)) then
    exit;
  if not SameText(entry.S['currentVersion'], currentVersion.ToStringNoMeta) then
    exit;

  sResult := entry.S['result'];
  if SameText(sResult, cResultUpToDate) then
  begin
    checkResult := TUpgradeCheckResult.UpToDate;
    exit(true);
  end;

  if not SameText(sResult, cResultUpgradeAvailable) then
    exit; //unknown/absent - treat as a miss rather than guessing.

  if not TPackageVersion.TryParse(entry.S['latestVersion'], version) then
    exit;

  //Without a download url the entry is useless - re-check rather than surface
  //an upgrade the user cannot action.
  if entry.S['downloadUrl'] = '' then
    exit;

  upgradeInfo := TUpgradeInfo.Create(version, entry.S['tagName'], entry.S['assetName'], entry.S['downloadUrl'],
                                     entry.S['releaseUrl'], entry.B['isPrerelease']);
  checkResult := TUpgradeCheckResult.UpgradeAvailable;
  result := true;
end;

procedure TUpgradeCheckCache.Put(const currentVersion : TPackageVersion; const includePrerelease : boolean;
                                 const checkResult : TUpgradeCheckResult; const upgradeInfo : IUpgradeInfo);
var
  doc : IYAMLDocument;
  entry : IYAMLMapping;
begin
  //Errors are not cached - see the unit header.
  if checkResult = TUpgradeCheckResult.Error then
    exit;
  if (checkResult = TUpgradeCheckResult.UpgradeAvailable) and (upgradeInfo = nil) then
    exit;

  try
    doc := TYAML.CreateMapping;
    entry := doc.AsMapping;
    entry.S['cachedAt'] := TDPMDateTimeUtils.DateToISO8601(TTimeZone.Local.ToUniversalTime(Now), true);
    entry.S['channel'] := ChannelToString(includePrerelease);
    entry.S['currentVersion'] := currentVersion.ToStringNoMeta;

    if checkResult = TUpgradeCheckResult.UpToDate then
      entry.S['result'] := cResultUpToDate
    else
    begin
      entry.S['result'] := cResultUpgradeAvailable;
      entry.S['latestVersion'] := upgradeInfo.Version.ToStringNoMeta;
      entry.S['tagName'] := upgradeInfo.TagName;
      entry.S['assetName'] := upgradeInfo.AssetName;
      entry.S['downloadUrl'] := upgradeInfo.DownloadUrl;
      entry.S['releaseUrl'] := upgradeInfo.ReleaseUrl;
      entry.B['isPrerelease'] := upgradeInfo.IsPrerelease;
    end;

    ForceDirectories(ExtractFilePath(FFileName));
    TFile.WriteAllText(FFileName, TYAML.WriteToJSONString(doc), TEncoding.UTF8);
  except
    //Failing to write the cache just means we check again next time - warn, but
    //never let it propagate into the update check.
    on e : Exception do
      if Assigned(FLogger) then
        FLogger.Warning('Could not write update check cache [' + FFileName + '] : ' + e.Message);
  end;
end;

procedure TUpgradeCheckCache.Clear;
begin
  try
    if TFile.Exists(FFileName) then
      TFile.Delete(FFileName);
  except
    on e : Exception do
      if Assigned(FLogger) then
        FLogger.Warning('Could not delete update check cache [' + FFileName + '] : ' + e.Message);
  end;
end;

end.
