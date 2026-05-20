{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Vuln.Cache;

{ Disk-backed response cache for vulnerability database queries. One JSON file
  per (source, sha256(purl)) under %APPDATA%\.dpm\vuln-cache\<source>\<hash>.json.
  TTL is 24h by default - configurable via constructor. The wrapped database
  produces the responses; this cache only stores and retrieves the raw JSON. }

interface

uses
  DPM.Core.Logging,
  DPM.Core.Vuln.Interfaces;

type
  TVulnResponseCache = class(TInterfacedObject, IVulnResponseCache)
  private
    FLogger : ILogger;
    FCacheDir : string;
    FTTLHours : integer;
    FBypassReads : boolean;
    function CacheFileFor(const purl, sourceName : string) : string;
  protected
    function GetCacheDir : string;
    function GetBypassReads : boolean;
    procedure SetBypassReads(const value : boolean);
    function TryGet(const purl : string; const sourceName : string;
                    out cachedJson : string) : boolean;
    procedure Put(const purl : string; const sourceName : string; const json : string);
    procedure Clear;
  public
    constructor Create(const logger : ILogger; const cacheDir : string = '';
                       const ttlHours : integer = 24);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  System.Classes,
  JsonDataObjects,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Hash;

const
  cDefaultVulnCacheDir = '%APPDATA%\.dpm\vuln-cache';

function SafeSourceName(const value : string) : string;
var
  i : integer;
  c : char;
begin
  result := '';
  for i := 1 to Length(value) do
  begin
    c := value[i];
    if CharInSet(c, ['A'..'Z','a'..'z','0'..'9','-','_']) then
      result := result + c
    else
      result := result + '_';
  end;
  if result = '' then
    result := 'unknown';
end;

function HashStringToHex(const s : string) : string;
var
  bytes : TBytes;
  stream : TBytesStream;
begin
  bytes := TEncoding.UTF8.GetBytes(s);
  stream := TBytesStream.Create(bytes);
  try
    result := THashSHA256.GetHashString(stream);
  finally
    stream.Free;
  end;
end;

{ TVulnResponseCache }

constructor TVulnResponseCache.Create(const logger : ILogger; const cacheDir : string;
                                      const ttlHours : integer);
begin
  inherited Create;
  FLogger := logger;
  if cacheDir <> '' then
    FCacheDir := cacheDir
  else
    FCacheDir := TSystemUtils.ExpandEnvironmentStrings(cDefaultVulnCacheDir);
  if ttlHours <= 0 then
    FTTLHours := 24
  else
    FTTLHours := ttlHours;
end;

function TVulnResponseCache.GetCacheDir : string;
begin
  result := FCacheDir;
end;

function TVulnResponseCache.GetBypassReads : boolean;
begin
  result := FBypassReads;
end;

procedure TVulnResponseCache.SetBypassReads(const value : boolean);
begin
  FBypassReads := value;
end;

function TVulnResponseCache.CacheFileFor(const purl, sourceName : string) : string;
var
  perSource : string;
begin
  perSource := IncludeTrailingPathDelimiter(FCacheDir) + SafeSourceName(sourceName);
  result := IncludeTrailingPathDelimiter(perSource) + HashStringToHex(purl) + '.json';
end;

function TVulnResponseCache.TryGet(const purl, sourceName : string;
                                   out cachedJson : string) : boolean;
var
  fileName : string;
  rawJson : string;
  entry : TJsonObject;
  cachedAtStr : string;
  cachedAt : TDateTime;
  ageHours : double;
begin
  result := false;
  cachedJson := '';
  if FBypassReads then
    exit;
  fileName := CacheFileFor(purl, sourceName);
  if not FileExists(fileName) then
    exit;

  try
    rawJson := TFile.ReadAllText(fileName, TEncoding.UTF8);
  except
    on e : Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Warning('Could not read vuln cache file [' + fileName + ']: ' + e.Message);
      exit;
    end;
  end;

  entry := nil;
  try
    try
      entry := TJsonBaseObject.Parse(rawJson) as TJsonObject;
    except
      on e : Exception do
      begin
        if Assigned(FLogger) then
          FLogger.Warning('Corrupt vuln cache file [' + fileName + ']: ' + e.Message);
        exit;
      end;
    end;

    if not entry.Contains('cachedAt') then
      exit;
    cachedAtStr := entry.S['cachedAt'];
    if not TryISO8601ToDate(cachedAtStr, cachedAt, true) then
      exit;
    ageHours := HoursBetween(TTimeZone.Local.ToUniversalTime(Now), cachedAt);
    if ageHours > FTTLHours then
      exit;

    if not entry.Contains('response') then
      exit;
    cachedJson := entry.O['response'].ToJSON(false);
    result := true;
  finally
    entry.Free;
  end;
end;

procedure TVulnResponseCache.Put(const purl, sourceName, json : string);
var
  fileName : string;
  perSourceDir : string;
  entry : TJsonObject;
  responseObj : TJsonBaseObject;
  outJson : string;
begin
  fileName := CacheFileFor(purl, sourceName);
  perSourceDir := ExtractFilePath(fileName);
  if not DirectoryExists(perSourceDir) then
  begin
    try
      ForceDirectories(perSourceDir);
    except
      on e : Exception do
      begin
        if Assigned(FLogger) then
          FLogger.Warning('Could not create vuln cache dir [' + perSourceDir + ']: ' + e.Message);
        exit;
      end;
    end;
  end;

  entry := TJsonObject.Create;
  try
    entry.S['cachedAt'] := DateToISO8601(TTimeZone.Local.ToUniversalTime(Now), true);
    entry.S['purl'] := purl;
    entry.S['source'] := sourceName;
    //Persist the raw response as a sub-object if it parses as JSON; otherwise
    //fall back to a string field. Storing it as an object keeps the cache file
    //pretty and lets test fixtures inspect it without re-parsing.
    try
      responseObj := TJsonBaseObject.Parse(json);
      try
        if responseObj is TJsonObject then
          entry.O['response'].Assign(TJsonObject(responseObj))
        else if responseObj is TJsonArray then
          entry.A['response'].Assign(TJsonArray(responseObj))
        else
          entry.S['response'] := json;
      finally
        responseObj.Free;
      end;
    except
      entry.S['response'] := json;
    end;

    outJson := entry.ToJSON(false);
    try
      TFile.WriteAllText(fileName, outJson, TEncoding.UTF8);
    except
      on e : Exception do
        if Assigned(FLogger) then
          FLogger.Warning('Could not write vuln cache file [' + fileName + ']: ' + e.Message);
    end;
  finally
    entry.Free;
  end;
end;

procedure TVulnResponseCache.Clear;
begin
  if DirectoryExists(FCacheDir) then
  begin
    try
      TDirectory.Delete(FCacheDir, true);
    except
      on e : Exception do
        if Assigned(FLogger) then
          FLogger.Warning('Could not clear vuln cache dir [' + FCacheDir + ']: ' + e.Message);
    end;
  end;
end;

end.
