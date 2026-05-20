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

unit DPM.Core.Vuln.OSV;

(* Open Source Vulnerabilities (OSV) database client.

  Two-step query pattern:
  1. POST https://api.osv.dev/v1/querybatch with a queries[] body, one entry
     per purl. Returns matched vuln IDs per query in the same order.
  2. POST https://api.osv.dev/v1/vulns/<id> (one per unique ID) for the full advisory.

  Both step outputs are cached by IVulnResponseCache - the cache stores raw JSON
  keyed by (source, sha(purl)) for batch results, and by (source, sha(vulnId))
  for advisories. *)

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Vuln.Interfaces;

const
  cOSVSourceName = 'osv';
  cOSVBaseUri = 'https://api.osv.dev';

type
  TOSVDatabase = class(TInterfacedObject, IVulnDatabase)
  private
    FLogger : ILogger;
    FCache : IVulnResponseCache;
    function FetchBatchIds(const cancellationToken : ICancellationToken;
                           const purls : IList<string>;
                           out perQueryIds : IList<IList<string>>) : boolean;
    function FetchVulnDetail(const cancellationToken : ICancellationToken;
                             const vulnId : string;
                             out advisory : IVulnDatabaseAdvisory) : boolean;
  protected
    function GetSourceName : string;
    function Query(const cancellationToken : ICancellationToken;
                   const purls : IList<string>) : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
  public
    constructor Create(const logger : ILogger; const cache : IVulnResponseCache);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects,
  VSoft.HttpClient,
  DPM.Core.Constants,
  DPM.Core.Vuln.Types;

const
  cOSVCacheKeyVulnPrefix = 'osv-vuln:';

function BuildPurlCacheBody(const ids : IList<string>) : string; forward;

type
  TOSVAdvisory = class(TInterfacedObject, IVulnDatabaseAdvisory)
  private
    FId : string;
    FAliases : IList<TVulnAlias>;
    FSummary : string;
    FDetails : string;
    FSeverity : TSeverity;
    FCvssScore : Double;
    FCvssVector : string;
    FPublished : string;
    FModified : string;
    FReferences : IList<string>;
    FFixedVersion : string;
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
    constructor Create;
    procedure LoadFromJson(const obj : TJsonObject);
  end;

{ TOSVAdvisory }

constructor TOSVAdvisory.Create;
begin
  inherited Create;
  FAliases := TCollections.CreateList<TVulnAlias>;
  FReferences := TCollections.CreateList<string>;
  FSeverity := TSeverity.Unknown;
end;

function TOSVAdvisory.GetId : string;
begin
  result := FId;
end;

function TOSVAdvisory.GetAliases : IList<TVulnAlias>;
begin
  result := FAliases;
end;

function TOSVAdvisory.GetSummary : string;
begin
  result := FSummary;
end;

function TOSVAdvisory.GetDetails : string;
begin
  result := FDetails;
end;

function TOSVAdvisory.GetSeverity : TSeverity;
begin
  result := FSeverity;
end;

function TOSVAdvisory.GetCvssScore : Double;
begin
  result := FCvssScore;
end;

function TOSVAdvisory.GetCvssVector : string;
begin
  result := FCvssVector;
end;

function TOSVAdvisory.GetPublished : string;
begin
  result := FPublished;
end;

function TOSVAdvisory.GetModified : string;
begin
  result := FModified;
end;

function TOSVAdvisory.GetReferences : IList<string>;
begin
  result := FReferences;
end;

function TOSVAdvisory.GetFixedVersion : string;
begin
  result := FFixedVersion;
end;

procedure TOSVAdvisory.LoadFromJson(const obj : TJsonObject);
var
  aliasesArr : TJsonArray;
  i, j, k : integer;
  aliasId : string;
  alias : TVulnAlias;
  refsArr : TJsonArray;
  refObj : TJsonObject;
  affectedArr : TJsonArray;
  affObj : TJsonObject;
  rangesArr : TJsonArray;
  rangeObj : TJsonObject;
  eventsArr : TJsonArray;
  evtObj : TJsonObject;
  severityArr : TJsonArray;
  sevObj : TJsonObject;
  databaseSpec : TJsonObject;
  databaseSev : string;
  score : double;
begin
  if obj.Contains('id') then
    FId := obj.S['id'];
  if obj.Contains('summary') then
    FSummary := obj.S['summary'];
  if obj.Contains('details') then
    FDetails := obj.S['details'];
  if obj.Contains('published') then
    FPublished := obj.S['published'];
  if obj.Contains('modified') then
    FModified := obj.S['modified'];

  if obj.Contains('aliases') and (not obj.IsNull('aliases')) then
  begin
    aliasesArr := obj.A['aliases'];
    for i := 0 to aliasesArr.Count - 1 do
    begin
      aliasId := aliasesArr.S[i];
      if aliasId = '' then
        continue;
      //Naive source inference: GHSA-... -> GHSA, CVE-... -> CVE, else OSV.
      alias.Id := aliasId;
      if Pos('CVE-', aliasId) = 1 then
        alias.Source := 'CVE'
      else if Pos('GHSA-', aliasId) = 1 then
        alias.Source := 'GHSA'
      else
        alias.Source := 'OSV';
      FAliases.Add(alias);
    end;
  end;

  //severity[] can carry CVSS_V3 / CVSS_V2 etc. Prefer V3 if present.
  if obj.Contains('severity') and (not obj.IsNull('severity')) then
  begin
    severityArr := obj.A['severity'];
    for i := 0 to severityArr.Count - 1 do
    begin
      sevObj := severityArr.O[i];
      if (sevObj.Contains('type')) and (Pos('CVSS', UpperCase(sevObj.S['type'])) = 1) then
      begin
        FCvssVector := sevObj.S['score'];
        if ParseCvssBaseScore(FCvssVector, score) then
        begin
          FCvssScore := score;
          FSeverity := CvssScoreToSeverity(score);
        end;
        if FSeverity <> TSeverity.Unknown then
          break;
      end;
    end;
  end;

  //GitHub publishes a textual severity at database_specific.severity
  //('LOW' / 'MODERATE' / 'HIGH' / 'CRITICAL'). Use it as a fallback if we
  //couldn't derive a CVSS score.
  if (FSeverity = TSeverity.Unknown) and obj.Contains('database_specific') and (not obj.IsNull('database_specific')) then
  begin
    databaseSpec := obj.O['database_specific'];
    if databaseSpec.Contains('severity') then
    begin
      databaseSev := UpperCase(databaseSpec.S['severity']);
      if databaseSev = 'CRITICAL' then
        FSeverity := TSeverity.Critical
      else if databaseSev = 'HIGH' then
        FSeverity := TSeverity.High
      else if (databaseSev = 'MEDIUM') or (databaseSev = 'MODERATE') then
        FSeverity := TSeverity.Medium
      else if databaseSev = 'LOW' then
        FSeverity := TSeverity.Low;
    end;
  end;

  if obj.Contains('references') and (not obj.IsNull('references')) then
  begin
    refsArr := obj.A['references'];
    for i := 0 to refsArr.Count - 1 do
    begin
      refObj := refsArr.O[i];
      if refObj.Contains('url') then
        FReferences.Add(refObj.S['url']);
    end;
  end;

  //affected[].ranges[].events[].fixed - take the first 'fixed' we see.
  //OSV may list multiple per-ecosystem ranges; for our reporting we just want
  //a single hint at "the fix landed in version X".
  if obj.Contains('affected') and (not obj.IsNull('affected')) then
  begin
    affectedArr := obj.A['affected'];
    for i := 0 to affectedArr.Count - 1 do
    begin
      affObj := affectedArr.O[i];
      if not affObj.Contains('ranges') then
        continue;
      rangesArr := affObj.A['ranges'];
      for j := 0 to rangesArr.Count - 1 do
      begin
        rangeObj := rangesArr.O[j];
        if not rangeObj.Contains('events') then
          continue;
        eventsArr := rangeObj.A['events'];
        for k := 0 to eventsArr.Count - 1 do
        begin
          evtObj := eventsArr.O[k];
          if evtObj.Contains('fixed') then
          begin
            FFixedVersion := evtObj.S['fixed'];
            break;
          end;
        end;
        if FFixedVersion <> '' then
          break;
      end;
      if FFixedVersion <> '' then
        break;
    end;
  end;
end;

{ TOSVDatabase }

constructor TOSVDatabase.Create(const logger : ILogger; const cache : IVulnResponseCache);
begin
  inherited Create;
  FLogger := logger;
  FCache := cache;
end;

function TOSVDatabase.GetSourceName : string;
begin
  result := cOSVSourceName;
end;

function TOSVDatabase.FetchBatchIds(const cancellationToken : ICancellationToken;
                                    const purls : IList<string>;
                                    out perQueryIds : IList<IList<string>>) : boolean;
var
  reqBody : TJsonObject;
  queriesArr : TJsonArray;
  i, j : integer;
  queryObj : TJsonObject;
  packageObj : TJsonObject;
  sBody : string;
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  responseJson : TJsonObject;
  resultsArr : TJsonArray;
  resultObj : TJsonObject;
  vulnsArr : TJsonArray;
  vulnObj : TJsonObject;
  idList : IList<string>;
  cacheJson : string;
  cachedHit : boolean;
  toQueryPurls : IList<string>;
  toQueryIdx : IList<integer>;
  cachedIds : IDictionary<integer, IList<string>>;
  fetchedResponse : string;
begin
  result := false;
  perQueryIds := TCollections.CreateList<IList<string>>;
  for i := 0 to purls.Count - 1 do
    perQueryIds.Add(TCollections.CreateList<string>);

  cachedIds := TCollections.CreateDictionary<integer, IList<string>>;
  toQueryPurls := TCollections.CreateList<string>;
  toQueryIdx := TCollections.CreateList<integer>;

  for i := 0 to purls.Count - 1 do
  begin
    cachedHit := false;
    if Assigned(FCache) then
      cachedHit := FCache.TryGet(purls[i], cOSVSourceName, cacheJson);
    if cachedHit then
    begin
      idList := TCollections.CreateList<string>;
      try
        responseJson := TJsonBaseObject.Parse(cacheJson) as TJsonObject;
        try
          if responseJson.Contains('vulnIds') then
          begin
            vulnsArr := responseJson.A['vulnIds'];
            for j := 0 to vulnsArr.Count - 1 do
              idList.Add(vulnsArr.S[j]);
          end;
        finally
          responseJson.Free;
        end;
        cachedIds[i] := idList;
      except
        on e : Exception do
        begin
          FLogger.Warning('Invalid vuln cache entry for ' + purls[i] + ': ' + e.Message);
          toQueryPurls.Add(purls[i]);
          toQueryIdx.Add(i);
        end;
      end;
    end
    else
    begin
      toQueryPurls.Add(purls[i]);
      toQueryIdx.Add(i);
    end;
  end;

  //Stamp the cached lookups into the per-query result list.
  for i := 0 to perQueryIds.Count - 1 do
  begin
    if cachedIds.ContainsKey(i) then
      perQueryIds[i].AddRange(cachedIds[i].ToArray);
  end;

  if toQueryPurls.Count = 0 then
  begin
    result := true;
    exit;
  end;

  //Build the OSV batch request body.
  reqBody := TJsonObject.Create;
  try
    queriesArr := reqBody.A['queries'];
    for i := 0 to toQueryPurls.Count - 1 do
    begin
      queryObj := queriesArr.AddObject;
      packageObj := queryObj.O['package'];
      packageObj.S['purl'] := toQueryPurls[i];
    end;
    sBody := reqBody.ToJSON(false);
  finally
    reqBody.Free;
  end;

  try
    httpClient := THttpClientFactory.CreateClient(cOSVBaseUri);
    request := httpClient.CreateRequest('/v1/querybatch')
                          .WithHeader(cUserAgentHeader, cDPMUserAgent)
                          .WithHeader(cClientVersionHeader, cDPMClientVersion)
                          .WithBody(sBody, TEncoding.UTF8)
                          .WithContentType('application/json', 'utf-8');
    FLogger.Verbose('POST ' + cOSVBaseUri + '/v1/querybatch (' + IntToStr(toQueryPurls.Count) + ' purls)');
    response := httpClient.Post(request, cancellationToken);
  except
    on e : Exception do
    begin
      FLogger.Error('OSV batch query failed: ' + e.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    FLogger.Error('OSV batch query returned HTTP ' + IntToStr(response.StatusCode) + ': ' + response.ErrorMessage);
    exit;
  end;

  fetchedResponse := response.Response;
  try
    responseJson := TJsonBaseObject.Parse(fetchedResponse) as TJsonObject;
  except
    on e : Exception do
    begin
      FLogger.Error('Could not parse OSV batch response: ' + e.Message);
      exit;
    end;
  end;

  try
    if not responseJson.Contains('results') then
    begin
      FLogger.Warning('OSV batch response missing results[]');
      result := true;
      exit;
    end;
    resultsArr := responseJson.A['results'];
    for i := 0 to toQueryPurls.Count - 1 do
    begin
      if i >= resultsArr.Count then
        break;
      idList := TCollections.CreateList<string>;
      resultObj := resultsArr.O[i];
      if resultObj.Contains('vulns') and (not resultObj.IsNull('vulns')) then
      begin
        vulnsArr := resultObj.A['vulns'];
        for j := 0 to vulnsArr.Count - 1 do
        begin
          vulnObj := vulnsArr.O[j];
          if vulnObj.Contains('id') then
            idList.Add(vulnObj.S['id']);
        end;
      end;
      perQueryIds[toQueryIdx[i]].AddRange(idList.ToArray);

      //Persist this purl's lookup result so the next run hits the cache.
      if Assigned(FCache) then
        FCache.Put(toQueryPurls[i], cOSVSourceName, BuildPurlCacheBody(idList));
    end;
    result := true;
  finally
    responseJson.Free;
  end;
end;

function TOSVDatabase.FetchVulnDetail(const cancellationToken : ICancellationToken;
                                      const vulnId : string;
                                      out advisory : IVulnDatabaseAdvisory) : boolean;
var
  cacheKey : string;
  cacheJson : string;
  cachedHit : boolean;
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  responseJson : TJsonObject;
  osv : TOSVAdvisory;
begin
  result := false;
  advisory := nil;
  if vulnId = '' then
    exit;
  cacheKey := cOSVCacheKeyVulnPrefix + vulnId;
  cachedHit := false;
  if Assigned(FCache) then
    cachedHit := FCache.TryGet(cacheKey, cOSVSourceName, cacheJson);

  if not cachedHit then
  begin
    try
      httpClient := THttpClientFactory.CreateClient(cOSVBaseUri);
      request := httpClient.CreateRequest('/v1/vulns/' + vulnId)
                            .WithHeader(cUserAgentHeader, cDPMUserAgent)
                            .WithHeader(cClientVersionHeader, cDPMClientVersion);
      FLogger.Verbose('GET ' + cOSVBaseUri + '/v1/vulns/' + vulnId);
      response := httpClient.Get(request, cancellationToken);
    except
      on e : Exception do
      begin
        FLogger.Error('OSV vuln detail fetch [' + vulnId + '] failed: ' + e.Message);
        exit;
      end;
    end;

    if response.StatusCode <> 200 then
    begin
      FLogger.Error('OSV vuln detail [' + vulnId + '] returned HTTP ' + IntToStr(response.StatusCode));
      exit;
    end;

    cacheJson := response.Response;
    if Assigned(FCache) then
      FCache.Put(cacheKey, cOSVSourceName, cacheJson);
  end;

  try
    responseJson := TJsonBaseObject.Parse(cacheJson) as TJsonObject;
  except
    on e : Exception do
    begin
      FLogger.Error('Could not parse OSV vuln detail for [' + vulnId + ']: ' + e.Message);
      exit;
    end;
  end;

  try
    osv := TOSVAdvisory.Create;
    osv.LoadFromJson(responseJson);
    if osv.GetId = '' then
      osv.FId := vulnId;  // defensive - shouldn't happen for a valid OSV record
    advisory := osv;
    result := true;
  finally
    responseJson.Free;
  end;
end;

function BuildPurlCacheBody(const ids : IList<string>) : string;
var
  obj : TJsonObject;
  arr : TJsonArray;
  i : integer;
begin
  obj := TJsonObject.Create;
  try
    arr := obj.A['vulnIds'];
    for i := 0 to ids.Count - 1 do
      arr.Add(ids[i]);
    result := obj.ToJSON(false);
  finally
    obj.Free;
  end;
end;

function TOSVDatabase.Query(const cancellationToken : ICancellationToken;
                            const purls : IList<string>) : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
var
  perQueryIds : IList<IList<string>>;
  i, j : integer;
  uniqueIds : IDictionary<string, IVulnDatabaseAdvisory>;
  ids : IList<string>;
  id : string;
  advisory : IVulnDatabaseAdvisory;
  perPurlList : IList<IVulnDatabaseAdvisory>;
begin
  result := TCollections.CreateDictionary<string, IList<IVulnDatabaseAdvisory>>;
  if purls.Count = 0 then
    exit;

  if not FetchBatchIds(cancellationToken, purls, perQueryIds) then
    exit;

  uniqueIds := TCollections.CreateDictionary<string, IVulnDatabaseAdvisory>;
  for i := 0 to perQueryIds.Count - 1 do
  begin
    ids := perQueryIds[i];
    for id in ids do
    begin
      if cancellationToken.IsCancelled then
        exit;
      if uniqueIds.ContainsKey(id) then
        continue;
      if FetchVulnDetail(cancellationToken, id, advisory) then
        uniqueIds[id] := advisory
      else
        uniqueIds[id] := nil;  // remember the miss so we don't retry
    end;
  end;

  for i := 0 to purls.Count - 1 do
  begin
    ids := perQueryIds[i];
    if ids.Count = 0 then
      continue;
    perPurlList := TCollections.CreateList<IVulnDatabaseAdvisory>;
    for j := 0 to ids.Count - 1 do
    begin
      advisory := uniqueIds[ids[j]];
      if advisory <> nil then
        perPurlList.Add(advisory);
    end;
    if perPurlList.Count > 0 then
      result[purls[i]] := perPurlList;
  end;
end;

end.
