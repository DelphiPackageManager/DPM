{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Repository.Http;

interface

uses
  Generics.Defaults,
  VSoft.CancellationToken,
  VSoft.HttpClient,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base;


type
  TDPMServerPackageRepository = class(TBaseRepository, IPackageRepository)
  private
    FServiceIndex : IServiceIndex;
    //why the service index could not be fetched, and when - see GetServiceIndex.
    FServiceIndexError : string;
    FServiceIndexFailedAt : TDateTime;
    procedure ServiceIndexFailed(const reason : string);
  protected
    procedure Configure(const source : ISourceConfig); override;
    function GetServiceIndex(const cancellationToken : ICancellationToken) : IServiceIndex;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : Boolean;
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;


    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId: string; const packageVersion: string; const compilerVersion: TCompilerVersion): IPackageSearchResultItem;


    function IsRetryableStatus(const statusCode : integer) : boolean;
    function ComputeRetryDelayMs(const response : IHttpResponse; const attempt : integer; const baseSeconds : integer) : Cardinal;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;

  public
    constructor Create(const logger : ILogger); override;
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  System.DateUtils,
  JsonDataObjects,
  VSoft.URI,
  DPM.Core.Diagnostics,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec,
  DPM.Core.Package.Icon,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Package.SearchResults,
  DPM.Core.Sources.ServiceIndex,
  DPM.Core.Package.ListItem, DPM.Core.Utils.Hash;


const
  cPreReleaseParam = 'prerel';
  cCommercialParam = 'commercial';
  cTrialParam      = 'trial';

  //Default push timeout in seconds when the user does not pass -t/--timeout (Timeout <= 0).
  //Matches the value documented in the console option help. Uploads of large packages can
  //take a while, so this is well above the VSoft.HttpClient 60s default.
  cDefaultPushTimeoutSeconds = 300;

  //a source whose service index could not be fetched isn't asked again for this long - otherwise every
  //request an operation makes waits on it again.
  cServiceIndexRetryMs = 60000;

//winhttp's messages can come with a leading ': '.
function CleanMessage(const value : string) : string;
begin
  result := Trim(value);
  while (result <> '') and (result[1] = ':') do
    result := Trim(Copy(result, 2, MaxInt));
end;

//'404 Not Found - <the start of what the server said>'
function ResponseError(const response : IHttpResponse) : string;
begin
  result := IntToStr(response.StatusCode);
  if response.ErrorMessage <> '' then
    result := result + ' ' + CleanMessage(response.ErrorMessage);
  if response.Response <> '' then
    result := result + ' - ' + Copy(response.Response, 1, 300);
end;

//function GetBaseUri(const uri : IUri) : string;
//begin
//  result := uri.Scheme + '://' + uri.Host;
//  if uri.Scheme = 'http' then
//  begin
//    if uri.Port <> 80 then
//       result := result + ':' + IntToStr(uri.Port);
//  end
//  else if uri.Scheme = 'https' then
//  begin
//    if uri.Port <> 443 then
//       result := result + ':' + IntToStr(uri.Port);
//  end;
//
//end;


{ TDPMServerPackageRepository }

procedure TDPMServerPackageRepository.Configure(const source: ISourceConfig);
begin
  //if the uri changes service index will be invalid.
  if source.Source <> Self.SourceUri then
  begin
    FServiceIndex := nil;
    FServiceIndexError := '';
  end;
  inherited;
end;

constructor TDPMServerPackageRepository.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

destructor TDPMServerPackageRepository.Destroy;
begin
  FServiceIndex := nil;
  inherited;
end;


function TDPMServerPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : Boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  destFile : string;
  uri : IUri;
  stopwatch : TStopwatch;
  hash : string;
  hashFileName : string;
begin
  result := false;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageDownload');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageDownload resource - is it a DPM server?');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/dpkg', [uri.AbsolutePath, packageInfo.Id, CompilerToString(packageInfo.CompilerVersion),  packageInfo.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent);

  ForceDirectories(localFolder);
  //Canonical .dpkg filename is {id}-{compiler}-{binPlatforms}-{version}.dpkg —
  //must match DPM.Core.Packaging.Writer.pas:463 and the cPackageFileRegex used
  //by the cache loader. packageInfo.ToString drops the platforms segment.
  destFile := IncludeTrailingPathDelimiter(localFolder)
    + packageInfo.Id + '-'
    + CompilerToString(packageInfo.CompilerVersion) + '-'
    + DPMPlatformsToBinString(packageInfo.SupportedPlatforms) + '-'
    + packageInfo.Version.ToStringNoMeta + cPackageFileExt;
  hashFileName := destFile + cPackageHashAlgorithmExt;
  request.SaveAsFile := destFile;
  stopwatch := TStopwatch.StartNew;
  Logger.Information('GET ' + uri.BaseUriString + path);
  try
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('download failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('download failed : ' + ResponseError(response));
    exit;
  end;
  stopwatch.Stop;
  Logger.Information('OK ' + uri.BaseUriString + path + ' [' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms]');

  hash := THashSHA256.GetHashStringFromFile(destFile);
  //Only compare when we actually have a hash to compare against. Not every IPackageInfo
  //carries one - an older cache with no .sha256 sidecar and no .dpkg to self-heal from
  //yields Hash = '', as does a feed that doesn't publish the field. Comparing regardless
  //turned those into a bogus "does not match server hash []" failure. Mirrors the
  //directory repo, which only verifies when a .sha256 sidecar exists.
  if packageInfo.Hash = '' then
    Logger.Warning(TWarningCode.W1204_PackageHashUnknown, 'No hash is recorded for [' + packageInfo.ToString + '] - the download was not verified.')
  else if not SameText(hash, packageInfo.Hash) then
  begin
    RequestFailed('sent a package that does not match its recorded hash (sha256 ' + hash + ', expected ' + packageInfo.Hash + ')', TErrorCode.E1202_PackageHashMismatch);
    exit(false);
  end;
  //Persist the sidecar so tools that consume the cache (SBOM, audits, signature
  //verification) can read it without recomputing. The directory repo writes this
  //on push - HTTP downloads went without it and any consumer just had to recompute.
  try
    TFile.WriteAllText(hashFileName, hash);
  except
    on e : Exception do
      Logger.Warning(TWarningCode.W1205_HashFileWriteFailed, 'Unable to write the hash file [' + hashFileName + '] : ' + e.Message);
  end;


  request := nil;
  response := nil;
  httpClient := nil;

  fileName := destFile;
  result := true;
end;


function TDPMServerPackageRepository.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const includePrerelease : boolean): IPackageInfo;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  jsonObj : TJsonObject;
  uri : IUri;
  requestUrl : string;
begin
  result := nil;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageFindLatest');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageFindLatest resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);


  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithAccept('application/json')
                       .WithParameter('id', id)
                       .WithParameter('compiler', CompilerToString(compilerVersion));

  //this looks odd but if we specify a version then we want that version only!
  if not version.IsEmpty then
    request.WithParameter('version', version.ToStringNoMeta)
  else if includePrerelease then
    request.WithParameter(cPreReleaseParam, LowerCase(BoolToStr(includePrerelease, true)));

  requestUrl := httpClient.GetRequestUrl(request);
  try
    Logger.Information('GET ' + requestUrl);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  //if the package or package version is not on the server this is fine
  if response.StatusCode = 404 then
  begin
    Logger.Information('NOTFOUND ' + requestUrl);
    exit;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content');
    exit;
  end;
  Logger.Information('OK ' + requestUrl);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;
  try
    TPackageInfo.TryLoadFromJson(Logger, jsonObj, Self.Name, Result);
  finally
    jsonObj.Free;
  end;
end;

function TDPMServerPackageRepository.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  jsonObj : TJsonObject;
  jsonArr : TJsonArray;
  itemObj : TJsonObject;
  i: Integer;
  uri : IUri;
  resultItem : IPackageSearchResultItem;
  requestUrl : string;
begin
  Result := TDPMPackageSearchResult.Create(options.Skip, 0);
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageSearch');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageSearch resource - is it a DPM server?');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithAccept('application/json');

  if options.SearchTerms <> '' then
    request.WithParameter('q', Trim(options.SearchTerms));

  request.WithParameter('compiler', CompilerToString(compilerVersion));
  request.WithParameter(cPreReleaseParam, LowerCase(BoolToStr(options.Prerelease, true)));
  request.WithParameter(cCommercialParam, LowerCase(BoolToStr(options.Commercial, true)));
  request.WithParameter(cTrialParam, LowerCase(BoolToStr(options.Trial, true)));


  if options.Skip <> 0 then
    request.WithParameter('skip', IntToStr(options.Skip));

  if options.Take <> 0 then
    request.WithParameter('take', IntToStr(options.Take));

  requestUrl := httpClient.GetRequestUrl(request);
  try
    Logger.Information('GET ' + requestUrl);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content');
    exit;
  end;

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  try
    Result.TotalCount := jsonObj.I['totalHits'];

    if jsonObj.Contains('results') and (not jsonObj.IsNull('results')) then
    begin
      jsonArr := jsonObj.A['results'];
      for i := 0 to jsonArr.Count -1 do
      begin
        if cancellationToken.IsCancelled then
          exit;
        itemObj := jsonArr.O[i];
        resultItem := TDPMPackageSearchResultItem.FromJson(Name, itemObj);
        Result.Results.Add(resultItem);
      end;

    end;
  finally
    jsonObj.Free;
  end;

end;

function TDPMServerPackageRepository.GetPackageFeedByIds(const cancellationToken: ICancellationToken; const ids: IList<IPackageIdentity>; const compilerVersion: TCompilerVersion): IPackageSearchResult;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uri : IUri;
  jsonObj : TJsonObject;
  itemObj : TJsonObject;
  idArray : TJsonArray;
  jsonArr : TJsonArray;
  sBody : string;
  jsonId : TJsonObject;
  i: Integer;
  resultItem : IPackageSearchResultItem;
begin
  Result := TDPMPackageSearchResult.Create(0, 0);
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;
  serviceItem := serviceIndex.FindItem('PackageSearchIds');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageSearchIds resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  jsonObj := TJsonObject.Create;
  try
    jsonObj.S['compiler'] := CompilerToString(compilerVersion);
    idArray := jsonObj.A['packageids'];

    for i := 0 to ids.Count -1 do
    begin
      jsonId := idArray.AddObject;
      jsonId.S['id'] := ids.Items[i].Id;
      jsonId.S['version'] := ids.Items[i].Version.ToStringNoMeta();
    end;
    sBody := jsonObj.ToJSON(false);
  finally
    jsonObj.Free;
  end;

  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithBody(sBody, TEncoding.UTF8)
                       .WithContentType('application/json', 'utf-8');

  try
    Logger.Verbose('POST ' + uri.ToString);
    response := httpClient.Post(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;


  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content');
    exit;
  end;
  Logger.Verbose('OK ' + uri.ToString);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  try
    Result.TotalCount := jsonObj.I['totalHits'];

    if jsonObj.Contains('results') and (not jsonObj.IsNull('results')) then
    begin
      jsonArr := jsonObj.A['results'];
      for i := 0 to jsonArr.Count -1 do
      begin
        if cancellationToken.IsCancelled then
          exit;

        itemObj := jsonArr.O[i];
        resultItem := TDPMPackageSearchResultItem.FromJson(Name, itemObj);
        Result.Results.Add(resultItem);
      end;

    end;
  finally
    jsonObj.Free;
  end;

end;

function TDPMServerPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  stream : TMemoryStream;
  uri : IUri;
begin
  result := nil;
  serviceIndex := GetServiceIndex(cancelToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageIcon');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageIcon resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);
  path := Format('%s/%s/%s/%s/icon', [uri.AbsolutePath, packageId, CompilerToString(compilerVersion), packageVersion]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent);
  try
    response := httpClient.Get(request, cancelToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content for icon');
    exit;
  end;


  if SameText(response.ContentType, 'image/png') then
  begin
    stream := TMemoryStream.Create;
    response.ResponseStream.Seek(0, soFromBeginning);
    stream.CopyFrom(response.ResponseStream,response.ContentLength);
    result := CreatePackageIcon(TPackageIconKind.ikPng, stream);
  end
  else if SameText(response.ContentType, 'image/svg+xml') then
  begin
    stream := TMemoryStream.Create;
    stream.CopyFrom(response.ResponseStream,response.ContentLength);
    result := CreatePackageIcon(TPackageIconKind.ikSvg, stream);
  end;


end;

function TDPMServerPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
  uri : IUri;
begin
  result := nil;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageInfo');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageInfo resource - is it a DPM server?');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/info', [uri.AbsolutePath, packageId.Id, CompilerToString(packageId.CompilerVersion), packageId.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cAcceptHeader, 'application/json');

  try
    Logger.Information('GET ' + uri.BaseUriString + path);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode = 404 then
  begin
    //not an error!
    Logger.Information('Package ' + packageId.ToString + ' not found on server');
    exit;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    RequestFailed('returned an empty response for ' + uri.BaseUriString + path);
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  if not TPackageInfo.TryLoadFromJson(Logger, jsonObj, Self.Name, result) then
    result := nil;

end;


//Note - we are downloading the dpspec file here rather than hitting the database with the /info endpoint.
//since the dpsec will be on the CDN - this will be less load on the db.
//TODO : profile CDN response times vs hitting the server.
function TDPMServerPackageRepository.GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion): IPackageSearchResultItem;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
  uri : IUri;
begin
  Logger.Debug('TDPMServerPackageRepository.GetPackageMetaData');
  result := nil;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

    serviceItem := serviceIndex.FindItem('PackageMetadata');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageMetadata resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/info', [uri.AbsolutePath, packageId, CompilerToString(compilerVersion), packageVersion]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion);

  Logger.Information('GET ' + uri.BaseUriString + path);

  try
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  try
    result := TDPMPackageSearchResultItem.FromJson(Name, jsonObj);
    Logger.Information('OK ' + uri.BaseUriString + path);
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;
end;

function TDPMServerPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
  versionsArr : TJsonArray;
  i : integer;
  sVersion : string;
  packageVersion : TPackageVersion;
  uri : IUri;
  requestUrl : string;
begin
  result := TCollections.CreateList<TPackageVersion>;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageVersions');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageVersions resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/versions', [uri.AbsolutePath, id, CompilerToString(compilerVersion)]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithParameter(cPreReleaseParam, Lowercase(BoolToStr(preRelease, true)));

  requestUrl := httpClient.GetRequestUrl(request);
  try
    Logger.Information('GET ' + requestUrl);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;
  Logger.Information('OK ' + requestUrl);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  if not jsonObj.Contains('versions') then
    exit;

  versionsArr := jsonObj.A['versions'];

  for i := 0 to versionsArr.Count -1 do
  begin
    sVersion := versionsArr.Items[i].Value;
    if TPackageVersion.TryParse(sVersion, packageVersion) then
      result.Add(packageVersion);
  end;


end;

function TDPMServerPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
  versionsArr : TJsonArray;
  versionObj : TJsonObject;
  i: Integer;
  packageInfo : IPackageInfo;
  uri : IUri;
  stopwatch : TStopwatch;
  requestUrl : string;
begin
  result := TCollections.CreateList<IPackageInfo>;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageVersionsWithDeps');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageVersionsWithDeps resource - is it a DPM server?');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/versionswithdependencies', [uri.AbsolutePath, id, CompilerToString(compilerVersion)]);


  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                        .WithHeader(cUserAgentHeader,cDPMUserAgent)
                        .WithHeader(cClientVersionHeader,cDPMClientVersion)
                        .WithAccept('application/json')
                        //parameters can't have spaces.. winhttp will report invalid header!
                        .WithParameter('versionRange', StringReplace(versionRange.ToString, ' ', '', [rfReplaceAll]))
                        .WithParameter('prerel', Lowercase(BoolToStr(preRelease, true)));

  //log the fully assembled url (including the encoded query string) so failures are diagnosable
  requestUrl := httpClient.GetRequestUrl(request);

  stopwatch := TStopwatch.StartNew;
  try
    Logger.Information('GET ' + requestUrl);
    response := httpClient.Get(request, cancellationToken);
    stopwatch.Stop;
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;
  Logger.Information('OK ' + requestUrl + ' [' + IntTostr(stopwatch.ElapsedMilliseconds) + 'ms]');

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + e.Message);
      exit;
    end;
  end;

  if not jsonObj.Contains('versions') then
    exit;

  versionsArr := jsonObj.A['versions'];

  for i := 0 to versionsArr.Count -1 do
  begin
    versionObj := versionsArr.O[i];
    if TPackageInfo.TryLoadFromJson(Logger, versionObj, Name, packageInfo) then
      result.Add(packageInfo)
  end;

  //TODO : should make sure the server returns a sorted result.
  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := right.Version.CompareTo(left.Version); ;
    end));
end;



function TDPMServerPackageRepository.GetServiceIndex(const cancellationToken: ICancellationToken): IServiceIndex;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  uri : IUri;
begin
  result := FServiceIndex;
  if result <> nil then
    exit;
  //a source that could not be reached a moment ago isn't asked again straight away.
  if (FServiceIndexError <> '') and (MilliSecondsBetween(Now, FServiceIndexFailedAt) < cServiceIndexRetryMs) then
  begin
    RequestFailed(FServiceIndexError);
    exit;
  end;
  uri := TUriFactory.Parse(self.SourceUri);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);
  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithAccept('application/json');

  try
    response := httpClient.Get(request, cancellationToken);
  except
    on e : Exception do
    begin
      if not cancellationToken.IsCancelled then
        ServiceIndexFailed('could not be reached : ' + CleanMessage(e.Message));
      exit;
    end;
  end;
  if cancellationToken.IsCancelled then
    exit;
  if response.StatusCode <> 200 then
  begin
    if response.StatusCode > 0 then
      ServiceIndexFailed('could not be reached : ' + ResponseError(response))
    else
      ServiceIndexFailed('could not be reached : ' + CleanMessage(response.ErrorMessage));
    exit;
  end;
  try
    result := TServiceIndex.LoadFromString(response.Response);
  except
    on e : Exception do
    begin
      ServiceIndexFailed('returned a service index DPM could not read : ' + e.Message);
      exit;
    end;
  end;
  FServiceIndex := result;
  FServiceIndexError := '';
end;

procedure TDPMServerPackageRepository.ServiceIndexFailed(const reason : string);
begin
  FServiceIndexError := reason;
  FServiceIndexFailedAt := Now;
  RequestFailed(reason);
end;

function TDPMServerPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uri : IUri;
  jsonObj : TJsonObject;
  jsonArr : TJsonArray;

  listItem : IPackageListItem;
  I: Integer;
  requestUrl : string;

begin
  result := TCollections.CreateList<IPackageListItem>;
  if cancellationToken.IsCancelled then
    exit;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageList');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackageList resource - is it a DPM server?');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithAccept('application/json');

  if options.SearchTerms <> '' then
    request.WithParameter('q', options.SearchTerms);

  if options.Prerelease then
    request.WithParameter('prerel', 'true');

  if not options.Commercial then
    request.WithParameter('commercial','false');

  if not options.Trial then
    request.WithParameter('trial','false');

  if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
    request.WithParameter('compiler',  CompilerToString(options.CompilerVersion));

  if options.Platforms  <> [] then
    request.WithParameter('platforms',  DPMPlatformsToString(options.Platforms));

  if options.Skip <> 0 then
    request.WithParameter('skip', IntToStr(options.Skip));

  if options.Take <> 0 then
    request.WithParameter('take', IntToStr(options.Take));


  requestUrl := httpClient.GetRequestUrl(request);
  try
    Logger.Information('GET ' + requestUrl);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      RequestFailed('request failed : ' + CleanMessage(ex.Message));
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    RequestFailed('returned an error : ' + ResponseError(response));
    exit;
  end;
  Logger.Information('OK ' + requestUrl);

  try
    jsonObj := TJsonObject.Parse(response.Response) as TJsonObject;
  except
    on ex : Exception do
    begin
      RequestFailed('returned a response DPM could not read : ' + ex.Message);
      exit;
    end;
  end;

  jsonArr := jsonObj.A['results'];
  if jsonArr.Count > 0 then
  begin
    for I := 0 to jsonArr.Count -1 do
    begin
      if TPackageListItem.TryLoadFromJson(Logger, jsonArr.O[i], listItem) then
        Result.Add(listItem)
      else
        exit; //error would have been logged in the tryloadfromjson
    end;

  end;

end;


function TDPMServerPackageRepository.IsRetryableStatus(const statusCode : integer) : boolean;
begin
  //429 = Too Many Requests (rate limiting), 502/503/504 = transient gateway/unavailable errors.
  result := (statusCode = 429) or (statusCode = 502) or (statusCode = 503) or (statusCode = 504);
end;

function TDPMServerPackageRepository.ComputeRetryDelayMs(const response : IHttpResponse; const attempt : integer; const baseSeconds : integer) : Cardinal;
const
  cMaxRetryDelaySeconds = 60;
  cMinRetryDelayMs = 1000;
var
  retryAfter : string;
  retryAfterSeconds : integer;
  delaySeconds : integer;
  delayMs : Int64;
  exponent : integer;
begin
  delaySeconds := -1;

  //Honor the server's Retry-After header when it is the common integer-seconds form.
  //The HTTP-date form is not parsed here - we fall back to exponential backoff for it.
  retryAfter := Trim(response.Headers.Values['Retry-After']);
  if retryAfter <> '' then
  begin
    retryAfterSeconds := StrToIntDef(retryAfter, -1);
    if retryAfterSeconds >= 0 then
      delaySeconds := retryAfterSeconds;
  end;

  if delaySeconds < 0 then
  begin
    //Exponential backoff : baseSeconds * 2^(attempt-1).
    exponent := attempt - 1;
    if exponent < 0 then
      exponent := 0;
    //Cap the exponent so the shift cannot overflow before clamping below.
    if exponent > 16 then
      exponent := 16;
    delaySeconds := baseSeconds * (1 shl exponent);
  end;

  if delaySeconds > cMaxRetryDelaySeconds then
    delaySeconds := cMaxRetryDelaySeconds;

  delayMs := Int64(delaySeconds) * 1000;

  //Add jitter (up to 1s) to avoid a thundering herd when no Retry-After was given.
  if retryAfter = '' then
    delayMs := delayMs + Random(1000);

  if delayMs < cMinRetryDelayMs then
    delayMs := cMinRetryDelayMs;

  result := Cardinal(delayMs);
end;

function TDPMServerPackageRepository.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uri : IUri;
  fileStream : TFileStream;
  fileName : string;
  jsonObj : TJsonObject;
  packageVersionId : string;
  status : string;
  statusUrl : string;
  errorMsg : string;
  attempt : integer;
  delayMs : Cardinal;
  timeoutSeconds : integer;
  timeoutMs : integer;
begin
  result := false;
  if pushOptions.ApiKey = '' then
  begin
    Logger.Error(TErrorCode.E0101_MissingArgument, 'An api key is needed to push to [' + Name + '] - pass --apiKey, or set the DPM_API_KEY environment variable.');
    exit;
  end;

  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    Logger.Error(TErrorCode.E3004_PackageFileNotFound, 'The package file [' + pushOptions.PackagePath + '] does not exist.');
    exit;
  end;


  if cancellationToken.IsCancelled then
    exit;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackagePublish');
  if serviceItem = nil then
  begin
    RequestFailed('does not provide the PackagePublish resource - is it a DPM server?');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);
  fileName := TPath.GetFileName(pushOptions.PackagePath);

  //todo: Add BaseUri to Uri class
  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  //Apply the configured push timeout. pushOptions.Timeout is in seconds and defaults to -1
  //(unset) - fall back to cDefaultPushTimeoutSeconds in that case. WinHttp timeouts are in
  //milliseconds. Setting them on the client means the request created below inherits them at
  //construction (TRequest copies the client's Send/Response timeouts). ResponseTimeout drives
  //the overall async wait, and SendTimeout covers uploading the package body.
  timeoutSeconds := pushOptions.Timeout;
  if timeoutSeconds <= 0 then
    timeoutSeconds := cDefaultPushTimeoutSeconds;
  timeoutMs := timeoutSeconds * 1000;
  httpClient.SendTimeout := timeoutMs;
  httpClient.ResponseTimeout := timeoutMs;

  attempt := 0;
  repeat
    if cancellationToken.IsCancelled then
      exit;

    // (Re)create the body stream and request inside the loop - WithBody(stream, true)
    // transfers stream ownership and the stream is consumed by the PUT, so it cannot be reused on retry.
    fileStream := TFileStream.Create(pushOptions.PackagePath, fmOpenRead or fmShareDenyWrite);
    request := httpClient.CreateRequest(uri.AbsolutePath)
                         .WithHeader(cUserAgentHeader, cDPMUserAgent)
                         .WithHeader(cClientVersionHeader, cDPMClientVersion)
                         .WithHeader('X-ApiKey', pushOptions.ApiKey)
                         .WithHeader('X-DPM-Filename', fileName)
                         .WithContentType('application/octet-stream')
                         .WithBody(fileStream, true);

    if pushOptions.Unlisted then
      //WithQueryParameter (not WithParameter) - this is a PUT with a file body, and
      //WithParameter would send params as the form body / drop them rather than putting
      //them on the url query string. WithQueryParameter always appends to the query string.
      request.WithQueryParameter('unlisted', 'true');

    try
      response := httpClient.Put(request, cancellationToken);
    except
      //the repository manager reports it.
      on e : Exception do
      begin
        if not cancellationToken.IsCancelled then
          RequestFailed('push request failed : ' + CleanMessage(e.Message));
        exit;
      end;
    end;

    if IsRetryableStatus(response.StatusCode) and (attempt < pushOptions.MaxRetries) then
    begin
      Inc(attempt);
      delayMs := ComputeRetryDelayMs(response, attempt, pushOptions.RetryDelay);
      Logger.Information(Format('The server returned [%d] - retrying the push in %d s (attempt %d of %d)...',
        [response.StatusCode, delayMs div 1000, attempt, pushOptions.MaxRetries]), true);
      //cancellable wait - returns wrSignaled if the token is cancelled during the wait.
      if cancellationToken.WaitFor(delayMs) = wrSignaled then
        exit;
      Continue;
    end;
    Break;
  until False;

  case response.StatusCode of
    202:
      begin
        packageVersionId := '';
        status := '';
        statusUrl := '';
        try
          jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
          try
            if jsonObj.Contains('packageVersionId') then
              packageVersionId := jsonObj.S['packageVersionId'];
            if jsonObj.Contains('status') then
              status := jsonObj.S['status'];
            if jsonObj.Contains('statusUrl') then
              statusUrl := jsonObj.S['statusUrl'];
          finally
            jsonObj.Free;
          end;
        except
          on e : Exception do
            Logger.Verbose('Could not parse 202 response body : ' + e.Message);
        end;

        Logger.Information('Package accepted for processing.');
        if packageVersionId <> '' then
          Logger.Information('  packageVersionId : ' + packageVersionId);
        if status <> '' then
          Logger.Information('  status           : ' + status);
        if statusUrl <> '' then
          Logger.Information('  statusUrl        : ' + statusUrl);
        result := true;
      end;
    409:
      begin
        errorMsg := '';
        status := '';
        try
          jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
          try
            if jsonObj.Contains('error') then
              errorMsg := jsonObj.S['error'];
            if jsonObj.Contains('status') then
              status := jsonObj.S['status'];
          finally
            jsonObj.Free;
          end;
        except
          // body not JSON or empty - errorMsg stays blank, fallback below
        end;
        if errorMsg = '' then
          errorMsg := 'package version already exists';

        if pushOptions.SkipDuplicate then
        begin
          if status <> '' then
            Logger.Information('Package version already exists on server, skipping (existing status: ' + status + ').')
          else
            Logger.Information('Package version already exists on server, skipping.');
          result := true;
        end
        else
        begin
          if status <> '' then
            Logger.Error(TErrorCode.E6201_PushRejected, Format('The server [%s] rejected [%s] : 409 %s (existing status: %s)', [Name, ExtractFileName(pushOptions.PackagePath), errorMsg, status]))
          else
            Logger.Error(TErrorCode.E6201_PushRejected, Format('The server [%s] rejected [%s] : 409 %s', [Name, ExtractFileName(pushOptions.PackagePath), errorMsg]));
        end;
      end;
  else
    errorMsg := '';
    try
      jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
      try
        if jsonObj.Contains('error') then
          errorMsg := jsonObj.S['error'];
      finally
        jsonObj.Free;
      end;
    except
      // body not JSON or empty - fall back to raw response below
    end;
    if errorMsg = '' then
      errorMsg := response.Response;
    Logger.Error(TErrorCode.E6201_PushRejected, Format('The server [%s] rejected [%s] : %d %s', [Name, ExtractFileName(pushOptions.PackagePath), response.StatusCode, errorMsg]));
  end;
end;

initialization
  //seed the RNG so retry backoff jitter varies between processes (avoids a thundering herd).
  Randomize;

end.

