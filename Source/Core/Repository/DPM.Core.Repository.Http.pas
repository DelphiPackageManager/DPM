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
  protected
    procedure Configure(const source : ISourceConfig); override;
    function GetServiceIndex(const cancellationToken : ICancellationToken) : IServiceIndex;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : Boolean;
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean) : IPackageIdentity;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) :  IPackageSearchResult;


    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId: string; const packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageSearchResultItem;


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
  System.Diagnostics,
  JsonDataObjects,
  VSoft.HttpClient,
  VSoft.URI,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec,
  DPM.Core.Package.Icon,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Package.SearchResults,
  DPM.Core.Sources.ServiceIndex,
  DPM.Core.Package.ListItem;


const
  cPreReleaseParam = 'prerel';
  cCommercialParam = 'commercial';
  cTrialParam      = 'trial';

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
    FServiceIndex := nil;
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

function TDPMServerPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : Boolean;
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
begin
  result := false;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageDownload');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageDownload resource from Service Index');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/%s/dpkg', [uri.AbsolutePath, packageIdentity.Id, CompilerToString(packageIdentity.CompilerVersion), DPMPlatformToString(packageIdentity.Platform),packageIdentity.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent);

  ForceDirectories(localFolder);
  destFile := IncludeTrailingPathDelimiter(localFolder) + packageIdentity.ToString + cPackageFileExt;

  request.SaveAsFile := destFile;
  stopwatch := TStopwatch.StartNew;
  Logger.Information('GET ' + uri.BaseUriString + path);
  try
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error downloading package from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error downloading package from server : ' + response.ErrorMessage);
    exit;
  end;
  stopwatch.Stop;
  Logger.Information('OK ' + uri.BaseUriString + path + ' [' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms]');

  request := nil;
  response := nil;
  httpClient := nil;

  fileName := destFile;
  result := true;
end;


function TDPMServerPackageRepository.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform; const includePrerelease : boolean): IPackageIdentity;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  jsonObj : TJsonObject;
  uri : IUri;
begin
  result := nil;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageFind');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageDownload resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);


  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithAccept('application/json')
                       .WithParameter('id', id)
                       .WithParameter('compiler', CompilerToString(compilerVersion))
                       .WithParameter('platform', DPMPlatformToString(platform));

  //this looks odd but if we specify a version then we want that version only!
  if not version.IsEmpty then
    request.WithParameter('version', version.ToStringNoMeta)
  else if includePrerelease then
    request.WithParameter(cPreReleaseParam, LowerCase(BoolToStr(includePrerelease, true)));
  try
    Logger.Information('GET ' + uri.BaseUriString);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error finding package identity from server : ' + ex.Message);
      exit;
    end;
  end;

  //if the package or package version is not on the server this is fine
  if response.StatusCode = 404 then
  begin
    Logger.Information('NOTFOUND ' + uri.BaseUriString);
    exit;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching list from server : ' + response.ErrorMessage);
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content');
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
      exit;
    end;
  end;
  try
    TPackageIdentity.TryLoadFromJson(Logger, jsonObj, Self.Name, Result);
  finally
    jsonObj.Free;
  end;


end;

function TDPMServerPackageRepository.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
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
begin
  Result := TDPMPackageSearchResult.Create(options.Skip, 0);
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageSearch');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageSearch resource from Service Index');
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
  request.WithParameter('platform', DPMPlatformToString(platform));
  request.WithParameter(cPreReleaseParam, LowerCase(BoolToStr(options.Prerelease, true)));
  request.WithParameter(cCommercialParam, LowerCase(BoolToStr(options.Commercial, true)));
  request.WithParameter(cTrialParam, LowerCase(BoolToStr(options.Trial, true)));


  if options.Skip <> 0 then
    request.WithParameter('skip', IntToStr(options.Skip));

  if options.Take <> 0 then
    request.WithParameter('take', IntToStr(options.Take));

  try
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching search result from server : ' + response.ErrorMessage);
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
      Logger.Error('Error parsing json response from server : ' + e.Message);
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

function TDPMServerPackageRepository.GetPackageFeedByIds(const cancellationToken: ICancellationToken; const ids: IList<IPackageIdentity>; const compilerVersion: TCompilerVersion;
  const platform: TDPMPlatform): IPackageSearchResult;
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
    Logger.Error('Unabled to determine PackageSearchIds resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  jsonObj := TJsonObject.Create;
  try
    jsonObj.S['compiler'] := CompilerToString(compilerVersion);
    jsonObj.S['platform'] := DPMPlatformToString(platform);
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
    Logger.Information('POST ' + uri.ToString);
    response := httpClient.Post(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;


  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching search result from server : ' + response.ErrorMessage);
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Verbose('Server returned no content');
    exit;
  end;
  Logger.Information('OK ' + uri.ToString);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
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

function TDPMServerPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
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

  serviceItem := serviceIndex.FindItem('PackageDownload');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageDownload resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);
  path := Format('%s/%s/%s/%s/%s/icon', [uri.AbsolutePath, packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform),packageVersion]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent);
  try
    response := httpClient.Get(request, cancelToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching list from server : ' + response.ErrorMessage);
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
  spec : IPackageSpec;
begin
  result := nil;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageInfo');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageInfo resource from Service Index');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/%s/dspec', [uri.AbsolutePath, packageId.Id, CompilerToString(packageId.CompilerVersion), DPMPlatformToString(packageId.Platform), packageId.Version.ToStringNoMeta]);

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
      Logger.Error('Error fetching packageinfo from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching packageinfo from server : ' + response.ErrorMessage);
    exit;
  end;

  if response.ContentLength = 0 then
  begin
    Logger.Error('Server returned empty dspec from : ' + uri.BaseUriString + path);
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
      exit;
    end;
  end;

  try
    spec := TSpec.Create(Logger,'');
    if not spec.LoadFromJson(jsonObj) then
    begin
      Logger.Error('Http Repository : Error reading dspec');
      exit;
    end;
    result := TPackageInfo.CreateFromSpec(Self.Name, spec);
  except
    on e : Exception do
    begin
      Logger.Error('Error fetching reading json response from server : ' + e.Message);
      exit;
    end;
  end;

end;


//Note - we are downloading the dpspec file here rather than hitting the database with the /info endpoint.
//since the dpsec will be on the CDN - this will be less load on the db.
//TODO : profile CDN response times vs hitting the server.
function TDPMServerPackageRepository.GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageSearchResultItem;
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

    serviceItem := serviceIndex.FindItem('PackageMetadata');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageMetadata resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/%s/info', [uri.AbsolutePath, packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform), packageVersion]);

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
      Logger.Error('Error fetching packageinfo from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching packageinfo from server : ' + response.ErrorMessage);
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
      exit;
    end;
  end;

  try
    result := TDPMPackageSearchResultItem.FromJson(Name, jsonObj);
    Logger.Information('OK ' + uri.BaseUriString + path);
  except
    on e : Exception do
    begin
      Logger.Error('Error deserializing json response from server : ' + e.Message);
      exit;
    end;
  end;
end;

function TDPMServerPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>;
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
begin
  result := TCollections.CreateList<TPackageVersion>;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageVersions');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageVersions resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/versions', [uri.AbsolutePath, id, CompilerToString(compilerVersion), DPMPlatformToString(platform)]);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithParameter(cPreReleaseParam, Lowercase(BoolToStr(preRelease, true)));

  try
    Logger.Information('GET ' + uri.BaseUriString + path);
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching versions from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching versions from server : ' + response.ErrorMessage);
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path);

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
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

function TDPMServerPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
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
begin
  result := TCollections.CreateList<IPackageInfo>;

  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageVersionsWithDeps');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageVersionsWithDeps resource from Service Index');
    exit;
  end;
  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  path := Format('%s/%s/%s/%s/versionswithdependencies', [uri.AbsolutePath, id, CompilerToString(compilerVersion), DPMPlatformToString(platform)]);


  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);

  request := httpClient.CreateRequest(path)
                        .WithHeader(cUserAgentHeader,cDPMUserAgent)
                        .WithHeader(cClientVersionHeader,cDPMClientVersion)
                        .WithAccept('application/json')
                        //parameters can't have spaces.. winhttp will report invalid header!
                        .WithParameter('versionRange', StringReplace(versionRange.ToString, ' ', '', [rfReplaceAll]))
                        .WithParameter('prerel', Lowercase(BoolToStr(preRelease, true)));

  stopwatch := TStopwatch.StartNew;
  try
    Logger.Information('GET ' + uri.BaseUriString + path);
    response := httpClient.Get(request, cancellationToken);
    stopwatch.Stop;
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching packageinfo from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching packageinfo from server : ' + response.ErrorMessage);
    exit;
  end;
  Logger.Information('OK ' + uri.BaseUriString + path + ' [' + IntTostr(stopwatch.ElapsedMilliseconds) + 'ms]');

  try
    jsonObj := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing json response from server : ' + e.Message);
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
  uri := TUriFactory.Parse(self.SourceUri);

  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);
  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithAccept('application/json');

  try
//    Logger.Debug('getting service Index ' + self.SourceUri);
    response := httpClient.Get(request, cancellationToken);
    if response.StatusCode <> 200 then
    begin
      if response.StatusCode > 0 then
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.StatusCode, response.ErrorMessage]))
      else
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.StatusCode, 'unabled to contact source']));
      exit;
    end;
    result := TServiceIndex.LoadFromString(response.Response);
    FServiceIndex := result;
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing serviceindex json : ' + e.Message);
      Logger.Error(e.ClassName);
      if e.InnerException <> nil then
        Logger.Error(e.InnerException.Message);
      exit;
    end;
  end;
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
    Logger.Error('Unabled to determine PackageList resource from Service Index');
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


  try
    response := httpClient.Get(request, cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching list from server : ' + response.ErrorMessage);
    exit;
  end;

  try
    jsonObj := TJsonObject.Parse(response.Response) as TJsonObject;
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching parsing json response server : ' + ex.Message);
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


function TDPMServerPackageRepository.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uri : IUri;
begin
  result := false;
  if pushOptions.ApiKey = '' then
  begin
    Logger.Error('ApiKey arg required for remote push');
  end;

  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    Logger.Error('Package file [' + pushOptions.PackagePath + '] not found.');
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
    Logger.Error('Unabled to determine PackagePublish resource from Service Index');
    exit;
  end;

  uri := TUriFactory.Parse(serviceItem.ResourceUrl);

  //todo: Add BaseUri to Uri class
  httpClient := THttpClientFactory.CreateClient(uri.BaseUriString);
  request := httpClient.CreateRequest(uri.AbsolutePath)
                       .WithHeader(cUserAgentHeader,cDPMUserAgent)
                       .WithHeader(cClientVersionHeader,cDPMClientVersion)
                       .WithHeader('X-ApiKey', pushOptions.ApiKey)
                       .WithFile(pushOptions.PackagePath);

  response := httpClient.Put(request, cancellationToken);

  Logger.Information(Format('Package Upload [%d] : %s', [response.StatusCode, response.Response]));

  result := response.StatusCode = 201;
end;

end.

