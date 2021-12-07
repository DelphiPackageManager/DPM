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

unit DPM.Core.Repository.Http;

interface

uses
  Generics.Defaults,
  VSoft.Awaitable,
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
  protected
    function GetServiceIndex(const cancellationToken : ICancellationToken) : IServiceIndex;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : Boolean;
    function Find(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean) : IPackageIdentity;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
    function GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion;
      const preRelease: Boolean): IDictionary<string, TPackageVersion>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId: string; const packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageSearchResultItem;


    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;

  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  JsonDataObjects,
  VSoft.HttpClient,
  VSoft.URI,
  DPM.Core.Package.Icon,
  DPM.Core.Constants,
  DPM.Core.Package.Metadata,
  DPM.Core.Package.SearchResults,
  DPM.Core.Sources.ServiceIndex,
  DPM.Core.Package.ListItem;

function GetBaseUri(const uri : IUri) : string;
begin
  result := uri.Scheme + '://' + uri.Host;
  if uri.Scheme = 'http' then
  begin
    if uri.Port <> 80 then
       result := result + ':' + IntToStr(uri.Port);
  end
  else if uri.Scheme = 'https' then
  begin
    if uri.Port <> 443 then
       result := result + ':' + IntToStr(uri.Port);
  end;

end;


{ TDPMServerPackageRepository }

constructor TDPMServerPackageRepository.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

function TDPMServerPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : Boolean;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  destFile : string;
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
  path := Format('%s/%s/%s/%s/dpkg', [packageIdentity.Id, CompilerToString(packageIdentity.CompilerVersion), DPMPlatformToString(packageIdentity.Platform),packageIdentity.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest(path);

  destFile := IncludeTrailingPathDelimiter(localFolder) + packageIdentity.ToString + cPackageFileExt;

  request.SaveAsFile := destFile;

  try
    response := request.Get(cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching downloading package from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.StatusCode <> 200 then
  begin
    Logger.Error('Error fetching downloading package from server : ' + response.ErrorMessage);
    exit;
  end;

  fileName := destFile;
  result := true;



end;


function TDPMServerPackageRepository.Find(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform; const includePrerelease : boolean): IPackageIdentity;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  jsonObj : TJsonObject;
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
  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest('')
            .WithAccept('application/json')
            .WithParameter('id', id)
            .WithParameter('compiler', CompilerToString(compilerVersion))
            .WithParameter('platform', DPMPlatformToString(platform));

  if not version.IsEmpty then
    request.WithParameter('version', version.ToStringNoMeta)
  else if includePrerelease then
       request.WithParameter('prerel', LowerCase(BoolToStr(includePrerelease, true)));

  try
    response := request.Get(cancellationToken);
  except
    on ex : Exception do
    begin
      Logger.Error('Error finding package identity from server : ' + ex.Message);
      exit;
    end;
  end;

  //if the package or package version is not on the server this is fine
  if response.StatusCode = 404 then
    exit;

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

function TDPMServerPackageRepository.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  jsonObj : TJsonObject;
  jsonArr : TJsonArray;
  itemObj : TJsonObject;
  i: Integer;

  resultItem : IPackageSearchResultItem;
begin
  Result := TDPMPackageSearchResult.Create(options.Skip, 0);
  serviceIndex := GetServiceIndex(cancelToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageSearch');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageSearch resource from Service Index');
    exit;
  end;

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest('')
    .WithAccept('application/json');

  if options.SearchTerms <> '' then
    request.WithParameter('q', Trim(options.SearchTerms));

  request.WithParameter('compiler', CompilerToString(compilerVersion));
  request.WithParameter('platform', DPMPlatformToString(platform));

  if options.Skip <> 0 then
    request.WithParameter('skip', IntToStr(options.Skip));

  if options.Take <> 0 then
    request.WithParameter('take', IntToStr(options.Take));

  try
    response := request.Get(cancelToken);
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

    if jsonObj.Contains('results') then
    begin
      jsonArr := jsonObj.A['results'];
      for i := 0 to jsonArr.Count -1 do
      begin
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
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  stream : TMemoryStream;
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
  path := Format('/%s/%s/%s/%s/icon', [packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform),packageVersion]);

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest(path);

  try
    response := request.Get(cancelToken);
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

function TDPMServerPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
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
  path := Format('/%s/%s/%s/%s/dspec', [packageId.Id, CompilerToString(packageId.CompilerVersion), DPMPlatformToString(packageId.Platform), packageId.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest(path);

  try
    response := request.Get(cancellationToken);
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
    result := TPackageMetadata.Create(Name, jsonObj);
  except
    on e : Exception do
    begin
      Logger.Error('Error fetching reading json response from server : ' + e.Message);
      exit;
    end;
  end;

end;

function TDPMServerPackageRepository.GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion;
  const preRelease: Boolean): IDictionary<string, TPackageVersion>;
begin
  result := TCollections.CreateDictionary<string, TPackageVersion>;
end;


//Note - we are downloading the dpspec file here rather than hitting the database with the /info endpoint.
//since the dpsec will be on the CDN - this will be less load on the db.
//TODO : profile CD response times vs hitting the server.
function TDPMServerPackageRepository.GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageSearchResultItem;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
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
  path := Format('/%s/%s/%s/%s/info', [packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform), packageVersion]);

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest(path);

  try
    response := request.Get(cancellationToken);
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
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  path : string;
  jsonObj : TJsonObject;
  versionsArr : TJsonArray;
  i : integer;
  sVersion : string;
  packageVersion : TPackageVersion;
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
  path := Format('/%s/%s/%s/versions?includePrerelease=', [id, CompilerToString(compilerVersion), DPMPlatformToString(platform), Lowercase(BoolToStr(preRelease, true))]);


  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := httpClient.CreateRequest(path);


  try
    response := request.Get(cancellationToken);
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
  request : TRequest;
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



  httpClient := THttpClientFactory.CreateClient(GetBaseUri(uri));

  request := httpClient.CreateRequest(path);
  request.WithAccept('application/json')
          //parameters can't have spaces.. winhttp will report invalid header!
         .WithParameter('versionRange', StringReplace(versionRange.ToString, ' ', '', [rfReplaceAll]))
         .WithParameter('prerel', Lowercase(BoolToStr(preRelease, true)));

  try
    response := request.Get(cancellationToken);
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
  request : TRequest;
  response : IHttpResponse;
  uri : IUri;
  sBase : string;
begin
  result := nil;
  uri := TUriFactory.Parse(self.SourceUri);

  sBase := uri.Scheme + '://' + uri.Host;
  if uri.Scheme = 'http' then
  begin
    if uri.Port <> 80 then
       sBase := sBase + ':' + IntToStr(uri.Port);
  end
  else if uri.Scheme = 'https' then
  begin
    if uri.Port <> 443 then
       sBase := sBase + ':' + IntToStr(uri.Port);
  end;


  httpClient := THttpClientFactory.CreateClient(sBase);
  request := httpClient.CreateRequest(uri.AbsolutePath)
            .WithAccept('application/json');
  try
    response := request.Get(cancellationToken);
    if response.StatusCode <> 200 then
    begin
      if response.StatusCode > 0 then
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.StatusCode, response.ErrorMessage]))
      else
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.StatusCode, 'unabled to contact source']));
      exit;
    end;
    result := TServiceIndex.LoadFromString(response.Response);
  except
    on e : Exception do
    begin
      Logger.Error('Error parsing serviceindex json : ' + e.Message);
      exit;
    end;
  end;
end;

function TDPMServerPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
var
  httpClient : IHttpClient;
  request : TRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  baseUri : string;
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

  baseUri := GetBaseUri(uri);


  httpClient := THttpClientFactory.CreateClient(baseUri);

  request := httpClient.CreateRequest(uri.AbsolutePath)
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
    response := request.Get(cancellationToken);
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
  request : TRequest;
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
  httpClient := THttpClientFactory.CreateClient(GetBaseUri(uri));
  request := httpClient.CreateRequest(uri.AbsolutePath);

  request.WithHeader('X-ApiKey', pushOptions.ApiKey);

  request.WithFile(pushOptions.PackagePath);

  response := request.Put(cancellationToken);

  Logger.Information(Format('Package Upload [%d] : %s', [response.StatusCode, response.Response]));

  result := response.StatusCode = 201;
end;

end.

