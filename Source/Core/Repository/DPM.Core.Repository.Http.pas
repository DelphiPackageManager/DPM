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
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
    function GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion;
      const preRelease: Boolean): IDictionary<string, TPackageVersion>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageIdentity>; overload;
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration) : IList<IPackageSearchResultItem>;
    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;


    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;

  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  JsonDataObjects,
  VSoft.HttpClient,
  DPM.Core.Package.Metadata,
  DPM.Core.Sources.ServiceIndex;

{ TDPMServerPackageRepository }

constructor TDPMServerPackageRepository.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

function TDPMServerPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : Boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uriTemplate : string;
  path : string;
begin
  result := false;
  serviceIndex := GetServiceIndex(cancellationToken);
  if serviceIndex = nil then
    exit;

  serviceItem := serviceIndex.FindItem('PackageDownloadTemplate');
  if serviceItem = nil then
  begin
    Logger.Error('Unabled to determine PackageDownloadTemplate resource from Service Index');
    exit;
  end;
  path := Format('/%s/%s/%s/%s/dpkg', [packageIdentity.Id, CompilerToString(packageIdentity.CompilerVersion), DPMPlatformToString(packageIdentity.Platform),packageIdentity.Version.ToStringNoMeta]);

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := THttpClientFactory.CreateRequest(path);

  try
    response := httpClient.Get(request);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.ResponseCode <> 200 then
  begin
    Logger.Error('Error fetching list from server : ' + response.ErrorMessage);
    exit;
  end;

  response.ResponseStream

end;


function TDPMServerPackageRepository.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration) : IList<IPackageSearchResultItem>;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;
end;

function TDPMServerPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
begin
  result := nil;
end;

function TDPMServerPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
begin
  result := nil;
end;

function TDPMServerPackageRepository.GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion;
  const preRelease: Boolean): IDictionary<string, TPackageVersion>;
begin
  result := TCollections.CreateDictionary<string, TPackageVersion>;
end;

function TDPMServerPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;


end;

function TDPMServerPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;


function TDPMServerPackageRepository.GetServiceIndex(const cancellationToken: ICancellationToken): IServiceIndex;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
begin
  result := nil;
  httpClient := THttpClientFactory.CreateClient(Self.SourceUri);
  request := THttpClientFactory.CreateRequest;

  try
    response := httpClient.Get(request, cancellationToken);
    if response.ResponseCode <> 200 then
    begin
      if response.ResponseCode > 0 then
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.ResponseCode, response.ErrorMessage]))
      else
        Logger.Error(Format('Error [%d] getting source service index : %s ', [response.ResponseCode, 'unabled to contact source']));
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

function TDPMServerPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageIdentity>;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  serviceIndex : IServiceIndex;
  serviceItem : IServiceIndexItem;
  uriTemplate : string;
  path : string;

  jsonObj : TJsonObject;
  jsonArr : TJsonArray;

  newIdentity : IPackageIdentity;
  I: Integer;

  procedure AddToPath(const value : string);
  begin
    if path = '' then
      path := '?' + value
    else
      path := path + '&' + value;
  end;

begin
  result := TCollections.CreateList<IPackageIdentity>;
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


  uriTemplate := serviceItem.ResourceUrl + '?';

  path := '';

  if options.SearchTerms <> '' then
    AddToPath('q=' + options.SearchTerms);

  if not options.Prerelease then
    AddToPath('prerelease=false');

  if not options.Commercial then
    AddToPath('commercial=false');

  if not options.Trial then
    AddToPath('trial=false');

  if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
    AddToPath('compiler=' + CompilerToString(options.CompilerVersion));

  if options.Platforms  <> [] then
    AddToPath('compiler=' + DPMPlatformsToString(options.Platforms));

  //we need all to roll them up. TODO : Get the server to do the rollup and send less data?
  AddToPath('take=2000000'); //if we have more than 2m versions this will need to change!

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);

  request := THttpClientFactory.CreateRequest(path);

  try
    response := httpClient.Get(request);
  except
    on ex : Exception do
    begin
      Logger.Error('Error fetching list from server : ' + ex.Message);
      exit;
    end;
  end;

  if response.ResponseCode <> 200 then
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
      if TPackageIdentity.TryLoadFromJson(Logger, jsonArr.O[i], Name, newIdentity) then
        Result.Add(newIdentity)
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

  httpClient := THttpClientFactory.CreateClient(serviceItem.ResourceUrl);
  request := THttpClientFactory.CreateRequest;

  request.AddHeader('X-ApiKey', pushOptions.ApiKey);

  request.AddFile(pushOptions.PackagePath);

  response := httpClient.Put(request, cancellationToken);

  Logger.Debug(Format('Package Upload [%d] : %s', [response.ResponseCode, response.ErrorMessage]));

  result := response.ResponseCode = 201;
end;

end.

