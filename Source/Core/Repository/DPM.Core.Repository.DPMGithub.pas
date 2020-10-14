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

unit DPM.Core.Repository.DPMGithub;

interface

uses
  Generics.Defaults,
  VSoft.Awaitable,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.BaseGitHub;


type
  TDPMGithubPackageRepository = class(TGithubBasePackageRepository, IPackageRepository)
  private

  protected
    function DownloadPackage(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity; const localFolder: string; var fileName: string): Boolean;
    function GetPackageInfo(const cancellationToken: ICancellationToken; const packageId : IPackageId): IPackageInfo;
    function GetPackageVersions(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion): IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): IList<IPackageInfo>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions ) : IList<IPackageIdentity>;overload;

    //ui
    function GetPackageFeed(const cancelToken: ICancellationToken; const options: TSearchOptions; const configuration: IConfiguration = nil): IList<IPackageSearchResultItem>;
    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId: string; const packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageIcon;

  public
    constructor Create(const logger : ILogger);override;

  end;


implementation


uses
  System.SysUtils,
  VSoft.HttpClient,
  JsonDataObjects;

const
  cGithubDPMRepositorySearch = 'search/repositories?q=topic:dpmpackage+archived:false';
  cGithubDPMSpecSearchFormat = '/search/code?q=extension:dspec+repo:%s'; // add repo to search in


{ TDPMGithubPackageRepository }

constructor TDPMGithubPackageRepository.Create(const logger: ILogger);
begin
  inherited Create(logger);
end;

function TDPMGithubPackageRepository.DownloadPackage(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity; const localFolder: string; var fileName: string): Boolean;
begin
  result := false;
end;

function TDPMGithubPackageRepository.GetPackageFeed(const cancelToken: ICancellationToken; const options: TSearchOptions; const configuration: IConfiguration): IList<IPackageSearchResultItem>;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;
end;

function TDPMGithubPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageIcon;
begin
  result := nil;
end;

function TDPMGithubPackageRepository.GetPackageInfo(const cancellationToken: ICancellationToken; const packageId : IPackageId): IPackageInfo;
begin
  result := nil;
end;

function TDPMGithubPackageRepository.GetPackageVersions(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion): IList<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;
end;

function TDPMGithubPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;


function TDPMGithubPackageRepository.List(const cancellationToken: ICancellationToken; const options: TSearchOptions): IList<IPackageIdentity>;
var
  pageSize : string;
  pageNo : string;
  resource : string;
  request : IHttpRequest;
  response : IHttpResponse;
  jsonObj : TJsonObject;
  items : TJsonArray;
  repoItem : TJsonObject;
  i, j : integer;
  repoId : string;

  specSearchJson : TJSonObject;
  specItems : TJsonArray;
//  specItem : TJDOJsonObject;
  specUrl  : string;
begin
  result := TCollections.CreateList<IPackageIdentity>;
  if options.Take > 0 then
    pageSize := 'per_page=' + IntToStr(options.Take);

  if options.Skip > 0 then
  begin
    if options.Take = 0 then
      options.Take := 10;
    pageNo := 'page=' + IntToStr(options.Skip div options.Take);
  end;

  resource := cGithubDPMRepositorySearch;
  if pageSize <> '' then
    resource := resource + '&' + pageSize;
  if pageNo <> '' then
    resource := resource + '&' + pageNo;

  request := THttpClientFactory.CreateRequest(resource);
  request.Accept := cTopicSearchAccept;

  response := HttpClient.Get(request, cancellationToken);
  //if we get no response object, that means the cancellation token was cancelled.
  if response = nil then
  begin
    Logger.Error('request cancelled.');
    exit;
  end;

  if response.ResponseCode <> HTTP_OK then
  begin
    Logger.Error('Error getting list of possible repos from github : ' + response.ErrorMessage);
  end;

  jsonObj := TJsonObject.ParseFromStream(response.ResponseStream) as TJsonObject;
  try
    items := jsonObj.ExtractArray('items');
    for i := 0 to items.Count -1 do
    begin
      repoItem := items.O[i];
      repoId := repoItem.S['full_name'];

      resource := 'search/code?q=extension:dspec+repo:' + repoId;
      Logger.Debug(resource);
      request := THttpClientFactory.CreateRequest(resource);
      request.Accept := cGithubv3Accept;
      response := HttpClient.Get(request, cancellationToken);
      //if we get no response object, that means the cancellation token was cancelled.
      if response = nil then
      begin
        Logger.Error('request cancelled.');
        exit;
      end;

      if response.ResponseCode <> HTTP_OK then
      begin
        Logger.Error('Error getting list of possible repos from github : ' + response.ErrorMessage);
      end;

      specSearchJson := TJsonObject.ParseFromStream(response.ResponseStream) as TJsonObject;
      try
        specItems := specSearchJson.ExtractArray('items');
        for j := 0 to specItems.Count -1 do
        begin
          specUrl := specItems.O[j].S['html_url'];
          Logger.Debug(specUrl);

        end;

      finally
        specSearchJson.Free;
      end;


    end;
  finally
    jsonObj.Free;
  end;




end;


end.
