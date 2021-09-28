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

unit DPM.Core.Sources.RemoteClient;

interface

uses
  VSoft.Uri,
  VSoft.Awaitable,
  DPM.Core.Logging,
  DPM.Core.Options.Push,
  DPM.Core.Sources.Interfaces;

type
  TRemoteClient = class(TInterfacedObject, ISourceClient)
  private
    FLogger : ILogger;
    FSourceUri : IUri;
  protected
    function Push(const pushOptions : TPushOptions; const cancellationToken : ICancellationToken) : Boolean;
  public
    constructor Create(const logger : ILogger; const sourceUri : IUri);
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  JsonDataObjects,
  VSoft.HttpClient;

{ TLocalClient }

constructor TRemoteClient.Create(const logger : ILogger; const sourceUri : IUri);
begin
  FLogger := logger;
  FSourceUri := sourceUri;
end;

function TRemoteClient.Push(const pushOptions : TPushOptions; const cancellationToken : ICancellationToken) : Boolean;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  jsonObj : TJsonObject;
  resourcesObj : TJsonArray;
  i : integer;
  resourceObj : TJsonObject;
  resource : string;
begin
  result := false;

  if pushOptions.ApiKey = '' then
  begin
    FLogger.Error('ApiKey arg required for remote push');
  end;

    //  if TPath.IsRelativePath(pushOptions.PackagePath) then
  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    FLogger.Error('Package file [' + pushOptions.PackagePath + '] not found.');
    exit;
  end;
  //FLogger.Error('Remote client not implemented yet, no server implementation exists!');


  if cancellationToken.IsCancelled then
    exit;

  httpClient := THttpClientFactory.CreateClient(FSourceUri.ToString);
  request := THttpClientFactory.CreateRequest;

  response := httpClient.Get(request, cancellationToken);

  if response.ResponseCode <> 200 then
  begin
    FLogger.Error(Format('Error [%d] getting source service index : %s ', [response.ResponseCode, response.ErrorMessage]));
    exit;
  end;

//  FLogger.Debug(response.Response);

  //TODO : Create a ServiceIndex object to deserialize to as we will need the other services eventually anyway!

  jsonObj := nil;
  try
    try
      jsonObj := TJsonObject.Parse(response.Response) as TJsonObject;
    except
      on e : Exception do
      begin
        FLogger.Error('Error parsing spec json : ' + e.Message);
        exit;
      end;
    end;
    if jsonObj = nil then
    begin
      FLogger.Error('serviceindex json is nil - should never happen!');
      exit;
    end;


    resourcesObj := jsonObj.A['resources'];

    for i := 0 to resourcesObj.Count -1 do
    begin
      resourceObj := resourcesObj.O[i];
      if resourceObj.S['resourceType'] = 'PackagePublish' then
      begin
        resource := resourceObj.S['id'];
        Break;
      end;
    end;

    if resource = '' then
    begin
      FLogger.Error('Unabled to determine PackagePublish resource from Service Index');
      exit;
    end;

    FLogger.Debug('PackagePublish : ' + resource);
  finally
    jsonObj.Free;
  end;

  httpClient := THttpClientFactory.CreateClient(resource);
  request := THttpClientFactory.CreateRequest;

  request.AddHeader('X-ApiKey', pushOptions.ApiKey);

  request.AddFile(pushOptions.PackagePath);

  response := httpClient.Put(request, cancellationToken);

  FLogger.Debug(Format('Package Upload [%d] : %s', [response.ResponseCode, response.ErrorMessage]));

  result := false;
end;



end.

