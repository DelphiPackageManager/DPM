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
  System.SysUtils;

{ TLocalClient }

constructor TRemoteClient.Create(const logger : ILogger; const sourceUri : IUri);
begin
  FLogger := logger;
  FSourceUri := sourceUri;
end;

function TRemoteClient.Push(const pushOptions : TPushOptions; const cancellationToken : ICancellationToken) : Boolean;
begin
  //  if TPath.IsRelativePath(pushOptions.PackagePath) then
  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    FLogger.Error('Package file [' + pushOptions.PackagePath + '] not found.');
    exit;
  end;

  FLogger.Error('Remote client not implemented yet, no server implementation exists!');
  result := false;
end;



end.

