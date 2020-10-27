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

unit DPM.Core.Sources.ClientFactory;

interface

uses
  VSoft.Uri,
  DPM.Core.Logging,
  DPM.Core.Sources.Interfaces;

type
  TClientFactory = class(TInterfacedObject, ISourceClientFactory)
  private
    FLogger : ILogger;
  protected
    function CreateClient(const uri : IUri) : ISourceClient;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Sources.LocalClient,
  DPM.Core.Sources.RemoteClient;


{ TClientFactory }

constructor TClientFactory.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

function TClientFactory.CreateClient(const uri : IUri) : ISourceClient;
begin
  result := nil;
  if SameText(uri.Scheme, 'http') or SameText(uri.Scheme, 'https') then
    result := TRemoteClient.Create(FLogger, uri)
  else if SameText(uri.Scheme, 'file') then
    result := TLocalClient.Create(FLogger, uri)
  else
    FLogger.Error('Unabled to create a client for uri [' + uri.ToString + ']');
end;

end.

