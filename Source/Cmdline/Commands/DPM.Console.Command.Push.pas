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

unit DPM.Console.Command.Push;

interface

uses
  VSoft.Awaitable,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Logging;

type
  TPushCommand = class(TBaseCommand)
  private
    FClientFactory : ISourceClientFactory;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const clientFactory : ISourceClientFactory);reintroduce;
  end;


implementation

uses
  System.SysUtils,
  VSoft.Uri,
  DPM.Core.Options.Common,
  DPM.Core.Options.Push;

{ TPushCommand }

constructor TPushCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager; const clientFactory : ISourceClientFactory);
begin
  inherited Create(logger,configurationManager);
  FClientFactory := clientFactory;
end;

function TPushCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  config : IConfiguration;
  sourceConfig : ISourceConfig;
  sourceUri : IUri;
  error : string;
  client : ISourceClient;
begin
  TPushOptions.Default.ApplyCommon(TCommonOptions.Default);

  if not FileExists(TPushOptions.Default.ConfigFile) then
  begin
    Logger.Error('No configuration file found, create one by adding a source - see sources command.');
    exit(TExitCode.InitException);
  end;


  config := FConfigurationManager.LoadConfig(TPushOptions.Default.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);

  //validate the source
  sourceConfig := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, TPushOptions.Default.Source );
                      end).FirstOrDefault;

  if sourceConfig = nil then
  begin
    Logger.Error('No source named [' + TPushOptions.Default.Source + '] is registered');
    exit(TExitCode.InvalidArguments);
  end;

  if not TUriFactory.TryParseWithError(sourceConfig.Source,false, sourceUri, error) then
  begin
    Logger.Error('Error parsing source Uri [' + sourceConfig.Source + '] - ' + error);
    exit(TExitCode.Error);
  end;

  client := FClientFactory.CreateClient(sourceUri);
  if client <> nil then
  begin
    if not client.Push(TPushOptions.Default) then
      exit(TExitCode.Error);
  end
  else
    exit(TExitCode.InitException);

  result := TExitCode.OK;
end;

end.
