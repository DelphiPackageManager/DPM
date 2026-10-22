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

unit DPM.Console.Command.Push;

interface

uses
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Configuration.Interfaces,
//  DPM.Core.Sources.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Logging;

type
  TPushCommand = class(TBaseCommand)
  private
    FRepositoryManager : IPackageRepositoryManager;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repositoryManager : IPackageRepositoryManager);reintroduce;
  end;


implementation

uses
  System.SysUtils,
  VSoft.Uri,
  DPM.Core.Constants,
  DPM.Core.Options.Common,
  DPM.Core.Options.Push,
  DPM.Core.Diagnostics;

{ TPushCommand }

constructor TPushCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger,configurationManager);
  FRepositoryManager := repositoryManager;
end;

function TPushCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  config : IConfiguration;
  sourceConfig : ISourceConfig;
  sourceUri : IUri;
  error : string;
begin
  TPushOptions.Default.ApplyCommon(TCommonOptions.Default);

  //fall back to the DPM_API_KEY environment variable when no -apiKey arg was provided.
  if TPushOptions.Default.ApiKey = '' then
    TPushOptions.Default.ApiKey := GetEnvironmentVariable(cDPMApiKeyEnviromentVar);

  if not FileExists(TPushOptions.Default.ConfigFile) then
  begin
    Logger.Error(TErrorCode.E0203_ConfigurationNotFound, 'The configuration file [' + TPushOptions.Default.ConfigFile + '] does not exist - add a source with `dpm sources add` to create it.');
    exit(TExitCode.InitException);
  end;


  config := FConfigurationManager.LoadConfig(TPushOptions.Default.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);
  //it reported the source that is not valid.
  if not FRepositoryManager.Initialize(config) then
    exit(TExitCode.InitException);

  //validate the source
  sourceConfig := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, TPushOptions.Default.Source );
                      end).FirstOrDefault;

  if sourceConfig = nil then
  begin
    Logger.Error(TErrorCode.E1003_UnknownSource, 'Unable to push - the source [' + TPushOptions.Default.Source + '] is not configured.');
    exit(TExitCode.InvalidArguments);
  end;

  if not TUriFactory.TryParseWithError(sourceConfig.Source,false, sourceUri, error) then
  begin
    Logger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + sourceConfig.Name + '] is not valid - [' + sourceConfig.Source + '] is not a valid uri : ' + error);
    exit(TExitCode.Error);
  end;

  if not FRepositoryManager.Push(cancellationToken, TPushOptions.Default) then
      exit(TExitCode.Error);


  result := TExitCode.OK;
end;

end.
