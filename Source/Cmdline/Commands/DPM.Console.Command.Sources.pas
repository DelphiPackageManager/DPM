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

unit DPM.Console.Command.Sources;

interface

uses
  VSoft.Awaitable,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Sources.Interfaces;

type
  TSourcesCommand = class(TBaseCommand)
  private
    FSourcesManager : ISourcesManager;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
    function ForceNoBanner: Boolean; override;

  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const sourcesManager : ISourcesManager);reintroduce;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Options.Common,
  DPM.Core.Options.Sources,
  DPM.Core.Sources.Types;


{ TSourcesCommand }

constructor TSourcesCommand.Create(const logger: ILogger; const configurationManager : IConfigurationManager; const sourcesManager : ISourcesManager);
begin
  inherited Create(logger,configurationManager);
  FSourcesManager := sourcesManager;
end;

function TSourcesCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  bResult : boolean;
  newConfig : IConfiguration;
begin
  TSourcesOptions.Default.ApplyCommon(TCommonOptions.Default);

  if not (TSourcesOptions.Default.Command in [TSourcesSubCommand.Invalid, TSourcesSubCommand.List])  then
  begin
    //name is required
    if TSourcesOptions.Default.Name = '' then
    begin
      Logger.Error('Name is required for this command.');
      result := TExitCode.MissingArg;
      exit;
    end;
  end;

  //TODO : Find a way to do this for all commands that might write to a new config file.
  if not FileExists(TSourcesOptions.Default.ConfigFile) then
  begin
    newConfig := FConfigurationManager.NewConfig;
    FConfigurationManager.SaveConfig(newConfig,TSourcesOptions.Default.ConfigFile);
  end;


  Logger.Information('');
  result := TExitCode.OK;
  case TSourcesOptions.Default.Command of
    TSourcesSubCommand.Invalid:
    begin
      Logger.Error('invalid command');
      result := TExitCode.InvalidArguments;
      exit;
    end;

    TSourcesSubCommand.Add    :
    begin
      if TSourcesOptions.Default.Source = '' then
      begin
        Logger.Error('Source is required for this command.');
        result := TExitCode.MissingArg;
        exit;
      end;
      bResult := FSourcesManager.AddSource(TSourcesOptions.Default);
    end;
    TSourcesSubCommand.Remove : bResult := FSourcesManager.RemoveSource(TSourcesOptions.Default);
    TSourcesSubCommand.List   : bResult := FSourcesManager.ListSources(TSourcesOptions.Default);
    TSourcesSubCommand.Enable : bResult := FSourcesManager.EnableSource(TSourcesOptions.Default);
    TSourcesSubCommand.Disable: bResult := FSourcesManager.DisableSource(TSourcesOptions.Default);
    TSourcesSubCommand.Update : bResult := FSourcesManager.UpdateSource(TSourcesOptions.Default);
  else
    result := TExitCode.NotImplemented;
    exit;
  end;

  if not bResult then
    result := TExitCode.Error;

end;

function TSourcesCommand.ForceNoBanner: Boolean;
begin
  result := true;
end;

end.
