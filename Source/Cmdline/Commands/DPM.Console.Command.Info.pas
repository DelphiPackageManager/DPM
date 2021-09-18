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

unit DPM.Console.Command.Info;

interface
uses
  VSoft.Awaitable,
  DPM.Console.Writer,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging;

type
  TInfoCommand = class(TBaseCommand)
  private
    //FConsole : IConsoleWriter;

  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager);reintroduce;
  end;


implementation

uses
  DPM.Console.Banner,
  DPM.Core.Options.Common,
  DPM.Core.Options.Info;


constructor TInfoCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager);
begin
  inherited Create(logger, configurationManager);
end;

function TInfoCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
begin
  TInfoOptions.Default.ApplyCommon(TCommonOptions.Default);

  Logger.Information('DPM Config file : '  + TInfoOptions.Default.ConfigFile);
  result := TExitCode.OK;
end;
end.
