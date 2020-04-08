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

unit DPM.Console.Command.Help;

interface

uses
  VSoft.Awaitable,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.Writer,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base;

type
  THelpCommand = class(TBaseCommand)
  private
    FConsole : IConsoleWriter;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  public
    constructor Create(const console : IConsoleWriter; const logger : ILogger; const configurationManager : IConfigurationManager);reintroduce;
  end;

implementation

uses
  Generics.Defaults,
  VSoft.CommandLine.Options,
  DPM.Console.Banner,
  DPM.Console.Types,
  DPM.Console.Options;



{ THelpCommand }

constructor THelpCommand.Create(const console: IConsoleWriter; const logger: ILogger; const configurationManager : IConfigurationManager);
begin
  inherited Create(logger, configurationManager);
  FConsole := console;

end;

function THelpCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  cw : integer;
  maxDescW : integer;
begin
  result := TExitCode.OK;

  cw := FConsole.Width;

  if cw < High(Integer) then
    maxDescW := cw
  else
    maxDescW := High(Integer);

  if THelpOptions.HelpCommand = TDPMCommand.None then
  begin
    FConsole.WriteLine;
    FConsole.WriteLine('Type ''dpm help <command>'' for help on a specific command.');
    TOptionsRegistry.DescriptionTab := 43;
    TOptionsRegistry.PrintUsage(
      procedure(const value : string)
      begin
        FConsole.WriteLine(value);
      end);
    FConsole.WriteLine;

  end
  else
  begin
    FConsole.WriteLine;
    TOptionsRegistry.DescriptionTab := 43;

    TOptionsRegistry.PrintUsage(CommandString[THelpOptions.HelpCommand],
      procedure (const value : string)
      begin
          FConsole.WriteLine(value);
      end);
    FConsole.WriteLine;
  end;

end;

end.
