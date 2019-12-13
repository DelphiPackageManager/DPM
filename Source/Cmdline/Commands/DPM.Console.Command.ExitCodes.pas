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

unit DPM.Console.Command.ExitCodes;

interface

uses
  DPM.Console.Writer,
  DPM.Console.ExitCodes,
  DPM.Console.Command;

type
  TExitCodesCommand = class(TInterfacedObject,ICommandHandler)
  private
    FConsole : IConsoleWriter;
  protected
    function ExecuteCommand: TExitCode;
    function Execute: TExitCode;
    function ForceNoBanner: Boolean;

  public
    constructor Create(const console : IConsoleWriter);
  end;


implementation

uses
  DPM.Console.Options,
  DPM.Console.Banner;


{ TUpdateCommand }

constructor TExitCodesCommand.Create(const console: IConsoleWriter);
begin
  FConsole := console;
end;

function TExitCodesCommand.Execute: TExitCode;
begin
  FConsole.WriteLine;
  FConsole.WriteLine('Exit Codes :',ccBrightWhite);
  DPM.Console.ExitCodes.LogExitCodes(FConsole);
  result := TExitCode.OK;
end;

function TExitCodesCommand.ExecuteCommand: TExitCode;
begin
  result := Execute;
end;

function TExitCodesCommand.ForceNoBanner: Boolean;
begin
  result := false;
end;

end.
