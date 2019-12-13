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

unit DPM.Console.Application;

interface

uses
  DPM.Console.ExitCodes,
  Spring.Container;

type
  TDPMConsoleApplication = class
  private
    class var
      FContainer : TContainer;
  protected
    class function InitContainer : TExitCode;
  public
    class function Run : TExitCode;
  end;

implementation

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  DPM.Core.Options.Common,
  DPM.Core.Init,
  DPM.Console.Types,
  DPM.Console.Options,
  DPM.Console.Writer,
  DPM.Console.Banner,
  DPM.Console.Command,
  DPM.Console.Reg;

{ TPackItApplications }

///
class function TDPMConsoleApplication.InitContainer : TExitCode;
begin
  result := TExitCode.OK;
  try
    FContainer := TContainer.Create;
    DPM.Core.Init.InitCore(FContainer);
    DPM.Console.Reg.InitConsole(FContainer);
    FContainer.Build;
  except
    on e : Exception do
    begin
      //we don't jhave a logger or console here so using system.writeln is ok.
      System.Writeln('Exception while initializing : ' + e.Message);
      result := TExitCode.InitException;
    end;
  end;
end;

class function TDPMConsoleApplication.Run: TExitCode;
var
  console : IConsoleWriter;
  parseresult :  ICommandLineParseResult;
  commandHandler : ICommandHandler;
  commandFactory : ICommandFactory;
  bError : boolean;
  command : TDPMCommand;
begin
  result := InitContainer; //Init our DI container
  if result <> TExitCode.OK then
    exit;
  console := FContainer.Resolve<IConsoleWriter>();
  Assert(console <> nil);
  bError := false;

  parseresult := TOptionsRegistry.Parse;

  if parseresult.HasErrors then
  begin
    ShowBanner(console);
    TCommonOptions.Default.NoBanner := true;  //make sure it doesn't show again.
    console.WriteLine(parseresult.ErrorText,ccBrightRed);
    bError := true;
    TCommonOptions.Default.Help := true; //if it's a valid command but invalid options this will give us command help.
  end;

  command := GetCommandFromString(parseresult.Command);

  if command = TDPMCommand.Invalid then
  begin
    ShowBanner(console);
    console.WriteLine('Invalid command : [' + parseResult.Command + ']',ccBrightRed );
    exit(TExitCode.InvalidCommand);
  end;

  if command = TDPMCommand.None then
    command := TDPMCommand.Help;


  if command <> TDPMCommand.Help then
  begin
    if TCommonOptions.Default.Help then
    begin
      THelpOptions.HelpCommand := command;
      command := TDPMCommand.Help;
    end;
  end;


  commandFactory := FContainer.Resolve<ICommandFactory>;
  Assert(commandFactory <> nil, 'CommandFactory did not resolve!');
  commandHandler := commandFactory.CreateCommand(command);
  if commandHandler = nil then
  begin
    console.WriteLine('No command handler registred for command : [' + CommandString[command] + ']',ccBrightRed );
    exit(TExitCode.NoCommandHandler);
  end;

  if (not TCommonOptions.Default.NoBanner) and (not commandHandler.ForceNoBanner) then
    ShowBanner(console);

  result := commandHandler.ExecuteCommand;
  if bError then //an error occured earlier so ignore command result.
    result := TExitCode.InvalidArguments;
  FreeAndNil(FContainer);
end;

end.
