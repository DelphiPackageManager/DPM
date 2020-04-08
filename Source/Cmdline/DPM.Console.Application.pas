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
  DPM.Core.Logging,
  Spring.Container,
  VSoft.Awaitable;

type
  TDPMConsoleApplication = class
  private
    class var
      FContainer : TContainer;
      FLogger : ILogger;
      FCancellationTokenSource : ICancellationTokenSource;
  protected
    class function InitContainer : TExitCode;
  public
    class function Run : TExitCode;
    class procedure CtrlCPressed;
    class procedure CtrlBreakPressed;
    class procedure CloseEvent;
    class procedure ShutdownEvent;

  end;

implementation

uses
  WinApi.Windows,
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

function ConProc(CtrlType : DWord) : Bool; stdcall;
begin
 Result := True;
 case CtrlType of
   CTRL_C_EVENT        : TDPMConsoleApplication.CtrlCPressed;
   CTRL_BREAK_EVENT    : TDPMConsoleApplication.CtrlBreakPressed;
   CTRL_CLOSE_EVENT    : TDPMConsoleApplication.CloseEvent;
   CTRL_SHUTDOWN_EVENT : TDPMConsoleApplication.ShutdownEvent;
 end;
end;



{ T }

///
class procedure TDPMConsoleApplication.CloseEvent;
begin
  FLogger.Information('Close Detected.');

end;

class procedure TDPMConsoleApplication.CtrlBreakPressed;
begin
  FLogger.Information('Ctrl-Break detected.');
  FCancellationTokenSource.Cancel;
end;

class procedure TDPMConsoleApplication.CtrlCPressed;
begin
  FLogger.Information('Ctrl-C detected.');
  FCancellationTokenSource.Cancel;
end;

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
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;

  SetConsoleCtrlHandler(@ConProc, True);

  result := InitContainer; //Init our DI container
  if result <> TExitCode.OK then
    exit;
  console := FContainer.Resolve<IConsoleWriter>();
  Assert(console <> nil);
  FLogger := FContainer.Resolve<ILogger>;
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

  FLogger.Verbosity := TCommonOptions.Default.Verbosity;

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

  result := commandHandler.ExecuteCommand(FCancellationTokenSource.Token);
  if bError then //an error occured earlier so ignore command result.
    result := TExitCode.InvalidArguments;
  FreeAndNil(FContainer);
end;

class procedure TDPMConsoleApplication.ShutdownEvent;
begin
  FLogger.Information('Shutdown detected.');
  FCancellationTokenSource.Cancel;

end;

end.
