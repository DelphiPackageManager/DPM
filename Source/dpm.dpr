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

program dpm;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  WinApi.ActiveX,
  DPM.Console.Types in 'Cmdline\DPM.Console.Types.pas',
  DPM.Console.Reg in 'Cmdline\DPM.Console.Reg.pas',
  DPM.Console.Utils in 'Cmdline\DPM.Console.Utils.pas',
  DPM.Console.Application in 'Cmdline\DPM.Console.Application.pas',
  DPM.Console.Banner in 'Cmdline\DPM.Console.Banner.pas',
  DPM.Console.Options in 'Cmdline\Options\DPM.Console.Options.pas',
  DPM.Console.Options.Reg in 'Cmdline\Options\DPM.Console.Options.Reg.pas',
  DPM.Console.Writer in 'Cmdline\Writer\DPM.Console.Writer.pas',
  DPM.Console.Writer.Windows in 'Cmdline\Writer\DPM.Console.Writer.Windows.pas',
  DPM.Console.Command.Install in 'Cmdline\Commands\DPM.Console.Command.Install.pas',
  DPM.Console.Command in 'Cmdline\Commands\DPM.Console.Command.pas',
  DPM.Console.Command.Restore in 'Cmdline\Commands\DPM.Console.Command.Restore.pas',
  DPM.Console.Command.Pack in 'Cmdline\Commands\DPM.Console.Command.Pack.pas',
  DPM.Console.Command.Factory in 'Cmdline\Commands\DPM.Console.Command.Factory.pas',
  DPM.Console.Command.Help in 'Cmdline\Commands\DPM.Console.Command.Help.pas',
  DPM.Console.Command.Config in 'Cmdline\Commands\DPM.Console.Command.Config.pas',
  DPM.Console.Command.Delete in 'Cmdline\Commands\DPM.Console.Command.Delete.pas',
  DPM.Console.Command.List in 'Cmdline\Commands\DPM.Console.Command.List.pas',
  DPM.Console.Command.Push in 'Cmdline\Commands\DPM.Console.Command.Push.pas',
  DPM.Console.Command.SetApiKey in 'Cmdline\Commands\DPM.Console.Command.SetApiKey.pas',
  DPM.Console.Command.Sources in 'Cmdline\Commands\DPM.Console.Command.Sources.pas',
  DPM.Console.Command.Spec in 'Cmdline\Commands\DPM.Console.Command.Spec.pas',
  DPM.Console.ExitCodes in 'Cmdline\DPM.Console.ExitCodes.pas',
  DPM.Console.Command.Update in 'Cmdline\Commands\DPM.Console.Command.Update.pas',
  DPM.Console.Command.ExitCodes in 'Cmdline\Commands\DPM.Console.Command.ExitCodes.pas',
  DPM.Console.Command.Base in 'Cmdline\Commands\DPM.Console.Command.Base.pas',
  DPM.Console.Command.Sign in 'Cmdline\Commands\DPM.Console.Command.Sign.pas',
  DPM.Console.Command.Verify in 'Cmdline\Commands\DPM.Console.Command.Verify.pas',
  DPM.Console.Logger in 'Cmdline\Logging\DPM.Console.Logger.pas',
  DPM.Console.Command.Remove in 'Cmdline\Commands\DPM.Console.Command.Remove.pas',
  DPM.Console.Command.Why in 'Cmdline\Commands\DPM.Console.Command.Why.pas',
  DPM.Console.Command.Add in 'Cmdline\Commands\DPM.Console.Command.Add.pas',
  DPM.Console.Command.Cache in 'Cmdline\Commands\DPM.Console.Command.Cache.pas';

begin
  //TODO : pull in VSoft.SemanticVersion as a package once we are bootstrapped.
  //TODO : pull in VSoft.CommmandLine as a package once we are bootstrapped.
  //TODO : pull in VSoft.AntPatterns as a package once we are bootstrapped.
  CoInitialize(nil); //needed for msxml
  try
    System.ExitCode := Ord(TDPMConsoleApplication.Run);
    {$IFDEF DEBUG}
      //if running under the debugger, pause so we can see the output!
      {$WARN SYMBOL_PLATFORM OFF}
      if DebugHook <> 0 then
        ReadLn;
    {$ENDIF};
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := Ord(TExitCode.UnhandledException);
    {$IFDEF DEBUG}
      ReadLn;
    {$ENDIF};
    end;
  end;
end.
