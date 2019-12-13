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

unit DPM.Console.Reg;

interface

uses
  Spring.Container;

procedure InitConsole(const container : TContainer);

implementation

uses
  DPM.Console.Command,
  DPM.Console.Command.Factory,
  DPM.Console.Command.Cache,
  DPM.Console.Command.Config,
  DPM.Console.Command.Delete,
  DPM.Console.Command.ExitCodes,
  DPM.Console.Command.Help,
  DPM.Console.Command.Install,
  DPM.Console.Command.List,
  DPM.Console.Command.Pack,
  DPM.Console.Command.Push,
  DPM.Console.Command.Remove,
  DPM.Console.Command.Restore,
  DPM.Console.Command.SetApiKey,
  DPM.Console.Command.Sign,
  DPM.Console.Command.Sources,
  DPM.Console.Command.Spec,
  DPM.Console.Command.Update,
  DPM.Console.Command.Verify,
  DPM.Console.Command.Why,
  DPM.Core.Logging,
  DPM.Console.Logger,
  DPM.Console.Writer,
{$IFDEF MSWINDOWS}
  DPM.Console.Writer.Windows
{$ENDIF}
{$IFDEF MACOS}
  DPM.Console.MacOS
{$ENDIF};


procedure InitConsole(const container : TContainer);
var
  console : IConsoleWriter;
begin
{$IFDEF MSWINDOWS}
  console := TWindowsConsole.Create;
{$ENDIF}
{$IFDEF MACOS}
  console := TMacOSConsole.Create;
{$ENDIF}
 container.RegisterInstance<IConsoleWriter>(console);
 container.RegisterType<ILogger, TDPMConsoleLogger>;

 container.RegisterType<ICommandHandler,TCacheCommand>('command.cache');
 container.RegisterType<ICommandHandler,TConfigCommand>('command.config');
 container.RegisterType<ICommandHandler,TDeleteCommand>('command.delete');
 container.RegisterType<ICommandHandler,TExitCodesCommand>('command.exitcodes');
 container.RegisterType<ICommandHandler,THelpCommand>('command.help');
 container.RegisterType<ICommandHandler,TInstallCommand>('command.install');
 container.RegisterType<ICommandHandler,TListCommand>('command.list');
 container.RegisterType<ICommandHandler,TPackCommand>('command.pack');
 container.RegisterType<ICommandHandler,TPushCommand>('command.push');
 container.RegisterType<ICommandHandler,TRemoveCommand>('command.remove');
 container.RegisterType<ICommandHandler,TRestoreCommand>('command.restore');
 container.RegisterType<ICommandHandler,TSetApiKeyCommand>('command.setapikey');
 container.RegisterType<ICommandHandler,TSignCommand>('command.sign');
 container.RegisterType<ICommandHandler,TSourcesCommand>('command.sources');
 container.RegisterType<ICommandHandler,TSpecCommand>('command.spec');
 container.RegisterType<ICommandHandler,TUpdateCommand>('command.update');
 container.RegisterType<ICommandHandler,TVerifyCommand>('command.verify');
 container.RegisterType<ICommandHandler,TWhyCommand>('command.why');
 container.RegisterType<ICommandFactory,TCommandFactory>;

end;

end.
