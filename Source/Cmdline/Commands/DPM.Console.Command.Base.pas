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

unit DPM.Console.Command.Base;

interface

uses
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command;

type
  TBaseCommand = class(TInterfacedObject, ICommandHandler)
  private
    FLogger : ILogger;
  protected
    FConfigurationManager : IConfigurationManager;
    function Execute: TExitCode;virtual;
    function ExecuteCommand: TExitCode;
    function ForceNoBanner  : boolean;virtual;
    property Logger : ILogger read FLogger;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager);virtual;
  end;


implementation

uses
  DPM.Core.Constants,
  DPM.Console.Options;


{ TBaseCommand }

constructor TBaseCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager);
begin
  FLogger := logger;
  FConfigurationManager := configurationManager;
end;


function TBaseCommand.Execute: TExitCode;
begin
  result := TExitCode.NotImplemented;
end;

function TBaseCommand.ExecuteCommand: TExitCode;
begin
  if not FConfigurationManager.EnsureDefaultConfig then
    result := TExitCode.InitException
  else
    result := Execute;
end;

function TBaseCommand.ForceNoBanner: boolean;
begin
  result := false;
end;

end.
