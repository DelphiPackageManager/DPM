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

unit DPM.Console.Command.Factory;

interface

uses
  Spring.Container,
  DPM.Console.Types,
  DPM.Console.Command;

type
  TCommandFactory = class(TInterfacedObject,ICommandFactory)
  private
    FContainer : TContainer;
  protected
    function CreateCommand(const command : TDPMCommand) : ICommandHandler;
  public
    constructor Create(const container : TContainer);
  end;

implementation

uses
  System.SysUtils;

{ TCommandFactory }

constructor TCommandFactory.Create(const container: TContainer);
begin
  FContainer := container;
end;

function TCommandFactory.CreateCommand(const command: TDPMCommand): ICommandHandler;
var
  commandName : string;
begin
  result := nil;
  commandName := 'command.' + LowerCase(CommandString[command]);

  result := FContainer.Resolve<ICommandHandler>(commandName);

end;

end.
