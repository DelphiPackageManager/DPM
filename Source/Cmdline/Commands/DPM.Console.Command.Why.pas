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

unit DPM.Console.Command.Why;

interface

//TODO : Why command to explain a dependency. Just use the lock file to present a clear explaination
//https://theimowski.com/blog/2016/10-30-paket-why-command/index.html

uses
  VSoft.Awaitable,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base;

type
  TWhyCommand = class(TBaseCommand)
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  end;


implementation

{ TWhyCommand }

function TWhyCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
begin
  Logger.Error('Why command not implemented');
  result := TExitCode.NotImplemented;
end;

end.
