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

unit DPM.Console.Command;

interface

uses
  Spring.Container.Common,
  DPM.Console.Types,
  DPM.Console.ExitCodes;

type
  ICommandHandler = interface
  ['{84F36151-773B-47B2-A396-728CB2D9D78E}']
    function ExecuteCommand : TExitCode;
    function ForceNoBanner  : boolean;
  end;

  ICommandFactory = interface
  ['{08710160-1755-49CA-B0A7-8A22144E4E92}']
    function CreateCommand(const command : TDPMCommand) : ICommandHandler;
  end;


implementation

end.
