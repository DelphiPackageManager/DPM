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

unit DPM.Console.ExitCodes;

interface

uses
  DPM.Console.Writer;


type
  {$SCOPEDENUMS ON}
  TExitCode  = (
    OK = 0,
    Error = 1,
    MissingArg = 2,
    InitException = 100,
    InvalidArguments = 101,
    InvalidCommand = 102,

    NotImplemented = 200,

    //developer/config related errors. Do not add these to ExitCodeString (except for notimplemented)
    NoCommandHandler = 201,

    UnhandledException = 999

    );

function ExitCodeString(const exitCode : TExitCode) : string;

procedure LogExitCodes(const console : IConsoleWriter);

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Strings;

function ExitCodeString(const exitCode : TExitCode) : string;
begin
  case exitCode of
    TExitCode.OK : result := 'Success.';
    TExitCode.InitException : result := 'Initialization Exception';
    TExitCode.InvalidArguments : result := 'Invalid arguments';
    TExitCode.InvalidCommand : result := 'Invalid command';

    TExitCode.NotImplemented : result := 'Feature not implemented yet!';
    TExitCode.UnhandledException : result := 'Unhandled Exception';
  else
    result := '';
  end;
end;


procedure LogExitCodes(const console : IConsoleWriter);

var
  exitCode : TExitCode;

  procedure LogCode(const code : TExitCode);
  var
    clr : TConsoleColor;
    s : string;
  begin
    s := ExitCodeString(code);
    if s = '' then
      exit;

    if code = TExitCode.OK  then
      clr := ccBrightGreen
    else
      clr := ccBrightRed;
    console.WriteLine('   ' +  TStringUtils.PadRight(IntToStr(Ord(code)), 5) + s,clr);
  end;

begin
  for exitCode := Low(TExitCode) to High(TExitCode) do
    LogCode(exitCode);
end;


end.
