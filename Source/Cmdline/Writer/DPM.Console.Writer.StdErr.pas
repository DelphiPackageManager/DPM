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

unit DPM.Console.Writer.StdErr;

interface

uses
  DPM.Console.Writer;

type
  ///<summary>
  ///  An IConsoleWriter that sends everything to stderr, uncoloured and unwrapped.
  ///</summary>
  ///<remarks>
  ///  Swapped in for TWindowsConsole when an invocation needs a clean stdout - currently
  ///  --format=Json, and later the mcp server.
  ///
  ///  Substituting the WRITER rather than the logger is deliberate, and is the only approach
  ///  that actually works. ILogger is a container singleton injected into the configuration
  ///  manager, the repository manager and the project editor, and TBaseCommand.ExecuteCommand
  ///  calls EnsureDefaultConfig before a command's Execute even runs. A logger swapped inside
  ///  one command would therefore leave all of those still writing to stdout. Replacing the
  ///  writer at registration catches every one of them, plus the banner, parse errors and help
  ///  output - none of which go through ILogger at all.
  ///
  ///  Colour is dropped rather than translated: stderr is usually redirected to a file or read
  ///  by a parent process, and escape sequences there are noise. Width is reported as huge so
  ///  TConsoleBase.InternalBreakupMessage never wraps - a wrapped diagnostic is harder to grep.
  ///</remarks>
  TStdErrConsole = class(TConsoleBase)
  protected
    function GetConsoleWidth : Integer; override;
    procedure InternalWriteLn(const s : String); override;
    procedure InternalWrite(const s : String); override;
    function GetCurrentForegroundColor : TConsoleColor; override;
    function GetCurrentBackgroundColor : TConsoleColor; override;
    procedure SetForegroundColor(const foreground : TConsoleColor); override;
    procedure SetColour(const foreground : TConsoleColor; const background : TConsoleColor = ccDefault); override;
  public
    constructor Create; override;
  end;

implementation

uses
  DPM.Console.RawIO;

const
  //Large enough that InternalBreakupMessage never splits a line, small enough to leave room
  //for the indent arithmetic it does on this value.
  cNoWrapWidth = 1024 * 1024;

{ TStdErrConsole }

constructor TStdErrConsole.Create;
begin
  inherited;
  ConsoleWidth := cNoWrapWidth;
  RedirectedStdOut := true;
end;

function TStdErrConsole.GetConsoleWidth : Integer;
begin
  result := cNoWrapWidth;
end;

procedure TStdErrConsole.InternalWrite(const s : String);
begin
  TStdErr.Write(s);
end;

procedure TStdErrConsole.InternalWriteLn(const s : String);
begin
  TStdErr.WriteLine(s);
end;

function TStdErrConsole.GetCurrentForegroundColor : TConsoleColor;
begin
  result := ccDefault;
end;

function TStdErrConsole.GetCurrentBackgroundColor : TConsoleColor;
begin
  result := ccDefault;
end;

procedure TStdErrConsole.SetForegroundColor(const foreground : TConsoleColor);
begin
  //no colour on stderr - see the class remarks.
end;

procedure TStdErrConsole.SetColour(const foreground : TConsoleColor; const background : TConsoleColor);
begin
  //no colour on stderr - see the class remarks.
end;

end.
