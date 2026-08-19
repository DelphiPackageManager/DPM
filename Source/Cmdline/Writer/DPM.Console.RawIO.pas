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

unit DPM.Console.RawIO;

interface

{
  Raw, byte level stdout and stderr.

  Why this exists
  ---------------
  Machine readable output cannot go through IConsoleWriter: TConsoleBase indents and word wraps
  to the console width, which would corrupt JSON.

  It also must not go through the RTL text file (Write/WriteLn), for three reasons:
    1. The RTL converts to the console/ANSI code page, so any non ASCII character - and package
       descriptions are full of them - comes out as the wrong bytes or as '?'. JSON is defined
       as UTF-8.
    2. WriteLn emits CRLF. Newline delimited framing (the MCP server) requires a bare LF.
    3. With I/O checking on, an unusable stdout raises EInOutError - the 'I/O error 6' problem
       in dpm.dpr - which would replace the real result with a context free error.

  So this writes UTF-8 bytes straight to the process stdout handle, with no BOM, and swallows
  write failures rather than raising: if stdout is unusable there is nothing useful to say and
  the exit code still carries the result.

  Everything emitting JSON on stdout should use this, so there is exactly one audited path to
  the real handle rather than a copy of it per command.
}

type
  TStdOut = record
  public
    /// <summary> Writes value as UTF-8. No newline, no BOM. </summary>
    class procedure Write(const value : string); static;

    /// <summary> Writes value as UTF-8 followed by a single LF (never CRLF). </summary>
    class procedure WriteLine(const value : string); static;
  end;

  ///<summary> Diagnostics, so they never contaminate whatever stdout is carrying. </summary>
  TStdErr = record
  public
    class procedure Write(const value : string); static;
    ///<summary> Human facing, so this ends the line the way the host platform expects. </summary>
    class procedure WriteLine(const value : string); static;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.SysUtils;

const
  cLF : byte = 10;

procedure WriteBytes(const bytes : TBytes; const useStdErr : boolean);
var
  offset : integer;
  remaining : integer;
{$IFDEF MSWINDOWS}
  handle : THandle;
  written : DWORD;
{$ELSE}
  written : integer;
{$ENDIF}
begin
  if Length(bytes) = 0 then
    exit;

{$IFDEF MSWINDOWS}
  if useStdErr then
    handle := GetStdHandle(STD_ERROR_HANDLE)
  else
    handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if (handle = 0) or (handle = INVALID_HANDLE_VALUE) then
    exit;
{$ENDIF}

  offset := 0;
  remaining := Length(bytes);
  //Pipes accept partial writes, so this has to loop rather than assume one call is enough.
  while remaining > 0 do
  begin
    written := 0;
{$IFDEF MSWINDOWS}
    if not WriteFile(handle, bytes[offset], DWORD(remaining), written, nil) then
      exit;
{$ELSE}
    if useStdErr then
      written := FileWrite(THandle(2), bytes[offset], remaining)
    else
      written := FileWrite(THandle(1), bytes[offset], remaining);
    if written < 0 then
      exit;
{$ENDIF}
    if written = 0 then
      exit;
    Inc(offset, written);
    Dec(remaining, written);
  end;
end;

{ TStdOut }

class procedure TStdOut.Write(const value : string);
begin
  //GetBytes never emits a preamble - GetPreamble is a separate call - so this is BOM free.
  WriteBytes(TEncoding.UTF8.GetBytes(value), false);
end;

class procedure TStdOut.WriteLine(const value : string);
var
  bytes : TBytes;
  len : integer;
begin
  bytes := TEncoding.UTF8.GetBytes(value);
  len := Length(bytes);
  SetLength(bytes, len + 1);
  bytes[len] := cLF;
  WriteBytes(bytes, false);
end;

{ TStdErr }

class procedure TStdErr.Write(const value : string);
begin
  WriteBytes(TEncoding.UTF8.GetBytes(value), true);
end;

class procedure TStdErr.WriteLine(const value : string);
begin
  //sLineBreak, not a bare LF - unlike stdout this is read by a person in a terminal.
  WriteBytes(TEncoding.UTF8.GetBytes(value + sLineBreak), true);
end;

end.
