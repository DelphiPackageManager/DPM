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

unit DPM.Console.MCP.StdioTransport;

interface

uses
  System.SysUtils,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.FrameBuffer;

{
  The real stdio transport - the only part of the MCP server that touches OS handles.

  Everything here works in raw bytes rather than the RTL text file, for three reasons that are
  all load bearing:

    1. The RTL Write converts to the console/ANSI code page. JSON-RPC MUST be UTF-8, and a
       package description with a non ASCII character would otherwise go out as the wrong
       bytes. This is documented in DPM.Console.Command.Verify where the same trap was hit.
    2. WriteLn emits CRLF. The framing is one message per LF.
    3. With I/O checking on, an unusable stdout raises EInOutError - the "I/O error 6" problem
       dpm.dpr already works around.

  This lives in Cmdline rather than Core because owning the process standard streams is a CLI
  concern; the IDE plugin must never link it.
}

type
  TMCPStdioReader = class(TInterfacedObject, IMCPMessageReader)
  private
    FBuffer : TMCPFrameBuffer;
    FChunk : TBytes;
    FAtEof : boolean;
    function ReadChunk(out bytesRead : integer) : boolean;
  protected
    function ReadMessage(out message : string) : boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMCPStdioWriter = class(TInterfacedObject, IMCPMessageWriter)
  protected
    procedure WriteMessage(const message : string);
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  DPM.Console.RawIO;

const
  cChunkSize = 8192;

{ TMCPStdioReader }

constructor TMCPStdioReader.Create;
begin
  inherited Create;
  FBuffer := TMCPFrameBuffer.Create;
  SetLength(FChunk, cChunkSize);
  FAtEof := false;
end;

destructor TMCPStdioReader.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

function TMCPStdioReader.ReadChunk(out bytesRead : integer) : boolean;
{$IFDEF MSWINDOWS}
var
  handle : THandle;
  read : DWORD;
  lastError : DWORD;
{$ENDIF}
begin
  bytesRead := 0;
{$IFDEF MSWINDOWS}
  handle := GetStdHandle(STD_INPUT_HANDLE);
  if (handle = 0) or (handle = INVALID_HANDLE_VALUE) then
    exit(false);

  read := 0;
  if not ReadFile(handle, FChunk[0], cChunkSize, read, nil) then
  begin
    lastError := GetLastError;
    //Both of these mean the client closed its end, which is the normal shutdown path rather
    //than a fault.
    if (lastError = ERROR_BROKEN_PIPE) or (lastError = ERROR_HANDLE_EOF) then
      exit(false);
    exit(false);
  end;
  //A redirected file signals EOF by succeeding with zero bytes, so both shapes must be
  //handled - only checking the error path would hang on `dpm mcp < frames.txt`.
  if read = 0 then
    exit(false);
  bytesRead := read;
  result := true;
{$ELSE}
  bytesRead := FileRead(THandle(0), FChunk[0], cChunkSize);
  if bytesRead <= 0 then
  begin
    bytesRead := 0;
    exit(false);
  end;
  result := true;
{$ENDIF}
end;

function TMCPStdioReader.ReadMessage(out message : string) : boolean;
var
  bytesRead : integer;
begin
  message := '';

  //Anything already buffered may hold a complete message - a single read can deliver several.
  if FBuffer.TryGetMessage(message) then
    exit(true);

  while not FAtEof do
  begin
    if not ReadChunk(bytesRead) then
    begin
      FAtEof := true;
      break;
    end;
    FBuffer.AppendBytes(FChunk, bytesRead);
    if FBuffer.TryGetMessage(message) then
      exit(true);
  end;

  //End of stream. Anything left in the buffer is a partial line with no terminator; there is
  //no way to know whether it was truncated, so it is dropped rather than guessed at.
  result := false;
end;

{ TMCPStdioWriter }

procedure TMCPStdioWriter.WriteMessage(const message : string);
begin
  //UTF-8, no BOM, exactly one LF appended - see DPM.Console.RawIO.
  //Write failures are swallowed there. That is the right behaviour here too: if stdout has
  //gone away the client is gone, and our stdin will report end of stream on the next read,
  //which ends the loop through the normal path.
  TStdOut.WriteLine(message);
end;

end.
