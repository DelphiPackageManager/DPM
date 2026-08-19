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

unit DPM.Core.MCP.FrameBuffer;

interface

uses
  System.SysUtils;

type
  ///<summary>
  ///  Accumulates raw bytes from a stream and hands back whole messages.
  ///</summary>
  ///<remarks>
  ///  MCP stdio framing is one JSON message per line, UTF-8, LF delimited, with no embedded
  ///  newlines. This does the byte level half of that: buffer, find the delimiter, decode.
  ///
  ///  It is a separate class from the transport for one reason - a read from a pipe can split
  ///  a multi byte UTF-8 sequence across two chunks, and that is the single most likely subtle
  ///  bug in the whole server. Keeping the buffering pure means it can be tested exhaustively
  ///  without a pipe, a process, or a timing dependency.
  ///
  ///  Splitting on the LF byte BEFORE decoding is safe: in UTF-8 every byte of a multi byte
  ///  sequence has the high bit set, so $0A can never appear inside one. Decoding a partial
  ///  sequence, on the other hand, would corrupt it - which is why the raw bytes are buffered
  ///  rather than decoded text.
  ///</remarks>
  TMCPFrameBuffer = class
  private
    FBuffer : TBytes;
    FLength : integer;
    FSeenFirstMessage : boolean;
    procedure Consume(const count : integer);
  public
    constructor Create;
    ///<summary> Appends the first count bytes of chunk to the buffer. </summary>
    procedure AppendBytes(const chunk : TBytes; const count : integer);
    ///<summary>
    ///  Returns the next complete message, if one has arrived. Blank lines are skipped rather
    ///  than returned as empty messages - some clients pad with them and an empty line is not
    ///  a message.
    ///</summary>
    function TryGetMessage(out message : string) : boolean;
    ///<summary> Bytes currently buffered but not yet forming a complete message. </summary>
    function PendingByteCount : integer;
  end;

implementation

const
  cLF = 10;
  cCR = 13;
  cInitialCapacity = 8192;

{ TMCPFrameBuffer }

constructor TMCPFrameBuffer.Create;
begin
  inherited;
  SetLength(FBuffer, cInitialCapacity);
  FLength := 0;
  FSeenFirstMessage := false;
end;

procedure TMCPFrameBuffer.AppendBytes(const chunk : TBytes; const count : integer);
var
  required : integer;
  capacity : integer;
begin
  if count <= 0 then
    exit;

  required := FLength + count;
  if required > Length(FBuffer) then
  begin
    capacity := Length(FBuffer);
    if capacity = 0 then
      capacity := cInitialCapacity;
    while capacity < required do
      capacity := capacity * 2;
    SetLength(FBuffer, capacity);
  end;

  Move(chunk[0], FBuffer[FLength], count);
  FLength := required;
end;

procedure TMCPFrameBuffer.Consume(const count : integer);
begin
  if count >= FLength then
  begin
    FLength := 0;
    exit;
  end;
  Move(FBuffer[count], FBuffer[0], FLength - count);
  Dec(FLength, count);
end;

function TMCPFrameBuffer.PendingByteCount : integer;
begin
  result := FLength;
end;

function TMCPFrameBuffer.TryGetMessage(out message : string) : boolean;
var
  i : integer;
  lineEnd : integer;
  lineStart : integer;
  lineBytes : TBytes;
  lineLen : integer;
begin
  result := false;
  message := '';

  while true do
  begin
    lineEnd := -1;
    for i := 0 to FLength - 1 do
    begin
      if FBuffer[i] = cLF then
      begin
        lineEnd := i;
        break;
      end;
    end;

    //No delimiter yet - the rest of this message has not arrived.
    if lineEnd < 0 then
      exit;

    lineStart := 0;
    lineLen := lineEnd;

    //Be liberal about CRLF. The spec says LF, but a client on Windows that used a text mode
    //stream will send CRLF, and rejecting those would be an unhelpful way to fail.
    if (lineLen > 0) and (FBuffer[lineEnd - 1] = cCR) then
      Dec(lineLen);

    //A BOM is not legal in JSON, but an over helpful writer can emit one on its first write.
    //Strip it once, on the first message only.
    if (not FSeenFirstMessage) and (lineLen >= 3) and
       (FBuffer[0] = $EF) and (FBuffer[1] = $BB) and (FBuffer[2] = $BF) then
    begin
      Inc(lineStart, 3);
      Dec(lineLen, 3);
    end;

    SetLength(lineBytes, lineLen);
    if lineLen > 0 then
      Move(FBuffer[lineStart], lineBytes[0], lineLen);

    Consume(lineEnd + 1);

    if lineLen > 0 then
    begin
      FSeenFirstMessage := true;
      message := TEncoding.UTF8.GetString(lineBytes);
      //A line of nothing but whitespace is padding, not a message.
      if Trim(message) <> '' then
        exit(true);
      message := '';
    end;
    //Blank line - keep looking.
  end;
end;

end.
