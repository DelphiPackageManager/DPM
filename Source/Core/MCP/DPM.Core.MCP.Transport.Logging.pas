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

unit DPM.Core.MCP.Transport.Logging;

interface

uses
  DPM.Core.Logging,
  DPM.Core.MCP.Interfaces;

{
  Decorators that tee every MCP frame to a file, in both directions.

  Why this exists: when a client reports "connected, but no tools", nothing on either side
  tells you why. The client shows a summary, the server sees a request it answered
  successfully, and stdout cannot be used for diagnostics because it IS the protocol channel.
  The only way to settle it is to see the exact bytes exchanged.

  Enabled with 'dpm mcp --logfile=<path>'. Off by default - this writes everything a client
  sends, so it should be turned on deliberately and pointed somewhere temporary.

  Failures to write are swallowed. A diagnostic that can break the thing it is diagnosing is
  worse than no diagnostic.
}

type
  TMCPFrameLog = class
  private
    FFileName : string;
    FLogger : ILogger;
    FReportedFailure : boolean;
    procedure Append(const direction : string; const message : string);
  public
    ///<summary>
    ///  fileName may contain environment variables in %NAME% form. A client launches this
    ///  server as a direct subprocess rather than through a shell, so nothing else is going
    ///  to expand them - and an unexpanded %TEMP% is otherwise a directory that does not
    ///  exist, which fails silently at the first write.
    ///</summary>
    constructor Create(const fileName : string; const logger : ILogger);
    ///<summary> The path actually being written to, after expansion. </summary>
    property FileName : string read FFileName;
    procedure LogIncoming(const message : string);
    procedure LogOutgoing(const message : string);
    procedure LogNote(const note : string);
  end;

  TMCPLoggingReader = class(TInterfacedObject, IMCPMessageReader)
  private
    FInner : IMCPMessageReader;
    FLog : TMCPFrameLog;
  protected
    function ReadMessage(out message : string) : boolean;
  public
    constructor Create(const inner : IMCPMessageReader; const log : TMCPFrameLog);
  end;

  TMCPLoggingWriter = class(TInterfacedObject, IMCPMessageWriter)
  private
    FInner : IMCPMessageWriter;
    FLog : TMCPFrameLog;
  protected
    procedure WriteMessage(const message : string);
  public
    constructor Create(const inner : IMCPMessageWriter; const log : TMCPFrameLog);
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  System.SysUtils,
  System.Classes;

///<summary> Expands %NAME% style variables, since no shell is involved. </summary>
function ExpandVariables(const value : string) : string;
{$IFDEF MSWINDOWS}
var
  required : DWORD;
  buffer : string;
{$ENDIF}
begin
  result := value;
{$IFDEF MSWINDOWS}
  if Pos('%', value) = 0 then
    exit;
  required := ExpandEnvironmentStrings(PChar(value), nil, 0);
  if required = 0 then
    exit;
  SetLength(buffer, required);
  if ExpandEnvironmentStrings(PChar(value), PChar(buffer), required) = 0 then
    exit;
  //The returned length includes the terminating null.
  result := Trim(Copy(buffer, 1, required - 1));
{$ENDIF}
end;

{ TMCPFrameLog }

constructor TMCPFrameLog.Create(const fileName : string; const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FReportedFailure := false;
  FFileName := ExpandVariables(fileName);
end;

procedure TMCPFrameLog.Append(const direction : string; const message : string);
var
  stream : TFileStream;
  line : string;
  bytes : TBytes;
  mode : word;
begin
  try
    line := FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + direction + ' ' + message + sLineBreak;
    bytes := TEncoding.UTF8.GetBytes(line);

    if FileExists(FFileName) then
      mode := fmOpenReadWrite or fmShareDenyNone
    else
      mode := fmCreate or fmShareDenyNone;

    //Opened and closed per frame rather than held: the file stays readable while the server
    //is running, which is the whole point when a client is hanging.
    stream := TFileStream.Create(FFileName, mode);
    try
      stream.Seek(0, soFromEnd);
      if Length(bytes) > 0 then
        stream.WriteBuffer(bytes[0], Length(bytes));
    finally
      stream.Free;
    end;
  except
    //Never let logging break the session - but say so once, or an unwritable log looks
    //exactly like a server that was never started. That silence cost real debugging time.
    on E : Exception do
    begin
      if not FReportedFailure then
      begin
        FReportedFailure := true;
        if FLogger <> nil then
          FLogger.Error('mcp: cannot write the frame log at "' + FFileName + '" - ' + E.Message +
            '. Frame logging is disabled for this session; the server itself is unaffected.');
      end;
    end;
  end;
end;

procedure TMCPFrameLog.LogIncoming(const message : string);
begin
  Append('-->', message);
end;

procedure TMCPFrameLog.LogOutgoing(const message : string);
begin
  Append('<--', message);
end;

procedure TMCPFrameLog.LogNote(const note : string);
begin
  Append('###', note);
end;

{ TMCPLoggingReader }

constructor TMCPLoggingReader.Create(const inner : IMCPMessageReader; const log : TMCPFrameLog);
begin
  inherited Create;
  FInner := inner;
  FLog := log;
end;

function TMCPLoggingReader.ReadMessage(out message : string) : boolean;
begin
  result := FInner.ReadMessage(message);
  if result then
    FLog.LogIncoming(message)
  else
    FLog.LogNote('end of stdin - client closed the stream, exiting');
end;

{ TMCPLoggingWriter }

constructor TMCPLoggingWriter.Create(const inner : IMCPMessageWriter; const log : TMCPFrameLog);
begin
  inherited Create;
  FInner := inner;
  FLog := log;
end;

procedure TMCPLoggingWriter.WriteMessage(const message : string);
begin
  FLog.LogOutgoing(message);
  FInner.WriteMessage(message);
end;

end.
