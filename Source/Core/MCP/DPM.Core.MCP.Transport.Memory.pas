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

unit DPM.Core.MCP.Transport.Memory;

interface

uses
  Spring.Collections,
  DPM.Core.MCP.Interfaces;

{
  In-memory transport. This is what makes the server testable at all: a whole session can be
  scripted as a list of message strings and the entire stdout transcript asserted, with no
  pipe, no child process, and no timing dependency.
}

type
  TMCPMemoryReader = class(TInterfacedObject, IMCPMessageReader)
  private
    FMessages : IList<string>;
    FIndex : integer;
    FFailAfter : integer;
  protected
    function ReadMessage(out message : string) : boolean;
  public
    constructor Create(const messages : IList<string>); overload;
    constructor Create(const messages : array of string); overload;
  end;

  TMCPMemoryWriter = class(TInterfacedObject, IMCPMessageWriter)
  private
    FMessages : IList<string>;
    FFailOnWrite : boolean;
  protected
    procedure WriteMessage(const message : string);
  public
    constructor Create;
    ///<summary> Everything the server wrote, in order. </summary>
    property Messages : IList<string> read FMessages;
    ///<summary> Makes the next write raise, to exercise the unwritable stdout path. </summary>
    property FailOnWrite : boolean read FFailOnWrite write FFailOnWrite;
  end;

implementation

uses
  System.SysUtils;

{ TMCPMemoryReader }

constructor TMCPMemoryReader.Create(const messages : IList<string>);
begin
  inherited Create;
  FMessages := messages;
  FIndex := 0;
  FFailAfter := -1;
end;

constructor TMCPMemoryReader.Create(const messages : array of string);
var
  i : integer;
begin
  inherited Create;
  FMessages := TCollections.CreateList<string>;
  for i := Low(messages) to High(messages) do
    FMessages.Add(messages[i]);
  FIndex := 0;
  FFailAfter := -1;
end;

function TMCPMemoryReader.ReadMessage(out message : string) : boolean;
begin
  message := '';
  //Running out of scripted messages stands in for the client closing our stdin.
  if FIndex >= FMessages.Count then
    exit(false);
  message := FMessages[FIndex];
  Inc(FIndex);
  result := true;
end;

{ TMCPMemoryWriter }

constructor TMCPMemoryWriter.Create;
begin
  inherited Create;
  FMessages := TCollections.CreateList<string>;
  FFailOnWrite := false;
end;

procedure TMCPMemoryWriter.WriteMessage(const message : string);
begin
  if FFailOnWrite then
    raise Exception.Create('simulated write failure');
  FMessages.Add(message);
end;

end.
