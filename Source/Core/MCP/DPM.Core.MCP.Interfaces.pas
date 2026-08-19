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

unit DPM.Core.MCP.Interfaces;

interface

uses
  System.SysUtils,
  Spring.Collections,
  VSoft.CancellationToken,
  VSoft.YAML;

type
  ///<summary>
  ///  Raised by a tool for anything the caller could correct - a bad compiler string, a
  ///  project that is not there, a feed that would not answer.
  ///</summary>
  ///<remarks>
  ///  The server turns this into a result with isError true rather than a JSON-RPC error.
  ///  That distinction matters: protocol errors are usually swallowed by the client, while
  ///  tool errors are handed to the model, which can read the message and retry. So the
  ///  message should say what was wrong AND what a valid value looks like.
  ///</remarks>
  EMCPToolError = class(Exception);

  ///<summary> A JSON-RPC id. MCP forbids null ids on requests, but an error reply to an
  ///  unparseable message still has to carry one, so the absent/null cases are modelled. </summary>
  TMCPRequestIdKind = (ridAbsent, ridNull, ridInt, ridStr);

  TMCPRequestId = record
    Kind : TMCPRequestIdKind;
    IntValue : Int64;
    StrValue : string;
    ///<summary> True for a message that is a notification - no id, so no reply may be sent. </summary>
    function IsAbsent : boolean;
    class function CreateNull : TMCPRequestId; static;
  end;

  ///<summary> Reads one framed message at a time. Returns false at end of stream. </summary>
  IMCPMessageReader = interface
    ['{7B1E5A20-4C8D-4A0E-9F31-2E7C0B6A54D1}']
    function ReadMessage(out message : string) : boolean;
  end;

  ///<summary> Writes one framed message. Implementations own the framing. </summary>
  IMCPMessageWriter = interface
    ['{0F4A9C33-6D12-4B77-8C55-A1D3E9B27F60}']
    procedure WriteMessage(const message : string);
  end;

  ///<summary>
  ///  One MCP tool. Implementations must be read only - nothing here may modify a project,
  ///  download a package, or write to disk.
  ///</summary>
  IMCPTool = interface
    ['{2C6B8E41-9A57-4D23-B0F8-5E1C7A94D3B2}']
    function GetName : string;
    function GetTitle : string;
    function GetDescription : string;
    ///<summary> True when the tool reaches out to the network - drives openWorldHint. </summary>
    function GetIsOpenWorld : boolean;
    ///<summary> Fills target with the tool's JSON Schema (type object, properties, required). </summary>
    procedure BuildInputSchema(const target : IYAMLMapping);
    ///<summary>
    ///  Runs the tool and returns the JSON document to hand back as text content.
    ///  Raise EMCPToolError for anything the caller could correct - a bad compiler string, a
    ///  project that is not there, an unreachable feed. The server turns that into a result
    ///  with isError true rather than a protocol error, so the model can see it and retry.
    ///</summary>
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;

    property Name : string read GetName;
    property Title : string read GetTitle;
    property Description : string read GetDescription;
    property IsOpenWorld : boolean read GetIsOpenWorld;
  end;

  IMCPToolRegistry = interface
    ['{9D3F1A08-5B64-4E90-A7C2-D6408F5B1E73}']
    procedure Add(const tool : IMCPTool);
    function TryGetTool(const name : string; out tool : IMCPTool) : boolean;
    function GetTools : IReadOnlyList<IMCPTool>;
    property Tools : IReadOnlyList<IMCPTool> read GetTools;
  end;

  ///<summary> Default registry. Tool names are case sensitive, as the spec requires. </summary>
  TMCPToolRegistry = class(TInterfacedObject, IMCPToolRegistry)
  private
    FTools : IList<IMCPTool>;
    FByName : IDictionary<string, IMCPTool>;
  protected
    procedure Add(const tool : IMCPTool);
    function TryGetTool(const name : string; out tool : IMCPTool) : boolean;
    function GetTools : IReadOnlyList<IMCPTool>;
  public
    constructor Create;
  end;

implementation

{ TMCPRequestId }

function TMCPRequestId.IsAbsent : boolean;
begin
  result := Kind = TMCPRequestIdKind.ridAbsent;
end;

class function TMCPRequestId.CreateNull : TMCPRequestId;
begin
  result.Kind := TMCPRequestIdKind.ridNull;
  result.IntValue := 0;
  result.StrValue := '';
end;

{ TMCPToolRegistry }

constructor TMCPToolRegistry.Create;
begin
  inherited;
  FTools := TCollections.CreateList<IMCPTool>;
  //Ordinal comparison - tool names are case sensitive per the spec, so a lookup must not
  //match a differently cased name.
  FByName := TCollections.CreateDictionary<string, IMCPTool>;
end;

procedure TMCPToolRegistry.Add(const tool : IMCPTool);
begin
  FTools.Add(tool);
  FByName[tool.Name] := tool;
end;

function TMCPToolRegistry.GetTools : IReadOnlyList<IMCPTool>;
begin
  //Registration order is stable, which gives tools/list the deterministic ordering the spec
  //asks for so clients can cache it.
  result := FTools.AsReadOnly;
end;

function TMCPToolRegistry.TryGetTool(const name : string; out tool : IMCPTool) : boolean;
begin
  result := FByName.TryGetValue(name, tool);
end;

end.
