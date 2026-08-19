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

unit DPM.Core.MCP.Server;

interface

uses
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.Logging,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.Protocol;

type
  ///<summary>
  ///  The MCP read/dispatch loop. Knows the protocol; knows nothing about DPM.
  ///</summary>
  ///<remarks>
  ///  Exactly three things end the loop: end of stream on the reader (the client closed our
  ///  stdin - the primary and only portable shutdown signal), a write failure (nothing useful
  ///  can be said if the client is gone), and cancellation. Every other condition is answered
  ///  and the session continues. A server that dies on one malformed frame is useless.
  ///</remarks>
  TMCPServer = class
  private
    FReader : IMCPMessageReader;
    FWriter : IMCPMessageWriter;
    FRegistry : IMCPToolRegistry;
    FLogger : ILogger;
    FInstructions : string;
    procedure Send(const doc : IYAMLDocument);
    procedure SendError(const id : TMCPRequestId; const code : integer; const message : string);
    procedure AddToolsCapability(const target : IYAMLMapping);
    procedure HandleInitialize(const message : TMCPMessage);
    procedure HandleDiscover(const message : TMCPMessage);
    procedure HandleToolsList(const message : TMCPMessage);
    procedure HandleToolsCall(const cancellationToken : ICancellationToken; const message : TMCPMessage);
    procedure HandlePing(const message : TMCPMessage);
    procedure DispatchParseFailure(const outcome : TMCPParseOutcome; const message : TMCPMessage);
    procedure DispatchMessage(const cancellationToken : ICancellationToken; const text : string);
    //Deliberate shadows of the System intrinsics. This class is outside the TConsoleBase
    //hierarchy that already guards against them, and a stray WriteLn here would write into the
    //JSON-RPC stream on stdout - silently corrupting the session. Raising turns a typo into an
    //immediate loud failure instead. Not Assert, which compiles out in Release.
    procedure WriteLn(const s : string = '');
    procedure Write(const s : string = '');
  public
    constructor Create(const reader : IMCPMessageReader; const writer : IMCPMessageWriter;
                       const registry : IMCPToolRegistry; const logger : ILogger;
                       const instructions : string);
    ///<summary> Runs until end of stream, cancellation, or an unwritable output. </summary>
    procedure Run(const cancellationToken : ICancellationToken);
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Version,
  DPM.Core.Json.Utils;

{ TMCPServer }

constructor TMCPServer.Create(const reader : IMCPMessageReader; const writer : IMCPMessageWriter;
                              const registry : IMCPToolRegistry; const logger : ILogger;
                              const instructions : string);
begin
  inherited Create;
  FReader := reader;
  FWriter := writer;
  FRegistry := registry;
  FLogger := logger;
  FInstructions := instructions;
end;

procedure TMCPServer.WriteLn(const s : string);
begin
  raise Exception.Create('System.WriteLn called inside the MCP server - that would corrupt stdout');
end;

procedure TMCPServer.Write(const s : string);
begin
  raise Exception.Create('System.Write called inside the MCP server - that would corrupt stdout');
end;

procedure TMCPServer.Send(const doc : IYAMLDocument);
begin
  //Compact, always. Pretty printing would put raw newlines inside a frame, and the framing is
  //one message per line.
  FWriter.WriteMessage(TJsonUtils.ToCompactJson(doc));
end;

procedure TMCPServer.SendError(const id : TMCPRequestId; const code : integer; const message : string);
begin
  Send(TMCPProtocol.BuildError(id, code, message));
end;

procedure TMCPServer.AddToolsCapability(const target : IYAMLMapping);
var
  tools : IYAMLMapping;
begin
  tools := target.AddOrSetMapping('tools');
  //We never change the tool set at runtime, so say so rather than declaring a capability we
  //do not implement.
  tools.AddOrSetValue('listChanged', false);
end;

procedure TMCPServer.HandleInitialize(const message : TMCPMessage);
var
  doc : IYAMLDocument;
  body : IYAMLMapping;
  capabilities : IYAMLMapping;
  serverInfo : IYAMLMapping;
  requested : string;
  negotiated : string;
begin
  //Legacy handshake. Modern clients never send this - they carry their version per request
  //and call server/discover instead.
  requested := '';
  if message.Params <> nil then
    requested := message.Params.S['protocolVersion'];

  //Never answers with the modern revision - see TMCPProtocol.NegotiateLegacyVersion for why
  //that would leave the client connected but unable to do anything.
  negotiated := TMCPProtocol.NegotiateLegacyVersion(requested);

  //On stderr, so it is visible in the client's server log without touching stdout. The
  //handshake is the first thing to check when a client connects but shows no tools.
  FLogger.Information('mcp: initialize - client requested ' +
    IfThen(requested = '', '(none)', requested) + ', negotiated ' + negotiated);

  doc := TMCPProtocol.BeginResult(message.Id, body);
  body.AddOrSetValue('protocolVersion', negotiated);
  capabilities := body.AddOrSetMapping('capabilities');
  AddToolsCapability(capabilities);

  //Legacy puts serverInfo at the top of the result; modern puts it in _meta (BeginResult has
  //already done that). Emitting both keeps one code path for two eras.
  serverInfo := body.AddOrSetMapping('serverInfo');
  serverInfo.AddOrSetValue('name', cMCPServerName);
  serverInfo.AddOrSetValue('version', TDPMVersion.CurrentVersionString);

  TJsonUtils.AddIfNotEmpty(body, 'instructions', FInstructions);
  Send(doc);
end;

procedure TMCPServer.HandleDiscover(const message : TMCPMessage);
var
  doc : IYAMLDocument;
  body : IYAMLMapping;
  capabilities : IYAMLMapping;
begin
  doc := TMCPProtocol.BeginResult(message.Id, body);
  TMCPProtocol.AddSupportedVersions(body, 'supportedVersions');
  capabilities := body.AddOrSetMapping('capabilities');
  AddToolsCapability(capabilities);
  TJsonUtils.AddIfNotEmpty(body, 'instructions', FInstructions);
  //Private, not public: the instructions name this machine's project path, which has no
  //business being served from a shared cache to anyone else.
  TMCPProtocol.AddCachingHints(body, cMCPListTtlMs, cMCPCacheScopePrivate);
  Send(doc);
end;

procedure TMCPServer.HandlePing(const message : TMCPMessage);
var
  doc : IYAMLDocument;
  body : IYAMLMapping;
begin
  doc := TMCPProtocol.BeginResult(message.Id, body);
  Send(doc);
end;

procedure TMCPServer.HandleToolsList(const message : TMCPMessage);
var
  doc : IYAMLDocument;
  body : IYAMLMapping;
  toolsSeq : IYAMLSequence;
  toolObj : IYAMLMapping;
  annotations : IYAMLMapping;
  tool : IMCPTool;
begin
  doc := TMCPProtocol.BeginResult(message.Id, body);
  toolsSeq := body.AddOrSetSequence('tools');

  for tool in FRegistry.Tools do
  begin
    toolObj := toolsSeq.AddMapping;
    toolObj.AddOrSetValue('name', tool.Name);
    TJsonUtils.AddIfNotEmpty(toolObj, 'title', tool.Title);
    TJsonUtils.AddIfNotEmpty(toolObj, 'description', tool.Description);
    tool.BuildInputSchema(toolObj.AddOrSetMapping('inputSchema'));

    annotations := toolObj.AddOrSetMapping('annotations');
    //Every tool here is a query. Saying so lets a client auto approve them.
    annotations.AddOrSetValue('readOnlyHint', true);
    annotations.AddOrSetValue('destructiveHint', false);
    annotations.AddOrSetValue('idempotentHint', true);
    annotations.AddOrSetValue('openWorldHint', tool.IsOpenWorld);
  end;

  //Public: the tool set is identical for every caller - same names, same schemas, no
  //per-user filtering - which is exactly the case the spec calls out for 'public'.
  TMCPProtocol.AddCachingHints(body, cMCPListTtlMs, cMCPCacheScopePublic);

  //No nextCursor - the tool set is small and fixed, so there is never a second page.
  Send(doc);
end;

procedure TMCPServer.HandleToolsCall(const cancellationToken : ICancellationToken; const message : TMCPMessage);
var
  doc : IYAMLDocument;
  body : IYAMLMapping;
  content : IYAMLSequence;
  textBlock : IYAMLMapping;
  toolName : string;
  tool : IMCPTool;
  argumentsValue : IYAMLValue;
  arguments : IYAMLMapping;
  toolResult : string;
  isError : boolean;
  emptyArgsDoc : IYAMLDocument;

  procedure Reply(const text : string; const failed : boolean);
  begin
    doc := TMCPProtocol.BeginResult(message.Id, body);
    content := body.AddOrSetSequence('content');
    textBlock := content.AddMapping;
    textBlock.AddOrSetValue('type', 'text');
    textBlock.AddOrSetValue('text', text);
    body.AddOrSetValue('isError', failed);
    Send(doc);
  end;

begin
  if message.Params = nil then
  begin
    SendError(message.Id, cJsonRpcInvalidParams, 'Missing params for tools/call');
    exit;
  end;

  toolName := message.Params.S['name'];
  if toolName = '' then
  begin
    SendError(message.Id, cJsonRpcInvalidParams, 'Missing tool name');
    exit;
  end;

  //An unknown tool is a protocol error, not a tool error - the model cannot fix it by
  //adjusting arguments, and the spec names -32602 for exactly this.
  if not FRegistry.TryGetTool(toolName, tool) then
  begin
    SendError(message.Id, cJsonRpcInvalidParams, 'Unknown tool: ' + toolName);
    exit;
  end;

  arguments := nil;
  if message.Params.TryGetValue('arguments', argumentsValue) then
  begin
    if not argumentsValue.IsMapping then
    begin
      SendError(message.Id, cJsonRpcInvalidParams, 'tools/call arguments must be an object');
      exit;
    end;
    arguments := argumentsValue.AsMapping;
  end;
  if arguments = nil then
  begin
    //Hold the document in a local: the root mapping belongs to it, so letting a temporary
    //document go out of scope here would leave `arguments` dangling.
    emptyArgsDoc := TYAML.CreateMapping;
    arguments := emptyArgsDoc.AsMapping;
  end;

  isError := false;
  toolResult := '';
  try
    toolResult := tool.Invoke(cancellationToken, arguments);
  except
    //Anything a caller could plausibly correct comes back as a tool error with isError true,
    //so the model can read it and try again. Only a genuine internal fault is a protocol error.
    on E : EMCPToolError do
    begin
      toolResult := E.Message;
      isError := true;
    end;
    on E : Exception do
    begin
      FLogger.Error('mcp: unhandled exception in tool ' + toolName + ' : ' + E.Message);
      SendError(message.Id, cJsonRpcInternalError, E.ClassName + ': ' + E.Message);
      exit;
    end;
  end;

  Reply(toolResult, isError);
end;

procedure TMCPServer.DispatchParseFailure(const outcome : TMCPParseOutcome; const message : TMCPMessage);
begin
  case outcome of
    TMCPParseOutcome.poNotJson :
      SendError(TMCPRequestId.CreateNull, cJsonRpcParseError, 'Parse error - not valid JSON');
    TMCPParseOutcome.poNotAnObject :
      SendError(TMCPRequestId.CreateNull, cJsonRpcParseError, 'Parse error - message must be a JSON object');
    TMCPParseOutcome.poBatch :
      //Batching was removed in the modern revision. Half supporting it would be worse than
      //saying plainly that we do not.
      SendError(TMCPRequestId.CreateNull, cJsonRpcInvalidRequest, 'JSON-RPC batches are not supported');
    TMCPParseOutcome.poNotJsonRpc2 :
      if message.IsRequest then
        SendError(message.Id, cJsonRpcInvalidRequest, 'Invalid request - jsonrpc must be "2.0"');
    TMCPParseOutcome.poNoMethod :
      if message.IsRequest then
        SendError(message.Id, cJsonRpcInvalidRequest, 'Invalid request - missing method');
  end;
end;

procedure TMCPServer.DispatchMessage(const cancellationToken : ICancellationToken; const text : string);
var
  message : TMCPMessage;
  outcome : TMCPParseOutcome;
begin
  outcome := TMCPProtocol.TryParseMessage(text, message);
  if outcome <> TMCPParseOutcome.poOk then
  begin
    DispatchParseFailure(outcome, message);
    exit;
  end;

  //A modern client states its version on every request. Reject one we cannot serve, naming
  //what we can - that is what lets the client retry rather than give up.
  if message.IsModern and (not TMCPProtocol.IsSupportedVersion(message.ProtocolVersion)) then
  begin
    if message.IsRequest then
      Send(TMCPProtocol.BuildUnsupportedVersionError(message.Id, message.ProtocolVersion));
    exit;
  end;

  //Note there is deliberately no "not initialized yet" check. The modern protocol is stateless
  //and has no handshake at all, so refusing work before an initialize we may never receive
  //would break every modern client.
  if message.Method = cMethodInitialize then
    HandleInitialize(message)
  else if message.Method = cMethodDiscover then
    HandleDiscover(message)
  else if message.Method = cMethodToolsList then
    HandleToolsList(message)
  else if message.Method = cMethodToolsCall then
    HandleToolsCall(cancellationToken, message)
  else if message.Method = cMethodPing then
    HandlePing(message)
  else if (message.Method = cMethodInitialized) or (message.Method = cMethodCancelled) then
  begin
    //Accepted and ignored. Cancellation cannot be actioned because the loop is single
    //threaded - by the time the notification is read, the request it refers to has already
    //been answered.
  end
  else if message.IsRequest then
    SendError(message.Id, cJsonRpcMethodNotFound, 'Method not found: ' + message.Method);
  //An unknown NOTIFICATION gets no reply at all - a notification never receives one, not even
  //an error.
end;

procedure TMCPServer.Run(const cancellationToken : ICancellationToken);
var
  text : string;
begin
  while FReader.ReadMessage(text) do
  begin
    if cancellationToken.IsCancelled then
      exit;
    try
      DispatchMessage(cancellationToken, text);
    except
      //A write failure means the client is gone; there is nowhere to report it.
      on E : Exception do
      begin
        FLogger.Error('mcp: ' + E.Message);
        exit;
      end;
    end;
  end;
  //Reader returned false: end of stream. The client closed our stdin, which is the graceful
  //shutdown signal, so exit promptly.
end;

end.
