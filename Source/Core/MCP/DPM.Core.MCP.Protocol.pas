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

unit DPM.Core.MCP.Protocol;

interface

uses
  VSoft.YAML,
  DPM.Core.MCP.Interfaces;

{
  JSON-RPC / MCP codec. No I/O, no tools, no knowledge of the read loop - just turning text
  into a parsed message and turning results into envelopes.

  Two protocol eras
  -----------------
  MCP changed shape in revision 2026-07-28:

    - MODERN (2026-07-28 and later) is stateless. There is no initialize handshake. Every
      request carries its protocol version and the client capabilities in
      _meta['io.modelcontextprotocol/*'], servers MUST implement server/discover, and every
      result MUST carry a resultType.

    - LEGACY (2025-11-25 and earlier) establishes a session with an initialize handshake and
      has no resultType.

  The spec's own compatibility matrix says a modern client talking to a legacy-only server
  fails, and a legacy client talking to a modern-only server fails. Since we cannot know which
  era a given editor speaks, this server implements BOTH - which the spec explicitly allows
  ("dual-era"). The cost is small: the tools, framing and transport are shared, and only the
  dispatch layer differs.

  resultType is emitted on every result, including replies to legacy clients. That is safe:
  legacy clients ignore unknown result fields, and the modern spec requires it.
}

const
  //Newest first - the order is meaningful, callers pick the first mutually supported entry.
  cMCPVersionModern = '2026-07-28';

  //Legacy revisions, newest first. We advertise all of them because the subset this server
  //implements - initialize, tools/list, tools/call, ping - is wire identical across every
  //one of them. Refusing a client over a revision difference that does not affect anything
  //we actually do would break working clients for no benefit.
  cMCPVersionLegacy2025_11 = '2025-11-25';
  cMCPVersionLegacy2025_06 = '2025-06-18';
  cMCPVersionLegacy2025_03 = '2025-03-26';
  cMCPVersionLegacy2024_11 = '2024-11-05';

  //What an initialize handshake falls back to when the client asks for something we do not
  //recognise. It MUST be a legacy revision - see NegotiateLegacyVersion.
  cMCPVersionNewestLegacy = cMCPVersionLegacy2025_11;

  cMCPServerName = 'dpm';

  //Caching hints. From revision 2026-07-28 these are REQUIRED - not optional - on any
  //resultType 'complete' returned by server/discover, tools/list, prompts/list,
  //resources/list, resources/templates/list and resources/read. A client validating against
  //that schema rejects the whole result when they are missing, which surfaces as a server
  //that connects but whose tools never load.
  cMCPCacheScopePublic = 'public';
  cMCPCacheScopePrivate = 'private';

  //Five minutes. The tool set is fixed for the life of the process, but a restart can pick
  //up a different project, so this is deliberately short rather than indefinite.
  cMCPListTtlMs = 300000;

  //Standard JSON-RPC 2.0 codes.
  cJsonRpcParseError = -32700;
  cJsonRpcInvalidRequest = -32600;
  cJsonRpcMethodNotFound = -32601;
  cJsonRpcInvalidParams = -32602;
  cJsonRpcInternalError = -32603;

  //MCP defined. The -32020..-32099 sub range belongs to the specification - we must not invent
  //codes in it, and must use the defined ones only with their defined meaning.
  cMCPUnsupportedProtocolVersion = -32022;

  //Reserved _meta keys.
  cMetaProtocolVersion = 'io.modelcontextprotocol/protocolVersion';
  cMetaClientInfo = 'io.modelcontextprotocol/clientInfo';
  cMetaClientCapabilities = 'io.modelcontextprotocol/clientCapabilities';
  cMetaServerInfo = 'io.modelcontextprotocol/serverInfo';

  //Methods.
  cMethodInitialize = 'initialize';
  cMethodInitialized = 'notifications/initialized';
  cMethodDiscover = 'server/discover';
  cMethodToolsList = 'tools/list';
  cMethodToolsCall = 'tools/call';
  cMethodPing = 'ping';
  cMethodCancelled = 'notifications/cancelled';

type
  ///<summary> A parsed inbound message. </summary>
  TMCPMessage = record
    Id : TMCPRequestId;
    Method : string;
    Params : IYAMLMapping;
    ///<summary> True when the message carried no id, so no reply may be sent. </summary>
    IsNotification : boolean;
    ///<summary> Protocol version from _meta, empty when absent (i.e. a legacy client). </summary>
    ProtocolVersion : string;
    ///<summary> True when _meta carried the modern per request protocol fields. </summary>
    IsModern : boolean;
    function IsRequest : boolean;
  end;

  TMCPParseOutcome = (poOk, poNotJson, poNotAnObject, poBatch, poNotJsonRpc2, poNoMethod);

  TMCPProtocol = record
  public
    ///<summary> Parses one framed message. On failure the outcome says which error to emit. </summary>
    class function TryParseMessage(const text : string; out message : TMCPMessage) : TMCPParseOutcome; static;

    ///<summary> True when version is one this server can serve. </summary>
    class function IsSupportedVersion(const version : string) : boolean; static;

    ///<summary>
    ///  Picks the protocol version to return from an initialize handshake.
    ///</summary>
    ///<remarks>
    ///  Always a LEGACY revision. A client that sent initialize is by definition legacy era,
    ///  so answering with the modern revision would be self contradictory - the modern
    ///  protocol has no initialize handshake at all. A client told to speak it would have no
    ///  coherent way to continue, and the symptom is a server that looks connected but never
    ///  lists any tools.
    ///</remarks>
    class function NegotiateLegacyVersion(const requested : string) : string; static;

    ///<summary> Adds supportedVersions to a mapping, newest first. </summary>
    class procedure AddSupportedVersions(const target : IYAMLMapping; const key : string); static;

    ///<summary>
    ///  Builds a result envelope. The caller fills the returned mapping with the result body;
    ///  resultType and the server identity are already set.
    ///</summary>
    class function BeginResult(const id : TMCPRequestId; out resultBody : IYAMLMapping) : IYAMLDocument; static;

    class function BuildError(const id : TMCPRequestId; const code : integer; const message : string) : IYAMLDocument; static;

    ///<summary> The -32022 reply, carrying the versions we do support. </summary>
    class function BuildUnsupportedVersionError(const id : TMCPRequestId; const requested : string) : IYAMLDocument; static;

    class procedure AddServerInfo(const target : IYAMLMapping); static;

    ///<summary>
    ///  Adds the ttlMs and cacheScope hints a cacheable result must carry.
    ///</summary>
    ///<remarks>
    ///  Only for the operations the spec lists as cacheable. Adding them to something like
    ///  tools/call would be wrong - that result is not cacheable at all.
    ///</remarks>
    class procedure AddCachingHints(const target : IYAMLMapping; const ttlMs : integer;
                                    const cacheScope : string); static;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Version;

{ TMCPMessage }

function TMCPMessage.IsRequest : boolean;
begin
  result := not IsNotification;
end;

{ TMCPProtocol }

class function TMCPProtocol.IsSupportedVersion(const version : string) : boolean;
begin
  result := (version = cMCPVersionModern) or
            (version = cMCPVersionLegacy2025_11) or
            (version = cMCPVersionLegacy2025_06) or
            (version = cMCPVersionLegacy2025_03) or
            (version = cMCPVersionLegacy2024_11);
end;

class function TMCPProtocol.NegotiateLegacyVersion(const requested : string) : string;
begin
  //Echo back anything we know, so the client keeps the revision it asked for.
  if (requested <> cMCPVersionModern) and IsSupportedVersion(requested) then
    exit(requested);

  //Unknown, or the modern revision arriving down the legacy path. Answer with the newest
  //legacy revision - never the modern one, which has no handshake for the client to use.
  result := cMCPVersionNewestLegacy;
end;

class procedure TMCPProtocol.AddSupportedVersions(const target : IYAMLMapping; const key : string);
var
  seq : IYAMLSequence;
begin
  seq := target.AddOrSetSequence(key);
  seq.AddValue(cMCPVersionModern);
  seq.AddValue(cMCPVersionLegacy2025_11);
  seq.AddValue(cMCPVersionLegacy2025_06);
  seq.AddValue(cMCPVersionLegacy2025_03);
  seq.AddValue(cMCPVersionLegacy2024_11);
end;

class procedure TMCPProtocol.AddServerInfo(const target : IYAMLMapping);
var
  info : IYAMLMapping;
begin
  info := target.AddOrSetMapping(cMetaServerInfo);
  info.AddOrSetValue('name', cMCPServerName);
  info.AddOrSetValue('version', TDPMVersion.CurrentVersionString);
end;

class procedure TMCPProtocol.AddCachingHints(const target : IYAMLMapping; const ttlMs : integer;
                                             const cacheScope : string);
begin
  //Must be >= 0 per the spec; 0 means treat as immediately stale.
  if ttlMs < 0 then
    target.AddOrSetValue('ttlMs', 0)
  else
    target.AddOrSetValue('ttlMs', ttlMs);
  target.AddOrSetValue('cacheScope', cacheScope);
end;

class function TMCPProtocol.TryParseMessage(const text : string; out message : TMCPMessage) : TMCPParseOutcome;
var
  doc : IYAMLDocument;
  root : IYAMLMapping;
  idValue : IYAMLValue;
  metaValue : IYAMLValue;
  meta : IYAMLMapping;
  versionValue : IYAMLValue;
begin
  message.Id.Kind := TMCPRequestIdKind.ridAbsent;
  message.Id.IntValue := 0;
  message.Id.StrValue := '';
  message.Method := '';
  message.Params := nil;
  message.IsNotification := true;
  message.ProtocolVersion := '';
  message.IsModern := false;

  try
    doc := TYAML.LoadFromString(text);
  except
    //Deliberately broad. A malformed frame must not end the session, and every parse failure
    //means the same thing to the client.
    on E : Exception do
      exit(TMCPParseOutcome.poNotJson);
  end;

  if doc = nil then
    exit(TMCPParseOutcome.poNotJson);

  //Batching existed in earlier revisions and is gone from the modern one. Rejecting a
  //top level array with a clear error beats half supporting it.
  if doc.IsSequence then
    exit(TMCPParseOutcome.poBatch);

  if not doc.IsMapping then
    exit(TMCPParseOutcome.poNotAnObject);

  root := doc.AsMapping;

  //Read the id before validating anything else, so an error reply can be correlated.
  if root.TryGetValue('id', idValue) then
  begin
    if idValue.IsNull then
      message.Id.Kind := TMCPRequestIdKind.ridNull
    else if idValue.IsString then
    begin
      message.Id.Kind := TMCPRequestIdKind.ridStr;
      message.Id.StrValue := idValue.AsString;
    end
    else
    begin
      message.Id.Kind := TMCPRequestIdKind.ridInt;
      message.Id.IntValue := idValue.AsInteger;
    end;
    message.IsNotification := message.Id.Kind = TMCPRequestIdKind.ridNull;
  end;

  if root.S['jsonrpc'] <> '2.0' then
    exit(TMCPParseOutcome.poNotJsonRpc2);

  message.Method := root.S['method'];
  if message.Method = '' then
    exit(TMCPParseOutcome.poNoMethod);

  if root.TryGetValue('params', metaValue) and metaValue.IsMapping then
  begin
    message.Params := metaValue.AsMapping;
    if message.Params.TryGetValue('_meta', metaValue) and metaValue.IsMapping then
    begin
      meta := metaValue.AsMapping;
      if meta.TryGetValue(cMetaProtocolVersion, versionValue) and versionValue.IsString then
      begin
        message.ProtocolVersion := versionValue.AsString;
        message.IsModern := message.ProtocolVersion <> '';
      end;
    end;
  end;

  result := TMCPParseOutcome.poOk;
end;

procedure SetId(const envelope : IYAMLMapping; const id : TMCPRequestId);
begin
  case id.Kind of
    TMCPRequestIdKind.ridInt : envelope.AddOrSetValue('id', id.IntValue);
    TMCPRequestIdKind.ridStr : envelope.AddOrSetValue('id', id.StrValue);
  else
    //JSON-RPC wants a null id when the request id could not be determined.
    envelope.AddOrSetNull('id');
  end;
end;

class function TMCPProtocol.BeginResult(const id : TMCPRequestId; out resultBody : IYAMLMapping) : IYAMLDocument;
var
  envelope : IYAMLMapping;
  meta : IYAMLMapping;
begin
  result := TYAML.CreateMapping;
  envelope := result.AsMapping;
  envelope.AddOrSetValue('jsonrpc', '2.0');
  SetId(envelope, id);

  resultBody := envelope.AddOrSetMapping('result');
  //Required by the modern revision; harmless to legacy clients, which are told to treat an
  //absent resultType as complete and to ignore fields they do not know.
  resultBody.AddOrSetValue('resultType', 'complete');

  meta := resultBody.AddOrSetMapping('_meta');
  AddServerInfo(meta);
end;

class function TMCPProtocol.BuildError(const id : TMCPRequestId; const code : integer; const message : string) : IYAMLDocument;
var
  envelope : IYAMLMapping;
  errorObj : IYAMLMapping;
begin
  result := TYAML.CreateMapping;
  envelope := result.AsMapping;
  envelope.AddOrSetValue('jsonrpc', '2.0');
  SetId(envelope, id);

  errorObj := envelope.AddOrSetMapping('error');
  errorObj.AddOrSetValue('code', code);
  errorObj.AddOrSetValue('message', message);
end;

class function TMCPProtocol.BuildUnsupportedVersionError(const id : TMCPRequestId; const requested : string) : IYAMLDocument;
var
  envelope : IYAMLMapping;
  errorObj : IYAMLMapping;
  data : IYAMLMapping;
begin
  result := TYAML.CreateMapping;
  envelope := result.AsMapping;
  envelope.AddOrSetValue('jsonrpc', '2.0');
  SetId(envelope, id);

  errorObj := envelope.AddOrSetMapping('error');
  errorObj.AddOrSetValue('code', cMCPUnsupportedProtocolVersion);
  errorObj.AddOrSetValue('message', 'Unsupported protocol version');
  data := errorObj.AddOrSetMapping('data');
  AddSupportedVersions(data, 'supported');
  data.AddOrSetValue('requested', requested);
end;

end.
