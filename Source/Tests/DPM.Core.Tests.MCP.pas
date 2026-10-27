unit DPM.Core.Tests.MCP;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TMCPFrameBufferTests = class
  public
    [Test]
    procedure Two_Messages_In_One_Chunk;

    [Test]
    procedure One_Message_Split_Across_Three_Chunks;

    ///<summary> The highest value test here - see the comment in the body. </summary>
    [Test]
    procedure Multi_Byte_Utf8_Split_Across_A_Chunk_Boundary;

    [Test]
    procedure Crlf_Terminated_Input_Is_Accepted;

    [Test]
    procedure Blank_Lines_Are_Skipped;

    [Test]
    procedure Leading_Bom_Is_Stripped;

    [Test]
    procedure Partial_Line_Is_Not_Returned;

    //it is dropped and said to be bad - the lines after it still come out.
    [Test]
    procedure Line_That_Is_Not_Utf8_Is_A_Frame_Error;
    //what decoding would quietly have turned into replacement characters - and what is fine.
    [Test]
    procedure Strict_Utf8_Is_Required;
  end;

  {$M+}
  [TestFixture]
  TMCPProtocolTests = class
  public
    [Test]
    procedure Integer_Id_Round_Trips_As_An_Integer;

    [Test]
    procedure String_Id_Round_Trips_As_A_String;

    [Test]
    procedure Message_Without_Id_Is_A_Notification;

    [Test]
    procedure Malformed_Json_Is_A_Parse_Error;

    [Test]
    procedure Top_Level_Array_Is_Rejected_As_A_Batch;

    [Test]
    procedure Modern_Meta_Protocol_Version_Is_Read;

    [Test]
    procedure Absent_Meta_Means_Legacy;

    [Test]
    procedure Result_Envelope_Carries_ResultType_And_ServerInfo;
  end;

  {$M+}
  [TestFixture]
  TMCPServerTests = class
  public
    [Test]
    procedure Legacy_Initialize_Echoes_A_Supported_Version;

    [Test]
    procedure Modern_Discover_Lists_Supported_Versions;

    ///<summary>
    ///  An initialize handshake must never be answered with the modern revision - the modern
    ///  protocol has no handshake, so the client would be left connected with no coherent way
    ///  to continue. The symptom is a server that shows as connected but lists no tools.
    ///</summary>
    [Test]
    [TestCase('2025-11-25', '2025-11-25,2025-11-25')]
    [TestCase('2025-06-18', '2025-06-18,2025-06-18')]
    [TestCase('2025-03-26', '2025-03-26,2025-03-26')]
    [TestCase('2024-11-05', '2024-11-05,2024-11-05')]
    [TestCase('unknown', '1999-01-01,2025-11-25')]
    [TestCase('modern down the legacy path', '2026-07-28,2025-11-25')]
    procedure Initialize_Never_Negotiates_The_Modern_Version(const requested : string; const expected : string);

    [Test]
    procedure Notifications_Produce_No_Output;

    [Test]
    procedure Unknown_Method_Is_Method_Not_Found;

    [Test]
    procedure Unknown_Tool_Is_Invalid_Params;

    [Test]
    procedure Tool_Error_Is_Reported_In_The_Result_Not_As_A_Protocol_Error;

    [Test]
    procedure Unsupported_Modern_Version_Is_Rejected_With_The_Supported_List;

    ///<summary> A server that dies on one bad frame is useless. </summary>
    [Test]
    procedure A_Bad_Frame_Does_Not_End_The_Session;

    //bytes that aren't UTF-8 ended the session (the reader raised out of the loop).
    [Test]
    procedure A_Frame_That_Is_Not_Utf8_Is_A_Parse_Error;

    //an id JSON-RPC doesn't allow (a fraction, true, an object) ended the session.
    [Test]
    procedure An_Id_That_Is_Not_A_String_Or_Integer_Is_An_Invalid_Request;

    //a notification is never answered - and a notification of a method only a request may use is not carried out.
    [Test]
    procedure A_Request_Method_Sent_As_A_Notification_Is_Ignored;

    //our own fault handling one message - answered as an internal error, and the session goes on.
    [Test]
    procedure A_Fault_Handling_A_Message_Is_An_Internal_Error;

    //dpm mcp says (exit code) when the server stopped because something failed, not because the client went.
    [Test]
    procedure Run_Says_Whether_It_Ended_Cleanly;

    [Test]
    procedure Tools_List_Reports_Registered_Tools_In_Order;

    ///<summary>
    ///  From revision 2026-07-28 ttlMs and cacheScope are REQUIRED on every cacheable
    ///  result, not optional hints. A client validating the schema rejects the entire
    ///  result when they are missing, which looks like a server that connects fine but
    ///  whose tools never load - with nothing on either side saying why.
    ///</summary>
    [Test]
    procedure Tools_List_Carries_The_Required_Caching_Hints;

    [Test]
    procedure Discover_Carries_The_Required_Caching_Hints;

    ///<summary> tools/call is not a cacheable operation, so it must not claim to be. </summary>
    [Test]
    procedure Tools_Call_Does_Not_Carry_Caching_Hints;
  end;

  //The package tools, over a repository manager whose sources may not all answer.
  [TestFixture]
  TMCPPackageToolTests = class
  public
    //nothing found while a source couldn't be asked is not 'there is no such package' - it may be on that source.
    [Test]
    procedure Nothing_Found_With_A_Source_Unavailable_Is_A_Tool_Error;
    //what was found may not be all there is - the result says so.
    [Test]
    procedure Found_With_A_Source_Unavailable_Says_So;
    [Test]
    procedure Nothing_Found_With_Every_Source_Answering_Is_An_Answer;
    //dpm mcp --source restricts what the tools ask.
    [Test]
    procedure Source_Option_Restricts_The_Queries;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Json.Utils,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.FrameBuffer,
  DPM.Core.MCP.Protocol,
  DPM.Core.MCP.Transport.Memory,
  DPM.Core.MCP.Server,
  DPM.Core.MCP.Tools.Common,
  DPM.Core.MCP.Tools.Packages,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Tests.Dependency.Fakes,
  TestLogger;

type
  //Minimal tool so the server can be tested without any DPM machinery.
  TStubTool = class(TInterfacedObject, IMCPTool)
  private
    FName : string;
    FFail : boolean;
  protected
    function GetName : string;
    function GetTitle : string;
    function GetDescription : string;
    function GetIsOpenWorld : boolean;
    procedure BuildInputSchema(const target : IYAMLMapping);
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
  public
    constructor Create(const name : string; const fail : boolean = false);
  end;

{ TStubTool }

constructor TStubTool.Create(const name : string; const fail : boolean);
begin
  inherited Create;
  FName := name;
  FFail := fail;
end;

function TStubTool.GetName : string;
begin
  result := FName;
end;

function TStubTool.GetTitle : string;
begin
  result := 'Stub ' + FName;
end;

function TStubTool.GetDescription : string;
begin
  result := 'A stub tool.';
end;

function TStubTool.GetIsOpenWorld : boolean;
begin
  result := false;
end;

procedure TStubTool.BuildInputSchema(const target : IYAMLMapping);
begin
  target.AddOrSetValue('type', 'object');
  target.AddOrSetMapping('properties');
end;

function TStubTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
begin
  if FFail then
    raise EMCPToolError.Create('stub failure the caller could fix');
  result := '{"ok":true}';
end;

type
  //a tool whose schema can't be built - tools/list fails while handling it.
  TBrokenSchemaTool = class(TStubTool, IMCPTool)
  protected
    procedure BuildInputSchema(const target : IYAMLMapping);
  end;

  //Reads scripted messages - cBadFrame stands for a line the frame buffer couldn't decode, cReadFailure for the
  //reading itself failing.
  TScriptedReader = class(TInterfacedObject, IMCPMessageReader)
  private
    FMessages : TArray<string>;
    FIndex : integer;
  protected
    function ReadMessage(out message : string) : boolean;
  public
    constructor Create(const messages : array of string);
  end;

const
  cBadFrame = '<not utf-8>';
  cReadFailure = '<read failure>';

procedure TBrokenSchemaTool.BuildInputSchema(const target : IYAMLMapping);
begin
  raise Exception.Create('the schema could not be built');
end;

constructor TScriptedReader.Create(const messages : array of string);
var
  i : integer;
begin
  inherited Create;
  SetLength(FMessages, Length(messages));
  for i := 0 to High(messages) do
    FMessages[i] := messages[i];
  FIndex := 0;
end;

function TScriptedReader.ReadMessage(out message : string) : boolean;
begin
  message := '';
  if FIndex >= Length(FMessages) then
    exit(false);
  message := FMessages[FIndex];
  Inc(FIndex);
  if message = cBadFrame then
    raise EMCPFrameError.Create('the message is not valid UTF-8');
  if message = cReadFailure then
    raise Exception.Create('the pipe broke');
  result := true;
end;

function IdIsNull(const reply : IYAMLMapping) : boolean;
var
  idValue : IYAMLValue;
begin
  result := reply.TryGetValue('id', idValue) and idValue.IsNull;
end;

//As RunSession, with the reader and tools given - and what Run returned.
function RunWith(const reader : IMCPMessageReader; const registry : IMCPToolRegistry; out ranCleanly : boolean) : IList<string>;
var
  writer : TMCPMemoryWriter;
  writerIntf : IMCPMessageWriter;
  server : TMCPServer;
  tokenSource : ICancellationTokenSource;
begin
  writer := TMCPMemoryWriter.Create;
  writerIntf := writer;
  tokenSource := TCancellationTokenSourceFactory.Create;
  server := TMCPServer.Create(reader, writerIntf, registry, TTestLogger.Create, 'test instructions');
  try
    ranCleanly := server.Run(tokenSource.Token);
  finally
    server.Free;
  end;
  result := writer.Messages;
end;

function StubRegistry : IMCPToolRegistry;
begin
  result := TMCPToolRegistry.Create;
  result.Add(TStubTool.Create('alpha'));
end;

///<summary> Runs a scripted session and returns everything the server wrote. </summary>
function RunSession(const requests : array of string; const failingTool : boolean = false) : IList<string>;
var
  reader : IMCPMessageReader;
  writer : TMCPMemoryWriter;
  writerIntf : IMCPMessageWriter;
  registry : IMCPToolRegistry;
  server : TMCPServer;
  tokenSource : ICancellationTokenSource;
begin
  registry := TMCPToolRegistry.Create;
  registry.Add(TStubTool.Create('alpha'));
  registry.Add(TStubTool.Create('beta', failingTool));

  reader := TMCPMemoryReader.Create(requests);
  writer := TMCPMemoryWriter.Create;
  writerIntf := writer;
  tokenSource := TCancellationTokenSourceFactory.Create;

  server := TMCPServer.Create(reader, writerIntf, registry, TTestLogger.Create, 'test instructions');
  try
    server.Run(tokenSource.Token);
  finally
    server.Free;
  end;
  result := writer.Messages;
end;

function ParseOne(const text : string) : IYAMLMapping;
begin
  result := TYAML.LoadFromString(text).AsMapping;
end;

function BytesOf(const values : array of byte) : TBytes;
var
  i : integer;
begin
  SetLength(result, Length(values));
  for i := Low(values) to High(values) do
    result[i] := values[i];
end;

function Utf8Bytes(const value : string) : TBytes;
begin
  result := TEncoding.UTF8.GetBytes(value);
end;

{ TMCPFrameBufferTests }

procedure TMCPFrameBufferTests.Two_Messages_In_One_Chunk;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    chunk := Utf8Bytes('{"a":1}'#10'{"b":2}'#10);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{"a":1}', message, false);
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{"b":2}', message, false);
    Assert.IsFalse(buffer.TryGetMessage(message));
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.One_Message_Split_Across_Three_Chunks;
var
  buffer : TMCPFrameBuffer;
  part : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    part := Utf8Bytes('{"met');
    buffer.AppendBytes(part, Length(part));
    Assert.IsFalse(buffer.TryGetMessage(message));
    part := Utf8Bytes('hod":"pi');
    buffer.AppendBytes(part, Length(part));
    Assert.IsFalse(buffer.TryGetMessage(message));
    part := Utf8Bytes('ng"}'#10);
    buffer.AppendBytes(part, Length(part));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{"method":"ping"}', message, false);
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Multi_Byte_Utf8_Split_Across_A_Chunk_Boundary;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  //A pipe read can land anywhere, including the middle of a multi byte character. If the
  //buffer decoded each chunk as it arrived instead of buffering raw bytes, the euro sign here
  //would come back as replacement characters. This is the bug this class exists to prevent.
  buffer := TMCPFrameBuffer.Create;
  try
    chunk := BytesOf([Ord('"'), $E2, $82]);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsFalse(buffer.TryGetMessage(message), 'no delimiter yet');

    chunk := BytesOf([$AC, Ord('"'), 10]);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('"' + #$20AC + '"', message, false);
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Crlf_Terminated_Input_Is_Accepted;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    //The spec says LF, but a Windows client using a text stream will send CRLF. Being strict
    //here would fail in a way that is very hard to diagnose from the client side.
    chunk := Utf8Bytes('{"a":1}'#13#10);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{"a":1}', message, false);
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Blank_Lines_Are_Skipped;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    chunk := Utf8Bytes(#10#10'{"a":1}'#10#10);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{"a":1}', message, false);
    Assert.IsFalse(buffer.TryGetMessage(message));
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Leading_Bom_Is_Stripped;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    chunk := BytesOf([$EF, $BB, $BF, Ord('{'), Ord('}'), 10]);
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsTrue(buffer.TryGetMessage(message));
    Assert.AreEqual('{}', message, false);
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Partial_Line_Is_Not_Returned;
var
  buffer : TMCPFrameBuffer;
  chunk : TBytes;
  message : string;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    chunk := Utf8Bytes('{"incomplete":true}');
    buffer.AppendBytes(chunk, Length(chunk));
    Assert.IsFalse(buffer.TryGetMessage(message), 'a line with no terminator is not a message');
    Assert.AreEqual(19, buffer.PendingByteCount);
  finally
    buffer.Free;
  end;
end;

{ TMCPProtocolTests }

procedure TMCPProtocolTests.Integer_Id_Round_Trips_As_An_Integer;
var
  message : TMCPMessage;
  json : string;
begin
  Assert.AreEqual(Ord(TMCPParseOutcome.poOk),
    Ord(TMCPProtocol.TryParseMessage('{"jsonrpc":"2.0","id":42,"method":"ping"}', message)));
  Assert.AreEqual(Ord(TMCPRequestIdKind.ridInt), Ord(message.Id.Kind));
  Assert.AreEqual(Int64(42), message.Id.IntValue);

  //Compact, so the assertion is about the value and not about whitespace.
  json := TJsonUtils.ToCompactJson(TMCPProtocol.BuildError(message.Id, -32601, 'nope'));
  //Must come back as a bare 42, not "42" - a client correlating by id would not match it.
  Assert.IsTrue(json.Contains('"id":42'), json);
end;

procedure TMCPProtocolTests.String_Id_Round_Trips_As_A_String;
var
  message : TMCPMessage;
  json : string;
begin
  TMCPProtocol.TryParseMessage('{"jsonrpc":"2.0","id":"abc-1","method":"ping"}', message);
  Assert.AreEqual(Ord(TMCPRequestIdKind.ridStr), Ord(message.Id.Kind));
  Assert.AreEqual('abc-1', message.Id.StrValue, false);

  json := TJsonUtils.ToCompactJson(TMCPProtocol.BuildError(message.Id, -32601, 'nope'));
  Assert.IsTrue(json.Contains('"id":"abc-1"'), json);
end;

procedure TMCPProtocolTests.Message_Without_Id_Is_A_Notification;
var
  message : TMCPMessage;
begin
  TMCPProtocol.TryParseMessage('{"jsonrpc":"2.0","method":"notifications/initialized"}', message);
  Assert.IsTrue(message.IsNotification);
  Assert.IsFalse(message.IsRequest);
end;

procedure TMCPProtocolTests.Malformed_Json_Is_A_Parse_Error;
var
  message : TMCPMessage;
  outcome : TMCPParseOutcome;
begin
  outcome := TMCPProtocol.TryParseMessage('{not valid', message);
  Assert.IsTrue(outcome <> TMCPParseOutcome.poOk);
end;

procedure TMCPProtocolTests.Top_Level_Array_Is_Rejected_As_A_Batch;
var
  message : TMCPMessage;
begin
  //Batching was removed in the modern revision.
  Assert.AreEqual(Ord(TMCPParseOutcome.poBatch),
    Ord(TMCPProtocol.TryParseMessage('[{"jsonrpc":"2.0","id":1,"method":"ping"}]', message)));
end;

procedure TMCPProtocolTests.Modern_Meta_Protocol_Version_Is_Read;
var
  message : TMCPMessage;
begin
  TMCPProtocol.TryParseMessage(
    '{"jsonrpc":"2.0","id":1,"method":"ping","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}',
    message);
  Assert.IsTrue(message.IsModern);
  Assert.AreEqual('2026-07-28', message.ProtocolVersion, false);
end;

procedure TMCPProtocolTests.Absent_Meta_Means_Legacy;
var
  message : TMCPMessage;
begin
  TMCPProtocol.TryParseMessage('{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}', message);
  Assert.IsFalse(message.IsModern);
  Assert.AreEqual('', message.ProtocolVersion, false);
end;

procedure TMCPProtocolTests.Result_Envelope_Carries_ResultType_And_ServerInfo;
var
  id : TMCPRequestId;
  body : IYAMLMapping;
  doc : IYAMLDocument;
  parsed : IYAMLMapping;
  resultObj : IYAMLMapping;
begin
  id.Kind := TMCPRequestIdKind.ridInt;
  id.IntValue := 7;
  doc := TMCPProtocol.BeginResult(id, body);

  parsed := TYAML.LoadFromString(TJsonUtils.ToCompactJson(doc)).AsMapping;
  Assert.AreEqual('2.0', parsed.S['jsonrpc'], false);
  resultObj := parsed.O['result'];
  //Required by the modern revision on every result.
  Assert.AreEqual('complete', resultObj.S['resultType'], false);
  Assert.AreEqual('dpm', resultObj.O['_meta'].O['io.modelcontextprotocol/serverInfo'].S['name'], false);
end;

{ TMCPServerTests }

procedure TMCPServerTests.Legacy_Initialize_Echoes_A_Supported_Version;
var
  sent : IList<string>;
  reply : IYAMLMapping;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}']);
  Assert.AreEqual(1, sent.Count);
  reply := ParseOne(sent[0]).O['result'];
  Assert.AreEqual('2025-06-18', reply.S['protocolVersion'], false);
  Assert.AreEqual('dpm', reply.O['serverInfo'].S['name'], false);
end;

procedure TMCPServerTests.Modern_Discover_Lists_Supported_Versions;
var
  sent : IList<string>;
  reply : IYAMLMapping;
  versions : IYAMLSequence;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"server/discover","params":' +
                      '{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}']);
  Assert.AreEqual(1, sent.Count);
  reply := ParseOne(sent[0]).O['result'];
  versions := reply.A['supportedVersions'];
  Assert.IsTrue(versions.Count >= 1);
  Assert.AreEqual('2026-07-28', versions.Items[0].AsString, false);
end;

procedure TMCPServerTests.Initialize_Never_Negotiates_The_Modern_Version(const requested : string; const expected : string);
var
  sent : IList<string>;
  negotiated : string;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"' + requested + '"}}']);
  Assert.AreEqual(1, sent.Count);

  negotiated := ParseOne(sent[0]).O['result'].S['protocolVersion'];
  Assert.AreEqual(expected, negotiated, false);
  Assert.AreNotEqual(cMCPVersionModern, negotiated, false,
    'a legacy handshake must not be answered with the modern revision');
end;

procedure TMCPServerTests.Notifications_Produce_No_Output;
var
  sent : IList<string>;
begin
  //A notification never gets a reply, not even an error - including one we do not recognise.
  sent := RunSession(['{"jsonrpc":"2.0","method":"notifications/initialized"}',
                      '{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}',
                      '{"jsonrpc":"2.0","method":"notifications/somethingUnknown"}']);
  Assert.AreEqual(0, sent.Count);
end;

procedure TMCPServerTests.Unknown_Method_Is_Method_Not_Found;
var
  sent : IList<string>;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"no/such/method"}']);
  Assert.AreEqual(1, sent.Count);
  Assert.AreEqual(Int64(-32601), ParseOne(sent[0]).O['error'].L['code']);
end;

procedure TMCPServerTests.Unknown_Tool_Is_Invalid_Params;
var
  sent : IList<string>;
begin
  //An unknown tool is a protocol error, not a tool error - the model cannot fix it by
  //changing arguments.
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"nope"}}']);
  Assert.AreEqual(Int64(-32602), ParseOne(sent[0]).O['error'].L['code']);
end;

procedure TMCPServerTests.Tool_Error_Is_Reported_In_The_Result_Not_As_A_Protocol_Error;
var
  sent : IList<string>;
  reply : IYAMLMapping;
begin
  //The distinction that matters most: a tool failure the caller could correct comes back as a
  //RESULT with isError true, so the model sees the message and can retry.
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"beta","arguments":{}}}'], true);
  reply := ParseOne(sent[0]);
  Assert.IsFalse(reply.ContainsKey('error'), 'must not be a JSON-RPC error');
  Assert.IsTrue(reply.O['result'].B['isError']);
  Assert.IsTrue(reply.O['result'].A['content'].O[0].S['text'].Contains('stub failure'));
end;

procedure TMCPServerTests.Unsupported_Modern_Version_Is_Rejected_With_The_Supported_List;
var
  sent : IList<string>;
  errorObj : IYAMLMapping;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/list","params":' +
                      '{"_meta":{"io.modelcontextprotocol/protocolVersion":"1900-01-01"}}}']);
  errorObj := ParseOne(sent[0]).O['error'];
  Assert.AreEqual(Int64(-32022), errorObj.L['code']);
  //The supported list is what lets a client retry rather than give up.
  Assert.IsTrue(errorObj.O['data'].A['supported'].Count >= 1);
  Assert.AreEqual('1900-01-01', errorObj.O['data'].S['requested'], false);
end;

procedure TMCPServerTests.A_Bad_Frame_Does_Not_End_The_Session;
var
  sent : IList<string>;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"ping"}',
                      'this is not json',
                      '{"jsonrpc":"2.0","id":3,"method":"ping"}']);
  //Three requests in, three replies out - the malformed one is answered with an error and the
  //session carries on.
  Assert.AreEqual(3, sent.Count);
  Assert.IsTrue(ParseOne(sent[1]).ContainsKey('error'));
  Assert.AreEqual(Int64(3), ParseOne(sent[2]).L['id']);
end;

procedure TMCPServerTests.Tools_List_Carries_The_Required_Caching_Hints;
var
  reply : IYAMLMapping;
  scope : string;
begin
  reply := ParseOne(RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'])[0]).O['result'];

  Assert.IsTrue(reply.ContainsKey('ttlMs'), 'ttlMs is required on a cacheable result');
  //Must be a number - a string here fails client side schema validation just as hard as
  //the field being absent.
  Assert.IsTrue(reply.Items['ttlMs'].IsInteger, 'ttlMs must be a number');
  Assert.IsTrue(reply.L['ttlMs'] >= 0, 'ttlMs must be >= 0');

  Assert.IsTrue(reply.ContainsKey('cacheScope'), 'cacheScope is required');
  scope := reply.S['cacheScope'];
  Assert.IsTrue((scope = cMCPCacheScopePublic) or (scope = cMCPCacheScopePrivate),
    'cacheScope must be public or private, got ' + scope);
  //The tool set is the same for every caller, so it is genuinely public.
  Assert.AreEqual(cMCPCacheScopePublic, scope, false);
end;

procedure TMCPServerTests.Discover_Carries_The_Required_Caching_Hints;
var
  reply : IYAMLMapping;
begin
  reply := ParseOne(RunSession(['{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}}'])[0]).O['result'];

  Assert.IsTrue(reply.Items['ttlMs'].IsInteger, 'ttlMs must be a number');
  Assert.IsTrue(reply.L['ttlMs'] >= 0);
  //Private: the instructions embed this machine's project path.
  Assert.AreEqual(cMCPCacheScopePrivate, reply.S['cacheScope'], false);
end;

procedure TMCPServerTests.Tools_Call_Does_Not_Carry_Caching_Hints;
var
  reply : IYAMLMapping;
begin
  reply := ParseOne(RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"alpha","arguments":{}}}'])[0]).O['result'];

  Assert.IsFalse(reply.ContainsKey('ttlMs'), 'tools/call results are not cacheable');
  Assert.IsFalse(reply.ContainsKey('cacheScope'));
end;

procedure TMCPServerTests.Tools_List_Reports_Registered_Tools_In_Order;
var
  sent : IList<string>;
  tools : IYAMLSequence;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}']);
  tools := ParseOne(sent[0]).O['result'].A['tools'];
  Assert.AreEqual(2, tools.Count);
  //Registration order, which is what makes the list cacheable by a client.
  Assert.AreEqual('alpha', tools.O[0].S['name'], false);
  Assert.AreEqual('beta', tools.O[1].S['name'], false);
  Assert.IsTrue(tools.O[0].O['annotations'].B['readOnlyHint']);
end;

procedure TMCPFrameBufferTests.Line_That_Is_Not_Utf8_Is_A_Frame_Error;
var
  buffer : TMCPFrameBuffer;
  message : string;
  raised : boolean;
begin
  buffer := TMCPFrameBuffer.Create;
  try
    //$FF is never part of UTF-8.
    buffer.AppendBytes(BytesOf([Ord('{'), Ord('"'), Ord('a'), Ord('"'), Ord(':'), Ord('"'), $FF, Ord('"'), Ord('}'), 10]), 10);
    buffer.AppendBytes(Utf8Bytes('{"b":1}' + #10), 8);
    raised := false;
    try
      buffer.TryGetMessage(message);
    except
      on e : EMCPFrameError do
        raised := true;
    end;
    Assert.IsTrue(raised, 'not taken for a message : [' + message + ']');
    Assert.IsTrue(buffer.TryGetMessage(message), 'the next line is still there');
    Assert.AreEqual('{"b":1}', message);
  finally
    buffer.Free;
  end;
end;

//true if the bytes, as a line, are a frame error.
function IsFrameError(const bytes : TBytes) : boolean;
var
  buffer : TMCPFrameBuffer;
  line : TBytes;
  message : string;
begin
  result := false;
  line := Copy(bytes, 0, Length(bytes));
  SetLength(line, Length(line) + 1);
  line[High(line)] := 10;
  buffer := TMCPFrameBuffer.Create;
  try
    buffer.AppendBytes(line, Length(line));
    try
      buffer.TryGetMessage(message);
    except
      on e : EMCPFrameError do
        result := true;
    end;
  finally
    buffer.Free;
  end;
end;

procedure TMCPFrameBufferTests.Strict_Utf8_Is_Required;
begin
  Assert.IsTrue(IsFrameError(BytesOf([Ord('a'), $C0, $AF])), 'overlong form of /');
  Assert.IsTrue(IsFrameError(BytesOf([Ord('a'), $ED, $A0, $80])), 'a surrogate');
  Assert.IsTrue(IsFrameError(BytesOf([Ord('a'), $F4, $90, $80, $80])), 'past U+10FFFF');
  Assert.IsTrue(IsFrameError(BytesOf([Ord('a'), $E2, $82])), 'cut short');
  Assert.IsTrue(IsFrameError(BytesOf([Ord('a'), $80])), 'a continuation byte on its own');
  //U+1F600, four bytes - it must still be a message.
  Assert.IsFalse(IsFrameError(BytesOf([Ord('"'), $F0, $9F, $98, $80, Ord('"')])), 'a four byte character');
end;

procedure TMCPServerTests.A_Frame_That_Is_Not_Utf8_Is_A_Parse_Error;
var
  sent : IList<string>;
  ranCleanly : boolean;
  reply : IYAMLMapping;
begin
  sent := RunWith(TScriptedReader.Create([cBadFrame, '{"jsonrpc":"2.0","id":2,"method":"ping"}']), StubRegistry, ranCleanly);
  Assert.AreEqual(2, sent.Count, 'answered, and the session went on');
  reply := ParseOne(sent[0]);
  Assert.AreEqual(Int64(-32700), reply.O['error'].L['code']);
  Assert.IsTrue(IdIsNull(reply), sent[0]);
  Assert.AreEqual(Int64(2), ParseOne(sent[1]).L['id']);
  Assert.IsTrue(ranCleanly);
end;

procedure TMCPServerTests.An_Id_That_Is_Not_A_String_Or_Integer_Is_An_Invalid_Request;
var
  sent : IList<string>;
  i : integer;
  reply : IYAMLMapping;
begin
  sent := RunSession(['{"jsonrpc":"2.0","id":1.5,"method":"ping"}',
                      '{"jsonrpc":"2.0","id":true,"method":"ping"}',
                      '{"jsonrpc":"2.0","id":{"x":1},"method":"ping"}',
                      '{"jsonrpc":"2.0","id":4,"method":"ping"}']);
  Assert.AreEqual(4, sent.Count, 'each is answered');
  for i := 0 to 2 do
  begin
    reply := ParseOne(sent[i]);
    Assert.AreEqual(Int64(-32600), reply.O['error'].L['code'], sent[i]);
    Assert.IsTrue(IdIsNull(reply), sent[i]);
  end;
  Assert.AreEqual(Int64(4), ParseOne(sent[3]).L['id']);
end;

procedure TMCPServerTests.A_Request_Method_Sent_As_A_Notification_Is_Ignored;
var
  sent : IList<string>;
begin
  sent := RunSession(['{"jsonrpc":"2.0","method":"tools/call","params":{"name":"alpha","arguments":{}}}',
                      '{"jsonrpc":"2.0","method":"tools/list"}',
                      '{"jsonrpc":"2.0","method":"ping"}',
                      '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-11-25"}}']);
  //the count alone - IfThen would read sent[0] whether there is one or not.
  Assert.AreEqual(0, sent.Count, 'notifications were answered');
end;

procedure TMCPServerTests.A_Fault_Handling_A_Message_Is_An_Internal_Error;
var
  registry : IMCPToolRegistry;
  sent : IList<string>;
  ranCleanly : boolean;
  reply : IYAMLMapping;
begin
  registry := TMCPToolRegistry.Create;
  registry.Add(TBrokenSchemaTool.Create('broken'));
  sent := RunWith(TScriptedReader.Create(['{"jsonrpc":"2.0","id":1,"method":"tools/list"}',
                                          '{"jsonrpc":"2.0","id":2,"method":"ping"}']), registry, ranCleanly);
  Assert.AreEqual(2, sent.Count);
  reply := ParseOne(sent[0]);
  Assert.AreEqual(Int64(-32603), reply.O['error'].L['code'], sent[0]);
  Assert.AreEqual(Int64(1), reply.L['id']);
  Assert.AreEqual(Int64(2), ParseOne(sent[1]).L['id']);
  Assert.IsTrue(ranCleanly);
end;

procedure TMCPServerTests.Run_Says_Whether_It_Ended_Cleanly;
var
  ranCleanly : boolean;
begin
  RunWith(TScriptedReader.Create(['{"jsonrpc":"2.0","id":1,"method":"ping"}']), StubRegistry, ranCleanly);
  Assert.IsTrue(ranCleanly, 'the client closing the stream is the end of a session');
  RunWith(TScriptedReader.Create(['{"jsonrpc":"2.0","id":1,"method":"ping"}', cReadFailure]), StubRegistry, ranCleanly);
  Assert.IsFalse(ranCleanly, 'reading failed');
end;

{ TMCPPackageToolTests }

//dpm_package_versions over repository - asked for Some.Pkg on 12.0, the server started with --source=sources.
function InvokeVersionsTool(const repository : IPackageRepositoryManager; const sources : string = '') : string;
var
  logger : ILogger;
  configuration : IConfiguration;
  context : IMCPToolContext;
  tool : IMCPTool;
  argsDoc : IYAMLDocument;
begin
  logger := TTestLogger.Create;
  configuration := TConfiguration.Create(logger);
  context := TMCPToolContext.Create(repository, configuration, logger, TCompilerVersion.Delphi12_0, '', sources);
  tool := TMCPPackageVersionsTool.Create(context);
  argsDoc := TYAML.CreateMapping;
  argsDoc.AsMapping.AddOrSetValue('packageId', 'Some.Pkg');
  argsDoc.AsMapping.AddOrSetValue('compiler', '12.0');
  result := tool.Invoke(TCancellationTokenSourceFactory.Create.Token, argsDoc.AsMapping);
end;

procedure TMCPPackageToolTests.Nothing_Found_With_A_Source_Unavailable_Is_A_Tool_Error;
var
  repository : TFakeRepositoryManager;
  repositoryIntf : IPackageRepositoryManager;
  error : string;
begin
  repository := TFakeRepositoryManager.Create;
  repositoryIntf := repository;
  repository.Unavailable := 'Gallery (timed out)';
  error := '';
  try
    InvokeVersionsTool(repositoryIntf);
  except
    on e : EMCPToolError do
      error := e.Message;
  end;
  Assert.IsTrue(Pos('Gallery', error) > 0, 'a tool error naming the source : [' + error + ']');
end;

procedure TMCPPackageToolTests.Found_With_A_Source_Unavailable_Says_So;
var
  repository : TFakeRepositoryManager;
  repositoryIntf : IPackageRepositoryManager;
  versions : IList<IPackageInfo>;
  reply : IYAMLMapping;
begin
  repository := TFakeRepositoryManager.Create;
  repositoryIntf := repository;
  versions := TCollections.CreateList<IPackageInfo>;
  versions.Add(TPackageInfo.Create('Mirror', 'Some.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.Delphi12_0, '', ''));
  repository.SetVersions('Some.Pkg', versions);
  repository.Unavailable := 'Gallery (timed out)';
  reply := ParseOne(InvokeVersionsTool(repositoryIntf));
  Assert.AreEqual(1, reply.A['versions'].Count);
  Assert.IsTrue(Pos('Gallery', reply.S['unavailableSources']) > 0, 'the source that could not be asked is named');
end;

procedure TMCPPackageToolTests.Nothing_Found_With_Every_Source_Answering_Is_An_Answer;
var
  repository : TFakeRepositoryManager;
  repositoryIntf : IPackageRepositoryManager;
  reply : IYAMLMapping;
begin
  repository := TFakeRepositoryManager.Create;
  repositoryIntf := repository;
  reply := ParseOne(InvokeVersionsTool(repositoryIntf));
  Assert.AreEqual(0, reply.A['versions'].Count);
  Assert.IsFalse(reply.ContainsKey('unavailableSources'));
end;

procedure TMCPPackageToolTests.Source_Option_Restricts_The_Queries;
var
  repository : TFakeRepositoryManager;
  repositoryIntf : IPackageRepositoryManager;
begin
  repository := TFakeRepositoryManager.Create;
  repositoryIntf := repository;
  InvokeVersionsTool(repositoryIntf, 'Mine,Theirs');
  Assert.AreEqual('Mine,Theirs', repository.VersionsSources);
end;

initialization
  TDUnitX.RegisterTestFixture(TMCPFrameBufferTests);
  TDUnitX.RegisterTestFixture(TMCPProtocolTests);
  TDUnitX.RegisterTestFixture(TMCPServerTests);
  TDUnitX.RegisterTestFixture(TMCPPackageToolTests);

end.
