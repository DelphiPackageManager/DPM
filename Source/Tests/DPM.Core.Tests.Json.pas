unit DPM.Core.Tests.Json;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TJsonUtilsTests = class
  public
    //The reason TJsonUtils exists at all - a regression guard on the VSoft.YAML behaviour the
    //helpers work around. If this ever starts failing, the library changed and the omit-empty
    //policy should be revisited.
    [Test]
    procedure Mapping_AddOrSetValue_With_Empty_String_Stores_Null;

    [Test]
    procedure Sequence_AddValue_With_Empty_String_Stores_A_Real_Empty_String;

    [Test]
    procedure AddIfNotEmpty_Omits_The_Key_When_Value_Is_Empty;

    [Test]
    procedure AddIfNotEmpty_Writes_The_Value_When_Not_Empty;

    //Load bearing for the mcp server - a pretty printed frame would contain raw newlines and
    //break newline delimited framing.
    [Test]
    procedure ToCompactJson_Contains_No_Line_Breaks;

    [Test]
    procedure ToPrettyJson_Contains_Line_Breaks;

    [Test]
    procedure AddPlatforms_Writes_An_Array_Of_Platform_Names;

    [Test]
    procedure AddPlatforms_Output_Is_Accepted_By_StringToDPMPlatforms;

    [Test]
    procedure AddPlatforms_Writes_An_Empty_Array_When_The_Set_Is_Empty;

    [Test]
    procedure AddCompiler_Omits_The_Key_When_Unknown;

    [Test]
    procedure AddCompiler_Writes_The_Wire_Form;

    [Test]
    procedure AddVersion_Omits_The_Key_When_Empty;

    [Test]
    procedure AddStrings_Skips_Empty_Entries;

    [Test]
    procedure AddStrings_Writes_An_Empty_Array_For_A_Nil_List;

    [Test]
    procedure Escaping_Survives_A_Round_Trip;
  end;

  {$M+}
  [TestFixture]
  TJsonProjectionsTests = class
  public
    [Test]
    procedure PackageListItem_Round_Trips_Through_Json;

    [Test]
    procedure PackageListItem_Omits_SignedBy_When_Empty;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.ListItem,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections;

{ TJsonUtilsTests }

procedure TJsonUtilsTests.Mapping_AddOrSetValue_With_Empty_String_Stores_Null;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  doc.AsMapping.AddOrSetValue('key', '');
  Assert.IsTrue(doc.AsMapping.Items['key'].IsNull,
    'VSoft.YAML stores an empty mapping string as null - TJsonUtils.AddIfNotEmpty exists because of this');
end;

procedure TJsonUtilsTests.Sequence_AddValue_With_Empty_String_Stores_A_Real_Empty_String;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateSequence;
  doc.AsSequence.AddValue('');
  Assert.IsFalse(doc.AsSequence.Items[0].IsNull, 'sequences do not share the mapping quirk');
end;

procedure TJsonUtilsTests.AddIfNotEmpty_Omits_The_Key_When_Value_Is_Empty;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddIfNotEmpty(doc.AsMapping, 'key', '');
  Assert.IsFalse(doc.AsMapping.ContainsKey('key'), 'the key should be absent, not null');
  Assert.AreEqual('{}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.AddIfNotEmpty_Writes_The_Value_When_Not_Empty;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddIfNotEmpty(doc.AsMapping, 'key', 'value');
  Assert.AreEqual('{"key":"value"}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.ToCompactJson_Contains_No_Line_Breaks;
var
  doc : IYAMLDocument;
  root : IYAMLMapping;
  json : string;
begin
  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  root.AddOrSetValue('a', 1);
  root.AddOrSetMapping('nested').AddOrSetValue('b', 'two');
  root.AddOrSetSequence('list').AddValue('x');

  json := TJsonUtils.ToCompactJson(doc);
  Assert.IsFalse(json.Contains(#10), 'compact json must not contain LF');
  Assert.IsFalse(json.Contains(#13), 'compact json must not contain CR');
  Assert.AreEqual('{"a":1,"nested":{"b":"two"},"list":["x"]}', json, false);
end;

procedure TJsonUtilsTests.ToPrettyJson_Contains_Line_Breaks;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  doc.AsMapping.AddOrSetValue('a', 1);
  doc.AsMapping.AddOrSetValue('b', 2);
  Assert.IsTrue(TJsonUtils.ToPrettyJson(doc).Contains(#10), 'pretty json should be multi line');
end;

procedure TJsonUtilsTests.AddPlatforms_Writes_An_Array_Of_Platform_Names;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddPlatforms(doc.AsMapping, 'platforms', [TDPMPlatform.Win32, TDPMPlatform.Win64]);
  //Lower case is what DPMPlatformToString produces, and therefore what the rest of DPM uses
  //on the wire. Asserted explicitly so a casing change cannot slip through unnoticed.
  Assert.AreEqual('{"platforms":["win32","win64"]}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.AddPlatforms_Output_Is_Accepted_By_StringToDPMPlatforms;
var
  doc : IYAMLDocument;
  seq : IYAMLSequence;
  i : integer;
  rebuilt : TDPMPlatforms;
const
  cExpected : TDPMPlatforms = [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Linux64];
begin
  //StringToDPMPlatform silently maps anything it does not recognise to UnknownPlatform, and
  //StringToDPMPlatforms then drops it. So emitting a casing our own parser rejects would lose
  //platforms with no error anywhere. Prove the round trip rather than assuming it.
  doc := TYAML.CreateMapping;
  TJsonUtils.AddPlatforms(doc.AsMapping, 'platforms', cExpected);

  rebuilt := [];
  seq := doc.AsMapping.Items['platforms'].AsSequence;
  for i := 0 to seq.Count - 1 do
    rebuilt := rebuilt + StringToDPMPlatforms(seq.Items[i].AsString);

  Assert.IsTrue(rebuilt = cExpected, 'platforms did not survive a round trip through StringToDPMPlatforms');
end;

procedure TJsonUtilsTests.AddPlatforms_Writes_An_Empty_Array_When_The_Set_Is_Empty;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddPlatforms(doc.AsMapping, 'platforms', []);
  //An empty array, not a missing key - a caller counting platforms should not have to branch.
  Assert.AreEqual('{"platforms":[]}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.AddCompiler_Omits_The_Key_When_Unknown;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddCompiler(doc.AsMapping, 'compiler', TCompilerVersion.UnknownVersion);
  Assert.IsFalse(doc.AsMapping.ContainsKey('compiler'));
end;

procedure TJsonUtilsTests.AddCompiler_Writes_The_Wire_Form;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddCompiler(doc.AsMapping, 'compiler', TCompilerVersion.Delphi12_0);
  //Must match what the feed uses, so output can be fed straight back to the server.
  Assert.AreEqual('{"compiler":"delphi12.0"}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.AddVersion_Omits_The_Key_When_Empty;
var
  doc : IYAMLDocument;
begin
  doc := TYAML.CreateMapping;
  TJsonUtils.AddVersion(doc.AsMapping, 'version', TPackageVersion.Empty);
  Assert.IsFalse(doc.AsMapping.ContainsKey('version'));
end;

procedure TJsonUtilsTests.AddStrings_Skips_Empty_Entries;
var
  doc : IYAMLDocument;
  values : IList<string>;
begin
  values := TCollections.CreateList<string>;
  values.Add('one');
  values.Add('');
  values.Add('two');

  doc := TYAML.CreateMapping;
  TJsonUtils.AddStrings(doc.AsMapping, 'tags', values);
  Assert.AreEqual('{"tags":["one","two"]}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.AddStrings_Writes_An_Empty_Array_For_A_Nil_List;
var
  doc : IYAMLDocument;
  values : IList<string>;
begin
  values := nil;
  doc := TYAML.CreateMapping;
  TJsonUtils.AddStrings(doc.AsMapping, 'tags', values);
  Assert.AreEqual('{"tags":[]}', TJsonUtils.ToCompactJson(doc), false);
end;

procedure TJsonUtilsTests.Escaping_Survives_A_Round_Trip;
var
  doc : IYAMLDocument;
  json : string;
  parsed : IYAMLDocument;
const
  //Release notes and descriptions really do contain newlines and quotes - if these are not
  //escaped the document stops being one line and the mcp framing breaks.
  cAwkward = 'line1'#13#10'line2 "quoted" \backslash\ '#9'tab';
begin
  doc := TYAML.CreateMapping;
  doc.AsMapping.AddOrSetValue('text', cAwkward);
  json := TJsonUtils.ToCompactJson(doc);

  Assert.IsFalse(json.Contains(#10), 'a literal newline must not survive into the output');
  Assert.IsFalse(json.Contains(#13), 'a literal carriage return must not survive into the output');

  parsed := TYAML.LoadFromString(json);
  Assert.AreEqual(cAwkward, parsed.AsMapping.Items['text'].AsString, false);
end;

{ TJsonProjectionsTests }

procedure TJsonProjectionsTests.PackageListItem_Round_Trips_Through_Json;
var
  item : IPackageListItem;
  doc : IYAMLDocument;
  parsed : IYAMLMapping;
  platforms : IYAMLSequence;
begin
  item := TPackageListItem.Create('VSoft.HttpClient', TCompilerVersion.Delphi12_0,
                                  TPackageVersion.Parse('2.9.0'),
                                  [TDPMPlatform.Win32, TDPMPlatform.Win64]);

  doc := TYAML.CreateMapping;
  TJsonProjections.PackageListItem(item, doc.AsMapping);

  parsed := TYAML.LoadFromString(TJsonUtils.ToCompactJson(doc)).AsMapping;
  Assert.AreEqual('VSoft.HttpClient', parsed.Items['id'].AsString, false);
  Assert.AreEqual('2.9.0', parsed.Items['version'].AsString, false);
  Assert.AreEqual('delphi12.0', parsed.Items['compiler'].AsString, false);
  Assert.IsFalse(parsed.Items['isSigned'].AsBoolean);

  platforms := parsed.Items['platforms'].AsSequence;
  Assert.AreEqual(2, platforms.Count);
  Assert.AreEqual('win32', platforms.Items[0].AsString, false);
  Assert.AreEqual('win64', platforms.Items[1].AsString, false);
end;

procedure TJsonProjectionsTests.PackageListItem_Omits_SignedBy_When_Empty;
var
  item : IPackageListItem;
  doc : IYAMLDocument;
begin
  item := TPackageListItem.Create('Some.Package', TCompilerVersion.Delphi12_0,
                                  TPackageVersion.Parse('1.0.0'), [TDPMPlatform.Win32]);
  doc := TYAML.CreateMapping;
  TJsonProjections.PackageListItem(item, doc.AsMapping);
  //Not null, and not an empty string - simply absent.
  Assert.IsFalse(doc.AsMapping.ContainsKey('signedBy'));
end;

initialization
  TDUnitX.RegisterTestFixture(TJsonUtilsTests);
  TDUnitX.RegisterTestFixture(TJsonProjectionsTests);

end.
