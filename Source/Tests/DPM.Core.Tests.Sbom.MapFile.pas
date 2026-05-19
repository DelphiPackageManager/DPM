unit DPM.Core.Tests.SBOM.MapFile;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMapFileReaderTests = class
  private
    function WriteFixture(const content : string) : string;
    function NewTempPath(const tag : string) : string;
  public
    [Test]
    procedure ParsesWin32SegmentsAndLineNumbers;
    [Test]
    procedure ParsesWin64SixteenHexAddresses;
    [Test]
    procedure HandlesUtf8Bom;
    [Test]
    procedure SegmentOnlyEntriesHaveEmptySourcePath;
    [Test]
    procedure MissingFileReturnsNil;
    [Test]
    procedure DedupesGenericInstantiations;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  TestLogger,
  DPM.Core.Project.MapFile;

{ TMapFileReaderTests }

function TMapFileReaderTests.NewTempPath(const tag : string) : string;
var
  g : TGUID;
  guidStr : string;
begin
  CreateGUID(g);
  guidStr := LowerCase(Copy(GUIDToString(g), 2, 36));
  result := TPath.Combine(TPath.GetTempPath, 'dpm-sbom-' + tag + '-' + guidStr + '.map');
end;

function TMapFileReaderTests.WriteFixture(const content : string) : string;
var
  bytes : TBytes;
  fs : TFileStream;
begin
  result := NewTempPath('mapfile');
  bytes := TEncoding.UTF8.GetBytes(content);
  fs := TFileStream.Create(result, fmCreate);
  try
    if Length(bytes) > 0 then
      fs.WriteBuffer(bytes[0], Length(bytes));
  finally
    fs.Free;
  end;
end;

procedure TMapFileReaderTests.ParsesWin32SegmentsAndLineNumbers;
const
  cFixture =
    'Start         Length     Name                   Class' + sLineBreak +
    ' 0001:00400000 0000A000H .text                  CODE' + sLineBreak +
    sLineBreak +
    'Detailed map of segments' + sLineBreak +
    ' 0001:00400000 0000A000 C=CODE     S=.text    G=(none)   M=System ACBP=A9' + sLineBreak +
    ' 0001:0040A000 00001000 C=CODE     S=.text    G=(none)   M=Spring.Collections ACBP=A9' + sLineBreak +
    ' 0001:0040B000 00001000 C=CODE     S=.text    G=(none)   M=MyProject ACBP=A9' + sLineBreak +
    sLineBreak +
    'Address         Publics by Name' + sLineBreak +
    ' 0001:00401000       _SomeProcedure' + sLineBreak +
    sLineBreak +
    'Line numbers for System(System.pas) segment .text' + sLineBreak +
    '   1 0001:00400000   2 0001:00400010   3 0001:00400020' + sLineBreak +
    'Line numbers for Spring.Collections(C:\packages\Spring.Collections\1.0.0\src\Spring.Collections.pas) segment .text' + sLineBreak +
    '   1 0001:0040A000   2 0001:0040A010' + sLineBreak;
var
  fixturePath : string;
  reader : IMapFileReader;
  info : IMapFileInfo;
  u : TMapUnit;
  systemPath : string;
  springPath : string;
begin
  fixturePath := WriteFixture(cFixture);
  try
    reader := TMapFileReader.Create(TTestLogger.Create);
    info := reader.Read(fixturePath);
    Assert.IsNotNull(info);
    Assert.IsTrue(info.Units.Count >= 3, 'expected at least 3 units, got ' + IntToStr(info.Units.Count));
    Assert.IsTrue(info.HasSourcePaths);

    systemPath := '';
    springPath := '';
    for u in info.Units do
    begin
      if SameText(u.UnitName, 'System') then
        systemPath := u.SourcePath
      else if SameText(u.UnitName, 'Spring.Collections') then
        springPath := u.SourcePath;
    end;
    Assert.AreEqual('System.pas', systemPath);
    Assert.AreEqual('C:\packages\Spring.Collections\1.0.0\src\Spring.Collections.pas', springPath);
  finally
    if FileExists(fixturePath) then
      DeleteFile(fixturePath);
  end;
end;

procedure TMapFileReaderTests.ParsesWin64SixteenHexAddresses;
const
  cFixture =
    'Detailed map of segments' + sLineBreak +
    ' 0001:0000000000400000 0000000000050000 C=CODE S=.text G=(none) M=System ACBP=A9' + sLineBreak +
    ' 0001:0000000000450000 0000000000001000 C=CODE S=.text G=(none) M=System.SysUtils ACBP=A9' + sLineBreak +
    sLineBreak +
    'Line numbers for System.SysUtils(c:\delphi\source\rtl\sys\System.SysUtils.pas) segment .text' + sLineBreak +
    '  1 0001:0000000000450000' + sLineBreak;
var
  fixturePath : string;
  reader : IMapFileReader;
  info : IMapFileInfo;
  u : TMapUnit;
  found : boolean;
begin
  fixturePath := WriteFixture(cFixture);
  try
    reader := TMapFileReader.Create(TTestLogger.Create);
    info := reader.Read(fixturePath);
    Assert.IsNotNull(info);
    found := false;
    for u in info.Units do
    begin
      if SameText(u.UnitName, 'System.SysUtils') then
      begin
        found := true;
        Assert.AreEqual('c:\delphi\source\rtl\sys\System.SysUtils.pas', u.SourcePath);
      end;
    end;
    Assert.IsTrue(found, 'expected System.SysUtils to appear in the map units');
  finally
    if FileExists(fixturePath) then
      DeleteFile(fixturePath);
  end;
end;

procedure TMapFileReaderTests.HandlesUtf8Bom;
const
  cBody =
    'Detailed map of segments' + sLineBreak +
    ' 0001:00400000 0000A000 C=CODE S=.text G=(none) M=System ACBP=A9' + sLineBreak;
var
  fixturePath : string;
  reader : IMapFileReader;
  info : IMapFileInfo;
  fs : TFileStream;
  bom : array[0..2] of byte;
  body : TBytes;
begin
  fixturePath := NewTempPath('bom');
  bom[0] := $EF; bom[1] := $BB; bom[2] := $BF;
  body := TEncoding.UTF8.GetBytes(cBody);
  fs := TFileStream.Create(fixturePath, fmCreate);
  try
    fs.WriteBuffer(bom[0], 3);
    fs.WriteBuffer(body[0], Length(body));
  finally
    fs.Free;
  end;
  try
    reader := TMapFileReader.Create(TTestLogger.Create);
    info := reader.Read(fixturePath);
    Assert.IsNotNull(info);
    Assert.IsTrue(info.Units.Count > 0);
    //First unit shouldn't carry a BOM-prefixed name.
    Assert.AreEqual('System', info.Units[0].UnitName);
  finally
    if FileExists(fixturePath) then
      DeleteFile(fixturePath);
  end;
end;

procedure TMapFileReaderTests.SegmentOnlyEntriesHaveEmptySourcePath;
const
  cFixture =
    'Detailed map of segments' + sLineBreak +
    ' 0001:00400000 0000A000 C=CODE S=.text G=(none) M=OrphanUnit ACBP=A9' + sLineBreak;
var
  fixturePath : string;
  reader : IMapFileReader;
  info : IMapFileInfo;
begin
  fixturePath := WriteFixture(cFixture);
  try
    reader := TMapFileReader.Create(TTestLogger.Create);
    info := reader.Read(fixturePath);
    Assert.IsNotNull(info);
    Assert.AreEqual<integer>(1, info.Units.Count);
    Assert.AreEqual('OrphanUnit', info.Units[0].UnitName);
    Assert.AreEqual('', info.Units[0].SourcePath);
    Assert.IsFalse(info.HasSourcePaths);
  finally
    if FileExists(fixturePath) then
      DeleteFile(fixturePath);
  end;
end;

procedure TMapFileReaderTests.MissingFileReturnsNil;
var
  reader : IMapFileReader;
  info : IMapFileInfo;
begin
  reader := TMapFileReader.Create(TTestLogger.Create);
  info := reader.Read(NewTempPath('does-not-exist'));
  Assert.IsNull(info);
end;

procedure TMapFileReaderTests.DedupesGenericInstantiations;
const
  cFixture =
    'Detailed map of segments' + sLineBreak +
    ' 0001:00400000 0000A000 C=CODE S=.text G=(none) M=Spring.Generics ACBP=A9' + sLineBreak +
    ' 0001:0040A000 00001000 C=CODE S=.text G=(none) M=Spring.Generics ACBP=A9' + sLineBreak +
    sLineBreak +
    'Line numbers for Spring.Generics(Spring.Generics.pas) segment .text' + sLineBreak +
    '   1 0001:00400000' + sLineBreak +
    'Line numbers for Spring.Generics(Spring.Generics.pas) segment .text' + sLineBreak +
    '   2 0001:0040A000' + sLineBreak;
var
  fixturePath : string;
  reader : IMapFileReader;
  info : IMapFileInfo;
begin
  fixturePath := WriteFixture(cFixture);
  try
    reader := TMapFileReader.Create(TTestLogger.Create);
    info := reader.Read(fixturePath);
    Assert.IsNotNull(info);
    Assert.AreEqual<integer>(1, info.Units.Count, 'duplicate unit names should dedupe');
    Assert.AreEqual('Spring.Generics.pas', info.Units[0].SourcePath);
  finally
    if FileExists(fixturePath) then
      DeleteFile(fixturePath);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMapFileReaderTests);

end.
