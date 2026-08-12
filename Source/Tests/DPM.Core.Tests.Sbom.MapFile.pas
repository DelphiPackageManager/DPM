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

  //Where the SBOM generator looks for the linker MAP file. Pure path rules - no
  //build tree required.
  [TestFixture]
  TMapFilePathTests = class
  private
    function IndexOfPath(const candidates : TArray<string>; const path : string) : integer;
  public
    [Test]
    procedure AbsoluteOutputDirIsNotAppendedToProjectDir;
    [Test]
    procedure UncOutputDirIsNotAppendedToProjectDir;
    [Test]
    procedure RelativeOutputDirResolvesAgainstProjectDir;
    [Test]
    procedure OutputDirTokensAreExpanded;
    [Test]
    procedure EmptyOutputDirProbesProjectDirFirst;
    [Test]
    procedure ProjectDirIsAlwaysProbed;
    [Test]
    procedure CandidatesAreDeduped;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.SBOM.Generator,
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

{ TMapFilePathTests }

function TMapFilePathTests.IndexOfPath(const candidates : TArray<string>; const path : string) : integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to Length(candidates) - 1 do
    if SameText(candidates[i], path) then
      exit(i);
end;

//The reported bug : a dproj with an absolute DCC_ExeOutput produced
//'D:\Ajur2000\D:\Ajur2000\ajur.map' because the project dir was prepended blindly.
procedure TMapFilePathTests.AbsoluteOutputDirIsNotAppendedToProjectDir;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Ajur2000', 'D:\Ajur2000', 'ajur', 'Release', TDPMPlatform.Win64);
  Assert.IsTrue(Length(candidates) > 0, 'expected at least one candidate');
  Assert.AreEqual('D:\Ajur2000\ajur.map', candidates[0]);
end;

procedure TMapFilePathTests.UncOutputDirIsNotAppendedToProjectDir;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Proj', '\\build\drops\bin', 'ajur', 'Release', TDPMPlatform.Win64);
  Assert.IsTrue(Length(candidates) > 0, 'expected at least one candidate');
  Assert.AreEqual('\\build\drops\bin\ajur.map', candidates[0]);
end;

procedure TMapFilePathTests.RelativeOutputDirResolvesAgainstProjectDir;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Proj', '..\bin', 'ajur', 'Release', TDPMPlatform.Win32);
  Assert.IsTrue(Length(candidates) > 0, 'expected at least one candidate');
  Assert.AreEqual('D:\bin\ajur.map', candidates[0]);
end;

procedure TMapFilePathTests.OutputDirTokensAreExpanded;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Proj', '.\$(Platform)\$(Config)', 'ajur', 'Debug', TDPMPlatform.Win64);
  Assert.IsTrue(Length(candidates) > 0, 'expected at least one candidate');
  Assert.AreEqual('D:\Proj\Win64\Debug\ajur.map', candidates[0]);
end;

//dcc with no -E switch writes the exe and the map next to the project, so an
//absent/empty DCC_ExeOutput must probe the project dir before the IDE default shape.
procedure TMapFilePathTests.EmptyOutputDirProbesProjectDirFirst;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Proj', '', 'ajur', 'Release', TDPMPlatform.Win32);
  Assert.IsTrue(Length(candidates) >= 2, 'expected the project dir and the default shape');
  Assert.AreEqual('D:\Proj\ajur.map', candidates[0]);
  Assert.IsTrue(IndexOfPath(candidates, 'D:\Proj\Win32\Release\ajur.map') > 0,
                'expected the .\$(Platform)\$(Config) shape as a later candidate');
end;

procedure TMapFilePathTests.ProjectDirIsAlwaysProbed;
var
  candidates : TArray<string>;
begin
  candidates := GetMapFileCandidates('D:\Proj', '.\Win32\Release', 'ajur', 'Release', TDPMPlatform.Win32);
  Assert.AreEqual('D:\Proj\Win32\Release\ajur.map', candidates[0]);
  Assert.IsTrue(IndexOfPath(candidates, 'D:\Proj\ajur.map') > 0,
                'the project dir should be probed when the configured output dir has no map');
end;

procedure TMapFilePathTests.CandidatesAreDeduped;
var
  candidates : TArray<string>;
begin
  //output dir resolves to the project dir itself - shouldn't be listed twice.
  candidates := GetMapFileCandidates('D:\Proj', '.', 'ajur', 'Release', TDPMPlatform.Win32);
  Assert.AreEqual('D:\Proj\ajur.map', candidates[0]);
  Assert.AreEqual<integer>(2, Length(candidates), 'expected the project dir once plus the default shape');
end;

initialization
  TDUnitX.RegisterTestFixture(TMapFileReaderTests);
  TDUnitX.RegisterTestFixture(TMapFilePathTests);

end.
