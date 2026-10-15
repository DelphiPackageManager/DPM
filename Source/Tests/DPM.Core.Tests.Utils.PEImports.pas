unit DPM.Core.Tests.Utils.PEImports;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework;

type
  //How a delay-load descriptor names its DLL. Current linkers write an RVA (Attributes bit 0 set);
  //the original Visual C++ 6 form wrote a virtual address.
  TTestPEDelayForm = (RVA, VA);

///<summary>A minimal PE image : headers plus one section holding import descriptors for imports
/// and delay-load descriptors for delayed, and the module names. Nothing else in it is valid - it
/// exists to feed an import table reader without depending on whatever binaries are lying around.</summary>
function BuildTestPE(const is64 : boolean; const imports, delayed : array of string;
                     const delayForm : TTestPEDelayForm) : TBytes;

procedure WriteTestPE(const fileName : string; const is64 : boolean; const imports, delayed : array of string);

type
  [TestFixture]
  TPEImportsTests = class
  private
    FTempDir : string;
    function WriteImage(const bytes : TBytes) : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ReadsStaticImports_PE32;
    [Test]
    procedure ReadsStaticImports_PE32Plus;
    [Test]
    procedure ReadsDelayLoadImports;
    [Test]
    procedure ReadsDelayLoadImports_LegacyVirtualAddressForm;
    [Test]
    procedure ModuleImportedTwice_IsListedOnce;
    [Test]
    procedure ReadsARealDelphiExecutable;
    [Test]
    procedure NotAPEImage_Fails;
    [Test]
    procedure TruncatedImage_Fails;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  DPM.Core.Utils.PEImports;

procedure PutWord(var bytes : TBytes; const offset : integer; const value : Word);
begin
  bytes[offset] := Byte(value);
  bytes[offset + 1] := Byte(value shr 8);
end;

procedure PutDWord(var bytes : TBytes; const offset : integer; const value : Cardinal);
begin
  PutWord(bytes, offset, Word(value));
  PutWord(bytes, offset + 2, Word(value shr 16));
end;

function BuildTestPE(const is64 : boolean; const imports, delayed : array of string;
                     const delayForm : TTestPEDelayForm) : TBytes;
const
  cFileAlign = $200;
  cSectionRva = $1000;
  cImageBase = $400000;
  cPEOffset = $40;
var
  importDescSize : integer;
  delayDescSize : integer;
  rawSize : integer;
  section : TBytes;
  namePos : integer;
  nameRva : Cardinal;
  i : integer;
  j : integer;
  optStart : integer;
  optSize : integer;
  dataDirs : integer;
  sectStart : integer;
  name : AnsiString;

  function AddName(const value : string) : Cardinal;
  var
    k : integer;
  begin
    name := AnsiString(value);
    result := cSectionRva + Cardinal(namePos);
    for k := 1 to Length(name) do
      section[namePos + k - 1] := Byte(name[k]);
    section[namePos + Length(name)] := 0;
    namePos := namePos + Length(name) + 1;
  end;

begin
  importDescSize := (Length(imports) + 1) * 20;
  delayDescSize := (Length(delayed) + 1) * 32;
  rawSize := importDescSize + delayDescSize;
  for i := 0 to Length(imports) - 1 do
    rawSize := rawSize + Length(imports[i]) + 1;
  for i := 0 to Length(delayed) - 1 do
    rawSize := rawSize + Length(delayed[i]) + 1;
  rawSize := ((rawSize + cFileAlign - 1) div cFileAlign) * cFileAlign;
  SetLength(section, rawSize);
  FillChar(section[0], rawSize, 0);

  namePos := importDescSize + delayDescSize;
  for i := 0 to Length(imports) - 1 do
  begin
    nameRva := AddName(imports[i]);
    PutDWord(section, i * 20 + 12, nameRva);    //Name
    PutDWord(section, i * 20 + 16, $2000);      //FirstThunk - anything non zero
  end;
  for i := 0 to Length(delayed) - 1 do
  begin
    nameRva := AddName(delayed[i]);
    j := importDescSize + i * 32;
    if delayForm = TTestPEDelayForm.RVA then
    begin
      PutDWord(section, j, 1);                  //Attributes : dlattrRva
      PutDWord(section, j + 4, nameRva);
    end
    else
      PutDWord(section, j + 4, nameRva + cImageBase);
  end;

  SetLength(result, cFileAlign + rawSize);
  FillChar(result[0], Length(result), 0);
  result[0] := Ord('M');
  result[1] := Ord('Z');
  PutDWord(result, $3C, cPEOffset);
  result[cPEOffset] := Ord('P');
  result[cPEOffset + 1] := Ord('E');

  optStart := cPEOffset + 4 + 20;
  if is64 then
  begin
    optSize := 112 + 16 * 8;
    PutWord(result, cPEOffset + 4, $8664);      //Machine
    PutWord(result, optStart, $20B);            //Magic PE32+
    PutDWord(result, optStart + 24, cImageBase); //ImageBase (low dword of 8)
    PutDWord(result, optStart + 108, 16);       //NumberOfRvaAndSizes
    dataDirs := optStart + 112;
  end
  else
  begin
    optSize := 96 + 16 * 8;
    PutWord(result, cPEOffset + 4, $14C);
    PutWord(result, optStart, $10B);            //Magic PE32
    PutDWord(result, optStart + 28, cImageBase);
    PutDWord(result, optStart + 92, 16);
    dataDirs := optStart + 96;
  end;
  PutWord(result, cPEOffset + 4 + 2, 1);        //NumberOfSections
  PutWord(result, cPEOffset + 4 + 16, optSize); //SizeOfOptionalHeader
  PutDWord(result, dataDirs + 1 * 8, cSectionRva);
  PutDWord(result, dataDirs + 1 * 8 + 4, importDescSize);
  if Length(delayed) > 0 then
  begin
    PutDWord(result, dataDirs + 13 * 8, cSectionRva + Cardinal(importDescSize));
    PutDWord(result, dataDirs + 13 * 8 + 4, delayDescSize);
  end;

  sectStart := optStart + optSize;
  result[sectStart] := Ord('.');
  result[sectStart + 1] := Ord('i');
  result[sectStart + 2] := Ord('d');
  result[sectStart + 3] := Ord('a');
  result[sectStart + 4] := Ord('t');
  result[sectStart + 5] := Ord('a');
  PutDWord(result, sectStart + 8, rawSize);     //VirtualSize
  PutDWord(result, sectStart + 12, cSectionRva); //VirtualAddress
  PutDWord(result, sectStart + 16, rawSize);    //SizeOfRawData
  PutDWord(result, sectStart + 20, cFileAlign); //PointerToRawData

  Move(section[0], result[cFileAlign], rawSize);
end;

procedure WriteTestPE(const fileName : string; const is64 : boolean; const imports, delayed : array of string);
begin
  ForceDirectories(ExtractFilePath(fileName));
  TFile.WriteAllBytes(fileName, BuildTestPE(is64, imports, delayed, TTestPEDelayForm.RVA));
end;

{ TPEImportsTests }

procedure TPEImportsTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FTempDir := TPath.Combine(TPath.GetTempPath, 'dpm-pe-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(FTempDir);
end;

procedure TPEImportsTests.TearDown;
begin
  if DirectoryExists(FTempDir) then
    TDirectory.Delete(FTempDir, true);
end;

function TPEImportsTests.WriteImage(const bytes : TBytes) : string;
begin
  result := TPath.Combine(FTempDir, 'image.exe');
  TFile.WriteAllBytes(result, bytes);
end;

procedure TPEImportsTests.ReadsStaticImports_PE32;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  Assert.IsTrue(TryReadPEImports(WriteImage(BuildTestPE(false, ['rtl370.bpl', 'KERNEL32.dll'], [], TTestPEDelayForm.RVA)), imports, error), error);

  Assert.AreEqual(2, Length(imports));
  //case sensitive - the name as the import table spells it
  Assert.AreEqual('rtl370.bpl', imports[0].ModuleName, false);
  Assert.AreEqual('KERNEL32.dll', imports[1].ModuleName, false);
  Assert.IsFalse(imports[0].Delayed);
end;

procedure TPEImportsTests.ReadsStaticImports_PE32Plus;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  Assert.IsTrue(TryReadPEImports(WriteImage(BuildTestPE(true, ['vcl370.bpl', 'user32.dll', 'sqlite3.dll'], [], TTestPEDelayForm.RVA)), imports, error), error);

  Assert.AreEqual(3, Length(imports));
  Assert.AreEqual('sqlite3.dll', imports[2].ModuleName, false);
end;

procedure TPEImportsTests.ReadsDelayLoadImports;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  Assert.IsTrue(TryReadPEImports(WriteImage(BuildTestPE(true, ['kernel32.dll'], ['libssl-3-x64.dll'], TTestPEDelayForm.RVA)), imports, error), error);

  Assert.AreEqual(2, Length(imports));
  Assert.AreEqual('libssl-3-x64.dll', imports[1].ModuleName, false);
  Assert.IsTrue(imports[1].Delayed, 'from the delay-load table');
  Assert.IsFalse(imports[0].Delayed);
end;

procedure TPEImportsTests.ReadsDelayLoadImports_LegacyVirtualAddressForm;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  Assert.IsTrue(TryReadPEImports(WriteImage(BuildTestPE(false, ['kernel32.dll'], ['fbclient.dll'], TTestPEDelayForm.VA)), imports, error), error);

  Assert.AreEqual(2, Length(imports));
  Assert.AreEqual('fbclient.dll', imports[1].ModuleName, false);
end;

procedure TPEImportsTests.ModuleImportedTwice_IsListedOnce;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  //a module can have several descriptors, and be both statically and delay loaded
  Assert.IsTrue(TryReadPEImports(WriteImage(BuildTestPE(false, ['kernel32.dll', 'user32.dll', 'KERNEL32.DLL'], ['user32.dll'], TTestPEDelayForm.RVA)), imports, error), error);

  Assert.AreEqual(2, Length(imports));
  Assert.IsFalse(imports[1].Delayed, 'statically imported, so not a delay-load dependency');
end;

//BuildTestPE and the reader share one reading of the PE spec, so check the reader against an
//image a real linker wrote too - this test executable. Every Delphi program imports kernel32.
procedure TPEImportsTests.ReadsARealDelphiExecutable;
var
  imports : TArray<TPEImport>;
  error : string;
  i : integer;
  found : boolean;
begin
  Assert.IsTrue(TryReadPEImports(ParamStr(0), imports, error), error);

  found := false;
  for i := 0 to Length(imports) - 1 do
    if SameText(imports[i].ModuleName, 'kernel32.dll') then
      found := true;
  Assert.IsTrue(found, 'kernel32.dll not among ' + IntToStr(Length(imports)) + ' imports');
end;

procedure TPEImportsTests.NotAPEImage_Fails;
var
  imports : TArray<TPEImport>;
  error : string;
begin
  Assert.IsFalse(TryReadPEImports(WriteImage(TEncoding.ASCII.GetBytes('hello, not an executable')), imports, error));
  Assert.AreNotEqual('', error);
end;

procedure TPEImportsTests.TruncatedImage_Fails;
var
  bytes : TBytes;
  imports : TArray<TPEImport>;
  error : string;
begin
  //headers intact, the section holding the import table cut off
  bytes := BuildTestPE(false, ['kernel32.dll'], [], TTestPEDelayForm.RVA);
  SetLength(bytes, $200 + 8);
  Assert.IsFalse(TryReadPEImports(WriteImage(bytes), imports, error));
  Assert.AreNotEqual('', error);
end;

initialization
  TDUnitX.RegisterTestFixture(TPEImportsTests);

end.
