{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Tests.Utils.PE;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TPEUtilsTests = class
  published
    procedure PE32_Is_Detected;
    procedure PE32Plus_Is_Detected;
    procedure PE32_Without_SecurityDir_Is_Unsigned;
    procedure PE32_With_SecurityDir_Is_Signed;
    procedure PE32Plus_With_SecurityDir_Is_Signed;
    procedure Text_Is_Not_PE;
    procedure Truncated_Buffer_Is_Not_PE;
    procedure Pascal_Source_With_MZ_Prefix_But_No_PE_Is_Not_PE;
    procedure File_Overload_Detects_Signed_Binary;
    procedure Stream_Overload_Detects_PE_And_Signature;
    procedure SecurityDir_Index_Beyond_NumberOfRvaAndSizes_Is_Unsigned;
    procedure Unknown_OptionalHeader_Magic_Is_Unsigned;
    procedure Empty_Buffer_Is_Not_PE;
    procedure Signed_Buffer_Is_Still_A_PE;
  end;

  // VerifyAuthenticode is what stands between a downloaded DPM installer and running it. The signed
  // fixture is the IDE's bds.exe - every machine that builds these tests has a Delphi, and Embarcadero
  // embeds its signature (the files in System32 are catalog signed, which doesn't count). Revocation
  // is not checked here, so the tests don't depend on reaching a certificate authority.
  [TestFixture]
  TAuthenticodeTests = class
  private
    FTempFiles : TArray<string>;
    function TempExe : string;
  public
    [TearDown]
    procedure TearDown;
  published
    procedure Signed_Executable_Verifies;
    procedure Modified_Copy_Of_A_Signed_Executable_Is_Refused;
    procedure Unsigned_Executable_Is_Refused;
    procedure Catalog_Signed_System_File_Is_Refused;
    procedure Text_File_Named_Exe_Is_Refused;
    procedure Missing_File_Is_Refused;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.EnvironmentProvider,
  DPM.Core.Utils.PE;

const
  cPEOffset = $80;          // where the PE header starts in our synthetic images
  cOptMagicPE32 = $010B;
  cOptMagicPE32Plus = $020B;

{ helpers }

procedure PutWord(var bytes : TBytes; const offset : integer; const value : Word);
begin
  bytes[offset] := Byte(value and $FF);
  bytes[offset + 1] := Byte((value shr 8) and $FF);
end;

procedure PutCardinal(var bytes : TBytes; const offset : integer; const value : Cardinal);
begin
  bytes[offset] := Byte(value and $FF);
  bytes[offset + 1] := Byte((value shr 8) and $FF);
  bytes[offset + 2] := Byte((value shr 16) and $FF);
  bytes[offset + 3] := Byte((value shr 24) and $FF);
end;

// Builds a minimal but structurally-correct PE image. When 'signed' the security data directory
// (IMAGE_DIRECTORY_ENTRY_SECURITY, index 4) carries a non-zero VirtualAddress + Size, which is what
// HasAuthenticodeSignature looks for.
function MakePE(const plus : boolean; const signed : boolean) : TBytes;
var
  result_ : TBytes;
  optHeaderStart : integer;
  numRvaOffset : integer;
  dataDirStart : integer;
  secDirPos : integer;
begin
  optHeaderStart := cPEOffset + 4 + 20; // PE sig (4) + COFF header (20)
  if plus then
  begin
    numRvaOffset := optHeaderStart + 108;
    dataDirStart := optHeaderStart + 112;
  end
  else
  begin
    numRvaOffset := optHeaderStart + 92;
    dataDirStart := optHeaderStart + 96;
  end;
  secDirPos := dataDirStart + (4 * 8);

  SetLength(result_, secDirPos + 8 + 16); // room for the security dir entry + slack
  FillChar(result_[0], Length(result_), 0);

  // DOS header
  result_[0] := Ord('M');
  result_[1] := Ord('Z');
  PutCardinal(result_, $3C, cPEOffset); // e_lfanew

  // PE signature 'PE'#0#0
  result_[cPEOffset] := Ord('P');
  result_[cPEOffset + 1] := Ord('E');
  result_[cPEOffset + 2] := 0;
  result_[cPEOffset + 3] := 0;

  // Optional header magic
  if plus then
    PutWord(result_, optHeaderStart, cOptMagicPE32Plus)
  else
    PutWord(result_, optHeaderStart, cOptMagicPE32);

  // NumberOfRvaAndSizes - must be > 4 for the security dir to exist
  PutCardinal(result_, numRvaOffset, 16);

  if signed then
  begin
    PutCardinal(result_, secDirPos, $1000);    // VirtualAddress (file offset of the cert table)
    PutCardinal(result_, secDirPos + 4, $200); // Size
  end;

  result := result_;
end;

{ TPEUtilsTests }

procedure TPEUtilsTests.PE32_Is_Detected;
begin
  Assert.IsTrue(TPEUtils.IsPE(MakePE(false, false)), 'PE32 should be detected as a PE');
end;

procedure TPEUtilsTests.PE32Plus_Is_Detected;
begin
  Assert.IsTrue(TPEUtils.IsPE(MakePE(true, false)), 'PE32+ should be detected as a PE');
end;

procedure TPEUtilsTests.PE32_Without_SecurityDir_Is_Unsigned;
begin
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(MakePE(false, false)),
    'a PE32 with an empty security directory must be reported unsigned');
end;

procedure TPEUtilsTests.PE32_With_SecurityDir_Is_Signed;
begin
  Assert.IsTrue(TPEUtils.HasAuthenticodeSignature(MakePE(false, true)),
    'a PE32 with a non-zero security directory must be reported signed');
end;

procedure TPEUtilsTests.PE32Plus_With_SecurityDir_Is_Signed;
begin
  Assert.IsTrue(TPEUtils.HasAuthenticodeSignature(MakePE(true, true)),
    'a PE32+ with a non-zero security directory must be reported signed');
end;

procedure TPEUtilsTests.Text_Is_Not_PE;
var
  bytes : TBytes;
begin
  bytes := TEncoding.UTF8.GetBytes('unit Foo; interface implementation end.');
  Assert.IsFalse(TPEUtils.IsPE(bytes), 'plain text must not be detected as a PE');
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(bytes), 'plain text cannot be signed');
end;

procedure TPEUtilsTests.Truncated_Buffer_Is_Not_PE;
var
  bytes : TBytes;
begin
  //'MZ' present but the buffer is too short to contain e_lfanew / the PE header.
  SetLength(bytes, 4);
  bytes[0] := Ord('M');
  bytes[1] := Ord('Z');
  bytes[2] := 0;
  bytes[3] := 0;
  Assert.IsFalse(TPEUtils.IsPE(bytes), 'a truncated MZ stub must not be treated as a PE');
end;

procedure TPEUtilsTests.Pascal_Source_With_MZ_Prefix_But_No_PE_Is_Not_PE;
var
  bytes : TBytes;
begin
  //An MZ header whose e_lfanew points somewhere that is not 'PE\0\0' must be rejected.
  bytes := MakePE(false, false);
  //corrupt the PE signature
  bytes[cPEOffset] := Ord('X');
  Assert.IsFalse(TPEUtils.IsPE(bytes), 'MZ without a valid PE signature is not a PE');
end;

procedure TPEUtilsTests.File_Overload_Detects_Signed_Binary;
var
  tempFile : string;
  bytes : TBytes;
begin
  //never write to the working dir - use the environment temp folder.
  tempFile := TPath.GetTempFileName;
  bytes := MakePE(false, true);
  TFile.WriteAllBytes(tempFile, bytes);
  try
    Assert.IsTrue(TPEUtils.IsPE(tempFile), 'file overload should detect the PE');
    Assert.IsTrue(TPEUtils.HasAuthenticodeSignature(tempFile), 'file overload should detect the signature');
  finally
    if TFile.Exists(tempFile) then
      TFile.Delete(tempFile);
  end;
end;

procedure TPEUtilsTests.Stream_Overload_Detects_PE_And_Signature;
var
  ms : TBytesStream;
begin
  ms := TBytesStream.Create(MakePE(false, true));
  try
    Assert.IsTrue(TPEUtils.IsPE(TStream(ms)), 'stream overload IsPE');
    //the overload must use absolute offsets, not the caller's current position.
    ms.Position := 123;
    Assert.IsTrue(TPEUtils.HasAuthenticodeSignature(TStream(ms)), 'stream overload HasAuthenticodeSignature');
  finally
    ms.Free;
  end;
end;

procedure TPEUtilsTests.SecurityDir_Index_Beyond_NumberOfRvaAndSizes_Is_Unsigned;
var
  bytes : TBytes;
  numRvaPos : integer;
begin
  bytes := MakePE(false, true); // security-dir bytes are populated...
  numRvaPos := (cPEOffset + 4 + 20) + 92;
  //...but force NumberOfRvaAndSizes = 4, so the security directory (index 4) does not exist.
  bytes[numRvaPos] := 4;
  bytes[numRvaPos + 1] := 0;
  bytes[numRvaPos + 2] := 0;
  bytes[numRvaPos + 3] := 0;
  Assert.IsTrue(TPEUtils.IsPE(bytes), 'still a PE');
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(bytes),
    'security dir index >= NumberOfRvaAndSizes must read as unsigned');
end;

procedure TPEUtilsTests.Unknown_OptionalHeader_Magic_Is_Unsigned;
var
  bytes : TBytes;
  optHeaderStart : integer;
begin
  bytes := MakePE(false, true);
  optHeaderStart := cPEOffset + 4 + 20;
  //0x0107 = ROM image, neither PE32 (0x10B) nor PE32+ (0x20B).
  bytes[optHeaderStart] := $07;
  bytes[optHeaderStart + 1] := $01;
  Assert.IsTrue(TPEUtils.IsPE(bytes), 'still a PE by signature');
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(bytes), 'unknown optional-header magic -> unsigned');
end;

procedure TPEUtilsTests.Empty_Buffer_Is_Not_PE;
var
  bytes : TBytes;
begin
  SetLength(bytes, 0);
  Assert.IsFalse(TPEUtils.IsPE(bytes), 'empty buffer is not a PE');
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(bytes), 'empty buffer is not signed');
end;

procedure TPEUtilsTests.Signed_Buffer_Is_Still_A_PE;
begin
  Assert.IsTrue(TPEUtils.IsPE(MakePE(false, true)), 'a signed PE32 is still a PE');
  Assert.IsTrue(TPEUtils.IsPE(MakePE(true, true)), 'a signed PE32+ is still a PE');
end;

{ TAuthenticodeTests }

//the newest installed Delphi's bds.exe that carries an embedded signature (they all do, XE2's included).
function FindSignedDelphiExe : string;
var
  provider : ICompilerEnvironmentProvider;
  version : TCompilerVersion;
  candidate : string;
begin
  result := '';
  provider := TCompilerEnvironmentProvider.Create(TTestLogger.Create);
  for version := DelphiMax downto TCompilerVersion.DelphiXE2 do
  begin
    try
      candidate := provider.GetRootDir(version) + '\bin\bds.exe';
    except
      //not installed.
      continue;
    end;
    if FileExists(candidate) and TPEUtils.HasAuthenticodeSignature(candidate) then
      exit(candidate);
  end;
  Assert.Fail('No installed Delphi with a signed bds.exe was found - these tests use it as the signed executable.');
end;

function TAuthenticodeTests.TempExe : string;
var
  guid : TGUID;
begin
  CreateGUID(guid);
  result := TPath.Combine(TPath.GetTempPath, 'dpm-authenticode-' + Copy(GUIDToString(guid), 2, 36) + '.exe');
  SetLength(FTempFiles, Length(FTempFiles) + 1);
  FTempFiles[High(FTempFiles)] := result;
end;

procedure TAuthenticodeTests.TearDown;
var
  i : integer;
begin
  for i := 0 to High(FTempFiles) do
  begin
    if FileExists(FTempFiles[i]) then
      System.SysUtils.DeleteFile(FTempFiles[i]);
  end;
  SetLength(FTempFiles, 0);
end;

procedure TAuthenticodeTests.Signed_Executable_Verifies;
var
  reason : string;
begin
  Assert.IsTrue(TPEUtils.VerifyAuthenticode(FindSignedDelphiExe, false, reason), 'refused : the file ' + reason);
  Assert.AreEqual('', reason);
end;

procedure TAuthenticodeTests.Modified_Copy_Of_A_Signed_Executable_Is_Refused;
var
  fileName : string;
  bytes : TBytes;
  reason : string;
begin
  //the middle of the image is code/data the signature covers (the signature itself is at the end).
  bytes := TFile.ReadAllBytes(FindSignedDelphiExe);
  bytes[Length(bytes) div 2] := bytes[Length(bytes) div 2] xor $FF;
  fileName := TempExe;
  TFile.WriteAllBytes(fileName, bytes);
  Assert.IsTrue(TPEUtils.HasAuthenticodeSignature(fileName), 'precondition : the copy still carries the signature');

  Assert.IsFalse(TPEUtils.VerifyAuthenticode(fileName, false, reason));
  Assert.Contains(reason, 'changed since it was signed');
end;

procedure TAuthenticodeTests.Unsigned_Executable_Is_Refused;
var
  reason : string;
begin
  //the test runner is built here and never signed.
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(ParamStr(0)), 'precondition : this test exe is not signed');
  Assert.IsFalse(TPEUtils.VerifyAuthenticode(ParamStr(0), false, reason));
  Assert.Contains(reason, 'is not signed');
end;

procedure TAuthenticodeTests.Catalog_Signed_System_File_Is_Refused;
var
  fileName : string;
  reason : string;
begin
  //Windows vouches for cmd.exe through a catalog - an installer has to carry its own signature.
  fileName := TPath.Combine(GetEnvironmentVariable('SystemRoot'), 'System32\cmd.exe');
  Assert.IsFalse(TPEUtils.HasAuthenticodeSignature(fileName), 'precondition : cmd.exe has no embedded signature');
  Assert.IsFalse(TPEUtils.VerifyAuthenticode(fileName, false, reason));
  Assert.Contains(reason, 'is not signed');
end;

procedure TAuthenticodeTests.Text_File_Named_Exe_Is_Refused;
var
  fileName : string;
  reason : string;
begin
  //an html error page saved under the installer's name, say.
  fileName := TempExe;
  TFile.WriteAllText(fileName, '<html><body>Not Found</body></html>');
  Assert.IsFalse(TPEUtils.VerifyAuthenticode(fileName, false, reason));
  Assert.AreNotEqual('', reason);
end;

procedure TAuthenticodeTests.Missing_File_Is_Refused;
var
  reason : string;
begin
  Assert.IsFalse(TPEUtils.VerifyAuthenticode(TempExe, false, reason));
  Assert.Contains(reason, 'does not exist');
end;

initialization
  TDUnitX.RegisterTestFixture(TPEUtilsTests);
  TDUnitX.RegisterTestFixture(TAuthenticodeTests);

end.
