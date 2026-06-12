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

// End-to-end pack tests for the auto-derived 'precompiledBinaries' declaration. Each test builds a
// temp working folder with a .dspec.yaml + source files (some real PE binaries, some not), runs the
// real TPackageWriter, then opens the produced .dpkg and asserts the embedded package.dspec.yaml
// declares exactly the shipped PE binaries with canonical (forward-slash) archive paths that match
// the dpm-manifest.json entries.

unit DPM.Core.Tests.Packaging.PrecompiledBinaries;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TPrecompiledBinariesPackTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Declares_Shipped_Bpl_With_ForwardSlash_Path;
    procedure Does_Not_Declare_NonPE_Source_Files;
    procedure Declared_Path_Matches_Manifest_Entry;
    procedure No_PE_Files_Emits_No_Declaration;
    procedure Detects_PE_By_Content_When_Extension_Is_Disguised;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
  Spring.Collections,
  VSoft.CancellationToken,
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Constants,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Manifest,
  DPM.Core.Package.Archive,
  DPM.Core.Packaging,
  DPM.Core.Packaging.Archive,
  DPM.Core.Packaging.Writer,
  DPM.Core.Packaging.Archive.Writer,
  DPM.Core.Options.Pack;

var
  GWorkDirCounter : integer = 0;

{ helpers }

// Minimal but valid-enough PE image: MZ header + e_lfanew pointing at a 'PE'#0#0 signature. That is
// all TPEUtils.IsPE checks, which is all the pack-time auto-derivation needs.
function MinimalPEBytes : TBytes;
const
  cPEOffset = $80;
begin
  SetLength(result, cPEOffset + 4 + 8);
  //dynamic arrays are zero-initialised by SetLength
  result[0] := Ord('M');
  result[1] := Ord('Z');
  //e_lfanew (u32 LE) at 0x3C
  result[$3C] := Byte(cPEOffset and $FF);
  result[$3D] := Byte((cPEOffset shr 8) and $FF);
  result[$3E] := 0;
  result[$3F] := 0;
  //'PE'#0#0
  result[cPEOffset] := Ord('P');
  result[cPEOffset + 1] := Ord('E');
  result[cPEOffset + 2] := 0;
  result[cPEOffset + 3] := 0;
end;

function NewWriter : IPackageWriter;
var
  logger : ILogger;
  hashing : IHashingService;
  manifestSvc : IManifestService;
  validator : IArchiveValidator;
  archiveWriter : IPackageArchiveWriter;
  specReader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  hashing := TBCryptHashingService.Create;
  manifestSvc := TManifestService.Create(hashing);
  validator := TArchiveValidator.Create(manifestSvc);
  archiveWriter := TPackageArchiveWriter.Create(logger);
  specReader := TPackageSpecReader.Create(logger);
  result := TPackageWriter.Create(logger, archiveWriter, specReader, manifestSvc, validator);
end;

function MakeWorkDir : string;
begin
  Inc(GWorkDirCounter);
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-pack-pcb-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '-' + IntToStr(GWorkDirCounter));
  TDirectory.CreateDirectory(result);
  TDirectory.CreateDirectory(TPath.Combine(result, 'source'));
end;

// Writes a standard single-compiler dspec into workDir that ships everything under .\source as
// lib/Win32. Returns the full path to the written .dspec.yaml.
function WriteDspec(const workDir : string) : string;
var
  yaml : TStringList;
begin
  result := TPath.Combine(workDir, 'test.dspec.yaml');
  yaml := TStringList.Create;
  try
    yaml.Add('metadata:');
    yaml.Add('  id: Test.Pcb');
    yaml.Add('  version: 1.0.0');
    yaml.Add('  description: PE binary declaration pack test');
    yaml.Add('  authors:');
    yaml.Add('    - Vincent Parrett');
    yaml.Add('  license: Apache-2.0');
    yaml.Add('targetPlatforms:');
    yaml.Add('  - compiler: 12.0');
    yaml.Add('    platforms: [Win32, Win64]');
    yaml.Add('    template: default');
    yaml.Add('templates:');
    yaml.Add('  - name: default');
    yaml.Add('    source:');
    yaml.Add('      - src: .\source\*.bpl');
    yaml.Add('        dest: lib/Win32');
    yaml.Add('      - src: .\source\*.dll');
    yaml.Add('        dest: lib/Win32');
    yaml.Add('      - src: .\source\*.pas');
    yaml.Add('        dest: lib/Win32');
    //CRLF, UTF-8 - the reader is happy with either, but match repo convention.
    yaml.WriteBOM := false;
    yaml.SaveToFile(result, TEncoding.UTF8);
  finally
    yaml.Free;
  end;
end;

function Pack(const workDir : string) : string;
var
  writer : IPackageWriter;
  options : TPackOptions;
  token : ICancellationToken;
  produced : TArray<string>;
  ok : boolean;
begin
  writer := NewWriter;
  options := TPackOptions.Create;
  try
    options.SpecFile := TPath.Combine(workDir, 'test.dspec.yaml');
    options.BasePath := workDir;
    options.OutputFolder := workDir;
    token := TCancellationTokenSourceFactory.Create.Token;
    ok := writer.WritePackageFromSpec(token, options);
    Assert.IsTrue(ok, 'WritePackageFromSpec failed');
  finally
    options.Free;
  end;

  produced := TDirectory.GetFiles(workDir, '*' + cPackageFileExt, TSearchOption.soTopDirectoryOnly);
  Assert.AreEqual<integer>(1, Length(produced), 'expected exactly one .dpkg');
  result := produced[0];
end;

// Reads a single named entry from a .dpkg as a UTF-8 string. Returns '' if the entry is absent.
function ReadArchiveText(const dpkgPath, entryName : string) : string;
var
  zip : TZipFile;
  i : integer;
  bytes : TBytes;
begin
  result := '';
  zip := TZipFile.Create;
  try
    zip.Open(dpkgPath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      if SameText(zip.FileName[i], entryName) then
      begin
        zip.Read(i, bytes);
        result := TEncoding.UTF8.GetString(bytes);
        exit;
      end;
    end;
  finally
    zip.Free;
  end;
end;

function ArchiveHasEntry(const dpkgPath, entryName : string) : boolean;
var
  zip : TZipFile;
  i : integer;
begin
  result := false;
  zip := TZipFile.Create;
  try
    zip.Open(dpkgPath, zmRead);
    for i := 0 to zip.FileCount - 1 do
      if SameText(zip.FileName[i], entryName) then
        exit(true);
  finally
    zip.Free;
  end;
end;

// Loads the embedded package.dspec.yaml from a .dpkg into the object model.
function LoadEmbeddedSpec(const dpkgPath : string) : IPackageSpec;
var
  reader : IPackageSpecReader;
  yaml : string;
begin
  yaml := ReadArchiveText(dpkgPath, cPackageDspecFile);
  Assert.IsTrue(yaml <> '', 'package.dspec.yaml missing from package');
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  result := reader.ReadSpecString(yaml);
  Assert.IsNotNull(result, 'could not parse embedded dspec');
end;

function FirstTemplateBinaries(const spec : IPackageSpec) : IList<string>;
begin
  Assert.IsTrue(spec.Templates.Count > 0, 'no templates in embedded spec');
  result := spec.Templates[0].PrecompiledBinaries;
end;

procedure CleanDir(const dir : string);
begin
  try
    if TDirectory.Exists(dir) then
      TDirectory.Delete(dir, true);
  except
    //best effort - temp dir, leave it if something still has a handle.
  end;
end;

{ TPrecompiledBinariesPackTests }

procedure TPrecompiledBinariesPackTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TPrecompiledBinariesPackTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TPrecompiledBinariesPackTests.Declares_Shipped_Bpl_With_ForwardSlash_Path;
var
  workDir : string;
  dpkg : string;
  bins : IList<string>;
begin
  workDir := MakeWorkDir;
  try
    TFile.WriteAllBytes(TPath.Combine(workDir, 'source\runtime.bpl'), MinimalPEBytes);
    TFile.WriteAllText(TPath.Combine(workDir, 'source\runtime.pas'), 'unit runtime; interface implementation end.');
    WriteDspec(workDir);

    dpkg := Pack(workDir);
    bins := FirstTemplateBinaries(LoadEmbeddedSpec(dpkg));

    Assert.AreEqual(1, bins.Count, 'exactly one PE should be declared');
    Assert.AreEqual('lib/Win32/runtime.bpl', bins[0], 'declared path must be forward-slash + dest-prefixed');
    Assert.IsTrue(ArchiveHasEntry(dpkg, 'lib/Win32/runtime.bpl'), 'declared binary must actually be in the archive');
  finally
    CleanDir(workDir);
  end;
end;

procedure TPrecompiledBinariesPackTests.Does_Not_Declare_NonPE_Source_Files;
var
  workDir : string;
  dpkg : string;
  bins : IList<string>;
begin
  workDir := MakeWorkDir;
  try
    TFile.WriteAllBytes(TPath.Combine(workDir, 'source\design.bpl'), MinimalPEBytes);
    TFile.WriteAllText(TPath.Combine(workDir, 'source\design.pas'), 'unit design; interface implementation end.');
    WriteDspec(workDir);

    dpkg := Pack(workDir);
    bins := FirstTemplateBinaries(LoadEmbeddedSpec(dpkg));

    //the .pas ships in the package but must NOT be declared as a precompiled binary.
    Assert.IsTrue(ArchiveHasEntry(dpkg, 'lib/Win32/design.pas'), 'the .pas should still be packed');
    Assert.AreEqual(1, bins.Count, 'only the PE should be declared');
    Assert.IsFalse(bins.Contains('lib/Win32/design.pas'), 'non-PE source must not be declared');
  finally
    CleanDir(workDir);
  end;
end;

procedure TPrecompiledBinariesPackTests.Declared_Path_Matches_Manifest_Entry;
var
  workDir : string;
  dpkg : string;
  bins : IList<string>;
  manifestJson : string;
begin
  workDir := MakeWorkDir;
  try
    TFile.WriteAllBytes(TPath.Combine(workDir, 'source\foo.dll'), MinimalPEBytes);
    WriteDspec(workDir);

    dpkg := Pack(workDir);
    bins := FirstTemplateBinaries(LoadEmbeddedSpec(dpkg));
    Assert.AreEqual(1, bins.Count, 'one PE declared');
    Assert.AreEqual('lib/Win32/foo.dll', bins[0], 'declared path');

    //the gallery cross-checks precompiledBinaries against manifest file entries, so the path must be
    //byte-identical to the one recorded in dpm-manifest.json.
    manifestJson := ReadArchiveText(dpkg, cManifestFileName);
    Assert.IsTrue(manifestJson <> '', 'manifest missing');
    Assert.IsTrue(Pos('"' + bins[0] + '"', manifestJson) > 0,
      'declared path must appear verbatim in dpm-manifest.json');
  finally
    CleanDir(workDir);
  end;
end;

procedure TPrecompiledBinariesPackTests.No_PE_Files_Emits_No_Declaration;
var
  workDir : string;
  dpkg : string;
  spec : IPackageSpec;
  dspecYaml : string;
begin
  workDir := MakeWorkDir;
  try
    TFile.WriteAllText(TPath.Combine(workDir, 'source\only.pas'), 'unit only; interface implementation end.');
    WriteDspec(workDir);

    dpkg := Pack(workDir);
    spec := LoadEmbeddedSpec(dpkg);
    Assert.AreEqual(0, FirstTemplateBinaries(spec).Count, 'no PE -> no declared binaries');

    //the empty list must not be serialised at all (ToYAML only writes it when non-empty). Match the
    //key form ('precompiledBinaries:') so a stray mention in another value can't trip the check.
    dspecYaml := ReadArchiveText(dpkg, cPackageDspecFile);
    Assert.IsFalse(Pos('precompiledBinaries:', dspecYaml) > 0, 'precompiledBinaries key must be omitted when empty');
  finally
    CleanDir(workDir);
  end;
end;

procedure TPrecompiledBinariesPackTests.Detects_PE_By_Content_When_Extension_Is_Disguised;
var
  workDir : string;
  dpkg : string;
  bins : IList<string>;
begin
  workDir := MakeWorkDir;
  try
    //a PE renamed to .pas - detection is by content, so it must still be declared.
    TFile.WriteAllBytes(TPath.Combine(workDir, 'source\sneaky.pas'), MinimalPEBytes);
    WriteDspec(workDir);

    dpkg := Pack(workDir);
    bins := FirstTemplateBinaries(LoadEmbeddedSpec(dpkg));

    Assert.AreEqual(1, bins.Count, 'a disguised PE must still be detected');
    Assert.AreEqual('lib/Win32/sneaky.pas', bins[0], 'declared by content regardless of extension');
  finally
    CleanDir(workDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPrecompiledBinariesPackTests);

end.
