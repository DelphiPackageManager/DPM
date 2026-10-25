unit DPM.Core.Tests.Package.Archive;

// Conformance V-9..V-13: archive-format rule enforcement.
//
// Each test builds a minimal .dpkg programmatically with System.Zip, calls
// TArchiveValidator.Validate, and asserts the expected accept/reject. The
// tests exercise the validator end-to-end without requiring any signing.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TArchiveValidatorTests = class
  private
    function MakeTempPath(const suffix : string) : string;
  public
    [Test] procedure AcceptsValidMinimalArchive;
    [Test] procedure AcceptsArchiveWithNestedFolders;
    [Test] procedure AcceptsManifestAndSignaturesEntries;
    [Test] procedure AcceptsAsciiNameWithoutUtf8Flag;

    [Test] procedure RejectsNonAsciiNameWithoutUtf8Flag;
    [Test] procedure RejectsDuplicateEntryNames;
    [Test] procedure RejectsCaseCollisionDuplicates;
    [Test] procedure RejectsAdsColonInName;
    [Test] procedure RejectsReservedDeviceName;
    [Test] procedure RejectsBackslashInName;
    [Test] procedure RejectsDotDotPath;
    [Test] procedure RejectsTrailingDotSegment;

    [Test] procedure AcceptsRawArchiveFromTestWriter;
    [Test] procedure RejectsUnicodePathFieldInLocalHeader;
    [Test] procedure RejectsUnicodePathFieldInCentralDirectory;
    [Test] procedure RejectsLocalNameDifferingFromCentralName;
    [Test] procedure RejectsDosReparsePointEntry;
    [Test] procedure RejectsDosDeviceEntry;
    [Test] procedure RejectsNonCanonicalSignatureEntries;
    [Test] procedure AcceptsCanonicalSignatureEntries;
    [Test] procedure RejectsGitMarkerAtRoot;
    [Test] procedure AcceptsGitMarkerNameInSubfolder;
  end;

  [TestFixture]
  TPackageArchiveExtractorTests = class
  private
    FRoot : string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure ExtractsFilesAndSkipsSignatures;
    [Test] procedure IgnoresUnicodePathField;
    [Test] procedure RefusesEntryOutsideTheFolder;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
  System.ZLib,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Manifest,
  DPM.Core.Package.Archive;

function TArchiveValidatorTests.MakeTempPath(const suffix : string) : string;
begin
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-archive-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '-' + suffix + '.dpkg');
end;

function NewValidator : IArchiveValidator;
var
  hashing : IHashingService;
  manifestSvc : IManifestService;
begin
  hashing := TBCryptHashingService.Create;
  manifestSvc := TManifestService.Create(hashing);
  result := TArchiveValidator.Create(manifestSvc);
end;

// Tiny zip-building helper. Strings go in as UTF-8 bytes.
procedure AddText(zip : TZipFile; const entryName, content : string);
var
  ms : TMemoryStream;
  bytes : TBytes;
begin
  bytes := TEncoding.UTF8.GetBytes(content);
  ms := TMemoryStream.Create;
  try
    if Length(bytes) > 0 then
      ms.WriteBuffer(bytes[0], Length(bytes));
    ms.Position := 0;
    zip.Add(ms, entryName, zcDeflate);
  finally
    ms.Free;
  end;
end;

procedure BuildArchive(const path : string; const entries : array of string);
var
  zip : TZipFile;
  i : integer;
begin
  // entries is alternating: entryName, content, entryName, content, ...
  zip := TZipFile.Create;
  try
    zip.Open(path, zmWrite);
    i := 0;
    while i < Length(entries) do
    begin
      AddText(zip, entries[i], entries[i + 1]);
      Inc(i, 2);
    end;
  finally
    zip.Free;
  end;
end;

// As BuildArchive, but writes entry names WITHOUT the UTF-8 language-encoding
// flag (general-purpose bit 11) - exactly how 7-Zip, .NET and PowerShell
// Compress-Archive store ASCII names. TZipFile with UTF8Support := False encodes
// names via the system codepage and leaves bit 11 clear.
procedure BuildArchiveNoUtf8Flag(const path : string; const entries : array of string);
var
  zip : TZipFile;
  i : integer;
begin
  zip := TZipFile.Create;
  try
    zip.Open(path, zmWrite);
    zip.UTF8Support := False;
    i := 0;
    while i < Length(entries) do
    begin
      AddText(zip, entries[i], entries[i + 1]);
      Inc(i, 2);
    end;
  finally
    zip.Free;
  end;
end;

// A hand-built single-or-multi entry STORED zip, for the shapes TZipFile.Add can't produce : a
// local name that differs from the central one, extra fields, other host attributes.
type
  TRawEntry = record
    CentralName : string;
    LocalName : string;
    CentralExtra : TBytes;
    LocalExtra : TBytes;
    MadeBy : Word;
    ExternalAttributes : Cardinal;
    Content : string;
  end;

function RawEntry(const name, content : string) : TRawEntry;
begin
  result.CentralName := name;
  result.LocalName := name;
  SetLength(result.CentralExtra, 0);
  SetLength(result.LocalExtra, 0);
  result.MadeBy := $0014; //MS-DOS host, spec 2.0
  result.ExternalAttributes := 0;
  result.Content := content;
end;

function Crc32Of(const bytes : TBytes) : Cardinal;
begin
  if Length(bytes) = 0 then
    result := 0
  else
    result := crc32(0, @bytes[0], Length(bytes));
end;

// The Info-ZIP Unicode Path field : version 1, CRC32 of the header's own name, then the new name.
function UnicodePathField(const headerName, newName : string) : TBytes;
var
  nameBytes : TBytes;
  newNameBytes : TBytes;
  crc : Cardinal;
  size : integer;
begin
  nameBytes := TEncoding.UTF8.GetBytes(headerName);
  newNameBytes := TEncoding.UTF8.GetBytes(newName);
  crc := Crc32Of(nameBytes);
  size := 5 + Length(newNameBytes);
  SetLength(result, 4 + size);
  result[0] := $75; result[1] := $70;
  result[2] := size and $FF; result[3] := (size shr 8) and $FF;
  result[4] := 1;
  result[5] := crc and $FF; result[6] := (crc shr 8) and $FF; result[7] := (crc shr 16) and $FF; result[8] := (crc shr 24) and $FF;
  if Length(newNameBytes) > 0 then
    Move(newNameBytes[0], result[9], Length(newNameBytes));
end;

procedure WriteRawArchive(const path : string; const entries : array of TRawEntry);
var
  stream : TMemoryStream;
  offsets : array of Cardinal;
  i : integer;
  cdStart : Cardinal;
  cdSize : Cardinal;
  data : TBytes;
  name : TBytes;

  procedure W16(value : Word);
  begin
    stream.WriteBuffer(value, 2);
  end;

  procedure W32(value : Cardinal);
  begin
    stream.WriteBuffer(value, 4);
  end;

  procedure WBytes(const value : TBytes);
  begin
    if Length(value) > 0 then
      stream.WriteBuffer(value[0], Length(value));
  end;

begin
  stream := TMemoryStream.Create;
  try
    SetLength(offsets, Length(entries));
    for i := 0 to High(entries) do
    begin
      offsets[i] := stream.Position;
      data := TEncoding.UTF8.GetBytes(entries[i].Content);
      name := TEncoding.UTF8.GetBytes(entries[i].LocalName);
      W32($04034B50);
      W16(20); W16($0800); W16(0); //version needed, utf-8 names, stored
      W16(0); W16($21);            //time, date (1980-01-01)
      W32(Crc32Of(data)); W32(Length(data)); W32(Length(data));
      W16(Length(name)); W16(Length(entries[i].LocalExtra));
      WBytes(name); WBytes(entries[i].LocalExtra); WBytes(data);
    end;
    cdStart := stream.Position;
    for i := 0 to High(entries) do
    begin
      data := TEncoding.UTF8.GetBytes(entries[i].Content);
      name := TEncoding.UTF8.GetBytes(entries[i].CentralName);
      W32($02014B50);
      W16(entries[i].MadeBy); W16(20); W16($0800); W16(0);
      W16(0); W16($21);
      W32(Crc32Of(data)); W32(Length(data)); W32(Length(data));
      W16(Length(name)); W16(Length(entries[i].CentralExtra)); W16(0);
      W16(0); W16(0); W32(entries[i].ExternalAttributes); W32(offsets[i]);
      WBytes(name); WBytes(entries[i].CentralExtra);
    end;
    cdSize := Cardinal(stream.Position) - cdStart;
    W32($06054B50);
    W16(0); W16(0); W16(Length(entries)); W16(Length(entries));
    W32(cdSize); W32(cdStart); W16(0);
    stream.SaveToFile(path);
  finally
    stream.Free;
  end;
end;

{ TArchiveValidatorTests }

procedure TArchiveValidatorTests.AcceptsRawArchiveFromTestWriter;
var
  path : string;
  res : TArchiveValidationResult;
begin
  //guards the tests below - a rejection there must be the crafted defect, not the writer.
  path := MakeTempPath('raw');
  try
    WriteRawArchive(path, [RawEntry('lib/a.pas', 'unit A; end.'), RawEntry('readme.md', 'hi')]);
    res := NewValidator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsUnicodePathFieldInLocalHeader;
var
  path : string;
  entry : TRawEntry;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('upath-local');
  try
    entry := RawEntry('lib/a.pas', 'payload');
    entry.LocalExtra := UnicodePathField('lib/a.pas', '../../escaped.txt');
    WriteRawArchive(path, [entry]);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'a Unicode Path extra field in the local header was accepted');
    Assert.IsTrue(Pos('0x7075', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsUnicodePathFieldInCentralDirectory;
var
  path : string;
  entry : TRawEntry;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('upath-central');
  try
    entry := RawEntry('lib/a.pas', 'payload');
    entry.CentralExtra := UnicodePathField('lib/a.pas', 'lib/b.pas');
    WriteRawArchive(path, [entry]);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'a Unicode Path extra field in the central directory was accepted');
    Assert.IsTrue(Pos('0x7075', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsLocalNameDifferingFromCentralName;
var
  path : string;
  entry : TRawEntry;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('name-mismatch');
  try
    entry := RawEntry('lib/a.pas', 'payload');
    entry.LocalName := 'lib/b.pas';
    WriteRawArchive(path, [entry]);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'a local/central name mismatch was accepted');
    Assert.IsTrue(Pos('local header name', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsDosReparsePointEntry;
var
  path : string;
  entry : TRawEntry;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('reparse');
  try
    entry := RawEntry('lib/link', 'C:\Windows');
    entry.ExternalAttributes := $0400; //FILE_ATTRIBUTE_REPARSE_POINT
    WriteRawArchive(path, [entry]);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'a reparse point entry was accepted');
    Assert.IsTrue(Pos('reparse point', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsDosDeviceEntry;
var
  path : string;
  entry : TRawEntry;
  res : TArchiveValidationResult;
begin
  //the RTL's TZipFile.Extract (11 Alexandria+) makes a symbolic link of an entry with this attribute.
  path := MakeTempPath('device');
  try
    entry := RawEntry('lib/link', 'C:\Windows');
    entry.ExternalAttributes := $0040; //FILE_ATTRIBUTE_DEVICE
    WriteRawArchive(path, [entry]);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'a device entry was accepted');
    Assert.IsTrue(Pos('device', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsNonCanonicalSignatureEntries;
const
  cBadNames : array[0..5] of string = ('signatures/notes.txt', 'signatures/author-1.pas', 'signatures/author-0.p7s',
                                       'signatures/author-.p7s', 'Signatures/author-1.p7s', 'signatures/author-1x.p7s');
var
  path : string;
  i : integer;
  res : TArchiveValidationResult;
begin
  for i := Low(cBadNames) to High(cBadNames) do
  begin
    path := MakeTempPath('sig-' + IntToStr(i));
    try
      BuildArchive(path, ['lib/a.pas', 'unit A; end.', cBadNames[i], 'x']);
      res := NewValidator.Validate(path);
      Assert.IsFalse(res.Ok, cBadNames[i] + ' was accepted');
      Assert.IsTrue(Pos('signatures/', res.Reason) > 0, cBadNames[i] + ' : unexpected reason: ' + res.Reason);
    finally
      if FileExists(path) then DeleteFile(path);
    end;
  end;
end;

procedure TArchiveValidatorTests.AcceptsCanonicalSignatureEntries;
var
  path : string;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('sig-ok');
  try
    BuildArchive(path, ['lib/a.pas', 'unit A; end.',
                        'signatures/author-1.p7s', 'x',
                        'signatures/author-12.p7s', 'x',
                        'signatures/repository-2.p7s', 'x']);
    res := NewValidator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsGitMarkerAtRoot;
var
  path : string;
  res : TArchiveValidationResult;
begin
  path := MakeTempPath('git-marker');
  try
    BuildArchive(path, ['lib/a.pas', 'unit A; end.', '.DPM-git-commit', 'abc']);
    res := NewValidator.Validate(path);
    Assert.IsFalse(res.Ok, 'the git registry marker was accepted');
    Assert.IsTrue(Pos('reserved', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.AcceptsGitMarkerNameInSubfolder;
var
  path : string;
  res : TArchiveValidationResult;
begin
  //only the package root marker is honoured by the cache, so a nested one is harmless.
  path := MakeTempPath('git-marker-nested');
  try
    BuildArchive(path, ['lib/a.pas', 'unit A; end.', 'docs/.dpm-git-commit', 'abc']);
    res := NewValidator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

{ TPackageArchiveExtractorTests }

procedure TPackageArchiveExtractorTests.Setup;
begin
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-extract-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(TPath.Combine(FRoot, 'pkg'));
end;

procedure TPackageArchiveExtractorTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
end;

procedure TPackageArchiveExtractorTests.ExtractsFilesAndSkipsSignatures;
var
  archive : string;
  folder : string;
begin
  archive := TPath.Combine(FRoot, 'a.dpkg');
  folder := TPath.Combine(FRoot, 'pkg');
  BuildArchive(archive, ['package.dspec.yaml', 'spec', 'lib/sub/a.pas', 'unit A; end.', 'signatures/author-1.p7s', 'sig']);
  TPackageArchiveExtractor.Extract(archive, folder);
  Assert.AreEqual('spec', TFile.ReadAllText(TPath.Combine(folder, 'package.dspec.yaml')));
  Assert.AreEqual('unit A; end.', TFile.ReadAllText(TPath.Combine(folder, 'lib\sub\a.pas')));
  Assert.IsFalse(DirectoryExists(TPath.Combine(folder, 'signatures')), 'signatures/ should not be extracted');
end;

procedure TPackageArchiveExtractorTests.IgnoresUnicodePathField;
var
  archive : string;
  folder : string;
  entry : TRawEntry;
  escaped : string;
begin
  archive := TPath.Combine(FRoot, 'upath.dpkg');
  folder := TPath.Combine(FRoot, 'pkg');
  escaped := TPath.Combine(FRoot, 'escaped.txt');
  entry := RawEntry('lib/a.pas', 'payload');
  entry.LocalExtra := UnicodePathField('lib/a.pas', '../escaped.txt');
  WriteRawArchive(archive, [entry]);

  {$IF CompilerVersion >= 31.0 }
  //10.1 Berlin and later : prove the crafted archive really does escape through the RTL, so the
  //assertion below can't pass vacuously.
  TZipFile.ExtractZipFile(archive, TPath.Combine(FRoot, 'rtl'));
  Assert.IsTrue(FileExists(escaped), 'the crafted archive did not escape through TZipFile.ExtractZipFile - test is vacuous');
  DeleteFile(escaped);
  {$IFEND}

  TPackageArchiveExtractor.Extract(archive, folder);
  Assert.IsFalse(FileExists(escaped), 'the Unicode Path extra field moved the entry out of the package folder');
  Assert.AreEqual('payload', TFile.ReadAllText(TPath.Combine(folder, 'lib\a.pas')));
end;

procedure TPackageArchiveExtractorTests.RefusesEntryOutsideTheFolder;
var
  archive : string;
  folder : string;
begin
  archive := TPath.Combine(FRoot, 'dotdot.dpkg');
  folder := TPath.Combine(FRoot, 'pkg');
  WriteRawArchive(archive, [RawEntry('../escaped.txt', 'payload')]);
  Assert.WillRaise(
    procedure
    begin
      TPackageArchiveExtractor.Extract(archive, folder);
    end, EArchiveExtract);
  Assert.IsFalse(FileExists(TPath.Combine(FRoot, 'escaped.txt')), 'an entry was written outside the package folder');
end;

procedure TArchiveValidatorTests.AcceptsValidMinimalArchive;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('minimal');
  try
    BuildArchive(path, [
      'lib/foo.pas', 'unit Foo; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.AcceptsArchiveWithNestedFolders;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('nested');
  try
    BuildArchive(path, [
      'source/sub1/a.pas', 'unit A; end.',
      'source/sub2/b.pas', 'unit B; end.',
      'tools/run.bat',     '@echo off'
    ]);
    res := validator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.AcceptsManifestAndSignaturesEntries;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('with-mani-sig');
  try
    BuildArchive(path, [
      'lib/foo.pas',              'unit Foo; end.',
      cManifestFileName,          '{}',
      'signatures/author-1.p7s',  'not real - validator only checks format'
    ]);
    res := validator.Validate(path);
    Assert.IsTrue(res.Ok, 'rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.AcceptsAsciiNameWithoutUtf8Flag;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  // V-10: bit 11 is a SHOULD, not a MUST. Pure-ASCII names are unambiguous even
  // with the flag clear, so an archive repacked by a tool that omits it (the
  // sivv.* packages downloaded from the gallery) must still be accepted - this
  // matches the server verifier, which treats bit 11 as a hint.
  validator := NewValidator;
  path := MakeTempPath('ascii-noflag');
  try
    BuildArchiveNoUtf8Flag(path, [
      'README.md',   '# readme',
      'src/foo.pas', 'unit Foo; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsTrue(res.Ok, 'ASCII name without UTF-8 flag was rejected: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsNonAsciiNameWithoutUtf8Flag;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  // A non-ASCII name with bit 11 clear is genuinely ambiguous (the codepage is
  // unknown), so it must still be rejected.
  validator := NewValidator;
  path := MakeTempPath('nonascii-noflag');
  try
    BuildArchiveNoUtf8Flag(path, [
      'src/caf'#$00E9'.pas', 'unit Cafe; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'non-ASCII name without UTF-8 flag was accepted');
    Assert.IsTrue(Pos('non-ASCII', res.Reason) > 0, 'unexpected reason: ' + res.Reason);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsDuplicateEntryNames;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('dup');
  try
    // System.Zip happily lets us write two entries with the same name —
    // this is precisely the V-9 attack we reject.
    BuildArchive(path, [
      'foo.pas', 'unit Foo; end.',
      'foo.pas', 'unit FooHax; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'duplicate entry name was accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsCaseCollisionDuplicates;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('case');
  try
    BuildArchive(path, [
      'Foo.pas', 'unit Foo; end.',
      'foo.pas', 'unit FOO; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'case-colliding duplicate accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsAdsColonInName;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('ads');
  try
    BuildArchive(path, [
      'lib/foo:stream', 'ads payload'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'NTFS ADS path accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsReservedDeviceName;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('reserved');
  try
    BuildArchive(path, [
      'CON.pas', 'unit Con; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'Windows reserved device name accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsBackslashInName;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('backslash');
  try
    // Path-safety rejects backslash separators — must use forward slashes only.
    BuildArchive(path, [
      'lib\foo.pas', 'unit Foo; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'backslash separator accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsDotDotPath;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('dotdot');
  try
    BuildArchive(path, [
      '../escape.pas', 'unit Escape; end.'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'parent-directory escape accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TArchiveValidatorTests.RejectsTrailingDotSegment;
var
  validator : IArchiveValidator;
  path : string;
  res : TArchiveValidationResult;
begin
  validator := NewValidator;
  path := MakeTempPath('trailingdot');
  try
    BuildArchive(path, [
      'foo.pas.', 'content'
    ]);
    res := validator.Validate(path);
    Assert.IsFalse(res.Ok, 'trailing-dot segment accepted');
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TArchiveValidatorTests);
  TDUnitX.RegisterTestFixture(TPackageArchiveExtractorTests);

end.
