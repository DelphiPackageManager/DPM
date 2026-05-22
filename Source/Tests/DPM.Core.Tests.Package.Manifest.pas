unit DPM.Core.Tests.Package.Manifest;

// Phase 1 §1.12 — manifest emitter (M-1..M-12) and parser (V-2).

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TManifestTests = class
  private
    procedure MakeTempPackage(out root : string; const fileMap : array of string);
    procedure WriteFile(const path, content : string);
  public
    [Test]
    procedure Generate_ProducesByteStableOutputAcrossTwoRuns;
    [Test]
    procedure Generate_SortsFilesLexicographicallyByPath;
    [Test]
    procedure Generate_ExcludesManifestAndSignaturesFolder;

    [Test]
    [TestCase('AbsolutePath',  '/etc/passwd')]
    [TestCase('DriveLetter',   'C:foo')]
    [TestCase('DotDot',        '../etc')]
    [TestCase('BackSlash',     'a\b\c')]
    [TestCase('ReservedDevice','CON.pas')]
    [TestCase('TrailingDot',   'Foo.pas.')]
    [TestCase('LeadingSpace',  ' Foo.pas')]
    [TestCase('TrailingSpace', 'Foo.pas ')]
    [TestCase('AdsColon',      'Foo:stream')]
    [TestCase('ControlChar',   'Foo'#9'.pas')]
    procedure ValidatePath_RejectsM8Violations(const path : string);

    [Test]
    procedure ValidatePath_AcceptsValidPaths;

    [Test]
    procedure Parse_RejectsDuplicateKeys;

    [Test]
    procedure Parse_RejectsOversizeInput;

    [Test]
    procedure Parse_RejectsUnsupportedHashAlgorithm;

    [Test]
    procedure Parse_RoundTripsGeneratedManifest;

    [Test]
    procedure Parse_RejectsEmptyInput;
    [Test]
    procedure Parse_RejectsNonObjectRoot;
    [Test]
    procedure Parse_RejectsMissingHashAlgorithm;
    [Test]
    procedure Parse_RejectsMissingFiles;
    [Test]
    procedure Parse_RejectsFilesEntryWithWrongType;
    [Test]
    procedure Parse_RejectsDuplicatePathInFiles;
    [Test]
    procedure Parse_AcceptsEmptyFilesArray;

    [Test]
    procedure Generate_HashAlgorithmRoundTrip_Sha384;
    [Test]
    procedure Generate_HashAlgorithmRoundTrip_Sha512;
    [Test]
    procedure Generate_EmptyPackage_ProducesValidManifest;
    [Test]
    procedure Generate_RejectsUnknownAlgorithm;
    [Test]
    procedure Generate_RejectsMissingRoot;

    [Test]
    procedure GenerateFromArchive_ProducesSameManifestAsGenerateFromFolder;
    [Test]
    procedure InjectIntoArchive_AddsManifestEntry;
    [Test]
    procedure EmitOutput_UTF8_NoBom;
    [Test]
    procedure EmitOutput_LineEndings_LF;
    [Test]
    procedure EmitOutput_KeysAreLexicographicallyOrdered;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Manifest;

function MakeManifestService : IManifestService;
var
  hashing : IHashingService;
begin
  hashing := TBCryptHashingService.Create;
  result := TManifestService.Create(hashing);
end;

procedure TManifestTests.WriteFile(const path, content : string);
var
  dir : string;
begin
  dir := ExtractFilePath(path);
  if (dir <> '') and not DirectoryExists(dir) then
    ForceDirectories(dir);
  TFile.WriteAllText(path, content, TEncoding.UTF8);
end;

procedure TManifestTests.MakeTempPackage(out root : string; const fileMap : array of string);
var
  i : integer;
begin
  root := TPath.Combine(TPath.GetTempPath, 'dpm-manifest-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(root);
  // fileMap is alternating: relative path, content, relative path, content...
  i := 0;
  while i < Length(fileMap) do
  begin
    WriteFile(TPath.Combine(root, StringReplace(fileMap[i], '/', PathDelim, [rfReplaceAll])), fileMap[i + 1]);
    Inc(i, 2);
  end;
end;

procedure TManifestTests.Generate_ProducesByteStableOutputAcrossTwoRuns;
var
  service : IManifestService;
  root : string;
  m1, m2 : IPackageManifest;
  b1, b2 : TBytes;
begin
  service := MakeManifestService;
  MakeTempPackage(root, [
    'lib/foo.pas', 'unit Foo;'#10'end.',
    'source/bar.pas', 'unit Bar;'#10'end.',
    'package.dspec.yaml', 'id: Test'#10
  ]);
  try
    m1 := service.Generate(root, 'Test', '1.0.0', haSha256);
    m2 := service.Generate(root, 'Test', '1.0.0', haSha256);
    b1 := m1.RawBytes;
    b2 := m2.RawBytes;
    Assert.IsTrue(BytesEqual(b1, b2), 'manifest bytes differ across runs');
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_SortsFilesLexicographicallyByPath;
var
  service : IManifestService;
  root : string;
  manifest : IPackageManifest;
  i : integer;
begin
  service := MakeManifestService;
  MakeTempPackage(root, [
    'zz.pas', 'unit ZZ; end.',
    'aa.pas', 'unit AA; end.',
    'mm.pas', 'unit MM; end.'
  ]);
  try
    manifest := service.Generate(root, 'Test', '1.0.0', haSha256);
    Assert.IsTrue(Length(manifest.Files) >= 3);
    for i := 1 to High(manifest.Files) do
      Assert.IsTrue(CompareStr(manifest.Files[i - 1].Path, manifest.Files[i].Path) < 0,
        Format('Files not sorted at index %d: "%s" vs "%s"',
          [i, manifest.Files[i - 1].Path, manifest.Files[i].Path]));
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_ExcludesManifestAndSignaturesFolder;
var
  service : IManifestService;
  root : string;
  manifest : IPackageManifest;
  i : integer;
begin
  service := MakeManifestService;
  MakeTempPackage(root, [
    'lib/foo.pas', 'unit Foo; end.',
    'dpm-manifest.json', '{}',
    'signatures/author-1.p7s', 'not a real signature'
  ]);
  try
    manifest := service.Generate(root, 'Test', '1.0.0', haSha256);
    for i := 0 to High(manifest.Files) do
    begin
      Assert.AreNotEqual('dpm-manifest.json', manifest.Files[i].Path);
      Assert.IsFalse(manifest.Files[i].Path.StartsWith('signatures/'),
        'signatures/ entry leaked into manifest: ' + manifest.Files[i].Path);
    end;
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.ValidatePath_RejectsM8Violations(const path : string);
var
  service : IManifestService;
  reason : string;
begin
  service := MakeManifestService;
  Assert.IsFalse(service.ValidatePath(path, reason),
    'expected "' + path + '" to be rejected');
  Assert.IsTrue(reason <> '', 'rejection reason was empty');
end;

procedure TManifestTests.ValidatePath_AcceptsValidPaths;
var
  service : IManifestService;
  reason : string;
begin
  service := MakeManifestService;
  Assert.IsTrue(service.ValidatePath('lib/foo.pas', reason), reason);
  Assert.IsTrue(service.ValidatePath('source/sub/bar.pas', reason), reason);
  Assert.IsTrue(service.ValidatePath('a', reason), reason);
end;

procedure TManifestTests.Parse_RejectsDuplicateKeys;
const
  cBad =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1, "packageId": "X",' +
    ' "packageId": "Y", "version": "1.0.0", "hashAlgorithm": "SHA256",' +
    ' "files": [] }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cBad)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsOversizeInput;
var
  service : IManifestService;
  oversize : TBytes;
begin
  service := MakeManifestService;
  SetLength(oversize, cManifestMaxBytes + 1);
  FillChar(oversize[0], Length(oversize), Ord('a'));
  Assert.WillRaise(
    procedure begin service.Parse(oversize); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsUnsupportedHashAlgorithm;
const
  cBad =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1, "packageId": "X",' +
    ' "version": "1.0.0", "hashAlgorithm": "SHA1", "files": [] }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cBad)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RoundTripsGeneratedManifest;
var
  service : IManifestService;
  root : string;
  generated, parsed : IPackageManifest;
begin
  service := MakeManifestService;
  MakeTempPackage(root, [
    'a.pas', 'unit A; end.'
  ]);
  try
    generated := service.Generate(root, 'Test', '1.0.0', haSha256);
    parsed := service.Parse(generated.RawBytes);
    Assert.AreEqual(generated.PackageId, parsed.PackageId);
    Assert.AreEqual(generated.Version, parsed.Version);
    Assert.AreEqual(Length(generated.Files), Length(parsed.Files));
    if Length(generated.Files) > 0 then
    begin
      Assert.AreEqual(generated.Files[0].Path, parsed.Files[0].Path);
      Assert.AreEqual(generated.Files[0].Size, parsed.Files[0].Size);
      Assert.IsTrue(BytesEqual(generated.Files[0].Hash, parsed.Files[0].Hash));
    end;
  finally
    TDirectory.Delete(root, true);
  end;
end;

// ---------------------------------------------------------------------------
// Additional parser failure-mode tests
// ---------------------------------------------------------------------------

procedure TManifestTests.Parse_RejectsEmptyInput;
var
  service : IManifestService;
  empty : TBytes;
begin
  service := MakeManifestService;
  SetLength(empty, 0);
  Assert.WillRaise(
    procedure begin service.Parse(empty); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsNonObjectRoot;
const
  cArrayRoot = '[1, 2, 3]';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cArrayRoot)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsMissingHashAlgorithm;
const
  cNoAlg =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1,' +
    ' "packageId": "X", "version": "1.0.0", "files": [] }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cNoAlg)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsMissingFiles;
const
  cNoFiles =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1,' +
    ' "packageId": "X", "version": "1.0.0", "hashAlgorithm": "SHA256" }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cNoFiles)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsFilesEntryWithWrongType;
const
  // size as a string, not an integer
  cBad =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1, "packageId": "X",' +
    ' "version": "1.0.0", "hashAlgorithm": "SHA256",' +
    ' "files": [ { "path": "a.pas", "size": "not-a-number", "hash": "ab" } ] }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cBad)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_RejectsDuplicatePathInFiles;
const
  cBad =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1, "packageId": "X",' +
    ' "version": "1.0.0", "hashAlgorithm": "SHA256",' +
    ' "files": [ { "path": "a.pas", "size": 0, "hash": "ab" },' +
    '            { "path": "a.pas", "size": 1, "hash": "cd" } ] }';
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Parse(TEncoding.UTF8.GetBytes(cBad)); end,
    EManifestParse);
end;

procedure TManifestTests.Parse_AcceptsEmptyFilesArray;
const
  cEmptyFiles =
    '{ "dpmPackageFormat": 1, "manifestSchemaVersion": 1, "packageId": "X",' +
    ' "version": "1.0.0", "hashAlgorithm": "SHA256", "files": [] }';
var
  service : IManifestService;
  m : IPackageManifest;
begin
  service := MakeManifestService;
  m := service.Parse(TEncoding.UTF8.GetBytes(cEmptyFiles));
  Assert.IsNotNull(m);
  Assert.AreEqual(0, Length(m.Files));
end;

// ---------------------------------------------------------------------------
// Generate edge cases
// ---------------------------------------------------------------------------

procedure TManifestTests.Generate_HashAlgorithmRoundTrip_Sha384;
var
  service : IManifestService;
  root : string;
  m, parsed : IPackageManifest;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['a.pas', 'unit A; end.']);
  try
    m := service.Generate(root, 'Test', '1.0.0', haSha384);
    Assert.AreEqual(Ord(haSha384), Ord(m.HashAlgorithm));
    Assert.AreEqual(48, Length(m.Files[0].Hash));
    parsed := service.Parse(m.RawBytes);
    Assert.AreEqual(Ord(haSha384), Ord(parsed.HashAlgorithm));
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_HashAlgorithmRoundTrip_Sha512;
var
  service : IManifestService;
  root : string;
  m, parsed : IPackageManifest;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['a.pas', 'unit A; end.']);
  try
    m := service.Generate(root, 'Test', '1.0.0', haSha512);
    Assert.AreEqual(Ord(haSha512), Ord(m.HashAlgorithm));
    Assert.AreEqual(64, Length(m.Files[0].Hash));
    parsed := service.Parse(m.RawBytes);
    Assert.AreEqual(Ord(haSha512), Ord(parsed.HashAlgorithm));
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_EmptyPackage_ProducesValidManifest;
var
  service : IManifestService;
  root : string;
  m : IPackageManifest;
begin
  service := MakeManifestService;
  root := TPath.Combine(TPath.GetTempPath,
    'dpm-manifest-empty-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(root);
  try
    m := service.Generate(root, 'Empty.Pkg', '1.0.0', haSha256);
    Assert.IsNotNull(m);
    Assert.AreEqual('Empty.Pkg', m.PackageId);
    Assert.AreEqual(0, Length(m.Files));
    Assert.IsTrue(Length(m.RawBytes) > 0);
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_RejectsUnknownAlgorithm;
var
  service : IManifestService;
  root : string;
begin
  service := MakeManifestService;
  root := TPath.Combine(TPath.GetTempPath,
    'dpm-manifest-bad-alg-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(root);
  try
    Assert.WillRaise(
      procedure begin service.Generate(root, 'X', '1.0.0', haUnknown); end,
      EManifest);
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.Generate_RejectsMissingRoot;
var
  service : IManifestService;
begin
  service := MakeManifestService;
  Assert.WillRaise(
    procedure begin service.Generate('C:\does-not-exist-' + FormatDateTime('hhnnsszzz', Now),
                                     'X', '1.0.0', haSha256); end,
    EManifest);
end;

// ---------------------------------------------------------------------------
// Archive helpers
// ---------------------------------------------------------------------------

procedure TManifestTests.GenerateFromArchive_ProducesSameManifestAsGenerateFromFolder;
var
  service : IManifestService;
  root : string;
  archivePath : string;
  fromFolder, fromArchive : IPackageManifest;
  zip : TZipFile;
begin
  service := MakeManifestService;
  MakeTempPackage(root, [
    'lib/a.pas', 'unit A; end.',
    'lib/b.pas', 'unit B; end.',
    'tools/run.bat', '@echo off'
  ]);
  try
    fromFolder := service.Generate(root, 'Same.Pkg', '1.0.0', haSha256);

    archivePath := root + '.dpkg';
    zip := TZipFile.Create;
    try
      zip.Open(archivePath, zmWrite);
      zip.Add(TPath.Combine(root, 'lib\a.pas'), 'lib/a.pas');
      zip.Add(TPath.Combine(root, 'lib\b.pas'), 'lib/b.pas');
      zip.Add(TPath.Combine(root, 'tools\run.bat'), 'tools/run.bat');
    finally
      zip.Free;
    end;
    try
      fromArchive := service.GenerateFromArchive(archivePath, 'Same.Pkg', '1.0.0', haSha256);
      Assert.AreEqual(Length(fromFolder.Files), Length(fromArchive.Files));
      // Per-entry hash equality is the strong assertion — same content, same hash.
      Assert.IsTrue(BytesEqual(fromFolder.Files[0].Hash, fromArchive.Files[0].Hash));
    finally
      if FileExists(archivePath) then
        DeleteFile(archivePath);
    end;
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.InjectIntoArchive_AddsManifestEntry;
var
  service : IManifestService;
  root : string;
  archivePath : string;
  m : IPackageManifest;
  zip : TZipFile;
  i : integer;
  found : boolean;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['x.pas', 'unit X; end.']);
  try
    archivePath := root + '.dpkg';
    zip := TZipFile.Create;
    try
      zip.Open(archivePath, zmWrite);
      zip.Add(TPath.Combine(root, 'x.pas'), 'x.pas');
    finally
      zip.Free;
    end;
    try
      m := service.GenerateFromArchive(archivePath, 'Inj.Pkg', '1.0.0', haSha256);
      service.InjectIntoArchive(archivePath, m);

      zip := TZipFile.Create;
      try
        zip.Open(archivePath, zmRead);
        found := false;
        for i := 0 to zip.FileCount - 1 do
          if SameText(zip.FileName[i], cManifestFileName) then
          begin
            found := true;
            Break;
          end;
        Assert.IsTrue(found, 'dpm-manifest.json missing after InjectIntoArchive');
      finally
        zip.Free;
      end;
    finally
      if FileExists(archivePath) then
        DeleteFile(archivePath);
    end;
  finally
    TDirectory.Delete(root, true);
  end;
end;

// ---------------------------------------------------------------------------
// Emit-format guarantees (M-1, M-2)
// ---------------------------------------------------------------------------

procedure TManifestTests.EmitOutput_UTF8_NoBom;
var
  service : IManifestService;
  root : string;
  m : IPackageManifest;
  bytes : TBytes;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['a.pas', 'unit A; end.']);
  try
    m := service.Generate(root, 'Bom.Pkg', '1.0.0', haSha256);
    bytes := m.RawBytes;
    Assert.IsTrue(Length(bytes) > 3, 'manifest too short');
    // UTF-8 BOM would be EF BB BF as the leading three bytes.
    Assert.IsFalse((bytes[0] = $EF) and (bytes[1] = $BB) and (bytes[2] = $BF),
      'manifest must not start with a UTF-8 BOM');
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.EmitOutput_LineEndings_LF;
var
  service : IManifestService;
  root : string;
  m : IPackageManifest;
  bytes : TBytes;
  i : integer;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['a.pas', 'unit A; end.']);
  try
    m := service.Generate(root, 'Lf.Pkg', '1.0.0', haSha256);
    bytes := m.RawBytes;
    for i := 0 to Length(bytes) - 1 do
      Assert.AreNotEqual<Byte>(13, bytes[i],
        Format('CR byte at offset %d — manifest must use LF only', [i]));
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TManifestTests.EmitOutput_KeysAreLexicographicallyOrdered;
const
  cExpectedOrder : array[0..6] of string = (
    '"created"', '"dpmPackageFormat"', '"files"', '"hashAlgorithm"',
    '"manifestSchemaVersion"', '"packageId"', '"version"');
var
  service : IManifestService;
  root : string;
  text : string;
  prev, p : integer;
  i : integer;
begin
  service := MakeManifestService;
  MakeTempPackage(root, ['a.pas', 'unit A; end.']);
  try
    text := TEncoding.UTF8.GetString(
      service.Generate(root, 'Lex.Pkg', '1.0.0', haSha256).RawBytes);
    prev := 0;
    for i := Low(cExpectedOrder) to High(cExpectedOrder) do
    begin
      p := Pos(cExpectedOrder[i], text);
      Assert.IsTrue(p > 0,
        'manifest is missing top-level key ' + cExpectedOrder[i]);
      Assert.IsTrue(p > prev,
        Format('key %s appears out of lexicographic order', [cExpectedOrder[i]]));
      prev := p;
    end;
  finally
    TDirectory.Delete(root, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TManifestTests);

end.
