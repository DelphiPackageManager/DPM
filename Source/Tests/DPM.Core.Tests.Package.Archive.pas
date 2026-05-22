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

    [Test] procedure RejectsDuplicateEntryNames;
    [Test] procedure RejectsCaseCollisionDuplicates;
    [Test] procedure RejectsAdsColonInName;
    [Test] procedure RejectsReservedDeviceName;
    [Test] procedure RejectsBackslashInName;
    [Test] procedure RejectsDotDotPath;
    [Test] procedure RejectsTrailingDotSegment;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
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

{ TArchiveValidatorTests }

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

end.
