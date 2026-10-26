unit DPM.Core.Tests.Utils.Files;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFileUtilsTests = class
  private
    FRoot : string;
    function CopyOfVersionedExe(const name : string) : string;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure SameFileVersion_Equal;
    [Test] procedure SameFileVersion_DiffersInAnyPart;
    [Test] procedure AreSameFiles_IdenticalCopies;
    [Test] procedure AreSameFiles_SameSizeDifferentFileVersion;
    //a dll without version info, data files : what is in them decides.
    [Test] procedure AreSameFiles_NoVersionInfo_SameContent;
    [Test] procedure AreSameFiles_NoVersionInfo_SameSizeDifferentContent;

    [Test] procedure AtomicWriteAllText_CreatesFile;
    [Test] procedure AtomicWriteAllText_ReplacesExisting;
    [Test] procedure AtomicWriteAllText_LockedTargetLeavesOriginalAndNoTemp;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  DPM.Core.Utils.Files;

const
  cFixedFileInfoSignature : Cardinal = $FEEF04BD;

function MakeFixedInfo(major, minor, release, build : Word) : TVSFixedFileInfo;
begin
  FillChar(result, SizeOf(result), 0);
  result.dwSignature := cFixedFileInfoSignature;
  result.dwFileVersionMS := (Cardinal(major) shl 16) or minor;
  result.dwFileVersionLS := (Cardinal(release) shl 16) or build;
end;

// Finds the VS_FIXEDFILEINFO in a PE's version resource by its signature dword.
function FindFixedFileInfo(const bytes : TBytes) : integer;
var
  i : integer;
  value : Cardinal;
begin
  result := -1;
  for i := 0 to Length(bytes) - SizeOf(TVSFixedFileInfo) do
  begin
    Move(bytes[i], value, SizeOf(value));
    if value = cFixedFileInfoSignature then
      exit(i);
  end;
end;

{ TFileUtilsTests }

procedure TFileUtilsTests.Setup;
begin
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-fileutils-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(FRoot);
end;

procedure TFileUtilsTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
end;

function TFileUtilsTests.CopyOfVersionedExe(const name : string) : string;
var
  systemDir : array[0..MAX_PATH] of Char;
begin
  //a copy - version queries on the System32 original can be shimmed for unmanifested processes.
  GetSystemDirectory(systemDir, MAX_PATH);
  result := TPath.Combine(FRoot, name);
  TFile.Copy(IncludeTrailingPathDelimiter(systemDir) + 'cmd.exe', result);
end;

procedure TFileUtilsTests.SameFileVersion_Equal;
begin
  Assert.IsTrue(TFileUtils.SameFileVersion(MakeFixedInfo(1, 2, 3, 4), MakeFixedInfo(1, 2, 3, 4)));
end;

procedure TFileUtilsTests.SameFileVersion_DiffersInAnyPart;
begin
  Assert.IsFalse(TFileUtils.SameFileVersion(MakeFixedInfo(1, 2, 3, 4), MakeFixedInfo(2, 2, 3, 4)), 'major');
  Assert.IsFalse(TFileUtils.SameFileVersion(MakeFixedInfo(1, 2, 3, 4), MakeFixedInfo(1, 3, 3, 4)), 'minor');
  Assert.IsFalse(TFileUtils.SameFileVersion(MakeFixedInfo(1, 2, 3, 4), MakeFixedInfo(1, 2, 4, 4)), 'release');
  Assert.IsFalse(TFileUtils.SameFileVersion(MakeFixedInfo(1, 2, 3, 4), MakeFixedInfo(1, 2, 3, 5)), 'build');
end;

procedure TFileUtilsTests.AreSameFiles_IdenticalCopies;
var
  fileA, fileB : string;
begin
  fileA := CopyOfVersionedExe('a.exe');
  fileB := CopyOfVersionedExe('b.exe');
  Assert.IsTrue(TFileUtils.AreSameFiles(fileA, fileB));
end;

procedure TFileUtilsTests.AreSameFiles_SameSizeDifferentFileVersion;
var
  fileA, fileB : string;
  bytes : TBytes;
  offset : integer;
  info : TVSFixedFileInfo;
begin
  //what copy-local sees after a patch upgrade : same size, same size of version info, new file version.
  fileA := CopyOfVersionedExe('a.exe');
  fileB := CopyOfVersionedExe('b.exe');
  bytes := TFile.ReadAllBytes(fileB);
  offset := FindFixedFileInfo(bytes);
  Assert.IsTrue(offset >= 0, 'no VS_FIXEDFILEINFO found in the test executable');
  Move(bytes[offset], info, SizeOf(info));
  info.dwFileVersionLS := info.dwFileVersionLS + 1;
  Move(info, bytes[offset], SizeOf(info));
  TFile.WriteAllBytes(fileB, bytes);

  Assert.AreEqual(TFileUtils.GetSize(fileA), TFileUtils.GetSize(fileB), 'the copies should still be the same size');
  Assert.IsFalse(TFileUtils.AreSameFiles(fileA, fileB), 'files with different file versions were reported as the same');
end;

procedure TFileUtilsTests.AreSameFiles_NoVersionInfo_SameContent;
var
  fileA, fileB : string;
  bytes : TBytes;
  i : integer;
begin
  //bigger than one read buffer, so the compare is not just of the first chunk.
  SetLength(bytes, 200000);
  for i := 0 to High(bytes) do
    bytes[i] := Byte(i mod 251);
  fileA := TPath.Combine(FRoot, 'a.dat');
  fileB := TPath.Combine(FRoot, 'b.dat');
  TFile.WriteAllBytes(fileA, bytes);
  TFile.WriteAllBytes(fileB, bytes);
  //copying again over a file another process has loaded fails - an identical copy is the same file.
  Assert.IsTrue(TFileUtils.AreSameFiles(fileA, fileB));
end;

procedure TFileUtilsTests.AreSameFiles_NoVersionInfo_SameSizeDifferentContent;
var
  fileA, fileB : string;
  bytes : TBytes;
  i : integer;
begin
  SetLength(bytes, 200000);
  for i := 0 to High(bytes) do
    bytes[i] := Byte(i mod 251);
  fileA := TPath.Combine(FRoot, 'a.dat');
  fileB := TPath.Combine(FRoot, 'b.dat');
  TFile.WriteAllBytes(fileA, bytes);
  //the difference is past the first read buffer.
  bytes[150000] := bytes[150000] xor $FF;
  TFile.WriteAllBytes(fileB, bytes);
  Assert.IsFalse(TFileUtils.AreSameFiles(fileA, fileB));
end;

procedure TFileUtilsTests.AtomicWriteAllText_CreatesFile;
var
  target : string;
begin
  target := TPath.Combine(FRoot, 'new.txt');
  TFileUtils.AtomicWriteAllText(target, 'hello', TEncoding.UTF8);
  Assert.AreEqual('hello', TFile.ReadAllText(target, TEncoding.UTF8));
end;

procedure TFileUtilsTests.AtomicWriteAllText_ReplacesExisting;
var
  target : string;
  files : TStringDynArray;
begin
  target := TPath.Combine(FRoot, 'existing.txt');
  TFile.WriteAllText(target, 'old', TEncoding.UTF8);
  TFileUtils.AtomicWriteAllText(target, 'new', TEncoding.UTF8);
  Assert.AreEqual('new', TFile.ReadAllText(target, TEncoding.UTF8));
  files := TDirectory.GetFiles(FRoot);
  Assert.AreEqual(1, Length(files), 'a temporary file was left behind');
end;

procedure TFileUtilsTests.AtomicWriteAllText_LockedTargetLeavesOriginalAndNoTemp;
var
  target : string;
  locker : TFileStream;
  files : TStringDynArray;
begin
  target := TPath.Combine(FRoot, 'locked.txt');
  TFile.WriteAllText(target, 'original', TEncoding.UTF8);
  //no FILE_SHARE_DELETE, so MoveFileEx can't replace it.
  locker := TFileStream.Create(target, fmOpenRead or fmShareDenyWrite);
  try
    Assert.WillRaise(
      procedure
      begin
        TFileUtils.AtomicWriteAllText(target, 'replacement', TEncoding.UTF8);
      end, EInOutError);
  finally
    locker.Free;
  end;
  Assert.AreEqual('original', TFile.ReadAllText(target, TEncoding.UTF8));
  files := TDirectory.GetFiles(FRoot);
  Assert.AreEqual(1, Length(files), 'the temporary file was not removed after the failure');
end;

initialization
  TDUnitX.RegisterTestFixture(TFileUtilsTests);

end.
