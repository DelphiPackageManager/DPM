unit DPM.Core.Tests.Repository.Directory.Index;

// The index of what a folder source's package sidecars held (DPM.Core.Repository.Directory.Index) : what a listing
// records, when an entry is used, and that the index file is only ever a cache - anything wrong with it means
// reading the share again, never a failure.

interface

uses
  DUnitX.TestFramework,
  DPM.Core.Types,
  DPM.Core.Repository.Directory.Index;

type
  [TestFixture]
  TFolderIndexTests = class
  private
    FRoot : string;
    FSource : string;
    FIndexFolder : string;
    function IndexFile : string;
    function NewIndex : IFolderSourceIndex;
    function WriteSourceFile(const name : string; const text : string) : string;
    function SomeStamps : TPackageFileStamps;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    //the order the folder gives them in - the order the feed found them in before there was an index.
    [Test]
    procedure Listing_Has_The_Packages_In_The_Order_The_Folder_Lists_Them;
    [Test]
    procedure Listing_Records_The_Package_And_Its_Sidecars;
    //a share that can't be reached says nothing about which packages have gone.
    [Test]
    procedure Listing_Of_A_Missing_Folder_Is_Incomplete;
    [Test]
    procedure Listing_That_Matches_Nothing_Is_Complete;
    [Test]
    procedure Listing_With_Two_Patterns_Has_Each_File_Once;

    [Test]
    procedure Entry_Is_Used_When_Nothing_Changed;
    [Test]
    [TestCase('package there', '0')]
    [TestCase('package size', '1')]
    [TestCase('package time', '2')]
    [TestCase('hash file there', '3')]
    [TestCase('hash file size', '4')]
    [TestCase('hash file time', '5')]
    [TestCase('spec there', '6')]
    [TestCase('spec size', '7')]
    [TestCase('spec time', '8')]
    procedure Entry_Is_Not_Used_When_Any_Stamp_Differs(const which : integer);

    [Test]
    procedure Save_And_Load_Keep_The_Text_Exactly;
    [Test]
    [TestCase('no file', '0')]
    [TestCase('empty', '1')]
    [TestCase('not json', '2')]
    [TestCase('an array', '3')]
    [TestCase('cut short', '4')]
    [TestCase('another format', '5')]
    [TestCase('another folder', '6')]
    [TestCase('another compiler', '7')]
    [TestCase('no packages', '8')]
    procedure An_Unusable_Index_File_Is_Ignored(const which : integer);
    [Test]
    procedure An_Index_File_Too_Big_To_Be_One_Is_Ignored;
    [Test]
    procedure Unreadable_Entries_Are_Dropped_And_The_Rest_Kept;

    [Test]
    procedure Save_Does_Not_Raise_When_The_Folder_Cannot_Be_Made;
    [Test]
    procedure Save_Does_Not_Raise_When_The_File_Is_Held_Open;
    [Test]
    procedure Save_Clears_Old_Leftovers_Of_Killed_Saves;
    [Test]
    procedure Save_Writes_Only_When_Something_Changed;

    [Test]
    procedure Prune_Drops_Packages_No_Longer_Listed;
    [Test]
    procedure Prune_Does_Nothing_For_An_Incomplete_Listing;

    [Test]
    procedure Concurrent_Use_Is_Safe;

    [Test]
    procedure Entries_Not_Used_For_60_Days_Are_Dropped_When_Saving;
    [Test]
    procedure Beyond_The_Most_Entries_The_Least_Recently_Used_Go;
    //a search uses the same entries all day - it mustn't rewrite the file each time.
    [Test]
    procedure Using_An_Entry_Again_The_Same_Day_Is_Not_Saved;
    [Test]
    procedure An_Entry_Without_A_Used_Day_Counts_As_Used_Today;

    [Test]
    procedure Index_File_Is_Per_Folder_And_Compiler;
    [Test]
    [TestCase('off', 'off,false')]
    [TestCase('OFF', 'OFF,false')]
    [TestCase('zero', '0,false')]
    [TestCase('false', 'false,false')]
    [TestCase('on', 'on,true')]
    [TestCase('one', '1,true')]
    [TestCase('unset', ',true')]
    procedure Environment_Variable_Turns_The_Index_Off(const value : string; const enabled : boolean);
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.StrUtils,
  System.Types,
  System.Classes,
  System.IOUtils,
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Utils.Directory,
  TestLogger;

const
  cPackageA = 'Alpha.Pkg-delphi12.0-0000000000000001-1.0.0.dpkg';
  cPackageB = 'Beta.Pkg-delphi12.0-0000000000000001-1.0.0.dpkg';

function MakeStamp(const present : boolean; const size : Int64; const lastWrite : Int64) : TFileStamp;
begin
  result.Present := present;
  result.Size := size;
  result.LastWrite := lastWrite;
end;

//the package and both sidecars as one stamp.
function MakeStampsFor(const stamp : TFileStamp) : TPackageFileStamps;
begin
  result.Package := stamp;
  result.Hash := stamp;
  result.Spec := stamp;
end;

{ TFolderIndexTests }

procedure TFolderIndexTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-folder-index-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  FSource := TPath.Combine(FRoot, 'feed');
  FIndexFolder := TPath.Combine(FRoot, 'index');
  ForceDirectories(FSource);
end;

procedure TFolderIndexTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
end;

function TFolderIndexTests.IndexFile : string;
begin
  result := FolderIndexFileName(FIndexFolder, FSource, TCompilerVersion.Delphi12_0);
end;

function TFolderIndexTests.NewIndex : IFolderSourceIndex;
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  result := TFolderSourceIndex.Create(logger, IndexFile, FSource, TCompilerVersion.Delphi12_0);
end;

function TFolderIndexTests.WriteSourceFile(const name : string; const text : string) : string;
begin
  result := TPath.Combine(FSource, name);
  TFile.WriteAllText(result, text, TEncoding.ASCII);
end;

function TFolderIndexTests.SomeStamps : TPackageFileStamps;
begin
  result.Package := MakeStamp(true, 1000, 132000000000000000);
  result.Hash := MakeStamp(true, 64, 132000000000000001);
  result.Spec := MakeStamp(false, 0, 0);
end;

procedure TFolderIndexTests.Listing_Has_The_Packages_In_The_Order_The_Folder_Lists_Them;
var
  listing : IFolderListing;
  expected : IList<string>;
  i : integer;
begin
  WriteSourceFile(cPackageB, 'b');
  WriteSourceFile(cPackageA, 'a');
  WriteSourceFile('readme.txt', 'not a package');
  WriteSourceFile(cPackageA + cPackageHashAlgorithmExt, 'hash');
  listing := ListFolder(FSource, ['*'], cPackageFileExt);
  expected := TDirectoryUtils.GetFiles(FSource, '*' + cPackageFileExt);
  Assert.AreEqual(2, expected.Count, 'precondition');
  Assert.AreEqual(expected.Count, listing.PackageFiles.Count);
  for i := 0 to expected.Count - 1 do
    Assert.AreEqual(expected[i], listing.PackageFiles[i], false);
  Assert.IsTrue(listing.Complete);
end;

procedure TFolderIndexTests.Listing_Records_The_Package_And_Its_Sidecars;
var
  packageFile : string;
  first : TPackageFileStamps;
  second : TPackageFileStamps;
begin
  packageFile := WriteSourceFile(cPackageA, 'abc');
  WriteSourceFile(cPackageA + cPackageHashAlgorithmExt, 'hash');
  first := ListFolder(FSource, ['*'], cPackageFileExt).StampsFor(packageFile);
  Assert.IsTrue(first.Package.Present);
  Assert.AreEqual(Int64(3), first.Package.Size);
  Assert.IsTrue(first.Hash.Present);
  Assert.AreEqual(Int64(4), first.Hash.Size);
  Assert.IsFalse(first.Spec.Present, 'there is no spec beside it');

  //only the package changed.
  TFile.SetLastWriteTime(packageFile, Now - 1);
  second := ListFolder(FSource, ['*'], cPackageFileExt).StampsFor(packageFile);
  Assert.IsTrue(first.Package.LastWrite <> second.Package.LastWrite, 'the package''s time');
  Assert.IsTrue(first.Hash.SameAs(second.Hash));
  Assert.IsTrue(first.Spec.SameAs(second.Spec));
  //a name the listing didn't see.
  Assert.IsFalse(ListFolder(FSource, ['*'], cPackageFileExt).StampsFor(TPath.Combine(FSource, cPackageB)).Package.Present);
end;

procedure TFolderIndexTests.Listing_Of_A_Missing_Folder_Is_Incomplete;
var
  listing : IFolderListing;
begin
  listing := ListFolder(TPath.Combine(FRoot, 'not-there'), ['*'], cPackageFileExt);
  Assert.IsFalse(listing.Complete);
  Assert.AreEqual(0, listing.PackageFiles.Count);
end;

procedure TFolderIndexTests.Listing_That_Matches_Nothing_Is_Complete;
var
  listing : IFolderListing;
begin
  WriteSourceFile(cPackageA, 'a');
  listing := ListFolder(FSource, ['Nothing.Like.This-*'], cPackageFileExt);
  Assert.IsTrue(listing.Complete);
  Assert.AreEqual(0, listing.PackageFiles.Count);
end;

procedure TFolderIndexTests.Listing_With_Two_Patterns_Has_Each_File_Once;
var
  listing : IFolderListing;
begin
  WriteSourceFile(cPackageB, 'b');
  WriteSourceFile(cPackageA, 'a');
  listing := ListFolder(FSource, ['Beta*', '*'], cPackageFileExt);
  Assert.AreEqual(2, listing.PackageFiles.Count);
  Assert.AreEqual(cPackageB, ExtractFileName(listing.PackageFiles[0]), false, 'where it was first seen');
  Assert.AreEqual(cPackageA, ExtractFileName(listing.PackageFiles[1]), false);
end;

procedure TFolderIndexTests.Entry_Is_Used_When_Nothing_Changed;
var
  index : IFolderSourceIndex;
  hash : string;
  specText : string;
begin
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  Assert.AreEqual('the hash', hash, false);
  Assert.AreEqual('the spec', specText, false);
  //the name, whatever its case - as the folder treats it.
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, UpperCase(cPackageA)), SomeStamps, hash, specText));
end;

procedure TFolderIndexTests.Entry_Is_Not_Used_When_Any_Stamp_Differs(const which : integer);
var
  index : IFolderSourceIndex;
  stamps : TPackageFileStamps;
  hash : string;
  specText : string;
begin
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  stamps := SomeStamps;
  case which of
    0 : stamps.Package.Present := false;
    1 : stamps.Package.Size := stamps.Package.Size + 1;
    2 : stamps.Package.LastWrite := stamps.Package.LastWrite + 1;
    3 : stamps.Hash.Present := not stamps.Hash.Present;
    4 : stamps.Hash.Size := stamps.Hash.Size + 1;
    5 : stamps.Hash.LastWrite := stamps.Hash.LastWrite + 1;
    6 : stamps.Spec.Present := not stamps.Spec.Present;
    7 : stamps.Spec.Size := stamps.Spec.Size + 1;
    8 : stamps.Spec.LastWrite := stamps.Spec.LastWrite + 1;
  end;
  Assert.IsFalse(index.TryGet(TPath.Combine(FSource, cPackageA), stamps, hash, specText));
  Assert.AreEqual('', specText);
end;

procedure TFolderIndexTests.Save_And_Load_Keep_The_Text_Exactly;
var
  index : IFolderSourceIndex;
  spec : string;
  hash : string;
  specText : string;
begin
  spec := '# a "quoted" ''single'' \back\slash\ path' + #13#10 +
          'description: a: b # not a comment' + #10 +
          #9'a tab, then {braces} and [brackets]' + #13#10 +
          'non-ascii: caf' + #$00E9 + ' ' + #$4E2D + ' ' + #$D83D#$DE00 + #13#10 +
          '\u0041 is not an escape here' + #13#10 +
          StringOfChar('x', 10000);
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'aBc123', spec);
  index.Save;
  Assert.IsTrue(FileExists(IndexFile));

  index := NewIndex;
  Assert.AreEqual(1, index.Count);
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  Assert.AreEqual('aBc123', hash, false);
  Assert.AreEqual(Length(spec), Length(specText));
  Assert.IsTrue(spec = specText, 'the spec text came back changed');
end;

procedure TFolderIndexTests.An_Unusable_Index_File_Is_Ignored(const which : integer);
var
  index : IFolderSourceIndex;
  logger : ILogger;
  text : string;
  hash : string;
  specText : string;

  //a good index file's text, for this folder and compiler unless they are given.
  function GoodIndexText(const folder : string; const compilerVersion : TCompilerVersion) : string;
  var
    other : IFolderSourceIndex;
    fileName : string;
  begin
    fileName := TPath.Combine(FRoot, 'other.json');
    other := TFolderSourceIndex.Create(logger, fileName, folder, compilerVersion);
    other.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
    other.Save;
    result := TFile.ReadAllText(fileName, TEncoding.UTF8);
  end;

begin
  logger := TTestLogger.Create;
  text := '';
  case which of
    1 : text := '';
    2 : text := 'this is not json';
    3 : text := '[]';
    4 :
    begin
      text := GoodIndexText(FSource, TCompilerVersion.Delphi12_0);
      text := Copy(text, 1, Length(text) div 2);
    end;
    5 : text := StringReplace(GoodIndexText(FSource, TCompilerVersion.Delphi12_0), '"format":1', '"format":2', []);
    6 : text := GoodIndexText(TPath.Combine(FRoot, 'another-feed'), TCompilerVersion.Delphi12_0);
    7 : text := GoodIndexText(FSource, TCompilerVersion.Delphi11_0);
    8 : text := '{"format":1,"compiler":"delphi12.0","source":' + '"' + StringReplace(NormaliseFolderIndexSource(FSource), '\', '\\', [rfReplaceAll]) + '"}';
  end;
  if which = 5 then
    Assert.IsTrue(Pos('"format":2', text) > 0, 'precondition');
  if which <> 0 then
  begin
    ForceDirectories(FIndexFolder);
    TFile.WriteAllText(IndexFile, text, TEncoding.UTF8);
  end;

  index := NewIndex;
  Assert.AreEqual(0, index.Count);
  Assert.IsFalse(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  //and it carries on as an index - the file is replaced with a good one.
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  Assert.AreEqual(1, NewIndex.Count);
end;

procedure TFolderIndexTests.An_Index_File_Too_Big_To_Be_One_Is_Ignored;
var
  stream : TFileStream;
begin
  ForceDirectories(FIndexFolder);
  stream := TFileStream.Create(IndexFile, fmCreate);
  try
    stream.Size := cFolderIndexMaxFileSize + 1;
  finally
    stream.Free;
  end;
  Assert.AreEqual(0, NewIndex.Count);
end;

procedure TFolderIndexTests.Unreadable_Entries_Are_Dropped_And_The_Rest_Kept;
var
  root : TJsonObject;
  packages : TJsonObject;
  noHash : TJsonObject;
  index : IFolderSourceIndex;
  hash : string;
  specText : string;

  function AddEntry(const name : string; const packagePresent : boolean; const size : Int64; const spec : string) : TJsonObject;
  var
    stamp : string;
  begin
    result := packages.O[name];
    for stamp in TArray<string>.Create('dpkg', 'sha256', 'dspec') do
    begin
      result.A[stamp].Add(packagePresent);
      result.A[stamp].Add(size);
      result.A[stamp].Add(Int64(132000000000000000));
    end;
    result.S['hash'] := 'h';
    result.S['spec'] := spec;
  end;

begin
  root := TJsonObject.Create;
  try
    root.L['format'] := cFolderIndexFormat;
    root.S['source'] := NormaliseFolderIndexSource(FSource);
    root.S['compiler'] := 'delphi12.0';
    packages := root.O['packages'];
    AddEntry(LowerCase(cPackageA), true, 10, 'good');
    AddEntry('no spec', true, 10, '');
    AddEntry('not listed', false, 10, 'spec');
    AddEntry('negative size', true, -1, 'spec');
    AddEntry('short stamp', true, 10, 'spec').A['dspec'].Delete(2);
    AddEntry('stamp of strings', true, 10, 'spec').A['dpkg'].S[1] := 'ten';
    noHash := AddEntry('no hash', true, 10, 'spec');
    noHash.Delete(noHash.IndexOf('hash'));
    packages.S['not an object'] := 'just text';
    ForceDirectories(FIndexFolder);
    TFile.WriteAllText(IndexFile, root.ToJSON, TEncoding.UTF8);
  finally
    root.Free;
  end;

  index := NewIndex;
  Assert.AreEqual(1, index.Count);
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA),
                             MakeStampsFor(MakeStamp(true, 10, 132000000000000000)), hash, specText));
  Assert.AreEqual('good', specText);
end;

procedure TFolderIndexTests.Save_Does_Not_Raise_When_The_Folder_Cannot_Be_Made;
var
  logger : ILogger;
  blocker : string;
  index : IFolderSourceIndex;
  hash : string;
  specText : string;
begin
  logger := TTestLogger.Create;
  //a file where the index folder would be.
  blocker := TPath.Combine(FRoot, 'blocker');
  TFile.WriteAllText(blocker, 'a file', TEncoding.ASCII);
  index := TFolderSourceIndex.Create(logger, TPath.Combine(blocker, 'index.json'), FSource, TCompilerVersion.Delphi12_0);
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText), 'it still works in memory');
end;

procedure TFolderIndexTests.Save_Does_Not_Raise_When_The_File_Is_Held_Open;
var
  index : IFolderSourceIndex;
  holder : TFileStream;
  before : string;
  leftovers : TStringDynArray;
  hash : string;
  specText : string;
begin
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  before := TFile.ReadAllText(IndexFile, TEncoding.UTF8);

  index.Put(TPath.Combine(FSource, cPackageB), SomeStamps, 'the hash', 'the spec');
  holder := TFileStream.Create(IndexFile, fmOpenRead or fmShareDenyWrite);
  try
    index.Save;
  finally
    holder.Free;
  end;
  Assert.AreEqual(before, TFile.ReadAllText(IndexFile, TEncoding.UTF8), false, 'the file it couldn''t replace is as it was');
  leftovers := TDirectory.GetFiles(FIndexFolder, '*.tmp-*');
  Assert.AreEqual(0, Length(leftovers), 'no part written file left');
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageB), SomeStamps, hash, specText), 'it still works in memory');
end;

procedure TFolderIndexTests.Save_Clears_Old_Leftovers_Of_Killed_Saves;
var
  index : IFolderSourceIndex;
  oldLeftover : string;
  newLeftover : string;
begin
  ForceDirectories(FIndexFolder);
  oldLeftover := IndexFile + '.tmp-old';
  newLeftover := IndexFile + '.tmp-new';
  TFile.WriteAllText(oldLeftover, 'killed', TEncoding.ASCII);
  TFile.SetLastWriteTime(oldLeftover, Now - 2 / 24);
  //another process's save, under way.
  TFile.WriteAllText(newLeftover, 'saving', TEncoding.ASCII);

  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  Assert.IsFalse(FileExists(oldLeftover));
  Assert.IsTrue(FileExists(newLeftover));
end;

procedure TFolderIndexTests.Save_Writes_Only_When_Something_Changed;
var
  index : IFolderSourceIndex;
begin
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  Assert.IsTrue(FileExists(IndexFile));
  System.SysUtils.DeleteFile(IndexFile);
  index.Save;
  Assert.IsFalse(FileExists(IndexFile), 'nothing changed');
  //the same again is no change either.
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  Assert.IsFalse(FileExists(IndexFile), 'the same entry again');
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'another spec');
  index.Save;
  Assert.IsTrue(FileExists(IndexFile));
end;

procedure TFolderIndexTests.Prune_Drops_Packages_No_Longer_Listed;
var
  index : IFolderSourceIndex;
  hash : string;
  specText : string;
begin
  WriteSourceFile(cPackageA, 'a');
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Put(TPath.Combine(FSource, cPackageB), SomeStamps, 'the hash', 'the spec');
  index.Prune(ListFolder(FSource, ['*'], cPackageFileExt));
  Assert.AreEqual(1, index.Count);
  Assert.IsFalse(index.TryGet(TPath.Combine(FSource, cPackageB), SomeStamps, hash, specText));
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
end;

procedure TFolderIndexTests.Prune_Does_Nothing_For_An_Incomplete_Listing;
var
  index : IFolderSourceIndex;
begin
  index := NewIndex;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Put(TPath.Combine(FSource, cPackageB), SomeStamps, 'the hash', 'the spec');
  index.Prune(ListFolder(TPath.Combine(FRoot, 'not-there'), ['*'], cPackageFileExt));
  Assert.AreEqual(2, index.Count);
end;

type
  TIndexWorker = class(TThread)
  private
    FIndex : IFolderSourceIndex;
    FFolder : string;
    FNumber : integer;
    FError : string;
  protected
    procedure Execute; override;
  public
    constructor Create(const index : IFolderSourceIndex; const folder : string; const number : integer);
    property Error : string read FError;
  end;

constructor TIndexWorker.Create(const index : IFolderSourceIndex; const folder : string; const number : integer);
begin
  inherited Create(true);
  FIndex := index;
  FFolder := folder;
  FNumber := number;
end;

procedure TIndexWorker.Execute;
var
  i : integer;
  stamps : TPackageFileStamps;
  packageFile : string;
  hash : string;
  specText : string;
begin
  try
    for i := 1 to 200 do
    begin
      stamps.Package := MakeStamp(true, i mod 20, 132000000000000000 + FNumber);
      stamps.Hash := MakeStamp(true, 64, 1);
      stamps.Spec := MakeStamp(true, 100, 2);
      packageFile := TPath.Combine(FFolder, 'Pkg' + IntToStr(i mod 20) + '-delphi12.0-01-1.0.0.dpkg');
      FIndex.Put(packageFile, stamps, 'hash ' + IntToStr(FNumber), 'spec ' + IntToStr(i));
      FIndex.TryGet(packageFile, stamps, hash, specText);
      if i mod 7 = 0 then
        FIndex.Remove(packageFile);
      if i mod 25 = 0 then
        FIndex.Save;
    end;
  except
    on e : Exception do
      FError := e.ClassName + ' : ' + e.Message;
  end;
end;

procedure TFolderIndexTests.Concurrent_Use_Is_Safe;
var
  index : IFolderSourceIndex;
  workers : array[0..7] of TIndexWorker;
  i : integer;
begin
  index := NewIndex;
  for i := 0 to High(workers) do
    workers[i] := TIndexWorker.Create(index, FSource, i);
  try
    for i := 0 to High(workers) do
      workers[i].Start;
    for i := 0 to High(workers) do
      workers[i].WaitFor;
    for i := 0 to High(workers) do
      Assert.AreEqual('', workers[i].Error, 'worker ' + IntToStr(i));
  finally
    for i := 0 to High(workers) do
      workers[i].Free;
  end;
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'the hash', 'the spec');
  index.Save;
  //what was written last loads, entry for entry.
  Assert.AreEqual(index.Count, NewIndex.Count);
end;

type
  //An index whose today is whatever the test says.
  TDayIndex = class(TFolderSourceIndex)
  private
    FDay : integer;
  protected
    function CurrentDay : integer; override;
  public
    property Day : integer read FDay write FDay;
  end;

function TDayIndex.CurrentDay : integer;
begin
  result := FDay;
end;

function NewDayIndex(const fileName : string; const sourceFolder : string; const day : integer; out index : IFolderSourceIndex) : TDayIndex;
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  result := TDayIndex.Create(logger, fileName, sourceFolder, TCompilerVersion.Delphi12_0);
  result.Day := day;
  index := result;
end;

procedure TFolderIndexTests.Entries_Not_Used_For_60_Days_Are_Dropped_When_Saving;
var
  dayIndex : TDayIndex;
  index : IFolderSourceIndex;
  hash : string;
  specText : string;

  function Has(const name : string; const day : integer) : boolean;
  var
    reloaded : IFolderSourceIndex;
  begin
    NewDayIndex(IndexFile, FSource, day, reloaded);
    result := reloaded.TryGet(TPath.Combine(FSource, name), SomeStamps, hash, specText);
  end;

begin
  dayIndex := NewDayIndex(IndexFile, FSource, 1000, index);
  index.Put(TPath.Combine(FSource, 'a.dpkg'), SomeStamps, 'h', 'used again on day 1030');
  index.Put(TPath.Combine(FSource, 'b.dpkg'), SomeStamps, 'h', 'never used again');
  index.Save;
  dayIndex.Day := 1030;
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, 'a.dpkg'), SomeStamps, hash, specText));
  index.Save;

  //60 days unused is kept...
  dayIndex.Day := 1060;
  index.Put(TPath.Combine(FSource, 'c.dpkg'), SomeStamps, 'h', 'a change, so it saves');
  index.Save;
  Assert.IsTrue(Has('b.dpkg', 1060), '60 days');

  //...61 isn't.
  dayIndex.Day := 1061;
  index.Put(TPath.Combine(FSource, 'd.dpkg'), SomeStamps, 'h', 'a change, so it saves');
  index.Save;
  Assert.IsFalse(Has('b.dpkg', 1061), '61 days');
  Assert.IsTrue(Has('a.dpkg', 1061), 'used 31 days ago');
  Assert.IsTrue(Has('c.dpkg', 1061));
  Assert.IsTrue(Has('d.dpkg', 1061));
end;

procedure TFolderIndexTests.Beyond_The_Most_Entries_The_Least_Recently_Used_Go;
var
  dayIndex : TDayIndex;
  index : IFolderSourceIndex;
  reloaded : IFolderSourceIndex;
  i : integer;
  hash : string;
  specText : string;
begin
  dayIndex := NewDayIndex(IndexFile, FSource, 1000, index);
  index.Put(TPath.Combine(FSource, 'oldest.dpkg'), SomeStamps, 'h', 'used first');
  dayIndex.Day := 1001;
  for i := 1 to cFolderIndexMaxEntries do
    index.Put(TPath.Combine(FSource, 'p' + IntToStr(i) + '.dpkg'), SomeStamps, 'h', 'spec ' + IntToStr(i));
  Assert.AreEqual(cFolderIndexMaxEntries + 1, index.Count, 'precondition');
  index.Save;

  NewDayIndex(IndexFile, FSource, 1001, reloaded);
  Assert.AreEqual(cFolderIndexMaxEntries, reloaded.Count);
  Assert.IsFalse(reloaded.TryGet(TPath.Combine(FSource, 'oldest.dpkg'), SomeStamps, hash, specText), 'the least recently used');
  Assert.IsTrue(reloaded.TryGet(TPath.Combine(FSource, 'p1.dpkg'), SomeStamps, hash, specText));
  Assert.IsTrue(reloaded.TryGet(TPath.Combine(FSource, 'p' + IntToStr(cFolderIndexMaxEntries) + '.dpkg'), SomeStamps, hash, specText));
end;

procedure TFolderIndexTests.Using_An_Entry_Again_The_Same_Day_Is_Not_Saved;
var
  dayIndex : TDayIndex;
  index : IFolderSourceIndex;
  hash : string;
  specText : string;
begin
  dayIndex := NewDayIndex(IndexFile, FSource, 1000, index);
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'h', 'spec');
  index.Save;
  System.SysUtils.DeleteFile(IndexFile);

  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  index.Save;
  Assert.IsFalse(FileExists(IndexFile), 'used again the same day');

  dayIndex.Day := 1001;
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  index.Save;
  Assert.IsTrue(FileExists(IndexFile), 'used the next day');
end;

procedure TFolderIndexTests.An_Entry_Without_A_Used_Day_Counts_As_Used_Today;
var
  dayIndex : TDayIndex;
  index : IFolderSourceIndex;
  text : string;
  hash : string;
  specText : string;
begin
  //an index written without the day.
  NewDayIndex(IndexFile, FSource, 1000, index);
  index.Put(TPath.Combine(FSource, cPackageA), SomeStamps, 'h', 'spec');
  index.Save;
  text := TFile.ReadAllText(IndexFile, TEncoding.UTF8);
  Assert.IsTrue(Pos(',"used":1000', text) > 0, 'precondition : ' + text);
  TFile.WriteAllText(IndexFile, StringReplace(text, ',"used":1000', '', []), TEncoding.UTF8);

  dayIndex := NewDayIndex(IndexFile, FSource, 2000, index);
  Assert.IsTrue(index.TryGet(TPath.Combine(FSource, cPackageA), SomeStamps, hash, specText));
  //it counts from the day it was loaded.
  dayIndex.Day := 2060;
  index.Put(TPath.Combine(FSource, cPackageB), SomeStamps, 'h', 'a change, so it saves');
  index.Save;
  NewDayIndex(IndexFile, FSource, 2060, index);
  Assert.AreEqual(2, index.Count, '60 days after it was loaded, it is still there');
end;

procedure TFolderIndexTests.Index_File_Is_Per_Folder_And_Compiler;
var
  fileName : string;
begin
  fileName := FolderIndexFileName(FIndexFolder, 'C:\Some\Feed', TCompilerVersion.Delphi12_0);
  Assert.AreEqual(IncludeTrailingPathDelimiter(FIndexFolder), ExtractFilePath(fileName), false);
  Assert.IsTrue(EndsText('-delphi12.0.v' + IntToStr(cFolderIndexFormat) + '.json', fileName), fileName);
  Assert.AreEqual(fileName, FolderIndexFileName(FIndexFolder, 'c:\some\feed\', TCompilerVersion.Delphi12_0), false, 'the same folder');
  Assert.IsFalse(SameText(fileName, FolderIndexFileName(FIndexFolder, 'C:\Some\Other', TCompilerVersion.Delphi12_0)), 'another folder');
  Assert.IsFalse(SameText(fileName, FolderIndexFileName(FIndexFolder, 'C:\Some\Feed', TCompilerVersion.Delphi11_0)), 'another compiler');
end;

procedure TFolderIndexTests.Environment_Variable_Turns_The_Index_Off(const value : string; const enabled : boolean);
var
  previous : string;
begin
  previous := GetEnvironmentVariable(cDPMFolderIndexEnvironmentVar);
  try
    if value = '' then
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), nil)
    else
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), PChar(value));
    Assert.IsTrue(FolderIndexEnabled = enabled, 'DPMFOLDERINDEX=' + value);
  finally
    if previous = '' then
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), nil)
    else
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), PChar(previous));
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFolderIndexTests);

end.
