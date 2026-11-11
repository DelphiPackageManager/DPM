unit DPM.Core.Tests.Repository.Directory;

// Getting a package from a folder source into the package cache : the copy is what is checked, against the
// source's .sha256 file and the hash the package was listed with, and it only replaces the cached package once
// it passes.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDirectoryRepositoryDownloadTests = class
  private
    FRoot : string;
    FSourceFolder : string;
    FCacheFolder : string;
    FSourceFile : string;
    FCachedFile : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Download_CopiesThePackageAndRecordsItsHash;
    [Test]
    procedure Download_AcceptsAHashFileEndingInANewline;
    [Test]
    procedure Download_OfAFileNotMatchingItsHashFile_FailsAndKeepsTheCachedCopy;
    [Test]
    procedure Download_OfAFileChangedSinceItWasListed_Fails;
    //--skip/--take did nothing for a folder source - every search was the whole folder.
    [Test]
    procedure Feed_Honours_Skip_And_Take;
    //the feed worked out each package's hash (writing it beside the package) and then left it out of the results.
    [Test]
    procedure Feed_Items_Carry_The_Package_Hash;
  end;

  //The index of what the folder's package sidecars held (DPM.Core.Repository.Directory.Index) : a search, the
  //installed list and a package's details read only what has changed - and installing never uses it.
  [TestFixture]
  TDirectoryRepositoryIndexTests = class
  private
    FRoot : string;
    FFeed : string;
    FIndexFolder : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Search_Again_Reads_Nothing_From_The_Folder;
    [Test]
    procedure Details_And_Installed_List_Read_Nothing_The_Second_Time;

    [Test]
    procedure A_Rewritten_Package_Is_Read_Again;
    [Test]
    procedure A_Package_Written_To_Without_Changing_Size_Is_Read_Again;
    [Test]
    procedure An_Edited_Spec_Shows_The_New_Details;
    [Test]
    procedure A_Deleted_Spec_Is_Read_From_The_Package_Again;
    [Test]
    procedure A_Changed_Hash_File_Shows_The_New_Hash;
    [Test]
    procedure A_Removed_Hash_File_Is_Worked_Out_From_The_Package;
    [Test]
    procedure A_New_Version_Is_Found;
    [Test]
    procedure A_Deleted_Package_Is_Not_Found;
    //what the index trusts : a file whose size and write time are as they were hasn't changed.
    [Test]
    procedure A_Spec_Rewritten_To_The_Same_Size_And_Time_Is_Not_Noticed;

    [Test]
    procedure Only_A_Search_For_Everything_Forgets_Deleted_Packages;
    [Test]
    procedure Another_Folder_Has_Its_Own_Index;
    [Test]
    procedure With_The_Index_Turned_Off_The_Folder_Is_Read_Each_Time;
    [Test]
    procedure Installing_Never_Uses_The_Index;
    [Test]
    procedure Concurrent_Searches_And_Installed_Lists_Are_Safe;

    //a push left the old package's spec and icon beside the new one, so it was shown (and installed) as the old one.
    [Test]
    procedure Pushing_A_Changed_Package_Removes_What_Was_Read_From_The_Old_One;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.Zip,
  Spring.Collections,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Utils.Hash,
  DPM.Core.Dependency.Version,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Directory,
  DPM.Core.Repository.Directory.Index,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  TestLogger;

const
  cPackageFile = 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg';

function NewRepository(const sourceFolder : string) : IPackageRepository;
var
  logger : ILogger;
  source : ISourceConfig;
  repository : TDirectoryPackageRepository;
begin
  logger := TTestLogger.Create;
  repository := TDirectoryPackageRepository.Create(logger);
  result := repository;
  //never the per-user index - a folder beside the source, which goes with the test's folder.
  repository.IndexFolder := TPath.Combine(ExtractFilePath(ExcludeTrailingPathDelimiter(sourceFolder)), 'index');
  source := TSourceConfig.Create('folder', sourceFolder, TSourceType.Folder, '', '', true);
  result.Configure(source);
end;

function NewInfo(const hash : string) : IPackageInfo;
begin
  result := TPackageInfo.Create('folder', 'Sample.Pkg', TPackageVersion.Parse('1.2.3'), TCompilerVersion.Delphi12_0, hash, 'SHA256');
end;

function Download(const sourceFolder : string; const cacheFolder : string; const hash : string; out repo : IPackageRepository;
                  out fileName : string) : boolean;
begin
  repo := NewRepository(sourceFolder);
  fileName := '';
  result := repo.DownloadPackage(TCancellationTokenSourceFactory.Create.Token, NewInfo(hash), cacheFolder, fileName);
end;

//nothing but the package and its hash file - no copy left part way.
procedure AssertNoLeftovers(const folder : string);
var
  files : TStringDynArray;
  f : string;
begin
  files := TDirectory.GetFiles(folder);
  for f in files do
    Assert.IsTrue(SameText(ExtractFileName(f), cPackageFile) or SameText(ExtractFileName(f), cPackageFile + cPackageHashAlgorithmExt),
                  'left behind : ' + f);
end;

//A package in the folder, as the feed reads it : the .dpkg (only looked for) and the dspec read from it beside it.
procedure WriteFeedPackage(const folder : string; const id : string);
var
  packageFile : string;
begin
  packageFile := TPath.Combine(folder, id + '-delphi12.0-0000000000000001-1.0.0.dpkg');
  TFile.WriteAllText(packageFile, 'a package', TEncoding.ASCII);
  TFile.WriteAllText(ChangeFileExt(packageFile, cLegacyPackageSpecExt),
    'metadata:' + sLineBreak +
    '  id: ' + id + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: feed test package' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: Source\*.pas' + sLineBreak, TEncoding.UTF8);
end;

{ TDirectoryRepositoryDownloadTests }

procedure TDirectoryRepositoryDownloadTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-dir-repo-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  FSourceFolder := TPath.Combine(FRoot, 'feed');
  FCacheFolder := TPath.Combine(FRoot, 'cache');
  ForceDirectories(FSourceFolder);
  ForceDirectories(FCacheFolder);
  FSourceFile := TPath.Combine(FSourceFolder, cPackageFile);
  FCachedFile := TPath.Combine(FCacheFolder, cPackageFile);
  TFile.WriteAllText(FSourceFile, 'the package as it was pushed', TEncoding.ASCII);
end;

procedure TDirectoryRepositoryDownloadTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
end;

procedure TDirectoryRepositoryDownloadTests.Download_CopiesThePackageAndRecordsItsHash;
var
  repo : IPackageRepository;
  fileName : string;
  hash : string;
  downloaded : boolean;
begin
  hash := THashSHA256.GetHashStringFromFile(FSourceFile);
  TFile.WriteAllText(FSourceFile + cPackageHashAlgorithmExt, hash, TEncoding.ASCII);
  //repo is only set once Download returns - don't read it in the same call.
  downloaded := Download(FSourceFolder, FCacheFolder, '', repo, fileName);
  Assert.IsTrue(downloaded, repo.LastError);
  Assert.IsTrue(SameText(FCachedFile, fileName), fileName);
  Assert.AreEqual('the package as it was pushed', TFile.ReadAllText(FCachedFile));
  Assert.AreEqual(hash, Trim(TFile.ReadAllText(FCachedFile + cPackageHashAlgorithmExt)));
  AssertNoLeftovers(FCacheFolder);
end;

procedure TDirectoryRepositoryDownloadTests.Download_AcceptsAHashFileEndingInANewline;
var
  repo : IPackageRepository;
  fileName : string;
  downloaded : boolean;
begin
  //what an editor, or another tool, leaves.
  TFile.WriteAllText(FSourceFile + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(FSourceFile) + sLineBreak, TEncoding.ASCII);
  downloaded := Download(FSourceFolder, FCacheFolder, '', repo, fileName);
  Assert.IsTrue(downloaded, repo.LastError);
end;

procedure TDirectoryRepositoryDownloadTests.Download_OfAFileNotMatchingItsHashFile_FailsAndKeepsTheCachedCopy;
var
  repo : IPackageRepository;
  fileName : string;
begin
  TFile.WriteAllText(FSourceFile + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(FSourceFile), TEncoding.ASCII);
  TFile.WriteAllText(FSourceFile, 'the package, changed after it was pushed', TEncoding.ASCII);
  TFile.WriteAllText(FCachedFile, 'the good cached copy', TEncoding.ASCII);
  Assert.IsFalse(Download(FSourceFolder, FCacheFolder, '', repo, fileName));
  Assert.AreEqual(Ord(TErrorCode.E1202_PackageHashMismatch), Ord(repo.LastErrorCode), repo.LastError);
  Assert.AreEqual('the good cached copy', TFile.ReadAllText(FCachedFile), 'a copy that fails must not replace the cached package');
  AssertNoLeftovers(FCacheFolder);
end;

procedure TDirectoryRepositoryDownloadTests.Download_OfAFileChangedSinceItWasListed_Fails;
var
  repo : IPackageRepository;
  fileName : string;
begin
  Assert.IsFalse(Download(FSourceFolder, FCacheFolder, StringOfChar('a', 64), repo, fileName));
  Assert.AreEqual(Ord(TErrorCode.E1202_PackageHashMismatch), Ord(repo.LastErrorCode), repo.LastError);
  Assert.IsFalse(FileExists(FCachedFile), 'a copy that fails must not be left in the cache');
  AssertNoLeftovers(FCacheFolder);
end;

procedure TDirectoryRepositoryDownloadTests.Feed_Honours_Skip_And_Take;
var
  feedFolder : string;
  repo : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  feedFolder := TPath.Combine(FRoot, 'paged');
  ForceDirectories(feedFolder);
  WriteFeedPackage(feedFolder, 'Gamma.Pkg');
  WriteFeedPackage(feedFolder, 'Alpha.Pkg');
  WriteFeedPackage(feedFolder, 'Beta.Pkg');
  repo := NewRepository(feedFolder);
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := TCompilerVersion.Delphi12_0;
    options.Skip := 1;
    options.Take := 1;
    feed := repo.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  Assert.AreEqual(1, feed.Results.Count);
  Assert.AreEqual('Beta.Pkg', feed.Results[0].Id, 'the second, in id order');
  Assert.AreEqual(Int64(3), feed.TotalCount);
end;

procedure TDirectoryRepositoryDownloadTests.Feed_Items_Carry_The_Package_Hash;
var
  feedFolder : string;
  repo : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  feedFolder := TPath.Combine(FRoot, 'hashed');
  ForceDirectories(feedFolder);
  WriteFeedPackage(feedFolder, 'Alpha.Pkg');
  repo := NewRepository(feedFolder);
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := TCompilerVersion.Delphi12_0;
    feed := repo.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  Assert.AreEqual(1, feed.Results.Count);
  Assert.AreEqual(THashSHA256.GetHashStringFromFile(TPath.Combine(feedFolder, 'Alpha.Pkg-delphi12.0-0000000000000001-1.0.0.dpkg')),
                  feed.Results[0].Hash);
  Assert.AreEqual(cPackageHashAlgorithm, feed.Results[0].HashAlgorithm);
end;

{ index tests }

const
  cPackagePart = '-delphi12.0-0000000000000001-';

type
  //Counts the reads of the folder the index saves.
  TCountingDirectoryRepository = class(TDirectoryPackageRepository)
  private
    FHashReads : integer;
    FSpecReads : integer;
    FHashesWorkedOut : integer;
  protected
    function ReadHashFile(const fileName : string) : string; override;
    function ReadSpecFile(const fileName : string) : string; override;
    function HashPackageFile(const fileName : string) : string; override;
  public
    procedure ResetCounts;
    function IndexFor(const compilerVersion : TCompilerVersion) : IFolderSourceIndex;
    property HashReads : integer read FHashReads;
    property SpecReads : integer read FSpecReads;
    property HashesWorkedOut : integer read FHashesWorkedOut;
  end;

function TCountingDirectoryRepository.ReadHashFile(const fileName : string) : string;
begin
  InterlockedIncrement(FHashReads);
  result := inherited ReadHashFile(fileName);
end;

function TCountingDirectoryRepository.ReadSpecFile(const fileName : string) : string;
begin
  InterlockedIncrement(FSpecReads);
  result := inherited ReadSpecFile(fileName);
end;

function TCountingDirectoryRepository.HashPackageFile(const fileName : string) : string;
begin
  InterlockedIncrement(FHashesWorkedOut);
  result := inherited HashPackageFile(fileName);
end;

procedure TCountingDirectoryRepository.ResetCounts;
begin
  FHashReads := 0;
  FSpecReads := 0;
  FHashesWorkedOut := 0;
end;

function TCountingDirectoryRepository.IndexFor(const compilerVersion : TCompilerVersion) : IFolderSourceIndex;
begin
  result := GetIndex(compilerVersion);
end;

function NewCountingRepository(const folder : string; const indexFolder : string; out counting : TCountingDirectoryRepository) : IPackageRepository;
var
  logger : ILogger;
  source : ISourceConfig;
begin
  logger := TTestLogger.Create;
  counting := TCountingDirectoryRepository.Create(logger);
  result := counting;
  counting.IndexFolder := indexFolder;
  source := TSourceConfig.Create('folder', folder, TSourceType.Folder, '', '', true);
  result.Configure(source);
end;

function PackageFileName(const folder : string; const id : string; const version : string) : string;
begin
  result := TPath.Combine(folder, id + cPackagePart + version + cPackageFileExt);
end;

function SpecYaml(const id : string; const version : string; const description : string; const dependencyId : string = '') : string;
begin
  result :=
    'metadata:' + sLineBreak +
    '  id: ' + id + sLineBreak +
    '  version: ' + version + sLineBreak +
    '  description: ' + description + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak;
  if dependencyId <> '' then
    result := result +
      '    dependencies:' + sLineBreak +
      '      - id: ' + dependencyId + sLineBreak +
      '        version: 1.0.0' + sLineBreak;
  result := result +
    '    source:' + sLineBreak +
    '      - src: Source\*.pas' + sLineBreak;
end;

//A real package file : a zip holding its spec and an icon.
function WritePackageFile(const packageFile : string; const id : string; const version : string; const description : string) : string;
var
  zip : TZipFile;
begin
  ForceDirectories(ExtractFilePath(packageFile));
  zip := TZipFile.Create;
  try
    zip.Open(packageFile, TZipMode.zmWrite);
    zip.Add(TEncoding.UTF8.GetBytes(SpecYaml(id, version, description)), cPackageDspecFile);
    zip.Add(TEncoding.UTF8.GetBytes('<svg xmlns="http://www.w3.org/2000/svg"/>'), cIconFileSVG);
    zip.Close;
  finally
    zip.Free;
  end;
  result := packageFile;
end;

function SpecSidecar(const packageFile : string) : string;
begin
  result := ChangeFileExt(packageFile, cLegacyPackageSpecExt);
end;

//A package as a push and a first read leave it : the package, its .sha256 and the spec read from it.
function WritePackage(const folder : string; const id : string; const version : string; const description : string) : string;
begin
  result := WritePackageFile(PackageFileName(folder, id, version), id, version, description);
  TFile.WriteAllText(result + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(result), TEncoding.ASCII);
  TFile.WriteAllText(SpecSidecar(result), SpecYaml(id, version, description), TEncoding.UTF8);
end;

procedure DeletePackage(const packageFile : string);
begin
  System.SysUtils.DeleteFile(packageFile);
  System.SysUtils.DeleteFile(packageFile + cPackageHashAlgorithmExt);
  System.SysUtils.DeleteFile(SpecSidecar(packageFile));
end;

function Search(const repo : IPackageRepository; const terms : string) : IPackageSearchResult;
var
  options : TSearchOptions;
begin
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := TCompilerVersion.Delphi12_0;
    options.SearchTerms := terms;
    options.Prerelease := true;
    result := repo.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
end;

function FindItem(const feed : IPackageSearchResult; const id : string) : IPackageSearchResultItem;
var
  i : integer;
begin
  result := nil;
  for i := 0 to feed.Results.Count - 1 do
    if SameText(feed.Results[i].Id, id) then
      exit(feed.Results[i]);
end;

function Describe(const repo : IPackageRepository; const id : string) : string;
var
  item : IPackageSearchResultItem;
begin
  item := FindItem(Search(repo, ''), id);
  Assert.IsNotNull(item, id + ' was not found');
  result := item.Description;
end;

function NewIdentity(const id : string; const version : string) : IPackageIdentity;
var
  info : IPackageInfo;
begin
  info := TPackageInfo.Create('folder', id, TPackageVersion.Parse(version), TCompilerVersion.Delphi12_0, '', '');
  result := info;
end;

procedure AssertSameResults(const expected : IPackageSearchResult; const actual : IPackageSearchResult);
var
  i : integer;
  other : IPackageSearchResultItem;
begin
  Assert.AreEqual(expected.Results.Count, actual.Results.Count);
  for i := 0 to expected.Results.Count - 1 do
  begin
    other := FindItem(actual, expected.Results[i].Id);
    Assert.IsNotNull(other, expected.Results[i].Id);
    Assert.AreEqual(expected.Results[i].Description, other.Description, false);
    Assert.AreEqual(expected.Results[i].Hash, other.Hash, false);
    Assert.AreEqual(expected.Results[i].Version.ToString, other.Version.ToString, false);
    Assert.AreEqual(expected.Results[i].LatestVersion.ToString, other.LatestVersion.ToString, false);
    Assert.AreEqual(expected.Results[i].PublishedDate, other.PublishedDate, false);
  end;
end;

//the time as it is stored - TFile's TDateTime loses the part of it below a millisecond.
function RawWriteTime(const fileName : string) : TFileTime;
var
  data : TWin32FileAttributeData;
begin
  Assert.IsTrue(GetFileAttributesEx(PChar(fileName), GetFileExInfoStandard, @data), fileName);
  result := data.ftLastWriteTime;
end;

procedure SetRawWriteTime(const fileName : string; const fileTime : TFileTime);
var
  handle : THandle;
begin
  handle := CreateFile(PChar(fileName), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Assert.IsTrue(handle <> INVALID_HANDLE_VALUE, fileName);
  try
    Assert.IsTrue(SetFileTime(handle, nil, nil, @fileTime), fileName);
  finally
    CloseHandle(handle);
  end;
end;

procedure TDirectoryRepositoryIndexTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-dir-index-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  FFeed := TPath.Combine(FRoot, 'feed');
  FIndexFolder := TPath.Combine(FRoot, 'index');
  ForceDirectories(FFeed);
end;

procedure TDirectoryRepositoryIndexTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
end;

procedure TDirectoryRepositoryIndexTests.Search_Again_Reads_Nothing_From_The_Folder;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  first : IPackageSearchResult;
  second : IPackageSearchResult;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  WritePackage(FFeed, 'Beta.Pkg', '1.0.0', 'beta');
  WritePackage(FFeed, 'Gamma.Pkg', '1.0.0', 'gamma');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  first := Search(repo, '');
  Assert.AreEqual(3, first.Results.Count);
  Assert.AreEqual(3, counting.SpecReads, 'the first search reads each spec');
  Assert.AreEqual(3, counting.HashReads, 'the first search reads each hash');

  counting.ResetCounts;
  second := Search(repo, '');
  Assert.AreEqual(0, counting.SpecReads, 'specs read again');
  Assert.AreEqual(0, counting.HashReads, 'hashes read again');
  AssertSameResults(first, second);

  //another process - the index comes from its file.
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  second := Search(repo, '');
  Assert.AreEqual(0, counting.SpecReads, 'specs read by another process');
  Assert.AreEqual(0, counting.HashReads, 'hashes read by another process');
  AssertSameResults(first, second);
end;

procedure TDirectoryRepositoryIndexTests.Details_And_Installed_List_Read_Nothing_The_Second_Time;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  details : IPackageSearchResultItem;
  ids : IList<IPackageIdentity>;
  installed : IPackageSearchResult;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  details := repo.GetPackageMetaData(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', '1.0.0', TCompilerVersion.Delphi12_0);
  Assert.IsNotNull(details);
  Assert.AreEqual('alpha', details.Description, false);
  Assert.AreEqual(1, counting.SpecReads);

  counting.ResetCounts;
  details := repo.GetPackageMetaData(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', '1.0.0', TCompilerVersion.Delphi12_0);
  Assert.AreEqual('alpha', details.Description, false);
  Assert.AreEqual(THashSHA256.GetHashStringFromFile(PackageFileName(FFeed, 'Alpha.Pkg', '1.0.0')), details.Hash, false);
  Assert.AreEqual(0, counting.SpecReads, 'details read again');
  Assert.AreEqual(0, counting.HashReads, 'hash read again');

  //the installed list, in another process.
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  ids := TCollections.CreateList<IPackageIdentity>;
  ids.Add(NewIdentity('Alpha.Pkg', '1.0.0'));
  installed := repo.GetPackageFeedByIds(TCancellationTokenSourceFactory.Create.Token, ids, TCompilerVersion.Delphi12_0);
  Assert.AreEqual(1, installed.Results.Count);
  Assert.AreEqual('alpha', installed.Results[0].Description, false);
  Assert.AreEqual(0, counting.SpecReads, 'the installed list read it again');
  Assert.AreEqual(0, counting.HashReads, 'the installed list read the hash again');
end;

procedure TDirectoryRepositoryIndexTests.A_Rewritten_Package_Is_Read_Again;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  counting.ResetCounts;
  WritePackageFile(packageFile, 'Alpha.Pkg', '1.0.0', 'alpha, written again with a longer description');
  Search(repo, '');
  Assert.AreEqual(1, counting.SpecReads);
  Assert.AreEqual(1, counting.HashReads);
end;

procedure TDirectoryRepositoryIndexTests.A_Package_Written_To_Without_Changing_Size_Is_Read_Again;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  counting.ResetCounts;
  TFile.SetLastWriteTime(packageFile, Now + 1);
  Search(repo, '');
  Assert.AreEqual(1, counting.SpecReads);
end;

procedure TDirectoryRepositoryIndexTests.An_Edited_Spec_Shows_The_New_Details;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Assert.AreEqual('alpha', Describe(repo, 'Alpha.Pkg'), false);
  TFile.WriteAllText(SpecSidecar(packageFile), SpecYaml('Alpha.Pkg', '1.0.0', 'alpha, edited'), TEncoding.UTF8);
  Assert.AreEqual('alpha, edited', Describe(repo, 'Alpha.Pkg'), false);
end;

procedure TDirectoryRepositoryIndexTests.A_Deleted_Spec_Is_Read_From_The_Package_Again;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackageFile(PackageFileName(FFeed, 'Alpha.Pkg', '1.0.0'), 'Alpha.Pkg', '1.0.0', 'from the package');
  TFile.WriteAllText(packageFile + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(packageFile), TEncoding.ASCII);
  TFile.WriteAllText(SpecSidecar(packageFile), SpecYaml('Alpha.Pkg', '1.0.0', 'from the sidecar'), TEncoding.UTF8);
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Assert.AreEqual('from the sidecar', Describe(repo, 'Alpha.Pkg'), false);

  System.SysUtils.DeleteFile(SpecSidecar(packageFile));
  Assert.AreEqual('from the package', Describe(repo, 'Alpha.Pkg'), false);
  Assert.IsTrue(FileExists(SpecSidecar(packageFile)), 'read from the package, and written beside it again');

  //the spec written beside it is new to the index - read once, then not again.
  counting.ResetCounts;
  Assert.AreEqual('from the package', Describe(repo, 'Alpha.Pkg'), false);
  Assert.AreEqual(1, counting.SpecReads);
  counting.ResetCounts;
  Assert.AreEqual('from the package', Describe(repo, 'Alpha.Pkg'), false);
  Assert.AreEqual(0, counting.SpecReads);
end;

procedure TDirectoryRepositoryIndexTests.A_Changed_Hash_File_Shows_The_New_Hash;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  TFile.WriteAllText(packageFile + cPackageHashAlgorithmExt, StringOfChar('a', 64), TEncoding.ASCII);
  Assert.AreEqual(StringOfChar('a', 64), FindItem(Search(repo, ''), 'Alpha.Pkg').Hash, false);
end;

procedure TDirectoryRepositoryIndexTests.A_Removed_Hash_File_Is_Worked_Out_From_The_Package;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  System.SysUtils.DeleteFile(packageFile + cPackageHashAlgorithmExt);

  counting.ResetCounts;
  Assert.AreEqual(THashSHA256.GetHashStringFromFile(packageFile), FindItem(Search(repo, ''), 'Alpha.Pkg').Hash, false);
  Assert.AreEqual(1, counting.HashesWorkedOut);
  Assert.IsTrue(FileExists(packageFile + cPackageHashAlgorithmExt), 'written beside it again');

  //the hash file written beside it is new to the index - read once, then not again.
  counting.ResetCounts;
  Search(repo, '');
  Assert.AreEqual(0, counting.HashesWorkedOut);
  Assert.AreEqual(1, counting.HashReads);
  counting.ResetCounts;
  Search(repo, '');
  Assert.AreEqual(0, counting.HashReads);
end;

procedure TDirectoryRepositoryIndexTests.A_New_Version_Is_Found;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  item : IPackageSearchResultItem;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'one');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  counting.ResetCounts;
  WritePackage(FFeed, 'Alpha.Pkg', '1.1.0', 'one point one');
  item := FindItem(Search(repo, ''), 'Alpha.Pkg');
  Assert.AreEqual('1.1.0', item.LatestVersion.ToString, false);
  Assert.AreEqual('one point one', item.Description, false);
  Assert.AreEqual(1, counting.SpecReads);
end;

procedure TDirectoryRepositoryIndexTests.A_Deleted_Package_Is_Not_Found;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  feed : IPackageSearchResult;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  DeletePackage(WritePackage(FFeed, 'Beta.Pkg', '1.0.0', 'beta'));
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Assert.AreEqual(1, Search(repo, '').Results.Count, 'precondition');
  WritePackage(FFeed, 'Beta.Pkg', '1.0.0', 'beta');
  Assert.AreEqual(2, Search(repo, '').Results.Count);
  DeletePackage(PackageFileName(FFeed, 'Beta.Pkg', '1.0.0'));
  feed := Search(repo, '');
  Assert.AreEqual(1, feed.Results.Count);
  Assert.IsNull(FindItem(feed, 'Beta.Pkg'));
end;

procedure TDirectoryRepositoryIndexTests.A_Spec_Rewritten_To_The_Same_Size_And_Time_Is_Not_Noticed;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
  writeTime : TFileTime;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'aaaa');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Assert.AreEqual('aaaa', Describe(repo, 'Alpha.Pkg'), false);
  writeTime := RawWriteTime(SpecSidecar(packageFile));
  TFile.WriteAllText(SpecSidecar(packageFile), SpecYaml('Alpha.Pkg', '1.0.0', 'bbbb'), TEncoding.UTF8);
  SetRawWriteTime(SpecSidecar(packageFile), writeTime);
  Assert.AreEqual('aaaa', Describe(repo, 'Alpha.Pkg'), false);
end;

procedure TDirectoryRepositoryIndexTests.Only_A_Search_For_Everything_Forgets_Deleted_Packages;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  WritePackage(FFeed, 'Beta.Pkg', '1.0.0', 'beta');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Search(repo, '');
  Assert.AreEqual(2, counting.IndexFor(TCompilerVersion.Delphi12_0).Count);
  DeletePackage(PackageFileName(FFeed, 'Beta.Pkg', '1.0.0'));
  Search(repo, 'Alpha');
  Assert.AreEqual(2, counting.IndexFor(TCompilerVersion.Delphi12_0).Count, 'a search for Alpha can''t tell Beta has gone');
  Search(repo, '');
  Assert.AreEqual(1, counting.IndexFor(TCompilerVersion.Delphi12_0).Count);
end;

procedure TDirectoryRepositoryIndexTests.Another_Folder_Has_Its_Own_Index;
var
  secondFeed : string;
  firstCounting : TCountingDirectoryRepository;
  secondCounting : TCountingDirectoryRepository;
  firstRepo : IPackageRepository;
  secondRepo : IPackageRepository;
begin
  secondFeed := TPath.Combine(FRoot, 'second-feed');
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'in the first folder');
  WritePackage(secondFeed, 'Alpha.Pkg', '1.0.0', 'in the second folder');
  firstRepo := NewCountingRepository(FFeed, FIndexFolder, firstCounting);
  secondRepo := NewCountingRepository(secondFeed, FIndexFolder, secondCounting);
  Assert.AreEqual('in the first folder', Describe(firstRepo, 'Alpha.Pkg'), false);
  Assert.AreEqual('in the second folder', Describe(secondRepo, 'Alpha.Pkg'), false);
  Assert.AreEqual(1, secondCounting.SpecReads);
  firstCounting.ResetCounts;
  Assert.AreEqual('in the first folder', Describe(firstRepo, 'Alpha.Pkg'), false);
  Assert.AreEqual(0, firstCounting.SpecReads);
end;

procedure TDirectoryRepositoryIndexTests.With_The_Index_Turned_Off_The_Folder_Is_Read_Each_Time;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  previous : string;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  previous := GetEnvironmentVariable(cDPMFolderIndexEnvironmentVar);
  SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), 'off');
  try
    repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  finally
    if previous = '' then
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), nil)
    else
      SetEnvironmentVariable(PChar(cDPMFolderIndexEnvironmentVar), PChar(previous));
  end;
  Assert.IsNull(counting.IndexFor(TCompilerVersion.Delphi12_0));
  Assert.AreEqual('alpha', Describe(repo, 'Alpha.Pkg'), false);
  Assert.AreEqual(1, counting.HashReads);
  counting.ResetCounts;
  Assert.AreEqual('alpha', Describe(repo, 'Alpha.Pkg'), false);
  Assert.AreEqual(1, counting.HashReads, 'read again');
  Assert.IsFalse(DirectoryExists(FIndexFolder), 'nothing written');
end;

procedure TDirectoryRepositoryIndexTests.Installing_Never_Uses_The_Index;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  packageFile : string;
  details : IPackageSearchResultItem;
  info : IPackageInfo;
  versions : IList<IPackageInfo>;
begin
  packageFile := WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'from the folder');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  //an entry that matches the folder exactly, but says something else - and has a dependency.
  counting.IndexFor(TCompilerVersion.Delphi12_0).Put(packageFile, ListFolder(FFeed, ['*'], cPackageFileExt).StampsFor(packageFile), 'the hash',
                                                     SpecYaml('Alpha.Pkg', '1.0.0', 'from the index', 'Some.Dependency'));

  //what is shown comes from it...
  details := repo.GetPackageMetaData(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', '1.0.0', TCompilerVersion.Delphi12_0);
  Assert.AreEqual('from the index', details.Description, false, 'precondition : the index is used for what is shown');

  //...what is installed doesn't.
  info := repo.GetPackageInfo(TCancellationTokenSourceFactory.Create.Token, NewIdentity('Alpha.Pkg', '1.0.0'));
  Assert.IsNotNull(info);
  Assert.AreEqual(0, info.Dependencies.Count, 'GetPackageInfo');
  versions := repo.GetPackageVersionsWithDependencies(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', TCompilerVersion.Delphi12_0,
                                                      TVersionRange.Create(TPackageVersion.Parse('1.0.0')), true);
  Assert.AreEqual(1, versions.Count);
  Assert.AreEqual(0, versions[0].Dependencies.Count, 'GetPackageVersionsWithDependencies');
  info := repo.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', TCompilerVersion.Delphi12_0, TPackageVersion.Empty, true);
  Assert.IsNotNull(info);
  Assert.AreEqual(0, info.Dependencies.Count, 'FindLatestVersion');
end;

type
  //Searches (or asks for the installed list) over and over, alongside another doing the other.
  TRepositoryWorker = class(TThread)
  private
    FRepo : IPackageRepository;
    FIds : IList<IPackageIdentity>;
    FInstalledList : boolean;
    FError : string;
  protected
    procedure Execute; override;
  public
    constructor Create(const repo : IPackageRepository; const ids : IList<IPackageIdentity>; const installedList : boolean);
    property Error : string read FError;
  end;

constructor TRepositoryWorker.Create(const repo : IPackageRepository; const ids : IList<IPackageIdentity>; const installedList : boolean);
begin
  inherited Create(true);
  FRepo := repo;
  FIds := ids;
  FInstalledList := installedList;
end;

procedure TRepositoryWorker.Execute;
var
  i : integer;
  count : integer;
begin
  try
    for i := 1 to 20 do
    begin
      if FInstalledList then
        count := FRepo.GetPackageFeedByIds(TCancellationTokenSourceFactory.Create.Token, FIds, TCompilerVersion.Delphi12_0).Results.Count
      else
        count := Search(FRepo, '').Results.Count;
      if count <> 3 then
      begin
        FError := 'pass ' + IntToStr(i) + ' found ' + IntToStr(count) + ' packages';
        exit;
      end;
    end;
  except
    on e : Exception do
      FError := e.ClassName + ' : ' + e.Message;
  end;
end;

procedure TDirectoryRepositoryIndexTests.Concurrent_Searches_And_Installed_Lists_Are_Safe;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  ids : IList<IPackageIdentity>;
  searcher : TRepositoryWorker;
  lister : TRepositoryWorker;
begin
  WritePackage(FFeed, 'Alpha.Pkg', '1.0.0', 'alpha');
  WritePackage(FFeed, 'Beta.Pkg', '1.0.0', 'beta');
  WritePackage(FFeed, 'Gamma.Pkg', '1.0.0', 'gamma');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  ids := TCollections.CreateList<IPackageIdentity>;
  ids.Add(NewIdentity('Alpha.Pkg', '1.0.0'));
  ids.Add(NewIdentity('Beta.Pkg', '1.0.0'));
  ids.Add(NewIdentity('Gamma.Pkg', '1.0.0'));
  searcher := TRepositoryWorker.Create(repo, ids, false);
  try
    lister := TRepositoryWorker.Create(repo, ids, true);
    try
      searcher.Start;
      lister.Start;
      searcher.WaitFor;
      lister.WaitFor;
      Assert.AreEqual('', searcher.Error, 'search');
      Assert.AreEqual('', lister.Error, 'installed list');
    finally
      lister.Free;
    end;
  finally
    searcher.Free;
  end;
  Assert.AreEqual(3, counting.IndexFor(TCompilerVersion.Delphi12_0).Count);
end;

procedure TDirectoryRepositoryIndexTests.Pushing_A_Changed_Package_Removes_What_Was_Read_From_The_Old_One;
var
  counting : TCountingDirectoryRepository;
  repo : IPackageRepository;
  localFile : string;
  pushedFile : string;

  procedure Push;
  var
    options : TPushOptions;
    pushed : boolean;
  begin
    options := TPushOptions.Create;
    try
      options.PackagePath := localFile;
      //not inside the Assert - its message can be worked out before the push runs.
      pushed := repo.Push(TCancellationTokenSourceFactory.Create.Token, options);
      Assert.IsTrue(pushed, repo.LastError);
    finally
      options.Free;
    end;
  end;

  function Details : string;
  begin
    result := repo.GetPackageMetaData(TCancellationTokenSourceFactory.Create.Token, 'Alpha.Pkg', '1.0.0', TCompilerVersion.Delphi12_0).Description;
  end;

begin
  localFile := WritePackageFile(PackageFileName(TPath.Combine(FRoot, 'local'), 'Alpha.Pkg', '1.0.0'), 'Alpha.Pkg', '1.0.0', 'first');
  pushedFile := PackageFileName(FFeed, 'Alpha.Pkg', '1.0.0');
  repo := NewCountingRepository(FFeed, FIndexFolder, counting);
  Push;
  Assert.AreEqual('first', Details, false);
  Assert.IsTrue(FileExists(SpecSidecar(pushedFile)), 'precondition : the spec was read from the package and kept beside it');
  Assert.IsTrue(FileExists(ChangeFileExt(pushedFile, '.svg')), 'precondition : and the icon');

  WritePackageFile(localFile, 'Alpha.Pkg', '1.0.0', 'second, pushed under the same name');
  Push;
  Assert.IsFalse(FileExists(SpecSidecar(pushedFile)), 'the old package''s spec');
  Assert.IsFalse(FileExists(ChangeFileExt(pushedFile, '.svg')), 'the old package''s icon');
  Assert.AreEqual(THashSHA256.GetHashStringFromFile(localFile), Trim(TFile.ReadAllText(pushedFile + cPackageHashAlgorithmExt)), false);
  Assert.AreEqual('second, pushed under the same name', Details, false);
end;

initialization
  TDUnitX.RegisterTestFixture(TDirectoryRepositoryDownloadTests);
  TDUnitX.RegisterTestFixture(TDirectoryRepositoryIndexTests);

end.
