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

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Utils.Hash,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Directory,
  DPM.Core.Options.Search,
  TestLogger;

const
  cPackageFile = 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg';

function NewRepository(const sourceFolder : string) : IPackageRepository;
var
  logger : ILogger;
  source : ISourceConfig;
begin
  logger := TTestLogger.Create;
  result := TDirectoryPackageRepository.Create(logger);
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

initialization
  TDUnitX.RegisterTestFixture(TDirectoryRepositoryDownloadTests);

end.
