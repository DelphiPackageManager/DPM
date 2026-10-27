unit DPM.Core.Tests.Registry.Catalog;

interface

uses
  DUnitX.TestFramework,
  DPM.Core.Git.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  [TestFixture]
  TRegistryCatalogTests = class
  private
    FRootDir : string;
    procedure WritePackageFolder(const id : string; const repositoryUrl : string; const withDependency : boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FolderRegistry_LoadsPackageSpec;

    [Test]
    procedure GetPackageSpec_IsCaseInsensitive;

    [Test]
    procedure GetPackageSpec_UnknownId_ReturnsNil;

    [Test]
    procedure GetPackageIds_ReturnsAllPackages;
    [Test]
    procedure GitRegistry_ExplicitRefreshFailure_IsAnError;
    [Test]
    procedure GitRegistry_CloneFailureDuringAQuery_IsAWarning;
  end;

  //the local clone of a git url registry.
  [TestFixture]
  TRegistryMirrorTests = class
  private
    FRootDir : string;
    FRegistriesDir : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    //a source renamed to the name another url used (or the same name configured with another url) must not
    //read the other registry's packages.
    [Test]
    procedure Same_Name_Different_Url_Gets_Its_Own_Mirror;
    //a mirror folder that is a clone of something else is cloned again, not used.
    [Test]
    procedure Mirror_Of_Another_Url_Is_Cloned_Again;
    //git failing to say what a mirror is a clone of (git broken, say) is no reason to throw the mirror away.
    [Test]
    procedure Mirror_Whose_Origin_Cannot_Be_Read_Is_Kept;
    //every query of a source whose clone fails ran git again - in the IDE that is every search and selection.
    [Test]
    procedure Failed_Clone_Is_Not_Retried_Straight_Away;
    [Test]
    procedure Explicit_Refresh_Retries_A_Failed_Clone;
    [Test]
    procedure Failed_Clone_Is_Retried_Once_The_Wait_Is_Over;
    //the IDE queries a source from several threads at once - one of them clones it, the others wait for it.
    [Test]
    procedure Concurrent_First_Use_Clones_Once;
  end;

  [TestFixture]
  TGitRegistryRepositoryTests = class
  private
    FRootDir : string;
    function CreateRepository(const git : IGitClient; const sourceUrl : string) : IPackageRepository;
    procedure Configure(const repository : IPackageRepository; const sourceUrl : string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    //the repository manager reconfigures its repositories whenever the configuration is loaded - a source
    //that didn't change keeps its catalog rather than loading (and pulling) the registry again.
    [Test]
    procedure Configuring_The_Same_Source_Keeps_The_Catalog;
    [Test]
    procedure Configuring_Another_Source_Uses_Its_Registry;
    //listing versions, resolving and installing each asked git for the package repo's tags.
    [Test]
    procedure Tags_Are_Listed_Once_For_Repeated_Queries;
    [Test]
    procedure A_Failed_Tag_Listing_Is_Not_Remembered;
    //a refresh interval of 0 asks for the registry to be fetched every time - the tags are too.
    [Test]
    procedure Tags_Are_Not_Remembered_When_The_Refresh_Interval_Is_Zero;
    //--skip/--take did nothing, and every search listed the tags of every entry in the registry.
    [Test]
    procedure Feed_Honours_Skip_And_Take;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.SyncObjs,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Git.Client,
  DPM.Core.Registry.Interfaces,
  DPM.Core.Registry.Catalog,
  DPM.Core.Repository.GitRegistry,
  TestLogger,
  DPM.Core.Diagnostics;

const
  cRegistryUrl = 'https://example.invalid/registry.git';
  cPackageId = 'Test.GitPackage';
  cPackageUrl = 'https://example.invalid/Test.GitPackage.git';

type
  //every git command fails, as it does when the registry url is wrong.
  TFailingGitClient = class(TInterfacedObject, IGitClient)
  protected
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
    function GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
    function LastError : string;
  end;

  //a git that works. A clone writes a registry holding Test.GitPackage, and the commands are counted.
  TRecordingGitClient = class(TInterfacedObject, IGitClient)
  private
    FLock : TCriticalSection;
    FClonedUrls : TStringList;
    FCloneTargets : TStringList;
    FOrigins : IDictionary<string, string>; //lowercase folder -> what it is a clone of
    FPulls : integer;
    FTagListings : integer;
    FFailClone : boolean;
    FFailTagListings : integer;
    FCloneDelayMs : integer;
  protected
    function IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
    function LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
    function LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
    function Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
    function Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
    function Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
    function GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
    function LastError : string;
  public
    constructor Create;
    destructor Destroy; override;
    //'' forgets it, as if git couldn't say.
    procedure SetOrigin(const folder : string; const url : string);
    property ClonedUrls : TStringList read FClonedUrls;
    property CloneTargets : TStringList read FCloneTargets;
    property Pulls : integer read FPulls;
    property TagListings : integer read FTagListings;
    property FailClone : boolean read FFailClone write FFailClone;
    //this many tag listings fail before they work.
    property FailTagListings : integer read FFailTagListings write FFailTagListings;
    property CloneDelayMs : integer read FCloneDelayMs write FCloneDelayMs;
  end;

//a registry entry - <registryDir>\<id>\<id>.dspec.yaml.
procedure WriteRegistryEntry(const registryDir : string; const id : string; const repositoryUrl : string; const withDependency : boolean);
var
  folder : string;
  lines : TStringList;
begin
  folder := TPath.Combine(registryDir, id);
  TDirectory.CreateDirectory(folder);
  lines := TStringList.Create;
  try
    lines.Add('packageKind: git');
    lines.Add('metadata:');
    lines.Add('  id: ' + id);
    lines.Add('  description: A git registry test package');
    lines.Add('  authors:');
    lines.Add('    - Vincent Parrett');
    lines.Add('  license: Apache-2.0');
    lines.Add('  repositoryUrl: ' + repositoryUrl);
    lines.Add('targetPlatforms:');
    lines.Add('  - compiler: 12.0');
    lines.Add('    platforms: [Win32, Win64]');
    lines.Add('    template: default');
    lines.Add('templates:');
    lines.Add('  - name: default');
    if withDependency then
    begin
      lines.Add('    dependencies:');
      lines.Add('      - id: Some.Dependency');
      lines.Add('        version: "[1.0.0,2.0.0)"');
    end;
    lines.Add('    source:');
    lines.Add('      - src: .\source\*.pas');
    lines.Add('        dest: src');
    //SaveToFile uses platform (CRLF) line endings.
    lines.SaveToFile(TPath.Combine(folder, id + '.dspec.yaml'));
  finally
    lines.Free;
  end;
end;

function FolderKey(const folder : string) : string;
begin
  result := LowerCase(ExcludeTrailingPathDelimiter(folder));
end;

{ TRecordingGitClient }

constructor TRecordingGitClient.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FClonedUrls := TStringList.Create;
  FCloneTargets := TStringList.Create;
  FOrigins := TCollections.CreateDictionary<string, string>;
end;

destructor TRecordingGitClient.Destroy;
begin
  FClonedUrls.Free;
  FCloneTargets.Free;
  FLock.Free;
  inherited;
end;

procedure TRecordingGitClient.SetOrigin(const folder : string; const url : string);
begin
  FLock.Enter;
  try
    if url = '' then
      FOrigins.Remove(FolderKey(folder))
    else
      FOrigins[FolderKey(folder)] := url;
  finally
    FLock.Leave;
  end;
end;

function TRecordingGitClient.Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
begin
  result := true;
end;

function TRecordingGitClient.Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
begin
  if FCloneDelayMs > 0 then
    Sleep(FCloneDelayMs);
  FLock.Enter;
  try
    FClonedUrls.Add(url);
    FCloneTargets.Add(targetDir);
  finally
    FLock.Leave;
  end;
  if FFailClone then
    exit(false);
  //as git does - it won't clone into a folder that has something in it.
  if TDirectory.Exists(targetDir) and (not TDirectory.IsEmpty(targetDir)) then
    exit(false);
  TDirectory.CreateDirectory(TPath.Combine(targetDir, '.git'));
  WriteRegistryEntry(targetDir, cPackageId, cPackageUrl, false);
  WriteRegistryEntry(targetDir, 'Test.Beta', 'https://example.invalid/Test.Beta.git', false);
  WriteRegistryEntry(targetDir, 'Test.Alpha', 'https://example.invalid/Test.Alpha.git', false);
  SetOrigin(targetDir, url);
  result := true;
end;

function TRecordingGitClient.GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
begin
  FLock.Enter;
  try
    result := FOrigins.TryGetValue(FolderKey(repoDir), url);
  finally
    FLock.Leave;
  end;
end;

function TRecordingGitClient.IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
begin
  result := true;
end;

function TRecordingGitClient.LastError : string;
begin
  result := 'fatal: repository not found';
end;

function TRecordingGitClient.LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
begin
  commit := '1111111111111111111111111111111111111111';
  result := true;
end;

function TRecordingGitClient.LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
begin
  FLock.Enter;
  try
    Inc(FTagListings);
    if FFailTagListings > 0 then
    begin
      Dec(FFailTagListings);
      tags := nil;
      exit(false);
    end;
  finally
    FLock.Leave;
  end;
  tags := TCollections.CreateDictionary<string, string>;
  tags['v1.0.0'] := '2222222222222222222222222222222222222222';
  tags['1.1.0'] := '3333333333333333333333333333333333333333';
  result := true;
end;

function TRecordingGitClient.Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
begin
  FLock.Enter;
  try
    Inc(FPulls);
  finally
    FLock.Leave;
  end;
  result := true;
end;

{ TFailingGitClient }

function TFailingGitClient.Checkout(const cancellationToken : ICancellationToken; const repoDir : string; const commitOrRef : string) : boolean;
begin
  result := false;
end;

function TFailingGitClient.Clone(const cancellationToken : ICancellationToken; const url : string; const targetDir : string) : boolean;
begin
  result := false;
end;

function TFailingGitClient.GetRemoteUrl(const cancellationToken : ICancellationToken; const repoDir : string; out url : string) : boolean;
begin
  url := '';
  result := false;
end;

function TFailingGitClient.IsGitAvailable(const cancellationToken : ICancellationToken) : boolean;
begin
  result := true;
end;

function TFailingGitClient.LastError : string;
begin
  result := 'fatal: repository not found';
end;

function TFailingGitClient.LsRemoteHead(const cancellationToken : ICancellationToken; const url : string; const ref : string; out commit : string) : boolean;
begin
  commit := '';
  result := false;
end;

function TFailingGitClient.LsRemoteTags(const cancellationToken : ICancellationToken; const url : string; out tags : IDictionary<string, string>) : boolean;
begin
  tags := nil;
  result := false;
end;

function TFailingGitClient.Pull(const cancellationToken : ICancellationToken; const repoDir : string) : boolean;
begin
  result := false;
end;

{ TRegistryCatalogTests }

procedure TRegistryCatalogTests.WritePackageFolder(const id : string; const repositoryUrl : string; const withDependency : boolean);
begin
  WriteRegistryEntry(FRootDir, id, repositoryUrl, withDependency);
end;

procedure TRegistryCatalogTests.Setup;
begin
  FRootDir := TPath.Combine(TPath.GetTempPath, 'dpm_catalog_test');
  if TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
  TDirectory.CreateDirectory(FRootDir);
  WritePackageFolder('Test.GitPackage', 'https://github.com/example/Test.GitPackage.git', true);
  WritePackageFolder('Test.Other', 'https://github.com/example/Test.Other.git', false);
end;

procedure TRegistryCatalogTests.TearDown;
begin
  if (FRootDir <> '') and TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
end;

function CreateCatalog(const rootDir : string) : IRegistryCatalog;
var
  logger : ILogger;
  gitClient : IGitClient;
begin
  logger := TTestLogger.Create;
  gitClient := TGitClient.Create(logger);
  //folder source - git client is unused, registries folder irrelevant.
  result := TRegistryCatalog.Create(logger, gitClient, 'testreg', rootDir, '', 60);
end;

procedure TRegistryCatalogTests.FolderRegistry_LoadsPackageSpec;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  spec : IPackageSpec;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  spec := catalog.GetPackageSpec(ct, 'Test.GitPackage');
  Assert.IsNotNull(spec, 'spec should be found');
  Assert.AreEqual('Test.GitPackage', spec.MetaData.Id);
  Assert.IsTrue(spec.PackageKind = TDPMPackageKind.git, 'packageKind should be git');
  Assert.AreEqual('https://github.com/example/Test.GitPackage.git', spec.MetaData.RepositoryUrl);
end;

procedure TRegistryCatalogTests.GetPackageSpec_IsCaseInsensitive;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  Assert.IsNotNull(catalog.GetPackageSpec(ct, 'test.gitpackage'), 'lookup should be case-insensitive');
end;

procedure TRegistryCatalogTests.GetPackageSpec_UnknownId_ReturnsNil;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  Assert.IsNull(catalog.GetPackageSpec(ct, 'No.Such.Package'));
end;

procedure TRegistryCatalogTests.GetPackageIds_ReturnsAllPackages;
var
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  ids : IList<string>;
begin
  catalog := CreateCatalog(FRootDir);
  ct := TCancellationTokenSourceFactory.Create.Token;
  ids := catalog.GetPackageIds(ct);
  Assert.AreEqual(2, ids.Count);
end;

procedure TRegistryCatalogTests.GitRegistry_ExplicitRefreshFailure_IsAnError;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  catalog := TRegistryCatalog.Create(loggerIntf, TFailingGitClient.Create, 'gitreg', 'https://example.invalid/registry.git',
                                     TPath.Combine(FRootDir, 'registries'), 60);
  //dpm sources refresh - the refresh is what was asked for, so its failure is the error.
  Assert.IsFalse(catalog.EnsureUpdated(TCancellationTokenSourceFactory.Create.Token, true));
  Assert.IsTrue(logger.HasError(TErrorCode.E1203_GitCommandFailed), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'repository not found'), 'the error says what git said : ' + logger.Messages.Text);
  Assert.IsFalse(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
end;

procedure TRegistryCatalogTests.GitRegistry_CloneFailureDuringAQuery_IsAWarning;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  catalog := TRegistryCatalog.Create(loggerIntf, TFailingGitClient.Create, 'gitreg', 'https://example.invalid/registry.git',
                                     TPath.Combine(FRootDir, 'registries'), 60);
  //a query may find the package in another source - the one that needed it decides.
  Assert.IsFalse(catalog.EnsureUpdated(TCancellationTokenSourceFactory.Create.Token, false));
  Assert.IsTrue(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
end;

{ TRegistryMirrorTests }

procedure TRegistryMirrorTests.Setup;
begin
  FRootDir := TPath.Combine(TPath.GetTempPath, 'dpm_mirror_test');
  if TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
  FRegistriesDir := TPath.Combine(FRootDir, 'registries');
  TDirectory.CreateDirectory(FRegistriesDir);
end;

procedure TRegistryMirrorTests.TearDown;
begin
  if (FRootDir <> '') and TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
end;

procedure TRegistryMirrorTests.Same_Name_Different_Url_Gets_Its_Own_Mirror;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  loggerIntf : ILogger;
  first : IRegistryCatalog;
  second : IRegistryCatalog;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  loggerIntf := TTestLogger.Create;
  ct := TCancellationTokenSourceFactory.Create.Token;
  first := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsTrue(first.EnsureUpdated(ct, false));
  second := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', 'https://example.invalid/another.git', FRegistriesDir, 60);
  Assert.IsTrue(second.EnsureUpdated(ct, false));

  Assert.AreEqual(2, git.ClonedUrls.Count, 'each url is cloned : ' + git.ClonedUrls.CommaText);
  Assert.AreNotEqual(FolderKey(git.CloneTargets[0]), FolderKey(git.CloneTargets[1]), 'into a folder of its own');
end;

procedure TRegistryMirrorTests.Mirror_Of_Another_Url_Is_Cloned_Again;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  mirror : string;
  origin : string;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  loggerIntf := TTestLogger.Create;
  ct := TCancellationTokenSourceFactory.Create.Token;
  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsTrue(catalog.EnsureUpdated(ct, false));
  mirror := git.CloneTargets[0];
  git.SetOrigin(mirror, 'https://example.invalid/something-else.git');

  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsTrue(catalog.EnsureUpdated(ct, false));
  Assert.AreEqual(2, git.ClonedUrls.Count, 'cloned again : ' + git.ClonedUrls.CommaText);
  Assert.AreEqual(cRegistryUrl, git.ClonedUrls[1]);
  Assert.AreEqual(FolderKey(mirror), FolderKey(git.CloneTargets[1]), 'into the same folder');
  Assert.IsTrue(gitIntf.GetRemoteUrl(ct, mirror, origin));
  Assert.AreEqual(cRegistryUrl, origin);
  Assert.IsNotNull(catalog.GetPackageSpec(ct, cPackageId), 'and it is the registry that is read');
end;

procedure TRegistryMirrorTests.Mirror_Whose_Origin_Cannot_Be_Read_Is_Kept;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  loggerIntf := TTestLogger.Create;
  ct := TCancellationTokenSourceFactory.Create.Token;
  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsTrue(catalog.EnsureUpdated(ct, false));
  git.SetOrigin(git.CloneTargets[0], '');

  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsTrue(catalog.EnsureUpdated(ct, false));
  Assert.AreEqual(1, git.ClonedUrls.Count, git.ClonedUrls.CommaText);
  Assert.IsNotNull(catalog.GetPackageSpec(ct, cPackageId));
end;

procedure TRegistryMirrorTests.Failed_Clone_Is_Not_Retried_Straight_Away;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  git.FailClone := true;
  logger := TTestLogger.Create;
  loggerIntf := logger;
  ct := TCancellationTokenSourceFactory.Create.Token;
  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsFalse(catalog.EnsureUpdated(ct, false));
  loggerIntf.Clear;

  Assert.IsFalse(catalog.EnsureUpdated(ct, false));
  Assert.AreEqual(1, git.ClonedUrls.Count, 'git is not asked again');
  //each query that couldn't use the source still says so, and why.
  Assert.IsTrue(logger.Logged('warning', 'repository not found'), logger.Messages.Text);
  Assert.AreEqual(2, logger.WarningCount, logger.Messages.Text);
end;

procedure TRegistryMirrorTests.Explicit_Refresh_Retries_A_Failed_Clone;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  logger : TTestLogger;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  git.FailClone := true;
  logger := TTestLogger.Create;
  loggerIntf := logger;
  ct := TCancellationTokenSourceFactory.Create.Token;
  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  Assert.IsFalse(catalog.EnsureUpdated(ct, false));

  //dpm sources refresh - asked for, so it is tried.
  Assert.IsFalse(catalog.EnsureUpdated(ct, true));
  Assert.AreEqual(2, git.ClonedUrls.Count);
  Assert.IsTrue(logger.HasError(TErrorCode.E1203_GitCommandFailed), logger.Messages.Text);

  git.FailClone := false;
  Assert.IsTrue(catalog.EnsureUpdated(ct, true));
  Assert.IsNotNull(catalog.GetPackageSpec(ct, cPackageId));
end;

procedure TRegistryMirrorTests.Failed_Clone_Is_Retried_Once_The_Wait_Is_Over;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  ct : ICancellationToken;
  oldRetrySeconds : integer;
begin
  oldRetrySeconds := TRegistryCatalog.CloneRetrySeconds;
  TRegistryCatalog.CloneRetrySeconds := 0;
  try
    git := TRecordingGitClient.Create;
    gitIntf := git;
    git.FailClone := true;
    loggerIntf := TTestLogger.Create;
    ct := TCancellationTokenSourceFactory.Create.Token;
    catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
    Assert.IsFalse(catalog.EnsureUpdated(ct, false));
    Assert.IsFalse(catalog.EnsureUpdated(ct, false));
    Assert.AreEqual(2, git.ClonedUrls.Count);

    git.FailClone := false;
    Assert.IsTrue(catalog.EnsureUpdated(ct, false));
    Assert.IsNotNull(catalog.GetPackageSpec(ct, cPackageId));
  finally
    TRegistryCatalog.CloneRetrySeconds := oldRetrySeconds;
  end;
end;

procedure TRegistryMirrorTests.Concurrent_First_Use_Clones_Once;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  loggerIntf : ILogger;
  catalog : IRegistryCatalog;
  firstThread : TThread;
  secondThread : TThread;
  firstFound : boolean;
  secondFound : boolean;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  //long enough that the second thread asks while the first is still cloning.
  git.CloneDelayMs := 300;
  loggerIntf := TTestLogger.Create;
  catalog := TRegistryCatalog.Create(loggerIntf, gitIntf, 'reg', cRegistryUrl, FRegistriesDir, 60);
  firstFound := false;
  secondFound := false;
  firstThread := TThread.CreateAnonymousThread(
    procedure
    begin
      firstFound := catalog.GetPackageSpec(TCancellationTokenSourceFactory.Create.Token, cPackageId) <> nil;
    end);
  firstThread.FreeOnTerminate := false;
  secondThread := TThread.CreateAnonymousThread(
    procedure
    begin
      secondFound := catalog.GetPackageSpec(TCancellationTokenSourceFactory.Create.Token, cPackageId) <> nil;
    end);
  secondThread.FreeOnTerminate := false;
  try
    firstThread.Start;
    secondThread.Start;
    firstThread.WaitFor;
    secondThread.WaitFor;
  finally
    firstThread.Free;
    secondThread.Free;
  end;
  Assert.AreEqual(1, git.ClonedUrls.Count, git.ClonedUrls.CommaText);
  Assert.IsTrue(firstFound and secondFound, 'both threads see the package');
end;

{ TGitRegistryRepositoryTests }

procedure TGitRegistryRepositoryTests.Setup;
begin
  FRootDir := TPath.Combine(TPath.GetTempPath, 'dpm_gitregistry_test');
  if TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
  TDirectory.CreateDirectory(FRootDir);
end;

procedure TGitRegistryRepositoryTests.TearDown;
begin
  if (FRootDir <> '') and TDirectory.Exists(FRootDir) then
    TDirectory.Delete(FRootDir, true);
end;

procedure TGitRegistryRepositoryTests.Configure(const repository : IPackageRepository; const sourceUrl : string);
var
  source : ISourceConfig;
begin
  source := TSourceConfig.Create('gitreg', sourceUrl, TSourceType.GitRegistry, '', '', true);
  repository.Configure(source);
end;

function TGitRegistryRepositoryTests.CreateRepository(const git : IGitClient; const sourceUrl : string) : IPackageRepository;
var
  repository : TGitRegistryPackageRepository;
  loggerIntf : ILogger;
begin
  loggerIntf := TTestLogger.Create;
  repository := TGitRegistryPackageRepository.Create(loggerIntf, git);
  result := repository;
  //never the real %APPDATA%\.dpm\registries.
  repository.RegistriesFolder := TPath.Combine(FRootDir, 'registries');
  Configure(result, sourceUrl);
end;

procedure TGitRegistryRepositoryTests.Configuring_The_Same_Source_Keeps_The_Catalog;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  ct := TCancellationTokenSourceFactory.Create.Token;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  //always pull - a catalog loading the registry again would pull it.
  (repository as IGitRegistryRepository).SetRefreshIntervalMinutes(0);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);

  Configure(repository, cRegistryUrl);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(1, git.ClonedUrls.Count);
  Assert.AreEqual(0, git.Pulls, 'the registry is not loaded again');
end;

procedure TGitRegistryRepositoryTests.Configuring_Another_Source_Uses_Its_Registry;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  ct := TCancellationTokenSourceFactory.Create.Token;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);

  Configure(repository, 'https://example.invalid/another.git');
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, git.ClonedUrls.Count, git.ClonedUrls.CommaText);
  Assert.AreEqual('https://example.invalid/another.git', git.ClonedUrls[1]);
end;

procedure TGitRegistryRepositoryTests.Tags_Are_Listed_Once_For_Repeated_Queries;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  ct : ICancellationToken;
  latest : IPackageInfo;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  ct := TCancellationTokenSourceFactory.Create.Token;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(1, repository.GetPackageVersionsWithDependencies(ct, cPackageId, TCompilerVersion.Delphi12_0,
                                                                   TVersionRange.Parse('1.1.0'), false).Count);
  latest := repository.FindLatestVersion(ct, cPackageId, TCompilerVersion.Delphi12_0, TPackageVersion.Empty, false);
  Assert.IsNotNull(latest);
  Assert.AreEqual('1.1.0', latest.Version.ToStringNoMeta);
  Assert.AreEqual(1, git.TagListings);
end;

procedure TGitRegistryRepositoryTests.A_Failed_Tag_Listing_Is_Not_Remembered;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  git.FailTagListings := 1;
  ct := TCancellationTokenSourceFactory.Create.Token;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  Assert.AreEqual(0, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, git.TagListings);
end;

procedure TGitRegistryRepositoryTests.Tags_Are_Not_Remembered_When_The_Refresh_Interval_Is_Zero;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  ct : ICancellationToken;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  ct := TCancellationTokenSourceFactory.Create.Token;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  (repository as IGitRegistryRepository).SetRefreshIntervalMinutes(0);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, repository.GetPackageVersions(ct, cPackageId, TCompilerVersion.Delphi12_0, false).Count);
  Assert.AreEqual(2, git.TagListings);
end;

procedure TGitRegistryRepositoryTests.Feed_Honours_Skip_And_Take;
var
  git : TRecordingGitClient;
  gitIntf : IGitClient;
  repository : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  git := TRecordingGitClient.Create;
  gitIntf := git;
  repository := CreateRepository(gitIntf, cRegistryUrl);
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := TCompilerVersion.Delphi12_0;
    options.Skip := 1;
    options.Take := 1;
    feed := repository.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  //Test.Alpha, Test.Beta, Test.GitPackage.
  Assert.AreEqual(1, feed.Results.Count);
  Assert.AreEqual('Test.Beta', feed.Results[0].Id);
  Assert.AreEqual('1.1.0', feed.Results[0].LatestVersion.ToStringNoMeta);
  Assert.AreEqual(Int64(3), feed.TotalCount);
  Assert.AreEqual(1, git.TagListings, 'only the entry on the page has its tags listed');
end;

initialization
  TDUnitX.RegisterTestFixture(TRegistryCatalogTests);
  TDUnitX.RegisterTestFixture(TRegistryMirrorTests);
  TDUnitX.RegisterTestFixture(TGitRegistryRepositoryTests);

end.
