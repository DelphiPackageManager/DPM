unit DPM.Core.Tests.Repository.Manager;

// The repository manager is shared by the IDE's background requests (TAsync), so it is used from several
// threads at once : the repository list is only rebuilt when the configuration changes, a repository is never
// reconfigured while a query may be using it, and what one query found (a source's error, the sources that
// could not be used) is that thread's.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TRepositoryManagerTests = class
  public
    [Test]
    procedure Unchanged_Configuration_Keeps_Its_Repositories;
    [Test]
    procedure Changed_Source_Gets_A_Fresh_Repository;
    [Test]
    procedure Removed_Source_Is_Dropped;
    [Test]
    procedure Repository_LastError_Belongs_To_The_Thread;
    [Test]
    procedure Unavailable_Sources_Belong_To_The_Thread;
    //dpm search --skip/--take across several sources - the page is of the merged list, not of each source's.
    [Test]
    procedure Feed_From_Several_Sources_Pages_The_Merged_List;
    [Test]
    procedure Feed_From_Several_Sources_Without_A_Take_Skips_The_Merged_List;
    //one source pages itself (a server, the whole gallery) - it is asked for just the page, and its total is the total.
    [Test]
    procedure Feed_From_One_Source_Is_Paged_By_The_Source;
    //a disabled source nothing uses failed every operation when its uri wasn't valid.
    [Test]
    procedure Invalid_Disabled_Source_Is_A_Warning;
    //dpm mcp --source - the versions query asks only the sources named.
    [Test]
    procedure Versions_Query_Asks_Only_The_Sources_Named;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base,
  DPM.Core.Repository.Manager,
  DPM.Core.Package.SearchResults,
  TestLogger;

type
  //counts how often it is configured; a source named Bad* fails every request.
  TCountingSource = class(TBaseRepository, IPackageRepository)
  private
    FConfigureCount : integer;
    FFeedSkip : integer;
    FFeedTake : integer;
    function Answer : boolean;
  protected
    procedure Configure(const source : ISourceConfig); override;
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion;  const includePrerelease : boolean) : IPackageInfo;
    function DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;
    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
  public
    //a request on this thread failing.
    procedure Fail(const reason : string);
    property ConfigureCount : integer read FConfigureCount;
    //what the last feed request asked for.
    property FeedSkip : integer read FFeedSkip;
    property FeedTake : integer read FFeedTake;
  end;

  //hands out a new counting source each time, and remembers them.
  TCountingFactory = class(TInterfacedObject, IPackageRepositoryFactory)
  private
    FCreated : IList<IPackageRepository>;
  protected
    function CreateRepository(const repoType : TSourceType) : IPackageRepository;
  public
    constructor Create;
    property Created : IList<IPackageRepository> read FCreated;
  end;

  //reaches the manager's protected repository lookup.
  TManagerAccess = class(TPackageRepositoryManager);

  //runs proc on a thread of its own.
  TCallThread = class(TThread)
  private
    FProc : TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const proc : TProc);
  end;

var
  //the ids each source's feed has (by source name), in its order - pages of it are served as a server would.
  SourceFeeds : IDictionary<string, TArray<string>>;

{ TCountingSource }

function TCountingSource.Answer : boolean;
begin
  result := Copy(Name, 1, 3) <> 'Bad';
  if not result then
    RequestFailed('could not be reached : timed out');
end;

procedure TCountingSource.Configure(const source : ISourceConfig);
begin
  Inc(FConfigureCount);
  inherited;
end;

procedure TCountingSource.Fail(const reason : string);
begin
  RequestFailed(reason);
end;

function TCountingSource.DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
begin
  result := false;
end;

function TCountingSource.FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;
begin
  result := nil;
  Answer;
end;

function TCountingSource.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
begin
  result := nil;
end;

function TCountingSource.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;
  Answer;
end;

function TCountingSource.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;

function TCountingSource.GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
begin
  result := nil;
end;

function TCountingSource.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
begin
  result := nil;
end;

function TCountingSource.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
var
  ids : TArray<string>;
  i : integer;
begin
  FFeedSkip := options.Skip;
  FFeedTake := options.Take;
  if (SourceFeeds = nil) or (not SourceFeeds.TryGetValue(Name, ids)) then
    SetLength(ids, 0);
  result := TDPMPackageSearchResult.Create(options.Skip, Length(ids));
  for i := options.Skip to Length(ids) - 1 do
  begin
    if (options.Take > 0) and (result.Results.Count >= options.Take) then
      break;
    result.Results.Add(TDPMPackageSearchResultItem.FromError(ids[i], TPackageVersion.Parse('1.0.0'), compilerVersion, ''));
  end;
end;

function TCountingSource.GetPackageFeedByIds(const cancellationToken : ICancellationToken; const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
begin
  result := nil;
end;

function TCountingSource.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
begin
  result := TCollections.CreateList<IPackageListItem>;
end;

function TCountingSource.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
begin
  result := false;
end;

{ TCountingFactory }

constructor TCountingFactory.Create;
begin
  inherited Create;
  FCreated := TCollections.CreateList<IPackageRepository>;
end;

function TCountingFactory.CreateRepository(const repoType : TSourceType) : IPackageRepository;
begin
  result := TCountingSource.Create(TTestLogger.Create);
  FCreated.Add(result);
end;

{ TCallThread }

constructor TCallThread.Create(const proc : TProc);
begin
  FProc := proc;
  //not suspended - TThread starts it in AfterConstruction, once this has run.
  inherited Create(false);
end;

procedure TCallThread.Execute;
begin
  FProc();
end;

{ helpers }

function NewConfig(const sourceNames : array of string) : IConfiguration;
var
  i : integer;
begin
  result := TConfiguration.Create(TTestLogger.Create);
  for i := 0 to High(sourceNames) do
    result.Sources.Add(TSourceConfig.Create(sourceNames[i], 'https://' + LowerCase(sourceNames[i]) + '.example/index.json', TSourceType.DPMServer, '', '', true));
end;

function ConfigureCountOf(const repo : IPackageRepository) : integer;
begin
  result := (repo as TCountingSource).ConfigureCount;
end;

function AnyQuery(const manager : IPackageRepositoryManager) : IList<TPackageVersion>;
begin
  //every query brings the repositories up to date with the configuration first.
  result := manager.GetPackageVersions(TCancellationTokenSourceFactory.Create.Token, TCompilerVersion.Delphi12_0, 'Some.Package', false);
end;

{ TRepositoryManagerTests }

procedure TRepositoryManagerTests.Unchanged_Configuration_Keeps_Its_Repositories;
var
  factory : TCountingFactory;
  factoryIntf : IPackageRepositoryFactory;
  manager : IPackageRepositoryManager;
begin
  factory := TCountingFactory.Create;
  factoryIntf := factory;
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, factoryIntf);
  Assert.IsTrue(manager.Initialize(NewConfig(['One', 'Two'])));
  AnyQuery(manager);
  AnyQuery(manager);

  //nothing changed, so nothing is created or configured again - a query on another thread may be using them.
  Assert.AreEqual(2, factory.Created.Count);
  Assert.AreEqual(1, ConfigureCountOf(factory.Created[0]), 'One was reconfigured');
  Assert.AreEqual(1, ConfigureCountOf(factory.Created[1]), 'Two was reconfigured');
end;

procedure TRepositoryManagerTests.Changed_Source_Gets_A_Fresh_Repository;
var
  factory : TCountingFactory;
  factoryIntf : IPackageRepositoryFactory;
  access : TManagerAccess;
  manager : IPackageRepositoryManager;
  config : IConfiguration;
  oldOne : IPackageRepository;
  oldTwo : IPackageRepository;
begin
  factory := TCountingFactory.Create;
  factoryIntf := factory;
  access := TManagerAccess.Create(TTestLogger.Create, factoryIntf);
  manager := access;
  config := NewConfig(['One', 'Two']);
  Assert.IsTrue(manager.Initialize(config));
  oldOne := access.GetRepositoryByName('One');
  oldTwo := access.GetRepositoryByName('Two');

  config.Sources[0].Source := 'https://moved.example/index.json';
  AnyQuery(manager);

  Assert.IsTrue(access.GetRepositoryByName('One') <> oldOne, 'One should be a new repository');
  Assert.AreEqual('https://moved.example/index.json', access.GetRepositoryByName('One').Source);
  //the one in use is left as it was.
  Assert.AreEqual(1, ConfigureCountOf(oldOne));
  Assert.AreEqual('https://one.example/index.json', oldOne.Source);
  Assert.IsTrue(access.GetRepositoryByName('Two') = oldTwo, 'Two did not change');
  Assert.AreEqual(3, factory.Created.Count);
end;

procedure TRepositoryManagerTests.Removed_Source_Is_Dropped;
var
  factoryIntf : IPackageRepositoryFactory;
  access : TManagerAccess;
  manager : IPackageRepositoryManager;
  config : IConfiguration;
begin
  factoryIntf := TCountingFactory.Create;
  access := TManagerAccess.Create(TTestLogger.Create, factoryIntf);
  manager := access;
  config := NewConfig(['One', 'Two']);
  Assert.IsTrue(manager.Initialize(config));

  config.Sources.Delete(1);
  AnyQuery(manager);

  Assert.IsNull(access.GetRepositoryByName('Two'));
  Assert.IsNotNull(access.GetRepositoryByName('One'));
end;

procedure TRepositoryManagerTests.Repository_LastError_Belongs_To_The_Thread;
var
  source : TCountingSource;
  repo : IPackageRepository;
  failed : TEvent;
  checked : TEvent;
  seenOnThread : string;
  thread : TCallThread;
begin
  source := TCountingSource.Create(TTestLogger.Create);
  repo := source;
  failed := TEvent.Create(nil, true, false, '');
  checked := TEvent.Create(nil, true, false, '');
  try
    seenOnThread := '';
    thread := TCallThread.Create(
      procedure
      begin
        source.Fail('could not be reached : timed out');
        failed.SetEvent;
        checked.WaitFor(10000);
        seenOnThread := repo.LastError;
      end);
    try
      Assert.IsTrue(failed.WaitFor(10000) = wrSignaled, 'the thread never got going');
      //a query starting on this thread clears this thread's error - not the one the other thread is about to read.
      repo.ClearLastError;
      Assert.AreEqual('', repo.LastError);
      checked.SetEvent;
      thread.WaitFor;
    finally
      thread.Free;
    end;
    Assert.AreEqual('could not be reached : timed out', seenOnThread);
  finally
    failed.Free;
    checked.Free;
  end;
end;

procedure TRepositoryManagerTests.Unavailable_Sources_Belong_To_The_Thread;
var
  manager : IPackageRepositoryManager;
  queried : TEvent;
  checked : TEvent;
  seenOnThread : string;
  thread : TCallThread;
begin
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, TCountingFactory.Create);
  Assert.IsTrue(manager.Initialize(NewConfig(['BadOne', 'Good'])));
  queried := TEvent.Create(nil, true, false, '');
  checked := TEvent.Create(nil, true, false, '');
  try
    seenOnThread := '';
    thread := TCallThread.Create(
      procedure
      begin
        manager.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Some.Package', TCompilerVersion.Delphi12_0,
                                  TPackageVersion.Empty, false, 'BadOne');
        queried.SetEvent;
        checked.WaitFor(10000);
        seenOnThread := manager.UnavailableSources;
      end);
    try
      Assert.IsTrue(queried.WaitFor(10000) = wrSignaled, 'the thread never got going');
      //another query, here, between the other thread's query and it asking what could not be used.
      manager.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Some.Package', TCompilerVersion.Delphi12_0,
                                TPackageVersion.Empty, false, 'Good');
      Assert.AreEqual('', manager.UnavailableSources, 'this thread''s query used only Good');
      checked.SetEvent;
      thread.WaitFor;
    finally
      thread.Free;
    end;
    Assert.AreEqual('BadOne', seenOnThread);
  finally
    queried.Free;
    checked.Free;
  end;
end;

function FeedIds(const feed : IPackageSearchResult) : string;
var
  item : IPackageSearchResultItem;
begin
  result := '';
  for item in feed.Results do
  begin
    if result <> '' then
      result := result + ',';
    result := result + item.Id;
  end;
end;

function SearchFeed(const manager : IPackageRepositoryManager; const skip : integer; const take : integer) : IPackageSearchResult;
var
  options : TSearchOptions;
begin
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := TCompilerVersion.Delphi12_0;
    options.Skip := skip;
    options.Take := take;
    result := manager.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
end;

procedure TRepositoryManagerTests.Feed_From_Several_Sources_Pages_The_Merged_List;
var
  factory : TCountingFactory;
  factoryIntf : IPackageRepositoryFactory;
  manager : IPackageRepositoryManager;
  feed : IPackageSearchResult;
begin
  factory := TCountingFactory.Create;
  factoryIntf := factory;
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, factoryIntf);
  Assert.IsTrue(manager.Initialize(NewConfig(['One', 'Two'])));
  SourceFeeds := TCollections.CreateDictionary<string, TArray<string>>;
  try
    SourceFeeds['One'] := TArray<string>.Create('A', 'C', 'E');
    SourceFeeds['Two'] := TArray<string>.Create('B', 'C', 'D');
    feed := SearchFeed(manager, 1, 2);
  finally
    SourceFeeds := nil;
  end;
  //A B C D E - the second page of two.
  Assert.AreEqual('B,C', FeedIds(feed));
  Assert.AreEqual(Int64(5), feed.TotalCount, 'what the sources have, C once');
  Assert.AreEqual(1, Integer(feed.Skip));
  Assert.AreEqual(0, (factory.Created[0] as TCountingSource).FeedSkip, 'each source is asked for everything up to the end of the page');
  Assert.AreEqual(3, (factory.Created[0] as TCountingSource).FeedTake);
end;

procedure TRepositoryManagerTests.Feed_From_Several_Sources_Without_A_Take_Skips_The_Merged_List;
var
  factory : TCountingFactory;
  factoryIntf : IPackageRepositoryFactory;
  manager : IPackageRepositoryManager;
  feed : IPackageSearchResult;
begin
  factory := TCountingFactory.Create;
  factoryIntf := factory;
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, factoryIntf);
  Assert.IsTrue(manager.Initialize(NewConfig(['One', 'Two'])));
  SourceFeeds := TCollections.CreateDictionary<string, TArray<string>>;
  try
    SourceFeeds['One'] := TArray<string>.Create('A', 'C', 'E');
    SourceFeeds['Two'] := TArray<string>.Create('B', 'C', 'D');
    feed := SearchFeed(manager, 2, 0);
  finally
    SourceFeeds := nil;
  end;
  Assert.AreEqual('C,D,E', FeedIds(feed));
  Assert.AreEqual(Int64(5), feed.TotalCount);
  Assert.AreEqual(0, (factory.Created[1] as TCountingSource).FeedSkip);
  Assert.AreEqual(0, (factory.Created[1] as TCountingSource).FeedTake);
end;

procedure TRepositoryManagerTests.Feed_From_One_Source_Is_Paged_By_The_Source;
var
  factory : TCountingFactory;
  factoryIntf : IPackageRepositoryFactory;
  manager : IPackageRepositoryManager;
  feed : IPackageSearchResult;
begin
  factory := TCountingFactory.Create;
  factoryIntf := factory;
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, factoryIntf);
  Assert.IsTrue(manager.Initialize(NewConfig(['One'])));
  SourceFeeds := TCollections.CreateDictionary<string, TArray<string>>;
  try
    SourceFeeds['One'] := TArray<string>.Create('A', 'B', 'C', 'D', 'E');
    feed := SearchFeed(manager, 2, 2);
  finally
    SourceFeeds := nil;
  end;
  Assert.AreEqual('C,D', FeedIds(feed));
  Assert.AreEqual(Int64(5), feed.TotalCount, 'the source''s total, not the page''s count');
  Assert.AreEqual(2, (factory.Created[0] as TCountingSource).FeedSkip, 'just the page is asked for');
  Assert.AreEqual(2, (factory.Created[0] as TCountingSource).FeedTake);
end;

procedure TRepositoryManagerTests.Invalid_Disabled_Source_Is_A_Warning;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
  manager : IPackageRepositoryManager;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := NewConfig(['Good']);
  config.Sources.Add(TSourceConfig.Create('Broken', 'ftp://nowhere.example', TSourceType.DPMServer, '', '', false));
  manager := TPackageRepositoryManager.Create(loggerIntf, TCountingFactory.Create);
  Assert.IsTrue(manager.Initialize(config), logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W1006_DisabledSourceInvalid), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'Broken'), 'it names the source : ' + logger.Messages.Text);
  AnyQuery(manager);
  Assert.AreEqual(1, logger.WarningCount, 'said once : ' + logger.Messages.Text);
end;

procedure TRepositoryManagerTests.Versions_Query_Asks_Only_The_Sources_Named;
var
  manager : IPackageRepositoryManager;
begin
  manager := TPackageRepositoryManager.Create(TTestLogger.Create, TCountingFactory.Create);
  Assert.IsTrue(manager.Initialize(NewConfig(['BadOne', 'Good'])));
  manager.GetPackageVersions(TCancellationTokenSourceFactory.Create.Token, TCompilerVersion.Delphi12_0, 'Some.Package', false, 'Good');
  Assert.AreEqual('', manager.UnavailableSources, 'BadOne was asked');
  manager.GetPackageVersions(TCancellationTokenSourceFactory.Create.Token, TCompilerVersion.Delphi12_0, 'Some.Package', false);
  Assert.AreEqual('BadOne', manager.UnavailableSources, 'precondition : unrestricted, it is asked');
end;

initialization
  TDUnitX.RegisterTestFixture(TRepositoryManagerTests);

end.
