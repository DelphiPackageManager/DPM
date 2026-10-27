unit DPM.Core.Tests.Repository.Http;

// What a DPM server sends that isn't what DPM hoped for - a package it doesn't have, results it can't all read -
// is not the source failing. Served from a local Indy server, so the repository's own http client makes the
// requests.

interface

uses
  DUnitX.TestFramework,
  Spring.Collections,
  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer;

type
  [TestFixture]
  THttpRepositoryTests = class
  private
    FServer : TIdHTTPServer;
    FPort : integer;
    //lowercase path -> the json answered; anything else is a 404.
    FRoutes : IDictionary<string, string>;
    procedure HandleRequest(AContext : TIdContext; ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
    function Url(const path : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
    [Setup]
    procedure Setup;

    //a server without the package (it is on another source, say) - the source has answered, it hasn't failed.
    [Test]
    procedure Metadata_Of_A_Package_The_Server_Does_Not_Have_Is_Not_A_Source_Error;
    //one result DPM can't read made the whole page fail.
    [Test]
    procedure Search_Result_That_Cannot_Be_Read_Is_Skipped;
    [Test]
    procedure Search_By_Ids_Result_That_Cannot_Be_Read_Is_Skipped;
    [Test]
    procedure List_Entry_That_Cannot_Be_Read_Is_Skipped;
    //a dependency whose range DPM can't read was dropped without a word - the result is not one to rely on.
    [Test]
    procedure Search_Result_With_An_Unreadable_Dependency_Is_Skipped;
    //older servers don't send it.
    [Test]
    procedure Search_Result_Without_A_Latest_Version_Uses_Its_Version;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  IdSocketHandle,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Http,
  TestLogger;

const
  cGoodItem = '{"id":"Good.Pkg","version":"1.0.0","compiler":"12.0","latestVersion":"1.0.0","platforms":"Win32"}';
  cOtherItem = '{"id":"Other.Pkg","version":"2.0.0","compiler":"12.0","latestVersion":"2.0.0","platforms":"Win32"}';
  cBadVersionItem = '{"id":"Bad.Pkg","version":"not a version","compiler":"12.0","latestVersion":"1.0.0"}';
  cBadDependencyItem = '{"id":"Dep.Pkg","version":"1.0.0","compiler":"12.0","latestVersion":"1.0.0",' +
                       '"dependencies":[{"packageId":"Some.Dependency","versionRange":"[1.0.0,"}]}';

function ResultIds(const results : IList<IPackageSearchResultItem>) : string;
var
  item : IPackageSearchResultItem;
begin
  result := '';
  for item in results do
  begin
    if result <> '' then
      result := result + ',';
    result := result + item.Id;
  end;
end;

function UnreadableEntryWarnings(const logger : TTestLogger) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to logger.Diagnostics.Count - 1 do
    if logger.Diagnostics[i] = 'warning ' + WarningCodeToString(TWarningCode.W1207_PackageEntryUnreadable) then
      Inc(result);
end;

function NewRepository(const indexUrl : string; const logger : ILogger) : IPackageRepository;
var
  source : ISourceConfig;
begin
  result := TDPMServerPackageRepository.Create(logger);
  source := TSourceConfig.Create('gallery', indexUrl, TSourceType.DPMServer, '', '', true);
  result.Configure(source);
end;

function SearchOptions : TSearchOptions;
begin
  result := TSearchOptions.Create;
  result.CompilerVersion := TCompilerVersion.Delphi12_0;
end;

{ THttpRepositoryTests }

procedure THttpRepositoryTests.FixtureSetup;
var
  binding : TIdSocketHandle;
begin
  FRoutes := TCollections.CreateDictionary<string, string>;
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := HandleRequest;
  binding := FServer.Bindings.Add;
  binding.IP := '127.0.0.1';
  //any free port - read back once it is listening.
  binding.Port := 0;
  FServer.Active := true;
  FPort := FServer.Bindings[0].Port;
end;

procedure THttpRepositoryTests.FixtureTearDown;
begin
  FServer.Active := false;
  FServer.Free;
end;

procedure THttpRepositoryTests.Setup;
begin
  FRoutes.Clear;
  FRoutes['/index.json'] :=
    '{"version":"1.0.0","resources":[' +
    '{"@id":"' + Url('/api/v1/package') + '","@type":"PackageMetadata"},' +
    '{"@id":"' + Url('/api/v1/search') + '","@type":"PackageSearch"},' +
    '{"@id":"' + Url('/api/v1/searchbyids') + '","@type":"PackageSearchIds"},' +
    '{"@id":"' + Url('/api/v1/list') + '","@type":"PackageList"}]}';
end;

function THttpRepositoryTests.Url(const path : string) : string;
begin
  result := 'http://127.0.0.1:' + IntToStr(FPort) + path;
end;

procedure THttpRepositoryTests.HandleRequest(AContext : TIdContext; ARequestInfo : TIdHTTPRequestInfo; AResponseInfo : TIdHTTPResponseInfo);
var
  body : string;
begin
  AResponseInfo.ContentType := 'application/json';
  AResponseInfo.CharSet := 'utf-8';
  if FRoutes.TryGetValue(LowerCase(ARequestInfo.Document), body) then
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentText := body;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '{"error":"not found"}';
  end;
end;

procedure THttpRepositoryTests.Metadata_Of_A_Package_The_Server_Does_Not_Have_Is_Not_A_Source_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  Assert.IsNull(repository.GetPackageMetaData(TCancellationTokenSourceFactory.Create.Token, 'Missing.Pkg', '1.0.0', TCompilerVersion.Delphi12_0));
  Assert.AreEqual('', repository.LastError, 'the source answered');
  Assert.AreEqual(0, logger.WarningCount, logger.Messages.Text);
end;

procedure THttpRepositoryTests.Search_Result_That_Cannot_Be_Read_Is_Skipped;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  FRoutes['/api/v1/search'] := '{"totalHits":4,"results":[' + cGoodItem + ',' + cBadVersionItem + ',42,' + cOtherItem + ']}';
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  options := SearchOptions;
  try
    feed := repository.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  Assert.AreEqual('Good.Pkg,Other.Pkg', ResultIds(feed.Results));
  Assert.AreEqual(2, UnreadableEntryWarnings(logger), 'one for each it skipped : ' + logger.Messages.Text);
  Assert.AreEqual('', repository.LastError, 'the source answered');
end;

procedure THttpRepositoryTests.Search_By_Ids_Result_That_Cannot_Be_Read_Is_Skipped;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
  ids : IList<IPackageIdentity>;
  feed : IPackageSearchResult;
begin
  FRoutes['/api/v1/searchbyids'] := '{"totalHits":4,"results":[' + cGoodItem + ',' + cBadVersionItem + ',42,' + cOtherItem + ']}';
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  ids := TCollections.CreateList<IPackageIdentity>;
  ids.Add(TPackageIdentity.Create('gallery', 'Good.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.Delphi12_0));
  feed := repository.GetPackageFeedByIds(TCancellationTokenSourceFactory.Create.Token, ids, TCompilerVersion.Delphi12_0);
  Assert.AreEqual('Good.Pkg,Other.Pkg', ResultIds(feed.Results));
  Assert.AreEqual(2, UnreadableEntryWarnings(logger), logger.Messages.Text);
end;

procedure THttpRepositoryTests.List_Entry_That_Cannot_Be_Read_Is_Skipped;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
  options : TSearchOptions;
  items : IList<IPackageListItem>;
begin
  FRoutes['/api/v1/list'] := '{"results":[' +
    '{"id":"Good.Pkg","compiler":"12.0","version":"1.0.0","platforms":"Win32"},' +
    '{"id":"Bad.Pkg","compiler":"not a compiler","version":"1.0.0","platforms":"Win32"},' +
    '42,' +
    '{"id":"Other.Pkg","compiler":"12.0","version":"2.0.0","platforms":"Win32"}]}';
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  options := SearchOptions;
  try
    items := repository.List(TCancellationTokenSourceFactory.Create.Token, options);
  finally
    options.Free;
  end;
  Assert.AreEqual(2, items.Count, logger.Messages.Text);
  Assert.AreEqual('Other.Pkg', items[1].Id);
  Assert.AreEqual(2, UnreadableEntryWarnings(logger), logger.Messages.Text);
end;

procedure THttpRepositoryTests.Search_Result_With_An_Unreadable_Dependency_Is_Skipped;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  FRoutes['/api/v1/search'] := '{"totalHits":2,"results":[' + cBadDependencyItem + ',' + cGoodItem + ']}';
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  options := SearchOptions;
  try
    feed := repository.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  Assert.AreEqual('Good.Pkg', ResultIds(feed.Results));
  Assert.AreEqual(1, UnreadableEntryWarnings(logger), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'Some.Dependency'), 'it says which dependency : ' + logger.Messages.Text);
end;

procedure THttpRepositoryTests.Search_Result_Without_A_Latest_Version_Uses_Its_Version;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repository : IPackageRepository;
  options : TSearchOptions;
  feed : IPackageSearchResult;
begin
  FRoutes['/api/v1/search'] := '{"totalHits":1,"results":[{"id":"Old.Pkg","version":"1.2.0","compiler":"12.0","platforms":"Win32"}]}';
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repository := NewRepository(Url('/index.json'), loggerIntf);
  options := SearchOptions;
  try
    feed := repository.GetPackageFeed(TCancellationTokenSourceFactory.Create.Token, options, TCompilerVersion.Delphi12_0);
  finally
    options.Free;
  end;
  Assert.AreEqual('Old.Pkg', ResultIds(feed.Results), logger.Messages.Text);
  Assert.AreEqual('1.2.0', feed.Results[0].LatestVersion.ToStringNoMeta);
end;

initialization
  TDUnitX.RegisterTestFixture(THttpRepositoryTests);

end.
