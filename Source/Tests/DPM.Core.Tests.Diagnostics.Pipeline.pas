unit DPM.Core.Tests.Diagnostics.Pipeline;

interface

uses
  DUnitX.TestFramework;

type
  //A failure is reported once, by the code that found it, with its code - nothing above it adds another
  //error, and something that only might have been a problem is not an error at all. These drive each
  //origin directly and count what was reported.
  [TestFixture]
  TDiagnosticsPipelineTests = class
  private
    FTempFolder : string;
    function TempFile(const name : string; const content : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure Compiler_Not_Installed_Is_One_Error;

    [Test]
    procedure Missing_Project_Is_One_Error;
    [Test]
    procedure Invalid_Project_Xml_Is_One_Error;
    [Test]
    procedure Duplicate_Package_Reference_Fails_The_Load_With_One_Error;
    [Test]
    procedure Invalid_Group_Project_Is_One_Error;

    [Test]
    procedure Source_Down_Package_Found_Elsewhere_Is_A_Warning;
    [Test]
    procedure Sources_Down_Nothing_Found_Is_Only_Warnings;
    [Test]
    procedure Download_From_A_Fallback_Source_Is_A_Warning;
    [Test]
    procedure Download_Failing_Everywhere_Is_One_Error;
    [Test]
    procedure Download_Hash_Mismatch_Is_One_Hash_Error;
    [Test]
    procedure Invalid_Source_Is_Reported_Once;

    [Test]
    procedure Unsatisfiable_Dependency_Is_One_Error;
    [Test]
    procedure Unsatisfiable_Dependency_With_A_Source_Down_Says_So;

    [Test]
    procedure Failed_Plan_Without_Errors_Logs_Nothing;
    [Test]
    procedure Plan_Errors_Are_Logged_With_Their_Codes;

    [Test]
    procedure Invalid_Package_File_Name_Is_One_Error;
    [Test]
    procedure Unreadable_Build_Record_Is_Not_An_Error;
    [Test]
    procedure Demoted_Problems_Are_Kept_For_The_Caller;

    [Test]
    procedure Missing_Spec_Is_One_Error;
    [Test]
    procedure Unreadable_Spec_Is_One_Error;
    [Test]
    procedure Spec_Field_Errors_Each_Name_The_Spec;
    [Test]
    procedure Spec_Needing_A_Newer_DPM_Says_So;
    [Test]
    procedure Area_Exceptions_Carry_Their_Code;
    [Test]
    procedure Context_Is_Added_To_Warnings_And_Errors_Only;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  WinApi.ActiveX,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.InstallPlan,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base,
  DPM.Core.Repository.Manager,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.MSBuild,
  DPM.Core.Compiler.BOM,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Tests.Dependency.Fakes,
  TestLogger;

type
  //the compiler is never installed.
  TMissingCompilerEnvironment = class(TInterfacedObject, ICompilerEnvironmentProvider)
  protected
    function GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
  end;

  //A package source that either answers, or fails with a given reason.
  TFakeSource = class(TBaseRepository, IPackageRepository)
  private
    FFailReason : string;
    FFailCode : TErrorCode;
    FHasPackage : boolean;
  protected
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
    function Answer : boolean;
  public
    constructor CreateSource(const logger : ILogger; const hasPackage : boolean; const failReason : string; const failCode : TErrorCode = TErrorCode.E1201_PackageDownloadFailed);
  end;

  //hands out the sources in the order the configuration lists them.
  TFakeSourceFactory = class(TInterfacedObject, IPackageRepositoryFactory)
  private
    FSources : IList<IPackageRepository>;
  protected
    function CreateRepository(const repoType : TSourceType) : IPackageRepository;
  public
    constructor Create(const sources : array of IPackageRepository);
  end;

{ TMissingCompilerEnvironment }

function TMissingCompilerEnvironment.GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
begin
  raise EDPMError.Create(TErrorCode.E5001_CompilerNotInstalled, 'Delphi ' + CompilerToString(compilerVersion) + ' is not installed.');
end;

{ TFakeSource }

constructor TFakeSource.CreateSource(const logger : ILogger; const hasPackage : boolean; const failReason : string; const failCode : TErrorCode);
begin
  inherited Create(logger);
  FHasPackage := hasPackage;
  FFailReason := failReason;
  FFailCode := failCode;
end;

function TFakeSource.Answer : boolean;
begin
  result := FFailReason = '';
  if not result then
    RequestFailed(FFailReason, FFailCode);
end;

function TFakeSource.DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
begin
  result := Answer and FHasPackage;
  if result then
    fileName := 'downloaded.dpkg';
end;

function TFakeSource.FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;
begin
  result := nil;
  if Answer and FHasPackage then
    result := TPackageInfo.Create(Name, id, TPackageVersion.Parse('1.0.0'), compilerVersion, '', '');
end;

function TFakeSource.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
begin
  result := nil;
end;

function TFakeSource.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;
end;

function TFakeSource.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;

function TFakeSource.GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
begin
  result := nil;
end;

function TFakeSource.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
begin
  result := nil;
end;

function TFakeSource.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
begin
  result := nil;
end;

function TFakeSource.GetPackageFeedByIds(const cancellationToken : ICancellationToken; const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
begin
  result := nil;
end;

function TFakeSource.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
begin
  result := TCollections.CreateList<IPackageListItem>;
end;

function TFakeSource.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
begin
  result := false;
end;

{ TFakeSourceFactory }

constructor TFakeSourceFactory.Create(const sources : array of IPackageRepository);
var
  i : integer;
begin
  inherited Create;
  FSources := TCollections.CreateList<IPackageRepository>;
  for i := 0 to High(sources) do
    FSources.Add(sources[i]);
end;

function TFakeSourceFactory.CreateRepository(const repoType : TSourceType) : IPackageRepository;
begin
  result := FSources[0];
  FSources.Delete(0);
end;

{ helpers }

function NewConfig(const logger : ILogger; const sourceNames : array of string) : IConfiguration;
var
  i : integer;
begin
  result := TConfiguration.Create(logger);
  for i := 0 to High(sourceNames) do
    result.Sources.Add(TSourceConfig.Create(sourceNames[i], 'https://' + LowerCase(sourceNames[i]) + '.example/index.json', TSourceType.DPMServer, '', '', true));
end;

function NewManager(const logger : ILogger; const config : IConfiguration; const sources : array of IPackageRepository) : IPackageRepositoryManager;
begin
  result := TPackageRepositoryManager.Create(logger, TFakeSourceFactory.Create(sources));
  Assert.IsTrue(result.Initialize(config), 'the sources should be valid');
end;

function UnsourcedPackage(const id : string) : IPackageInfo;
begin
  //no source name - the manager tries every source for it.
  result := TPackageInfo.Create('', id, TPackageVersion.Parse('1.0.0'), cTestCompiler, '', '');
end;

{ TDiagnosticsPipelineTests }

procedure TDiagnosticsPipelineTests.FixtureSetup;
begin
  CoInitialize(nil);
  FTempFolder := TPath.Combine(TPath.GetTempPath, 'dpm-diagnostics-' + IntToHex(Random(MaxInt), 8));
  ForceDirectories(FTempFolder);
end;

procedure TDiagnosticsPipelineTests.FixtureTearDown;
begin
  if DirectoryExists(FTempFolder) then
    TDirectory.Delete(FTempFolder, true);
  CoUninitialize;
end;

function TDiagnosticsPipelineTests.TempFile(const name : string; const content : string) : string;
begin
  result := TPath.Combine(FTempFolder, name);
  TFile.WriteAllText(result, content, TEncoding.UTF8);
end;

procedure TDiagnosticsPipelineTests.Compiler_Not_Installed_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  compiler : ICompiler;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  compiler := TMSBuildCompiler.Create(loggerIntf, cTestCompiler, TDPMPlatform.Win32, TMissingCompilerEnvironment.Create);
  Assert.IsFalse(compiler.BuildProject(TCancellationTokenSourceFactory.Create.Token, TDPMPlatform.Win32, 'C:\nowhere\pkg.dproj', 'Release', TPackageVersion.Parse('1.0.0')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E5001_CompilerNotInstalled), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Missing_Project_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  editor : IProjectEditor;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  editor := TProjectEditor.Create(loggerIntf, nil, cTestCompiler);
  Assert.IsFalse(editor.LoadProject(TPath.Combine(FTempFolder, 'missing.dproj')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E4001_ProjectNotFound), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Invalid_Project_Xml_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  editor : IProjectEditor;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  editor := TProjectEditor.Create(loggerIntf, nil, cTestCompiler);
  Assert.IsFalse(editor.LoadProject(TempFile('broken.dproj', '<Project><PropertyGroup>')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E4003_ProjectLoadFailed), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'broken.dproj'), 'the error should name the file : ' + logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Duplicate_Package_Reference_Fails_The_Load_With_One_Error;
const
  cProject =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' + sLineBreak +
    '  <ProjectExtensions><DPM><Packages>' + sLineBreak +
    '    <PackageReference id="Same.Package" version="1.0.0"/>' + sLineBreak +
    '    <PackageReference id="Same.Package" version="1.0.0"/>' + sLineBreak +
    '  </Packages></DPM></ProjectExtensions>' + sLineBreak +
    '</Project>';
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  editor : IProjectEditor;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  editor := TProjectEditor.Create(loggerIntf, nil, cTestCompiler);
  //the duplicate is raised while reading the references - the load must fail, not report and succeed.
  Assert.IsFalse(editor.LoadProject(TempFile('duplicate.dproj', cProject), [TProjectElement.PackageRefs]));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E4004_InvalidPackageReference), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Invalid_Group_Project_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  reader : IGroupProjectReader;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  reader := TGroupProjectReader.Create(loggerIntf);
  Assert.IsFalse(reader.LoadGroupProj(TempFile('broken.groupproj', '<Project><ItemGroup>')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E4003_ProjectLoadFailed), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Source_Down_Package_Found_Elsewhere_Is_A_Warning;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : IPackageRepositoryManager;
  info : IPackageInfo;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := NewManager(loggerIntf, NewConfig(loggerIntf, ['Down', 'Up']),
                        [TFakeSource.CreateSource(loggerIntf, false, 'could not be reached : timed out'),
                         TFakeSource.CreateSource(loggerIntf, true, '')]);
  info := manager.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Some.Package', cTestCompiler, TPackageVersion.Empty, false, '');
  Assert.IsNotNull(info, 'the second source has it');
  Assert.AreEqual(0, loggerIntf.ErrorCount, 'a source that is down is not an error when another has the package : ' + logger.Messages.Text);
  Assert.AreEqual(1, logger.WarningCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'Source [Down] could not be reached : timed out'), logger.Messages.Text);
  Assert.AreEqual('Down', manager.UnavailableSources, false);
end;

procedure TDiagnosticsPipelineTests.Sources_Down_Nothing_Found_Is_Only_Warnings;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : IPackageRepositoryManager;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := NewManager(loggerIntf, NewConfig(loggerIntf, ['First', 'Second']),
                        [TFakeSource.CreateSource(loggerIntf, false, 'could not be reached : timed out'),
                         TFakeSource.CreateSource(loggerIntf, false, 'returned an error : 500')]);
  Assert.IsNull(manager.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Some.Package', cTestCompiler, TPackageVersion.Empty, false, ''));
  //whether finding nothing is an error is the caller's call - with UnavailableSources it can say why.
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual(2, logger.WarningCount, logger.Messages.Text);
  Assert.AreEqual('First, Second', manager.UnavailableSources, false);
end;

procedure TDiagnosticsPipelineTests.Download_From_A_Fallback_Source_Is_A_Warning;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : IPackageRepositoryManager;
  fileName : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := NewManager(loggerIntf, NewConfig(loggerIntf, ['Down', 'Up']),
                        [TFakeSource.CreateSource(loggerIntf, true, 'download failed : connection reset'),
                         TFakeSource.CreateSource(loggerIntf, true, '')]);
  Assert.IsTrue(manager.DownloadPackage(TCancellationTokenSourceFactory.Create.Token, UnsourcedPackage('Some.Package'), FTempFolder, fileName));
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual(1, logger.WarningCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W1004_SourceUnavailable), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Download_Failing_Everywhere_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : IPackageRepositoryManager;
  fileName : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := NewManager(loggerIntf, NewConfig(loggerIntf, ['First', 'Second']),
                        [TFakeSource.CreateSource(loggerIntf, true, 'download failed : connection reset'),
                         TFakeSource.CreateSource(loggerIntf, true, 'could not be reached : timed out')]);
  Assert.IsFalse(manager.DownloadPackage(TCancellationTokenSourceFactory.Create.Token, UnsourcedPackage('Some.Package'), FTempFolder, fileName));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual(0, logger.WarningCount, 'the sources that failed are part of the error, not warnings of their own : ' + logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E1201_PackageDownloadFailed), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'source [First] download failed : connection reset'), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'source [Second] could not be reached : timed out'), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Download_Hash_Mismatch_Is_One_Hash_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  manager : IPackageRepositoryManager;
  fileName : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  manager := NewManager(loggerIntf, NewConfig(loggerIntf, ['Only']),
                        [TFakeSource.CreateSource(loggerIntf, true, 'sent a package that does not match its recorded hash', TErrorCode.E1202_PackageHashMismatch)]);
  Assert.IsFalse(manager.DownloadPackage(TCancellationTokenSourceFactory.Create.Token, UnsourcedPackage('Some.Package'), FTempFolder, fileName));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E1202_PackageHashMismatch), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Invalid_Source_Is_Reported_Once;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  config : IConfiguration;
  manager : IPackageRepositoryManager;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  config := TConfiguration.Create(loggerIntf);
  config.Sources.Add(TSourceConfig.Create('Broken', 'ftp://nowhere.example', TSourceType.DPMServer, '', '', true));
  manager := TPackageRepositoryManager.Create(loggerIntf, TFakeSourceFactory.Create([]));
  Assert.IsFalse(manager.Initialize(config));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E1002_InvalidSource), logger.Messages.Text);
  //later requests just stop - Initialize said why.
  Assert.IsNull(manager.FindLatestVersion(TCancellationTokenSourceFactory.Create.Token, 'Some.Package', cTestCompiler, TPackageVersion.Empty, false, ''));
  Assert.AreEqual(1, loggerIntf.ErrorCount, 'the invalid source must not be reported again : ' + logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Unsatisfiable_Dependency_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cacheIntf : IPackageCache;
  contextIntf : IPackageInstallerContext;
  resolver : IDependencyResolver;
  options : TSearchOptions;
  graph : IPackageReference;
  resolved : IList<IPackageInfo>;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  cacheIntf := TFakePackageCache.Create;
  contextIntf := TFakeInstallerContext.Create(loggerIntf);
  resolver := TDependencyResolver.Create(loggerIntf, repoIntf, cacheIntf, contextIntf);
  resolver.Initialize(TConfiguration.Create(loggerIntf));
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := cTestCompiler;
    //nothing provides Missing.Dependency.
    Assert.IsFalse(resolver.ResolveForInstall(TCancellationTokenSourceFactory.Create.Token, cTestCompiler, cTestProject, options,
                                              MakeInfo('Some.Package', '1.0.0', [MakeDep('Missing.Dependency', '1.0.0')]),
                                              TCollections.CreateList<IPackageReference>, graph, resolved));
  finally
    options.Free;
  end;
  //the conflict, and no summary error after it.
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E2001_DependencyUnsatisfiable), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Unsatisfiable_Dependency_With_A_Source_Down_Says_So;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cacheIntf : IPackageCache;
  contextIntf : IPackageInstallerContext;
  resolver : IDependencyResolver;
  options : TSearchOptions;
  graph : IPackageReference;
  resolved : IList<IPackageInfo>;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  //the version lookups could not use this source.
  repo.Unavailable := 'Down';
  cacheIntf := TFakePackageCache.Create;
  contextIntf := TFakeInstallerContext.Create(loggerIntf);
  resolver := TDependencyResolver.Create(loggerIntf, repoIntf, cacheIntf, contextIntf);
  resolver.Initialize(TConfiguration.Create(loggerIntf));
  options := TSearchOptions.Create;
  try
    options.CompilerVersion := cTestCompiler;
    Assert.IsFalse(resolver.ResolveForInstall(TCancellationTokenSourceFactory.Create.Token, cTestCompiler, cTestProject, options,
                                              MakeInfo('Some.Package', '1.0.0', [MakeDep('Missing.Dependency', '1.0.0')]),
                                              TCollections.CreateList<IPackageReference>, graph, resolved));
  finally
    options.Free;
  end;
  //not a conflict - the versions could not all be looked at.
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E1102_PackageNotFoundSourcesUnavailable), logger.Messages.Text);
  Assert.IsFalse(logger.HasError(TErrorCode.E2001_DependencyUnsatisfiable), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'Down'), 'the error should name the source : ' + logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Failed_Plan_Without_Errors_Logs_Nothing;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  plan : IInstallPlan;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //the cause was reported where it happened (or it was cancelled) - the plan adds nothing.
  plan := CreateFailedPlan;
  Assert.IsFalse(plan.Succeeded);
  Assert.AreEqual(0, plan.Errors.Count);
  LogPlanErrors(loggerIntf, plan);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Plan_Errors_Are_Logged_With_Their_Codes;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  plan : IInstallPlan;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  plan := CreateFailedPlan(TErrorCode.E3004_PackageFileNotFound, 'The package file [x.dpkg] does not exist.');
  Assert.IsFalse(plan.Succeeded);
  LogPlanErrors(loggerIntf, plan);
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual('error: DPME3004: The package file [x.dpkg] does not exist.', logger.Messages[0], false);
end;

procedure TDiagnosticsPipelineTests.Invalid_Package_File_Name_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  identity : IPackageIdentity;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(TPackageIdentity.TryCreateFromString(loggerIntf, 'not-a-package-name', '', identity));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E3005_InvalidPackageFile), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Unreadable_Build_Record_Is_Not_An_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //a build record that can't be read just means the package is rebuilt.
  Assert.IsNull(TBOMFile.LoadFromFile(loggerIntf, TempFile('package.Win32.bom', '{ this is not json')));
  Assert.IsNull(TBOMFile.LoadFromFile(loggerIntf, TPath.Combine(FTempFolder, 'missing.bom')));
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual(0, logger.WarningCount, logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Demoted_Problems_Are_Kept_For_The_Caller;
var
  target : TTestLogger;
  targetIntf : ILogger;
  demoting : IDemotingLogger;
begin
  target := TTestLogger.Create;
  targetIntf := target;
  demoting := TDemotingLogger.Create(targetIntf);
  Assert.AreEqual('', demoting.FirstProblem);
  demoting.Error(TErrorCode.E5003_PackageCompileFailed, 'the first');
  demoting.Error(TErrorCode.E5003_PackageCompileFailed, 'the second');
  Assert.AreEqual('error DPME5003: the first', demoting.FirstProblem, false);
  Assert.AreEqual(0, targetIntf.ErrorCount, target.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Missing_Spec_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  reader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  reader := TPackageSpecReader.Create(loggerIntf);
  Assert.IsNull(reader.ReadSpec(TPath.Combine(FTempFolder, 'missing.dspec.yaml')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E6001_SpecNotFound), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Unreadable_Spec_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  reader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  reader := TPackageSpecReader.Create(loggerIntf);
  Assert.IsNull(reader.ReadSpec(TempFile('broken.dspec.yaml', 'metadata: [unclosed')));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E6002_SpecUnreadable), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Spec_Field_Errors_Each_Name_The_Spec;
const
  //no version, description or authors.
  cSpec =
    'metadata:' + sLineBreak +
    '  id: VSoft.Incomplete' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  specFile : string;
  i : integer;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  specFile := TempFile('incomplete.dspec.yaml', cSpec);
  reader := TPackageSpecReader.Create(loggerIntf);
  spec := reader.ReadSpec(specFile);
  Assert.IsNotNull(spec);
  Assert.IsFalse(spec.IsValid);
  //each missing field is its own error - independent problems, no summary after them.
  Assert.AreEqual(3, loggerIntf.ErrorCount, logger.Messages.Text);
  for i := 0 to logger.Diagnostics.Count - 1 do
    Assert.AreEqual('error DPME6003', logger.Diagnostics[i], false);
  //the spec's parts don't know the file - the reader adds it.
  for i := 0 to logger.Messages.Count - 1 do
    Assert.IsTrue(Pos('Spec [' + specFile + ']', logger.Messages[i]) > 0, logger.Messages[i]);
end;

procedure TDiagnosticsPipelineTests.Spec_Needing_A_Newer_DPM_Says_So;
const
  cSpec =
    'min dpm client version: 99.0.0' + sLineBreak +
    'metadata:' + sLineBreak +
    '  id: VSoft.FromTheFuture' + sLineBreak;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  reader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  reader := TPackageSpecReader.Create(loggerIntf);
  Assert.IsNull(reader.ReadSpec(TempFile('future.dspec.yaml', cSpec)));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E6005_SpecNeedsNewerDPM), logger.Messages.Text);
end;

procedure TDiagnosticsPipelineTests.Area_Exceptions_Carry_Their_Code;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  e : Exception;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  e := ECryptoX509.Create('bad certificate');
  try
    Assert.IsTrue(e is EDPMError);
    Assert.IsTrue(EDPMError(e).Code = TErrorCode.E7104_CertificateInvalid);
    LogException(loggerIntf, e);
    Assert.IsTrue(ErrorCodeOf(e, TErrorCode.E7102_SigningFailed) = TErrorCode.E7104_CertificateInvalid);
  finally
    e.Free;
  end;
  Assert.AreEqual('error: DPME7104: bad certificate', logger.Messages[0], false);
  e := Exception.Create('plain');
  try
    Assert.IsTrue(ErrorCodeOf(e, TErrorCode.E7102_SigningFailed) = TErrorCode.E7102_SigningFailed);
  finally
    e.Free;
  end;
end;

procedure TDiagnosticsPipelineTests.Context_Is_Added_To_Warnings_And_Errors_Only;
var
  target : TTestLogger;
  targetIntf : ILogger;
  context : ILogger;
begin
  target := TTestLogger.Create;
  targetIntf := target;
  context := TContextLogger.Create(targetIntf, 'Spec [x.dspec] : ');
  context.Information('reading');
  context.Warning(TWarningCode.W6104_PatternMatchedNoFiles, 'no files');
  context.Error(TErrorCode.E6003_SpecElementMissing, 'no version');
  Assert.AreEqual('information: reading', target.Messages[0], false);
  Assert.AreEqual('warning: DPMW6104: Spec [x.dspec] : no files', target.Messages[1], false);
  Assert.AreEqual('error: DPME6003: Spec [x.dspec] : no version', target.Messages[2], false);
  Assert.AreEqual(1, context.ErrorCount, 'counted by the logger it wraps');
  Assert.AreEqual(1, targetIntf.ErrorCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TDiagnosticsPipelineTests);

end.
