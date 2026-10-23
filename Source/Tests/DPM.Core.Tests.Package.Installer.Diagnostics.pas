unit DPM.Core.Tests.Package.Installer.Diagnostics;

interface

uses
  DUnitX.TestFramework;

type
  //What a whole installer operation reports - one error per cause with nothing above the cause adding
  //another, and no errors at all for an operation that succeeded or was cancelled. These drive the real
  //installer, with the package source, the package cache, the configuration and the compiler faked.
  [TestFixture]
  TInstallerDiagnosticsTests = class
  private
    FTempFolder : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure Compile_Failure_Is_One_Error;
    [Test]
    procedure Successful_Build_Reports_No_Errors;
    [Test]
    procedure Unexpected_Exception_Is_One_Error;
    [Test]
    procedure Cancelled_Build_Is_One_Warning_And_No_Errors;
    [Test]
    procedure Two_Conflicts_Are_Two_Errors_And_Nothing_Else;
    [Test]
    procedure Restore_Conflict_On_Two_Platforms_Is_Reported_Once;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  WinApi.ActiveX,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.Installer,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Tests.Dependency.Fakes,
  TestLogger;

type
  TCompileOutcome = (coSucceed, coFail, coRaise, coCancel);

  //Stands in for msbuild : the build succeeds, fails (reporting it, as TMSBuildCompiler does), raises,
  //or is stopped part way by cancelling the operation.
  TFakeCompiler = class(TInterfacedObject, ICompiler)
  private
    FLogger : ILogger;
    FOutcome : TCompileOutcome;
    FCancellation : ICancellationTokenSource;
    FCompilerVersion : TCompilerVersion;
    FPlatform : TDPMPlatform;
    FConfiguration : string;
    FSearchPaths : IList<string>;
    FBPLOutput : string;
    FLibOutput : string;
    FVerbosity : TCompilerVerbosity;
    FOutput : TStringList;
  protected
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;
    function GetConfiguration : string;
    procedure SetConfiguration(const value : string);
    function GetSearchPaths : IList<string>;
    procedure SetSearchPaths(const value : IList<string>);
    function GetBPLOutput : string;
    procedure SetBPLOutput(const value : string);
    function GetLibOutput : string;
    procedure SetLibOutput(const value : string);
    function GetVerbosity : TCompilerVerbosity;
    procedure SetVerbosity(const value : TCompilerVerbosity);
    function GetCompilerOutput : TStrings;
    function BuildProject(const cancellationToken : ICancellationToken; const platform : TDPMPlatform; const projectFile : string;
                          const configName : string; const packageVersion : TPackageVersion; const forDesign : boolean = false) : boolean;
  public
    constructor Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource;
                       const compilerVersion : TCompilerVersion; const platform : TDPMPlatform);
    destructor Destroy; override;
  end;

  TFakeCompilerFactory = class(TInterfacedObject, ICompilerFactory)
  private
    FLogger : ILogger;
    FOutcome : TCompileOutcome;
    FCancellation : ICancellationTokenSource;
  protected
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler; overload;
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler; overload;
  public
    constructor Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource);
  end;

  //Every package is already in the cache, extracted to one folder, with the spec given.
  TExtractedPackageCache = class(TFakePackageCache, IPackageCache)
  private
    FPackageFolder : string;
    FSpec : IPackageSpec;
  protected
    function EnsurePackage(const packageId : IPackageIdentity) : boolean;
    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string; overload;
  public
    constructor Create(const packageFolder : string; const spec : IPackageSpec);
  end;

  //The configuration the installer loads - in memory, nothing is read from or written to the profile.
  TFakeConfigurationManager = class(TInterfacedObject, IConfigurationManager)
  private
    FConfig : IConfiguration;
  protected
    function LoadConfig(const configFile : string) : IConfiguration;
    function NewConfig : IConfiguration;
    function EnsureDefaultConfig : boolean;
    function SaveConfig(const configuration : IConfiguration; const fileName : string = '') : boolean;
  public
    constructor Create(const config : IConfiguration);
  end;

const
  cPackageId = 'VSoft.Compiles';

  cPackageSpec =
    'metadata:' + sLineBreak +
    '  id: ' + cPackageId + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: installer diagnostics test package' + sLineBreak +
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
    '      - src: .\source\*.pas' + sLineBreak +
    '        dest: source' + sLineBreak +
    '    build:' + sLineBreak +
    '      - project: .\source\Pkg.dproj' + sLineBreak;

  //nothing in it that DPM refuses to build.
  cPackageProject =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' + sLineBreak +
    '    <PropertyGroup>' + sLineBreak +
    '        <MainSource>Pkg.dpk</MainSource>' + sLineBreak +
    '        <AppType>Package</AppType>' + sLineBreak +
    '    </PropertyGroup>' + sLineBreak +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>' + sLineBreak +
    '</Project>' + sLineBreak;

  //targets Win32 and Win64, and uses VSoft.Foo - recorded without the dependency it declares.
  cConsumerProject =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' + sLineBreak +
    '    <PropertyGroup>' + sLineBreak +
    '        <ProjectGuid>{5B2E1A9C-3D4F-4E6A-8B7C-9D0E1F2A3B4C}</ProjectGuid>' + sLineBreak +
    '        <MainSource>Consumer.dpr</MainSource>' + sLineBreak +
    '        <ProjectVersion>20.1</ProjectVersion>' + sLineBreak +
    '        <Base>True</Base>' + sLineBreak +
    '        <Config Condition="''$(Config)''==''''">Debug</Config>' + sLineBreak +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>' + sLineBreak +
    '        <AppType>Console</AppType>' + sLineBreak +
    '    </PropertyGroup>' + sLineBreak +
    '    <ItemGroup>' + sLineBreak +
    '        <DelphiCompile Include="$(MainSource)">' + sLineBreak +
    '            <MainSource>MainSource</MainSource>' + sLineBreak +
    '        </DelphiCompile>' + sLineBreak +
    '        <BuildConfiguration Include="Base">' + sLineBreak +
    '            <Key>Base</Key>' + sLineBreak +
    '        </BuildConfiguration>' + sLineBreak +
    '    </ItemGroup>' + sLineBreak +
    '    <ProjectExtensions>' + sLineBreak +
    '        <Borland.Personality>Delphi.Personality.12</Borland.Personality>' + sLineBreak +
    '        <Borland.ProjectType>Application</Borland.ProjectType>' + sLineBreak +
    '        <BorlandProject>' + sLineBreak +
    '            <Delphi.Personality>' + sLineBreak +
    '                <Source>' + sLineBreak +
    '                    <Source Name="MainSource">Consumer.dpr</Source>' + sLineBreak +
    '                </Source>' + sLineBreak +
    '            </Delphi.Personality>' + sLineBreak +
    '            <Platforms>' + sLineBreak +
    '                <Platform value="Win32">True</Platform>' + sLineBreak +
    '                <Platform value="Win64">True</Platform>' + sLineBreak +
    '            </Platforms>' + sLineBreak +
    '        </BorlandProject>' + sLineBreak +
    '        <ProjectFileVersion>12</ProjectFileVersion>' + sLineBreak +
    '        <DPM>' + sLineBreak +
    '            <Packages>' + sLineBreak +
    '                <PackageReference id="VSoft.Foo" version="1.0.0"/>' + sLineBreak +
    '            </Packages>' + sLineBreak +
    '        </DPM>' + sLineBreak +
    '    </ProjectExtensions>' + sLineBreak +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets" Condition="Exists(''$(BDS)\Bin\CodeGear.Delphi.Targets'')"/>' + sLineBreak +
    '</Project>' + sLineBreak;

{ TFakeCompiler }

constructor TFakeCompiler.Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource;
                                 const compilerVersion : TCompilerVersion; const platform : TDPMPlatform);
begin
  inherited Create;
  FLogger := logger;
  FOutcome := outcome;
  FCancellation := cancellation;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FSearchPaths := TCollections.CreateList<string>;
  FOutput := TStringList.Create;
end;

destructor TFakeCompiler.Destroy;
begin
  FOutput.Free;
  inherited;
end;

function TFakeCompiler.BuildProject(const cancellationToken : ICancellationToken; const platform : TDPMPlatform; const projectFile : string;
                                    const configName : string; const packageVersion : TPackageVersion; const forDesign : boolean) : boolean;
begin
  case FOutcome of
    coSucceed :
      result := true;
    coFail :
    begin
      FLogger.Error(TErrorCode.E5003_PackageCompileFailed, 'Unable to compile [' + projectFile + '] for ' + DPMPlatformToString(platform) + ' - msbuild exited with 1.');
      result := false;
    end;
    coRaise :
      raise Exception.Create('the compiler fell over');
  else
    //Ctrl-C while msbuild runs - it is stopped, and says nothing.
    FCancellation.Cancel;
    result := false;
  end;
end;

function TFakeCompiler.GetBPLOutput : string;
begin
  result := FBPLOutput;
end;

function TFakeCompiler.GetCompilerOutput : TStrings;
begin
  result := FOutput;
end;

function TFakeCompiler.GetCompilerVersion : TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TFakeCompiler.GetConfiguration : string;
begin
  result := FConfiguration;
end;

function TFakeCompiler.GetLibOutput : string;
begin
  result := FLibOutput;
end;

function TFakeCompiler.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

function TFakeCompiler.GetSearchPaths : IList<string>;
begin
  result := FSearchPaths;
end;

function TFakeCompiler.GetVerbosity : TCompilerVerbosity;
begin
  result := FVerbosity;
end;

procedure TFakeCompiler.SetBPLOutput(const value : string);
begin
  FBPLOutput := value;
end;

procedure TFakeCompiler.SetConfiguration(const value : string);
begin
  FConfiguration := value;
end;

procedure TFakeCompiler.SetLibOutput(const value : string);
begin
  FLibOutput := value;
end;

procedure TFakeCompiler.SetSearchPaths(const value : IList<string>);
begin
  FSearchPaths := value;
end;

procedure TFakeCompiler.SetVerbosity(const value : TCompilerVerbosity);
begin
  FVerbosity := value;
end;

{ TFakeCompilerFactory }

constructor TFakeCompilerFactory.Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource);
begin
  inherited Create;
  FLogger := logger;
  FOutcome := outcome;
  FCancellation := cancellation;
end;

function TFakeCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler;
begin
  result := TFakeCompiler.Create(FLogger, FOutcome, FCancellation, compilerVersion, platform);
end;

function TFakeCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler;
begin
  result := TFakeCompiler.Create(logger, FOutcome, FCancellation, compilerVersion, platform);
end;

{ TExtractedPackageCache }

constructor TExtractedPackageCache.Create(const packageFolder : string; const spec : IPackageSpec);
begin
  inherited Create;
  FPackageFolder := packageFolder;
  FSpec := spec;
end;

function TExtractedPackageCache.EnsurePackage(const packageId : IPackageIdentity) : boolean;
begin
  result := true;
end;

function TExtractedPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := FPackageFolder;
end;

function TExtractedPackageCache.GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;
begin
  result := FPackageFolder;
end;

function TExtractedPackageCache.GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;
begin
  result := FSpec;
end;

{ TFakeConfigurationManager }

constructor TFakeConfigurationManager.Create(const config : IConfiguration);
begin
  inherited Create;
  FConfig := config;
end;

function TFakeConfigurationManager.EnsureDefaultConfig : boolean;
begin
  result := true;
end;

function TFakeConfigurationManager.LoadConfig(const configFile : string) : IConfiguration;
begin
  result := FConfig;
end;

function TFakeConfigurationManager.NewConfig : IConfiguration;
begin
  result := FConfig;
end;

function TFakeConfigurationManager.SaveConfig(const configuration : IConfiguration; const fileName : string) : boolean;
begin
  result := true;
end;

{ helpers }

//The installer as dpm.exe builds it, over the fakes. The planner, receipts, package generator and dproj
//patcher are not used by the cache and restore paths these tests take.
function NewInstaller(const logger : ILogger; const repo : IPackageRepositoryManager; const cache : IPackageCache;
                      const context : IPackageInstallerContext; const compilerFactory : ICompilerFactory) : IPackageInstaller;
var
  config : IConfiguration;
begin
  config := TConfiguration.Create(logger);
  result := TPackageInstaller.Create(logger, TFakeConfigurationManager.Create(config), repo, cache,
                                     TDependencyResolver.Create(logger, repo, cache, context), context,
                                     compilerFactory, nil, nil, nil, nil);
end;

function NewCacheOptions : TCacheOptions;
begin
  result := TCacheOptions.Create;
  result.Command := TCacheSubCommand.Install;
  result.PackageId := cPackageId;
  result.VersionString := '1.0.0';
  result.CompilerVersion := cTestCompiler;
  result.ConfigFile := 'dpm.config.yaml';
end;

{ TInstallerDiagnosticsTests }

procedure TInstallerDiagnosticsTests.FixtureSetup;
begin
  //the project editor and the build hook check use MSXML.
  CoInitialize(nil);
  FTempFolder := TPath.Combine(TPath.GetTempPath, 'dpm-installer-diagnostics-' + IntToHex(Random(MaxInt), 8));
  ForceDirectories(TPath.Combine(FTempFolder, 'package\source'));
  TFile.WriteAllText(TPath.Combine(FTempFolder, 'package\source\Pkg.dproj'), cPackageProject, TEncoding.UTF8);
end;

procedure TInstallerDiagnosticsTests.FixtureTearDown;
begin
  if DirectoryExists(FTempFolder) then
    TDirectory.Delete(FTempFolder, true);
  CoUninitialize;
end;

//Builds the test package into the 'cache' with the compiler doing what outcome says - returns what
//dpm cache install would have.
function CachePackage(const tempFolder : string; const logger : ILogger; const outcome : TCompileOutcome;
                      const cancellation : ICancellationTokenSource) : boolean;
var
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  info : IPackageInfo;
  installer : IPackageInstaller;
  options : TCacheOptions;
  packageFolder : string;
  reader : IPackageSpecReader;
begin
  packageFolder := TPath.Combine(tempFolder, 'package');
  //the BOM a successful build leaves would skip the next build.
  DeleteFile(TPath.Combine(packageFolder, 'package.Win32.bom'));
  info := MakeInfo(cPackageId, '1.0.0', []);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions(cPackageId, ListOf([info]));
  reader := TPackageSpecReader.Create(logger);
  cache := TExtractedPackageCache.Create(packageFolder, reader.ReadSpecString(cPackageSpec));
  cacheIntf := cache;
  cache.RegisterInfo(info);
  installer := NewInstaller(logger, repoIntf, cacheIntf, TFakeInstallerContext.Create(logger),
                            TFakeCompilerFactory.Create(logger, outcome, cancellation));
  options := NewCacheOptions;
  try
    result := installer.Cache(cancellation.Token, options);
  finally
    options.Free;
  end;
end;

procedure TInstallerDiagnosticsTests.Compile_Failure_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //the compiler reports it - the build, the cache step and the operation above it add nothing.
  Assert.IsFalse(CachePackage(FTempFolder, loggerIntf, coFail, TCancellationTokenSourceFactory.Create));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E5003_PackageCompileFailed), logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Successful_Build_Reports_No_Errors;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsTrue(CachePackage(FTempFolder, loggerIntf, coSucceed, TCancellationTokenSourceFactory.Create), logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Unexpected_Exception_Is_One_Error;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(CachePackage(FTempFolder, loggerIntf, coRaise, TCancellationTokenSourceFactory.Create));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E0001_UnexpectedException), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'the compiler fell over'), 'the error carries the exception''s message : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Cancelled_Build_Is_One_Warning_And_No_Errors;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  cancellation : ICancellationTokenSource;
  guard : TOperationGuard;
  cached : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  cancellation := TCancellationTokenSourceFactory.Create;
  //the boundary that owns the token - TDPMConsoleApplication.Run does the same around a command.
  guard := TOperationGuard.Start(loggerIntf, true);
  cached := CachePackage(FTempFolder, loggerIntf, coCancel, cancellation);
  guard.Finish(cached, cancellation.Token.IsCancelled, 'dpm cache');

  Assert.IsFalse(cached);
  Assert.AreEqual(0, loggerIntf.ErrorCount, 'cancelling is not an error : ' + logger.Messages.Text);
  Assert.AreEqual(1, logger.WarningCount, 'the boundary says it was cancelled, once - the installer does not : ' + logger.Messages.Text);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W0004_OperationCancelled), logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Two_Conflicts_Are_Two_Errors_And_Nothing_Else;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  info : IPackageInfo;
  installer : IPackageInstaller;
  options : TCacheOptions;
  i : integer;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //neither dependency exists at any version - two independent problems, so two errors.
  info := MakeInfo(cPackageId, '1.0.0', [MakeDep('VSoft.MissingOne', '1.0.0'), MakeDep('VSoft.MissingTwo', '1.0.0')]);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions(cPackageId, ListOf([info]));
  //in the cache already - resolving is what fails, before the spec is needed.
  cache := TExtractedPackageCache.Create(TPath.Combine(FTempFolder, 'package'), nil);
  cacheIntf := cache;
  cache.RegisterInfo(info);
  installer := NewInstaller(loggerIntf, repoIntf, cacheIntf, TFakeInstallerContext.Create(loggerIntf),
                            TFakeCompilerFactory.Create(loggerIntf, coSucceed, nil));
  options := NewCacheOptions;
  try
    Assert.IsFalse(installer.Cache(TCancellationTokenSourceFactory.Create.Token, options));
  finally
    options.Free;
  end;

  Assert.AreEqual(2, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.AreEqual(2, logger.Diagnostics.Count, 'nothing else - no warnings either : ' + logger.Messages.Text);
  for i := 0 to logger.Diagnostics.Count - 1 do
    Assert.AreEqual('error DPME2001', logger.Diagnostics[i], logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'VSoft.MissingOne'), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'VSoft.MissingTwo'), logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Restore_Conflict_On_Two_Platforms_Is_Reported_Once;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cacheIntf : IPackageCache;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  options : TRestoreOptions;
  projectFile : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  projectFile := TPath.Combine(FTempFolder, 'Consumer.dproj');
  TFile.WriteAllText(projectFile, cConsumerProject, TEncoding.UTF8);
  //VSoft.Foo needs VSoft.Missing, which no source has - restoring has to resolve it, and can't.
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions('VSoft.Foo', ListOf([MakeInfo('VSoft.Foo', '1.0.0', [MakeDep('VSoft.Missing', '1.0.0')])]));
  cacheIntf := TFakePackageCache.Create;
  context := TFakeInstallerContext.Create(loggerIntf);
  installer := NewInstaller(loggerIntf, repoIntf, cacheIntf, context, TFakeCompilerFactory.Create(loggerIntf, coSucceed, nil));
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    Assert.IsFalse(installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context));
  finally
    options.Free;
  end;

  //the project's graph is resolved once, not once per platform.
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E2001_DependencyUnsatisfiable), logger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallerDiagnosticsTests);

end.
