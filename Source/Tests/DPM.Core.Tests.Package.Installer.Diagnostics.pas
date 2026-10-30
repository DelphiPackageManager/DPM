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

    //A spec's build/design project must be inside the package - DPM only reads, patches and builds what
    //the package itself contains.
    [Test]
    procedure Build_Project_Outside_The_Package_Is_One_Error_And_Not_Built;
    [Test]
    procedure Absolute_Build_Project_Is_One_Error_And_Not_Built;
    [Test]
    procedure Design_Project_Outside_The_Package_Is_One_Error_And_Not_Built;
    [Test]
    procedure CopyToBin_Files_Outside_The_Package_Are_Not_Copied;

    //A source entry's folder goes on the consumer's search path - never one outside the package.
    [Test]
    procedure Source_Folder_Inside_The_Package_Is_Added_To_The_Project;
    [Test]
    procedure Source_Folder_Outside_The_Package_Is_Not_Added_To_The_Project;
    [Test]
    procedure Git_Source_Folder_Outside_The_Package_Is_Not_Added_To_The_Project;
    [Test]
    procedure Git_Source_Search_Does_Not_Follow_A_Junction_Out_Of_The_Package;

    //dpm copylocal, run by the build, copies a spec's copyLocal files into the project's output folder.
    [Test]
    procedure CopyLocal_Files_Outside_The_Package_Are_Not_Copied;

    //The cached packages must be what the project pinned (manifestHash) - checked before anything is
    //built, since a build runs the package's own project, and for transitive packages too.
    [Test]
    procedure Restore_Matching_Locks_Builds_And_Keeps_Them;
    [Test]
    procedure Restore_Transitive_Lock_Mismatch_Fails_Before_Building;
    [Test]
    procedure Restore_Re_Resolve_Keeps_Transitive_Lock;
    [Test]
    procedure Install_Keeps_The_Locks_Of_The_Packages_It_Keeps;
    [Test]
    procedure Install_Stale_Lock_On_A_Kept_Package_Fails_Before_Building;

    //A package the cache has the details of, but can't use, is downloaded against the hash its source
    //publishes - not the cache's own record of it, which is what is in doubt.
    [Test]
    procedure Download_Of_A_Cached_Package_Uses_The_Source_Hash;

    //A platform that can't be built (no SDK for it, say) doesn't take the others down : the project is changed
    //for the platforms that built, the failed one's search path is left alone, and the operation still fails.
    [Test]
    procedure Install_A_Failing_Last_Platform_Still_Installs_The_Others;
    [Test]
    procedure Install_A_Failing_First_Platform_Still_Installs_The_Others;
    [Test]
    procedure Install_Failing_On_Every_Platform_Changes_Nothing;
    [Test]
    procedure Restore_A_Failing_Platform_Still_Restores_The_Others;
    //dpm cache install - and DSpecCreator's test of a package, which is one - builds every platform the package supports.
    [Test]
    procedure Cache_A_Failing_Platform_Still_Builds_The_Others;
    //On Linux and macOS a package links against its dependencies' runtime packages (.so / .dylib), which are in their
    //bpl folders - not needed on Windows, where the .dcp in lib is linked against.
    [Test]
    procedure Package_Built_For_Linux_Finds_Its_Dependencies_Runtime_Packages;

    //In the IDE the design-time packages are also built for the platform the IDE runs on, when the project
    //doesn't target it. A package that doesn't declare that platform isn't built for it (that would add the
    //platform to its projects), nor is one that needs it; a package that doesn't build doesn't stop the
    //others; every problem is in one warning, and a design package that wasn't built isn't loaded.
    [Test]
    procedure Design_Host_Build_Skips_A_Package_That_Does_Not_Declare_The_Host;
    [Test]
    procedure Design_Host_Build_Keeps_Going_And_Warns_Once;
    [Test]
    procedure Design_Host_Package_That_Was_Not_Built_Is_Not_Loaded;

    //A package's search paths come from the folders that hold its source - not the ones DPM puts its compiled
    //output in (which can hold .dfm/.res files, copied there by copyToLib), nor its signatures.
    [Test]
    procedure Packed_Package_Source_Walk_Skips_The_Package_Output;
    [Test]
    procedure Git_Source_Walk_Skips_The_Package_Output;

    //'use source' is a choice for one project in the IDE session - another project using the same package, in
    //the same session, still gets its compiled output.
    [Test]
    procedure Use_Source_In_One_Project_Does_Not_Carry_Over_To_Another;

    //Refreshing the search paths (what the IDE does when 'use source' changes) leaves a package out of the
    //search path of a platform it doesn't support, as restore does.
    [Test]
    procedure Refresh_Leaves_A_Package_Out_Of_A_Platform_It_Does_Not_Support;

    //What a project resolved is recorded for the other projects in the group to be checked against - the graph
    //a restore found intact as well as one it re-resolved, but not one it didn't keep, and not packages an
    //uninstall removed.
    [Test]
    procedure Restore_Records_The_Graph_It_Validates_For_The_Other_Projects;
    [Test]
    procedure Restore_That_Keeps_Nothing_Records_Nothing;
    [Test]
    procedure Uninstall_Drops_What_The_Project_Recorded;

    //With no compiler given, each project in a group is restored (or uninstalled from) for its own compiler -
    //not the one the first project was for.
    [Test]
    procedure Restore_Uses_Each_Projects_Own_Compiler;
    [Test]
    procedure Uninstall_Uses_Each_Projects_Own_Compiler;

    //dpm cache install --force builds the package again, though its BOM says nothing changed - asked for by id or
    //by package file.
    [Test]
    procedure Cache_Install_With_Force_Builds_The_Package_Again;

    //A cached spec with no target platform (one that didn't read cleanly) has nothing for copylocal to copy - it is
    //not a crash.
    [Test]
    procedure CopyLocal_Of_A_Package_With_No_Target_Platform_Copies_Nothing;

    //A restore that re-resolves has the package infos the resolve found - a package not in the cache yet isn't
    //looked up on the sources again.
    [Test]
    procedure Restore_Re_Resolve_Does_Not_Look_Up_What_It_Resolved;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.IOUtils,
  WinApi.ActiveX,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Constants,
  DPM.Core.Utils.Process,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolution,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Restore,
  DPM.Core.Options.Install,
  DPM.Core.Options.UnInstall,
  DPM.Core.Options.CopyLocal,
  DPM.Core.Package.CopyLocal,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.Installer,
  DPM.Core.Package.InstallPlan,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Utils.System,
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
    FFailPlatforms : TDPMPlatforms;
    function OutcomeFor(const platform : TDPMPlatform) : TCompileOutcome;
  protected
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler; overload;
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler; overload;
  public
    //the builds for failPlatforms fail whatever outcome says - a platform the machine can't build for.
    constructor Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource;
                       const failPlatforms : TDPMPlatforms = []);
  end;

  //Every package is already in the cache, extracted to one folder, with the spec given.
  TExtractedPackageCache = class(TFakePackageCache, IPackageCache)
  private
    FPackageFolder : string;
    FSpec : IPackageSpec;
  protected
    function EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
    //the package file is already extracted, as the package folder.
    function InstallPackageFromFile(const cancellationToken : ICancellationToken; const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;
    function GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string; overload;
  public
    constructor Create(const packageFolder : string; const spec : IPackageSpec);
  end;

  //Each package extracted to its own folder - <root>\<id> - with the spec given.
  TPerPackageCache = class(TFakePackageCache, IPackageCache)
  private
    FRoot : string;
    FSpec : IPackageSpec;
    FSpecs : IDictionary<string, IPackageSpec>;
    FMissing : ISet<string>;
  protected
    function EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
    function GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string; overload;
    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;
  public
    constructor Create(const root : string; const spec : IPackageSpec);
    //ids (lowercase) that are not extracted - EnsurePackage says so, and the installer downloads them.
    property Missing : ISet<string> read FMissing;
    //the spec of a package (lowercase id) that isn't the one given.
    property Specs : IDictionary<string, IPackageSpec> read FSpecs;
  end;

  //Records the packages it is asked to load the design-time packages of.
  TRecordingInstallerContext = class(TFakeInstallerContext)
  private
    FLoadedIds : IList<string>;
  protected
    function InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean; override;
  public
    constructor Create(const logger : ILogger);override;
    //lowercase ids, from the last call.
    property LoadedIds : IList<string> read FLoadedIds;
  end;

  //The receipts the cache wrote - just each package folder's manifest hash.
  TFakeReceiptService = class(TInterfacedObject, IReceiptService)
  private
    FHashes : IDictionary<string, string>;
  protected
    procedure Write(const folder : string; const receipt : TVerificationReceipt);
    function TryRead(const folder : string; out receipt : TVerificationReceipt) : boolean;
    function ReceiptPath(const folder : string) : string;
  public
    constructor Create;
    procedure SetHash(const folder : string; const manifestHashHex : string);
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

var
  //how many projects the fake compiler was asked to build - CachePackageSpec resets it.
  BuildCount : integer;
  //every build the fake compiler was asked for, as '<platform> <project file>' - when set.
  BuiltProjects : IList<string>;
  //builds that fail whatever the outcome, as '<platform>|<part of the project file>' - when set.
  FailingBuilds : IList<string>;
  //every build's search paths, as '<platform> <project file>|<search paths, ; separated>' - when set.
  BuildSearchPaths : IList<string>;

const
  cPackageId = 'VSoft.Compiles';

  //the spec up to the template's build/design/copy entries.
  cPackageSpecHead =
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
    '        dest: source' + sLineBreak;

  cPackageBuild =
    '    build:' + sLineBreak +
    '      - project: .\source\Pkg.dproj' + sLineBreak;

  cPackageSpec = cPackageSpecHead + cPackageBuild;

  //VSoft.Foo - the package cConsumerProject uses - as source only, up to its source entries.
  cSourcePackageSpecHead =
    'metadata:' + sLineBreak +
    '  id: VSoft.Foo' + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: installer search path test package' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32, Win64]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak;

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
var
  outcome : TCompileOutcome;
  failing : string;
  separator : integer;
  paths : string;
  path : string;
begin
  Inc(BuildCount);
  if BuiltProjects <> nil then
    BuiltProjects.Add(DPMPlatformToString(platform) + ' ' + projectFile);
  if BuildSearchPaths <> nil then
  begin
    paths := '';
    if FSearchPaths <> nil then
      for path in FSearchPaths do
        paths := paths + path + ';';
    BuildSearchPaths.Add(DPMPlatformToString(platform) + ' ' + projectFile + '|' + paths);
  end;
  outcome := FOutcome;
  if FailingBuilds <> nil then
    for failing in FailingBuilds do
    begin
      separator := Pos('|', failing);
      if SameText(Copy(failing, 1, separator - 1), DPMPlatformToString(platform)) and
         (Pos(LowerCase(Copy(failing, separator + 1, MaxInt)), LowerCase(projectFile)) > 0) then
        outcome := coFail;
    end;
  case outcome of
    coSucceed :
    begin
      //where msbuild would have written the package.
      if FBPLOutput <> '' then
        ForceDirectories(FBPLOutput);
      result := true;
    end;
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

constructor TFakeCompilerFactory.Create(const logger : ILogger; const outcome : TCompileOutcome; const cancellation : ICancellationTokenSource;
                                        const failPlatforms : TDPMPlatforms);
begin
  inherited Create;
  FLogger := logger;
  FOutcome := outcome;
  FCancellation := cancellation;
  FFailPlatforms := failPlatforms;
end;

function TFakeCompilerFactory.OutcomeFor(const platform : TDPMPlatform) : TCompileOutcome;
begin
  if platform in FFailPlatforms then
    result := coFail
  else
    result := FOutcome;
end;

function TFakeCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler;
begin
  result := TFakeCompiler.Create(FLogger, OutcomeFor(platform), FCancellation, compilerVersion, platform);
end;

function TFakeCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler;
begin
  result := TFakeCompiler.Create(logger, OutcomeFor(platform), FCancellation, compilerVersion, platform);
end;

{ TExtractedPackageCache }

constructor TExtractedPackageCache.Create(const packageFolder : string; const spec : IPackageSpec);
begin
  inherited Create;
  FPackageFolder := packageFolder;
  FSpec := spec;
end;

function TExtractedPackageCache.EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
begin
  result := true;
end;

function TExtractedPackageCache.InstallPackageFromFile(const cancellationToken : ICancellationToken; const packageFileName : string; const skipTrustRatchets : boolean) : boolean;
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

function TExtractedPackageCache.GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
begin
  result := FSpec;
end;

{ TPerPackageCache }

constructor TPerPackageCache.Create(const root : string; const spec : IPackageSpec);
begin
  inherited Create;
  FRoot := root;
  FSpec := spec;
  FSpecs := TCollections.CreateDictionary<string, IPackageSpec>;
  FMissing := TCollections.CreateSet<string>;
end;

function TPerPackageCache.EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
begin
  result := not FMissing.Contains(LowerCase(packageId.Id));
end;

function TPerPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := TPath.Combine(FRoot, packageId.Id);
end;

function TPerPackageCache.GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;
begin
  result := TPath.Combine(FRoot, id);
end;

function TPerPackageCache.GetPackageFileFolder(const packageId : IPackageIdentity) : string;
begin
  result := FRoot;
end;

function TPerPackageCache.GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
begin
  if not FSpecs.TryGetValue(LowerCase(packageId.Id), result) then
    result := FSpec;
end;

{ TRecordingInstallerContext }

constructor TRecordingInstallerContext.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FLoadedIds := TCollections.CreateList<string>;
end;

function TRecordingInstallerContext.InstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean;
var
  id : string;
begin
  FLoadedIds.Clear;
  for id in packageSpecs.Keys do
    FLoadedIds.Add(LowerCase(id));
  result := true;
end;

{ TFakeReceiptService }

constructor TFakeReceiptService.Create;
begin
  inherited Create;
  FHashes := TCollections.CreateDictionary<string, string>;
end;

procedure TFakeReceiptService.SetHash(const folder : string; const manifestHashHex : string);
begin
  FHashes[LowerCase(ExcludeTrailingPathDelimiter(folder))] := manifestHashHex;
end;

procedure TFakeReceiptService.Write(const folder : string; const receipt : TVerificationReceipt);
begin
  SetHash(folder, receipt.ManifestHashHex);
end;

function TFakeReceiptService.TryRead(const folder : string; out receipt : TVerificationReceipt) : boolean;
var
  hash : string;
begin
  receipt := Default(TVerificationReceipt);
  result := FHashes.TryGetValue(LowerCase(ExcludeTrailingPathDelimiter(folder)), hash);
  if result then
    receipt.ManifestHashHex := hash;
end;

function TFakeReceiptService.ReceiptPath(const folder : string) : string;
begin
  result := TPath.Combine(folder, cReceiptFileName);
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
  //next to the package folder, not in it - a project that would build, and a dll to copy.
  ForceDirectories(TPath.Combine(FTempFolder, 'outside'));
  TFile.WriteAllText(TPath.Combine(FTempFolder, 'outside\Evil.dproj'), cPackageProject, TEncoding.UTF8);
  TFile.WriteAllText(TPath.Combine(FTempFolder, 'outside\Evil.dll'), 'not really a dll', TEncoding.UTF8);
  ForceDirectories(TPath.Combine(FTempFolder, 'outside\deep'));
  TFile.WriteAllText(TPath.Combine(FTempFolder, 'outside\deep\Evil.pas'), 'unit Evil; interface implementation end.', TEncoding.UTF8);
end;

procedure TInstallerDiagnosticsTests.FixtureTearDown;
begin
  if DirectoryExists(FTempFolder) then
    TDirectory.Delete(FTempFolder, true);
  CoUninitialize;
end;

//Builds a package with the spec given into the 'cache' with the compiler doing what outcome says - returns
//what dpm cache install would have.
function CachePackageSpec(const tempFolder : string; const logger : ILogger; const outcome : TCompileOutcome;
                          const cancellation : ICancellationTokenSource; const spec : string) : boolean;
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
  DeleteFile(TPath.Combine(packageFolder, 'package.Win64.bom'));
  BuildCount := 0;
  info := MakeInfo(cPackageId, '1.0.0', []);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions(cPackageId, ListOf([info]));
  reader := TPackageSpecReader.Create(logger);
  cache := TExtractedPackageCache.Create(packageFolder, reader.ReadSpecString(spec));
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

function CachePackage(const tempFolder : string; const logger : ILogger; const outcome : TCompileOutcome;
                      const cancellation : ICancellationTokenSource) : boolean;
begin
  result := CachePackageSpec(tempFolder, logger, outcome, cancellation, cPackageSpec);
end;

//dpm cache install <package> again, with --force when force - package is the id or a package file. Unlike
//CachePackageSpec, the BOM the last build left is kept.
function CacheAgain(const tempFolder : string; const logger : ILogger; const package : string; const force : boolean) : boolean;
var
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  info : IPackageInfo;
  installer : IPackageInstaller;
  options : TCacheOptions;
  cancellation : ICancellationTokenSource;
  reader : IPackageSpecReader;
begin
  BuildCount := 0;
  info := MakeInfo(cPackageId, '1.0.0', []);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions(cPackageId, ListOf([info]));
  reader := TPackageSpecReader.Create(logger);
  cache := TExtractedPackageCache.Create(TPath.Combine(tempFolder, 'package'), reader.ReadSpecString(cPackageSpec));
  cacheIntf := cache;
  cache.RegisterInfo(info);
  cancellation := TCancellationTokenSourceFactory.Create;
  installer := NewInstaller(logger, repoIntf, cacheIntf, TFakeInstallerContext.Create(logger), TFakeCompilerFactory.Create(logger, coSucceed, cancellation));
  options := NewCacheOptions;
  try
    options.PackageId := package;
    options.Force := force;
    result := installer.Cache(cancellation.Token, options);
  finally
    options.Free;
  end;
end;

//project with the base configuration the IDE writes - where the search path goes.
function ConsumerProjectWithBaseFor(const project : string) : string;
begin
  result := StringReplace(project, '    <ItemGroup>' + sLineBreak,
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">' + sLineBreak +
    '        <Base>true</Base>' + sLineBreak +
    '    </PropertyGroup>' + sLineBreak +
    '    <PropertyGroup Condition="''$(Base)''!=''''">' + sLineBreak +
    '        <DCC_UnitSearchPath>$(DCC_UnitSearchPath)</DCC_UnitSearchPath>' + sLineBreak +
    '    </PropertyGroup>' + sLineBreak +
    '    <ItemGroup>' + sLineBreak, []);
end;

function ConsumerProjectWithBase : string;
begin
  result := ConsumerProjectWithBaseFor(cConsumerProject);
end;

const
  cFooHash = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  cBarHash = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
  cStaleHash = 'cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc';
  cBazHash = 'dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd';

//The consumer project recording VSoft.Foo (locked to cFooHash) and its dependency VSoft.Bar, locked to barHash.
function LockedConsumerProject(const barHash : string) : string;
begin
  result := StringReplace(ConsumerProjectWithBaseFor(cConsumerProject),
    '                <PackageReference id="VSoft.Foo" version="1.0.0"/>' + sLineBreak,
    '                <PackageReference id="VSoft.Foo" version="1.0.0" manifestHash="sha256:' + cFooHash + '">' + sLineBreak +
    '                    <PackageReference id="VSoft.Bar" version="1.0.0" range="1.0.0" manifestHash="sha256:' + barHash + '"/>' + sLineBreak +
    '                </PackageReference>' + sLineBreak, []);
end;

//cPackageSpec for Win32 and Win64.
function TwoPlatformPackageSpec : string;
begin
  result := StringReplace(cPackageSpec, 'platforms: [Win32]', 'platforms: [Win32, Win64]', []);
end;

//An installer over a cache holding VSoft.Foo (depending on fooDependencies), VSoft.Bar and VSoft.Baz, each in its
//own folder with the spec given - or the one in specOverrides (lowercase id -> spec, may be nil) - whose receipts say
//they are cFooHash, cBarHash and cBazHash - with projectFile, holding the project text given, beside them. Their
//builds fail for failPlatforms. The sources have all three - the cache has no package info for those in uncachedIds,
//as for a package not downloaded yet. repository is the fake the installer uses.
function NewLockedFixtureWith(const tempFolder : string; const logger : ILogger; const project : string;
                              const fooDependencies : array of IPackageDependency; const spec : string;
                              const specOverrides : IDictionary<string, string>; const uncachedIds : array of string;
                              const failPlatforms : TDPMPlatforms; const context : IPackageInstallerContext;
                              out projectFile : string; out repository : TFakeRepositoryManager) : IPackageInstaller;

  function IsUncached(const info : IPackageInfo) : boolean;
  var
    i : integer;
  begin
    result := false;
    for i := 0 to High(uncachedIds) do
      if SameText(uncachedIds[i], info.Id) then
        exit(true);
  end;

var
  root : string;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TPerPackageCache;
  cacheIntf : IPackageCache;
  receipts : TFakeReceiptService;
  receiptsIntf : IReceiptService;
  config : IConfiguration;
  reader : IPackageSpecReader;
  fooInfo, barInfo, bazInfo : IPackageInfo;
  id : string;
  overridePair : TPair<string, string>;
begin
  root := TPath.Combine(tempFolder, 'locks');
  if DirectoryExists(root) then
    TDirectory.Delete(root, true);
  for id in TArray<string>.Create('VSoft.Foo', 'VSoft.Bar', 'VSoft.Baz') do
  begin
    ForceDirectories(TPath.Combine(root, id + '\source'));
    TFile.WriteAllText(TPath.Combine(root, id + '\source\Pkg.dproj'), cPackageProject, TEncoding.UTF8);
    TFile.WriteAllText(TPath.Combine(root, id + '\source\PkgD.dproj'), cPackageProject, TEncoding.UTF8);
  end;
  projectFile := TPath.Combine(root, 'Consumer.dproj');
  TFile.WriteAllText(projectFile, project, TEncoding.UTF8);

  fooInfo := MakeInfo('VSoft.Foo', '1.0.0', fooDependencies);
  barInfo := MakeInfo('VSoft.Bar', '1.0.0', []);
  bazInfo := MakeInfo('VSoft.Baz', '1.0.0', []);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repository := repo;
  repo.SetVersions('VSoft.Foo', ListOf([fooInfo]));
  repo.SetVersions('VSoft.Bar', ListOf([barInfo]));
  repo.SetVersions('VSoft.Baz', ListOf([bazInfo]));
  reader := TPackageSpecReader.Create(logger);
  cache := TPerPackageCache.Create(root, reader.ReadSpecString(spec));
  cacheIntf := cache;
  if specOverrides <> nil then
    for overridePair in specOverrides do
      cache.Specs[overridePair.Key] := reader.ReadSpecString(overridePair.Value);
  if not IsUncached(fooInfo) then
    cache.RegisterInfo(fooInfo);
  if not IsUncached(barInfo) then
    cache.RegisterInfo(barInfo);
  if not IsUncached(bazInfo) then
    cache.RegisterInfo(bazInfo);
  receipts := TFakeReceiptService.Create;
  receiptsIntf := receipts;
  receipts.SetHash(TPath.Combine(root, 'VSoft.Foo'), cFooHash);
  receipts.SetHash(TPath.Combine(root, 'VSoft.Bar'), cBarHash);
  receipts.SetHash(TPath.Combine(root, 'VSoft.Baz'), cBazHash);
  config := TConfiguration.Create(logger);
  result := TPackageInstaller.Create(logger, TFakeConfigurationManager.Create(config), repoIntf, cacheIntf,
                                     TDependencyResolver.Create(logger, repoIntf, cacheIntf, context), context,
                                     TFakeCompilerFactory.Create(logger, coSucceed, nil, failPlatforms), receiptsIntf, nil, nil,
                                     TInstallPlanner.Create(logger, repoIntf, cacheIntf));
  BuildCount := 0;
  BuiltProjects := TCollections.CreateList<string>;
end;

function NewLockedFixture(const tempFolder : string; const logger : ILogger; const project : string;
                          const fooDependencies : array of IPackageDependency; const spec : string;
                          const failPlatforms : TDPMPlatforms; out projectFile : string;
                          out context : IPackageInstallerContext) : IPackageInstaller;
var
  repository : TFakeRepositoryManager;
begin
  context := TFakeInstallerContext.Create(logger);
  result := NewLockedFixtureWith(tempFolder, logger, project, fooDependencies, spec, nil, [], failPlatforms, context, projectFile, repository);
end;

//Restores the project text given over the locked fixture for the platforms given - projectText is the project afterwards.
function RestoreLockedProjectFor(const tempFolder : string; const logger : ILogger; const project : string;
                                 const fooDependencies : array of IPackageDependency; const spec : string;
                                 const platforms : TDPMPlatforms; const failPlatforms : TDPMPlatforms;
                                 out projectText : string) : boolean;
var
  projectFile : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  options : TRestoreOptions;
begin
  installer := NewLockedFixture(tempFolder, logger, project, fooDependencies, spec, failPlatforms, projectFile, context);
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := platforms;
    result := installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  projectText := TFile.ReadAllText(projectFile);
end;

//Restores the project text given over the locked fixture - projectText is the project afterwards.
function RestoreLockedProject(const tempFolder : string; const logger : ILogger; const project : string;
                              const fooDependencies : array of IPackageDependency; out projectText : string) : boolean;
begin
  result := RestoreLockedProjectFor(tempFolder, logger, project, fooDependencies, cPackageSpec, [TDPMPlatform.Win32], [], projectText);
end;

//Installs VSoft.Baz 1.0.0 into the project text given over the locked fixture for the platforms given - projectText
//is the project afterwards, and context the one the install ran with.
function InstallIntoLockedProjectFor(const tempFolder : string; const logger : ILogger; const project : string; const spec : string;
                                     const platforms : TDPMPlatforms; const failPlatforms : TDPMPlatforms;
                                     out projectText : string; out context : IPackageInstallerContext) : boolean;
var
  projectFile : string;
  installer : IPackageInstaller;
  options : TInstallOptions;
begin
  installer := NewLockedFixture(tempFolder, logger, project, [MakeDep('VSoft.Bar', '1.0.0')], spec, failPlatforms, projectFile, context);
  options := TInstallOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := platforms;
    options.AddPackage('VSoft.Baz', TPackageVersion.Parse('1.0.0'));
    result := installer.Install(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  projectText := TFile.ReadAllText(projectFile);
end;

//Installs VSoft.Baz 1.0.0 into the project text given over the locked fixture - projectText is the project afterwards.
function InstallIntoLockedProject(const tempFolder : string; const logger : ILogger; const project : string;
                                  out projectText : string) : boolean;
var
  context : IPackageInstallerContext;
begin
  result := InstallIntoLockedProjectFor(tempFolder, logger, project, cPackageSpec, [TDPMPlatform.Win32], [], projectText, context);
end;

//The text of the project's DPMSearch property for the platform given - '' when it has none.
function SearchPathFor(const projectText : string; const platform : string) : string;
var
  marker : string;
  startPos : integer;
  endPos : integer;
begin
  result := '';
  marker := '<DPMSearch Condition="''$(Platform)''==''' + platform + '''">';
  startPos := Pos(marker, projectText);
  if startPos = 0 then
    exit;
  startPos := startPos + Length(marker);
  endPos := PosEx('</DPMSearch>', projectText, startPos);
  if endPos > 0 then
    result := Copy(projectText, startPos, endPos - startPos);
end;

//Refreshes the search paths of a project using VSoft.Foo, cached in packageFolder with the spec given, the way
//the IDE does when 'use source' changes - projectText is the project file afterwards.
function RefreshSearchPathsWithSpec(const packageFolder : string; const spec : string; const logger : ILogger;
                                    out projectText : string) : boolean;
var
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  reader : IPackageSpecReader;
  projectFile : string;
begin
  projectFile := packageFolder + '.dproj';
  TFile.WriteAllText(projectFile, ConsumerProjectWithBase, TEncoding.UTF8);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  reader := TPackageSpecReader.Create(logger);
  cache := TExtractedPackageCache.Create(packageFolder, reader.ReadSpecString(spec));
  cacheIntf := cache;
  cache.RegisterInfo(MakeInfo('VSoft.Foo', '1.0.0', []));
  context := TFakeInstallerContext.Create(logger);
  installer := NewInstaller(logger, repoIntf, cacheIntf, context, TFakeCompilerFactory.Create(logger, coSucceed, nil));
  result := installer.RefreshProjectSearchPaths(TCancellationTokenSourceFactory.Create.Token, projectFile, cTestCompiler, 'dpm.config.yaml', context);
  projectText := TFile.ReadAllText(projectFile);
end;

//As RefreshSearchPathsWithSpec, VSoft.Foo being for Win32 and Win64 with the source entries given.
function RefreshSearchPaths(const packageFolder : string; const sourceEntries : string; const logger : ILogger;
                            out projectText : string) : boolean;
begin
  result := RefreshSearchPathsWithSpec(packageFolder, cSourcePackageSpecHead + sourceEntries, logger, projectText);
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

//Caches a package whose template has the entries given, which the installer must refuse, and checks it
//said so once - naming the package and the path - without building anything.
procedure CheckProjectRefused(const tempFolder : string; const entries : string; const path : string);
var
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(CachePackageSpec(tempFolder, loggerIntf, coSucceed, TCancellationTokenSourceFactory.Create, cPackageSpecHead + entries),
                 'the package must not install : ' + logger.Messages.Text);
  Assert.AreEqual(0, BuildCount, 'nothing outside the package may reach msbuild : ' + logger.Messages.Text);
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E5019_ProjectOutsidePackage), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', cPackageId), 'the error names the package : ' + logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', path), 'the error names the path : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Build_Project_Outside_The_Package_Is_One_Error_And_Not_Built;
begin
  //a project that exists and would build, one folder up from the package.
  CheckProjectRefused(FTempFolder,
    '    build:' + sLineBreak +
    '      - project: ''..\outside\Evil.dproj''' + sLineBreak,
    '..\outside\Evil.dproj');
end;

procedure TInstallerDiagnosticsTests.Absolute_Build_Project_Is_One_Error_And_Not_Built;
var
  projectFile : string;
begin
  //refused even though it is the package's own project - spec paths are relative to the package.
  projectFile := TPath.Combine(FTempFolder, 'package\source\Pkg.dproj');
  CheckProjectRefused(FTempFolder,
    '    build:' + sLineBreak +
    '      - project: ''' + projectFile + '''' + sLineBreak,
    projectFile);
end;

procedure TInstallerDiagnosticsTests.Design_Project_Outside_The_Package_Is_One_Error_And_Not_Built;
begin
  CheckProjectRefused(FTempFolder,
    cPackageBuild +
    '    design:' + sLineBreak +
    '      - project: ''/../outside/Evil.dproj''' + sLineBreak,
    '/../outside/Evil.dproj');
end;

procedure TInstallerDiagnosticsTests.CopyToBin_Files_Outside_The_Package_Are_Not_Copied;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  copiedFile : string;
  outsideWarnings : integer;
  i : integer;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  copiedFile := TPath.Combine(FTempFolder, 'package\bpl\Win32\Evil.dll');
  //one pattern up out of the package, one rooted somewhere else entirely.
  Assert.IsTrue(CachePackageSpec(FTempFolder, loggerIntf, coSucceed, TCancellationTokenSourceFactory.Create,
                  cPackageSpecHead + cPackageBuild +
                  '    copyToBin:' + sLineBreak +
                  '      - src: ''..\outside\*.dll''' + sLineBreak +
                  '        copyToBin: Win32' + sLineBreak +
                  '      - src: ''' + TPath.Combine(FTempFolder, 'outside\*.dll') + '''' + sLineBreak +
                  '        copyToBin: Win32' + sLineBreak),
                'the package itself is fine : ' + logger.Messages.Text);
  Assert.IsFalse(TFile.Exists(copiedFile), 'a file from outside the package was copied into its bpl folder');
  outsideWarnings := 0;
  for i := 0 to logger.Diagnostics.Count - 1 do
    if logger.Diagnostics[i] = 'warning ' + WarningCodeToString(TWarningCode.W5012_PathOutsidePackage) then
      Inc(outsideWarnings);
  Assert.AreEqual(2, outsideWarnings, 'one warning per pattern : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Source_Folder_Inside_The_Package_Is_Added_To_The_Project;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-inside');
  ForceDirectories(TPath.Combine(packageFolder, 'source\sub'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'source\sub\Foo.pas'), 'unit Foo; interface implementation end.', TEncoding.UTF8);
  refreshed := RefreshSearchPaths(packageFolder,
                 '      - src: ''.\source\**\*.pas''' + sLineBreak +
                 '        dest: source' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  Assert.IsTrue(ContainsText(projectText, '$(DPM)\VSoft.Foo\1.0.0\source;'), projectText);
  Assert.IsTrue(ContainsText(projectText, '$(DPM)\VSoft.Foo\1.0.0\source\sub'), 'found by the ** walk : ' + projectText);
  Assert.IsFalse(logger.HasWarning(TWarningCode.W5012_PathOutsidePackage), logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Source_Folder_Outside_The_Package_Is_Not_Added_To_The_Project;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-outside');
  ForceDirectories(packageFolder);
  //a dest one folder up out of the package - and, with **, every folder below it holding source.
  refreshed := RefreshSearchPaths(packageFolder,
                 '      - src: ''.\source\**\*.pas''' + sLineBreak +
                 '        dest: ''..\outside''' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  Assert.IsFalse(ContainsText(projectText, 'outside'), 'a search path outside the package was written : ' + projectText);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W5012_PathOutsidePackage), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', '..\outside'), 'the warning names the folder : ' + logger.Messages.Text);
  Assert.IsTrue(logger.Logged('warning', 'VSoft.Foo'), 'the warning names the package : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Git_Source_Folder_Outside_The_Package_Is_Not_Added_To_The_Project;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-git-outside');
  ForceDirectories(packageFolder);
  //what a git registry install leaves - the package is the clone itself, its source where src says.
  TFile.WriteAllText(TPath.Combine(packageFolder, cGitPackageMarkerFile), 'abc', TEncoding.UTF8);
  refreshed := RefreshSearchPaths(packageFolder, '      - src: ''..\outside\**\*.pas''' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  Assert.IsFalse(ContainsText(projectText, 'outside'), 'a search path outside the package was written : ' + projectText);
  Assert.IsTrue(logger.HasWarning(TWarningCode.W5012_PathOutsidePackage), logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Git_Source_Search_Does_Not_Follow_A_Junction_Out_Of_The_Package;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  linkFolder : string;
  projectText : string;
  exitCode : Cardinal;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-junction');
  ForceDirectories(TPath.Combine(packageFolder, 'src'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'src\Foo.pas'), 'unit Foo; interface implementation end.', TEncoding.UTF8);
  TFile.WriteAllText(TPath.Combine(packageFolder, cGitPackageMarkerFile), 'abc', TEncoding.UTF8);
  //a link in the clone to a folder outside it - a junction needs no special rights to make.
  linkFolder := TPath.Combine(packageFolder, 'link');
  exitCode := TProcess.Execute2(TCancellationTokenSourceFactory.Create.Token, GetEnvironmentVariable('COMSPEC'),
                                '/c mklink /J "' + linkFolder + '" "' + TPath.Combine(FTempFolder, 'outside') + '"', '');
  Assert.IsTrue((exitCode = 0) and DirectoryExists(linkFolder), 'could not make the junction');
  try
    refreshed := RefreshSearchPaths(packageFolder, '      - src: ''**\*.pas''' + sLineBreak, loggerIntf, projectText);
    Assert.IsTrue(refreshed, logger.Messages.Text);
    Assert.IsTrue(ContainsText(projectText, '$(DPM)\VSoft.Foo\1.0.0\src'), projectText);
    Assert.IsFalse(ContainsText(projectText, '1.0.0\link'), 'the junction was followed : ' + projectText);
  finally
    //the link only - never what it points at.
    RemoveDir(linkFolder);
  end;
end;

procedure TInstallerDiagnosticsTests.CopyLocal_Files_Outside_The_Package_Are_Not_Copied;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  outputFolder : string;
  projectFile : string;
  reader : IPackageSpecReader;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  service : ICopyLocalService;
  options : TCopyLocalOptions;
  copied : boolean;
  outsideWarnings : integer;
  i : integer;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'copylocal');
  outputFolder := TPath.Combine(FTempFolder, 'copylocal-output');
  ForceDirectories(TPath.Combine(packageFolder, 'bin'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'bin\Good.dll'), 'not really a dll', TEncoding.UTF8);
  projectFile := TPath.Combine(FTempFolder, 'copylocal.dproj');
  TFile.WriteAllText(projectFile, ConsumerProjectWithBase, TEncoding.UTF8);
  reader := TPackageSpecReader.Create(loggerIntf);
  cache := TExtractedPackageCache.Create(packageFolder, reader.ReadSpecString(cSourcePackageSpecHead +
    '      - src: ''.\source\*.pas''' + sLineBreak +
    '    copyLocal:' + sLineBreak +
    '      - src: ''bin\*.dll''' + sLineBreak +
    //one pattern up out of the package, one rooted somewhere else entirely.
    '      - src: ''..\outside\*.dll''' + sLineBreak +
    '      - src: ''' + TPath.Combine(FTempFolder, 'outside\*.dll') + '''' + sLineBreak));
  cacheIntf := cache;
  service := TCopyLocalService.Create(loggerIntf, TFakeConfigurationManager.Create(TConfiguration.Create(loggerIntf)), cacheIntf);
  options := TCopyLocalOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.Platform := TDPMPlatform.Win32;
    options.Config := 'Debug';
    options.OutputDir := outputFolder;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    copied := service.CopyLocal(TCancellationTokenSourceFactory.Create.Token, options);
  finally
    options.Free;
  end;

  Assert.IsTrue(copied, logger.Messages.Text);
  Assert.IsTrue(TFile.Exists(TPath.Combine(outputFolder, 'Good.dll')), 'the package''s own file is copied : ' + logger.Messages.Text);
  Assert.IsFalse(TFile.Exists(TPath.Combine(outputFolder, 'Evil.dll')), 'a file from outside the package was copied into the output folder');
  outsideWarnings := 0;
  for i := 0 to logger.Diagnostics.Count - 1 do
    if logger.Diagnostics[i] = 'warning ' + WarningCodeToString(TWarningCode.W5012_PathOutsidePackage) then
      Inc(outsideWarnings);
  Assert.AreEqual(2, outsideWarnings, 'one warning per pattern : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.CopyLocal_Of_A_Package_With_No_Target_Platform_Copies_Nothing;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  outputFolder : string;
  projectFile : string;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  cache : TExtractedPackageCache;
  cacheIntf : IPackageCache;
  service : ICopyLocalService;
  options : TCopyLocalOptions;
  copied : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'copylocal-no-target');
  outputFolder := TPath.Combine(FTempFolder, 'copylocal-no-target-output');
  ForceDirectories(TPath.Combine(packageFolder, 'bin'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'bin\Good.dll'), 'not really a dll', TEncoding.UTF8);
  projectFile := TPath.Combine(FTempFolder, 'copylocal-no-target.dproj');
  TFile.WriteAllText(projectFile, ConsumerProjectWithBase, TEncoding.UTF8);
  //read with a logger of its own - the spec's own problem was reported when it was read.
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(StringReplace(cSourcePackageSpecHead,
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32, Win64]' + sLineBreak +
    '    template: default' + sLineBreak, '', []) +
    '      - src: ''.\source\*.pas''' + sLineBreak +
    '    copyLocal:' + sLineBreak +
    '      - src: ''bin\*.dll''' + sLineBreak);
  Assert.IsNotNull(spec);
  Assert.IsNull(spec.TargetPlatform, 'precondition : the spec has no target platform');
  cache := TExtractedPackageCache.Create(packageFolder, spec);
  cacheIntf := cache;
  service := TCopyLocalService.Create(loggerIntf, TFakeConfigurationManager.Create(TConfiguration.Create(loggerIntf)), cacheIntf);
  options := TCopyLocalOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.Platform := TDPMPlatform.Win32;
    options.Config := 'Debug';
    options.OutputDir := outputFolder;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    copied := service.CopyLocal(TCancellationTokenSourceFactory.Create.Token, options);
  finally
    options.Free;
  end;
  Assert.IsTrue(copied, logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsFalse(TFile.Exists(TPath.Combine(outputFolder, 'Good.dll')), 'with no template there is nothing to copy');
end;

procedure TInstallerDiagnosticsTests.Restore_Matching_Locks_Builds_And_Keeps_Them;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  //the control : the same project and cache, with every lock matching, restores and builds.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsTrue(RestoreLockedProject(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), [MakeDep('VSoft.Bar', '1.0.0')], projectText),
                logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(BuildCount > 0, 'the packages should have been built : ' + logger.Messages.Text);
  Assert.IsTrue(Pos('manifestHash="sha256:' + cBarHash + '"', projectText) > 0, 'the transitive lock was not kept : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Restore_Transitive_Lock_Mismatch_Fails_Before_Building;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  //VSoft.Bar is only a dependency of VSoft.Foo - its lock counts as much as a top level one.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(RestoreLockedProject(FTempFolder, loggerIntf, LockedConsumerProject(cStaleHash), [MakeDep('VSoft.Bar', '1.0.0')], projectText));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E3007_LockHashMismatch), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'VSoft.Bar'), 'the error names the package : ' + logger.Messages.Text);
  Assert.AreEqual(0, BuildCount, 'nothing may be built from a package that is not what the project pinned : ' + logger.Messages.Text);
  Assert.IsTrue(Pos('manifestHash="sha256:' + cStaleHash + '"', projectText) > 0, 'the recorded lock must be kept : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Restore_Re_Resolve_Keeps_Transitive_Lock;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  //VSoft.Foo now also needs VSoft.Baz, which the project hasn't recorded - the graph is re-resolved, and
  //VSoft.Bar, still at the version it was locked at, is still held to its lock.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(RestoreLockedProject(FTempFolder, loggerIntf, LockedConsumerProject(cStaleHash),
                                      [MakeDep('VSoft.Bar', '1.0.0'), MakeDep('VSoft.Baz', '1.0.0')], projectText));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E3007_LockHashMismatch), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'VSoft.Bar'), 'the error names the package : ' + logger.Messages.Text);
  Assert.AreEqual(0, BuildCount, 'nothing may be built from a package that is not what the project pinned : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Install_Keeps_The_Locks_Of_The_Packages_It_Keeps;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  //the control : installing VSoft.Baz builds, keeps the transitive lock on VSoft.Bar and locks VSoft.Baz.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsTrue(InstallIntoLockedProject(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), projectText), logger.Messages.Text);
  Assert.AreEqual(0, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(BuildCount > 0, 'the packages should have been built : ' + logger.Messages.Text);
  Assert.IsTrue(Pos('manifestHash="sha256:' + cBarHash + '"', projectText) > 0, 'the transitive lock was not kept : ' + projectText);
  Assert.IsTrue(Pos('manifestHash="sha256:' + cBazHash + '"', projectText) > 0, 'the installed package was not locked : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Install_Stale_Lock_On_A_Kept_Package_Fails_Before_Building;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  //installing something else doesn't make VSoft.Bar's lock go away - it used to be overwritten with whatever the cache held.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(InstallIntoLockedProject(FTempFolder, loggerIntf, LockedConsumerProject(cStaleHash), projectText));
  Assert.AreEqual(1, loggerIntf.ErrorCount, logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E3007_LockHashMismatch), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('error', 'VSoft.Bar'), 'the error names the package : ' + logger.Messages.Text);
  Assert.AreEqual(0, BuildCount, 'nothing may be built from a package that is not what the project pinned : ' + logger.Messages.Text);
  Assert.IsTrue(Pos('manifestHash="sha256:' + cStaleHash + '"', projectText) > 0, 'the project must not change : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Download_Of_A_Cached_Package_Uses_The_Source_Hash;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  root : string;
  projectFile : string;
  repo : TFakeRepositoryManager;
  repoIntf : IPackageRepositoryManager;
  cache : TPerPackageCache;
  cacheIntf : IPackageCache;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  config : IConfiguration;
  options : TRestoreOptions;
  reader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  root := TPath.Combine(FTempFolder, 'download');
  ForceDirectories(root);
  projectFile := TPath.Combine(root, 'Consumer.dproj');
  TFile.WriteAllText(projectFile, cConsumerProject, TEncoding.UTF8);
  repo := TFakeRepositoryManager.Create;
  repoIntf := repo;
  repo.SetVersions('VSoft.Foo', ListOf([TPackageInfo.Create('test-source', 'VSoft.Foo', TPackageVersion.Parse('1.0.0'), cTestCompiler, cFooHash, 'SHA256')]));
  reader := TPackageSpecReader.Create(loggerIntf);
  cache := TPerPackageCache.Create(root, reader.ReadSpecString(cPackageSpec));
  cacheIntf := cache;
  //what the cache knows of it - read from its own .sha256 file, which no longer matches anything.
  cache.RegisterInfo(TPackageInfo.Create('', 'VSoft.Foo', TPackageVersion.Parse('1.0.0'), cTestCompiler, cStaleHash, 'SHA256'));
  cache.Missing.Add('vsoft.foo');
  context := TFakeInstallerContext.Create(loggerIntf);
  config := TConfiguration.Create(loggerIntf);
  installer := TPackageInstaller.Create(loggerIntf, TFakeConfigurationManager.Create(config), repoIntf, cacheIntf,
                                        TDependencyResolver.Create(loggerIntf, repoIntf, cacheIntf, context), context,
                                        TFakeCompilerFactory.Create(loggerIntf, coSucceed, nil), nil, nil, nil, nil);
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := [TDPMPlatform.Win32];
    installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  Assert.AreEqual(1, repo.Downloaded.Count, 'the package should have been downloaded : ' + logger.Messages.Text);
  Assert.AreEqual(cFooHash, repo.Downloaded[0].Hash, 'the download was checked against the cache''s record of the hash');
  Assert.AreEqual('test-source', repo.Downloaded[0].SourceName);
end;

//Installs VSoft.Baz into the two platform project with failing's build failing - checks the install fails, says so
//once, and changed the project for building but not for failing.
procedure CheckPartialInstall(const tempFolder : string; const failing : TDPMPlatform; const building : TDPMPlatform);
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
  context : IPackageInstallerContext;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(InstallIntoLockedProjectFor(tempFolder, loggerIntf, LockedConsumerProject(cBarHash), TwoPlatformPackageSpec,
                                             [TDPMPlatform.Win32, TDPMPlatform.Win64], [failing], projectText, context),
                 'a platform failed - the install did not entirely succeed : ' + logger.Messages.Text);
  Assert.AreEqual(1, loggerIntf.ErrorCount, 'the failed build, once : ' + logger.Messages.Text);
  Assert.IsTrue(logger.HasError(TErrorCode.E5003_PackageCompileFailed), logger.Messages.Text);
  Assert.IsTrue(Pos('id="VSoft.Baz"', projectText) > 0, 'the package reference was not written : ' + projectText);
  Assert.IsTrue(Pos('VSoft.Baz', SearchPathFor(projectText, DPMPlatformToBDString(building))) > 0,
                'the search path of the platform that built was not written : ' + projectText);
  Assert.AreEqual(0, Pos('VSoft.Baz', SearchPathFor(projectText, DPMPlatformToBDString(failing))),
                  'the failed platform''s search path must not change : ' + projectText);
  Assert.IsTrue(context.AnyProjectCommitted, 'the project was saved - the IDE has to know to reload it');
end;

procedure TInstallerDiagnosticsTests.Install_A_Failing_Last_Platform_Still_Installs_The_Others;
begin
  CheckPartialInstall(FTempFolder, TDPMPlatform.Win64, TDPMPlatform.Win32);
end;

procedure TInstallerDiagnosticsTests.Install_A_Failing_First_Platform_Still_Installs_The_Others;
begin
  CheckPartialInstall(FTempFolder, TDPMPlatform.Win32, TDPMPlatform.Win64);
end;

procedure TInstallerDiagnosticsTests.Install_Failing_On_Every_Platform_Changes_Nothing;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
  context : IPackageInstallerContext;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(InstallIntoLockedProjectFor(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), TwoPlatformPackageSpec,
                                             [TDPMPlatform.Win32, TDPMPlatform.Win64], [TDPMPlatform.Win32, TDPMPlatform.Win64],
                                             projectText, context));
  Assert.AreEqual(2, loggerIntf.ErrorCount, 'one failed build per platform - each was tried : ' + logger.Messages.Text);
  Assert.AreEqual(0, Pos('VSoft.Baz', projectText), 'nothing built - the project must not change : ' + projectText);
  Assert.IsFalse(context.AnyProjectCommitted, 'no project was saved');
end;

//The platform the IDE would load design packages for, were this process the IDE - and one it isn't.
function HostPlatform : TDPMPlatform;
begin
  if TSystemUtils.Is64BitProcess then
    result := TDPMPlatform.Win64
  else
    result := TDPMPlatform.Win32;
end;

function OtherPlatform : TDPMPlatform;
begin
  if HostPlatform = TDPMPlatform.Win32 then
    result := TDPMPlatform.Win64
  else
    result := TDPMPlatform.Win32;
end;

//cPackageSpec for the platforms given (eg 'Win32, Win64'), with a design entry when withDesign.
function PackageSpecFor(const platforms : string; const withDesign : boolean) : string;
begin
  result := StringReplace(cPackageSpec, 'platforms: [Win32]', 'platforms: [' + platforms + ']', []);
  if withDesign then
    result := result +
      '    design:' + sLineBreak +
      '      - project: .\source\PkgD.dproj' + sLineBreak +
      '        platforms: [' + platforms + ']' + sLineBreak;
end;

//The locked consumer project with VSoft.Baz installed as well.
function LockedConsumerProjectWithBaz : string;
begin
  result := StringReplace(LockedConsumerProject(cBarHash), '            </Packages>' + sLineBreak,
    '                <PackageReference id="VSoft.Baz" version="1.0.0" manifestHash="sha256:' + cBazHash + '"/>' + sLineBreak +
    '            </Packages>' + sLineBreak, []);
end;

//Restores the project text given over the locked fixture, in the IDE, for the platform the IDE doesn't run on - so
//the design-time packages are also built for the one it does. VSoft.Foo uses VSoft.Bar; the packages build for
//both platforms unless specOverrides says otherwise.
function RestoreInTheIDE(const tempFolder : string; const logger : ILogger; const project : string;
                         const specOverrides : IDictionary<string, string>; const context : IPackageInstallerContext) : boolean;
var
  projectFile : string;
  installer : IPackageInstaller;
  options : TRestoreOptions;
  repository : TFakeRepositoryManager;
begin
  installer := NewLockedFixtureWith(tempFolder, logger, project, [MakeDep('VSoft.Bar', '1.0.0')], PackageSpecFor('Win32, Win64', false),
                                    specOverrides, [], [], context, projectFile, repository);
  options := TRestoreOptions.Create;
  TSystemUtils.SetIsIDE(true);
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := [OtherPlatform];
    result := installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    TSystemUtils.SetIsIDE(false);
    options.Free;
  end;
end;

//Whether the fake compiler was asked to build a project of the package (id) for the platform.
function WasBuilt(const platform : TDPMPlatform; const id : string) : boolean;
var
  build : string;
begin
  result := false;
  for build in BuiltProjects do
    if StartsText(DPMPlatformToString(platform) + ' ', build) and ContainsText(build, '\' + id + '\') then
      exit(true);
end;

//The items, a line each - for assertion messages.
function ListText(const items : IList<string>) : string;
var
  item : string;
begin
  result := '';
  for item in items do
    result := result + sLineBreak + item;
end;

//The W5010s logged.
function DesignHostWarnings(const logger : TTestLogger) : IList<string>;
var
  i : integer;
begin
  result := TCollections.CreateList<string>;
  for i := 0 to logger.Messages.Count - 1 do
    if ContainsText(logger.Messages[i], WarningCodeToString(TWarningCode.W5010_DesignHostCompileFailed)) then
      result.Add(logger.Messages[i]);
end;

//A package folder as DPM leaves it after a build : the source, DPM's compiled output (with the .dfm/.res
//copyToLib puts beside it) and the signatures - and a lib folder of source the package has itself.
procedure WritePackageFolderWithOutput(const packageFolder : string);
begin
  ForceDirectories(TPath.Combine(packageFolder, 'source'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'source\Foo.pas'), 'unit Foo; interface implementation end.', TEncoding.UTF8);
  ForceDirectories(TPath.Combine(packageFolder, 'lib\Win32'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'lib\Win32\Foo.dfm'), 'object Foo end', TEncoding.UTF8);
  TFile.WriteAllText(TPath.Combine(packageFolder, 'lib\Common.pas'), 'unit Common; interface implementation end.', TEncoding.UTF8);
  ForceDirectories(TPath.Combine(packageFolder, 'bpl\Win64'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'bpl\Win64\Foo.res'), 'not really a res', TEncoding.UTF8);
  ForceDirectories(TPath.Combine(packageFolder, 'signatures'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'signatures\Evil.pas'), 'unit Evil; interface implementation end.', TEncoding.UTF8);
end;

procedure CheckOnlySourceFoldersAdded(const projectText : string);
begin
  Assert.IsTrue(ContainsText(projectText, '$(DPM)\VSoft.Foo\1.0.0\source;'), 'the source folder : ' + projectText);
  Assert.IsTrue(ContainsText(projectText, '$(DPM)\VSoft.Foo\1.0.0\lib;'), 'the package''s own folder of source : ' + projectText);
  Assert.IsFalse(ContainsText(projectText, '\lib\Win32'), 'DPM''s compiled output is not source : ' + projectText);
  Assert.IsFalse(ContainsText(projectText, '\bpl\Win64'), 'DPM''s compiled output is not source : ' + projectText);
  Assert.IsFalse(ContainsText(projectText, '\signatures'), 'the signatures are not source : ' + projectText);
end;

const
  cOtherProject = 'C:\test\other.dproj';

//The consumer project with VSoft.Foo locked but not the VSoft.Bar it uses - restoring it resolves VSoft.Bar again.
function FooOnlyLockedProject : string;
begin
  result := StringReplace(ConsumerProjectWithBase,
    '<PackageReference id="VSoft.Foo" version="1.0.0"/>',
    '<PackageReference id="VSoft.Foo" version="1.0.0" manifestHash="sha256:' + cFooHash + '"/>', []);
end;

//Restores the project over the fixture for Win32.
function RestoreForWin32(const installer : IPackageInstaller; const context : IPackageInstallerContext; const projectFile : string) : boolean;
var
  options : TRestoreOptions;
begin
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := [TDPMPlatform.Win32];
    result := installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
end;

//A folder holding two projects with no packages, one for Delphi 12 (ProjectVersion 20.1) and one for Delphi 11 (19.5).
function TwoCompilerProjectsFolder(const tempFolder : string) : string;
var
  noPackages : string;
begin
  result := TPath.Combine(tempFolder, 'two-compilers');
  if DirectoryExists(result) then
    TDirectory.Delete(result, true);
  ForceDirectories(result);
  noPackages := StringReplace(ConsumerProjectWithBase, '                <PackageReference id="VSoft.Foo" version="1.0.0"/>' + sLineBreak, '', []);
  TFile.WriteAllText(TPath.Combine(result, 'A12.dproj'), noPackages, TEncoding.UTF8);
  TFile.WriteAllText(TPath.Combine(result, 'B11.dproj'), StringReplace(noPackages, '<ProjectVersion>20.1</ProjectVersion>', '<ProjectVersion>19.5</ProjectVersion>', []), TEncoding.UTF8);
end;

procedure TInstallerDiagnosticsTests.Restore_A_Failing_Platform_Still_Restores_The_Others;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectText : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  Assert.IsFalse(RestoreLockedProjectFor(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), [MakeDep('VSoft.Bar', '1.0.0')],
                                         TwoPlatformPackageSpec, [TDPMPlatform.Win32, TDPMPlatform.Win64], [TDPMPlatform.Win64], projectText),
                 'a platform failed - the restore did not entirely succeed : ' + logger.Messages.Text);
  Assert.AreEqual(1, loggerIntf.ErrorCount, 'the failed build, once : ' + logger.Messages.Text);
  Assert.IsTrue(Pos('VSoft.Foo', SearchPathFor(projectText, 'Win32')) > 0, 'the Win32 search path was not written : ' + projectText);
  Assert.AreEqual(0, Pos('VSoft.Foo', SearchPathFor(projectText, 'Win64')), 'the failed platform''s search path must not change : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Cache_A_Failing_Platform_Still_Builds_The_Others;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  build : string;
  builtWin64 : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  BuiltProjects := TCollections.CreateList<string>;
  //Win32 - the first platform built.
  FailingBuilds := TCollections.CreateList<string>;
  FailingBuilds.Add(DPMPlatformToString(TDPMPlatform.Win32) + '|Pkg.dproj');
  try
    Assert.IsFalse(CachePackageSpec(FTempFolder, loggerIntf, coSucceed, TCancellationTokenSourceFactory.Create, TwoPlatformPackageSpec),
                   'a platform failed - the cache install did not entirely succeed : ' + logger.Messages.Text);
  finally
    FailingBuilds := nil;
  end;
  builtWin64 := false;
  for build in BuiltProjects do
    if StartsText(DPMPlatformToString(TDPMPlatform.Win64) + ' ', build) then
      builtWin64 := true;
  Assert.IsTrue(builtWin64, 'Win32 failing must not stop Win64 being built : ' + ListText(BuiltProjects));
  Assert.AreEqual(1, loggerIntf.ErrorCount, 'the failed build, once : ' + logger.Messages.Text);
  Assert.IsTrue(ContainsText(logger.Messages.Text, 'failed on platforms [' + DPMPlatformToString(TDPMPlatform.Win32) + ']'),
                'it says which platform didn''t build : ' + logger.Messages.Text);
end;

//The search paths package id's build for platform was given (see BuildSearchPaths) - fails when it wasn't built.
function SearchPathsOfBuild(const platform : TDPMPlatform; const id : string) : string;
var
  build : string;
  separator : integer;
begin
  for build in BuildSearchPaths do
  begin
    separator := Pos('|', build);
    if StartsText(DPMPlatformToString(platform) + ' ', build) and ContainsText(Copy(build, 1, separator - 1), '\' + id + '\') then
      exit(Copy(build, separator + 1, MaxInt));
  end;
  Assert.Fail(id + ' was not built for ' + DPMPlatformToString(platform) + ' : ' + ListText(BuildSearchPaths));
  result := '';
end;

procedure TInstallerDiagnosticsTests.Package_Built_For_Linux_Finds_Its_Dependencies_Runtime_Packages;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  project : string;
  spec : string;
  projectText : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //a consumer of VSoft.Foo, which depends on VSoft.Bar, for Win32 and Linux64.
  project := StringReplace(LockedConsumerProject(cBarHash), '<Platform value="Win64">True</Platform>', '<Platform value="Linux64">True</Platform>', []);
  spec := StringReplace(cPackageSpec, 'platforms: [Win32]', 'platforms: [Win32, Linux64]', []);
  BuildSearchPaths := TCollections.CreateList<string>;
  try
    Assert.IsTrue(RestoreLockedProjectFor(FTempFolder, loggerIntf, project, [MakeDep('VSoft.Bar', '1.0.0')], spec,
                                          [TDPMPlatform.Win32, TDPMPlatform.Linux64], [], projectText),
                  'the restore : ' + logger.Messages.Text);
    Assert.IsTrue(ContainsText(SearchPathsOfBuild(TDPMPlatform.Linux64, 'VSoft.Foo'), '\VSoft.Bar\bpl\Linux64'),
                  'VSoft.Bar''s runtime package is linked against : ' + ListText(BuildSearchPaths));
    Assert.IsTrue(ContainsText(SearchPathsOfBuild(TDPMPlatform.Linux64, 'VSoft.Foo'), '\VSoft.Bar\lib\Linux64'),
                  'and its units are still found : ' + ListText(BuildSearchPaths));
    Assert.IsFalse(ContainsText(SearchPathsOfBuild(TDPMPlatform.Win32, 'VSoft.Foo'), '\VSoft.Bar\bpl\'),
                   'Windows links against the .dcp in lib : ' + ListText(BuildSearchPaths));
  finally
    BuildSearchPaths := nil;
  end;
end;

procedure TInstallerDiagnosticsTests.Design_Host_Build_Skips_A_Package_That_Does_Not_Declare_The_Host;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  overrides : IDictionary<string, string>;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //VSoft.Bar is only for the project's platform.
  overrides := TCollections.CreateDictionary<string, string>;
  overrides['vsoft.bar'] := PackageSpecFor(DPMPlatformToString(OtherPlatform), false);

  Assert.IsTrue(RestoreInTheIDE(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), overrides, TFakeInstallerContext.Create(loggerIntf)),
                logger.Messages.Text);
  Assert.IsTrue(WasBuilt(OtherPlatform, 'VSoft.Bar'), 'precondition : the project''s platform is built');
  Assert.IsFalse(WasBuilt(HostPlatform, 'VSoft.Bar'), 'VSoft.Bar does not declare the platform the IDE runs on : ' + ListText(BuiltProjects));
  Assert.IsFalse(WasBuilt(HostPlatform, 'VSoft.Foo'), 'VSoft.Foo can''t build without VSoft.Bar : ' + ListText(BuiltProjects));
  Assert.AreEqual(0, DesignHostWarnings(logger).Count, 'neither has design packages to load : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Design_Host_Build_Keeps_Going_And_Warns_Once;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  warnings : IList<string>;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //VSoft.Baz is built first, and fails - VSoft.Bar fails too.
  FailingBuilds := TCollections.CreateList<string>;
  FailingBuilds.Add(DPMPlatformToString(HostPlatform) + '|\VSoft.Baz\');
  FailingBuilds.Add(DPMPlatformToString(HostPlatform) + '|\VSoft.Bar\');
  try
    Assert.IsTrue(RestoreInTheIDE(FTempFolder, loggerIntf, LockedConsumerProjectWithBaz, nil, TFakeInstallerContext.Create(loggerIntf)),
                  'the project''s own platform built : ' + logger.Messages.Text);
  finally
    FailingBuilds := nil;
  end;
  Assert.IsTrue(WasBuilt(HostPlatform, 'VSoft.Baz') and WasBuilt(HostPlatform, 'VSoft.Bar'),
                'one failure must not stop the other packages being built : ' + ListText(BuiltProjects));
  Assert.IsFalse(WasBuilt(HostPlatform, 'VSoft.Foo'), 'VSoft.Foo can''t build without VSoft.Bar : ' + ListText(BuiltProjects));
  Assert.AreEqual(0, loggerIntf.ErrorCount, 'not the operation''s failure : ' + logger.Messages.Text);
  warnings := DesignHostWarnings(logger);
  Assert.AreEqual(1, warnings.Count, 'one warning : ' + logger.Messages.Text);
  Assert.IsTrue(ContainsText(warnings[0], 'VSoft.Baz') and ContainsText(warnings[0], 'VSoft.Bar'), 'it names every failure : ' + warnings[0]);
end;

procedure TInstallerDiagnosticsTests.Design_Host_Package_That_Was_Not_Built_Is_Not_Loaded;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  overrides : IDictionary<string, string>;
  context : TRecordingInstallerContext;
  contextIntf : IPackageInstallerContext;
  warnings : IList<string>;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  context := TRecordingInstallerContext.Create(loggerIntf);
  contextIntf := context;
  //VSoft.Foo has design packages - but needs VSoft.Bar, which is only for the project's platform.
  overrides := TCollections.CreateDictionary<string, string>;
  overrides['vsoft.bar'] := PackageSpecFor(DPMPlatformToString(OtherPlatform), false);
  overrides['vsoft.foo'] := PackageSpecFor('Win32, Win64', true);

  Assert.IsTrue(RestoreInTheIDE(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), overrides, contextIntf), logger.Messages.Text);
  Assert.IsFalse(WasBuilt(HostPlatform, 'VSoft.Foo'), 'VSoft.Foo can''t build without VSoft.Bar : ' + ListText(BuiltProjects));
  warnings := DesignHostWarnings(logger);
  Assert.AreEqual(1, warnings.Count, logger.Messages.Text);
  Assert.IsTrue(ContainsText(warnings[0], 'VSoft.Foo') and ContainsText(warnings[0], 'VSoft.Bar'), 'it says what was not built, and why : ' + warnings[0]);
  Assert.IsTrue(context.LoadedIds.Contains('vsoft.bar'), 'precondition : the design packages were loaded : ' + ListText(context.LoadedIds));
  Assert.IsFalse(context.LoadedIds.Contains('vsoft.foo'), 'VSoft.Foo has no design package for the IDE to load');
end;

procedure TInstallerDiagnosticsTests.Packed_Package_Source_Walk_Skips_The_Package_Output;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-output');
  WritePackageFolderWithOutput(packageFolder);
  //a packed package's source entries are not in the manifest - every folder with source in it is added.
  refreshed := RefreshSearchPaths(packageFolder, '      []' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  CheckOnlySourceFoldersAdded(projectText);
end;

procedure TInstallerDiagnosticsTests.Git_Source_Walk_Skips_The_Package_Output;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-git-output');
  WritePackageFolderWithOutput(packageFolder);
  //a git package is built where it was cloned - its output is in the folder the ** walk starts from.
  TFile.WriteAllText(TPath.Combine(packageFolder, cGitPackageMarkerFile), 'abc', TEncoding.UTF8);
  refreshed := RefreshSearchPaths(packageFolder, '      - src: ''**\*.pas''' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  CheckOnlySourceFoldersAdded(projectText);
end;

procedure TInstallerDiagnosticsTests.Use_Source_In_One_Project_Does_Not_Carry_Over_To_Another;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectFile : string;
  otherProject : string;
  otherText : string;
  root : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  options : TRestoreOptions;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  installer := NewLockedFixture(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), [MakeDep('VSoft.Bar', '1.0.0')], cPackageSpec, [],
                                projectFile, context);
  root := ExtractFileDir(projectFile);
  //both are built - the search paths point at their compiled output unless source is wanted.
  ForceDirectories(TPath.Combine(root, 'VSoft.Foo\lib\Win32'));
  ForceDirectories(TPath.Combine(root, 'VSoft.Bar\lib\Win32'));

  //'use source' ticked for VSoft.Foo in the project - VSoft.Bar, which it uses, comes as source too.
  context.SetUseSource(projectFile, 'VSoft.Foo', true);
  done := installer.RefreshProjectSearchPaths(TCancellationTokenSourceFactory.Create.Token, projectFile, cTestCompiler, 'dpm.config.yaml', context);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(ContainsText(SearchPathFor(TFile.ReadAllText(projectFile), 'Win32'), 'VSoft.Bar\1.0.0\source'),
                'precondition : the project uses the source : ' + TFile.ReadAllText(projectFile));

  //another project using VSoft.Foo, which hasn't recorded VSoft.Bar - restoring it resolves VSoft.Bar again.
  otherProject := TPath.Combine(root, 'Other.dproj');
  TFile.WriteAllText(otherProject, StringReplace(ConsumerProjectWithBase,
    '<PackageReference id="VSoft.Foo" version="1.0.0"/>',
    '<PackageReference id="VSoft.Foo" version="1.0.0" manifestHash="sha256:' + cFooHash + '"/>', []), TEncoding.UTF8);
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := otherProject;
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    options.Platforms := [TDPMPlatform.Win32];
    done := installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  Assert.IsTrue(done, logger.Messages.Text);
  otherText := TFile.ReadAllText(otherProject);
  Assert.IsTrue(ContainsText(SearchPathFor(otherText, 'Win32'), 'VSoft.Bar\1.0.0\lib\Win32'), 'the other project uses the compiled VSoft.Bar : ' + otherText);
  Assert.IsFalse(ContainsText(SearchPathFor(otherText, 'Win32'), '\source'), 'the other project did not choose source : ' + otherText);
end;

procedure TInstallerDiagnosticsTests.Refresh_Leaves_A_Package_Out_Of_A_Platform_It_Does_Not_Support;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFolder : string;
  projectText : string;
  refreshed : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  packageFolder := TPath.Combine(FTempFolder, 'search-one-platform');
  ForceDirectories(TPath.Combine(packageFolder, 'source'));
  TFile.WriteAllText(TPath.Combine(packageFolder, 'source\Foo.pas'), 'unit Foo; interface implementation end.', TEncoding.UTF8);
  //the project targets Win32 and Win64 - VSoft.Foo is for Win32 only.
  refreshed := RefreshSearchPathsWithSpec(packageFolder,
                 StringReplace(cSourcePackageSpecHead, 'platforms: [Win32, Win64]', 'platforms: [Win32]', []) +
                 '      - src: ''.\source\*.pas''' + sLineBreak +
                 '        dest: source' + sLineBreak, loggerIntf, projectText);
  Assert.IsTrue(refreshed, logger.Messages.Text);
  Assert.IsTrue(ContainsText(SearchPathFor(projectText, 'Win32'), 'VSoft.Foo'), 'precondition : Win32 has it : ' + projectText);
  Assert.IsFalse(ContainsText(SearchPathFor(projectText, 'Win64'), 'VSoft.Foo'), 'VSoft.Foo is not for Win64 : ' + projectText);
end;

procedure TInstallerDiagnosticsTests.Restore_Records_The_Graph_It_Validates_For_The_Other_Projects;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectFile : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  done : boolean;
  resolution : IResolvedPackage;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //the project's graph is intact - the restore doesn't need the resolver.
  installer := NewLockedFixture(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), [MakeDep('VSoft.Bar', '1.0.0')], cPackageSpec, [],
                                projectFile, context);
  done := RestoreForWin32(installer, context, projectFile);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(logger.Logged('verbose', 'skipping resolution'), 'precondition : the graph was found intact : ' + logger.Messages.Text);
  resolution := context.FindPackageResolution(cOtherProject, 'VSoft.Bar');
  Assert.IsNotNull(resolution, 'another project in the group must be checked against what this one uses');
  Assert.AreEqual('1.0.0', resolution.PackageInfo.Version.ToStringNoMeta);
  Assert.IsNotNull(context.FindPackageResolution(cOtherProject, 'VSoft.Foo'));
end;

procedure TInstallerDiagnosticsTests.Restore_That_Keeps_Nothing_Records_Nothing;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectFile : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //VSoft.Bar is resolved again - but nothing builds, so the project isn't saved with it.
  installer := NewLockedFixture(FTempFolder, loggerIntf, FooOnlyLockedProject, [MakeDep('VSoft.Bar', '1.0.0')], cPackageSpec, [TDPMPlatform.Win32],
                                projectFile, context);
  done := RestoreForWin32(installer, context, projectFile);
  Assert.IsFalse(done, 'precondition : the restore failed : ' + logger.Messages.Text);
  Assert.IsNull(context.FindPackageResolution(cOtherProject, 'VSoft.Bar'), 'the project did not keep the resolution - nothing may be checked against it');
end;

procedure TInstallerDiagnosticsTests.Uninstall_Drops_What_The_Project_Recorded;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectFile : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  options : TUnInstallOptions;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  installer := NewLockedFixture(FTempFolder, loggerIntf, LockedConsumerProject(cBarHash), [MakeDep('VSoft.Bar', '1.0.0')], cPackageSpec, [],
                                projectFile, context);
  //what an earlier install or restore of the project recorded.
  (context as TFakeInstallerContext).SeedResolutions(projectFile,
    [TResolvedPackage.Create(MakeInfo('VSoft.Foo', '1.0.0', [MakeDep('VSoft.Bar', '1.0.0')]), MakeRange('1.0.0'), cRootNode, projectFile),
     TResolvedPackage.Create(MakeInfo('VSoft.Bar', '1.0.0', []), MakeRange('1.0.0'), 'VSoft.Foo', projectFile)]);
  Assert.IsNotNull(context.FindPackageResolution(cOtherProject, 'VSoft.Bar'), 'precondition');

  options := TUnInstallOptions.Create;
  try
    options.ProjectPath := projectFile;
    options.PackageId := 'VSoft.Foo';
    options.CompilerVersion := cTestCompiler;
    options.ConfigFile := 'dpm.config.yaml';
    done := installer.Uninstall(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsNull(context.FindPackageResolution(cOtherProject, 'VSoft.Foo'), 'VSoft.Foo was removed from the project');
  Assert.IsNull(context.FindPackageResolution(cOtherProject, 'VSoft.Bar'), 'nothing else in the project uses VSoft.Bar');
end;

procedure TInstallerDiagnosticsTests.Cache_Install_With_Force_Builds_The_Package_Again;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  packageFile : string;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  //built once - the BOM it leaves says nothing has changed since.
  done := CachePackage(FTempFolder, loggerIntf, coSucceed, TCancellationTokenSourceFactory.Create);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(FileExists(TPath.Combine(FTempFolder, 'package\package.Win32.bom')), 'precondition : the build left its BOM');
  done := CacheAgain(FTempFolder, loggerIntf, cPackageId, false);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.AreEqual(0, BuildCount, 'precondition : without --force it is not built again');

  done := CacheAgain(FTempFolder, loggerIntf, cPackageId, true);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(BuildCount > 0, 'asked for by id, --force builds it again');

  packageFile := TPath.Combine(FTempFolder, cPackageId + '-' + CompilerToString(cTestCompiler) + '-win32-1.0.0.dpkg');
  TFile.WriteAllText(packageFile, 'the fake cache does not read it', TEncoding.UTF8);
  done := CacheAgain(FTempFolder, loggerIntf, packageFile, true);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(BuildCount > 0, 'from a package file, --force builds it again');
end;

procedure TInstallerDiagnosticsTests.Restore_Uses_Each_Projects_Own_Compiler;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  installer : IPackageInstaller;
  context : IPackageInstallerContext;
  options : TRestoreOptions;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  context := TFakeInstallerContext.Create(loggerIntf);
  installer := NewInstaller(loggerIntf, TFakeRepositoryManager.Create, TFakePackageCache.Create, context,
                            TFakeCompilerFactory.Create(loggerIntf, coSucceed, nil));
  options := TRestoreOptions.Create;
  try
    options.ProjectPath := TwoCompilerProjectsFolder(FTempFolder);
    options.ConfigFile := 'dpm.config.yaml';
    done := installer.Restore(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(logger.Logged('information', 'compiler version  [' + CompilerToString(TCompilerVersion.Delphi12_0) + ']'), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('information', 'compiler version  [' + CompilerToString(TCompilerVersion.Delphi11_0) + ']'), 'each project is restored for its own compiler : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Uninstall_Uses_Each_Projects_Own_Compiler;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  installer : IPackageInstaller;
  context : IPackageInstallerContext;
  options : TUnInstallOptions;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  context := TFakeInstallerContext.Create(loggerIntf);
  installer := NewInstaller(loggerIntf, TFakeRepositoryManager.Create, TFakePackageCache.Create, context,
                            TFakeCompilerFactory.Create(loggerIntf, coSucceed, nil));
  options := TUnInstallOptions.Create;
  try
    options.ProjectPath := TwoCompilerProjectsFolder(FTempFolder);
    options.PackageId := 'VSoft.Foo';
    options.ConfigFile := 'dpm.config.yaml';
    done := installer.Uninstall(TCancellationTokenSourceFactory.Create.Token, options, context);
  finally
    options.Free;
  end;
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(logger.Logged('information', 'compiler version  [' + CompilerToString(TCompilerVersion.Delphi12_0) + ']'), logger.Messages.Text);
  Assert.IsTrue(logger.Logged('information', 'compiler version  [' + CompilerToString(TCompilerVersion.Delphi11_0) + ']'), 'each project is uninstalled from for its own compiler : ' + logger.Messages.Text);
end;

procedure TInstallerDiagnosticsTests.Restore_Re_Resolve_Does_Not_Look_Up_What_It_Resolved;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  projectFile : string;
  context : IPackageInstallerContext;
  installer : IPackageInstaller;
  repository : TFakeRepositoryManager;
  done : boolean;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  context := TFakeInstallerContext.Create(loggerIntf);
  //the project hasn't recorded VSoft.Bar, and it isn't in the cache - the resolve finds it on the source.
  installer := NewLockedFixtureWith(FTempFolder, loggerIntf, FooOnlyLockedProject, [MakeDep('VSoft.Bar', '1.0.0')], cPackageSpec, nil,
                                    ['VSoft.Bar'], [], context, projectFile, repository);
  done := RestoreForWin32(installer, context, projectFile);
  Assert.IsTrue(done, logger.Messages.Text);
  Assert.IsTrue(repository.GetCallCount('VSoft.Bar') > 0, 'precondition : VSoft.Bar was resolved from the source');
  Assert.AreEqual(0, repository.InfoCallCount, 'what the resolve found is not looked up again');
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallerDiagnosticsTests);

end.
