unit DPM.Core.Tests.SBOM.Generator;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  JsonDataObjects,
  Spring.Container,
  DPM.Core.Logging,
  TestLogger,
  DPM.Core.Options.SBOM,
  DPM.Core.SBOM.ProductInfo,
  DPM.Core.SBOM.Interfaces;

type
  //End to end : a dproj (or groupproj) and its MAP file on disk go in, the CycloneDX / SPDX files
  //the generator writes come out. The fixture projects reference no DPM packages, so the package
  //cache is never asked for a spec - it only supplies its location.
  {$M+}
  [TestFixture]
  TSBOMGeneratorTests = class
  private
    FTempDir : string;
    FContainer : TContainer;
    //the logger the last generator was created with - FLoggerRef keeps it alive
    FLogger : TTestLogger;
    FLoggerRef : ILogger;
    //AppType written by MakeDproj - Application unless a test says otherwise
    FAppType : string;
    //DPM package references MakeDproj writes into ProjectExtensions, as PackageReference elements
    FDpmPackages : string;
    //what the generator takes as the Delphi install folder - empty (none) unless a test sets it,
    //so results never depend on which Delphi is installed on the machine running the tests
    FDelphiRoot : string;
    function CreateGenerator : ISbomGenerator;
    procedure WriteBytes(const fileName, content : string);
    //content of the hashes[] entry with this alg, '' when there is none
    function HashOf(const comp : TJsonObject; const alg : string) : string;
    function WriteConfig : string;
    function MakeDproj(const projectName, baseWin32Props, releaseWin32Props : string) : string;
    function VerInfo(const keys : string) : string;
    procedure WriteDproj(const fileName, projectName, baseWin32Props, releaseWin32Props : string);
    procedure WriteMap(const fileName : string; const units : array of string);
    procedure WriteGroupProj(const fileName : string; const projects : array of string);
    procedure WriteText(const fileName, content : string);
    function Generate(const projectPath, configName, outDir : string; const spdx : boolean) : boolean;
    //configure runs after the defaults (Release, CycloneDX only, no runtime) are applied
    function GenerateWith(const projectPath, outDir : string; const configure : TProc<TSBOMOptions>) : boolean;
    function RootOf(const doc : TJsonObject) : TJsonObject;
    function LoadJson(const fileName : string) : TJsonObject;
    function FindComponent(const doc : TJsonObject; const name : string) : TJsonObject;
    function ComponentsOfKind(const doc : TJsonObject; const kind : string) : TArray<TJsonObject>;
    function DependsOn(const doc : TJsonObject; const parentRef, childRef : string) : boolean;
    procedure AssertBomRefsAreUnique(const doc : TJsonObject);
    procedure WriteSharedComponentsGroup;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure RootVersion_IsTheProductVersionFromVersionInfo;
    [Test]
    procedure RootVersion_UsesTheSelectedConfiguration;
    [Test]
    procedure RootVersion_FallsBackToFileVersion;
    [Test]
    procedure RootVersion_IsOmittedWithoutVersionInfo;

    [Test]
    procedure ThirdPartyFoldersWithTheSameName_GetDistinctBomRefs;
    [Test]
    procedure ThirdPartyLibraryWithSeveralUnits_IsLinkedOnce;
    [Test]
    procedure ThirdPartyComponent_HasNoInventedVersion;
    [Test]
    procedure UnidentifiedComponent_HasNoInventedVersion;

    [Test]
    procedure GroupProject_ComponentsSharedByProjects_AreEmittedOnce;
    [Test]
    procedure GroupProject_SharedThirdParty_IsLinkedFromEachProject;
    [Test]
    procedure GroupProject_SharedComponents_DoNotBreakSPDXOutput;
    [Test]
    procedure GroupProject_ProjectVersionsComeFromVersionInfo;
    [Test]
    procedure GroupProject_RootHasNoInventedVersion;

    //product metadata - CLI options, the <project>.sbom.yaml sidecar, version info fallbacks
    [Test]
    procedure Supplier_FromOptions_IsWrittenOnTheRoot;
    [Test]
    procedure SupplierUrl_ThatIsAnEmail_IsTheSupplierContact;
    [Test]
    procedure License_FromOptions_IsWrittenOnTheRoot;
    [Test]
    procedure Version_FromOptions_OverridesVersionInfo;
    [Test]
    procedure Supplier_FallsBackToVersionInfoCompanyName;
    [Test]
    procedure SbomCreator_Email_IsTheManufacturerContact;
    [Test]
    procedure SbomCreator_Url_IsTheManufacturerUrl;
    [Test]
    procedure SbomCreator_ThatIsNeitherEmailNorUrl_FailsGeneration;
    [Test]
    procedure Sidecar_SuppliesProductMetadata;
    [Test]
    procedure Options_OverrideTheSidecar;
    [Test]
    procedure UnreadableSidecar_FailsGeneration;
    [Test]
    procedure SPDX_RootPackage_CarriesSupplierLicenseAndCreator;
    [Test]
    procedure GroupProject_ProjectSidecarOverridesGroupSupplier;
    [Test]
    procedure GroupProject_GroupVersion_DoesNotReplaceProjectVersions;

    //CRA / BSI header warnings
    [Test]
    procedure MissingMetadata_IsLoggedAsWarnings;
    [Test]
    procedure CompleteMetadata_IsNotWarned;
    [Test]
    procedure Warnings_AreListedInTheMarkdownReport;

    //the built binary - SHA-256 and SHA-512 of the deliverable the SBOM describes
    [Test]
    procedure BuiltExecutable_IsHashedOnTheRoot;
    [Test]
    procedure BuiltExecutable_InTheOutputDirectory_IsHashed;
    [Test]
    procedure BuiltLibrary_IsHashed;
    [Test]
    procedure BuiltPackage_WithAutoSuffix_IsHashed;
    [Test]
    procedure MissingBinary_IsWarned;
    [Test]
    procedure SPDX_RootPackage_HasBinaryChecksums;
    [Test]
    procedure GroupProject_EachProjectBinaryIsHashed;

    //runtime packages - the .bpl files the built binary imports
    [Test]
    procedure RuntimePackage_FromADpmPackage_IsAttributedToIt;
    [Test]
    procedure RuntimePackage_FromDelphi_IsEvidenceOnTheRuntimeComponent;
    [Test]
    procedure RuntimePackage_FromElsewhere_IsAComponentOfItsOwn;
    [Test]
    procedure DllImports_AreNotRuntimePackages;
    [Test]
    procedure GroupProject_SharedRuntimePackage_IsOneComponentLinkedFromEach;

    //CycloneDX compositions - how complete each component's dependency list is
    [Test]
    procedure Compositions_DpmPackageDependencies_AreComplete;
    [Test]
    procedure Compositions_LeafDpmPackage_StatesItHasNoDependencies;
    [Test]
    procedure Compositions_TheProjectsDependencies_AreIncomplete;
    [Test]
    procedure Compositions_ComponentsFoundInTheBuild_AreUnknown;
    [Test]
    procedure Compositions_GroupRoot_IsCompleteAndItsProjectsAreNot;

    //DLLs the built binary imports
    [Test]
    procedure DllImport_IsAComponentOfItsOwn;
    [Test]
    procedure ApiSetDll_IsNotAComponent;
    [Test]
    procedure DllFromDelphi_IsEvidenceOnTheRuntimeComponent;
    [Test]
    procedure DelayLoadedDll_IsMarkedAsSuch;
    [Test]
    procedure DllNextToTheBinary_IsHashed;
    [Test]
    procedure DllNextToTheBinary_TakesVersionAndSupplierFromItsVersionResource;
    [Test]
    procedure RuntimePackageNextToTheBinary_IsHashed;

    //component rules - the sidecar's components:, ownUnits and ownUnitPrefixes
    [Test]
    procedure ComponentRule_ClaimsMatchingMapUnits;
    [Test]
    procedure ComponentRule_ClaimsUnitsWithoutASourcePath;
    [Test]
    procedure ComponentRule_ClaimsLibrarySourceVendoredIntoTheProject;
    [Test]
    procedure OwnUnits_AreNotComponents;
    [Test]
    procedure DpmPackage_KeepsItsUnitsWhenARuleAlsoMatches;
    [Test]
    procedure DeclaredComponent_WithoutUnitRules_IsAlwaysListed;
    [Test]
    procedure ComponentRuleThatMatchesNothing_IsWarned;
    [Test]
    procedure ComponentRule_IsNotCalledStaleWithoutAMapFile;
    [Test]
    procedure ComponentRule_ForALibraryDpmManages_IsNotCalledStale;
    [Test]
    procedure RelativeSourcePath_IsResolvedThroughTheSearchPath;
    [Test]
    procedure ComponentRuleType_IsTheCycloneDXType;
    [Test]
    procedure GroupProject_GroupRule_IsOneComponentLinkedFromEachProject;
  end;

  //Which gaps in the product metadata get a CRA / BSI warning. Each warning names the sidecar key
  //that fixes it, which is how these tests tell them apart.
  [TestFixture]
  TSBOMProductWarningsTests = class
  private
    function Complete : TSBOMProductInfo;
    procedure AssertOneWarningAbout(const warnings : TArray<string>; const key : string);
  public
    [Test]
    procedure CompleteMetadata_HasNoWarnings;
    [Test]
    procedure MissingSupplier_IsWarned;
    [Test]
    procedure SupplierWithoutContact_IsWarned;
    [Test]
    procedure SupplierThatIsAnEmail_IsItsOwnContact;
    [Test]
    procedure MissingCreator_IsWarned;
    [Test]
    procedure MissingVersion_IsWarned;
    [Test]
    procedure DelphiDefaultVersionFromVersionInfo_IsWarned;
    [Test]
    procedure DelphiDefaultVersionGivenExplicitly_IsNotWarned;
    [Test]
    procedure MissingLicense_IsWarned;
  end;

  //An SBOM creator / supplier contact must be one of these, or it is rejected or misfiled.
  [TestFixture]
  TSBOMContactTests = class
  public
    [Test]
    [TestCase('plain', 'sbom@acme.example,true')]
    [TestCase('subdomain', 'a.b@mail.acme.example,true')]
    [TestCase('company name', 'Acme Pty Ltd,false')]
    [TestCase('no at', 'acme.example,false')]
    [TestCase('nothing before at', '@acme.example,false')]
    [TestCase('two ats', 'a@b@acme.example,false')]
    [TestCase('no dot in domain', 'sbom@localhost,false')]
    [TestCase('trailing dot', 'sbom@acme.,false')]
    [TestCase('space', 'sbom team@acme.example,false')]
    [TestCase('url with userinfo', 'https://user@acme.example,false')]
    procedure IsEmailAddress(const value : string; const expected : boolean);
    [Test]
    [TestCase('https', 'https://acme.example,true')]
    [TestCase('http upper case', 'HTTP://acme.example,true')]
    [TestCase('scheme only', 'https://,false')]
    [TestCase('ftp', 'ftp://acme.example,false')]
    [TestCase('bare host', 'acme.example,false')]
    procedure IsHttpUrl(const value : string; const expected : boolean);
  end;

implementation

uses
  Winapi.ActiveX,
  System.Classes,
  System.IOUtils,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Configuration.Manager,
  DPM.Core.Cache,
  DPM.Core.Project.MapFile,
  DPM.Core.Crypto.Hashing,
  DPM.Core.SBOM.Writers,
  DPM.Core.SBOM.Writers.Reports,
  DPM.Core.SBOM.Generator,
  DPM.Core.Tests.Utils.PEImports;

const
  cKindProperty = 'dpm:component-kind';

type
  //Takes the Delphi install folder from the test rather than the registry.
  TTestableSBOMGenerator = class(TSBOMGenerator)
  private
    FDelphiRoot : string;
  protected
    function GetDelphiRootDir(const compilerVersion : TCompilerVersion) : string; override;
  public
    property DelphiRoot : string read FDelphiRoot write FDelphiRoot;
  end;

function TTestableSBOMGenerator.GetDelphiRootDir(const compilerVersion : TCompilerVersion) : string;
begin
  result := FDelphiRoot;
end;

{ TSBOMGeneratorTests }

procedure TSBOMGeneratorTests.FixtureSetup;
begin
  //the project editor and settings loader both use MSXML
  CoInitialize(nil);
end;

procedure TSBOMGeneratorTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TSBOMGeneratorTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FTempDir := TPath.Combine(TPath.GetTempPath, 'dpm-sbomgen-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(FTempDir);
  FContainer := nil;
  FAppType := 'Application';
  FDpmPackages := '';
  FDelphiRoot := '';
end;

procedure TSBOMGeneratorTests.WriteBytes(const fileName, content : string);
begin
  ForceDirectories(ExtractFilePath(fileName));
  //no BOM - these stand in for build output, and the tests check their hashes
  TFile.WriteAllBytes(fileName, TEncoding.ASCII.GetBytes(content));
end;

function TSBOMGeneratorTests.HashOf(const comp : TJsonObject; const alg : string) : string;
var
  hashes : TJsonArray;
  i : integer;
begin
  result := '';
  hashes := comp.A['hashes'];
  for i := 0 to hashes.Count - 1 do
    if hashes.O[i].S['alg'] = alg then
      exit(hashes.O[i].S['content']);
end;

procedure TSBOMGeneratorTests.TearDown;
begin
  FreeAndNil(FContainer);
  FLogger := nil;
  FLoggerRef := nil;
  if DirectoryExists(FTempDir) then
    TDirectory.Delete(FTempDir, true);
end;

function TSBOMGeneratorTests.CreateGenerator : ISbomGenerator;
var
  generator : TTestableSBOMGenerator;
begin
  FLogger := TTestLogger.Create;
  FLoggerRef := FLogger;
  FreeAndNil(FContainer);
  FContainer := TContainer.Create;
  FContainer.RegisterType<ISbomWriter, TCycloneDXWriter>(cSBOMWriterCycloneDX);
  FContainer.RegisterType<ISbomWriter, TSPDXWriter>(cSBOMWriterSPDX);
  FContainer.RegisterType<ISbomWriter, TMarkdownReportWriter>(cSBOMWriterMarkdown);
  FContainer.RegisterType<ISbomWriter, THTMLReportWriter>(cSBOMWriterHTML);
  FContainer.Build;
  generator := TTestableSBOMGenerator.Create(FContainer, FLoggerRef, TConfigurationManager.Create(FLoggerRef),
                                             TPackageCache.Create(FLoggerRef, nil, nil, nil, nil, nil, nil),
                                             TMapFileReader.Create(FLoggerRef),
                                             TBCryptHashingService.Create);
  generator.DelphiRoot := FDelphiRoot;
  result := generator;
end;

procedure TSBOMGeneratorTests.WriteText(const fileName, content : string);
begin
  ForceDirectories(ExtractFilePath(fileName));
  TFile.WriteAllText(fileName, content, TEncoding.UTF8);
end;

function TSBOMGeneratorTests.WriteConfig : string;
begin
  result := TPath.Combine(FTempDir, 'dpm.config.yaml');
  //single quoted - a YAML single quoted scalar has no escapes, so the backslashes survive
  WriteText(result, 'packageCacheLocation: ''' + TPath.Combine(FTempDir, 'cache') + '''' + sLineBreak);
end;

function TSBOMGeneratorTests.VerInfo(const keys : string) : string;
begin
  result := '        <VerInfo_IncludeVerInfo>true</VerInfo_IncludeVerInfo>'#13#10 +
            '        <VerInfo_Keys>' + keys + '</VerInfo_Keys>'#13#10;
end;

//IDE authored shape, trimmed to what the generator reads. Debug is Cfg_1, Release is Cfg_2, the
//only platform is Win32. baseWin32Props and releaseWin32Props land in the Base_Win32 and
//Cfg_2_Win32 settings groups, which is where the IDE writes version info. No output directory
//is set, so the MAP file and the binary are looked for next to the dproj.
function TSBOMGeneratorTests.MakeDproj(const projectName, baseWin32Props, releaseWin32Props : string) : string;
begin
  result :=
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <ProjectVersion>20.3</ProjectVersion>'#13#10 +
    '        <FrameworkType>VCL</FrameworkType>'#13#10 +
    '        <Base>True</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Release</Config>'#13#10 +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>'#13#10 +
    '        <TargetedPlatforms>1</TargetedPlatforms>'#13#10 +
    '        <AppType>' + FAppType + '</AppType>'#13#10 +
    '        <MainSource>' + projectName + '.dpr</MainSource>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <DPMCompiler>delphi13.0</DPMCompiler>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win32'' and ''$(Base)''==''true'') or ''$(Base_Win32)''!=''''">'#13#10 +
    '        <Base_Win32>true</Base_Win32>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Debug'' or ''$(Cfg_1)''!=''''">'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win32'' and ''$(Cfg_1)''==''true'') or ''$(Cfg_1_Win32)''!=''''">'#13#10 +
    '        <Cfg_1_Win32>true</Cfg_1_Win32>'#13#10 +
    '        <CfgParent>Cfg_1</CfgParent>'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''">'#13#10 +
    '        <Cfg_2>true</Cfg_2>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win32'' and ''$(Cfg_2)''==''true'') or ''$(Cfg_2_Win32)''!=''''">'#13#10 +
    '        <Cfg_2_Win32>true</Cfg_2_Win32>'#13#10 +
    '        <CfgParent>Cfg_2</CfgParent>'#13#10 +
    '        <Cfg_2>true</Cfg_2>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <SanitizedProjectName>' + projectName + '</SanitizedProjectName>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base_Win32)''!=''''">'#13#10 +
    baseWin32Props +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_2_Win32)''!=''''">'#13#10 +
    releaseWin32Props +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <DelphiCompile Include="$(MainSource)">'#13#10 +
    '            <MainSource>MainSource</MainSource>'#13#10 +
    '        </DelphiCompile>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Debug">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_2</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '    <ProjectExtensions>'#13#10 +
    '        <Borland.Personality>Delphi.Personality.12</Borland.Personality>'#13#10 +
    '        <Borland.ProjectType>Application</Borland.ProjectType>'#13#10 +
    '        <BorlandProject>'#13#10 +
    '            <Delphi.Personality>'#13#10 +
    '                <Source>'#13#10 +
    '                    <Source Name="MainSource">' + projectName + '.dpr</Source>'#13#10 +
    '                </Source>'#13#10 +
    '            </Delphi.Personality>'#13#10 +
    '            <Platforms>'#13#10 +
    '                <Platform value="Win32">True</Platform>'#13#10 +
    '            </Platforms>'#13#10 +
    '        </BorlandProject>'#13#10 +
    '        <ProjectFileVersion>12</ProjectFileVersion>'#13#10 +
    '        <DPM>'#13#10 +
    '            <Packages>'#13#10 +
    FDpmPackages +
    '            </Packages>'#13#10 +
    '        </DPM>'#13#10 +
    '    </ProjectExtensions>'#13#10 +
    '</Project>'#13#10;
end;

procedure TSBOMGeneratorTests.WriteDproj(const fileName, projectName, baseWin32Props, releaseWin32Props : string);
begin
  WriteText(fileName, MakeDproj(projectName, baseWin32Props, releaseWin32Props));
end;

//units are 'UnitName=SourcePath' pairs. An empty source path writes a segment entry only, which
//is what a unit linked from a .dcu without debug info looks like.
procedure TSBOMGeneratorTests.WriteMap(const fileName : string; const units : array of string);
var
  sb : TStringBuilder;
  i : integer;
  unitName : string;
  sourcePath : string;
  sepPos : integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append('Start         Length     Name                   Class').Append(sLineBreak);
    sb.Append(' 0001:00401000 0000A000H .text                  CODE').Append(sLineBreak);
    sb.Append(sLineBreak);
    sb.Append('Detailed map of segments').Append(sLineBreak);
    for i := 0 to Length(units) - 1 do
    begin
      sepPos := Pos('=', units[i]);
      unitName := Copy(units[i], 1, sepPos - 1);
      sb.Append(Format(' 0001:%.8x 00000100 C=CODE     S=.text    G=(none)   M=%s ACBP=A9', [$401000 + i * $100, unitName])).Append(sLineBreak);
    end;
    sb.Append(sLineBreak);
    for i := 0 to Length(units) - 1 do
    begin
      sepPos := Pos('=', units[i]);
      unitName := Copy(units[i], 1, sepPos - 1);
      sourcePath := Copy(units[i], sepPos + 1, MaxInt);
      if sourcePath = '' then
        continue;
      sb.Append('Line numbers for ' + unitName + '(' + sourcePath + ') segment .text').Append(sLineBreak);
      sb.Append(Format('   1 0001:%.8x', [$401000 + i * $100])).Append(sLineBreak);
    end;
    WriteText(fileName, sb.ToString);
  finally
    sb.Free;
  end;
end;

procedure TSBOMGeneratorTests.WriteGroupProj(const fileName : string; const projects : array of string);
var
  content : string;
  i : integer;
begin
  content := '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
             '    <ItemGroup>'#13#10;
  for i := 0 to Length(projects) - 1 do
    content := content + '        <Projects Include="' + projects[i] + '">'#13#10 +
                         '            <Dependencies/>'#13#10 +
                         '        </Projects>'#13#10;
  content := content + '    </ItemGroup>'#13#10 +
                       '</Project>'#13#10;
  WriteText(fileName, content);
end;

function TSBOMGeneratorTests.Generate(const projectPath, configName, outDir : string; const spdx : boolean) : boolean;
begin
  result := GenerateWith(projectPath, outDir,
    procedure(options : TSBOMOptions)
    begin
      options.Config := configName;
      if spdx then
        options.Formats := [TSBOMFormat.CycloneDX, TSBOMFormat.SPDX];
    end);
end;

function TSBOMGeneratorTests.GenerateWith(const projectPath, outDir : string; const configure : TProc<TSBOMOptions>) : boolean;
var
  options : TSBOMOptions;
begin
  options := TSBOMOptions.Create;
  try
    options.ConfigFile := WriteConfig;
    options.ProjectPath := projectPath;
    options.OutputDir := outDir;
    options.Config := 'Release';
    options.Formats := [TSBOMFormat.CycloneDX];
    //the runtime component depends on which Delphi is installed on this machine
    options.IncludeRuntime := false;
    if Assigned(configure) then
      configure(options);
    result := CreateGenerator.Generate(nil, options);
  finally
    options.Free;
  end;
end;

function TSBOMGeneratorTests.RootOf(const doc : TJsonObject) : TJsonObject;
begin
  result := doc.O['metadata'].O['component'];
end;

function TSBOMGeneratorTests.LoadJson(const fileName : string) : TJsonObject;
begin
  Assert.IsTrue(FileExists(fileName), 'expected the generator to write ' + fileName);
  result := TJsonObject.ParseFromFile(fileName) as TJsonObject;
end;

function TSBOMGeneratorTests.FindComponent(const doc : TJsonObject; const name : string) : TJsonObject;
var
  components : TJsonArray;
  i : integer;
begin
  result := nil;
  components := doc.A['components'];
  for i := 0 to components.Count - 1 do
    if SameText(components.O[i].S['name'], name) then
      exit(components.O[i]);
end;

function TSBOMGeneratorTests.ComponentsOfKind(const doc : TJsonObject; const kind : string) : TArray<TJsonObject>;
var
  components : TJsonArray;
  props : TJsonArray;
  i : integer;
  j : integer;
begin
  result := nil;
  components := doc.A['components'];
  for i := 0 to components.Count - 1 do
  begin
    props := components.O[i].A['properties'];
    for j := 0 to props.Count - 1 do
    begin
      if SameText(props.O[j].S['name'], cKindProperty) and SameText(props.O[j].S['value'], kind) then
      begin
        SetLength(result, Length(result) + 1);
        result[Length(result) - 1] := components.O[i];
        break;
      end;
    end;
  end;
end;

function TSBOMGeneratorTests.DependsOn(const doc : TJsonObject; const parentRef, childRef : string) : boolean;
var
  deps : TJsonArray;
  children : TJsonArray;
  i : integer;
  j : integer;
begin
  result := false;
  deps := doc.A['dependencies'];
  for i := 0 to deps.Count - 1 do
  begin
    if deps.O[i].S['ref'] <> parentRef then
      continue;
    children := deps.O[i].A['dependsOn'];
    for j := 0 to children.Count - 1 do
      if children.S[j] = childRef then
        exit(true);
  end;
end;

//CycloneDX requires every bom-ref in a document to be unique, and dependencies[].ref and each
//dependsOn list to have unique items.
procedure TSBOMGeneratorTests.AssertBomRefsAreUnique(const doc : TJsonObject);
var
  seen : ISet<string>;
  components : TJsonArray;
  deps : TJsonArray;
  children : TJsonArray;
  childSeen : ISet<string>;
  bomRef : string;
  i : integer;
  j : integer;
begin
  seen := TCollections.CreateSet<string>;
  seen.Add(doc.O['metadata'].O['component'].S['bom-ref']);
  components := doc.A['components'];
  for i := 0 to components.Count - 1 do
  begin
    bomRef := components.O[i].S['bom-ref'];
    Assert.IsTrue(seen.Add(bomRef), 'duplicate component bom-ref [' + bomRef + ']');
  end;

  seen := TCollections.CreateSet<string>;
  deps := doc.A['dependencies'];
  for i := 0 to deps.Count - 1 do
  begin
    bomRef := deps.O[i].S['ref'];
    Assert.IsTrue(seen.Add(bomRef), 'duplicate dependencies entry for [' + bomRef + ']');
    childSeen := TCollections.CreateSet<string>;
    children := deps.O[i].A['dependsOn'];
    for j := 0 to children.Count - 1 do
      Assert.IsTrue(childSeen.Add(children.S[j]), '[' + bomRef + '] lists [' + children.S[j] + '] more than once');
  end;
end;

procedure TSBOMGeneratorTests.RootVersion_IsTheProductVersionFromVersionInfo;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('CompanyName=Acme;FileVersion=3.1.4.0;ProductVersion=3.1.4'), '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //not 20.3 - that is the dproj's ProjectVersion, the MSBuild project format version
    Assert.AreEqual('3.1.4', doc.O['metadata'].O['component'].S['version']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RootVersion_UsesTheSelectedConfiguration;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('FileVersion=3.1.4.0;ProductVersion=3.1.4'),
                           VerInfo('FileVersion=3.1.5.0;ProductVersion=3.1.5'));

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'release'), false));
  Assert.IsTrue(Generate(dproj, 'Debug', TPath.Combine(FTempDir, 'debug'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'release\App-Win32.cdx.json'));
  try
    Assert.AreEqual('3.1.5', doc.O['metadata'].O['component'].S['version'], 'Release overrides the version');
  finally
    doc.Free;
  end;
  doc := LoadJson(TPath.Combine(FTempDir, 'debug\App-Win32.cdx.json'));
  try
    Assert.AreEqual('3.1.4', doc.O['metadata'].O['component'].S['version'], 'Debug inherits the Base_Win32 version');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RootVersion_FallsBackToFileVersion;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('FileVersion=2.0.0.7;ProductVersion='), '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('2.0.0.7', doc.O['metadata'].O['component'].S['version']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RootVersion_IsOmittedWithoutVersionInfo;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  //the keys are there but the binary does not get a version resource
  WriteDproj(dproj, 'App', '        <VerInfo_Keys>FileVersion=1.0.0.0;ProductVersion=1.0.0.0</VerInfo_Keys>'#13#10, '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsFalse(doc.O['metadata'].O['component'].Contains('version'),
      'no version info means no known version, got [' + doc.O['metadata'].O['component'].S['version'] + ']');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ThirdPartyFoldersWithTheSameName_GetDistinctBomRefs;
var
  dproj : string;
  doc : TJsonObject;
  thirdParty : TArray<TJsonObject>;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //two unrelated libraries that both keep their units in a folder called Source
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['LibA.Core=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Core.pas'),
     'LibB.Core=' + TPath.Combine(FTempDir, 'libs\LibB\Source\LibB.Core.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    thirdParty := ComponentsOfKind(doc, 'third-party');
    Assert.AreEqual(2, Length(thirdParty), 'one component per library folder');
    AssertBomRefsAreUnique(doc);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ThirdPartyLibraryWithSeveralUnits_IsLinkedOnce;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['LibA.Core=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Core.pas'),
     'LibA.Utils=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Utils.pas'),
     'ZzNoSuchUnitAnywhere=',
     'ZzNorThisOne=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(1, Length(ComponentsOfKind(doc, 'third-party')));
    AssertBomRefsAreUnique(doc);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ThirdPartyComponent_HasNoInventedVersion;
var
  dproj : string;
  doc : TJsonObject;
  thirdParty : TArray<TJsonObject>;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['LibA.Core=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Core.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    thirdParty := ComponentsOfKind(doc, 'third-party');
    Assert.AreEqual(1, Length(thirdParty));
    Assert.IsFalse(thirdParty[0].Contains('version'), 'we do not know the version, got [' + thirdParty[0].S['version'] + ']');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.UnidentifiedComponent_HasNoInventedVersion;
var
  dproj : string;
  doc : TJsonObject;
  unidentified : TArray<TJsonObject>;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //no source path, and nothing on any search path provides it
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'), ['ZzNoSuchUnitAnywhere=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    unidentified := ComponentsOfKind(doc, 'unidentified');
    Assert.AreEqual(1, Length(unidentified));
    Assert.IsFalse(unidentified[0].Contains('version'), 'we do not know the version, got [' + unidentified[0].S['version'] + ']');
  finally
    doc.Free;
  end;
end;

//Group with two projects that link the same third-party unit and the same unresolvable unit.
procedure TSBOMGeneratorTests.WriteSharedComponentsGroup;
var
  units : array of string;
begin
  SetLength(units, 2);
  units[0] := 'LibA.Core=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Core.pas');
  units[1] := 'ZzNoSuchUnitAnywhere=';
  WriteDproj(TPath.Combine(FTempDir, 'group\app1\App1.dproj'), 'App1', VerInfo('FileVersion=1.2.0.0;ProductVersion=1.2'), '');
  WriteMap(TPath.Combine(FTempDir, 'group\app1\App1.map'), units);
  WriteDproj(TPath.Combine(FTempDir, 'group\app2\App2.dproj'), 'App2', VerInfo('FileVersion=4.5.0.0;ProductVersion=4.5'), '');
  WriteMap(TPath.Combine(FTempDir, 'group\app2\App2.map'), units);
  WriteGroupProj(TPath.Combine(FTempDir, 'group\Suite.groupproj'), ['app1\App1.dproj', 'app2\App2.dproj']);
end;

procedure TSBOMGeneratorTests.GroupProject_ComponentsSharedByProjects_AreEmittedOnce;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    Assert.AreEqual(1, Length(ComponentsOfKind(doc, 'third-party')), 'LibA is one component, used by both projects');
    Assert.AreEqual(1, Length(ComponentsOfKind(doc, 'unidentified')));
    AssertBomRefsAreUnique(doc);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_SharedThirdParty_IsLinkedFromEachProject;
var
  doc : TJsonObject;
  libRef : string;
  app1Ref : string;
  app2Ref : string;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    libRef := ComponentsOfKind(doc, 'third-party')[0].S['bom-ref'];
    app1Ref := FindComponent(doc, 'App1').S['bom-ref'];
    app2Ref := FindComponent(doc, 'App2').S['bom-ref'];
    Assert.IsTrue(DependsOn(doc, app1Ref, libRef), 'App1 links LibA');
    Assert.IsTrue(DependsOn(doc, app2Ref, libRef), 'App2 links LibA');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_SharedComponents_DoNotBreakSPDXOutput;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), true);

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.spdx.json'));
  try
    Assert.AreEqual('SPDX-2.3', doc.S['spdxVersion']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_ProjectVersionsComeFromVersionInfo;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    Assert.AreEqual('1.2', FindComponent(doc, 'App1').S['version']);
    Assert.AreEqual('4.5', FindComponent(doc, 'App2').S['version']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_RootHasNoInventedVersion;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    //a group project has no version info of its own
    Assert.IsFalse(doc.O['metadata'].O['component'].Contains('version'),
      'got [' + doc.O['metadata'].O['component'].S['version'] + ']');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Supplier_FromOptions_IsWrittenOnTheRoot;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Supplier := 'Acme Pty Ltd';
      options.SupplierUrl := 'https://acme.example';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('Acme Pty Ltd', RootOf(doc).O['supplier'].S['name']);
    Assert.AreEqual(1, RootOf(doc).O['supplier'].A['url'].Count);
    Assert.AreEqual('https://acme.example', RootOf(doc).O['supplier'].A['url'].S[0]);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.SupplierUrl_ThatIsAnEmail_IsTheSupplierContact;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Supplier := 'Acme Pty Ltd';
      options.SupplierUrl := 'security@acme.example';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsFalse(RootOf(doc).O['supplier'].Contains('url'), 'an email address is not a url');
    Assert.AreEqual('security@acme.example', RootOf(doc).O['supplier'].A['contact'].O[0].S['email']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.License_FromOptions_IsWrittenOnTheRoot;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.License := 'mit';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //case sensitive - license.id is the SPDX enum, so the canonical casing is the point
    Assert.AreEqual('MIT', RootOf(doc).A['licenses'].O[0].O['license'].S['id'], false);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Version_FromOptions_OverridesVersionInfo;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('FileVersion=1.0.0.0;ProductVersion=1.0.0.0'), '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.ProductVersion := '2.1.0';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('2.1.0', RootOf(doc).S['version']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Supplier_FallsBackToVersionInfoCompanyName;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('CompanyName=Acme Pty Ltd;FileVersion=1.2.0.0;ProductVersion=1.2'), '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('Acme Pty Ltd', RootOf(doc).O['supplier'].S['name']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.SbomCreator_Email_IsTheManufacturerContact;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.SbomCreator := 'sbom@acme.example';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //CycloneDX 1.6 metadata.manufacturer is the organisation that created the BOM
    Assert.AreEqual('sbom@acme.example', doc.O['metadata'].O['manufacturer'].A['contact'].O[0].S['email']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.SbomCreator_Url_IsTheManufacturerUrl;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.SbomCreator := 'https://acme.example/security';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('https://acme.example/security', doc.O['metadata'].O['manufacturer'].A['url'].S[0]);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.SbomCreator_ThatIsNeitherEmailNorUrl_FailsGeneration;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  //BSI TR-03183-2 wants a contact - an email address or a URL - not a name
  Assert.IsFalse(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.SbomCreator := 'Acme Pty Ltd';
    end));
  Assert.IsFalse(FileExists(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json')));
end;

procedure TSBOMGeneratorTests.Sidecar_SuppliesProductMetadata;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', VerInfo('FileVersion=1.0.0.0;ProductVersion=1.0.0.0'), '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'supplier: Acme Pty Ltd' + sLineBreak +
    'supplierUrl: https://acme.example' + sLineBreak +
    'license: Apache-2.0' + sLineBreak +
    'sbomCreator: sbom@acme.example' + sLineBreak +
    //unquoted, so YAML reads it as a number - the text is what matters
    'version: 2.1' + sLineBreak);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('Acme Pty Ltd', RootOf(doc).O['supplier'].S['name']);
    Assert.AreEqual('https://acme.example', RootOf(doc).O['supplier'].A['url'].S[0]);
    Assert.AreEqual('Apache-2.0', RootOf(doc).A['licenses'].O[0].O['license'].S['id']);
    Assert.AreEqual('sbom@acme.example', doc.O['metadata'].O['manufacturer'].A['contact'].O[0].S['email']);
    Assert.AreEqual('2.1', RootOf(doc).S['version']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Options_OverrideTheSidecar;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), 'supplier: Sidecar Co' + sLineBreak + 'license: MIT' + sLineBreak);

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Supplier := 'Command Line Co';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('Command Line Co', RootOf(doc).O['supplier'].S['name']);
    Assert.AreEqual('MIT', RootOf(doc).A['licenses'].O[0].O['license'].S['id'], 'fields the option does not set still come from the sidecar');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.UnreadableSidecar_FailsGeneration;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), 'supplier: [unclosed' + sLineBreak);

  //silently ignoring it would emit an SBOM without the metadata the user asked for
  Assert.IsFalse(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));
end;

procedure TSBOMGeneratorTests.SPDX_RootPackage_CarriesSupplierLicenseAndCreator;
var
  dproj : string;
  doc : TJsonObject;
  rootPackage : TJsonObject;
  creators : TJsonArray;
  i : integer;
  found : boolean;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Formats := [TSBOMFormat.SPDX];
      options.Supplier := 'Acme Pty Ltd';
      options.SupplierUrl := 'security@acme.example';
      options.License := 'MIT';
      options.SbomCreator := 'sbom@acme.example';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.spdx.json'));
  try
    //the first package is the root - the one DOCUMENT DESCRIBES
    rootPackage := doc.A['packages'].O[0];
    Assert.AreEqual('App', rootPackage.S['name']);
    Assert.AreEqual('Organization: Acme Pty Ltd (security@acme.example)', rootPackage.S['supplier']);
    Assert.AreEqual('MIT', rootPackage.S['licenseDeclared']);
    //the product licence is asserted by its supplier, not just declared by a third party
    Assert.AreEqual('MIT', rootPackage.S['licenseConcluded']);

    creators := doc.O['creationInfo'].A['creators'];
    found := false;
    for i := 0 to creators.Count - 1 do
      if creators.S[i] = 'Organization: sbom@acme.example (sbom@acme.example)' then
        found := true;
    Assert.IsTrue(found, 'the SBOM creator is listed in creationInfo.creators');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_ProjectSidecarOverridesGroupSupplier;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;
  WriteText(TPath.Combine(FTempDir, 'group\Suite.sbom.yaml'), 'supplier: Suite Co' + sLineBreak);
  WriteText(TPath.Combine(FTempDir, 'group\app1\App1.sbom.yaml'), 'supplier: App One Co' + sLineBreak);

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    Assert.AreEqual('Suite Co', RootOf(doc).O['supplier'].S['name']);
    Assert.AreEqual('App One Co', FindComponent(doc, 'App1').O['supplier'].S['name'], 'App1 has its own sidecar');
    Assert.AreEqual('Suite Co', FindComponent(doc, 'App2').O['supplier'].S['name'], 'App2 has none, so the group supplier applies');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_GroupVersion_DoesNotReplaceProjectVersions;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(GenerateWith(TPath.Combine(FTempDir, 'group\Suite.groupproj'), TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.ProductVersion := '9.0';
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    Assert.AreEqual('9.0', RootOf(doc).S['version'], 'the version given is the group''s');
    Assert.AreEqual('1.2', FindComponent(doc, 'App1').S['version'], 'App1 keeps its own version info');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.MissingMetadata_IsLoggedAsWarnings;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  //warnings, not errors - the SBOM is still written
  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  Assert.IsTrue(FLogger.Logged('warning', 'App.sbom.yaml'), 'the warnings point at the sidecar that fixes them');
  Assert.IsTrue(FLogger.Logged('warning', 'supplier'));
  Assert.IsTrue(FLogger.Logged('warning', 'license'));
end;

procedure TSBOMGeneratorTests.CompleteMetadata_IsNotWarned;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'supplier: Acme Pty Ltd' + sLineBreak +
    'supplierUrl: https://acme.example' + sLineBreak +
    'license: MIT' + sLineBreak +
    'sbomCreator: sbom@acme.example' + sLineBreak +
    'version: 2.1.0' + sLineBreak);
  WriteBytes(TPath.Combine(FTempDir, 'app\App.exe'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  Assert.IsFalse(FLogger.Logged('warning', 'CRA'), FLogger.Messages.Text);
end;

procedure TSBOMGeneratorTests.Warnings_AreListedInTheMarkdownReport;
var
  dproj : string;
  markdown : string;
  html : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //everything but the licence
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'supplier: Acme Pty Ltd' + sLineBreak +
    'supplierUrl: https://acme.example' + sLineBreak +
    'sbomCreator: sbom@acme.example' + sLineBreak +
    'version: 2.1.0' + sLineBreak);

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Formats := [TSBOMFormat.Markdown, TSBOMFormat.HTML];
    end));

  markdown := TFile.ReadAllText(TPath.Combine(FTempDir, 'out\App-Win32.md'), TEncoding.UTF8);
  Assert.Contains(markdown, '## Warnings');
  Assert.Contains(markdown, 'license:');
  html := TFile.ReadAllText(TPath.Combine(FTempDir, 'out\App-Win32.html'), TEncoding.UTF8);
  Assert.Contains(html, '<h2>Warnings');
  Assert.Contains(html, 'license:');
end;

const
  //'hello', hashed independently (python hashlib)
  cHelloSha256 = '2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824';
  cHelloSha512 = '9b71d224bd62f3785d96d46ad3ea3d73319bfbc2890caadae2dff72519673ca7' +
                 '2323c3d99ba5c11d7c7acc6e14b8c5da0c4663475c2e5c3adef46f73bcdec043';

procedure TSBOMGeneratorTests.BuiltExecutable_IsHashedOnTheRoot;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteBytes(TPath.Combine(FTempDir, 'app\App.exe'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(cHelloSha256, HashOf(RootOf(doc), 'SHA-256'));
    //BSI TR-03183-2 5.2.2 asks for the SHA-512 of the deployable component
    Assert.AreEqual(cHelloSha512, HashOf(RootOf(doc), 'SHA-512'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.BuiltExecutable_InTheOutputDirectory_IsHashed;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '        <DCC_ExeOutput>.\$(Platform)\$(Config)</DCC_ExeOutput>'#13#10, '');
  WriteBytes(TPath.Combine(FTempDir, 'app\Win32\Release\App.exe'), 'hello');
  //a stale copy next to the dproj must not be the one hashed
  WriteBytes(TPath.Combine(FTempDir, 'app\App.exe'), 'stale');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(cHelloSha256, HashOf(RootOf(doc), 'SHA-256'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.BuiltLibrary_IsHashed;
var
  dproj : string;
  doc : TJsonObject;
begin
  FAppType := 'Library';
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteBytes(TPath.Combine(FTempDir, 'app\App.dll'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(cHelloSha256, HashOf(RootOf(doc), 'SHA-256'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.BuiltPackage_WithAutoSuffix_IsHashed;
var
  dproj : string;
  doc : TJsonObject;
begin
  FAppType := 'Package';
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  //$(Auto) is the compiler's package version - 370 for the fixture's delphi13.0
  WriteDproj(dproj, 'App', '        <DllSuffix>$(Auto)</DllSuffix>'#13#10, '');
  WriteBytes(TPath.Combine(FTempDir, 'app\App370.bpl'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(cHelloSha256, HashOf(RootOf(doc), 'SHA-256'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.MissingBinary_IsWarned;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false), 'still written, just without the hash');

  Assert.IsTrue(FLogger.Logged('warning', 'App.exe'), FLogger.Messages.Text);
end;

procedure TSBOMGeneratorTests.SPDX_RootPackage_HasBinaryChecksums;
var
  dproj : string;
  doc : TJsonObject;
  checksums : TJsonArray;
  i : integer;
  sha512 : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteBytes(TPath.Combine(FTempDir, 'app\App.exe'), 'hello');

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.Formats := [TSBOMFormat.SPDX];
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.spdx.json'));
  try
    checksums := doc.A['packages'].O[0].A['checksums'];
    sha512 := '';
    for i := 0 to checksums.Count - 1 do
      if checksums.O[i].S['algorithm'] = 'SHA512' then
        sha512 := checksums.O[i].S['checksumValue'];
    Assert.AreEqual(cHelloSha512, sha512);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_EachProjectBinaryIsHashed;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;
  WriteBytes(TPath.Combine(FTempDir, 'group\app1\App1.exe'), 'app one');
  WriteBytes(TPath.Combine(FTempDir, 'group\app2\App2.exe'), 'app two');

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    //hashed independently (python hashlib)
    Assert.AreEqual('960a1d4da8e891de81d13cca7b4b3cdccd5de68ac15322279590e690e5760c4b', HashOf(FindComponent(doc, 'App1'), 'SHA-256'));
    Assert.AreEqual('9a8383ff71d32f9f471d5d21ddf7b34fbf924704b8a52e4481edd175eb9d2eef', HashOf(FindComponent(doc, 'App2'), 'SHA-256'));
  finally
    doc.Free;
  end;
end;

function EvidenceLocations(const comp : TJsonObject) : string;
var
  occurrences : TJsonArray;
  i : integer;
begin
  result := '';
  occurrences := comp.O['evidence'].A['occurrences'];
  for i := 0 to occurrences.Count - 1 do
    result := result + occurrences.O[i].S['location'] + ';';
end;

procedure TSBOMGeneratorTests.RuntimePackage_FromADpmPackage_IsAttributedToIt;
var
  dproj : string;
  doc : TJsonObject;
begin
  FDpmPackages := '                <PackageReference id="Acme.Lib" version="1.0.0"/>'#13#10;
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //where the installer puts a package's compiled runtime packages
  WriteBytes(TPath.Combine(FTempDir, 'cache\delphi13.0\Acme.Lib\1.0.0\bpl\Win32\AcmeLibR370.bpl'), 'bpl');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['AcmeLibR370.bpl', 'kernel32.dll'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'Acme.Lib'));
    Assert.Contains(EvidenceLocations(FindComponent(doc, 'Acme.Lib')), 'AcmeLibR370.bpl');
    Assert.IsNull(FindComponent(doc, 'AcmeLibR370.bpl'), 'not a component of its own - it is part of Acme.Lib');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RuntimePackage_FromDelphi_IsEvidenceOnTheRuntimeComponent;
var
  dproj : string;
  doc : TJsonObject;
  runtime : TArray<TJsonObject>;
begin
  FDelphiRoot := TPath.Combine(FTempDir, 'delphi');
  WriteBytes(TPath.Combine(FDelphiRoot, 'bin\rtl370.bpl'), 'bpl');
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['rtl370.bpl'], []);

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.IncludeRuntime := true;
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    runtime := ComponentsOfKind(doc, 'delphi-runtime');
    Assert.AreEqual(1, Length(runtime));
    Assert.Contains(EvidenceLocations(runtime[0]), 'rtl370.bpl');
    Assert.IsNull(FindComponent(doc, 'rtl370.bpl'), 'Delphi''s own packages are part of the runtime component');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RuntimePackage_FromElsewhere_IsAComponentOfItsOwn;
var
  dproj : string;
  doc : TJsonObject;
  bpl : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['Acme.Widgets370.bpl'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    bpl := FindComponent(doc, 'Acme.Widgets370.bpl');
    Assert.IsNotNull(bpl, 'the package the exe loads is a component');
    Assert.IsTrue(DependsOn(doc, RootOf(doc).S['bom-ref'], bpl.S['bom-ref']), 'linked from the project');
    AssertBomRefsAreUnique(doc);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DllImports_AreNotRuntimePackages;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['kernel32.dll', 'user32.dll'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNull(FindComponent(doc, 'kernel32.dll'));
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'third-party')));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_SharedRuntimePackage_IsOneComponentLinkedFromEach;
var
  doc : TJsonObject;
  bplRef : string;
  matches : integer;
  components : TJsonArray;
  i : integer;
begin
  WriteSharedComponentsGroup;
  WriteTestPE(TPath.Combine(FTempDir, 'group\app1\App1.exe'), false, ['Acme.Widgets370.bpl'], []);
  WriteTestPE(TPath.Combine(FTempDir, 'group\app2\App2.exe'), false, ['Acme.Widgets370.bpl'], []);

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    matches := 0;
    components := doc.A['components'];
    for i := 0 to components.Count - 1 do
      if components.O[i].S['name'] = 'Acme.Widgets370.bpl' then
        Inc(matches);
    Assert.AreEqual(1, matches);
    bplRef := FindComponent(doc, 'Acme.Widgets370.bpl').S['bom-ref'];
    Assert.IsTrue(DependsOn(doc, FindComponent(doc, 'App1').S['bom-ref'], bplRef));
    Assert.IsTrue(DependsOn(doc, FindComponent(doc, 'App2').S['bom-ref'], bplRef));
  finally
    doc.Free;
  end;
end;

//The aggregate of the composition whose dependencies list bomRef, '' when none does.
function AggregateOf(const doc : TJsonObject; const bomRef : string) : string;
var
  compositions : TJsonArray;
  refs : TJsonArray;
  i : integer;
  j : integer;
begin
  result := '';
  compositions := doc.A['compositions'];
  for i := 0 to compositions.Count - 1 do
  begin
    refs := compositions.O[i].A['dependencies'];
    for j := 0 to refs.Count - 1 do
      if refs.S[j] = bomRef then
        exit(compositions.O[i].S['aggregate']);
  end;
end;

procedure TSBOMGeneratorTests.Compositions_DpmPackageDependencies_AreComplete;
var
  dproj : string;
  doc : TJsonObject;
begin
  FDpmPackages := '                <PackageReference id="Acme.Lib" version="1.0.0">'#13#10 +
                  '                    <PackageReference id="Acme.Base" version="2.0.0" range="[2.0.0,]"/>'#13#10 +
                  '                </PackageReference>'#13#10;
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //what a DPM package depends on is declared in its dspec and resolved into the dproj
    Assert.AreEqual('complete', AggregateOf(doc, FindComponent(doc, 'Acme.Lib').S['bom-ref']));
    Assert.AreEqual('complete', AggregateOf(doc, FindComponent(doc, 'Acme.Base').S['bom-ref']));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Compositions_LeafDpmPackage_StatesItHasNoDependencies;
var
  dproj : string;
  doc : TJsonObject;
  baseRef : string;
  deps : TJsonArray;
  i : integer;
  found : boolean;
begin
  FDpmPackages := '                <PackageReference id="Acme.Base" version="2.0.0"/>'#13#10;
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //"complete" with no dependencies entry at all would say nothing - an empty dependsOn says none
    baseRef := FindComponent(doc, 'Acme.Base').S['bom-ref'];
    found := false;
    deps := doc.A['dependencies'];
    for i := 0 to deps.Count - 1 do
      if deps.O[i].S['ref'] = baseRef then
      begin
        found := true;
        Assert.AreEqual(0, deps.O[i].A['dependsOn'].Count);
      end;
    Assert.IsTrue(found, 'expected a dependencies entry for ' + baseRef);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Compositions_TheProjectsDependencies_AreIncomplete;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //anything the program loads by name at runtime is in neither the MAP file nor its imports
    Assert.AreEqual('incomplete', AggregateOf(doc, RootOf(doc).S['bom-ref']));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Compositions_ComponentsFoundInTheBuild_AreUnknown;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['LibA.Core=' + TPath.Combine(FTempDir, 'libs\LibA\Source\LibA.Core.pas'),
     'ZzNoSuchUnitAnywhere=']);
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['Acme.Widgets370.bpl'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual('unknown', AggregateOf(doc, ComponentsOfKind(doc, 'third-party')[0].S['bom-ref']));
    Assert.AreEqual('unknown', AggregateOf(doc, ComponentsOfKind(doc, 'unidentified')[0].S['bom-ref']));
    Assert.AreEqual('unknown', AggregateOf(doc, FindComponent(doc, 'Acme.Widgets370.bpl').S['bom-ref']));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.Compositions_GroupRoot_IsCompleteAndItsProjectsAreNot;
var
  doc : TJsonObject;
begin
  WriteSharedComponentsGroup;

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    //the group is exactly the projects the .groupproj lists
    Assert.AreEqual('complete', AggregateOf(doc, RootOf(doc).S['bom-ref']));
    Assert.AreEqual('incomplete', AggregateOf(doc, FindComponent(doc, 'App1').S['bom-ref']));
  finally
    doc.Free;
  end;
end;

function PropertyOf(const comp : TJsonObject; const name : string) : string;
var
  props : TJsonArray;
  i : integer;
begin
  result := '';
  props := comp.A['properties'];
  for i := 0 to props.Count - 1 do
    if props.O[i].S['name'] = name then
      exit(props.O[i].S['value']);
end;

procedure TSBOMGeneratorTests.DllImport_IsAComponentOfItsOwn;
var
  dproj : string;
  doc : TJsonObject;
  dll : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['kernel32.dll', 'acmecrypto.dll'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    dll := FindComponent(doc, 'acmecrypto.dll');
    Assert.IsNotNull(dll);
    Assert.IsTrue(DependsOn(doc, RootOf(doc).S['bom-ref'], dll.S['bom-ref']), 'linked from the project');
    Assert.IsNull(FindComponent(doc, 'kernel32.dll'), 'part of Windows, not of the product');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ApiSetDll_IsNotAComponent;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //an API set is a name Windows resolves to one of its own DLLs - there is no such file
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), true, ['api-ms-win-crt-runtime-l1-1-0.dll', 'ext-ms-win-ntuser-uicontext-ext-l1-1-0.dll'], []);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'third-party')));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DllFromDelphi_IsEvidenceOnTheRuntimeComponent;
var
  dproj : string;
  doc : TJsonObject;
begin
  FDelphiRoot := TPath.Combine(FTempDir, 'delphi');
  WriteBytes(TPath.Combine(FDelphiRoot, 'bin\midas.dll'), 'dll');
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['midas.dll'], []);

  Assert.IsTrue(GenerateWith(dproj, TPath.Combine(FTempDir, 'out'),
    procedure(options : TSBOMOptions)
    begin
      options.IncludeRuntime := true;
    end));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.Contains(EvidenceLocations(ComponentsOfKind(doc, 'delphi-runtime')[0]), 'midas.dll');
    Assert.IsNull(FindComponent(doc, 'midas.dll'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DelayLoadedDll_IsMarkedAsSuch;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['kernel32.dll'], ['acmecrypto.dll']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'acmecrypto.dll'));
    //loaded on first call rather than at startup - it may never be loaded at all
    Assert.AreEqual('true', PropertyOf(FindComponent(doc, 'acmecrypto.dll'), 'dpm:delayLoad'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DllNextToTheBinary_IsHashed;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['acmecrypto.dll'], []);
  //the copy that ships with the program
  WriteBytes(TPath.Combine(FTempDir, 'app\acmecrypto.dll'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'acmecrypto.dll'));
    Assert.AreEqual(cHelloSha256, HashOf(FindComponent(doc, 'acmecrypto.dll'), 'SHA-256'));
    Assert.AreEqual(cHelloSha512, HashOf(FindComponent(doc, 'acmecrypto.dll'), 'SHA-512'));
    Assert.IsFalse(FindComponent(doc, 'acmecrypto.dll').Contains('version'), 'no version resource, no version');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DllNextToTheBinary_TakesVersionAndSupplierFromItsVersionResource;
var
  dproj : string;
  doc : TJsonObject;
  dll : TJsonObject;
  systemDll : string;
  fileVersion : Cardinal;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['acmecrypto.dll'], []);
  //any file with a version resource will do - kernel32's is always there, and Microsoft's
  systemDll := TPath.Combine(GetEnvironmentVariable('SystemRoot'), 'System32\kernel32.dll');
  TFile.Copy(systemDll, TPath.Combine(FTempDir, 'app\acmecrypto.dll'));

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    dll := FindComponent(doc, 'acmecrypto.dll');
    Assert.IsNotNull(dll);
    //major.minor read independently, through the RTL. From the copy, not systemDll - Windows answers
    //version queries on its own files with a compatibility shim (6.2) for unmanifested programs.
    fileVersion := GetFileVersion(TPath.Combine(FTempDir, 'app\acmecrypto.dll'));
    Assert.IsTrue(Pos(Format('%d.%d.', [fileVersion shr 16, fileVersion and $FFFF]), dll.S['version']) = 1,
      'expected it to start [' + Format('%d.%d.', [fileVersion shr 16, fileVersion and $FFFF]) + '], got [' + dll.S['version'] + ']');
    Assert.Contains(dll.O['supplier'].S['name'], 'Microsoft');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.RuntimePackageNextToTheBinary_IsHashed;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteTestPE(TPath.Combine(FTempDir, 'app\App.exe'), false, ['Acme.Widgets370.bpl'], []);
  WriteBytes(TPath.Combine(FTempDir, 'app\Acme.Widgets370.bpl'), 'hello');

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'Acme.Widgets370.bpl'));
    Assert.AreEqual(cHelloSha256, HashOf(FindComponent(doc, 'Acme.Widgets370.bpl'), 'SHA-256'));
  finally
    doc.Free;
  end;
end;

const
  cTmsRule =
    'components:' + sLineBreak +
    '  - name: TMS VCL UI Pack' + sLineBreak +
    '    version: ''13.0''' + sLineBreak +
    '    supplier: tmssoftware.com' + sLineBreak +
    '    supplierUrl: https://www.tmssoftware.com' + sLineBreak +
    '    license: Commercial' + sLineBreak +
    '    purl: pkg:generic/tms-vcl-ui-pack@13.0' + sLineBreak +
    '    unitPrefixes: [AdvGrid, AdvEdit]' + sLineBreak;

procedure TSBOMGeneratorTests.ComponentRule_ClaimsMatchingMapUnits;
var
  dproj : string;
  doc : TJsonObject;
  tms : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), cTmsRule);
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['AdvGrid=' + TPath.Combine(FTempDir, 'libs\tms\Source\AdvGrid.pas'),
     'AdvEdit=' + TPath.Combine(FTempDir, 'libs\tms\Source\AdvEdit.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    tms := FindComponent(doc, 'TMS VCL UI Pack');
    Assert.IsNotNull(tms, 'the rule names the library');
    Assert.AreEqual('13.0', tms.S['version']);
    Assert.AreEqual('tmssoftware.com', tms.O['supplier'].S['name']);
    Assert.AreEqual('Commercial', tms.A['licenses'].O[0].O['license'].S['name']);
    Assert.AreEqual('pkg:generic/tms-vcl-ui-pack@13.0', tms.S['purl']);
    Assert.Contains(EvidenceLocations(tms), 'AdvGrid.pas');
    Assert.Contains(EvidenceLocations(tms), 'AdvEdit.pas');
    Assert.IsTrue(DependsOn(doc, RootOf(doc).S['bom-ref'], tms.S['bom-ref']), 'linked from the project');
    Assert.AreEqual(1, Length(ComponentsOfKind(doc, 'third-party')), 'not also grouped under the folder name');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ComponentRule_ClaimsUnitsWithoutASourcePath;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), cTmsRule);
  //linked from a .dcu without debug info - only the unit name is known
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'), ['AdvGrid=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'TMS VCL UI Pack'));
    Assert.Contains(EvidenceLocations(FindComponent(doc, 'TMS VCL UI Pack')), 'AdvGrid');
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'unidentified')));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ComponentRule_ClaimsLibrarySourceVendoredIntoTheProject;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: SynEdit' + sLineBreak +
    '    license: MPL-1.1' + sLineBreak +
    '    unitPrefixes: [SynEdit]' + sLineBreak);
  //inside the project folder, which on its own would make it the project's own code
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['SynEdit=' + TPath.Combine(FTempDir, 'app\lib\SynEdit\SynEdit.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'SynEdit'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.OwnUnits_AreNotComponents;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'ownUnits: [MainForm]' + sLineBreak +
    'ownUnitPrefixes: [AcmeShared]' + sLineBreak);
  //code shared between the company's projects, outside this project's folder
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['MainForm=' + TPath.Combine(FTempDir, 'shared\MainForm.pas'),
     'AcmeShared.Logging=' + TPath.Combine(FTempDir, 'shared\AcmeShared.Logging.pas'),
     'AcmeShared.Config=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'third-party')));
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'unidentified')));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DpmPackage_KeepsItsUnitsWhenARuleAlsoMatches;
var
  dproj : string;
  doc : TJsonObject;
begin
  FDpmPackages := '                <PackageReference id="Acme.Lib" version="1.0.0"/>'#13#10;
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: Acme Lib By Hand' + sLineBreak +
    '    unitPrefixes: [Acme.]' + sLineBreak);
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['Acme.Core=' + TPath.Combine(FTempDir, 'cache\delphi13.0\Acme.Lib\1.0.0\Source\Acme.Core.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    //DPM knows which package installed the unit - a rule written by hand doesn't override that
    Assert.Contains(EvidenceLocations(FindComponent(doc, 'Acme.Lib')), 'Acme.Core.pas');
    Assert.IsNull(FindComponent(doc, 'Acme Lib By Hand'));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.DeclaredComponent_WithoutUnitRules_IsAlwaysListed;
var
  dproj : string;
  doc : TJsonObject;
  declared : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  //loaded with LoadLibrary at runtime, so neither the MAP file nor the imports show it
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: Acme Scanner Driver' + sLineBreak +
    '    version: 4.2.0' + sLineBreak +
    '    license: Proprietary' + sLineBreak);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    declared := FindComponent(doc, 'Acme Scanner Driver');
    Assert.IsNotNull(declared);
    Assert.AreEqual('4.2.0', declared.S['version']);
    Assert.IsTrue(DependsOn(doc, RootOf(doc).S['bom-ref'], declared.S['bom-ref']));
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ComponentRuleThatMatchesNothing_IsWarned;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), cTmsRule);
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'), ['ZzNoSuchUnitAnywhere=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  //a stale rule - the library was dropped, or the prefix is wrong
  Assert.IsTrue(FLogger.Logged('warning', 'TMS VCL UI Pack'), FLogger.Messages.Text);
end;

procedure TSBOMGeneratorTests.ComponentRule_IsNotCalledStaleWithoutAMapFile;
var
  dproj : string;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'), cTmsRule);
  //no MAP file - there was nothing to match against, and the missing MAP file is warned about already

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  Assert.IsFalse(FLogger.Logged('warning', 'TMS VCL UI Pack'), FLogger.Messages.Text);
end;

procedure TSBOMGeneratorTests.ComponentRule_ForALibraryDpmManages_IsNotCalledStale;
var
  dproj : string;
begin
  //a rule carried over from before the library was installed with DPM
  FDpmPackages := '                <PackageReference id="Acme.Lib" version="1.0.0"/>'#13#10;
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: Acme Lib By Hand' + sLineBreak +
    '    unitPrefixes: [Acme.]' + sLineBreak);
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'),
    ['Acme.Core=' + TPath.Combine(FTempDir, 'cache\delphi13.0\Acme.Lib\1.0.0\Source\Acme.Core.pas')]);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  //the library is linked - the DPM package accounts for it
  Assert.IsFalse(FLogger.Logged('warning', 'Acme Lib By Hand'), FLogger.Messages.Text);
end;

procedure TSBOMGeneratorTests.RelativeSourcePath_IsResolvedThroughTheSearchPath;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteBytes(TPath.Combine(FTempDir, 'app\AppUtils.pas'), 'unit AppUtils;');
  //dcc records some source paths as bare file names - here the project's own unit
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'), ['AppUtils=AppUtils.pas']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.AreEqual(0, Length(ComponentsOfKind(doc, 'third-party')), 'the project''s own unit is not a third-party component');
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.ComponentRuleType_IsTheCycloneDXType;
var
  dproj : string;
  doc : TJsonObject;
begin
  dproj := TPath.Combine(FTempDir, 'app\App.dproj');
  WriteDproj(dproj, 'App', '', '');
  WriteText(TPath.Combine(FTempDir, 'app\App.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: Spring4D' + sLineBreak +
    '    type: framework' + sLineBreak +
    '    unitPrefixes: [Spring]' + sLineBreak);
  WriteMap(TPath.Combine(FTempDir, 'app\App.map'), ['Spring.Collections=']);

  Assert.IsTrue(Generate(dproj, 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\App-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'Spring4D'));
    Assert.AreEqual('framework', FindComponent(doc, 'Spring4D').S['type']);
  finally
    doc.Free;
  end;
end;

procedure TSBOMGeneratorTests.GroupProject_GroupRule_IsOneComponentLinkedFromEachProject;
var
  doc : TJsonObject;
  libRef : string;
begin
  WriteSharedComponentsGroup;
  WriteText(TPath.Combine(FTempDir, 'group\Suite.sbom.yaml'),
    'components:' + sLineBreak +
    '  - name: Library A' + sLineBreak +
    '    unitPrefixes: [LibA.]' + sLineBreak);

  Assert.IsTrue(Generate(TPath.Combine(FTempDir, 'group\Suite.groupproj'), 'Release', TPath.Combine(FTempDir, 'out'), false));

  doc := LoadJson(TPath.Combine(FTempDir, 'out\Suite-Win32.cdx.json'));
  try
    Assert.IsNotNull(FindComponent(doc, 'Library A'));
    libRef := FindComponent(doc, 'Library A').S['bom-ref'];
    Assert.IsTrue(DependsOn(doc, FindComponent(doc, 'App1').S['bom-ref'], libRef));
    Assert.IsTrue(DependsOn(doc, FindComponent(doc, 'App2').S['bom-ref'], libRef));
    AssertBomRefsAreUnique(doc);
  finally
    doc.Free;
  end;
end;

{ TSBOMProductWarningsTests }

function TSBOMProductWarningsTests.Complete : TSBOMProductInfo;
begin
  result.Version := '2.1.0';
  result.Supplier := 'Acme Pty Ltd';
  result.SupplierUrl := 'https://acme.example';
  result.License := 'MIT';
  result.SbomCreator := 'sbom@acme.example';
end;

procedure TSBOMProductWarningsTests.AssertOneWarningAbout(const warnings : TArray<string>; const key : string);
begin
  Assert.AreEqual(1, Length(warnings), 'expected exactly one warning');
  //the key name is what a user types into the sidecar, so its casing matters
  Assert.IsTrue(Pos(key, warnings[0]) > 0, 'expected the warning to name [' + key + '] : ' + warnings[0]);
end;

procedure TSBOMProductWarningsTests.CompleteMetadata_HasNoWarnings;
begin
  Assert.AreEqual(0, Length(GetProductInfoWarnings(Complete, true, 'App.sbom.yaml')));
end;

procedure TSBOMProductWarningsTests.MissingSupplier_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.Supplier := '';
  info.SupplierUrl := '';
  AssertOneWarningAbout(GetProductInfoWarnings(info, true, 'App.sbom.yaml'), 'supplier:');
end;

procedure TSBOMProductWarningsTests.SupplierWithoutContact_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.SupplierUrl := '';
  AssertOneWarningAbout(GetProductInfoWarnings(info, true, 'App.sbom.yaml'), 'supplierUrl:');
end;

procedure TSBOMProductWarningsTests.SupplierThatIsAnEmail_IsItsOwnContact;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.Supplier := 'sales@acme.example';
  info.SupplierUrl := '';
  Assert.AreEqual(0, Length(GetProductInfoWarnings(info, true, 'App.sbom.yaml')));
end;

procedure TSBOMProductWarningsTests.MissingCreator_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.SbomCreator := '';
  AssertOneWarningAbout(GetProductInfoWarnings(info, true, 'App.sbom.yaml'), 'sbomCreator:');
end;

procedure TSBOMProductWarningsTests.MissingVersion_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.Version := '';
  AssertOneWarningAbout(GetProductInfoWarnings(info, false, 'App.sbom.yaml'), 'version:');
end;

procedure TSBOMProductWarningsTests.DelphiDefaultVersionFromVersionInfo_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.Version := '1.0.0.0';
  //a new Delphi project starts at 1.0.0.0, so coming from version info it is probably not real
  AssertOneWarningAbout(GetProductInfoWarnings(info, false, 'App.sbom.yaml'), 'version:');
end;

procedure TSBOMProductWarningsTests.DelphiDefaultVersionGivenExplicitly_IsNotWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.Version := '1.0.0.0';
  Assert.AreEqual(0, Length(GetProductInfoWarnings(info, true, 'App.sbom.yaml')));
end;

procedure TSBOMProductWarningsTests.MissingLicense_IsWarned;
var
  info : TSBOMProductInfo;
begin
  info := Complete;
  info.License := '';
  AssertOneWarningAbout(GetProductInfoWarnings(info, true, 'App.sbom.yaml'), 'license:');
end;

{ TSBOMContactTests }

procedure TSBOMContactTests.IsEmailAddress(const value : string; const expected : boolean);
begin
  Assert.AreEqual(expected, DPM.Core.SBOM.ProductInfo.IsEmailAddress(value));
end;

procedure TSBOMContactTests.IsHttpUrl(const value : string; const expected : boolean);
begin
  Assert.AreEqual(expected, DPM.Core.SBOM.ProductInfo.IsHttpUrl(value));
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMGeneratorTests);
  TDUnitX.RegisterTestFixture(TSBOMContactTests);
  TDUnitX.RegisterTestFixture(TSBOMProductWarningsTests);

end.
