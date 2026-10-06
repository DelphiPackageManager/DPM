unit DPM.Core.Tests.Compiler.ProjectSettings;

interface

uses
  DPM.Core.Types,
  DUnitX.TestFramework;

type
  //TDPMProjectSettingsLoader stands in for msbuild's own evaluation of the project - what it
  //returns is passed as a global /p:DCC_UnitSearchPath, which REPLACES everything the dproj
  //sets. Anything the loader misses is therefore missing from the build entirely.
  {$M+}
  [TestFixture]
  TProjectSettingsLoaderTests = class
  private
    function WriteSandbox(const xml : string) : string;
    function LoadSearchPath(const xml : string; const configName : string; const platform : TDPMPlatform) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure BaseSearchPath_IsIncluded;

    [Test]
    procedure BasePlatformSearchPath_IsIncluded;

    [Test]
    procedure ConfigPlatformSearchPath_IsIncluded;

    [Test]
    procedure OtherPlatformSearchPath_IsNotIncluded;

    [Test]
    procedure InheritedConfig_UsesParentConfigButNotItsPlatformGroup;

    [Test]
    procedure PlatformAndConfigMacros_AreExpanded;

    [Test]
    procedure PlatformAndConfigMacros_AreExpanded_IgnoringCase;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Compiler.ProjectSettings,
  TestLogger;

const
  //IDE authored shape. Release is Cfg_1, Debug is Cfg_2 and Custom (Cfg_3) inherits from Release.
  //Every level of the chain contributes its own search path entry so each test can tell exactly
  //which property groups were visited :
  //
  //  Base              .\BaseOnly and .\$(Platform)\$(Config)
  //  Base_WinARM64EC   ..\Source\WinARM64EC   <- the Abbrevia case, static libs for one platform
  //  Base_Win64        ..\Source\Win64
  //  Cfg_1             .\ReleaseOnly
  //  Cfg_1_Win64       .\Release64
  //  Cfg_2             ..\Lib\$(PLATFORM)\$(CONFIG)   <- msbuild property names ignore case
  //  Cfg_3             .\CustomOnly
  cDproj =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <Base>True</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Release</Config>'#13#10 +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win64'' and ''$(Base)''==''true'') or ''$(Base_Win64)''!=''''">'#13#10 +
    '        <Base_Win64>true</Base_Win64>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''WinARM64EC'' and ''$(Base)''==''true'') or ''$(Base_WinARM64EC)''!=''''">'#13#10 +
    '        <Base_WinARM64EC>true</Base_WinARM64EC>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Release'' or ''$(Cfg_1)''!=''''">'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win64'' and ''$(Cfg_1)''==''true'') or ''$(Cfg_1_Win64)''!=''''">'#13#10 +
    '        <Cfg_1_Win64>true</Cfg_1_Win64>'#13#10 +
    '        <CfgParent>Cfg_1</CfgParent>'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Debug'' or ''$(Cfg_2)''!=''''">'#13#10 +
    '        <Cfg_2>true</Cfg_2>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Custom'' or ''$(Cfg_3)''!=''''">'#13#10 +
    '        <Cfg_3>true</Cfg_3>'#13#10 +
    '        <CfgParent>Cfg_1</CfgParent>'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>.\BaseOnly;.\$(Platform)\$(Config);$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base_Win64)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>..\Source\Win64;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base_WinARM64EC)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>..\Source\WinARM64EC;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_1)''!=''''">'#13#10 +
    '        <DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>'#13#10 +
    '        <DCC_UnitSearchPath>.\ReleaseOnly;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_1_Win64)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>.\Release64;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '        <DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>'#13#10 +
    '        <DCC_UnitSearchPath>..\Lib\$(PLATFORM)\$(CONFIG);$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_3)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>.\CustomOnly;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Debug">'#13#10 +
    '            <Key>Cfg_2</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Custom">'#13#10 +
    '            <Key>Cfg_3</Key>'#13#10 +
    '            <CfgParent>Cfg_1</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '</Project>'#13#10;

{ TProjectSettingsLoaderTests }

procedure TProjectSettingsLoaderTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TProjectSettingsLoaderTests.FixtureTearDown;
begin
  CoUninitialize;
end;

function TProjectSettingsLoaderTests.WriteSandbox(const xml : string) : string;
begin
  result := TPath.Combine(TPath.GetTempPath, 'dpmsettings_' + TGuid.NewGuid.ToString + '.dproj');
  TFile.WriteAllText(result, xml, TEncoding.UTF8);
end;

function TProjectSettingsLoaderTests.LoadSearchPath(const xml : string; const configName : string; const platform : TDPMPlatform) : string;
var
  sandbox : string;
  loader : IProjectSettingsLoader;
begin
  sandbox := WriteSandbox(xml);
  try
    loader := TDPMProjectSettingsLoader.Create(TTestLogger.Create, sandbox, configName, platform);
    result := loader.GetSearchPath;
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectSettingsLoaderTests.BaseSearchPath_IsIncluded;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Debug', TDPMPlatform.Win32);
  Assert.Contains(searchPath, '.\BaseOnly');
end;

//The reported failure - a library that links per platform static libs puts their folder on the
//search path of that one platform only (Base_WinARM64EC). The IDE build finds them, ours did not.
procedure TProjectSettingsLoaderTests.BasePlatformSearchPath_IsIncluded;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Release', TDPMPlatform.WinARM64EC);
  Assert.Contains(searchPath, '..\Source\WinARM64EC');
  Assert.Contains(searchPath, '.\BaseOnly', 'the platform group adds to the Base value, it does not replace it');
  Assert.Contains(searchPath, '.\ReleaseOnly');
end;

procedure TProjectSettingsLoaderTests.ConfigPlatformSearchPath_IsIncluded;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Release', TDPMPlatform.Win64);
  Assert.Contains(searchPath, '.\Release64');
  Assert.Contains(searchPath, '..\Source\Win64');
  Assert.Contains(searchPath, '.\ReleaseOnly');
  Assert.Contains(searchPath, '.\BaseOnly');
end;

procedure TProjectSettingsLoaderTests.OtherPlatformSearchPath_IsNotIncluded;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Release', TDPMPlatform.Win32);
  Assert.DoesNotContain(searchPath, 'WinARM64EC');
  Assert.DoesNotContain(searchPath, 'Win64');
  Assert.DoesNotContain(searchPath, '.\Release64');
  Assert.Contains(searchPath, '.\ReleaseOnly');

  //Debug has no Win64 group of its own, so Release's must not leak into it.
  searchPath := LoadSearchPath(cDproj, 'Debug', TDPMPlatform.Win64);
  Assert.DoesNotContain(searchPath, '.\Release64');
  Assert.DoesNotContain(searchPath, '.\ReleaseOnly');
  Assert.Contains(searchPath, '..\Source\Win64');
end;

//Custom -> Release -> Base. The Custom activator turns Cfg_1 on, so Release's all-platforms group
//applies - but the IDE writes that activator AFTER the Cfg_1_Win64 one, which msbuild has by then
//already evaluated as false. So the parent config's platform group is NOT part of the chain; only
//the config's own platform group and Base's are.
procedure TProjectSettingsLoaderTests.InheritedConfig_UsesParentConfigButNotItsPlatformGroup;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Custom', TDPMPlatform.Win64);
  Assert.Contains(searchPath, '.\CustomOnly');
  Assert.Contains(searchPath, '.\ReleaseOnly');
  Assert.Contains(searchPath, '..\Source\Win64');
  Assert.Contains(searchPath, '.\BaseOnly');
  Assert.DoesNotContain(searchPath, '.\Release64');
end;

//Property references do not survive being passed in a /p: value - left as they are $(Platform)
//and $(Config) reach the compiler as nothing at all, so ..\Source\$(Platform) becomes ..\Source.
//Both are known when we build, so the loader has to hand back the resolved path.
procedure TProjectSettingsLoaderTests.PlatformAndConfigMacros_AreExpanded;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Release', TDPMPlatform.WinARM64EC);
  Assert.Contains(searchPath, '.\WinARM64EC\Release');
  Assert.DoesNotContain(searchPath, '$(Platform)');
  Assert.DoesNotContain(searchPath, '$(Config)');

  //each platform and config gets its own values, not whatever was resolved first.
  searchPath := LoadSearchPath(cDproj, 'Custom', TDPMPlatform.Win64);
  Assert.Contains(searchPath, '.\Win64\Custom');
end;

procedure TProjectSettingsLoaderTests.PlatformAndConfigMacros_AreExpanded_IgnoringCase;
var
  searchPath : string;
begin
  searchPath := LoadSearchPath(cDproj, 'Debug', TDPMPlatform.Win64);
  Assert.Contains(searchPath, '..\Lib\Win64\Debug');
  Assert.Contains(searchPath, '.\Win64\Debug');
  Assert.DoesNotContain(searchPath, '$(');
end;

initialization
  TDUnitX.RegisterTestFixture(TProjectSettingsLoaderTests);

end.
