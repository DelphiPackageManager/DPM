{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Tests.Project.ConfigPatcher;

interface

uses
  DPM.Core.Types,
  DPM.Core.Project.ConfigPatcher,
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TProjectConfigPatcherTests = class
  private
    function MakeTempFile : string;
    function WriteSandbox(const xml : string) : string;
    function ReadSandbox(const sandbox : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure MissingConfig_AddsBuildConfigurationItem;

    [Test]
    procedure MissingConfig_AddsActivatorPropertyGroup;

    [Test]
    procedure MissingConfig_ActivatorPrecedesBaseSettingsGroup;

    [Test]
    procedure MissingConfig_AddsCanonicalReleaseSettings;

    [Test]
    procedure MissingConfig_Debug_AddsDebugSettings;

    [Test]
    procedure MissingConfig_DebugInformation_IsIntegerOnXE5Plus;

    [Test]
    procedure MissingConfig_DebugInformation_IsBooleanPreXE5;

    [Test]
    procedure MissingConfig_AllocatesNextFreeCfgKey;

    [Test]
    procedure MissingPlatform_AddsPlatformListEntry;

    [Test]
    procedure MissingPlatform_AddsChainStub;

    [Test]
    procedure MissingPlatform_WithoutOption_LeavesPlatformListAlone;

    [Test]
    procedure MissingPlatform_DisabledEntry_IsEnabled;

    [Test]
    procedure MissingPlatform_AddsBaseStub_WhenProjectUsesThem;

    [Test]
    procedure MissingPlatform_DoesNotAddBaseStub_WhenProjectHasNone;

    [Test]
    procedure MissingPlatform_AddsWindowsNamespaces_WhenBaseStubsUsed;

    [Test]
    procedure NoPlatformsBlock_DoesNotSynthesizeProjectExtensions;

    [Test]
    procedure NothingMissing_ReturnsNotNeeded;

    [Test]
    procedure NothingMissing_DoesNotRewriteFile;

    [Test]
    procedure SecondCall_IsNoOp;

    [Test]
    procedure MissingFile_ReturnsFailed;

    [Test]
    procedure MalformedXml_ReturnsFailed;

    [Test]
    procedure GeneratedTemplateProject_NeedsNoPatch;

    [Test]
    procedure PatchedProject_LoadsInProjectEditor;

    [Test]
    procedure PatchedProject_SearchPathLoaderResolvesConfig;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.Project.Prepare.Templates,
  DPM.Core.Compiler.ProjectSettings,
  TestLogger;

const
  //Template shaped - everything under '$(Base)'!='', no Base_<Platform> stubs. Win32 only,
  //Debug (Cfg_1) + Release (Cfg_2).
  cDprojWin32Only =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Foo.dpk</MainSource>'#13#10 +
    '        <ProjectVersion>20.3</ProjectVersion>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Release</Config>'#13#10 +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>'#13#10 +
    '        <AppType>Package</AppType>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
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
    '        <DCC_DcuOutput>.\$(Platform)\$(Config)</DCC_DcuOutput>'#13#10 +
    '        <DCC_UnitSearchPath>.\Source;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_1)''!=''''">'#13#10 +
    '        <DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '        <DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <DCCReference Include="rtl.dcp"/>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_2</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Debug">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '    <ProjectExtensions>'#13#10 +
    '        <BorlandProject>'#13#10 +
    '            <Platforms>'#13#10 +
    '                <Platform value="Win32">true</Platform>'#13#10 +
    '            </Platforms>'#13#10 +
    '        </BorlandProject>'#13#10 +
    '    </ProjectExtensions>'#13#10 +
    '</Project>'#13#10;

  //Win32 + Win64 declared, but only a Debug config exists.
  cDprojDebugOnly =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Debug</Config>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
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
    '    <PropertyGroup Condition="(''$(Platform)''==''Win64'' and ''$(Cfg_1)''==''true'') or ''$(Cfg_1_Win64)''!=''''">'#13#10 +
    '        <Cfg_1_Win64>true</Cfg_1_Win64>'#13#10 +
    '        <CfgParent>Cfg_1</CfgParent>'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <DCC_UnitSearchPath>.\Source;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Debug">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '    <ProjectExtensions>'#13#10 +
    '        <BorlandProject>'#13#10 +
    '            <Platforms>'#13#10 +
    '                <Platform value="Win32">true</Platform>'#13#10 +
    '                <Platform value="Win64">true</Platform>'#13#10 +
    '            </Platforms>'#13#10 +
    '        </BorlandProject>'#13#10 +
    '    </ProjectExtensions>'#13#10 +
    '</Project>'#13#10;

  //Non contiguous config keys - Cfg_1 and Cfg_4, so the next free key is Cfg_5.
  cDprojSparseCfgKeys =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Debug'' or ''$(Cfg_1)''!=''''">'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''JCLDEBUG'' or ''$(Cfg_4)''!=''''">'#13#10 +
    '        <Cfg_4>true</Cfg_4>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Debug">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="JCLDEBUG">'#13#10 +
    '            <Key>Cfg_4</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '</Project>'#13#10;

  //IDE authored shape - uses Base_<Platform> stubs and hangs DCC_Namespace off them. Win32 only.
  cDprojWithBaseStubs =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <Base>True</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Release</Config>'#13#10 +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="(''$(Platform)''==''Win32'' and ''$(Base)''==''true'') or ''$(Base_Win32)''!=''''">'#13#10 +
    '        <Base_Win32>true</Base_Win32>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
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
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base_Win32)''!=''''">'#13#10 +
    '        <DCC_Namespace>Winapi;System.Win;Bde;$(DCC_Namespace)</DCC_Namespace>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_2</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '    <ProjectExtensions>'#13#10 +
    '        <BorlandProject>'#13#10 +
    '            <Platforms>'#13#10 +
    '                <Platform value="Win32">true</Platform>'#13#10 +
    '                <Platform value="Win64">False</Platform>'#13#10 +
    '            </Platforms>'#13#10 +
    '        </BorlandProject>'#13#10 +
    '    </ProjectExtensions>'#13#10 +
    '</Project>'#13#10;

  //No ProjectExtensions at all.
  cDprojNoPlatformsBlock =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''">'#13#10 +
    '        <Cfg_2>true</Cfg_2>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_2</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '</Project>'#13#10;

{ TProjectConfigPatcherTests }

procedure TProjectConfigPatcherTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TProjectConfigPatcherTests.FixtureTearDown;
begin
  CoUninitialize;
end;

function TProjectConfigPatcherTests.MakeTempFile : string;
begin
  result := TPath.Combine(TPath.GetTempPath, 'dpmpatch_' + TGuid.NewGuid.ToString + '.dproj');
end;

function TProjectConfigPatcherTests.WriteSandbox(const xml : string) : string;
begin
  result := MakeTempFile;
  TFile.WriteAllText(result, xml, TEncoding.UTF8);
end;

function TProjectConfigPatcherTests.ReadSandbox(const sandbox : string) : string;
begin
  result := TFile.ReadAllText(sandbox, TEncoding.UTF8);
end;

procedure TProjectConfigPatcherTests.MissingConfig_AddsBuildConfigurationItem;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
  patchResult : TProjectPatchResult;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patchResult := patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    Assert.IsTrue(patchResult = TProjectPatchResult.Patched, 'expected Patched');
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<BuildConfiguration Include="Release">');
    Assert.Contains(xml, '<Key>Cfg_2</Key>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_AddsActivatorPropertyGroup;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, 'Condition="''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''"');
    Assert.Contains(xml, '<Cfg_2>true</Cfg_2>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//msbuild evaluates property groups top down - an activator placed after the '$(Base)'!=''
//settings group would leave Base empty when that group is evaluated.
procedure TProjectConfigPatcherTests.MissingConfig_ActivatorPrecedesBaseSettingsGroup;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
  activatorPos : integer;
  settingsPos : integer;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    activatorPos := Pos('''$(Config)''==''Release''', xml);
    settingsPos := Pos('Condition="''$(Base)''!=''''"', xml);
    Assert.IsTrue(activatorPos > 0, 'activator group not found');
    Assert.IsTrue(settingsPos > 0, 'base settings group not found');
    Assert.IsTrue(activatorPos < settingsPos, 'activator must precede the base settings group');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_AddsCanonicalReleaseSettings;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, 'Condition="''$(Cfg_2)''!=''''"');
    Assert.Contains(xml, '<DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_Debug_AddsDebugSettings;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojNoPlatformsBlock);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Debug', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>');
    Assert.Contains(xml, '<DCC_Optimize>false</DCC_Optimize>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_DebugInformation_IsIntegerOnXE5Plus;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.DelphiXE5, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>0</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_DebugInformation_IsBooleanPreXE5;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.DelphiXE2, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>false</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingConfig_AllocatesNextFreeCfgKey;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojSparseCfgKeys);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<Key>Cfg_5</Key>');
    Assert.IsFalse(Pos('<Key>Cfg_2</Key>', xml) > 0, 'must not reuse a key below the highest in use');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_AddsPlatformListEntry;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<Platform value="Win64">true</Platform>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_AddsChainStub;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, 'Condition="(''$(Platform)''==''Win64'' and ''$(Cfg_2)''==''true'') or ''$(Cfg_2_Win64)''!=''''"');
    Assert.Contains(xml, '<Cfg_2_Win64>true</Cfg_2_Win64>');
    Assert.Contains(xml, '<CfgParent>Cfg_2</CfgParent>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//The design entry call site passes no options - writing the platform list there would poison the
//manifest-silent design platform inspection on every later restore.
procedure TProjectConfigPatcherTests.MissingPlatform_WithoutOption_LeavesPlatformListAlone;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    xml := ReadSandbox(sandbox);
    Assert.IsFalse(Pos('<Platform value="Win64"', xml) > 0, 'platform list must not be touched');
    Assert.Contains(xml, '<Cfg_2_Win64>true</Cfg_2_Win64>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_DisabledEntry_IsEnabled;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWithBaseStubs);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<Platform value="Win64">true</Platform>');
    Assert.IsFalse(Pos('<Platform value="Win64">False</Platform>', xml) > 0, 'the disabled entry should have been flipped, not duplicated');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_AddsBaseStub_WhenProjectUsesThem;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWithBaseStubs);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, '<Base_Win64>true</Base_Win64>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_DoesNotAddBaseStub_WhenProjectHasNone;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.IsFalse(Pos('Base_Win64', xml) > 0, 'template shaped projects need no Base_<Platform> stubs');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingPlatform_AddsWindowsNamespaces_WhenBaseStubsUsed;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWithBaseStubs);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.Contains(xml, 'Condition="''$(Base_Win64)''!=''''"');
    Assert.Contains(xml, 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;$(DCC_Namespace)');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.NoPlatformsBlock_DoesNotSynthesizeProjectExtensions;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  xml : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojNoPlatformsBlock);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    xml := ReadSandbox(sandbox);
    Assert.IsFalse(Pos('<ProjectExtensions', xml) > 0, 'ProjectExtensions must not be synthesized');
    Assert.Contains(xml, '<Cfg_2_Win64>true</Cfg_2_Win64>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.NothingMissing_ReturnsNotNeeded;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  patchResult : TProjectPatchResult;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patchResult := patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0,
                                             [TProjectPatchOption.UpdatePlatformList]);
    Assert.IsTrue(patchResult = TProjectPatchResult.NotNeeded, 'expected NotNeeded');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//PrettyFormatXML reflows the whole document on save, so an unconditional save would rewrite every
//cached dproj on every install. Prove the save is skipped entirely.
procedure TProjectConfigPatcherTests.NothingMissing_DoesNotRewriteFile;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  before : string;
  after : string;
  writeTimeBefore : TDateTime;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    before := ReadSandbox(sandbox);
    writeTimeBefore := TFile.GetLastWriteTime(sandbox);
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    after := ReadSandbox(sandbox);
    Assert.AreEqual(before, after, 'file content must be untouched');
    Assert.IsTrue(writeTimeBefore = TFile.GetLastWriteTime(sandbox), 'file timestamp must be untouched');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.SecondCall_IsNoOp;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  firstResult : TProjectPatchResult;
  secondResult : TProjectPatchResult;
  afterFirst : string;
  afterSecond : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    firstResult := patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                                             [TProjectPatchOption.UpdatePlatformList]);
    afterFirst := ReadSandbox(sandbox);
    secondResult := patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                                              [TProjectPatchOption.UpdatePlatformList]);
    afterSecond := ReadSandbox(sandbox);
    Assert.IsTrue(firstResult = TProjectPatchResult.Patched, 'first call should patch');
    Assert.IsTrue(secondResult = TProjectPatchResult.NotNeeded, 'second call should be a no-op');
    Assert.AreEqual(afterFirst, afterSecond, 'second call must not change the file');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectConfigPatcherTests.MissingFile_ReturnsFailed;
var
  patcher : IProjectConfigPatcher;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  Assert.IsTrue(patcher.EnsureBuildTarget(MakeTempFile, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0, [])
    = TProjectPatchResult.Failed, 'a missing file should report Failed');
end;

procedure TProjectConfigPatcherTests.MalformedXml_ReturnsFailed;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox('this is not xml <<<');
  try
    Assert.IsTrue(patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0, [])
      = TProjectPatchResult.Failed, 'malformed xml should report Failed');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//Projects generated into the cache by TPackageProjectGenerator are correct by construction - the
//patcher must never rewrite them.
procedure TProjectConfigPatcherTests.GeneratedTemplateProject_NeedsNoPatch;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(TPrepareTemplates.RenderDproj('MyPkg', TCompilerVersion.Delphi12_0, nil, nil,
                                                        pkRuntime, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
  try
    Assert.IsTrue(patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Release', TCompilerVersion.Delphi12_0, [TProjectPatchOption.UpdatePlatformList])
      = TProjectPatchResult.NotNeeded, 'generated projects need no patch');
    Assert.IsTrue(patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, [TProjectPatchOption.UpdatePlatformList])
      = TProjectPatchResult.NotNeeded, 'generated projects need no patch');
    Assert.IsTrue(patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win32, 'Debug', TCompilerVersion.Delphi12_0, [TProjectPatchOption.UpdatePlatformList])
      = TProjectPatchResult.NotNeeded, 'generated projects need no patch');
    Assert.IsTrue(patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Debug', TCompilerVersion.Delphi12_0, [TProjectPatchOption.UpdatePlatformList])
      = TProjectPatchResult.NotNeeded, 'generated projects need no patch');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//The patched file has to satisfy DPM's own readers, not just msbuild.
procedure TProjectConfigPatcherTests.PatchedProject_LoadsInProjectEditor;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  editor : IProjectEditor;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojWin32Only);
  try
    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0,
                              [TProjectPatchOption.UpdatePlatformList]);
    editor := TProjectEditor.Create(TTestLogger.Create, nil, TCompilerVersion.Delphi12_0);
    Assert.IsTrue(editor.LoadProject(sandbox, [TProjectElement.Platforms, TProjectElement.Configs]), 'LoadProject should succeed');
    Assert.IsTrue(TDPMPlatform.Win64 in editor.Platforms, 'Win64 should be a declared platform');
    Assert.IsTrue(editor.GetProjectConfiguration('Release', TDPMPlatform.Win64) <> nil, 'Release/Win64 config should resolve');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

//Without the BuildConfiguration ItemGroup entry the settings loader silently returns '' and the
//project's own DCC_UnitSearchPath never reaches the msbuild command line.
procedure TProjectConfigPatcherTests.PatchedProject_SearchPathLoaderResolvesConfig;
var
  patcher : IProjectConfigPatcher;
  sandbox : string;
  loader : IProjectSettingsLoader;
begin
  patcher := TProjectConfigPatcher.Create(TTestLogger.Create);
  sandbox := WriteSandbox(cDprojDebugOnly);
  try
    //before patching the loader cannot resolve the Release key at all and returns nothing.
    loader := TDPMProjectSettingsLoader.Create(TTestLogger.Create, sandbox, 'Release', TDPMPlatform.Win64);
    Assert.AreEqual('', loader.GetSearchPath, 'unpatched project should not resolve the Release config');

    patcher.EnsureBuildTarget(sandbox, TDPMPlatform.Win64, 'Release', TCompilerVersion.Delphi12_0, []);
    loader := TDPMProjectSettingsLoader.Create(TTestLogger.Create, sandbox, 'Release', TDPMPlatform.Win64);
    Assert.Contains(loader.GetSearchPath, '.\Source');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TProjectConfigPatcherTests);

end.
