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

unit DPM.Core.Project.Prepare.Templates;

interface

uses
  Spring.Collections,
  DPM.Core.Types;

type
  //pkRuntime  : runtime-only package (RuntimeOnlyPackage=true; requires rtl)
  //pkDesign   : design-time package (DesignOnlyPackage=true; requires rtl, designide)
  TPrepareProjectKind = (pkRuntime, pkDesign);

  TPrepareTemplates = class
  public
    //Renders a minimal .dpk file. sourceFiles are relative paths from the package folder
    //(e.g. "..\..\Source\Unit1.pas") - one `contains` entry is emitted per file.
    //Pass nil/empty to emit a TODO-placeholder.
    class function RenderDpk(const packageId : string; const compiler : TCompilerVersion;
                             const sourceFiles : IList<string>;
                             const kind : TPrepareProjectKind) : string;

    //Renders a minimal .dproj file flavored for the supplied compiler. sourceFiles are
    //relative paths from the package folder - one `<DCCReference>` per file. Pass nil/empty
    //to emit just the rtl.dcp reference. platforms drives the per-platform Cfg_N_<Platform>
    //chain stubs and the BorlandProject <Platforms> list; pass [] to fall back to Win32.
    class function RenderDproj(const packageId : string; const compiler : TCompilerVersion;
                               const sourceFiles : IList<string>;
                               const kind : TPrepareProjectKind;
                               const platforms : TDPMPlatforms) : string;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

const
  cDpkTemplate =
    'package {{PACKAGE_ID}};'#13#10 +
    #13#10 +
    '{$R *.res}'#13#10 +
    '{$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}'#13#10 +
    '{$ALIGN 8}'#13#10 +
    '{$ASSERTIONS ON}'#13#10 +
    '{$BOOLEVAL OFF}'#13#10 +
    '{$DEBUGINFO ON}'#13#10 +
    '{$EXTENDEDSYNTAX ON}'#13#10 +
    '{$IMPORTEDDATA ON}'#13#10 +
    '{$IOCHECKS ON}'#13#10 +
    '{$LOCALSYMBOLS ON}'#13#10 +
    '{$LONGSTRINGS ON}'#13#10 +
    '{$OPENSTRINGS ON}'#13#10 +
    '{$OPTIMIZATION OFF}'#13#10 +
    '{$OVERFLOWCHECKS OFF}'#13#10 +
    '{$RANGECHECKS OFF}'#13#10 +
    '{$REFERENCEINFO ON}'#13#10 +
    '{$SAFEDIVIDE OFF}'#13#10 +
    '{$STACKFRAMES ON}'#13#10 +
    '{$TYPEDADDRESS OFF}'#13#10 +
    '{$VARSTRINGCHECKS ON}'#13#10 +
    '{$WRITEABLECONST OFF}'#13#10 +
    '{$MINENUMSIZE 1}'#13#10 +
    '{$IMAGEBASE $400000}'#13#10 +
    '{$DEFINE DEBUG}'#13#10 +
    '{$ENDIF IMPLICITBUILDING}'#13#10 +
    '{{LIBSUFFIX_DIRECTIVE}}'#13#10 +
    '{{KIND_DIRECTIVE}}'#13#10 +
    '{$IMPLICITBUILD OFF}'#13#10 +
    #13#10 +
    'requires'#13#10 +
    '{{REQUIRES_ENTRIES}}'#13#10 +
    #13#10 +
    'contains'#13#10 +
    '{{CONTAINS_ENTRIES}}'#13#10 +
    'end.'#13#10;

  //Minimal dproj template - XE2-flavored, modelled on a real XE2 package dproj. The
  //transformer rewrites ProjectVersion, DCC_DebugInformation (and DCC_LibSuffix if
  //present), DPMCompiler per target compiler when the file is propagated. Targets
  //Win32 + Win64 by default; the author can add other platforms in the IDE.
  cDprojTemplate =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <ProjectGuid>{{GUID_PROJECT}}</ProjectGuid>'#13#10 +
    '        <MainSource>{{PACKAGE_ID}}.dpk</MainSource>'#13#10 +
    '        <ProjectVersion>{{PROJECT_VERSION}}</ProjectVersion>'#13#10 +
    '        <FrameworkType>None</FrameworkType>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '        <Config Condition="''$(Config)''==''''">Release</Config>'#13#10 +
    '        <Platform Condition="''$(Platform)''==''''">{{DEFAULT_PLATFORM}}</Platform>'#13#10 +
    '        <TargetedPlatforms>3</TargetedPlatforms>'#13#10 +
    '        <AppType>Package</AppType>'#13#10 +
    '        <ProjectName Condition="''$(ProjectName)''==''''">{{PACKAGE_ID}}</ProjectName>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Debug'' or ''$(Cfg_1)''!=''''">'#13#10 +
    '        <Cfg_1>true</Cfg_1>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '{{CFG_DEBUG_PLATFORM_GROUPS}}' +
    '    <PropertyGroup Condition="''$(Config)''==''Release'' or ''$(Cfg_2)''!=''''">'#13#10 +
    '        <Cfg_2>true</Cfg_2>'#13#10 +
    '        <CfgParent>Base</CfgParent>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '{{CFG_RELEASE_PLATFORM_GROUPS}}' +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '        <DllSuffix>{{LIB_SUFFIX}}</DllSuffix>'#13#10 +
    '        <DCC_OutputNeverBuildDcps>true</DCC_OutputNeverBuildDcps>'#13#10 +
    '        <DCC_CBuilderOutput>All</DCC_CBuilderOutput>'#13#10 +
    '        <GenPackage>true</GenPackage>'#13#10 +
    '        <GenDll>true</GenDll>'#13#10 +
    '{{PACKAGE_KIND_FLAG}}' +
    '        <DCC_DcuOutput>.\$(Platform)\$(Config)</DCC_DcuOutput>'#13#10 +
    '        <DCC_ExeOutput>.\$(Platform)\$(Config)</DCC_ExeOutput>'#13#10 +
    '        <DCC_E>false</DCC_E>'#13#10 +
    '        <DCC_N>false</DCC_N>'#13#10 +
    '        <DCC_S>false</DCC_S>'#13#10 +
    '        <DCC_F>false</DCC_F>'#13#10 +
    '        <DCC_K>false</DCC_K>'#13#10 +
    '        <SanitizedProjectName>{{PACKAGE_ID}}</SanitizedProjectName>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <PropertyGroup Condition="''$(Cfg_1)''!=''''">'#13#10 +
    '        <DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>'#13#10 +
    '        <DCC_Optimize>false</DCC_Optimize>'#13#10 +
    '        <DCC_GenerateStackFrames>true</DCC_GenerateStackFrames>'#13#10 +
    '        <DCC_DebugInfoInExe>true</DCC_DebugInfoInExe>'#13#10 +
    '        <DCC_RemoteDebug>true</DCC_RemoteDebug>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '{{PLATFORM_OVERRIDES}}' +
    '    <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '        <DCC_LocalDebugSymbols>false</DCC_LocalDebugSymbols>'#13#10 +
    '        <DCC_Define>RELEASE;$(DCC_Define)</DCC_Define>'#13#10 +
    '        <DCC_SymbolReferenceInfo>0</DCC_SymbolReferenceInfo>'#13#10 +
    '        <DCC_DebugInformation>{{DEBUG_INFO_RELEASE}}</DCC_DebugInformation>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <DelphiCompile Include="$(MainSource)">'#13#10 +
    '            <MainSource>MainSource</MainSource>'#13#10 +
    '        </DelphiCompile>'#13#10 +
    '        <DCCReference Include="rtl.dcp"/>'#13#10 +
    '{{DCC_REFERENCES}}' +
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
    '        <Borland.Personality>Delphi.Personality.12</Borland.Personality>'#13#10 +
    '        <Borland.ProjectType>Package</Borland.ProjectType>'#13#10 +
    '        <BorlandProject>'#13#10 +
    '            <Delphi.Personality>'#13#10 +
    '                <Source>'#13#10 +
    '                    <Source Name="MainSource">{{PACKAGE_ID}}.dpk</Source>'#13#10 +
    '                </Source>'#13#10 +
    '            </Delphi.Personality>'#13#10 +
    '            <Platforms>'#13#10 +
    '{{PLATFORMS_LIST}}' +
    '            </Platforms>'#13#10 +
    '        </BorlandProject>'#13#10 +
    '        <ProjectFileVersion>12</ProjectFileVersion>'#13#10 +
    '    </ProjectExtensions>'#13#10 +
    '    <Import Condition="Exists(''$(BDS)\Bin\CodeGear.Delphi.Targets'')" Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '</Project>'#13#10;

function ExtractUnitName(const relativePath : string) : string;
begin
  //unit identifier = file name without extension. The relative path uses the source
  //file's actual name (e.g. "..\..\Source\VSoft.Base64.pas" -> "VSoft.Base64").
  result := ChangeFileExt(ExtractFileName(relativePath), '');
end;

class function TPrepareTemplates.RenderDpk(const packageId : string; const compiler : TCompilerVersion;
                                            const sourceFiles : IList<string>;
                                            const kind : TPrepareProjectKind) : string;
var
  containsBlock : string;
  requiresBlock : string;
  i : integer;
  relativePath : string;
  unitName : string;
  separator : string;
begin
  containsBlock := '';
  if (sourceFiles = nil) or (sourceFiles.Count = 0) then
    containsBlock := '  // TODO: add your units here, e.g. MyUnit in ''..\..\src\MyUnit.pas'';'#13#10 +
                     '  ;'
  else
  begin
    for i := 0 to sourceFiles.Count - 1 do
    begin
      relativePath := sourceFiles[i];
      unitName := ExtractUnitName(relativePath);
      if i < sourceFiles.Count - 1 then
        separator := ','
      else
        separator := ';';
      containsBlock := containsBlock + '  ' + unitName + ' in ''' + relativePath + '''' + separator + #13#10;
    end;
    //trim trailing CRLF for cleaner output (template already adds the closing CRLF after).
    if (Length(containsBlock) >= 2) and (Copy(containsBlock, Length(containsBlock) - 1, 2) = #13#10) then
      SetLength(containsBlock, Length(containsBlock) - 2);
  end;

  //design-time packages need designide for IDE registration to work.
  case kind of
    pkDesign  : requiresBlock := '  rtl,'#13#10'  designide;';
  else //pkRuntime
    requiresBlock := '  rtl;';
  end;

  result := cDpkTemplate;
  result := StringReplace(result, '{{PACKAGE_ID}}', packageId, [rfReplaceAll]);

  //LIBSUFFIX directive syntax differs by version:
  //  - 10.4+ : {$LIBSUFFIX AUTO}  (bare keyword - IDE expands to BDS version)
  //  - older : {$LIBSUFFIX '160'} (quoted string literal)
  //This is NOT the same as the dproj's <DllSuffix>$(Auto)</DllSuffix> MSBuild macro.
  if compiler >= TCompilerVersion.Delphi10_4 then
    result := StringReplace(result, '{{LIBSUFFIX_DIRECTIVE}}', '{$LIBSUFFIX AUTO}', [rfReplaceAll])
  else
    result := StringReplace(result, '{{LIBSUFFIX_DIRECTIVE}}',
      '{$LIBSUFFIX ''' + CompilerToLibSuffix(compiler) + '''}', [rfReplaceAll]);

  //{$RUNONLY} / {$DESIGNONLY} bakes the package kind into the compiled BPL so the
  //IDE doesn't try to load a runtime package as design-time (and vice versa).
  case kind of
    pkDesign  : result := StringReplace(result, '{{KIND_DIRECTIVE}}', '{$DESIGNONLY}', [rfReplaceAll]);
  else //pkRuntime
    result := StringReplace(result, '{{KIND_DIRECTIVE}}', '{$RUNONLY}', [rfReplaceAll]);
  end;
  result := StringReplace(result, '{{REQUIRES_ENTRIES}}', requiresBlock, [rfReplaceAll]);
  result := StringReplace(result, '{{CONTAINS_ENTRIES}}', containsBlock, [rfReplaceAll]);
end;

function ChainStubFor(const platformName, cfg : string) : string;
begin
  //Establishes the Cfg_<n>_<Platform> child config so Delphi can hang per-platform
  //settings off it. Mirrors what the IDE emits when a platform is enabled.
  result :=
    '    <PropertyGroup Condition="(''$(Platform)''==''' + platformName +
      ''' and ''$(' + cfg + ')''==''true'') or ''$(' + cfg + '_' + platformName + ')''!=''''">'#13#10 +
    '        <' + cfg + '_' + platformName + '>true</' + cfg + '_' + platformName + '>'#13#10 +
    '        <CfgParent>' + cfg + '</CfgParent>'#13#10 +
    '        <' + cfg + '>true</' + cfg + '>'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10;
end;

function BuildChainStubs(const platforms : TDPMPlatforms; const cfg : string) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform); //sentinel - never present in a real platform set
var
  i : integer;
begin
  //iterate in a stable, IDE-friendly order rather than enum order so Win32/Win64
  //come first - matches what the Delphi IDE emits and keeps diffs readable.
  result := '';
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
      result := result + ChainStubFor(DPMPlatformToBDString(cOrder[i]), cfg);
end;

function BuildPlatformsList(const platforms : TDPMPlatforms) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform);
var
  i : integer;
begin
  result := '';
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
      result := result + '                <Platform value="' + DPMPlatformToBDString(cOrder[i]) + '">true</Platform>'#13#10;
end;

function PickDefaultPlatform(const platforms : TDPMPlatforms) : string;
const
  cOrder : array[0..14] of TDPMPlatform = (
    TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC,
    TDPMPlatform.MacOS32, TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64,
    TDPMPlatform.Android, TDPMPlatform.Android64,
    TDPMPlatform.iOS32, TDPMPlatform.iOS64, TDPMPlatform.iOSSimulator, TDPMPlatform.iOSSimARM64,
    TDPMPlatform.Linux64,
    TDPMPlatform.UnknownPlatform);
var
  i : integer;
begin
  //prefer Win32 since the IDE is Win32 and design-time loading expects it; fall
  //back to the first platform in canonical order if Win32 isn't supported.
  if TDPMPlatform.Win32 in platforms then
  begin
    result := 'Win32';
    exit;
  end;
  for i := 0 to High(cOrder) do
    if cOrder[i] in platforms then
    begin
      result := DPMPlatformToBDString(cOrder[i]);
      exit;
    end;
  result := 'Win32';
end;

class function TPrepareTemplates.RenderDproj(const packageId : string; const compiler : TCompilerVersion;
                                              const sourceFiles : IList<string>;
                                              const kind : TPrepareProjectKind;
                                              const platforms : TDPMPlatforms) : string;
var
  debugInfoRelease : string;
  projectGuid : string;
  newGuid : TGUID;
  references : string;
  packageKindFlag : string;
  effectivePlatforms : TDPMPlatforms;
  cfgDebugStubs : string;
  cfgReleaseStubs : string;
  platformOverrides : string;
  platformsList : string;
  i : integer;
begin
  //Release config gets debug info disabled (false / 0). Debug config relies on
  //DCC_DebugInfoInExe rather than DCC_DebugInformation, so we only need to set
  //the release value here. The transformer will normalize it for the target
  //compiler when this dproj is propagated.
  if CompilerUsesIntegerDebugInformation(compiler) then
    debugInfoRelease := '0'
  else
    debugInfoRelease := 'false';

  //GUIDToString returns the value wrapped in braces.
  CreateGUID(newGuid);
  projectGuid := GUIDToString(newGuid);

  references := '';
  if sourceFiles <> nil then
    for i := 0 to sourceFiles.Count - 1 do
      references := references + '        <DCCReference Include="' + sourceFiles[i] + '"/>'#13#10;

  //defensive: never emit a dproj with zero <Platform> entries - that would prevent
  //the IDE from loading it. Caller logs the fallback when platforms = [].
  effectivePlatforms := platforms;
  if effectivePlatforms = [] then
    effectivePlatforms := [TDPMPlatform.Win32];

  cfgDebugStubs := BuildChainStubs(effectivePlatforms, 'Cfg_1');
  cfgReleaseStubs := BuildChainStubs(effectivePlatforms, 'Cfg_2');
  platformsList := BuildPlatformsList(effectivePlatforms);

  //Only Win32 gets the DCC_RemoteDebug override (matches what the IDE emits).
  if TDPMPlatform.Win32 in effectivePlatforms then
    platformOverrides :=
      '    <PropertyGroup Condition="''$(Cfg_1_Win32)''!=''''">'#13#10 +
      '        <DCC_RemoteDebug>false</DCC_RemoteDebug>'#13#10 +
      '    </PropertyGroup>'#13#10
  else
    platformOverrides := '';

  //<DllSuffix> tells the IDE the BPL output filename has the libsuffix appended
  //(otherwise the IDE looks for a base-name BPL the compiler never produces).
  //The .dpk's {$LIBSUFFIX} directive still controls the actual compile-time suffix.

  case kind of
    pkDesign  : packageKindFlag := '        <DesignOnlyPackage>true</DesignOnlyPackage>'#13#10;
  else //pkRuntime
    packageKindFlag := '        <RuntimeOnlyPackage>true</RuntimeOnlyPackage>'#13#10;
  end;

  result := cDprojTemplate;
  result := StringReplace(result, '{{PACKAGE_ID}}', packageId, [rfReplaceAll]);
  result := StringReplace(result, '{{PROJECT_VERSION}}', CompilerVersionToProjectVersion(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{LIB_SUFFIX}}', CompilerToDefaultLibSuffix(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{DPM_COMPILER}}', CompilerToString(compiler), [rfReplaceAll]);
  result := StringReplace(result, '{{DEFAULT_PLATFORM}}', PickDefaultPlatform(effectivePlatforms), [rfReplaceAll]);
  result := StringReplace(result, '{{CFG_DEBUG_PLATFORM_GROUPS}}', cfgDebugStubs, [rfReplaceAll]);
  result := StringReplace(result, '{{CFG_RELEASE_PLATFORM_GROUPS}}', cfgReleaseStubs, [rfReplaceAll]);
  result := StringReplace(result, '{{PLATFORM_OVERRIDES}}', platformOverrides, [rfReplaceAll]);
  result := StringReplace(result, '{{PLATFORMS_LIST}}', platformsList, [rfReplaceAll]);
  result := StringReplace(result, '{{DEBUG_INFO_RELEASE}}', debugInfoRelease, [rfReplaceAll]);
  result := StringReplace(result, '{{GUID_PROJECT}}', projectGuid, [rfReplaceAll]);
  result := StringReplace(result, '{{DCC_REFERENCES}}', references, [rfReplaceAll]);
  result := StringReplace(result, '{{PACKAGE_KIND_FLAG}}', packageKindFlag, [rfReplaceAll]);
end;

end.
