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

unit DPM.Core.Tests.Project.Transformer;

interface

uses
  DPM.Core.Types,
  DPM.Core.Project.Transformer,
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TProjectTransformerTests = class
  private
    function LoadDprojFromString(const transformer : IProjectTransformer; const xml : string) : string;
    function SaveAndReadXml(const transformer : IProjectTransformer; const sandboxPath : string) : string;
    function MakeTempFile(const prefix, ext : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    [TestCase('XE2', '1,13.4')]
    [TestCase('XE5', '4,15.1')]
    [TestCase('10.4', '12,19.2')]
    [TestCase('11', '13,19.5')]
    [TestCase('12', '14,20.3')]
    procedure SetProjectVersion_WritesExpectedValue(const compilerOrdinal : integer; const expected : string);

    [Test]
    procedure SetLibSuffix_UsesAutoFor10_4_AndUp;

    [Test]
    procedure SetLibSuffix_UsesDigitsFor_PreXE2_through_10_3;

    [Test]
    procedure SetLibSuffix_DoesNotSynthesize_WhenMissing;

    [Test]
    procedure SetDebugInformation_BoolFalse_ToZero_OnXE5Plus;

    [Test]
    procedure SetDebugInformation_BoolTrue_ToTwo_OnXE5Plus;

    [Test]
    procedure SetDebugInformation_IntZero_ToFalse_OnPreXE5;

    [Test]
    procedure SetDebugInformation_IntTwo_ToTrue_OnPreXE5;

    [Test]
    procedure RemoveDeploymentBlocks_RemovesElements;

    [Test]
    procedure SetDPMCompiler_UpdatesValue_WhenPresent;

    [Test]
    procedure SetDPMCompiler_DoesNotAdd_WhenMissing;

    [Test]
    procedure DpkTransformer_RewritesLibSuffix_FromDigitsToAuto;

    [Test]
    procedure DpkTransformer_RewritesLibSuffix_FromAutoToDigits;

    [Test]
    procedure DpkTransformer_NoDirective_LeavesFileUnchanged;

    [Test]
    procedure DpkTransformer_CaseInsensitiveOnDirectiveKeyword;

    //the dpk is written back in its own encoding - one BOM if it had one, and a Windows-1252 file stays one.
    [Test]
    procedure DpkTransformer_Keeps_A_Single_BOM;
    [Test]
    procedure DpkTransformer_Keeps_A_Cp1252_File;

    //the projects prepare generates are valid whatever is in the source paths - an & in a dproj, a ' in a dpk.
    [Test]
    procedure Generated_Dproj_Escapes_The_Source_Paths;
    [Test]
    procedure Generated_Dpk_Doubles_A_Quote_In_A_Source_Path;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Variants,
  Winapi.msxml,
  Spring.Collections,
  DPM.Core.Project.Prepare.Templates,
  TestLogger;

const
  cMinimalDproj =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup>'#13#10 +
    '    <ProjectVersion>13.4</ProjectVersion>'#13#10 +
    '    <DPMCompiler>delphi.xe2</DPMCompiler>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '  <PropertyGroup Condition="''$(Base)''!=''''">'#13#10 +
    '    <DCC_LibSuffix>160</DCC_LibSuffix>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '  <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '    <DCC_DebugInformation>false</DCC_DebugInformation>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '  <ItemGroup>'#13#10 +
    '    <Deployment/>'#13#10 +
    '    <DeployFile Include="something.dll">'#13#10 +
    '      <Platform Name="Win32"/>'#13#10 +
    '    </DeployFile>'#13#10 +
    '    <DCCReference Include="rtl.dcp"/>'#13#10 +
    '  </ItemGroup>'#13#10 +
    '</Project>'#13#10;

  cDprojNoLibSuffix =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup>'#13#10 +
    '    <ProjectVersion>13.4</ProjectVersion>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '</Project>'#13#10;

  cDprojNoDpmCompiler =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup>'#13#10 +
    '    <ProjectVersion>13.4</ProjectVersion>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '</Project>'#13#10;

  cDprojDebugIntZero =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '    <DCC_DebugInformation>0</DCC_DebugInformation>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '</Project>'#13#10;

  cDprojDebugIntTwo =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '    <DCC_DebugInformation>2</DCC_DebugInformation>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '</Project>'#13#10;

  cDprojDebugBoolTrue =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '  <PropertyGroup Condition="''$(Cfg_2)''!=''''">'#13#10 +
    '    <DCC_DebugInformation>true</DCC_DebugInformation>'#13#10 +
    '  </PropertyGroup>'#13#10 +
    '</Project>'#13#10;

  cDpkWithDigits =
    'package MyPkg;'#13#10 +
    '{$LIBSUFFIX ''160''}'#13#10 +
    'requires rtl;'#13#10 +
    'end.'#13#10;

  cDpkWithAuto =
    'package MyPkg;'#13#10 +
    '{$LIBSUFFIX AUTO}'#13#10 +
    'requires rtl;'#13#10 +
    'end.'#13#10;

  cDpkNoDirective =
    'package MyPkg;'#13#10 +
    'requires rtl;'#13#10 +
    'end.'#13#10;

  cDpkMixedCase =
    'package MyPkg;'#13#10 +
    '{$LibSuffix ''160''}'#13#10 +
    'requires rtl;'#13#10 +
    'end.'#13#10;

{ TProjectTransformerTests }

procedure TProjectTransformerTests.DpkTransformer_Keeps_A_Single_BOM;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  bytes : TBytes;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  try
    //TEncoding.UTF8 writes a BOM.
    TFile.WriteAllText(inFile, cDpkWithDigits, TEncoding.UTF8);
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.Delphi10_4);
    bytes := TFile.ReadAllBytes(outFile);
    Assert.IsTrue((Length(bytes) > 6) and (bytes[0] = $EF) and (bytes[1] = $BB) and (bytes[2] = $BF), 'the BOM is kept');
    Assert.IsFalse((bytes[3] = $EF) and (bytes[4] = $BB) and (bytes[5] = $BF), 'only one BOM');
    Assert.Contains(TFile.ReadAllText(outFile, TEncoding.UTF8), '{$LIBSUFFIX AUTO}');
  finally
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

procedure TProjectTransformerTests.DpkTransformer_Keeps_A_Cp1252_File;
const
  cCopyright = #$00A9;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  cp1252 : TEncoding;
  bytes : TBytes;
  text : string;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  cp1252 := TEncoding.GetEncoding(1252);
  try
    //a (c) as the single byte $A9 - not valid UTF-8.
    TFile.WriteAllBytes(inFile, cp1252.GetBytes('// ' + cCopyright + ' 2019 VSoft' + #13#10 + cDpkWithDigits));
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.Delphi10_4);
    bytes := TFile.ReadAllBytes(outFile);
    Assert.AreEqual(Integer($A9), Integer(bytes[3]), 'the (c) is still the one byte it was');
    text := cp1252.GetString(bytes);
    Assert.Contains(text, '// ' + cCopyright + ' 2019 VSoft');
    Assert.Contains(text, '{$LIBSUFFIX AUTO}');
  finally
    cp1252.Free;
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

procedure TProjectTransformerTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TProjectTransformerTests.FixtureTearDown;
begin
  CoUninitialize;
end;

function TProjectTransformerTests.MakeTempFile(const prefix, ext : string) : string;
begin
  result := TPath.Combine(TPath.GetTempPath, prefix + TGuid.NewGuid.ToString + ext);
end;

function TProjectTransformerTests.LoadDprojFromString(const transformer : IProjectTransformer; const xml : string) : string;
begin
  result := MakeTempFile('dpmprep_', '.dproj');
  TFile.WriteAllText(result, xml, TEncoding.UTF8);
  Assert.IsTrue(transformer.LoadFromFile(result), 'LoadFromFile should succeed');
end;

function TProjectTransformerTests.SaveAndReadXml(const transformer : IProjectTransformer; const sandboxPath : string) : string;
begin
  Assert.IsTrue(transformer.SaveToFile(sandboxPath), 'SaveToFile should succeed');
  result := TFile.ReadAllText(sandboxPath, TEncoding.UTF8);
end;

procedure TProjectTransformerTests.SetProjectVersion_WritesExpectedValue(const compilerOrdinal : integer; const expected : string);
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
  compiler : TCompilerVersion;
begin
  compiler := TCompilerVersion(compilerOrdinal);
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.SetProjectVersion(compiler);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<ProjectVersion>' + expected + '</ProjectVersion>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetLibSuffix_UsesAutoFor10_4_AndUp;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.SetLibSuffix(TCompilerVersion.Delphi10_4);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_LibSuffix>$(Auto)</DCC_LibSuffix>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetLibSuffix_UsesDigitsFor_PreXE2_through_10_3;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.SetLibSuffix(TCompilerVersion.Delphi10_3);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_LibSuffix>260</DCC_LibSuffix>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetLibSuffix_DoesNotSynthesize_WhenMissing;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cDprojNoLibSuffix);
  try
    transformer.SetLibSuffix(TCompilerVersion.Delphi10_4);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.IsFalse(Pos('<DCC_LibSuffix>', xml) > 0, 'DCC_LibSuffix should not be synthesized');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDebugInformation_BoolFalse_ToZero_OnXE5Plus;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.SetDebugInformation(TCompilerVersion.DelphiXE5);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>0</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDebugInformation_BoolTrue_ToTwo_OnXE5Plus;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cDprojDebugBoolTrue);
  try
    transformer.SetDebugInformation(TCompilerVersion.DelphiXE5);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>2</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDebugInformation_IntZero_ToFalse_OnPreXE5;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cDprojDebugIntZero);
  try
    transformer.SetDebugInformation(TCompilerVersion.DelphiXE2);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>false</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDebugInformation_IntTwo_ToTrue_OnPreXE5;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cDprojDebugIntTwo);
  try
    transformer.SetDebugInformation(TCompilerVersion.DelphiXE2);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DCC_DebugInformation>true</DCC_DebugInformation>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.RemoveDeploymentBlocks_RemovesElements;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.RemoveDeploymentBlocks;
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.IsFalse(Pos('<Deployment', xml) > 0, 'Deployment elements should be removed');
    Assert.IsFalse(Pos('<DeployFile', xml) > 0, 'DeployFile elements should be removed');
    //the non-Deployment item in the same ItemGroup should still be present.
    Assert.Contains(xml, '<DCCReference Include="rtl.dcp"');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDPMCompiler_UpdatesValue_WhenPresent;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cMinimalDproj);
  try
    transformer.SetDPMCompiler(TCompilerVersion.Delphi11_0);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.Contains(xml, '<DPMCompiler>delphi11.0</DPMCompiler>');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.SetDPMCompiler_DoesNotAdd_WhenMissing;
var
  transformer : IProjectTransformer;
  sandbox : string;
  xml : string;
begin
  transformer := TProjectTransformer.Create(TTestLogger.Create);
  sandbox := LoadDprojFromString(transformer, cDprojNoDpmCompiler);
  try
    transformer.SetDPMCompiler(TCompilerVersion.Delphi11_0);
    xml := SaveAndReadXml(transformer, sandbox);
    Assert.IsFalse(Pos('<DPMCompiler>', xml) > 0, 'DPMCompiler should not be synthesized');
  finally
    if FileExists(sandbox) then
      TFile.Delete(sandbox);
  end;
end;

procedure TProjectTransformerTests.DpkTransformer_RewritesLibSuffix_FromDigitsToAuto;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  contents : string;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  try
    TFile.WriteAllText(inFile, cDpkWithDigits, TEncoding.UTF8);
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.Delphi10_4);
    contents := TFile.ReadAllText(outFile, TEncoding.UTF8);
    //10.4+ uses the bare AUTO keyword, not a quoted MSBuild-style $(Auto) macro.
    Assert.Contains(contents, '{$LIBSUFFIX AUTO}');
  finally
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

procedure TProjectTransformerTests.DpkTransformer_RewritesLibSuffix_FromAutoToDigits;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  contents : string;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  try
    TFile.WriteAllText(inFile, cDpkWithAuto, TEncoding.UTF8);
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.DelphiXE2);
    contents := TFile.ReadAllText(outFile, TEncoding.UTF8);
    Assert.Contains(contents, '{$LIBSUFFIX ''160''}');
  finally
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

procedure TProjectTransformerTests.DpkTransformer_NoDirective_LeavesFileUnchanged;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  contents : string;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  try
    TFile.WriteAllText(inFile, cDpkNoDirective, TEncoding.UTF8);
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.Delphi10_4);
    contents := TFile.ReadAllText(outFile, TEncoding.UTF8);
    Assert.IsFalse(Pos('LIBSUFFIX', UpperCase(contents)) > 0, 'No directive should have been added');
    Assert.Contains(contents, 'package MyPkg;');
  finally
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

procedure TProjectTransformerTests.DpkTransformer_CaseInsensitiveOnDirectiveKeyword;
var
  dpk : IDpkTransformer;
  inFile, outFile : string;
  contents : string;
begin
  dpk := TDpkTransformer.Create(TTestLogger.Create);
  inFile := MakeTempFile('dpmpkg_', '.dpk');
  outFile := MakeTempFile('dpmpkg_', '.dpk');
  try
    TFile.WriteAllText(inFile, cDpkMixedCase, TEncoding.UTF8);
    dpk.RewriteLibSuffix(inFile, outFile, TCompilerVersion.Delphi10_4);
    contents := TFile.ReadAllText(outFile, TEncoding.UTF8);
    //the directive keyword's case is preserved (we only rewrite the value).
    Assert.Contains(contents, '{$LibSuffix AUTO}');
  finally
    if FileExists(inFile) then
      TFile.Delete(inFile);
    if FileExists(outFile) then
      TFile.Delete(outFile);
  end;
end;

//one source file, in a folder with the name given.
function SourceIn(const folder : string) : IList<TPrepareSourceFile>;
var
  entry : TPrepareSourceFile;
begin
  result := TCollections.CreateList<TPrepareSourceFile>;
  entry.Path := folder + '\Unit1.pas';
  entry.FormName := '';
  result.Add(entry);
end;

procedure TProjectTransformerTests.Generated_Dproj_Escapes_The_Source_Paths;
var
  dproj : string;
  doc : IXMLDOMDocument;
begin
  dproj := TPrepareTemplates.RenderDproj('MyPkg', TCompilerVersion.Delphi12_0, SourceIn('..\R&D'), nil, pkRuntime, [TDPMPlatform.Win32]);
  Assert.Contains(dproj, 'Include="..\R&amp;D\Unit1.pas"');
  doc := CoDOMDocument60.Create;
  Assert.IsTrue(doc.loadXML(dproj), 'the generated project is XML : ' + VarToStr(doc.parseError.reason));
end;

procedure TProjectTransformerTests.Generated_Dpk_Doubles_A_Quote_In_A_Source_Path;
var
  dpk : string;
begin
  dpk := TPrepareTemplates.RenderDpk('MyPkg', TCompilerVersion.Delphi12_0, SourceIn('..\O''Brien'), nil, pkRuntime);
  Assert.Contains(dpk, 'Unit1 in ''..\O''''Brien\Unit1.pas''');
end;

initialization
  TDUnitX.RegisterTestFixture(TProjectTransformerTests);

end.
