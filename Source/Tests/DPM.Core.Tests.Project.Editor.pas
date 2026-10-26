{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Tests.Project.Editor;

//Install/uninstall rewrite the consumer's dproj - which the IDE wrote and will write again. These
//tests pin down that DPM only changes the elements it owns: the byte order mark, the IDE's entity
//forms (&apos; / &quot; in text) and every bit of whitespace outside DPM's elements must survive.

interface

uses
  System.SysUtils,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Project.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TProjectEditorFormattingTests = class
  private
    function MakeTempFile : string;
    procedure WriteProjectFile(const fileName : string; const content : string; const withBOM : boolean);
    function FileHasBOM(const fileName : string) : boolean;
    function ReadProjectFile(const fileName : string) : string;
    function CreateEditor : IProjectEditor;
    function InstalledGraph : IPackageReference;
    function SearchPaths(const ids : array of string) : IList<string>;
    procedure AssertSameText(const expected : string; const actual : string);
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure SaveAs_Unmodified_WithBOM_IsByteIdentical;

    [Test]
    procedure SaveAs_Unmodified_WithoutBOM_IsByteIdentical;

    [Test]
    procedure SaveAs_Unmodified_LFLineBreaks_IsByteIdentical;

    [Test]
    procedure Install_LFLineBreaks_StaysLF;

    [Test]
    procedure Install_PreservesBOM;

    [Test]
    procedure Install_WithoutBOM_DoesNotAddOne;

    [Test]
    procedure Install_OnlyChangesDPMElements;

    //earlier versions wrote the restoring machine's dpm.exe path into the project - it goes, cleanly.
    [Test]
    procedure Install_RemovesARecordedDPMExe;

    [Test]
    procedure Uninstall_RemovesReferenceWithoutLeavingBlankLines;

    [Test]
    procedure UninstallAll_CollapsesPackagesElement;

    //the search path is ';' separated - a space in another package's path is part of the path.
    [Test]
    procedure Uninstall_KeepsAnotherPackagesPathWithASpace;

    [Test]
    procedure Install_TabIndentedProject_IndentsNewElementsWithTabs;
  end;

implementation

uses
  Winapi.ActiveX,
  System.Classes,
  System.IOUtils,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Reference,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Classes,
  DPM.Core.Project.Editor,
  DPM.Core.Project.CopyLocalTargets,
  TestLogger;

const
  CRLF = #13#10;

  cBaseSearchPath = '.\Source;$(DCC_UnitSearchPath)';

  //Shaped like an IDE written dproj, plus the things a DOM round trip loses - &apos; and &quot;
  //in text, quotes in attributes and CDATA (which must NOT be escaped), a comment, a blank line
  //and a slightly off indent the IDE would never write but a person might.
  cProjectHead =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' + CRLF +
    '    <PropertyGroup>' + CRLF +
    '        <ProjectGuid>{8A4B3C2D-1E0F-4A5B-9C8D-7E6F5A4B3C2D}</ProjectGuid>' + CRLF +
    '        <MainSource>Foo.dpr</MainSource>' + CRLF +
    '        <ProjectVersion>20.3</ProjectVersion>' + CRLF +
    '        <Base>True</Base>' + CRLF +
    '        <Config Condition="''$(Config)''==''''">Debug</Config>' + CRLF +
    '        <Platform Condition="''$(Platform)''==''''">Win32</Platform>' + CRLF +
    '        <AppType>Console</AppType>' + CRLF +
    '    </PropertyGroup>' + CRLF;

  cProjectBaseGroups =
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">' + CRLF +
    '        <Base>true</Base>' + CRLF +
    '    </PropertyGroup>' + CRLF +
    '    <PropertyGroup Condition="''$(Base)''!=''''">' + CRLF +
    '        <DCC_UnitSearchPath>%s</DCC_UnitSearchPath>' + CRLF +
    '        <VerInfo_Keys>CompanyName=;NSSpeechRecognitionUsageDescription=Apple&apos;s servers</VerInfo_Keys>' + CRLF +
    '        <Custom_Styles>&quot;Windows11 MineShaft|VCLSTYLE|$(BDSCOMMONDIR)\Styles\x.vsf&quot;</Custom_Styles>' + CRLF +
    '        <DCC_Define>A&amp;B;a&gt;b;$(DCC_Define)</DCC_Define>' + CRLF +
    '        <PostBuildEvent><![CDATA[copy "a.txt" ''b.txt'']]></PostBuildEvent>' + CRLF +
    '    </PropertyGroup>' + CRLF +
    CRLF +
    '    <!-- items -->' + CRLF +
    '    <ItemGroup>' + CRLF +
    '        <DelphiCompile Include="$(MainSource)">' + CRLF +
    '            <MainSource>MainSource</MainSource>' + CRLF +
    '        </DelphiCompile>' + CRLF +
    '          <DCCReference Include="Unit1.pas"/>' + CRLF +
    '        <BuildConfiguration Include="Base">' + CRLF +
    '            <Key>Base</Key>' + CRLF +
    '        </BuildConfiguration>' + CRLF +
    '        <BuildConfiguration Include="Debug">' + CRLF +
    '            <Key>Cfg_1</Key>' + CRLF +
    '            <CfgParent>Base</CfgParent>' + CRLF +
    '        </BuildConfiguration>' + CRLF +
    '    </ItemGroup>' + CRLF +
    '    <ProjectExtensions>' + CRLF +
    '        <Borland.Personality>Delphi.Personality.12</Borland.Personality>' + CRLF +
    '        <Borland.ProjectType>Application</Borland.ProjectType>' + CRLF +
    '        <BorlandProject>' + CRLF +
    '            <Delphi.Personality>' + CRLF +
    '                <Source>' + CRLF +
    '                    <Source Name="MainSource">Foo.dpr</Source>' + CRLF +
    '                </Source>' + CRLF +
    '            </Delphi.Personality>' + CRLF +
    '            <Platforms>' + CRLF +
    '                <Platform value="Win32">True</Platform>' + CRLF +
    '            </Platforms>' + CRLF +
    '        </BorlandProject>' + CRLF +
    '        <ProjectFileVersion>12</ProjectFileVersion>' + CRLF;

  cProjectExtensionsTail =
    '    </ProjectExtensions>' + CRLF +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets" Condition="Exists(''$(BDS)\Bin\CodeGear.Delphi.Targets'')"/>' + CRLF;

  cProjectClose =
    '</Project>' + CRLF;

  cCopyLocalImport =
    '    <Import Project="' + cCopyLocalImportProject + '" Condition="Exists(''' + cCopyLocalImportProject + ''')"/>' + CRLF;

  cDPMCache = 'c:\dpmcache';

  //what DPM adds on install, at the indent the IDE uses.
  cDPMGroupFmt =
    '    <PropertyGroup>' + CRLF +
    '        <DPMCompiler>%s</DPMCompiler>' + CRLF +
    '        <DPMCache Condition="''$(DPMCache)'' == ''''">' + cDPMCache + '</DPMCache>' + CRLF +
    '        <DPM>$(DPMCache)\$(DPMCompiler)</DPM>' + CRLF +
    '        <DPMSearch Condition="''$(Platform)''==''Win32''">%s</DPMSearch>' + CRLF +
    '    </PropertyGroup>' + CRLF;

  cPackagesHead =
    '        <DPM>' + CRLF +
    '            <Packages>' + CRLF;

  cFooReference =
    '                <PackageReference id="Foo" version="1.0.0">' + CRLF +
    '                    <PackageReference id="Bar" version="2.0.0" range="[2.0.0,]"/>' + CRLF +
    '                </PackageReference>' + CRLF;

  cBazReference =
    '                <PackageReference id="Baz" version="3.1.0"/>' + CRLF;

  cPackagesTail =
    '            </Packages>' + CRLF +
    '        </DPM>' + CRLF;

function BuildProject(const dpmGroup : string; const searchPath : string; const dpmExtension : string; const dpmImport : string) : string;
begin
  result := cProjectHead + dpmGroup + Format(cProjectBaseGroups, [searchPath]) + dpmExtension +
            cProjectExtensionsTail + dpmImport + cProjectClose;
end;

function OriginalProject : string;
begin
  result := BuildProject('', cBaseSearchPath, '', '');
end;

function DPMGroup(const searchPath : string) : string;
begin
  result := Format(cDPMGroupFmt, [CompilerToString(TCompilerVersion.Delphi12_0), searchPath]);
end;

function InstalledProject : string;
begin
  result := BuildProject(DPMGroup('$(DPM)\Foo\1.0.0\lib\Win32;$(DPM)\Bar\2.0.0\lib\Win32;$(DPM)\Baz\3.1.0\lib\Win32;'),
                         '$(DPMSearch);' + cBaseSearchPath,
                         //references are written in id order.
                         cPackagesHead + cBazReference + cFooReference + cPackagesTail,
                         cCopyLocalImport);
end;

//CR/LF/tab made visible so a whitespace mismatch is readable in the failure message.
function Visible(const value : string) : string;
begin
  result := StringReplace(value, #13, '\r', [rfReplaceAll]);
  result := StringReplace(result, #10, '\n', [rfReplaceAll]);
  result := StringReplace(result, #9, '\t', [rfReplaceAll]);
end;

{ TProjectEditorFormattingTests }

procedure TProjectEditorFormattingTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TProjectEditorFormattingTests.FixtureTearDown;
begin
  CoUninitialize;
end;

function TProjectEditorFormattingTests.MakeTempFile : string;
begin
  result := TPath.Combine(TPath.GetTempPath, 'dpmeditor_' + TGuid.NewGuid.ToString + '.dproj');
end;

procedure TProjectEditorFormattingTests.WriteProjectFile(const fileName : string; const content : string; const withBOM : boolean);
var
  bytes : TBytes;
  preamble : TBytes;
  fs : TFileStream;
begin
  bytes := TEncoding.UTF8.GetBytes(content);
  preamble := TEncoding.UTF8.GetPreamble;
  fs := TFileStream.Create(fileName, fmCreate);
  try
    if withBOM then
      fs.WriteBuffer(preamble[0], Length(preamble));
    fs.WriteBuffer(bytes[0], Length(bytes));
  finally
    fs.Free;
  end;
end;

function TProjectEditorFormattingTests.FileHasBOM(const fileName : string) : boolean;
var
  bytes : TBytes;
begin
  bytes := TFile.ReadAllBytes(fileName);
  result := (Length(bytes) >= 3) and (bytes[0] = $EF) and (bytes[1] = $BB) and (bytes[2] = $BF);
end;

//decodes the raw bytes - the BOM (if any) is checked separately and left out of the text.
function TProjectEditorFormattingTests.ReadProjectFile(const fileName : string) : string;
var
  bytes : TBytes;
  offset : integer;
begin
  bytes := TFile.ReadAllBytes(fileName);
  offset := 0;
  if FileHasBOM(fileName) then
    offset := 3;
  result := TEncoding.UTF8.GetString(bytes, offset, Length(bytes) - offset);
end;

procedure TProjectEditorFormattingTests.AssertSameText(const expected : string; const actual : string);
var
  i : integer;
  firstDiff : integer;
  snippetStart : integer;
begin
  if expected = actual then
    exit;
  firstDiff := 0;
  for i := 1 to Length(expected) do
  begin
    if (i > Length(actual)) or (expected[i] <> actual[i]) then
    begin
      firstDiff := i;
      break;
    end;
  end;
  if firstDiff = 0 then
    firstDiff := Length(expected) + 1;
  snippetStart := firstDiff - 40;
  if snippetStart < 1 then
    snippetStart := 1;
  Assert.Fail('Project text differs at char ' + IntToStr(firstDiff) + sLineBreak +
              'expected : ' + Visible(Copy(expected, snippetStart, 120)) + sLineBreak +
              'actual   : ' + Visible(Copy(actual, snippetStart, 120)));
end;

function TProjectEditorFormattingTests.CreateEditor : IProjectEditor;
var
  config : IConfiguration;
begin
  config := TConfiguration.Create(TTestLogger.Create);
  config.PackageCacheLocation := cDPMCache;
  result := TProjectEditor.Create(TTestLogger.Create, config, TCompilerVersion.Delphi12_0);
end;

function TProjectEditorFormattingTests.InstalledGraph : IPackageReference;
var
  foo : IPackageReference;
begin
  result := TPackageReference.CreateRoot(TCompilerVersion.Delphi12_0);
  foo := result.AddChild('Foo', TPackageVersion.Parse('1.0.0'), TVersionRange.Empty);
  foo.AddChild('Bar', TPackageVersion.Parse('2.0.0'), TVersionRange.Parse('[2.0.0,]'));
  result.AddChild('Baz', TPackageVersion.Parse('3.1.0'), TVersionRange.Empty);
end;

function TProjectEditorFormattingTests.SearchPaths(const ids : array of string) : IList<string>;
var
  i : integer;
begin
  result := TCollections.CreateList<string>;
  for i := 0 to Length(ids) - 1 do
    result.Add(ids[i] + '\lib\Win32');
end;

procedure TProjectEditorFormattingTests.SaveAs_Unmodified_WithBOM_IsByteIdentical;
var
  editor : IProjectEditor;
  source : string;
  target : string;
begin
  source := MakeTempFile;
  target := MakeTempFile;
  WriteProjectFile(source, OriginalProject, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(source), 'LoadProject should succeed');
    //an explicit filename always writes, so this is a pure load/save round trip.
    Assert.IsTrue(editor.SaveProject(target), 'SaveProject should succeed');
    Assert.IsTrue(FileHasBOM(target), 'byte order mark was dropped');
    AssertSameText(OriginalProject, ReadProjectFile(target));
  finally
    TFile.Delete(source);
    if FileExists(target) then
      TFile.Delete(target);
  end;
end;

procedure TProjectEditorFormattingTests.SaveAs_Unmodified_WithoutBOM_IsByteIdentical;
var
  editor : IProjectEditor;
  source : string;
  target : string;
begin
  source := MakeTempFile;
  target := MakeTempFile;
  WriteProjectFile(source, OriginalProject, false);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(source), 'LoadProject should succeed');
    Assert.IsTrue(editor.SaveProject(target), 'SaveProject should succeed');
    Assert.IsFalse(FileHasBOM(target), 'a byte order mark was added');
    AssertSameText(OriginalProject, ReadProjectFile(target));
  finally
    TFile.Delete(source);
    if FileExists(target) then
      TFile.Delete(target);
  end;
end;

//eg a dproj checked out with core.autocrlf=input - MSXML writes every line break as CRLF.
procedure TProjectEditorFormattingTests.SaveAs_Unmodified_LFLineBreaks_IsByteIdentical;
var
  editor : IProjectEditor;
  source : string;
  target : string;
  original : string;
begin
  source := MakeTempFile;
  target := MakeTempFile;
  original := StringReplace(OriginalProject, CRLF, #10, [rfReplaceAll]);
  WriteProjectFile(source, original, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(source), 'LoadProject should succeed');
    Assert.IsTrue(editor.SaveProject(target), 'SaveProject should succeed');
    AssertSameText(original, ReadProjectFile(target));
  finally
    TFile.Delete(source);
    if FileExists(target) then
      TFile.Delete(target);
  end;
end;

procedure TProjectEditorFormattingTests.Install_LFLineBreaks_StaysLF;
var
  editor : IProjectEditor;
  sandbox : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, StringReplace(OriginalProject, CRLF, #10, [rfReplaceAll]), true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    Assert.IsTrue(editor.EnsureCopyLocalImport, 'EnsureCopyLocalImport should succeed');
    Assert.IsTrue(editor.AddSearchPaths(TDPMPlatform.Win32, SearchPaths(['Foo\1.0.0', 'Bar\2.0.0', 'Baz\3.1.0']), cDPMCache), 'AddSearchPaths should succeed');
    editor.UpdatePackageReferences(InstalledGraph);
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');
    AssertSameText(StringReplace(InstalledProject, CRLF, #10, [rfReplaceAll]), ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Install_PreservesBOM;
var
  editor : IProjectEditor;
  sandbox : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, OriginalProject, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    Assert.IsTrue(editor.AddSearchPaths(TDPMPlatform.Win32, SearchPaths(['Foo\1.0.0']), cDPMCache), 'AddSearchPaths should succeed');
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');
    Assert.IsTrue(FileHasBOM(sandbox), 'byte order mark was dropped');
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Install_WithoutBOM_DoesNotAddOne;
var
  editor : IProjectEditor;
  sandbox : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, OriginalProject, false);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    Assert.IsTrue(editor.AddSearchPaths(TDPMPlatform.Win32, SearchPaths(['Foo\1.0.0']), cDPMCache), 'AddSearchPaths should succeed');
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');
    Assert.IsFalse(FileHasBOM(sandbox), 'a byte order mark was added');
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Install_OnlyChangesDPMElements;
var
  editor : IProjectEditor;
  sandbox : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, OriginalProject, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    //the calls the installer makes, in its order.
    Assert.IsTrue(editor.EnsureCopyLocalImport, 'EnsureCopyLocalImport should succeed');
    Assert.IsTrue(editor.AddSearchPaths(TDPMPlatform.Win32, SearchPaths(['Foo\1.0.0', 'Bar\2.0.0', 'Baz\3.1.0']), cDPMCache), 'AddSearchPaths should succeed');
    editor.UpdatePackageReferences(InstalledGraph);
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');
    AssertSameText(InstalledProject, ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Install_RemovesARecordedDPMExe;
var
  editor : IProjectEditor;
  sandbox : string;
  withDPMExe : string;
begin
  //where it was written - the end of the DPM property group.
  withDPMExe := StringReplace(InstalledProject, '</DPMSearch>' + CRLF,
                              '</DPMSearch>' + CRLF + '        <DPMExe>c:\somebody\else\dpm.exe</DPMExe>' + CRLF, []);
  Assert.IsTrue(withDPMExe <> InstalledProject, 'precondition : the test project records a dpm.exe');
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, withDPMExe, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    Assert.IsTrue(editor.EnsureCopyLocalImport, 'EnsureCopyLocalImport should succeed');
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');
    AssertSameText(InstalledProject, ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Uninstall_RemovesReferenceWithoutLeavingBlankLines;
var
  editor : IProjectEditor;
  sandbox : string;
  graph : IPackageReference;
  expected : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, InstalledProject, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    //what the uninstaller does for Foo - its search paths (and its transitive Bar) go, Baz stays.
    editor.RemoveFromSearchPath(TDPMPlatform.Win32, 'Foo');
    editor.RemoveFromSearchPath(TDPMPlatform.Win32, 'Bar');
    graph := TPackageReference.CreateRoot(TCompilerVersion.Delphi12_0);
    graph.AddChild('Baz', TPackageVersion.Parse('3.1.0'), TVersionRange.Empty);
    editor.UpdatePackageReferences(graph);
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');

    expected := BuildProject(DPMGroup('$(DPM)\Baz\3.1.0\lib\Win32'),
                             '$(DPMSearch);' + cBaseSearchPath,
                             cPackagesHead + cBazReference + cPackagesTail,
                             cCopyLocalImport);
    AssertSameText(expected, ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Uninstall_KeepsAnotherPackagesPathWithASpace;
var
  editor : IProjectEditor;
  sandbox : string;
  rest : string;
  expected : string;
begin
  rest := cPackagesHead + cBazReference + cFooReference + cPackagesTail;
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, BuildProject(DPMGroup('$(DPM)\Foo\1.0.0\lib\Win32;$(DPM)\Baz\3.1.0\src\Some Folder;'),
                                         '$(DPMSearch);' + cBaseSearchPath, rest, cCopyLocalImport), true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    editor.RemoveFromSearchPath(TDPMPlatform.Win32, 'Foo');
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');

    expected := BuildProject(DPMGroup('$(DPM)\Baz\3.1.0\src\Some Folder'), '$(DPMSearch);' + cBaseSearchPath, rest, cCopyLocalImport);
    AssertSameText(expected, ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.UninstallAll_CollapsesPackagesElement;
var
  editor : IProjectEditor;
  sandbox : string;
  expected : string;
begin
  sandbox := MakeTempFile;
  WriteProjectFile(sandbox, InstalledProject, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    editor.UpdatePackageReferences(TPackageReference.CreateRoot(TCompilerVersion.Delphi12_0));
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');

    expected := BuildProject(DPMGroup('$(DPM)\Foo\1.0.0\lib\Win32;$(DPM)\Bar\2.0.0\lib\Win32;$(DPM)\Baz\3.1.0\lib\Win32;'),
                             '$(DPMSearch);' + cBaseSearchPath,
                             '        <DPM>' + CRLF +
                             '            <Packages/>' + CRLF +
                             '        </DPM>' + CRLF,
                             cCopyLocalImport);
    AssertSameText(expected, ReadProjectFile(sandbox));
  finally
    TFile.Delete(sandbox);
  end;
end;

procedure TProjectEditorFormattingTests.Install_TabIndentedProject_IndentsNewElementsWithTabs;
var
  editor : IProjectEditor;
  sandbox : string;
  original : string;
  actual : string;
begin
  sandbox := MakeTempFile;
  //same project, one tab per level instead of four spaces.
  original := StringReplace(OriginalProject, '    ', #9, [rfReplaceAll]);
  WriteProjectFile(sandbox, original, true);
  try
    editor := CreateEditor;
    Assert.IsTrue(editor.LoadProject(sandbox), 'LoadProject should succeed');
    Assert.IsTrue(editor.AddSearchPaths(TDPMPlatform.Win32, SearchPaths(['Foo\1.0.0']), cDPMCache), 'AddSearchPaths should succeed');
    editor.UpdatePackageReferences(InstalledGraph);
    Assert.IsTrue(editor.SaveProject, 'SaveProject should succeed');

    actual := ReadProjectFile(sandbox);
    Assert.Contains(actual, #9'</PropertyGroup>' + CRLF + #9'<PropertyGroup>' + CRLF + #9#9'<DPMCompiler>', false);
    Assert.Contains(actual, #9#9'<ProjectFileVersion>12</ProjectFileVersion>' + CRLF +
                            #9#9'<DPM>' + CRLF +
                            #9#9#9'<Packages>' + CRLF +
                            #9#9#9#9'<PackageReference id="Baz" version="3.1.0"/>' + CRLF +
                            #9#9#9#9'<PackageReference id="Foo" version="1.0.0">' + CRLF +
                            #9#9#9#9#9'<PackageReference id="Bar"', false);
    Assert.DoesNotContain(actual, '    ', false);
  finally
    TFile.Delete(sandbox);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TProjectEditorFormattingTests);

end.
