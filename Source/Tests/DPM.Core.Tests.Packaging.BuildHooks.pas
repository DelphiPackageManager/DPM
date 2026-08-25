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

unit DPM.Core.Tests.Packaging.BuildHooks;

// Pack time half of the build hook check. An author who ships a dproj that would run something
// during install is told at pack time, when they can still do something about it, rather than
// leaving it for every consumer to hit. See DPM.Core.Project.BuildHookValidator.

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TBuildHookPackTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Pack_Succeeds_When_Build_Project_Is_Clean;
    procedure Pack_Fails_When_Build_Project_Declares_Target;
    procedure Pack_Fails_When_Build_Project_Declares_UsingTask;
    procedure Pack_Fails_When_Build_Project_Imports_Unknown_Targets;
    procedure Pack_Fails_When_Build_Project_Sets_PostBuildEvent;
    procedure Pack_Fails_When_Design_Project_Declares_Target;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  VSoft.CancellationToken,
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Manifest,
  DPM.Core.Package.Archive,
  DPM.Core.Packaging,
  DPM.Core.Packaging.Archive,
  DPM.Core.Packaging.Writer,
  DPM.Core.Packaging.Archive.Writer,
  DPM.Core.Options.Pack;

var
  GWorkDirCounter : integer = 0;

const
  cCleanDproj =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '        <AppType>Package</AppType>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '    <Import Project="$(MSBuildProjectName).deployproj"/>'#13#10 +
    '</Project>'#13#10;

  cDprojWithTarget =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <Target Name="Mischief" AfterTargets="Build">'#13#10 +
    '        <Exec Command="calc.exe"/>'#13#10 +
    '    </Target>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '</Project>'#13#10;

  cDprojWithUsingTask =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <UsingTask TaskName="Payload" AssemblyFile="payload.dll"/>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '</Project>'#13#10;

  cDprojWithUnknownImport =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '    <Import Project="build\extra.targets"/>'#13#10 +
    '</Project>'#13#10;

  cDprojWithPostBuildEvent =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '        <PostBuildEvent>copy /Y .\readme.txt ..\Output</PostBuildEvent>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '</Project>'#13#10;

{ helpers }

function NewWriter : IPackageWriter;
var
  logger : ILogger;
  hashing : IHashingService;
  manifestSvc : IManifestService;
  validator : IArchiveValidator;
  archiveWriter : IPackageArchiveWriter;
  specReader : IPackageSpecReader;
begin
  logger := TTestLogger.Create;
  hashing := TBCryptHashingService.Create;
  manifestSvc := TManifestService.Create(hashing);
  validator := TArchiveValidator.Create(manifestSvc);
  archiveWriter := TPackageArchiveWriter.Create(logger);
  specReader := TPackageSpecReader.Create(logger);
  result := TPackageWriter.Create(logger, archiveWriter, specReader, manifestSvc, validator);
end;

function MakeWorkDir : string;
begin
  Inc(GWorkDirCounter);
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-pack-hooks-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '-' + IntToStr(GWorkDirCounter));
  TDirectory.CreateDirectory(result);
  TDirectory.CreateDirectory(TPath.Combine(result, 'source'));
end;

procedure WriteProjectFile(const workDir, name, xml : string);
begin
  TFile.WriteAllText(TPath.Combine(TPath.Combine(workDir, 'source'), name), xml, TEncoding.UTF8);
end;

// Writes a dspec whose template ships everything under .\source and names the given projects as a
// build entry / design entry. Pass '' for designProject to leave the design section out.
procedure WriteDspec(const workDir, buildProject, designProject : string);
var
  yaml : TStringList;
begin
  yaml := TStringList.Create;
  try
    yaml.Add('metadata:');
    yaml.Add('  id: Test.Hooks');
    yaml.Add('  version: 1.0.0');
    yaml.Add('  description: build hook pack test');
    yaml.Add('  authors:');
    yaml.Add('    - Vincent Parrett');
    yaml.Add('  license: Apache-2.0');
    yaml.Add('targetPlatforms:');
    yaml.Add('  - compiler: 12.0');
    yaml.Add('    platforms: [Win32]');
    yaml.Add('    template: default');
    yaml.Add('templates:');
    yaml.Add('  - name: default');
    yaml.Add('    source:');
    yaml.Add('      - src: .\source\*.dproj');
    yaml.Add('        dest: source');
    yaml.Add('    build:');
    yaml.Add('      - project: .\source\' + buildProject);
    if designProject <> '' then
    begin
      yaml.Add('    design:');
      yaml.Add('      - project: .\source\' + designProject);
    end;
    yaml.WriteBOM := false;
    yaml.SaveToFile(TPath.Combine(workDir, 'test.dspec.yaml'), TEncoding.UTF8);
  finally
    yaml.Free;
  end;
end;

// Packs workDir. Unlike the other pack fixtures this deliberately lets an exception escape - a
// rejected build hook is reported the same way an uncovered build entry is, by raising.
function Pack(const workDir : string) : boolean;
var
  writer : IPackageWriter;
  options : TPackOptions;
  token : ICancellationToken;
begin
  writer := NewWriter;
  options := TPackOptions.Create;
  try
    options.SpecFile := TPath.Combine(workDir, 'test.dspec.yaml');
    options.BasePath := workDir;
    options.OutputFolder := workDir;
    token := TCancellationTokenSourceFactory.Create.Token;
    result := writer.WritePackageFromSpec(token, options);
  finally
    options.Free;
  end;
end;

// Packs and returns the message of whatever was raised, or '' when the pack completed.
function PackExpectingFailure(const workDir : string) : string;
begin
  result := '';
  try
    if not Pack(workDir) then
      result := 'pack returned false without a message';
  except
    on e : Exception do
      result := e.Message;
  end;
end;

procedure Cleanup(const workDir : string);
begin
  if TDirectory.Exists(workDir) then
    TDirectory.Delete(workDir, true);
end;

{ TBuildHookPackTests }

procedure TBuildHookPackTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TBuildHookPackTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TBuildHookPackTests.Pack_Succeeds_When_Build_Project_Is_Clean;
var
  workDir : string;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteDspec(workDir, 'Pkg.dproj', '');
    Assert.IsTrue(Pack(workDir), 'a clean package project should pack');
  finally
    Cleanup(workDir);
  end;
end;

procedure TBuildHookPackTests.Pack_Fails_When_Build_Project_Declares_Target;
var
  workDir : string;
  message : string;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithTarget);
    WriteDspec(workDir, 'Pkg.dproj', '');
    message := PackExpectingFailure(workDir);
    Assert.Contains(message, 'Mischief', true, 'pack should name the offending target : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

procedure TBuildHookPackTests.Pack_Fails_When_Build_Project_Declares_UsingTask;
var
  workDir : string;
  message : string;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithUsingTask);
    WriteDspec(workDir, 'Pkg.dproj', '');
    message := PackExpectingFailure(workDir);
    Assert.Contains(message, 'Payload', true, 'pack should name the offending task : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

procedure TBuildHookPackTests.Pack_Fails_When_Build_Project_Imports_Unknown_Targets;
var
  workDir : string;
  message : string;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithUnknownImport);
    WriteDspec(workDir, 'Pkg.dproj', '');
    message := PackExpectingFailure(workDir);
    Assert.Contains(message, 'extra.targets', true, 'pack should name the offending import : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

procedure TBuildHookPackTests.Pack_Fails_When_Build_Project_Sets_PostBuildEvent;
var
  workDir : string;
  message : string;
begin
  //Advisory at install (TMSBuildCompiler already blanks it) but an error here - the author still
  //believes the step runs, and only they can remove it.
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithPostBuildEvent);
    WriteDspec(workDir, 'Pkg.dproj', '');
    message := PackExpectingFailure(workDir);
    Assert.Contains(message, 'PostBuildEvent', true, 'pack should name the offending property : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

procedure TBuildHookPackTests.Pack_Fails_When_Design_Project_Declares_Target;
var
  workDir : string;
  message : string;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteProjectFile(workDir, 'PkgDesign.dproj', cDprojWithTarget);
    WriteDspec(workDir, 'Pkg.dproj', 'PkgDesign.dproj');
    message := PackExpectingFailure(workDir);
    Assert.Contains(message, 'Mischief', true, 'design entries are compiled too : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBuildHookPackTests);

end.
