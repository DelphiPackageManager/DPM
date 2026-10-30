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
// leaving it for every consumer to hit. Build events DPM already blanks only warn.
// See DPM.Core.Project.BuildHookValidator.

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
    procedure Pack_Warns_When_Build_Project_Sets_PostBuildEvent;
    procedure Pack_Fails_When_Design_Project_Declares_Target;
  end;

  // How pack resolves the paths a dspec gives. A dspec can sit in a sub-folder and reach its content with
  // '..\' - its build projects then end up relative to the package's effective root, and that is the file
  // checked. A glob ending in a lone '*' is a pattern like any other (its last segment used to be dropped).
  {$M+}
  [TestFixture]
  TPackPathTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Pack_Checks_Hooks_Of_A_Build_Project_Reached_With_DotDot;
    procedure Pack_Covers_A_Build_Project_Reached_With_DotDot;
    procedure Pack_Lone_Star_Source_With_Dest_Keeps_Its_Folder;
  end;

  // What a pack leaves in the output folder : the package when it worked, and nothing new when it didn't - not
  // half a package, and not a package from before replaced by one that failed.
  {$M+}
  [TestFixture]
  TPackOutputTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Pack_That_Works_Leaves_Only_The_Package;
    procedure Pack_That_Fails_Leaves_The_Package_From_Before;
    procedure Pack_With_A_Token_It_Can_Not_Replace_Fails;
    procedure Pack_Resolves_Variables_Whatever_Their_Case;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
  VSoft.CancellationToken,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Diagnostics,
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

function NewWriter(const logger : ILogger) : IPackageWriter;
var
  hashing : IHashingService;
  manifestSvc : IManifestService;
  validator : IArchiveValidator;
  archiveWriter : IPackageArchiveWriter;
  specReader : IPackageSpecReader;
begin
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

// Packs specFile, its paths relative to basePath (the dspec's folder, as the command line defaults it),
// into outputFolder. Unlike the other pack fixtures this deliberately lets an exception escape - a
// rejected build hook is reported the same way an uncovered build entry is, by raising.
function PackSpec(const specFile, basePath, outputFolder : string; const logger : ILogger) : boolean;
var
  writer : IPackageWriter;
  options : TPackOptions;
  token : ICancellationToken;
begin
  writer := NewWriter(logger);
  options := TPackOptions.Create;
  try
    options.SpecFile := specFile;
    options.BasePath := basePath;
    options.OutputFolder := outputFolder;
    token := TCancellationTokenSourceFactory.Create.Token;
    result := writer.WritePackageFromSpec(token, options);
  finally
    options.Free;
  end;
end;

function Pack(const workDir : string; const logger : ILogger) : boolean; overload;
begin
  result := PackSpec(TPath.Combine(workDir, 'test.dspec.yaml'), workDir, workDir, logger);
end;

function Pack(const workDir : string) : boolean; overload;
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  result := Pack(workDir, logger);
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

procedure TBuildHookPackTests.Pack_Warns_When_Build_Project_Sets_PostBuildEvent;
var
  workDir : string;
  logger : TTestLogger;
  loggerIntf : ILogger;
begin
  //Advisory, same as at install - TMSBuildCompiler blanks the event so it cannot run. Authors keep
  //build events in the project for their own builds, so it is worth saying but not worth refusing.
  logger := TTestLogger.Create;
  loggerIntf := logger;
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithPostBuildEvent);
    WriteDspec(workDir, 'Pkg.dproj', '');
    Assert.IsTrue(Pack(workDir, loggerIntf), 'a build event should not stop the pack');
    Assert.IsTrue(logger.Logged('warning', 'PostBuildEvent'),
                  'pack should warn naming the property : ' + logger.Messages.Text);
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

{ TPackPathTests }

procedure WriteFileIn(const fileName, contents : string);
begin
  TDirectory.CreateDirectory(ExtractFilePath(fileName));
  TFile.WriteAllText(fileName, contents, TEncoding.UTF8);
end;

procedure WriteSpecFile(const fileName : string; const templateLines : array of string);
var
  yaml : TStringList;
  i : integer;
begin
  yaml := TStringList.Create;
  try
    yaml.Add('metadata:');
    yaml.Add('  id: Test.Paths');
    yaml.Add('  version: 1.0.0');
    yaml.Add('  description: pack path test');
    yaml.Add('  authors:');
    yaml.Add('    - Vincent Parrett');
    yaml.Add('  license: Apache-2.0');
    yaml.Add('targetPlatforms:');
    yaml.Add('  - compiler: 12.0');
    yaml.Add('    platforms: [Win32]');
    yaml.Add('    template: default');
    yaml.Add('templates:');
    yaml.Add('  - name: default');
    for i := 0 to High(templateLines) do
      yaml.Add('    ' + templateLines[i]);
    yaml.WriteBOM := false;
    TDirectory.CreateDirectory(ExtractFilePath(fileName));
    yaml.SaveToFile(fileName, TEncoding.UTF8);
  finally
    yaml.Free;
  end;
end;

// The entry names of the one .dpkg packed into outputFolder.
function PackedEntries(const outputFolder : string) : TStringList;
var
  packages : TArray<string>;
  zip : TZipFile;
  i : integer;
begin
  packages := TArray<string>(TDirectory.GetFiles(outputFolder, '*.dpkg'));
  Assert.AreEqual(1, Length(packages), 'one package should have been packed');
  result := TStringList.Create;
  zip := TZipFile.Create;
  try
    zip.Open(packages[0], zmRead);
    for i := 0 to zip.FileCount - 1 do
      result.Add(zip.FileName[i]);
    zip.Close;
  finally
    zip.Free;
  end;
end;

// work\a\b\test.dspec.yaml reaches work\shared (so the effective root is work\) and work\a\pkg - the build
// project, '..\pkg\Pkg.dproj' from the dspec, is a\pkg\Pkg.dproj in the package. Dropping the '..' instead
// (as pack used to) points at work\pkg\Pkg.dproj, which isn't there.
procedure WriteDotDotLayout(const workDir, dprojXml : string);
begin
  WriteFileIn(TPath.Combine(workDir, 'shared\Unit1.pas'), 'unit Unit1; interface implementation end.');
  WriteFileIn(TPath.Combine(workDir, 'a\pkg\Pkg.dproj'), dprojXml);
  WriteSpecFile(TPath.Combine(workDir, 'a\b\test.dspec.yaml'),
    ['source:',
     '  - src: ''..\..\shared\*.pas''',
     '  - src: ''..\pkg\*.dproj''',
     'build:',
     '  - project: ''..\pkg\Pkg.dproj''']);
end;

procedure TPackPathTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TPackPathTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TPackPathTests.Pack_Checks_Hooks_Of_A_Build_Project_Reached_With_DotDot;
var
  workDir : string;
  message : string;
  logger : ILogger;
begin
  workDir := MakeWorkDir;
  try
    WriteDotDotLayout(workDir, cDprojWithTarget);
    message := '';
    logger := TTestLogger.Create;
    try
      if not PackSpec(TPath.Combine(workDir, 'a\b\test.dspec.yaml'), TPath.Combine(workDir, 'a\b'), TPath.Combine(workDir, 'out'), logger) then
        message := 'pack returned false without a message';
    except
      on e : Exception do
        message := e.Message;
    end;
    //the scan has to look at the file that ends up in the package - otherwise the dproj isn't found and
    //goes unchecked.
    Assert.Contains(message, 'Mischief', true, 'pack should have scanned a\pkg\Pkg.dproj : ' + message);
  finally
    Cleanup(workDir);
  end;
end;

procedure TPackPathTests.Pack_Covers_A_Build_Project_Reached_With_DotDot;
var
  workDir : string;
  entries : TStringList;
  logger : ILogger;
begin
  //a local - created inline into PackSpec's const parameter it would never be released.
  logger := TTestLogger.Create;
  workDir := MakeWorkDir;
  try
    WriteDotDotLayout(workDir, cCleanDproj);
    Assert.IsTrue(PackSpec(TPath.Combine(workDir, 'a\b\test.dspec.yaml'), TPath.Combine(workDir, 'a\b'), TPath.Combine(workDir, 'out'), logger));
    entries := PackedEntries(TPath.Combine(workDir, 'out'));
    try
      Assert.IsTrue(entries.IndexOf('a/pkg/Pkg.dproj') >= 0, 'entries : ' + entries.CommaText);
      Assert.IsTrue(entries.IndexOf('shared/Unit1.pas') >= 0, 'entries : ' + entries.CommaText);
    finally
      entries.Free;
    end;
  finally
    Cleanup(workDir);
  end;
end;

procedure TPackPathTests.Pack_Lone_Star_Source_With_Dest_Keeps_Its_Folder;
var
  workDir : string;
  entries : TStringList;
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  workDir := MakeWorkDir;
  try
    WriteFileIn(TPath.Combine(workDir, 'bpl\Win32\readme.txt'), 'prebuilt');
    WriteSpecFile(TPath.Combine(workDir, 'test.dspec.yaml'),
      ['source:',
       '  - src: ''.\bpl\Win32\*''',
       '    dest: ''bpl\Win32''']);
    Assert.IsTrue(PackSpec(TPath.Combine(workDir, 'test.dspec.yaml'), workDir, TPath.Combine(workDir, 'out'), logger));
    entries := PackedEntries(TPath.Combine(workDir, 'out'));
    try
      //files keep their place relative to the glob's folder - bpl\Win32\ - as they do for bpl\Win32\*.txt.
      Assert.IsTrue(entries.IndexOf('bpl/Win32/readme.txt') >= 0, 'entries : ' + entries.CommaText);
      Assert.IsTrue(entries.IndexOf('bpl/Win32/Win32/readme.txt') < 0, 'entries : ' + entries.CommaText);
    finally
      entries.Free;
    end;
  finally
    Cleanup(workDir);
  end;
end;

{ TPackOutputTests }

//the package WriteDspec's spec makes.
function PackageFileName(const workDir : string) : string;
begin
  result := TPath.Combine(workDir, 'Test.Hooks-' + CompilerToString(TCompilerVersion.Delphi12_0) + '-' +
                                   DPMPlatformsToBinString([TDPMPlatform.Win32]) + '-1.0.0.dpkg');
end;

//the files in the work folder, apart from the ones WriteDspec and WriteProjectFile put there.
function OutputFiles(const workDir : string) : TStringList;
var
  f : string;
begin
  result := TStringList.Create;
  for f in TDirectory.GetFiles(workDir) do
    if not SameText(ExtractFileName(f), 'test.dspec.yaml') then
      result.Add(ExtractFileName(f));
end;

procedure TPackOutputTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TPackOutputTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TPackOutputTests.Pack_That_Works_Leaves_Only_The_Package;
var
  workDir : string;
  files : TStringList;
begin
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteDspec(workDir, 'Pkg.dproj', '');
    Assert.IsTrue(Pack(workDir), 'precondition : a clean package packs');
    files := OutputFiles(workDir);
    try
      Assert.AreEqual(1, files.Count, files.Text);
      Assert.AreEqual(ExtractFileName(PackageFileName(workDir)), files[0], false);
    finally
      files.Free;
    end;
  finally
    Cleanup(workDir);
  end;
end;

procedure TPackOutputTests.Pack_That_Fails_Leaves_The_Package_From_Before;
var
  workDir : string;
  files : TStringList;
begin
  workDir := MakeWorkDir;
  try
    //what an earlier pack of the same version left.
    TFile.WriteAllText(PackageFileName(workDir), 'the package from before', TEncoding.UTF8);
    //the build project is refused part way through the pack - after the package file was started.
    WriteProjectFile(workDir, 'Pkg.dproj', cDprojWithTarget);
    WriteDspec(workDir, 'Pkg.dproj', '');
    Assert.IsTrue(PackExpectingFailure(workDir) <> '', 'precondition : the pack fails');
    Assert.AreEqual('the package from before', TFile.ReadAllText(PackageFileName(workDir), TEncoding.UTF8),
                    'the failed pack must not touch the package already there');
    files := OutputFiles(workDir);
    try
      Assert.AreEqual(1, files.Count, 'nothing left of the failed pack : ' + files.Text);
    finally
      files.Free;
    end;
  finally
    Cleanup(workDir);
  end;
end;

procedure TPackOutputTests.Pack_With_A_Token_It_Can_Not_Replace_Fails;
var
  workDir : string;
  logger : TTestLogger;
  loggerIntf : ILogger;
  spec : string;
  didPack : boolean;
begin
  workDir := MakeWorkDir;
  logger := TTestLogger.Create;
  loggerIntf := logger;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteDspec(workDir, 'Pkg.dproj', '');
    spec := TFile.ReadAllText(TPath.Combine(workDir, 'test.dspec.yaml'), TEncoding.UTF8);
    spec := StringReplace(spec, 'description: build hook pack test', 'description: built for $nosuchtoken$', []);
    TFile.WriteAllText(TPath.Combine(workDir, 'test.dspec.yaml'), spec, TEncoding.UTF8);
    didPack := Pack(workDir, loggerIntf);
    Assert.IsFalse(didPack, 'a package with a token left in it is no use');
    Assert.IsTrue(logger.HasError(TErrorCode.E6103_VariableReplacementFailed), logger.Messages.Text);
    Assert.IsFalse(FileExists(PackageFileName(workDir)), 'no package was written');
  finally
    Cleanup(workDir);
  end;
end;

//the dspec packed into dpkgPath.
function ReadPackedSpec(const dpkgPath : string) : IPackageSpec;
var
  zip : TZipFile;
  bytes : TBytes;
  i : integer;
  found : boolean;
  reader : IPackageSpecReader;
begin
  found := false;
  zip := TZipFile.Create;
  try
    zip.Open(dpkgPath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      if SameText(zip.FileName[i], cPackageDspecFile) then
      begin
        zip.Read(i, bytes);
        found := true;
        break;
      end;
    end;
  finally
    zip.Free;
  end;
  Assert.IsTrue(found, cPackageDspecFile + ' missing from the package');
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  result := reader.ReadSpecString(TEncoding.UTF8.GetString(bytes));
  Assert.IsNotNull(result, 'the packed dspec reads');
end;

procedure TPackOutputTests.Pack_Resolves_Variables_Whatever_Their_Case;
var
  workDir : string;
  specFile : string;
  spec : string;
  packedSpec : IPackageSpec;
begin
  //variable names are case insensitive : the platform's PACKAGESOURCE overrides the package's PackageSource, and tokens
  //in another case again find both it and the package's Company.
  workDir := MakeWorkDir;
  try
    WriteProjectFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteDspec(workDir, 'Pkg.dproj', '');
    specFile := TPath.Combine(workDir, 'test.dspec.yaml');
    spec := TFile.ReadAllText(specFile, TEncoding.UTF8);
    spec := StringReplace(spec, 'description: build hook pack test', 'description: built from $packagesource$ by $COMPANY$', []);
    spec := StringReplace(spec, 'targetPlatforms:', 'variables:' + sLineBreak + '  PackageSource: the package' + sLineBreak +
                                                    '  Company: VSoft' + sLineBreak + 'targetPlatforms:', []);
    spec := StringReplace(spec, '    template: default', '    template: default' + sLineBreak + '    variables:' + sLineBreak +
                                                        '      PACKAGESOURCE: the platform', []);
    TFile.WriteAllText(specFile, spec, TEncoding.UTF8);
    Assert.IsTrue(Pack(workDir), 'pack');
    packedSpec := ReadPackedSpec(PackageFileName(workDir));
    Assert.AreEqual('built from the platform by VSoft', packedSpec.MetaData.Description, false);
  finally
    Cleanup(workDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TBuildHookPackTests);
  TDUnitX.RegisterTestFixture(TPackPathTests);
  TDUnitX.RegisterTestFixture(TPackOutputTests);

end.
