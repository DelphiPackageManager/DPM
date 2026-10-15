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

unit DPM.Core.Tests.Spec.ExcludeCompilers;

// A template is shared by every compiler whose targetPlatform names it, but a build or design
// project does not always exist for all of them - eg a component set with no animated components
// package for XE6. excludeCompilers on the entry leaves it out for those compilers. It is applied
// at pack time, so each package only carries the entries that apply to its own compiler.

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TExcludeCompilersSpecTests = class
  published
    procedure Build_Entry_Loads_ExcludeCompilers;
    procedure Design_Entry_Loads_ExcludeCompilers;
    procedure Entry_Without_ExcludeCompilers_Applies_To_Every_Compiler;
    procedure Unknown_Compiler_Fails_The_Load;
    procedure ExcludeCompilers_Round_Trips;
    procedure Empty_ExcludeCompilers_Is_Not_Written;
    procedure Clone_Preserves_ExcludeCompilers;
    procedure ApplyCompilerExclusions_Narrows_Template_To_One_Compiler;
    procedure DuplicateTemplate_Copies_Build_And_Design_Entries;
    procedure Design_SetPlatforms_Keeps_IDE_Platforms;
  end;

  {$M+}
  [TestFixture]
  TExcludeCompilersPackTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Pack_Drops_Excluded_Entries_For_That_Compiler_Only;
    procedure Packed_Spec_Does_Not_Carry_ExcludeCompilers;
    procedure Pack_Does_Not_Validate_Excluded_Entry;
  end;

  {$M+}
  [TestFixture]
  TExcludeCompilersPrepareTests = class
  published
    procedure Prepare_Does_Not_Scaffold_Excluded_Entry;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.IOUtils,
  System.Zip,
  VSoft.CancellationToken,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Constants,
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
  DPM.Core.Options.Pack,
  DPM.Core.Project.Prepare;

var
  GWorkDirCounter : integer = 0;

const
  //%0:s and %1:s are extra lines (indented, CRLF terminated) appended to the build entry and the
  //design entry - '' leaves the entry with no excludeCompilers.
  cSpecYaml =
    'metadata:'#13#10 +
    '  id: Test.Exclude'#13#10 +
    '  version: 1.0.0'#13#10 +
    '  description: exclude compilers test'#13#10 +
    '  authors:'#13#10 +
    '    - Vincent Parrett'#13#10 +
    '  license: Apache-2.0'#13#10 +
    'targetPlatforms:'#13#10 +
    '  - compiler from: delphi12.0'#13#10 +
    '    compiler to: delphi13.0'#13#10 +
    '    platforms: [Win32]'#13#10 +
    '    template: default'#13#10 +
    'templates:'#13#10 +
    '  - name: default'#13#10 +
    '    source:'#13#10 +
    '      - src: source/*.dproj'#13#10 +
    '    build:'#13#10 +
    '      - project: source/Pkg.dproj'#13#10 +
    '%0:s' +
    '    design:'#13#10 +
    '      - project: source/PkgDesign.dproj'#13#10 +
    '%1:s';

  cCleanDproj =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup>'#13#10 +
    '        <MainSource>Pkg.dpk</MainSource>'#13#10 +
    '        <AppType>Package</AppType>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <Import Project="$(BDS)\Bin\CodeGear.Delphi.Targets"/>'#13#10 +
    '</Project>'#13#10;

{ helpers }

function ReadSpecWith(const logger : ILogger; const buildExtra, designExtra : string) : IPackageSpec;
var
  reader : IPackageSpecReader;
begin
  reader := TPackageSpecReader.Create(logger);
  result := reader.ReadSpecString(Format(cSpecYaml, [buildExtra, designExtra]));
  Assert.IsNotNull(result, 'failed to read spec');
end;

function ReadSpec(const buildExtra, designExtra : string) : IPackageSpec;
begin
  result := ReadSpecWith(TTestLogger.Create, buildExtra, designExtra);
  Assert.IsTrue(result.IsValid, 'spec should be valid');
end;

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

function MakeWorkDir(const prefix : string) : string;
begin
  Inc(GWorkDirCounter);
  result := TPath.Combine(TPath.GetTempPath,
    prefix + FormatDateTime('yyyymmddhhnnsszzz', Now) + '-' + IntToStr(GWorkDirCounter));
  TDirectory.CreateDirectory(result);
  TDirectory.CreateDirectory(TPath.Combine(result, 'source'));
end;

procedure WriteSourceFile(const workDir, name, content : string);
begin
  TFile.WriteAllText(TPath.Combine(TPath.Combine(workDir, 'source'), name), content, TEncoding.UTF8);
end;

procedure WriteDspec(const workDir : string; const lines : array of string);
var
  yaml : TStringList;
  i : integer;
begin
  yaml := TStringList.Create;
  try
    yaml.Add('metadata:');
    yaml.Add('  id: Test.Exclude');
    yaml.Add('  version: 1.0.0');
    yaml.Add('  description: exclude compilers pack test');
    yaml.Add('  authors:');
    yaml.Add('    - Vincent Parrett');
    yaml.Add('  license: Apache-2.0');
    for i := 0 to High(lines) do
      yaml.Add(lines[i]);
    yaml.WriteBOM := false;
    yaml.SaveToFile(TPath.Combine(workDir, 'test.dspec.yaml'), TEncoding.UTF8);
  finally
    yaml.Free;
  end;
end;

// Packs workDir. Lets an exception escape - an uncovered build entry is reported by raising.
function Pack(const workDir : string) : boolean;
var
  writer : IPackageWriter;
  options : TPackOptions;
  token : ICancellationToken;
begin
  writer := NewWriter(TTestLogger.Create);
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

// The package written for one compiler, eg 'delphi12.0'.
function FindPackage(const workDir, compiler : string) : string;
var
  produced : TArray<string>;
begin
  produced := TArray<string>(TDirectory.GetFiles(workDir, '*-' + compiler + '-*' + cPackageFileExt, TSearchOption.soTopDirectoryOnly));
  Assert.AreEqual<integer>(1, Length(produced), 'expected exactly one package for ' + compiler);
  result := produced[0];
end;

// Reads a single named entry from a .dpkg as a UTF-8 string. Returns '' if the entry is absent.
function ReadArchiveText(const dpkgPath, entryName : string) : string;
var
  zip : TZipFile;
  i : integer;
  bytes : TBytes;
begin
  result := '';
  zip := TZipFile.Create;
  try
    zip.Open(dpkgPath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      if SameText(zip.FileName[i], entryName) then
      begin
        zip.Read(i, bytes);
        result := TEncoding.UTF8.GetString(bytes);
        exit;
      end;
    end;
  finally
    zip.Free;
  end;
end;

function LoadEmbeddedTemplate(const dpkgPath : string) : ISpecTemplate;
var
  reader : IPackageSpecReader;
  yaml : string;
  spec : IPackageSpec;
begin
  yaml := ReadArchiveText(dpkgPath, cPackageDspecFile);
  Assert.IsTrue(yaml <> '', 'package.dspec.yaml missing from package');
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(yaml);
  Assert.IsNotNull(spec, 'could not parse embedded dspec');
  Assert.AreEqual(1, spec.Templates.Count, 'packed spec should have a single template');
  result := spec.Templates[0];
end;

procedure Cleanup(const workDir : string);
begin
  try
    if TDirectory.Exists(workDir) then
      TDirectory.Delete(workDir, true);
  except
    //best effort - temp dir, leave it if something still has a handle.
  end;
end;

// Two compilers sharing one template. Extra.dproj and PkgDesign.dproj do not exist for 12.0.
procedure WriteSharedTemplateDspec(const workDir : string);
begin
  WriteDspec(workDir, [
    'targetPlatforms:',
    '  - compiler from: 12.0',
    '    compiler to: 13.0',
    '    platforms: [Win32]',
    '    template: default',
    'templates:',
    '  - name: default',
    '    source:',
    '      - src: .\source\*.dproj',
    '        dest: source',
    '    build:',
    '      - project: .\source\Pkg.dproj',
    '      - project: .\source\Extra.dproj',
    '        excludeCompilers: [delphi12.0]',
    '    design:',
    '      - project: .\source\PkgDesign.dproj',
    '        excludeCompilers:',
    '          - 12.0']);
end;

{ TExcludeCompilersSpecTests }

procedure TExcludeCompilersSpecTests.Build_Entry_Loads_ExcludeCompilers;
var
  build : ISpecBuildEntry;
begin
  build := ReadSpec('        excludeCompilers: [delphixe6, delphi12.0]'#13#10, '').FindTemplate('default').BuildEntries[0];
  Assert.IsTrue(build.ExcludeCompilers = [TCompilerVersion.DelphiXE6, TCompilerVersion.Delphi12_0], 'excludeCompilers not loaded');
  Assert.IsFalse(build.AppliesTo(TCompilerVersion.Delphi12_0), 'entry should not apply to an excluded compiler');
  Assert.IsTrue(build.AppliesTo(TCompilerVersion.Delphi13_0), 'entry should apply to a compiler it does not exclude');
end;

//Block sequence and the short compiler spelling - the same forms targetPlatforms accepts.
procedure TExcludeCompilersSpecTests.Design_Entry_Loads_ExcludeCompilers;
var
  design : ISpecDesignEntry;
begin
  design := ReadSpec('', '        excludeCompilers:'#13#10 + '          - 12.0'#13#10).FindTemplate('default').DesignEntries[0];
  Assert.IsTrue(design.ExcludeCompilers = [TCompilerVersion.Delphi12_0], 'excludeCompilers not loaded');
  Assert.IsFalse(design.AppliesTo(TCompilerVersion.Delphi12_0), 'entry should not apply to an excluded compiler');
  Assert.IsTrue(design.AppliesTo(TCompilerVersion.Delphi13_0), 'entry should apply to a compiler it does not exclude');
end;

procedure TExcludeCompilersSpecTests.Entry_Without_ExcludeCompilers_Applies_To_Every_Compiler;
var
  template : ISpecTemplate;
  compiler : TCompilerVersion;
begin
  template := ReadSpec('', '').FindTemplate('default');
  Assert.IsTrue(template.BuildEntries[0].ExcludeCompilers = [], 'build entry should exclude nothing');
  Assert.IsTrue(template.DesignEntries[0].ExcludeCompilers = [], 'design entry should exclude nothing');
  for compiler := Succ(TCompilerVersion.UnknownVersion) to High(TCompilerVersion) do
  begin
    Assert.IsTrue(template.BuildEntries[0].AppliesTo(compiler), 'build entry should apply to ' + CompilerToString(compiler));
    Assert.IsTrue(template.DesignEntries[0].AppliesTo(compiler), 'design entry should apply to ' + CompilerToString(compiler));
  end;
end;

//A typo must not quietly leave the entry in the package it was meant to be excluded from.
procedure TExcludeCompilersSpecTests.Unknown_Compiler_Fails_The_Load;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  spec : IPackageSpec;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  spec := ReadSpecWith(loggerIntf, '        excludeCompilers: [delphixe66]'#13#10, '');
  Assert.IsFalse(spec.IsValid, 'an unknown compiler in excludeCompilers should fail the load');
  Assert.IsTrue(logger.Logged('error', 'delphixe66'), 'error should name the bad value : ' + logger.Messages.Text);

  loggerIntf.Clear;
  spec := ReadSpecWith(loggerIntf, '', '        excludeCompilers: [delphixe66]'#13#10);
  Assert.IsFalse(spec.IsValid, 'an unknown compiler in a design entry excludeCompilers should fail the load');
  Assert.IsTrue(logger.Logged('error', 'delphixe66'), 'error should name the bad value : ' + logger.Messages.Text);
end;

procedure TExcludeCompilersSpecTests.ExcludeCompilers_Round_Trips;
var
  spec : IPackageSpec;
  reloaded : IPackageSpec;
  reader : IPackageSpecReader;
  yaml : string;
  excludeStart : integer;
  template : ISpecTemplate;
begin
  spec := ReadSpec('        excludeCompilers: [delphi12.0, delphixe6]'#13#10, '        excludeCompilers: [delphi13.0]'#13#10);
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  excludeStart := Pos('excludeCompilers', yaml);
  Assert.IsTrue(excludeStart > 0, 'excludeCompilers should be written, got:'#13#10 + yaml);
  //canonical spelling, in compiler order - not whatever the author typed. Searched from the key, as
  //targetPlatforms above it names delphi12.0 too.
  Assert.IsTrue(PosEx('delphixe6', yaml, excludeStart) < PosEx('delphi12.0', yaml, excludeStart), 'compilers should be written in compiler order, got:'#13#10 + yaml);

  reader := TPackageSpecReader.Create(TTestLogger.Create);
  reloaded := reader.ReadSpecString(yaml);
  Assert.IsNotNull(reloaded, 'failed to reload generated yaml');
  Assert.IsTrue(reloaded.IsValid, 'reloaded spec should be valid');
  template := reloaded.FindTemplate('default');
  Assert.IsTrue(template.BuildEntries[0].ExcludeCompilers = [TCompilerVersion.DelphiXE6, TCompilerVersion.Delphi12_0], 'build entry excludes lost in round trip');
  Assert.IsTrue(template.DesignEntries[0].ExcludeCompilers = [TCompilerVersion.Delphi13_0], 'design entry excludes lost in round trip');
end;

procedure TExcludeCompilersSpecTests.Empty_ExcludeCompilers_Is_Not_Written;
var
  spec : IPackageSpec;
  yaml : string;
begin
  spec := ReadSpec('', '');
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  Assert.IsTrue(Pos('excludeCompilers', yaml) = 0, 'an empty exclude list should not be written, got:'#13#10 + yaml);
end;

procedure TExcludeCompilersSpecTests.Clone_Preserves_ExcludeCompilers;
var
  template : ISpecTemplate;
  clonedTemplate : ISpecTemplate;
begin
  template := ReadSpec('        excludeCompilers: [delphi12.0]'#13#10, '        excludeCompilers: [delphi13.0]'#13#10).FindTemplate('default');
  Assert.IsTrue(template.BuildEntries[0].Clone.ExcludeCompilers = [TCompilerVersion.Delphi12_0], 'build entry clone lost excludes');
  Assert.IsTrue(template.DesignEntries[0].Clone.ExcludeCompilers = [TCompilerVersion.Delphi13_0], 'design entry clone lost excludes');

  clonedTemplate := template.Clone;
  Assert.IsTrue(clonedTemplate.BuildEntries[0].ExcludeCompilers = [TCompilerVersion.Delphi12_0], 'template clone lost build entry excludes');
  Assert.IsTrue(clonedTemplate.DesignEntries[0].ExcludeCompilers = [TCompilerVersion.Delphi13_0], 'template clone lost design entry excludes');
end;

//Shared by pack and by a git package's dspec being reduced to the compiler that is installing it.
procedure TExcludeCompilersSpecTests.ApplyCompilerExclusions_Narrows_Template_To_One_Compiler;
var
  spec : IPackageSpec;
  template : ISpecTemplate;
begin
  //Pkg excludes 12.0, Extra excludes 13.0, the design entry excludes 12.0
  spec := ReadSpec('        excludeCompilers: [delphi12.0]'#13#10 +
                   '      - project: source/Extra.dproj'#13#10 +
                   '        excludeCompilers: [delphi13.0]'#13#10,
                   '        excludeCompilers: [delphi12.0]'#13#10);

  template := spec.FindTemplate('default').Clone;
  template.ApplyCompilerExclusions(TCompilerVersion.Delphi12_0);
  Assert.AreEqual(1, template.BuildEntries.Count, '12.0 build entries');
  Assert.AreEqual('source/Extra.dproj', template.BuildEntries[0].Project);
  Assert.IsTrue(template.BuildEntries[0].ExcludeCompilers = [], 'a single compiler template has nothing left to exclude');
  Assert.AreEqual(0, template.DesignEntries.Count, '12.0 design entries');

  template := spec.FindTemplate('default').Clone;
  template.ApplyCompilerExclusions(TCompilerVersion.Delphi13_0);
  Assert.AreEqual(1, template.BuildEntries.Count, '13.0 build entries');
  Assert.AreEqual('source/Pkg.dproj', template.BuildEntries[0].Project);
  Assert.AreEqual(1, template.DesignEntries.Count, '13.0 design entries');
  Assert.IsTrue(template.DesignEntries[0].ExcludeCompilers = [], 'a single compiler template has nothing left to exclude');
end;

procedure TExcludeCompilersSpecTests.DuplicateTemplate_Copies_Build_And_Design_Entries;
var
  spec : IPackageSpec;
  template : ISpecTemplate;
  duplicate : ISpecTemplate;
begin
  spec := ReadSpec('        excludeCompilers: [delphi12.0]'#13#10, '        excludeCompilers: [delphi13.0]'#13#10);
  template := spec.FindTemplate('default');
  duplicate := spec.DuplicateTemplate(template, 'copy');

  Assert.IsTrue(spec.FindTemplate('copy') <> nil, 'duplicate should be added to the spec');
  Assert.AreEqual('copy', duplicate.Name);
  Assert.AreEqual(1, duplicate.BuildEntries.Count, 'build entries not duplicated');
  Assert.AreEqual(1, duplicate.DesignEntries.Count, 'design entries not duplicated');
  Assert.AreEqual(1, duplicate.SourceEntries.Count, 'source entries not duplicated');
  Assert.IsTrue(duplicate.BuildEntries[0].ExcludeCompilers = [TCompilerVersion.Delphi12_0], 'build entry excludes not duplicated');
  Assert.IsTrue(duplicate.DesignEntries[0].ExcludeCompilers = [TCompilerVersion.Delphi13_0], 'design entry excludes not duplicated');
  Assert.IsTrue(duplicate.BuildEntries[0] <> template.BuildEntries[0], 'duplicate should not share entries with its source');
  Assert.AreEqual('default', template.Name, 'source template should keep its name');
end;

//Design packages load into the IDE, which only comes in Win32 and Win64.
procedure TExcludeCompilersSpecTests.Design_SetPlatforms_Keeps_IDE_Platforms;
var
  design : ISpecDesignEntry;
begin
  design := ReadSpec('', '').FindTemplate('default').DesignEntries[0];

  design.Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
  Assert.IsTrue(design.Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64], 'Win32 + Win64 should be kept');

  design.Platforms := [TDPMPlatform.Win64, TDPMPlatform.Android64];
  Assert.IsTrue(design.Platforms = [TDPMPlatform.Win64], 'only the IDE platforms should be kept');

  design.Platforms := [];
  Assert.IsTrue(design.Platforms = [], 'clearing the platforms should clear them');
end;

{ TExcludeCompilersPackTests }

procedure TExcludeCompilersPackTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TExcludeCompilersPackTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TExcludeCompilersPackTests.Pack_Drops_Excluded_Entries_For_That_Compiler_Only;
var
  workDir : string;
  template12 : ISpecTemplate;
  template13 : ISpecTemplate;
begin
  workDir := MakeWorkDir('dpm-pack-exclude-');
  try
    WriteSourceFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteSourceFile(workDir, 'Extra.dproj', cCleanDproj);
    WriteSourceFile(workDir, 'PkgDesign.dproj', cCleanDproj);
    WriteSharedTemplateDspec(workDir);
    Assert.IsTrue(Pack(workDir), 'pack failed');

    template12 := LoadEmbeddedTemplate(FindPackage(workDir, 'delphi12.0'));
    Assert.AreEqual(1, template12.BuildEntries.Count, 'excluded build entry should be dropped for 12.0');
    Assert.AreEqual('source/Pkg.dproj', template12.BuildEntries[0].Project);
    Assert.AreEqual(0, template12.DesignEntries.Count, 'excluded design entry should be dropped for 12.0');

    template13 := LoadEmbeddedTemplate(FindPackage(workDir, 'delphi13.0'));
    Assert.AreEqual(2, template13.BuildEntries.Count, '13.0 is not excluded and should keep both build entries');
    Assert.AreEqual('source/Extra.dproj', template13.BuildEntries[1].Project);
    Assert.AreEqual(1, template13.DesignEntries.Count, '13.0 is not excluded and should keep the design entry');
  finally
    Cleanup(workDir);
  end;
end;

//A packed spec covers one compiler and the exclusion has already been applied - carrying the list
//into the package would only advertise compilers the package was never built for.
procedure TExcludeCompilersPackTests.Packed_Spec_Does_Not_Carry_ExcludeCompilers;
var
  workDir : string;
  yaml : string;
begin
  workDir := MakeWorkDir('dpm-pack-exclude-');
  try
    WriteSourceFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteSourceFile(workDir, 'Extra.dproj', cCleanDproj);
    WriteSourceFile(workDir, 'PkgDesign.dproj', cCleanDproj);
    WriteSharedTemplateDspec(workDir);
    Assert.IsTrue(Pack(workDir), 'pack failed');

    yaml := ReadArchiveText(FindPackage(workDir, 'delphi13.0'), cPackageDspecFile);
    Assert.IsTrue(Pos('Extra.dproj', yaml) > 0, 'fixture should keep the excluding entry for 13.0, got:'#13#10 + yaml);
    Assert.IsTrue(Pos('excludeCompilers', yaml) = 0, 'packed spec should not carry excludeCompilers, got:'#13#10 + yaml);
  finally
    Cleanup(workDir);
  end;
end;

//The reason to exclude an entry is usually that its project does not exist for that compiler, so
//the pack must not go looking for it.
procedure TExcludeCompilersPackTests.Pack_Does_Not_Validate_Excluded_Entry;
var
  workDir : string;
  packOk : boolean;
begin
  workDir := MakeWorkDir('dpm-pack-exclude-');
  try
    WriteSourceFile(workDir, 'Pkg.dproj', cCleanDproj);
    WriteDspec(workDir, [
      'targetPlatforms:',
      '  - compiler: 12.0',
      '    platforms: [Win32]',
      '    template: default',
      'templates:',
      '  - name: default',
      '    source:',
      '      - src: .\source\*.dproj',
      '        dest: source',
      '    build:',
      '      - project: .\source\Pkg.dproj',
      '      - project: .\missing\Extra.dproj',
      '        excludeCompilers: [delphi12.0]']);
    packOk := false;
    try
      packOk := Pack(workDir);
    except
      on e : Exception do
        Assert.Fail('pack should ignore an excluded entry : ' + e.Message);
    end;
    Assert.IsTrue(packOk, 'pack failed');
    Assert.AreEqual(1, LoadEmbeddedTemplate(FindPackage(workDir, 'delphi12.0')).BuildEntries.Count, 'excluded entry should not be packed');
  finally
    Cleanup(workDir);
  end;
end;

{ TExcludeCompilersPrepareTests }

procedure TExcludeCompilersPrepareTests.Prepare_Does_Not_Scaffold_Excluded_Entry;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  prepare : IPreparePackageFolders;
  workDir : string;
  folder12 : string;
  folder13 : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  workDir := MakeWorkDir('dpm-prepare-exclude-');
  try
    WriteSourceFile(workDir, 'Unit1.pas', 'unit Unit1;'#13#10'interface'#13#10'implementation'#13#10'end.'#13#10);
    WriteDspec(workDir, [
      'targetPlatforms:',
      '  - compiler from: 12.0',
      '    compiler to: 13.0',
      '    platforms: [Win32]',
      '    template: default',
      'templates:',
      '  - name: default',
      '    source:',
      '      - src: .\source\*.pas',
      '    build:',
      '      - project: .\packages\Pkg.dproj',
      '      - project: .\packages\Extra.dproj',
      '        excludeCompilers: [delphi12.0]']);

    //dry run - only logs what it would scaffold, which is what we assert on.
    prepare := TPreparePackageFolders.Create(loggerIntf, TPackageSpecReader.Create(loggerIntf), nil, nil);
    Assert.IsTrue(prepare.Execute(TPath.Combine(workDir, 'test.dspec.yaml'), false, true, TCancellationTokenSourceFactory.Create.Token),
                  'prepare failed : ' + logger.Messages.Text);

    folder12 := TPath.Combine(TPath.Combine(workDir, 'packages'), CompilerToRADStudioFolderName(TCompilerVersion.Delphi12_0));
    folder13 := TPath.Combine(TPath.Combine(workDir, 'packages'), CompilerToRADStudioFolderName(TCompilerVersion.Delphi13_0));
    Assert.IsTrue(logger.Logged('information', TPath.Combine(folder12, 'Pkg.dproj')), 'Pkg should be scaffolded for 12.0 : ' + logger.Messages.Text);
    Assert.IsFalse(logger.Logged('information', TPath.Combine(folder12, 'Extra.dproj')), 'Extra is excluded for 12.0 : ' + logger.Messages.Text);
    Assert.IsTrue(logger.Logged('information', TPath.Combine(folder13, 'Extra.dproj')), 'Extra should be scaffolded for 13.0 : ' + logger.Messages.Text);
  finally
    Cleanup(workDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TExcludeCompilersSpecTests);
  TDUnitX.RegisterTestFixture(TExcludeCompilersPackTests);
  TDUnitX.RegisterTestFixture(TExcludeCompilersPrepareTests);

end.
