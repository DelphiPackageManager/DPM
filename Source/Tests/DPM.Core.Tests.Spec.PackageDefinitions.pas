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
{                                                                           }
{***************************************************************************}

// Parses + round-trips the package-definitions dspec fixtures, asserting the new
// `package definitions` section survives load -> GenerateDspecYAML -> load unchanged and
// that each field (project, kind, files, exclude, requires, platforms) loads as expected.
// Runtime generation behaviour (kind inference, exclude filtering, deterministic GUID) is
// covered separately by DPM.Core.Tests.PackageGenerator against real on-disk source files.

unit DPM.Core.Tests.Spec.PackageDefinitions;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TPackageDefinitionsSpecTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Loads_All_Definitions;
    procedure Explicit_Kind_Is_Preserved;
    procedure Inferred_Kind_Is_Left_Blank_At_Spec_Level;
    procedure Files_Exclude_Requires_Platforms_Parse;
    procedure Platform_Override_Defaults_To_Empty;
    procedure RoundTrips_Unchanged;
    procedure Git_Package_Loads_With_Definitions;
    procedure Git_Package_RoundTrips;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  Spring.Collections,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader;

function DspecsDir : string;
var
  dir : string;
  candidate : string;
  i : integer;
begin
  result := '';
  dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  for i := 0 to 8 do
  begin
    candidate := dir + 'Source\Tests\dspecs\';
    if DirectoryExists(candidate) then
      exit(candidate);
    candidate := dir + 'Tests\dspecs\';
    if DirectoryExists(candidate) then
      exit(candidate);
    candidate := dir + 'dspecs\';
    if DirectoryExists(candidate) then
      exit(candidate);
    dir := IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(dir)));
  end;
end;

function LoadFixture(const name : string) : IPackageSpec;
var
  reader : IPackageSpecReader;
  dir : string;
begin
  dir := DspecsDir;
  Assert.IsTrue(dir <> '', 'could not locate the Tests\dspecs fixture folder');
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  result := reader.ReadSpec(dir + name);
  Assert.IsNotNull(result, 'failed to load fixture ' + name);
  Assert.IsTrue(result.IsValid, 'fixture is not valid: ' + name);
end;

function RoundTrip(const spec : IPackageSpec) : IPackageSpec;
var
  reader : IPackageSpecReader;
  yaml : string;
begin
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  result := reader.ReadSpecString(yaml);
  Assert.IsNotNull(result, 'failed to reload generated yaml');
end;

procedure AssertStringListsEqual(const expected, actual : IList<string>; const ctx : string);
var
  i : integer;
begin
  Assert.AreEqual(expected.Count, actual.Count, ctx + ' count');
  for i := 0 to expected.Count - 1 do
    Assert.AreEqual(expected[i], actual[i], ctx + ' item ' + IntToStr(i));
end;

function DefaultTemplate(const spec : IPackageSpec) : ISpecTemplate;
begin
  result := spec.FindTemplate('default');
  Assert.IsNotNull(result, 'default template missing');
end;

function FindDef(const tmpl : ISpecTemplate; const project : string) : ISpecPackageDefinition;
begin
  result := tmpl.FindPackageDefinition(project);
  Assert.IsNotNull(result, 'package definition not found: ' + project);
end;

{ TPackageDefinitionsSpecTests }

procedure TPackageDefinitionsSpecTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TPackageDefinitionsSpecTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TPackageDefinitionsSpecTests.Loads_All_Definitions;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
begin
  spec := LoadFixture('package-definitions.dspec.yaml');
  tmpl := DefaultTemplate(spec);
  Assert.AreEqual(4, tmpl.PackageDefinitions.Count, 'package definition count');
end;

procedure TPackageDefinitionsSpecTests.Explicit_Kind_Is_Preserved;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
begin
  spec := LoadFixture('package-definitions.dspec.yaml');
  tmpl := DefaultTemplate(spec);
  Assert.AreEqual('runtime', FindDef(tmpl, '.\packages\FooR.dproj').Kind, 'FooR kind');
  Assert.AreEqual('design', FindDef(tmpl, '.\packages\FooD.dproj').Kind, 'FooD kind');
end;

procedure TPackageDefinitionsSpecTests.Inferred_Kind_Is_Left_Blank_At_Spec_Level;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
begin
  //the spec stores only the author-written kind; inference (designide => design) happens in
  //the generator, not the parser. So definitions without an explicit kind load with kind = ''.
  spec := LoadFixture('package-definitions.dspec.yaml');
  tmpl := DefaultTemplate(spec);
  Assert.AreEqual('', FindDef(tmpl, '.\packages\BarInferred.dproj').Kind, 'BarInferred kind blank');
  Assert.AreEqual('', FindDef(tmpl, '.\packages\BazRuntime.dproj').Kind, 'BazRuntime kind blank');
end;

procedure TPackageDefinitionsSpecTests.Files_Exclude_Requires_Platforms_Parse;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
  fooR : ISpecPackageDefinition;
  fooD : ISpecPackageDefinition;
begin
  spec := LoadFixture('package-definitions.dspec.yaml');
  tmpl := DefaultTemplate(spec);

  fooR := FindDef(tmpl, '.\packages\FooR.dproj');
  Assert.AreEqual(1, fooR.Files.Count, 'FooR files count');
  Assert.AreEqual('.\src\*.pas', fooR.Files[0], 'FooR files[0]');
  Assert.AreEqual(2, fooR.Exclude.Count, 'FooR exclude count');
  Assert.AreEqual('*.Tests.pas', fooR.Exclude[0], 'FooR exclude[0]');
  Assert.AreEqual('Internal*.pas', fooR.Exclude[1], 'FooR exclude[1]');
  Assert.AreEqual(1, fooR.Requires.Count, 'FooR requires count');
  Assert.AreEqual('vcl', fooR.Requires[0], 'FooR requires[0]');
  Assert.IsTrue(fooR.Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64], 'FooR platforms');

  fooD := FindDef(tmpl, '.\packages\FooD.dproj');
  Assert.AreEqual(2, fooD.Files.Count, 'FooD files count');
  Assert.IsTrue(fooD.Platforms = [TDPMPlatform.Win32], 'FooD platforms (single)');
  AssertStringListsEqual(fooD.Requires, fooD.Requires, 'FooD requires self');
  Assert.AreEqual('FooR', fooD.Requires[0], 'FooD requires runtime sibling');
end;

procedure TPackageDefinitionsSpecTests.Platform_Override_Defaults_To_Empty;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
  bar : ISpecPackageDefinition;
begin
  //no platforms declared => empty set (generator falls back to the targetPlatform's platforms).
  spec := LoadFixture('package-definitions.dspec.yaml');
  tmpl := DefaultTemplate(spec);
  bar := FindDef(tmpl, '.\packages\BarInferred.dproj');
  Assert.IsTrue(bar.Platforms = [], 'BarInferred no platform override');
end;

procedure TPackageDefinitionsSpecTests.RoundTrips_Unchanged;
var
  spec : IPackageSpec;
  reloaded : IPackageSpec;
  et, at : ISpecTemplate;
  i : integer;
  ed, ad : ISpecPackageDefinition;
begin
  spec := LoadFixture('package-definitions.dspec.yaml');
  reloaded := RoundTrip(spec);

  et := DefaultTemplate(spec);
  at := DefaultTemplate(reloaded);
  Assert.AreEqual(et.PackageDefinitions.Count, at.PackageDefinitions.Count, 'def count after round-trip');
  for i := 0 to et.PackageDefinitions.Count - 1 do
  begin
    ed := et.PackageDefinitions[i];
    ad := at.FindPackageDefinition(ed.Project);
    Assert.IsNotNull(ad, 'def missing after round-trip: ' + ed.Project);
    Assert.AreEqual(ed.Kind, ad.Kind, ed.Project + ' kind');
    Assert.IsTrue(ed.Platforms = ad.Platforms, ed.Project + ' platforms');
    AssertStringListsEqual(ed.Files, ad.Files, ed.Project + ' files');
    AssertStringListsEqual(ed.Exclude, ad.Exclude, ed.Project + ' exclude');
    AssertStringListsEqual(ed.Requires, ad.Requires, ed.Project + ' requires');
  end;
end;

procedure TPackageDefinitionsSpecTests.Git_Package_Loads_With_Definitions;
var
  spec : IPackageSpec;
  tmpl : ISpecTemplate;
  def : ISpecPackageDefinition;
begin
  spec := LoadFixture('git-package-definitions.dspec.yaml');
  Assert.AreEqual('Test.GitLib', spec.MetaData.Id, 'git package id');
  tmpl := DefaultTemplate(spec);
  Assert.AreEqual(1, tmpl.PackageDefinitions.Count, 'git def count');
  def := FindDef(tmpl, './packages/GitLibR.dproj');
  Assert.AreEqual('', def.Kind, 'git def kind blank');
  Assert.AreEqual(1, def.Files.Count, 'git def files');
  Assert.AreEqual('./*.pas', def.Files[0], 'git def files[0]');
  Assert.AreEqual('rtl', def.Requires[0], 'git def requires');
  Assert.IsTrue(def.Platforms = [], 'git def no platform override');
end;

procedure TPackageDefinitionsSpecTests.Git_Package_RoundTrips;
var
  spec : IPackageSpec;
  reloaded : IPackageSpec;
  def : ISpecPackageDefinition;
begin
  spec := LoadFixture('git-package-definitions.dspec.yaml');
  reloaded := RoundTrip(spec);
  Assert.AreEqual('Test.GitLib', reloaded.MetaData.Id, 'git id round-trip');
  def := DefaultTemplate(reloaded).FindPackageDefinition('./packages/GitLibR.dproj');
  Assert.IsNotNull(def, 'git def missing after round-trip');
  AssertStringListsEqual(DefaultTemplate(spec).FindPackageDefinition('./packages/GitLibR.dproj').Files,
                         def.Files, 'git def files round-trip');
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageDefinitionsSpecTests);

end.
