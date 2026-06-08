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

// Round-trips the dspec fixture files in Tests\dspecs through load -> GenerateDspecYAML -> load and
// asserts every element survives unchanged. Also exercises the DSpecCreator expand/collapse path so
// the per-compiler editing model preserves the targetPlatform semantics regardless of the authoring
// form (single compiler / compiler from-to range / compilers list).

unit DPM.Core.Tests.Spec.RoundTrip;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TSpecRoundTripTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure SingleCompiler_RoundTrips;
    procedure CompilerRange_RoundTrips;
    procedure CompilerSet_RoundTrips;
    procedure Full_RoundTrips;

    procedure CompilerRange_Loads_Expected_Coverage;
    procedure CompilerSet_Loads_Expected_Coverage;
    procedure Full_Loads_All_Metadata;
    procedure Full_Loads_PrecompiledBinaries;
    procedure Full_Loads_EnvironmentVariables;
    procedure CompilerRange_Per_Platform_Variable_Overrides;

    procedure Creator_ExpandCollapse_Preserves_Range;
    procedure Creator_ExpandCollapse_Preserves_Set;
    procedure Creator_ExpandCollapse_Preserves_Full;

    procedure Bundled_Dependency_Alias_And_Sentinel_Load;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  Spring.Collections,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Dependency.Version,
  DPM.Creator.TargetPlatform.Collapse;

{ path helpers }

function DspecsDir : string;
var
  dir : string;
  candidate : string;
  i : integer;
begin
  result := '';
  dir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  //the exe can land in Output\ or Source\<platform>\<config>\ - walk up until we find the fixtures.
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

{ comparison helpers - call DUnitX Assert directly }

procedure AssertStringListsEqual(const expected, actual : IList<string>; const ctx : string);
var
  i : integer;
begin
  Assert.AreEqual(expected.Count, actual.Count, ctx + ' count');
  for i := 0 to expected.Count - 1 do
    Assert.AreEqual(expected[i], actual[i], ctx + ' item ' + IntToStr(i));
end;

procedure AssertTStringsEqual(const expected, actual : TStrings; const ctx : string);
var
  i : integer;
begin
  Assert.AreEqual(expected.Count, actual.Count, ctx + ' count');
  for i := 0 to expected.Count - 1 do
    Assert.AreEqual(expected[i], actual[i], ctx + ' item ' + IntToStr(i));
end;

procedure AssertVariablesEqual(const expected, actual : IVariables; const ctx : string);
var
  pair : TPair<string,string>;
  otherValue : string;
begin
  Assert.AreEqual(expected.Count, actual.Count, ctx + ' variable count');
  for pair in expected do
  begin
    Assert.IsTrue(actual.TryGetValue(pair.Key, otherValue), ctx + ' missing variable ' + pair.Key);
    Assert.AreEqual(pair.Value, otherValue, ctx + ' variable ' + pair.Key);
  end;
end;

procedure AssertMetaDataEqual(const expected, actual : ISpecMetaData);
var
  i : integer;
begin
  Assert.AreEqual(expected.Id, actual.Id, 'metadata.id');
  Assert.AreEqual(expected.Version.ToString, actual.Version.ToString, 'metadata.version');
  Assert.AreEqual(expected.Description, actual.Description, 'metadata.description');
  AssertStringListsEqual(expected.Authors, actual.Authors, 'metadata.authors');
  Assert.AreEqual(expected.ProjectUrl, actual.ProjectUrl, 'metadata.projectUrl');
  Assert.AreEqual(expected.RepositoryUrl, actual.RepositoryUrl, 'metadata.repositoryUrl');
  Assert.AreEqual(expected.RepositoryType, actual.RepositoryType, 'metadata.repositoryType');
  Assert.AreEqual(expected.RepositoryBranch, actual.RepositoryBranch, 'metadata.repositoryBranch');
  Assert.AreEqual(expected.RepositoryCommit, actual.RepositoryCommit, 'metadata.repositoryCommit');
  Assert.AreEqual(expected.ReleaseNotes, actual.ReleaseNotes, 'metadata.releaseNotes');
  Assert.AreEqual(expected.License, actual.License, 'metadata.license');
  Assert.AreEqual(expected.Icon, actual.Icon, 'metadata.icon');
  Assert.AreEqual(expected.Copyright, actual.Copyright, 'metadata.copyright');
  Assert.AreEqual(expected.ReadMe, actual.ReadMe, 'metadata.readme');
  Assert.IsTrue(expected.IsTrial = actual.IsTrial, 'metadata.isTrial');
  Assert.IsTrue(expected.IsCommercial = actual.IsCommercial, 'metadata.isCommercial');
  AssertTStringsEqual(expected.Tags, actual.Tags, 'metadata.tags');
  Assert.AreEqual(Length(expected.Frameworks), Length(actual.Frameworks), 'metadata.frameworks count');
  for i := 0 to Length(expected.Frameworks) - 1 do
    Assert.AreEqual(Ord(expected.Frameworks[i]), Ord(actual.Frameworks[i]), 'metadata.framework ' + IntToStr(i));
end;

function CoveringEntry(const spec : IPackageSpec; const compiler : TCompilerVersion) : ISpecTargetPlatform;
var
  i : integer;
begin
  result := nil;
  for i := 0 to spec.TargetPlatforms.Count - 1 do
    if spec.TargetPlatforms[i].IsForCompiler(compiler) then
      exit(spec.TargetPlatforms[i]);
end;

//Compare per-compiler so the comparison is independent of how compilers are grouped into entries
//(single / range / list) - exactly what we need for round-trip and expand/collapse.
procedure AssertTargetPlatformsEqual(const expected, actual : IPackageSpec);
var
  c : TCompilerVersion;
  expectedTP : ISpecTargetPlatform;
  actualTP : ISpecTargetPlatform;
begin
  for c := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if c = TCompilerVersion.UnknownVersion then
      continue;
    expectedTP := CoveringEntry(expected, c);
    actualTP := CoveringEntry(actual, c);
    if expectedTP = nil then
    begin
      Assert.IsTrue(actualTP = nil, 'compiler ' + CompilerToString(c) + ' should not be covered');
      continue;
    end;
    Assert.IsTrue(actualTP <> nil, 'compiler ' + CompilerToString(c) + ' missing after round-trip');
    Assert.IsTrue(expectedTP.Platforms = actualTP.Platforms, 'platforms differ for ' + CompilerToString(c));
    Assert.IsTrue(SameText(expectedTP.TemplateName, actualTP.TemplateName), 'template differs for ' + CompilerToString(c));
    AssertVariablesEqual(expectedTP.Variables, actualTP.Variables, 'targetPlatform ' + CompilerToString(c));
  end;
end;

procedure AssertTemplatesEqual(const expected, actual : IPackageSpec);
var
  i, j : integer;
  et, at : ISpecTemplate;
  ed, ad : ISpecDependency;
  es, asrc : ISpecSourceEntry;
  eb, ab : ISpecBuildEntry;
  edsn, adsn : ISpecDesignEntry;
  epd, apd : ISpecPackageDefinition;
  ctx : string;
begin
  Assert.AreEqual(expected.Templates.Count, actual.Templates.Count, 'template count');
  for i := 0 to expected.Templates.Count - 1 do
  begin
    et := expected.Templates[i];
    at := actual.FindTemplate(et.Name);
    Assert.IsTrue(at <> nil, 'template missing: ' + et.Name);

    ctx := 'template ' + et.Name;
    Assert.AreEqual(et.Dependencies.Count, at.Dependencies.Count, ctx + ' dependency count');
    for j := 0 to et.Dependencies.Count - 1 do
    begin
      ed := et.Dependencies[j];
      ad := at.FindDependency(ed.Id);
      Assert.IsTrue(ad <> nil, ctx + ' missing dependency ' + ed.Id);
      Assert.AreEqual(ed.Version.ToString, ad.Version.ToString, ctx + ' dependency version ' + ed.Id);
    end;

    Assert.AreEqual(et.SourceEntries.Count, at.SourceEntries.Count, ctx + ' source count');
    for j := 0 to et.SourceEntries.Count - 1 do
    begin
      es := et.SourceEntries[j];
      asrc := at.FindSourceEntry(es.Source);
      Assert.IsTrue(asrc <> nil, ctx + ' missing source ' + es.Source);
      Assert.AreEqual(es.Destination, asrc.Destination, ctx + ' source dest ' + es.Source);
      AssertStringListsEqual(es.Exclude, asrc.Exclude, ctx + ' source exclude ' + es.Source);
    end;

    Assert.AreEqual(et.BuildEntries.Count, at.BuildEntries.Count, ctx + ' build count');
    for j := 0 to et.BuildEntries.Count - 1 do
    begin
      eb := et.BuildEntries[j];
      ab := at.FindBuildEntry(eb.Project);
      Assert.IsTrue(ab <> nil, ctx + ' missing build ' + eb.Project);
      Assert.IsTrue(eb.Platforms = ab.Platforms, ctx + ' build platforms ' + eb.Project);
      Assert.AreEqual(eb.Defines, ab.Defines, ctx + ' build defines ' + eb.Project);
      AssertStringListsEqual(eb.References, ab.References, ctx + ' build references ' + eb.Project);
    end;

    Assert.AreEqual(et.DesignEntries.Count, at.DesignEntries.Count, ctx + ' design count');
    for j := 0 to et.DesignEntries.Count - 1 do
    begin
      edsn := et.DesignEntries[j];
      adsn := at.FindDesignEntry(edsn.Project);
      Assert.IsTrue(adsn <> nil, ctx + ' missing design ' + edsn.Project);
      Assert.IsTrue(edsn.Platforms = adsn.Platforms, ctx + ' design platforms ' + edsn.Project);
      Assert.AreEqual(edsn.Defines, adsn.Defines, ctx + ' design defines ' + edsn.Project);
      Assert.AreEqual(edsn.LibSuffix, adsn.LibSuffix, ctx + ' design libSuffix ' + edsn.Project);
      Assert.AreEqual(edsn.LibPrefix, adsn.LibPrefix, ctx + ' design libPrefix ' + edsn.Project);
      Assert.AreEqual(edsn.LibVersion, adsn.LibVersion, ctx + ' design libVersion ' + edsn.Project);
      AssertStringListsEqual(edsn.References, adsn.References, ctx + ' design references ' + edsn.Project);
    end;

    Assert.AreEqual(et.PackageDefinitions.Count, at.PackageDefinitions.Count, ctx + ' package definition count');
    for j := 0 to et.PackageDefinitions.Count - 1 do
    begin
      epd := et.PackageDefinitions[j];
      apd := at.FindPackageDefinition(epd.Project);
      Assert.IsTrue(apd <> nil, ctx + ' missing package definition ' + epd.Project);
      Assert.AreEqual(epd.Kind, apd.Kind, ctx + ' package definition kind ' + epd.Project);
      Assert.IsTrue(epd.Platforms = apd.Platforms, ctx + ' package definition platforms ' + epd.Project);
      AssertStringListsEqual(epd.Files, apd.Files, ctx + ' package definition files ' + epd.Project);
      AssertStringListsEqual(epd.Exclude, apd.Exclude, ctx + ' package definition exclude ' + epd.Project);
      AssertStringListsEqual(epd.Requires, apd.Requires, ctx + ' package definition requires ' + epd.Project);
    end;

    AssertStringListsEqual(et.PrecompiledBinaries, at.PrecompiledBinaries, ctx + ' precompiledBinaries');

    AssertVariablesEqual(et.EnvironmentVariables, at.EnvironmentVariables, ctx + ' environmentVariables');
  end;
end;

procedure AssertSpecsEqual(const expected, actual : IPackageSpec);
begin
  AssertMetaDataEqual(expected.MetaData, actual.MetaData);
  AssertVariablesEqual(expected.Variables, actual.Variables, 'package');
  AssertTargetPlatformsEqual(expected, actual);
  AssertTemplatesEqual(expected, actual);
end;

{ TSpecRoundTripTests }

procedure TSpecRoundTripTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TSpecRoundTripTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TSpecRoundTripTests.SingleCompiler_RoundTrips;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('single-compiler.dspec.yaml');
  AssertSpecsEqual(spec, RoundTrip(spec));
end;

procedure TSpecRoundTripTests.CompilerRange_RoundTrips;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('compiler-range.dspec.yaml');
  AssertSpecsEqual(spec, RoundTrip(spec));
end;

procedure TSpecRoundTripTests.CompilerSet_RoundTrips;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('compiler-set.dspec.yaml');
  AssertSpecsEqual(spec, RoundTrip(spec));
end;

procedure TSpecRoundTripTests.Full_RoundTrips;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('full.dspec.yaml');
  AssertSpecsEqual(spec, RoundTrip(spec));
end;

procedure TSpecRoundTripTests.CompilerRange_Loads_Expected_Coverage;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('compiler-range.dspec.yaml');
  //XE2..11.0 covered by the first entry, 12.0..13.0 by the second; nothing outside.
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.DelphiXE2) <> nil, 'XE2 covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi11_0) <> nil, '11.0 covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi12_0) <> nil, '12.0 covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi13_0) <> nil, '13.0 covered');
  //the two entries differ on platforms and the per-platform variable override.
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.DelphiXE2).Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64], 'legacy platforms');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi12_0).Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Linux64], 'modern platforms');
end;

procedure TSpecRoundTripTests.CompilerSet_Loads_Expected_Coverage;
var
  spec : IPackageSpec;
begin
  spec := LoadFixture('compiler-set.dspec.yaml');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.DelphiXE2) <> nil, 'XE2 covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.DelphiXE7) <> nil, 'XE7 covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi12_0) <> nil, '12.0 covered');
  //the gaps between the discrete compilers are not covered.
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.DelphiXE4) = nil, 'XE4 not covered');
  Assert.IsTrue(CoveringEntry(spec, TCompilerVersion.Delphi11_0) = nil, '11.0 not covered');
end;

procedure TSpecRoundTripTests.Full_Loads_All_Metadata;
var
  spec : IPackageSpec;
  md : ISpecMetaData;
begin
  spec := LoadFixture('full.dspec.yaml');
  md := spec.MetaData;
  Assert.AreEqual('Test.Full', md.Id);
  Assert.AreEqual('git', md.RepositoryType);
  Assert.AreEqual('master', md.RepositoryBranch);
  Assert.AreEqual('Initial release', md.ReleaseNotes);
  Assert.IsTrue(md.IsCommercial, 'isCommercial');
  Assert.IsFalse(md.IsTrial, 'isTrial');
  Assert.AreEqual(2, Length(md.Frameworks), 'frameworks');
  Assert.AreEqual(2, spec.Variables.Count, 'package variables');
end;

procedure TSpecRoundTripTests.Full_Loads_PrecompiledBinaries;
var
  spec : IPackageSpec;
  template : ISpecTemplate;
begin
  spec := LoadFixture('full.dspec.yaml');
  template := spec.FindTemplate('default');
  Assert.IsNotNull(template, 'default template');
  Assert.AreEqual(2, template.PrecompiledBinaries.Count, 'precompiledBinaries count');
  Assert.AreEqual('lib/Win32/Test.Full.bpl', template.PrecompiledBinaries[0], 'first binary');
  Assert.AreEqual('lib/Win32/Test.FullDesign.bpl', template.PrecompiledBinaries[1], 'second binary');
  //the legacy template declares none
  Assert.AreEqual(0, spec.FindTemplate('legacy').PrecompiledBinaries.Count, 'legacy precompiledBinaries empty');
end;

procedure TSpecRoundTripTests.Full_Loads_EnvironmentVariables;
var
  spec : IPackageSpec;
  template : ISpecTemplate;
  value : string;
begin
  spec := LoadFixture('full.dspec.yaml');
  template := spec.FindTemplate('default');
  Assert.IsNotNull(template, 'default template');
  Assert.AreEqual(2, template.EnvironmentVariables.Count, 'environment variable count');
  Assert.IsTrue(template.EnvironmentVariables.TryGetValue('TESTFULLDIR', value), 'TESTFULLDIR present');
  Assert.AreEqual('$packageDir$', value, 'TESTFULLDIR value (deferred token preserved)');
  Assert.IsTrue(template.EnvironmentVariables.TryGetValue('PATH', value), 'PATH present');
  Assert.AreEqual('$packageDir$\bin', value, 'PATH value');
end;

procedure TSpecRoundTripTests.CompilerRange_Per_Platform_Variable_Overrides;
var
  spec : IPackageSpec;
  legacy, modern : ISpecTargetPlatform;
  value : string;
begin
  spec := LoadFixture('compiler-range.dspec.yaml');
  legacy := CoveringEntry(spec, TCompilerVersion.DelphiXE2);
  modern := CoveringEntry(spec, TCompilerVersion.Delphi12_0);
  Assert.IsTrue(legacy.Variables.TryGetValue('extradefines', value) and (value = 'LEGACY'), 'legacy override');
  Assert.IsTrue(modern.Variables.TryGetValue('extradefines', value) and (value = 'MODERN'), 'modern override');
end;

procedure CheckExpandCollapse(const fixtureName : string);
var
  reader : IPackageSpecReader;
  dir : string;
  original : IPackageSpec;
  edited : IPackageSpec;
  collapsed : IList<ISpecTargetPlatform>;
begin
  dir := DspecsDir;
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  original := reader.ReadSpec(dir + fixtureName);
  edited := reader.ReadSpec(dir + fixtureName);
  Assert.IsNotNull(original);
  Assert.IsNotNull(edited);

  //simulate the DSpecCreator load -> edit -> save path.
  ExpandTargetPlatforms(edited);
  collapsed := CollapseTargetPlatforms(edited);
  edited.TargetPlatforms.Clear;
  edited.TargetPlatforms.AddRange(collapsed);

  //per-compiler semantics must be identical to the freshly loaded original.
  AssertTargetPlatformsEqual(original, edited);
end;

procedure TSpecRoundTripTests.Creator_ExpandCollapse_Preserves_Range;
begin
  CheckExpandCollapse('compiler-range.dspec.yaml');
end;

procedure TSpecRoundTripTests.Creator_ExpandCollapse_Preserves_Set;
begin
  CheckExpandCollapse('compiler-set.dspec.yaml');
end;

procedure TSpecRoundTripTests.Creator_ExpandCollapse_Preserves_Full;
begin
  CheckExpandCollapse('full.dspec.yaml');
end;

procedure TSpecRoundTripTests.Bundled_Dependency_Alias_And_Sentinel_Load;
const
  cYaml =
    'metadata:'#13#10 +
    '  id: Test.Bundled'#13#10 +
    '  version: 1.0.0'#13#10 +
    '  description: bundled dep test'#13#10 +
    '  authors:'#13#10 +
    '    - Vincent Parrett'#13#10 +
    '  license: Apache-2.0'#13#10 +
    'targetPlatforms:'#13#10 +
    '  - compiler: 12.0'#13#10 +
    '    platforms: [Win32, Win64]'#13#10 +
    '    template: default'#13#10 +
    'templates:'#13#10 +
    '  - name: default'#13#10 +
    '    dependencies:'#13#10 +
    '      - id: Test.IndyAlias'#13#10 +
    '        version: bundled'#13#10 +
    '      - id: Test.IndySentinel'#13#10 +
    '        version: 999.999.999'#13#10;

  function FindDep(const spec : IPackageSpec; const id : string) : ISpecDependency;
  var
    dep : ISpecDependency;
  begin
    result := nil;
    for dep in spec.Templates[0].Dependencies do
      if SameText(dep.Id, id) then
        exit(dep);
  end;

  procedure AssertBothBundled(const spec : IPackageSpec; const ctx : string);
  var
    aliasDep : ISpecDependency;
    sentinelDep : ISpecDependency;
  begin
    Assert.IsNotNull(spec, ctx + ': spec should load');
    aliasDep := FindDep(spec, 'Test.IndyAlias');
    sentinelDep := FindDep(spec, 'Test.IndySentinel');
    Assert.IsNotNull(aliasDep, ctx + ': alias dependency should be present');
    Assert.IsNotNull(sentinelDep, ctx + ': sentinel dependency should be present');
    Assert.IsTrue(aliasDep.Version.IsBundledSentinel, ctx + ': "bundled" alias must load as the sentinel range');
    Assert.IsTrue(sentinelDep.Version.IsBundledSentinel, ctx + ': "999.999.999" must load as the sentinel range');
  end;

var
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  reloaded : IPackageSpec;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(cYaml);
  AssertBothBundled(spec, 'initial load');

  //round-trip: generate yaml and reload - the sentinel must survive serialization.
  reloaded := RoundTrip(spec);
  AssertBothBundled(reloaded, 'round-trip');
end;

initialization
  TDUnitX.RegisterTestFixture(TSpecRoundTripTests);

end.
