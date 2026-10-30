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
{                                                                           }
{***************************************************************************}

unit DPM.Core.Tests.Spec.Collapse;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TSpecCollapseTests = class
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;
  published
    procedure Expand_Range_Produces_One_Entry_Per_Compiler;
    procedure Collapse_Contiguous_Produces_Range;
    procedure Collapse_Splits_When_A_Compiler_Differs;
    procedure Collapse_NonContiguous_Produces_List;
    procedure Package_Variables_Survive_Round_Trip;
    procedure Variable_Names_Keep_Their_Case_Round_Trip;
    procedure Variable_Names_Are_Case_Insensitive;
    procedure Collapse_Ignores_Variable_Name_Case;
    procedure Prepare_Resolves_Variables_Whatever_Their_Case;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  Spring.Collections,
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Spec,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Project.Prepare.SourceFiles,
  DPM.Creator.TargetPlatform.Collapse;

const
  //variable names in mixed case, at both levels.
  cMixedCaseVariablesSpec =
    'metadata:' + sLineBreak +
    '  id: Test.VariableCase' + sLineBreak +
    '  version: 1.0.0' + sLineBreak +
    '  description: Variable names keep their case' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - Vincent Parrett' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'variables:' + sLineBreak +
    '  PackageSource: RAD Studio $compilernoprefix$' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: 12.0' + sLineBreak +
    '    platforms: [Win32, Win64]' + sLineBreak +
    '    template: default' + sLineBreak +
    '    variables:' + sLineBreak +
    '      ExtraDefines: MODERN' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: .\source\**.pas' + sLineBreak +
    '        dest: src' + sLineBreak;

//The name of the only variable in the list.
function OnlyVariableName(const variables : IVariables) : string;
var
  pair : TPair<string, string>;
begin
  Assert.AreEqual(1, variables.Count, 'one variable');
  result := '';
  for pair in variables do
    result := pair.Key;
end;

//Builds a single-compiler target platform entry.
function SingleTP(const logger : ILogger; const compiler : TCompilerVersion; const platforms : TDPMPlatforms) : ISpecTargetPlatform;
begin
  result := TSpecTargetPlatform.Create(logger);
  result.Compiler := compiler;
  result.Platforms := platforms;
  result.TemplateName := 'default';
end;

//Finds the entry in the list that covers the given compiler, or nil.
function EntryFor(const list : IList<ISpecTargetPlatform>; const compiler : TCompilerVersion) : ISpecTargetPlatform;
var
  i : integer;
begin
  result := nil;
  for i := 0 to list.Count - 1 do
    if list[i].IsForCompiler(compiler) then
      exit(list[i]);
end;

{ TSpecCollapseTests }

procedure TSpecCollapseTests.FixtureSetup;
begin
  CoInitialize(nil);
end;

procedure TSpecCollapseTests.FixtureTearDown;
begin
  CoUninitialize;
end;

procedure TSpecCollapseTests.Expand_Range_Produces_One_Entry_Per_Compiler;
var
  logger : ILogger;
  spec : IPackageSpec;
  tp : ISpecTargetPlatform;
  entry : ISpecTargetPlatform;
begin
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  //a single range entry XE2..XE6.
  tp := TSpecTargetPlatform.Create(logger);
  tp.MinCompiler := TCompilerVersion.DelphiXE2;
  tp.MaxCompiler := TCompilerVersion.DelphiXE6;
  tp.Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
  tp.TemplateName := 'default';
  spec.TargetPlatforms.Add(tp);

  ExpandTargetPlatforms(spec);

  //XE2,XE3,XE4,XE5,XE6 = 5 single-compiler entries.
  Assert.AreEqual(5, spec.TargetPlatforms.Count);
  entry := EntryFor(spec.TargetPlatforms, TCompilerVersion.DelphiXE4);
  Assert.IsNotNull(entry);
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE4), Ord(entry.Compiler), 'expanded entry should be single compiler');
  Assert.AreEqual(Ord(TCompilerVersion.UnknownVersion), Ord(entry.MinCompiler));
  Assert.AreEqual(Ord(TCompilerVersion.UnknownVersion), Ord(entry.MaxCompiler));
  Assert.IsTrue(entry.Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64], 'platforms carried to each compiler');
end;

procedure TSpecCollapseTests.Collapse_Contiguous_Produces_Range;
var
  logger : ILogger;
  spec : IPackageSpec;
  tp : ISpecTargetPlatform;
  collapsed : IList<ISpecTargetPlatform>;
  entry : ISpecTargetPlatform;
begin
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  tp := TSpecTargetPlatform.Create(logger);
  tp.MinCompiler := TCompilerVersion.DelphiXE2;
  tp.MaxCompiler := TCompilerVersion.DelphiXE6;
  tp.Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
  tp.TemplateName := 'default';
  spec.TargetPlatforms.Add(tp);
  ExpandTargetPlatforms(spec);

  collapsed := CollapseTargetPlatforms(spec);

  Assert.AreEqual(1, collapsed.Count, 'contiguous identical compilers collapse to a single range');
  entry := collapsed[0];
  Assert.AreEqual(Ord(TCompilerVersion.UnknownVersion), Ord(entry.Compiler), 'range entry has no single compiler');
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE2), Ord(entry.MinCompiler));
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE6), Ord(entry.MaxCompiler));
end;

procedure TSpecCollapseTests.Collapse_Splits_When_A_Compiler_Differs;
var
  logger : ILogger;
  spec : IPackageSpec;
  collapsed : IList<ISpecTargetPlatform>;
  rangeEntry : ISpecTargetPlatform;
  singleEntry : ISpecTargetPlatform;
begin
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  //XE2 and XE3 share platforms, XE4 has only Win32 - so XE4 must split out.
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.DelphiXE2, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.DelphiXE3, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.DelphiXE4, [TDPMPlatform.Win32]));

  collapsed := CollapseTargetPlatforms(spec);

  Assert.AreEqual(2, collapsed.Count, 'differing compiler splits the group');

  rangeEntry := EntryFor(collapsed, TCompilerVersion.DelphiXE2);
  Assert.IsNotNull(rangeEntry);
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE2), Ord(rangeEntry.MinCompiler));
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE3), Ord(rangeEntry.MaxCompiler));

  singleEntry := EntryFor(collapsed, TCompilerVersion.DelphiXE4);
  Assert.IsNotNull(singleEntry);
  Assert.AreEqual(Ord(TCompilerVersion.DelphiXE4), Ord(singleEntry.Compiler), 'odd one out stays a single compiler');
  Assert.IsTrue(singleEntry.Platforms = [TDPMPlatform.Win32]);
end;

procedure TSpecCollapseTests.Collapse_NonContiguous_Produces_List;
var
  logger : ILogger;
  spec : IPackageSpec;
  collapsed : IList<ISpecTargetPlatform>;
  entry : ISpecTargetPlatform;
begin
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  //XE2, XE7 and 12.0 are not contiguous but share config - collapse to a compilers list.
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.DelphiXE2, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.DelphiXE7, [TDPMPlatform.Win32, TDPMPlatform.Win64]));
  spec.TargetPlatforms.Add(SingleTP(logger, TCompilerVersion.Delphi12_0, [TDPMPlatform.Win32, TDPMPlatform.Win64]));

  collapsed := CollapseTargetPlatforms(spec);

  Assert.AreEqual(1, collapsed.Count, 'non-contiguous identical compilers collapse to one list');
  entry := collapsed[0];
  Assert.AreEqual(Ord(TCompilerVersion.UnknownVersion), Ord(entry.Compiler));
  Assert.AreEqual(Ord(TCompilerVersion.UnknownVersion), Ord(entry.MinCompiler), 'a list, not a range');
  Assert.AreEqual<integer>(3, Length(entry.Compilers));
end;

procedure TSpecCollapseTests.Package_Variables_Survive_Round_Trip;
var
  logger : ILogger;
  spec : IPackageSpec;
  reader : IPackageSpecReader;
  reloaded : IPackageSpec;
  yaml : string;
  value : string;
begin
  //regression: TSpec.ToYAML used to create the variables mapping but never write the values.
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  spec.MetaData.Id := 'Test.Pkg';
  spec.Variables['foo'] := 'bar';
  spec.Variables['baz'] := 'qux';
  Assert.AreEqual(2, spec.Variables.Count, 'variables set');

  //serialise then re-read - the writer's own output is guaranteed parseable.
  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  reader := TPackageSpecReader.Create(logger);
  reloaded := reader.ReadSpecString(yaml);
  Assert.IsNotNull(reloaded);
  Assert.AreEqual(2, reloaded.Variables.Count, 'variables written out and re-read');
  Assert.IsTrue(reloaded.Variables.TryGetValue('foo', value));
  Assert.AreEqual('bar', value);
end;

procedure TSpecCollapseTests.Variable_Names_Keep_Their_Case_Round_Trip;
var
  logger : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  reloaded : IPackageSpec;
  yaml : string;
begin
  //regression: the reader lowercased every variable name, so a load and save (DSpecCreator) wrote them back lowercased.
  logger := TTestLogger.Create;
  reader := TPackageSpecReader.Create(logger);
  spec := reader.ReadSpecString(cMixedCaseVariablesSpec);
  Assert.IsNotNull(spec, 'spec loaded');
  Assert.AreEqual('PackageSource', OnlyVariableName(spec.Variables), false, 'package variable as loaded');
  Assert.AreEqual('ExtraDefines', OnlyVariableName(spec.TargetPlatforms[0].Variables), false, 'target platform variable as loaded');

  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  Assert.IsTrue(Pos('PackageSource:', yaml) > 0, 'package variable written as authored');
  Assert.IsTrue(Pos('ExtraDefines:', yaml) > 0, 'target platform variable written as authored');

  reloaded := reader.ReadSpecString(yaml);
  Assert.IsNotNull(reloaded, 'spec reloaded');
  Assert.AreEqual('PackageSource', OnlyVariableName(reloaded.Variables), false, 'package variable after a round trip');
  Assert.AreEqual('ExtraDefines', OnlyVariableName(reloaded.TargetPlatforms[0].Variables), false, 'target platform variable after a round trip');
end;

procedure TSpecCollapseTests.Variable_Names_Are_Case_Insensitive;
var
  logger : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  value : string;
begin
  logger := TTestLogger.Create;
  reader := TPackageSpecReader.Create(logger);
  spec := reader.ReadSpecString(cMixedCaseVariablesSpec);
  Assert.IsNotNull(spec, 'spec loaded');
  Assert.IsTrue(spec.Variables.TryGetValue('packagesource', value), 'package variable found in another case');
  Assert.IsTrue(spec.TargetPlatforms[0].Variables.TryGetValue('EXTRADEFINES', value), 'target platform variable found in another case');
  Assert.AreEqual('MODERN', value);

  //the same name in another case is the same variable.
  spec.Variables['PACKAGESOURCE'] := 'Other';
  Assert.AreEqual(1, spec.Variables.Count, 'still one package variable');
  Assert.IsTrue(spec.Variables.TryGetValue('PackageSource', value));
  Assert.AreEqual('Other', value);
end;

procedure TSpecCollapseTests.Collapse_Ignores_Variable_Name_Case;
var
  logger : ILogger;
  spec : IPackageSpec;
  first : ISpecTargetPlatform;
  second : ISpecTargetPlatform;
  collapsed : IList<ISpecTargetPlatform>;
begin
  //12.0 and 13.0 differ only in how the variable name is cased - that is the same config.
  logger := TTestLogger.Create;
  spec := TSpec.Create(logger, '');
  first := SingleTP(logger, TCompilerVersion.Delphi12_0, [TDPMPlatform.Win32]);
  first.Variables['ExtraDefines'] := 'MODERN';
  second := SingleTP(logger, TCompilerVersion.Delphi13_0, [TDPMPlatform.Win32]);
  second.Variables['extradefines'] := 'MODERN';
  spec.TargetPlatforms.Add(first);
  spec.TargetPlatforms.Add(second);

  collapsed := CollapseTargetPlatforms(spec);

  Assert.AreEqual(1, collapsed.Count, 'variable names that differ only in case collapse');
end;

procedure TSpecCollapseTests.Prepare_Resolves_Variables_Whatever_Their_Case;
var
  logger : ILogger;
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  variables : IDictionary<string, string>;
  expected : string;
begin
  //PackageSource as written, found by a token in any case - and its own $compilernoprefix$ resolved too.
  logger := TTestLogger.Create;
  reader := TPackageSpecReader.Create(logger);
  spec := reader.ReadSpecString(cMixedCaseVariablesSpec);
  Assert.IsNotNull(spec, 'spec loaded');
  variables := TPrepareSourceGatherer.BuildVariables(spec, TCompilerVersion.Delphi12_0);
  expected := 'RAD Studio ' + CompilerNoPrefix(TCompilerVersion.Delphi12_0);
  Assert.AreEqual(expected, TPrepareSourceGatherer.ResolveVariables('$PACKAGESOURCE$', variables), false);
  Assert.AreEqual(expected, TPrepareSourceGatherer.ResolveVariables('$packagesource$', variables), false);
end;

initialization
  TDUnitX.RegisterTestFixture(TSpecCollapseTests);

end.
