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
  DPM.Creator.TargetPlatform.Collapse;

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

initialization
  TDUnitX.RegisterTestFixture(TSpecCollapseTests);

end.
