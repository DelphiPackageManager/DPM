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
{                                                                           }
{***************************************************************************}

// Exercises the runtime/design naming conventions used by the spec scaffolder to
// pair package projects into logical packages: ComputeLogicalPackageStem (suffix
// stripping) and GroupDProjsByStem (pairing). The key case is the delphimvcframework
// scheme - dmvcframeworkRT.dproj + dmvcframeworkDT.dproj must collapse to one package.

unit DPM.Core.Tests.Spec.Discovery;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TSpecDiscoveryTests = class
  published
    // stem stripping - recognised suffixes collapse to a shared stem
    [TestCase('RT suffix',            'dmvcframeworkRT.dproj,dmvcframework')]
    [TestCase('DT suffix',            'dmvcframeworkDT.dproj,dmvcframework')]
    [TestCase('R suffix',             'FooR.dproj,Foo')]
    [TestCase('D suffix',             'FooD.dproj,Foo')]
    [TestCase('Runtime word',         'BarRuntime.dproj,Bar')]
    [TestCase('DesignTime word',      'BarDesignTime.dproj,Bar')]
    [TestCase('Design word',          'BazDesign.dproj,Baz')]
    [TestCase('Design dot separator', 'Spring.Design.dproj,Spring')]
    [TestCase('DT underscore sep',    'MyLib_DT.dproj,MyLib')]
    procedure Stem_Strips_Known_Suffixes(const leaf : string; const expected : string);

    // guards - all-caps acronyms, digit forms and plain words must be left intact
    [TestCase('acronym CERT',         'MyLibCERT.dproj,MyLibCERT')]
    [TestCase('digit 4D',             'Spring4D.dproj,Spring4D')]
    [TestCase('plain word',           'VSoft.CancellationToken.dproj,VSoft.CancellationToken')]
    [TestCase('acronym HTTP',         'MyHTTP.dproj,MyHTTP')]
    [TestCase('lowercase rt',         'Report.dproj,Report')]
    procedure Stem_Leaves_NonConventional_Names(const leaf : string; const expected : string);

    procedure Grouping_RT_DT_Pair_Into_One_Package;
    procedure Grouping_Distinct_Stems_Stay_Separate;

    // flattening a logical package into individually-selectable dproj entries and
    // regrouping a chosen subset back into logical packages.
    procedure Flatten_Emits_Runtime_And_Design_Entries;
    procedure Flatten_Skips_Empty_Sides;
    procedure BuildSelectedLogicals_Regroups_Selected_By_Stem;
    procedure BuildSelectedLogicals_Partial_Selection_Keeps_Only_Chosen_Side;

    // source globs - disk-less fallback always emits folder globs.
    procedure Globs_Single_Folder_Collapses_To_One_Glob;
    procedure Globs_Multiple_Roots_Emit_One_Glob_Each_Not_Files;
    procedure Globs_Root_Level_File_Uses_Flat_Glob;

    // source globs - filesystem-aware: only glob a folder the package fully owns.
    procedure Globs_Subset_Of_Shared_Folder_Lists_Files;
    procedure Globs_Full_Ownership_Of_Folder_Uses_Glob;
    procedure Globs_Namespace_Prefix_Collapses_With_Excludes;

    // bare package-name stripping (used when emitting dependency ids) - must NOT
    // mistake a trailing '.Word' for a file extension.
    [TestCase('RT suffix',        'dmvcframeworkRT,dmvcframework')]
    [TestCase('R suffix',         'SomethingR,Something')]
    [TestCase('Design word',      'SomethingDesign,Something')]
    [TestCase('dotted base kept', 'Spring.Base,Spring.Base')]
    [TestCase('dotted + Design',  'Spring.BaseDesign,Spring.Base')]
    [TestCase('plain dotted',     'VSoft.CancellationToken,VSoft.CancellationToken')]
    procedure StripSuffix_Handles_Bare_PackageNames(const name : string; const expected : string);
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  DPM.Console.Command.Spec.Discovery;

procedure TSpecDiscoveryTests.Stem_Strips_Known_Suffixes(const leaf : string; const expected : string);
begin
  Assert.AreEqual(expected, ComputeLogicalPackageStem(leaf));
end;

procedure TSpecDiscoveryTests.Stem_Leaves_NonConventional_Names(const leaf : string; const expected : string);
begin
  Assert.AreEqual(expected, ComputeLogicalPackageStem(leaf));
end;

procedure TSpecDiscoveryTests.Grouping_RT_DT_Pair_Into_One_Package;
var
  dprojs : TArray<string>;
  groups : TLogicalPackages;
begin
  //the delphimvcframework scheme - one runtime + one design dproj, one package.
  dprojs := TArray<string>.Create('c:\repo\dmvcframeworkRT.dproj', 'c:\repo\dmvcframeworkDT.dproj');
  groups := GroupDProjsByStem(dprojs);

  Assert.AreEqual<integer>(1, Length(groups), 'RT/DT pair should collapse to one logical package');
  Assert.AreEqual('dmvcframework', groups[0].Stem);
  Assert.AreEqual('dmvcframeworkRT.dproj', ExtractFileName(groups[0].RuntimeDProj), 'runtime slot');
  Assert.AreEqual('dmvcframeworkDT.dproj', ExtractFileName(groups[0].DesignDProj), 'design slot');
end;

procedure TSpecDiscoveryTests.Grouping_Distinct_Stems_Stay_Separate;
var
  dprojs : TArray<string>;
  groups : TLogicalPackages;
begin
  //two genuinely different packages, each with a runtime + design dproj.
  dprojs := TArray<string>.Create(
    'c:\repo\Spring.BaseR.dproj', 'c:\repo\Spring.BaseDesign.dproj',
    'c:\repo\Spring.CoreR.dproj', 'c:\repo\Spring.CoreDesign.dproj');
  groups := GroupDProjsByStem(dprojs);

  Assert.AreEqual<integer>(2, Length(groups), 'distinct stems must stay separate');
end;

function MakeLogical(const stem, runtimeDProj, designDProj : string) : TLogicalPackage;
begin
  result.Stem := stem;
  result.RuntimeDProj := runtimeDProj;
  result.DesignDProj := designDProj;
end;

procedure TSpecDiscoveryTests.Flatten_Emits_Runtime_And_Design_Entries;
var
  packages : TLogicalPackages;
  entries : TSelectableDProjs;
begin
  SetLength(packages, 1);
  packages[0] := MakeLogical('MARS.Core', 'c:\repo\MARS.CoreR.dproj', 'c:\repo\MARS.CoreD.dproj');

  entries := FlattenSelectableDProjs(packages);

  Assert.AreEqual<integer>(2, Length(entries), 'runtime + design produce two entries');
  Assert.AreEqual('MARS.Core', entries[0].Stem);
  Assert.IsTrue(entries[0].Kind = dkRuntime, 'first entry is the runtime');
  Assert.AreEqual('MARS.CoreR.dproj', ExtractFileName(entries[0].Path));
  Assert.IsTrue(entries[1].Kind = dkDesign, 'second entry is the design');
  Assert.AreEqual('MARS.CoreD.dproj', ExtractFileName(entries[1].Path));
end;

procedure TSpecDiscoveryTests.Flatten_Skips_Empty_Sides;
var
  packages : TLogicalPackages;
  entries : TSelectableDProjs;
begin
  SetLength(packages, 2);
  packages[0] := MakeLogical('OnlyRuntime', 'c:\repo\OnlyRuntimeR.dproj', '');
  packages[1] := MakeLogical('OnlyDesign', '', 'c:\repo\OnlyDesignD.dproj');

  entries := FlattenSelectableDProjs(packages);

  Assert.AreEqual<integer>(2, Length(entries), 'one entry per non-empty dproj');
  Assert.IsTrue(entries[0].Kind = dkRuntime);
  Assert.IsTrue(entries[1].Kind = dkDesign);
end;

procedure TSpecDiscoveryTests.BuildSelectedLogicals_Regroups_Selected_By_Stem;
var
  packages : TLogicalPackages;
  entries : TSelectableDProjs;
  selected : TArray<integer>;
  logicals : TLogicalPackages;
begin
  SetLength(packages, 2);
  packages[0] := MakeLogical('MARS.Core', 'c:\repo\MARS.CoreR.dproj', 'c:\repo\MARS.CoreD.dproj');
  packages[1] := MakeLogical('MARS.Utils', 'c:\repo\MARS.UtilsR.dproj', 'c:\repo\MARS.UtilsD.dproj');
  entries := FlattenSelectableDProjs(packages); //4 entries: 0=CoreR,1=CoreD,2=UtilsR,3=UtilsD

  //select everything - regrouping must rebuild the two original logical packages.
  selected := TArray<integer>.Create(0, 1, 2, 3);
  logicals := BuildSelectedLogicals(entries, selected);

  Assert.AreEqual<integer>(2, Length(logicals), 'two stems -> two logical packages');
  Assert.AreEqual('MARS.Core', logicals[0].Stem);
  Assert.AreEqual('MARS.CoreR.dproj', ExtractFileName(logicals[0].RuntimeDProj));
  Assert.AreEqual('MARS.CoreD.dproj', ExtractFileName(logicals[0].DesignDProj));
  Assert.AreEqual('MARS.Utils', logicals[1].Stem);
  Assert.AreEqual('MARS.UtilsR.dproj', ExtractFileName(logicals[1].RuntimeDProj));
  Assert.AreEqual('MARS.UtilsD.dproj', ExtractFileName(logicals[1].DesignDProj));
end;

procedure TSpecDiscoveryTests.BuildSelectedLogicals_Partial_Selection_Keeps_Only_Chosen_Side;
var
  packages : TLogicalPackages;
  entries : TSelectableDProjs;
  selected : TArray<integer>;
  logicals : TLogicalPackages;
begin
  SetLength(packages, 1);
  packages[0] := MakeLogical('MARS.Core', 'c:\repo\MARS.CoreR.dproj', 'c:\repo\MARS.CoreD.dproj');
  entries := FlattenSelectableDProjs(packages); //0=runtime, 1=design

  //pick the design dproj only - the runtime side must stay empty.
  selected := TArray<integer>.Create(1);
  logicals := BuildSelectedLogicals(entries, selected);

  Assert.AreEqual<integer>(1, Length(logicals));
  Assert.AreEqual('MARS.Core', logicals[0].Stem);
  Assert.AreEqual('', logicals[0].RuntimeDProj, 'runtime not selected, stays empty');
  Assert.AreEqual('MARS.CoreD.dproj', ExtractFileName(logicals[0].DesignDProj));
end;

function GlobsContain(const globs : TScaffoldSourceEntries; const value : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(globs) do
    if SameText(globs[i].Glob, value) then
      exit(true);
end;

function FindGlob(const globs : TScaffoldSourceEntries; const value : string; out entry : TScaffoldSourceEntry) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(globs) do
    if SameText(globs[i].Glob, value) then
    begin
      entry := globs[i];
      exit(true);
    end;
end;

procedure TSpecDiscoveryTests.Globs_Single_Folder_Collapses_To_One_Glob;
var
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
begin
  units := TArray<string>.Create(
    'Source/MARS.Core.A.pas', 'Source/MARS.Core.B.pas', 'Source/MARS.Core.C.pas');
  globs := DeriveSourceGlobs(units, '');
  Assert.AreEqual<integer>(1, Length(globs), 'one shared folder -> one glob');
  Assert.AreEqual('Source/**.pas', globs[0].Glob);
end;

procedure TSpecDiscoveryTests.Globs_Multiple_Roots_Emit_One_Glob_Each_Not_Files;
var
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
  i : integer;
begin
  //the MARS.JOSE case - units spread across Source and ThirdParty must produce a
  //glob per top-level folder, never individual file entries.
  units := TArray<string>.Create(
    'Source/MARS.JOSEJWT.Token.pas',
    'Source/MARS.JOSEJWT.Token.InjectionService.pas',
    'ThirdParty/delphi-jose-jwt/Source/JOSE.Builder.pas',
    'ThirdParty/delphi-jose-jwt/Source/JOSE.Core.JWT.pas');
  globs := DeriveSourceGlobs(units, '');

  Assert.AreEqual<integer>(2, Length(globs), 'two roots -> two globs');
  Assert.IsTrue(GlobsContain(globs, 'Source/**.pas'), 'Source glob present');
  Assert.IsTrue(GlobsContain(globs, 'ThirdParty/delphi-jose-jwt/Source/**.pas'), 'ThirdParty glob present');
  //no entry may be an individual .pas file (i.e. a .pas not ending in **.pas)
  for i := 0 to High(globs) do
    Assert.IsTrue(EndsText('**.pas', globs[i].Glob), 'entry must be a glob, not a file: ' + globs[i].Glob);
end;

procedure TSpecDiscoveryTests.Globs_Root_Level_File_Uses_Flat_Glob;
var
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
begin
  //a unit at the project root plus units under Source - root file uses a flat
  //'*.pas' glob rather than being listed individually.
  units := TArray<string>.Create(
    'MARS.pas', 'Source/MARS.Core.A.pas', 'Source/MARS.Core.B.pas');
  globs := DeriveSourceGlobs(units, '');

  Assert.AreEqual<integer>(2, Length(globs));
  Assert.IsTrue(GlobsContain(globs, '*.pas'), 'flat root glob present');
  Assert.IsTrue(GlobsContain(globs, 'Source/**.pas'), 'Source glob present');
end;

//Creates a unique temp project root with a Source folder holding the given .pas
//leaf names. Caller must remove the returned folder.
function MakeTempSourceTree(const pasLeaves : TArray<string>) : string;
var
  root : string;
  srcDir : string;
  i : integer;
begin
  root := TPath.Combine(TPath.GetTempPath, 'dpm_globtest_' + TGUID.NewGuid.ToString.Replace('{', '').Replace('}', ''));
  srcDir := TPath.Combine(root, 'Source');
  TDirectory.CreateDirectory(srcDir);
  for i := 0 to High(pasLeaves) do
    TFile.WriteAllText(TPath.Combine(srcDir, pasLeaves[i]), 'unit ' + ChangeFileExt(pasLeaves[i], '') + ';');
  result := root;
end;

procedure TSpecDiscoveryTests.Globs_Subset_Of_Shared_Folder_Lists_Files;
var
  root : string;
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
begin
  //disk has three files; the package references only two - a './Source/**.pas'
  //glob would over-include the third, so we must list the two referenced files.
  root := MakeTempSourceTree(TArray<string>.Create('Pkg.A.pas', 'Pkg.B.pas', 'Pkg.C.pas'));
  try
    units := TArray<string>.Create('Source/Pkg.A.pas', 'Source/Pkg.B.pas');
    globs := DeriveSourceGlobs(units, root);

    Assert.AreEqual<integer>(2, Length(globs), 'subset must list its files, not glob the folder');
    Assert.IsTrue(GlobsContain(globs, 'Source/Pkg.A.pas'));
    Assert.IsTrue(GlobsContain(globs, 'Source/Pkg.B.pas'));
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TSpecDiscoveryTests.Globs_Full_Ownership_Of_Folder_Uses_Glob;
var
  root : string;
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
begin
  //the package references every .pas on disk in the folder - a single recursive
  //glob is exact, so use it rather than listing each file.
  root := MakeTempSourceTree(TArray<string>.Create('Pkg.A.pas', 'Pkg.B.pas', 'Pkg.C.pas'));
  try
    units := TArray<string>.Create('Source/Pkg.A.pas', 'Source/Pkg.B.pas', 'Source/Pkg.C.pas');
    globs := DeriveSourceGlobs(units, root);

    Assert.AreEqual<integer>(1, Length(globs), 'full ownership collapses to one glob');
    Assert.AreEqual('Source/**.pas', globs[0].Glob);
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TSpecDiscoveryTests.Globs_Namespace_Prefix_Collapses_With_Excludes;
var
  root : string;
  units : TArray<string>;
  globs : TScaffoldSourceEntries;
  entry : TScaffoldSourceEntry;
begin
  //the folder holds four 'Pkg.Core.*' files but
  //the package only owns three. Collapse to one 'Pkg.Core.*.pas' wildcard and
  //exclude the file that isn't ours, rather than listing every owned file.
  root := MakeTempSourceTree(TArray<string>.Create(
    'Pkg.Core.A.pas', 'Pkg.Core.B.pas', 'Pkg.Core.C.pas', 'Pkg.Core.D.pas'));
  try
    units := TArray<string>.Create('Source/Pkg.Core.A.pas', 'Source/Pkg.Core.B.pas', 'Source/Pkg.Core.C.pas');
    globs := DeriveSourceGlobs(units, root);

    Assert.AreEqual<integer>(1, Length(globs), 'collapses to a single namespace wildcard');
    Assert.IsTrue(FindGlob(globs, 'Source/Pkg.Core.*.pas', entry), 'namespace wildcard present');
    Assert.AreEqual<integer>(1, Length(entry.Exclude), 'the one unowned file is excluded');
    Assert.AreEqual('Source/Pkg.Core.D.pas', entry.Exclude[0]);
  finally
    TDirectory.Delete(root, true);
  end;
end;

procedure TSpecDiscoveryTests.StripSuffix_Handles_Bare_PackageNames(const name : string; const expected : string);
var
  kind : TDProjKind;
begin
  Assert.AreEqual(expected, StripRuntimeDesignSuffix(name, kind));
end;

initialization
  TDUnitX.RegisterTestFixture(TSpecDiscoveryTests);

end.
