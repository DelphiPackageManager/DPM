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

  Assert.AreEqual(1, Length(groups), 'RT/DT pair should collapse to one logical package');
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

  Assert.AreEqual(2, Length(groups), 'distinct stems must stay separate');
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
