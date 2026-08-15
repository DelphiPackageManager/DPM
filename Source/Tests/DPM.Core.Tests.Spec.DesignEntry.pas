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

// Covers the "prebuilt design package" detection on design entries. A package that ships
// precompiled design-time binaries has no dproj to build - its design entry points straight
// at the .bpl inside the archive. The installer must recognise that and skip the dproj patch
// and the msbuild call (both of which fail hard on a PE file), treating the bpl as already built.

unit DPM.Core.Tests.Spec.DesignEntry;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TDesignEntrySpecTests = class
  published
    [Test]
    [TestCase('dproj is built',      'packages/MyDesign.dproj,False')]
    [TestCase('dpk is built',        'packages/MyDesign.dpk,False')]
    [TestCase('bpl is prebuilt',     'bpl/Win32/MyDesign370.bpl,True')]
    [TestCase('bpl case insensitive','bpl/Win32/MyDesign370.BPL,True')]
    [TestCase('backslashes',         'bpl\Win32\MyDesign370.bpl,True')]
    [TestCase('no extension',        'MyDesign,False')]
    procedure IsPrebuilt_Reflects_Project_Extension(const projectPath : string; const expected : boolean);

    [Test]
    procedure IsPrebuilt_Survives_Clone;

    [Test]
    procedure IsPrebuilt_Is_Derived_Not_Serialised;

    [Test]
    procedure ResolvePrebuiltPlatform_Uses_Declared_Platform;
    [Test]
    procedure ResolvePrebuiltPlatform_Requires_A_Declared_Platform;
    [Test]
    procedure ResolvePrebuiltPlatform_Rejects_Multiple_Declared_Platforms;
    [Test]
    procedure ResolvePrebuiltPlatform_Rejects_Non_Design_Host_Platform;
    [Test]
    procedure Two_Entries_One_Per_IDE_Bitness_Resolve_Independently;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  TestLogger;

const
  cDspecTemplate =
    'metadata:'#13#10 +
    //id/description deliberately avoid the word IsPrebuilt_Is_Derived_Not_Serialised greps for
    '  id: Test.ShippedDesign'#13#10 +
    '  version: 1.0.0'#13#10 +
    '  description: ships a design bpl'#13#10 +
    '  authors:'#13#10 +
    '    - Vincent Parrett'#13#10 +
    '  license: Apache-2.0'#13#10 +
    'targetPlatforms:'#13#10 +
    '  - compiler: 13.0'#13#10 +
    '    platforms: [%1:s]'#13#10 +
    '    template: default'#13#10 +
    'templates:'#13#10 +
    '  - name: default'#13#10 +
    '    source:'#13#10 +
    '      - src: bpl\Win32\*'#13#10 +
    '        dest: bpl'#13#10 +
    '    design:'#13#10 +
    '      - project: %0:s'#13#10 +
    '%2:s';

//designPlatformsLine is the entry's own `platforms:` line (indented, CRLF terminated) or '' to
//leave it out - that absence is exactly what ResolvePrebuiltPlatform has to disambiguate.
function LoadDesignEntryEx(const projectPath, targetPlatforms, designPlatformsLine : string) : ISpecDesignEntry;
var
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  template : ISpecTemplate;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(Format(cDspecTemplate, [projectPath, targetPlatforms, designPlatformsLine]));
  Assert.IsNotNull(spec, 'failed to read spec for project [' + projectPath + ']');
  template := spec.FindTemplate('default');
  Assert.IsNotNull(template, 'default template missing');
  Assert.AreEqual(1, template.DesignEntries.Count, 'expected exactly one design entry');
  result := template.DesignEntries[0];
end;

function LoadDesignEntry(const projectPath : string) : ISpecDesignEntry;
begin
  result := LoadDesignEntryEx(projectPath, 'Win32', '        platforms: [Win32]'#13#10);
end;

procedure TDesignEntrySpecTests.IsPrebuilt_Reflects_Project_Extension(const projectPath : string; const expected : boolean);
var
  designEntry : ISpecDesignEntry;
begin
  designEntry := LoadDesignEntry(projectPath);
  Assert.AreEqual(expected, designEntry.IsPrebuilt, 'IsPrebuilt for [' + projectPath + ']');
end;

procedure TDesignEntrySpecTests.IsPrebuilt_Survives_Clone;
var
  designEntry : ISpecDesignEntry;
  cloned : ISpecDesignEntry;
begin
  designEntry := LoadDesignEntry('bpl/Win32/MyDesign370.bpl');
  cloned := designEntry.Clone;
  Assert.IsTrue(cloned.IsPrebuilt, 'clone of a prebuilt entry should still be prebuilt');
end;

//The entry names the IDE bitness its binary loads into. Win64 here even though the package only
//targets Win32 - the two axes are independent, which is the whole reason the entry has to say.
procedure TDesignEntrySpecTests.ResolvePrebuiltPlatform_Uses_Declared_Platform;
var
  designEntry : ISpecDesignEntry;
  designPlatform : TDPMPlatform;
  error : string;
begin
  designEntry := LoadDesignEntryEx('bpl/Win64/MyDesign370.bpl', 'Win32', '        platforms: [Win64]'#13#10);
  Assert.IsTrue(designEntry.ResolvePrebuiltPlatform(designPlatform, error), error);
  Assert.IsTrue(designPlatform = TDPMPlatform.Win64, 'expected Win64, got ' + DPMPlatformToString(designPlatform));
end;

//A bpl only loads into an IDE of its own bitness, and targetPlatforms says nothing about IDE
//bitness - so a silent entry is always ambiguous, even for a single-platform package.
procedure TDesignEntrySpecTests.ResolvePrebuiltPlatform_Requires_A_Declared_Platform;
var
  designEntry : ISpecDesignEntry;
  designPlatform : TDPMPlatform;
  error : string;
begin
  designEntry := LoadDesignEntryEx('bpl/Win32/MyDesign370.bpl', 'Win32', '');
  Assert.IsTrue(designEntry.Platforms = [], 'fixture should leave the entry platforms unset');
  Assert.IsFalse(designEntry.ResolvePrebuiltPlatform(designPlatform, error), 'a silent prebuilt entry should not resolve');
  Assert.IsTrue(Pos('must declare a platform', error) > 0, 'unhelpful error: ' + error);
end;

//One bpl cannot serve both IDE bitnesses, so declaring both is wrong even though it looks explicit.
procedure TDesignEntrySpecTests.ResolvePrebuiltPlatform_Rejects_Multiple_Declared_Platforms;
var
  designEntry : ISpecDesignEntry;
  designPlatform : TDPMPlatform;
  error : string;
begin
  designEntry := LoadDesignEntryEx('bpl/Win32/MyDesign370.bpl', 'Win32, Win64', '        platforms: [Win32, Win64]'#13#10);
  Assert.IsFalse(designEntry.ResolvePrebuiltPlatform(designPlatform, error), 'one prebuilt bpl cannot serve both IDE bitnesses');
  Assert.IsTrue(Pos('single-platform binary', error) > 0, 'unhelpful error: ' + error);
end;

//The IDE only comes in Win32 and Win64 - a design bpl declared for anything else can never load.
procedure TDesignEntrySpecTests.ResolvePrebuiltPlatform_Rejects_Non_Design_Host_Platform;
var
  designEntry : ISpecDesignEntry;
  designPlatform : TDPMPlatform;
  error : string;
begin
  designEntry := LoadDesignEntryEx('bpl/Android64/MyDesign370.bpl', 'Android64', '        platforms: [Android64]'#13#10);
  Assert.IsFalse(designEntry.ResolvePrebuiltPlatform(designPlatform, error), 'a non-IDE platform should not resolve');
  Assert.IsTrue(Pos('must be Win32 or Win64', error) > 0, 'unhelpful error: ' + error);
end;

//Two entries, one per IDE bitness - the documented way to support both IDEs.
procedure TDesignEntrySpecTests.Two_Entries_One_Per_IDE_Bitness_Resolve_Independently;
var
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  template : ISpecTemplate;
  designPlatform : TDPMPlatform;
  error : string;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(Format(cDspecTemplate, ['bpl/Win32/MyDesign370.bpl', 'Win32',
                                       '        platforms: [Win32]'#13#10 +
                                       '      - project: bpl/Win64/MyDesign370.bpl'#13#10 +
                                       '        platforms: [Win64]'#13#10]));
  Assert.IsNotNull(spec, 'failed to read two-entry spec');
  template := spec.FindTemplate('default');
  Assert.AreEqual(2, template.DesignEntries.Count, 'expected one design entry per IDE bitness');

  Assert.IsTrue(template.DesignEntries[0].ResolvePrebuiltPlatform(designPlatform, error), error);
  Assert.IsTrue(designPlatform = TDPMPlatform.Win32, 'first entry should be the Win32 IDE binary');

  Assert.IsTrue(template.DesignEntries[1].ResolvePrebuiltPlatform(designPlatform, error), error);
  Assert.IsTrue(designPlatform = TDPMPlatform.Win64, 'second entry should be the Win64 IDE binary');
end;

//IsPrebuilt is derived from the project extension, not a stored yaml key - a round trip
//through GenerateDspecYAML must not introduce a new property, and must still be prebuilt.
procedure TDesignEntrySpecTests.IsPrebuilt_Is_Derived_Not_Serialised;
var
  reader : IPackageSpecReader;
  spec : IPackageSpec;
  reloaded : IPackageSpec;
  yaml : string;
  designEntry : ISpecDesignEntry;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(Format(cDspecTemplate, ['bpl/Win32/MyDesign370.bpl', 'Win32', '        platforms: [Win32]'#13#10]));
  Assert.IsNotNull(spec, 'failed to read spec');

  yaml := spec.GenerateDspecYAML(spec.MetaData.Version);
  Assert.IsTrue(Pos('prebuilt', LowerCase(yaml)) = 0, 'IsPrebuilt should not be serialised into the dspec, got:'#13#10 + yaml);
  Assert.IsTrue(Pos('project: bpl/Win32/MyDesign370.bpl', yaml) > 0, 'design entry project should round trip verbatim, got:'#13#10 + yaml);

  reloaded := reader.ReadSpecString(yaml);
  Assert.IsNotNull(reloaded, 'failed to reload generated yaml');
  designEntry := reloaded.FindTemplate('default').DesignEntries[0];
  Assert.IsTrue(designEntry.IsPrebuilt, 'prebuilt state should survive a dspec round trip');
end;

initialization
  TDUnitX.RegisterTestFixture(TDesignEntrySpecTests);

end.
