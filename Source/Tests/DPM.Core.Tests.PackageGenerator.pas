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

// Exercises the install-time package-project generator: rendering the dpk/dproj from the
// embedded resource templates, kind inference, exclude filtering, and deterministic GUIDs.

unit DPM.Core.Tests.PackageGenerator;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TPackageGeneratorTests = class
  private
    FRoot : string;
    procedure WriteFile(const relPath, content : string);
    function ReadGenerated(const relPath : string) : string;
    function MakeSpec(const packageDefsYaml : string) : IInterface;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    procedure Runtime_Generates_Dpk_And_Dproj;
    procedure Runtime_Excludes_Matching_Files;
    procedure Design_Kind_Inferred_From_Designide;
    procedure Explicit_Kind_Overrides_Inference;
    procedure Generated_Dproj_Guid_Is_Deterministic;
    procedure Empty_Files_Match_Fails;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.IOUtils,
  VSoft.CancellationToken,
  TestLogger,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Project.PackageGenerator;

const
  cSpecPrefix =
    'min dpm client version: 1.0.0'#13#10 +
    'metadata:'#13#10 +
    '  id: Test.Gen'#13#10 +
    '  version: 1.0.0'#13#10 +
    'targetPlatforms:'#13#10 +
    '  - compiler: 12.0'#13#10 +
    '    platforms: [Win32, Win64]'#13#10 +
    '    template: default'#13#10 +
    'templates:'#13#10 +
    '  - name: default'#13#10 +
    '    source:'#13#10 +
    '      - src: ./src/*.pas'#13#10 +
    '    package definitions:'#13#10;

procedure TPackageGeneratorTests.Setup;
begin
  CoInitialize(nil);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm_gen_' + TPath.GetGUIDFileName);
  TDirectory.CreateDirectory(TPath.Combine(FRoot, 'src'));
  //runtime + design source units; Test.Foo is a candidate for exclude testing.
  WriteFile('src\Foo.pas', 'unit Foo;'#13#10'interface'#13#10'implementation'#13#10'end.'#13#10);
  WriteFile('src\Bar.pas', 'unit Bar;'#13#10'interface'#13#10'implementation'#13#10'end.'#13#10);
  WriteFile('src\Test.Foo.pas', 'unit Test.Foo;'#13#10'interface'#13#10'implementation'#13#10'end.'#13#10);
end;

procedure TPackageGeneratorTests.TearDown;
begin
  try
    if (FRoot <> '') and TDirectory.Exists(FRoot) then
      TDirectory.Delete(FRoot, true);
  except
    //best effort cleanup
  end;
  CoUninitialize;
end;

procedure TPackageGeneratorTests.WriteFile(const relPath, content : string);
var
  fullPath : string;
begin
  fullPath := TPath.Combine(FRoot, relPath);
  TDirectory.CreateDirectory(ExtractFilePath(fullPath));
  TFile.WriteAllText(fullPath, content, TEncoding.UTF8);
end;

function TPackageGeneratorTests.ReadGenerated(const relPath : string) : string;
begin
  result := TFile.ReadAllText(TPath.Combine(FRoot, relPath));
end;

function TPackageGeneratorTests.MakeSpec(const packageDefsYaml : string) : IInterface;
var
  reader : IPackageSpecReader;
  spec : IPackageSpec;
begin
  reader := TPackageSpecReader.Create(TTestLogger.Create);
  spec := reader.ReadSpecString(cSpecPrefix + packageDefsYaml);
  Assert.IsNotNull(spec, 'failed to parse test spec');
  Assert.IsTrue(spec.IsValid, 'test spec is not valid');
  result := spec;
end;

function RunGenerator(const spec : IPackageSpec; const root : string) : boolean;
var
  generator : IPackageProjectGenerator;
  token : ICancellationToken;
  template : ISpecTemplate;
begin
  generator := TPackageProjectGenerator.Create(TTestLogger.Create);
  token := TCancellationTokenSourceFactory.Create.Token;
  template := spec.FindTemplate('default');
  result := generator.Generate(token, spec, template, root, TCompilerVersion.Delphi12_0,
                               [TDPMPlatform.Win32, TDPMPlatform.Win64]);
end;

procedure TPackageGeneratorTests.Runtime_Generates_Dpk_And_Dproj;
var
  spec : IPackageSpec;
  dpk, dproj : string;
begin
  spec := MakeSpec(
    '      - project: ./pkg/GenR.dproj'#13#10 +
    '        kind: runtime'#13#10 +
    '        requires: [vcl]'#13#10 +
    '        files: [./src/*.pas]'#13#10) as IPackageSpec;

  Assert.IsTrue(RunGenerator(spec, FRoot), 'generator should succeed');

  Assert.IsTrue(TFile.Exists(TPath.Combine(FRoot, 'pkg\GenR.dpk')), 'dpk not generated');
  Assert.IsTrue(TFile.Exists(TPath.Combine(FRoot, 'pkg\GenR.dproj')), 'dproj not generated');

  dpk := ReadGenerated('pkg\GenR.dpk');
  Assert.IsTrue(Pos('{$RUNONLY}', dpk) > 0, 'runtime dpk must be {$RUNONLY}');
  Assert.IsTrue(Pos('package GenR;', dpk) > 0, 'dpk package name');
  Assert.IsTrue(Pos('Foo in', dpk) > 0, 'Foo unit included');
  Assert.IsTrue(Pos('Bar in', dpk) > 0, 'Bar unit included');
  Assert.IsTrue(Pos('vcl', dpk) > 0, 'vcl in requires');

  dproj := ReadGenerated('pkg\GenR.dproj');
  Assert.IsTrue(Pos('<RuntimeOnlyPackage>true</RuntimeOnlyPackage>', dproj) > 0, 'runtime dproj flag');
  Assert.IsTrue(Pos('Win32', dproj) > 0, 'Win32 platform present');
  Assert.IsTrue(Pos('Win64', dproj) > 0, 'Win64 platform present');
end;

procedure TPackageGeneratorTests.Runtime_Excludes_Matching_Files;
var
  spec : IPackageSpec;
  dpk : string;
begin
  spec := MakeSpec(
    '      - project: ./pkg/GenR.dproj'#13#10 +
    '        kind: runtime'#13#10 +
    '        files: [./src/*.pas]'#13#10 +
    '        exclude: ["Test*.pas"]'#13#10) as IPackageSpec;

  Assert.IsTrue(RunGenerator(spec, FRoot), 'generator should succeed');
  dpk := ReadGenerated('pkg\GenR.dpk');
  Assert.IsTrue(Pos('Foo in', dpk) > 0, 'Foo included');
  Assert.IsTrue(Pos('Test.Foo in', dpk) = 0, 'Test.Foo must be excluded');
end;

procedure TPackageGeneratorTests.Design_Kind_Inferred_From_Designide;
var
  spec : IPackageSpec;
  dpk, dproj : string;
begin
  //no explicit kind, but designide in requires => inferred design.
  spec := MakeSpec(
    '      - project: ./pkg/GenD.dproj'#13#10 +
    '        requires: [designide]'#13#10 +
    '        files: [./src/*.pas]'#13#10) as IPackageSpec;

  Assert.IsTrue(RunGenerator(spec, FRoot), 'generator should succeed');
  dpk := ReadGenerated('pkg\GenD.dpk');
  dproj := ReadGenerated('pkg\GenD.dproj');
  Assert.IsTrue(Pos('{$DESIGNONLY}', dpk) > 0, 'inferred design dpk must be {$DESIGNONLY}');
  Assert.IsTrue(Pos('designide', dpk) > 0, 'designide in requires');
  Assert.IsTrue(Pos('<DesignOnlyPackage>true</DesignOnlyPackage>', dproj) > 0, 'design dproj flag');
end;

procedure TPackageGeneratorTests.Explicit_Kind_Overrides_Inference;
var
  spec : IPackageSpec;
  dpk : string;
begin
  //requires designide would infer design, but explicit kind: runtime wins.
  spec := MakeSpec(
    '      - project: ./pkg/GenR.dproj'#13#10 +
    '        kind: runtime'#13#10 +
    '        requires: [designide]'#13#10 +
    '        files: [./src/*.pas]'#13#10) as IPackageSpec;

  Assert.IsTrue(RunGenerator(spec, FRoot), 'generator should succeed');
  dpk := ReadGenerated('pkg\GenR.dpk');
  Assert.IsTrue(Pos('{$RUNONLY}', dpk) > 0, 'explicit runtime must win over designide inference');
end;

procedure TPackageGeneratorTests.Generated_Dproj_Guid_Is_Deterministic;
var
  spec : IPackageSpec;
  first, second : string;
begin
  spec := MakeSpec(
    '      - project: ./pkg/GenR.dproj'#13#10 +
    '        kind: runtime'#13#10 +
    '        files: [./src/*.pas]'#13#10) as IPackageSpec;

  Assert.IsTrue(RunGenerator(spec, FRoot), 'first run');
  first := ReadGenerated('pkg\GenR.dproj');
  //regenerate (always-overwrite) - output must be byte-identical, including the GUID.
  Assert.IsTrue(RunGenerator(spec, FRoot), 'second run');
  second := ReadGenerated('pkg\GenR.dproj');
  Assert.AreEqual(first, second, 'regenerated dproj must be byte-identical (deterministic GUID)');
end;

procedure TPackageGeneratorTests.Empty_Files_Match_Fails;
var
  spec : IPackageSpec;
begin
  //a glob that matches nothing must fail the definition (no usable bpl).
  spec := MakeSpec(
    '      - project: ./pkg/GenR.dproj'#13#10 +
    '        kind: runtime'#13#10 +
    '        files: [./src/*.nonexistent]'#13#10) as IPackageSpec;

  Assert.IsFalse(RunGenerator(spec, FRoot), 'generator must fail when no files match');
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageGeneratorTests);

end.
