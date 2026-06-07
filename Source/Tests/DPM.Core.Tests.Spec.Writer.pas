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

// Asserts the spec-scaffolder writer only emits the $packageSource$ variable when
// the dproj folder differs per compiler; when all compilers share one folder it
// emits a literal path and no variable.

unit DPM.Core.Tests.Spec.Writer;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TSpecWriterTests = class
  published
    procedure SingleRootFolder_OmitsVariable_LiteralPath;
    procedure SharedNamedFolder_OmitsVariable_LiteralPath;
    procedure PerCompilerFolders_EmitsVariable_TokenPath;
    procedure MixedFolders_EmitsVariable_WithOverride;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Types,
  DPM.Console.Command.Spec.Writer;

function BaseScaffold : TSpecScaffold;
begin
  result := Default(TSpecScaffold);
  result.PackageId := 'Foo';
  result.Version := '0.1.0';
  result.Description := 'desc';
  result.Author := 'A';
  result.HasPackagesFolder := True;
  result.PackagesFolderRel := 'packages';
  SetLength(result.SourceGlobs, 1);
  result.SourceGlobs[0] := 'src/*.pas';
  SetLength(result.BuildDProjs, 1);
  result.BuildDProjs[0] := 'Foo.dproj';
  result.BuildPlatforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
end;

function Target(const compiler : TCompilerVersion; const literal : string; const template : string) : TSpecTargetInfo;
begin
  result.Compiler := compiler;
  result.Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
  result.PackageSourceLiteral := literal;
  result.PackageSourceTemplate := template;
end;

procedure TSpecWriterTests.SingleRootFolder_OmitsVariable_LiteralPath;
var
  scaffold : TSpecScaffold;
  yaml : string;
begin
  scaffold := BaseScaffold;
  SetLength(scaffold.Targets, 1);
  scaffold.Targets[0] := Target(TCompilerVersion.Delphi12_0, '', '');
  yaml := BuildSpecYaml(scaffold);

  Assert.IsFalse(ContainsText(yaml, 'packageSource'), 'no variable for a single shared folder');
  Assert.IsTrue(ContainsText(yaml, './packages/Foo.dproj'), 'literal packages-root path');
end;

procedure TSpecWriterTests.SharedNamedFolder_OmitsVariable_LiteralPath;
var
  scaffold : TSpecScaffold;
  yaml : string;
begin
  scaffold := BaseScaffold;
  SetLength(scaffold.Targets, 2);
  scaffold.Targets[0] := Target(TCompilerVersion.Delphi11_0, 'lib', 'lib');
  scaffold.Targets[1] := Target(TCompilerVersion.Delphi12_0, 'lib', 'lib');
  yaml := BuildSpecYaml(scaffold);

  Assert.IsFalse(ContainsText(yaml, 'packageSource'), 'no variable when both compilers share one folder');
  Assert.IsTrue(ContainsText(yaml, './packages/lib/Foo.dproj'), 'literal shared-folder path');
end;

procedure TSpecWriterTests.PerCompilerFolders_EmitsVariable_TokenPath;
var
  scaffold : TSpecScaffold;
  yaml : string;
begin
  scaffold := BaseScaffold;
  SetLength(scaffold.Targets, 2);
  scaffold.Targets[0] := Target(TCompilerVersion.Delphi11_0, 'Rad Studio 11.0', 'Rad Studio $compilernoprefix$');
  scaffold.Targets[1] := Target(TCompilerVersion.Delphi12_0, 'Rad Studio 12.0', 'Rad Studio $compilernoprefix$');
  yaml := BuildSpecYaml(scaffold);

  Assert.IsTrue(ContainsText(yaml, 'packageSource: "Rad Studio $compilernoprefix$"'), 'templated default value');
  Assert.IsTrue(ContainsText(yaml, './packages/$packageSource$/Foo.dproj'), 'token path');
end;

procedure TSpecWriterTests.MixedFolders_EmitsVariable_WithOverride;
var
  scaffold : TSpecScaffold;
  yaml : string;
begin
  scaffold := BaseScaffold;
  SetLength(scaffold.Targets, 2);
  scaffold.Targets[0] := Target(TCompilerVersion.Delphi11_0, 'old', 'old');
  scaffold.Targets[1] := Target(TCompilerVersion.Delphi12_0, 'new', 'new');
  yaml := BuildSpecYaml(scaffold);

  Assert.IsTrue(ContainsText(yaml, 'packageSource: old'), 'most-common default');
  Assert.IsTrue(ContainsText(yaml, 'packageSource: new'), 'per-target override');
  Assert.IsTrue(ContainsText(yaml, './packages/$packageSource$/Foo.dproj'), 'token path');
end;

initialization
  TDUnitX.RegisterTestFixture(TSpecWriterTests);

end.
