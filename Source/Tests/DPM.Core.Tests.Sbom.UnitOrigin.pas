unit DPM.Core.Tests.SBOM.UnitOrigin;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUnitSearchIndexTests = class
  private
    function MakeTempDir(const tag : string) : string;
    procedure WriteEmptyFile(const path : string);
  public
    [Test]
    procedure FindsPasInFirstAddedPath;
    [Test]
    procedure FindsDcuFallback;
    [Test]
    procedure FirstPathWinsOnCollision;
    [Test]
    procedure MissingUnitReturnsFalse;
    [Test]
    procedure OriginTagPropagates;
    [Test]
    procedure EmptyAndWhitespacePathsIgnored;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.StrUtils,
  DPM.Core.Project.SearchPaths;

{ TUnitSearchIndexTests }

function TUnitSearchIndexTests.MakeTempDir(const tag : string) : string;
var
  g : TGUID;
  guidStr : string;
begin
  CreateGUID(g);
  guidStr := LowerCase(Copy(GUIDToString(g), 2, 36));
  result := TPath.Combine(TPath.GetTempPath, 'dpm-uoi-' + tag + '-' + guidStr);
  ForceDirectories(result);
end;

procedure TUnitSearchIndexTests.WriteEmptyFile(const path : string);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(path, fmCreate);
  try
    //empty file - existence is the only thing the index checks
  finally
    fs.Free;
  end;
end;

procedure TUnitSearchIndexTests.FindsPasInFirstAddedPath;
var
  dir : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dir := MakeTempDir('finds');
  try
    WriteEmptyFile(TPath.Combine(dir, 'Foo.Bar.pas'));
    index := TUnitSearchIndex.Create;
    index.AddSearchPath(dir, TUnitOrigin.Other);
    Assert.IsTrue(index.TryFindUnit('Foo.Bar', found, origin));
    Assert.IsTrue(SameText(ExtractFileName(found), 'Foo.Bar.pas'));
  finally
    TDirectory.Delete(dir, true);
  end;
end;

procedure TUnitSearchIndexTests.FindsDcuFallback;
var
  dir : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dir := MakeTempDir('dcu');
  try
    WriteEmptyFile(TPath.Combine(dir, 'CompiledOnly.dcu'));
    index := TUnitSearchIndex.Create;
    index.AddSearchPath(dir, TUnitOrigin.Other);
    Assert.IsTrue(index.TryFindUnit('CompiledOnly', found, origin));
    Assert.IsTrue(EndsText('.dcu', found), 'expected dcu match, got ' + found);
  finally
    TDirectory.Delete(dir, true);
  end;
end;

procedure TUnitSearchIndexTests.FirstPathWinsOnCollision;
var
  dirA : string;
  dirB : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dirA := MakeTempDir('colA');
  dirB := MakeTempDir('colB');
  try
    WriteEmptyFile(TPath.Combine(dirA, 'Shared.pas'));
    WriteEmptyFile(TPath.Combine(dirB, 'Shared.pas'));
    index := TUnitSearchIndex.Create;
    index.AddSearchPath(dirA, TUnitOrigin.Project);
    index.AddSearchPath(dirB, TUnitOrigin.IdeLibrary);
    Assert.IsTrue(index.TryFindUnit('Shared', found, origin));
    Assert.AreEqual(IncludeTrailingPathDelimiter(dirA) + 'Shared.pas', found);
    Assert.AreEqual<TUnitOrigin>(TUnitOrigin.Project, origin, 'first-path-wins should preserve Project origin');
  finally
    TDirectory.Delete(dirA, true);
    TDirectory.Delete(dirB, true);
  end;
end;

procedure TUnitSearchIndexTests.MissingUnitReturnsFalse;
var
  dir : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dir := MakeTempDir('missing');
  try
    WriteEmptyFile(TPath.Combine(dir, 'Real.pas'));
    index := TUnitSearchIndex.Create;
    index.AddSearchPath(dir, TUnitOrigin.Other);
    Assert.IsFalse(index.TryFindUnit('Imaginary', found, origin));
    Assert.AreEqual('', found);
  finally
    TDirectory.Delete(dir, true);
  end;
end;

procedure TUnitSearchIndexTests.OriginTagPropagates;
var
  dir : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dir := MakeTempDir('orig');
  try
    WriteEmptyFile(TPath.Combine(dir, 'Vcl.Forms.pas'));
    index := TUnitSearchIndex.Create;
    index.AddSearchPath(dir, TUnitOrigin.IdeLibrary);
    Assert.IsTrue(index.TryFindUnit('Vcl.Forms', found, origin));
    Assert.AreEqual<TUnitOrigin>(TUnitOrigin.IdeLibrary, origin);
  finally
    TDirectory.Delete(dir, true);
  end;
end;

procedure TUnitSearchIndexTests.EmptyAndWhitespacePathsIgnored;
var
  dir : string;
  index : IUnitSearchIndex;
  found : string;
  origin : TUnitOrigin;
begin
  dir := MakeTempDir('emptypath');
  try
    WriteEmptyFile(TPath.Combine(dir, 'Real.pas'));
    index := TUnitSearchIndex.Create;
    //Whitespace / empty entries should be silently dropped, not crash the scan.
    index.AddSearchPath('', TUnitOrigin.Other);
    index.AddSearchPath('   ', TUnitOrigin.Other);
    index.AddSearchPath(dir, TUnitOrigin.Other);
    Assert.IsTrue(index.TryFindUnit('Real', found, origin));
  finally
    TDirectory.Delete(dir, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUnitSearchIndexTests);

end.
