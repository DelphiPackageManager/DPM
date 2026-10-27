unit DPM.Core.Tests.Repository.RequestCache;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.RequestCache;

type
  [TestFixture]
  TRepositoryRequestCacheTests = class
  private
    FCache : TRepositoryRequestCache;
    FVersions : IList<IPackageInfo>;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Nothing_Cached_Outside_A_Scope;

    [Test]
    procedure Repeated_Query_Is_A_Hit;

    [Test]
    procedure Different_Range_Prerelease_Or_Compiler_Is_A_Miss;

    [Test]
    procedure Hit_Returns_A_Fresh_List;

    [Test]
    procedure Nested_Scopes_Keep_Results_Until_Outermost_End;

    [Test]
    procedure Versions_Keyed_By_Id_Compiler_And_Prerelease;

    [Test]
    procedure Package_Info_Keyed_By_Id_Compiler_And_Version;
  end;

implementation

uses
  DPM.Core.Package.Classes;

const
  cCompiler = TCompilerVersion.Delphi12_0;

function Range(const value : string) : TVersionRange;
begin
  result := TVersionRange.Parse(value);
end;

{ TRepositoryRequestCacheTests }

procedure TRepositoryRequestCacheTests.Setup;
begin
  FCache := TRepositoryRequestCache.Create;
  FVersions := TCollections.CreateList<IPackageInfo>;
  FVersions.Add(TPackageInfo.Create('test-source', 'Base', TPackageVersion.Parse('1.0.3'), cCompiler, '', ''));
end;

procedure TRepositoryRequestCacheTests.Teardown;
begin
  FCache.Free;
  FCache := nil;
  FVersions := nil;
end;

procedure TRepositoryRequestCacheTests.Nothing_Cached_Outside_A_Scope;
var
  found : IList<IPackageInfo>;
begin
  FCache.AddVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, FVersions);
  Assert.IsFalse(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, found), 'outside a scope nothing is remembered');
end;

procedure TRepositoryRequestCacheTests.Repeated_Query_Is_A_Hit;
var
  found : IList<IPackageInfo>;
begin
  FCache.BeginScope;
  FCache.AddVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, FVersions);
  Assert.IsTrue(FCache.TryGetVersionsWithDependencies('base', cCompiler, Range('1.0.3'), false, found), 'same query (id is case insensitive) should hit');
  Assert.AreEqual(1, found.Count);
  Assert.AreEqual('1.0.3', found[0].Version.ToStringNoMeta);
  FCache.EndScope;
end;

procedure TRepositoryRequestCacheTests.Different_Range_Prerelease_Or_Compiler_Is_A_Miss;
var
  found : IList<IPackageInfo>;
begin
  FCache.BeginScope;
  FCache.AddVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, FVersions);
  Assert.IsFalse(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.1'), false, found), 'a different range is a different query');
  Assert.IsFalse(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), true, found), 'a different prerelease flag is a different query');
  Assert.IsFalse(FCache.TryGetVersionsWithDependencies('Base', TCompilerVersion.Delphi11_0, Range('1.0.3'), false, found), 'a different compiler is a different query');
  FCache.EndScope;
end;

procedure TRepositoryRequestCacheTests.Hit_Returns_A_Fresh_List;
var
  found : IList<IPackageInfo>;
begin
  FCache.BeginScope;
  FCache.AddVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, FVersions);
  FVersions.Clear; //the caller changing its list must not change what was remembered
  Assert.IsTrue(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, found));
  found.Clear; //nor must changing what it got back
  Assert.IsTrue(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, found));
  Assert.AreEqual(1, found.Count, 'each hit should return its own copy');
  FCache.EndScope;
end;

procedure TRepositoryRequestCacheTests.Nested_Scopes_Keep_Results_Until_Outermost_End;
var
  found : IList<IPackageInfo>;
begin
  FCache.BeginScope;
  FCache.BeginScope;
  FCache.AddVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, FVersions);
  FCache.EndScope;
  Assert.IsTrue(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, found), 'an inner EndScope keeps the results');
  FCache.EndScope;
  Assert.IsFalse(FCache.Active, 'the outermost EndScope ends caching');
  FCache.BeginScope;
  Assert.IsFalse(FCache.TryGetVersionsWithDependencies('Base', cCompiler, Range('1.0.3'), false, found), 'a new scope starts empty - nothing stale is served');
  FCache.EndScope;
end;

procedure TRepositoryRequestCacheTests.Versions_Keyed_By_Id_Compiler_And_Prerelease;
var
  versions : IList<TPackageVersion>;
  found : IList<TPackageVersion>;
begin
  versions := TCollections.CreateList<TPackageVersion>;
  versions.Add(TPackageVersion.Parse('1.0.3'));
  FCache.BeginScope;
  FCache.AddVersions('Ext', cCompiler, true, versions);
  Assert.IsTrue(FCache.TryGetVersions('EXT', cCompiler, true, found));
  Assert.AreEqual(1, found.Count);
  Assert.IsFalse(FCache.TryGetVersions('Ext', cCompiler, false, found), 'a different prerelease flag is a different query');
  Assert.IsFalse(FCache.TryGetVersions('Ext', TCompilerVersion.Delphi11_0, true, found), 'a different compiler is a different query');
  FCache.EndScope;
end;

procedure TRepositoryRequestCacheTests.Package_Info_Keyed_By_Id_Compiler_And_Version;
var
  info : IPackageInfo;
  found : IPackageInfo;
  identity : IPackageIdentity;
begin
  info := TPackageInfo.Create('src', 'Base', TPackageVersion.Parse('1.0.3'), cCompiler, '', '');
  FCache.AddPackageInfo(info, info);
  Assert.IsFalse(FCache.TryGetPackageInfo(info, found), 'nothing is kept outside a scope');

  FCache.BeginScope;
  FCache.AddPackageInfo(info, info);
  //held in a local - created inline for the const parameter it would never be freed.
  identity := TPackageIdentity.Create('', 'base', TPackageVersion.Parse('1.0.3'), cCompiler);
  Assert.IsTrue(FCache.TryGetPackageInfo(identity, found), 'the id is case insensitive');
  Assert.AreEqual('1.0.3', found.Version.ToStringNoMeta);
  identity := TPackageIdentity.Create('', 'Base', TPackageVersion.Parse('1.0.4'), cCompiler);
  Assert.IsFalse(FCache.TryGetPackageInfo(identity, found), 'another version');
  identity := TPackageIdentity.Create('', 'Base', TPackageVersion.Parse('1.0.3'), TCompilerVersion.Delphi11_0);
  Assert.IsFalse(FCache.TryGetPackageInfo(identity, found), 'another compiler');
  FCache.EndScope;
  Assert.IsFalse(FCache.TryGetPackageInfo(info, found), 'forgotten at the end of the scope');
end;

initialization
  TDUnitX.RegisterTestFixture(TRepositoryRequestCacheTests);

end.
