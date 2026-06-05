unit DPM.Core.Tests.Dependency.Resolver;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  DUnitX.TestFramework,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Sources.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Resolution,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Package.Installer.Interfaces;

type
  ///<summary>Records every call the resolver makes against the repository manager so tests can
  /// distinguish "served from cache" from "went to the repo". Stores canned IList&lt;IPackageInfo&gt;
  /// per package id (lowercase). Anything not stubbed raises ENotImplemented.</summary>
  TFakeRepositoryManager = class(TInterfacedObject, IPackageRepositoryManager)
  private
    FCanned : IDictionary<string, IList<IPackageInfo>>;
    FCallCount : IDictionary<string, integer>;
  protected
    function Initialize(const configuration : IConfiguration) : boolean;
    function HasSources : boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean; const sources : string) : IPackageInfo;
    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
    function TryGetSourceType(const sourceName : string; out sourceType : TSourceType) : boolean;
    function InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean) : IList<IPackageInfo>;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>;
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function GetPackageIcon(const cancellationToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetInstalledPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
  public
    constructor Create;
    procedure SetVersions(const id : string; const versions : IList<IPackageInfo>);
    function GetCallCount(const id : string) : integer;
  end;

  ///<summary>Minimal IPackageCache: canned cached-versions lists per id, and a key-based
  /// GetPackageInfo lookup. Everything else raises ENotImplemented.</summary>
  TFakePackageCache = class(TInterfacedObject, IPackageCache)
  private
    FCanned : IDictionary<string, IList<IPackageInfo>>;
    FInfoByKey : IDictionary<string, IPackageInfo>;
  protected
    function GetLocation : string;
    procedure SetLocation(const value : string);
    function GetPackagesFolder : string;
    function Clean : boolean;
    function CreatePackagePath(const packageId : IPackageIdentity) : string;
    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string; overload;
    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;
    function EnsurePackage(const packageId : IPackageIdentity) : boolean;
    function InstallPackageFromFile(const packageFileName : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;
    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;
    function TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
    function GetPackageHash(const packageId : IPackageIdentity) : string;
    function FullReVerify : integer;
    function GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
    function RemovePackage(const packageId : IPackageIdentity) : boolean;
  public
    constructor Create;
    ///<summary>Register a package info under its identity key (id|version|compiler) so
    /// GetPackageInfo can return it during preferred-version resolution.</summary>
    procedure RegisterInfo(const info : IPackageInfo);
    procedure SetCachedVersions(const id : string; const versions : IList<IPackageInfo>);
  end;

  ///<summary>Extends the core installer context so we get FindPackageResolution / RecordResolutions
  /// for free. Tests can pre-populate FProjectResolutions via SeedResolutions to simulate sibling
  /// projects that have already resolved a package.</summary>
  TFakeInstallerContext = class(TCorePackageInstallerContext)
  public
    procedure SeedResolutions(const projectFile : string; const resolutions : TArray<IResolvedPackage>);
  end;

  [TestFixture]
  TDependencyResolverTests = class
  private
    FRepo : TFakeRepositoryManager;
    FCache : TFakePackageCache;
    FContext : TFakeInstallerContext;
    FRepoIntf : IPackageRepositoryManager;
    FCacheIntf : IPackageCache;
    FContextIntf : IPackageInstallerContext;
    FLogger : ILogger;
    FConfig : IConfiguration;
    FResolver : IDependencyResolver;
    FCancellation : ICancellationToken;
    FOptions : TSearchOptions;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Linear_Chain_Resolves;

    [Test]
    procedure PreferredVersion_Honoured_When_In_Range;

    [Test]
    procedure PreferredVersion_Falls_Through_When_Out_Of_Range;

    [Test]
    procedure Sibling_Conflict_NoOverlap_Records_Unresolvable;

    [Test]
    procedure Dependency_Range_Not_Mutated_After_Intersect;

    [Test]
    procedure BuildDependencyGraph_Tolerates_Partial_Resolve;

    //Additional baseline coverage of resolver behaviour as it stands today.
    [Test]
    procedure Empty_Dependencies_Resolves_Top_Level_Only;

    [Test]
    procedure Multiple_Dependencies_All_Resolve;

    [Test]
    procedure Diamond_Dependency_No_Conflict;

    [Test]
    procedure Diamond_Dependency_With_Overlap_Uses_Intersection;

    [Test]
    procedure Sibling_Conflict_With_Overlap_Uses_Intersection;

    [Test]
    procedure Top_Level_Conflict_Records_Unresolvable;

    [Test]
    procedure Missing_Repo_Package_Records_Unresolvable;

    [Test]
    procedure Prerelease_Excluded_By_Default;

    [Test]
    procedure Prerelease_Allowed_When_Parent_Is_Prerelease;

    [Test]
    procedure PreferredVersion_Missing_From_Cache_Falls_Through;

    [Test]
    procedure Cross_Project_Resolution_Reused_When_Range_Matches;

    [Test]
    procedure Narrowed_Range_Persists_Across_Reprocessing;

    [Test]
    procedure Cyclic_Dependency_Terminates_Without_Hang;

    [Test]
    procedure Iteration_Limit_Halts_Pathological_Input;

    [Test]
    procedure Prerelease_TopLevel_Promotes_Globally_For_Transitive_Deps;

    [Test]
    procedure Prerelease_Stable_And_Prerelease_Siblings_Share_Same_Transient;

    //Cache + repo interaction, deep graphs, multi-failure handling, shared-version-cache reuse.
    [Test]
    procedure Linear_Deep_Chain_Resolves;

    [Test]
    procedure Repo_Not_Called_When_Cache_Satisfies_Range;

    [Test]
    procedure Repo_Called_When_Cache_Empty_For_Id;

    [Test]
    procedure Repo_Called_When_Cache_Versions_All_Out_Of_Range;

    [Test]
    procedure SharedVersionCache_Avoids_Repo_Requery_Across_Resolves;

    [Test]
    procedure Failed_Dep_Doesnt_Block_Sibling_Dep_Resolution;

    [Test]
    procedure Prerelease_Option_Forces_Globally;

    [Test]
    procedure Cross_Project_Conflict_Outside_Range_Records_Unresolvable;

    [Test]
    procedure Group_Project_Version_Conflict_Counted_As_Error;

    [Test]
    procedure BuildDependencyGraph_Reflects_Resolved_Tree_Structure;

    [Test]
    procedure Resolved_Top_Level_Leaf_Does_Not_Cause_Extra_Requirements;

    [Test]
    procedure BuildDependencyGraph_Transitive_Range_Is_Parent_Declared_Range;

    [Test]
    procedure BuildDependencyGraph_Transitive_Range_Not_Collapsed_After_Promotion;

    [Test]
    procedure BuildDependencyGraph_Same_Transitive_Two_Parents_Records_Each_Declared_Range;
  end;

implementation

uses
  System.TypInfo,
  VSoft.CancellationToken.Impl,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Dependency,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Configuration.Classes,
  TestLogger;

const
  cTestCompiler = TCompilerVersion.Delphi12_0;
  cTestProject = 'C:\test\project.dproj';

//-----------------------------------------------------------------------------
// Helpers shared by tests
//-----------------------------------------------------------------------------

function MakeVersion(const s : string) : TPackageVersion;
begin
  if not TPackageVersion.TryParse(s, result) then
    raise Exception.Create('Bad test version: ' + s);
end;

function MakeRange(const s : string) : TVersionRange;
begin
  result := TVersionRange.Parse(s);
end;

function MakeDep(const id : string; const range : string) : IPackageDependency;
begin
  result := TPackageDependency.Create(id, MakeRange(range));
end;

///<summary>Builds an IPackageInfo with the given id/version/dependencies. Dependencies
/// are added in the order supplied. Empty array = leaf package.</summary>
function MakeInfo(const id : string; const version : string; const deps : array of IPackageDependency) : IPackageInfo;
var
  info : IPackageInfo;
  i : integer;
begin
  info := TPackageInfo.Create('test-source', id, MakeVersion(version), cTestCompiler, '', '');
  for i := 0 to High(deps) do
    info.Dependencies.Add(deps[i]);
  result := info;
end;

///<summary>Builds a top-level IPackageReference with PackageInfo populated. Parented to a fresh
/// root node so ParentId resolves to cRootNode (the marker the resolver checks for IsTopLevel).
/// Used to feed projectReferences into ResolveForInstall when simulating restore-style
/// multi-top-level resolves.</summary>
function MakeTopLevelRef(const info : IPackageInfo; const selectedOn : TVersionRange) : IPackageReference;
var
  root : IPackageReference;
begin
  root := TPackageReference.CreateRoot(info.CompilerVersion);
  result := TPackageReference.Create(root, info.Id, info.Version, info.CompilerVersion, selectedOn, false);
  result.PackageInfo := info;
end;

function ListOf(const items : array of IPackageInfo) : IList<IPackageInfo>;
var
  i : integer;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for i := 0 to High(items) do
    result.Add(items[i]);
end;

function MakeCacheKey(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion) : string;
begin
  result := LowerCase(id) + '|' + version.ToStringNoMeta + '|' + CompilerToString(compiler);
end;

//-----------------------------------------------------------------------------
// TFakeRepositoryManager
//-----------------------------------------------------------------------------

constructor TFakeRepositoryManager.Create;
begin
  inherited;
  FCanned := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FCallCount := TCollections.CreateDictionary<string, integer>;
end;

procedure TFakeRepositoryManager.SetVersions(const id : string; const versions : IList<IPackageInfo>);
begin
  FCanned[LowerCase(id)] := versions;
end;

function TFakeRepositoryManager.GetCallCount(const id : string) : integer;
begin
  if not FCallCount.TryGetValue(LowerCase(id), result) then
    result := 0;
end;

function TFakeRepositoryManager.Initialize(const configuration : IConfiguration) : boolean;
begin
  result := true;
end;

function TFakeRepositoryManager.HasSources : boolean;
begin
  result := true;
end;

function TFakeRepositoryManager.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                                   const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean) : IList<IPackageInfo>;
var
  key : string;
  all : IList<IPackageInfo>;
  filtered : IList<IPackageInfo>;
  info : IPackageInfo;
  prior : integer;
begin
  key := LowerCase(packageId);
  if FCallCount.TryGetValue(key, prior) then
    FCallCount[key] := prior + 1
  else
    FCallCount[key] := 1;

  filtered := TCollections.CreateList<IPackageInfo>;
  if not FCanned.TryGetValue(key, all) then
  begin
    result := filtered;
    exit;
  end;
  for info in all do
  begin
    if not versionRange.IsSatisfiedBy(info.Version) then
      continue;
    if (not includePrerelease) and (not info.Version.IsStable) then
      continue;
    filtered.Add(info);
  end;
  //Match the real TPackageRepositoryManager - descending sort so the resolver's first-match loop
  //picks the highest in-range version (including prereleases when allowed).
  filtered.Sort(TComparer<IPackageInfo>.Construct(
    function(const left, right : IPackageInfo) : integer
    begin
      result := right.Version.CompareTo(left.Version);
    end));
  result := filtered;
end;

function TFakeRepositoryManager.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.List');
end;

function TFakeRepositoryManager.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.Push');
end;

function TFakeRepositoryManager.FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean; const sources : string) : IPackageInfo;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.FindLatestVersion');
end;

function TFakeRepositoryManager.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.DownloadPackage');
end;

function TFakeRepositoryManager.TryGetSourceType(const sourceName : string; out sourceType : TSourceType) : boolean;
begin
  result := false;
end;

function TFakeRepositoryManager.InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.InstallPackageInPlace');
end;

function TFakeRepositoryManager.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetPackageInfo');
end;

function TFakeRepositoryManager.GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetPackageVersions');
end;

function TFakeRepositoryManager.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetPackageMetaData');
end;

function TFakeRepositoryManager.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetPackageFeed');
end;

function TFakeRepositoryManager.GetPackageIcon(const cancellationToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetPackageIcon');
end;

function TFakeRepositoryManager.GetInstalledPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
begin
  raise ENotImplemented.Create('TFakeRepositoryManager.GetInstalledPackageFeed');
end;

//-----------------------------------------------------------------------------
// TFakePackageCache
//-----------------------------------------------------------------------------

constructor TFakePackageCache.Create;
begin
  inherited;
  FCanned := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FInfoByKey := TCollections.CreateDictionary<string, IPackageInfo>;
end;

procedure TFakePackageCache.RegisterInfo(const info : IPackageInfo);
begin
  FInfoByKey[MakeCacheKey(info.Id, info.Version, info.CompilerVersion)] := info;
end;

procedure TFakePackageCache.SetCachedVersions(const id : string; const versions : IList<IPackageInfo>);
var
  info : IPackageInfo;
begin
  FCanned[LowerCase(id)] := versions;
  for info in versions do
    RegisterInfo(info);
end;

function TFakePackageCache.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  key : string;
begin
  key := MakeCacheKey(packageId.Id, packageId.Version, packageId.CompilerVersion);
  if not FInfoByKey.TryGetValue(key, result) then
    result := nil;
end;

function TFakePackageCache.GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                                    const id : string;
                                                                    const compilerVersion : TCompilerVersion;
                                                                    const versionRange : TVersionRange;
                                                                    const preRelease : boolean) : IList<IPackageInfo>;
var
  key : string;
  all : IList<IPackageInfo>;
  filtered : IList<IPackageInfo>;
  info : IPackageInfo;
begin
  filtered := TCollections.CreateList<IPackageInfo>;
  key := LowerCase(id);
  if not FCanned.TryGetValue(key, all) then
  begin
    result := filtered;
    exit;
  end;
  for info in all do
  begin
    if not versionRange.IsSatisfiedBy(info.Version) then
      continue;
    if (not preRelease) and (not info.Version.IsStable) then
      continue;
    filtered.Add(info);
  end;
  //Mirror the real TPackageCache - descending sort so the resolver's first-match loop
  //picks the highest in-range version, not the order tests happened to add them in.
  filtered.Sort(TComparer<IPackageInfo>.Construct(
    function(const left, right : IPackageInfo) : integer
    begin
      result := right.Version.CompareTo(left.Version);
    end));
  result := filtered;
end;

function TFakePackageCache.GetLocation : string;
begin
  result := '';
end;

procedure TFakePackageCache.SetLocation(const value : string);
begin
end;

function TFakePackageCache.GetPackagesFolder : string;
begin
  result := '';
end;

function TFakePackageCache.Clean : boolean;
begin
  result := true;
end;

function TFakePackageCache.CreatePackagePath(const packageId : IPackageIdentity) : string;
begin
  raise ENotImplemented.Create('TFakePackageCache.CreatePackagePath');
end;

function TFakePackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackagePath');
end;

function TFakePackageCache.GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackagePath(2)');
end;

function TFakePackageCache.GetPackageFileFolder(const packageId : IPackageIdentity) : string;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackageFileFolder');
end;

function TFakePackageCache.EnsurePackage(const packageId : IPackageIdentity) : boolean;
begin
  result := false;
end;

function TFakePackageCache.InstallPackageFromFile(const packageFileName : string) : boolean;
begin
  raise ENotImplemented.Create('TFakePackageCache.InstallPackageFromFile');
end;

function TFakePackageCache.GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackageMetadata');
end;

function TFakePackageCache.GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackageSpec');
end;

function TFakePackageCache.GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;
begin
  result := [];
end;

function TFakePackageCache.TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
begin
  icon := nil;
  result := false;
end;

function TFakePackageCache.GetPackageHash(const packageId : IPackageIdentity) : string;
begin
  result := '';
end;

function TFakePackageCache.FullReVerify : integer;
begin
  // Resolver tests don't exercise signing; nothing to re-verify.
  result := 0;
end;

function TFakePackageCache.GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
begin
  // Resolver tests don't exercise cache removal.
  result := TCollections.CreateList<IPackageIdentity>;
end;

function TFakePackageCache.RemovePackage(const packageId : IPackageIdentity) : boolean;
begin
  result := false;
end;

//-----------------------------------------------------------------------------
// TFakeInstallerContext
//-----------------------------------------------------------------------------

procedure TFakeInstallerContext.SeedResolutions(const projectFile : string; const resolutions : TArray<IResolvedPackage>);
begin
  RecordResolutions(projectFile, resolutions);
end;

//-----------------------------------------------------------------------------
// TDependencyResolverTests
//-----------------------------------------------------------------------------

procedure TDependencyResolverTests.Setup;
begin
  FLogger := TTestLogger.Create;
  FRepo := TFakeRepositoryManager.Create;
  FCache := TFakePackageCache.Create;
  FContext := TFakeInstallerContext.Create(FLogger);
  FRepoIntf := FRepo;
  FCacheIntf := FCache;
  FContextIntf := FContext;
  FConfig := TConfiguration.Create(FLogger);
  FCancellation := TCancellationTokenSourceFactory.Create.Token;
  FOptions := TSearchOptions.Create;
  FOptions.CompilerVersion := cTestCompiler;
  FOptions.Prerelease := false;

  FResolver := TDependencyResolver.Create(FLogger, FRepoIntf, FCacheIntf, FContextIntf);
  FResolver.Initialize(FConfig);
end;

procedure TDependencyResolverTests.Teardown;
begin
  FOptions.Free;
  FOptions := nil;
  FResolver := nil;
  FConfig := nil;
  FRepoIntf := nil;
  FCacheIntf := nil;
  FContextIntf := nil;
  FRepo := nil;
  FCache := nil;
  FContext := nil;
  FLogger := nil;
  FCancellation := nil;
end;

//-----------------------------------------------------------------------------
// Tests
//-----------------------------------------------------------------------------

procedure TDependencyResolverTests.Linear_Chain_Resolves;
var
  c : IPackageInfo;
  b : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  found : IPackageInfo;
  cFound, bFound, aFound : boolean;
begin
  // Build: A 1.0 -> B [1.0,1.1) -> C [1.0,1.1)
  c := MakeInfo('C', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,1.0.99]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,1.0.99]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'Linear chain should resolve');
  Assert.IsNotNull(resolved, 'resolved list should not be nil');
  Assert.AreEqual(3, resolved.Count, 'should resolve A, B, C');

  cFound := false; bFound := false; aFound := false;
  for found in resolved do
  begin
    if SameText(found.Id, 'A') then aFound := true;
    if SameText(found.Id, 'B') then bFound := true;
    if SameText(found.Id, 'C') then cFound := true;
  end;
  Assert.IsTrue(aFound, 'A should be in resolved set');
  Assert.IsTrue(bFound, 'B should be in resolved set');
  Assert.IsTrue(cFound, 'C should be in resolved set');
end;

procedure TDependencyResolverTests.PreferredVersion_Honoured_When_In_Range;
var
  c00, c02, c09 : IPackageInfo;
  a : IPackageInfo;
  preferred : IDictionary<string, TPackageVersion>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  found : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Cache and repo both have C 1.0.0, 1.0.2 and 1.0.9. Preferred says 1.0.2. Range [1.0.0,].
  // Expect C 1.0.2 picked even though 1.0.9 is the highest in range. Only patch may float
  // in DPM ranges, so C versions stay within 1.0.x.
  c00 := MakeInfo('C', '1.0.0', []);
  c02 := MakeInfo('C', '1.0.2', []);
  c09 := MakeInfo('C', '1.0.9', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([c00, c02, c09]));
  FCache.SetCachedVersions('C', ListOf([c00, c02, c09]));
  FCache.RegisterInfo(c02); //GetPackageInfo path for the preferred lookup

  preferred := TCollections.CreateDictionary<string, TPackageVersion>;
  preferred.Add('c', MakeVersion('1.0.2'));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved, nil, preferred);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for found in resolved do
    if SameText(found.Id, 'C') then
      selectedC := found.Version;
  Assert.AreEqual('1.0.2', selectedC.ToStringNoMeta, 'C should be the preferred version, not the latest');
end;

procedure TDependencyResolverTests.PreferredVersion_Falls_Through_When_Out_Of_Range;
var
  c00, c02 : IPackageInfo;
  a : IPackageInfo;
  preferred : IDictionary<string, TPackageVersion>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  found : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Preferred wants C 0.9.0 but A's range [1.0.0,] excludes the 0.x line. Resolver should
  // ignore the hint and pick the highest in-range version (1.0.2).
  c00 := MakeInfo('C', '1.0.0', []);
  c02 := MakeInfo('C', '1.0.2', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([c00, c02]));
  FCache.SetCachedVersions('C', ListOf([c00, c02]));

  preferred := TCollections.CreateDictionary<string, TPackageVersion>;
  preferred.Add('c', MakeVersion('0.9.0'));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved, nil, preferred);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for found in resolved do
    if SameText(found.Id, 'C') then
      selectedC := found.Version;
  Assert.AreEqual('1.0.2', selectedC.ToStringNoMeta, 'Out-of-range preferred should be ignored');
end;

procedure TDependencyResolverTests.Sibling_Conflict_NoOverlap_Records_Unresolvable;
var
  c10, c30 : IPackageInfo;
  a, b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  refForA : IPackageReference;
  refForC : IPackageReference;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A needs C 1.0.x; B needs C 3.0.x. Different major.minor means no overlap.
  // Resolve A first, then B with A's refs AND A's recorded transient C so B's resolve sees
  // the already-resolved C and detects the conflict.
  c10 := MakeInfo('C', '1.0.0', []);
  c30 := MakeInfo('C', '3.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,1.0.99]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[3.0.0,3.0.99]')]);

  FRepo.SetVersions('C', ListOf([c10, c30]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'A''s solo resolve should succeed');

  // Build project refs that B will see: A as top-level, and C as A's recorded transient
  // (parent = refForA so C.ParentId = 'A' and IsTopLevel = false - meaning the resolver
  // will attempt overlap negotiation rather than treating C as an immovable top-level pin).
  refForA := MakeTopLevelRef(a, MakeRange('1.0.0'));
  refForC := TPackageReference.Create(refForA, c10.Id, c10.Version, c10.CompilerVersion, MakeRange('[1.0.0,1.0.99]'), false);
  refForC.PackageInfo := c10;
  projectRefs.Add(refForA);
  projectRefs.Add(refForC);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsFalse(ok, 'B''s resolve should report failure (no overlap between [1.0.0,1.0.99] and [3.0.0,3.0.99])');
end;

procedure TDependencyResolverTests.Dependency_Range_Not_Mutated_After_Intersect;
var
  c00, c02, c05, c07 : IPackageInfo;
  a, b : IPackageInfo;
  originalBRange : TVersionRange;
  postBRange : TVersionRange;
  projectRefs : IList<IPackageReference>;
  refForA : IPackageReference;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
begin
  // A needs C [1.0.0,1.0.5]; B needs C [1.0.2,1.0.7]. Same project (sibling resolves).
  // After B's resolve, the intersection-narrowing must NOT have mutated B.Dependencies[0].VersionRange.
  c00 := MakeInfo('C', '1.0.0', []);
  c02 := MakeInfo('C', '1.0.2', []);
  c05 := MakeInfo('C', '1.0.5', []);
  c07 := MakeInfo('C', '1.0.7', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,1.0.5]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.2,1.0.7]')]);

  originalBRange := b.Dependencies[0].VersionRange;

  FRepo.SetVersions('C', ListOf([c00, c02, c05, c07]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  refForA := MakeTopLevelRef(a, MakeRange('1.0.0'));
  projectRefs.Add(refForA);
  FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);

  postBRange := b.Dependencies[0].VersionRange;
  Assert.AreEqual(originalBRange.ToString, postBRange.ToString,
    'B''s declared dependency range must not be mutated by resolver intersection');
end;

procedure TDependencyResolverTests.BuildDependencyGraph_Tolerates_Partial_Resolve;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  raised : boolean;
  raisedMsg : string;
begin
  // A depends on Missing — no repo entry exists, so resolution fails. The resolver should not
  // raise an exception when building the (partial) dependency graph for diagnostics.
  a := MakeInfo('A', '1.0.0', [MakeDep('Missing', '[1.0.0,1.0.99]')]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  raised := false;
  raisedMsg := '';
  try
    FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  except
    on e : Exception do
    begin
      raised := true;
      raisedMsg := e.Message;
    end;
  end;

  Assert.IsFalse(raised,
    'BuildDependencyGraph must not raise when some dependencies are unresolvable - got: ' + raisedMsg);
end;

//-----------------------------------------------------------------------------
// Additional baseline tests
//-----------------------------------------------------------------------------

procedure TDependencyResolverTests.Empty_Dependencies_Resolves_Top_Level_Only;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A is a leaf - no dependencies. Resolve should succeed with just A in the resolved set.
  a := MakeInfo('A', '1.0.0', []);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'leaf package should resolve');
  Assert.AreEqual(1, resolved.Count, 'only A should be resolved');
  Assert.IsTrue(SameText(resolved[0].Id, 'A'), 'resolved package should be A');
end;

procedure TDependencyResolverTests.Multiple_Dependencies_All_Resolve;
var
  b, c, d, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  ids : ISet<string>;
  info : IPackageInfo;
begin
  // A has three independent leaf deps - all should resolve.
  b := MakeInfo('B', '1.0.0', []);
  c := MakeInfo('C', '1.0.0', []);
  d := MakeInfo('D', '1.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]'), MakeDep('C', '[1.0.0,]'), MakeDep('D', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));
  FRepo.SetVersions('D', ListOf([d]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'all deps should resolve');
  ids := TCollections.CreateSet<string>;
  for info in resolved do
    ids.Add(LowerCase(info.Id));
  Assert.IsTrue(ids.Contains('a'), 'A should be resolved');
  Assert.IsTrue(ids.Contains('b'), 'B should be resolved');
  Assert.IsTrue(ids.Contains('c'), 'C should be resolved');
  Assert.IsTrue(ids.Contains('d'), 'D should be resolved');
end;

procedure TDependencyResolverTests.Diamond_Dependency_No_Conflict;
var
  d, b, c, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  dCount : integer;
  info : IPackageInfo;
begin
  // A -> B -> D and A -> C -> D with compatible ranges. D should appear once in the resolved set.
  d := MakeInfo('D', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('D', '[1.0.0,]')]);
  c := MakeInfo('C', '1.0.0', [MakeDep('D', '[1.0.0,]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]'), MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));
  FRepo.SetVersions('D', ListOf([d]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'diamond should resolve cleanly');
  dCount := 0;
  for info in resolved do
    if SameText(info.Id, 'D') then
      Inc(dCount);
  Assert.AreEqual(1, dCount, 'D should be resolved exactly once');
end;

procedure TDependencyResolverTests.Diamond_Dependency_With_Overlap_Uses_Intersection;
var
  d00, d01, d02, d03, d04, d05 : IPackageInfo;
  b, c, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  selectedD : TPackageVersion;
  info : IPackageInfo;
  lowerBound, upperBound : TPackageVersion;
begin
  // A -> B -> D [1.0.0,1.0.5]
  // A -> C -> D [1.0.2,1.0.4]
  // B is processed first, picks D 1.0.5 (highest in its range). Then C finds D 1.0.5 outside
  // its range; intersect = [1.0.2,1.0.4]; D removed, C re-pushed, picks something in the overlap.
  d00 := MakeInfo('D', '1.0.0', []);
  d01 := MakeInfo('D', '1.0.1', []);
  d02 := MakeInfo('D', '1.0.2', []);
  d03 := MakeInfo('D', '1.0.3', []);
  d04 := MakeInfo('D', '1.0.4', []);
  d05 := MakeInfo('D', '1.0.5', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('D', '[1.0.0,1.0.5]')]);
  c := MakeInfo('C', '1.0.0', [MakeDep('D', '[1.0.2,1.0.4]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]'), MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));
  FRepo.SetVersions('D', ListOf([d00, d01, d02, d03, d04, d05]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'diamond with overlapping ranges should resolve');
  selectedD := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'D') then
      selectedD := info.Version;

  lowerBound := MakeVersion('1.0.2');
  upperBound := MakeVersion('1.0.4');
  Assert.IsTrue(selectedD.CompareTo(lowerBound) >= 0, 'D version must be >= 1.0.2 (was ' + selectedD.ToStringNoMeta + ')');
  Assert.IsTrue(selectedD.CompareTo(upperBound) <= 0, 'D version must be <= 1.0.4 (was ' + selectedD.ToStringNoMeta + ')');
end;

procedure TDependencyResolverTests.Sibling_Conflict_With_Overlap_Uses_Intersection;
var
  c00, c01, c02, c03, c04, c05 : IPackageInfo;
  a, b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  refForA : IPackageReference;
  refForC : IPackageReference;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  selectedC : TPackageVersion;
  info : IPackageInfo;
  lowerBound, upperBound : TPackageVersion;
begin
  // A and B are sibling top-levels in the same project. A's resolve picks C 1.0.5 (top of its
  // range). Then B's resolve sees C 1.0.5 outside [1.0.2,1.0.4] - the intersection of the two
  // ranges is [1.0.2,1.0.4], so C is removed and re-resolved within that intersection.
  c00 := MakeInfo('C', '1.0.0', []);
  c01 := MakeInfo('C', '1.0.1', []);
  c02 := MakeInfo('C', '1.0.2', []);
  c03 := MakeInfo('C', '1.0.3', []);
  c04 := MakeInfo('C', '1.0.4', []);
  c05 := MakeInfo('C', '1.0.5', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,1.0.5]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.2,1.0.4]')]);

  FRepo.SetVersions('C', ListOf([c00, c01, c02, c03, c04, c05]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'A''s solo resolve should succeed');

  // Build refs that carry A's resolved C 1.0.5 into B's resolve as a transient under A.
  refForA := MakeTopLevelRef(a, MakeRange('1.0.0'));
  refForC := TPackageReference.Create(refForA, c05.Id, c05.Version, c05.CompilerVersion, MakeRange('[1.0.0,1.0.5]'), false);
  refForC.PackageInfo := c05;
  projectRefs.Add(refForA);
  projectRefs.Add(refForC);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'B''s resolve should succeed by intersecting ranges');

  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  lowerBound := MakeVersion('1.0.2');
  upperBound := MakeVersion('1.0.4');
  Assert.IsTrue(selectedC.CompareTo(lowerBound) >= 0, 'C version must be >= 1.0.2 (was ' + selectedC.ToStringNoMeta + ')');
  Assert.IsTrue(selectedC.CompareTo(upperBound) <= 0, 'C version must be <= 1.0.4 (was ' + selectedC.ToStringNoMeta + ')');
end;

procedure TDependencyResolverTests.Top_Level_Conflict_Records_Unresolvable;
var
  a : IPackageInfo;
  b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  refForA : IPackageReference;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A is a top-level package pinned at 1.0.0. B (newPackage) declares a dependency on A in
  // range [2.0.0,]. The resolver must treat A as not-negotiable and record an unresolvable.
  a := MakeInfo('A', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('A', '[2.0.0,]')]);

  refForA := MakeTopLevelRef(a, MakeRange('1.0.0'));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(refForA);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsFalse(ok, 'top-level pin must not be silently renegotiated');
end;

procedure TDependencyResolverTests.Missing_Repo_Package_Records_Unresolvable;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A depends on Ghost - nothing in the repo, nothing in the cache. Unresolvable.
  a := MakeInfo('A', '1.0.0', [MakeDep('Ghost', '[1.0.0,]')]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsFalse(ok, 'resolver must surface a conflict when no version of a dependency exists anywhere');
end;

procedure TDependencyResolverTests.Prerelease_Excluded_By_Default;
var
  cStable, cBeta : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // A is stable; default Prerelease=false. Repo has C 1.0.4 stable and C 1.0.5-beta. The
  // resolver should exclude the prerelease and select the stable version.
  cStable := MakeInfo('C', '1.0.4', []);
  cBeta := MakeInfo('C', '1.0.5-beta', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([cStable, cBeta]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed using only stable versions');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.4', selectedC.ToStringNoMeta, 'prerelease must be skipped when not requested');
end;

procedure TDependencyResolverTests.Prerelease_Allowed_When_Parent_Is_Prerelease;
var
  cStable, cBeta : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // A itself is a prerelease (1.0.0-beta). The resolver derives 'preRelease := includePrerelease
  // or (not currentPackage.Version.IsStable)' so prereleases of C become eligible. Repo has
  // both stable C 1.0.4 and prerelease C 1.0.5-beta - the latest, prerelease, should be picked.
  cStable := MakeInfo('C', '1.0.4', []);
  cBeta := MakeInfo('C', '1.0.5-beta', []);
  a := MakeInfo('A', '1.0.0-beta', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([cStable, cBeta]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5-beta', selectedC.ToStringNoMeta, 'prerelease parent should allow prerelease transient');
end;

procedure TDependencyResolverTests.PreferredVersion_Missing_From_Cache_Falls_Through;
var
  c00, c05 : IPackageInfo;
  a : IPackageInfo;
  preferred : IDictionary<string, TPackageVersion>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Preferred hint says C 1.0.2, but no info is registered in the cache for that key. The
  // resolver's GetPackageInfo returns nil, so it falls through to range-based selection.
  c00 := MakeInfo('C', '1.0.0', []);
  c05 := MakeInfo('C', '1.0.5', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([c00, c05]));
  //Deliberately not seeding the cache with C 1.0.2 - it's the preferred version but missing.
  FCache.SetCachedVersions('C', ListOf([c00, c05]));

  preferred := TCollections.CreateDictionary<string, TPackageVersion>;
  preferred.Add('c', MakeVersion('1.0.2'));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved, nil, preferred);

  Assert.IsTrue(ok, 'resolve should succeed via fallback');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5', selectedC.ToStringNoMeta, 'preferred not in cache must fall through to range-based pick');
end;

procedure TDependencyResolverTests.Cross_Project_Resolution_Reused_When_Range_Matches;
var
  c, a : IPackageInfo;
  resolvedC : IResolvedPackage;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Sibling project already resolved C at 1.0.3. Current project's A depends on C in a range
  // that includes 1.0.3. The resolver should adopt the sibling's resolution via the installer
  // context's FindPackageResolution rather than re-resolving.
  c := MakeInfo('C', '1.0.3', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  //Pre-record C 1.0.3 against another project file.
  resolvedC := TResolvedPackage.Create(c, MakeRange('1.0.3'), 'root-node', 'C:\test\sibling.dproj');
  FContext.SeedResolutions('C:\test\sibling.dproj', [resolvedC]);

  //Repo has a newer C - but it must not be picked because the cross-project resolution is binding.
  FRepo.SetVersions('C', ListOf([c, MakeInfo('C', '1.0.5', [])]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should adopt sibling project resolution');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.3', selectedC.ToStringNoMeta, 'must reuse sibling project resolution, not pick the latest');
end;

procedure TDependencyResolverTests.Narrowed_Range_Persists_Across_Reprocessing;
var
  c00, c02, c04, c07 : IPackageInfo;
  b, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
  upperBound : TPackageVersion;
begin
  // A -> B -> C [1.0.0,1.0.7] - B's dep is wide.
  // A also has direct dep on C [1.0.0,1.0.4] - tighter.
  // The resolver picks C 1.0.7 for B first, then sees A's C range conflicting, intersects to
  // [1.0.0,1.0.4], narrows on the context, removes the C resolution and re-picks something in
  // [1.0.0,1.0.4] when A is re-processed. The narrowed range MUST survive across iterations
  // (regression for Fix #1's overlay-not-mutation approach).
  c00 := MakeInfo('C', '1.0.0', []);
  c02 := MakeInfo('C', '1.0.2', []);
  c04 := MakeInfo('C', '1.0.4', []);
  c07 := MakeInfo('C', '1.0.7', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,1.0.7]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]'), MakeDep('C', '[1.0.0,1.0.4]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c00, c02, c04, c07]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed with narrowed range');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  upperBound := MakeVersion('1.0.4');
  Assert.IsTrue(selectedC.CompareTo(upperBound) <= 0,
    'C must respect the narrower [1.0.0,1.0.4] - got ' + selectedC.ToStringNoMeta);
end;

procedure TDependencyResolverTests.Cyclic_Dependency_Terminates_Without_Hang;
var
  a, b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
begin
  // A depends on B and B depends on A. Without an iteration limit the resolver re-pushes
  // both forever. With the limit, the resolve must return - this test would hang otherwise.
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('A', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('A', ListOf([a]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsNotNull(resolved, 'resolver must return - the iteration limit terminates the cycle');
end;

procedure TDependencyResolverTests.Iteration_Limit_Halts_Pathological_Input;
var
  a, b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Cyclic graph that would otherwise loop indefinitely - the iteration limit must record
  // an unresolvable so callers can surface the failure rather than hang.
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('A', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('A', ListOf([a]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsFalse(ok, 'iteration-limit-triggered resolve must report failure');
end;

procedure TDependencyResolverTests.Prerelease_TopLevel_Promotes_Globally_For_Transitive_Deps;
var
  b, cStable, cBeta : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // A 1.0.0-beta (prerelease top-level) -> B 1.0.0 (stable transient) -> C [1.0.0,]
  // Under the old per-package rule, B is stable so its dep C is resolved with preRelease=false
  // (picks stable). Under the NuGet-style global rule, A being prerelease promotes the whole
  // resolve, so C 1.0.5-beta becomes eligible and is picked as the highest in-range.
  cStable := MakeInfo('C', '1.0.4', []);
  cBeta := MakeInfo('C', '1.0.5-beta', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  a := MakeInfo('A', '1.0.0-beta', [MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([cStable, cBeta]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5-beta', selectedC.ToStringNoMeta,
    'prerelease top-level must promote prerelease eligibility for all transients, including those reached via stable parents');
end;

procedure TDependencyResolverTests.Prerelease_Stable_And_Prerelease_Siblings_Share_Same_Transient;
var
  cStable, cBeta : IPackageInfo;
  a, b : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  refForB : IPackageReference;
  refForC : IPackageReference;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Stable A and prerelease B both depend on C. Resolve B first - it's prerelease, so C
  // 1.0.5-beta is picked. Then resolve A as a sibling with B's resolved graph. Under the new
  // rule, A's resolve also sees a prerelease top-level (B) so prereleases stay eligible, and
  // C 1.0.5-beta is honoured (no downgrade attempt). Both siblings end up agreeing on the
  // same C - the regression bar for the "two versions of the same id in one resolve" bug.
  cStable := MakeInfo('C', '1.0.4', []);
  cBeta := MakeInfo('C', '1.0.5-beta', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0-beta', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([cStable, cBeta]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'B''s solo resolve should succeed');

  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5-beta', selectedC.ToStringNoMeta, 'prerelease B should pull in prerelease C');

  // Build refs that carry B (prerelease top-level) and its transient C 1.0.5-beta into A's resolve.
  refForB := MakeTopLevelRef(b, MakeRange('1.0.0-beta'));
  refForC := TPackageReference.Create(refForB, cBeta.Id, cBeta.Version, cBeta.CompilerVersion, MakeRange('[1.0.0,]'), false);
  refForC.PackageInfo := cBeta;
  projectRefs.Add(refForB);
  projectRefs.Add(refForC);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'A''s sibling resolve should succeed');

  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5-beta', selectedC.ToStringNoMeta,
    'sibling resolves must agree on the same C - prerelease top-level (B) promotes prerelease eligibility for A''s resolve too');
end;

//-----------------------------------------------------------------------------
// Deeper coverage: cache+repo interaction, multi-failure, shared caches, graph shape.
//-----------------------------------------------------------------------------

procedure TDependencyResolverTests.Linear_Deep_Chain_Resolves;
var
  e, d, c, b, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  ids : ISet<string>;
begin
  // A -> B -> C -> D -> E. All resolve in one pass.
  e := MakeInfo('E', '1.0.0', []);
  d := MakeInfo('D', '1.0.0', [MakeDep('E', '[1.0.0,]')]);
  c := MakeInfo('C', '1.0.0', [MakeDep('D', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));
  FRepo.SetVersions('D', ListOf([d]));
  FRepo.SetVersions('E', ListOf([e]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'deep chain should resolve');
  ids := TCollections.CreateSet<string>;
  for info in resolved do
    ids.Add(LowerCase(info.Id));
  Assert.IsTrue(ids.Contains('a') and ids.Contains('b') and ids.Contains('c') and ids.Contains('d') and ids.Contains('e'),
    'all five levels of the chain must appear in the resolved set');
end;

procedure TDependencyResolverTests.Repo_Not_Called_When_Cache_Satisfies_Range;
var
  c : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Cache has C 1.0.0 within A's range. The cache-first probe returns it; the repository
  // should never be queried. Documents the cache-first behaviour landed during the restore work.
  c := MakeInfo('C', '1.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  //deliberately leaving repo empty - any call would return no versions and break the test
  FCache.SetCachedVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed via cache');
  Assert.AreEqual(0, FRepo.GetCallCount('C'),
    'repository must not be queried for C when the cache already satisfies the range');
end;

procedure TDependencyResolverTests.Repo_Called_When_Cache_Empty_For_Id;
var
  c : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Cache has no entries for C; the resolver must fall back to the repository.
  c := MakeInfo('C', '1.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([c]));
  //cache deliberately empty

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed via repo');
  Assert.AreEqual(1, FRepo.GetCallCount('C'),
    'repository must be queried once when the cache has nothing for the id');
end;

procedure TDependencyResolverTests.Repo_Called_When_Cache_Versions_All_Out_Of_Range;
var
  cOld, cNew : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Cache has only C 1.0.0 (old). A's range demands 1.0.5+. The cache-first probe returns an
  // empty filtered list, so versionsFromCache=false and the resolver goes straight to the repo
  // which has C 1.0.5. This exercises the cache-empty-after-filter path.
  cOld := MakeInfo('C', '1.0.0', []);
  cNew := MakeInfo('C', '1.0.5', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.5,]')]);

  FCache.SetCachedVersions('C', ListOf([cOld]));
  FRepo.SetVersions('C', ListOf([cOld, cNew]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed via repo fallback');
  Assert.AreEqual(1, FRepo.GetCallCount('C'),
    'repository must be queried when no cached version is in range');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5', selectedC.ToStringNoMeta, 'should pick the repo-only in-range version');
end;

procedure TDependencyResolverTests.SharedVersionCache_Avoids_Repo_Requery_Across_Resolves;
var
  c : IPackageInfo;
  a, b : IPackageInfo;
  shared : IDictionary<string, IList<IPackageInfo>>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Two top-levels resolved in sequence (restore-style), both depending on C. Passing the
  // SAME shared version cache to both ResolveForInstall calls means the second resolve reuses
  // the first's version lookup - the repo should only be queried once for C across both.
  c := MakeInfo('C', '1.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([c]));

  shared := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  projectRefs := TCollections.CreateList<IPackageReference>;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved, shared, nil);
  Assert.IsTrue(ok, 'A''s resolve should succeed');

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved, shared, nil);
  Assert.IsTrue(ok, 'B''s resolve should succeed');

  Assert.AreEqual(1, FRepo.GetCallCount('C'),
    'sharedVersionCache must collapse the repo query for C to a single call across both resolves');
end;

procedure TDependencyResolverTests.Failed_Dep_Doesnt_Block_Sibling_Dep_Resolution;
var
  b : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  bFound : boolean;
  info : IPackageInfo;
begin
  // A depends on Missing (unresolvable) AND B (resolvable). The resolver must continue to
  // attempt B even after recording the unresolvable for Missing - the goal is to surface ALL
  // conflicts in a single run rather than bailing on the first one.
  b := MakeInfo('B', '1.0.0', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('Missing', '[1.0.0,]'), MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsFalse(ok, 'resolve must report failure because of the unresolvable Missing dep');
  bFound := false;
  for info in resolved do
    if SameText(info.Id, 'B') then
      bFound := true;
  Assert.IsTrue(bFound, 'B should still appear in the resolved set even though a sibling dep failed');
end;

procedure TDependencyResolverTests.Prerelease_Option_Forces_Globally;
var
  cStable, cBeta : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Stable A, no prerelease top-levels - but the user explicitly passed Prerelease=true. The
  // resolver must honour that and allow prerelease C, matching NuGet's `-Prerelease` switch.
  cStable := MakeInfo('C', '1.0.4', []);
  cBeta := MakeInfo('C', '1.0.5-beta', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('C', ListOf([cStable, cBeta]));
  FOptions.Prerelease := true;

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('1.0.5-beta', selectedC.ToStringNoMeta,
    'explicit Prerelease=true must promote prerelease eligibility globally');
end;

procedure TDependencyResolverTests.Cross_Project_Conflict_Outside_Range_Records_Unresolvable;
var
  c, a : IPackageInfo;
  siblingC : IResolvedPackage;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Sibling project resolved C 1.0.5. Current project's A demands C in [2.0.0,]. The sibling's
  // version cannot satisfy the new range, and we can't renegotiate a sibling's resolution
  // (Fix #5 was skipped intentionally), so the resolver must record an unresolvable conflict.
  c := MakeInfo('C', '1.0.5', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[2.0.0,]')]);

  siblingC := TResolvedPackage.Create(c, MakeRange('1.0.5'), 'root-node', 'C:\test\sibling.dproj');
  FContext.SeedResolutions('C:\test\sibling.dproj', [siblingC]);

  //repo has a 2.0.0 but the resolver should defer to the sibling's resolution and reject it.
  FRepo.SetVersions('C', ListOf([c, MakeInfo('C', '2.0.0', [])]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsFalse(ok, 'cross-project resolution outside our range must surface as unresolvable');
end;

procedure TDependencyResolverTests.Group_Project_Version_Conflict_Counted_As_Error;
var
  x, b : IPackageInfo;
  siblingX : IResolvedPackage;
  xLocal : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Sibling project has resolved X 1.0.0 under range [1.0.0,1.0.0]. Current project's project
  // graph carries X at 1.0.5 (incompatible with the sibling's recorded range). The pre-resolve
  // loop in ResolveForInstall checks each projectRef against the cross-project resolutions and
  // increments errorCount on mismatch - the final result must be false even if the local
  // resolve completes.
  x := MakeInfo('X', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', []);
  siblingX := TResolvedPackage.Create(x, MakeRange('1.0.0'), 'root-node', 'C:\test\sibling.dproj');
  FContext.SeedResolutions('C:\test\sibling.dproj', [siblingX]);

  xLocal := MakeTopLevelRef(MakeInfo('X', '1.0.5', []), MakeRange('1.0.5'));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(xLocal);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsFalse(ok, 'project-group version mismatch on a top-level must be counted as a resolver error');
end;

procedure TDependencyResolverTests.BuildDependencyGraph_Reflects_Resolved_Tree_Structure;
var
  c, b, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  aNode : IPackageReference;
  bNode : IPackageReference;
  cNode : IPackageReference;
begin
  // After a successful resolve, the returned graph must mirror the parent-child structure of
  // the resolved set: root -> A -> B -> C. Walks the graph by FindFirstChild.
  c := MakeInfo('C', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'resolve should succeed');
  Assert.IsNotNull(graph, 'graph should be returned');
  Assert.IsTrue(graph.IsRoot, 'returned graph node should be the root');

  aNode := graph.FindFirstChild('A');
  Assert.IsNotNull(aNode, 'graph root should have A as a child');
  Assert.AreEqual('1.0.0', aNode.Version.ToStringNoMeta, 'A should be at version 1.0.0');

  bNode := aNode.FindFirstChild('B');
  Assert.IsNotNull(bNode, 'A should have B as a child');
  Assert.AreEqual('1.0.0', bNode.Version.ToStringNoMeta, 'B should be at version 1.0.0');

  cNode := bNode.FindFirstChild('C');
  Assert.IsNotNull(cNode, 'B should have C as a child');
  Assert.AreEqual('1.0.0', cNode.Version.ToStringNoMeta, 'C should be at version 1.0.0');
end;

procedure TDependencyResolverTests.Resolved_Top_Level_Leaf_Does_Not_Cause_Extra_Requirements;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A is a top-level leaf (no deps). The resolver pushes it as a requirement in context.Create,
  // pops it, sees no deps, and continues. No exception, no repo/cache calls, exactly one entry
  // in the resolved set - this is the smoke test for the trivial-graph path.
  a := MakeInfo('A', '1.0.0', []);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'leaf top-level should resolve');
  Assert.AreEqual(1, resolved.Count, 'only A should be resolved - no spurious extra entries');
  Assert.AreEqual(0, FRepo.GetCallCount('A'),
    'no repo call needed for a leaf top-level since it has no deps to fetch versions for');
end;

procedure TDependencyResolverTests.BuildDependencyGraph_Transitive_Range_Is_Parent_Declared_Range;
var
  c, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  aNode : IPackageReference;
  cNode : IPackageReference;
begin
  // The range recorded on a transitive edge must be the PARENT's declared dependency range
  // (e.g. [0.1.4,]) - not the resolved single version. A declared open-ended range must survive
  // verbatim into the graph so it can be re-evaluated on the next restore/resolve.
  c := MakeInfo('C', '0.1.4', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[0.1.4,]')]);

  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'resolve should succeed');

  aNode := graph.FindFirstChild('A');
  Assert.IsNotNull(aNode, 'graph root should have A as a child');
  cNode := aNode.FindFirstChild('C');
  Assert.IsNotNull(cNode, 'A should have C as a child');
  Assert.AreEqual('[0.1.4,]', cNode.VersionRange.ToString,
    'transitive C must record A''s declared range [0.1.4,], not the selected version 0.1.4');
end;

procedure TDependencyResolverTests.BuildDependencyGraph_Transitive_Range_Not_Collapsed_After_Promotion;
var
  c, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  aNode : IPackageReference;
  cNode : IPackageReference;
begin
  // Regression: C is already an installed top-level reference whose range has been collapsed to a
  // single version (this is what CreateProjectRefs produces for a directly-pinned package). When A,
  // which declares C [0.1.4,], is installed, the shared resolution carries the collapsed single
  // version 0.1.4. The A->C edge must still record A's declared [0.1.4,], not the collapsed 0.1.4.
  c := MakeInfo('C', '0.1.4', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[0.1.4,]')]);

  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(MakeTopLevelRef(c, MakeRange('0.1.4')));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'resolve should succeed');

  aNode := graph.FindFirstChild('A');
  Assert.IsNotNull(aNode, 'graph root should have A as a child');
  cNode := aNode.FindFirstChild('C');
  Assert.IsNotNull(cNode, 'A should have C as a child');
  Assert.AreEqual('[0.1.4,]', cNode.VersionRange.ToString,
    'promotion of a seeded top-level must not collapse the recorded transitive range');
end;

procedure TDependencyResolverTests.BuildDependencyGraph_Same_Transitive_Two_Parents_Records_Each_Declared_Range;
var
  c, a, b, root : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  rootNode : IPackageReference;
  aNode : IPackageReference;
  bNode : IPackageReference;
  cUnderA : IPackageReference;
  cUnderB : IPackageReference;
begin
  // A single resolved C is shared by two parents that declare DIFFERENT ranges. Each edge must
  // record its own parent's declared range - something the shared per-id resolution.VersionRange
  // can never represent. C 1.0.5 satisfies both ranges so no narrowing occurs.
  c := MakeInfo('C', '1.0.5', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[1.0.0,]')]);
  b := MakeInfo('B', '1.0.0', [MakeDep('C', '[1.0.2,1.0.7]')]);
  root := MakeInfo('Root', '1.0.0', [MakeDep('A', '[1.0.0,]'), MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('A', ListOf([a]));
  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, root, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'resolve should succeed');

  rootNode := graph.FindFirstChild('Root');
  Assert.IsNotNull(rootNode, 'graph root should have Root as a child');

  aNode := rootNode.FindFirstChild('A');
  Assert.IsNotNull(aNode, 'Root should have A as a child');
  cUnderA := aNode.FindFirstChild('C');
  Assert.IsNotNull(cUnderA, 'A should have C as a child');
  Assert.AreEqual('[1.0.0,]', cUnderA.VersionRange.ToString,
    'C under A must record A''s declared range [1.0.0,]');

  bNode := rootNode.FindFirstChild('B');
  Assert.IsNotNull(bNode, 'Root should have B as a child');
  cUnderB := bNode.FindFirstChild('C');
  Assert.IsNotNull(cUnderB, 'B should have C as a child');
  Assert.AreEqual('[1.0.2,1.0.7]', cUnderB.VersionRange.ToString,
    'C under B must record B''s declared range [1.0.2,1.0.7]');
end;

initialization
  TDUnitX.RegisterTestFixture(TDependencyResolverTests);

end.
