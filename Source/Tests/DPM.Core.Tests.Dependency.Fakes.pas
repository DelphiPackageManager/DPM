unit DPM.Core.Tests.Dependency.Fakes;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
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
    FVersionsCallCount : IDictionary<string, integer>;
    FInfoCallCount : integer;
    FUnavailable : string;
    FDownloaded : IList<IPackageInfo>;
    procedure Count(const counts : IDictionary<string, integer>; const id : string);
  protected
    function UnavailableSources : string;
    procedure BeginRequestCache;
    procedure EndRequestCache;
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
    //GetPackageVersionsWithDependencies calls for this id
    function GetCallCount(const id : string) : integer;
    //GetPackageVersions calls for this id
    function GetVersionsCallCount(const id : string) : integer;
    //GetPackageInfo calls, any id
    property InfoCallCount : integer read FInfoCallCount;
    //what UnavailableSources returns - set it to pretend a source could not be used.
    property Unavailable : string read FUnavailable write FUnavailable;
    //the package infos DownloadPackage was asked for - it downloads nothing, and fails.
    property Downloaded : IList<IPackageInfo> read FDownloaded;
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
    function EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
    function InstallPackageFromFile(const cancellationToken : ICancellationToken; const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;
    procedure SetSkipTrustRatchets(const value : boolean);
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageMetadata(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageMetadata;
    function GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePlatforms(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : TDPMPlatforms;
    function HasSource(const packageId : IPackageIdentity) : boolean;
    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;
    function TryGetPackageIcon(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
    function GetPackageHash(const packageId : IPackageIdentity) : string;
    function FullReVerify(const cancellationToken : ICancellationToken) : integer;
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
    //the committed graph recorded for a project (nil if none).
    function ProjectGraph(const projectFile : string) : IPackageReference;
  end;

const
  cTestCompiler = TCompilerVersion.Delphi12_0;
  cTestProject = 'C:\test\project.dproj';

function MakeVersion(const s : string) : TPackageVersion;
function MakeRange(const s : string) : TVersionRange;
function MakeDep(const id : string; const range : string) : IPackageDependency;
function MakeBundledDep(const id : string) : IPackageDependency;
function MakeInfo(const id : string; const version : string; const deps : array of IPackageDependency) : IPackageInfo;
function MakeTopLevelRef(const info : IPackageInfo; const selectedOn : TVersionRange) : IPackageReference;
function ListOf(const items : array of IPackageInfo) : IList<IPackageInfo>;
function MakeCacheKey(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion) : string;

implementation

uses
  System.TypInfo,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Dependency,
  DPM.Core.Dependency.Reference;

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

///<summary>A dependency on a Delphi-bundled library (e.g. Indy) - declared with the sentinel
/// version 999.999.999 (cBundledDependencyVersion).</summary>
function MakeBundledDep(const id : string) : IPackageDependency;
begin
  result := TPackageDependency.Create(id, MakeRange(cBundledDependencyVersion));
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
  FVersionsCallCount := TCollections.CreateDictionary<string, integer>;
  FDownloaded := TCollections.CreateList<IPackageInfo>;
end;

procedure TFakeRepositoryManager.Count(const counts : IDictionary<string, integer>; const id : string);
var
  key : string;
  prior : integer;
begin
  key := LowerCase(id);
  if counts.TryGetValue(key, prior) then
    counts[key] := prior + 1
  else
    counts[key] := 1;
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

function TFakeRepositoryManager.GetVersionsCallCount(const id : string) : integer;
begin
  if not FVersionsCallCount.TryGetValue(LowerCase(id), result) then
    result := 0;
end;

procedure TFakeRepositoryManager.BeginRequestCache;
begin
  //no-op : the fake counts every call, which is what the tests assert on.
end;

procedure TFakeRepositoryManager.EndRequestCache;
begin
end;

function TFakeRepositoryManager.Initialize(const configuration : IConfiguration) : boolean;
begin
  result := true;
end;

function TFakeRepositoryManager.HasSources : boolean;
begin
  result := true;
end;

function TFakeRepositoryManager.UnavailableSources : string;
begin
  result := FUnavailable;
end;

function TFakeRepositoryManager.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                                   const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean) : IList<IPackageInfo>;
var
  key : string;
  all : IList<IPackageInfo>;
  filtered : IList<IPackageInfo>;
  info : IPackageInfo;
begin
  key := LowerCase(packageId);
  Count(FCallCount, packageId);

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
  FDownloaded.Add(packageIdentity);
  result := false;
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
var
  all : IList<IPackageInfo>;
  info : IPackageInfo;
begin
  Inc(FInfoCallCount);
  result := nil;
  if not FCanned.TryGetValue(LowerCase(packageId.Id), all) then
    exit;
  for info in all do
  begin
    if info.Version = packageId.Version then
      exit(info);
  end;
end;

function TFakeRepositoryManager.GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>;
var
  all : IList<IPackageInfo>;
  info : IPackageInfo;
begin
  Count(FVersionsCallCount, packageId);
  result := TCollections.CreateList<TPackageVersion>;
  if not FCanned.TryGetValue(LowerCase(packageId), all) then
    exit;
  for info in all do
  begin
    if (not includePrerelease) and (not info.Version.IsStable) then
      continue;
    result.Add(info.Version);
  end;
  //descending, like the real repository manager.
  result.Sort(TComparer<TPackageVersion>.Construct(
    function(const left, right : TPackageVersion) : integer
    begin
      result := right.CompareTo(left);
    end));
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

function TFakePackageCache.EnsurePackage(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : boolean;
begin
  result := false;
end;

function TFakePackageCache.InstallPackageFromFile(const cancellationToken : ICancellationToken; const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;
begin
  raise ENotImplemented.Create('TFakePackageCache.InstallPackageFromFile');
end;

procedure TFakePackageCache.SetSkipTrustRatchets(const value : boolean);
begin
  // no-op for tests
end;

function TFakePackageCache.GetPackageMetadata(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageMetadata;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackageMetadata');
end;

function TFakePackageCache.GetPackageSpec(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageSpec;
begin
  raise ENotImplemented.Create('TFakePackageCache.GetPackageSpec');
end;

function TFakePackageCache.GetPackagePlatforms(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : TDPMPlatforms;
begin
  result := [];
end;

function TFakePackageCache.HasSource(const packageId : IPackageIdentity) : boolean;
begin
  result := false;
end;

function TFakePackageCache.TryGetPackageIcon(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
begin
  icon := nil;
  result := false;
end;

function TFakePackageCache.GetPackageHash(const packageId : IPackageIdentity) : string;
begin
  result := '';
end;

function TFakePackageCache.FullReVerify(const cancellationToken : ICancellationToken) : integer;
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

function TFakeInstallerContext.ProjectGraph(const projectFile : string) : IPackageReference;
begin
  result := GetProjectGraph(projectFile);
end;

end.
