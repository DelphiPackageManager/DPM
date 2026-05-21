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
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Dependency.Context;

interface

uses
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces;

//only intended for internal use by the resolver so interfaces can stay here
type
  /// <summary>
  ///  Records a dependency the resolver could not satisfy. Collected on the context so we can keep
  ///  resolving sibling dependencies and report all conflicts at the end of the run instead of bailing
  ///  on the first one. Reason is the human-readable message already logged at the conflict site.
  /// </summary>
  TUnresolvableConflict = record
    PackageId : string;
    ParentId : string;
    RequestedRange : TVersionRange;
    Reason : string;
  end;

  /// <summary>
  /// Used to cache information during dependency resolution.
  /// </summary>
  IResolverContext = interface
    ['{B97E7843-4C13-490A-A776-DFAC1BC60A0D}']
    procedure RecordNoGood(const bad : IPackageInfo);
    function IsNoGood(const package : IPackageInfo) : boolean;
    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);

    ///  Read-only lookup against the local resolution table. Returns false (and resolution=nil)
    ///  if the package isn't already recorded in THIS context's FResolved. Does not consult the
    ///  cross-project installer context. Use this for parent / backtrack lookups where importing
    ///  another project's resolution would be incorrect.
    function TryGetResolvedPackage(const packageId : string; out resolution : IResolvedPackage) : boolean;

    ///  Like TryGetResolvedPackage, but on miss also consults the cross-project installer
    ///  context. If a sibling project has resolved this id, the resolution is cloned (with the
    ///  supplied parentId) into FResolved and its transitive resolutions are imported. Mutates
    ///  the context's resolution state - use only at the primary dep-resolution call site.
    function GetOrImportResolvedPackage(const packageId : string; const parentId : string; out resolution : IResolvedPackage) : boolean;

    procedure RemoveResolvedPackage(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolvedPackages : TArray<IResolvedPackage>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure CachePackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    function GetResolvedPackageInfos : IList<IPackageInfo>;
    function BuildDependencyGraph : IPackageReference;
    function ProjectFile : string;
    procedure RecordUnresolvable(const conflict : TUnresolvableConflict);
    function HasUnresolvable : boolean;
    function GetUnresolvable : IList<TUnresolvableConflict>;

    ///  When restore re-uses the dproj's recorded graph as a lock file, the installer hands the
    ///  recorded (id -> version) map to the resolver via the context. Returns true and the
    ///  pinned version when there is a hint for this id - the resolver should try to honour it
    ///  before picking a new version from the cache or repo. Returns false otherwise.
    function TryGetPreferredVersion(const packageId : string; out version : TPackageVersion) : boolean;

    ///  Records that a parent's dependency on (depId) has been narrowed to a tighter range than
    ///  the parent's manifest declares - used when an intersection is found with another already-
    ///  resolved version. Stored on the context so the next iteration sees the narrower range
    ///  without mutating the parent's IPackageInfo (which would corrupt the manifest's view if
    ///  the same info is re-evaluated).
    procedure NarrowDependencyRange(const parentId, depId : string; const range : TVersionRange);

    ///  Returns the effective range for a dependency: the narrowed range if NarrowDependencyRange
    ///  was called for this (parentId, depId) pair, otherwise the supplied declared range.
    function GetEffectiveDependencyRange(const parentId, depId : string; const declaredRange : TVersionRange) : TVersionRange;

    ///  Returns true if any package recorded as top-level in this context is a prerelease.
    ///  Used by the resolver to implement NuGet-style implicit promotion: when the user pins
    ///  a prerelease (or has one in their project), prereleases become eligible globally for
    ///  the whole resolve - matching what NuGet does when you install a prerelease by name.
    function AnyTopLevelIsPrerelease : boolean;

    ///  Returns true the first time we see this id (caller should sort dependencies now),
    ///  false on subsequent calls (sort already done in a previous iteration). Mutates the
    ///  context's "already sorted" set as a side effect. Sort order is stable across iterations
    ///  because the resolver no longer mutates IPackageDependency ranges - so re-sorting on
    ///  every backtrack-driven re-push of the same parent is wasted work.
    function ShouldSortDependencies(const packageId : string) : boolean;
  end;

  TResolverContext = class(TInterfacedObject, IResolverContext)
  private
    FLogger : ILogger;
    FNoGoods : IDictionary<string, ISet<TPackageVersion>>;
    FResolved : IDictionary<string, IResolvedPackage>;
    FOpenRequirements : IQueue<IPackageInfo>;
    FVersionCache : IDictionary<string, IList<IPackageInfo>>;
    FUnresolvable : IList<TUnresolvableConflict>;
    FCompilerVersion : TCompilerVersion;
    FProjectFile : string;
    FPackageInstallerContext : IPackageInstallerContext;
    FPreferredVersions : IDictionary<string, TPackageVersion>;
    FNarrowedRanges : IDictionary<string, TVersionRange>;
    //Tracks ids already imported via GetOrImportResolvedPackage's CopyDependencies, so a
    //cyclic cross-project resolution graph can't re-enter the same node and loop forever.
    FImportedIds : ISet<string>;
    FSortedDependencyIds : ISet<string>;
  protected
    procedure RecordNoGood(const bad : IPackageInfo);
    function IsNoGood(const package : IPackageInfo) : boolean;

    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
    function TryGetResolvedPackage(const packageId : string; out resolution : IResolvedPackage) : boolean;
    function GetOrImportResolvedPackage(const packageId : string; const parentId : string; out resolution : IResolvedPackage) : boolean;
    procedure RemoveResolvedPackage(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolvedPackages : TArray<IResolvedPackage>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure CachePackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    function GetResolvedPackageInfos : IList<IPackageInfo>;
    function BuildDependencyGraph : IPackageReference;
    function ProjectFile : string;
    procedure RecordUnresolvable(const conflict : TUnresolvableConflict);
    function HasUnresolvable : boolean;
    function GetUnresolvable : IList<TUnresolvableConflict>;
    function TryGetPreferredVersion(const packageId : string; out version : TPackageVersion) : boolean;
    procedure NarrowDependencyRange(const parentId, depId : string; const range : TVersionRange);
    function GetEffectiveDependencyRange(const parentId, depId : string; const declaredRange : TVersionRange) : TVersionRange;
    function AnyTopLevelIsPrerelease : boolean;
    function ShouldSortDependencies(const packageId : string) : boolean;
  public
    /// sharedVersionCache: when supplied, the resolver shares a version cache across multiple
    /// ResolveForInstall calls (e.g. one per top-level package in a restore). nil = create a
    /// fresh per-call cache, which is the historical behaviour.
    /// preferredVersions: lock-file-style hints (id -> version) - the resolver prefers these
    /// versions over picking the latest in-range alternative. nil for install/update, populated
    /// by restore from the existing project graph.
    constructor Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>> = nil; const preferredVersions : IDictionary<string, TPackageVersion> = nil);overload;
    constructor Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const compilerVersion : TCompilerVersion; const projectReferences : IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>> = nil; const preferredVersions : IDictionary<string, TPackageVersion> = nil);overload;
    destructor Destroy;override;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Constants,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Resolution;

  { TResolverContext }

procedure TResolverContext.CachePackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
var
  list : IList<IPackageInfo>;
begin
  if not FVersionCache.TryGetValue(LowerCase(packageId), list) then
  begin
    list := TCollections.CreateList<IPackageInfo>;
    FVersionCache.Add(LowerCase(packageId), list);
  end;
  list.AddRange(versions);
end;

function TResolverContext.AnyOpenRequrements : boolean;
begin
  result := FOpenRequirements.Any;
end;

function TResolverContext.BuildDependencyGraph : IPackageReference;
var
  toplevelPackages : TArray<IResolvedPackage>;
  topLevelPackage : IResolvedPackage;

  //Returns true if [id] is already on the parent chain of [node] - used to short-circuit
  //recursion in cyclic resolved sets (which can survive when the resolver hits its iteration
  //limit on an A->B->A graph).
  function IsAncestor(const node : IPackageReference; const id : string) : boolean;
  var
    p : IPackageReference;
  begin
    result := false;
    p := node;
    while p <> nil do
    begin
      if SameText(p.Id, id) then
        exit(true);
      p := p.Parent;
    end;
  end;

  //Tolerates partial resolution: when a package's dependency isn't in FResolved (because the
  //resolver couldn't satisfy it OR backtracked and removed it), skip the dep instead of raising.
  //Also tolerates cycles surviving in the resolved set - we skip transients whose id is already
  //on the path. Either way the graph is then partial, which is useful for diagnostics; callers
  //that need a complete graph already check the DoResolve return value before consuming this.
  procedure AddNode(const parent : IPackageReference; const package : IPackageInfo; const versionRange : TVersionRange);
  var
    resolution : IResolvedPackage;
    dependency : IPackageDependency;
    dependencyReference : IPackageReference;
  begin
    dependencyReference := parent.AddChild(package.Id, package.Version, versionRange);
    dependencyReference.UseSource := package.UseSource;
    if package.Dependencies <> nil then
      for dependency in package.Dependencies do
      begin
        if not TryGetResolvedPackage(dependency.Id, resolution) then
        begin
          FLogger.Debug('BuildDependencyGraph: no resolution for [' + dependency.Id +
                        '] required by [' + package.Id + '-' + package.Version.ToStringNoMeta +
                        '] - graph will be incomplete');
          continue;
        end;
        if IsAncestor(dependencyReference, dependency.Id) then
        begin
          FLogger.Debug('BuildDependencyGraph: cycle detected for [' + dependency.Id +
                        '] under [' + package.Id + '] - skipping to avoid an infinite graph');
          continue;
        end;
        AddNode(dependencyReference, resolution.PackageInfo, resolution.VersionRange);
      end;
  end;

begin
  result := TPackageReference.CreateRoot(FCompilerVersion);
  toplevelPackages := FResolved.Values.Where(function(const value : IResolvedPackage) : boolean
    begin
      result := value.IsTopLevel;
    end).ToArray;

  for toplevelPackage in toplevelPackages do
    AddNode(result, topLevelPackage.PackageInfo, TVersionRange.Empty);
end;

constructor TResolverContext.Create(const logger: ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const compilerVersion : TCompilerVersion; const projectReferences: IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>; const preferredVersions : IDictionary<string, TPackageVersion>);
var
  projectReference : IPackageReference;

  function IsProjectRef(const id : string) : boolean;
  begin
    result := projectReferences.Any(
      function (const projectRef : IPackageReference) : boolean
      begin
           result := SameText(id, projectRef.Id);
      end);
  end;

begin
  inherited Create;
  FCompilerVersion := compilerVersion;
  FLogger := logger;
  FPackageInstallerContext := packageInstallerContext;
  FProjectFile := projectFile;
  FNoGoods := TCollections.CreateDictionary<string, ISet<TPackageVersion>>;
  FResolved := TCollections.CreateDictionary<string, IResolvedPackage>;
  FOpenRequirements := TCollections.CreateQueue<IPackageInfo>;
  //honour the caller-supplied cache so version lookups can be reused across multiple
  //ResolveForInstall iterations during a single restore.
  if sharedVersionCache <> nil then
    FVersionCache := sharedVersionCache
  else
    FVersionCache := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FUnresolvable := TCollections.CreateList<TUnresolvableConflict>;
  //Lock-file hints. nil for install/update (the caller doesn't pin anything), populated by
  //restore from the existing dproj graph so the resolver re-uses recorded versions.
  FPreferredVersions := preferredVersions;
  //Per-(parentId, depId) overlay of narrowed dependency ranges. Used by the resolver when a
  //sibling has already constrained a shared transient - it stores the intersection here rather
  //than mutating the parent's manifest IPackageDependency.
  FNarrowedRanges := TCollections.CreateDictionary<string, TVersionRange>;
  FImportedIds := TCollections.CreateSet<string>;
  FSortedDependencyIds := TCollections.CreateSet<string>;

  for projectReference in projectReferences do
  begin
    //don't add to the list of packages to resolve if it has no dependencies..

    //this is causing unnecessary work during restore.
    //what we should be doing here is checking if there are any missing dependencies and only resolving them.
    if projectReference.PackageInfo.Dependencies.Any and (not IsProjectRef(projectReference.Id))  then
      PushRequirement(projectReference.PackageInfo);

    //record the project reference as already resolved so that we do not change the version used.
    RecordResolution(projectReference.PackageInfo, projectReference.VersionRange, projectReference.ParentId);
  end;

end;

destructor TResolverContext.Destroy;
begin
  FLogger := nil;
  FPackageInstallerContext := nil;
  FNoGoods := nil;
  FResolved := nil;
  FOpenRequirements := nil;
  FVersionCache := nil;
  FUnresolvable := nil;
  FPreferredVersions := nil;
  FNarrowedRanges := nil;
  FImportedIds := nil;
  FSortedDependencyIds := nil;
  inherited;
end;

procedure TResolverContext.RecordUnresolvable(const conflict : TUnresolvableConflict);
begin
  FUnresolvable.Add(conflict);
end;

function TResolverContext.HasUnresolvable : boolean;
begin
  result := FUnresolvable.Any;
end;

function TResolverContext.GetUnresolvable : IList<TUnresolvableConflict>;
begin
  result := FUnresolvable;
end;

constructor TResolverContext.Create(const logger : ILogger;  const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>; const preferredVersions : IDictionary<string, TPackageVersion>);
begin
  Assert(newPackage <> nil);
  Create(logger, packageInstallerContext, projectFile, newPackage.CompilerVersion, projectReferences, sharedVersionCache, preferredVersions);
  PushRequirement(newPackage);
  RecordResolution(newPackage, TVersionRange.Create(newPackage.Version), cRootNode);
end;

function TResolverContext.TryGetPreferredVersion(const packageId : string; out version : TPackageVersion) : boolean;
begin
  result := false;
  if FPreferredVersions = nil then
    exit;
  result := FPreferredVersions.TryGetValue(LowerCase(packageId), version);
end;

procedure TResolverContext.NarrowDependencyRange(const parentId, depId : string; const range : TVersionRange);
begin
  FNarrowedRanges[LowerCase(parentId) + '|' + LowerCase(depId)] := range;
end;

function TResolverContext.GetEffectiveDependencyRange(const parentId, depId : string; const declaredRange : TVersionRange) : TVersionRange;
begin
  if not FNarrowedRanges.TryGetValue(LowerCase(parentId) + '|' + LowerCase(depId), result) then
    result := declaredRange;
end;

function TResolverContext.AnyTopLevelIsPrerelease : boolean;
var
  res : IResolvedPackage;
begin
  result := false;
  for res in FResolved.Values do
    if res.IsTopLevel and (not res.PackageInfo.Version.IsStable) then
      exit(true);
end;

function TResolverContext.ShouldSortDependencies(const packageId : string) : boolean;
var
  key : string;
begin
  key := LowerCase(packageId);
  result := not FSortedDependencyIds.Contains(key);
  if result then
    FSortedDependencyIds.Add(key);
end;


procedure TResolverContext.PushRequirement(const package : IPackageInfo);
begin
  FOpenRequirements.Enqueue(package);
end;

function TResolverContext.GetResolvedPackages: TArray<IResolvedPackage>;
begin
  result := FResolved.Values.ToArray;
end;

function TResolverContext.GetResolvedPackageInfos : IList<IPackageInfo>;
var
  resolution : IResolvedPackage;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for resolution in FResolved.Values do
    result.Add(resolution.PackageInfo);
end;

function TResolverContext.GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
begin
  result := nil;
  FVersionCache.TryGetValue(LowerCase(packageId), result);
end;

function TResolverContext.IsNoGood(const package : IPackageInfo) : boolean;
var
  nogoods : ISet<TPackageVersion>;
begin
  result := false;
  if FNoGoods.TryGetValue(LowerCase(package.Id), nogoods) then
  begin
    if nogoods.Contains(package.Version) then
      result := true;
  end;
end;

function TResolverContext.PopRequirement : IPackageInfo;
begin
  if FOpenRequirements.Any then
    result := FOpenRequirements.Dequeue
  else
    result := nil;
end;

function TResolverContext.ProjectFile: string;
begin
  result := FProjectFile;
end;

procedure TResolverContext.RecordNoGood(const bad : IPackageInfo);
var
  nogoods : ISet<TPackageVersion>;
begin
  if not FNoGoods.TryGetValue(LowerCase(bad.Id), nogoods) then
  begin
    nogoods := TCollections.CreateSet<TPackageVersion>;
    FNoGoods.Add(LowerCase(bad.Id), nogoods);
  end;
  nogoods.Add(bad.Version);
end;

procedure TResolverContext.RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
var
  resolution : IResolvedPackage;
begin
  if FResolved.ContainsKey(LowerCase(package.Id)) then
    raise Exception.Create('Resolution already exists for package [' + package.Id + ']');
  resolution := TResolvedPackage.Create(package, versionRange, parentId, FProjectFile);
  FResolved.Add(LowerCase(package.Id), resolution);
end;

procedure TResolverContext.RemoveResolvedPackage(const packageId : string);
begin
  if FResolved.ContainsKey(LowerCase(packageId)) then
    FResolved.Remove(LowerCase(packageId));
end;


function TResolverContext.TryGetResolvedPackage(const packageId : string; out resolution : IResolvedPackage) : boolean;
begin
  result := FResolved.TryGetValue(LowerCase(packageId), resolution);
end;

function TResolverContext.GetOrImportResolvedPackage(const packageId : string; const parentId : string; out resolution : IResolvedPackage) : boolean;

  procedure CopyDependencies(const res : IResolvedPackage);
  var
    i: Integer;
    id : string;
    childRes : IResolvedPackage;
  begin
    for i := 0 to res.PackageInfo.Dependencies.Count - 1 do
    begin
      id := res.PackageInfo.Dependencies[i].Id;
      //Defend against cyclic cross-project resolution graphs by tracking which ids we've
      //already pulled in. Without this guard, a sibling project whose resolved set has
      //A->B->A would re-enter the same nodes indefinitely.
      if FImportedIds.Contains(LowerCase(id)) then
        continue;
      childRes := FPackageInstallerContext.FindPackageResolution(FProjectFile, id);
      if childRes <> nil then
      begin
        FImportedIds.Add(LowerCase(id));
        FResolved[LowerCase(id)] := childRes;
        if childRes.PackageInfo.Dependencies.Any then
          CopyDependencies(childRes);
      end;
    end;
  end;

begin
  result := FResolved.TryGetValue(LowerCase(packageId), resolution);
  if result then
    exit;

  //Miss locally - consult the cross-project installer context. If a sibling project has
  //resolved this id, clone its resolution into ours under the supplied parentId and pull
  //in any transitive resolutions it relied on.
  resolution := FPackageInstallerContext.FindPackageResolution(FProjectFile, packageId);
  result := resolution <> nil;
  if not result then
    exit;

  FImportedIds.Add(LowerCase(packageId));
  FResolved[LowerCase(packageId)] := resolution.Clone(parentId) as IResolvedPackage;
  if resolution.PackageInfo.Dependencies.Any then
    CopyDependencies(resolution);
end;

end.


