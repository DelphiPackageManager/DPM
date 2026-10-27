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
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Tests.Dependency.Fakes;

type
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
    procedure Group_Project_Existing_Version_Mismatch_Is_Warning;

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

    //bundled dependencies (sentinel 999.999.999, e.g. Indy)
    [Test]
    procedure Bundled_Dependency_NoRealPackage_Resolves_AsNoop;

    [Test]
    procedure Bundled_Dependency_RealTopLevel_Wins;

    [Test]
    procedure Bundled_Dependency_RealTransitive_Wins;

    [Test]
    procedure Bundled_Dependency_TwoParents_SingleNode;

    [Test]
    procedure Bundled_Dependency_CrossProject_Real_Wins;

    [Test]
    procedure Bundled_Dependency_Excluded_From_Graph;

    //restore slow path : a package that is both a top level and a transient of an earlier top level
    [Test]
    procedure TopLevel_Already_In_ProjectRefs_As_Transient_Promotes_To_TopLevel;

    [Test]
    procedure TopLevel_Already_In_ProjectRefs_TopLevel_Version_Wins;

    //upgrade in all projects : a sibling's recorded resolutions must not carry back our own stale ones
    [Test]
    procedure Group_Upgrade_Does_Not_Loop_On_Own_Resolution_Recorded_By_Sibling;

    [Test]
    procedure Failed_Resolve_Records_Nothing_In_Group_Context;

    //resolved edges are checked, including those from project references that are never re-evaluated
    [Test]
    procedure Upgrading_Pinned_Transitive_Directly_Fails;

    [Test]
    procedure Preexisting_Inconsistency_Between_Unchanged_Refs_Is_Tolerated;

    [Test]
    procedure Restore_Tolerates_Recorded_Top_Level_Outside_Dependent_Range;

    [Test]
    procedure Imported_Sibling_Resolution_Not_Promoted_To_Top_Level;

    //several new top-level packages in one resolve (an upgrade and its companions)
    [Test]
    procedure Multi_Package_Resolve_Shares_One_Transitive;

    [Test]
    procedure Multi_Package_Replaces_Project_References_With_New_Ids;

    [Test]
    procedure Multi_Package_Conflict_Reported_In_Conflicts_List;

    //multi-project install operations (IPackageInstallerContext.BeginOperation)
    [Test]
    procedure Operation_Projects_Not_Yet_Processed_Do_Not_Block;

    [Test]
    procedure Operation_Later_Project_Checked_Against_Earlier_Staged_Resolutions;

    [Test]
    procedure Operation_Project_Out_Of_Scope_Still_Blocks;

    //The search (T1-T15) : a solution that exists is found, nothing is left behind by a branch that was given up,
    //only a conflict no choice can avoid is reported, and the output is the same every time.
    [Test]
    procedure T01_Exhausted_Dependency_Backtracks_To_An_Older_Parent;
    [Test]
    procedure T02_Preferred_Version_With_Failing_Dependencies_Falls_Back;
    [Test]
    procedure T03_Exact_Pin_Family_Finds_The_Versions_That_Fit;
    [Test]
    procedure T04_Narrow_Range_Lookup_Does_Not_Hide_Other_Versions;
    [Test]
    procedure T05_Cache_Versions_Exhausted_Falls_Back_To_The_Repository;
    [Test]
    procedure T06_Version_Rejected_For_One_Requester_Is_Still_Tried_Later;
    [Test]
    procedure T07_Narrowing_Does_Not_Follow_A_Requester_To_Another_Version;
    [Test]
    procedure T08_Backtracking_Leaves_No_Orphans;
    [Test]
    procedure T09_Backtracking_Undoes_Cross_Project_Imports;
    [Test]
    procedure T10_Conflict_In_An_Abandoned_Branch_Is_Not_Reported;
    [Test]
    procedure T11_Bundled_Dependency_Imported_From_A_Sibling_Is_Not_A_Top_Level;
    [Test]
    procedure T12_Dependency_Cycle_Is_Reported_Once_And_Quickly;
    [Test]
    procedure T13_Cancellation_Stops_Without_Errors;
    [Test]
    procedure T14_Layered_Diamonds_Resolve_In_Linear_Steps;
    [Test]
    procedure T15_Output_Is_Topological_And_Manifests_Are_Not_Reordered;
    [Test]
    procedure T16_Unavoidable_Conflict_Is_Reported_Once;
    [Test]
    procedure T17_Transient_The_New_Version_Drops_Is_Not_Output;
  end;

implementation

uses
  System.TypInfo,
  VSoft.CancellationToken.Impl,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Dependency,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Configuration.Classes,
  TestLogger;

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

procedure TDependencyResolverTests.Group_Project_Existing_Version_Mismatch_Is_Warning;
var
  x, b : IPackageInfo;
  siblingX : IResolvedPackage;
  xLocal : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  logger : TTestLogger;
  ok : boolean;
begin
  // Sibling project has resolved X 1.0.0 under range [1.0.0,1.0.0]. Current project's project
  // graph already carries X at 1.0.5. Installing B (which has nothing to do with X) doesn't change
  // X anywhere, so refusing the install would not fix the mismatch - it would only stop every
  // unrelated install in the group until the projects are brought in line by hand. The mismatch is
  // reported as a warning and the install goes ahead.
  x := MakeInfo('X', '1.0.0', []);
  b := MakeInfo('B', '1.0.0', []);
  siblingX := TResolvedPackage.Create(x, MakeRange('1.0.0'), 'root-node', 'C:\test\sibling.dproj');
  FContext.SeedResolutions('C:\test\sibling.dproj', [siblingX]);

  xLocal := MakeTopLevelRef(MakeInfo('X', '1.0.5', []), MakeRange('1.0.5'));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(xLocal);

  logger := FLogger as TTestLogger;
  logger.Clear;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, b, projectRefs, graph, resolved);
  Assert.IsTrue(ok, 'an existing project-group version mismatch the install does not touch must not fail the install');
  Assert.IsFalse(logger.Logged('error', 'project group'), 'an existing mismatch must not be logged as an error');
  Assert.IsTrue(logger.Logged('warning', 'X-1.0.5 in project [' + cTestProject + '] does not satisfy 1.0.0'),
    'the warning should name this project''s version first, then the range it does not satisfy');
  Assert.IsTrue(logger.Logged('warning', 'C:\test\sibling.dproj'), 'the warning should name the sibling project');
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

//-----------------------------------------------------------------------------
// Bundled dependency tests (sentinel 999.999.999)
//-----------------------------------------------------------------------------

procedure TDependencyResolverTests.Bundled_Dependency_NoRealPackage_Resolves_AsNoop;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  indyInfo : IPackageInfo;
begin
  // A declares a bundled dependency on Indy. No Indy package exists anywhere. Resolution must
  // succeed (contrast Missing_Repo_Package_Records_Unresolvable), the repo must never be queried
  // for Indy, and the resolved set must carry a synthetic bundled no-op for Indy.
  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy')]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'bundled dependency must resolve even with no real package present');
  Assert.AreEqual(0, FRepo.GetCallCount('Indy'), 'bundled dependency must never query the repository');

  indyInfo := nil;
  for info in resolved do
    if SameText(info.Id, 'Indy') then
      indyInfo := info;
  Assert.IsNotNull(indyInfo, 'a synthetic bundled Indy must be present in the resolved set');
  Assert.IsTrue(IsBundledPackageInfo(indyInfo), 'the Indy resolution must be the bundled no-op (999.999.999)');
end;

procedure TDependencyResolverTests.Bundled_Dependency_RealTopLevel_Wins;
var
  indy : IPackageInfo;
  a : IPackageInfo;
  refForIndy : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  indyCount : integer;
  selectedIndy : TPackageVersion;
begin
  // The user has added a real Indy 10.6.2 as a top-level package. A declares a bundled dependency
  // on Indy. The real package must win at its actual version - no synthetic no-op.
  indy := MakeInfo('Indy', '10.6.2', []);
  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy')]);

  refForIndy := MakeTopLevelRef(indy, MakeRange('10.6.2'));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(refForIndy);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  indyCount := 0;
  selectedIndy := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'Indy') then
    begin
      Inc(indyCount);
      selectedIndy := info.Version;
    end;
  Assert.AreEqual(1, indyCount, 'Indy must appear exactly once');
  Assert.AreEqual('10.6.2', selectedIndy.ToStringNoMeta, 'the real top-level Indy must win, not the bundled no-op');
end;

procedure TDependencyResolverTests.Bundled_Dependency_RealTransitive_Wins;
var
  indy : IPackageInfo;
  b : IPackageInfo;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  indyCount : integer;
  selectedIndy : TPackageVersion;
begin
  // A declares a bundled dependency on Indy AND depends on B, which depends on a real Indy in
  // range. The real transitive Indy must satisfy the bundled dependency - one real Indy, no no-op.
  indy := MakeInfo('Indy', '10.6.2', []);
  b := MakeInfo('B', '1.0.0', [MakeDep('Indy', '[10.6.0,]')]);
  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy'), MakeDep('B', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('Indy', ListOf([indy]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  indyCount := 0;
  selectedIndy := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'Indy') then
    begin
      Inc(indyCount);
      selectedIndy := info.Version;
    end;
  Assert.AreEqual(1, indyCount, 'Indy must appear exactly once');
  Assert.AreEqual('10.6.2', selectedIndy.ToStringNoMeta, 'the real transitive Indy must win, not the bundled no-op');
end;

procedure TDependencyResolverTests.Bundled_Dependency_TwoParents_SingleNode;
var
  b, c, a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  xCount : integer;
begin
  // Two packages (B and C) both declare a bundled dependency on the same id X. Only one synthetic
  // bundled node must be created.
  b := MakeInfo('B', '1.0.0', [MakeBundledDep('X')]);
  c := MakeInfo('C', '1.0.0', [MakeBundledDep('X')]);
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,]'), MakeDep('C', '[1.0.0,]')]);

  FRepo.SetVersions('B', ListOf([b]));
  FRepo.SetVersions('C', ListOf([c]));

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  xCount := 0;
  for info in resolved do
    if SameText(info.Id, 'X') then
      Inc(xCount);
  Assert.AreEqual(1, xCount, 'a bundled id required by two parents must resolve to a single node');
end;

procedure TDependencyResolverTests.Bundled_Dependency_CrossProject_Real_Wins;
var
  indy : IPackageInfo;
  resolvedIndy : IResolvedPackage;
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  indyCount : integer;
  selectedIndy : TPackageVersion;
begin
  // A sibling project has resolved a real Indy 10.6.2. A in this project declares a bundled
  // dependency on Indy. The imported sibling resolution must satisfy it - no synthetic no-op.
  indy := MakeInfo('Indy', '10.6.2', []);
  resolvedIndy := TResolvedPackage.Create(indy, MakeRange('10.6.2'), 'root-node', 'C:\test\sibling.dproj');
  FContext.SeedResolutions('C:\test\sibling.dproj', [resolvedIndy]);

  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy')]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  indyCount := 0;
  selectedIndy := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'Indy') then
    begin
      Inc(indyCount);
      selectedIndy := info.Version;
    end;
  Assert.AreEqual(1, indyCount, 'Indy must appear exactly once');
  Assert.AreEqual('10.6.2', selectedIndy.ToStringNoMeta, 'the imported sibling Indy must win, not the bundled no-op');
end;

procedure TDependencyResolverTests.Bundled_Dependency_Excluded_From_Graph;
var
  a : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  aNode : IPackageReference;
begin
  // The synthetic bundled no-op must never appear in the dproj dependency graph (else restore
  // would pin a dangling 999.999.999 reference). A's node must have no Indy child.
  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy')]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  Assert.IsNull(graph.FindFirstChild('Indy'), 'bundled Indy must not appear anywhere in the graph');
  aNode := graph.FindFirstChild('A');
  Assert.IsNotNull(aNode, 'A should be in the graph');
  Assert.IsFalse(aNode.HasAnyChild('Indy'), 'A must have no Indy child in the graph');
end;

procedure TDependencyResolverTests.TopLevel_Already_In_ProjectRefs_As_Transient_Promotes_To_TopLevel;
var
  a, c : IPackageInfo;
  root : IPackageReference;
  aRef, cRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  cNode : IPackageReference;
begin
  // Restore's slow path resolves one top level at a time, feeding the previous iteration's
  // flattened references (top levels AND transients) back in as projectReferences. A package
  // that is a top level of the project AND a transient of an earlier top level therefore
  // arrives in projectReferences and as newPackage in the same call. That must resolve, with
  // the package promoted to top level - not raise 'Resolution already exists'.
  c := MakeInfo('C', '0.1.6', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[0.1.4,]')]);
  FRepo.SetVersions('C', ListOf([c]));

  root := TPackageReference.CreateRoot(cTestCompiler);
  aRef := root.AddChild(a.Id, a.Version, MakeRange('1.0.0'));
  aRef.PackageInfo := a;
  cRef := aRef.AddChild(c.Id, c.Version, MakeRange('[0.1.4,]'));
  cRef.PackageInfo := c;

  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(aRef);
  projectRefs.Add(cRef);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, c, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed when the top level is already present as a transient');
  cNode := graph.FindTopLevelChild('C');
  Assert.IsNotNull(cNode, 'C must be promoted to a top level node in the graph');
  Assert.AreEqual('0.1.6', cNode.Version.ToStringNoMeta, 'C should be resolved at the top level version');
  Assert.IsNotNull(graph.FindTopLevelChild('A'), 'A must still be a top level node in the graph');
end;

procedure TDependencyResolverTests.TopLevel_Already_In_ProjectRefs_TopLevel_Version_Wins;
var
  a, c14, c16 : IPackageInfo;
  root : IPackageReference;
  aRef, cRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  selectedC : TPackageVersion;
begin
  // Same shape as above, but the earlier iteration resolved C at a different version than the
  // project's top level pin. The top level version is not negotiable, so it must win over the
  // version carried in projectReferences.
  c14 := MakeInfo('C', '0.1.4', []);
  c16 := MakeInfo('C', '0.1.6', []);
  a := MakeInfo('A', '1.0.0', [MakeDep('C', '[0.1.4,]')]);
  FRepo.SetVersions('C', ListOf([c16, c14]));

  root := TPackageReference.CreateRoot(cTestCompiler);
  aRef := root.AddChild(a.Id, a.Version, MakeRange('1.0.0'));
  aRef.PackageInfo := a;
  //previous iteration resolved C 0.1.4 as a transient of A
  cRef := aRef.AddChild(c14.Id, c14.Version, MakeRange('[0.1.4,]'));
  cRef.PackageInfo := c14;

  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(aRef);
  projectRefs.Add(cRef);

  //the project pins C 0.1.6 as a top level
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, c16, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should succeed');
  selectedC := MakeVersion('0.0.0');
  for info in resolved do
    if SameText(info.Id, 'C') then
      selectedC := info.Version;
  Assert.AreEqual('0.1.6', selectedC.ToStringNoMeta, 'the top level pin must win over the transient resolution');
end;

procedure TDependencyResolverTests.Group_Upgrade_Does_Not_Loop_On_Own_Resolution_Recorded_By_Sibling;
const
  cAppProject = 'C:\test\app.dproj';
  cTestsProject = 'C:\test\tests.dproj';
var
  base1, base3 : IPackageInfo;
  core1, core3 : IPackageInfo;
  ext1 : IPackageInfo;
  logger : TTestLogger;
  ok : boolean;

  //Both projects reference Core and Ext as top levels, each pinning Base exactly. The installer
  //removes the package being upgraded (Core) from the graph, so only Ext -> Base remains.
  function MakeProjectRefs : IList<IPackageReference>;
  var
    root : IPackageReference;
    extRef, baseRef : IPackageReference;
  begin
    root := TPackageReference.CreateRoot(cTestCompiler);
    extRef := root.AddChild(ext1.Id, ext1.Version, MakeRange('1.0.1'));
    extRef.PackageInfo := ext1;
    baseRef := extRef.AddChild(base1.Id, base1.Version, MakeRange('1.0.1'));
    baseRef.PackageInfo := base1;
    result := TCollections.CreateList<IPackageReference>;
    result.Add(extRef);
    result.Add(baseRef);
  end;

  function Resolve(const projectFile : string) : boolean;
  var
    resolved : IList<IPackageInfo>;
    graph : IPackageReference;
  begin
    result := FResolver.ResolveForInstall(FCancellation, cTestCompiler, projectFile, FOptions, core3, MakeProjectRefs, graph, resolved);
  end;

begin
  // Core 1.0.3 pins Base 1.0.3 while Ext 1.0.1 still pins Base 1.0.1 - a genuine conflict that must
  // fail. Earlier in the session the tests project adopted app's Base 1.0.1 through the installer
  // context, so its recorded resolutions are the ones app resolved (still naming app as their project
  // when handed to RecordResolutions). When app re-resolves, that record must not hand app back its
  // own (stale) resolution - doing so made app treat it as local and negotiable, so every backtrack
  // removed it and the next lookup re-imported it, until the iteration limit.
  base1 := MakeInfo('Base', '1.0.1', []);
  base3 := MakeInfo('Base', '1.0.3', []);
  core1 := MakeInfo('Core', '1.0.1', [MakeDep('Base', '1.0.1')]);
  core3 := MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')]);
  ext1 := MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]);
  FRepo.SetVersions('Base', ListOf([base3, base1]));
  FRepo.SetVersions('Ext', ListOf([ext1]));

  //app's resolutions from earlier in the session
  FContext.SeedResolutions(cAppProject, [TResolvedPackage.Create(core1, MakeRange('1.0.1'), cRootNode, cAppProject),
                                         TResolvedPackage.Create(ext1, MakeRange('1.0.1'), cRootNode, cAppProject),
                                         TResolvedPackage.Create(base1, MakeRange('1.0.1'), 'Ext', cAppProject)]);
  //the tests project's resolutions, adopted from app
  FContext.SeedResolutions(cTestsProject, [TResolvedPackage.Create(core1, MakeRange('1.0.1'), cRootNode, cAppProject),
                                           TResolvedPackage.Create(ext1, MakeRange('1.0.1'), cRootNode, cAppProject),
                                           TResolvedPackage.Create(base1, MakeRange('1.0.1'), 'Ext', cAppProject)]);

  logger := FLogger as TTestLogger;
  logger.Clear;
  ok := Resolve(cAppProject);
  Assert.IsFalse(ok, 'app project : Core 1.0.3 and Ext 1.0.1 cannot share a Base');
  Assert.IsFalse(logger.Logged('error', 'exceeded'), 'resolve must end on the conflict, not the iteration limit');
  Assert.IsTrue(logger.Logged('error', cTestsProject), 'the conflict should name the sibling project that holds Base 1.0.1');
end;

procedure TDependencyResolverTests.Failed_Resolve_Records_Nothing_In_Group_Context;
var
  base1, core3, ext1 : IPackageInfo;
  root, extRef, baseRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A failed install leaves the project as it was, so the group context must not claim the project
  // now uses Core 1.0.3 - other projects would be checked against versions it doesn't have.
  base1 := MakeInfo('Base', '1.0.1', []);
  core3 := MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')]);
  ext1 := MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]);
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), base1]));

  root := TPackageReference.CreateRoot(cTestCompiler);
  extRef := root.AddChild(ext1.Id, ext1.Version, MakeRange('1.0.1'));
  extRef.PackageInfo := ext1;
  baseRef := extRef.AddChild(base1.Id, base1.Version, MakeRange('1.0.1'));
  baseRef.PackageInfo := base1;
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(extRef);
  projectRefs.Add(baseRef);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, core3, projectRefs, graph, resolved);

  Assert.IsFalse(ok, 'Core 1.0.3 and Ext 1.0.1 cannot share a Base');
  Assert.IsNull(FContextIntf.FindPackageResolution('C:\test\other.dproj', 'Core'), 'a failed resolve must not be recorded for other projects to see');
end;

procedure TDependencyResolverTests.Upgrading_Pinned_Transitive_Directly_Fails;
var
  base1, base3, ext1 : IPackageInfo;
  root, extRef, baseRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project has Ext 1.0.1, which pins Base 1.0.1. Installing Base 1.0.3 directly must fail -
  // project references are recorded as resolved but never re-evaluated, so without checking the
  // resolved edges this "succeeded" and wrote a graph the next restore rejects.
  base1 := MakeInfo('Base', '1.0.1', []);
  base3 := MakeInfo('Base', '1.0.3', []);
  ext1 := MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]);

  root := TPackageReference.CreateRoot(cTestCompiler);
  extRef := root.AddChild(ext1.Id, ext1.Version, MakeRange('1.0.1'));
  extRef.PackageInfo := ext1;
  baseRef := extRef.AddChild(base1.Id, base1.Version, MakeRange('1.0.1'));
  baseRef.PackageInfo := base1;
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(extRef);
  projectRefs.Add(baseRef);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, base3, projectRefs, graph, resolved);

  Assert.IsFalse(ok, 'Ext 1.0.1 pins Base 1.0.1 - Base 1.0.3 cannot be installed alongside it');
end;

procedure TDependencyResolverTests.Preexisting_Inconsistency_Between_Unchanged_Refs_Is_Tolerated;
var
  base2, ext1, foo : IPackageInfo;
  root, extRef, baseRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project already has Ext 1.0.1 (pins Base 1.0.1) next to Base 1.0.2 - inconsistent, but
  // nothing to do with installing Foo. Checking the resolved edges must not make that unrelated
  // install fail.
  base2 := MakeInfo('Base', '1.0.2', []);
  ext1 := MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]);
  foo := MakeInfo('Foo', '1.0.0', []);

  root := TPackageReference.CreateRoot(cTestCompiler);
  extRef := root.AddChild(ext1.Id, ext1.Version, MakeRange('1.0.1'));
  extRef.PackageInfo := ext1;
  baseRef := extRef.AddChild(base2.Id, base2.Version, MakeRange('1.0.1'));
  baseRef.PackageInfo := base2;
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(extRef);
  projectRefs.Add(baseRef);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, foo, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'an unrelated install must not fail on an inconsistency that was already there');
end;

procedure TDependencyResolverTests.Restore_Tolerates_Recorded_Top_Level_Outside_Dependent_Range;
var
  console, ansi : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  preferred : IDictionary<string, TPackageVersion>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project pins Console 1.3.0 as a top level, while Ansi 0.3.0 asks for [1.2.4,] (1.2.x only).
  // Restore's slow path re-resolves each top level as a 'new' package, but Console is still at the version
  // the project records (the preferred version), so this inconsistency was already there - restore must not
  // start failing on it.
  console := MakeInfo('Console', '1.3.0', []);
  ansi := MakeInfo('Ansi', '0.3.0', [MakeDep('Console', '[1.2.4,]')]);
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(MakeTopLevelRef(ansi, MakeRange('0.3.0')));
  preferred := TCollections.CreateDictionary<string, TPackageVersion>;
  preferred['console'] := console.Version;
  preferred['ansi'] := ansi.Version;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, console, projectRefs, graph, resolved, nil, preferred);

  Assert.IsTrue(ok, 'a recorded top level outside a dependent''s range is pre-existing - restore must still succeed');
end;

procedure TDependencyResolverTests.Imported_Sibling_Resolution_Not_Promoted_To_Top_Level;
const
  cSibling = 'C:\test\sibling.dproj';
var
  base1, core1, app : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  coreNode : IPackageReference;
begin
  // The sibling has Base as a top level of its own, and Core (which uses Base). This project adopts
  // the sibling's Core, which pulls in the sibling's Base too. Base is not a top level HERE - it is
  // Core's dependency - so it must end up under Core, not as a top level of this project's graph.
  base1 := MakeInfo('Base', '1.0.1', []);
  core1 := MakeInfo('Core', '1.0.1', [MakeDep('Base', '1.0.1')]);
  app := MakeInfo('App', '1.0.0', [MakeDep('Core', '1.0.1')]);
  FContext.SeedResolutions(cSibling, [TResolvedPackage.Create(core1, MakeRange('1.0.1'), cRootNode, cSibling),
                                      TResolvedPackage.Create(base1, MakeRange('1.0.1'), cRootNode, cSibling)]);

  projectRefs := TCollections.CreateList<IPackageReference>;
  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, projectRefs, graph, resolved);

  Assert.IsTrue(ok, 'resolve should adopt the sibling''s Core and Base');
  Assert.IsNull(graph.FindTopLevelChild('Base'), 'Base is only a dependency of Core in this project');
  coreNode := graph.FindFirstChild('Core');
  Assert.IsNotNull(coreNode, 'Core should be in the graph');
  Assert.IsNotNull(coreNode.FindFirstChild('Base'), 'Base should be under Core');
end;

//Spring4D style family : Core and Ext both pin Base exactly, so they can only move together.
function FamilyNewPackages(const coreVersion, extVersion : string) : IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
  result.Add(MakeInfo('Core', coreVersion, [MakeDep('Base', coreVersion)]));
  result.Add(MakeInfo('Ext', extVersion, [MakeDep('Base', extVersion)]));
end;

procedure TDependencyResolverTests.Multi_Package_Resolve_Shares_One_Transitive;
var
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  info : IPackageInfo;
  baseCount : integer;
begin
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));
  projectRefs := TCollections.CreateList<IPackageReference>;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);

  Assert.IsTrue(ok, 'Core 1.0.3 and Ext 1.0.3 both want Base 1.0.3');
  baseCount := 0;
  for info in resolved do
    if SameText(info.Id, 'Base') then
    begin
      Inc(baseCount);
      Assert.AreEqual('1.0.3', info.Version.ToStringNoMeta);
    end;
  Assert.AreEqual(1, baseCount, 'Base should be resolved once');
  Assert.IsNotNull(graph.FindTopLevelChild('Core'), 'Core should be a top level');
  Assert.IsNotNull(graph.FindTopLevelChild('Ext'), 'Ext should be a top level');
  Assert.IsNotNull(graph.FindTopLevelChild('Ext').FindFirstChild('Base'), 'Base should be under Ext as well as Core');
end;

procedure TDependencyResolverTests.Multi_Package_Replaces_Project_References_With_New_Ids;
var
  ext1 : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project already references Ext 1.0.1 - the upgrade replaces it with 1.0.3 rather than raising
  // 'Resolution already exists'.
  ext1 := MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]);
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', [])]));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(MakeTopLevelRef(ext1, MakeRange('1.0.1')));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);

  Assert.IsTrue(ok, 'the new Ext should replace the referenced one');
  Assert.AreEqual('1.0.3', graph.FindTopLevelChild('Ext').Version.ToStringNoMeta);
end;

procedure TDependencyResolverTests.Multi_Package_Conflict_Reported_In_Conflicts_List;
var
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  conflicts : IList<TUnresolvableConflict>;
  conflict : TUnresolvableConflict;
  baseNamed : boolean;
begin
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));
  projectRefs := TCollections.CreateList<IPackageReference>;
  conflicts := TCollections.CreateList<TUnresolvableConflict>;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, FamilyNewPackages('1.0.3', '1.0.1'), projectRefs, graph, resolved, nil, nil, conflicts);

  Assert.IsFalse(ok, 'Core 1.0.3 and Ext 1.0.1 cannot share a Base');
  baseNamed := false;
  for conflict in conflicts do
    if SameText(conflict.PackageId, 'Base') then
      baseNamed := true;
  Assert.IsTrue(baseNamed, 'the conflicts list should name Base');
end;

procedure TDependencyResolverTests.Operation_Projects_Not_Yet_Processed_Do_Not_Block;
const
  cAppProject = 'C:\test\app.dproj';
  cTestsProject = 'C:\test\tests.dproj';
var
  projects : IList<string>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Both projects are being upgraded. The tests project still has its old Base 1.0.1 recorded, but
  // that is about to be replaced, so it must not block app.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));
  FContext.SeedResolutions(cTestsProject, [TResolvedPackage.Create(MakeInfo('Base', '1.0.1', []), MakeRange('1.0.1'), 'Ext', cTestsProject)]);
  projectRefs := TCollections.CreateList<IPackageReference>;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cAppProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);
  Assert.IsFalse(ok, 'outside an operation the tests project''s Base 1.0.1 blocks app');

  projects := TCollections.CreateList<string>;
  projects.Add(cAppProject);
  projects.Add(cTestsProject);
  FContextIntf.BeginOperation(projects);
  try
    ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cAppProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);
    Assert.IsTrue(ok, 'in the operation the tests project''s old record must not block app');
  finally
    FContextIntf.EndOperation;
  end;
end;

procedure TDependencyResolverTests.Operation_Later_Project_Checked_Against_Earlier_Staged_Resolutions;
const
  cAppProject = 'C:\test\app.dproj';
  cTestsProject = 'C:\test\tests.dproj';
var
  projects : IList<string>;
  projectRefs : IList<IPackageReference>;
  newPackages : IList<IPackageInfo>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  logger : TTestLogger;
begin
  // app resolves first and stages Base 1.0.3. The tests project, processed next, asks for Ext 1.0.1
  // (Base 1.0.1) - it must be checked against app's NEW resolution and fail.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));
  projectRefs := TCollections.CreateList<IPackageReference>;
  projects := TCollections.CreateList<string>;
  projects.Add(cAppProject);
  projects.Add(cTestsProject);
  logger := FLogger as TTestLogger;
  FContextIntf.BeginOperation(projects);
  try
    ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cAppProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);
    Assert.IsTrue(ok, 'app should resolve');

    newPackages := TCollections.CreateList<IPackageInfo>;
    newPackages.Add(MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]));
    logger.Clear;
    ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestsProject, FOptions, newPackages, projectRefs, graph, resolved, nil, nil, nil);
    Assert.IsFalse(ok, 'tests must conflict with app''s staged Base 1.0.3');
    Assert.IsTrue(logger.Logged('error', cAppProject), 'the conflict should name app');
  finally
    FContextIntf.EndOperation;
  end;
end;

procedure TDependencyResolverTests.Operation_Project_Out_Of_Scope_Still_Blocks;
const
  cAppProject = 'C:\test\app.dproj';
  cOtherProject = 'C:\test\other.dproj';
var
  projects : IList<string>;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  logger : TTestLogger;
begin
  // other is not part of the upgrade - its Base 1.0.1 is real and must still block app.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));
  FContext.SeedResolutions(cOtherProject, [TResolvedPackage.Create(MakeInfo('Base', '1.0.1', []), MakeRange('1.0.1'), cRootNode, cOtherProject)]);
  projectRefs := TCollections.CreateList<IPackageReference>;
  projects := TCollections.CreateList<string>;
  projects.Add(cAppProject);
  logger := FLogger as TTestLogger;
  logger.Clear;
  FContextIntf.BeginOperation(projects);
  try
    ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cAppProject, FOptions, FamilyNewPackages('1.0.3', '1.0.3'), projectRefs, graph, resolved, nil, nil, nil);
    Assert.IsFalse(ok, 'a project outside the operation still constrains app');
    Assert.IsTrue(logger.Logged('error', cOtherProject), 'the conflict should name the other project');
  finally
    FContextIntf.EndOperation;
  end;
end;

//-----------------------------------------------------------------------------
// The search (T1-T15)
//-----------------------------------------------------------------------------

function LogText(const logger : ILogger) : string;
begin
  result := (logger as TTestLogger).Messages.Text;
end;

//the version resolved for id, '' when it wasn't.
function VersionOf(const resolved : IList<IPackageInfo>; const id : string) : string;
var
  info : IPackageInfo;
begin
  result := '';
  if resolved = nil then
    exit;
  for info in resolved do
    if SameText(info.Id, id) then
      exit(info.Version.ToStringNoMeta);
end;

//every transitive edge is satisfied by the version below it, and an id has one version.
procedure AssertGraphConsistent(const graph : IPackageReference);
var
  versions : IDictionary<string, string>;
begin
  Assert.IsNotNull(graph, 'no graph');
  versions := TCollections.CreateDictionary<string, string>;
  graph.VisitDFS(
    procedure(const node : IPackageReference)
    var
      seen : string;
    begin
      if (node.Parent <> nil) and (not node.Parent.IsRoot) then
        Assert.IsTrue(node.VersionRange.IsSatisfiedBy(node.Version),
          node.Parent.Id + '-' + node.Parent.Version.ToStringNoMeta + ' requires ' + node.Id + ' ' + node.VersionRange.ToString +
          ' but the graph has ' + node.Version.ToStringNoMeta);
      if versions.TryGetValue(LowerCase(node.Id), seen) then
        Assert.AreEqual(seen, node.Version.ToStringNoMeta, node.Id + ' appears at two versions')
      else
        versions[LowerCase(node.Id)] := node.Version.ToStringNoMeta;
    end);
end;

procedure TDependencyResolverTests.T01_Exhausted_Dependency_Backtracks_To_An_Older_Parent;
var
  root : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Every B needs a C 2.0.x nobody has, so A 1.0.2 - the A that needs B - can't be used. That is A 1.0.2's
  // failure, not the install's : A 1.0.1 works.
  root := MakeInfo('Root', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.2', [MakeDep('B', '[1.0.0,1.0.9]')]),
                                 MakeInfo('A', '1.0.1', [MakeDep('C', '[1.0.0,1.0.9]')])]));
  FRepo.SetVersions('B', ListOf([MakeInfo('B', '1.0.1', [MakeDep('C', '[2.0.0,2.0.9]')]),
                                 MakeInfo('B', '1.0.0', [MakeDep('C', '[2.0.0,2.0.9]')])]));
  FRepo.SetVersions('C', ListOf([MakeInfo('C', '1.0.0', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, root, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'A'));
  Assert.AreEqual('1.0.0', VersionOf(resolved, 'C'));
  Assert.AreEqual('', VersionOf(resolved, 'B'), 'only the A that was given up needs B');
  AssertGraphConsistent(graph);
end;

procedure TDependencyResolverTests.T02_Preferred_Version_With_Failing_Dependencies_Falls_Back;
var
  c02, c05, root : IPackageInfo;
  preferred : IDictionary<string, TPackageVersion>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project's graph records C 1.0.2, which needs a D nobody has. Preferring the recorded version must not
  // mean failing with it - another C in range works.
  c02 := MakeInfo('C', '1.0.2', [MakeDep('D', '[9.0.0,9.0.9]')]);
  c05 := MakeInfo('C', '1.0.5', []);
  root := MakeInfo('Root', '1.0.0', [MakeDep('C', '[1.0.0,1.0.9]')]);
  FCache.SetCachedVersions('C', ListOf([c05, c02]));
  FRepo.SetVersions('C', ListOf([c05, c02]));
  preferred := TCollections.CreateDictionary<string, TPackageVersion>;
  preferred['c'] := c02.Version;

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, root, TCollections.CreateList<IPackageReference>,
                                    graph, resolved, nil, preferred);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.5', VersionOf(resolved, 'C'));
end;

procedure TDependencyResolverTests.T03_Exact_Pin_Family_Finds_The_Versions_That_Fit;
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Spring4D style : Core and Ext pin Base exactly, so they only fit at the same version. The newest Core
  // (1.0.3) has no Ext to go with it - 1.0.1 of all three does.
  app := MakeInfo('App', '1.0.0', [MakeDep('Core', '[1.0.0,1.0.9]'), MakeDep('Ext', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('Core', ListOf([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')]), MakeInfo('Core', '1.0.1', [MakeDep('Base', '1.0.1')])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')])]));
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'Core'));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'Ext'));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'Base'));
  AssertGraphConsistent(graph);
end;

procedure TDependencyResolverTests.T04_Narrow_Range_Lookup_Does_Not_Hide_Other_Versions;
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // X is first looked up for P 1.0.1's exact 1.0.1, which Q rules out. Once P drops to 1.0.0, X 1.0.5 is
  // needed - the earlier lookup for the narrow range must not stand in for this one.
  app := MakeInfo('App', '1.0.0', [MakeDep('P', '[1.0.0,1.0.9]'), MakeDep('Q', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('P', ListOf([MakeInfo('P', '1.0.1', [MakeDep('X', '1.0.1')]), MakeInfo('P', '1.0.0', [MakeDep('X', '[1.0.0,1.0.9]')])]));
  FRepo.SetVersions('Q', ListOf([MakeInfo('Q', '1.0.0', [MakeDep('X', '[1.0.2,1.0.9]')])]));
  FRepo.SetVersions('X', ListOf([MakeInfo('X', '1.0.5', []), MakeInfo('X', '1.0.1', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.0', VersionOf(resolved, 'P'));
  Assert.AreEqual('1.0.5', VersionOf(resolved, 'X'));
  AssertGraphConsistent(graph);
end;

procedure TDependencyResolverTests.T05_Cache_Versions_Exhausted_Falls_Back_To_The_Repository;
var
  root : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The cache only has C 1.0.2, which can't be used (no D). The repository has C 1.0.1 as well.
  root := MakeInfo('Root', '1.0.0', [MakeDep('C', '[1.0.0,1.0.9]')]);
  FCache.SetCachedVersions('C', ListOf([MakeInfo('C', '1.0.2', [MakeDep('D', '[9.0.0,9.0.9]')])]));
  FRepo.SetVersions('C', ListOf([MakeInfo('C', '1.0.2', [MakeDep('D', '[9.0.0,9.0.9]')]), MakeInfo('C', '1.0.1', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, root, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'C'));
  Assert.AreEqual(1, FRepo.GetCallCount('C'), 'the repository is asked once, when the cache''s versions are used up');
end;

procedure TDependencyResolverTests.T06_Version_Rejected_For_One_Requester_Is_Still_Tried_Later;
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A only works at 1.0.2 (X 2.0.0). B 1.0.1 wants X 1.0.0, which rules out X 2.0.0 - but only while B is at
  // 1.0.1. B 1.0.0 wants X 2.0.0 too, so X 2.0.0 is the answer once B moves.
  app := MakeInfo('App', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]'), MakeDep('B', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.2', [MakeDep('X', '2.0.0')]), MakeInfo('A', '1.0.1', [MakeDep('Y', '[9.0.0,9.0.9]')])]));
  FRepo.SetVersions('B', ListOf([MakeInfo('B', '1.0.1', [MakeDep('X', '1.0.0')]), MakeInfo('B', '1.0.0', [MakeDep('X', '2.0.0')])]));
  FRepo.SetVersions('X', ListOf([MakeInfo('X', '2.0.0', []), MakeInfo('X', '1.0.0', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.2', VersionOf(resolved, 'A'));
  Assert.AreEqual('1.0.0', VersionOf(resolved, 'B'));
  Assert.AreEqual('2.0.0', VersionOf(resolved, 'X'));
  AssertGraphConsistent(graph);
end;

procedure TDependencyResolverTests.T07_Narrowing_Does_Not_Follow_A_Requester_To_Another_Version;
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Q 1.0.2 holds D to [1.0.0,1.0.3], then fails (no M). Q 1.0.1 wants D [1.0.6,1.0.9] - what Q 1.0.2 asked for
  // must go with it.
  app := MakeInfo('App', '1.0.0', [MakeDep('P', '[1.0.0,1.0.9]'), MakeDep('Q', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('P', ListOf([MakeInfo('P', '1.0.0', [MakeDep('D', '[1.0.0,1.0.9]')])]));
  FRepo.SetVersions('Q', ListOf([MakeInfo('Q', '1.0.2', [MakeDep('D', '[1.0.0,1.0.3]'), MakeDep('M', '[1.0.0,1.0.99]')]),
                                 MakeInfo('Q', '1.0.1', [MakeDep('D', '[1.0.6,1.0.9]')])]));
  FRepo.SetVersions('D', ListOf([MakeInfo('D', '1.0.9', []), MakeInfo('D', '1.0.7', []), MakeInfo('D', '1.0.3', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'Q'));
  Assert.AreEqual('1.0.9', VersionOf(resolved, 'D'));
  AssertGraphConsistent(graph);
end;

procedure TDependencyResolverTests.T08_Backtracking_Leaves_No_Orphans;
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A 1.0.2 gets as far as choosing C before it fails (no B). A 1.0.1 doesn't use C - so nothing may install it.
  app := MakeInfo('App', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.2', [MakeDep('C', '[1.0.0,1.0.9]'), MakeDep('B', '[9.0.0,9.0.99]')]),
                                 MakeInfo('A', '1.0.1', [])]));
  FRepo.SetVersions('C', ListOf([MakeInfo('C', '1.0.0', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'A'));
  Assert.AreEqual('', VersionOf(resolved, 'C'), 'C was only needed by the A that was given up');
  Assert.IsNull(graph.FindFirstChild('C'));
  Assert.AreEqual(2, resolved.Count, 'App and A');
end;

procedure TDependencyResolverTests.T09_Backtracking_Undoes_Cross_Project_Imports;
const
  cSibling = 'C:\test\sibling.dproj';
var
  app : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // A 1.0.2 adopts the sibling's S (and S's T) before it fails. A 1.0.1 uses neither.
  FContext.SeedResolutions(cSibling, [TResolvedPackage.Create(MakeInfo('S', '1.0.0', [MakeDep('T', '[1.0.0,1.0.9]')]), MakeRange('1.0.0'), cRootNode, cSibling),
                                      TResolvedPackage.Create(MakeInfo('T', '1.0.0', []), MakeRange('[1.0.0,1.0.9]'), 'S', cSibling)]);
  app := MakeInfo('App', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.2', [MakeDep('S', '[1.0.0,1.0.9]'), MakeDep('B', '[9.0.0,9.0.99]')]),
                                 MakeInfo('A', '1.0.1', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'A'));
  Assert.AreEqual('', VersionOf(resolved, 'S'), 'S was only adopted for the A that was given up');
  Assert.AreEqual('', VersionOf(resolved, 'T'), 'nor T');
end;

procedure TDependencyResolverTests.T10_Conflict_In_An_Abandoned_Branch_Is_Not_Reported;
var
  app : IPackageInfo;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project references T 1.0.0 directly. A 1.0.2 wants T 2.0.x - a conflict with the top level, but only
  // for A 1.0.2. A 1.0.1 is fine with T 1.0.0, so there is nothing to report.
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(MakeTopLevelRef(MakeInfo('T', '1.0.0', []), MakeRange('1.0.0')));
  app := MakeInfo('App', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.2', [MakeDep('T', '[2.0.0,2.0.9]')]), MakeInfo('A', '1.0.1', [MakeDep('T', '[1.0.0,1.0.9]')])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, projectRefs, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, 'the conflict belonged to a version that was given up : ' + LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'A'));
end;

procedure TDependencyResolverTests.T11_Bundled_Dependency_Imported_From_A_Sibling_Is_Not_A_Top_Level;
const
  cSibling = 'C:\test\sibling.dproj';
var
  a : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  aNode : IPackageReference;
begin
  // A's bundled Indy is satisfied by the sibling's real Indy - adopted for A, so it belongs under A. It is
  // not a package this project references directly.
  FContext.SeedResolutions(cSibling, [TResolvedPackage.Create(MakeInfo('Indy', '10.6.2', []), MakeRange('10.6.2'), cRootNode, cSibling)]);
  a := MakeInfo('A', '1.0.0', [MakeBundledDep('Indy')]);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual('10.6.2', VersionOf(resolved, 'Indy'));
  Assert.IsNull(graph.FindTopLevelChild('Indy'), 'the adopted Indy is not a top level');
  aNode := graph.FindTopLevelChild('A');
  Assert.IsNotNull(aNode);
  Assert.IsNotNull(aNode.FindFirstChild('Indy'), 'the adopted Indy belongs under A');
end;

procedure TDependencyResolverTests.T12_Dependency_Cycle_Is_Reported_Once_And_Quickly;
var
  a : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  a := MakeInfo('A', '1.0.0', [MakeDep('B', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('B', ListOf([MakeInfo('B', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]')])]));
  FRepo.SetVersions('A', ListOf([a]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsFalse(ok);
  Assert.AreEqual(1, FLogger.ErrorCount, LogText(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).HasError(TErrorCode.E2013_DependencyCycle), LogText(FLogger));
  Assert.IsTrue(TDependencyResolver.LastStepCount < 100, 'a cycle is found, not run into the step limit : ' + IntToStr(TDependencyResolver.LastStepCount));
end;

procedure TDependencyResolverTests.T13_Cancellation_Stops_Without_Errors;
var
  app : IPackageInfo;
  source : ICancellationTokenSource;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Ctrl+C while B's versions are looked up - the lookup finds nothing, which is not a missing dependency.
  // Whoever owns the token says it was cancelled; the resolver says nothing.
  source := TCancellationTokenSourceFactory.Create;
  app := MakeInfo('App', '1.0.0', [MakeDep('A', '[1.0.0,1.0.9]'), MakeDep('B', '[1.0.0,1.0.9]'), MakeDep('C', '[1.0.0,1.0.9]')]);
  FRepo.SetVersions('A', ListOf([MakeInfo('A', '1.0.0', [])]));
  FRepo.SetVersions('B', ListOf([MakeInfo('B', '1.0.0', [])]));
  FRepo.SetVersions('C', ListOf([MakeInfo('C', '1.0.0', [])]));
  FRepo.CancelOnCallFor('B', source);

  ok := FResolver.ResolveForInstall(source.Token, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsFalse(ok);
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  Assert.AreEqual(0, FRepo.GetCallCount('C'), 'the search stops when it is cancelled');
end;

procedure TDependencyResolverTests.T14_Layered_Diamonds_Resolve_In_Linear_Steps;
const
  cLayers = 8;
  cNames : array[0..1] of string = ('A', 'B');
var
  layer : integer;
  name : string;
  version : integer;
  versions : IList<IPackageInfo>;
  root : IPackageInfo;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
  range : string;
  expected : string;
begin
  // Each layer's A asks for the next layer's A and B in [1.0.0,1.0.9], its B for [1.0.0,1.0.5] - so below the
  // top every package ends up at 1.0.5, and every package is needed by two others.
  for layer := 1 to cLayers do
    for name in cNames do
    begin
      if name = 'A' then
        range := '[1.0.0,1.0.9]'
      else
        range := '[1.0.0,1.0.5]';
      versions := TCollections.CreateList<IPackageInfo>;
      for version := 9 downto 0 do
      begin
        if layer < cLayers then
          versions.Add(MakeInfo(name + IntToStr(layer), '1.0.' + IntToStr(version), [MakeDep('A' + IntToStr(layer + 1), range), MakeDep('B' + IntToStr(layer + 1), range)]))
        else
          versions.Add(MakeInfo(name + IntToStr(layer), '1.0.' + IntToStr(version), []));
      end;
      FRepo.SetVersions(name + IntToStr(layer), versions);
    end;
  root := MakeInfo('Root', '1.0.0', [MakeDep('A1', '[1.0.0,1.0.9]'), MakeDep('B1', '[1.0.0,1.0.9]')]);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, root, TCollections.CreateList<IPackageReference>, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual(0, FLogger.ErrorCount, LogText(FLogger));
  for layer := 1 to cLayers do
    for name in cNames do
    begin
      if layer = 1 then
        expected := '1.0.9'
      else
        expected := '1.0.5';
      Assert.AreEqual(expected, VersionOf(resolved, name + IntToStr(layer)), name + IntToStr(layer));
    end;
  AssertGraphConsistent(graph);
  //30 requirements between 17 packages.
  Assert.IsTrue(TDependencyResolver.LastStepCount <= 200, 'took ' + IntToStr(TDependencyResolver.LastStepCount) + ' steps');
end;

procedure TDependencyResolverTests.T15_Output_Is_Topological_And_Manifests_Are_Not_Reordered;
var
  infos : IList<IPackageInfo>;
  app : IPackageInfo;
  firstOrder : string;
  secondOrder : string;

  function ResolveOrder : string;
  var
    context : IPackageInstallerContext;
    resolver : IDependencyResolver;
    resolved : IList<IPackageInfo>;
    graph : IPackageReference;
    seen : ISet<string>;
    info : IPackageInfo;
    dependency : IPackageDependency;
  begin
    context := TFakeInstallerContext.Create(FLogger);
    resolver := TDependencyResolver.Create(FLogger, FRepoIntf, FCacheIntf, context);
    resolver.Initialize(FConfig);
    Assert.IsTrue(resolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, app, TCollections.CreateList<IPackageReference>, graph, resolved),
                  LogText(FLogger));
    result := '';
    seen := TCollections.CreateSet<string>;
    for info in resolved do
    begin
      for dependency in info.Dependencies do
        Assert.IsTrue(seen.Contains(LowerCase(dependency.Id)), info.Id + ' comes before its dependency ' + dependency.Id);
      seen.Add(LowerCase(info.Id));
      result := result + info.Id + ' ';
    end;
  end;

begin
  // Z is listed before Y in App's manifest, though Y's range is the narrower one.
  infos := ListOf([MakeInfo('X', '1.0.0', [])]);
  FRepo.SetVersions('X', infos);
  FRepo.SetVersions('Z', ListOf([MakeInfo('Z', '1.0.0', [MakeDep('X', '[1.0.0,1.0.9]')])]));
  FRepo.SetVersions('Y', ListOf([MakeInfo('Y', '1.0.0', [MakeDep('X', '[1.0.0,1.0.9]')])]));
  app := MakeInfo('App', '1.0.0', [MakeDep('Z', '[1.0.0,1.0.99]'), MakeDep('Y', '1.0.0')]);

  firstOrder := ResolveOrder;
  Assert.AreEqual('Z', app.Dependencies[0].Id, 'the manifest''s dependencies must keep their order');
  secondOrder := ResolveOrder;
  Assert.AreEqual(firstOrder, secondOrder, 'the same input resolves in the same order');
end;

procedure TDependencyResolverTests.T16_Unavoidable_Conflict_Is_Reported_Once;
var
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // Core 1.0.3 and Ext 1.0.1 are both being installed, and pin different Bases - one problem, found from Core's
  // requirement and again from Ext's.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.1', [])]));

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, FamilyNewPackages('1.0.3', '1.0.1'),
                                    TCollections.CreateList<IPackageReference>, graph, resolved, nil, nil, nil);

  Assert.IsFalse(ok);
  Assert.AreEqual(1, FLogger.ErrorCount, LogText(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).HasError(TErrorCode.E2005_SelectedVersionConflict), LogText(FLogger));
  Assert.IsTrue((FLogger as TTestLogger).Logged('error', 'Ext-1.0.1 requires 1.0.1'), 'the error names both requirements : ' + LogText(FLogger));
end;

procedure TDependencyResolverTests.T17_Transient_The_New_Version_Drops_Is_Not_Output;
var
  a100 : IPackageInfo;
  a101 : IPackageInfo;
  x : IPackageInfo;
  root : IPackageReference;
  aRef : IPackageReference;
  xRef : IPackageReference;
  projectRefs : IList<IPackageReference>;
  resolved : IList<IPackageInfo>;
  graph : IPackageReference;
  ok : boolean;
begin
  // The project has A 1.0.0 -> X 1.0.0. A 1.0.1 no longer uses X - X is still among the project's references,
  // but nothing being installed needs it, so it is not installed.
  x := MakeInfo('X', '1.0.0', []);
  a100 := MakeInfo('A', '1.0.0', [MakeDep('X', '[1.0.0,]')]);
  a101 := MakeInfo('A', '1.0.1', []);

  root := TPackageReference.CreateRoot(cTestCompiler);
  aRef := root.AddChild(a100.Id, a100.Version, MakeRange('1.0.0'));
  aRef.PackageInfo := a100;
  xRef := aRef.AddChild(x.Id, x.Version, MakeRange('[1.0.0,]'));
  xRef.PackageInfo := x;
  projectRefs := TCollections.CreateList<IPackageReference>;
  projectRefs.Add(aRef);
  projectRefs.Add(xRef);

  ok := FResolver.ResolveForInstall(FCancellation, cTestCompiler, cTestProject, FOptions, a101, projectRefs, graph, resolved);

  Assert.IsTrue(ok, LogText(FLogger));
  Assert.AreEqual('1.0.1', VersionOf(resolved, 'A'));
  Assert.AreEqual('', VersionOf(resolved, 'X'), 'X was only needed by the A being replaced');
  Assert.IsNull(graph.FindFirstChild('X'));
  Assert.AreEqual(1, resolved.Count);
end;

initialization
  TDUnitX.RegisterTestFixture(TDependencyResolverTests);

end.
