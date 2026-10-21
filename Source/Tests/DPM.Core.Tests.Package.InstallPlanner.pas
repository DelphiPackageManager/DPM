unit DPM.Core.Tests.Package.InstallPlanner;

interface

uses
  DUnitX.TestFramework,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Tests.Dependency.Fakes;

type
  [TestFixture]
  TInstallPlannerTests = class
  private
    FRepo : TFakeRepositoryManager;
    FCache : TFakePackageCache;
    FRepoIntf : IPackageRepositoryManager;
    FCacheIntf : IPackageCache;
    FLogger : ILogger;
    FConfig : IConfiguration;
    FPlanner : IInstallPlanner;
    FCancellation : ICancellationToken;
    FOptions : TSearchOptions;
    FProjects : IList<TInstallPlanProject>;
    function AddProject(const projectFile : string) : IPackageReference;
    function MakePlan(const requested : array of IPackageInfo; const upgrade : boolean = true) : IInstallPlan;
    function FindItem(const plan : IInstallPlan; const id : string) : IInstallPlanItem;
    //the Spring4D shape : Core and Ext each pin Base exactly.
    procedure SetupFamily(const versions : array of string);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Package_Pinning_Shared_Dependency_Becomes_Companion;

    [Test]
    procedure Newest_Compatible_Companion_Version_Is_Chosen;

    [Test]
    procedure No_Compatible_Companion_Fails_The_Plan;

    [Test]
    procedure Floating_Range_Is_Not_A_Blocker;

    [Test]
    procedure Pinned_Top_Level_Dependency_Becomes_Companion;

    [Test]
    procedure Upgrading_Dependency_Directly_Adds_Its_Dependents;

    [Test]
    procedure Companion_Of_Companion_Found_In_Next_Round;

    [Test]
    procedure Upgrade_Never_Downgrades_Companions;

    [Test]
    procedure Downgrade_Moves_Companions_Down;

    [Test]
    procedure Summary_Puts_Reason_And_Projects_On_Lines_Of_Their_Own;

    [Test]
    procedure Prerelease_Companion_Only_When_Allowed;

    [Test]
    procedure Upgrade_Applies_Packages_Only_Where_Top_Level;

    [Test]
    procedure Requested_Package_Not_Installed_Fails_Upgrade;

    [Test]
    procedure Install_Gives_Every_Package_To_Every_Project;

    [Test]
    procedure Planning_Does_Not_Change_The_Graphs;

    [Test]
    procedure Companion_Needing_Minor_Bump_Lists_Versions_Once;
  end;

implementation

uses
  System.SysUtils,
  VSoft.CancellationToken.Impl,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Reference,
  DPM.Core.Configuration.Classes,
  DPM.Core.Package.InstallPlan,
  TestLogger;

const
  cApp = 'C:\test\app.dproj';
  cTests = 'C:\test\tests.dproj';

{ TInstallPlannerTests }

procedure TInstallPlannerTests.Setup;
begin
  FLogger := TTestLogger.Create;
  FRepo := TFakeRepositoryManager.Create;
  FCache := TFakePackageCache.Create;
  FRepoIntf := FRepo;
  FCacheIntf := FCache;
  FConfig := TConfiguration.Create(FLogger);
  FCancellation := TCancellationTokenSourceFactory.Create.Token;
  FOptions := TSearchOptions.Create;
  FOptions.CompilerVersion := cTestCompiler;
  FOptions.Prerelease := false;
  FPlanner := TInstallPlanner.Create(FLogger, FRepoIntf, FCacheIntf);
  FProjects := TCollections.CreateList<TInstallPlanProject>;
end;

procedure TInstallPlannerTests.Teardown;
begin
  FOptions.Free;
  FOptions := nil;
  FPlanner := nil;
  FProjects := nil;
  FConfig := nil;
  FRepoIntf := nil;
  FCacheIntf := nil;
  FRepo := nil;
  FCache := nil;
  FLogger := nil;
  FCancellation := nil;
end;

function TInstallPlannerTests.AddProject(const projectFile : string) : IPackageReference;
var
  project : TInstallPlanProject;
begin
  result := TPackageReference.CreateRoot(cTestCompiler);
  project.ProjectFile := projectFile;
  project.Graph := result;
  FProjects.Add(project);
end;

//adds a package node with the dependency edges its manifest declares, each to a node at that exact version.
function AddPackage(const parent : IPackageReference; const id, version : string; const range : string; const deps : array of string) : IPackageReference;
var
  i : integer;
  depVersion : TPackageVersion;
begin
  if range = '' then
    result := parent.AddChild(id, MakeVersion(version), TVersionRange.Empty)
  else
    result := parent.AddChild(id, MakeVersion(version), MakeRange(range));
  //deps are 'id=version' pairs, the edge range is the exact version.
  i := 0;
  while i < Length(deps) do
  begin
    depVersion := MakeVersion(deps[i + 1]);
    result.AddChild(deps[i], depVersion, MakeRange(deps[i + 1]));
    Inc(i, 2);
  end;
end;

function TInstallPlannerTests.MakePlan(const requested : array of IPackageInfo; const upgrade : boolean) : IInstallPlan;
var
  list : IList<IPackageInfo>;
  i : integer;
begin
  list := TCollections.CreateList<IPackageInfo>;
  for i := 0 to High(requested) do
    list.Add(requested[i]);
  result := FPlanner.Plan(FCancellation, FConfig, FOptions, list, FProjects, upgrade);
end;

function TInstallPlannerTests.FindItem(const plan : IInstallPlan; const id : string) : IInstallPlanItem;
var
  item : IInstallPlanItem;
begin
  result := nil;
  for item in plan.Items do
    if SameText(item.PackageInfo.Id, id) then
      exit(item);
end;

procedure TInstallPlannerTests.SetupFamily(const versions : array of string);
var
  bases, cores, exts : IList<IPackageInfo>;
  i : integer;
begin
  bases := TCollections.CreateList<IPackageInfo>;
  cores := TCollections.CreateList<IPackageInfo>;
  exts := TCollections.CreateList<IPackageInfo>;
  for i := 0 to High(versions) do
  begin
    bases.Add(MakeInfo('Base', versions[i], []));
    cores.Add(MakeInfo('Core', versions[i], [MakeDep('Base', versions[i])]));
    exts.Add(MakeInfo('Ext', versions[i], [MakeDep('Base', versions[i])]));
  end;
  FRepo.SetVersions('Base', bases);
  FRepo.SetVersions('Core', cores);
  FRepo.SetVersions('Ext', exts);
end;

procedure TInstallPlannerTests.Package_Pinning_Shared_Dependency_Becomes_Companion;
var
  root : IPackageReference;
  plan : IInstallPlan;
  ext : IInstallPlanItem;
begin
  // Upgrading Core to 1.0.3 needs Base 1.0.3, but Ext 1.0.1 pins Base 1.0.1 in both projects - Ext must
  // move to 1.0.3 with it, and finding that should cost one request.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  root := AddProject(cTests);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded, 'the plan should succeed');
  Assert.IsTrue(plan.HasCompanions, 'Ext should be a companion');
  ext := FindItem(plan, 'Ext');
  Assert.IsNotNull(ext);
  Assert.IsTrue(ext.IsCompanion);
  Assert.AreEqual('1.0.3', ext.PackageInfo.Version.ToStringNoMeta);
  Assert.AreEqual('1.0.1', ext.CurrentVersion.ToStringNoMeta);
  Assert.IsTrue(Pos('Base', ext.Reason) > 0, 'the reason should name the shared dependency');
  Assert.AreEqual(2, ext.Projects.Count, 'Ext moves in both projects');
  Assert.AreEqual(2, plan.Projects.Count);
  Assert.AreEqual(1, FRepo.GetCallCount('Ext'), 'one request for the companion''s versions');
  Assert.AreEqual(0, FRepo.GetVersionsCallCount('Ext'), 'the installed line had a match - no need to list versions');
end;

procedure TInstallPlannerTests.Newest_Compatible_Companion_Version_Is_Chosen;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // Ext 1.0.4 exists but needs Base 1.0.4 - Core 1.0.3 needs Base 1.0.3, so Ext 1.0.3 is the one.
  SetupFamily(['1.0.1', '1.0.3', '1.0.4']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.0.3', FindItem(plan, 'Ext').PackageInfo.Version.ToStringNoMeta);
end;

procedure TInstallPlannerTests.No_Compatible_Companion_Fails_The_Plan;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // There is no Ext that works with Base 1.0.3.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.1', []), MakeInfo('Base', '1.0.2', []), MakeInfo('Base', '1.0.3', [])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]), MakeInfo('Ext', '1.0.2', [MakeDep('Base', '1.0.2')])]));
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsFalse(plan.Succeeded, 'no Ext works with Base 1.0.3');
  Assert.IsTrue(Pos('Ext', plan.Errors[0]) > 0, 'the error should name Ext');
  Assert.IsTrue(Pos('Base', plan.Errors[0]) > 0, 'the error should name Base');
end;

procedure TInstallPlannerTests.Floating_Range_Is_Not_A_Blocker;
var
  root : IPackageReference;
  foo : IPackageReference;
  plan : IInstallPlan;
begin
  // Foo accepts any Base from 1.0.1 to 1.0.9 - it doesn't need to move.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  foo := AddPackage(root, 'Foo', '1.0.0', '', []);
  foo.AddChild('Base', MakeVersion('1.0.1'), MakeRange('[1.0.1,1.0.9]'));

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.IsFalse(plan.HasCompanions, 'Foo''s range already allows Base 1.0.3');
end;

procedure TInstallPlannerTests.Pinned_Top_Level_Dependency_Becomes_Companion;
var
  root : IPackageReference;
  plan : IInstallPlan;
  base : IInstallPlanItem;
begin
  // The project references Base itself, at 1.0.1 - Core 1.0.3 needs it at 1.0.3.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Base', '1.0.1', '', []);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  base := FindItem(plan, 'Base');
  Assert.IsNotNull(base, 'Base should be a companion');
  Assert.IsTrue(base.IsCompanion);
  Assert.AreEqual('1.0.3', base.PackageInfo.Version.ToStringNoMeta);
end;

procedure TInstallPlannerTests.Upgrading_Dependency_Directly_Adds_Its_Dependents;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // Upgrading Base directly - both Core and Ext pin it, so both must move.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Base', '1.0.1', '', []);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Base', '1.0.3', [])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.0.3', FindItem(plan, 'Core').PackageInfo.Version.ToStringNoMeta);
  Assert.AreEqual('1.0.3', FindItem(plan, 'Ext').PackageInfo.Version.ToStringNoMeta);
end;

procedure TInstallPlannerTests.Companion_Of_Companion_Found_In_Next_Round;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // Ext 1.0.3 (the first companion) also needs Util 1.0.3, and Tool 1.0.1 pins Util 1.0.1 - so Tool
  // only turns out to be a blocker once Ext is in the plan.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.1', []), MakeInfo('Base', '1.0.3', [])]));
  FRepo.SetVersions('Util', ListOf([MakeInfo('Util', '1.0.1', []), MakeInfo('Util', '1.0.3', [])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]),
                                   MakeInfo('Ext', '1.0.3', [MakeDep('Base', '1.0.3'), MakeDep('Util', '1.0.3')])]));
  FRepo.SetVersions('Tool', ListOf([MakeInfo('Tool', '1.0.1', [MakeDep('Util', '1.0.1')]),
                                    MakeInfo('Tool', '1.0.3', [MakeDep('Util', '1.0.3')])]));
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Tool', '1.0.1', '', ['Util', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.0.3', FindItem(plan, 'Ext').PackageInfo.Version.ToStringNoMeta);
  Assert.IsNotNull(FindItem(plan, 'Tool'), 'Tool should be added in the second round');
  Assert.AreEqual('1.0.3', FindItem(plan, 'Tool').PackageInfo.Version.ToStringNoMeta);
end;

procedure TInstallPlannerTests.Downgrade_Moves_Companions_Down;
var
  root : IPackageReference;
  plan : IInstallPlan;
  ext : IInstallPlanItem;
  line : string;
  downgraded : boolean;
begin
  // Both are at 1.0.3 and Core goes back to 1.0.1 - Ext pins Base 1.0.3, so it has to go back with it.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.3', '', ['Base', '1.0.3']);
  AddPackage(root, 'Ext', '1.0.3', '', ['Base', '1.0.3']);

  plan := MakePlan([MakeInfo('Core', '1.0.1', [MakeDep('Base', '1.0.1')])]);

  Assert.IsTrue(plan.Succeeded, 'a downgrade should take its companions down with it');
  ext := FindItem(plan, 'Ext');
  Assert.IsNotNull(ext);
  Assert.AreEqual('1.0.1', ext.PackageInfo.Version.ToStringNoMeta);
  downgraded := false;
  for line in plan.SummaryLines do
    if Pos('Ext 1.0.3 -> 1.0.1 (also downgraded)', line) = 1 then
      downgraded := true;
  Assert.IsTrue(downgraded, 'the summary should say Ext is downgraded');
end;

procedure TInstallPlannerTests.Summary_Puts_Reason_And_Projects_On_Lines_Of_Their_Own;
var
  root : IPackageReference;
  plan : IInstallPlan;
  lines : IList<string>;
begin
  // The summary is shown in a message box - short lines, the reason split in two, a gap before the projects.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  lines := plan.SummaryLines;
  Assert.AreEqual(6, lines.Count);
  Assert.AreEqual('Core 1.0.1 -> 1.0.3', lines[0], false);
  Assert.AreEqual('Ext 1.0.1 -> 1.0.3 (also upgraded)', lines[1], false);
  Assert.AreEqual('    Ext 1.0.1 requires Base 1.0.1,', lines[2], false);
  Assert.AreEqual('    but Core 1.0.3 requires Base 1.0.3', lines[3], false);
  Assert.AreEqual('', lines[4]);
  Assert.AreEqual('Projects : app.dproj', lines[5], false);
end;

procedure TInstallPlannerTests.Upgrade_Never_Downgrades_Companions;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // Ext 1.0.5 is installed and pins Base 1.0.5. The only Ext that works with Core 1.0.3 is 1.0.3 - older
  // than what is installed, so the plan fails rather than downgrading it.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.3', []), MakeInfo('Base', '1.0.5', [])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.3', [MakeDep('Base', '1.0.3')]), MakeInfo('Ext', '1.0.5', [MakeDep('Base', '1.0.5')])]));
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.5', '', ['Base', '1.0.5']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsFalse(plan.Succeeded, 'Ext must not be downgraded to 1.0.3');
end;

procedure TInstallPlannerTests.Prerelease_Companion_Only_When_Allowed;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // The only Ext that works with Base 1.0.3 is a prerelease. Everything installed and requested is
  // stable, so it is only used when prereleases are allowed.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.1', []), MakeInfo('Base', '1.0.3', [])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]), MakeInfo('Ext', '1.0.3-beta.1', [MakeDep('Base', '1.0.3')])]));
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);
  Assert.IsFalse(plan.Succeeded, 'a prerelease companion needs prereleases to be allowed');

  FOptions.Prerelease := true;
  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);
  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.0.3-beta.1', FindItem(plan, 'Ext').PackageInfo.Version.ToStringNoMeta);
end;

procedure TInstallPlannerTests.Upgrade_Applies_Packages_Only_Where_Top_Level;
const
  cTool = 'C:\test\tool.dproj';
  cOther = 'C:\test\other.dproj';
var
  root : IPackageReference;
  plan : IInstallPlan;
  core, ext : IInstallPlanItem;
begin
  // app and tests have Core and Ext, tool has only Ext, other has neither. Core moves where it is
  // installed; Ext (the companion) moves everywhere it is installed, tool included, so the group stays
  // on one Base; other is left alone.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  root := AddProject(cTests);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  root := AddProject(cTool);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  root := AddProject(cOther);
  AddPackage(root, 'Foo', '1.0.0', '', []);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  core := FindItem(plan, 'Core');
  ext := FindItem(plan, 'Ext');
  Assert.AreEqual(2, core.Projects.Count, 'Core moves in app and tests');
  Assert.AreEqual(3, ext.Projects.Count, 'Ext moves in app, tests and tool');
  Assert.AreEqual(3, plan.Projects.Count, 'other has nothing to apply');
  Assert.AreEqual(1, plan.PackagesForProject(cTool).Count, 'tool only gets Ext');
  Assert.AreEqual(0, plan.PackagesForProject(cOther).Count, 'other gets nothing');
end;

procedure TInstallPlannerTests.Requested_Package_Not_Installed_Fails_Upgrade;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Foo', '1.0.0', '', []);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsFalse(plan.Succeeded, 'there is nothing to upgrade');
  Assert.IsTrue(Pos('not installed', plan.Errors[0]) > 0);
end;

procedure TInstallPlannerTests.Install_Gives_Every_Package_To_Every_Project;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // A plain install (not an upgrade) applies what was asked for to every project, and looks nothing up.
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);
  AddProject(cTests);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])], false);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual(1, plan.Items.Count, 'no companions for a plain install');
  Assert.AreEqual(2, plan.Projects.Count);
  Assert.AreEqual(0, FRepo.GetCallCount('Base'), 'a plain install plan makes no requests');
end;

procedure TInstallPlannerTests.Planning_Does_Not_Change_The_Graphs;
var
  root : IPackageReference;
  ext : IPackageReference;
  plan : IInstallPlan;
begin
  SetupFamily(['1.0.1', '1.0.3']);
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  ext := AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.0.1', root.FindTopLevelChild('Core').Version.ToStringNoMeta, 'the graph must not be changed');
  Assert.AreEqual('1.0.1', root.FindTopLevelChild('Ext').Version.ToStringNoMeta, 'the graph must not be changed');
  Assert.IsNotNull(ext.FindFirstChild('Base'), 'the graph must not be changed');
end;

procedure TInstallPlannerTests.Companion_Needing_Minor_Bump_Lists_Versions_Once;
var
  root : IPackageReference;
  plan : IInstallPlan;
begin
  // Nothing on Ext's installed 1.0 line works with Base 1.0.3 - Ext 1.1.0 does. Finding it should list
  // the versions once and query only the 1.1 line after the 1.0 line.
  FRepo.SetVersions('Base', ListOf([MakeInfo('Base', '1.0.1', []), MakeInfo('Base', '1.0.2', []), MakeInfo('Base', '1.0.3', [])]));
  FRepo.SetVersions('Ext', ListOf([MakeInfo('Ext', '1.0.1', [MakeDep('Base', '1.0.1')]),
                                   MakeInfo('Ext', '1.0.2', [MakeDep('Base', '1.0.2')]),
                                   MakeInfo('Ext', '1.1.0', [MakeDep('Base', '1.0.3')])]));
  root := AddProject(cApp);
  AddPackage(root, 'Core', '1.0.1', '', ['Base', '1.0.1']);
  AddPackage(root, 'Ext', '1.0.1', '', ['Base', '1.0.1']);

  plan := MakePlan([MakeInfo('Core', '1.0.3', [MakeDep('Base', '1.0.3')])]);

  Assert.IsTrue(plan.Succeeded);
  Assert.AreEqual('1.1.0', FindItem(plan, 'Ext').PackageInfo.Version.ToStringNoMeta);
  Assert.AreEqual(1, FRepo.GetVersionsCallCount('Ext'), 'versions listed once');
  Assert.AreEqual(2, FRepo.GetCallCount('Ext'), 'the installed 1.0 line, then the 1.1 line');
end;

initialization
  TDUnitX.RegisterTestFixture(TInstallPlannerTests);

end.
