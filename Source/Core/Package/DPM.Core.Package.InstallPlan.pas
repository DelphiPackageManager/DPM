unit DPM.Core.Package.InstallPlan;

interface

uses
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
  DPM.Core.Package.Installer.Interfaces;

type
  ///<summary> Builds install plans - see IInstallPlanner.
  ///  For an upgrade, packages that would block it are found from the projects' recorded graphs (each edge
  ///  carries the range its parent declared), so that needs no downloads. A blocker's version is then chosen
  ///  with trial resolves, which run against a context of their own - the group's recorded resolutions don't
  ///  influence them, and planning leaves nothing recorded. </summary>
  TInstallPlanner = class(TInterfacedObject, IInstallPlanner)
  private
    FLogger : ILogger;
    FRepositoryManager : IPackageRepositoryManager;
    FPackageCache : IPackageCache;
  protected
    function Plan(const cancellationToken : ICancellationToken; const config : IConfiguration; const options : TSearchOptions;
                  const requested : IList<IPackageInfo>; const projects : IList<TInstallPlanProject>; const upgrade : boolean) : IInstallPlan;
  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache);
  end;

///<summary> A plan for an install that couldn't be planned at all (options, projects, packages not found...). </summary>
function CreateFailedPlan(const error : string) : IInstallPlan;

implementation

uses
  System.SysUtils,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Package.InstallerContext;

const
  //trial resolves are recorded against this pseudo project - never a real one.
  cPlanPseudoProject = ':plan:';
  //a companion can need companions of its own - this bounds how many times that can happen.
  cMaxPlanRounds = 10;
  //every companion reason reads '<what blocks><separator><what it conflicts with>' - SummaryLines puts the
  //two halves on lines of their own.
  cReasonSeparator = ', but ';

type
  TInstallPlanItem = class(TInterfacedObject, IInstallPlanItem)
  private
    FPackageInfo : IPackageInfo;
    FIsCompanion : boolean;
    FCurrentVersion : TPackageVersion;
    //every version referenced at the top level of the plan's projects, lowest first, no duplicates.
    FCurrentVersions : IList<TPackageVersion>;
    FReason : string;
    FProjects : IList<string>;
  protected
    function GetPackageInfo : IPackageInfo;
    function GetIsCompanion : boolean;
    function GetCurrentVersion : TPackageVersion;
    function GetReason : string;
    function GetProjects : IList<string>;
    function VersionChangeText : string;
  public
    constructor Create(const packageInfo : IPackageInfo; const isCompanion : boolean; const reason : string);
    procedure AddCurrentVersion(const version : TPackageVersion);
  end;

  TInstallPlan = class(TInterfacedObject, IInstallPlan)
  private
    FItems : IList<IInstallPlanItem>;
    FErrors : IList<string>;
    FProjects : IList<string>;
  protected
    function GetSucceeded : boolean;
    function GetItems : IList<IInstallPlanItem>;
    function GetErrors : IList<string>;
    function GetProjects : IList<string>;
    function HasCompanions : boolean;
    function PackagesForProject(const projectFile : string) : IList<IPackageInfo>;
    function SummaryLines : IList<string>;
  public
    constructor Create;
    procedure AddItem(const item : TInstallPlanItem);
    procedure AddError(const value : string);
    //fills Projects from the items, in the order the projects were given.
    procedure CollectProjects(const projects : IList<TInstallPlanProject>);
  end;

  //a range one of the plan's packages (or their dependencies) requires of a package.
  TDemand = record
    Range : TVersionRange;
    RequiredBy : string;
  end;

  //an installed top level package that would block the upgrade.
  TBlocker = record
    Id : string;
    InstalledVersion : TPackageVersion;
    Reason : string;
  end;

  ///<summary> The state of one upgrade plan as it is built. </summary>
  TUpgradePlanBuilder = class
  private
    FCancellationToken : ICancellationToken;
    FOptions : TSearchOptions;
    FCompilerVersion : TCompilerVersion;
    FRepositoryManager : IPackageRepositoryManager;
    FResolver : IDependencyResolver;
    FProjects : IList<TInstallPlanProject>;
    FPlan : TInstallPlan;
    //requested packages first, then companions as they are found.
    FPlanPackages : IList<IPackageInfo>;
    //lowercase id -> version, for every package in FPlanPackages.
    FPlanVersions : IDictionary<string, TPackageVersion>;
    //lowercase id -> why the companion is included.
    FCompanionReasons : IDictionary<string, string>;
    //lowercase id -> the ranges the plan's packages and their dependencies require of it.
    FDemands : IDictionary<string, IList<TDemand>>;
    FPrereleaseRequested : boolean;
    //companions move the same way as the requested packages - up for an upgrade, down for a downgrade.
    FAllowUp : boolean;
    FAllowDown : boolean;

    procedure AddToPlan(const packageInfo : IPackageInfo; const reason : string);
    function IsCandidateVersion(const version, installedVersion : TPackageVersion) : boolean;
    function DirectionWord : string;
    function ResolveIsolated(const packages : IList<IPackageInfo>; const conflicts : IList<TUnresolvableConflict>) : IList<IPackageInfo>;
    function BuildDemands : boolean;
    function CheckAgainstDemands(const id : string; const version : TPackageVersion) : string;
    function CheckEdges(const node : IPackageReference) : string;
    procedure DetectBlockers(const project : TInstallPlanProject; const blockers : IDictionary<string, TBlocker>);
    function IsStaticallyCompatible(const candidate : IPackageInfo) : boolean;
    function TrialResolve(const candidate : IPackageInfo) : boolean;
    function PickCompanion(const blocker : TBlocker; const candidates : IList<IPackageInfo>) : IPackageInfo;
    function FindCompanion(const blocker : TBlocker) : IPackageInfo;
    procedure PopulatePlan;
  public
    constructor Create(const cancellationToken : ICancellationToken; const logger : ILogger; const config : IConfiguration; const options : TSearchOptions;
                       const compilerVersion : TCompilerVersion; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
                       const projects : IList<TInstallPlanProject>; const plan : TInstallPlan);
    procedure Build(const requested : IList<IPackageInfo>);
  end;

function ConflictText(const conflict : TUnresolvableConflict) : string;
begin
  result := conflict.ParentId + ' -> ' + conflict.PackageId + ' ' + conflict.RequestedRange.ToString;
  if conflict.Reason <> '' then
    result := result + ' : ' + conflict.Reason;
end;

//the top of the major.minor line a version is on - DPM ranges only let the patch float.
function LineMax(const version : TPackageVersion) : TPackageVersion;
begin
  result := TPackageVersion.Create(version.Major, version.Minor, High(Word));
end;

function SameLine(const a, b : TPackageVersion) : boolean;
begin
  result := (a.Major = b.Major) and (a.Minor = b.Minor);
end;

{ TInstallPlanItem }

constructor TInstallPlanItem.Create(const packageInfo : IPackageInfo; const isCompanion : boolean; const reason : string);
begin
  inherited Create;
  FPackageInfo := packageInfo;
  FIsCompanion := isCompanion;
  FReason := reason;
  FCurrentVersion := TPackageVersion.Empty;
  FCurrentVersions := TCollections.CreateList<TPackageVersion>;
  FProjects := TCollections.CreateList<string>;
end;

procedure TInstallPlanItem.AddCurrentVersion(const version : TPackageVersion);
var
  i : integer;
begin
  if version > FCurrentVersion then
    FCurrentVersion := version;
  //kept in order by hand - the list's default comparer compares the records' memory, not the versions.
  for i := 0 to FCurrentVersions.Count - 1 do
  begin
    if FCurrentVersions[i] = version then
      exit;
    if FCurrentVersions[i] > version then
    begin
      FCurrentVersions.Insert(i, version);
      exit;
    end;
  end;
  FCurrentVersions.Add(version);
end;

function TInstallPlanItem.VersionChangeText : string;
var
  version : TPackageVersion;
  fromText : string;
begin
  //projects already on the new version don't change, so only the others are listed.
  fromText := '';
  for version in FCurrentVersions do
  begin
    if version = FPackageInfo.Version then
      continue;
    if fromText <> '' then
      fromText := fromText + ', ';
    fromText := fromText + version.ToStringNoMeta;
  end;
  result := FPackageInfo.Version.ToStringNoMeta;
  if fromText <> '' then
    result := fromText + ' -> ' + result;
end;

function TInstallPlanItem.GetCurrentVersion : TPackageVersion;
begin
  result := FCurrentVersion;
end;

function TInstallPlanItem.GetIsCompanion : boolean;
begin
  result := FIsCompanion;
end;

function TInstallPlanItem.GetPackageInfo : IPackageInfo;
begin
  result := FPackageInfo;
end;

function TInstallPlanItem.GetProjects : IList<string>;
begin
  result := FProjects;
end;

function TInstallPlanItem.GetReason : string;
begin
  result := FReason;
end;

{ TInstallPlan }

constructor TInstallPlan.Create;
begin
  inherited Create;
  FItems := TCollections.CreateList<IInstallPlanItem>;
  FErrors := TCollections.CreateList<string>;
  FProjects := TCollections.CreateList<string>;
end;

procedure TInstallPlan.AddError(const value : string);
begin
  FErrors.Add(value);
end;

procedure TInstallPlan.AddItem(const item : TInstallPlanItem);
begin
  FItems.Add(item);
end;

procedure TInstallPlan.CollectProjects(const projects : IList<TInstallPlanProject>);
var
  project : TInstallPlanProject;
  item : IInstallPlanItem;
  projectFile : string;
begin
  FProjects.Clear;
  for project in projects do
  begin
    for item in FItems do
    begin
      for projectFile in item.Projects do
      begin
        if SameText(projectFile, project.ProjectFile) and (not FProjects.Contains(project.ProjectFile)) then
          FProjects.Add(project.ProjectFile);
      end;
    end;
  end;
end;

function TInstallPlan.GetErrors : IList<string>;
begin
  result := FErrors;
end;

function TInstallPlan.GetItems : IList<IInstallPlanItem>;
begin
  result := FItems;
end;

function TInstallPlan.GetProjects : IList<string>;
begin
  result := FProjects;
end;

function TInstallPlan.GetSucceeded : boolean;
begin
  result := not FErrors.Any;
end;

function TInstallPlan.HasCompanions : boolean;
var
  item : IInstallPlanItem;
begin
  result := false;
  for item in FItems do
  begin
    if item.IsCompanion then
      exit(true);
  end;
end;

function TInstallPlan.PackagesForProject(const projectFile : string) : IList<IPackageInfo>;
var
  item : IInstallPlanItem;
  itemProject : string;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for item in FItems do
  begin
    for itemProject in item.Projects do
    begin
      if SameText(itemProject, projectFile) then
      begin
        result.Add(item.PackageInfo);
        break;
      end;
    end;
  end;
end;

//Short lines - this is shown in the IDE's confirmation message box as well as the log.
function TInstallPlan.SummaryLines : IList<string>;
var
  item : IInstallPlanItem;
  line : string;
  projectFile : string;
  separatorPos : integer;
begin
  result := TCollections.CreateList<string>;
  for item in FItems do
  begin
    line := item.PackageInfo.Id + ' ' + item.VersionChangeText;
    if item.IsCompanion then
    begin
      if item.PackageInfo.Version < item.CurrentVersion then
        line := line + ' (also downgraded)'
      else
        line := line + ' (also upgraded)';
    end;
    result.Add(line);
    if item.IsCompanion and (item.Reason <> '') then
    begin
      //why, indented, with each half of the reason on a line of its own.
      separatorPos := Pos(cReasonSeparator, item.Reason);
      if separatorPos > 0 then
      begin
        result.Add('    ' + Copy(item.Reason, 1, separatorPos));
        result.Add('    ' + Copy(item.Reason, separatorPos + 2, Length(item.Reason)));
      end
      else
        result.Add('    ' + item.Reason);
    end;
  end;
  if FProjects.Any then
  begin
    line := '';
    for projectFile in FProjects do
    begin
      if line <> '' then
        line := line + ', ';
      line := line + ExtractFileName(projectFile);
    end;
    result.Add('');
    result.Add('Projects : ' + line);
  end;
end;

{ TUpgradePlanBuilder }

constructor TUpgradePlanBuilder.Create(const cancellationToken : ICancellationToken; const logger : ILogger; const config : IConfiguration; const options : TSearchOptions;
                                       const compilerVersion : TCompilerVersion; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
                                       const projects : IList<TInstallPlanProject>; const plan : TInstallPlan);
var
  quietLogger : ILogger;
  isolatedContext : IPackageInstallerContext;
begin
  inherited Create;
  FCancellationToken := cancellationToken;
  FOptions := options;
  FCompilerVersion := compilerVersion;
  FRepositoryManager := repositoryManager;
  FProjects := projects;
  FPlan := plan;
  FPlanPackages := TCollections.CreateList<IPackageInfo>;
  FPlanVersions := TCollections.CreateDictionary<string, TPackageVersion>;
  FCompanionReasons := TCollections.CreateDictionary<string, string>;
  FDemands := TCollections.CreateDictionary<string, IList<TDemand>>;
  //Trial resolves are expected to fail - that's how a companion's version is chosen - so their output
  //goes to Debug rather than alarming the user.
  //a context of its own : trial resolves must not be influenced by (or recorded in) the group's resolutions.
  quietLogger := TDemotingLogger.Create(logger);
  isolatedContext := TCorePackageInstallerContext.Create(quietLogger);
  FResolver := TDependencyResolver.Create(quietLogger, repositoryManager, packageCache, isolatedContext);
  FResolver.Initialize(config);
end;

procedure TUpgradePlanBuilder.AddToPlan(const packageInfo : IPackageInfo; const reason : string);
begin
  FPlanPackages.Add(packageInfo);
  FPlanVersions[LowerCase(packageInfo.Id)] := packageInfo.Version;
  if reason <> '' then
    FCompanionReasons[LowerCase(packageInfo.Id)] := reason;
  if not packageInfo.Version.IsStable then
    FPrereleaseRequested := true;
end;

function TUpgradePlanBuilder.ResolveIsolated(const packages : IList<IPackageInfo>; const conflicts : IList<TUnresolvableConflict>) : IList<IPackageInfo>;
var
  projectReferences : IList<IPackageReference>;
  graph : IPackageReference;
  resolved : IList<IPackageInfo>;
begin
  projectReferences := TCollections.CreateList<IPackageReference>;
  if FResolver.ResolveForInstall(FCancellationToken, FCompilerVersion, cPlanPseudoProject, FOptions, packages, projectReferences, graph, resolved, nil, nil, conflicts) then
    result := resolved
  else
    result := nil;
end;

//The ranges the plan needs : what its packages, and everything they pull in, require of each dependency.
//Resolved on their own - if the requested packages can't even resolve together, no companion will help.
function TUpgradePlanBuilder.BuildDemands : boolean;
var
  conflicts : IList<TUnresolvableConflict>;
  conflict : TUnresolvableConflict;
  closure : IList<IPackageInfo>;
  info : IPackageInfo;
  dependency : IPackageDependency;
  demand : TDemand;
  demands : IList<TDemand>;
  message : string;
begin
  FDemands.Clear;
  conflicts := TCollections.CreateList<TUnresolvableConflict>;
  closure := ResolveIsolated(FPlanPackages, conflicts);
  result := closure <> nil;
  if not result then
  begin
    message := 'The packages to install cannot be resolved together';
    for conflict in conflicts do
      message := message + sLineBreak + '  ' + ConflictText(conflict);
    FPlan.AddError(message);
    exit;
  end;

  for info in closure do
  begin
    if info.Dependencies = nil then
      continue;
    for dependency in info.Dependencies do
    begin
      if dependency.VersionRange.IsBundledSentinel then
        continue;
      if not FDemands.TryGetValue(LowerCase(dependency.Id), demands) then
      begin
        demands := TCollections.CreateList<TDemand>;
        FDemands.Add(LowerCase(dependency.Id), demands);
      end;
      demand.Range := dependency.VersionRange;
      demand.RequiredBy := info.Id + ' ' + info.Version.ToStringNoMeta;
      demands.Add(demand);
    end;
  end;
end;

//'' when the version satisfies everything the plan requires of the package, otherwise why not.
function TUpgradePlanBuilder.CheckAgainstDemands(const id : string; const version : TPackageVersion) : string;
var
  demands : IList<TDemand>;
  demand : TDemand;
begin
  result := '';
  if not FDemands.TryGetValue(LowerCase(id), demands) then
    exit;
  for demand in demands do
  begin
    if not demand.Range.IsSatisfiedBy(version) then
      exit(id + ' ' + version.ToStringNoMeta + ' is installed' + cReasonSeparator + demand.RequiredBy + ' requires ' + id + ' ' + demand.Range.ToString);
  end;
end;

//Walks a top level package's recorded graph. Each child edge carries the range its parent declared, so
//an edge whose range can't meet what the plan requires means the top level package blocks the upgrade.
//'' when nothing blocks.
function TUpgradePlanBuilder.CheckEdges(const node : IPackageReference) : string;
var
  child : IPackageReference;
  childKey : string;
  planVersion : TPackageVersion;
  demands : IList<TDemand>;
  demand : TDemand;
  intersection : TVersionRange;
  checkRange : boolean;
begin
  result := '';
  for child in node.Children do
  begin
    childKey := LowerCase(child.Id);
    checkRange := (not child.VersionRange.IsEmpty) and (not child.VersionRange.IsBundledSentinel);
    if FPlanVersions.TryGetValue(childKey, planVersion) then
    begin
      //the child is being replaced - only the range this node requires of it matters, not its old dependencies.
      if checkRange and (not child.VersionRange.IsSatisfiedBy(planVersion)) then
        exit(node.Id + ' ' + node.Version.ToStringNoMeta + ' requires ' + child.Id + ' ' + child.VersionRange.ToString + cReasonSeparator + child.Id + ' ' + planVersion.ToStringNoMeta + ' is being installed');
      continue;
    end;
    if checkRange and FDemands.TryGetValue(childKey, demands) then
    begin
      for demand in demands do
      begin
        if not child.VersionRange.TryGetIntersectingRange(demand.Range, intersection) then
          exit(node.Id + ' ' + node.Version.ToStringNoMeta + ' requires ' + child.Id + ' ' + child.VersionRange.ToString + cReasonSeparator + demand.RequiredBy + ' requires ' + child.Id + ' ' + demand.Range.ToString);
      end;
    end;
    result := CheckEdges(child);
    if result <> '' then
      exit;
  end;
end;

procedure TUpgradePlanBuilder.DetectBlockers(const project : TInstallPlanProject; const blockers : IDictionary<string, TBlocker>);
var
  applied : boolean;
  topLevel : IPackageReference;
  reason : string;
  blocker : TBlocker;
begin
  if project.Graph = nil then
    exit;
  //a project none of the plan's packages are installed in has nothing applied, so it can't block.
  applied := false;
  for topLevel in project.Graph.Children do
  begin
    if FPlanVersions.ContainsKey(LowerCase(topLevel.Id)) then
    begin
      applied := true;
      break;
    end;
  end;
  if not applied then
    exit;

  for topLevel in project.Graph.Children do
  begin
    if FPlanVersions.ContainsKey(LowerCase(topLevel.Id)) then
      continue;
    reason := CheckAgainstDemands(topLevel.Id, topLevel.Version);
    if reason = '' then
      reason := CheckEdges(topLevel);
    if reason = '' then
      continue;
    if blockers.TryGetValue(LowerCase(topLevel.Id), blocker) then
    begin
      //never downgrade - start from the highest version installed in any of the projects.
      if topLevel.Version > blocker.InstalledVersion then
        blocker.InstalledVersion := topLevel.Version;
    end
    else
    begin
      blocker.Id := topLevel.Id;
      blocker.InstalledVersion := topLevel.Version;
      blocker.Reason := reason;
    end;
    blockers[LowerCase(topLevel.Id)] := blocker;
  end;
end;

//A cheap check before a trial resolve : the candidate's own dependencies must be able to meet what the
//plan requires.
function TUpgradePlanBuilder.IsStaticallyCompatible(const candidate : IPackageInfo) : boolean;
var
  dependency : IPackageDependency;
  dependencyKey : string;
  planVersion : TPackageVersion;
  demands : IList<TDemand>;
  demand : TDemand;
  intersection : TVersionRange;
begin
  result := false;
  if CheckAgainstDemands(candidate.Id, candidate.Version) <> '' then
    exit;
  if candidate.Dependencies <> nil then
  begin
    for dependency in candidate.Dependencies do
    begin
      if dependency.VersionRange.IsBundledSentinel then
        continue;
      dependencyKey := LowerCase(dependency.Id);
      if FPlanVersions.TryGetValue(dependencyKey, planVersion) then
      begin
        if not dependency.VersionRange.IsSatisfiedBy(planVersion) then
          exit;
        continue;
      end;
      if FDemands.TryGetValue(dependencyKey, demands) then
      begin
        for demand in demands do
        begin
          if not dependency.VersionRange.TryGetIntersectingRange(demand.Range, intersection) then
            exit;
        end;
      end;
    end;
  end;
  result := true;
end;

function TUpgradePlanBuilder.TrialResolve(const candidate : IPackageInfo) : boolean;
var
  packages : IList<IPackageInfo>;
begin
  packages := TCollections.CreateList<IPackageInfo>;
  packages.AddRange(FPlanPackages);
  packages.Add(candidate);
  result := ResolveIsolated(packages, nil) <> nil;
end;

function TUpgradePlanBuilder.PickCompanion(const blocker : TBlocker; const candidates : IList<IPackageInfo>) : IPackageInfo;
var
  sorted : IList<IPackageInfo>;
  candidate : IPackageInfo;
begin
  result := nil;
  sorted := TCollections.CreateList<IPackageInfo>;
  sorted.AddRange(candidates);
  sorted.Sort(
    function(const left, right : IPackageInfo) : integer
    begin
      result := right.Version.CompareTo(left.Version);
    end);
  for candidate in sorted do
  begin
    if FCancellationToken.IsCancelled then
      exit;
    if not IsCandidateVersion(candidate.Version, blocker.InstalledVersion) then
      continue;
    if not IsStaticallyCompatible(candidate) then
      continue;
    if TrialResolve(candidate) then
      exit(candidate);
  end;
end;

//true for a version a companion may move to - the other side of the installed version, in a direction the
//requested packages are moving.
function TUpgradePlanBuilder.IsCandidateVersion(const version, installedVersion : TPackageVersion) : boolean;
begin
  result := ((version > installedVersion) and FAllowUp) or ((version < installedVersion) and FAllowDown);
end;

function TUpgradePlanBuilder.DirectionWord : string;
begin
  if FAllowUp and FAllowDown then
    result := 'other'
  else if FAllowDown then
    result := 'older'
  else
    result := 'newer';
end;

//The newest version of the blocker that works with the plan, moving the way the requested packages move.
//DPM ranges only let the patch float, so one query covers one major.minor line. For an upgrade the installed
//line is tried first - that's usually where the matching release is, and it's one request. Otherwise (or if
//it has nothing) we list the versions to find the lines worth trying.
function TUpgradePlanBuilder.FindCompanion(const blocker : TBlocker) : IPackageInfo;
var
  prerelease : boolean;
  range : TVersionRange;
  candidates : IList<IPackageInfo>;
  versions : IList<TPackageVersion>;
  version : TPackageVersion;
  lines : IList<TPackageVersion>;
  i : integer;
  found : boolean;
begin
  result := nil;
  prerelease := FOptions.Prerelease or FPrereleaseRequested or (not blocker.InstalledVersion.IsStable);

  if not FAllowDown then
  begin
    range := TVersionRange.Create('', blocker.InstalledVersion, true, LineMax(blocker.InstalledVersion), true);
    candidates := FRepositoryManager.GetPackageVersionsWithDependencies(FCancellationToken, FCompilerVersion, blocker.Id, range, prerelease);
    result := PickCompanion(blocker, candidates);
    if (result <> nil) or FCancellationToken.IsCancelled then
      exit;
  end;

  //the lowest candidate version of each line - the bottom of that line's range.
  versions := FRepositoryManager.GetPackageVersions(FCancellationToken, FCompilerVersion, blocker.Id, prerelease);
  lines := TCollections.CreateList<TPackageVersion>;
  for version in versions do
  begin
    if not IsCandidateVersion(version, blocker.InstalledVersion) then
      continue;
    //an upgrade has already tried the installed line.
    if (not FAllowDown) and SameLine(version, blocker.InstalledVersion) then
      continue;
    found := false;
    for i := 0 to lines.Count - 1 do
    begin
      if SameLine(lines[i], version) then
      begin
        found := true;
        if version < lines[i] then
          lines[i] := version;
        break;
      end;
    end;
    if not found then
      lines.Add(version);
  end;
  lines.Sort(
    function(const left, right : TPackageVersion) : integer
    begin
      result := right.CompareTo(left);
    end);

  for version in lines do
  begin
    range := TVersionRange.Create('', version, true, LineMax(version), true);
    candidates := FRepositoryManager.GetPackageVersionsWithDependencies(FCancellationToken, FCompilerVersion, blocker.Id, range, prerelease);
    result := PickCompanion(blocker, candidates);
    if (result <> nil) or FCancellationToken.IsCancelled then
      exit;
  end;
end;

procedure TUpgradePlanBuilder.PopulatePlan;
var
  info : IPackageInfo;
  reason : string;
  item : TInstallPlanItem;
  project : TInstallPlanProject;
  node : IPackageReference;
begin
  for info in FPlanPackages do
  begin
    if not FCompanionReasons.TryGetValue(LowerCase(info.Id), reason) then
      reason := '';
    item := TInstallPlanItem.Create(info, reason <> '', reason);
    FPlan.AddItem(item);
    //an upgrade only touches projects where the package is a top level reference.
    for project in FProjects do
    begin
      if project.Graph = nil then
        continue;
      node := project.Graph.FindTopLevelChild(info.Id);
      if node = nil then
        continue;
      item.FProjects.Add(project.ProjectFile);
      item.AddCurrentVersion(node.Version);
    end;
    if not item.FProjects.Any then
      FPlan.AddError(info.Id + ' is not installed in any of the projects - there is nothing to change.');
  end;
  FPlan.CollectProjects(FProjects);
end;

procedure TUpgradePlanBuilder.Build(const requested : IList<IPackageInfo>);
var
  info : IPackageInfo;
  round : integer;
  blockers : IDictionary<string, TBlocker>;
  blockerIds : IList<string>;
  blockerId : string;
  blocker : TBlocker;
  project : TInstallPlanProject;
  companion : IPackageInfo;
  current : TPackageVersion;
  node : IPackageReference;
begin
  for info in requested do
  begin
    AddToPlan(info, '');
    //which way the requested package moves, from the highest version installed in the projects.
    current := TPackageVersion.Empty;
    for project in FProjects do
    begin
      if project.Graph = nil then
        continue;
      node := project.Graph.FindTopLevelChild(info.Id);
      if (node <> nil) and (node.Version > current) then
        current := node.Version;
    end;
    if current.IsEmpty or (info.Version > current) then
      FAllowUp := true
    else if info.Version < current then
      FAllowDown := true;
  end;
  //reinstalling the installed version moves nothing - should anything block it, treat it like an upgrade.
  if not (FAllowUp or FAllowDown) then
    FAllowUp := true;

  for round := 1 to cMaxPlanRounds do
  begin
    if FCancellationToken.IsCancelled then
    begin
      FPlan.AddError('Cancelled.');
      exit;
    end;
    if not BuildDemands then
      exit;

    blockers := TCollections.CreateDictionary<string, TBlocker>;
    for project in FProjects do
      DetectBlockers(project, blockers);
    if not blockers.Any then
    begin
      PopulatePlan;
      exit;
    end;

    //a deterministic order, so the same projects always give the same plan.
    blockerIds := TCollections.CreateList<string>;
    blockerIds.AddRange(blockers.Keys);
    blockerIds.Sort;
    for blockerId in blockerIds do
    begin
      blocker := blockers[blockerId];
      companion := FindCompanion(blocker);
      if FCancellationToken.IsCancelled then
      begin
        FPlan.AddError('Cancelled.');
        exit;
      end;
      if companion = nil then
        FPlan.AddError(blocker.Reason + ' - and no ' + DirectionWord + ' version of ' + blocker.Id + ' is compatible.')
      else
        AddToPlan(companion, blocker.Reason);
    end;
    if FPlan.FErrors.Any then
      exit;
  end;
  FPlan.AddError('Companion packages were still being added after ' + IntToStr(cMaxPlanRounds) + ' rounds - giving up.');
end;

function CreateFailedPlan(const error : string) : IInstallPlan;
var
  plan : TInstallPlan;
begin
  plan := TInstallPlan.Create;
  plan.AddError(error);
  result := plan;
end;

{ TInstallPlanner }

constructor TInstallPlanner.Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache);
begin
  inherited Create;
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
end;

function TInstallPlanner.Plan(const cancellationToken : ICancellationToken; const config : IConfiguration; const options : TSearchOptions;
                              const requested : IList<IPackageInfo>; const projects : IList<TInstallPlanProject>; const upgrade : boolean) : IInstallPlan;
var
  plan : TInstallPlan;
  builder : TUpgradePlanBuilder;
  compilerVersion : TCompilerVersion;
  info : IPackageInfo;
  item : TInstallPlanItem;
  project : TInstallPlanProject;
  node : IPackageReference;
begin
  plan := TInstallPlan.Create;
  result := plan;
  if not requested.Any then
  begin
    plan.AddError('No packages to install.');
    exit;
  end;

  if not upgrade then
  begin
    //an install applies every requested package to every project.
    for info in requested do
    begin
      item := TInstallPlanItem.Create(info, false, '');
      plan.AddItem(item);
      for project in projects do
      begin
        item.FProjects.Add(project.ProjectFile);
        if project.Graph = nil then
          continue;
        node := project.Graph.FindTopLevelChild(info.Id);
        if node <> nil then
          item.AddCurrentVersion(node.Version);
      end;
    end;
    plan.CollectProjects(projects);
    exit;
  end;

  compilerVersion := options.CompilerVersion;
  if compilerVersion = TCompilerVersion.UnknownVersion then
    compilerVersion := requested[0].CompilerVersion;

  builder := TUpgradePlanBuilder.Create(cancellationToken, FLogger, config, options, compilerVersion, FRepositoryManager, FPackageCache, projects, plan);
  try
    builder.Build(requested);
  finally
    builder.Free;
  end;
end;

end.
