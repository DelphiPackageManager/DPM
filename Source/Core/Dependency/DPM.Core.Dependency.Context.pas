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
  System.SysUtils,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces;

//The state of one resolve - only intended for use by the resolver (DPM.Core.Dependency.Resolver), which does
//the searching. Everything the search changes goes through Assign and the agenda, and can be undone back to a
//Mark - so a choice that is given up leaves nothing behind.
type
  TAssignmentKind = (
    //a package being installed, or the top level restore is re-resolving - its version is not negotiable.
    akTopLevel,
    //what the project already has. Kept as it is, and its own dependencies are not checked again, unless a
    //requirement needs it to change - which a top level reference never does.
    akProjectReference,
    //adopted from another project in the group - the group shares one version of each package.
    akImported,
    //a version the search chose.
    akChosen,
    //the no-op for a dependency the IDE ships (e.g. Indy), when no real package provides it.
    akBundled
  );

  //The decisions (by level) something depends on - sorted, without duplicates. Empty for a fact.
  TLevels = record
  private
    FItems : TArray<integer>;
  public
    class function Empty : TLevels; static;
    class function Single(const level : integer) : TLevels; static;
    procedure Add(const level : integer);
    procedure AddAll(const other : TLevels);
    function Without(const level : integer) : TLevels;
    function IsEmpty : boolean;
    //the latest decision, 0 when there is none.
    function Highest : integer;
  end;

  //A package in the solution, at its version, and why it is there. Replaced whole, never changed in place, so
  //the trail can put back the one it replaced.
  TAssignment = record
    PackageInfo : IPackageInfo;
    Kind : TAssignmentKind;
    //the decisions it depends on - undoing any of them takes it out again.
    Responsible : TLevels;
    ParentId : string;
    VersionRange : TVersionRange;
    //akImported : the other project's resolution.
    Imported : IResolvedPackage;
    //akProjectReference : one the project references directly.
    TopLevelReference : boolean;
    function IsTopLevel : boolean;
  end;

  //a package's requirement of one of its dependencies, waiting to be checked.
  TRequirement = record
    Owner : IPackageInfo;
    Dependency : IPackageDependency;
  end;

  //the range a package in the solution declares for one of its dependencies.
  TConstraint = record
    Owner : IPackageInfo;
    Responsible : TLevels;
    Range : TVersionRange;
  end;

  //what UndoTo goes back to.
  TResolverMark = record
    Trail : integer;
    AgendaHead : integer;
    AgendaCount : integer;
    DeferredHead : integer;
    DeferredCount : integer;
  end;

  TResolverContext = class
  private
    FLogger : ILogger;
    FPackageInstallerContext : IPackageInstallerContext;
    FProjectFile : string;
    FCompilerVersion : TCompilerVersion;
    FAssignments : IDictionary<string, TAssignment>;
    //lowercase id -> the ranges the packages in the solution declare for it.
    FConstraints : IDictionary<string, IList<TConstraint>>;
    //requirements still to check, oldest first. Only ever appended to, so going back is just restoring
    //the head and the count.
    FAgenda : IList<TRequirement>;
    FAgendaHead : integer;
    //bundled requirements (e.g. Indy) - checked once everything else is, so a real package always wins.
    FDeferred : IList<TRequirement>;
    FDeferredHead : integer;
    FTrail : IList<TProc>;
    //'id@version' -> why it can't be used, whatever else is chosen.
    FNoGoods : IDictionary<string, string>;
    FVersionLists : IDictionary<string, IList<IPackageInfo>>;
    FPreferredVersions : IDictionary<string, TPackageVersion>;
    FProjectReferenceVersions : IDictionary<string, TPackageVersion>;
    //the top levels, in the order they go in the graph : the project's, then the new ones.
    FTopLevels : IList<string>;
    FSortedDependencies : IDictionary<IPackageInfo, IList<IPackageDependency>>;
    procedure SetAssignment(const packageId : string; const assignment : TAssignment);
    procedure AddConstraints(const assignment : TAssignment);
    procedure RemoveConstraints(const assignment : TAssignment);
  public
    /// newPackages : the packages being installed (an upgrade and its companions, or the top level restore is
    /// re-resolving) - each is a top level at its exact version, and replaces any project reference with its id.
    /// sharedVersionCache : version lookups shared by several resolves (restore resolves one top level at a
    /// time) - nil for a cache of its own.
    /// preferredVersions : lock-file style hints (id -> version) tried before anything else - restore passes
    /// the project's graph, install nil.
    constructor Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string;
                       const compilerVersion : TCompilerVersion; const newPackages : IList<IPackageInfo>;
                       const projectReferences : IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>;
                       const preferredVersions : IDictionary<string, TPackageVersion>);

    function TryGetAssignment(const packageId : string; out assignment : TAssignment) : boolean;
    //puts the assignment in the solution, replacing any with its id - undone by UndoTo.
    procedure Assign(const assignment : TAssignment);
    //the ranges the packages in the solution declare for packageId.
    function ConstraintsOn(const packageId : string) : IList<TConstraint>;

    //queues the owner's dependencies to be checked - narrowest range first.
    procedure Require(const owner : IPackageInfo);
    function NextRequirement(out requirement : TRequirement; out deferred : boolean) : boolean;

    function Mark : TResolverMark;
    procedure UndoTo(const mark : TResolverMark);

    function IsNoGood(const package : IPackageInfo; out reason : string) : boolean;
    procedure RecordNoGood(const package : IPackageInfo; const reason : string);
    function TryGetVersionList(const key : string; out versions : IList<IPackageInfo>) : boolean;
    procedure CacheVersionList(const key : string; const versions : IList<IPackageInfo>);
    //the dependencies sorted narrowest range first (fail early), equal ones in the manifest's order - the
    //manifest itself is never reordered.
    function SortedDependencies(const package : IPackageInfo) : IList<IPackageDependency>;

    function TryGetPreferredVersion(const packageId : string; out version : TPackageVersion) : boolean;
    //another project in the group's resolution of packageId, nil when none has one.
    function FindSiblingResolution(const packageId : string) : IResolvedPackage;
    ///  True when the package is at the version the project already had : a project reference kept as it was,
    ///  another project's version, or (restore) the version the project's graph records - restore re-resolves
    ///  each top level as a 'new' package, but that doesn't make it a change. Used to tell an edge this resolve
    ///  made inconsistent from one that already was.
    function IsUnchanged(const assignment : TAssignment) : boolean;
    function AnyTopLevelIsPrerelease : boolean;

    //What the solution holds, reachable from the top levels, dependencies before the packages that need them.
    //(A choice that was given up is not in it, nor anything only it needed.)
    function ResolvedOrder : IList<TAssignment>;
    function GetResolvedPackageInfos : IList<IPackageInfo>;
    function GetResolvedPackages : TArray<IResolvedPackage>;
    function BuildDependencyGraph : IPackageReference;

    property ProjectFile : string read FProjectFile;
    property CompilerVersion : TCompilerVersion read FCompilerVersion;
    property TopLevels : IList<string> read FTopLevels;
  end;

//an assignment with every field set (a record local's unmanaged fields start as garbage).
function MakeAssignment(const packageInfo : IPackageInfo; const kind : TAssignmentKind; const responsible : TLevels; const parentId : string;
                        const versionRange : TVersionRange) : TAssignment;

implementation

uses
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Dependency.Reference,
  DPM.Core.Dependency.Resolution;

function MakeAssignment(const packageInfo : IPackageInfo; const kind : TAssignmentKind; const responsible : TLevels; const parentId : string;
                        const versionRange : TVersionRange) : TAssignment;
begin
  result.PackageInfo := packageInfo;
  result.Kind := kind;
  result.Responsible := responsible;
  result.ParentId := parentId;
  result.VersionRange := versionRange;
  result.Imported := nil;
  result.TopLevelReference := false;
end;

function NoGoodKey(const package : IPackageInfo) : string;
begin
  result := LowerCase(package.Id) + '@' + package.Version.ToStringNoMeta;
end;

{ TLevels }

class function TLevels.Empty : TLevels;
begin
  result.FItems := nil;
end;

class function TLevels.Single(const level : integer) : TLevels;
begin
  SetLength(result.FItems, 1);
  result.FItems[0] := level;
end;

procedure TLevels.Add(const level : integer);
var
  items : TArray<integer>;
  i : integer;
  j : integer;
begin
  i := 0;
  while (i < Length(FItems)) and (FItems[i] < level) do
    Inc(i);
  if (i < Length(FItems)) and (FItems[i] = level) then
    exit;
  //a new array - a TLevels copied from this one shares FItems.
  SetLength(items, Length(FItems) + 1);
  for j := 0 to i - 1 do
    items[j] := FItems[j];
  items[i] := level;
  for j := i to Length(FItems) - 1 do
    items[j + 1] := FItems[j];
  FItems := items;
end;

procedure TLevels.AddAll(const other : TLevels);
var
  i : integer;
begin
  for i := 0 to Length(other.FItems) - 1 do
    Add(other.FItems[i]);
end;

function TLevels.Without(const level : integer) : TLevels;
var
  i : integer;
begin
  result := TLevels.Empty;
  for i := 0 to Length(FItems) - 1 do
    if FItems[i] <> level then
      result.Add(FItems[i]);
end;

function TLevels.IsEmpty : boolean;
begin
  result := Length(FItems) = 0;
end;

function TLevels.Highest : integer;
begin
  if Length(FItems) = 0 then
    result := 0
  else
    result := FItems[Length(FItems) - 1];
end;

{ TAssignment }

function TAssignment.IsTopLevel : boolean;
begin
  result := (Kind = akTopLevel) or ((Kind = akProjectReference) and TopLevelReference);
end;

{ TResolverContext }

constructor TResolverContext.Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string;
                                    const compilerVersion : TCompilerVersion; const newPackages : IList<IPackageInfo>;
                                    const projectReferences : IList<IPackageReference>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>;
                                    const preferredVersions : IDictionary<string, TPackageVersion>);
var
  newIds : ISet<string>;
  newPackage : IPackageInfo;
  reference : IPackageReference;
  assignment : TAssignment;
  id : string;
begin
  inherited Create;
  Assert((newPackages <> nil) and newPackages.Any);
  FLogger := logger;
  FPackageInstallerContext := packageInstallerContext;
  FProjectFile := projectFile;
  FCompilerVersion := compilerVersion;
  FAssignments := TCollections.CreateDictionary<string, TAssignment>;
  FConstraints := TCollections.CreateDictionary<string, IList<TConstraint>>;
  FAgenda := TCollections.CreateList<TRequirement>;
  FDeferred := TCollections.CreateList<TRequirement>;
  FTrail := TCollections.CreateList<TProc>;
  FNoGoods := TCollections.CreateDictionary<string, string>;
  if sharedVersionCache <> nil then
    FVersionLists := sharedVersionCache
  else
    FVersionLists := TCollections.CreateDictionary<string, IList<IPackageInfo>>;
  FPreferredVersions := preferredVersions;
  FProjectReferenceVersions := TCollections.CreateDictionary<string, TPackageVersion>;
  FTopLevels := TCollections.CreateList<string>;
  FSortedDependencies := TCollections.CreateDictionary<IPackageInfo, IList<IPackageDependency>>;

  newIds := TCollections.CreateSet<string>;
  for newPackage in newPackages do
  begin
    Assert(not newIds.Contains(LowerCase(newPackage.Id)), 'duplicate new package [' + newPackage.Id + ']');
    newIds.Add(LowerCase(newPackage.Id));
  end;

  for reference in projectReferences do
  begin
    id := LowerCase(reference.Id);
    //The package being installed - or, on restore's slow path, the top level being re-resolved - can also be
    //among the references : restore feeds the previous iteration's flattened graph (top levels AND transients)
    //back in. It replaces the reference - the top level pin is not negotiable.
    if newIds.Contains(id) or FAssignments.ContainsKey(id) then
      continue;
    assignment := MakeAssignment(reference.PackageInfo, akProjectReference, TLevels.Empty, reference.ParentId, reference.VersionRange);
    assignment.TopLevelReference := reference.IsTopLevel;
    SetAssignment(id, assignment);
    FProjectReferenceVersions[id] := reference.PackageInfo.Version;
    if reference.IsTopLevel then
      FTopLevels.Add(id);
  end;

  for newPackage in newPackages do
  begin
    id := LowerCase(newPackage.Id);
    SetAssignment(id, MakeAssignment(newPackage, akTopLevel, TLevels.Empty, cRootNode, TVersionRange.Create(newPackage.Version)));
    FTopLevels.Add(id);
  end;
  //once every top level is in, so their ranges constrain the first choice already.
  for newPackage in newPackages do
    Require(newPackage);
end;

procedure TResolverContext.SetAssignment(const packageId : string; const assignment : TAssignment);
begin
  FAssignments[packageId] := assignment;
  AddConstraints(assignment);
end;

procedure TResolverContext.AddConstraints(const assignment : TAssignment);
var
  dependency : IPackageDependency;
  list : IList<TConstraint>;
  constraint : TConstraint;
begin
  if assignment.PackageInfo.Dependencies = nil then
    exit;
  for dependency in assignment.PackageInfo.Dependencies do
  begin
    //any version (or the IDE's own) satisfies a bundled dependency - it constrains nothing.
    if dependency.VersionRange.IsBundledSentinel then
      continue;
    if not FConstraints.TryGetValue(LowerCase(dependency.Id), list) then
    begin
      list := TCollections.CreateList<TConstraint>;
      FConstraints[LowerCase(dependency.Id)] := list;
    end;
    constraint.Owner := assignment.PackageInfo;
    constraint.Responsible := assignment.Responsible;
    constraint.Range := dependency.VersionRange;
    list.Add(constraint);
  end;
end;

procedure TResolverContext.RemoveConstraints(const assignment : TAssignment);
var
  dependency : IPackageDependency;
  list : IList<TConstraint>;
  i : integer;
begin
  if assignment.PackageInfo.Dependencies = nil then
    exit;
  for dependency in assignment.PackageInfo.Dependencies do
  begin
    if dependency.VersionRange.IsBundledSentinel then
      continue;
    if not FConstraints.TryGetValue(LowerCase(dependency.Id), list) then
      continue;
    //undo runs newest first, so it is the last one this package added.
    for i := list.Count - 1 downto 0 do
      if list[i].Owner = assignment.PackageInfo then
      begin
        list.Delete(i);
        break;
      end;
  end;
end;

function TResolverContext.TryGetAssignment(const packageId : string; out assignment : TAssignment) : boolean;
begin
  result := FAssignments.TryGetValue(LowerCase(packageId), assignment);
end;

procedure TResolverContext.Assign(const assignment : TAssignment);
var
  id : string;
  current : TAssignment;
  previous : TAssignment;
  hadPrevious : boolean;
begin
  id := LowerCase(assignment.PackageInfo.Id);
  current := assignment;
  hadPrevious := FAssignments.TryGetValue(id, previous);
  if hadPrevious then
    RemoveConstraints(previous);
  SetAssignment(id, current);
  FTrail.Add(
    procedure
    begin
      RemoveConstraints(current);
      if hadPrevious then
        SetAssignment(id, previous)
      else
        FAssignments.Remove(id);
    end);
end;

function TResolverContext.ConstraintsOn(const packageId : string) : IList<TConstraint>;
begin
  if not FConstraints.TryGetValue(LowerCase(packageId), result) then
    result := TCollections.CreateList<TConstraint>;
end;

procedure TResolverContext.Require(const owner : IPackageInfo);
var
  dependency : IPackageDependency;
  requirement : TRequirement;
begin
  for dependency in SortedDependencies(owner) do
  begin
    requirement.Owner := owner;
    requirement.Dependency := dependency;
    if dependency.VersionRange.IsBundledSentinel then
      FDeferred.Add(requirement)
    else
      FAgenda.Add(requirement);
  end;
end;

function TResolverContext.NextRequirement(out requirement : TRequirement; out deferred : boolean) : boolean;
begin
  result := true;
  if FAgendaHead < FAgenda.Count then
  begin
    requirement := FAgenda[FAgendaHead];
    Inc(FAgendaHead);
    deferred := false;
    exit;
  end;
  if FDeferredHead < FDeferred.Count then
  begin
    requirement := FDeferred[FDeferredHead];
    Inc(FDeferredHead);
    deferred := true;
    exit;
  end;
  deferred := false;
  result := false;
end;

function TResolverContext.Mark : TResolverMark;
begin
  result.Trail := FTrail.Count;
  result.AgendaHead := FAgendaHead;
  result.AgendaCount := FAgenda.Count;
  result.DeferredHead := FDeferredHead;
  result.DeferredCount := FDeferred.Count;
end;

procedure TResolverContext.UndoTo(const mark : TResolverMark);
var
  undo : TProc;
begin
  while FTrail.Count > mark.Trail do
  begin
    undo := FTrail[FTrail.Count - 1];
    FTrail.Delete(FTrail.Count - 1);
    undo();
  end;
  while FAgenda.Count > mark.AgendaCount do
    FAgenda.Delete(FAgenda.Count - 1);
  FAgendaHead := mark.AgendaHead;
  while FDeferred.Count > mark.DeferredCount do
    FDeferred.Delete(FDeferred.Count - 1);
  FDeferredHead := mark.DeferredHead;
end;

function TResolverContext.IsNoGood(const package : IPackageInfo; out reason : string) : boolean;
begin
  result := FNoGoods.TryGetValue(NoGoodKey(package), reason);
end;

procedure TResolverContext.RecordNoGood(const package : IPackageInfo; const reason : string);
begin
  FNoGoods[NoGoodKey(package)] := reason;
end;

function TResolverContext.TryGetVersionList(const key : string; out versions : IList<IPackageInfo>) : boolean;
begin
  result := FVersionLists.TryGetValue(key, versions);
end;

procedure TResolverContext.CacheVersionList(const key : string; const versions : IList<IPackageInfo>);
begin
  FVersionLists[key] := versions;
end;

function TResolverContext.SortedDependencies(const package : IPackageInfo) : IList<IPackageDependency>;
var
  dependency : IPackageDependency;
  i : integer;
begin
  if FSortedDependencies.TryGetValue(package, result) then
    exit;
  result := TCollections.CreateList<IPackageDependency>;
  if package.Dependencies <> nil then
    for dependency in package.Dependencies do
    begin
      //insertion sort - stable, so dependencies of the same width keep the manifest's order.
      i := result.Count;
      while (i > 0) and (result[i - 1].VersionRange.PatchWidth > dependency.VersionRange.PatchWidth) do
        Dec(i);
      result.Insert(i, dependency);
    end;
  FSortedDependencies[package] := result;
end;

function TResolverContext.TryGetPreferredVersion(const packageId : string; out version : TPackageVersion) : boolean;
begin
  result := false;
  if FPreferredVersions = nil then
    exit;
  result := FPreferredVersions.TryGetValue(LowerCase(packageId), version);
end;

function TResolverContext.FindSiblingResolution(const packageId : string) : IResolvedPackage;
begin
  result := FPackageInstallerContext.FindPackageResolution(FProjectFile, packageId);
end;

function TResolverContext.IsUnchanged(const assignment : TAssignment) : boolean;
var
  key : string;
  version : TPackageVersion;
begin
  if assignment.Kind = akImported then
    exit(true);
  key := LowerCase(assignment.PackageInfo.Id);
  result := FProjectReferenceVersions.TryGetValue(key, version) and (version = assignment.PackageInfo.Version);
  if (not result) and (FPreferredVersions <> nil) then
    result := FPreferredVersions.TryGetValue(key, version) and (version = assignment.PackageInfo.Version);
end;

function TResolverContext.AnyTopLevelIsPrerelease : boolean;
var
  assignment : TAssignment;
begin
  result := false;
  for assignment in FAssignments.Values do
    if assignment.IsTopLevel and (not assignment.PackageInfo.Version.IsStable) then
      exit(true);
end;

function TResolverContext.ResolvedOrder : IList<TAssignment>;
var
  order : IList<TAssignment>;
  visited : ISet<string>;
  id : string;

  procedure Visit(const packageId : string);
  var
    assignment : TAssignment;
    dependency : IPackageDependency;
  begin
    if visited.Contains(packageId) then
      exit;
    if not FAssignments.TryGetValue(packageId, assignment) then
      exit;
    visited.Add(packageId);
    if assignment.PackageInfo.Dependencies <> nil then
      for dependency in assignment.PackageInfo.Dependencies do
        Visit(LowerCase(dependency.Id));
    order.Add(assignment);
  end;

begin
  order := TCollections.CreateList<TAssignment>;
  visited := TCollections.CreateSet<string>;
  for id in FTopLevels do
    Visit(id);
  result := order;
end;

function TResolverContext.GetResolvedPackageInfos : IList<IPackageInfo>;
var
  assignment : TAssignment;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for assignment in ResolvedOrder do
    result.Add(assignment.PackageInfo);
end;

function TResolverContext.GetResolvedPackages : TArray<IResolvedPackage>;
var
  order : IList<TAssignment>;
  i : integer;
begin
  order := ResolvedOrder;
  SetLength(result, order.Count);
  for i := 0 to order.Count - 1 do
  begin
    if order[i].Kind = akImported then
      result[i] := order[i].Imported.Clone(order[i].ParentId)
    else
      result[i] := TResolvedPackage.Create(order[i].PackageInfo, order[i].VersionRange, order[i].ParentId, FProjectFile, order[i].Kind = akBundled);
  end;
end;

function TResolverContext.BuildDependencyGraph : IPackageReference;
var
  root : IPackageReference;
  id : string;
  assignment : TAssignment;

  //true if [id] is already on the parent chain of [node] - a cycle (reported by the resolver) must not make
  //the graph infinite.
  function IsAncestor(const node : IPackageReference; const packageId : string) : boolean;
  var
    p : IPackageReference;
  begin
    result := false;
    p := node;
    while p <> nil do
    begin
      if SameText(p.Id, packageId) then
        exit(true);
      p := p.Parent;
    end;
  end;

  //Tolerates a partial solution : a dependency the resolve couldn't satisfy is left out, which is still useful
  //for diagnostics - callers that need the whole graph check the resolve's result first.
  procedure AddNode(const parent : IPackageReference; const package : TAssignment; const versionRange : TVersionRange);
  var
    node : IPackageReference;
    dependency : IPackageDependency;
    child : TAssignment;
    childRange : TVersionRange;
  begin
    node := parent.AddChild(package.PackageInfo.Id, package.PackageInfo.Version, versionRange);
    //what was resolved - so nothing downstream has to look it up again.
    node.PackageInfo := package.PackageInfo;
    if package.PackageInfo.Dependencies = nil then
      exit;
    for dependency in package.PackageInfo.Dependencies do
    begin
      if not FAssignments.TryGetValue(LowerCase(dependency.Id), child) then
      begin
        FLogger.Debug('BuildDependencyGraph: no resolution for [' + dependency.Id + '] required by [' + package.PackageInfo.Id + '-' +
                      package.PackageInfo.Version.ToStringNoMeta + '] - graph will be incomplete');
        continue;
      end;
      //bundled deps (e.g. Indy) are a no-op satisfied by the IDE - never an edge, else the dproj gets a
      //dangling 999.999.999 PackageReference and restore would pin it.
      if child.Kind = akBundled then
        continue;
      //a manifest that lists a dependency twice still gets one node for it.
      if node.FindTopLevelChild(dependency.Id) <> nil then
        continue;
      if IsAncestor(node, dependency.Id) then
      begin
        FLogger.Debug('BuildDependencyGraph: cycle detected for [' + dependency.Id + '] under [' + package.PackageInfo.Id + '] - skipping to avoid an infinite graph');
        continue;
      end;
      //The edge records THIS parent's declared range - the one the next restore checks it against - not the
      //range the dependency was first selected on, which belongs to whichever package needed it first.
      childRange := dependency.VersionRange;
      if childRange.IsEmpty then
        childRange := child.VersionRange; //defensive fallback only
      AddNode(node, child, childRange);
    end;
  end;

begin
  root := TPackageReference.CreateRoot(FCompilerVersion);
  for id in FTopLevels do
    if FAssignments.TryGetValue(id, assignment) and (assignment.Kind <> akBundled) then
      AddNode(root, assignment, TVersionRange.Empty);
  result := root;
end;

end.
