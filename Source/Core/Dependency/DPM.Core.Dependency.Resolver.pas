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

unit DPM.Core.Dependency.Resolver;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Package.Installer.Interfaces;

type
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver)
  private
    class var
      FMaxSteps : integer;
      FLastStepCount : integer;
    class constructor Create;
  private
    FLogger : ILogger;
    FRepositoryManager : IPackageRepositoryManager;
    FPackageCache : IPackageCache;
    FPackageInstallerContext : IPackageInstallerContext;
    FConfiguration : IConfiguration;
  protected
    function Initialize(const config: IConfiguration) : boolean;

    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>) : boolean; overload;
    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>; const preferredVersions : IDictionary<string, TPackageVersion>) : boolean; overload;
    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const projectFile : string; const options : TSearchOptions; const newPackages : IList<IPackageInfo>; const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>; const preferredVersions : IDictionary<string, TPackageVersion>; const conflicts : IList<TUnresolvableConflict>) : boolean; overload;

  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache; const packageInstallerContext : IPackageInstallerContext);

    //how many steps a resolve may take before it gives up (E2004).
    class property MaxSteps : integer read FMaxSteps write FMaxSteps;
    //how many steps the last resolve took - for tests and diagnostics.
    class property LastStepCount : integer read FLastStepCount;
  end;

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Package.Classes,
  DPM.Core.Dependency.Context;

/// The search
/// ----------
/// A depth-first search over versions, with conflict-directed backjumping. Requirements (a package's dependency
/// on another) are checked oldest first. A requirement on a package not yet in the solution is a decision : the
/// versions that satisfy every range declared for it so far are tried in turn - the version the project's graph
/// records first (restore), then the package cache's, newest first, then the repository's. Choosing a version
/// queues its own requirements.
///
/// Every change goes through the context, which can undo back to a mark. When something can't be satisfied, the
/// failure carries the decisions it depends on (TLevels) - the search goes back to the latest of them and tries
/// its next version, undoing everything after it. A version that fails on fixed facts alone (the top levels, the
/// project's references, the other projects' versions, the versions that exist) is a no-good and never tried
/// again; one that only clashed with another choice is tried again once that choice changes. A failure that
/// depends on no decision at all can't be avoided : it is recorded, the requirement is skipped, and the search
/// carries on, so every problem is reported in one run - once, at the end, and only those (a conflict in a
/// branch that was given up is not reported).
///
/// The top levels are not negotiable. What the project already has stays as it is, its dependencies unchecked
/// (an inconsistency that was already there is not this resolve's to report), unless a requirement needs a
/// version that isn't the one it has - a package another project in the group uses then takes that project's
/// version, any other is chosen again. A package the project references directly is never changed.

type
  //something that can't be satisfied, ready to report.
  TResolveFailure = record
    Code : TErrorCode;
    Message : string;
    Conflict : TUnresolvableConflict;
  end;

  //a choice of version for a package, and what's needed to go back and try the next one.
  TDecision = class
  public
    Level : integer;
    Requirement : TRequirement;
    PackageId : string;
    //the decisions the requirement's owner depends on.
    OwnerResponsible : TLevels;
    //where trying another version starts from : the requirement taken, nothing chosen.
    Mark : TResolverMark;
    //the ranges declared for the package when the decision was made.
    Constraints : IList<TConstraint>;
    //every version in the requirement's range found so far, in the order they are tried.
    Candidates : IList<IPackageInfo>;
    Seen : ISet<string>;
    NextIndex : integer;
    //0 nothing looked up yet, 1 the preferred version, 2 the package cache, 3 the repository.
    Tier : integer;
    SourcesUnavailable : string;
    Current : IPackageInfo;
    //the earlier decisions the versions tried so far failed because of.
    Accumulated : TLevels;
    //version -> why it failed.
    FailureReasons : IDictionary<string, string>;
    LastFailure : TResolveFailure;
    constructor Create;
  end;

  TResolveSearch = class
  private
    FLogger : ILogger;
    FRepositoryManager : IPackageRepositoryManager;
    FPackageCache : IPackageCache;
    FCancellationToken : ICancellationToken;
    FContext : TResolverContext;
    FPreRelease : boolean;
    FMaxSteps : integer;
    FDecisions : IList<TDecision>;
    FFailures : IList<TResolveFailure>;
    //packages a requirement can't be met for whatever is chosen - later requirements on them are the same
    //problem, already reported.
    FGivenUp : ISet<string>;
    FSteps : integer;
    FStopped : boolean;
    FBudgetExceeded : boolean;
    function Step : boolean;
    procedure ProcessRequirement(const requirement : TRequirement);
    procedure ProcessBundled(const requirement : TRequirement);
    procedure ChooseFor(const requirement : TRequirement; const owner : TAssignment);
    procedure Import(const resolution : IResolvedPackage; const owner : TAssignment);
    procedure Decide(const requirement : TRequirement; const owner : TAssignment);
    function Acceptable(const decision : TDecision; const candidate : IPackageInfo) : boolean;
    function NextCandidate(const decision : TDecision) : IPackageInfo;
    procedure LoadNextTier(const decision : TDecision);
    procedure AddCandidates(const decision : TDecision; const versions : IList<IPackageInfo>);
    procedure AssignCandidate(const decision : TDecision; const candidate : IPackageInfo);
    procedure Exhausted(const decision : TDecision; out failure : TResolveFailure; out levels : TLevels);
    procedure HandleFailure(failure : TResolveFailure; levels : TLevels; giveUpId : string);
    procedure RecordFailure(const failure : TResolveFailure);
    function AlreadyRecorded(const packageId : string; const parentId : string) : boolean;
    procedure CheckCycles;
    procedure ValidateResolvedDependencies;
    function VersionListKey(const packageId : string; const range : TVersionRange; const tier : string) : string;
  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
                       const cancellationToken : ICancellationToken; const context : TResolverContext; const preRelease : boolean;
                       const maxSteps : integer);
    //true when everything was satisfied. Reports what wasn't (nothing when cancelled).
    function Run : boolean;
    property Steps : integer read FSteps;
    property Failures : IList<TResolveFailure> read FFailures;
  end;

function MakeFailure(const code : TErrorCode; const message : string; const packageId : string; const parentId : string;
                     const range : TVersionRange; const reason : string) : TResolveFailure;
begin
  result.Code := code;
  result.Message := message;
  result.Conflict.PackageId := packageId;
  result.Conflict.ParentId := parentId;
  result.Conflict.RequestedRange := range;
  result.Conflict.Reason := reason;
end;

function FirstLine(const value : string) : string;
var
  p : integer;
begin
  p := Pos(#13, value);
  if p = 0 then
    p := Pos(#10, value);
  if p = 0 then
    result := value
  else
    result := Copy(value, 1, p - 1);
end;

{ TDecision }

constructor TDecision.Create;
begin
  inherited Create;
  Candidates := TCollections.CreateList<IPackageInfo>;
  Seen := TCollections.CreateSet<string>;
  FailureReasons := TCollections.CreateDictionary<string, string>;
  Accumulated := TLevels.Empty;
end;

{ TResolveSearch }

constructor TResolveSearch.Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache;
                                  const cancellationToken : ICancellationToken; const context : TResolverContext; const preRelease : boolean;
                                  const maxSteps : integer);
begin
  inherited Create;
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FCancellationToken := cancellationToken;
  FContext := context;
  FPreRelease := preRelease;
  FMaxSteps := maxSteps;
  FDecisions := TCollections.CreateObjectList<TDecision>(true);
  FFailures := TCollections.CreateList<TResolveFailure>;
  FGivenUp := TCollections.CreateSet<string>;
end;

function TResolveSearch.Step : boolean;
begin
  result := false;
  if FStopped then
    exit;
  if FCancellationToken.IsCancelled then
  begin
    FStopped := true;
    exit;
  end;
  Inc(FSteps);
  if FSteps > FMaxSteps then
  begin
    FStopped := true;
    FBudgetExceeded := true;
    exit;
  end;
  result := true;
end;

function TResolveSearch.VersionListKey(const packageId : string; const range : TVersionRange; const tier : string) : string;
begin
  //keyed by the range looked up with - a lookup for a narrow range must not stand in for a wider one.
  result := LowerCase(packageId) + '|' + CompilerToString(FContext.CompilerVersion) + '|' + BoolToStr(FPreRelease, true) + '|' + tier + '|' + range.ToString;
end;

function TResolveSearch.Run : boolean;
var
  requirement : TRequirement;
  deferred : boolean;
begin
  result := false;
  while Step do
  begin
    if not FContext.NextRequirement(requirement, deferred) then
      break;
    if deferred then
      ProcessBundled(requirement)
    else
      ProcessRequirement(requirement);
  end;
  //whoever owns the token says it was cancelled - the resolver says nothing.
  if FCancellationToken.IsCancelled then
    exit;
  if FBudgetExceeded then
    RecordFailure(MakeFailure(TErrorCode.E2004_ResolverIterationLimit, 'Dependency resolution stopped after ' + IntToStr(FMaxSteps) +
                              ' steps without finding versions that work together.', '', '', TVersionRange.Empty, 'resolver step limit exceeded'))
  else
  begin
    CheckCycles;
    ValidateResolvedDependencies;
  end;
  result := FFailures.Count = 0;
end;

procedure TResolveSearch.ProcessRequirement(const requirement : TRequirement);
var
  id : string;
  range : TVersionRange;
  owner : TAssignment;
  assignment : TAssignment;
  levels : TLevels;
  dependency : IPackageDependency;
begin
  dependency := requirement.Dependency;
  id := LowerCase(dependency.Id);
  range := dependency.VersionRange;
  //requirements are only queued for a package in the solution, and undone with it - but be sure.
  if (not FContext.TryGetAssignment(requirement.Owner.Id, owner)) or (owner.PackageInfo <> requirement.Owner) then
    exit;

  if FContext.TryGetAssignment(id, assignment) and (assignment.Kind <> akBundled) then
  begin
    if range.IsSatisfiedBy(assignment.PackageInfo.Version) then
      exit;
    if FGivenUp.Contains(id) then
      exit;
    case assignment.Kind of
      akTopLevel :
        HandleFailure(MakeFailure(TErrorCode.E2002_TopLevelVersionConflict, 'Package conflict - ' + owner.PackageInfo.Id + ' requires ' + dependency.Id + ' ' + range.ToString +
                                  ', but the top level reference is ' + dependency.Id + '-' + assignment.PackageInfo.Version.ToString + '.',
                                  dependency.Id, owner.PackageInfo.Id, range,
                                  'top-level version ' + assignment.PackageInfo.Version.ToString + ' is not negotiable and does not satisfy ' + range.ToString),
                      owner.Responsible, '');
      akProjectReference :
      begin
        //a version the project references directly is never changed for it.
        if assignment.TopLevelReference then
          HandleFailure(MakeFailure(TErrorCode.E2002_TopLevelVersionConflict, 'Package conflict - ' + owner.PackageInfo.Id + ' requires ' + dependency.Id + ' ' + range.ToString +
                                    ', but the top level reference is ' + dependency.Id + '-' + assignment.PackageInfo.Version.ToString + '.',
                                    dependency.Id, owner.PackageInfo.Id, range,
                                    'top-level version ' + assignment.PackageInfo.Version.ToString + ' is not negotiable and does not satisfy ' + range.ToString),
                        owner.Responsible, '')
        else
          ChooseFor(requirement, owner);
      end;
      akImported :
        HandleFailure(MakeFailure(TErrorCode.E2003_ProjectGroupConflict, 'Package conflict - ' + owner.PackageInfo.Id + ' requires ' + dependency.Id + ' ' + range.ToString +
                                  ', but project [' + assignment.Imported.ProjectFile + '] uses ' + dependency.Id + '-' + assignment.PackageInfo.Version.ToString + '.',
                                  dependency.Id, owner.PackageInfo.Id, range,
                                  'cross-project: version ' + assignment.PackageInfo.Version.ToString + ' selected in project [' + assignment.Imported.ProjectFile +
                                  '] does not satisfy ' + range.ToString),
                      owner.Responsible, '');
      akChosen :
      begin
        levels := owner.Responsible;
        levels.AddAll(assignment.Responsible);
        HandleFailure(MakeFailure(TErrorCode.E2005_SelectedVersionConflict, 'Package conflict - ' + owner.PackageInfo.Id + '-' + owner.PackageInfo.Version.ToStringNoMeta +
                                  ' requires ' + dependency.Id + ' ' + range.ToString + ' but ' + dependency.Id + '-' + assignment.PackageInfo.Version.ToStringNoMeta + ' is selected.',
                                  dependency.Id, owner.PackageInfo.Id, range,
                                  owner.PackageInfo.Id + '-' + owner.PackageInfo.Version.ToStringNoMeta + ' requires ' + range.ToString + ' but version ' +
                                  assignment.PackageInfo.Version.ToStringNoMeta + ' is selected'),
                      levels, '');
      end;
    end;
    exit;
  end;
  if FGivenUp.Contains(id) then
    exit;
  ChooseFor(requirement, owner);
end;

procedure TResolveSearch.ChooseFor(const requirement : TRequirement; const owner : TAssignment);
var
  dependency : IPackageDependency;
  sibling : IResolvedPackage;
begin
  dependency := requirement.Dependency;
  //the projects in a group share one version of a package - another project's is the one.
  sibling := FContext.FindSiblingResolution(dependency.Id);
  if sibling <> nil then
  begin
    Import(sibling, owner);
    if not dependency.VersionRange.IsSatisfiedBy(sibling.PackageInfo.Version) then
      HandleFailure(MakeFailure(TErrorCode.E2003_ProjectGroupConflict, 'Package conflict - ' + owner.PackageInfo.Id + ' requires ' + dependency.Id + ' ' +
                                dependency.VersionRange.ToString + ', but project [' + sibling.ProjectFile + '] uses ' + dependency.Id + '-' +
                                sibling.PackageInfo.Version.ToString + '.',
                                dependency.Id, owner.PackageInfo.Id, dependency.VersionRange,
                                'cross-project: version ' + sibling.PackageInfo.Version.ToString + ' selected in project [' + sibling.ProjectFile +
                                '] does not satisfy ' + dependency.VersionRange.ToString),
                    owner.Responsible, '');
    exit;
  end;
  Decide(requirement, owner);
end;

procedure TResolveSearch.Import(const resolution : IResolvedPackage; const owner : TAssignment);
var
  assignment : TAssignment;
begin
  //here because owner needs it - it goes if owner does.
  assignment := MakeAssignment(resolution.PackageInfo, akImported, owner.Responsible, owner.PackageInfo.Id, resolution.VersionRange);
  assignment.Imported := resolution;
  FContext.Assign(assignment);
  //its dependencies are this project's requirements too.
  FContext.Require(resolution.PackageInfo);
  FLogger.Debug('    adopted ' + resolution.PackageInfo.Id + '-' + resolution.PackageInfo.Version.ToStringNoMeta + ' from [' + resolution.ProjectFile + ']');
end;

procedure TResolveSearch.ProcessBundled(const requirement : TRequirement);
var
  id : string;
  owner : TAssignment;
  assignment : TAssignment;
  sibling : IResolvedPackage;
begin
  //Checked once everything else is. Satisfied by any real package with the id - a top level, a transient, or
  //another project's - at whatever version; only when there is none is it the IDE's own (a no-op).
  id := LowerCase(requirement.Dependency.Id);
  if FContext.TryGetAssignment(id, assignment) then
    exit;
  if (not FContext.TryGetAssignment(requirement.Owner.Id, owner)) or (owner.PackageInfo <> requirement.Owner) then
    exit;
  sibling := FContext.FindSiblingResolution(id);
  if sibling <> nil then
  begin
    Import(sibling, owner);
    exit;
  end;
  //under the package that needs it, not the root - it is not something the project references.
  FContext.Assign(MakeAssignment(TPackageInfo.CreateBundled(requirement.Dependency.Id, FContext.CompilerVersion), akBundled, owner.Responsible,
                                 owner.PackageInfo.Id, TVersionRange.Parse(cBundledDependencyVersion)));
end;

procedure TResolveSearch.Decide(const requirement : TRequirement; const owner : TAssignment);
var
  decision : TDecision;
  candidate : IPackageInfo;
  failure : TResolveFailure;
  levels : TLevels;
  mark : TResolverMark;
  packageId : string;
begin
  decision := TDecision.Create;
  decision.Level := FDecisions.Count + 1;
  decision.Requirement := requirement;
  decision.PackageId := LowerCase(requirement.Dependency.Id);
  decision.OwnerResponsible := owner.Responsible;
  //before anything is chosen - going back here also puts back a project reference being replaced.
  decision.Mark := FContext.Mark;
  decision.Constraints := TCollections.CreateList<TConstraint>(FContext.ConstraintsOn(decision.PackageId));
  FDecisions.Add(decision);

  candidate := NextCandidate(decision);
  if FStopped then
    exit;
  if candidate <> nil then
  begin
    AssignCandidate(decision, candidate);
    exit;
  end;
  Exhausted(decision, failure, levels);
  packageId := decision.PackageId;
  mark := decision.Mark;
  FDecisions.Delete(FDecisions.Count - 1);
  FContext.UndoTo(mark);
  HandleFailure(failure, levels, packageId);
end;

function TResolveSearch.Acceptable(const decision : TDecision; const candidate : IPackageInfo) : boolean;
var
  constraint : TConstraint;
  reason : string;
begin
  result := false;
  if not decision.Requirement.Dependency.VersionRange.IsSatisfiedBy(candidate.Version) then
    exit;
  for constraint in decision.Constraints do
    if not constraint.Range.IsSatisfiedBy(candidate.Version) then
      exit;
  if FContext.IsNoGood(candidate, reason) then
    exit;
  result := true;
end;

function TResolveSearch.NextCandidate(const decision : TDecision) : IPackageInfo;
var
  candidate : IPackageInfo;
begin
  result := nil;
  while true do
  begin
    while decision.NextIndex < decision.Candidates.Count do
    begin
      candidate := decision.Candidates[decision.NextIndex];
      Inc(decision.NextIndex);
      if Acceptable(decision, candidate) then
        exit(candidate);
    end;
    if decision.Tier >= 3 then
      exit;
    LoadNextTier(decision);
    if FStopped then
      exit;
  end;
end;

procedure TResolveSearch.AddCandidates(const decision : TDecision; const versions : IList<IPackageInfo>);
var
  info : IPackageInfo;
begin
  if versions = nil then
    exit;
  for info in versions do
  begin
    if not decision.Requirement.Dependency.VersionRange.IsSatisfiedBy(info.Version) then
      continue;
    if decision.Seen.Contains(info.Version.ToStringNoMeta) then
      continue;
    decision.Seen.Add(info.Version.ToStringNoMeta);
    decision.Candidates.Add(info);
  end;
end;

procedure TResolveSearch.LoadNextTier(const decision : TDecision);
var
  dependency : IPackageDependency;
  versions : IList<IPackageInfo>;
  preferredVersion : TPackageVersion;
  preferred : IPackageInfo;
  identity : IPackageIdentity;
  key : string;
  unavailable : string;
begin
  dependency := decision.Requirement.Dependency;
  Inc(decision.Tier);
  case decision.Tier of
    1 :
    begin
      //restore : the version the project's graph records, when it is still in range and in the cache.
      if FContext.TryGetPreferredVersion(dependency.Id, preferredVersion) and dependency.VersionRange.IsSatisfiedBy(preferredVersion) then
      begin
        //held in a local - an object made in a const interface argument is never freed.
        identity := TPackageIdentity.Create('', dependency.Id, preferredVersion, FContext.CompilerVersion);
        preferred := FPackageCache.GetPackageInfo(FCancellationToken, identity);
        if preferred <> nil then
        begin
          versions := TCollections.CreateList<IPackageInfo>;
          versions.Add(preferred);
          AddCandidates(decision, versions);
        end;
      end;
    end;
    2 :
    begin
      //the package cache holds the manifests of what has been downloaded - usually what a restore needs.
      key := VersionListKey(dependency.Id, dependency.VersionRange, 'cache');
      if not FContext.TryGetVersionList(key, versions) then
      begin
        versions := FPackageCache.GetCachedPackageVersionsWithDependencies(FCancellationToken, dependency.Id, FContext.CompilerVersion, dependency.VersionRange, FPreRelease);
        if versions = nil then
          versions := TCollections.CreateList<IPackageInfo>;
        FContext.CacheVersionList(key, versions);
      end;
      AddCandidates(decision, versions);
    end;
    3 :
    begin
      //what the cache has is used up (or it had nothing) - the sources may have more. Asked once per range.
      key := VersionListKey(dependency.Id, dependency.VersionRange, 'repo');
      if not FContext.TryGetVersionList(key, versions) then
      begin
        versions := FRepositoryManager.GetPackageVersionsWithDependencies(FCancellationToken, FContext.CompilerVersion, dependency.Id, dependency.VersionRange, FPreRelease);
        unavailable := FRepositoryManager.UnavailableSources;
        if FCancellationToken.IsCancelled then
        begin
          //a cancelled lookup finds nothing - that is not an answer to keep.
          FStopped := true;
          exit;
        end;
        if versions = nil then
          versions := TCollections.CreateList<IPackageInfo>;
        //only an answer every source gave is kept - the next lookup may get through.
        if unavailable = '' then
          FContext.CacheVersionList(key, versions)
        else
          decision.SourcesUnavailable := unavailable;
      end;
      AddCandidates(decision, versions);
    end;
  end;
end;

procedure TResolveSearch.AssignCandidate(const decision : TDecision; const candidate : IPackageInfo);
begin
  decision.Current := candidate;
  FContext.Assign(MakeAssignment(candidate, akChosen, TLevels.Single(decision.Level), decision.Requirement.Owner.Id, decision.Requirement.Dependency.VersionRange));
  FContext.Require(candidate);
  FLogger.Debug('    [' + IntToStr(decision.Level) + '] trying ' + candidate.Id + '-' + candidate.Version.ToStringNoMeta + ' for ' +
                decision.Requirement.Owner.Id + ' ' + decision.Requirement.Dependency.VersionRange.ToString);
end;

procedure TResolveSearch.Exhausted(const decision : TDecision; out failure : TResolveFailure; out levels : TLevels);
var
  owner : IPackageInfo;
  dependency : IPackageDependency;
  constraint : TConstraint;
  candidate : IPackageInfo;
  reasons : string;
  reason : string;
  excludedByConstraint : boolean;
  allExcludedByConstraints : boolean;
  conflictingRanges : string;
begin
  owner := decision.Requirement.Owner;
  dependency := decision.Requirement.Dependency;
  levels := decision.Accumulated;

  //nothing in the range at all - only the requirement's own range matters.
  if decision.Candidates.Count = 0 then
  begin
    levels.AddAll(decision.OwnerResponsible);
    if decision.SourcesUnavailable <> '' then
      failure := MakeFailure(TErrorCode.E1102_PackageNotFoundSourcesUnavailable, 'Unable to satisfy dependency : ' + owner.Id + ' -> ' + dependency.Id + ' ' +
                             dependency.VersionRange.ToString + ' - the source(s) [' + decision.SourcesUnavailable + '] could not be used, see the warning(s) above.',
                             dependency.Id, owner.Id, dependency.VersionRange,
                             'no version satisfying the range was found - source(s) [' + decision.SourcesUnavailable + '] could not be used')
    else
      failure := MakeFailure(TErrorCode.E2001_DependencyUnsatisfiable, 'Unable to satisfy dependency : ' + owner.Id + ' -> ' + dependency.Id + ' ' +
                             dependency.VersionRange.ToString + ' - no available version satisfies the range.',
                             dependency.Id, owner.Id, dependency.VersionRange, 'no available version satisfies the requested range');
    exit;
  end;

  //the versions in range were ruled out by the other ranges declared for the package, or failed.
  for constraint in decision.Constraints do
    levels.AddAll(constraint.Responsible);

  //one version, tried and failed : its failure is the problem, said as it was found.
  if (decision.Candidates.Count = 1) and (decision.FailureReasons.Count = 1) then
  begin
    failure := decision.LastFailure;
    exit;
  end;

  reasons := '';
  allExcludedByConstraints := decision.FailureReasons.Count = 0;
  for candidate in decision.Candidates do
  begin
    if decision.FailureReasons.TryGetValue(candidate.Version.ToStringNoMeta, reason) then
    begin
      reasons := reasons + sLineBreak + '  ' + candidate.Id + '-' + candidate.Version.ToStringNoMeta + ' : ' + FirstLine(reason);
      continue;
    end;
    excludedByConstraint := false;
    for constraint in decision.Constraints do
      if not constraint.Range.IsSatisfiedBy(candidate.Version) then
      begin
        excludedByConstraint := true;
        reasons := reasons + sLineBreak + '  ' + candidate.Id + '-' + candidate.Version.ToStringNoMeta + ' : ' + constraint.Owner.Id + '-' +
                   constraint.Owner.Version.ToStringNoMeta + ' requires ' + constraint.Range.ToString;
        break;
      end;
    if excludedByConstraint then
      continue;
    allExcludedByConstraints := false;
    if FContext.IsNoGood(candidate, reason) then
      reasons := reasons + sLineBreak + '  ' + candidate.Id + '-' + candidate.Version.ToStringNoMeta + ' : ' + FirstLine(reason);
  end;

  if allExcludedByConstraints then
  begin
    conflictingRanges := '';
    for constraint in decision.Constraints do
      conflictingRanges := conflictingRanges + sLineBreak + '  ' + constraint.Owner.Id + '-' + constraint.Owner.Version.ToStringNoMeta + ' requires ' + constraint.Range.ToString;
    failure := MakeFailure(TErrorCode.E2005_SelectedVersionConflict, 'Package conflict - no version of ' + dependency.Id + ' satisfies every package that needs it :' +
                           conflictingRanges, dependency.Id, owner.Id, dependency.VersionRange,
                           'no version of ' + dependency.Id + ' satisfies every range declared for it');
    exit;
  end;
  failure := MakeFailure(TErrorCode.E2001_DependencyUnsatisfiable, 'Unable to satisfy dependency : ' + owner.Id + ' -> ' + dependency.Id + ' ' +
                         dependency.VersionRange.ToString + ' - none of its versions can be used :' + reasons,
                         dependency.Id, owner.Id, dependency.VersionRange, 'none of the versions in range can be used');
end;

procedure TResolveSearch.HandleFailure(failure : TResolveFailure; levels : TLevels; giveUpId : string);
var
  level : integer;
  rest : TLevels;
  decision : TDecision;
  candidate : IPackageInfo;
  mark : TResolverMark;
begin
  while true do
  begin
    if FStopped then
      exit;
    //a cancelled lookup finds nothing - that is not a problem to report.
    if FCancellationToken.IsCancelled then
    begin
      FStopped := true;
      exit;
    end;
    //no choice can avoid it.
    if levels.IsEmpty then
    begin
      RecordFailure(failure);
      if giveUpId <> '' then
        FGivenUp.Add(giveUpId);
      exit;
    end;

    //back to the latest decision it depends on - the ones after it go with what they were built on.
    level := levels.Highest;
    while FDecisions.Count > level do
      FDecisions.Delete(FDecisions.Count - 1);
    decision := FDecisions[level - 1];
    FContext.UndoTo(decision.Mark);
    rest := levels.Without(level);
    decision.Accumulated.AddAll(rest);
    decision.FailureReasons[decision.Current.Version.ToStringNoMeta] := failure.Message;
    decision.LastFailure := failure;
    //it failed on fixed facts alone - whatever else is chosen, it would again.
    if rest.IsEmpty then
      FContext.RecordNoGood(decision.Current, failure.Message);
    FLogger.Debug('    [' + IntToStr(level) + '] ' + decision.Current.Id + '-' + decision.Current.Version.ToStringNoMeta + ' fails : ' + FirstLine(failure.Message));

    if not Step then
      exit;
    candidate := NextCandidate(decision);
    if FStopped then
      exit;
    if candidate <> nil then
    begin
      AssignCandidate(decision, candidate);
      exit;
    end;
    //every version failed - the failure belongs to what limited the choice.
    Exhausted(decision, failure, levels);
    giveUpId := decision.PackageId;
    mark := decision.Mark;
    FDecisions.Delete(FDecisions.Count - 1);
    FContext.UndoTo(mark);
  end;
end;

function TResolveSearch.AlreadyRecorded(const packageId : string; const parentId : string) : boolean;
var
  failure : TResolveFailure;
begin
  result := false;
  for failure in FFailures do
    if SameText(failure.Conflict.PackageId, packageId) and SameText(failure.Conflict.ParentId, parentId) then
      exit(true);
end;

procedure TResolveSearch.RecordFailure(const failure : TResolveFailure);
var
  existing : TResolveFailure;
begin
  //the same problem found again, by another requirement or after going back, is said once.
  for existing in FFailures do
    if (existing.Code = failure.Code) and SameText(existing.Conflict.PackageId, failure.Conflict.PackageId) and
       SameText(existing.Conflict.ParentId, failure.Conflict.ParentId) then
      exit;
  FFailures.Add(failure);
end;

/// A package that needs itself, directly or through others, can't be built - Delphi packages can't require each
/// other. Only what this resolve expanded is followed : a project reference's dependencies are as they were.
procedure TResolveSearch.CheckCycles;
var
  visiting : ISet<string>;
  done : ISet<string>;
  path : IList<string>;
  id : string;

  procedure Visit(const packageId : string);
  var
    assignment : TAssignment;
    dependency : IPackageDependency;
    target : string;
    cycle : string;
    i : integer;
    start : integer;
  begin
    if done.Contains(packageId) then
      exit;
    if not FContext.TryGetAssignment(packageId, assignment) then
      exit;
    if not (assignment.Kind in [akTopLevel, akChosen, akImported]) then
    begin
      done.Add(packageId);
      exit;
    end;
    visiting.Add(packageId);
    path.Add(assignment.PackageInfo.Id + '-' + assignment.PackageInfo.Version.ToStringNoMeta);
    if assignment.PackageInfo.Dependencies <> nil then
      for dependency in assignment.PackageInfo.Dependencies do
      begin
        if dependency.VersionRange.IsBundledSentinel then
          continue;
        target := LowerCase(dependency.Id);
        if visiting.Contains(target) then
        begin
          start := 0;
          for i := 0 to path.Count - 1 do
            if SameText(Copy(path[i], 1, Length(dependency.Id) + 1), dependency.Id + '-') then
              start := i;
          cycle := '';
          for i := start to path.Count - 1 do
            cycle := cycle + path[i] + ' -> ';
          cycle := cycle + path[start];
          RecordFailure(MakeFailure(TErrorCode.E2013_DependencyCycle, 'Packages depend on each other in a cycle : ' + cycle + '.', dependency.Id,
                                    assignment.PackageInfo.Id, dependency.VersionRange, 'dependency cycle : ' + cycle));
          continue;
        end;
        Visit(target);
      end;
    path.Delete(path.Count - 1);
    visiting.Remove(packageId);
    done.Add(packageId);
  end;

begin
  visiting := TCollections.CreateSet<string>;
  done := TCollections.CreateSet<string>;
  path := TCollections.CreateList<string>;
  for id in FContext.TopLevels do
    Visit(id);
end;

/// The search checks the requirements of what it expands - the new packages and what they pull in. The project's
/// references are kept as they are and not expanded, so a reference that pins a package this resolve changed
/// (e.g. Ext 1.0.1 pinning Base 1.0.1 while Base 1.0.3 is installed) is caught here - otherwise an inconsistent
/// graph would be written, one the next restore rejects. An edge between two packages this resolve did not
/// change is skipped - an inconsistency that was already there is not this resolve's to report, and must not
/// make an unrelated install (or a restore) fail.
procedure TResolveSearch.ValidateResolvedDependencies;
var
  assignment : TAssignment;
  target : TAssignment;
  dependency : IPackageDependency;
begin
  for assignment in FContext.ResolvedOrder do
  begin
    if (assignment.Kind = akBundled) or (assignment.PackageInfo.Dependencies = nil) then
      continue;
    for dependency in assignment.PackageInfo.Dependencies do
    begin
      if dependency.VersionRange.IsBundledSentinel then
        continue;
      //the problems with a package that couldn't be resolved are reported already.
      if FGivenUp.Contains(LowerCase(dependency.Id)) then
        continue;
      if not FContext.TryGetAssignment(dependency.Id, target) then
        continue;
      if target.Kind = akBundled then
        continue;
      if dependency.VersionRange.IsSatisfiedBy(target.PackageInfo.Version) then
        continue;
      if FContext.IsUnchanged(assignment) and FContext.IsUnchanged(target) then
        continue;
      if AlreadyRecorded(dependency.Id, assignment.PackageInfo.Id) then
        continue;
      RecordFailure(MakeFailure(TErrorCode.E2005_SelectedVersionConflict, 'Package conflict - ' + assignment.PackageInfo.Id + '-' + assignment.PackageInfo.Version.ToStringNoMeta +
                                ' requires ' + dependency.Id + ' ' + dependency.VersionRange.ToString + ' but ' + dependency.Id + '-' +
                                target.PackageInfo.Version.ToStringNoMeta + ' is selected.',
                                dependency.Id, assignment.PackageInfo.Id, dependency.VersionRange,
                                assignment.PackageInfo.Id + '-' + assignment.PackageInfo.Version.ToStringNoMeta + ' requires ' + dependency.VersionRange.ToString +
                                ' but version ' + target.PackageInfo.Version.ToStringNoMeta + ' is selected'));
    end;
  end;
end;

{ TDependencyResolver }

class constructor TDependencyResolver.Create;
begin
  FMaxSteps := 20000;
end;

constructor TDependencyResolver.Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache; const packageInstallerContext : IPackageInstallerContext);
begin
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FPackageInstallerContext := packageInstallerContext;
end;

//The repository manager is shared, and its owner initializes it - doing it again here would re-check (and
//re-report) the sources each time a resolver is set up, eg for the trial resolves an install plan makes.
function TDependencyResolver.Initialize(const config: IConfiguration) : boolean;
begin
  Assert(config <> nil);
  FConfiguration := config;
  result := true;
end;

function TDependencyResolver.ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                               const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo;
                                               const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference;
                                               out resolved : IList<IPackageInfo>) : boolean;
var
  noSharedCache : IDictionary<string, IList<IPackageInfo>>;
  noPreferred : IDictionary<string, TPackageVersion>;
begin
  noSharedCache := nil;
  noPreferred := nil;
  result := ResolveForInstall(cancellationToken, compilerVersion, projectFile, options, newPackage, projectReferences, dependencyGraph, resolved, noSharedCache, noPreferred);
end;

function TDependencyResolver.ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                               const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo;
                                               const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference;
                                               out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>;
                                               const preferredVersions : IDictionary<string, TPackageVersion>) : boolean;
var
  newPackages : IList<IPackageInfo>;
begin
  newPackages := TCollections.CreateList<IPackageInfo>;
  newPackages.Add(newPackage);
  result := ResolveForInstall(cancellationToken, compilerVersion, projectFile, options, newPackages, projectReferences, dependencyGraph, resolved, sharedVersionCache, preferredVersions, nil);
end;

function TDependencyResolver.ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                               const projectFile : string; const options : TSearchOptions; const newPackages : IList<IPackageInfo>;
                                               const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference;
                                               out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>;
                                               const preferredVersions : IDictionary<string, TPackageVersion>; const conflicts : IList<TUnresolvableConflict>) : boolean;
var
  packageRef : IPackageReference;
  resolution : IResolvedPackage;
  context : TResolverContext;
  search : TResolveSearch;
  failure : TResolveFailure;
  failureCount : integer;
  assignment : TAssignment;
  parent : TAssignment;
  parentText : string;
  stopwatch : TStopwatch;
begin
  Assert(FConfiguration <> nil, 'config is nil, Initialize has not been called');

  //check for mismatches with already loaded projects. projectReferences are what the project already
  //has, minus the packages being installed, so a mismatch here was there before the install and is still
  //there whether it goes ahead or not. Failing the install over it fixes nothing - it only blocks every
  //unrelated install in the group until the projects are brought in line by hand - so just report it.
  for packageRef in projectReferences do
  begin
    resolution := FPackageInstallerContext.FindPackageResolution(projectFile, packageRef.Id);
    if (resolution <> nil) and (not resolution.VersionRange.IsSatisfiedBy(packageRef.Version)) then
      FLogger.Warning(TWarningCode.W2010_ProjectGroupMismatch, 'Package project group mismatch : ' + packageRef.Id + '-' + packageRef.Version.ToStringNoMeta + ' in project [' + projectFile +
                      '] does not satisfy ' + resolution.VersionRange.ToString + ' used in project [' + resolution.ProjectFile + ']');
  end;

  stopwatch := TStopwatch.StartNew;
  FLogger.Verbose('Starting dependency resolution...');
  context := TResolverContext.Create(FLogger, FPackageInstallerContext, projectFile, compilerVersion, newPackages, projectReferences, sharedVersionCache, preferredVersions);
  try
    //NuGet-style prerelease handling, decided once for the whole resolve : the user's flag, or any top level
    //being a prerelease (installing Foo 1.0.0-beta turns it on, as NuGet does). Deciding it per package lets a
    //stable sibling resolve a transient as stable while a prerelease one resolves it as prerelease - two
    //versions of the same id in one resolve.
    search := TResolveSearch.Create(FLogger, FRepositoryManager, FPackageCache, cancellationToken, context,
                                    options.Prerelease or context.AnyTopLevelIsPrerelease, FMaxSteps);
    try
      result := search.Run;
      FLastStepCount := search.Steps;
      failureCount := search.Failures.Count;
      //cancelled : whoever owns the token says so, and what was found before it is moot.
      if not cancellationToken.IsCancelled then
        for failure in search.Failures do
        begin
          FLogger.Error(failure.Code, failure.Message);
          if conflicts <> nil then
            conflicts.Add(failure.Conflict);
        end;
    finally
      search.Free;
    end;

    resolved := context.GetResolvedPackageInfos;
    dependencyGraph := context.BuildDependencyGraph;
    if result then
    begin
      for assignment in context.ResolvedOrder do
      begin
        if not (assignment.Kind in [akChosen, akImported]) then
          continue;
        parentText := assignment.ParentId;
        if context.TryGetAssignment(assignment.ParentId, parent) then
          parentText := parent.PackageInfo.Id + '.' + parent.PackageInfo.Version.ToStringNoMeta;
        FLogger.Information('Resolving dependency : ' + parentText + '->' + assignment.PackageInfo.Id + ' ' + assignment.VersionRange.ToString);
        if assignment.Kind = akImported then
          FLogger.Information('    resolved earlier : ' + assignment.PackageInfo.Id + '.' + assignment.PackageInfo.Version.ToString)
        else
          FLogger.Information('            selected : ' + assignment.PackageInfo.Id + '.' + assignment.PackageInfo.Version.ToStringNoMeta);
      end;
      //record the resolutions so they can be checked against by other projects in the group. Only when the
      //resolve succeeded - a failed install leaves the project as it was, so its resolutions must not claim
      //otherwise.
      FPackageInstallerContext.RecordResolutions(projectFile, context.GetResolvedPackages);
      FLogger.Success('Dependency resolution done in [' + IntToStr(stopwatch.ElapsedMilliseconds) + 'ms]');
    end
    else if not cancellationToken.IsCancelled then
      FLogger.Information('Dependency resolution failed : ' + IntToStr(failureCount) + ' conflict(s).', true);
    FLogger.NewLine;
  finally
    context.Free;
  end;
end;

end.
