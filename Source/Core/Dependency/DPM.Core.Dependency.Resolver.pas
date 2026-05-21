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
  System.Diagnostics,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Context,
  DPM.Core.Options.Search,
  DPM.Core.Package.Installer.Interfaces;

type
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver)
  private
    FLogger : ILogger;
    FRepositoryManager : IPackageRepositoryManager;
    FPackageCache : IPackageCache;
    FPackageInstallerContext : IPackageInstallerContext;
    FStopwatch : TStopWatch;
    FConfiguration : IConfiguration;
  protected
    function Initialize(const config: IConfiguration) : boolean;

    function DoResolve(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; const context : IResolverContext) : boolean;

    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>> = nil; const preferredVersions : IDictionary<string, TPackageVersion> = nil) : boolean;

  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache; const packageInstallerContext : IPackageInstallerContext);

  end;

implementation

uses
  System.SysUtils,
  Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Package.Classes;

{ TDependencyResolver }

constructor TDependencyResolver.Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager; const packageCache : IPackageCache; const packageInstallerContext : IPackageInstallerContext);
begin
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FPackageCache := packageCache;
  FStopwatch := TStopwatch.Create;
  FPackageInstallerContext := packageInstallerContext;
end;

function SortDependencies(const Left, Right : IPackageDependency) : integer;
var
  w : integer;
begin
  //check the width of the range.. smaller range equals less dep versions
  //so we can fail faster.
  w := Left.VersionRange.PatchWidth - Right.VersionRange.PatchWidth;
  if w = 0 then
    result := 0
  else if w > 0 then
    result := 1
  else
    result := -1;
end;

/// This a simple depth first search with backtracking. It records unresolvable paths (nogoods) to avoid searching those again
/// When a conflict is found, it tries to resolve that by finding an overlapping dependency version range between the new
/// dependency range and the one already resolved. If found then it will undo the previous resolution and push it back on
/// the stack to be redone with the overlapping range. If not then we have an unresolvable conflict and exit.
///
/// Note that top level (non transitive) dependencies always win out in conflicts with transient dependencies.
/// A small optimisation is to sort the dependencies by the width of their dependency version range and
/// process them from small to large. The idea there is to fail as early as possible.
///
/// In basic testing, this seems to be fast enough, however as the eco system grows and dependencies become
/// deeper and more widespread, this simple algorithm may become slow.
///
/// The current algorithm is not great at explaining the problem when conflicts are found.
///
/// In the future we can implement a better algorithms which provides faster resolution
///
/// The PubGrub algorithm used in the dart package manager (pub) looks to be a good contender
/// https://medium.com/@nex3/pubgrub-2fb6470504f
/// ://github.com/dart-lang/pub/blob/master/doc/solver.md
/// If anyone is good math and want's to have a stab at implementing it in Delphi that would be great!

function TDependencyResolver.DoResolve(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const includePrerelease : boolean; const context : IResolverContext) : boolean;
const
  //Hard ceiling on resolver iterations. The depth-first / backtrack algorithm can in principle
  //loop on pathological inputs (e.g. cyclic manifests, or oscillating no-good resolution chains).
  //Real graphs land in the hundreds of iterations at most; 100k is well above that while still
  //guaranteeing termination in seconds rather than indefinitely.
  cMaxIterations = 100000;
var
  currentPackage : IPackageInfo;
  dependency : IPackageDependency;
  effectiveRange : TVersionRange;
  resolution : IResolvedPackage;
  parentResolution : IResolvedPackage;
  intersectingRange : TVersionRange;
  versions : IList<IPackageInfo>;
  version : IPackageInfo;
  selected : boolean;
  versionsFromCache : boolean;
  preferredVersion : TPackageVersion;
  preferredIdentity : IPackageIdentity;
  preferredInfo : IPackageInfo;
  backtrackOk : boolean;
  choices : integer;
  preRelease : boolean;
  conflict : TUnresolvableConflict;
  iterations : integer;
  limitConflict : TUnresolvableConflict;
begin
  FStopwatch.Reset;
  FStopwatch.Start;
  FLogger.Verbose('Starting dependency resolution...');

  iterations := 0;
  //NuGet-style prerelease handling: compute eligibility ONCE per resolve, from the user's
  //explicit flag plus an implicit promotion if any top-level package is itself a prerelease.
  //This matches what NuGet does when you `Install-Package Foo -Version 1.0.0-beta` - the
  //prerelease flag is implicitly turned on for the whole resolve so the install can complete,
  //and the same applies if the project already contains a prerelease top-level. Picking the
  //flag per-package (e.g. only when currentPackage is itself prerelease) leads to a stable
  //sibling resolving its transient as stable while a prerelease sibling resolves the same
  //transient as prerelease - two versions of the same id in one resolve.
  preRelease := includePrerelease or context.AnyTopLevelIsPrerelease;

  //NOTE : we are only resolving dependencies here - the requirements on the stack are already deemed to be resolved.
  while context.AnyOpenRequrements do
  begin
    Inc(iterations);
    if iterations > cMaxIterations then
    begin
      FLogger.Error('Dependency resolution exceeded ' + IntToStr(cMaxIterations) + ' iterations - aborting (likely infinite loop, e.g. cyclic dependency)');
      limitConflict.PackageId := '';
      limitConflict.ParentId := '';
      limitConflict.RequestedRange := TVersionRange.Empty;
      limitConflict.Reason := 'resolver iteration limit (' + IntToStr(cMaxIterations) + ') exceeded - suspected infinite loop';
      context.RecordUnresolvable(limitConflict);
      break;
    end;
    currentPackage := context.PopRequirement;
    //if the package has no dependencies then we are done with it.
    if (currentPackage.Dependencies = nil) or (not currentPackage.Dependencies.Any) then
      continue;

    //Sort dependencies by versionrange patch-width (smaller = fewer candidate versions = fail
    //faster). Sort order is stable across iterations now that the resolver no longer mutates
    //dependency.VersionRange, so we only sort the first time we see each package id - skip on
    //the re-pushes that happen during backtracking.
    if context.ShouldSortDependencies(currentPackage.Id) then
      currentPackage.Dependencies.Sort(TComparer<IPackageDependency>.Construct(SortDependencies));

    for dependency in currentPackage.Dependencies do
    begin
      //effectiveRange overlays the parent's declared range with any narrowing the resolver
      //already applied (from sibling intersections). We never mutate dependency.VersionRange.
      effectiveRange := context.GetEffectiveDependencyRange(currentPackage.Id, dependency.Id, dependency.VersionRange);
      FLogger.Information('Resolving dependency : ' + currentPackage.Id + '.' + currentPackage.Version.ToStringNoMeta + '->' + dependency.Id + ' ' + effectiveRange.ToString);
      //first see if we have resolved this package already. That may be in this project, or another project in the group.
      if context.GetOrImportResolvedPackage(dependency.Id, currentPackage.Id, resolution) then
      begin
        //check if the dependency range is satisfied by already resolved version
        if not effectiveRange.IsSatisfiedBy(resolution.PackageInfo.Version) then
        begin
          FLogger.Information('       conflict - selected version : ' + dependency.Id + '-' + resolution.PackageInfo.Version.ToString + ' does not satisfy ' + effectiveRange.ToString);

          //Check if the resolution comes from a different project, if so record the conflict and carry on
          //- we want to surface every conflict in one run rather than bail on the first.
          if not SameText(resolution.ProjectFile, context.ProjectFile) then
          begin
            FLogger.Error('Package project conflict - version : ' + dependency.Id + '-' + resolution.PackageInfo.Version.ToString + ' in project : ' + resolution.ProjectFile + ' does not satisfy ' + effectiveRange.ToString  );
            conflict.PackageId := dependency.Id;
            conflict.ParentId := currentPackage.Id;
            conflict.RequestedRange := effectiveRange;
            conflict.Reason := 'cross-project: version ' + resolution.PackageInfo.Version.ToString + ' selected in project [' + resolution.ProjectFile + '] does not satisfy ' + effectiveRange.ToString;
            context.RecordUnresolvable(conflict);
            continue;
          end
          else if resolution.IsTopLevel then //if it's a top level package then the version is not negotiable.
          begin
            FLogger.Error('Package conflict - selected version : ' + dependency.Id + '-' + resolution.PackageInfo.Version.ToString + ' does not satisfy ' + effectiveRange.ToString);
            conflict.PackageId := dependency.Id;
            conflict.ParentId := currentPackage.Id;
            conflict.RequestedRange := effectiveRange;
            conflict.Reason := 'top-level version ' + resolution.PackageInfo.Version.ToString + ' is not negotiable and does not satisfy ' + effectiveRange.ToString;
            context.RecordUnresolvable(conflict);
            continue;
          end;

          FLogger.Verbose('Attempting to find overalapping versionrange with dependency and earlier resolution');
          //see if we can reduce to an overlapping versionrange that satisfies both
          if resolution.VersionRange.TryGetIntersectingRange(effectiveRange, intersectingRange) then
          begin
            //Stored in the resolver context rather than mutating dependency.VersionRange, which
            //lives on the parent's manifest IPackageInfo and would corrupt re-evaluation if the
            //same package is processed again.
            context.NarrowDependencyRange(currentPackage.Id, dependency.Id, intersectingRange);
            FLogger.Debug('       overlapping range found : ' + dependency.Id + '-' + intersectingRange.ToString);
          end
          else
          begin
            //Record the resolved version as no good so we don't try it again, then backtrack
            //the parent. The previous "RecordNoGood returned true means we've been here before"
            //loop-detection branch was fragile - the global iteration limit in DoResolve catches
            //true infinite loops, so the early-detection path no longer earns its keep.
            context.RecordNoGood(resolution.PackageInfo);
            if context.TryGetResolvedPackage(resolution.ParentId, parentResolution) then
            begin
              context.RecordNoGood(parentResolution.PackageInfo);
              context.PushRequirement(parentResolution.PackageInfo);
            end;
          end;
          //unresolve the dependency
          context.RemoveResolvedPackage(dependency.Id);
          //try the current package again
          context.PushRequirement(currentPackage);
        end
        else
        begin
          if resolution.ProjectFile <> context.ProjectFile then
          begin
            FLogger.Information('    resolved earlier : ' + dependency.Id + '.' + resolution.PackageInfo.Version.ToString);
          end
          else
            FLogger.Information('            selected : ' + dependency.Id + '.' + resolution.PackageInfo.Version.ToString);
          //in the case where we are promoting a transitive to a direct dependency, we need a range.
          //the direct will not have a range so we convert the version to a range.
          if resolution.VersionRange.IsEmpty then
            resolution.VersionRange := TVersionRange.Create(resolution.PackageInfo.Version);
          //if the resolution came from another project, then we still need to deal with it's dependencies.
          if resolution.PackageInfo.Dependencies.Any then
            context.PushRequirement(resolution.PackageInfo);
        end;
        //we're good.. this is resolved.
        continue;
      end
      else
      begin
        selected := false;

        //Restore lock-file behaviour: if the project graph recorded a specific version for this
        //transient, use it directly when it's available in the cache and still satisfies the
        //parent's declared range. We only fall through to range-based selection when the recorded
        //version is missing, out of range, or already marked no-good.
        if context.TryGetPreferredVersion(dependency.Id, preferredVersion) then
        begin
          if effectiveRange.IsSatisfiedBy(preferredVersion) then
          begin
            preferredIdentity := TPackageIdentity.Create('', dependency.Id, preferredVersion, compilerVersion);
            preferredInfo := FPackageCache.GetPackageInfo(cancellationToken, preferredIdentity);
            if (preferredInfo <> nil) and (not context.IsNoGood(preferredInfo)) then
            begin
              context.RecordResolution(preferredInfo, effectiveRange, currentPackage.Id);
              if preferredInfo.Dependencies.Any then
                context.PushRequirement(preferredInfo);
              FLogger.Information('            selected (from graph) : ' + preferredInfo.Id + '.' + preferredInfo.Version.ToStringNoMeta);
              selected := true;
            end;
          end;
        end;

        //it wasn't resolved before, so get the available versions
        versionsFromCache := false;
        if not selected then
        begin
          versions := context.GetPackageVersions(dependency.Id);
          if versions = nil then
          begin
            //cache-first: the local package cache stores manifests for any version we've previously
            //downloaded. For a restore (where the dproj effectively pins exact versions) the cache
            //usually has exactly what we need. Only fall through to the network if the cache is empty
            //for this id.
            versions := FPackageCache.GetCachedPackageVersionsWithDependencies(cancellationToken, dependency.Id, compilerVersion, effectiveRange, preRelease);
            versionsFromCache := versions.Any;
            if not versionsFromCache then
              versions := FRepositoryManager.GetPackageVersionsWithDependencies(cancellationToken, compilerVersion, dependency.Id, effectiveRange, preRelease);
            if versions.Any then //cache the versions in the context in case we need them again
              context.CachePackageVersions(dependency.Id, versions);
          end;

          for version in versions do
          begin
            //skip versions we already know are no good.
            if context.IsNoGood(version) then
              continue;
            if effectiveRange.IsSatisfiedBy(version.Version) then
            begin
              context.RecordResolution(version, effectiveRange, currentPackage.Id);
              if version.Dependencies.Any then //no point pushing it if there are no dependencies - see top of loop
                context.PushRequirement(version); //resolve it's dependencies
              FLogger.Information('            selected : ' + version.Id + '.' + version.Version.ToStringNoMeta);
              selected := true;
              break;
            end
            else
              context.RecordNoGood(version); // doesn't satisfy the dependency version range - record so we don't try to use it again.
          end;
          //If we couldn't select from the cached set, the repo may have a newer in-range version
          //that we haven't downloaded yet. Re-query the repo and retry once before backtracking.
          if (not selected) and versionsFromCache then
          begin
            FLogger.Debug('         No cached version of ' + dependency.Id + ' satisfies ' + effectiveRange.ToString + ' - querying repository');
            versions := FRepositoryManager.GetPackageVersionsWithDependencies(cancellationToken, compilerVersion, dependency.Id, effectiveRange, preRelease);
            if versions.Any then
              context.CachePackageVersions(dependency.Id, versions);
            for version in versions do
            begin
              if context.IsNoGood(version) then
                continue;
              if effectiveRange.IsSatisfiedBy(version.Version) then
              begin
                context.RecordResolution(version, effectiveRange, currentPackage.Id);
                if version.Dependencies.Any then
                  context.PushRequirement(version);
                FLogger.Information('            selected : ' + version.Id + '.' + version.Version.ToStringNoMeta);
                selected := true;
                break;
              end
              else
                context.RecordNoGood(version);
            end;
          end;
        end;
        if not selected then
        begin
          //if we get here we are blocked on this path.
          //make sure we never try the currentPackage version again
          context.RecordNoGood(currentPackage);
          FLogger.Debug('         Unable to satisfy dependency ' + dependency.Id + '-' + effectiveRange.ToString);

          //Try to backtrack to a point where a different choice could have been made.
          //If currentPackage has alternative untried versions cached, hand control back to its
          //parent so the parent re-evaluates against those alternatives. Otherwise the conflict
          //is genuine - record it and move on so the rest of the graph still gets evaluated.
          backtrackOk := false;
          versions := context.GetPackageVersions(currentPackage.id);
          if (versions <> nil) and versions.Any then
          begin
            choices := 0;
            for version in versions do
            begin
              if context.IsNoGood(version) then
                continue;
              Inc(choices);
            end;
            if choices > 0 then
            begin
              if context.TryGetResolvedPackage(currentPackage.Id, resolution) then
              begin
                //can't backtrack to a root (direct dependency)
                if (not resolution.IsTopLevel) and context.TryGetResolvedPackage(resolution.ParentId, parentResolution) then
                begin
                  FLogger.Debug('Backtracking to : ' + parentResolution.PackageInfo.Id + '-' + parentResolution.PackageInfo.Version.ToString);
                  context.RemoveResolvedPackage(currentPackage.Id);
                  context.PushRequirement(parentResolution.PackageInfo); //force the currentpackage dependencies to be re-evaluated.
                  break;
                end;
              end;
            end;
          end;

          if not backtrackOk then
          begin
            FLogger.Error('Unable to satisfy dependency : ' + currentPackage.Id + ' -> ' + dependency.Id + ' ' + effectiveRange.ToString);
            conflict.PackageId := dependency.Id;
            conflict.ParentId := currentPackage.Id;
            conflict.RequestedRange := effectiveRange;
            conflict.Reason := 'no available version satisfies the requested range';
            context.RecordUnresolvable(conflict);
            continue;
          end;
        end;
      end;
    end;
  end;

  if context.HasUnresolvable then
  begin
    FLogger.Error('Dependency resolution failed: ' + IntToStr(context.GetUnresolvable.Count) + ' conflict(s)');
    result := false;
  end
  else
  begin
    FLogger.Success('Dependency resolution done in [' + IntToStr(FStopwatch.ElapsedMilliseconds) + 'ms]');
    result := true;
  end;
  FLogger.NewLine;

end;

function TDependencyResolver.Initialize(const config: IConfiguration) : boolean;
begin
  Assert(config <> nil);
  FConfiguration := config;
  result :=FRepositoryManager.Initialize(config);
end;

function TDependencyResolver.ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                               const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo;
                                               const projectReferences : IList<IPackageReference>; out dependencyGraph : IPackageReference;
                                               out resolved : IList<IPackageInfo>; const sharedVersionCache : IDictionary<string, IList<IPackageInfo>>;
                                               const preferredVersions : IDictionary<string, TPackageVersion>) : boolean;
var
  context : IResolverContext;
  packageRef : IPackageReference;
  resolution : IResolvedPackage;
  errorCount : integer;
begin
  Assert(FConfiguration <> nil, 'config is nil, Initialize has not been called');

  errorCount := 0;
  //check for conflicts with already loaded projects
  for packageRef in projectReferences do
  begin
    resolution := FPackageInstallerContext.FindPackageResolution(projectFile, packageRef.Id);
    if (resolution <> nil) and (not resolution.VersionRange.IsSatisfiedBy(packageRef.Version)) then
    begin
      FLogger.Error('Package project group conflict : ' + packageRef.Id + '-' + resolution.PackageInfo.Version.ToString + ' in project : ' + resolution.ProjectFile + ' does not satisfy ' + packageRef.Version.ToString  );
      Inc(errorCount)
      //exit;
    end;
  end;
  context := TResolverContext.Create(FLogger, FPackageInstallerContext, projectFile, newPackage, projectReferences, sharedVersionCache, preferredVersions);

  result := DoResolve(cancellationToken, compilerVersion, options.Prerelease, context);
  resolved := context.GetResolvedPackageInfos;
  dependencyGraph := context.BuildDependencyGraph;
  //record the resolutions so they can be applied to other projects in the group
  FPackageInstallerContext.RecordResolutions(projectFile, context.GetResolvedPackages);
  result := result and (errorCount = 0);
end;


end.

