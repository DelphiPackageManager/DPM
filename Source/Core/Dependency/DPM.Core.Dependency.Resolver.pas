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
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Context,
  DPM.Core.Options.Search;

type
  TDependencyResolver = class(TInterfacedObject, IDependencyResolver)
  private
    FLogger : ILogger;
    FRepositoryManager : IPackageRepositoryManager;
    FStopwatch : TStopWatch;
  protected
    function DoResolve(const cancellationToken : ICancellationToken; const options : TSearchOptions; const context : IResolverContext; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : boolean;

    function ResolveForInstall(const cancellationToken : ICancellationToken; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;

    function ResolveForRestore(const cancellationToken : ICancellationToken; const options : TSearchOptions; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;

  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager);

  end;

implementation

uses
  System.SysUtils,
  Generics.Defaults,
  DPM.Core.Dependency.Graph;

{ TDependencyResolver }

constructor TDependencyResolver.Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager);
begin
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FStopwatch := TStopwatch.Create;
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
/// Note that top level (non transient) dependencies always win out in conflicts with transient dependencies.
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
/// https://github.com/dart-lang/pub/blob/master/doc/solver.md
/// If anyone is good math and want's to have a stab at implementing it in Delphi that would be great!

function TDependencyResolver.DoResolve(const cancellationToken : ICancellationToken; const options : TSearchOptions; const context : IResolverContext; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : boolean;
var
  currentPackage : IPackageInfo;
  dependency : IPackageDependency;
  resolution : IResolution;
  parentResolution : IResolution;
  overlappingRange : TVersionRange;
  versions : IList<IPackageInfo>;
  version : IPackageInfo;
  selected : boolean;
  choices : integer;
  searchOptions : TSearchOptions;
begin
  FStopwatch.Reset;
  FStopwatch.Start;
  FLogger.Information('Starting dependency resolution...');
  result := false;

  while context.AnyOpenRequrements do
  begin
    currentPackage := context.PopRequirement;
    //if the package has no dependencies then we are done with it.
    if not currentPackage.Dependencies.Any then
      continue;

    //Sort the dependencies by the width of the dependency's versionrange (smaller the better)
    //the idea is to fail as quickly as possible, the less work we do the better
    currentPackage.Dependencies.Sort(TComparer<IPackageDependency>.Construct(SortDependencies));

    for dependency in currentPackage.Dependencies do
    begin
      FLogger.Information('Resolving dependency : ' + currentPackage.Id + '.' + currentPackage.Version.ToStringNoMeta + '->' + dependency.Id + ' ' + dependency.VersionRange.ToString, true);
      //first see if we have resolved this package already.
      if context.TryGetResolution(dependency.Id, resolution) then
      begin
        //check if the dependency range satisfies the already resolved version
        if not dependency.VersionRange.Satisfies(resolution.Package.Version) then
        begin
          FLogger.Debug('       conflict - selected version : ' + dependency.Id + '-' + resolution.Package.Version.ToString + ' does not satisfy ' + dependency.VersionRange.ToString);

          //if it's a top level package then the version is not negotiable.
          if resolution.ParentId = 'root' then
          begin
            FLogger.Error('Package conflict - selected version : ' + dependency.Id + '-' + resolution.Package.Version.ToString + ' does not satisfy ' + dependency.VersionRange.ToString);
            exit;
          end;

          //see if we can reduce to an overlapping versionrange that satisfies both
          if resolution.VersionRange.TryGetOverlappingVersion(dependency.VersionRange, overlappingRange) then
          begin
            //resolution.Dependency.Version := overlappingRange;
            dependency.VersionRange := overlappingRange;
            FLogger.Debug('       overlapping range found : ' + dependency.Id + '-' + overlappingRange.ToString);
          end
          else
          begin
            //record the resolved version as no good, so we don't try it again
            context.RecordNoGood(resolution.Package);
            //backtrack the package/version that got us here in the first place
            //not 100% sure this is correct here. More testing needed.
            if context.TryGetResolution(resolution.ParentId, parentResolution) then
            begin
              context.RecordNoGood(parentResolution.Package);
              context.PushRequirement(parentResolution.Package);
            end;
          end;
          //unresolve the dependency
          context.RemoveResolution(dependency.Id);
          //try the current package again
          context.PushRequirement(currentPackage);
        end
        else
        begin
          FLogger.Debug('       resolved : ' + dependency.Id + '.' + resolution.Package.Version.ToString);
          //in the case where we are promoting a transient to a direct dependency, we need a range.
          //the direct will not have a range so we convert the version to a range.
          if resolution.VersionRange.IsEmpty then
            resolution.VersionRange := TVersionRange.Create(resolution.Package.Version);
        end;
        //we're good.. this is resolved.
        continue;
      end
      else
      begin
        //it wasn't resolved before, so get the available versions
        versions := context.GetPackageVersions(dependency.Id);
        if versions = nil then
        begin
          searchOptions := options.Clone;
          searchOptions.SearchTerms := dependency.Id;
          //passing in the version range to filter the results - not sure if this is valid.. do we need all versions?
          //I suspect it may result in more failures
          versions := FRepositoryManager.GetPackageVersionsWithDependencies(cancellationToken, searchOptions, platform, dependency.VersionRange); // TVersionRange.Empty);
          if versions.Any then
            context.AddPackageVersions(dependency.Id, versions);
        end;

        selected := false;
        for version in versions do
        begin
          if context.IsNoGood(version) then
            continue;
          if dependency.VersionRange.Satisfies(version.Version) then
          begin
            context.RecordResolution(version, dependency.VersionRange, currentPackage.Id);
            if version.Dependencies.Any then //no point pushing it if there are no dependencies - see top of loop
              context.PushRequirement(version); //resolve it's dependencies
            FLogger.Debug('            selected : ' + version.Id + '.' + version.Version.ToStringNoMeta);
            selected := true;
            break;
          end
          else
            context.RecordNoGood(version); // so we don't try to use it again.
        end;
        if not selected then
        begin
          //if we get here we are blocked on this path.
          //make sure we never try the currentPackage version again
          context.RecordNoGood(currentPackage);
          FLogger.Debug('         Unable to satisfy dependency ' + dependency.Id + '-' + dependency.VersionRange.ToString);

          //try backtrack up to where a different choice could have been made for the current package
          choices := 0;
          versions := context.GetPackageVersions(currentPackage.id);
          if (versions <> nil) and versions.Any then
          begin
            //see if there are any other choices for the current version
            for version in versions do
            begin
              if context.IsNoGood(version) then
                continue;
              Inc(choices);
            end;
            if choices > 0 then
            begin
              //get the parent, and backtrack to it.
              if context.TryGetResolution(currentPackage.Id, resolution) then
              begin
                //can't backtrack to a root (direct dependency)
                if (resolution.ParentId <> 'root') and context.TryGetResolution(resolution.ParentId, parentResolution) then
                begin
                  FLogger.Debug('Backtracking to : ' + parentResolution.Package.Id + '-' + parentResolution.Package.Version.ToString);
                  context.RemoveResolution(currentPackage.Id); //shouldn't this be the parentResolution.Pacakage???
                  context.PushRequirement(parentResolution.Package);
                  break;
                end;
              end;
            end;
            //if we get here we are unable to continue.
            //TODO : Context.RecordUnresolvable - and just continue until done.
            FLogger.Error('Unable to satisfy dependency : ' + currentPackage.Id + ' -> ' + dependency.Id + ' ' + dependency.VersionRange.ToString);
            exit(false);
          end;
        end;
      end;
    end;
  end;

  result := true;

  FLogger.Debug('Dependency resolution done in ' + IntToStr(FStopwatch.ElapsedMilliseconds) + 'ms');

end;

function TDependencyResolver.ResolveForInstall(const cancellationToken : ICancellationToken; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
var
  context : IResolverContext;
begin
  context := TResolverContext.Create(FLogger, newPackage, projectReferences);
  result := DoResolve(cancellationToken, options, context, compilerVersion, platform);
  resolved := context.GetResolvedPackages;
  dependencyGraph := context.BuildDependencyGraph;
end;

function TDependencyResolver.ResolveForRestore(const cancellationToken : ICancellationToken; const options : TSearchOptions; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
var
  context : IResolverContext;
begin
  context := TResolverContext.Create(FLogger, options.CompilerVersion,  platform, projectReferences);
  result := DoResolve(cancellationToken, options, context, compilerVersion, platform);
  resolved := context.GetResolvedPackages;
  dependencyGraph := context.BuildDependencyGraph;
end;


end.

