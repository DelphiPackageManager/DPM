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
    //cache package versions available to avoid getting them multiple times.
    FContext : IResolverContext;
    FStopwatch : TStopWatch;
  protected
    function SetInitialState(const newPackage : IPackageInfo; const projectReferences : IList<IPackageInfo>; const lockFile : IGraphNode) : boolean;

    function DoResolve(const options : TSearchOptions; const lockFile : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;

    function ResolveForInstall(const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageInfo>; const lockFile : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;

    function ResolveForRestore(const options : TSearchOptions; const projectReferences : IList<IPackageInfo>; const lockFile : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;

  public
    constructor Create(const logger : ILogger; const repositoryManager : IPackageRepositoryManager);

  end;

implementation

uses
  System.SysUtils,
  Generics.Defaults,
  DPM.Core.Dependency.Graph;

{ TDependencyResolver }

constructor TDependencyResolver.Create(const logger: ILogger; const repositoryManager : IPackageRepositoryManager);
begin
  FLogger := logger;
  FRepositoryManager := repositoryManager;
  FContext := TResolverContext.Create(FLogger); //TODO : Inject this!
  FStopwatch := TStopwatch.Create;
end;

function SortDependencies(const Left, Right: IPackageDependency): integer;
var
  w : integer;
begin
  //check the width of the range.. smaller range equals less dep versions
  //so we can fail faster.
  w := Left.Version.PatchWidth - Right.Version.PatchWidth;
  if w = 0 then
    result := 0
  else if w > 0  then
    result := 1
  else
    result := -1;
end;

function TDependencyResolver.DoResolve(const options: TSearchOptions; const lockFile: IGraphNode; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; out resolved: IList<IPackageInfo>): boolean;
var
  currentPackage : IPackageInfo;
  dependency : IPackageDependency;
  resolution : IResolution;
  parentResolution : IResolution;
  overlappingRange : TVersionRange;
  versions : IList<IPackageInfo>;
//  sAvailableVersions : string;
  version : IPackageInfo;
  selected : boolean;
  choices : integer;
  searchOptions : TSearchOptions;
begin
  FStopwatch.Reset;
  FStopwatch.Start;
  FLogger.Debug('Starting dependency resolution...');
  resolved := nil;
  result := false;
  //for now we will ignore the lock file and just do the full resolution.

  while FContext.AnyOpen do
  begin
    currentPackage := FContext.PopRequirement;
//    FLogger.Debug('Evaluating package : ' + currentPackage.ToString);
    if not currentPackage.Dependencies.Any then
    begin

    //  FLogger.Debug('  no dependencies');
      continue;
    end;

    //Sort the dependencies by the width of the dependencies range (lower the better)
    //the idea is to fail as quickly as possible, the less work we do the better the performance
    currentPackage.Dependencies.Sort(TComparer<IPackageDependency>.Construct(SortDependencies));

    for dependency in currentPackage.Dependencies do
    begin
      FLogger.Information('Resolving dependency : ' + currentPackage.Id + '.' + currentPackage.Version.ToStringNoMeta + '->' + dependency.Id + ' ' + dependency.Version.ToString, true);
      //first see if we have resolved this package already.
      if FContext.TryGetResolution(dependency.Id, resolution) then
      begin
        //check if the already dependency range satisfies the already resolved version
        if not dependency.Version.Satisfies(resolution.Package.Version) then
        begin
          FLogger.Debug('       conflict - selected version : ' + dependency.Id + '-' + resolution.Package.Version.ToString + ' does not satisfy ' + dependency.Version.ToString);

          //if it's a top level package then the version is not negotiable.
          if resolution.ParentId = '' then
          begin
            FLogger.Error('Package conflict - selected version : ' + dependency.Id + '-' + resolution.Package.Version.ToString + ' does not satisfy ' + dependency.Version.ToString);
            exit;
          end;

          //see if we can reduce to an overlapping versionrange that satisfies both
          if (resolution.Dependency <> nil) and resolution.Dependency.Version.TryGetOverlappingVersion(dependency.Version,overlappingRange) then
          begin
            //resolution.Dependency.Version := overlappingRange;
            dependency.Version := overlappingRange;
            FLogger.Debug('       overlapping range found : ' + dependency.Id + '-' + overlappingRange.ToString);
          end
          else
          begin
            FContext.RecordBadChoice(resolution.Package);
            if FContext.TryGetResolution(resolution.ParentId, parentResolution) then
            begin
              FContext.RecordBadChoice(parentResolution.Package);
              FContext.PushRequirement(parentResolution.Package);
            end;

          end;
          //unresolve the dependency
          FContext.RemoveResolution(dependency.Id);
          //try the current package again
          FContext.PushRequirement(currentPackage);
        end
        else
          FLogger.Debug('       existing selection ok : ' + dependency.Id + '.' + resolution.Package.Version.ToString);
        //we're good.. this is resolved.
        continue;
      end
      else
      begin
        versions := FContext.GetVersions(dependency.Id);
        if versions = nil then
        begin
           searchOptions := options.Clone;
           searchOptions.SearchTerms := dependency.Id;
           versions := FRepositoryManager.GetPackageVersions(searchOptions, platform, TVersionRange.Empty);
           if versions.Any then
              FContext.AddVersions(dependency.Id, versions);
        end;
        {
        for version in versions do
        begin
          if FContext.IsBad(version) then
            continue;

          if sAvailableVersions = '' then
            sAvailableVersions := '[' +version.Version.ToString
          else
            sAvailableVersions := sAvailableVersions +  ', ' + version.Version.ToString;
        end;
        sAvailableVersions := sAvailableVersions + ']';
        FLogger.Debug('       available versions of : ' + dependency.Id + ' ' + sAvailableVersions);
        }
        selected := false;
        for version in versions do
        begin
          if FContext.IsBad(version) then
            continue;
          if dependency.Version.Satisfies(version.Version) then
          begin
            FContext.RecordResolution(version, dependency, currentPackage.Id);
            if version.Dependencies.Any then //no point pushing it if there are no dependencies - see top of loop
              FContext.PushRequirement(version);
            FLogger.Debug('            selected : ' + version.Id + '.' + version.Version.ToStringNoMeta);
            selected := true;
            break;
          end
          else
            FContext.RecordBadChoice(version); // so we don't try to use it again.
        end;
        if not selected then
        begin
          //if we get here we are blocked on this path.
          FContext.RecordBadChoice(currentPackage); //make sure we never try the currentPackage version again
          FLogger.Debug('         Unable to satisfy dependency ' + dependency.Id + '-' + dependency.Version.ToString);
          //try backtrack up to where a different choice could have been made for the current
          choices := 0;
          versions := FContext.GetVersions(currentPackage.id);
          if (versions <> nil) and versions.Any then
          begin
            //see if there are any other choices for the current version
            for version in versions do
            begin
              if FContext.IsBad(version) then
                continue;
              Inc(choices);
            end;
            if choices > 0 then
            begin
              //get the parent, and requeue it.
              if FContext.TryGetResolution(currentPackage.Id, resolution) then
              begin
                if (resolution.ParentId <> '') and FContext.TryGetResolution(resolution.ParentId, parentResolution)  then
                begin
                  FLogger.Debug('Backtracking to : ' + parentResolution.Package.Id + '-' + parentResolution.Package.Version.ToString);
                  FContext.RemoveResolution(currentPackage.Id); //shouldn't this be the parentResolution.Pacakage???
                  FContext.PushRequirement(parentResolution.Package);
                  break;
                end;
              end;
            end;
            //TODO : Context.RecordUnresolvable - and just continue until done.
            FLogger.Error('Unable to satisfy dependency : ' + currentPackage.Id + ' -> ' + dependency.Id + ' ' + dependency.Version.ToString);
            exit(false);
          end;
        end;
      end;
    end;
  end;

  resolved := FContext.GetResolvedPackages;

  result := true;

  FLogger.Debug('Dependency resolution done in '  + IntToStr(FStopwatch.ElapsedMilliseconds) + 'ms');

end;

function TDependencyResolver.ResolveForInstall(const options : TSearchOptions; const newPackage: IPackageInfo; const projectReferences: IList<IPackageInfo>; const lockFile: IGraphNode; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; out resolved : IList<IPackageInfo>): boolean;
begin
  FContext.Reset;
  resolved := nil;
  //TODO : Use the graph to record resolutions and setup initial state.
  SetInitialState(newPackage, projectReferences,lockFile);

  result := DoResolve(options, lockFile, compilerVersion, platform, resolved);

end;

function TDependencyResolver.ResolveForRestore(const options: TSearchOptions; const projectReferences : IList<IPackageInfo>; const lockFile: IGraphNode; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; out resolved: IList<IPackageInfo>): boolean;
begin
  FContext.Reset;
  resolved := nil;
  //TODO : Use the graph to record resolutions and setup initial state.
  SetInitialState(nil, projectReferences,lockFile);

  result := DoResolve(options, lockFile, compilerVersion, platform, resolved);

  FContext.Reset;

end;

function TDependencyResolver.SetInitialState(const newPackage: IPackageInfo; const projectReferences: IList<IPackageInfo>; const lockFile: IGraphNode): boolean;
var
  package : IPackageInfo;
//  versions : IList<IPackageInfo>;
begin
  for package in projectReferences do
  begin
    FContext.PushRequirement(package);
    FContext.RecordResolution(package, nil, '');
//    versions := TCollections.CreateList<IPackageInfo>;
//    versions.Add(package);
//    FContext.AddVersions(package.Id, versions);
  end;
  if newPackage <> nil then
  begin
    FContext.PushRequirement(newPackage);
    FContext.RecordResolution(newPackage, nil, '');
//    versions := TCollections.CreateList<IPackageInfo>;
//    versions.Add(newPackage);
//    FContext.AddVersions(newPackage.Id, versions);
  end;
  result := true;
end;

end.
