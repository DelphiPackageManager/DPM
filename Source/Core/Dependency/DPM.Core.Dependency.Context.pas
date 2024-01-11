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
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces;

//only intended for internal use by the resolver so interfaces can stay here
type
  IResolverContext = interface
    ['{B97E7843-4C13-490A-A776-DFAC1BC60A0D}']
    function RecordNoGood(const bad : IPackageInfo) : boolean;
    function IsNoGood(const package : IPackageInfo) : boolean;
    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
    function TryGetResolution(const packageId : string; const parentId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddPackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemovePackageVersion(const packageId : string; const version : IPackageInfo);
    function GetResolvedPackages : IList<IPackageInfo>;
    function BuildDependencyGraph : IPackageReference;
    function ProjectFile : string;
  end;

  TResolverContext = class(TInterfacedObject, IResolverContext)
  private
    FLogger : ILogger;
    FNoGoods : IDictionary<string, ISet<TPackageVersion>>;
    FResolved : IDictionary<string, IResolution>;
    FOpenRequirements : IQueue<IPackageInfo>;
    FVersionCache : IDictionary<string, IList<IPackageInfo>>;
    FPlatform : TDPMPlatform;
    FCompilerVersion : TCompilerVersion;
    FProjectFile : string;
    FPackageInstallerContext : IPackageInstallerContext;
  protected
    function RecordNoGood(const bad : IPackageInfo) : boolean;
    function IsNoGood(const package : IPackageInfo) : boolean;

    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
    function TryGetResolution(const packageId : string; const parentId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddPackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemovePackageVersion(const packageId : string; const version : IPackageInfo);
    function GetResolvedPackages : IList<IPackageInfo>;
    function BuildDependencyGraph : IPackageReference;
    function ProjectFile : string;
  public
    constructor Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>);overload;
    constructor Create(const logger : ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectReferences : IList<TProjectReference>);overload;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Constants,
  DPM.Core.Dependency.Graph,
  DPM.Core.Dependency.Resolution;

  { TResolverContext }

procedure TResolverContext.AddPackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
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
  toplevelPackages : TArray<IResolution>;
  topLevelPackage : IResolution;

  procedure AddNode(const parent : IPackageReference; const package : IPackageInfo; const versionRange : TVersionRange);
  var
    resolution : IResolution;
    dependency : IPackageDependency;
    dependencyReference : IPackageReference;
  begin
    dependencyReference := parent.AddPackageDependency(package.Id, package.Version, versionRange);
    dependencyReference.UseSource := package.UseSource;
    for dependency in package.Dependencies do
    begin
      if not TryGetResolution(dependency.Id, parent.Id, resolution) then
        raise Exception.Create('Didn''t find a resolution for package [' + dependency.id + ']');
      AddNode(dependencyReference, resolution.Package, resolution.VersionRange);
    end;
  end;

begin
  result := TPackageReference.CreateRoot(FCompilerVersion, FPlatform);
  toplevelPackages := FResolved.Values.Where(function(const value : IResolution) : boolean
    begin
      result := value.ParentId = cRootNode;
    end).ToArray;

  for toplevelPackage in toplevelPackages do
    AddNode(result, topLevelPackage.Package, TVersionRange.Empty);
end;

constructor TResolverContext.Create(const logger: ILogger; const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const compilerVersion : TCompilerVersion; const platform: TDPMPlatform; const projectReferences: IList<TProjectReference>);
var
  projectReference : TProjectReference;
begin
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FLogger := logger;
  FPackageInstallerContext := packageInstallerContext;
  FProjectFile := projectFile;
  FNoGoods := TCollections.CreateDictionary<string, ISet<TPackageVersion>>;
  FResolved := TCollections.CreateDictionary<string, IResolution>;
  FOpenRequirements := TCollections.CreateQueue<IPackageInfo>;
  FVersionCache := TCollections.CreateDictionary<string, IList<IPackageInfo>>;

  for projectReference in projectReferences do
  begin
    //don't add to the list of packages to resolve if it has no dependencies..
    if projectReference.Package.Dependencies.Any then
      PushRequirement(projectReference.Package);

    RecordResolution(projectReference.Package, projectReference.VersionRange, projectReference.ParentId);
  end;

end;

constructor TResolverContext.Create(const logger : ILogger;  const packageInstallerContext : IPackageInstallerContext; const projectFile : string; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>);
begin
  Assert(newPackage <> nil);
  Create(logger, packageInstallerContext, projectFile, newPackage.CompilerVersion, newPackage.Platform, projectReferences);
  PushRequirement(newPackage);
  RecordResolution(newPackage, TVersionRange.Create(newPackage.Version), cRootNode);
end;


procedure TResolverContext.PushRequirement(const package : IPackageInfo);
begin
  FOpenRequirements.Enqueue(package);
end;

function TResolverContext.GetResolutions : TArray<IResolution>;
begin
  result := FResolved.Values.ToArray;
end;

function TResolverContext.GetResolvedPackages : IList<IPackageInfo>;
var
  resolution : IResolution;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for resolution in FResolved.Values do
    result.Add(resolution.Package);
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

function TResolverContext.RecordNoGood(const bad : IPackageInfo) : boolean;
var
  nogoods : ISet<TPackageVersion>;
begin
  if not FNoGoods.TryGetValue(LowerCase(bad.Id), nogoods) then
  begin
    nogoods := TCollections.CreateSet<TPackageVersion>;
    FNoGoods.Add(LowerCase(bad.Id), nogoods);
  end;
  result := nogoods.Contains(bad.Version);
  if not result then
    nogoods.add(bad.Version);
end;

procedure TResolverContext.RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
var
  resolution : IResolution;
begin
  if FResolved.ContainsKey(LowerCase(package.Id)) then
    raise Exception.Create('Resolution already exists for package [' + package.Id + ']');
  resolution := TResolution.Create(package, versionRange, parentId, FProjectFile);
  FResolved.Add(LowerCase(package.Id), resolution);
end;

procedure TResolverContext.RemoveResolution(const packageId : string);
begin
  if FResolved.ContainsKey(LowerCase(packageId)) then
    FResolved.Remove(LowerCase(packageId));
end;

procedure TResolverContext.RemovePackageVersion(const packageId : string; const version : IPackageInfo);
var
  list : IList<IPackageInfo>;
begin
  if FVersionCache.TryGetValue(LowerCase(packageId), list) then
  begin
    list.Remove(version)
  end;
end;

function TResolverContext.TryGetResolution(const packageId : string; const parentId : string; out resolution : IResolution) : boolean;

  procedure CopyDependencies(const parent : string; const res : IResolution);
  var
    i: Integer;
    id : string;
    childRes : IResolution;
  begin
    for i := 0 to res.package.Dependencies.Count -1 do
    begin
      id := res.package.Dependencies[i].Id;
      childRes := FPackageInstallerContext.FindPackageResolution(FProjectFile,  FPlatform, id);
      if childRes <> nil then
      begin
        FResolved[Lowercase(id)] := childRes;//.Clone(FProjectFile); //should we be cloning here?
        if childRes.Package.Dependencies.Any then
          CopyDependencies(parent,  childRes);
      end;
    end;
  end;

begin
  result := FResolved.TryGetValue(LowerCase(packageId), resolution);
  if not result then
  begin
    //check if it was resolved in another project in the group
    resolution := FPackageInstallerContext.FindPackageResolution(FProjectFile, FPlatform, packageId);
    result := resolution <> nil;
    if resolution <> nil then
    begin
      FResolved[Lowercase(packageId)] := resolution.Clone(parentId);
      //if we resolved via another projectr in the group, then we need to bring along the resolved dependencies too
      if resolution.Package.Dependencies.Any then
        CopyDependencies(resolution.Package.Id, resolution);
    end;
  end;
end;

end.


