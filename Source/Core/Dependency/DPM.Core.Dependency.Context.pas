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
  DPM.Core.Package.Interfaces;

//only intended for internal use by the resolver so interfaces can stay here
type
  IResolverContext = interface
    ['{B97E7843-4C13-490A-A776-DFAC1BC60A0D}']
    procedure RecordNoGood(const bad : IPackageInfo);
    function IsNoGood(const package : IPackageInfo) : boolean;
    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
    function TryGetResolution(const packageId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddPackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemovePackageVersion(const packageId : string; const version : IPackageInfo);
    function GetResolvedPackages : IList<IPackageInfo>;
    function BuildDependencyGraph : IGraphNode;
  end;

  TResolverContext = class(TInterfacedObject, IResolverContext)
  private
    FLogger : ILogger;
    FNoGoods : IDictionary<string, IDictionary<TPackageVersion, byte>>;
    FResolved : IDictionary<string, IResolution>;
    FOpenRequirements : IQueue<IPackageInfo>;
    FVersionCache : IDictionary<string, IList<IPackageInfo>>;
    FPlatform : TDPMPlatform;
  protected
    procedure RecordNoGood(const bad : IPackageInfo);
    function IsNoGood(const package : IPackageInfo) : boolean;

    procedure RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
    function TryGetResolution(const packageId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpenRequrements : boolean;
    function GetPackageVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddPackageVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemovePackageVersion(const packageId : string; const version : IPackageInfo);
    function GetResolvedPackages : IList<IPackageInfo>;
    function BuildDependencyGraph : IGraphNode;
  public
    constructor Create(const logger : ILogger; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>);overload;
    constructor Create(const logger : ILogger; const platform : TDPMPlatform; const projectReferences : IList<TProjectReference>);overload;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Dependency.Graph,
  DPM.Core.Dependency.Resolution;

const
  cRoot = 'root';


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

function TResolverContext.BuildDependencyGraph : IGraphNode;
var
  toplevelPackages : IEnumerable<IResolution>;
  topLevelPackage : IResolution;

  procedure AddNode(const parentNode : IGraphNode; const package : IPackageInfo; const versionRange : TVersionRange);
  var
    resolution : IResolution;
    dependency : IPackageDependency;
    childNode : IGraphNode;
  begin
    childNode := parentNode.AddChildNode(package.Id, package.Version, versionRange);
    for dependency in package.Dependencies do
    begin
      if not TryGetResolution(dependency.Id, resolution) then
        raise Exception.Create('Didn''t find a resolution for package [' + dependency.id + ']');
      AddNode(childNode, resolution.Package, resolution.VersionRange);
    end;
  end;

begin
  result := TGraphNode.CreateRoot(FPlatform);
  toplevelPackages := FResolved.Values.Where(function(const value : IResolution) : boolean
    begin
      result := value.ParentId = cRoot;
    end);

  for toplevelPackage in toplevelPackages do
    AddNode(result, topLevelPackage.Package, TVersionRange.Empty);
end;

constructor TResolverContext.Create(const logger: ILogger; const platform: TDPMPlatform; const projectReferences: IList<TProjectReference>);
var
  projectReference : TProjectReference;
begin
  FPlatform := platform;
  FLogger := logger;
  FNoGoods := TCollections.CreateDictionary < string, IDictionary<TPackageVersion, byte> > ;
  FResolved := TCollections.CreateDictionary < string, IResolution > ;
  FOpenRequirements := TCollections.CreateQueue<IPackageInfo>;
  FVersionCache := TCollections.CreateDictionary < string, IList<IPackageInfo> > ;

  for projectReference in projectReferences do
  begin
    //don't add to the list of packages to resolve if it has no dependencies..
    if projectReference.Package.Dependencies.Any then
      PushRequirement(projectReference.Package);

    RecordResolution(projectReference.Package, projectReference.VersionRange, projectReference.ParentId);
  end;

end;

constructor TResolverContext.Create(const logger : ILogger; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>);
begin
  Assert(newPackage <> nil);
  Create(logger,newPackage.Platform, projectReferences);
  PushRequirement(newPackage);
  RecordResolution(newPackage, TVersionRange.Empty, cRoot);
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
  dict : IDictionary<TPackageVersion, byte>;
begin
  result := false;
  if FNoGoods.TryGetValue(LowerCase(package.Id), dict) then
  begin
    if dict.ContainsKey(package.Version) then
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

procedure TResolverContext.RecordNoGood(const bad : IPackageInfo);
var
  dict : IDictionary<TPackageVersion, byte>;
begin
  if not FNoGoods.TryGetValue(LowerCase(bad.Id), dict) then
  begin
    dict := TCollections.CreateDictionary<TPackageVersion, byte>;
    FNoGoods.Add(LowerCase(bad.Id), dict);
  end;
  dict.Add(bad.Version, 0);
end;

procedure TResolverContext.RecordResolution(const package : IPackageInfo; const versionRange : TVersionRange; const parentId : string);
var
  resolution : IResolution;
begin
  if FResolved.ContainsKey(LowerCase(package.Id)) then
    raise Exception.Create('Resolution already exists for package [' + package.Id + ']');
  resolution := TResolution.Create(package, versionRange, parentId);
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

function TResolverContext.TryGetResolution(const packageId : string; out resolution : IResolution) : boolean;
begin
  result := FResolved.TryGetValue(LowerCase(packageId), resolution);
end;

end.


