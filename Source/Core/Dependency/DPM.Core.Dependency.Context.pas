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
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Interfaces;

 //only intended for internal use by the resolver so interfaces can stay here
type
  IResolverContext = interface
  ['{B97E7843-4C13-490A-A776-DFAC1BC60A0D}']
    procedure RecordBadChoice(const bad : IPackageInfo);
    function IsBad(const package : IPackageInfo) : boolean;
    procedure RecordResolution(const package : IPackageInfo; const dependency : IPackageDependency; const parentId : string);
    function TryGetResolution(const packageId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpen : boolean;
    function GetVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemoveVersion(const packageId : string; const version : IPackageInfo);
    procedure Reset;
    function GetResolvedPackages : IList<IPackageInfo>;
  end;

  TResolverContext = class(TInterfacedObject, IResolverContext)
  private
    FLogger : ILogger;
    FBadChoices : IDictionary<string, IDictionary<TPackageVersion, byte>>;
    FResolved : IDictionary<string, IResolution>;
    FOpenRequirements : IQueue<IPackageInfo>;
    FVersionCache : IDictionary<string,IList<IPackageInfo>>;

  protected
    procedure RecordBadChoice(const bad : IPackageInfo);
    function IsBad(const package : IPackageInfo) : boolean;
    procedure RecordResolution(const package : IPackageInfo; const dependency : IPackageDependency; const parentId : string);
    function TryGetResolution(const packageId : string; out resolution : IResolution) : boolean;
    procedure RemoveResolution(const packageId : string);
    procedure PushRequirement(const package : IPackageInfo);
    function PopRequirement : IPackageInfo;
    function GetResolutions : TArray<IResolution>;
    function AnyOpen : boolean;
    function GetVersions(const packageId : string) : IList<IPackageInfo>;
    procedure AddVersions(const packageId : string; const versions : IList<IPackageInfo>);
    procedure RemoveVersion(const packageId : string; const version : IPackageInfo);
    procedure Reset;
    function GetResolvedPackages : IList<IPackageInfo>;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Dependency.Resolution;

{ TResolverContext }

procedure TResolverContext.AddVersions(const packageId: string; const versions: IList<IPackageInfo>);
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

function TResolverContext.AnyOpen: boolean;
begin
  result := FOpenRequirements.Any;
end;

constructor TResolverContext.Create(const logger: ILogger);
begin
  FLogger := logger;
  FBadChoices := TCollections.CreateDictionary<string, IDictionary<TPackageVersion, byte>>;
  FResolved := TCollections.CreateDictionary<string, IResolution>;
  FOpenRequirements := TCollections.CreateQueue<IPackageInfo>;
  FVersionCache := TCollections.CreateDictionary<string,IList<IPackageInfo>>;
end;

procedure TResolverContext.PushRequirement(const package: IPackageInfo);
begin
  FOpenRequirements.Enqueue(package);
end;

function TResolverContext.GetResolutions: TArray<IResolution>;
begin
  result := FResolved.Values.ToArray;
end;

function TResolverContext.GetResolvedPackages: IList<IPackageInfo>;
var
  resolution : IResolution;
begin
  result := TCollections.CreateList<IPackageInfo>;
  for resolution in FResolved.Values do
    result.Add(resolution.Package);
end;

function TResolverContext.GetVersions(const packageId: string): IList<IPackageInfo>;
begin
  result := nil;
  FVersionCache.TryGetValue(LowerCase(packageId), result);
end;

function TResolverContext.IsBad(const package: IPackageInfo): boolean;
var
  dict : IDictionary<TPackageVersion, byte>;
begin
  result := false;
  if FBadChoices.TryGetValue(LowerCase(package.Id), dict) then
  begin
    if dict.ContainsKey(package.Version) then
      result := true;
  end;
end;

function TResolverContext.PopRequirement: IPackageInfo;
begin
  if FOpenRequirements.Any then
    result := FOpenRequirements.Dequeue
  else
    result := nil;
end;

procedure TResolverContext.RecordBadChoice(const bad: IPackageInfo);
var
  dict : IDictionary<TPackageVersion, byte>;
begin
  if not FBadChoices.TryGetValue(LowerCase(bad.Id), dict) then
  begin
    dict := TCollections.CreateDictionary<TPackageVersion, byte>;
    FBadChoices.Add(LowerCase(bad.Id), dict);
  end;
  dict.Add(bad.Version, 0);
end;

procedure TResolverContext.RecordResolution(const package: IPackageInfo; const dependency: IPackageDependency; const parentId: string);
var
  resolution : IResolution;
begin
  if FResolved.ContainsKey(LowerCase(package.Id)) then
    raise Exception.Create('Resolution already exists for package [' + package.Id + ']');
  resolution := TResolution.Create(package, dependency, parentId);
  FResolved.Add(LowerCase(package.Id), resolution);
end;

procedure TResolverContext.RemoveResolution(const packageId: string);
begin
  if FResolved.ContainsKey(LowerCase(packageId)) then
    FResolved.Remove(LowerCase(packageId));
end;

procedure TResolverContext.RemoveVersion(const packageId: string; const version: IPackageInfo);
var
  list :IList<IPackageInfo>;
begin
  if FVersionCache.TryGetValue(LowerCase(packageId), list) then
  begin
    list.Remove(version)
  end;
end;

procedure TResolverContext.Reset;
begin
  FBadChoices.Clear;
  FResolved.Clear;
  FOpenRequirements.Clear;
  FVersionCache.Clear;
end;

function TResolverContext.TryGetResolution(const packageId: string; out resolution: IResolution): boolean;
begin
  result := FResolved.TryGetValue(LowerCase(packageId), resolution);
end;

end.
