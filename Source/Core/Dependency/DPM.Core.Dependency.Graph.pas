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

unit DPM.Core.Dependency.Graph;

interface

uses
  System.Classes,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version;

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}

{$IF CompilerVersion >= 31.0 }
  {$DEFINE USEWEAK}
{$IFEND}

type
  TGraphNode = class(TInterfacedObject, IPackageReference)
  private
    {$IFDEF USEWEAK}
    [weak]
    FParent : IPackageReference;
    {$ELSE}
    FParent : Pointer;
    {$ENDIF}

    FDependencies : IDictionary<string, IPackageReference>;
    FId : string;
    FVersion : TPackageVersion;
    FPlatform : TDPMPlatform;
    FSelectedOn : TVersionRange;
    FUseSource : boolean;
    FSearchPaths : IList<string>;
    FLibPath : string;
    FBplPath : string;
    FCompilerVersion : TCompilerVersion;
    FProjectFile : string;

    FDesignBpls : IDictionary<TDPMPlatform, IList<string>>;
//    FDesignBplsLoaded : array[TDPMPlatform] of boolean;

  protected
    procedure RecursiveClone(const originalReference : IPackageReference; const newParent : IPackageReference);
    procedure AddExistingChild(const id : string; const packageReference : IPackageReference);
    function AddChild(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IPackageReference;
    function FindFirstChild(const id : string) : IPackageReference;
    function FindChildren(const id : string) : IList<IPackageReference>;
    function FindTopLevelChild(const id : string) : IPackageReference;
    function HasTopLevelChild(const id : string) : boolean;
    function HasAnyChild(const id : string) : boolean;
    function RemoveTopLevelchild(const id : string) : boolean;
    function RemoveChild(const packageReference : IPackageReference) : boolean;


    function GetDependencies : IEnumerable<IPackageReference>;
    function GetId : string;
    function GetParent : IPackageReference;
    function GetSelectedOn : TVersionRange;
    function GetVersion : TPackageVersion;
    function GetSearchPaths : IList<string>;
    function GetLibPath : string;
    procedure SetLibPath(const value : string);
    function GetBplPath : string;
    function GetCompilerVersion : TCompilerVersion;
    function GetIsTransitive : boolean;
    function GetProjectFile: string;
    function GetSourceName : string;
    procedure SetParent(const value : IPackageReference);
    procedure SetProjectFile(const value: string);

    procedure SetBplPath(const value : string);

    function GetPlatform : TDPMPlatform;
    procedure SetVersion(const value : TPackageVersion);
    procedure SetSelectedOn(const value : TVersionRange);
    function IsRoot : boolean;
    function HasChildren : boolean;
    procedure VisitDFS(const visitor : TNodeVisitProc);

    function AreEqual(const otherPackageReference : IPackageReference; const depth : integer = 1) : boolean;
    function GetUseSource: Boolean;
    procedure SetUseSource(const value: Boolean);
    function ToIdVersionString: string;
    function Clone : IPackageReference;

  public
    constructor Create(const parent : IPackageReference; const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const selectedOn : TVersionRange; const useSource : boolean);
    constructor CreateRoot(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform);
    destructor Destroy;override;

  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Constants;

{ TGraphNode }

procedure TGraphNode.AddExistingChild(const id: string; const packageReference: IPackageReference);
begin
  FDependencies[LowerCase(id)] := packageReference; //
end;

function TGraphNode.AddChild(const id: string; const version: TPackageVersion; const selectedOn: TVersionRange): IPackageReference;
var
  parent : IPackageReference;
begin
  //make sure we are not doing something stupid
  if FDependencies.ContainsKey(LowerCase(id)) then
    raise Exception.Create('Duplicate package reference ' + FId + '->' + id);

  //then  check for a cyclic dependency.
  parent := Self.GetParent;
  while parent <> nil do
  begin
    if SameText(parent.Id, id) then
      raise Exception.Create('Cycle detected ' + parent.id + '->' + id + '->' + parent.id);

    parent := parent.Parent;
  end;

  result := TGraphNode.Create(self, id, version, FPlatform, FCompilerVersion, selectedOn,  FUseSource);
  FDependencies.Add(LowerCase(id), result);

end;

function TGraphNode.AreEqual(const otherPackageReference: IPackageReference; const depth: integer): boolean;
var
  dependencyDepth : integer;
  res : boolean;
begin
  result := SameText(FId, otherPackageReference.Id);
  result := result and (Self.FVersion = otherPackageReference.Version);

  if (not result) or (depth = 0)  then
    exit;

  result := HasChildren = otherPackageReference.HasChildren;
  if not result then
    exit;

  dependencyDepth := depth -1;
  res := true;

  FDependencies.ForEach(
    procedure(const pair : TPair<string, IPackageReference>)
    var
      otherDependency : IPackageReference;
    begin
      if not res then
        exit;
      otherDependency := otherPackageReference.FindTopLevelChild(pair.Value.Id);
      res := otherDependency <> nil;
      if res then
        res := pair.Value.AreEqual(otherDependency, dependencyDepth);
    end);
  result := res;

end;

function TGraphNode.Clone: IPackageReference;
begin
  result := nil;
  //TODO : Recursive clone.

end;

constructor TGraphNode.Create(const parent : IPackageReference; const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const selectedOn : TVersionRange; const useSource : boolean);
begin
  FSearchPaths := TCollections.CreateList<string>;
  if parent <> nil then
  begin
    {$IFDEF USEWEAK}
    FParent := parent;
    {$ELSE}
    FParent := Pointer(parent);
    {$ENDIF}
  end
  else
    FParent := nil;

  FId := id;
  FVersion := version;
  FPlatform := platform;
  FSelectedOn := selectedOn;
  FUseSource := useSource;
  FDependencies := TCollections.CreateSortedDictionary<string, IPackageReference>();
  if FParent <> nil then
    FCompilerVersion := parent.CompilerVersion
  else
    FCompilerVersion := compilerVersion;

  FDesignBpls := TCollections.CreateDictionary<TDPMPlatform, IList<string>>;

end;

constructor TGraphNode.CreateRoot(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform);
begin
  Create(nil, cRootNode, TPackageVersion.Empty, platform, compilerVersion, TVersionRange.Empty, false);
end;

destructor TGraphNode.Destroy;
begin
  inherited;
end;

function TGraphNode.FindTopLevelChild(const id : string) : IPackageReference;
begin
  result := nil;
  FDependencies.TryGetValue(LowerCase(id), result)
end;

//non recursive breadth first search.
function TGraphNode.FindFirstChild(const id : string) : IPackageReference;
var
  queue : IQueue<IPackageReference>;
  currentNode : IPackageReference;
  dependency : IPackageReference;
begin
  result := nil;
  queue := TCollections.CreateQueue<IPackageReference>;
  queue.Enqueue(Self);
  while queue.Any do
  begin
    currentNode := queue.Dequeue;
    if SameText(currentNode.Id, id) then
    begin
      result := currentNode;
      exit;
    end;
    for dependency in currentNode.Children do
    begin
      if SameText(currentNode.Id, id) then
      begin
        result := dependency;
        exit;
      end;
      queue.Enqueue(dependency);
    end;
  end;
end;

function TGraphNode.FindChildren(const id : string) : IList<IPackageReference>;
var
  list : IList<IPackageReference>;
begin
  result := TCollections.CreateList<IPackageReference>;
  list := result;
  VisitDFS(procedure(const node : IPackageReference)
    begin
      if SameText(id, node.Id) then
        list.Add(node);
    end);
end;

function TGraphNode.GetBplPath: string;
begin
  result := FBplPath;
end;

function TGraphNode.GetDependencies : IEnumerable<IPackageReference>;
begin
  result := FDependencies.Values;
end;


function TGraphNode.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TGraphNode.GetId : string;
begin
  result := FId;
end;

function TGraphNode.GetIsTransitive: boolean;
begin
 result := (FParent <> nil) and (not {$IFDEF USEWEAK} FParent.IsRoot {$ELSE} IPackageReference(FParent).IsRoot{$ENDIF});
end;


function TGraphNode.GetLibPath: string;
begin
  result := FLibPath;
end;

function TGraphNode.GetParent : IPackageReference;
begin
  //easier to debug this way
  if FParent <> nil then
    result := {$IFDEF USEWEAK} FParent {$ELSE} IPackageReference(FParent) {$ENDIF}
  else
    result := nil;
end;

function TGraphNode.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TGraphNode.GetProjectFile: string;
begin
  if IsRoot then
    result := FProjectFile
  else if FParent <> nil then
    result :=  {$IFDEF USEWEAK} FParent.ProjectFile {$ELSE} IPackageReference(FParent).ProjectFile{$ENDIF}
  else
    result := '';
end;

function TGraphNode.GetSearchPaths: IList<string>;
begin
  result := FSearchPaths;
end;

function TGraphNode.GetSelectedOn : TVersionRange;
begin
  result := FSelectedOn;
end;

function TGraphNode.GetSourceName: string;
begin
  result := '';
end;

function TGraphNode.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;

function TGraphNode.GetUseSource: Boolean;
var
  parent : IPackageReference;
begin
  parent := GetParent;
  //if the parent is using the source then we should too.
  result := FUseSource or ((parent <> nil) and parent.UseSource);
end;

function TGraphNode.HasAnyChild(const id: string): boolean;
var
  packageRef : IPackageReference;
begin
  packageRef := FindFirstChild(id);
  result := packageRef <> nil;
end;

function TGraphNode.HasChildren : boolean;
begin
  result := FDependencies.Any;
end;

function TGraphNode.HasTopLevelChild(const id: string): boolean;
begin
  result := FDependencies.ContainsKey(LowerCase(id));
end;

function TGraphNode.IsRoot : boolean;
begin
  result := FId = cRootNode;
end;

procedure TGraphNode.RecursiveClone(const originalReference, newParent: IPackageReference);
var
  newChild : IPackageReference;
begin
  originalReference.Children.ForEach(
    procedure(const oldChild : IPackageReference)
    begin
      newChild := oldChild.Clone;
      newParent.AddExistingChild(newChild.Id, newChild);
    end);
end;

function TGraphNode.RemoveTopLevelChild(const id : string) : boolean;
begin
  result := FDependencies.ContainsKey(LowerCase(id));
  if result then
    FDependencies.Remove(LowerCase(id));
end;

function TGraphNode.RemoveChild(const packageReference : IPackageReference) : boolean;
var
  dependency : IPackageReference;
begin
  result := FDependencies.ContainsValue(packageReference);
  if result then
    FDependencies.Remove(LowerCase(packageReference.Id))
  else
    for dependency in FDependencies.Values do
    begin
      result := dependency.RemoveChild(packageReference);
      if result then
        exit;
    end;
end;

procedure TGraphNode.SetBplPath(const value: string);
begin
  FBplPath := value;
end;

procedure TGraphNode.SetLibPath(const value: string);
begin
  FLibPath := value;
end;

procedure TGraphNode.SetParent(const value: IPackageReference);
begin
  {$IFDEF USEWEAK}
  FParent := value;
  {$ELSE}
  FParent := Pointer(value);
  {$ENDIF}
end;

procedure TGraphNode.SetProjectFile(const value: string);
begin
  if IsRoot then
    FProjectFile := value;
end;

procedure TGraphNode.SetSelectedOn(const value : TVersionRange);
begin
  FSelectedOn := value;
end;

procedure TGraphNode.SetVersion(const value : TPackageVersion);
begin
  FVersion := value;
end;

procedure TGraphNode.SetUseSource(const value: Boolean);
begin
  FUseSource := value;
end;

function TGraphNode.ToIdVersionString: string;
begin
  result := FId +' [' + FVersion.ToStringNoMeta + ']';
end;


procedure TGraphNode.VisitDFS(const visitor : TNodeVisitProc);
var
  dependency : IPackageReference;
begin
  for dependency in FDependencies.Values do
    dependency.VisitDFS(visitor);
  //don't visit the root node as it's just a container
  if not self.IsRoot then
    visitor(self);
end;


end.


