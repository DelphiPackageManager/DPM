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
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Version;


type
  TGraphNode = class(TInterfacedObject, IGraphNode)
  private
    FParent : Pointer; //todo : use weakref??
    FChildNodes : IDictionary<string,IGraphNode>;
    FId : string;
    FVersion : TPackageVersion;
    FDependencies : IDictionary<string, IPackageDependency>;
    FState : TGraphNodeState;
    FSelectedOn : TVersionRange;
    FLevel : integer;
  protected
    function AddChildNode(const id: string; const version: TPackageVersion; const selectedOn : TVersionRange; const dependencies: IEnumerable<IPackageDependency>): IGraphNode;
    function FindNode(const id: string): IGraphNode;
    function FindChild(const id : string) : IGraphNode;
    function GetChildNodes: IEnumerable<IGraphNode>;
    function GetDependencies: IDictionary<string, IPackageDependency>;
    function GetId: string;
    function GetParent: IGraphNode;
    function GetSelectedVersion: TPackageVersion;
    function GetState: TGraphNodeState;
    function RemoveChildNode(const id: string): Boolean;
    procedure SetSelectedVersion(const value: TPackageVersion);
    procedure SetState(const value: TGraphNodeState);
    function IsRoot : boolean;
    function IsTopLevel : boolean;
    function HasChildren : boolean;
    function GetLevel: Integer;
    procedure VisitDFS(const visitor : TNodeVisitProc);

  public
    constructor Create(const parent : IGraphNode; const id : string; const version : TPackageVersion; const selectedOn : TVersionRange;  const dependencies : IEnumerable<IPackageDependency>);
    constructor CreateRoot;
  end;


implementation

uses
  System.SysUtils;

{ TGraphNode }

function TGraphNode.AddChildNode(const id: string; const version: TPackageVersion; const selectedOn : TVersionRange;  const dependencies: IEnumerable<IPackageDependency>): IGraphNode;
var
  parent : IGraphNode;
begin
  //make sure we are not doing something stupid
  if FChildNodes.ContainsKey(LowerCase(id)) then
    raise Exception.Create('Duplicate package reference ' + FId +'->' + id);

  //then  check for a cyclic dependency.
  parent := Self.GetParent;
  while parent <> nil do
  begin
    if SameText(parent.Id, id) then
      raise Exception.Create('Cycle detected ' + parent.id + '->' + id + '->' + parent.id);

    parent := parent.Parent;
  end;

  result := TGraphNode.Create(self, id, version, selectedOn, dependencies );
  FChildNodes.Add(LowerCase(id), result);
end;

constructor TGraphNode.Create(const parent: IGraphNode; const id: string; const version: TPackageVersion;  const selectedOn : TVersionRange;   const dependencies: IEnumerable<IPackageDependency>);
var
  dep : IPackageDependency;
begin
  FLevel := 0;
  if parent <> nil then
  begin
    FLevel := parent.Level + 1;
    FParent := Pointer(parent);
  end
  else
    FParent := nil;


  FId := id;
  FVersion := version;
  FSelectedOn := selectedOn;
  FDependencies := TCollections.CreateDictionary<string, IPackageDependency>;
  if dependencies <> nil then
  begin
    for dep in dependencies do
    begin
      FDependencies.Add(LowerCase(dep.Id), dep);
    end;
  end;

  FChildNodes := TCollections.CreateDictionary<string, IGraphNode>;
  FState := TGraphNodeState.Unknown;
end;

constructor TGraphNode.CreateRoot;
begin
  Create(nil,'root-node',TPackageVersion.Empty, TVersionRange.Empty,nil);
  FState := TGraphNodeState.Unknown;
end;

function TGraphNode.FindChild(const id: string): IGraphNode;
begin
  result := nil;
  FChildNodes.TryGetValue(LowerCase(id), result)
end;

function TGraphNode.FindNode(const id: string): IGraphNode;
var
  queue : IQueue<IGraphNode>;
  currentNode : IGraphNode;
  childNode : IGraphNode;
begin
  result := nil;
  queue := TCollections.CreateQueue<IGraphNode>;
  queue.Enqueue(Self);
  while queue.Any do
  begin
    currentNode := queue.Dequeue;
    if SameText(currentNode.Id, id) then
    begin
      result := currentNode;
      exit;
    end;
    for childNode in currentNode.ChildNodes do
    begin
      if SameText(currentNode.Id, id) then
      begin
        result := childNode;
        exit;
      end;
      queue.Enqueue(childNode);
    end;
  end;
end;

function TGraphNode.GetChildNodes: IEnumerable<IGraphNode>;
begin
  result := FChildNodes.Values;
end;

function TGraphNode.GetDependencies: IDictionary<string, IPackageDependency>;
begin
  result := FDependencies
end;

function TGraphNode.GetId: string;
begin
  result := FId;
end;

function TGraphNode.GetLevel: Integer;
begin
  result := FLevel;
end;

function TGraphNode.GetParent: IGraphNode;
begin
  //easier to debug this way
  if FParent <> nil then
    result := IGraphNode(FParent)
  else
    result := nil;

end;

function TGraphNode.GetSelectedVersion: TPackageVersion;
begin
  result := FVersion;
end;

function TGraphNode.GetState: TGraphNodeState;
begin
  result := FState;
end;

function TGraphNode.HasChildren: boolean;
begin
  result := FChildNodes.Any;
end;

function TGraphNode.IsRoot: boolean;
begin
  result := FParent = nil;
end;

function TGraphNode.IsTopLevel: boolean;
begin
  result := FLevel = 1;
end;

function TGraphNode.RemoveChildNode(const id: string): Boolean;
begin
  result := false;
  if FChildNodes.ContainsKey(LowerCase(id)) then
    result := FChildNodes.Remove(LowerCase(id));
end;

procedure TGraphNode.SetSelectedVersion(const value: TPackageVersion);
begin
  FVersion := value;
end;

procedure TGraphNode.SetState(const value: TGraphNodeState);
begin
  FState := value;
end;

procedure TGraphNode.VisitDFS(const visitor: TNodeVisitProc);
var
  childNode : IGraphNode;
begin
  for childNode in FChildNodes.Values do
  begin
    visitor(childNode);
  end;
  visitor(self);

end;


end.
