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
  TGraphNode = class(TInterfacedObject, IGraphNode)
  private
    //todo : in 10.1+ use weakref
    {$IFDEF USEWEAK}
    [weak]
    FParent : IGraphNode;
    {$ELSE}
    FParent : Pointer;
    {$ENDIF}

    FChildNodes : IDictionary<string, IGraphNode>;
    FId : string;
    FVersion : TPackageVersion;
    FPlatform : TDPMPlatform;
    FSelectedOn : TVersionRange;
    FUseSource : boolean;
    FLevel : integer;
    FSearchPaths : IList<string>;
    FLibPath : string;
    FBplPath : string;

  protected
    function AddChildNode(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IGraphNode;
    function FindFirstNode(const id : string) : IGraphNode;
    function FindNodes(const id : string) : IList<IGraphNode>;
    function FindChild(const id : string) : IGraphNode;
    function GetChildNodes : IEnumerable<IGraphNode>;
    function GetId : string;
    function GetParent : IGraphNode;
    function GetSelectedOn : TVersionRange;
    function GetSelectedVersion : TPackageVersion;
    function GetSearchPaths : IList<string>;
    function GetLibPath : string;
    procedure SetLibPath(const value : string);
    function GetBplPath : string;
    procedure SetBplPath(const value : string);

    function GetPlatform : TDPMPlatform;
    procedure SetSelectedVersion(const value : TPackageVersion);
    procedure SetSelectedOn(const value : TVersionRange);
    function RemoveNode(const node : IGraphNode) : boolean;
    function IsRoot : boolean;
    function IsTopLevel : boolean;
    function HasChildren : boolean;
    function GetLevel : Integer;
    procedure VisitDFS(const visitor : TNodeVisitProc);
    procedure Prune(const id : string);
    function AreEqual(const otherNode : IGraphNode; const depth : integer = 1) : boolean;
    function GetUseSource: Boolean;
    procedure SetUseSource(const value: Boolean);
  public
    constructor Create(const parent : IGraphNode; const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const selectedOn : TVersionRange; const useSource : boolean);
    constructor CreateRoot(const platform : TDPMPlatform);
    destructor Destroy;override;


  end;


implementation

uses
  System.SysUtils;

{ TGraphNode }

function TGraphNode.AddChildNode(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IGraphNode;
var
  parent : IGraphNode;
begin
  //make sure we are not doing something stupid
  if FChildNodes.ContainsKey(LowerCase(id)) then
    raise Exception.Create('Duplicate package reference ' + FId + '->' + id);

  //then  check for a cyclic dependency.
  parent := Self.GetParent;
  while parent <> nil do
  begin
    if SameText(parent.Id, id) then
      raise Exception.Create('Cycle detected ' + parent.id + '->' + id + '->' + parent.id);

    parent := parent.Parent;
  end;

  result := TGraphNode.Create(self, id, version, FPlatform, selectedOn, false);
  FChildNodes.Add(LowerCase(id), result);
end;

function TGraphNode.AreEqual(const otherNode: IGraphNode; const depth: integer): boolean;
var
  childDepth : integer;
  res : boolean;
begin
  result := SameText(FId, otherNode.Id);
  result := result and (Self.FVersion = otherNode.SelectedVersion);
  
  if (not result) or (depth = 0)  then
    exit;

  result := HasChildren = otherNode.HasChildren;
  if not result then
    exit;

  childDepth := depth -1;
  res := true;

  FChildNodes.ForEach(
    procedure(const pair : TPair<string, IGraphNode>)
    var
      otherChildNode : IGraphNode;
    begin
      if not res then
        exit;
      otherChildNode := otherNode.FindChild(pair.Value.Id);
      res := otherChildNode <> nil;
      if res then
        res := pair.Value.AreEqual(otherChildNode, childDepth);
    end);
  result := res;

end;

constructor TGraphNode.Create(const parent : IGraphNode; const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const selectedOn : TVersionRange; const useSource : boolean);
begin
  FSearchPaths := TCollections.CreateList<string>;
  FLevel := 0;
  if parent <> nil then
  begin
    FLevel := parent.Level + 1;
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

  FChildNodes := TCollections.CreateSortedDictionary < string, IGraphNode > ();
end;

constructor TGraphNode.CreateRoot(const platform : TDPMPlatform);
begin
  Create(nil, 'root', TPackageVersion.Empty, platform, TVersionRange.Empty, false);
end;

destructor TGraphNode.Destroy;
begin
  inherited;
end;

function TGraphNode.FindChild(const id : string) : IGraphNode;
begin
  result := nil;
  FChildNodes.TryGetValue(LowerCase(id), result)
end;

//non recursive breadth first search.
function TGraphNode.FindFirstNode(const id : string) : IGraphNode;
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

function TGraphNode.FindNodes(const id : string) : IList<IGraphNode>;
var
  list : IList<IGraphNode>;
begin
  result := TCollections.CreateList<IGraphNode>;
  list := result;
  VisitDFS(procedure(const node : IGraphNode)
    begin
      if SameText(id, node.Id) then
        list.Add(node);
    end);
end;

function TGraphNode.GetBplPath: string;
begin
  result := FBplPath;
end;

function TGraphNode.GetChildNodes : IEnumerable<IGraphNode>;
begin
  result := FChildNodes.Values;
end;


function TGraphNode.GetId : string;
begin
  result := FId;
end;

function TGraphNode.GetLevel : Integer;
begin
  result := FLevel;
end;

function TGraphNode.GetLibPath: string;
begin
  result := FLibPath;
end;

function TGraphNode.GetParent : IGraphNode;
begin
  //easier to debug this way
  if FParent <> nil then
    result := {$IFDEF USEWEAK} FParent {$ELSE} IGraphNode(FParent) {$ENDIF}
  else
    result := nil;
end;

function TGraphNode.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TGraphNode.GetSearchPaths: IList<string>;
begin
  result := FSearchPaths;
end;

function TGraphNode.GetSelectedOn : TVersionRange;
begin
  result := FSelectedOn;
end;

function TGraphNode.GetSelectedVersion : TPackageVersion;
begin
  result := FVersion;
end;

function TGraphNode.GetUseSource: Boolean;
begin
  result := FUseSource;
end;

function TGraphNode.HasChildren : boolean;
begin
  result := FChildNodes.Any;
end;

function TGraphNode.IsRoot : boolean;
begin
  result := FParent = nil;
end;

function TGraphNode.IsTopLevel : boolean;
begin
  result := FLevel = 1;
end;

procedure TGraphNode.Prune(const id : string);
var
  childNode : IGraphNode;
begin
  if FChildNodes.ContainsKey(LowerCase(id)) then
    FChildNodes.Remove(LowerCase(id))
  else
  begin
    for childNode in FChildNodes.Values do
      childNode.Prune(id);
  end;
end;

function TGraphNode.RemoveNode(const node : IGraphNode) : boolean;
var
  childNode : IGraphNode;
begin
  result := FChildNodes.ContainsValue(node);
  if result then
    FChildNodes.Remove(LowerCase(node.Id))
  else
    for childNode in FChildNodes.Values do
    begin
      result := childNode.RemoveNode(node);
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

procedure TGraphNode.SetSelectedOn(const value : TVersionRange);
begin
  FSelectedOn := value;
end;

procedure TGraphNode.SetSelectedVersion(const value : TPackageVersion);
begin
  FVersion := value;
end;

procedure TGraphNode.SetUseSource(const value: Boolean);
begin
  FUseSource := value;
end;

procedure TGraphNode.VisitDFS(const visitor : TNodeVisitProc);
var
  childNode : IGraphNode;
begin
  for childNode in FChildNodes.Values do
    childNode.VisitDFS(visitor);
  //don't visit the root node as it's just a container
  if not self.IsRoot then
    visitor(self);
end;


end.


