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


unit DPM.IDE.VSTProxy;

//The tools api provides no access to the the virtualstringtree type so hacking using RTTI

interface

uses
  System.Rtti,
  Vcl.Controls,
  Vcl.ImgList,
  Spring.Collections,
  DPM.IDE.Logger,
  DPM.IDE.ProjectTree.Containers;


type
  TNodeData = record
    //many hours of debugging later, seems they point to the same object which implements multiple interfaces
    GraphLocation : IGraphLocation;
    GraphData : IGraphData;
    Dummy : array[0..11] of Byte; //seem to be only used for target platforms children, which we don't need.
  end;

  PNodeData = ^TNodeData;

  PVirtualNode = type Pointer; //just to make it feel more like working with VST :)

  //A way of working on a VST when we can't use the class itself
  //fortunately VST has a well known interface that is pretty easy
  //to call via RTTI

  // copied from VirtualTrees.pas
  // TODO : check where anything has changed with this since the time of XE2

  // mode to describe a move action
  TVTNodeAttachMode = (
    amNoWhere, // just for simplified tests, means to ignore the Add/Insert command
    amInsertBefore, // insert node just before destination (as sibling of destination)
    amInsertAfter, // insert node just after destionation (as sibling of destination)
    amAddChildFirst, // add node as first child of destination
    amAddChildLast // add node as last child of destination
    );


  //quickndirty
  TRttiMethodEntry = class
  public
    rttiMethod : TRttiMethod;
    params : TArray<TRttiParameter>;
  end;

  TRttiPropEntry = class
  public
    rttiProp : TRttiProperty
  end;


  TVirtualStringTreeProxy = class
  private
    FTreeInstance : TControl;
    FRttiCtx : TRttiContext;
    FTreeType : TRttiType;
    FLogger : IDPMIDELogger;
    FMethodCache : IDictionary<string, TRttiMethodEntry>;
    FPropCache : IDictionary<string, TRttiPropEntry>;
    FExpandedRttiprop : TRttiIndexedProperty;
  protected
    function GetVSTProperty(const propertyName : string) : TValue;

    procedure SetVSTProperty(const propertyName : string; const value : TValue);

    //most VST methods take a node pointer so this is just a shortcut
    function InvokeMethodWithPointerParam(const methodName : string; const value : Pointer) : TValue;

    function GetImages : TCustomImageList;

    function GetMethodEntry(const name : string) : TRttiMethodEntry;
    function GetPropEntry(const name : string) : TRttiPropEntry;

  public
    constructor Create(const treeInstance : TControl; const logger : IDPMIDELogger);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNodeDataSize : integer;
    function GetRootNode : PVirtualNode;
    function GetFirstVisibleNode : PVirtualNode;
    function GetFirstChild(node : PVirtualNode) : PVirtualNode;
    function GetFirstNode : PVirtualNode;
    function GetNextNode(const node : PVirtualNode) : PVirtualNode;
    function GetNextSibling(const node : PVirtualNode) : PVirtualNode;
    function GetNextVisible(const node : PVirtualNode) : PVirtualNode;
    function GetNodeData(const node : PVirtualNode) : PNodeData;
    function AddChildNode(const parentNode : PVirtualNode) : PVirtualNode;
    function InsertNode(const parentNode : PVirtualNode; const attachMode : TVTNodeAttachMode) : PVirtualNode;
    procedure DeleteChildren(const parentNode : PVirtualNode);
    procedure SetExpanded(const node : PVirtualNode; const value : boolean);
    property Images : TCustomImageList read GetImages;
  end;

implementation

uses
  System.SysUtils;

{ TVirtualStringTreeHack }

function TVirtualStringTreeProxy.AddChildNode(const parentNode : PVirtualNode) : PVirtualNode;
var
  rttiMethodEntry : TRttiMethodEntry;
  param : TValue;
begin
  rttiMethodEntry := GetMethodEntry('AddChild');

  TValue.Make(@parentNode, rttiMethodEntry.params[0].ParamType.Handle, param);
  // function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; overload; virtual;
  //calling with just the parent fails.. seems like with rtti invoke optional params must be supplied?
  Result := PVirtualNode(PPointer(rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, [param, TValue.From<Pointer>(nil)]).GetReferenceToRawData())^);

end;

procedure TVirtualStringTreeProxy.BeginUpdate;
var
  rttiMethodEntry : TRttiMethodEntry;
begin
  rttiMethodEntry := GetMethodEntry('BeginUpdate');
  rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, []);
end;

constructor TVirtualStringTreeProxy.Create(const treeInstance : TControl; const logger : IDPMIDELogger);
begin
  FTreeInstance := treeInstance;
  FLogger := logger;
  FTreeType := FRttiCtx.GetType(FTreeInstance.ClassType).AsInstance;
  FMethodCache := TCollections.CreateDictionary<string, TRttiMethodEntry>;
  FPropCache := TCollections.CreateDictionary<string, TRttiPropEntry>;
  FExpandedRttiprop := nil;
end;

procedure TVirtualStringTreeProxy.DeleteChildren(const parentNode : PVirtualNode);
var
  rttiMethodEntry : TRttiMethodEntry;
  param : TValue;
begin
  rttiMethodEntry := GetMethodEntry('DeleteChildren');
  TValue.Make(@parentNode, rttiMethodEntry.params[0].ParamType.Handle, param);
  rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, [param, true]);
end;

destructor TVirtualStringTreeProxy.Destroy;
var
  mEntry : TRttiMethodEntry;
  propEntry : TRttiPropEntry;
begin
  for mEntry in FMethodCache.Values do
    mEntry.Free;

  for propEntry in FPropCache.Values do
    propEntry.Free;

  inherited;
end;

procedure TVirtualStringTreeProxy.EndUpdate;
var
  rttiMethodEntry : TRttiMethodEntry;
begin
  rttiMethodEntry := GetMethodEntry('EndUpdate');
  rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, []);
end;

function TVirtualStringTreeProxy.GetFirstChild(node : PVirtualNode) : PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetFirstChild', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetFirstNode : PVirtualNode;
var
  rttiMethodEntry : TRttiMethodEntry;
  res : TValue;
begin
  rttiMethodEntry := GetMethodEntry('GetFirst');
  //no params so easy
  res := rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, []);
  Result := PVirtualNode(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetFirstVisibleNode : PVirtualNode;
var
  rttiMethodEntry : TRttiMethodEntry;
  res : TValue;
begin
  rttiMethodEntry := GetMethodEntry('GetFirstVisible');
  //no params so easy
  res := rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, []);
  Result := PVirtualNode(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetImages : TCustomImageList;
begin
  Result := GetVSTProperty('Images').AsType<TCustomImageList>();
end;

function TVirtualStringTreeProxy.GetMethodEntry(const name: string): TRttiMethodEntry;
begin
  if not FMethodCache.TryGetValue(LowerCase(name), result) then
  begin
    result := TRttiMethodEntry.Create;
    result.rttiMethod := FTreeType.GetMethod(name);
    if not Assigned(result.rttiMethod) then
      raise Exception.Create('RTTI for method [' + name + '] not found.');
    result.params := result.rttiMethod.GetParameters;
    FMethodCache[LowerCase(name)] := result;
  end;
end;

function TVirtualStringTreeProxy.GetNextNode(const node : PVirtualNode) : PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNext', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNextSibling(const node : PVirtualNode) : PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNextSibling', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNextVisible(const node : PVirtualNode) : PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNextVisible', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNodeData(const node : PVirtualNode) : PNodeData;
var
  res : TValue;
begin
  res := InvokeMethodWithPointerParam('GetNodeData', node);
  Result := PNodeData(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNodeDataSize : integer;
begin
  Result := GetVSTProperty('NodeDataSize').AsType<integer>;
end;

function TVirtualStringTreeProxy.GetPropEntry(const name: string): TRttiPropEntry;
begin
  if not FPropCache.TryGetValue(LowerCase(name), result) then
  begin
    result := TRttiPropEntry.Create;
    result.rttiProp := FTreeType.GetProperty(name);
    if not Assigned(result.rttiProp) then
      raise Exception.Create('RTTI for property [' + name + '] not found.');
    FPropCache[LowerCase(name)] := result;
  end;
end;

function TVirtualStringTreeProxy.GetRootNode : PVirtualNode;
begin
  Result := PVirtualNode(PPointer(GetVSTProperty('RootNode').GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetVSTProperty(const propertyName : string) : TValue;
var
  rttiPropEntry :  TRttiPropEntry;
begin
  rttiPropEntry := GetPropEntry(propertyName);
  Result := rttiPropEntry.rttiProp.GetValue(FTreeInstance)
end;

function TVirtualStringTreeProxy.InsertNode(const parentNode : PVirtualNode; const attachMode : TVTNodeAttachMode) : PVirtualNode;
var
  rttiMethodEntry : TRttiMethodEntry;
  param1, param2 : TValue;
begin
  rttiMethodEntry := GetMethodEntry('InsertNode');
  rttiMethodEntry.params := rttiMethodEntry.rttiMethod.GetParameters;
  TValue.Make(@parentNode, rttiMethodEntry.params[0].ParamType.Handle, param1);
  TValue.Make(@attachMode, rttiMethodEntry.params[1].ParamType.Handle, param2);
  //function InsertNode(Node: PVirtualNode; Mode: TVTNodeAttachMode; UserData: Pointer = nil): PVirtualNode;
  Result := PVirtualNode(PPointer(rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, [param1, param2, TValue.From<Pointer>(nil)]).GetReferenceToRawData())^);

end;

function TVirtualStringTreeProxy.InvokeMethodWithPointerParam(const methodName : string; const value : Pointer) : TValue;
var
  rttiMethodEntry : TRttiMethodEntry;
  param : TValue;
begin
  rttiMethodEntry := GetMethodEntry(methodName);
  TValue.Make(@value, rttiMethodEntry.params[0].ParamType.Handle, param);
  Result := rttiMethodEntry.rttiMethod.Invoke(FTreeInstance, [param]);
end;

procedure TVirtualStringTreeProxy.SetExpanded(const node : PVirtualNode; const value : boolean);
var
  params : TArray<TRttiParameter>;
  param1, param2 : TValue;
begin
  if FExpandedRttiprop = nil then
    FExpandedRttiprop := FTreeType.GetIndexedProperty('Expanded');
  if not Assigned(FExpandedRttiprop) then
    raise Exception.Create('RTTI Error accessing [Expanded] property');
  params := FExpandedRttiprop.WriteMethod.GetParameters;

  TValue.Make(@node, params[0].ParamType.Handle, param1);
  TValue.Make(@value, params[1].ParamType.Handle, param2);
  FExpandedRttiprop.WriteMethod.Invoke(FTreeInstance, [param1, param2]);
end;

procedure TVirtualStringTreeProxy.SetVSTProperty(const propertyName : string; const value : TValue);
var
  rttiPropEntry : TRttiPropEntry;
begin
  rttiPropEntry := GetPropEntry(propertyName);
  rttiPropEntry.rttiProp.SetValue(FTreeInstance, value);
end;

end.

