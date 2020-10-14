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
    amNoWhere,        // just for simplified tests, means to ignore the Add/Insert command
    amInsertBefore,   // insert node just before destination (as sibling of destination)
    amInsertAfter,    // insert node just after destionation (as sibling of destination)
    amAddChildFirst,  // add node as first child of destination
    amAddChildLast    // add node as last child of destination
  );


  TVirtualStringTreeProxy = class
  private
    FTreeInstance : TControl;
    FRttiCtx : TRttiContext;
    FTreeType : TRttiType;
    FLogger : IDPMIDELogger;
  protected
    function GetVSTProperty(const propertyName : string) : TValue;

    procedure SetVSTProperty(const propertyName : string; const value : TValue);

    //most VST methods take a node pointer so this is just a shortcut
    function InvokeMethodWithPointerParam(const methodName : string; const value : Pointer) : TValue;

    function GetImages: TCustomImageList;


  public
    constructor Create(const treeInstance : TControl; const logger : IDPMIDELogger);
    destructor Destroy;override;
    //just need this for debugging.. but will leave here for checking if it's not what we expect
    //in future versions
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNodeDataSize : integer;
    function GetRootNode : PVirtualNode;
    function GetFirstVisibleNode : PVirtualNode;
    function GetFirstChild(node: PVirtualNode): PVirtualNode;
    function GetFirstNode : PVirtualNode;
    function GetNextNode(const node : PVirtualNode) : PVirtualNode;
    function GetNextSibling(const node : PVirtualNode) : PVirtualNode;
    function GetNextVisible(const node : PVirtualNode) : PVirtualNode;
    function GetNodeData(const node: PVirtualNode): PNodeData;
    function AddChildNode(const parentNode : PVirtualNode) : PVirtualNode;
    function InsertNode(const parentNode : PVirtualNode; const attachMode : TVTNodeAttachMode) : PVirtualNode;
    procedure DeleteChildren(const parentNode : PVirtualNode);
    property Images : TCustomImageList read GetImages;
  end;

implementation

uses
  System.SysUtils;

{ TVirtualStringTreeHack }

function TVirtualStringTreeProxy.AddChildNode(const parentNode: PVirtualNode): PVirtualNode;
var
  rttiMethod: TRttiMethod;
  params: TArray<TRttiParameter>;
  param: TValue;
begin
  rttiMethod := FTreeType.GetMethod('AddChild');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [AddChild] not found.');
  params := rttiMethod.GetParameters;
  TValue.Make(@parentNode, params[0].ParamType.Handle, param);
  // function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; overload; virtual;
  //calling with just the parent fails.. seems like with rtti invoke optional params must be supplied?
  Result := PVirtualNode(PPointer(rttiMethod.Invoke(FTreeInstance, [param, TValue.From<Pointer>(nil)]).GetReferenceToRawData())^);

end;

procedure TVirtualStringTreeProxy.BeginUpdate;
var
  rttiMethod: TRttiMethod;
begin
  rttiMethod := FTreeType.GetMethod('BeginUpdate');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [BeginUpdate] not found.');
  rttiMethod.Invoke(FTreeInstance, []);
end;

constructor TVirtualStringTreeProxy.Create(const treeInstance: TControl; const logger : IDPMIDELogger);
begin
  FTreeInstance := treeInstance;
  FLogger := logger;
  FTreeType := FRttiCtx.GetType(FTreeInstance.ClassType).AsInstance;
end;

procedure TVirtualStringTreeProxy.DeleteChildren(const parentNode: PVirtualNode);
var
  rttiMethod : TRttiMethod;
  params: TArray<TRttiParameter>;
  param: TValue;
begin
  rttiMethod := FTreeType.GetMethod('DeleteChildren');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [DeleteChildren] not found.');

  params := rttiMethod.GetParameters;
  TValue.Make(@parentNode, params[0].ParamType.Handle, param);
  rttiMethod.Invoke(FTreeInstance, [param, true]);
end;

destructor TVirtualStringTreeProxy.Destroy;
begin
  inherited;
end;

procedure TVirtualStringTreeProxy.EndUpdate;
var
  rttiMethod: TRttiMethod;
begin
  rttiMethod := FTreeType.GetMethod('EndUpdate');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [EndUpdate] not found.');
  rttiMethod.Invoke(FTreeInstance, []);
end;

function TVirtualStringTreeProxy.GetFirstChild(node: PVirtualNode): PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetFirstChild', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetFirstNode: PVirtualNode;
var
  rttiMethod: TRttiMethod;
  res : TValue;
begin
  rttiMethod := FTreeType.GetMethod('GetFirst');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [GetFirst] not found.');

  //no params so easy
  res := rttiMethod.Invoke(FTreeInstance, []);
  Result := PVirtualNode(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetFirstVisibleNode: PVirtualNode;
var
  rttiMethod: TRttiMethod;
  res : TValue;
begin
  rttiMethod := FTreeType.GetMethod('GetFirstVisible');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [GetFirstVisible] not found.');

  //no params so easy
  res := rttiMethod.Invoke(FTreeInstance, []);
  Result := PVirtualNode(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetImages: TCustomImageList;
begin
  Result := GetVSTProperty('Images').AsType<TCustomImageList>();
end;

function TVirtualStringTreeProxy.GetNextNode(const node: PVirtualNode): PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNext', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNextSibling(const node: PVirtualNode): PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNextSibling', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNextVisible(const node: PVirtualNode): PVirtualNode;
begin
  Result := PVirtualNode(PPointer(InvokeMethodWithPointerParam('GetNextVisible', node).GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNodeData(const node: PVirtualNode): PNodeData;
var
  res: TValue;
begin
  res := InvokeMethodWithPointerParam('GetNodeData', node);
  Result := PNodeData(PPointer(res.GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetNodeDataSize: integer;
begin
  Result := GetVSTProperty('NodeDataSize').AsType<integer>;
end;

function TVirtualStringTreeProxy.GetRootNode: PVirtualNode;
begin
  Result := PVirtualNode(PPointer(GetVSTProperty('RootNode').GetReferenceToRawData())^);
end;

function TVirtualStringTreeProxy.GetVSTProperty(const propertyName: string): TValue;
var
  rttiProp: TRttiProperty;
begin
  rttiProp :=  FTreeType.GetProperty(propertyName);
  if not Assigned(rttiProp) then
    raise Exception.Create('RTTI Error accessing [' + propertyName + '] property' );
  Result := rttiProp.GetValue(FTreeInstance)
end;

function TVirtualStringTreeProxy.InsertNode(const parentNode: PVirtualNode; const attachMode: TVTNodeAttachMode): PVirtualNode;
var
  rttiMethod: TRttiMethod;
  params: TArray<TRttiParameter>;
  param1, param2: TValue;
begin
  rttiMethod := FTreeType.GetMethod('InsertNode');
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [InsertNode] not found.');
  params := rttiMethod.GetParameters;
  TValue.Make(@parentNode, params[0].ParamType.Handle, param1);
  TValue.Make(@attachMode, params[1].ParamType.Handle, param2);
  //function InsertNode(Node: PVirtualNode; Mode: TVTNodeAttachMode; UserData: Pointer = nil): PVirtualNode;
  Result := PVirtualNode(PPointer(rttiMethod.Invoke(FTreeInstance, [param1,param2, TValue.From<Pointer>(nil)]).GetReferenceToRawData())^);

end;

function TVirtualStringTreeProxy.InvokeMethodWithPointerParam(const methodName: string; const value: Pointer): TValue;
var
  rttiMethod : TRttiMethod;
  params: TArray<TRttiParameter>;
  param: TValue;
begin
  rttiMethod := FTreeType.GetMethod(methodName);
  if not Assigned(rttiMethod) then
    raise Exception.Create('RTTI for method [' + methodName + '] not found.');
  params := rttiMethod.GetParameters;
  TValue.Make(@value, params[0].ParamType.Handle, param);
  Result := rttiMethod.Invoke(FTreeInstance, [param]);
end;

procedure TVirtualStringTreeProxy.SetVSTProperty(const propertyName: string; const value: TValue);
var
  rttiProp: TRttiProperty;
begin
  rttiProp :=  FTreeType.GetProperty(propertyName);
  if not Assigned(rttiProp) then
    raise Exception.Create('RTTI Error accessing [' + propertyName + '] property' );
  rttiProp.SetValue(FTreeInstance, value);

end;

end.
