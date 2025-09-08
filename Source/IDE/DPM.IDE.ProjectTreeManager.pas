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

unit DPM.IDE.ProjectTreeManager;

//an attempt to hack into the IDE project tree.
//since the IDE does not provide any api to do this
//we are drilling into the IDE internals.. messy.

interface

uses
  System.Rtti,
  Spring.Collections,
  Spring.Container,
  System.Classes,
  Vcl.Controls,
  WinApi.Messages,
  DPM.Core.Types,
  DPM.Core.Options.Search,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.IDE.Logger,
  DPM.IDE.VSTProxy,
  DPM.IDE.ProjectTree.Containers,
  DPM.IDE.Options;

type
  IDPMProjectTreeManager = interface
  ['{F0BA2907-E337-4591-8E16-FB684AE2E19B}']
    procedure EndLoading();
    procedure ProjectLoaded(const fileName : string);
    procedure ProjectClosed(const fileName : string);
    procedure ProjectGroupClosed;
  end;

const
  WM_PROJECTLOADED = WM_USER + $1234;

type
  TDPMProjectTreeManager = class(TInterfacedObject,IDPMProjectTreeManager)
  private
    FContainer : TContainer;
    FLogger : IDPMIDELogger;
    FOptions : IDPMIDEOptions;

    FWindowHandle : THandle;
    FTimerRunning : boolean;

    FProjectTreeInstance : TControl;
    FVSTProxy : TVirtualStringTreeProxy;
    FSearchOptions : TSearchOptions;

    FProjectLoadList : IQueue<string>;
    FDPMImageIndex : integer;
    FPlatformImageIndexes : array[TDPMPlatform] of integer;

  //TODO : Invalidate cache when projects close.
    FNodeCache : IDictionary<TProjectTreeContainer, PVirtualNode>;

  //procedure DumpInterfaces(AClass: TClass);
  protected
    procedure ProjectLoaded(const fileName : string);
    procedure ProjectClosed(const fileName : string);
    procedure EndLoading;
    procedure ProjectGroupClosed;

    procedure WndProc(var msg: TMessage);

    function EnsureProjectTree : boolean;

    function TryGetContainerTreeNode(const container : TProjectTreeContainer; out containerNode : PVirtualNode) : boolean;
    procedure AddChildContainer(const parentContainer, childContainer : TProjectTreeContainer);
    procedure AddSiblingContainer(const existingContainer, siblingContainer : TProjectTreeContainer);

    function GetProjects : IList<TProjectTreeContainer>;
    function FindProjectNode(const fileName : string) : TProjectTreeContainer;
    function FindTargetPlatformContainer(const projectContainer : TProjectTreeContainer) : TProjectTreeContainer;
    function FindDPMContainer(const projectContainer : TProjectTreeContainer) : TProjectTreeContainer;

    procedure ConfigureProjectDPMNode(const projectContainer : TProjectTreeContainer; const projectFile : string; const config : IConfiguration);
    procedure UpdateProjectDPMPackages(const targetPlatformsContainer : TProjectTreeContainer; const dpmContainer : TProjectTreeContainer; const projectFile : string; const projectEditor : IProjectEditor);

    procedure DoProjectLoaded(const projectFile : string);

    procedure DoDumpClass(const typ : TRttiInstanceType);
    procedure DumpClass(const obj : TClass);

  public
    constructor Create(const container : TContainer; const logger : IDPMIDELogger; const options : IDPMIDEOptions);
    destructor Destroy;override;

  end;

implementation

uses
  ToolsApi,
  System.TypInfo,
  System.SysUtils,
  WinApi.Windows,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Forms,
  {$IF CompilerVersion >= 35.0}
  Vcl.ImageCollection,
  Vcl.VirtualImageList,
  {$IFEND}
  DPM.Core.Constants,
  DPM.Core.Logging,
  DPM.Core.Options.Common,
  DPM.Core.Configuration.Manager,
  DPM.Core.Project.Editor,
  DPM.IDE.Constants,
  DPM.IDE.Utils,
  DPM.IDE.Types;


const
  cCategoryContainerClass = 'Containers.TStdContainerCategory';




{ TProjectTreeManager }

procedure TDPMProjectTreeManager.AddChildContainer(const parentContainer, childContainer: TProjectTreeContainer);
var
  parentNode, childNode : PVirtualNode;
  nodeData : PNodeData;
begin
  if TryGetContainerTreeNode(parentContainer, parentNode) then
  begin
    childNode := FVSTProxy.AddChildNode(parentNode);
    nodeData := FVSTProxy.GetNodeData(childNode);
    nodeData.GraphLocation := childContainer.GraphLocation;
    nodeData.GraphData := childContainer.GraphData;
  end;
end;

procedure TDPMProjectTreeManager.AddSiblingContainer(const existingContainer, siblingContainer: TProjectTreeContainer);
var
  parentNode : PVirtualNode;
  childNode : PVirtualNode;
  nodeData : PNodeData;
begin
  if not TryGetContainerTreeNode(existingContainer, parentNode) then
    raise Exception.Create('Unable to find node for project container');

  childNode := FVSTProxy.InsertNode(parentNode, TVTNodeAttachMode.amInsertAfter);
  nodeData := FVSTProxy.GetNodeData(childNode);
  nodeData.GraphLocation := siblingContainer.GraphLocation;
  nodeData.GraphData := siblingContainer.GraphData;
end;

procedure TDPMProjectTreeManager.ConfigureProjectDPMNode(const projectContainer: TProjectTreeContainer; const projectFile : string; const config : IConfiguration);
var
  dpmContainer : TProjectTreeContainer;
  targetPlatformContainer : TProjectTreeContainer;
  projectEditor : IProjectEditor;
  projectNode : PVirtualNode;
begin
  projectEditor := TProjectEditor.Create(FLogger as ILogger, config, IDECompilerVersion);

  targetPlatformContainer := FindTargetPlatformContainer(projectContainer);
  if targetPlatformContainer = nil then
  begin
   //the container might not exists yet if the project node was never expanded.
   //so we expand and collapse the project node to force it to be created.
    projectNode := FNodeCache[projectContainer];
    if projectNode <> nil then
    begin
      FVSTProxy.SetExpanded(projectNode,true);
      FVSTProxy.SetExpanded(projectNode,false);
    end;
    targetPlatformContainer := FindTargetPlatformContainer(projectContainer);

    if targetPlatformContainer = nil then
      exit;

  end;
//  DumpClass(targetPlatformContainer.ClassType);
  //first see if we have allready added dpm to the project
  dpmContainer := FindDPMContainer(projectContainer);
  if dpmContainer = nil then
  begin
    //not found so we need to add it.
    try
      dpmContainer := TProjectTreeContainer.CreateNewContainer(projectContainer, cDPMPackages, cDPMContainer);
      dpmContainer.ImageIndex := FDPMImageIndex;
      targetPlatformContainer := FindTargetPlatformContainer(projectContainer);
      Assert(targetPlatformContainer <> nil);
      //add it to the tree
      AddSiblingContainer(targetPlatformContainer, dpmContainer);

      //this is important.. add it to the model,  without this the dpm node disappears if the IDE rebuilds the tree
      projectContainer.Children.Insert(2, dpmContainer); // 0=build config, 1=target platforms
    except
      on e : Exception do
      begin
        FLogger.Error(e.Message);
        OutputDebugString(PChar(e.Message));
        exit;
      end;
    end;
  end;
  Application.ProcessMessages;
  UpdateProjectDPMPackages(targetPlatformContainer, dpmContainer, projectFile, projectEditor);

end;

constructor TDPMProjectTreeManager.Create(const container : TContainer;const logger : IDPMIDELogger; const options : IDPMIDEOptions);
begin
  FContainer := container;
  FLogger := logger;
  FOptions := options;
  FProjectLoadList := TCollections.CreateQueue<string>;
  FWindowHandle := AllocateHWnd(WndProc);

  //can't find it here as it's too early as our expert is loaded before the project manager is loaded.
  FProjectTreeInstance := nil;
  FVSTProxy := nil;

  FSearchOptions := TSearchOptions.Create;
  // This ensures that the default config file is uses if a project one doesn't exist.
  FSearchOptions.ApplyCommon(TCommonOptions.Default);


  FNodeCache := TCollections.CreateDictionary<TProjectTreeContainer, PVirtualNode>();


end;

destructor TDPMProjectTreeManager.Destroy;
begin
  FLogger := nil;
  FVSTProxy.Free;
  DeallocateHWnd(FWindowHandle);
  FSearchOptions.Free;
  inherited;
end;

function TDPMProjectTreeManager.EnsureProjectTree : boolean;
var

// i: Integer;
 platform : TDPMPlatform;
 {$IF CompilerVersion >= 35.0}
 imageCollection : TImageCollection;
 imageList : TVirtualImageList;
 {$ELSE}
 bitmap : TBitmap;
 imageList : TCustomImageList; 
 {$IFEND}
begin
  if FVSTProxy <> nil then
    exit(true);

  result := false;
  if FVSTProxy = nil then
  begin
   //control name and class discovered via IDE Explorer https://www.davidghoyle.co.uk/WordPress - need to check it's the same for each new IDE version
    FProjectTreeInstance := FindIDEControl('TVirtualStringTree', 'ProjectTree2');
    if FProjectTreeInstance <> nil then
    begin
      FVSTProxy := TVirtualStringTreeProxy.Create(FProjectTreeInstance, FLogger);
       {$IF CompilerVersion < 35.0}
      imageList := FVSTProxy.Images;
      {$ELSE}
      imageList := TVirtualImageList(FVSTProxy.Images);
      {$IFEND}

      for platform := Low(TDPMPlatform) to High(TDPMPlatform) do
        FPlatformImageIndexes[platform] := -1;

      Result := true;
      {$IF CompilerVersion < 35.0}
      //10.4 and earlier
      
      bitmap := TBitmap.Create;
      try
        bitmap.LoadFromResourceName(HInstance, 'DPMLOGOBMP_16');
        FDPMImageIndex := imageList.AddMasked(bitmap, clFuchsia);
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_WIN32');
        FPlatformImageIndexes[TDPMPlatform.Win32] := imageList.AddMasked(bitmap, clFuchsia);
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_WIN32'); //same for win32/64
        FPlatformImageIndexes[TDPMPlatform.Win64] := imageList.AddMasked(bitmap, clFuchsia);
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_MACOS');
        FPlatformImageIndexes[TDPMPlatform.OSX32] := imageList.AddMasked(bitmap, clFuchsia);
        FPlatformImageIndexes[TDPMPlatform.OSX64] := FPlatformImageIndexes[TDPMPlatform.OSX32];
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_ANDRIOD');
        FPlatformImageIndexes[TDPMPlatform.AndroidArm32] := imageList.AddMasked(bitmap, clFuchsia);
        FPlatformImageIndexes[TDPMPlatform.AndroidArm64] := FPlatformImageIndexes[TDPMPlatform.AndroidArm32];
        FPlatformImageIndexes[TDPMPlatform.AndroidIntel32] := FPlatformImageIndexes[TDPMPlatform.AndroidArm32];
        FPlatformImageIndexes[TDPMPlatform.AndroidIntel64] := FPlatformImageIndexes[TDPMPlatform.AndroidArm32];
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_IOS');
        FPlatformImageIndexes[TDPMPlatform.iOS32] := imageList.AddMasked(bitmap, clFuchsia);
        FPlatformImageIndexes[TDPMPlatform.iOS64] := FPlatformImageIndexes[TDPMPlatform.iOS32];
        bitmap.LoadFromResourceName(HInstance, 'PLATFORM_LINUX');
        FPlatformImageIndexes[TDPMPlatform.LinuxIntel32] := imageList.AddMasked(bitmap, clFuchsia);
        FPlatformImageIndexes[TDPMPlatform.LinuxIntel64] := FPlatformImageIndexes[TDPMPlatform.LinuxIntel32];
        FPlatformImageIndexes[TDPMPlatform.LinuxArm32] := FPlatformImageIndexes[TDPMPlatform.LinuxIntel32];
        FPlatformImageIndexes[TDPMPlatform.LinuxArm64] := FPlatformImageIndexes[TDPMPlatform.LinuxIntel32];

      finally
        bitmap.Free;
      end;
      {$ELSE}
      //11.x and later uses an imagecollection so it's easy to lookup the built in images and use those.
      //just need to add our logo image to the collection and then the image list.
      imageCollection :=  TImageCollection(TVirtualImageList(imageList).ImageCollection);
      imageCollection.Add('DPM\DPMLOGO', HInstance, 'DPMLOGO', ['_16','_24','_32']);
      imageList.Add('DPM\DPMLOGO','DPM\DPMLOGO');

      //TODO : This sometimes doesn't work and we end up with the wrong icon. Only seems to happen'
      // when loading large project groups.
      FDPMImageIndex := imageList.GetIndexByName('DPM\DPMLOGO');

      FPlatformImageIndexes[TDPMPlatform.Win32] := imageList.GetIndexByName('Platforms\PlatformWindows');
      FPlatformImageIndexes[TDPMPlatform.Win64] := FPlatformImageIndexes[TDPMPlatform.Win32];

      FPlatformImageIndexes[TDPMPlatform.OSX32] := imageList.GetIndexByName('Platforms\PlatformMacOS');
      FPlatformImageIndexes[TDPMPlatform.OSX64] := FPlatformImageIndexes[TDPMPlatform.OSX32];

      FPlatformImageIndexes[TDPMPlatform.AndroidArm32] := imageList.GetIndexByName('Platforms\PlatformAndroid');
      FPlatformImageIndexes[TDPMPlatform.AndroidArm64] := FPlatformImageIndexes[TDPMPlatform.AndroidArm32];
      
      FPlatformImageIndexes[TDPMPlatform.iOS32] := imageList.GetIndexByName('Platforms\PlatformiOS');
      FPlatformImageIndexes[TDPMPlatform.iOS64] := FPlatformImageIndexes[TDPMPlatform.iOS32];

      FPlatformImageIndexes[TDPMPlatform.LinuxIntel64] := imageList.GetIndexByName('Platforms\PlatformLinux');
      {$IFEND}
    end;
  end;
end;

function TDPMProjectTreeManager.FindDPMContainer(const projectContainer: TProjectTreeContainer): TProjectTreeContainer;
var
  childContainer : TProjectTreeContainer;
  children : IInterfaceList;
  i : integer;
begin
  children := projectContainer.Children;
  for i := 0 to children.Count -1 do
  begin
    childContainer := TProjectTreeContainer(children[i] as TObject);
    if SameText(childContainer.DisplayName, cDPMPackages ) then
      exit(childContainer);
  end;
  result := nil;
end;

function TDPMProjectTreeManager.FindProjectNode(const fileName: string): TProjectTreeContainer;
var
  displayName : string;
  rootNode, projectNode : PVirtualNode;
  nodeData: PNodeData;
  container : TProjectTreeContainer;
  project : ICustomProjectGroupProject;
//  graphLocation : IInterface;
//  graphData : IInterface;
begin
  result := nil;
  displayName := ChangeFileExt(ExtractFileName(fileName),'');

  rootNode := FVSTProxy.GetFirstVisibleNode;
  if Assigned(rootNode) then
  begin
    projectNode := FVSTProxy.GetFirstChild(rootNode);
    while projectNode <> nil do
    begin
      nodeData := FVSTProxy.GetNodeData(projectNode);

      //DumpClass((nodeData.GraphLocation as TObject).ClassType);
      container := TProjectTreeContainer(nodeData.GraphLocation as TObject);
      if Assigned(container) then
      begin
        project := container.Project;
        if project <> nil then
        begin
          if SameText(container.FileName, fileName) then
          begin
//            graphLocation := container.GraphLocation;
//            graphData := container.GraphData;
            //take this opportunity to cache the node.
            FNodeCache[container] := projectNode;
            exit(container);
          end;
        end;
      end;
      projectNode := FVSTProxy.GetNextSibling(projectNode);
    end;
  end;
end;

function TDPMProjectTreeManager.FindTargetPlatformContainer(const projectContainer: TProjectTreeContainer): TProjectTreeContainer;
var
  childContainer : TProjectTreeContainer;
  children : IInterfaceList;
  i : integer;
begin
  result := nil;
  children := projectContainer.Children;
  if children = nil then
  begin
    FLogger.Debug('projectContainer  children Empty for  ' + projectContainer.DisplayName);
    exit;
  end;

  for i := 0 to children.Count -1 do
  begin
    childContainer := TProjectTreeContainer(children[i] as TObject);
    if SameText(childContainer.ClassName, 'TBasePlatformContainer') then //class name comes from rtti inspection
      exit(childContainer);
  end;
end;

function TDPMProjectTreeManager.GetProjects: IList<TProjectTreeContainer>;
var
  rootNode, projectNode: Pointer;
  nodeData: PNodeData;
  proxy : TProjectTreeContainer;
begin
//
  Assert(EnsureProjectTree);
  result := TCollections.CreateList<TProjectTreeContainer>();
  rootNode := FVSTProxy.GetFirstVisibleNode;
  if Assigned(rootNode) then
  begin
    projectNode := FVSTProxy.GetFirstChild(rootNode);
    while projectNode <> nil do
    begin
      nodeData := FVSTProxy.GetNodeData(projectNode);
      proxy := TProjectTreeContainer(nodeData.GraphLocation as TObject);
      //FLogger.Debug('Project Container ' + proxy.DisplayName);
      if Assigned(proxy) then
        result.Add(proxy);
      //DumpClass(proxy.ClassType);

      projectNode := FVSTProxy.GetNextSibling(projectNode);
    end;
  end;
end;

procedure TDPMProjectTreeManager.EndLoading;
begin
  if not FOptions.AddDPMToProjectTree then
    exit;
  //kick off a timer that will eventually sort out the nodes. - see WndProc
  PostMessage(FWindowHandle, WM_PROJECTLOADED, 0,0);
end;

procedure TDPMProjectTreeManager.ProjectClosed(const fileName: string);
//var
//  projectContainer : TProjectTreeContainer;
begin
  //not ideal.. but not sure it will be a problem.. we don't really need the nodes
  //after it's all setup. When we add packages etc, that triggers a project reload
  //so the tree is built again.
  FNodeCache.Clear;

//  this doesn't work, the container's project is already gone when we get here so FindProjectNode fails.
//  projectContainer := FindProjectNode(fileName);
//  if projectContainer <> nil then
//  begin
//    if FNodeCache.ContainsKey(projectContainer) then
//      FNodeCache.Remove(projectContainer);
//  end;
end;

procedure TDPMProjectTreeManager.ProjectLoaded(const fileName: string);
begin
  if not FOptions.AddDPMToProjectTree then
    exit;
  //The project tree nodes do not seem to have been added at this stage
  //just enqueue the project, and we'll deal with it when all projects are loaded.
  FProjectLoadList.Enqueue(fileName);
end;

procedure TDPMProjectTreeManager.ProjectGroupClosed;
begin
  FNodeCache.Clear;
end;

function TDPMProjectTreeManager.TryGetContainerTreeNode(const container: TProjectTreeContainer; out containerNode: PVirtualNode): boolean;
var
  node : PVirtualNode;
  nodeData : PNodeData;
begin
  containerNode := nil;
  result := false;
  if FNodeCache.TryGetValue(container, containerNode) then
    exit(true);

  node := FVSTProxy.GetFirstNode;
  while node <> nil do
  begin
    nodeData := FVSTProxy.GetNodeData(node);
    if container = (nodeData.GraphLocation as TObject) then
    begin
      containerNode := node;
      FNodeCache[container] := containerNode;
      exit(true);
    end
    else
    begin
      if nodeData.GraphLocation <> nil then
        FNodeCache[TProjectTreeContainer(nodeData.GraphLocation as TObject)] := node;
    end;
    node := FVSTProxy.GetNextNode(node);
  end;
end;

procedure TDPMProjectTreeManager.UpdateProjectDPMPackages(const targetPlatformsContainer : TProjectTreeContainer; const dpmContainer: TProjectTreeContainer; const projectFile : string; const projectEditor : IProjectEditor);
var
  projectGroup : IOTAProjectGroup;
  project : IOTAProject;
  sConfigFile : string;
  pf : TDPMPlatform;
  dpmNode : PVirtualNode;
  dpmChildren : IInterfaceList;
  platformSortedList : TStringList;
  i : integer;
  platformPackageReferences : IPackageReference;

  function FindProject : IOTAProject;
  var
    j : integer;
  begin
    result := nil;
    for j := 0 to projectGroup.ProjectCount -1 do
    begin
      if SameText(projectGroup.Projects[j].FileName, projectFile) then
        exit(projectGroup.Projects[j]);
    end;
  end;

  //TODO: Figure out image indexes for platforms.
  function DPMPlatformImageIndex(const pf : TDPMPlatform) : integer;
  begin
    result := FPlatformImageIndexes[pf];
//
//    case pf of
//      TDPMPlatform.UnknownPlatform: ;
//      TDPMPlatform.Win32:  result := FPlatformImageIndexes[pf];
//      TDPMPlatform.Win64:  result := 94;
//      TDPMPlatform.WinArm32: ;
//      TDPMPlatform.WinArm64: ;
//      TDPMPlatform.OSX32: result := 91;
//      TDPMPlatform.OSX64: result := 91;
//      TDPMPlatform.AndroidArm32: result := 95;
//      TDPMPlatform.AndroidArm64: result := 95;
//      TDPMPlatform.AndroidIntel32: ;
//      TDPMPlatform.AndroidIntel64: ;
//      TDPMPlatform.iOS32: result := 96;
//      TDPMPlatform.iOS64: result := 93;
//      TDPMPlatform.LinuxIntel32: result := 92;
//      TDPMPlatform.LinuxIntel64: result := 92;
//      TDPMPlatform.LinuxArm32: ;
//      TDPMPlatform.LinuxArm64: ;
//    end;
  end;


  procedure AddPlatform(const pf : TDPMPlatform; const PackageReferences : IPackageReference);
  var
    platformContainer : TProjectTreeContainer;
    packageRef : IPackageReference;

    procedure AddPackage(const parentContainer : TProjectTreeContainer; const packageReference : IPackageReference; const children : IInterfaceList);
    var
      packageRefContainer : TProjectTreeContainer;
      depRef : IPackageReference;
    begin
      packageRefContainer := TProjectTreeContainer.CreateNewContainer(parentContainer, packageReference.ToIdVersionString,cDPMContainer);
      packageRefContainer.ImageIndex := -1;
      AddChildContainer(parentContainer, packageRefContainer);
      children.Add(packageRefContainer);

      if packageReference.HasChildren then
      begin
        packageRefContainer.Children := TInterfaceList.Create;
        for depRef in packageReference.Children do
        begin
          AddPackage(packageRefContainer, depRef, packageRefContainer.Children);
        end;
      end;
    end;

  begin
    platformContainer := TProjectTreeContainer.CreateNewContainer(dpmContainer, DPMPlatformToDisplayString(pf),cDPMContainer);
    platformContainer.ImageIndex := DPMPlatformImageIndex(pf);
    AddChildContainer(dpmContainer, platformContainer);
    dpmChildren.Add(platformContainer);

    if PackageReferences.HasChildren then
    begin
      platformContainer.Children := TInterfaceList.Create;

      //using for loop rather the enumerator for per reasons.
      for packageRef in PackageReferences.Children do
      begin
        if packageRef.Platform <> pf then
          continue;

        AddPackage(platformContainer, packageRef, platformContainer.Children);

      end;
    end;
  end;


begin
  Assert(dpmContainer <> nil);
  projectGroup := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;

  //projectGroup.FindProject doesn't work! so we do it ourselves.
  project := FindProject;
  if project = nil then
    exit;

  dpmChildren := dpmContainer.Children;
  //Clear dpm node before doing this
  if dpmChildren <> nil then
  begin
    dpmChildren.Clear;
    if TryGetContainerTreeNode(dpmContainer, dpmNode) then
      FVSTProxy.DeleteChildren(dpmContainer);
  end
  else
  begin
    //the container classes don't create the list, so we must.
    dpmChildren := TInterfaceList.Create;
    dpmContainer.Children := dpmChildren;
  end;

  //if there is a project specific config file then that is what we should use.
  sConfigFile := IncludeTrailingPathDelimiter(ExtractFilePath(project.FileName)) + cDPMConfigFileName;
  if FileExists(sConfigFile) then
    FSearchOptions.ConfigFile := sConfigFile;


  if projectEditor.LoadProject(projectFile) then
  begin
    platformSortedList := TStringList.Create;
    platformSortedList.Sorted := true;
    try
      for pf in projectEditor.Platforms do
        platformSortedList.AddObject(DPMPlatformToString(pf), TObjecT(Ord(pf)));

      for i := 0 to  platformSortedList.Count -1 do
      begin
        pf := TDPMPlatform(Integer(platformSortedList.Objects[i]));
        platformPackageReferences := projectEditor.GetPackageReferences(pf);
        if platformPackageReferences <> nil then
          AddPlatform(pf, platformPackageReferences);

      end;
    finally
      platformSortedList.Free;
    end;

  //      FLogger.Debug('Target platform : ' + DPMPlatformToString(pf));
//      Application.ProcessMessages;
  end;

end;

procedure TDPMProjectTreeManager.DoDumpClass(const typ: TRttiInstanceType);
var

  Field : TRttiField;

  Prop: TRttiProperty;
  IndexProp : TRttiIndexedProperty;
  intfType : TRttiInterfaceType;

  method : TRttiMethod;
  sMethod : string;

begin
  OutputDebugString(PChar(''));
  OutputDebugString(PChar('class ' + Typ.Name +' = class(' + Typ.MetaclassType.ClassParent.ClassName + ')'));

  for intfType in Typ.GetImplementedInterfaces do
  begin
     OutputDebugString(PChar('  implements interface ' + intfType.Name + ' [' + intfType.GUID.ToString + ']'));

  end;
  OutputDebugString(PChar(''));

  for Field in Typ.GetDeclaredFields do
  begin
    OutputDebugString(PChar('  ' + Field.Name + ' : ' + Field.FieldType.Name));
  end;
  OutputDebugString(PChar(''));

  for Prop in Typ.GetDeclaredProperties do
  begin
    OutputDebugString(PChar('  property ' + Prop.Name + ' : ' + prop.PropertyType.Name));

    if prop.PropertyType is TRttiInterfaceType then
    begin
      intfType := prop.PropertyType as TRttiInterfaceType;

       OutputDebugString(PChar('interface ' + intfType.Name + ' [' + intfType.GUID.ToString + ']'));
    end;
  end;

  for IndexProp in Typ.GetDeclaredIndexedProperties do
  begin
    OutputDebugString(PChar('  property ' + IndexProp.Name  + '[] : ' + IndexProp.PropertyType.Name));
  end;
  OutputDebugString(PChar(''));

  for method in Typ.GetDeclaredMethods do
  begin
    sMethod := method.Name;
    if method.ReturnType <> nil then
      sMethod := '  function ' + sMethod +  ' : ' + method.ReturnType.Name
    else if method.IsConstructor then
      sMethod := '  constructor ' + sMethod
    else if method.IsDestructor then
      sMethod := '  destructor ' + sMethod
    else
      sMethod := '  procedure ' + sMethod;

    OutputDebugString(PChar(sMethod));
  end;

end;

procedure TDPMProjectTreeManager.DoProjectLoaded(const projectFile : string);
var
  projectContainer : TProjectTreeContainer;
  configurationManager : IConfigurationManager;
  config : IConfiguration;
begin
  //load our dpm configuration
  configurationManager := FContainer.Resolve<IConfigurationManager>;
  config := configurationManager.LoadConfig(FSearchOptions.ConfigFile);

  EnsureProjectTree;
  projectContainer := FindProjectNode(projectFile);

  if projectContainer <> nil then
    ConfigureProjectDPMNode(projectContainer, projectFile, config)
  else
    FLogger.Debug('project container not found for ' + projectfile);

end;

procedure TDPMProjectTreeManager.DumpClass(const obj: TClass);
var
  Ctx: TRttiContext;
  typ : TRttiInstanceType;
begin
  if not (obj.ClassParent = TObject) then
    DumpClass(obj.ClassParent);

  Typ := Ctx.GetType(obj).AsInstance;
  DoDumpClass(typ);

end;

//procedure TDPMProjectTreeManager.DumpInterfaces(AClass: TClass);
//var
//  i : integer;
//  InterfaceTable: PInterfaceTable;
//  InterfaceEntry: PInterfaceEntry;
//begin
//  while Assigned(AClass) do
//  begin
//    InterfaceTable := AClass.GetInterfaceTable;
//    if Assigned(InterfaceTable) then
//    begin
//      OutputDebugString(PChar('Implemented interfaces in ' +  AClass.ClassName));
//      for i := 0 to InterfaceTable.EntryCount-1 do
//      begin
//        InterfaceEntry := @InterfaceTable.Entries[i];
//
//        OutputDebugString(PChar(Format('%d. GUID = %s offest = %s',[i, GUIDToString(InterfaceEntry.IID), IntToHex(InterfaceEntry.IOffset,2)])));
//      end;
//    end;
//    AClass := AClass.ClassParent;
//  end;
//  writeln;
//end;

procedure TDPMProjectTreeManager.WndProc(var msg: TMessage);
var
  project : string;
begin
  case msg.Msg of
    WM_PROJECTLOADED :
    begin
      if FTimerRunning then
      begin
        KillTimer(FWindowHandle, 1);
        FTimerRunning := false;
      end;
      //if we try too early, the tree might not exist yet
      //or the images are not added to the list and we get the
      //wrong image indexes for the platforms.
      SetTimer(FWindowHandle, 1, 500, nil);
      FTimerRunning := true;
      msg.Result := 1;
    end;
    WM_TIMER :
    begin
      if msg.WParam = 1 then
      begin
        //sometimes the tree isn't actually available when we get here.
        //in that case we'll just let the timer continue and try again later.
        if EnsureProjectTree then
        begin
          //stop the timer.
          FTimerRunning := false;
          KillTimer(FWindowHandle, 1);
          FVSTProxy.BeginUpdate;
          try
            while FProjectLoadList.TryDequeue(project) do
            begin
              DoProjectLoaded(project);
            end;
          finally
            FVSTProxy.EndUpdate;
          end;
        end;
        msg.Result := 1;
      end;
    end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end;
end;


end.
