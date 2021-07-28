{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.IDE.EditorViewManager;

interface

uses
  ToolsApi,
  Spring.Container,
  Spring.Collections,
  DPM.IDE.ProjectTreeManager;

type
  IDPMEditorViewManager = interface
    ['{BD31BE3A-5255-4290-9991-1A0071B24F81}']
    procedure ShowViewForProject(const project : IOTAProject);
    procedure ProjectClosed(const projectFile : string);
    procedure ProjectLoaded(const projectFile : string);
    procedure Destroyed;
    //todo : add methods to hand notifications of projects added to group
  end;

  TDPMEditorViewManager = class(TInterfacedObject, IDPMEditorViewManager{$IF CompilerVersion >= 32.0}, INTAIDEThemingServicesNotifier{$IFEND})
  private
    FContainer : TContainer;
    FOpenViews : IDictionary<string, INTACustomEditorView>;
    FEditorViewServices : IOTAEditorViewServices;
    FImageIndex : integer;
    FProjectTreeManager : IDPMProjectTreeManager;
  protected
    procedure ProjectLoaded(const projectFile : string);
    procedure ProjectClosed(const projectFile : string);
    procedure ShowViewForProject(const project : IOTAProject);
    procedure Destroyed;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;

    //INTAIDEThemingServicesNotifier
    procedure ChangingTheme();
    { This notifier will be called immediately after the active IDE Theme changes }
    procedure ChangedTheme();

  public
    constructor Create(const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  DPM.IDE.EditorView,
  Vcl.Graphics,
  Vcl.Controls;


{ TDPMEditorViewManager }

procedure TDPMEditorViewManager.AfterSave;
begin

end;

procedure TDPMEditorViewManager.BeforeSave;
begin

end;

procedure TDPMEditorViewManager.ChangedTheme;
var
  view : INTACustomEditorView;
begin
  for view in FOpenViews.Values do
  begin
    (view as IDPMEditorView).ThemeChanged;
  end;
end;

procedure TDPMEditorViewManager.ChangingTheme;
begin

end;

constructor TDPMEditorViewManager.Create(const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
var
  imageList : TImageList;
  bmp : TBitmap;
  vs : INTAEditorViewServices;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;
  FOpenViews := TCollections.CreateDictionary < string, INTACustomEditorView > ;

  if not Supports(BorlandIDEServices, IOTAEditorViewServices, FEditorViewServices) then
    raise Exception.Create('Unable to get IOTAEditorViewServices');

  if not Supports(BorlandIDEServices, INTAEditorViewServices, vs) then
    raise Exception.Create('Unable to get INTAEditorViewServices');


  imageList := TImageList.Create(nil);
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, 'DPMIDELOGO_16');
    imageList.AddMasked(bmp, clFuchsia);
    FImageIndex := vs.AddImages(imageList, 'DPM');
  finally
    bmp.Free;
    imageList.Free;
  end;


end;

destructor TDPMEditorViewManager.Destroy;
begin
  FOpenViews := nil;
  FEditorViewServices := nil;
  inherited;
end;

procedure TDPMEditorViewManager.Destroyed;
var
  pair : TPair<string, INTACustomEditorView>;
begin
  for pair in FOpenViews do
  begin
    FEditorViewServices.CloseEditorView(pair.Value);
    FEditorViewServices.UnregisterEditorView(pair.Value.ViewIdentifier);
  end;

  FEditorViewServices := nil;
end;

procedure TDPMEditorViewManager.Modified;
begin

end;

procedure TDPMEditorViewManager.ProjectClosed(const projectFile : string);
var
  view : INTACustomEditorView;
begin
  if FOpenViews.TryGetValue(LowerCase(projectFile), view) then
  begin
    FOpenViews.Remove(LowerCase(projectFile));
    if FEditorViewServices <> nil then
    begin
      FEditorViewServices.CloseEditorView(view);
      FEditorViewServices.UnregisterEditorView(view.ViewIdentifier);
    end;
    view := nil;
  end;
end;

procedure TDPMEditorViewManager.ProjectLoaded(const projectFile : string);
var
  view : INTACustomEditorView;
begin
  //we are only using this for reloads.. if the view is open then tell it to refresh
  if FOpenViews.TryGetValue(LowerCase(projectFile), view) then
  begin
    (view as IDPMEditorView).Reloaded;
  end;

end;

procedure TDPMEditorViewManager.ShowViewForProject(const project : IOTAProject);
var
  view : INTACustomEditorView;
begin
  if not FOpenViews.TryGetValue(LowerCase(project.FileName), view) then
  begin
    view := TDPMEditorView.Create(FContainer, project, FImageIndex, FProjectTreeManager);
    FOpenViews.Add(LowerCase(project.FileName), view);
  end;
  if FEditorViewServices <> nil then
    FEditorViewServices.ShowEditorView(view as INTACustomEditorView);


end;

end.

