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
  Winapi.Windows,
  Winapi.Messages,
  ToolsApi,
  Spring.Container,
  Spring.Collections,
  DPM.IDE.Logger,
  DPM.IDE.ProjectTreeManager;

{$I '..\DPMIDE.inc'}

type
  //handle calls from IDE and storage notifiers and manages the custom editorview
  IDPMEditorViewManager = interface
    ['{BD31BE3A-5255-4290-9991-1A0071B24F81}']
    procedure ShowViewForProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
    procedure ProjectClosed(const projectFile : string);
    procedure ProjectLoaded(const projectFile : string);
    procedure ProjectGroupClosed;
    procedure Destroyed;
    procedure ActivePlatformChanged(const platform : string);
    procedure ActiveProjectChanged(const project : IOTAProject);
  end;

  TDPMEditorViewManager = class(TInterfacedObject, IDPMEditorViewManager{$IF CompilerVersion >= 32.0}, INTAIDEThemingServicesNotifier{$IFEND})
  private
    FContainer : TContainer;
    FEditorView : INTACustomEditorView;
    FEditorViewServices : IOTAEditorViewServices;
    FImageIndex : integer;
    FProjectTreeManager : IDPMProjectTreeManager;
    FLogger : IDPMIDELogger;
    //Deferred-close machinery - see ProjectGroupClosed. Views waiting to be closed once the
    //group teardown that triggered the close has finished.
    FWindowHandle : HWND;
    FPendingCloseViews : IList<INTACustomEditorView>;
    procedure WndProc(var msg : TMessage);
  protected
    procedure ProjectLoaded(const projectFile : string);
    procedure ProjectClosed(const projectFile : string);
    procedure ProjectGroupClosed;
    procedure ShowViewForProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
    procedure Destroyed;
    procedure ActivePlatformChanged(const platform : string);
    procedure ActiveProjectChanged(const project : IOTAProject);

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
  System.StrUtils,
  System.Classes,
  DPM.IDE.EditorView,
  Vcl.Graphics,
  Vcl.Controls;

const
  //Posted by ProjectGroupClosed; handled in WndProc once the group close has unwound.
  WM_DPM_CLOSEPENDINGVIEWS = WM_USER + 1;


{ TDPMEditorViewManager }

procedure TDPMEditorViewManager.ActivePlatformChanged(const platform: string);
begin
  if FEditorView <> nil then
    (FEditorView as IDPMEditorView).ActivePlatformChanged(platform);
end;

procedure TDPMEditorViewManager.ActiveProjectChanged(const project: IOTAProject);
begin
  if FEditorView <> nil then
    (FEditorView as IDPMEditorView).FilterToProject(project);
end;

procedure TDPMEditorViewManager.AfterSave;
begin
  //NA
end;

procedure TDPMEditorViewManager.BeforeSave;
begin
  //NA
end;

procedure TDPMEditorViewManager.ChangedTheme;
begin
  if FEditorView <> nil then
    (FEditorView as IDPMEditorView).ThemeChanged;
end;

procedure TDPMEditorViewManager.ChangingTheme;
begin
  //NA
end;

constructor TDPMEditorViewManager.Create(const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
var
  imageList : TImageList;
  bmp : TBitmap;
  vs : INTAEditorViewServices;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;
  FLogger := FContainer.Resolve<IDPMIDELogger>;
  FEditorView := nil;

  if not Supports(BorlandIDEServices, IOTAEditorViewServices, FEditorViewServices) then
    raise Exception.Create('Unable to get IOTAEditorViewServices');

  if not Supports(BorlandIDEServices, INTAEditorViewServices, vs) then
    raise Exception.Create('Unable to get INTAEditorViewServices');

  imageList := TImageList.Create(nil);
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, 'DPMLOGOBMP_16');
    imageList.AddMasked(bmp, clFuchsia);
    FImageIndex := vs.AddImages(imageList, 'DPM');
  finally
    bmp.Free;
    imageList.Free;
  end;

  FPendingCloseViews := TCollections.CreateList<INTACustomEditorView>;
  FWindowHandle := AllocateHWnd(WndProc);
end;

destructor TDPMEditorViewManager.Destroy;
begin
  //Any still-pending views are simply dropped (not closed) - if we are being destroyed the
  //IDE is unloading the plugin and has destroyed (or is destroying) the views itself.
  FPendingCloseViews.Clear;
  DeallocateHWnd(FWindowHandle);
  FEditorView := nil;
  FEditorViewServices := nil;
  FProjectTreeManager := nil;
  inherited;
end;

procedure TDPMEditorViewManager.Destroyed;
begin
  //The views are already destroyed by the time we get here, so nothing to do - and no
  //deferred close must run after this point (WndProc checks FEditorViewServices).
  FPendingCloseViews.Clear;
  FEditorView := nil;
  FEditorViewServices := nil;
  FProjectTreeManager := nil;
end;

procedure TDPMEditorViewManager.WndProc(var msg : TMessage);
var
  view : INTACustomEditorView;
begin
  if msg.Msg = WM_DPM_CLOSEPENDINGVIEWS then
  begin
    msg.Result := 0;
    //Never let an exception escape into the IDE's message loop.
    try
      while FPendingCloseViews.Count > 0 do
      begin
        view := FPendingCloseViews[0];
        FPendingCloseViews.Delete(0);
        //Destroyed nils the services when the IDE is shutting down - just drop the reference.
        if FEditorViewServices <> nil then
          FEditorViewServices.CloseEditorView(view);
      end;
    except
      on e : Exception do
      begin
        if FLogger <> nil then
          FLogger.Debug('EditorViewManager : error closing deferred view : ' + e.Message);
      end;
    end;
  end
  else
    msg.Result := DefWindowProc(FWindowHandle, msg.Msg, msg.WParam, msg.LParam);
end;

procedure TDPMEditorViewManager.Modified;
begin
  //NA
end;

procedure TDPMEditorViewManager.ProjectClosed(const projectFile : string);
begin
  //Group closes are handled (deferred) by ProjectGroupClosed - never close the view
  //synchronously from inside a close notification (see ProjectGroupClosed).
  if (EndsText('.groupproj', projectFile)) then
    exit;
  FLogger.Debug('EditorViewManager.ProjectClosed : ' + projectFile);
  if FEditorView <> nil then
    (FEditorView as IDPMEditorView).ProjectClosed(projectFile);
end;

procedure TDPMEditorViewManager.ProjectGroupClosed;
begin
  //DO NOT close the view synchronously here. This notification arrives from inside
  //TProjectGroup.BeforeDestruction, and CloseEditorView makes the IDE select and display the
  //next editor buffer - activating a form designer and refreshing the component palette
  //against a half-destroyed module. During IDE shutdown that access violated deep in the
  //designer (Proxies.IsProxyClass). Instead, park the view and post ourselves a message : on
  //a normal group close the deferred close runs moments later when the teardown has unwound;
  //during IDE shutdown the message loop never pumps again and the IDE destroys the views
  //itself, which is exactly what we want.
  if FEditorView <> nil then
  begin
    FPendingCloseViews.Add(FEditorView);
    FEditorView := nil;
    PostMessage(FWindowHandle, WM_DPM_CLOSEPENDINGVIEWS, 0, 0);
  end;
end;

procedure TDPMEditorViewManager.ProjectLoaded(const projectFile : string);
begin
  FLogger.Debug('EditorViewManager.ProjectLoaded : ' + projectFile);
  //we are only using this for reloads.. if the view is open then tell it to refresh
  if FEditorView <> nil then
    (FEditorView as IDPMEditorView).ProjectChanged;

end;

procedure TDPMEditorViewManager.ShowViewForProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
begin
  if FEditorView = nil then
    FEditorView := TDPMEditorView.Create(FContainer, projectGroup, project, FImageIndex, FProjectTreeManager) as INTACustomEditorView
  else
    (FEditorView as IDPMEditorView).FilterToProject(project);

  if FEditorViewServices <> nil then
    FEditorViewServices.ShowEditorView(FEditorView);
end;


end.

