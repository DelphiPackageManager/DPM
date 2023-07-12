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

unit DPM.IDE.EditorViewManager;

interface

uses
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
  end;

  TDPMEditorViewManager = class(TInterfacedObject, IDPMEditorViewManager{$IF CompilerVersion >= 32.0}, INTAIDEThemingServicesNotifier{$IFEND})
  private
    FContainer : TContainer;
    FEditorView : INTACustomEditorView;
    FEditorViewServices : IOTAEditorViewServices;
    FImageIndex : integer;
    FProjectTreeManager : IDPMProjectTreeManager;
    FLogger : IDPMIDELogger;
  protected
    procedure ProjectLoaded(const projectFile : string);
    procedure ProjectClosed(const projectFile : string);
    procedure ProjectGroupClosed;
    procedure ShowViewForProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
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
  System.StrUtils,
  DPM.IDE.EditorView,
  Vcl.Graphics,
  Vcl.Controls;


{ TDPMEditorViewManager }

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

end;

destructor TDPMEditorViewManager.Destroy;
begin
  FEditorViewServices := nil;
  FProjectTreeManager := nil;
  inherited;
end;

procedure TDPMEditorViewManager.Destroyed;
begin
  //The views are already destroyed by the time we get here, so nothing to do.
  FEditorView := nil;
  FEditorViewServices := nil;
  FProjectTreeManager := nil;
end;

procedure TDPMEditorViewManager.Modified;
begin
  //NA
end;

procedure TDPMEditorViewManager.ProjectClosed(const projectFile : string);
begin
  if (EndsText('.groupproj', projectFile)) then
    exit;
  FLogger.Debug('EditorViewManager.ProjectClosed : ' + projectFile);
  if FEditorView <> nil then
  begin
    if (EndsText('.groupproj', projectFile)) then
    begin
      //closing the project group so close the view
      if FEditorView <> nil then
      begin
        if FEditorViewServices <> nil then
          FEditorViewServices.CloseEditorView(FEditorView);
        FEditorView := nil;
      end;
    end
    else
    begin
      //if it's not the project group being closed then just notify the view
//       (FEditorView as IDPMEditorView).ProjectChanged; //remove
       (FEditorView as IDPMEditorView).ProjectClosed(projectFile);
    end;
  end;

end;

procedure TDPMEditorViewManager.ProjectGroupClosed;
begin
  if FEditorView <> nil then
  begin
    if FEditorViewServices <> nil then
       FEditorViewServices.CloseEditorView(FEditorView);
    FEditorView := nil;
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
    FEditorView := TDPMEditorView.Create(FContainer, projectGroup, project, FImageIndex, FProjectTreeManager) as INTACustomEditorView;

  (FEditorView as IDPMEditorView).FilterToProject(projectGroup, project);

  if FEditorViewServices <> nil then
    FEditorViewServices.ShowEditorView(FEditorView);
end;


end.

