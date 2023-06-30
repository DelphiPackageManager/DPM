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

unit DPM.IDE.EditorView;

interface

uses
  ToolsAPI,
  DesignIntf,
  VCL.Forms,
  Spring.Container,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.BaseEditViewFrame,
  DPM.IDE.EditorViewFrame2;

type
  IDPMEditorView = interface
    ['{1DF76A55-76AC-4789-A35A-CA025583356A}']
    procedure ProjectChanged;
    procedure ProjectClosed(const projectName : string);
    procedure ProjectLoaded(const projectName : string);
    procedure ThemeChanged;
    procedure FilterToProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
  end;

  TDPMEditorView = class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150, IDPMEditorView)
  private
    FContainer : TContainer;
    FProject : IOTAProject;
    FProjectGroup : IOTAProjectGroup;
    FFrame : TDPMEditViewFrame2;
    FImageIndex : integer;
    FCaption : string;
    FProjectTreeManager : IDPMProjectTreeManager;
    FIdentifier : string;
  protected
    //IDPMEditorView
    procedure ProjectChanged;
    procedure ThemeChanged;
    procedure ProjectClosed(const projectName : string);
    procedure ProjectLoaded(const projectName : string);
    procedure FilterToProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);


    function CloneEditorView : INTACustomEditorView;
    procedure CloseAllCalled(var ShouldClose : Boolean);
    procedure DeselectView;
    function EditAction(Action : TEditAction) : Boolean;
    procedure FrameCreated(AFrame : TCustomFrame);
    function GetCanCloneView : Boolean;
    function GetCaption : string;
    function GetEditState : TEditState;
    function GetEditorWindowCaption : string;
    function GetFrameClass : TCustomFrameClass;
    function GetViewIdentifier : string;
    procedure SelectView;

    //INTACustomEditorView150
    function GetImageIndex : Integer;
    function GetTabHintText : string;
    procedure Close(var Allowed : Boolean);
  public
    constructor Create(const container : TContainer; const projectGroup : IOTAProjectGroup; const project : IOTAProject; const imageIndex : integer; const projectTreeManager : IDPMProjectTreeManager);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.System,
  DPM.IDE.EditorViewFrame,
  DPM.IDE.GroupEditorViewFrame;

{ TDPMEditorView }

function TDPMEditorView.CloneEditorView : INTACustomEditorView;
begin
  result := nil;
end;

procedure TDPMEditorView.Close(var Allowed : Boolean);
begin
  Allowed := FFrame.CanCloseView;
  if Allowed then
  begin
    FFrame.Closing;
    FFrame := nil;
  end;
end;

procedure TDPMEditorView.CloseAllCalled(var ShouldClose : Boolean);
begin
  //doesn't seem to get called???
  ShouldClose := FFrame.CanCloseView;
  if ShouldClose then
  begin
    FFrame.Closing;
    FFrame := nil;
  end;
end;

constructor TDPMEditorView.Create(const container : TContainer; const projectGroup : IOTAProjectGroup; const project : IOTAProject; const imageIndex : integer; const projectTreeManager : IDPMProjectTreeManager);
begin
  FContainer := container;
  FProjectGroup := projectGroup;
  FProject := project; //can be nil
  FImageIndex := imageIndex;
  FProjectTreeManager := projectTreeManager;
  FCaption := 'DPM';
  Assert(FProjectGroup <> nil);
//  if FProjectGroup <> nil then
//    FIdentifier := 'DPM_GROUP_VIEW_' + ChangeFileExt(ExtractFileName(FProjectGroup.FileName), '')
//  else
//    FIdentifier := 'DPM_VIEW_' + ChangeFileExt(ExtractFileName(FProject.FileName), '');
//   FIdentifier := StringReplace(FIdentifier, '.', '_', [rfReplaceAll]);

  FIdentifier := 'DPM_EDITOR_VIEW'; //we only have 1 view now
end;

procedure TDPMEditorView.DeselectView;
begin
  if FFrame <> nil then
    FFrame.ViewDeselected;
end;

destructor TDPMEditorView.Destroy;
begin

  inherited;
end;

function TDPMEditorView.EditAction(Action : TEditAction) : Boolean;
begin
  result := false;
end;


procedure TDPMEditorView.FilterToProject(const projectGroup : IOTAProjectGroup; const project : IOTAProject);
var
  sFileName : string;
begin
  FProjectGroup := projectGroup;
  FProject := project;
  if FFrame <> nil then
  begin
    if project <> nil then
      sFileName := project.FileName;
    FFrame.FilterToProject(sFileName);
  end;

end;

procedure TDPMEditorView.FrameCreated(AFrame : TCustomFrame);
begin
  FFrame := TDPMEditViewFrame2(AFrame);
  FFrame.Name := GetViewIdentifier;
  FFrame.Configure(FProjectGroup, FProject, FContainer, FProjectTreeManager);
end;

function TDPMEditorView.GetCanCloneView : Boolean;
begin
  result := false;
end;

function TDPMEditorView.GetCaption : string;
begin
  result := FCaption;
end;

function TDPMEditorView.GetEditorWindowCaption : string;
begin
  result := 'DPM Packages';
end;

function TDPMEditorView.GetEditState : TEditState;
begin
  result := [];
end;

function TDPMEditorView.GetFrameClass : TCustomFrameClass;
begin
    result := TDPMEditViewFrame2;
end;

function TDPMEditorView.GetImageIndex : Integer;
begin
  result := FImageIndex;
end;

function TDPMEditorView.GetTabHintText : string;
begin
  result := GetCaption;
end;

function TDPMEditorView.GetViewIdentifier : string;
begin
  result := FIdentifier;
end;

procedure TDPMEditorView.ProjectChanged;
begin
  if FFrame <> nil then
    FFrame.ProjectChanged;
end;

procedure TDPMEditorView.ProjectClosed(const projectName: string);
begin
  if FFrame <> nil then
    FFrame.ProjectClosed(projectName);
end;

procedure TDPMEditorView.ProjectLoaded(const projectName: string);
begin
  if FFrame <> nil then
    FFrame.ProjectLoaded(projectName);
end;

procedure TDPMEditorView.SelectView;
begin
  //Note : For some reason this is getting called twice each time the view is selected.
  if FFrame <> nil then
    FFrame.ViewSelected;
end;

procedure TDPMEditorView.ThemeChanged;
begin
  if FFrame <> nil then
    FFrame.ThemeChanged;
end;

end.

