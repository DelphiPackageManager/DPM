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

unit DPM.IDE.Wizard;

interface

uses
  System.Classes,
  ToolsApi,
  Vcl.ActnList,
  Vcl.Controls,
  Spring.Container,
  DPM.IDE.Logger,
  DPM.IDE.MessageService,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.EditorViewManager;

type
  TDPMWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard)
  private
    FStorageNotifierID : integer;
    FIDENotifier : integer;
    FProjectMenuNoftifierId : integer;
    FThemeChangeNotifierId : integer;
    FEditorViewManager : IDPMEditorViewManager;
    FDPMIDEMessageService : IDPMIDEMessageService;
    FLogger : IDPMIDELogger;
    FContainer : TContainer;
    procedure InitContainer;
  protected

    //IOTAWizard
    procedure Execute;
    function GetIDString : string;
    function GetName : string;
    function GetState : TWizardState;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;


    procedure DPMMenuClick(Sender : TObject);

    procedure DPMActionUpdate(Sender : TObject);

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  System.UITypes,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  Spring.Container.Registration,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Init,
  DPM.Core.Utils.Config,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.IDE.ProjectController,
  DPM.IDE.ProjectStorageNotifier,
  DPM.IDE.IDENotifier,
  DPM.IDE.ProjectMenu,
  DPM.IDE.AddInOptions,
  DPM.IDE.Options,
  DPM.IDE.InstallerContext,
  DPM.IDE.PathManager,
  DPM.IDE.ToolsAPI;

{$R DPM.IDE.Resources.res}
{ TDPMWizard }

procedure TDPMWizard.InitContainer;
begin
  try
    FContainer := TContainer.Create;
    FContainer.RegisterType<IDPMIDEOptions, TDPMIDEOptions>.AsSingleton();
    FContainer.RegisterType<IDPMIDEMessageService,TDPMIDEMessageService>.AsSingleton();
    FContainer.RegisterType<TDPMIDELogger>.Implements<IDPMIDELogger>.Implements<ILogger>.AsSingleton();
    FContainer.RegisterType<IDPMProjectTreeManager, TDPMProjectTreeManager>.AsSingleton();
    FContainer.RegisterType<IDPMEditorViewManager, TDPMEditorViewManager>.AsSingleton();
    FContainer.RegisterType<IDPMIDEProjectController,TDPMIDEProjectController>.AsSingleton();
    FContainer.RegisterType<IDPMIDEPathManager,TDPMIDEPathManager>.AsSingleton();

    DPM.Core.Init.InitCore(FContainer,
      //replaces core registration of the IPackageInstallerContext implementation
      procedure(const container : TContainer)
      begin
        container.RegisterType<IPackageInstallerContext, TDPMIDEPackageInstallerContext>().AsSingleton();
      end);
    FContainer.Build;
  except
    on e : Exception do
    begin
     FLogger.Error('Error setting up the container : ' + e.Message);
    end;
  end;
end;


procedure TDPMWizard.AfterSave;
begin

end;

procedure TDPMWizard.BeforeSave;
begin

end;

constructor TDPMWizard.Create;
var
  storageNotifier : IOTAProjectFileStorageNotifier;
  ideNotifier : IOTAIDENotifier;
  projMenuNotifier : IOTAProjectMenuItemCreatorNotifier;
  options : INTAAddInOptions;
  projectController : IDPMIDEProjectController;
  dpmIDEOptions : IDPMIDEOptions;
  dpmMenu : TMenuItem;
  action : TAction;
  bmp : TBitmap;
  png : TPngImage;
  idx : integer;
  menuServices : INTAServices;
begin
  InitContainer;
  FLogger := FContainer.Resolve<IDPMIDELogger>;
  dpmIDEOptions := FContainer.Resolve<IDPMIDEOptions>;
  if FileExists(dpmIDEOptions.FileName) then
    dpmIDEOptions.LoadFromFile()
  else
  begin
    TConfigUtils.EnsureDefaultConfigDir;
    dpmIDEOptions.SaveToFile(); //create the file
  end;

  FLogger.Verbosity := dpmIDEOptions.LogVerbosity;

  FEditorViewManager := FContainer.Resolve<IDPMEditorViewManager>;
  FDPMIDEMessageService := FContainer.Resolve<IDPMIDEMessageService>;

  {$IF CompilerVersion >= 32.0}
  FThemeChangeNotifierId := (BorlandIDEServices as IOTAIDEThemingServices).AddNotifier(FEditorViewManager as INTAIDEThemingServicesNotifier);
  {$ELSE}
  FThemeChangeNotifierId := -1;
  {$IFEND}

  projectController := FContainer.Resolve<IDPMIDEProjectController>;

  ideNotifier := TDPMIDENotifier.Create(FLogger, projectController);
  FIDENotifier := (BorlandIDEServices as IOTAServices).AddNotifier(ideNotifier);

  projMenuNotifier := TDPMProjectMenuNotifier.Create(FEditorViewManager);
  FProjectMenuNoftifierId := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(projMenuNotifier);

  options := TDPMAddinOptions.Create(FContainer);
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(options);

  storageNotifier := TDPMProjectStorageNotifier.Create(FLogger, projectController);
  FStorageNotifierID := (BorlandIDEServices as IOTAProjectFileStorage).AddNotifier(storageNotifier);

  menuServices := (BorlandIDEServices as INTAServices);


  bmp := TBitmap.Create;
  png := TPngImage.Create;
  try
    png.LoadFromResourceName(HInstance, 'DPMLOGO_16');
    png.AssignTo(bmp);
    bmp.AlphaFormat:=afIgnored;
    idx := menuServices.AddMasked(bmp, clNone, 'DPM');
  finally
    bmp.Free;
    png.Free;
  end;

  action := TAction.Create(menuServices.ActionList);
  action.Name := 'actDPMPackageManager';
  action.Caption := 'DPM Package Manager';
  action.OnUpdate := Self.DPMActionUpdate;
  action.OnExecute := Self.DPMMenuClick;
  action.Visible := true;
  action.Category := 'DPM';

  dpmMenu := TMenuItem.Create(nil);
  dpmMenu.Caption := 'DPM Package Manager';
  dpmMenu.Name := 'GlobalDPMMenuItem';
  dpmMenu.Action := action;

  menuServices.AddActionMenu('ProjectOptionsItem',action, dpmMenu,true);
  //need to assign image index after adding the action menu, otherwise the image never shows.
  action.ImageIndex := idx;

  menuServices.NewToolbar('DPMPackageManager','DPM Package Manager');
  menuServices.AddToolButton('DPMPackageManager','ShowDPMButton', action);
end;

destructor TDPMWizard.Destroy;
begin
  inherited;
end;

procedure TDPMWizard.Destroyed;
begin
  if FStorageNotifierId > -1 then
    (BorlandIDEServices as IOTAProjectFileStorage).RemoveNotifier(FStorageNotifierId);
  if FIDENotifier > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FIDENotifier);

  if FProjectMenuNoftifierId > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FProjectMenuNoftifierId);

  {$IF CompilerVersion >= 32.0}
  if FThemeChangeNotifierId > -1 then
    (BorlandIDEServices as IOTAIDEThemingServices).RemoveNotifier(FThemeChangeNotifierId);
  {$IFEND}

  FDPMIDEMessageService.ShutDown;
  FEditorViewManager.Destroyed; //don't try to resolve this here, errors in the rtl on 10.2
  FEditorViewManager := nil;

end;

procedure TDPMWizard.DPMActionUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := TToolsApiUtils.IsProjectAvailable;
end;

procedure TDPMWizard.DPMMenuClick(Sender: TObject);
begin
  FEditorViewManager.ShowViewForProject(TToolsApiUtils.GetMainProjectGroup,TToolsApiUtils.GetActiveProject);
end;

procedure TDPMWizard.Execute;
begin
end;

function TDPMWizard.GetIDString : string;
begin
  result := 'DPM.IDE';
end;

function TDPMWizard.GetName : string;
begin
  result := 'DPM';
end;

function TDPMWizard.GetState : TWizardState;
begin
  result := [wsEnabled];
end;

procedure TDPMWizard.Modified;
begin

end;

end.

