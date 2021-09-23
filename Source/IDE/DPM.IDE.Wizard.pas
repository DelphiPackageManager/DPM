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

unit DPM.IDE.Wizard;

interface

uses
  ToolsApi,
  Vcl.ActnList,
  Vcl.ImgList,
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

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  VCL.Dialogs,
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
  DPM.IDE.InstallerContext;

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

