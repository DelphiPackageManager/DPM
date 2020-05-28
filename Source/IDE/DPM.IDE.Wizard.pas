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
  Spring.Container,
  DPM.Core.Logging,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.Controls,
  DPM.IDE.EditorViewManager;

type
  TDPMWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard)
  private
    //FStorageNotifier : integer;
    FIDENotifier : integer;
    FProjectMenuNoftifierId : integer;
    FLogger : ILogger;
    FContainer : TContainer;
    FEditorViewManager : IDPMEditorViewManager;

    procedure InitContainer;
  protected

    //IOTAWizard
    procedure Execute;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

  public
    constructor Create;
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  VCL.Dialogs,
  DPM.IDE.Logger,
  DPM.Core.Init,
  DPM.IDE.ProjectStorageNotifier,
  DPM.IDE.IDENotifier,
  DPM.IDE.ProjectMenu,
  DPM.Core.Package.Interfaces,
  DPM.IDE.AddInOptions;

{$R DPM.IDE.Resources.res}
{ TDPMWizard }

procedure TDPMWizard.InitContainer;
begin
  try
    FContainer := TContainer.Create;
    FContainer.RegisterInstance<ILogger>(FLogger).AsSingleton();
    DPM.Core.Init.InitCore(FContainer);
    FContainer.Build;
  except
    on e : Exception do
    begin

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
  //storageNotifier : IOTAProjectFileStorageNotifier;
  ideNotifier : IOTAIDENotifier;
  packageInstaller : IPackageInstaller;
  projMenuNotifier : IOTAProjectMenuItemCreatorNotifier;
  options : INTAAddInOptions;
begin
  FLogger := TDPMIDELogger.Create;
  InitContainer;

  packageInstaller := FContainer.Resolve<IPackageInstaller>;
  FEditorViewManager := TDPMEditorViewManager.Create(FContainer);

  ideNotifier := TDPMIDENotifier.Create(FLogger as IDPMIDELogger, packageInstaller, FEditorViewManager );
  FIDENotifier := (BorlandIDEServices as IOTAServices).AddNotifier(ideNotifier);


  projMenuNotifier := TDPMProjectMenuNotifier.Create(FEditorViewManager);
  FProjectMenuNoftifierId := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(projMenuNotifier);

  options := TDPMAddinOptions.Create(FContainer);

 (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(options);


  //this didn't work, leaving here as we may need it to try and detect reloads.
//  storageNotifier := TDPMProjectStorageNotifier.Create(FLogger as IDPMIDELogger);
//  FStorageNotifier := (BorlandIDEServices As IOTAProjectFileStorage).AddNotifier(storageNotifier);

end;

destructor TDPMWizard.Destroy;
begin
////  If FStorageNotifier > -1 then
////    (BorlandIDEServices As IOTAProjectFileStorage).RemoveNotifier(FStorageNotifier);
//  if FIDENotifier > -1 then
//    (BorlandIDEServices as IOTAServices).RemoveNotifier(FIDENotifier);
//
//  if FProjectMenuNoftifierId > -1 then
//    (BorlandIDEServices as IOTAServices).RemoveNotifier(FProjectMenuNoftifierId);

  inherited;
end;

procedure TDPMWizard.Destroyed;
begin
  FEditorViewManager.Destroyed;
//  If FStorageNotifier > -1 then
//    (BorlandIDEServices As IOTAProjectFileStorage).RemoveNotifier(FStorageNotifier);
  if FIDENotifier > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FIDENotifier);

  if FProjectMenuNoftifierId > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FProjectMenuNoftifierId);

end;

procedure TDPMWizard.Execute;
begin
end;

function TDPMWizard.GetIDString: string;
begin
  result := 'DPM.IDE';
end;

function TDPMWizard.GetName: string;
begin
  result := 'DPM';
end;

function TDPMWizard.GetState: TWizardState;
begin
  result := [wsEnabled];
end;

procedure TDPMWizard.Modified;
begin

end;

end.
