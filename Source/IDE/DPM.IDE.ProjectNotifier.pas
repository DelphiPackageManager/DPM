unit DPM.IDE.ProjectNotifier;

interface
uses
  System.Classes,
  ToolsApi,
  DPM.IDE.Types,
  DPM.IDE.Logger;

  //not used at the moment, but the plan is to use it to detect active platform changes.

type
  TDPMProjectNotifier = class(TInterfacedObject,IOTAProjectNotifier, IOTAModuleNotifier,IOTANotifier )
  private
    FLogger : IDPMIDELogger;
    FIDENotifier : IInterface;
    FFileName : string;
    FProject : IOTAProject;
    FCurrentPlatform : string;
  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;

    function CheckOverwrite: Boolean;
    procedure Modified;
    procedure ModuleRemoved(const AFileName: string);
    procedure ModuleAdded(const AFileName: string);
    procedure ModuleRenamedA(const AOldFileName, ANewFileName: string);
    procedure ModuleRenamed(const NewName: string);
    procedure IOTAProjectNotifier.ModuleRenamed = ModuleRenamedA;
  public
    constructor Create(const logger : IDPMIDELogger; const ideNotifier : IInterface; const fileName : string; const project : IOTAProject);
  end;
implementation

uses
  System.SysUtils,
  DPM.IDE.IDENotifier;

{ TDPMProjectNotifier }

procedure TDPMProjectNotifier.AfterSave;
begin

end;

procedure TDPMProjectNotifier.BeforeSave;
begin

end;

function TDPMProjectNotifier.CheckOverwrite: Boolean;
begin
  result := true;
end;

constructor TDPMProjectNotifier.Create(const logger: IDPMIDELogger; const ideNotifier : IInterface; const fileName : string; const project : IOTAProject);
begin
  FLogger := logger;
  FIDENotifier := ideNotifier;
  FFileName := fileName;
  FProject := project;
  FCurrentPlatform := FProject.CurrentPlatform;
end;

procedure TDPMProjectNotifier.Destroyed;
begin
  //Release all interface refs so the cycle through FIDENotifier (-> TDPMIDENotifier -> our dict)
  //can settle. FProject was provided by the IDE; FIDENotifier and FLogger are container singletons
  //but holding them past Destroyed serves no purpose.
  FProject := nil;
  FIDENotifier := nil;
  FLogger := nil;
end;

procedure TDPMProjectNotifier.Modified;
begin
  if FCurrentPlatform <> FProject.CurrentPlatform then
  begin
    FCurrentPlatform := FProject.CurrentPlatform;
    (FIDENotifier as IDPMIDENotifier).ProjectActivePlatformChanged(FCurrentPlatform);
  end;
end;

procedure TDPMProjectNotifier.ModuleAdded(const AFileName: string);
begin

end;

procedure TDPMProjectNotifier.ModuleRemoved(const AFileName: string);
begin

end;


procedure TDPMProjectNotifier.ModuleRenamedA(const AOldFileName, ANewFileName: string);
begin

end;

procedure TDPMProjectNotifier.ModuleRenamed(const NewName: string);
var
  oldFileName : string;
begin
  oldFileName := FFileName;
  FFileName := NewName;
  (FIDENotifier as IDPMIDENotifier).ProjectRenamed(oldFileName, newName);
end;

end.
