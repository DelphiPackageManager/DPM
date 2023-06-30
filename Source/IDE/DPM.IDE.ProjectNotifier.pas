unit DPM.IDE.ProjectNotifier;

interface
uses
  System.Classes,
  ToolsApi,
  DPM.IDE.Logger;

  //not used at the moment, but the plan is to use it to detect active platform changes.

type
  TDPMProjectNotifier = class(TInterfacedObject,IOTAProjectNotifier, IOTAModuleNotifier,IOTANotifier )
  private
    FLogger : IDPMIDELogger;
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
    constructor Create(const logger : IDPMIDELogger);
  end;
implementation

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

constructor TDPMProjectNotifier.Create(const logger: IDPMIDELogger);
begin
  FLogger := logger;
end;

procedure TDPMProjectNotifier.Destroyed;
begin

end;

procedure TDPMProjectNotifier.Modified;
begin

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
begin

end;

end.
