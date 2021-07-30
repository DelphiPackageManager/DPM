unit DPM.IDE.GroupEditorViewFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  ToolsApi,
  Spring.Container,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.BaseEditViewFrame;

type
  TDPMGroupEditViewFrame = class(TDPMBaseEditViewFrame)
  private
  public
    procedure Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);override;
    procedure ViewSelected;override;
    procedure ViewDeselected;override;
    procedure Closing;override;
    procedure ProjectReloaded;override;
    procedure ThemeChanged;override;
  end;

implementation

{$R *.dfm}

{ TDPMGroupEditViewFrame }

procedure TDPMGroupEditViewFrame.Closing;
begin

end;

procedure TDPMGroupEditViewFrame.Configure(const projectOrGroup: IOTAProject; const container: TContainer; const projectTreeManager: IDPMProjectTreeManager);
begin

end;

procedure TDPMGroupEditViewFrame.ProjectReloaded;
begin

end;

procedure TDPMGroupEditViewFrame.ThemeChanged;
begin

end;

procedure TDPMGroupEditViewFrame.ViewDeselected;
begin

end;

procedure TDPMGroupEditViewFrame.ViewSelected;
begin

end;

end.
