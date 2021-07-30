unit DPM.IDE.BaseEditViewFrame;

interface

uses
  ToolsApi,
  Spring.Container,
  Vcl.Forms,
  DPM.IDE.ProjectTreeManager;

type
  TDPMBaseEditViewFrame = class(TFrame)
  public
    procedure Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);virtual;abstract;
    procedure ViewSelected;virtual;abstract;
    procedure ViewDeselected;virtual;abstract;
    procedure Closing;virtual;abstract;
    procedure ProjectReloaded;virtual;abstract;
    procedure ThemeChanged;virtual;abstract;
  end;

implementation
{$R *.dfm}

end.
