unit DPM.Creator.OptionsForm;

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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.IDE.AddInOptionsFrame,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Manager,
  DPM.IDE.Options
  ;

type

  TOptionsForm = class(TForm)
    DPMOptionsFrame: TDPMOptionsFrame;
    Panel1: TPanel;
    btnCancel: TButton;
    btnOk: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FConfigManager : IConfigurationManager;
    FLogger: ILogger;
    FIDEOptions : IDPMIDEOptions;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; logger: ILogger); reintroduce;
  end;

implementation

{$R *.dfm}

constructor TOptionsForm.Create(AOwner: TComponent; logger: ILogger);
begin
  inherited Create(AOwner);
  FLogger := logger;
  FIDEOptions := TDPMIDEOptions.Create;
  FIDEOptions.LoadFromFile();
end;

procedure TOptionsForm.btnCancelClick(Sender: TObject);
begin
  Close;
  Self.ModalResult := mrCancel;
end;

procedure TOptionsForm.btnOkClick(Sender: TObject);
begin
  if DPMOptionsFrame.Validate then
  begin
    DPMOptionsFrame.SaveSettings;
    Close;
    Self.ModalResult := mrOK;
  end;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  FConfigManager := TConfigurationManager.Create(FLogger);
  DPMOptionsFrame.Configure(FConfigManager, FIDEOptions, FLogger, '');
  DPMOptionsFrame.LoadSettings;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  DPMOptionsFrame.tsIDEOptions.Visible := False;
end;

end.
