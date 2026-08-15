unit DPM.Creator.BuildForm;

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
  Vcl.StdCtrls
  ;

type
  TBuildForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    lblProject: TLabel;
    edtProject: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; isDesign : boolean = false);reintroduce;
  end;

implementation

{$R *.dfm}

procedure TBuildForm.btnCancelClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TBuildForm.btnOkClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

constructor TBuildForm.Create(AOwner: TComponent; isDesign: boolean);
begin
  inherited Create(AOwner);
  if isDesign then
    lblProject.Caption := 'Project (.dproj) or precompiled (.bpl)';
end;

end.
