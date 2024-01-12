unit DPM.Creator.RuntimeForm;

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
  TBplForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    chkCopyLocal: TCheckBox;
    lblRuntimeSrc: TLabel;
    edtSource: TEdit;
    lblRuntimeBuildId: TLabel;
    edtBuildId: TEdit;
    chkInstall: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TBplForm.btnCancelClick(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TBplForm.btnOkClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

end.
