unit DPM.IDE.MessageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDPMMessageForm = class(TForm)
    LogMemo: TMemo;
    Cancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  protected
    procedure CreateParams(var Params: TCreateParams); override;

  public
    { Public declarations }
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;
  end;

var
  DPMMessageForm: TDPMMessageForm;

implementation


{$R *.dfm}

{ TDPMMessageForm }


{ TDPMMessageForm }

procedure TDPMMessageForm.Clear;
begin
  LogMemo.Clear;
end;

procedure TDPMMessageForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TDPMMessageForm.Debug(const data: string);
begin
  LogMemo.Lines.Add(data);
end;

procedure TDPMMessageForm.Error(const data: string);
begin
  LogMemo.Lines.Add(data);
end;

procedure TDPMMessageForm.FormCreate(Sender: TObject);
begin
  LogMemo.Clear;
end;

procedure TDPMMessageForm.Information(const data: string;  const important: Boolean);
begin
  LogMemo.Lines.Add(data);
end;

procedure TDPMMessageForm.NewLine;
begin
  LogMemo.Lines.Add('');
end;

procedure TDPMMessageForm.Success(const data: string;  const important: Boolean);
begin
  LogMemo.Lines.Add(data);
end;

procedure TDPMMessageForm.Verbose(const data: string;  const important: Boolean);
begin
  LogMemo.Lines.Add(data);
end;

procedure TDPMMessageForm.Warning(const data: string; const important: Boolean);
begin
  LogMemo.Lines.Add(data);
end;

end.
