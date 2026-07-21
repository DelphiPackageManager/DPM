unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DPM.Controls.LogMemo, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnBlockedOp: TButton;
    btnBlockedPopup: TButton;
    btnLoadStyle: TButton;
    dlgOpenStyle: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnBlockedOpClick(Sender: TObject);
    procedure btnBlockedPopupClick(Sender: TObject);
    procedure btnLoadStyleClick(Sender: TObject);
  private
    { Private declarations }
    FLogMemo : TLogMemo;
    procedure RunBlockedLogLoop(const logMemo : TLogMemo; const iterations : integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

const
  //cycle through message types so the colours are exercised too.
  MessageTypes : array[0..4] of TLogMessageType = (mtInformation, mtSuccess, mtWarning, mtError, mtDebug);

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FLogMemo := TLogMemo.Create(Self);
  FLogMemo.Top := 40;
  FLogMemo.Left := 10;
  FLogMemo.Width := Self.ClientWidth - 20;
  FLogMemo.Height := Self.ClientHeight - 50;
  FLogMemo.Anchors := [TAnchorKind.akLeft,TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
  FLogMemo.Parent := Self;
  for i := 0 to 50 do
    FLogMemo.AddRow( IntToStr(i) + '  This is a test xxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxx dxxxxxxxxxxxxxxxxxxxxxx', TLogMessageType.mtInformation);
end;

//Simulates the IDE during a synchronous restore/install : the main thread never pumps
//messages, so the ONLY painting comes from TLogMemo.Flush. The log must be visible from
//the first line and update continuously for the whole loop - if it goes blank/white the
//blank-log-window bug has regressed.
procedure TForm1.RunBlockedLogLoop(const logMemo : TLogMemo; const iterations : integer);
var
  i : integer;
begin
  logMemo.Clear;
  for i := 1 to iterations do
  begin
    logMemo.AddRow(Format('%.3d  blocked main thread - this line was painted without any message pumping', [i]), MessageTypes[i mod Length(MessageTypes)]);
    logMemo.Flush;
    Sleep(50);
  end;
  logMemo.AddRow('done.', TLogMessageType.mtImportantSuccess);
  logMemo.Flush;
end;

procedure TForm1.btnBlockedOpClick(Sender: TObject);
begin
  RunBlockedLogLoop(FLogMemo, 100);
end;

//Reproduces the IDE message-form scenario : a freshly created window is shown and the
//"operation" starts immediately with no pumping - historically the window stayed blank
//white until resized. The RedrawWindow after Show mirrors what the IDE message service
//does (DPM.IDE.MessageService.ShowMessageWindow). Run this both unstyled and with a VCL
//style loaded (Load VCL style... button) - the styled case mirrors the themed IDE.
procedure TForm1.btnBlockedPopupClick(Sender: TObject);
var
  popup : TForm;
  popupMemo : TLogMemo;
begin
  popup := TForm.CreateNew(Self);
  try
    popup.Caption := 'Blocked op popup';
    popup.Width := 700;
    popup.Height := 400;
    popup.Position := poOwnerFormCenter;
    popupMemo := TLogMemo.Create(popup);
    popupMemo.Align := alClient;
    popupMemo.Parent := popup;
    popup.Show;
    RedrawWindow(popup.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or RDW_ALLCHILDREN or RDW_UPDATENOW);
    RunBlockedLogLoop(popupMemo, 100);
    Sleep(1000); //leave the final state visible briefly before the window closes.
  finally
    popup.Free;
  end;
end;

procedure TForm1.btnLoadStyleClick(Sender: TObject);
begin
  //Styles ship with the IDE, eg C:\Program Files (x86)\Embarcadero\Studio\23.0\Redist\styles\vcl
  if dlgOpenStyle.Execute then
    TStyleManager.SetStyle(TStyleManager.LoadFromFile(dlgOpenStyle.FileName));
end;

end.
