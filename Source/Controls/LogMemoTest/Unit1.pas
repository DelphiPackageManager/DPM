unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DPM.Controls.LogMemo, Vcl.StdCtrls;

type
  //Mirrors how the IDE hosts the log : a top level popup OWNED by the main form (PopupParent,
  //not Parent) and WS_EX_TOPMOST. A plain TForm.CreateNew is none of those things, which is why
  //the first version of this harness could not reproduce the IDE's blank-log bug.
  TBlockedPopupForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TForm1 = class(TForm)
    btnBlockedOp: TButton;
    btnBlockedPopup: TButton;
    btnLoadStyle: TButton;
    dlgOpenStyle: TOpenDialog;
    btnPopupNoRedraw: TButton;
    btnPopupNoLogging: TButton;
    btnPartialInvalidate: TButton;
    btnReshowCycles: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBlockedOpClick(Sender: TObject);
    procedure btnBlockedPopupClick(Sender: TObject);
    procedure btnLoadStyleClick(Sender: TObject);
    procedure btnPopupNoRedrawClick(Sender: TObject);
    procedure btnPopupNoLoggingClick(Sender: TObject);
    procedure btnPartialInvalidateClick(Sender: TObject);
    procedure btnReshowCyclesClick(Sender: TObject);
  private
    { Private declarations }
    FLogMemo : TLogMemo;
    //Kept alive across cycles, exactly like the IDE's message form - created once, then
    //hidden and re-shown per task.
    FPersistentPopup : TForm;
    FPersistentMemo : TLogMemo;
    procedure RunBlockedLogLoop(const logMemo : TLogMemo; const iterations : integer);
    function CreatePopup(out popupMemo : TLogMemo) : TForm;
    procedure ScribbleClient(const logMemo : TLogMemo);
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
  FLogMemo.Top := 72;
  FLogMemo.Left := 10;
  FLogMemo.Width := Self.ClientWidth - 20;
  FLogMemo.Height := Self.ClientHeight - 82;
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

procedure TBlockedPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

function TForm1.CreatePopup(out popupMemo : TLogMemo) : TForm;
begin
  result := TBlockedPopupForm.CreateNew(Self);
  result.Width := 700;
  result.Height := 400;
  result.Position := poOwnerFormCenter;
  //Same ownership the IDE message service sets up - an owned top level window, not a child.
  result.PopupMode := pmExplicit;
  result.PopupParent := Self;
  popupMemo := TLogMemo.Create(result);
  popupMemo.Align := alClient;
  popupMemo.StyleServices := Vcl.Themes.StyleServices;
  popupMemo.Parent := result;
end;

procedure TForm1.btnBlockedOpClick(Sender: TObject);
begin
  RunBlockedLogLoop(FLogMemo, 100);
end;

//Reproduces the IDE message-form scenario : a freshly created window is shown and the
//"operation" starts immediately with no pumping - historically the window stayed blank
//white until resized. The RedrawWindow after Show mirrors what the IDE message service
//used to do. Run this both unstyled and with a VCL style loaded (Load VCL style... button)
//- the styled case mirrors the themed IDE.
procedure TForm1.btnBlockedPopupClick(Sender: TObject);
var
  popup : TForm;
  popupMemo : TLogMemo;
begin
  popup := CreatePopup(popupMemo);
  try
    popup.Caption := 'Blocked op popup (with redraw)';
    popup.Show;
    RedrawWindow(popup.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or RDW_ALLCHILDREN or RDW_UPDATENOW);
    RunBlockedLogLoop(popupMemo, 100);
    Sleep(1000); //leave the final state visible briefly before the window closes.
  finally
    popup.Free;
  end;
end;

//The real test. No RedrawWindow after Show, no pumping anywhere - the window must come up
//already showing its themed background and border purely because TLogMemo painted itself
//when it actually became visible. Any white here is the bug.
procedure TForm1.btnPopupNoRedrawClick(Sender: TObject);
var
  popup : TForm;
  popupMemo : TLogMemo;
begin
  popup := CreatePopup(popupMemo);
  try
    popup.Caption := 'Blocked op popup (no redraw)';
    popup.Show;
    RunBlockedLogLoop(popupMemo, 100);
    Sleep(1000);
  finally
    popup.Free;
  end;
end;

//Show a window and log NOTHING. There is no AddRow to trigger a paint and no message loop to
//deliver one, so the background and border have to come from the show path alone.
procedure TForm1.btnPopupNoLoggingClick(Sender: TObject);
var
  popup : TForm;
  popupMemo : TLogMemo;
begin
  popup := CreatePopup(popupMemo);
  try
    popup.Caption := 'Shown, nothing logged';
    popup.Show;
    Sleep(2000); //blocked main thread - nothing can paint but the show path itself.
  finally
    popup.Free;
  end;
end;

//Paints the whole client magenta behind the control's back, so anything the control fails to
//repaint stays obviously wrong.
procedure TForm1.ScribbleClient(const logMemo : TLogMemo);
var
  dc : HDC;
  brush : HBRUSH;
  r : TRect;
begin
  dc := GetDC(logMemo.Handle);
  if dc = 0 then
    exit;
  try
    Winapi.Windows.GetClientRect(logMemo.Handle, r);
    brush := CreateSolidBrush(RGB(255, 0, 255));
    try
      FillRect(dc, r, brush);
    finally
      DeleteObject(brush);
    end;
  finally
    ReleaseDC(logMemo.Handle, dc);
  end;
end;

//Regression test for the latch that made the control blank white forever : a PARTIAL WM_PAINT
//(here a 20x20 corner, in real life the strip exposed when a styled scrollbar appears) used to
//clear FContentDirty and set FPainted even though the BeginPaint DC was clipped to that corner,
//so the rest of the client kept whatever undefined content it had and no later Flush would ever
//repaint it. After this runs the WHOLE client must be correct - any magenta left is the bug.
procedure TForm1.btnPartialInvalidateClick(Sender: TObject);
var
  smallRect : TRect;
begin
  ScribbleClient(FLogMemo);
  Sleep(400); //long enough to see the magenta

  smallRect := Rect(0, 0, 20, 20);
  InvalidateRect(FLogMemo.Handle, @smallRect, False);
  UpdateWindow(FLogMemo.Handle); //partial WM_PAINT - only the corner may be repainted

  FLogMemo.AddRow('partial invalidate test - the whole client must be repainted', TLogMessageType.mtImportantSuccess);
  FLogMemo.Flush;
end;

//The IDE case that actually fails : ONE window, created once, then hidden and re-shown for each
//task - with the log cleared on hide, as TDPMMessageForm.FormHide does. In the IDE the first
//show paints fine and every show after it stays blank until the window is resized, so this runs
//several cycles. Watch cycles 2 and 3.
procedure TForm1.btnReshowCyclesClick(Sender: TObject);
var
  cycle : integer;
begin
  if FPersistentPopup = nil then
  begin
    FPersistentPopup := CreatePopup(FPersistentMemo);
    FPersistentPopup.Caption := 'Re-show cycles (watch cycle 2 onwards)';
  end;

  for cycle := 1 to 3 do
  begin
    //Same order as TDPMIDEMessageService.TaskStarted : clear, then show, then log with the
    //main thread blocked. Clear(False) because we are on our way to hiding - see TLogMemo.Clear.
    FPersistentMemo.Clear(False);
    FPersistentPopup.Hide;

    FPersistentPopup.Show;
    RunBlockedLogLoop(FPersistentMemo, 20);
    Sleep(800); //leave the result on screen long enough to see whether rows are there
  end;
  FPersistentMemo.Clear(False);
  FPersistentPopup.Hide;
end;

procedure TForm1.btnLoadStyleClick(Sender: TObject);
begin
  //Styles ship with the IDE, eg C:\Program Files (x86)\Embarcadero\Studio\23.0\Redist\styles\vcl
  if dlgOpenStyle.Execute then
    TStyleManager.SetStyle(TStyleManager.LoadFromFile(dlgOpenStyle.FileName));
end;

end.
