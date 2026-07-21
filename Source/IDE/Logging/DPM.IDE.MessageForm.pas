unit DPM.IDE.MessageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.Diagnostics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  VSoft.Awaitable,
  DPM.IDE.Options,
  DPM.Controls.LogMemo,
  ToolsAPI, System.Actions;

{$I ..\DPMIDE.inc}

type
  TDPMMessageForm = class(TForm {$IFDEF THEMESERVICES}, INTAIDEThemingServicesNotifier {$ENDIF})
    btnCancel: TButton;
    ActionList1: TActionList;
    actCanCancel: TAction;
    btnCopy: TButton;
    actCopyLog: TAction;
    btnClose: TButton;
    ClosingInTimer: TTimer;
    lblClosing: TLabel;
    lblDontClose: TLinkLabel;
    Panel1: TPanel;
    procedure actCanCancelExecute(Sender: TObject);
    procedure actCopyLogExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure ClosingInTimerTimer(Sender: TObject);
    procedure lblDontCloseLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FOptions : IDPMIDEOptions;
    FLogMemo : TLogMemo;
    FCancellationTokenSource : ICancellationTokenSource;
    FCloseDelayInSeconds : integer;
    FCurrentCloseDelay : integer;
    FStopwatch : TStopwatch;
    //True between TaskStarted and TaskDone. While set, the Close button and the window's
    //system-menu close are blocked so the user can't dismiss the log (and lose sight of a
    //running operation) without going through Cancel. Driven explicitly by the message
    //service rather than inferred from FCancellationTokenSource, which TaskDone does not
    //always clear (e.g. success with auto-close disabled).
    FTaskRunning : boolean;
    {$IFDEF THEMESERVICES}
    FNotifierId : integer;
    {$ENDIF}
    //Background brush for the TDPMMessageForm window CLASS - see CreateParams for the reasoning
    //and the lifetime rules. Created once, never deleted or replaced.
    class var FClassBackgroundBrush : HBRUSH;

    procedure SetCancellationTokenSource(const Value: ICancellationTokenSource);
    procedure SetCloseDelayInSeconds(const Value: integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

    //INTAIDEThemingServicesNotifier
    procedure ChangingTheme;
    procedure ChangedTheme;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    procedure ProcessMessages;

  public
    constructor Create(AOwner : TComponent; const options : IDPMIDEOptions);reintroduce;
    destructor Destroy; override;

    //The IDE registers global menu shortcuts (Edit|Select All = Ctrl+A, Copy = Ctrl+C). Because
    //this form is modeless and owned by the IDE, TApplication.IsShortCut lets the IDE main form
    //consume those keys before they are ever dispatched to the focused log control - so the memo's
    //own KeyDown (which handles Ctrl+A/Ctrl+C) never fires. Screen.ActiveCustomForm.IsShortCut is
    //checked before MainForm.IsShortCut, so we grab them here while the memo has focus.
    function IsShortCut(var Message: TWMKey): Boolean; override;

    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;

    //Paints the whole window tree synchronously, with no dependency on a message loop. The IDE
    //does not pump while a restore/install runs, so nothing here may rely on WM_PAINT arriving.
    procedure PaintNow;

    procedure DelayHide;

    //Called by the message service to mark a task as running / finished. While running the
    //Close button and the window close box are disabled.
    procedure SetTaskRunning(const value : boolean);

    property CancellationTokenSource : ICancellationTokenSource read FCancellationTokenSource write SetCancellationTokenSource;
    property CloseDelayInSeconds : integer read FCloseDelayInSeconds write SetCloseDelayInSeconds;
  end;

implementation

uses
  Vcl.Themes,
  Vcl.clipbrd,
  DPM.IDE.ToolsAPI;

{$R *.dfm}

//Windows replaces the top level windows of a GUI thread that has stopped servicing its message
//queue with GHOST windows - a frozen copy with "(Not Responding)" appended. Detection kicks in
//after a few seconds without the thread calling GetMessage/PeekMessage on its whole queue, which
//is exactly what this plugin does : the core runs restore/install synchronously on the IDE main
//thread, and the pump below only PeekMessages specific child HWNDs.
//Ghosting was NOT the cause of the blank log window (that was a latched flag in TLogMemo), so this
//is not load bearing - but a ghosted log window is still wrong, so it stays.
//DisableProcessWindowsGhosting is not declared in Winapi.Windows, so it is bound dynamically - it
//has existed since XP, but a missing export must never stop the plugin loading.
procedure DisableWindowsGhosting;
type
  TDisableProcessWindowsGhosting = procedure; stdcall;
var
  userHandle : HMODULE;
  proc : TDisableProcessWindowsGhosting;
begin
  userHandle := GetModuleHandle('user32.dll');
  if userHandle = 0 then
    exit;
  proc := GetProcAddress(userHandle, 'DisableProcessWindowsGhosting');
  if Assigned(proc) then
    proc;
end;


{ TDPMMessageForm }

procedure TDPMMessageForm.actCanCancelExecute(Sender: TObject);
var
  tokenSource : ICancellationTokenSource;
begin
  tokenSource := FCancellationTokenSource;
  if tokenSource <> nil then
    tokenSource.Cancel;
end;

procedure TDPMMessageForm.actCopyLogExecute(Sender: TObject);
begin
  Clipboard.AsText := FLogMemo.Text;
end;

procedure TDPMMessageForm.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actCopyLog.Enabled := FLogMemo.RowCount > 0;
  actCanCancel.Enabled := (FCancellationTokenSource <> nil) and (not FCancellationTokenSource.Token.IsCancelled);
  //Can't close the log while work is in flight - the user must Cancel first.
  btnClose.Enabled := not FTaskRunning;
  Handled := true;
end;

procedure TDPMMessageForm.AfterSave;
begin

end;

procedure TDPMMessageForm.BeforeSave;
begin

end;

procedure TDPMMessageForm.btnCloseClick(Sender: TObject);
begin
  //Defensive: the button is disabled while a task runs, but guard anyway so the log can't
  //be hidden out from under an in-flight operation.
  if FTaskRunning then
    exit;
  ClosingInTimer.Enabled := false;
  FCurrentCloseDelay := FCloseDelayInSeconds;
  Self.Hide;
end;

procedure TDPMMessageForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //Block the system-menu close box (and Alt+F4) while a task is running, same as the
  //Close button. Cancel the task first.
  CanClose := not FTaskRunning;
end;

procedure TDPMMessageForm.ChangedTheme;
{$IFDEF THEMESERVICES}
var
  ideThemeSvc : IOTAIDEThemingServices;
{$ENDIF}
begin
  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  if ideThemeSvc.IDEThemingEnabled then
    ideThemeSvc.ApplyTheme(Self);
  FLogMemo.StyleServices := ideThemeSvc.StyleServices;
  {$ENDIF}
end;

procedure TDPMMessageForm.ChangingTheme;
begin

end;


procedure TDPMMessageForm.Clear;
begin
  FLogMemo.Clear;
  FCurrentCloseDelay := FCloseDelayInSeconds;
  lblClosing.Visible := false;
  lblDontClose.Visible := false;
  PaintNow;
end;

procedure TDPMMessageForm.ClosingInTimerTimer(Sender: TObject);
begin
  ClosingInTimer.Enabled := false;
  Dec(FCurrentCloseDelay);
  if FCurrentCloseDelay > 0  then
  begin
    lblClosing.Caption := 'Closing in ' + IntToStr(FCurrentCloseDelay) + ' seconds' + StringOfChar('.', FCurrentCloseDelay);
    ClosingInTimer.Enabled := true;
  end
  else
  begin
    FCurrentCloseDelay := FCloseDelayInSeconds;
    Self.Hide;
  end;
end;


procedure TDPMMessageForm.CMStyleChanged(var Message: TMessage);
begin

  inherited;
end;

procedure TDPMMessageForm.CMShowingChanged(var Message: TMessage);
begin
  inherited; //TCustomForm.CMShowingChanged - ends with ShowWindow(Handle, ...)
  //Only now is IsWindowVisible true for us and for every child, so this is the earliest moment a
  //paint can actually produce pixels. Everything the controls attempted earlier in the show
  //cascade (their own CM_SHOWINGCHANGED fires from TWinControl.UpdateShowing, before the line
  //above) was silently discarded because we were still hidden. Painting here needs no message
  //loop, which is the whole point - the caller may go straight into a synchronous restore.
  //Reset first, unconditionally, on BOTH transitions : the memo cannot observe a hide at all (see
  //TLogMemo.ResetPaintState), so this is the only place its per-visibility state and its
  //re-entrancy latches are guaranteed to be cleared. Doing it on show as well means a run that
  //somehow latched state cannot poison the next invocation - which is exactly the failure where
  //the log window stayed blank for every subsequent restore once it had failed once.
  if FLogMemo <> nil then
    FLogMemo.ResetPaintState;
  if Showing then
    PaintNow;
end;

procedure TDPMMessageForm.PaintNow;
begin
  if not HandleAllocated then
    exit;
  if not IsWindowVisible(Handle) then
    exit;
  //Form background, panel, buttons, labels. RDW_UPDATENOW SENDS WM_NCPAINT/WM_ERASEBKGND/WM_PAINT
  //rather than queueing them, so this completes without the message loop running.
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
  //The memo owns its entire client surface via its own back buffer - force it explicitly rather
  //than relying on the RDW_ALLCHILDREN WM_PAINT above, so it also clears its own painted/dirty
  //latches and repaints its border and scrollbars.
  FLogMemo.PaintFrameNow;
  FLogMemo.PaintNow;
end;

constructor TDPMMessageForm.Create(AOwner: TComponent; const options : IDPMIDEOptions);
var
  {$IFDEF THEMESERVICES}
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
  IDEStyleServices : TCustomStyleServices;
begin
  inherited Create(AOwner);
  //Must happen before we ever show a window from a thread that is about to block - see the
  //comment on DisableWindowsGhosting.
  DisableWindowsGhosting;
  FOptions := options;
  Self.Width := FOptions.LogWindowWidth;
  Self.Height := FOptions.LogWindowHeight;
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}

  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  if ideThemeSvc.IDEThemingEnabled then
    ideThemeSvc.ApplyTheme(Self);
  FNotifierId := ideThemeSvc.AddNotifier(Self);
  IDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  IDEStyleServices := Vcl.Themes.StyleServices;
  {$ENDIF}


  FLogMemo := TLogMemo.Create(Self);
  FLogMemo.TabOrder := 0;
  FLogMemo.TabStop := true;
  FLogMemo.Align := alClient;
  FLogMemo.StyleServices := IDEStyleServices;
  FLogMemo.Clear;
  FLogMemo.Parent := Self;
  // Keep TLogMemo's monospaced font
  FLogMemo.Font.Size := Self.Font.Size + 2;
  Self.ActiveControl := btnCancel;

  FCloseDelayInSeconds := 3;
  FCurrentCloseDelay := FCloseDelayInSeconds;

  FTaskRunning := false;
  Self.OnCloseQuery := FormCloseQuery;

  FStopwatch := TStopwatch.Create;
end;

procedure TDPMMessageForm.CreateParams(var Params: TCreateParams);
var
  brushColor : TColor;
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;

  //Kill the white flash on the first show. This is a TOP LEVEL window concern, not a control one :
  //child windows share the top level window's DWM redirection surface, and that surface is white
  //when the compositor creates it. The VCL registers every window class with hbrBackground = 0
  //(Vcl.Controls.pas:10387), so nothing ever fills it, and the white stays visible until the app
  //composes its first frame. Giving the CLASS a brush is what colours that very first frame -
  //this is not the WM_ERASEBKGND path (TWinControl.WMEraseBkgnd intercepts that and the class
  //brush is never consulted there), it is the compositor's initial fill.
  //LIFETIME : RegisterClass runs once per class name for the life of the module and the class then
  //owns the handle, so this is created once and never deleted or replaced. The colour is therefore
  //latched to whatever the IDE theme was on first show - a stale shade on the single pre-paint
  //frame is invisible next to the white it replaces, whereas deleting a brush the class still
  //references crashes the IDE.
  //clWindow, not clBtnFace : the log memo is alClient and covers all but the ~49px button strip,
  //so matching the memo's background is what makes the pre-paint frame invisible. The colour comes
  //from the memo's style services (the IDE theme, assigned in the constructor) rather than the
  //global Vcl.Themes.StyleServices, which is not the IDE's.
  if FClassBackgroundBrush = 0 then
  begin
    if (FLogMemo <> nil) and (FLogMemo.StyleServices <> nil) and FLogMemo.StyleServices.Enabled then
      brushColor := FLogMemo.StyleServices.GetSystemColor(clWindow)
    else
      brushColor := clWindow;
    FClassBackgroundBrush := CreateSolidBrush(ColorToRGB(brushColor));
  end;
  Params.WindowClass.hbrBackground := FClassBackgroundBrush;
end;

procedure TDPMMessageForm.Debug(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtDebug);
  Self.ProcessMessages;
end;

procedure TDPMMessageForm.DelayHide;
begin
  ClosingInTimer.Enabled := true;
  lblClosing.Caption := 'Closing in ' + IntToStr(FCurrentCloseDelay) + ' seconds' + StringOfChar('.', FCurrentCloseDelay);
  lblDontClose.Left := lblClosing.Left +  lblClosing.Width + 30;
  lblClosing.Visible := true;
  lblDontClose.Visible := true;
  lblClosing.Update;
end;

destructor TDPMMessageForm.Destroy;
  {$IFDEF THEMESERVICES}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
begin
  FOptions.LogWindowWidth := Self.Width;
  FOptions.LogWindowHeight := Self.Height;
  FOptions.SaveToFile();

  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.RemoveNotifier(FNotifierId);
  {$ENDIF}
  inherited;
end;

procedure TDPMMessageForm.Destroyed;
begin

end;

procedure TDPMMessageForm.Error(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtError);
  Self.ProcessMessages;
end;


procedure TDPMMessageForm.FormHide(Sender: TObject);
begin
  //OnHide runs from DoHide inside TCustomForm.CMShowingChanged, BEFORE its ShowWindow(SW_HIDE) -
  //we are still on screen, so clear WITHOUT repainting, or the emptied log is drawn over the
  //window the user is still looking at.
  FLogMemo.Clear(False);
end;

procedure TDPMMessageForm.FormShow(Sender: TObject);
begin
  //Nothing to do here. This fires from TCustomForm.CMShowingChanged's DoShow, which runs BEFORE
  //that method's ShowWindow - so we are still hidden and no paint is possible yet. The
  //Application.ProcessMessages that used to live here could never have served the first paint;
  //all it did was pump the IDE queue from inside a notifier, mid show cascade. CMShowingChanged
  //does the painting, after inherited.
end;

procedure TDPMMessageForm.Information(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantInformation)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtInformation);
  Self.ProcessMessages;
end;

function TDPMMessageForm.IsShortCut(var Message: TWMKey): Boolean;
begin
  //Only steal the key when the log memo actually has focus - otherwise leave Ctrl+A/Ctrl+C to
  //their normal handling (e.g. focus on a button). GetKeyState is used because the Ctrl modifier
  //is not carried in the WM_KEYDOWN KeyData.
  result := false;
  if (ActiveControl = FLogMemo) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    case Message.CharCode of
      Ord('A') :
      begin
        FLogMemo.SelectAll;
        result := true;
      end;
      Ord('C') :
      begin
        FLogMemo.CopyToClipboard;
        result := true;
      end;
    end;
    if result then
      exit;
  end;
  result := inherited IsShortCut(Message);
end;

procedure TDPMMessageForm.lblDontCloseLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ClosingInTimer.Enabled := false;
  lblClosing.Visible := false;
  lblDontClose.Visible := false;
  FCurrentCloseDelay := FCloseDelayInSeconds;
end;

procedure TDPMMessageForm.Modified;
begin

end;

procedure TDPMMessageForm.NewLine;
begin
  FLogMemo.AddRow('',mtInformation);
  Self.ProcessMessages;
end;

// The core runs restore/install synchronously on the IDE main thread, so the IDE message loop is
// not pumped while a task runs. Painting no longer depends on that loop at all - TLogMemo.Flush
// renders straight through a DC it fetches itself - so all this has to do is keep the window's
// INPUT alive. Only messages already queued for the log window's own controls are dispatched.
// Posted IDE messages and input for IDE windows stay queued, so nothing re-enters project loading
// or the IDE notifiers mid-operation - the re-entrance that produced paint artifacts on the IDE
// main window and churned resources (a 'Not enough timers available' failure) when this pumped
// the whole thread queue.
procedure TDPMMessageForm.ProcessMessages;
var
  msg : TMsg;

  //Takes the control, not a handle - reading .Handle would force handle creation, and
  //creating child handles outside the form's own Show cascade breaks that cascade.
  procedure PumpControlInput(const ctrl : TWinControl);
  begin
    if (ctrl = nil) or (not ctrl.HandleAllocated) then
      exit;
    while PeekMessage(msg, ctrl.Handle, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;

begin
  //AddRow only marked the control dirty (cheap, coalesced). The synchronous repaint stays
  //throttled to ~10ms so rapid logging is smooth and cheap - one back-buffer blit per frame
  //rather than one per line.
  if (not FStopwatch.IsRunning) or (FStopwatch.ElapsedMilliseconds > 10) then
  begin
    FLogMemo.Flush; //synchronous, own-handle paint - does not depend on the IDE pumping WM_PAINT.
    FStopwatch.Reset; //Reset stops + zeroes
    FStopwatch.Start;
  end;

  //Input only, and unthrottled - when nothing is queued it is just a handful of PeekMessage calls
  //returning false. There is no paint pumping here any more : Flush above paints the client
  //directly, and TLogMemo repaints its border and the style hook's scrollbar overlay windows
  //itself, synchronously, whenever a scrollbar appears or disappears (TLogMemo.UpdateScrollBars
  //-> PaintFrameNow). That is what the EnumChildWindows paint pump used to be covering.
  if HandleAllocated and Showing then
  begin
    //Button clicks: WM_LBUTTONDOWN/UP are posted to the button hwnd (dispatched here); the
    //resulting BN_CLICKED arrives via a SENT WM_COMMAND, which needs no pumping. This is
    //what keeps Cancel responsive during an operation.
    //Deliberately NOT pumping the form's non-client input - dragging the caption would enter
    //DefWindowProc's modal move loop, which pumps the whole thread queue again.
    PumpControlInput(btnCancel);
    PumpControlInput(btnCopy);
    PumpControlInput(FLogMemo); //scroll/selection - handlers are self contained.
  end;
end;

procedure TDPMMessageForm.SetCancellationTokenSource(const Value: ICancellationTokenSource);
begin
  FCancellationTokenSource := Value;
  actCanCancel.Update;
end;

procedure TDPMMessageForm.SetCloseDelayInSeconds(const Value: integer);
begin
  FCloseDelayInSeconds := Value;
  FCurrentCloseDelay := FCloseDelayInSeconds;
end;

procedure TDPMMessageForm.SetTaskRunning(const value : boolean);
begin
  FTaskRunning := value;
  //Reflect immediately rather than waiting for the next ActionList idle update.
  btnClose.Enabled := not value;
  if value then
    //New task starting - reset the throttle so the very first log line of this task
    //forces an immediate synchronous flush + pump (otherwise a stale running stopwatch
    //left over from the previous task can throttle the first line out, leaving the
    //freshly shown window blank until a later line falls outside the 10ms window).
    FStopwatch.Reset
  else
    //Task finished - force out any lines logged inside the last throttle window so the final
    //output is visible immediately rather than waiting for the IDE loop to resume.
    FLogMemo.Flush;
end;

procedure TDPMMessageForm.Success(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantSuccess)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtSuccess);
  Self.ProcessMessages;
end;

procedure TDPMMessageForm.Verbose(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantVerbose)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtVerbose);
  Self.ProcessMessages;
end;

procedure TDPMMessageForm.Warning(const data: string; const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantWarning)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtWarning);
  Self.ProcessMessages;
end;

initialization

  TToolsApiUtils.RegisterFormClassForTheming(TDPMMessageForm);


end.
