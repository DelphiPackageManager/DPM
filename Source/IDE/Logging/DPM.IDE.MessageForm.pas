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
    //Window-class background brush matching the themed form colour. The OS prefills a newly
    //shown window's surface with the class brush before the first WM_PAINT is composed - the
    //default brush is light, which flashes white for a frame when the window first shows in
    //the dark IDE.
    FBackgroundBrush : HBRUSH;
    {$IFDEF THEMESERVICES}
    FNotifierId : integer;
    {$ENDIF}

    procedure SetCancellationTokenSource(const Value: ICancellationTokenSource);
    procedure SetCloseDelayInSeconds(const Value: integer);
    procedure UpdateBackgroundBrush;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;

    //INTAIDEThemingServicesNotifier
    procedure ChangingTheme;
    procedure ChangedTheme;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    procedure PumpLogWindowMessages;

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


{ TDPMMessageForm }

procedure TDPMMessageForm.actCanCancelExecute(Sender: TObject);
var
  tokenSource : ICancellationTokenSource;
begin
  tokenSource := FCancellationTokenSource;
  if tokenSource <> nil then
    tokenSource.Cancel;
  //ActionList OnUpdate runs on Application idle, which never fires while a synchronous task
  //blocks the IDE loop - disable directly so the click visibly registers.
  actCanCancel.Enabled := false;
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
  UpdateBackgroundBrush;
  {$ENDIF}
end;

procedure TDPMMessageForm.ChangingTheme;
begin

end;


procedure TDPMMessageForm.Clear;
begin
  FLogMemo.Clear; //flushes synchronously - no pumping needed (or wanted) here.
  FCurrentCloseDelay := FCloseDelayInSeconds;
  lblClosing.Visible := false;
  lblDontClose.Visible := false;
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

constructor TDPMMessageForm.Create(AOwner: TComponent; const options : IDPMIDEOptions);
var
  {$IFDEF THEMESERVICES}
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
  IDEStyleServices : TCustomStyleServices;
begin
  inherited Create(AOwner);
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
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
  //Prefill colour for the first composed frame - see FBackgroundBrush. The form colour is
  //already themed here (ApplyTheme runs in the constructor, before the handle exists).
  if FBackgroundBrush = 0 then
    FBackgroundBrush := CreateSolidBrush(ColorToRGB(Self.Color));
  Params.WindowClass.hbrBackground := FBackgroundBrush;
end;

procedure TDPMMessageForm.Debug(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtDebug);
  Self.PumpLogWindowMessages;
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
  if FBackgroundBrush <> 0 then
    DeleteObject(FBackgroundBrush);
end;

procedure TDPMMessageForm.Destroyed;
begin

end;

procedure TDPMMessageForm.Error(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtError);
  Self.PumpLogWindowMessages;
end;


procedure TDPMMessageForm.FormHide(Sender: TObject);
begin
  FLogMemo.Clear;
end;

procedure TDPMMessageForm.FormShow(Sender: TObject);
begin
  //Nothing to do - the message service forces a synchronous full-tree paint (RedrawWindow)
  //right after Show, so the window is never blank even if the IDE loop is not pumping.
end;

procedure TDPMMessageForm.Information(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantInformation)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtInformation);
  Self.PumpLogWindowMessages;
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
  Self.PumpLogWindowMessages;
end;

//Dispatches any queued WM_PAINT for one window of the log form's tree. Used with
//EnumChildWindows so that every descendant paints - including the VCL style hook's
//scrollbar overlay windows (TScrollingStyleHook.TScrollWindow), which are separate
//HWNDs parented to the form that nothing else repaints while the IDE loop is blocked
//(they otherwise show as white strips).
function PumpChildPaintProc(wnd : HWND; lParam : LPARAM) : BOOL; stdcall;
var
  msg : TMsg;
begin
  while PeekMessage(msg, wnd, WM_PAINT, WM_PAINT, PM_REMOVE) do
    DispatchMessage(msg);
  result := True;
end;

// The core runs restore/install synchronously on the IDE main thread, so the IDE message
// loop is not pumped while a task runs. This keeps the log window alive WITHOUT
// Application.ProcessMessages: only input queued for the log window's own controls and
// paint messages for the form's window tree are dispatched. Posted IDE messages and input
// for IDE windows stay queued, so nothing can re-enter project loading or IDE notifiers
// mid-operation (which full pumping risked).
procedure TDPMMessageForm.PumpLogWindowMessages;
var
  msg : TMsg;

  procedure PumpWindow(const wnd : HWND);
  begin
    if wnd = 0 then
      exit;
    while PeekMessage(msg, wnd, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;

begin
  //AddRow only marked the control dirty (cheap, coalesced). Throttle the synchronous repaint
  //and the pump to ~10ms so rapid logging stays smooth and flicker-free.
  if (not FStopwatch.IsRunning) or (FStopwatch.ElapsedMilliseconds > 10) then
  begin
    FLogMemo.Flush; //synchronous, own-handle paint - does not depend on the IDE pumping WM_PAINT.

    //Button clicks: WM_LBUTTONDOWN/UP are posted to the button hwnd (dispatched here); the
    //resulting BN_CLICKED arrives via SENT WM_COMMAND, which needs no pumping.
    PumpWindow(btnCancel.Handle);
    PumpWindow(btnCopy.Handle);
    PumpWindow(FLogMemo.Handle); //scroll/selection during the op - handlers are self-contained.

    //Paint-only for the form and every descendant window (panel, labels, and the style
    //hook's scrollbar overlay windows). Deliberately NOT pumping the form's non-client
    //input - dragging the caption would enter DefWindowProc's modal move loop, which pumps
    //the whole thread queue (the exact re-entrance hazard removed above).
    while PeekMessage(msg, Self.Handle, WM_PAINT, WM_PAINT, PM_REMOVE) do
      DispatchMessage(msg);
    EnumChildWindows(Self.Handle, @PumpChildPaintProc, 0);

    FStopwatch.Reset; //Reset stops + zeroes
    FStopwatch.Start;
  end;
end;

//Keeps the window-class background brush in sync with the themed form colour so the OS
//prefill of a freshly shown window is dark, not white (see FBackgroundBrush).
procedure TDPMMessageForm.UpdateBackgroundBrush;
var
  oldBrush : HBRUSH;
begin
  oldBrush := FBackgroundBrush;
  FBackgroundBrush := CreateSolidBrush(ColorToRGB(Self.Color));
  if HandleAllocated then
    {$IFDEF CPUX64}
    SetClassLongPtr(Handle, GCLP_HBRBACKGROUND, LONG_PTR(FBackgroundBrush));
    {$ELSE}
    SetClassLong(Handle, GCL_HBRBACKGROUND, Longint(FBackgroundBrush));
    {$ENDIF}
  if oldBrush <> 0 then
    DeleteObject(oldBrush);
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
  //Reflect immediately rather than waiting for the next ActionList idle update - idle never
  //fires while a synchronous task blocks the IDE loop.
  btnClose.Enabled := not value;
  actCanCancel.Enabled := value and (FCancellationTokenSource <> nil) and (not FCancellationTokenSource.Token.IsCancelled);
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
  Self.PumpLogWindowMessages;
end;

procedure TDPMMessageForm.Verbose(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantVerbose)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtVerbose);
  Self.PumpLogWindowMessages;
end;

procedure TDPMMessageForm.Warning(const data: string; const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantWarning)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtWarning);
  Self.PumpLogWindowMessages;
end;

initialization

  TToolsApiUtils.RegisterFormClassForTheming(TDPMMessageForm);


end.
