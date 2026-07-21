unit DPM.IDE.MessageService;

interface

uses
  Vcl.Forms,
  VSoft.Awaitable,
  DPM.IDE.MessageForm,
  DPM.IDE.Options;

//TODO : Make the message service own the TStringList that the Logmemo on the form currently owns?
//that way we don't need the form all the time, only when we need to display it.

type
  TMessageTask = (mtNone, mtRestore, mtInstall, mtUninstall, mtVerifyCache);

  ///<Summary>Manages the status windows that shows when installing or restoring packages</Summary>
  IDPMIDEMessageService = interface
  ['{B2305CD4-E2E0-4746-B988-7A0E2EF4DCF6}']
    procedure TaskStarted(const cancellationTokenSource : ICancellationTokenSource; const task : TMessageTask);
    procedure TaskDone(const success : boolean);

    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;

    procedure Shutdown;

    function CanHandleMessages : boolean;

  end;


  TDPMIDEMessageService = class(TInterfacedObject, IDPMIDEMessageService)
  private
    FOptions : IDPMIDEOptions;
    FMessageForm : TDPMMessageForm;
    FCancellationTokenSource : ICancellationTokenSource;
    FCurrentTask : TMessageTask;
    //Set true by Shutdown - blocks EnsureMessageForm from re-creating the form during IDE
    //teardown. Without this, any late logger call (e.g. from a notification fired after the
    //wizard's Destroyed has already run) creates a fresh TDPMMessageForm owned by
    //Application.MainForm - which may already be dying. Generic AV on shutdown was the
    //observed symptom when a project group was loaded (load triggers Debug logs, which create
    //FMessageForm; subsequent group-close notifications log too, into the freed state).
    FShutdown : boolean;
  protected
    procedure EnsureMessageForm;
    procedure HideMessageWindow;
    procedure ShowMessageWindow;
    procedure Shutdown;
    procedure TaskStarted(const cancellationTokenSource : ICancellationTokenSource; const task : TMessageTask);
    procedure TaskDone(const success : boolean);


    //Logging
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;
    function CanHandleMessages : boolean;

  public
    constructor Create(const options : IDPMIDEOptions);
    destructor Destroy;override;

  end;

implementation

uses
  System.SysUtils;

{ TDPMMessageService }

procedure TDPMIDEMessageService.Clear;
begin
  if FMessageForm <> nil then
    FMessageForm.Clear;
end;

constructor TDPMIDEMessageService.Create(const options : IDPMIDEOptions);
begin
  FMessageForm := nil;
  FOptions := options;
  FCurrentTask := TMessageTask.mtNone;
  FShutdown := false;
end;

procedure TDPMIDEMessageService.Debug(const data: string);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Debug(data)
end;

destructor TDPMIDEMessageService.Destroy;
begin
  if FMessageForm <> nil then
  begin
    FOptions.LogWindowWidth := FMessageForm.Width;
    FOptions.LogWindowHeight := FMessageForm.Height;
    FOptions.SaveToFile();
    FMessageForm.PopupParent := nil;
    FMessageForm.Free;
  end;
  inherited;
end;

procedure TDPMIDEMessageService.EnsureMessageForm;
begin
  //After Shutdown - or if MainForm has already been torn down by the IDE - refuse to create
  //a new form. Callers (Debug/Error/Information/...) all guard with 'if FMessageForm <> nil'
  //so this just silently drops the log entry, which is the right behaviour during shutdown.
  if FShutdown then
    exit;
  if Application.MainForm = nil then
    exit;
  if FMessageForm = nil then
  begin
    FMessageForm := TDPMMessageForm.Create(nil, FOptions);
    //Use PopupParent (owned top-level window) rather than Parent. Setting Parent makes the form a
    //WS_CHILD embedded in the IDE main form, which never receives WM_ACTIVATE - so it never becomes
    //Screen.ActiveCustomForm and its focus/keyboard handling is broken (Ctrl+A/Ctrl+C in the log
    //memo, and the form's IsShortCut override, never fire). PopupParent keeps it above the IDE main
    //form and owned by it while remaining a proper top-level window with working keyboard focus.
    FMessageForm.PopupMode := pmExplicit;
    FMessageForm.PopupParent := Application.MainForm;
  end;
  FMessageForm.CancellationTokenSource := FCancellationTokenSource;
  FMessageForm.CloseDelayInSeconds := FOptions.AutoCloseLogDelaySeconds;
end;

procedure TDPMIDEMessageService.Error(const data: string);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Error(data)
end;

procedure TDPMIDEMessageService.HideMessageWindow;
begin
  if FMessageForm <> nil then
    FMessageForm.DelayHide;
end;

procedure TDPMIDEMessageService.Information(const data: string; const important: Boolean);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Information(data, important);
end;

procedure TDPMIDEMessageService.NewLine;
begin
  if FMessageForm <> nil then
    FMessageForm.NewLine;
end;


function TDPMIDEMessageService.CanHandleMessages: boolean;
begin
  result := (FCurrentTask <> TMessageTask.mtNone) and (FMessageForm <> nil) and FMessageForm.Showing;
end;

procedure TDPMIDEMessageService.ShowMessageWindow;
begin
  EnsureMessageForm;
  if not FMessageForm.Showing then
  begin
    FMessageForm.Show; //paints synchronously via TDPMMessageForm.CMShowingChanged
    FMessageForm.BringToFront;
    //The z-order change can expose more of us, so paint again. Still synchronous - no
    //Application.ProcessMessages, which used to re-enter the IDE from inside a notifier here.
    FMessageForm.PaintNow;
  end;
end;

procedure TDPMIDEMessageService.Shutdown;
begin
  //Set the flag BEFORE freeing - any logger call racing with shutdown will see Shutdown=true
  //and skip rather than try to act on a half-freed form.
  FShutdown := true;
  if FMessageForm <> nil then
  begin
    FMessageForm.PopupParent := nil;
    FreeAndNil(FMessageForm);
  end;
end;

procedure TDPMIDEMessageService.Success(const data: string;  const important: Boolean);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Success(data, important);
end;

procedure TDPMIDEMessageService.TaskDone(const success : boolean);
begin
  FCancellationTokenSource := nil;
  FCurrentTask := mtNone;
  //Re-enable the Close button / close box now the work is done. Done unconditionally up
  //front so it happens regardless of the success/auto-close branches below.
  if FMessageForm <> nil then
    FMessageForm.SetTaskRunning(false);
  if not success then
  begin
    if FMessageForm <> nil then
      FMessageForm.CancellationTokenSource := nil;
    ShowMessageWindow;
  end
  else if FOptions.AutoCloseLogOnSuccess and success then
  begin
    if FMessageForm <> nil then
    begin
      FMessageForm.CancellationTokenSource := nil;
      HideMessageWindow;
    end;
  end;
end;

procedure TDPMIDEMessageService.TaskStarted(const cancellationTokenSource: ICancellationTokenSource; const task : TMessageTask);
begin
  if FCurrentTask <> task then
  begin
    FCancellationTokenSource := cancellationTokenSource;
    FCurrentTask := task;
    EnsureMessageForm;
    FMessageForm.Clear;
  end;

  //Mark the task as running so the log window can't be closed until it finishes (or is
  //cancelled). EnsureMessageForm may legitimately leave FMessageForm nil during shutdown.
  if FMessageForm <> nil then
    FMessageForm.SetTaskRunning(true);

  case task of
    mtNone :
    begin

    end;
    mtRestore:
    begin
       if FOptions.ShowLogForRestore then
        ShowMessageWindow;
    end;
    mtInstall:
    begin
       if FOptions.ShowLogForInstall then
        ShowMessageWindow;
    end;
    mtUninstall:
    begin
       if FOptions.ShowLogForUninstall then
        ShowMessageWindow;
    end;
    mtVerifyCache:
    begin
      //No per-task option for this one - the user explicitly invoked it from the DPM
      //menu and needs to see progress (and the Cancel button), so always show.
      ShowMessageWindow;
    end;
  end;


end;

procedure TDPMIDEMessageService.Verbose(const data: string;  const important: Boolean);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Verbose(data, important);
end;

procedure TDPMIDEMessageService.Warning(const data: string;  const important: Boolean);
begin
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Warning(data, important);
end;

end.
