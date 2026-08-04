unit DPM.IDE.MessageService;

interface

uses
  Vcl.Forms,
  System.SyncObjs,
  Spring.Collections,
  VSoft.Awaitable,
  DPM.IDE.MessageForm,
  DPM.IDE.Options;

//TODO : Make the message service own the TStringList that the Logmemo on the form currently owns?
//that way we don't need the form all the time, only when we need to display it.

type
  TMessageTask = (mtNone, mtRestore, mtInstall, mtUninstall, mtVerifyCache);

  //The log window is main thread only - the form, the log memo (which paints synchronously
  //through a DC it fetches itself) and the auto close countdown all are. Plenty of logging
  //however happens on a worker thread : the editor view runs its searches and icon fetches
  //through VSoft.Awaitable, and the core code called from inside those async bodies logs
  //(eg TPackageRepositoryManager.GetPackageIcon -> FLogger.Error). Those calls used to go
  //straight at the form, and if one of them was the first log of the session it CONSTRUCTED
  //the form on the worker thread - see the comment in EnsureMessageForm for what that broke.
  //Worker lines are now parked here and replayed the next time the main thread logs or shows
  //the window.
  TPendingLogKind = (plDebug, plError, plInformation, plSuccess, plVerbose, plWarning, plNewLine);

  TPendingLogLine = record
    Kind : TPendingLogKind;
    Data : string;
    Important : boolean;
  end;

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
    //Log lines that arrived on a worker thread, waiting to be replayed on the main thread.
    FPendingLines : IList<TPendingLogLine>;
    FPendingLock : TCriticalSection;
    //Guards against re-entering the replay - the form pumps its own controls' input while
    //logging, so a dispatched message could in principle log again.
    FFlushingPending : boolean;
  protected
    function IsMainThread : boolean;
    procedure QueueForMainThread(const kind : TPendingLogKind; const data : string; const important : boolean);
    procedure FlushPendingLines;
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
  Winapi.Windows,
  System.SysUtils;

const
  //Bounded so a session that never opens the log window cannot accumulate worker log lines
  //for the life of the IDE. The oldest go first - the most recent are the useful ones.
  CMaxPendingLines = 500;

{ TDPMMessageService }

procedure TDPMIDEMessageService.Clear;
begin
  if not IsMainThread then
    exit;
  if FMessageForm <> nil then
    FMessageForm.Clear;
end;

constructor TDPMIDEMessageService.Create(const options : IDPMIDEOptions);
begin
  FMessageForm := nil;
  FOptions := options;
  FCurrentTask := TMessageTask.mtNone;
  FShutdown := false;
  FPendingLines := TCollections.CreateList<TPendingLogLine>;
  FPendingLock := TCriticalSection.Create;
  FFlushingPending := false;
end;

procedure TDPMIDEMessageService.Debug(const data: string);
begin
  if not IsMainThread then
  begin
    QueueForMainThread(plDebug, data, false);
    exit;
  end;
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
  FPendingLines := nil;
  FPendingLock.Free;
  inherited;
end;

function TDPMIDEMessageService.IsMainThread : boolean;
begin
  result := GetCurrentThreadId = MainThreadID;
end;

procedure TDPMIDEMessageService.QueueForMainThread(const kind : TPendingLogKind; const data : string; const important : boolean);
var
  line : TPendingLogLine;
begin
  if FShutdown then
    exit;
  line.Kind := kind;
  line.Data := data;
  line.Important := important;
  FPendingLock.Enter;
  try
    while FPendingLines.Count >= CMaxPendingLines do
      FPendingLines.Delete(0);
    FPendingLines.Add(line);
  finally
    FPendingLock.Leave;
  end;
end;

procedure TDPMIDEMessageService.FlushPendingLines;
var
  lines : TArray<TPendingLogLine>;
  i : integer;
begin
  //Main thread only, and only once we actually have somewhere to put them.
  if FFlushingPending or (FMessageForm = nil) or (not IsMainThread) then
    exit;
  FPendingLock.Enter;
  try
    if FPendingLines.Count = 0 then
      exit;
    lines := FPendingLines.ToArray;
    FPendingLines.Clear;
  finally
    FPendingLock.Leave;
  end;

  FFlushingPending := true;
  try
    for i := 0 to Length(lines) - 1 do
    begin
      case lines[i].Kind of
        plDebug       : FMessageForm.Debug(lines[i].Data);
        plError       : FMessageForm.Error(lines[i].Data);
        plInformation : FMessageForm.Information(lines[i].Data, lines[i].Important);
        plSuccess     : FMessageForm.Success(lines[i].Data, lines[i].Important);
        plVerbose     : FMessageForm.Verbose(lines[i].Data, lines[i].Important);
        plWarning     : FMessageForm.Warning(lines[i].Data, lines[i].Important);
        plNewLine     : FMessageForm.NewLine;
      end;
    end;
  finally
    FFlushingPending := false;
  end;
end;

procedure TDPMIDEMessageService.EnsureMessageForm;
begin
  //After Shutdown - or if MainForm has already been torn down by the IDE - refuse to create
  //a new form. Callers (Debug/Error/Information/...) all guard with 'if FMessageForm <> nil'
  //so this just silently drops the log entry, which is the right behaviour during shutdown.
  if FShutdown then
    exit;
  //NEVER construct the form off the main thread. Everything on it is main thread bound, and one
  //piece of it fails hard : a TTimer allocates its hidden message window in its constructor, and
  //Windows destroys a thread's windows when that thread exits. A VSoft.Awaitable worker (repo
  //search / icon fetch) that logged before the main thread ever did would build the form here,
  //die seconds later, and leave the auto close countdown pointing at a dead HWND - so the next
  //DelayHide raised EOutOfResources 'Not enough timers available' (the VCL reports ANY SetTimer
  //failure that way, which is why it looked like a timer leak). Worker lines are buffered by the
  //log methods instead and replayed below. Callers all guard with 'if FMessageForm <> nil'.
  if not IsMainThread then
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
  //Anything a worker thread logged while we had no form (or before now) goes in first, so it
  //keeps its place ahead of the line the caller is about to add.
  FlushPendingLines;
end;

procedure TDPMIDEMessageService.Error(const data: string);
begin
  if not IsMainThread then
  begin
    QueueForMainThread(plError, data, false);
    exit;
  end;
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
  if not IsMainThread then
  begin
    QueueForMainThread(plInformation, data, important);
    exit;
  end;
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Information(data, important);
end;

procedure TDPMIDEMessageService.NewLine;
begin
  if not IsMainThread then
  begin
    QueueForMainThread(plNewLine, '', false);
    exit;
  end;
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
  //EnsureMessageForm legitimately leaves this nil - during shutdown, before the IDE main form
  //exists, or when called off the main thread.
  if FMessageForm = nil then
    exit;
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
  //Nothing is going to replay these now, and a worker still in flight can keep adding to the
  //list right up until its token is checked.
  FPendingLock.Enter;
  try
    FPendingLines.Clear;
  finally
    FPendingLock.Leave;
  end;
  if FMessageForm <> nil then
  begin
    FMessageForm.PopupParent := nil;
    FreeAndNil(FMessageForm);
  end;
end;

procedure TDPMIDEMessageService.Success(const data: string;  const important: Boolean);
begin
  if not IsMainThread then
  begin
    QueueForMainThread(plSuccess, data, important);
    exit;
  end;
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Success(data, important);
end;

procedure TDPMIDEMessageService.TaskDone(const success : boolean);
begin
  //Nothing was ever started, so there is nothing to finish - and in particular nothing worth
  //showing. Without this, a TaskDone(false) arriving from a load that never called TaskStarted
  //(eg opening a new project after an earlier restore failed) force-shows the log window below,
  //carrying whatever the previous task left in it.
  if FCurrentTask = mtNone then
    exit;

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
    if FMessageForm <> nil then
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
  if not IsMainThread then
  begin
    QueueForMainThread(plVerbose, data, important);
    exit;
  end;
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Verbose(data, important);
end;

procedure TDPMIDEMessageService.Warning(const data: string;  const important: Boolean);
begin
  if not IsMainThread then
  begin
    QueueForMainThread(plWarning, data, important);
    exit;
  end;
  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Warning(data, important);
end;

end.
