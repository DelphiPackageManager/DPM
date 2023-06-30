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
  TMessageTask = (mtNone, mtRestore, mtInstall, mtUninstall);

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
    FMessageForm.Parent := nil;
    FMessageForm.Free;
  end;
  inherited;
end;

procedure TDPMIDEMessageService.EnsureMessageForm;
begin
  if FMessageForm = nil then
  begin
    FMessageForm := TDPMMessageForm.Create(nil, FOptions);
    FMessageForm.Parent := Application.MainForm;
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
    FMessageForm.Show;
    FMessageForm.BringToFront;
    Application.ProcessMessages;//allow repaint
  end;
end;

procedure TDPMIDEMessageService.Shutdown;
begin
  if FMessageForm <> nil then
  begin
    FMessageForm.Parent := nil;
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
