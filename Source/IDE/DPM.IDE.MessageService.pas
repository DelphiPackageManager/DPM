unit DPM.IDE.MessageService;

interface

uses
  VSoft.Awaitable,
  DPM.IDE.MessageForm,
  DPM.IDE.Options;


type
  TMessageTask = (mtRestore, mtInstall, mtUninstall);

  ///<Summary>Manages the status windows that shows when installing or restoring packages</Summary>
  IDPMIDEMessageService = interface
  ['{B2305CD4-E2E0-4746-B988-7A0E2EF4DCF6}']
    procedure TaskStarted(const cancellationTokenSource : ICancellationTokenSource; const task : TMessageTask);
    procedure TaskDone(const succes : boolean);

    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;

    procedure Shutdown;
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

  public
    constructor Create(const options : IDPMIDEOptions);
    destructor Destroy;override;

  end;

implementation

uses
  Vcl.Forms,
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
end;

procedure TDPMIDEMessageService.Debug(const data: string);
begin
//  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Debug(data)
end;

destructor TDPMIDEMessageService.Destroy;
begin
  if FMessageForm <> nil then
  begin
    FMessageForm.Parent := nil;
    FMessageForm.Free;
  end;
  inherited;
end;

procedure TDPMIDEMessageService.EnsureMessageForm;
begin
  if FMessageForm = nil then
  begin
    FMessageForm := TDPMMessageForm.Create(nil);
    FMessageForm.Parent := Application.MainForm;
  end;
  FMessageForm.CancellationTokenSource := FCancellationTokenSource;
  FMessageForm.CloseDelayInSeconds := FOptions.AutoCloseLogDelaySeconds;
end;

procedure TDPMIDEMessageService.Error(const data: string);
begin
//  EnsureMessageForm;
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
//  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Information(data, important);
end;

procedure TDPMIDEMessageService.NewLine;
begin
  if FMessageForm <> nil then
    FMessageForm.NewLine;
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
  //EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Success(data, important);
end;

procedure TDPMIDEMessageService.TaskDone(const success : boolean);
begin
  FCancellationTokenSource := nil;
  if FMessageForm <> nil then
  begin
    FMessageForm.CancellationTokenSource := nil;
    if FOptions.AutoCloseLogOnSuccess and success then
    begin
      //TODO : implement delay in closing
      HideMessageWindow;

    end;
  end;
end;

procedure TDPMIDEMessageService.TaskStarted(const cancellationTokenSource: ICancellationTokenSource; const task : TMessageTask);
begin
  FCancellationTokenSource := cancellationTokenSource;
  FCurrentTask := task;

  case task of
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
//  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Verbose(data, important);
end;

procedure TDPMIDEMessageService.Warning(const data: string;  const important: Boolean);
begin
//  EnsureMessageForm;
  if FMessageForm <> nil then
    FMessageForm.Warning(data, important);
end;

end.
