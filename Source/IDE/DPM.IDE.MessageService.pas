unit DPM.IDE.MessageService;

interface

uses
  DPM.IDE.MessageForm;


type
  ///<Summary>Manages the status windows that shows when installing or restoring packages</Summary>
  IDPMIDEMessageService = interface
  ['{B2305CD4-E2E0-4746-B988-7A0E2EF4DCF6}']
    procedure HideMessageWindow;
    procedure ShowMessageWindow;

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
    FMessageForm : TDPMMessageForm;
  protected
    procedure EnsureMessageForm;
    procedure HideMessageWindow;
    procedure ShowMessageWindow;
    procedure Shutdown;


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
    constructor Create;
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

constructor TDPMIDEMessageService.Create;
begin
  FMessageForm := nil;

end;

procedure TDPMIDEMessageService.Debug(const data: string);
begin
  EnsureMessageForm;
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
end;

procedure TDPMIDEMessageService.Error(const data: string);
begin
  EnsureMessageForm;
  FMessageForm.Error(data)
end;

procedure TDPMIDEMessageService.HideMessageWindow;
begin
  if FMessageForm <> nil then
    FMessageForm.Hide;
end;

procedure TDPMIDEMessageService.Information(const data: string; const important: Boolean);
begin
  EnsureMessageForm;
  FMessageForm.Information(data, important);
end;

procedure TDPMIDEMessageService.NewLine;
begin
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
  EnsureMessageForm;
  FMessageForm.Success(data, important);
end;

procedure TDPMIDEMessageService.Verbose(const data: string;  const important: Boolean);
begin
  EnsureMessageForm;
  FMessageForm.Verbose(data, important);
end;

procedure TDPMIDEMessageService.Warning(const data: string;  const important: Boolean);
begin
  EnsureMessageForm;
  FMessageForm.Warning(data, important);
end;

end.
