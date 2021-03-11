unit DPM.IDE.MessageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  VSoft.Awaitable,
  DPM.Controls.LogMemo, Vcl.ActnList, Vcl.ExtCtrls;

type
  TDPMMessageForm = class(TForm)
    btnCancel: TButton;
    ActionList1: TActionList;
    actCanCancel: TAction;
    btnCopy: TButton;
    actCopyLog: TAction;
    btnClose: TButton;
    ClosingInTimer: TTimer;
    lblClosing: TLabel;
    lblDontClose: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure actCanCancelExecute(Sender: TObject);
    procedure actCopyLogExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnCloseClick(Sender: TObject);
    procedure ClosingInTimerTimer(Sender: TObject);
    procedure lblDontCloseLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    FLogMemo : TLogMemo;
    FCancellationTokenSource : ICancellationTokenSource;
    FCloseDelayInSeconds : integer;
    FCurrentCloseDelay : integer;
    procedure SetCancellationTokenSource(const Value: ICancellationTokenSource);
    procedure SetCloseDelayInSeconds(const Value: integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;

  public
    procedure Debug(const data : string);
    procedure Error(const data : string);
    procedure Information(const data : string; const important : Boolean = False);
    procedure Success(const data : string; const important : Boolean = False);
    procedure Verbose(const data : string; const important : Boolean = False);
    procedure Warning(const data : string; const important : Boolean = False);
    procedure NewLine;
    procedure Clear;

    procedure DelayHide;

    property CancellationTokenSource : ICancellationTokenSource read FCancellationTokenSource write SetCancellationTokenSource;
    property CloseDelayInSeconds : integer read FCloseDelayInSeconds write SetCloseDelayInSeconds;
  end;

var
  DPMMessageForm: TDPMMessageForm;

implementation

uses
  Vcl.Themes,
  Vcl.clipbrd,
  ToolsApi;

{$R *.dfm}

{ TDPMMessageForm }


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
  Handled := true;
end;

procedure TDPMMessageForm.btnCloseClick(Sender: TObject);
begin
  ClosingInTimer.Enabled := false;
  FCurrentCloseDelay := FCloseDelayInSeconds;
  Self.Hide;
end;

procedure TDPMMessageForm.Clear;
begin
  FLogMemo.Clear;
  FCurrentCloseDelay := FCloseDelayInSeconds;
end;

procedure TDPMMessageForm.ClosingInTimerTimer(Sender: TObject);
begin
  ClosingInTimer.Enabled := false;
  if FCurrentCloseDelay > 0  then
  begin
    lblClosing.Caption := 'Closing in ' + IntToStr(FCurrentCloseDelay) + ' seconds' + StringOfChar('.', FCurrentCloseDelay);
    ClosingInTimer.Enabled := true;
    Dec(FCurrentCloseDelay);
  end
  else
  begin
    FCurrentCloseDelay := FCloseDelayInSeconds;
    Self.Hide;
  end;
end;

procedure TDPMMessageForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TDPMMessageForm.Debug(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtDebug);
  Application.ProcessMessages;
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

procedure TDPMMessageForm.Error(const data: string);
begin
  FLogMemo.AddRow(data, TLogMessageType.mtError);
  Application.ProcessMessages;
end;

procedure TDPMMessageForm.FormCreate(Sender: TObject);
var
  IDEStyleServices : TCustomStyleServices;
{$IF CompilerVersion >=32.0}
  ideThemeSvc : IOTAIDEThemingServices;
  {$IFEND}
begin
  {$IF CompilerVersion >=32.0}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  IDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  IDEStyleServices := Vcl.Themes.StyleServices;
  {$IFEND}

  FLogMemo := TLogMemo.Create(Self);
  FLogMemo.TabOrder := 0;
  FLogMemo.TabStop := true;
  FLogMemo.Top := 10;
  FLogMemo.Left := 10;
  FLogMemo.Width := Self.ClientWidth - 20;
  FLogMemo.Height := Self.ClientHeight - 30 - btnCancel.Height;
  FLogMemo.Anchors := [akLeft, akRight, akTop, akBottom];
  FLogMemo.StyleServices := IDEStyleServices;
  FLogMemo.Clear;
  FLogMemo.Parent := Self;
  Self.ActiveControl := btnCancel;

  FCloseDelayInSeconds := 3;
  FCurrentCloseDelay := FCloseDelayInSeconds;
end;

procedure TDPMMessageForm.FormHide(Sender: TObject);
begin
  FLogMemo.Clear;
end;

procedure TDPMMessageForm.Information(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantInformation)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtInformation);
  Application.ProcessMessages;
end;

procedure TDPMMessageForm.lblDontCloseLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ClosingInTimer.Enabled := false;
  lblClosing.Visible := false;
  lblDontClose.Visible := false;
  FCurrentCloseDelay := FCloseDelayInSeconds;
end;

procedure TDPMMessageForm.NewLine;
begin
  FLogMemo.AddRow('',mtInformation);
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

procedure TDPMMessageForm.Success(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantSuccess)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtSuccess);
  Application.ProcessMessages;
end;

procedure TDPMMessageForm.Verbose(const data: string;  const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantVerbose)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtVerbose);
  Application.ProcessMessages;
end;

procedure TDPMMessageForm.Warning(const data: string; const important: Boolean);
begin
  if important then
    FLogMemo.AddRow(data, TLogMessageType.mtImportantWarning)
  else
    FLogMemo.AddRow(data, TLogMessageType.mtWarning);
  Application.ProcessMessages;
end;

end.
