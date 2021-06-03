unit DPM.Controls.GroupButton;

interface

uses
  WinApi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls;

const
  CM_ACTIVE_CHANGED = CM_BASE + 501;

type
  TDPMGroupButton = class(TCustomLabel)
  private
    FActive : boolean;
    FHover : boolean;
    FActiveColor : TColor;
    FHoverColor : TColor;
    FGroupIndex : NativeUInt;
    FBarHeight : Integer;
    procedure SetActive(const Value : boolean);
    procedure SetActiveColor(const Value : TColor);
    procedure SetHoverColor(const Value : TColor);
    procedure SetBarHeight(const Value : Integer);
  protected
    procedure WMLButtonDown(var Message : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;

    procedure SendActiveChangedMessage;

    procedure CMActiveChanged(var Message : TMessage); message CM_ACTIVE_CHANGED;
    procedure ChangeScale(M: Integer; D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;

  public
    constructor Create(AOwner : TComponent); override;
  published
    property Active : boolean read FActive write SetActive;
    property ActiveColor : TColor read FActiveColor write SetActiveColor;
    property BarHeight : Integer read FBarHeight write SetBarHeight;
    property Caption;
    property Font;
    property Height;
    property HoverColor : TColor read FActiveColor write SetHoverColor;
    property Group : NativeUInt read FGroupIndex write FGroupIndex;
    property Left;
    property Tag;
    property Top;
    property Width;
    property OnClick;
  end;


implementation

uses
  WinApi.Windows,
  System.Types,
  Vcl.Forms,
  Vcl.Themes;

constructor TDPMGroupButton.Create(AOwner : TComponent);
begin
  inherited;
  FActive := false;
  FGroupIndex := 0;
  Alignment := TAlignment.taCenter;
  AutoSize := false;
  Width := 65;
  Height := 24;
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  StyleElements := StyleElements - [seFont];
  {$IFEND}
  Font.Size := 11;
  Font.Color := StyleServices.GetStyleFontColor(TStyleFont.sfButtonTextNormal);
  FActiveColor := $00CC7A00;
  FHoverColor := $006464FA;
  FBarHeight := 3;
end;


procedure TDPMGroupButton.ChangeScale(M: Integer; D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend});
begin
  FBarHeight := MulDiv(FBarHeight, M, D);
  inherited;


end;

procedure TDPMGroupButton.CMActiveChanged(var Message : TMessage);
begin
  //ignore those for other groups.
  if Message.WParam <> FGroupIndex then
    exit;

  //ignore if it's us who sent it!
  if Message.LParam <> NativeInt(Self) then
    SetActive(false);

  //make sure other controls get the message.
  Message.Result := 0;
end;

procedure TDPMGroupButton.CMMouseEnter(var Message : TMessage);
begin
  FHover := True;
  inherited;
  Invalidate;
end;

procedure TDPMGroupButton.CMMouseLeave(var Message : TMessage);
begin
  FHover := false;
  inherited;
  Invalidate;
end;

procedure TDPMGroupButton.Paint;
var
  r : TRect;
begin
  inherited;
  if FHover or FActive then
  begin
    r := GetClientRect;
    if FHover then
      Canvas.Brush.Color := FHoverColor
    else
      Canvas.Brush.Color := FActiveColor;
    Canvas.Brush.Style := bsSolid;
    r.Top := r.Bottom - FBarHeight;
    Canvas.FillRect(r);
  end;
end;

procedure TDPMGroupButton.SendActiveChangedMessage;
var
  message : TMessage;
  //  form : TCustomForm;
begin
  message.Msg := CM_ACTIVE_CHANGED;
  message.LParam := NativeInt(Self);
  message.WParam := FGroupIndex;
  if Self.Parent <> nil then
    Self.Parent.Broadcast(message);
  //  form := GetParentForm(Self);
  //  if form <> nil then
  //    form.Broadcast(message);
end;

procedure TDPMGroupButton.SetActive(const Value : boolean);
begin
  FActive := Value;

  //  Caption := BoolToStr(FActive,true);

  if FActive then
    SendActiveChangedMessage;
  Refresh;
end;

procedure TDPMGroupButton.SetActiveColor(const Value : TColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    Invalidate;
  end;

end;

procedure TDPMGroupButton.SetBarHeight(const Value : Integer);
begin
  if FBarHeight <> value then
  begin
    FBarHeight := Value;
    if FBarHeight < 2 then
      FBarHeight := 2;
    if FActive then
      Invalidate;
  end;
end;

procedure TDPMGroupButton.SetHoverColor(const Value : TColor);
begin
  if FHoverColor <> Value then
  begin
    FHoverColor := Value;
  end;
end;

procedure TDPMGroupButton.WMLButtonDown(var Message : TWMLButtonDown);
begin
  SetActive(true);
  inherited;
end;


end.

