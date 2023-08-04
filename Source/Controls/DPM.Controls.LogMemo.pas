{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Controls.LogMemo;

interface

uses
  System.Classes,
  System.Types,
  System.Generics.Collections,
  WinApi.Messages,
  WinApi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes;


type
  TPaintRowState = (rsNormal, rsHot, rsSelected, rsFocusedNormal, rsFocusedHot, rsFocusedSelected);
  TLogMessageType = (mtDebug,mtError, mtInformation, mtImportantInformation, mtSuccess, mtImportantSuccess, mtVerbose, mtImportantVerbose, mtWarning, mtImportantWarning);

  TMessageColors = array[TLogMessageType] of TColor;

  TThemeType = (Light, Dark);

  TLogMemo = class(TCustomControl)
  private
    FBorderStyle : TBorderStyle;
    FPaintBmp : TBitmap;
    FItems : TStringList;
    FRowRects : TList<TRect>;
    FRowHeight: integer;
    FVScrollPos : integer;
    FHScrollPos : integer;

    FVisibleRows : integer; //number of rows we can see
    FSelectableRows : integer;
    FHoverRow : integer; //mouse over row in view (add to top row to get index)

    FTopRow : Int64; //this is the top row cursor .
    FCurrentRow : Int64;
    FMaxWidth : integer;
    FUpdating : boolean;
    FUpdatingScrollBars : boolean;

    FStyleServices : TCustomStyleServices;

    FThemeType : TThemeType;
    FMessageColors : array[TThemeType] of TMessageColors;
    function GetText: string;
    procedure SetStyleServices(const Value: TCustomStyleServices);

  protected
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetRowHeight(const Value: integer);

    procedure UpdateVisibleRows;
    function RowInView(const row: integer): boolean;
    procedure ScrollInView(const index : integer; const updateSBs : boolean = true);

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint;override;
    procedure DoPaintRow(const index : integer; const state : TPaintRowState; const copyCanvas : boolean = true);
    function GetRowPaintState(const rowIdx : integer) : TPaintRowState;


    procedure UpdateScrollBars;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;  var ScrollPos: Integer);
    function GetViewRow(const row : integer) : integer;
    function IsAtTop : boolean;
    function IsAtEnd : boolean;
    function GetRowFromY(const Y : integer) : integer;
    procedure UpdateHoverRow(const X, Y : integer);


    function GetMaxWidth : integer;
    function GetRowCount : integer;
    procedure RowsChanged(Sender: TObject);

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMVScroll); message WM_HSCROLL;

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;


    procedure DoLineUp(const fromScrollBar : boolean);
    procedure DoLineDown(const fromScrollBar : boolean);
    procedure DoPageUp(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoPageDown(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoGoTop;
    procedure DoGoBottom;
    procedure DoTrack(const newScrollPostition : integer);
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;


    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend}); override;


  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    class constructor Create;
    class destructor Destroy;

    procedure Clear;
    procedure AddRow(const value : string; const messageType : TLogMessageType);
    procedure CheckTheme;


    property RowCount : integer read GetRowCount;
    property StyleServices : TCustomStyleServices read FStyleServices write SetStyleServices;

  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderWidth;
    property Color default clWindow;
    property Enabled;
    property Font;
    property Height default 100;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    {$IF CompilerVersion >= 24.0}
      {$LEGACYIFEND ON}
    property StyleElements;
    {$IFEND}
    property TabOrder;
    property TabStop default True;
    property Width default 100;
    property Visible;

    property RowHeight : integer read FRowHeight write SetRowHeight;
    property Text : string read GetText;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  DPM.Core.Utils.Strings;


{ TVSoftColorMemo }
//copied from StyleUtils.inc
function TextWidth(Canvas: TCanvas; const AText: string; Flags: Integer = 0): Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  Winapi.Windows.DrawText(Canvas.Handle, PChar(AText), Length(AText), R, DT_CALCRECT or Flags);
  Result := R.Right;
end;


procedure TLogMemo.AddRow(const value: string; const messageType : TLogMessageType);
var
  w : integer;
  idx : integer;
begin
  FPaintBmp.Canvas.Font.Assign(Self.Font);
  if messageType in [mtImportantInformation, mtImportantSuccess, mtImportantVerbose, mtImportantWarning,mtError] then
    FPaintBmp.Canvas.Font.Style := [fsBold];
  w := FPaintBmp.Canvas.TextWidth(value);

  FMaxWidth := Max(w, FMaxWidth);
  UpdateScrollBars;
  idx := FItems.AddObject(value, TObject(NativeUInt(messageType)) );
  ScrollInView(idx);
end;

procedure TLogMemo.Clear;
begin
  FMaxWidth := -1;
  FItems.Clear;
end;

procedure TLogMemo.CMEnter(var Message: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TLogMemo.CMExit(var Message: TCMExit);
begin
  FHoverRow := -1;
  Invalidate;
  inherited;
end;

procedure TLogMemo.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TLogMemo.CMMouseLeave(var Msg: TMessage);
var
  oldHoverRow : integer;
  rowState : TPaintRowState;
begin
  oldHoverRow := FHoverRow;
  FHoverRow := -1;
  if (oldHoverRow <> -1) and RowInView(oldHoverRow) then
  begin
    rowState := GetRowPaintState(oldHoverRow);
    DoPaintRow(oldHoverRow, rowState);
  end;
  inherited;
end;

procedure TLogMemo.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  CheckTheme;
end;

class constructor TLogMemo.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TLogMemo, TScrollingStyleHook);
end;

procedure TLogMemo.CreateHandle;
begin
  inherited;
  Resize;
end;

procedure TLogMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style or WS_BORDER;
//    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  Params.Style := Params.Style + WS_VSCROLL + WS_HSCROLL;
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);

end;

procedure TLogMemo.CreateWnd;
begin
  inherited;
  UpdateVisibleRows;
  Invalidate;
end;

constructor TLogMemo.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FItems.OnChange := RowsChanged;
  FPaintBmp := TBitmap.Create;
  FPaintBmp.PixelFormat := pf32bit;
  FRowRects := TList<TRect>.Create;
  FBorderStyle := bsSingle;


  ControlStyle := [csCaptureMouse, csClickEvents, csPannable];
  FVScrollPos := 0;
  FHoverRow := -1;
  FCurrentRow := -1;
  FRowHeight := 16;
  TabStop := true;
  ParentBackground := false;
  ParentColor := true;
  ParentDoubleBuffered := false;
  ParentFont := true;
  DoubleBuffered := false;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Ctl3D := false;
  FMaxWidth := -1;
  FStyleServices := Vcl.Themes.StyleServices;

  FMessageColors[TThemeType.Light][mtDebug] := $00BBBBBB;
  FMessageColors[TThemeType.Light][mtError] := $00423CDB;

  FMessageColors[TThemeType.Light][mtInformation] := $00444444;
  FMessageColors[TThemeType.Light][mtImportantInformation] := $00444444;

  FMessageColors[TThemeType.Light][mtSuccess] := TColorRec.Green;
  FMessageColors[TThemeType.Light][mtImportantSuccess] := TColorRec.Green;

  FMessageColors[TThemeType.Light][mtVerbose] := FMessageColors[TThemeType.Light][mtInformation];
  FMessageColors[TThemeType.Light][mtImportantVerbose] := FMessageColors[TThemeType.Light][mtImportantInformation];

  FMessageColors[TThemeType.Light][mtWarning] := TColorRec.Orange;
  FMessageColors[TThemeType.Light][mtImportantWarning] := TColorRec.Orange;

  FMessageColors[TThemeType.Dark][mtDebug] := TColorRec.DarkGray;
  FMessageColors[TThemeType.Dark][mtError] := $006464FA;
  FMessageColors[TThemeType.Dark][mtInformation] := $00CCCCCC;
  FMessageColors[TThemeType.Dark][mtImportantInformation] := $00CCCCCC;
  FMessageColors[TThemeType.Dark][mtSuccess] := $0084D188;
  FMessageColors[TThemeType.Dark][mtImportantSuccess] := $0084D188;
  FMessageColors[TThemeType.Dark][mtVerbose] := FMessageColors[TThemeType.Light][mtInformation];
  FMessageColors[TThemeType.Dark][mtImportantVerbose] := FMessageColors[TThemeType.Light][mtImportantInformation];
  FMessageColors[TThemeType.Dark][mtWarning] := TColorRec.Orange;
  FMessageColors[TThemeType.Dark][mtImportantWarning] := TColorRec.Orange;

  FThemeType := TThemeType.Light;

end;

destructor TLogMemo.Destroy;
begin
  FRowRects.Free;
  FPaintBmp.Free;
  FItems.Free;
  inherited;
end;



class destructor TLogMemo.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TLogMemo, TScrollingStyleHook);
end;

procedure TLogMemo.DoGoBottom;
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  if RowCount = 0 then
    exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  FCurrentRow := RowCount - 1;
  FTopRow := RowCount - FSelectableRows;
  FVScrollPos := RowCount -1;

  if FTopRow <> oldTopRow then
    //need a full repaint.
    Repaint
  else if FCurrentRow <> oldCurrentRow then
  begin
    rowState := GetRowPaintState(oldCurrentRow);
    DoPaintRow(oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoPaintRow(FCurrentRow , rowState);
  end;
  UpdateScrollBars;

end;

procedure TLogMemo.DoGoTop;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  rowState : TPaintRowState;
begin
  if RowCount = 0 then
    exit;

  oldCurrentRow := FCurrentRow;
  oldTopRow := FTopRow;
  if (oldTopRow <> 0) or (oldCurrentRow <> 0) then
  begin
    //some work to do.
    if oldTopRow = 0  then
    begin
      //no scrolling so we can just paint the rows that changed.
      FCurrentRow := 0;
      FTopRow := 0;
      rowState := GetRowPaintState(oldCurrentRow);
      DoPaintRow(oldCurrentRow + oldTopRow, rowState);
      rowState := GetRowPaintState(0);
      DoPaintRow(0 , rowState);
    end
    else
    begin
      FTopRow := 0;
      FCurrentRow := 0;
      Repaint;
    end;
    FVScrollPos := 0;
    UpdateScrollBars;
  end;
end;

procedure TLogMemo.DoLineDown(const fromScrollBar: boolean);
var
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  if RowCount = 0 then
    exit;

  oldCurrentRow := FCurrentRow;

  //behavior depends on whether we are using the keyboard or the mouse on the scrollbar.
  //when we use the scrollbar, the current row doesn't change, we just scroll the view.

  if fromScrollBar then
  begin
    if (FTopRow + FSelectableRows -1) <  (RowCount -1) then
      Inc(FTopRow)
    else
      exit;

    if FHoverRow <> -1 then
      Inc(FHoverRow);

    FVScrollPos := FTopRow;
    //we scrolled so full paint.
    Repaint;
    UpdateScrollBars;
  end
  else //from keyboard
  begin
    if RowInView(FCurrentRow) then
    begin
      //if the currentRow is visible, then we can try to move the current row if it's not at the bottom.
      if (FCurrentRow - FTopRow < FSelectableRows - 1) then
      begin
        Inc(FCurrentRow);
        //no scrolling required so just paint the affected rows.
        //there may not have been a current row before.
        if (oldCurrentRow >= 0) and RowInView(oldCurrentRow) then
        begin
          rowState := GetRowPaintState(oldCurrentRow);
          DoPaintRow(oldCurrentRow , rowState);
        end;
        rowState := GetRowPaintState(FCurrentRow);
        DoPaintRow(FCurrentRow , rowState);
        //DoRowChanged(oldCurrentRow);
        FVScrollPos := FTopRow;
        UpdateScrollBars;
      end
      else
      begin
        //Current Row isn't in the view, so we will need a full paint
        if FCurrentRow < RowCount -1 then
        begin
          Inc(FCurrentRow);
          Inc(FTopRow);
          if FHoverRow <> -1 then
            Inc(FHoverRow);
          FVScrollPos := FTopRow;
          //DoRowChanged(oldCurrentRow);
          Invalidate;
          UpdateScrollBars;
        end;
      end;
    end
    else
    begin
      if FCurrentRow < RowCount -1 then
      begin
        Inc(FCurrentRow);
        FTopRow := FCurrentRow;
        FVScrollPos := FTopRow;
        //DoRowChanged(oldCurrentRow);
        Invalidate;
        UpdateScrollBars;
      end;
    end;
  end;
end;

procedure TLogMemo.DoLineUp(const fromScrollBar: boolean);
var
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  if RowCount = 0 then
    exit;

  oldCurrentRow := FCurrentRow;

  if fromScrollBar then
  begin
    if FTopRow > 0 then
    begin
      Dec(FTopRow);
      if FHoverRow > 0 then
        Dec(FHoverRow);

      FVScrollPos := FTopRow;
      //we scrolled so full paint.
      Invalidate;
      UpdateScrollBars;
    end;
  end
  else
  begin
    if RowInView(FCurrentRow) then
    begin
      //if the currentRow is visible, then we can try to move the current row if it's not at the bottom.
      if  ((FCurrentRow - FTopRow ) > 0) then
      begin
        Dec(FCurrentRow);
        if FHoverRow > 0 then
          Dec(FHoverRow);
        //no scrolling required so just paint the affected rows.
        //there may not have been a current row before.
        if (oldCurrentRow >= 0) and RowInView(oldCurrentRow) then
        begin
          rowState := GetRowPaintState(oldCurrentRow);
          DoPaintRow(oldCurrentRow , rowState);
        end;
        rowState := GetRowPaintState(FCurrentRow);
        DoPaintRow(FCurrentRow , rowState);
        FVScrollPos := FTopRow;
        UpdateScrollBars;
      end
      else
      begin
        //Current Row isn't in the view, so we will need a full paint
        if (FCurrentRow > 0) and (FTopRow > 0) then
        begin
          Dec(FCurrentRow);
          Dec(FTopRow);
          if FHoverRow > 0 then
            Dec(FHoverRow);
          FVScrollPos := FTopRow;
          Invalidate;
          UpdateScrollBars;
        end;
      end;
    end
    else
    begin
      if FCurrentRow > 0 then
      begin
        Dec(FCurrentRow);
        if FCurrentRow < FTopRow then
          FTopRow := FCurrentRow
        else
          FTopRow := Max(FCurrentRow - FSelectableRows - 1, 0);
        FVScrollPos := FTopRow;
        Invalidate;
        UpdateScrollBars;
      end;
    end;

  end;
end;

function TLogMemo.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  scrollPos : integer;
begin
  result := true;
  if RowCount = 0 then
    exit;

  if IsAtEnd then //nothing to do
    exit;

  scrollPos := Min(FVScrollPos + 1, RowCount - 1) ;
  ScrollBarScroll(Self,TScrollCode.scLineDown, scrollPos );
end;

function TLogMemo.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  scrollPos : integer;
begin
  result := true;
  if RowCount = 0 then
    exit;

  if IsAtTop then  //nothing to do
    exit;

  scrollPos := Max(0, FVScrollPos - 1);
  ScrollBarScroll(Self,TScrollCode.scLineUp, scrollPos );
end;

procedure TLogMemo.DoPageDown(const fromScrollBar: boolean; const newScrollPostition: integer);
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  pageSize : integer;
  rowState : TPaintRowState;
  fullRepaint : boolean;
  delta : integer;
begin
  if RowCount = 0 then
    exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  fullRepaint := false;
  delta := 0;

  if fromScrollBar  then
  begin
    //we do not change current row here.
    FTopRow := newScrollPostition;
    if FTopRow > (RowCount - FSelectableRows) then
      FTopRow := RowCount - FSelectableRows;
    if oldTopRow <> FTopRow then
    begin
      delta := FTopRow - oldTopRow;
      fullRepaint := true;
    end
  end
  else
  begin //from keyboard
    pageSize := Min(FSelectableRows, RowCount -1);

    if RowInView(FCurrentRow) and ((FCurrentRow + pageSize) <= (FTopRow +  FSelectableRows)) then
    begin
      FCurrentRow := FTopRow + FSelectableRows -1;
    end
    else
    begin
      Inc(FCurrentRow, pageSize);
      FCurrentRow := Min(FCurrentRow, RowCount -1);
      //position current row at the bottom
      FTopRow := Max(0,FCurrentRow - FSelectableRows + 1);
      fullRepaint := true;
    end;
    delta := FCurrentRow - oldCurrentRow;
  end;
  if delta > 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoPaintRow(oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoPaintRow(FCurrentRow , rowState);
    end
    else
      Invalidate;

    //DoRowChanged(oldCurrentRow);
    FVScrollPos := FTopRow;
    UpdateScrollBars;
  end;
end;

procedure TLogMemo.DoPageUp(const fromScrollBar: boolean; const newScrollPostition: integer);
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TPaintRowState;
  fullRepaint : boolean;
  delta : integer;
  pageSize : integer;
begin
  if RowCount = 0 then
    exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  fullRepaint := false;
  delta := 0;
  if fromScrollBar then
  begin
    FTopRow := newScrollPostition;
    FCurrentRow := 0;
    if oldTopRow <> FTopRow then
    begin
      delta := FTopRow - oldTopRow;
      fullRepaint := true;
    end
    else if oldCurrentRow <> FCurrentRow then
      delta := FCurrentRow  - oldCurrentRow;
  end
  else
  begin
    //from keyboard
    pageSize := Min(FSelectableRows, RowCount -1);
    if RowInView(FCurrentRow) and (FCurrentRow > FTopRow)  then
    begin
      FCurrentRow := FTopRow;
    end
    else
    begin
      Dec(FTopRow, pageSize);
      FTopRow := Max(FTopRow, 0);
      FCurrentRow := FTopRow;
      fullRepaint := true;
    end;
    delta := FCurrentRow  - oldCurrentRow;
  end;


  if delta < 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoPaintRow(oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoPaintRow(FCurrentRow , rowState);
    end
    else
      Invalidate;

    //DoRowChanged(oldCurrentRow);
    FVScrollPos := FTopRow;
    UpdateScrollBars;
  end;
end;

procedure TLogMemo.DoPaintRow(const index: integer; const state: TPaintRowState; const copyCanvas: boolean);
var
  viewRow : integer;
  rowRect : TRect;
  destRect : TRect;
  LCanvas : TCanvas;
  txt : string;
  backgroundColor : TColor;
  fontColor : TColor;
  messageType : TLogMessageType;
begin
  if not HandleAllocated then
    exit;

  LCanvas := FPaintBmp.Canvas;
  LCanvas.Font.Assign(Self.Font);
  LCanvas.Brush.Style := bsSolid;
  messageType := TLogMessageType(FItems.Objects[index]);


  viewRow := GetViewRow(index);
  rowRect := ClientRect;
  rowRect.Top := viewRow * FRowHeight;
  rowRect.Bottom := rowRect.Top + FRowHeight;
  rowRect.Width := FPaintBmp.Width; //paint bitmap may be wider.


  if (state in [TPaintRowState.rsFocusedSelected, TPaintRowState.rsFocusedHot, TPaintRowState.rsHot]) then
  begin
    backgroundColor := FStyleServices.GetSystemColor(clHighlight);
    fontColor := FStyleServices.GetSystemColor(clHighlightText);
  end
  else
  begin
    backgroundColor := FStyleServices.GetSystemColor(clWindow);
    fontColor := FMessageColors[FThemeType][messageType];
  end;

  if messageType in [mtImportantInformation, mtImportantSuccess, mtImportantVerbose, mtImportantWarning,mtError] then
    LCanvas.Font.Style := [fsBold];

  LCanvas.Brush.Color := backgroundColor;
  LCanvas.Font.Color := fontColor;
  LCanvas.FillRect(rowRect);

  LCanvas.Brush.Style := bsClear;
  txt := FItems.Strings[index];
  LCanvas.TextRect(rowRect, txt, [tfLeft, tfSingleLine, tfVerticalCenter] );

  destRect := rowRect;

  OffsetRect(rowRect, FHScrollPos,0);


  if copyCanvas then
    Canvas.CopyRect(destRect, LCanvas, rowRect);

end;

procedure TLogMemo.DoTrack(const newScrollPostition: integer);
var
  oldTopRow : integer;
begin
  oldTopRow := FTopRow;
  FTopRow := newScrollPostition;
  FVScrollPos := FTopRow;
  if oldTopRow <> FTopRow then
  begin
    Invalidate;
    UpdateScrollBars;
  end;
end;

function TLogMemo.GetMaxWidth: integer;
begin
  result := Max(ClientWidth, FMaxWidth);
end;

function TLogMemo.GetRowCount: integer;
begin
  result := FItems.Count;
end;

function TLogMemo.GetRowFromY(const Y: integer): integer;
begin
  result := (Y div FRowHeight); //this is probably not quite right.
end;

function TLogMemo.GetRowPaintState(const rowIdx: integer): TPaintRowState;
begin
  if Self.Focused then
  begin
    result := TPaintRowState.rsFocusedNormal;
    if RowInView(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := TPaintRowState.rsFocusedSelected
      else if rowIdx = FHoverRow then
        result := TPaintRowState.rsFocusedHot;
    end;
  end
  else
  begin
    result := TPaintRowState.rsNormal;
    if RowInview(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := TPaintRowState.rsSelected
      else if rowIdx = FHoverRow then
        result := TPaintRowState.rsHot;
    end;
  end;
end;

function TLogMemo.GetText: string;
begin
  result := FItems.Text;
end;

function TLogMemo.GetViewRow(const row: integer): integer;
begin
  result := row - FTopRow;
  if result < 0 then
    result := 0;
  if result > FVisibleRows -1 then
    result := FVisibleRows -1;
end;

function TLogMemo.IsAtEnd: boolean;
begin
  result := FVScrollPos = RowCount - FSelectableRows;
end;

function TLogMemo.IsAtTop: boolean;
begin
  result := FVScrollPos = 0;
end;

procedure TLogMemo.Loaded;
begin
  // Set the bmp to the size of the control.
  FPaintBmp.SetSize(Width, Height);
  inherited;

end;

procedure TLogMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
var
  row : Integer;
begin
  inherited;
  SetFocus;
  if RowCount = 0 then
    exit;
  row := GetRowFromY(Y);
  if (row > FVisibleRows) or (row >= RowCount) then
    exit;

  if FTopRow + row <> FCurrentRow then
  begin
    FCurrentRow := FTopRow + row;
    Invalidate;
    UpdateScrollBars
  end;
end;

procedure TLogMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateHoverRow(X, Y);

end;

procedure TLogMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TLogMemo.Paint;
var
  LCanvas : TCanvas;
  backgroundColor : TColor;
  r : TRect;
  i: Integer;
  rowIdx : integer;
  rowState : TPaintRowState;
begin
  LCanvas := FPaintBmp.Canvas;
  LCanvas.Font.Assign(Self.Font);

  if FStyleServices.Enabled {$IF CompilerVersion >= 24.0} and (seClient in StyleElements) {$IFEND} then
    backgroundColor := FStyleServices.GetSystemColor(clWindow)
  else
    backgroundColor := clWindow;

  //paint background
  r := Self.ClientRect;
  r.Width := Max(FPaintBmp.Width, r.Width); //paintbmp may be wider than the client.
  LCanvas.Brush.Style := bsSolid;
  LCanvas.Brush.Color := backgroundColor;
  LCanvas.FillRect(r);

  //paint all visible rows
  for i := 0 to FVisibleRows - 1 do
  begin
    rowIdx := FTopRow + i;
    if rowIdx >= RowCount then
      break;
    rowState := GetRowPaintState(rowIdx);
    DoPaintRow(rowIdx, rowState, false);
  end;


  r := ClientRect;
  OffsetRect(r, FHScrollPos,0);

  BitBlt(Canvas.Handle,0,0,r.Width, r.Height, FPaintBmp.Canvas.Handle,r.Left, r.Top, SRCCOPY);
//  Canvas.CopyRect(ClientRect, FPaintBmp.Canvas, r); //uses strechblt

end;

procedure TLogMemo.Resize;
var
  NewWidth, NewHeight: integer;
begin
  if (not HandleAllocated) then
    Exit;

  if csDestroying in ComponentState then
    Exit;

  NewWidth := Max(ClientWidth, GetMaxWidth);
  NewHeight := ClientHeight;

  FMaxWidth := Min(FMaxWidth, NewWidth);

  if (NewWidth <> FPaintBmp.Width) or (NewHeight <> FPaintBmp.Height) then
  begin
    // TBitmap does some stuff to try and preserve contents
    // which slows things down a lot - this avoids that
    FPaintBmp.SetSize(0, 0);
    FPaintBmp.SetSize(NewWidth, NewHeight);
  end;

  UpdateVisibleRows;

  if FMaxWidth <= ClientWidth then
    FHScrollPos := 0
  else if FHScrollPos > (FMaxWidth - ClientWidth) then
    FHScrollPos := FMaxWidth - ClientWidth;

  if FVisibleRows > RowCount then
  begin
    FTopRow := 0;
    FVScrollPos := 0;
  end
  else if (RowCount - FTopRow + 1) < FVisibleRows then
  begin
    FTopRow := RowCount - FVisibleRows;
  end;

  if (RowCount > 0) and (FCurrentRow > -1) then
  begin
    if not RowInView(FCurrentRow) then
      ScrollInView(FCurrentRow, false);

  end;
//  Refresh;
  UpdateScrollBars;
  Invalidate;

//  //force repainting scrollbars
//  if sfHandleMessages in StyleServices.Flags then
//    SendMessage(Handle, WM_NCPAINT, 0, 0);
  inherited;
end;

function TLogMemo.RowInView(const row: integer): boolean;
begin
  result := (row >= FTopRow) and (row < (FTopRow + FSelectableRows));
end;

procedure TLogMemo.RowsChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    if HandleAllocated then
    begin
      UpdateVisibleRows;
      Invalidate;
    end;
  end;
end;

procedure TLogMemo.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case scrollCode of
    TScrollCode.scLineUp: DoLineUp(true);
    TScrollCode.scLineDown: DoLineDown(true);
    TScrollCode.scPageUp:
    begin
      ScrollPos :=  FVScrollPos - FSelectableRows;
      if ScrollPos < 0 then
        ScrollPos := 0;
      DoPageUp(true,ScrollPos);
    end;
    TScrollCode.scPageDown:
    begin
      ScrollPos :=  FVScrollPos + FSelectableRows;
      if ScrollPos > RowCount -1 then
        ScrollPos := RowCount - 1;
      DoPageDown(true, ScrollPos);
    end;
    TScrollCode.scPosition,
    TScrollCode.scTrack:
    begin
      DoTrack(ScrollPos);
    end;
    TScrollCode.scTop: DoGoTop;
    TScrollCode.scBottom: DoGoBottom;
    TScrollCode.scEndScroll: ;
  end;

end;

procedure TLogMemo.ScrollInView(const index: integer; const updateSBs : boolean = true);
begin
  if (RowCount = 0) or (index > RowCount -1) then
    exit;

  //Figure out what the top row should be to make the current row visible.
  //current row below bottom of vieww
  if index >= (FTopRow + FSelectableRows) then
    FTopRow := Max(0, index - FSelectableRows + 1)
  else //above
    FTopRow := Min(index, RowCount - FVisibleRows );
  if FTopRow < 0 then
    FTopRow := 0;

  FVScrollPos := FTopRow;
//  Update;
  if updateSBs then
    UpdateScrollBars;
  Invalidate;

end;

procedure TLogMemo.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TLogMemo.SetRowHeight(const Value: integer);
begin
  if FRowHeight <> value then
  begin
    FRowHeight := Value;
    UpdateVisibleRows;
    Invalidate;
  end;
end;

procedure TLogMemo.SetStyleServices(const Value: TCustomStyleServices);
begin
  FStyleServices := Value;
  CheckTheme;
end;

procedure TLogMemo.ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$ifend});
begin
  FMaxWidth := MulDiv(FMaxWidth, M, D);
  UpdateScrollBars;
  inherited;
end;

procedure TLogMemo.CheckTheme;
begin
  if TStringUtils.Contains(FStyleServices.Name, 'Dark') then
    FThemeType := TThemeType.Dark
  else
    FThemeType := TThemeType.Light;
end;

procedure TLogMemo.UpdateHoverRow(const X, Y: integer);
var
  row : Integer;
  oldHoverRow : integer;
  rowState : TPaintRowState;
begin
  row := FTopRow + GetRowFromY(Y);
  if (row <> FHoverRow) and (row < FItems.Count) then
  begin
    oldHoverRow := FHoverRow;
    FHoverRow := row;
    if (oldHoverRow <> -1) and RowInView(oldHoverRow) then
    begin
      rowState := GetRowPaintState(oldHoverRow);
      DoPaintRow(oldHoverRow, rowState);
    end;
    if (FHoverRow > -1) and RowInView(FHoverRow)  then
    begin
      rowState := GetRowPaintState(FHoverRow);
      DoPaintRow(FHoverRow , rowState);
    end
  end;
end;

procedure TLogMemo.UpdateScrollBars;
var
  sbInfo : TScrollInfo;
begin
  if not HandleAllocated then
    exit;
  if FUpdatingScrollBars  then
    exit;

  FUpdatingScrollBars := true;
  try
    sbInfo.cbSize := SizeOf(TScrollInfo);
    sbInfo.fMask := SIF_ALL;
    sbInfo.nMin := 0;

    //Note : this may trigger a resize if the visibility changes
    if RowCount <= FSelectableRows  then
    begin
      sbInfo.nMax := 0;
      sbInfo.nPage := 0;
      sbInfo.nPos := 0;
      SetScrollInfo(Handle, SB_VERT, sbInfo, True);
    end
    else
    begin
      sbInfo.nMax := Max(RowCount -1, 0);
      sbInfo.nPage := Min(FSelectableRows, RowCount -1);
      sbInfo.nPos := Min(FVScrollPos, RowCount -1) ;
      SetScrollInfo(Handle, SB_VERT, sbInfo, True);
    end;

    sbInfo.cbSize := SizeOf(TScrollInfo);
    sbInfo.fMask := SIF_ALL;
    sbInfo.nMin := 0;
    sbInfo.nTrackPos := 0;

    if FMaxWidth <= ClientWidth then
    begin
      FHScrollPos := 0;
      sbInfo.nMax := 0;
      sbInfo.nPage := 0;
      sbInfo.nPos := 0;
      SetScrollInfo(Handle, SB_HORZ, sbInfo, True);
    end
    else
    begin
      sbInfo.nMax := Max(FMaxWidth, 0);
      sbInfo.nPage := ClientWidth;
      sbInfo.nPos := FHScrollPos;
      SetScrollInfo(Handle, SB_HORZ, sbInfo, True);
    end;
  finally
    FUpdatingScrollBars := false;
  end;
end;

procedure TLogMemo.UpdateVisibleRows;
begin
  FHoverRow := -1;
  if FUpdating then
    exit;
  FUpdating := true;
  if HandleAllocated then
  begin
    FVisibleRows :=  ClientHeight div RowHeight;
    FSelectableRows := Min(FVisibleRows, RowCount); //the number of full rows
    if (RowCount > FVisibleRows) and (ClientHeight mod RowHeight > 0) then
    begin
      //add 1 to ensure a partial row is painted.
      FVisibleRows  := FVisibleRows + 1;
    end;
    UpdateScrollBars;
  end;
  FUpdating := false;
end;

procedure TLogMemo.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1; //we will paint the background
end;

procedure TLogMemo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TLogMemo.WMHScroll(var Message: TWMVScroll);
var
  info : TScrollInfo;
begin
  Message.Result := 0;
  if FUpdatingScrollBars then
    exit;
  with Message do
  begin
    if ScrollCode = 8 then
      exit;
    ZeroMemory(@info, Sizeof(TScrollInfo));
    info.cbSize := SizeOf(TScrollInfo);
    info.fMask := SIF_ALL;
    GetScrollInfo(Self.Handle,SB_HORZ, info);

    case TScrollCode(scrollCode) of
      TScrollCode.scLineUp: FHScrollPos := Max(0, FHScrollPos -1) ;
      TScrollCode.scLineDown: FHScrollPos := Min(FMaxWidth - ClientWidth, FHScrollPos + 1) ;
      TScrollCode.scPageUp:
      begin
        FHScrollPos := Max(0, FHScrollPos - integer(info.nPage))
      end;
      TScrollCode.scPageDown:
      begin
        FHScrollPos := Min(FHScrollPos + integer(info.nPage), FMaxWidth - ClientWidth );
      end;
      TScrollCode.scPosition,
      TScrollCode.scTrack:
      begin
        FHScrollPos := Min(info.nTrackPos, FMaxWidth - ClientWidth )
      end;
      TScrollCode.scTop: FHScrollPos := 0;
      TScrollCode.scBottom: FHScrollPos := FMaxWidth;
  //    TScrollCode.scEndScroll: ;
    end;
    UpdateScrollBars;
    Refresh;
  end;
end;


procedure TLogMemo.WMVScroll(var Message: TWMVScroll);
var
  info : TScrollInfo;
begin
  Message.Result := 0;
  with Message do
  begin
    if ScrollCode = 8 then
      exit;
    info.cbSize := SizeOf(TScrollInfo);
    info.fMask := SIF_TRACKPOS;
    GetScrollInfo(Self.Handle,SB_VERT, info);
    Self.ScrollBarScroll(Self, TScrollCode(ScrollCode), info.nTrackPos);
  end;
end;

end.