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


unit DPM.Controls.VersionGrid;

interface

uses
  System.Classes,
  Generics.Collections,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.ImgList,
  DPM.Core.Types;

//A control to show the list of projects and the package version installed when working with project groups.
//code heavily borrows from VSoft.VirtualListView


type
  TVersionGridColumn = record
    Index : integer;
    Title : string;
    Width : integer;
    Left  : integer;
    Height : integer;
    function GetBounds : TRect;
  end;

  TVersionGridRow = class
    ProjectName : string;
    InstalledVersion : TPackageVersion;
  end;


  TVersionGridPaintRowState = (rsNormal, rsHot, rsSelected, rsFocusedNormal, rsFocusedHot, rsFocusedSelected);

  THitElement = (htRow, htColumnProject, htColumnInstalleVer, htColumnInstall, htColumnUpDn, htColumnRemove, htRowInstall, htRowUpDn, htRowRemove  );

  TOnInstallEvent = procedure(const project : string) of object;
  TOnUnInstallEvent = procedure(const project : string) of object;
  TOnUpgradeEvent = procedure(const project : string) of object;
  TOnDowngradeEvent = procedure(const project : string) of object;

  TVersionGrid = class(TCustomControl)
  private
    FBorderStyle : TBorderStyle;

    FColumns : TArray<TVersionGridColumn>;
    FRows : TObjectList<TVersionGridRow>;

    FPaintBmp : TBitmap;

    FRowHeight : integer;
    FTopRow :  Integer; //this is the top row cursor .
    FCurrentRow : Integer; //this is our currently selected row.
    FHoverRow : integer; //mouse over row in view (add to top row to get index)
    FUpdating : boolean;
    FVScrollPos : integer;
    FHScrollPos : integer;

    FVisibleRows : integer; //number of rows we can see
    FSelectableRows : integer;

    FIDEStyleServices : TCustomStyleServices;

    FHitElement : THitElement;

    FUpdateCount : integer;

    FPackageVersion: TPackageVersion;

    FSelectionChangedEvent : TNotifyEvent;

    FImageList : TCustomImageList;

    //events
    FOnInstallEvent : TOnInstallEvent;
    FOnUnInstallEvent : TOnUnInstallEvent;
    FOnUpgradeEvent : TOnUpgradeEvent;
    FOnDowngradeEvent : TOnDowngradeEvent;


    procedure SetImageList(const Value: TCustomImageList);

  protected
    function GetProjectName(index: integer): string;
    function GetRowCount : integer;
    procedure SetPackageVersion(const value : TPackageVersion);
    function GetProjectVersion(index: integer): TPackageVersion;
    procedure SetProjectVersion(index: integer; const Value: TPackageVersion);

    function GetTotalColumnWidth : integer;

    procedure SetRowHeight(const Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);

    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;


    function RowInView(const row: integer): boolean;
    procedure UpdateVisibleRows;
    procedure RowsChanged(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;  var ScrollPos: Integer);
    function GetRowPaintState(const rowIdx : integer) : TVersionGridPaintRowState;
    procedure ScrollInView(const index : integer);
    function GetViewRow(const row : integer) : integer;
    function IsAtTop : boolean;
    function IsAtEnd : boolean;
    function GetRowFromY(const Y : integer) : integer;
    procedure UpdateHoverRow(const X, Y : integer);
    procedure SetStyleServices(const Value: TCustomStyleServices);


    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint;override;
    procedure DoPaintRow(const index : integer; const state : TVersionGridPaintRowState; const copyCanvas : boolean = true);

    function GetButtonEnabled(const index : integer; const element : THitElement) : boolean;

    procedure UpdateScrollBars;
    procedure UpdateColumns;



    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMVScroll); message WM_HSCROLL;

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;




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

    procedure DoSelectionChanged;

    function GetHasAnyInstalled : boolean;

  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    class constructor Create;
    class destructor Destroy;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND} ); override;
    procedure Clear;
    procedure AddProject(const project : string; const version : string);
    function GetInstalledProjects : TArray<string>;
    function GetNotInstalledProjects : TArray<string>;
    property CurrentRow : Integer read FCurrentRow;
    property TopRow : Integer read FTopRow;

    property HasAnyInstalled : boolean read GetHasAnyInstalled;
    property ImageList : TCustomImageList read FImageList write SetImageList;
    property ProjectName[index : integer] : string read GetProjectName;
    property ProjectVersion[index : integer] : TPackageVersion read GetProjectVersion write SetProjectVersion;

    property PackageVersion : TPackageVersion read FPackageVersion write SetPackageVersion;
    property RowCount : integer read GetRowCount;

    property OnSelectionChanged : TNotifyEvent read FSelectionChangedEvent write FSelectionChangedEvent;

    //exposing this here so the IDE plugin can set this.
    property StyleServices : TCustomStyleServices read FIDEStyleServices write SetStyleServices;
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
    property RowHeight : integer read FRowHeight write SetRowHeight default 24;
    property OnInstallEvent : TOnInstallEvent read FOnInstallEvent write FOnInstallEvent;
    property OnUnInstallEvent : TOnUnInstallEvent read FOnUnInstallEvent write FOnUnInstallEvent;
    property OnUpgradeEvent : TOnUpgradeEvent read FOnUpgradeEvent write FOnUpgradeEvent;
    property OnDowngradeEvent : TOnDowngradeEvent read FOnDowngradeEvent write FOnDowngradeEvent;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  Vcl.Dialogs,
  DPM.Core.Utils.Strings;

{ TVersionGrid }

procedure TVersionGrid.AddProject(const project, version: string);
var
  newProject : TVersionGridRow;
begin
  newProject := TVersionGridRow.Create;
  newProject.ProjectName := project;
  newProject.InstalledVersion := TPackageVersion.Empty;
  FRows.Add(newProject);
  RowsChanged(Self);
end;

procedure TVersionGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TVersionGrid.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND} );
begin
  inherited;
  FRowHeight := MulDiv(FRowHeight, M, D);
  //for some reason this is not happening in D11.x
  Canvas.Font.Height := MulDiv(Canvas.Font.Height, M, D );
//  FPaintBmp.Canvas.Font := Self.Font;
  UpdateVisibleRows;

end;

procedure TVersionGrid.Clear;
begin
  FRows.Clear;
end;

procedure TVersionGrid.CMEnter(var Message: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TVersionGrid.CMExit(var Message: TCMExit);
begin
  FHoverRow := -1;
  FHitElement := htRow;
  Invalidate;
  inherited;
end;

procedure TVersionGrid.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TVersionGrid.CMMouseLeave(var Msg: TMessage);
var
  oldHoverRow : integer;
  rowState : TVersionGridPaintRowState;
begin
  if FHitElement in [htRow] then
  begin
    oldHoverRow := FHoverRow;
    FHoverRow := -1;
    FHitElement := htRow;
    if (oldHoverRow <> -1) and RowInView(oldHoverRow) then
    begin
      rowState := GetRowPaintState(oldHoverRow);
      DoPaintRow(oldHoverRow, rowState);
    end;
  end
  else
  begin
    FHitElement := htRow;
    Invalidate;
  end;

  inherited;
end;

constructor TVersionGrid.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateCount := 0;
  FBorderStyle := bsSingle;
  FPaintBmp := TBitmap.Create;
  FPaintBmp.PixelFormat := pf32bit;
  FRowHeight := 24;
  Color := clWindow;

 //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}

  //IOTAIDEThemingServices added in 10.2
  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$ENDIF}





  FRows := TObjectList<TVersionGridRow>.Create(true);

  SetLength(FColumns, 5);
  UpdateColumns;

  ControlStyle := [csDoubleClicks, csCaptureMouse, csDisplayDragImage, csClickEvents, csPannable];
  TabStop := true;
  ParentBackground := true;
  ParentColor := true;
  ParentDoubleBuffered := false;
  DoubleBuffered := false;
  Width := 300;
  Height := 250;

  FVScrollPos := 0;
  FHScrollPos := 0;

end;

class constructor TVersionGrid.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TVersionGrid, TScrollingStyleHook);

end;

procedure TVersionGrid.CreateHandle;
begin
  inherited;
  Resize;
end;

procedure TVersionGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  Params.Style := Params.Style + WS_VSCROLL;
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);

end;

procedure TVersionGrid.CreateWnd;
begin
  inherited;
  UpdateVisibleRows;
  Invalidate;
end;

destructor TVersionGrid.Destroy;
begin
  FPaintBmp.Free;
  FRows.Free;
  inherited;
end;

class destructor TVersionGrid.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TVersionGrid, TScrollingStyleHook);
end;

procedure TVersionGrid.DoGoBottom;
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TVersionGridPaintRowState;
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
    Invalidate
  else if FCurrentRow <> oldCurrentRow then
  begin
    rowState := GetRowPaintState(oldCurrentRow);
    DoPaintRow(oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoPaintRow(FCurrentRow , rowState);
  end;
  //DoRowChanged(oldCurrentRow);
  UpdateScrollBars;

end;

procedure TVersionGrid.DoGoTop;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  rowState : TVersionGridPaintRowState;
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
      Invalidate;
    end;
    FVScrollPos := 0;
    UpdateScrollBars;
    //DoRowChanged(oldCurrentRow);
  end;
end;

procedure TVersionGrid.DoLineDown(const fromScrollBar: boolean);
var
  oldCurrentRow : integer;
  rowState : TVersionGridPaintRowState;
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
    Invalidate;
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

procedure TVersionGrid.DoLineUp(const fromScrollBar: boolean);
var
  oldCurrentRow : integer;
  rowState : TVersionGridPaintRowState;
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
        //DoRowChanged(oldCurrentRow);
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
          //DoRowChanged(oldCurrentRow);
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
        //DoRowChanged(oldCurrentRow);
        Invalidate;
        UpdateScrollBars;
      end;
    end;

  end;
end;

function TVersionGrid.DoMouseWheelDown(Shift: TShiftState;  MousePos: TPoint): Boolean;
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

function TVersionGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
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

procedure TVersionGrid.DoPageDown(const fromScrollBar: boolean; const newScrollPostition: integer);
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  pageSize : integer;
  rowState : TVersionGridPaintRowState;
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

procedure TVersionGrid.DoPageUp(const fromScrollBar: boolean; const newScrollPostition: integer);
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TVersionGridPaintRowState;
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

procedure TVersionGrid.DoPaintRow(const index: integer; const state: TVersionGridPaintRowState; const copyCanvas : boolean);
var
  viewRow : integer;
  rowRect : TRect;
  destRect : TRect;
  i: Integer;
  colRect : TRect;
  imgRect : TRect;
  btnRect : TRect;
  txt : string;
  LCanvas : TCanvas;

  backgroundColor : TColor;
  fontColor : TColor;
  btnColor : TColor;

  project : TVersionGridRow;

  procedure CenterRect(const outerRect : TRect; var innerRect : TRect);
  var
    iw, ih : Integer;
  begin
    iw := innerRect.Width;
    ih := innerRect.Height;
    innerRect.Left := outerRect.Left + ((outerRect.Width - innerRect.Width) div 2);
    innerRect.Top := outerRect.Top + ((outerRect.Height - innerRect.Height) div 2);
    innerRect.Width := iw;
    innerRect.Height := ih;
  end;

  procedure DrawButton;
  begin
    LCanvas.Brush.Color := btnColor;
    LCanvas.Pen.Color := btnColor;
    LCanvas.RoundRect(btnRect,2,2);
  end;

begin
  if not HandleAllocated then
    exit;

  LCanvas := FPaintBmp.Canvas;
  LCanvas.Brush.Style := bsSolid;

  viewRow := GetViewRow(index);
  rowRect := ClientRect;
  rowRect.Top := FRowHeight +  viewRow * FRowHeight;
  rowRect.Bottom := rowRect.Top + FRowHeight;
  rowRect.Width := Max(rowRect.Width, FPaintBmp.Width);

  project := FRows.Items[index];

  if (state in [rsSelected, rsFocusedSelected, rsFocusedHot, rsHot]) then
  begin
    {$IF CompilerVersion < 32.0}
    backgroundColor := $00FFF0E9;
    fontcolor := FIDEStyleServices.GetSystemColor(clWindowText);
    {$ELSE}
    if state = TVersionGridPaintRowState.rsFocusedSelected then
      backgroundColor := FIDEStyleServices.GetSystemColor(clBtnHighlight)
    else
      backgroundColor := FIDEStyleServices.GetSystemColor(clBtnShadow);
    fontcolor := FIDEStyleServices.GetSystemColor(clHighlightText);
    {$IFEND}
  end
  else
  begin
    backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);
    fontColor := FIDEStyleServices.GetSystemColor(clWindowText);
  end;

  btnColor := FIDEStyleServices.GetSystemColor(clBtnFace);
  LCanvas.Brush.Color := backgroundColor;
  LCanvas.Font.Color := fontColor;
  LCanvas.FillRect(rowRect);

  LCanvas.Brush.Style := bsClear;


  for i := 0 to 4 do
  begin
    colRect := FColumns[i].GetBounds;
    colRect.Top := rowRect.Top;
    colRect.Bottom := rowRect.Bottom;
    colRect.Inflate(-5, 0);
    imgRect := TRect.Create(0,0,FImageList.Width, FImageList.Height);
    CenterRect(colRect, imgRect);
    btnRect := imgRect;
    btnRect.Inflate(3,3);
    txt := '';
    case i of
      0 : //project
      begin
        txt := ChangeFileExt(ExtractFileName(project.ProjectName),'');
      end;
      1 : //installed version
      begin
        if not project.InstalledVersion.IsEmpty then
          txt := project.InstalledVersion.ToStringNoMeta;
      end;
      2 : //add
      begin
        if project.InstalledVersion.IsEmpty then
        begin
          DrawButton;
          FImageList.Draw(LCanvas, imgRect.Left, imgRect.Top, 0,  TDrawingStyle.dsTransparent, TImageType.itImage, true );
        end;
      end;
      3 :  //upgrade/downgrade
      begin
        if FPackageVersion.IsEmpty then
          continue;

        if (not project.InstalledVersion.IsEmpty)  then //a version is installed already
        begin
          if FPackageVersion = project.InstalledVersion then
            continue;
          DrawButton;

          if FPackageVersion > project.InstalledVersion then
            FImageList.Draw(LCanvas, imgRect.Left, imgRect.Top, 2, TDrawingStyle.dsTransparent, TImageType.itImage, true )
          else
            FImageList.Draw(LCanvas, imgRect.Left, imgRect.Top, 3, TDrawingStyle.dsTransparent, TImageType.itImage, true );
        end;
      end;
      4 :
      begin
        if not project.InstalledVersion.IsEmpty then
        begin
          DrawButton;
          FImageList.Draw(LCanvas, imgRect.Left, imgRect.Top, 1, TDrawingStyle.dsTransparent, TImageType.itImage, true );
        end;
      end;
    end;
    if txt <> '' then
      DrawText(LCanvas.Handle, PChar(txt),Length(txt), colRect, DT_SINGLELINE + DT_LEFT + DT_VCENTER );

  end;
  destRect := rowRect;

  OffsetRect(rowRect, FHScrollPos,0);


  if copyCanvas then
    Canvas.CopyRect(destRect, LCanvas, rowRect);

end;

procedure TVersionGrid.DoSelectionChanged;
begin
  if Assigned(FSelectionChangedEvent) then
    FSelectionChangedEvent(Self);
end;

procedure TVersionGrid.DoTrack(const newScrollPostition: integer);
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

procedure TVersionGrid.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    if HandleAllocated then
    begin
      UpdateVisibleRows;
      Invalidate;
    end;
  end;
end;


function TVersionGrid.GetButtonEnabled(const index: integer; const element: THitElement): boolean;
var
  row : TVersionGridRow;
begin
  result := false;
  if (index >= 0) and (index < FRows.Count)  then
  begin
    row := FRows[index];
    case element of
      htRowInstall:
      begin
        result := row.InstalledVersion.IsEmpty;
      end;
      htRowUpDn:
      begin
        if FPackageVersion.IsEmpty or row.InstalledVersion.IsEmpty then
          exit;

        if (not row.InstalledVersion.IsEmpty) then
        begin
          result := FPackageVersion <> row.InstalledVersion;
        end;
      end;
      htRowRemove:
      begin
        result := not row.InstalledVersion.IsEmpty;
      end;
    end;
  end;

end;
function TVersionGrid.GetHasAnyInstalled: boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to FRows.Count -1 do
  begin
    if not FRows[i].InstalledVersion.IsEmpty then
      exit(true);
  end;
end;

function TVersionGrid.GetNotInstalledProjects: TArray<string>;
var
  list : TList<string>;
  i : integer;
begin
  result := [];
  list := TList<string>.Create;
  try
    for i := 0 to FRows.Count -1 do
    begin
      if FRows[i].InstalledVersion.IsEmpty then
        list.Add(FRows[i].ProjectName);
    end;
  finally
    result := list.ToArray;
    list.Free;
  end;
end;

function TVersionGrid.GetInstalledProjects: TArray<string>;
var
  list : TList<string>;
  i : integer;
begin
  result := [];
  list := TList<string>.Create;
  try
    for i := 0 to FRows.Count -1 do
    begin
      if not FRows[i].InstalledVersion.IsEmpty then
        list.Add(FRows[i].ProjectName);
    end;
  finally
    result := list.ToArray;
    list.Free;
  end;

end;

function TVersionGrid.GetProjectName(index: integer): string;
begin
  if (index >= 0) and (index < FRows.Count) then
    result := FRows[index].ProjectName
  else
    result := '';
end;

function TVersionGrid.GetProjectVersion(index: integer): TPackageVersion;
begin
  if (index >= 0) and (index < FRows.Count) then
    result := FRows[index].InstalledVersion
  else
    result := TPackageVersion.Empty;
end;


function TVersionGrid.GetRowCount: integer;
begin
  result := FRows.Count;
end;

function TVersionGrid.GetRowFromY(const Y: integer): integer;
begin
  result := (Y div FRowHeight) -1; //this is probably not quite right.
end;

function TVersionGrid.GetRowPaintState(const rowIdx: integer): TVersionGridPaintRowState;
begin
  if Self.Focused then
  begin
    result := rsFocusedNormal;
    if RowInView(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := rsFocusedSelected
      else if rowIdx = FHoverRow then
        result := rsFocusedHot;
    end;
  end
  else
  begin
    result := rsNormal;
    if RowInview(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := rsSelected
      else if rowIdx = FHoverRow then
        result := rsHot;
    end;
  end;
end;

function TVersionGrid.GetTotalColumnWidth: integer;
begin
  result := FColumns[3].Left + FColumns[3].Width;
end;

function TVersionGrid.GetViewRow(const row: integer): integer;
begin
  result := row - FTopRow;
  if result < 0 then
    result := 0;
  if result > FVisibleRows -1 then
    result := FVisibleRows -1;
end;

function TVersionGrid.IsAtEnd: boolean;
begin
  result := FVScrollPos = RowCount - FSelectableRows;
end;

function TVersionGrid.IsAtTop: boolean;
begin
  result := FVScrollPos = 0;
end;

procedure TVersionGrid.Loaded;
begin
  inherited;
  // Set the bmp to the size of the control.
  FPaintBmp.SetSize(Width, Height);

end;

procedure TVersionGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row : Integer;
begin
  inherited;
  if RowCount = 0 then
    exit;

  SetFocus;
  row := GetRowFromY(Y);

  if (row > FVisibleRows) or (row >= RowCount) then
    exit;

  if FTopRow + row <> FCurrentRow then
  begin
    FCurrentRow := FTopRow + row;
    Invalidate;
    UpdateScrollBars;
  end;

end;

procedure TVersionGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r : TRect;
  row : integer;
  oldHit : THitElement;
begin
  oldHit := FHitElement;
  FHitElement := htRow;
  row := GetRowFromY(Y);
  try
    if Y > FRowHeight then
    begin
      r := FColumns[2].GetBounds;
      r.Offset(0, (row + 1) * FRowHeight);
      if r.Contains(Point(X, Y)) then
      begin
        FHitElement := htRowInstall;
        exit;
      end;

      r := FColumns[3].GetBounds;
      r.Offset(0, (row + 1) * FRowHeight);
      if r.Contains(Point(X, Y)) then
      begin
        FHitElement := htRowUpDn;
        exit;
      end;

      r := FColumns[4].GetBounds;
      r.Offset(0, (row + 1) * FRowHeight);
      if r.Contains(Point(X, Y)) then
      begin
        FHitElement := htRowRemove;
        exit;
      end;
      FHitElement := htRow;
    end;
  finally
    if FHitElement <> oldHit then
      Invalidate;
    UpdateHoverRow(X, Y);
    Self.Cursor := crDefault;
  end;
end;


procedure TVersionGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  case FHitElement of
    htRowInstall :
    begin
      if Assigned(FOnInstallEvent) then
      begin
        if GetButtonEnabled(FCurrentRow, FHitElement) then
          FOnInstallEvent(FRows[FCurrentRow].ProjectName);
      end;
    end;
    htRowUpDn :
    begin
      if GetButtonEnabled(FCurrentRow, FHitElement) then
      begin
         if FPackageVersion > FRows[FCurrentRow].InstalledVersion then
         begin
          if Assigned(FOnUpgradeEvent) then
            FOnUpgradeEvent(FRows[FCurrentRow].ProjectName);
         end
         else
         begin
          if Assigned(FOnDowngradeEvent) then
            FOnDowngradeEvent(FRows[FCurrentRow].ProjectName);
         end;
      end;

    end;
    htRowRemove :
    begin
      if Assigned(FOnUnInstallEvent) then
      begin
        if GetButtonEnabled(FCurrentRow, FHitElement) then
          FOnUnInstallEvent(FRows[FCurrentRow].ProjectName);
      end;
    end;
  end;
  inherited;
end;

const
  hightLightElements : array[0..4] of THitElement = (htColumnProject,htColumnInstalleVer,htColumnInstall, htColumnUpDn,htColumnRemove);

procedure TVersionGrid.Paint;
var
  LCanvas : TCanvas;
  HeaderTextColor : TColor;
  HeaderBackgroundColor: TColor;
  HeaderBorderColor: TColor;
  backgroundColor : TColor;
  columnHightlightColor : TColor;
  r : TRect;
  i: Integer;
  rowIdx : integer;
  rowState : TVersionGridPaintRowState;
begin
  LCanvas := FPaintBmp.Canvas;
  LCanvas.Font.Assign(Self.Font);

  if FIDEStyleServices.Enabled {$IF CompilerVersion >= 24.0} and (seClient in StyleElements) {$IFEND} then
  begin
    //client
    backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);

    //header
    //TODO : When running in the IDE we will need to tweak this!
    if FIDEStyleServices.Name = 'Windows' then
    begin
      HeaderTextColor := FIDEStyleServices.GetStyleFontColor(sfHeaderSectionTextNormal);
      FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tgFixedCellNormal), ecBorderColor, HeaderBorderColor);
      FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tgFixedCellNormal ), ecFillColor, HeaderBackgroundColor);
      //FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tbPushButtonHot ), ecShadowColor, columnHightlightColor);
      columnHightlightColor := FIDEStyleServices.GetSystemColor(clBtnShadow);

    end
    else
    begin
      HeaderTextColor := FIDEStyleServices.GetSystemColor(clBtnText);

      FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tgClassicFixedCellNormal), ecBorderColor, HeaderBorderColor);
      FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tgClassicFixedCellNormal ), ecFillColor, HeaderBackgroundColor);
      FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tgClassicFixedCellHot ), ecFillColor, columnHightlightColor);
      //FIDEStyleServices.GetElementColor(FIDEStyleServices.GetElementDetails(tbPushButtonNormal ), ecTextColor, HeaderTextColor);
    end;
  end
  else
  begin
    //client
    backgroundColor := clWindow;
    //header
    HeaderBackgroundColor := clBtnFace;
    HeaderBorderColor := clWindowFrame;
    HeaderTextColor := clWindowText;
    columnHightlightColor := clBtnShadow;
  end;

  //paint background
  r := Self.ClientRect;
  r.Width := Max(FPaintBmp.Width, r.Width); //paintbmp may be wider than the client.
  LCanvas.Brush.Style := bsSolid;
  LCanvas.Brush.Color := backgroundColor;
  LCanvas.FillRect(r);

  //paint header
  LCanvas.Font.Color := HeaderTextColor;
  LCanvas.Brush.Color := HeaderBackgroundColor;
  LCanvas.Pen.Color := HeaderBorderColor;

  r.Bottom := r.Top + FRowHeight;
  LCanvas.Rectangle(r);

  r := FColumns[0].GetBounds;
  InflateRect(r,-1,-1);

  LCanvas.Brush.Style := bsClear;
  LCanvas.Font.Assign(Canvas.Font);
  LCanvas.Font.Color := HeaderTextColor;

  for i := 0 to 4 do
  begin
    if FHitElement = hightLightElements[i] then
    begin
      LCanvas.Brush.Style := bsSolid;
      LCanvas.Brush.Color := columnHightlightColor;
      LCanvas.FillRect(FColumns[i].GetBounds);
    end;

    LCanvas.Brush.Style := bsClear;
    LCanvas.MoveTo(FColumns[i].GetBounds.Right, 0);
    LCanvas.LineTo(FColumns[i].GetBounds.Right,FColumns[i].Height);
    if FColumns[i].Title <> '' then
    begin
      r := FColumns[i].GetBounds;
      r.Inflate(-5, 0);
      LCanvas.TextRect(r, FColumns[i].Title, [tfLeft, tfSingleLine, tfVerticalCenter]);
    end;
  end;


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

  Canvas.CopyRect(ClientRect, FPaintBmp.Canvas, r);

end;

procedure TVersionGrid.Resize;
var
  NewWidth, NewHeight: integer;
begin
  if (not HandleAllocated) then
    Exit;

  if csDestroying in ComponentState then
    Exit;

  NewWidth := Max(ClientWidth, GetTotalColumnWidth);
  NewHeight := ClientHeight;

  if (NewWidth <> FPaintBmp.Width) or (NewHeight <> FPaintBmp.Height) then
  begin
    // TBitmap does some stuff to try and preserve contents
    // which slows things down a lot - this avoids that
    FPaintBmp.SetSize(0, 0);
    FPaintBmp.SetSize(NewWidth, NewHeight);
  end;

  UpdateColumns;
  UpdateVisibleRows;
  UpdateScrollBars;

  if (RowCount > 0) and (FCurrentRow > -1) then
    if not RowInView(FCurrentRow) then
      ScrollInView(FCurrentRow);

  //Repaint;
  //RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);

  //force repainting scrollbars
  if sfHandleMessages in FIDEStyleServices.Flags then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  inherited;
end;

procedure TVersionGrid.RowsChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (FUpdateCount = 0) and HandleAllocated then
    begin
      UpdateVisibleRows;
      Invalidate;
    end;
  end;
end;

procedure TVersionGrid.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TVersionGrid.ScrollInView(const index: integer);
begin
  if (RowCount = 0) or (index > RowCount -1) then
    exit;

  //Figure out what the top row should be to make the current row visible.
  //current row below bottom of vieww
  if index >= (FTopRow + FSelectableRows) then
    FTopRow := Max(0, index - FSelectableRows + 1)
  else //above
    FTopRow := Min(index, RowCount - FVisibleRows );

  FVScrollPos := FTopRow;
  Invalidate;
  UpdateScrollBars;
end;

procedure TVersionGrid.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TVersionGrid.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
end;

procedure TVersionGrid.SetPackageVersion(const Value: TPackageVersion);
begin
  FPackageVersion := Value;
  Invalidate;
end;


procedure TVersionGrid.SetProjectVersion(index: integer; const Value: TPackageVersion);
begin
  if (index >= 0) and (index < FRows.Count) then
    FRows[index].InstalledVersion := value;
end;

procedure TVersionGrid.SetRowHeight(const Value: integer);
begin
  if FRowHeight <> value then
  begin
    FRowHeight := Value;
    UpdateVisibleRows;
    Invalidate;
  end;
end;

procedure TVersionGrid.SetStyleServices(const Value: TCustomStyleServices);
begin
  FIDEStyleServices := Value;
  Invalidate;
end;



procedure TVersionGrid.UpdateColumns;
begin
  //remove button
  FColumns[4].Index := 3;
  FColumns[4].Title := '';
  FColumns[4].Width := 32;
  FColumns[4].Left  := Self.Width - FColumns[4].Width - 1;
  FColumns[4].Height := FRowHeight;

  //upgrade/downgrade
  FColumns[3].Index := 3;
  FColumns[3].Title := '';
  FColumns[3].Width := 32;
  FColumns[3].Left  := FColumns[4].Left - FColumns[3].Width - 1;
  FColumns[3].Height := FRowHeight;

  //install
  FColumns[2].Index := 2;
  FColumns[2].Title := '';
  FColumns[2].Width := 32;
  FColumns[2].Left  := FColumns[3].Left - FColumns[2].Width - 1;
  FColumns[2].Height := FRowHeight;

  FColumns[1].Index := 1;
  FColumns[1].Title := 'Installed';
  FColumns[1].Width := 120;
  FColumns[1].Left  := FColumns[2].Left - FColumns[1].Width -1 ;
  FColumns[1].Height := FRowHeight;

  FColumns[0].Index := 0;
  FColumns[0].Title := 'Project';
  FColumns[0].Left  := 0;
  FColumns[0].Width := FColumns[1].Left - 2;
  FColumns[0].Height := FRowHeight;
end;

procedure TVersionGrid.UpdateHoverRow(const X, Y: integer);
var
  row : Integer;
  oldHoverRow : integer;
  rowState : TVersionGridPaintRowState;
begin
  row := FTopRow + GetRowFromY(Y);
  if row <> FHoverRow then
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

procedure TVersionGrid.UpdateScrollBars;
var
  sbInfo : TScrollInfo;
begin
  if not HandleAllocated then
    exit;

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

  if FPaintBmp.Width <= ClientWidth then
  begin
    sbInfo.nMax := 0;
    sbInfo.nPage := 0;
    sbInfo.nPos := 0;
    SetScrollInfo(Handle, SB_HORZ, sbInfo, True);
  end
  else
  begin
    sbInfo.nMax := Max(FPaintBmp.Width, 0);
    sbInfo.nPage := ClientWidth;
    sbInfo.nPos := Min(FHScrollPos, FPaintBmp.Width -1 ) ;
    SetScrollInfo(Handle, SB_HORZ, sbInfo, True);
  end;


end;

procedure TVersionGrid.UpdateVisibleRows;
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

function TVersionGrid.RowInView(const row: integer): boolean;
begin
  result := (row >= FTopRow) and (row < (FTopRow + FSelectableRows));
end;


procedure TVersionGrid.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1; //we will paint the background
end;

procedure TVersionGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TVersionGrid.WMHScroll(var Message: TWMVScroll);
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
    GetScrollInfo(Self.Handle,SB_HORZ, info);


    case TScrollCode(scrollCode) of
      TScrollCode.scLineUp: FHScrollPos := Max(0, FHScrollPos -1) ;
      TScrollCode.scLineDown: FHScrollPos := Min(FPaintBmp.Width, FHScrollPos + 1) ;
      TScrollCode.scPageUp:
      begin
        FHScrollPos := Max(0, FHScrollPos - ClientWidth)
      end;
      TScrollCode.scPageDown:
      begin
        FHScrollPos := Min(FHScrollPos + ClientWidth, FPaintBmp.Width );

  //      ScrollPos :=  FVScrollPos + FSelectableRows;
  //      if ScrollPos > RowCount -1 then
  //        ScrollPos := RowCount - 1;
  //      DoPageDown(true, ScrollPos);
      end;
      TScrollCode.scPosition,
      TScrollCode.scTrack:
      begin
        FHScrollPos := info.nTrackPos;
  //      DoTrack(ScrollPos);
      end;
      TScrollCode.scTop: FHScrollPos := 0;
      TScrollCode.scBottom: FHScrollPos := FPaintBmp.Width;
  //    TScrollCode.scEndScroll: ;
    end;
    UpdateScrollBars;
    Invalidate;


    //handle h scroll
  end;
end;

procedure TVersionGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  //force repaint during resizing rather than just after.
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
  //Repaint;
end;

procedure TVersionGrid.WMVScroll(var Message: TWMVScroll);
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

{ TVersionGridColumn }

function TVersionGridColumn.GetBounds: TRect;
begin
  result := Rect(Left,0,Left + Width, Height);
end;

end.
