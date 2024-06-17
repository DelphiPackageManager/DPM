unit DPM.IDE.PackageDetailsPanel;

interface


uses
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  ToolsApi,
  WinApi.Windows,
  WinApi.Messages,
  DPM.Core.Package.Interfaces;

{$I ..\DPMIDE.inc}

type
  TPackageDetailsPanel = class;

  TDetailElement = (deNone, deLicense, deProjectUrl, deReportUrl, deRepositoryUrl, deTags);
  TDetailElements = set of TDetailElement;
  TDetailsLayout = record
    PaddingX : integer;
    PaddingY : integer;
    LineSpacing : integer;
    VersionPadding : integer;

    VersionLabelRect : TRect;
    VersionRect : TRect;

    DescriptionLabelRect : TRect;
    DescriptionRect : TRect;
    DescriptionHeight : integer;

    AuthorsLabelRect : TRect;
    AuthorsRect : TRect;

    LicenseLabelRect : TRect;
    LicenseRect : TRect;

    PublishDateLabelRect : TRect;
    PublishDateRect : TRect;

    ProjectUrlLabelRect : TRect;
    ProjectUrlRect : TRect;

    ReportUrlLabelRect : TRect;
    ReportUrlRect : TRect;

    RepositoryUrlLabelRect : TRect;
    RepositoryUrlRect : TRect;

    TagsLabelRect : TRect;
    TagsRect : TRect;
    Tags : string;

    DependLabelRect : TRect;
    DependRect : TRect;

    LayoutHeight : integer;

    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
    procedure Update(const ACanvas : TCanvas; const AControl : TPackageDetailsPanel; const package : IPackageSearchResultItem; const optionalElements : TDetailElements);
    constructor Create(dummy : integer);
  end;

  TUriClickEvent = procedure(Sender : TObject; const uri : string; const element : TDetailElement) of object;

  TPackageDetailsPanel = class(TCustomControl)
  private
    FLayout : TDetailsLayout;
    FOptionalElements : TDetailElements;
    FHitElement : TDetailElement;
    FUpdating : boolean;
    FOnUriClickEvent : TUriClickEvent;
    FPackage : IPackageSearchResultItem;
  protected
    procedure UpdateLayout;
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBkgnd(var Message : TWmEraseBkgnd); message WM_ERASEBKGND;
    function HitTest(const pt : TPoint) : TDetailElement;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X : Integer; Y : Integer); override;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MouseLeave;
    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

  public
    constructor Create(AOwner : TComponent); override;
    procedure SetDetails(const package : IPackageSearchResultItem);

  published
    property OnUriClick : TUriClickEvent read FOnUriClickEvent write FOnUriClickEvent;
    property Color;
    property Font;
    property ParentColor;
    property ParentBackground;
    property ParentFont;
    property ParentDoubleBuffered;
    property DoubleBuffered;
    {$IFDEF STYLEELEMENTS}
    property StyleElements;
    {$ENDIF}
  end;

implementation

uses
  System.SysUtils,
  System.UITypes,
  Vcl.Themes,
  Vcl.Forms,
  {$IF CompilerVersion > 33.0 }
   BrandingAPI,
  {$IFEND}
  Spring.Collections,
  VSoft.Uri,
  DPM.Core.Types,
  DPM.Core.Utils.Strings;

{ TPackageDetailsPanel }


procedure TPackageDetailsPanel.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
begin
  inherited;
  //for some reason this is not happening in D11.x
//  Canvas.Font.Height := MulDiv(Canvas.Font.Height, M, D );
  FLayout.ChangeScale(M, D{$IF CompilerVersion > 33}, isDpiChange{$IFEND} );
  UpdateLayout;
end;

procedure TPackageDetailsPanel.CMMouseLeave(var Msg : TMessage);
begin
  if FHitElement <> deNone then
  begin
    FHitElement := deNone;
    Cursor := crDefault;
    invalidate;
  end;
end;

constructor TPackageDetailsPanel.Create(AOwner : TComponent);
begin
  inherited;
  DoubleBuffered := true;
  ParentColor := false;
  ParentFont := true;
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont,seClient];
  {$ENDIF}
  {$IF CompilerVersion > 33.0 }
  if TIDEThemeMetrics.Font.Enabled then
  begin
    Font.Assign( TIDEThemeMetrics.Font.GetFont );
    TIDEThemeMetrics.Font.AdjustDPISize( Font, TIDEThemeMetrics.Font.Size, CurrentPPI );
  end;
  {$IFEND}
  FLayout := TDetailsLayout.Create(0);
end;

function TPackageDetailsPanel.HitTest(const pt : TPoint) : TDetailElement;
begin
  result := deNone;
  if FPackage = nil then
    exit;

  if (deLicense in FOptionalElements) then
  begin
    if FLayout.LicenseRect.Contains(pt) then
      exit(deLicense);
  end;

  if deProjectUrl in FOptionalElements then
  begin
    if FLayout.ProjectUrlRect.Contains(pt) then
      exit(deProjectUrl);
  end;

  if deReportUrl in FOptionalElements then
  begin
    if FLayout.ReportUrlRect.Contains(pt) then
      exit(deReportUrl);
  end;

  if deRepositoryUrl in FOptionalElements then
  begin
    if FLayout.RepositoryUrlRect.Contains(pt) then
      exit(deRepositoryUrl);
  end;


end;

procedure TPackageDetailsPanel.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  sUri : string;
  uri : IUri;
begin
  if not Assigned(FOnUriClickEvent) then
    exit;

  if FPackage = nil then
    exit;

  if Button = mbLeft then
  begin

    FHitElement := HitTest(Point(X, Y));
    if FHitElement <> deNone then
    begin
      case FHitElement of
        deLicense :
          begin
            //TODO : There can be multiple licenses, comma separated. This does not deal with that.
            if TUriFactory.TryParse(FPackage.License,false, uri) then
              sUri := FPackage.License
            else
              sUri := 'https://spdx.org/licenses/' + FPackage.License + '.html';
          end;
        deProjectUrl : sUri := FPackage.ProjectUrl;
        deReportUrl : sUri := FPackage.ProjectUrl;
      else
        exit;
      end;
      FOnUriClickEvent(self, sUri, FHitElement);
    end;
  end;
end;

procedure TPackageDetailsPanel.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  prevElement : TDetailElement;
begin
  inherited;
  prevElement := FHitElement;
  FHitElement := HitTest(Point(X, Y));
  if FHitElement <> prevElement then
    Invalidate;

  if FHitElement in [deProjectUrl, deReportUrl, deLicense] then
    Cursor := crHandPoint
  else
    Cursor := crDefault;

end;

procedure TPackageDetailsPanel.Paint;
var
  fontStyle : TFontStyles;
  fillColor : TColor;
  fontColor : TColor;
  uriColor : TColor;
  dependRect : TRect;
  packageDep : IPackageDependency;
  textSize : TSize;
  value : string;
begin
  Canvas.Brush.Style := bsSolid;

  fillColor := Self.Color;
  fontColor := Self.Font.Color;
  Canvas.Font.Color := fontColor;
  Canvas.Brush.Color := fillColor;
  Canvas.FillRect(ClientRect);


  uriColor := $00C57321;

  if FPackage = nil then
    exit;


  fontStyle := Canvas.Font.Style;

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Description', Length('Description'), FLayout.DescriptionLabelRect, DT_LEFT);
  Canvas.Font.Style := [];
  DrawText(Canvas.Handle, FPackage.Description, Length(FPackage.Description), FLayout.DescriptionRect, DT_LEFT + DT_WORDBREAK);

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Version :', Length('Version :'), FLayout.VersionLabelRect, DT_LEFT);

  Canvas.Font.Style := [];
  DrawText(Canvas.Handle, FPackage.Version.ToStringNoMeta, Length(FPackage.Version.ToStringNoMeta), FLayout.VersionRect, DT_LEFT + DT_WORDBREAK);

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Authors :', Length('Authors :'), FLayout.AuthorsLabelRect, DT_LEFT);

  Canvas.Font.Style := [];
  DrawText(Canvas.Handle, FPackage.Authors, Length(FPackage.Authors), FLayout.AuthorsRect, DT_LEFT + DT_WORDBREAK);

  if (deLicense in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'License :', Length('License :'), FLayout.LicenseLabelRect, DT_LEFT);

    Canvas.Font.Color := uriColor;

    Canvas.Font.Style := [];
    if FHitElement = deLicense then
      Canvas.Font.Style := [fsUnderline];

//    if FLicenseIsUri then
//      DrawText(Canvas.Handle, 'View License', Length('View License'), FLayout.LicenseRect, DT_LEFT)
//    else
      DrawText(Canvas.Handle, FPackage.License, Length(FPackage.License), FLayout.LicenseRect, DT_LEFT + DT_WORDBREAK);

  end;

  Canvas.Font.Color := fontColor;

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Date published :', Length('Date published :'), FLayout.PublishDateLabelRect, DT_LEFT);

  Canvas.Font.Style := [];
  if FHitElement = deLicense then
    Canvas.Font.Style := [fsUnderline];

  DrawText(Canvas.Handle, FPackage.PublishedDate, Length(FPackage.PublishedDate), FLayout.PublishDateRect, DT_LEFT + DT_WORDBREAK);


  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Project URL :', Length('Project URL :'), FLayout.ProjectUrlLabelRect, DT_LEFT);

  Canvas.Font.Color := uriColor;
  Canvas.Font.Style := [];
  if FHitElement = deProjectUrl then
    Canvas.Font.Style := [fsUnderline];

  DrawText(Canvas.Handle, FPackage.ProjectUrl, Length(FPackage.ProjectUrl), FLayout.ProjectUrlRect, DT_LEFT + DT_WORDBREAK);
  Canvas.Font.Color := fontColor;

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Report URL :', Length('Report URL :'), FLayout.ReportUrlLabelRect, DT_LEFT);

  Canvas.Font.Color := uriColor;
  Canvas.Font.Style := [];
  if FHitElement = deReportUrl then
    Canvas.Font.Style := [fsUnderline];
  DrawText(Canvas.Handle, FPackage.ReportUrl, Length(FPackage.ReportUrl), FLayout.ReportUrlRect, DT_LEFT + DT_WORDBREAK);
  Canvas.Font.Color := fontColor;


  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Repository URL :', Length('Repository URL :'), FLayout.RepositoryUrlLabelRect, DT_LEFT);

  Canvas.Font.Color := uriColor;
  Canvas.Font.Style := [];
  if FHitElement = deRepositoryUrl then
    Canvas.Font.Style := [fsUnderline];
  DrawText(Canvas.Handle, FPackage.RepositoryUrl, Length(FPackage.RepositoryUrl), FLayout.RepositoryUrlRect, DT_LEFT + DT_WORDBREAK);
  Canvas.Font.Color := fontColor;

  if (deTags in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'Tags :', Length('Tags :'), FLayout.TagsLabelRect, DT_LEFT);
    Canvas.Font.Style := [];
    DrawText(Canvas.Handle, FLayout.Tags, Length(FLayout.Tags), FLayout.TagsRect, DT_LEFT + DT_WORDBREAK);
  end;

  //draw dependencies
  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Dependencies', Length('Dependencies'), FLayout.DependLabelRect, DT_LEFT);
  Canvas.Font.Style := [];

  dependRect := FLayout.DependRect;
  if FPackage.Dependencies.Any then
  begin
    textSize := Canvas.TextExtent('Win32');

    dependRect.Left := FLayout.DependRect.Left + textSize.cy;
    dependRect.Bottom := dependRect.Top + textSize.cy;
    value := '- ' + DPMPlatformToString(FPackage.Platform);
    DrawText(Canvas.Handle, value, Length(value), dependRect, DT_LEFT);

    dependRect.Left := dependRect.Left + textSize.cy;
    dependRect.Offset(0, textSize.cy * 2);

    for packageDep in FPackage.Dependencies do
    begin
      value := '- ' + packageDep.Id + ' ( ' + packageDep.VersionRange.ToDisplayString + ' )';
      DrawText(Canvas.Handle, value, Length(value), dependRect, DT_LEFT);
      dependRect.Offset(0, textSize.cy * 2);
    end;

  end
  else
    DrawText(Canvas.Handle, 'No dependencies.', Length('No dependencies.'), dependRect, DT_LEFT + DT_WORDBREAK);


  Canvas.Font.Style := fontStyle;
end;

procedure TPackageDetailsPanel.Resize;
begin
  inherited;
  UpdateLayout;
end;

procedure TPackageDetailsPanel.SetDetails(const package : IPackageSearchResultItem);
begin
  FPackage := package;
  if FPackage <> nil then
  begin
    FOptionalElements := [];
    if package.License <> '' then
    begin
      Include(FOptionalElements, deLicense);
      //TODO : check for multiple licenses.
    end;

    if package.ProjectUrl <> '' then
      Include(FOptionalElements, deProjectUrl);

    if package.ReportUrl <> '' then
      Include(FOptionalElements, deReportUrl);

    if package.Tags <> '' then
      Include(FOptionalElements, deTags);
  end;

  UpdateLayout;
  Invalidate;
end;


procedure TPackageDetailsPanel.UpdateLayout;
begin
  //We need this because Resize calls updatelayout which can trigger resize
  if FUpdating then
    exit;
  FUpdating := true;
  try
    if HandleAllocated then
      FLayout.Update(Self.Canvas, Self, FPackage, FOptionalElements);
  finally
    FUpdating := False;
  end;
  if HandleAllocated then
    if FLayout.LayoutHeight <> ClientHeight then
    begin
      Self.ClientHeight := FLayout.LayoutHeight;
      Invalidate;
    end;

end;

procedure TPackageDetailsPanel.WMEraseBkgnd(var Message : TWmEraseBkgnd);
begin
  message.Result := 1;
end;

{ TDetailsLayout }


procedure TDetailsLayout.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
begin
  VersionPadding := MulDiv(VersionPadding, M, D);
  PaddingX := MulDiv(PaddingX, M, D);
  PaddingY := MulDiv(PaddingY, M, D);
  LineSpacing := MulDiv(LineSpacing, M, D);
  DescriptionHeight := MulDiv(DescriptionHeight, M, D);
end;

constructor TDetailsLayout.Create(dummy: integer);
begin
  VersionPadding := 6;
  PaddingX := 6;
  PaddingY := 4;
  LineSpacing := 6;
  DescriptionHeight := 5000;
end;

procedure TDetailsLayout.Update(const ACanvas : TCanvas; const AControl : TPackageDetailsPanel; const package : IPackageSearchResultItem; const optionalElements : TDetailElements);
var
  textSize : TSize;
  clientRect : TRect;
  bottom : integer;
  count : integer;

  tagsArr : TArray<string>;
  i : integer;

begin
  if package = nil then
    exit;
  clientRect := AControl.ClientRect;

  InflateRect(clientRect, -PaddingX, -PaddingY);
  //we are going to calc this anyway
  clientRect.Height := 1000;

  //widest label
  ACanvas.Font.Style := [];
  textSize := ACanvas.TextExtent('Date published :    ');
  ACanvas.Font.Style := [];

  DescriptionLabelRect := clientRect;
  DescriptionLabelRect.Height := textSize.cy;

  DescriptionRect := clientRect;
  DescriptionRect.Top := DescriptionLabelRect.Bottom + textSize.cy;
  DescriptionRect.Bottom := DescriptionRect.Top + DescriptionHeight;
  DrawText(ACanvas.Handle, package.Description, Length(package.Description), DescriptionRect, DT_LEFT + DT_CALCRECT + DT_WORDBREAK);

  VersionLabelRect.Top := DescriptionRect.Bottom + LineSpacing;
  VersionLabelRect.Left := ClientRect.Left;
  VersionLabelRect.Width := textSize.cx;
  VersionLabelRect.Height := textSize.cy;


  VersionRect.Top := VersionLabelRect.Top;
  VersionRect.Left := VersionLabelRect.Right + VersionPadding;
  VersionRect.Right := clientRect.Right;
  VersionRect.Height := textSize.cy;
  DrawText(ACanvas.Handle, package.Version.ToStringNoMeta, Length(package.Version.ToStringNoMeta), VersionRect, DT_LEFT + DT_CALCRECT + DT_WORDBREAK);

  AuthorsLabelRect.Top := VersionRect.Bottom + LineSpacing;
  AuthorsLabelRect.Left := clientRect.Left;
  AuthorsLabelRect.Width := textSize.cx;
  AuthorsLabelRect.Height := textSize.cy;

  AuthorsRect.Top := AuthorsLabelRect.Top;
  AuthorsRect.Left := VersionRect.Left;
  AuthorsRect.Right := clientRect.Right;
  AuthorsRect.Height := textSize.cy;
  DrawText(ACanvas.Handle, package.Authors, Length(package.Authors), AuthorsRect, DT_LEFT + DT_CALCRECT + DT_WORDBREAK);

  bottom := AuthorsRect.Bottom;

  if (deLicense in optionalElements) then
  begin
    LicenseLabelRect.Top := bottom + LineSpacing;
    LicenseLabelRect.Left := clientRect.Left;
    LicenseLabelRect.Width := textSize.cx;
    LicenseLabelRect.Height := textSize.cy;


    LicenseRect.Top := LicenseLabelRect.Top;
    LicenseRect.Left := VersionRect.Left;
    LicenseRect.Right := clientRect.Right;
    LicenseRect.Height := textSize.cy;

    DrawText(ACanvas.Handle, package.License, Length(package.License), LicenseRect, DT_LEFT + DT_CALCRECT + DT_WORDBREAK);

    bottom := LicenseRect.Bottom;
  end;


  PublishDateLabelRect.Top := bottom + LineSpacing;
  PublishDateLabelRect.Left := clientRect.Left;
  PublishDateLabelRect.Width := textSize.cx;
  PublishDateLabelRect.Height := textSize.cy;


  PublishDateRect.Top := PublishDateLabelRect.Top;
  PublishDateRect.Left := VersionRect.Left;
  PublishDateRect.Right := clientRect.Right;
  PublishDateRect.Height := textSize.cy;
  bottom := PublishDateRect.Bottom;

  if (deProjectUrl in optionalElements) then
  begin

    ProjectUrlLabelRect.Top := bottom + LineSpacing;
    ProjectUrlLabelRect.Left := clientRect.Left;
    ProjectUrlLabelRect.Width := textSize.cx;
    ProjectUrlLabelRect.Height := textSize.cy;


    ProjectUrlRect.Top := ProjectUrlLabelRect.Top;
    ProjectUrlRect.Left := VersionRect.Left;
    ProjectUrlRect.Right := clientRect.Right;
    ProjectUrlRect.Height := textSize.cy;
    //drawtext doesn't wrap text without breaks and the windows api does not have any such functionalty;
    //TODO : Write a function to calc and split into lines
    DrawText(ACanvas.Handle, package.ProjectUrl, Length(package.ProjectUrl), ProjectUrlRect, DT_LEFT + DT_CALCRECT);
    bottom := ProjectUrlRect.Bottom;
  end;

  if (deReportUrl in optionalElements) then
  begin

    ReportUrlLabelRect.Top := bottom + LineSpacing;
    ReportUrlLabelRect.Left := clientRect.Left;
    ReportUrlLabelRect.Width := textSize.cx;
    ReportUrlLabelRect.Height := textSize.cy;


    ReportUrlRect.Top := ReportUrlLabelRect.Top;
    ReportUrlRect.Left := VersionRect.Left;
    ReportUrlRect.Right := clientRect.Right;
    ReportUrlRect.Height := textSize.cy;
    //drawtext doesn't wrap text without breaks and the windows api does not have any such functionalty;
    //TODO : Write a function to calc and split into lines
    //TODO : We need reporturl on the searchresultitem
    DrawText(ACanvas.Handle, package.ReportUrl, Length(package.ReportUrl), ReportUrlRect, DT_LEFT + DT_CALCRECT);
    bottom := ReportUrlRect.Bottom;
  end;

  if (deRepositoryUrl in optionalElements) then
  begin

    RepositoryUrlLabelRect.Top := bottom + LineSpacing;
    RepositoryUrlLabelRect.Left := clientRect.Left;
    RepositoryUrlLabelRect.Width := textSize.cx;
    RepositoryUrlLabelRect.Height := textSize.cy;


    RepositoryUrlRect.Top := RepositoryUrlLabelRect.Top;
    RepositoryUrlRect.Left := VersionRect.Left;
    RepositoryUrlRect.Right := clientRect.Right;
    ReportUrlRect.Height := textSize.cy;
    //RepositoryUrlRect doesn't wrap text without breaks and the windows api does not have any such functionalty;
    //TODO : Write a function to calc and split into lines
    //TODO : We need reporturl on the searchresultitem
    DrawText(ACanvas.Handle, package.RepositoryUrl, Length(package.RepositoryUrl), RepositoryUrlRect, DT_LEFT + DT_CALCRECT);
    bottom := RepositoryUrlRect.Bottom;
  end;


  if (deTags in optionalElements) then
  begin
    TagsLabelRect.Top := bottom + LineSpacing;
    TagsLabelRect.Left := clientRect.Left;
    TagsLabelRect.Width := textSize.cx;
    TagsLabelRect.Height := textSize.cy;


    TagsRect.Top := TagsLabelRect.Top;
    TagsRect.Left := VersionRect.Left;
    TagsRect.Right := clientRect.Right;
    TagsRect.Height := textSize.cy;

    //drawtext doesn't wrap text without breaks and the windows api does not have any such functionalty;
    //TODO : Write a function to calc and split into lines

    Tags := '';
    tagsArr := TStringUtils.SplitStr(package.Tags, ' ', TSplitStringOptions.ExcludeEmpty);
    for i := 0 to Length(tagsArr) -1 do
    begin
      if Tags <> '' then
        Tags := Tags + ', ' ;
      Tags := Tags + tagsArr[i];
    end;
    DrawText(ACanvas.Handle, Tags, Length(Tags), TagsRect, DT_LEFT + DT_CALCRECT);
    bottom := TagsRect.Bottom;
  end;


  DependLabelRect.Top := bottom + LineSpacing;
  DependLabelRect.Left := clientRect.Left;
  DependLabelRect.Width := textSize.cx;
  DependLabelRect.Height := textSize.cy;

  if package.Dependencies.Any then
  begin
    DependRect.Top := DependLabelRect.Bottom + LineSpacing;
    DependRect.Left := DependLabelRect.Left;
    DependRect.Right := clientRect.Right;
    count := 0;
    //work out the height;
    Inc(count, PaddingY div 2);
    Inc(count, package.Dependencies.Count * 2);
    DependRect.Height := count * textSize.cy;
  end
  else
  begin
    //single line, no dependencies
    DependRect.Top := DependLabelRect.Bottom + LineSpacing;
    DependRect.Left := DependLabelRect.Left;
    DependRect.Right := clientRect.Right;
    DependRect.Height := textSize.cy;
  end;

  bottom := DependRect.Bottom;


  //Change this as we add more fields
  LayoutHeight := bottom - clientRect.Top + textSize.cy * 2;
end;

end.


