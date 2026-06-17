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

  TDetailElement = (deNone, deLicense, deProjectUrl, deReportUrl, deRepositoryUrl, deRepositoryCommit, deTags);
  TDetailElements = set of TDetailElement;

  //One rendered license link on the license line.
  TLicenseLink = record
    Separator : string;   // plain text drawn before this link ('', ', ', ' OR ', ' AND ')
    SepRect : TRect;      // where the separator text is drawn
    Text : string;        // license display text, e.g. 'MIT'
    Uri : string;         // resolved target URL
    Rect : TRect;         // clickable / underline rect for the license text
  end;

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
    LicenseLinks : TArray<TLicenseLink>;
    LicenseHasEllipsis : boolean;
    LicenseEllipsisRect : TRect;

    PublishDateLabelRect : TRect;
    PublishDateRect : TRect;

    ProjectUrlLabelRect : TRect;
    ProjectUrlRect : TRect;

    ReportUrlLabelRect : TRect;
    ReportUrlRect : TRect;

    RepositoryUrlLabelRect : TRect;
    RepositoryUrlRect : TRect;

    RepositoryCommitLabelRect : TRect;
    RepositoryCommitRect : TRect;

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
    FHitLicenseIndex : integer;
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
  System.StrUtils,
  System.UITypes,
  Vcl.Themes,
  Vcl.Forms,
  {$IF CompilerVersion > 34.0 }
   BrandingAPI,
  {$IFEND}
  Spring.Collections,
  VSoft.Uri,
  DPM.Core.Utils.Spdx,
  DPM.Core.Utils.Strings;

const
  //horizontal ellipsis, written as a code point so the source stays pure ASCII (file has no BOM)
  cLicenseEllipsis : string = #$2026;

type
  TLicensePart = record
    Separator : string;   // separator text that preceded this license ('', ', ', ' OR ', ' AND ')
    License : string;     // the trimmed license id or url
  end;

//Resolve a single license id/url to a target url. Returns '' when the license is
//neither a real url nor a known SPDX id - the caller then renders it as plain,
//non-clickable text (e.g. a non-SPDX value like 'MPL 1.1' that would otherwise
//produce a broken, space-containing spdx.org url).
function LicenseToUri(const license : string) : string;
var
  uri : IUri;
begin
  if TUriFactory.TryParse(license, false, uri) then
    result := license
  else
    result := TSpdxLicenses.GetLicenseUrl(license);
end;

//True when the 'keyword' (e.g. OR / AND) appears at position index as a whole word,
//delimited by whitespace on both sides (so it won't match inside FOR, WITH, license names).
function KeywordAt(const value : string; const index : integer; const keyword : string) : boolean;
var
  len : integer;
begin
  result := false;
  len := Length(keyword);
  if index <= 1 then
    exit;
  if not CharInSet(value[index - 1], [' ', #9]) then
    exit;
  if (index + len) > Length(value) then
    exit;
  if not CharInSet(value[index + len], [' ', #9]) then
    exit;
  result := SameText(Copy(value, index, len), keyword);
end;

//Split a license expression into individual licenses, capturing the separator that
//preceded each. Splits on commas and the whole-word SPDX operators OR / AND.
function SplitLicenses(const value : string) : TArray<TLicensePart>;
var
  parts : TArray<TLicensePart>;
  count : integer;
  i : integer;
  n : integer;
  tokenStart : integer;
  pendingSep : string;
  sepStr : string;
  sepLen : integer;
  matched : boolean;
  token : string;

  procedure AddPart(const aSeparator, aLicense : string);
  var
    trimmed : string;
  begin
    trimmed := Trim(aLicense);
    if trimmed = '' then
      exit;
    if count >= Length(parts) then
      SetLength(parts, count + 4);
    parts[count].Separator := aSeparator;
    parts[count].License := trimmed;
    Inc(count);
  end;

begin
  count := 0;
  SetLength(parts, 0);
  n := Length(value);
  i := 1;
  tokenStart := 1;
  pendingSep := '';
  while i <= n do
  begin
    matched := false;
    sepStr := '';
    sepLen := 0;
    if value[i] = ',' then
    begin
      matched := true;
      sepStr := ', ';
      sepLen := 1;
    end
    else if KeywordAt(value, i, 'OR') then
    begin
      matched := true;
      sepStr := ' OR ';
      sepLen := 2;
    end
    else if KeywordAt(value, i, 'AND') then
    begin
      matched := true;
      sepStr := ' AND ';
      sepLen := 3;
    end;

    if matched then
    begin
      token := Copy(value, tokenStart, i - tokenStart);
      AddPart(pendingSep, token);
      pendingSep := sepStr;
      i := i + sepLen;
      tokenStart := i;
    end
    else
      Inc(i);
  end;
  token := Copy(value, tokenStart, n - tokenStart + 1);
  AddPart(pendingSep, token);

  SetLength(parts, count);
  result := parts;
end;

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
  {$IF CompilerVersion > 34.0 }
  if TIDEThemeMetrics.Font.Enabled then
  begin
    Font.Assign( TIDEThemeMetrics.Font.GetFont );
    TIDEThemeMetrics.Font.AdjustDPISize( Font, TIDEThemeMetrics.Font.Size, CurrentPPI );
  end;
  {$IFEND}
  FHitLicenseIndex := -1;
  FLayout := TDetailsLayout.Create(0);
end;

function TPackageDetailsPanel.HitTest(const pt : TPoint) : TDetailElement;
var
  i : integer;
begin
  result := deNone;
  FHitLicenseIndex := -1;
  if FPackage = nil then
    exit;

  if (deLicense in FOptionalElements) then
  begin
    for i := 0 to High(FLayout.LicenseLinks) do
    begin
      //only a license with a resolved uri is clickable; unknown licenses render
      //as plain text and must not hit-test as a link.
      if (FLayout.LicenseLinks[i].Uri <> '') and FLayout.LicenseLinks[i].Rect.Contains(pt) then
      begin
        FHitLicenseIndex := i;
        exit(deLicense);
      end;
    end;
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

  if deRepositoryCommit in FOptionalElements then
  begin
    if FLayout.RepositoryCommitRect.Contains(pt) then
      exit(deRepositoryCommit);
  end;

end;

procedure TPackageDetailsPanel.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  sUri : string;
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
            if (FHitLicenseIndex >= 0) and (FHitLicenseIndex <= High(FLayout.LicenseLinks)) then
              sUri := FLayout.LicenseLinks[FHitLicenseIndex].Uri
            else
              exit;
          end;
        deProjectUrl : sUri := FPackage.ProjectUrl;
        deReportUrl : sUri := FPackage.ProjectUrl;
        deRepositoryCommit :
        begin
          if StartsText('https://github.com', FPackage.RepositoryUrl)  then
          begin
            sUri := StringReplace(FPackage.RepositoryCommit, 'Id:','',[rfIgnoreCase]);
            sUri := FPackage.RepositoryUrl + '/commit/' + sUri;
          end;
        end
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
  prevLicenseIndex : integer;
begin
  inherited;
  prevElement := FHitElement;
  prevLicenseIndex := FHitLicenseIndex;
  FHitElement := HitTest(Point(X, Y));
  if (FHitElement <> prevElement) or (FHitLicenseIndex <> prevLicenseIndex) then
    Invalidate;

  if FHitElement in [deProjectUrl, deReportUrl, deRepositoryUrl, deRepositoryCommit, deLicense] then
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
  authorsDisplay : string;
  authorIdx : integer;
  licenseIdx : integer;
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

  if FPackage.IsError then
    Canvas.Font.Color := $006464FA;

  DrawText(Canvas.Handle, 'Description', Length('Description'), FLayout.DescriptionLabelRect, DT_LEFT);
  Canvas.Font.Style := [];
  DrawText(Canvas.Handle, FPackage.Description, Length(FPackage.Description), FLayout.DescriptionRect, DT_LEFT + DT_WORDBREAK);

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Version :', Length('Version :'), FLayout.VersionLabelRect, DT_LEFT);

  Canvas.Font.Style := [];
  DrawText(Canvas.Handle, FPackage.Version.ToStringNoMeta, Length(FPackage.Version.ToStringNoMeta), FLayout.VersionRect, DT_LEFT + DT_SINGLELINE);

  if FPackage.IsError then
  begin
    Canvas.Font.Style := fontStyle;
    exit;
  end;

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Authors :', Length('Authors :'), FLayout.AuthorsLabelRect, DT_LEFT);

  Canvas.Font.Style := [];
  authorsDisplay := '';
  if FPackage.Authors <> nil then
    for authorIdx := 0 to FPackage.Authors.Count - 1 do
    begin
      if authorsDisplay <> '' then
        authorsDisplay := authorsDisplay + ', ';
      authorsDisplay := authorsDisplay + FPackage.Authors[authorIdx];
    end;
  DrawText(Canvas.Handle, authorsDisplay, Length(authorsDisplay), FLayout.AuthorsRect, DT_LEFT + DT_SINGLELINE);

  if (deLicense in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'License :', Length('License :'), FLayout.LicenseLabelRect, DT_LEFT);

    for licenseIdx := 0 to High(FLayout.LicenseLinks) do
    begin
      //separator before the link is plain (non-link) text
      if FLayout.LicenseLinks[licenseIdx].Separator <> '' then
      begin
        Canvas.Font.Color := fontColor;
        Canvas.Font.Style := [];
        value := FLayout.LicenseLinks[licenseIdx].Separator;
        DrawText(Canvas.Handle, value, Length(value), FLayout.LicenseLinks[licenseIdx].SepRect, DT_LEFT + DT_SINGLELINE);
      end;

      Canvas.Font.Style := [];
      if FLayout.LicenseLinks[licenseIdx].Uri <> '' then
      begin
        //known SPDX id / real url - render as a clickable link
        Canvas.Font.Color := uriColor;
        if (FHitElement = deLicense) and (licenseIdx = FHitLicenseIndex) then
          Canvas.Font.Style := [fsUnderline];
      end
      else
        //unknown license - plain, non-clickable text
        Canvas.Font.Color := fontColor;
      value := FLayout.LicenseLinks[licenseIdx].Text;
      DrawText(Canvas.Handle, value, Length(value), FLayout.LicenseLinks[licenseIdx].Rect, DT_LEFT + DT_SINGLELINE);
    end;

    if FLayout.LicenseHasEllipsis then
    begin
      Canvas.Font.Color := fontColor;
      Canvas.Font.Style := [];
      DrawText(Canvas.Handle, cLicenseEllipsis, Length(cLicenseEllipsis), FLayout.LicenseEllipsisRect, DT_LEFT + DT_SINGLELINE);
    end;
  end;

  Canvas.Font.Color := fontColor;

  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, 'Date published :', Length('Date published :'), FLayout.PublishDateLabelRect, DT_LEFT);

  Canvas.Font.Style := [];

  DrawText(Canvas.Handle, FPackage.PublishedDate, Length(FPackage.PublishedDate), FLayout.PublishDateRect, DT_LEFT + DT_SINGLELINE);

  if (deProjectUrl in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'Project URL :', Length('Project URL :'), FLayout.ProjectUrlLabelRect, DT_LEFT);

    Canvas.Font.Color := uriColor;
    Canvas.Font.Style := [];
    if FHitElement = deProjectUrl then
      Canvas.Font.Style := [fsUnderline];

    DrawText(Canvas.Handle, FPackage.ProjectUrl, Length(FPackage.ProjectUrl), FLayout.ProjectUrlRect, DT_LEFT + DT_WORDBREAK);
    Canvas.Font.Color := fontColor;
  end;


  if (deReportUrl in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'Report URL :', Length('Report URL :'), FLayout.ReportUrlLabelRect, DT_LEFT);

    Canvas.Font.Color := uriColor;
    Canvas.Font.Style := [];
    if FHitElement = deReportUrl then
      Canvas.Font.Style := [fsUnderline];
    DrawText(Canvas.Handle, FPackage.ReportUrl, Length(FPackage.ReportUrl), FLayout.ReportUrlRect, DT_LEFT + DT_WORDBREAK);
    Canvas.Font.Color := fontColor;
  end;

  if (deRepositoryUrl in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'Repository URL :', Length('Repository URL :'), FLayout.RepositoryUrlLabelRect, DT_LEFT);

    Canvas.Font.Color := uriColor;
    Canvas.Font.Style := [];
    if FHitElement = deRepositoryUrl then
      Canvas.Font.Style := [fsUnderline];
    DrawText(Canvas.Handle, FPackage.RepositoryUrl, Length(FPackage.RepositoryUrl), FLayout.RepositoryUrlRect, DT_LEFT + DT_WORDBREAK);
    Canvas.Font.Color := fontColor;
  end;

  if (deRepositoryCommit in FOptionalElements) then
  begin
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, 'Commit :', Length('Commit :'), FLayout.RepositoryCommitLabelRect, DT_LEFT);

    Canvas.Font.Color := uriColor;
    Canvas.Font.Style := [];
    value := FPackage.RepositoryCommit;
    //treat github commits as url
    if StartsText('https://github.com/',FPackage.RepositoryUrl) then
    begin
      value := StringReplace(value, 'Id:','',[rfIgnoreCase]);
      if FHitElement = deRepositoryCommit then
        Canvas.Font.Style := [fsUnderline];
    end;
    DrawText(Canvas.Handle, value, Length(value), FLayout.RepositoryCommitRect, DT_LEFT + DT_WORDBREAK);
    Canvas.Font.Color := fontColor;
  end;


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
  if (FPackage.Dependencies <> nil) and FPackage.Dependencies.Any then
  begin
    textSize := Canvas.TextExtent('Win32');

    dependRect.Left := FLayout.DependRect.Left + textSize.cy;
    dependRect.Bottom := dependRect.Top + textSize.cy;

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
  FHitLicenseIndex := -1;
  if FPackage <> nil then
  begin
    FOptionalElements := [];
    if package.License <> '' then
      Include(FOptionalElements, deLicense);

    if package.ProjectUrl <> '' then
      Include(FOptionalElements, deProjectUrl);

    if package.ReportUrl <> '' then
      Include(FOptionalElements, deReportUrl);

    if (package.Tags <> nil) and (package.Tags.Count > 0) then
      Include(FOptionalElements, deTags);

    if package.RepositoryUrl <> '' then
      Include(FOptionalElements, deRepositoryUrl);

    if package.RepositoryCommit <> '' then
      Include(FOptionalElements, deRepositoryCommit);

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
  LineSpacing := 8;
  DescriptionHeight := 5000;
end;

procedure TDetailsLayout.Update(const ACanvas : TCanvas; const AControl : TPackageDetailsPanel; const package : IPackageSearchResultItem; const optionalElements : TDetailElements);
var
  textSize : TSize;
  clientRect : TRect;
  bottom : integer;
  count : integer;
  i : integer;
  licenseParts : TArray<TLicensePart>;
  licenseTop : integer;
  licenseRight : integer;
  ellipsisWidth : integer;
  sepWidth : integer;
  licWidth : integer;
  x : integer;
  placed : integer;

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
  DescriptionRect.Top := DescriptionLabelRect.Bottom + PaddingY;
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

  AuthorsLabelRect.Top := VersionRect.Bottom + LineSpacing;
  AuthorsLabelRect.Left := clientRect.Left;
  AuthorsLabelRect.Width := textSize.cx;
  AuthorsLabelRect.Height := textSize.cy;

  AuthorsRect.Top := AuthorsLabelRect.Top;
  AuthorsRect.Left := VersionRect.Left;
  AuthorsRect.Right := clientRect.Right;
  AuthorsRect.Height := textSize.cy;

  bottom := AuthorsRect.Bottom;

  if (deLicense in optionalElements) then
  begin
    LicenseLabelRect.Top := bottom + LineSpacing;
    LicenseLabelRect.Left := clientRect.Left;
    LicenseLabelRect.Width := textSize.cx;
    LicenseLabelRect.Height := textSize.cy;

    //Lay out each license as its own link on a single line, separated by the
    //original operator text. Stop with an ellipsis once a link won't fit.
    licenseParts := SplitLicenses(package.License);
    SetLength(LicenseLinks, Length(licenseParts));
    LicenseHasEllipsis := false;
    licenseTop := LicenseLabelRect.Top;
    licenseRight := clientRect.Right;
    ellipsisWidth := ACanvas.TextExtent(cLicenseEllipsis).cx;
    x := VersionRect.Left;
    placed := 0;
    for i := 0 to High(licenseParts) do
    begin
      if licenseParts[i].Separator <> '' then
        sepWidth := ACanvas.TextExtent(licenseParts[i].Separator).cx
      else
        sepWidth := 0;
      licWidth := ACanvas.TextExtent(licenseParts[i].License).cx;

      //once at least one link is placed, require the next to fit (leaving room for the ellipsis)
      if (placed > 0) and (x + sepWidth + licWidth > licenseRight) then
      begin
        LicenseHasEllipsis := true;
        Break;
      end;

      LicenseLinks[placed].Separator := licenseParts[i].Separator;
      if sepWidth > 0 then
      begin
        LicenseLinks[placed].SepRect := Rect(x, licenseTop, x + sepWidth, licenseTop + textSize.cy);
        x := x + sepWidth;
      end;
      LicenseLinks[placed].Text := licenseParts[i].License;
      LicenseLinks[placed].Uri := LicenseToUri(licenseParts[i].License);
      LicenseLinks[placed].Rect := Rect(x, licenseTop, x + licWidth, licenseTop + textSize.cy);
      x := x + licWidth;
      Inc(placed);
    end;
    SetLength(LicenseLinks, placed);

    if LicenseHasEllipsis then
      LicenseEllipsisRect := Rect(x, licenseTop, x + ellipsisWidth, licenseTop + textSize.cy);

    bottom := licenseTop + textSize.cy;
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

  if (deRepositoryCommit in optionalElements) then
  begin

    RepositoryCommitLabelRect.Top := bottom + LineSpacing;
    RepositoryCommitLabelRect.Left := clientRect.Left;
    RepositoryCommitLabelRect.Width := textSize.cx;
    RepositoryCommitLabelRect.Height := textSize.cy;

    RepositoryCommitRect.Top := RepositoryCommitLabelRect.Top;
    RepositoryCommitRect.Left := VersionRect.Left;
    RepositoryCommitRect.Right := clientRect.Right;
    RepositoryCommitRect.Height := textSize.cy;
    DrawText(ACanvas.Handle, package.RepositoryCommit, Length(package.RepositoryCommit), RepositoryCommitRect, DT_LEFT + DT_CALCRECT);
    bottom := RepositoryCommitRect.Bottom;
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
    for i := 0 to package.Tags.Count -1 do
    begin
      if Tags <> '' then
        Tags := Tags + ', ' ;
      Tags := Tags + package.Tags[i];
    end;
    DrawText(ACanvas.Handle, Tags, Length(Tags), TagsRect, DT_LEFT + DT_CALCRECT);
    bottom := TagsRect.Bottom;
  end;


  DependLabelRect.Top := bottom + LineSpacing;
  DependLabelRect.Left := clientRect.Left;
  DependLabelRect.Width := textSize.cx;
  DependLabelRect.Height := textSize.cy;

  if (package.Dependencies <> nil) and package.Dependencies.Any then
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
  LayoutHeight := bottom - clientRect.Top + PaddingY * 2;
end;

end.


