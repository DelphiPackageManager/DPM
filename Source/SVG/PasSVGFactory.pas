{-----------------------------------------------------------------------------
 Unit Name: D2DSVGHandler
 Author:    PyScripter
 Purpose:   High-level encapsuation of Direct2D Svg functionality
 History:
-----------------------------------------------------------------------------}
unit PasSVGFactory;

interface
Uses
  Winapi.D2D1,
  SVGInterfaces;

// Factory Methods
function GetPasSVGFactory: ISVGFactory;

implementation

Uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  SvgTypes,
  SvgCommon,
  Svg;

type

  TPasSVG = class(TInterfacedObject, ISVG)
  private
    fSvgDoc: TSVG;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const Color: TColor);
    function GetSource: string;
    procedure SetSource(const ASource: string);
    // procedures and functions
    function IsEmpty: Boolean;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPasSVGFactory = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
  end;

{ TPasSVG }

procedure TPasSVG.Clear;
begin
  fSvgDoc.Clear;
end;

constructor TPasSVG.Create;
begin
  inherited;
  fSvgDoc := TSVG.Create;
end;

procedure TPasSVG.LoadFromFile(const FileName: string);
begin
  fSvgDoc.LoadFromFile(FileName);
end;

destructor TPasSVG.Destroy;
begin
  fSvgDoc.Free;
  inherited;
end;

function TPasSVG.GetFixedColor: TColor;
begin
  Result := fSvgDoc.FixedColor;
end;

function TPasSVG.GetGrayScale: Boolean;
begin
  Result := fSvgDoc.GrayScale;
end;

function TPasSVG.GetHeight: Single;
begin
  Result := fSvgDoc.Height;
end;

function TPasSVG.GetOpacity: Single;
// ReadOnly property
begin
  Result := 1;
end;

function TPasSVG.GetSource: string;
begin
  Result := fSvgDoc.Source;
end;

function TPasSVG.GetWidth: Single;
begin
  Result := fSvgDoc.Width;
end;

function TPasSVG.IsEmpty: Boolean;
begin
  Result := fSvgDoc.Count = 0;
end;

procedure TPasSVG.LoadFromStream(Stream: TStream);
begin
  fSvgDoc.LoadFromStream(Stream);
end;

function FitInto(const Source : TRectF; const ADesignatedArea: TRectF; out Ratio: Single): TRectF;overload;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    Ratio := 1;
    Exit(Source);
  end;

  if (Source.Width / ADesignatedArea.Width) > (Source.Height / ADesignatedArea.Height) then
    Ratio := Source.Width / ADesignatedArea.Width
  else
    Ratio := Source.Height / ADesignatedArea.Height;

  if Ratio = 0 then
    Exit(Source)
  else
  begin
    Result := TRectF.Create(0, 0, Source.Width / Ratio, Source.Height / Ratio);
    RectCenter(Result, ADesignatedArea);
  end;
end;


function FitInto(const Source : TRectF; const ADesignatedArea: TRectF): TRectF;overload;
var
  ratio : Single;
begin
  result := FitInto(Source, ADesignatedArea, ratio);
end;


procedure TPasSVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  SvgRect : TRectF;
begin
  SvgRect:= R;
  if (fSvgDoc.Width > 0) and (fSvgDoc.Height > 0) and KeepAspectRatio then
  begin
      SvgRect := TRectF.Create(0, 0, fSvgDoc.Width, fSvgDoc.Height);
      SvgRect := FitInto(SvgRect, R);
  end;
  fSvgDoc.PaintTo(DC, ToGPRectF(SvgRect), nil, 0);
end;

procedure TPasSVG.SaveToFile(const FileName: string);
begin
  fSvgDoc.SaveToFile(FileName);
end;

procedure TPasSVG.SaveToStream(Stream: TStream);
begin
  fSvgDoc.SaveToStream(Stream);
end;


procedure TPasSVG.SetFixedColor(const Color: TColor);
begin
  if Color < 0  then
    fSvgDoc.FixedColor := GetSysColor(Color and $000000FF)
  else
    fSvgDoc.FixedColor := Color;
end;


procedure TPasSVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  fSvgDoc.GrayScale := IsGrayScale;
end;

procedure TPasSVG.SetOpacity(const Opacity: Single);
begin
  fSvgDoc.SVGOpacity := Opacity;
end;

procedure TPasSVG.SetSource(const ASource: string);
begin
  fSvgDoc.LoadFromText(ASource);
end;

{ TPasSVGHandler }

function TPasSVGFactory.NewSvg: ISVG;
begin
  Result := TPasSVG.Create;
end;

// Factory methods
function GetPasSVGFactory: ISVGFactory;
begin
  Result := TPasSVGFactory.Create;
end;

end.
