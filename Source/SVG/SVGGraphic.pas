unit SVGGraphic;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  SVGInterfaces;

type
  TSVGGraphic = class(TGraphic)
  strict private
    FSVG: ISVG;
    FOpacity: Byte;
    FFileName: TFileName;

    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    procedure Clear;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure AssignSVG(SVG: ISVG);

    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;

    procedure SaveToStream(Stream: TStream); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;

    property Opacity: Byte read FOpacity write SetOpacity;
  published
    property FileName: TFileName read FFileName write SetFileName;
  end;


implementation


uses
  System.Types;

constructor TSVGGraphic.Create;
begin
  inherited;
  FSVG := GlobalSVGFactory.NewSvg;
  FOpacity := 255;
end;

procedure TSVGGraphic.Clear;
begin
  FSVG.Clear;
  FFileName := '';
  Changed(Self);
end;

procedure TSVGGraphic.Assign(Source: TPersistent);
begin
  if (Source is TSVGGraphic) then
  begin
    try
      //AssignSVG(TSVGGraphic(Source).FSVG);
      FSVG := TSVGGraphic(Source).FSVG;
    except
    end;
    Changed(Self);
  end;
end;

procedure TSVGGraphic.AssignSVG(SVG: ISVG);
begin
  FSVG := SVG;
  Changed(Self);
end;

procedure TSVGGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGGraphic then
    TSVGGraphic(Dest).Assign(Self);
end;

procedure TSVGGraphic.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Changed(Self);
end;

procedure TSVGGraphic.SetWidth(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TSVGGraphic.SetHeight(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.ReadData(Stream: TStream);
var
  Size: LongInt;
  MemStream: TMemoryStream;
begin
  Stream.Read(Size, SizeOf(Size));
  MemStream := TMemoryStream.Create;
  try
    MemStream.CopyFrom(Stream, Size);
    MemStream.Position := 0;
    FSVG.LoadFromStream(MemStream);
  finally
    MemStream.Free;
  end;
end;

procedure TSVGGraphic.WriteData(Stream: TStream);
var
  Size: LongInt;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    FSVG.SaveToStream(MemStream);
    Size := MemStream.Size;
    Stream.Write(Size, SizeOf(Size));
    MemStream.Position := 0;
    MemStream.SaveToStream(Stream);
  finally
    MemStream.Free;
  end;
end;

procedure TSVGGraphic.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if Empty then
    Exit;

  FSVG.Opacity := FOpacity / 255;
  FSVG.PaintTo(ACanvas.Handle, TRectF.Create(Rect));
end;


function TSVGGraphic.GetEmpty: Boolean;
begin
  Result := FSVG.IsEmpty;
end;

function TSVGGraphic.GetWidth: Integer;
begin
  Result := Round(FSVG.Width);
end;

function TSVGGraphic.GetHeight: Integer;
begin
  Result := Round(FSVG.Height);
end;

procedure TSVGGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.LoadFromFile(const Filename: String);
begin
  FSVG.LoadFromFile(Filename);
  Changed(Self);
end;

procedure TSVGGraphic.LoadFromStream(Stream: TStream);
begin
  try
    FSVG.LoadFromStream(Stream);
  except
  end;
  Changed(Self);
end;

procedure TSVGGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.SaveToStream(Stream: TStream);
begin
  FSVG.SaveToStream(Stream);
end;




initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TSVGGraphic);

end.
