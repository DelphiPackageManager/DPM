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

unit DPM.IDE.IconCache;

interface

uses
  System.Types,
  Spring.Collections,
  SVGInterfaces,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  DPM.Core.Package.Interfaces,
  DPM.Core.Types;

type
  //This is a wrapper around either an svg or a png.
  IPackageIconImage = interface
  ['{E5617EA7-DEA6-41CF-BC40-64DF7705C6D6}']
    procedure PaintTo(const ACanvas : TCanvas; const bounds : TRect);
    function ToGraphic : TGraphic;
  end;

//In memory cache for package icons.
//TODO : back this with a disk cache, this will be more important to avoid http requests.

type
  TDPMIconCache = class
  private
    FIcons : IDictionary<string, IPackageIconImage>;
    FLock  : TObject;
  protected

  public
    constructor Create;
    destructor Destroy;override;
    function Query(const id : string) : boolean;
    function Request(const id : string ) : IPackageIconImage;
    procedure Cache(const id : string; const value : IPackageIconImage);
  end;

  TPackageIconImage = class(TInterfacedObject, IPackageIconImage)
  private
    FSVG : ISVG;
    FPng : TPngImage;
    FKind : TPackageIconKind;

  protected
    procedure PaintTo(const ACanvas : TCanvas; const bounds: TRect);
    function ToGraphic : TGraphic;
  public
    constructor Create(const icon : IPackageIcon);overload;
    constructor Create(const icon : ISVG);overload;
    constructor Create(const icon : TPngImage);overload;
    destructor Destroy;override;
  end;


implementation

uses
  System.Classes,
  System.SysUtils,
  SVGGraphic;

{ TDPMIconCache }

procedure TDPMIconCache.Cache(const id: string; const value: IPackageIconImage);
begin
  MonitorEnter(FLock);
  try
    FIcons[LowerCase(Id)] := value;
  finally
    MonitorExit(FLock);
  end;
end;

constructor TDPMIconCache.Create;
var
  ResStream: TResourceStream;
  missingImg : ISVG;
begin
  inherited;
  FIcons := TCollections.CreateDictionary<string, IPackageIconImage>;
  FLock  := TObject.Create;

  missingImg := GlobalSVGFactory.NewSvg;
  resStream := TResourceStream.Create(hInstance, 'DPM_ICON_MISSING_SVG', RT_RCDATA);
  try
    missingImg := GlobalSVGFactory.NewSvg;
    missingImg.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
  missingImg.Opacity := 0.333;
  FIcons['missing_icon'] := TPackageIconImage.Create(missingImg);
end;

destructor TDPMIconCache.Destroy;
begin
  MonitorEnter(FLock);
  try
    FIcons.Clear;
  finally
    MonitorExit(FLock);
  end;
  FLock.Free;
  inherited;
end;

function TDPMIconCache.Query(const id: string): boolean;
begin
  MonitorEnter(FLock);
  try
    result := FIcons.ContainsKey(LowerCase(id));
    if not result then
      FIcons[LowerCase(Id)] := nil;
  finally
    MonitorExit(FLock);
  end;
end;

function TDPMIconCache.Request(const id: string): IPackageIconImage;
begin
  result := nil;
  MonitorEnter(FLock);
  try
    if FIcons.ContainsKey(LowerCase(id)) then
      result := FIcons[LowerCase(Id)];
  finally
    MonitorExit(FLock);
  end;

end;

{ TPackageIconImage }

constructor TPackageIconImage.Create(const icon: IPackageIcon);
begin
  FKind := icon.Kind;
  FPng := nil;
  case icon.Kind of
    ikSvg:
    begin
      FSVG := GlobalSVGFactory.NewSvg;
      FSVG.LoadFromStream(icon.Stream);
    end;
    ikPng:
    begin
      FPng := TPngImage.Create;
      icon.stream.Position := 0;
      FPng.LoadFromStream(icon.stream);
    end;
  end;
end;

constructor TPackageIconImage.Create(const icon: ISVG);
begin
  FSVG := icon;
  FKind := TPackageIconKind.ikSvg;
end;

destructor TPackageIconImage.Destroy;
begin
  if FPng <> nil then
    FPng.Free;

  inherited;
end;

procedure TPackageIconImage.PaintTo(const ACanvas : TCanvas; const bounds: TRect);
begin
  case FKind of
    ikSvg: FSVG.PaintTo(ACanvas.Handle, TRectF.Create(bounds));
    ikPng: FPng.Draw(ACanvas, bounds);
  end;
end;

function TPackageIconImage.ToGraphic: TGraphic;
var
  svgGraphic: TSVGGraphic;
  clonedSVG : ISVG;
  clonedPng : TPngImage;
begin
  result := nil;
  case FKind of
    ikSvg:
    begin
      clonedSVG := GlobalSVGFactory.NewSvg;
      clonedSVG.Source := FSVG.Source;
      svgGraphic := TSVGGraphic.Create;
      svgGraphic.AssignSVG(clonedSVG);
      exit(svgGraphic);
    end;
    ikPng:
    begin
      clonedPng := TPngImage.Create;
      clonedPng.Assign(FPng);
      exit(clonedPng);
    end;
  end;
end;

constructor TPackageIconImage.Create(const icon: TPngImage);
begin
  FPng := TPngImage.Create;
  FPng.Assign(icon);
  FKind := TPackageIconKind.ikPng;
end;

end.
