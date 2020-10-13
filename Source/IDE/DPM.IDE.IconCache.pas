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
  Spring.Collections,
  SVGInterfaces,
  DPM.Core.Types;

//In memory cache for package icons.
//TODO : back this with a disk cache, this will be more important to avoid http requests.

type
  TDPMIconCache = class
  private
    FIcons : IDictionary<string, ISVG>;
    FLock  : TObject;
  protected

  public
    constructor Create;
    destructor Destroy;override;
    function Query(const id : string) : boolean;
    function Request(const id : string ) : ISVG;
    procedure Cache(const id : string; const value : ISVG);
  end;

implementation

uses
  System.Classes,
  System.Types,
  System.SysUtils;

{ TDPMIconCache }

procedure TDPMIconCache.Cache(const id: string; const value: ISVG);
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
  FIcons := TCollections.CreateDictionary<string, ISVG>;
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
  FIcons['missing_icon'] := missingImg;
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

function TDPMIconCache.Request(const id: string): ISVG;
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

end.
