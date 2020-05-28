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
  Vcl.Imaging.pngimage,
  Spring.Collections,
  DPM.Core.Types;

//In memory cache for package icons.
//TODO : back this with a disk cache, this will be more important to avoid http requests.

type
  TDPMIconCache = class
  private
    FIcons : IDictionary<string, TPngImage>;
    FLock  : TObject;
  protected

  public
    constructor Create;
    destructor Destroy;override;
    function Query(const id : string) : boolean;
    function Request(const id : string ) : TPngImage;
    procedure Cache(const id : string; const value : TPngImage);
  end;

implementation

uses
  System.SysUtils;

{ TDPMIconCache }

procedure TDPMIconCache.Cache(const id: string; const value: TPngImage);
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
  missingImg : TPngImage;
begin
  inherited;
  FIcons := TCollections.CreateDictionary<string, TPngImage>;
  FLock  := TObject.Create;

  missingImg := TPngImage.Create;
  missingImg.LoadFromResourceName(HInstance,'DPM_ICON_MISSING') ;
  FIcons['missing_icon'] := missingImg;
end;

destructor TDPMIconCache.Destroy;
var
  image : TPngImage;
begin
  MonitorEnter(FLock);
  try
    for image in FIcons.Values do
      image.Free;
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

function TDPMIconCache.Request(const id: string): TPngImage;
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
