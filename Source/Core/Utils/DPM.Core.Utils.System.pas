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

unit DPM.Core.Utils.System;

interface

type
  TSystemUtils = class
  public
    class function ExpandEnvironmentStrings(const value : string) : string;
    class function GetEnvironmentVariableDef(const name : string; const default : string = '') : string;
    class function GetVersionString: string;
    class procedure GetResourceVersionNumbers(out AMajor, AMinor, ARelease, ABuild: Integer);
  end;

implementation

uses
  System.SysUtils,
  WinApi.Windows;

{ TSystemUtils }

class function TSystemUtils.ExpandEnvironmentStrings(const value : string) : string;
var
  bufferLen : Integer;
begin
  bufferLen := Winapi.Windows.ExpandEnvironmentStrings(PChar(value), nil, 0);
  SetLength(result, bufferLen);
  Winapi.Windows.ExpandEnvironmentStrings(PChar(value), PChar(result), bufferLen);
  result := TrimRight(result); //trim the extr a null char.
end;

class function TSystemUtils.GetEnvironmentVariableDef(const name, default : string) : string;
begin
  result := GetEnvironmentVariable(name);
  if result = '' then
    result := ExpandEnvironmentStrings(default);
end;

class procedure TSystemUtils.GetResourceVersionNumbers(out AMajor, AMinor, ARelease, ABuild: Integer);
var
  HResource: TResourceHandle;
  HResData: THandle;
  PRes: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
begin
  AMajor := 0;
  AMinor := 0;
  ARelease := 0;
  ABuild := 0;
  HResource := FindResource(HInstance, MakeIntResource(VS_VERSION_INFO), RT_VERSION);
  if HResource <> 0 then
  begin
    HResData:=LoadResource(HInstance, HResource);
    if HResData <> 0 then
    begin
      PRes:=LockResource(HResData);
      if Assigned(PRes) then
      begin
        InfoSize := SizeofResource(HInstance, HResource);
        if InfoSize = 0 then
          exit; //we do not want to raise errors
        if VerQueryValue(PRes, '\', Pointer(FileInfo), FileInfoSize) then
        begin
          AMajor := FileInfo.dwFileVersionMS shr 16;
          AMinor := FileInfo.dwFileVersionMS and $FFFF;
          ARelease := FileInfo.dwFileVersionLS shr 16;
          ABuild := FileInfo.dwFileVersionLS and $FFFF;
        end;
      end;
    end;
  end;
end;

class function TSystemUtils.GetVersionString: string;
var
  Major, Minor, Release, Build: Integer;
begin
  GetResourceVersionNumbers(Major,Minor,Release,Build);
  result := Format('%d.%d.%d.%d',[Major,Minor,Release,Build]);
end;

end.

