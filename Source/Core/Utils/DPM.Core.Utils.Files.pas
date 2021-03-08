{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019-2021 Vincent Parrett and contributors          }
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

unit DPM.Core.Utils.Files;

interface

type
  TFileUtils = class
  public
    //TODO : Improve this!!
    class function AreSameFiles(const fileA, fileB : string ) : boolean;
  end;


implementation

uses
  WinApi.Windows,
  System.IOUtils,
  System.SysUtils;

{ TFileUtils }

class function TFileUtils.AreSameFiles(const fileA, fileB: string): boolean;
var
  fatA: TWin32FileAttributeData;
  fatB: TWin32FileAttributeData;
  verInfoSizeA : DWORD;
  verInfoSizeB : DWORD;
  dummy : DWORD;
  bufferA : array of byte;
  bufferB : array of byte;
  PVerInfoA: Pointer;
  PVerInfoB: Pointer;

  PVerValueA: PVSFixedFileInfo;
  PVerValueB: PVSFixedFileInfo;

begin
  result := false;
  if not GetFileAttributesEx(PChar(fileA), GetFileExInfoStandard, @fatA) then
    RaiseLastOSError;
  if not GetFileAttributesEx(PChar(fileB), GetFileExInfoStandard, @fatB) then
    RaiseLastOSError;

  //check the file size first, if not equal then we are done.

  if fatA.nFileSizeHigh <> fatB.nFileSizeHigh then
    exit;

  if fatA.nFileSizeLow <> fatB.nFileSizeLow then
    exit;

  //size is equal, but we are not sure, so test the version info

  verInfoSizeA := GetFileVersionInfoSize(PChar(fileA), Dummy);
  verInfoSizeB := GetFileVersionInfoSize(PChar(fileB), Dummy);
  //if the size of the version info is different then the files are not the same.
  if verInfoSizeA <> verInfoSizeB then
    exit;

  //we have version info.
  if (verInfoSizeA <> 0) then
  begin
    Setlength(bufferA,verInfoSizeA );
    PVerInfoA := @bufferA[0];
    if not GetFileVersionInfo(PChar(fileA), 0, verInfoSizeA, PVerInfoA) then
      exit;

    Setlength(bufferB,verInfoSizeB );
    PVerInfoB := @bufferB[0];
    if not GetFileVersionInfo(PChar(fileB), 0, verInfoSizeB, PVerInfoB) then
      exit;

    if not VerQueryValue(PVerInfoA, '\', Pointer(PVerValueA), dummy) then
      exit;

    if not VerQueryValue(PVerInfoB, '\', Pointer(PVerValueB), dummy) then
      exit;

    //major
    if HiWord(PVerValueA.dwFileVersionMS) <> HiWord(PVerValueA.dwFileVersionMS) then
      exit;

    //minor
    if LoWord(PVerValueA.dwFileVersionMS) <> LoWord(PVerValueA.dwFileVersionMS) then
      exit;

    //release
    if HiWord(PVerValueA.dwFileVersionLS) <> HiWord(PVerValueA.dwFileVersionLS) then
      exit;

    //build
    if LoWord(PVerValueA.dwFileVersionLS) <> LoWord(PVerValueA.dwFileVersionLS) then
      exit;

    exit(true);
  end;

  //if we get here, the file sizes match but there is no version info, so we cannot be sure.
  //ideally we would hash the files and compare hashes, but that might be slow (compared to copying files again).
  //so for now, we assume they are not equal.

  result := false;

end;

end.
