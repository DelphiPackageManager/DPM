{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019-2021 Vincent Parrett and contributors          }
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

uses
  WinApi.Windows,
  System.SysUtils;

type
  TFileUtils = class
  public
    //TODO : Improve this!!
    class function AreSameFiles(const fileA, fileB : string ) : boolean;
    //True when the two version resources carry the same file version (major.minor.release.build).
    class function SameFileVersion(const versionA, versionB : TVSFixedFileInfo) : boolean;
    class function GetSize(const path : string) : Int64;

    //A unique temporary file name in the same folder as fileName - a rename from it onto fileName
    //stays on one volume, which is what makes TryReplaceFile atomic.
    class function TempFileNameFor(const fileName : string) : string;
    //Swaps sourceFile over targetFile in a single MoveFileEx call (no delete-then-rename gap, so a
    //reader sees either the old file or the new one, never a truncated one). Retries briefly on the
    //transient sharing/lock violations AV scanners and the search indexer cause on a just-written file.
    class function TryReplaceFile(const sourceFile : string; const targetFile : string; out lastError : Cardinal) : boolean;
    //Writes to a temporary file next to fileName and atomically swaps it in. On failure the original
    //file is left untouched, the temporary file is removed and an EInOutError is raised.
    class procedure AtomicWriteAllText(const fileName : string; const contents : string; const encoding : TEncoding);
    class procedure AtomicWriteAllBytes(const fileName : string; const bytes : TBytes);
  end;


implementation

uses
  System.IOUtils;

{ TFileUtils }

class function TFileUtils.SameFileVersion(const versionA, versionB : TVSFixedFileInfo) : boolean;
begin
  result := (versionA.dwFileVersionMS = versionB.dwFileVersionMS) and
            (versionA.dwFileVersionLS = versionB.dwFileVersionLS);
end;

class function TFileUtils.TempFileNameFor(const fileName : string) : string;
var
  guid : TGUID;
begin
  CreateGUID(guid);
  result := fileName + '.tmp-' + Copy(GUIDToString(guid), 2, 36);
end;

class function TFileUtils.TryReplaceFile(const sourceFile : string; const targetFile : string; out lastError : Cardinal) : boolean;
const
  cReplaceMaxAttempts = 10;
  cReplaceRetryDelayMs = 25;
var
  attempt : integer;
begin
  lastError := 0;
  attempt := 0;
  while not MoveFileEx(PChar(sourceFile), PChar(targetFile), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) do
  begin
    lastError := GetLastError;
    Inc(attempt);
    if (attempt >= cReplaceMaxAttempts) or
       ((lastError <> ERROR_SHARING_VIOLATION) and
        (lastError <> ERROR_ACCESS_DENIED) and
        (lastError <> ERROR_LOCK_VIOLATION)) then
      exit(false);
    Sleep(cReplaceRetryDelayMs);
  end;
  result := true;
end;

class procedure TFileUtils.AtomicWriteAllBytes(const fileName : string; const bytes : TBytes);
var
  tempFile : string;
  lastError : Cardinal;
begin
  tempFile := TempFileNameFor(fileName);
  try
    TFile.WriteAllBytes(tempFile, bytes);
    if not TryReplaceFile(tempFile, fileName, lastError) then
      raise EInOutError.CreateFmt('Unable to replace [%s] : %s', [fileName, SysErrorMessage(lastError)]);
  except
    if FileExists(tempFile) then
      System.SysUtils.DeleteFile(tempFile);
    raise;
  end;
end;

class procedure TFileUtils.AtomicWriteAllText(const fileName : string; const contents : string; const encoding : TEncoding);
var
  tempFile : string;
  lastError : Cardinal;
begin
  tempFile := TempFileNameFor(fileName);
  try
    TFile.WriteAllText(tempFile, contents, encoding);
    if not TryReplaceFile(tempFile, fileName, lastError) then
      raise EInOutError.CreateFmt('Unable to replace [%s] : %s', [fileName, SysErrorMessage(lastError)]);
  except
    if FileExists(tempFile) then
      System.SysUtils.DeleteFile(tempFile);
    raise;
  end;
end;

class function TFileUtils.GetSize(const path : string) : Int64;
var
  fad : TWin32FileAttributeData;
begin
  if not GetFileAttributesEx(PChar(path), GetFileExInfoStandard, @fad) then
    RaiseLastOSError;
  Int64Rec(result).Lo := fad.nFileSizeLow;
  Int64Rec(result).Hi := fad.nFileSizeHigh;
end;

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

    exit(SameFileVersion(PVerValueA^, PVerValueB^));
  end;

  //if we get here, the file sizes match but there is no version info, so we cannot be sure.
  //ideally we would hash the files and compare hashes, but that might be slow (compared to copying files again).
  //so for now, we assume they are not equal.

  result := false;

end;

end.
