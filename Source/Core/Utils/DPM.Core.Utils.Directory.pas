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

unit DPM.Core.Utils.Directory;

interface

uses
  System.IOUtils,
  Spring.Collections;

type
  TDirectoryUtils = record
    class function IsDirectoryWriteable(const directory : string) : boolean; static;
    //this is about 30% faster than System.IO.Utils version
    class function GetFiles(const Path : string) : IList<string>; overload; inline; static;
    class function GetFiles(const Path, SearchPattern : string) : IList<string>; overload; inline; static;
    class function GetFiles(const Path, SearchPattern : string; const SearchOption : TSearchOption) : IList<string>; overload; static;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils;

{ TDirectoryUtils }



function RandomString(const ALength : Integer) : string;
var
  i : Integer;
  LCharType : Integer;
begin
  Result := '';
  for i := 1 to ALength do
  begin
    LCharType := Random(3);
    case LCharType of
      0 : Result := Result + Chr(ord('a') + Random(26));
      1 : Result := Result + Chr(ord('A') + Random(26));
      2 : Result := Result + Chr(ord('0') + Random(10));
    end;
  end;
end;

class function TDirectoryUtils.IsDirectoryWriteable(const directory : string) : boolean;
var
  fileName : string;
  H : THandle;
begin
  fileName := IncludeTrailingPathDelimiter(directory) + RandomString(5) + '.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(H);
end;

class function TDirectoryUtils.GetFiles(const Path : string) : IList<string>;
begin
  result := GetFiles(ExtractFilePath(Path), ExtractFileName(Path), TSearchOption.soTopDirectoryOnly);
end;

class function TDirectoryUtils.GetFiles(const Path, SearchPattern : string) : IList<string>;
begin
  result := GetFiles(Path, SearchPattern, TSearchOption.soTopDirectoryOnly);
end;

class function TDirectoryUtils.GetFiles(const Path, SearchPattern : string; const SearchOption : TSearchOption) : IList<string>;
var
  foundFiles : IList<string>;

  procedure DoSearch(const dir : string);
  var
    searchRec : TSearchRec;
    theDir : string;
  begin
    theDir := IncludeTrailingPathDelimiter(dir);
    if FindFirst(theDir + SearchPattern, faAnyFile, searchRec) = 0 then
    begin
      try
        repeat
          if (searchRec.Attr and faDirectory) = 0 then
            foundFiles.Add(theDir + searchRec.Name)
          else if SearchOption = TSearchOption.soAllDirectories then
          begin
            if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
              DoSearch(theDir + searchRec.Name);
          end;
        until (FindNext(searchRec) <> 0);
      finally
        FindClose(searchRec);
      end;
    end;
  end;

begin
  foundFiles := TCollections.CreateList < string > ;
  result := foundFiles;
  DoSearch(Path);
end;


end.

