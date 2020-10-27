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

unit DPM.Core.Utils.Path;

interface

type
  TPathUtils = class
    //sysutils.IsRelativePath returns false with paths starting with .\ grrrrrr
    class function IsRelativePath(const value : string) : boolean;
    class function CompressRelativePath(const basePath : string; path : string) : string;
  end;


implementation

uses
  System.Types,
  Spring.Collections,
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Utils.Strings;

//Copied from XE7
function StartsWith(const current : string; const Value : string; IgnoreCase : Boolean = false) : Boolean;
begin
  if not IgnoreCase then
    Result := System.SysUtils.StrLComp(PChar(current), PChar(Value), Length(Value)) = 0
  else
    Result := System.SysUtils.StrLIComp(PChar(current), PChar(Value), Length(Value)) = 0;
end;

function EndsWith(const theString : string; const Value : string; IgnoreCase : Boolean = true) : Boolean;
begin
  if IgnoreCase then
    Result := EndsText(Value, theString)
  else
    result := EndsStr(Value, theString);
end;




function IndexOfAny(const value : string; const AnyOf : array of Char; StartIndex, Count : Integer) : Integer;
var
  I : Integer;
  C : Char;
  Max : Integer;
begin
  if (StartIndex + Count) >= Length(value) then
    Max := Length(value)
  else
    Max := StartIndex + Count;

  I := StartIndex;
  while I < Max do
  begin
    for C in AnyOf do
      if value[I] = C then
        Exit(I);
    Inc(I);
  end;
  Result := -1;
end;

function LastIndexOf(const theString : string; Value : Char; StartIndex, Count : Integer) : Integer;
var
  I : Integer;
  Min : Integer;
begin
  if StartIndex < Length(theString) then
    I := StartIndex
  else
    I := Length(theString);
  if (StartIndex - Count) < 0 then
    Min := 1
  else
    Min := StartIndex - Count;
  while I >= Min do
  begin
    if theString[I] = Value then
      Exit(I);
    Dec(I);
  end;
  Result := -1;
end;



type
  TAntStringSplitOptions = (None, ExcludeEmpty);

function AntSplit(const value : string; const Separator : array of Char; Count : Integer; Options : TAntStringSplitOptions) : TArray<string>;
const
  DeltaGrow = 32;
var
  NextSeparator, LastIndex : Integer;
  Total : Integer;
  CurrentLength : Integer;
  S : string;
begin
  Total := 0;
  LastIndex := 1;
  CurrentLength := 0;
  NextSeparator := IndexOfAny(value, Separator, LastIndex, Length(value));
  while (NextSeparator >= 0) and (Total < Count) do
  begin
    S := Copy(value, LastIndex, NextSeparator - LastIndex);
    if (S <> '') or ((S = '') and (Options <> ExcludeEmpty)) then
    begin
      Inc(Total);
      if CurrentLength < Total then
      begin
        CurrentLength := Total + DeltaGrow;
        SetLength(Result, CurrentLength);
      end;
      Result[Total - 1] := S;
    end;
    LastIndex := NextSeparator + 1;
    NextSeparator := IndexOfAny(value, Separator, LastIndex, Length(value));
  end;

  if (LastIndex < Length(value)) and (Total < Count) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := Copy(value, LastIndex, Length(value));
  end
  else
    SetLength(Result, Total);
end;


{ TPathUtils }

class function TPathUtils.CompressRelativePath(const basePath : string; path : string) : string;
var
  stack : IStack<string>;
  segments : TArray<string>;
  segment : string;
begin
  if not TPath.IsPathRooted(path) then
    path := IncludeTrailingPathDelimiter(basePath) + path
  else if not StartsWith(path, basePath) then
    exit(path); //should probably except ?

  segments := AntSplit(path, [PathDelim], MaxInt, None);
  stack := TCollections.CreateStack < string > ;
  for segment in segments do
  begin
    if segment = '..' then
    begin
      if stack.Count > 0 then
        stack.Pop //up one
      else
        raise Exception.Create('Relative path goes below base path');
    end
    else if segment <> '.' then
      stack.Push(segment);
  end;
  result := '';
  while stack.Count > 0 do
  begin
    if result <> '' then
      result := stack.Pop + PathDelim + result
    else
      result := stack.Pop;
  end;
  if EndsWith(path, PathDelim) then
    result := IncludeTrailingPathDelimiter(result);
end;

class function TPathUtils.IsRelativePath(const value : string) : boolean;
begin
  result := (not TPath.IsUNCPath(value) and TStringUtils.StartsWith(value, '.\')) or System.SysUtils.IsRelativePath(value);
end;

end.

