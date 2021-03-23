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

unit DPM.Core.Utils.Strings;

interface

type
  TSplitStringOptions = (None, ExcludeEmpty);
  //using a class to avoid namespace conflicts
  TStringUtils = class
    //mostly functions from TStringHelper that are useful.
    class function PadString(const s: string; const totalLength: integer; const padLeft: boolean = True; padChr: Char = ' '): string;
    class function PadRight(const theString : string; TotalWidth : Integer) : string; overload; inline;
    class function PadRight(const theString : string; TotalWidth : Integer; PaddingChar : Char) : string; overload; inline;
    class function SplitStr(const value : string; const Separator : Char; const options : TSplitStringOptions) : TArray<string>; overload;
    class function SplitStr(const value : string; const Separator : Char) : TArray<string>; overload;
    class function StartsWith(const theString : string; const value : string; const IgnoreCase : boolean = false) : boolean;
    class function Contains(const theString : string; const value : string; const IgnoreCase : boolean = false) : boolean;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TStringUtils }


//borrowed from stringhelper, works in more versions of delphi
class function TStringUtils.SplitStr(const value : string; const Separator : Char; const options : TSplitStringOptions) : TArray<string>;
const
  DeltaGrow = 4; //we are not likely to need a lot
var
  NextSeparator, LastIndex : Integer;
  Total : Integer;
  CurrentLength : Integer;
  s : string;
begin
  Total := 0;
  LastIndex := 1;
  CurrentLength := 0;
  NextSeparator := PosEx(Separator, value, LastIndex);
  while (NextSeparator > 0) do
  begin
    s := Copy(value, LastIndex, NextSeparator - LastIndex);
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
    NextSeparator := PosEx(Separator, value, LastIndex);
  end;

  if (LastIndex <= Length(value)) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := Copy(value, LastIndex, Length(value) - LastIndex + 1);
  end
  else
    SetLength(Result, Total);

end;


class function TStringUtils.PadRight(const theString : string; TotalWidth : Integer) : string;
begin
  Result := PadRight(theString, TotalWidth, ' ');
end;

class function TStringUtils.Contains(const theString, value : string; const IgnoreCase : boolean) : boolean;
begin
  if IgnoreCase then
    Result := Pos(LowerCase(value), LowerCase(theString)) > 0
  else
    Result := Pos(value, theString) > 0;

end;

class function TStringUtils.PadRight(const theString : string; TotalWidth : Integer; PaddingChar : Char) : string;
begin
  TotalWidth := TotalWidth - Length(theString);
  if TotalWidth > 0 then
    Result := theString + System.StringOfChar(PaddingChar, TotalWidth)
  else
    Result := theString;
end;

class function TStringUtils.PadString(const s: string; const totalLength: integer; const padLeft: boolean; padChr: Char): string;
begin
  Result := s;
  while Length(result) < totalLength do
  begin
    if padLeft then
      Result := padChr + Result
    else
      Result := Result + padChr;
  end;
end;

class function TStringUtils.SplitStr(const value : string; const Separator : Char) : TArray<string>;
begin
  result := SplitStr(value, Separator, TSplitStringOptions.ExcludeEmpty);
end;

//copied from XE7
function StrLComp(const Str1, Str2 : PWideChar; MaxLen : Cardinal) : Integer;
var
  I : Cardinal;
  P1, P2 : PWideChar;
begin
  P1 := Str1;
  P2 := Str2;
  I := 0;
  while I < MaxLen do
  begin
    if (P1^ <> P2^) or (P1^ = #0) then
      Exit(Ord(P1^) - Ord(P2^));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;
  Result := 0;
end;

{$WARN WIDECHAR_REDUCED OFF} //not an issue as the set is a..z

function StrLIComp(const Str1, Str2 : PWideChar; MaxLen : Cardinal) : Integer;
var
  P1, P2 : PWideChar;
  I : Cardinal;
  C1, C2 : WideChar;
begin
  P1 := Str1;
  P2 := Str2;
  I := 0;
  while I < MaxLen do
  begin
    if P1^ in ['a'..'z'] then
      C1 := WideChar(Word(P1^) xor $20)
    else
      C1 := P1^;

    if P2^ in ['a'..'z'] then
      C2 := WideChar(Word(P2^) xor $20)
    else
      C2 := P2^;

    if (C1 <> C2) or (C1 = #0) then
      Exit(Ord(C1) - Ord(C2));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;
  Result := 0;
end;
{$WARN WIDECHAR_REDUCED ON}

class function TStringUtils.StartsWith(const theString, value : string; const IgnoreCase : boolean) : boolean;
begin
  if not IgnoreCase then
    Result := StrLComp(PChar(theString), PChar(Value), Length(Value)) = 0
  else
    Result := StrLIComp(PChar(theString), PChar(Value), Length(Value)) = 0;
end;

end.

