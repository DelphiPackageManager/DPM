{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Utils.DateTime;

// ISO 8601 helpers. System.DateUtils gained DateToISO8601 / ISO8601ToDate in
// XE7 (CompilerVersion 28). DPM supports XE2 (CompilerVersion 23), so on
// older compilers we implement the same shape ourselves. Newer compilers
// just delegate. Callers always use TDPMDateTimeUtils so the rest of the
// codebase doesn't have to care which side of the gate they're on.

interface

uses
  System.SysUtils;

type
  TDPMDateTimeUtils = class
  public
    /// <summary>
    /// Format an absolute date/time as ISO 8601. When asUtc=true the value
    /// is treated as UTC and emitted as "yyyy-mm-ddThh:nn:ss.zzzZ".
    /// When false the value is treated as local time and the local
    /// timezone offset is appended.
    /// </summary>
    class function DateToISO8601(const value : TDateTime; asUtc : boolean = false) : string; static;

    /// <summary>
    /// Parse an ISO 8601 date/time string. Accepts both "T" and " " as the
    /// date/time separator; accepts optional fractional seconds (up to 9
    /// digits, truncated to ms); accepts "Z", "+HH:MM", "-HH:MM" or no zone
    /// suffix. When returnUtc=true the value is normalised to UTC before
    /// return; when false the value is converted to local time.
    /// </summary>
    class function ISO8601ToDate(const value : string; returnUtc : boolean = false) : TDateTime; static;

    /// <summary>
    /// Non-raising variant of ISO8601ToDate. Returns false (and leaves
    /// dateValue untouched) when the input cannot be parsed.
    /// </summary>
    class function TryISO8601ToDate(const value : string; out dateValue : TDateTime;
                                    returnUtc : boolean = false) : boolean; static;
  end;

implementation

uses
  System.DateUtils,
  System.Math,
  System.TimeSpan;

// Helper: read N digits at position pos, advancing pos. Raises EConvertError
// on non-digit input.
function ReadInt(const s : string; var pos : integer; digits : integer) : integer;
var
  i : integer;
  c : Char;
begin
  result := 0;
  for i := 1 to digits do
  begin
    if pos > Length(s) then
      raise EConvertError.CreateFmt('Unexpected end of ISO 8601 string at position %d', [pos]);
    c := s[pos];
    if (c < '0') or (c > '9') then
      raise EConvertError.CreateFmt('Expected digit at position %d, got "%s"', [pos, c]);
    result := result * 10 + Ord(c) - Ord('0');
    Inc(pos);
  end;
end;

procedure ExpectChar(const s : string; var pos : integer; expected : Char);
begin
  if (pos > Length(s)) or (s[pos] <> expected) then
    raise EConvertError.CreateFmt('Expected "%s" at position %d', [expected, pos]);
  Inc(pos);
end;

class function TDPMDateTimeUtils.DateToISO8601(const value : TDateTime; asUtc : boolean) : string;
{$IF CompilerVersion >= 28.0}
begin
  // Delegate to System.DateUtils on XE7+. The RTL function returns a string
  // that matches our format exactly, including the trailing "Z" for UTC.
  result := System.DateUtils.DateToISO8601(value, asUtc);
end;
{$ELSE}
var
  y, m, d, hh, nn, ss, ms : Word;
  offsetMinutes : integer;
  sign : Char;
  offHH, offMM : integer;
  ts : TTimeSpan;
begin
  DecodeDateTime(value, y, m, d, hh, nn, ss, ms);
  if asUtc then
    result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
      [y, m, d, hh, nn, ss, ms])
  else
  begin
    // Local-time form — append the local timezone offset from UTC.
    ts := TTimeZone.Local.GetUtcOffset(value);
    offsetMinutes := Round(ts.TotalMinutes);
    if offsetMinutes < 0 then
    begin
      sign := '-';
      offsetMinutes := -offsetMinutes;
    end
    else
      sign := '+';
    offHH := offsetMinutes div 60;
    offMM := offsetMinutes mod 60;
    result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3d%s%.2d:%.2d',
      [y, m, d, hh, nn, ss, ms, sign, offHH, offMM]);
  end;
end;
{$IFEND}

{$IF CompilerVersion >= 28.0}
// Canonicalise input for the RTL ISO8601 parser. The RTL implementation
// in Delphi 12 strictly requires uppercase 'T' as the date/time prefix and
// uppercase 'Z' as the UTC marker, and does not accept a space separator
// between date and time. Our polyfill contract is lenient on all three —
// when delegating to the RTL we canonicalise first so callers don't have
// to care which branch their input is going through.
function CanonicaliseISO8601(const value : string) : string;
var
  i : integer;
begin
  result := value;
  for i := 1 to Length(result) do
  begin
    case result[i] of
      't' : result[i] := 'T';
      'z' : result[i] := 'Z';
      ' ' : result[i] := 'T';   // space between date and time
    end;
  end;
end;
{$IFEND}

class function TDPMDateTimeUtils.ISO8601ToDate(const value : string; returnUtc : boolean) : TDateTime;
{$IF CompilerVersion >= 28.0}
var
  canonical : string;
begin
  if Trim(value) = '' then
    raise EConvertError.Create('ISO 8601 string is empty');
  canonical := CanonicaliseISO8601(value);
  try
    result := System.DateUtils.ISO8601ToDate(canonical, returnUtc);
  except
    // The RTL raises EDateTimeException for malformed input; normalise to
    // EConvertError so callers (and TryISO8601ToDate) only have to catch one.
    on e : Exception do
      raise EConvertError.CreateFmt('Invalid ISO 8601 date "%s": %s', [value, e.Message]);
  end;
end;
{$ELSE}
var
  pos : integer;
  y, m, d, hh, nn, ss, ms : integer;
  fracStart, fracLen, fracVal, fracTaken, fi : integer;
  c : Char;
  hasZone : boolean;
  zoneIsUtc : boolean;
  zoneSign : integer;
  zoneHH, zoneMM : integer;
  parsedLocal : TDateTime;
begin
  if Trim(value) = '' then
    raise EConvertError.Create('ISO 8601 string is empty');

  pos := 1;
  y := ReadInt(value, pos, 4);
  ExpectChar(value, pos, '-');
  m := ReadInt(value, pos, 2);
  ExpectChar(value, pos, '-');
  d := ReadInt(value, pos, 2);

  hh := 0; nn := 0; ss := 0; ms := 0;
  hasZone := false;
  zoneIsUtc := false;
  zoneSign := 0;
  zoneHH := 0;
  zoneMM := 0;

  if pos <= Length(value) then
  begin
    c := value[pos];
    if (c = 'T') or (c = 't') or (c = ' ') then
    begin
      Inc(pos);
      hh := ReadInt(value, pos, 2);
      ExpectChar(value, pos, ':');
      nn := ReadInt(value, pos, 2);
      ExpectChar(value, pos, ':');
      ss := ReadInt(value, pos, 2);

      // Optional fractional seconds — accept any digit count, truncate to ms.
      if (pos <= Length(value)) and ((value[pos] = '.') or (value[pos] = ',')) then
      begin
        Inc(pos);
        fracStart := pos;
        while (pos <= Length(value)) and (value[pos] >= '0') and (value[pos] <= '9') do
          Inc(pos);
        fracLen := pos - fracStart;
        if fracLen = 0 then
          raise EConvertError.CreateFmt('Empty fractional seconds at position %d', [pos]);
        fracTaken := Min(3, fracLen);
        fracVal := 0;
        for fi := 0 to fracTaken - 1 do
          fracVal := fracVal * 10 + Ord(value[fracStart + fi]) - Ord('0');
        // Right-pad with zeros if fewer than 3 digits were present.
        for fi := fracTaken to 2 do
          fracVal := fracVal * 10;
        ms := fracVal;
      end;

      // Optional timezone designator.
      if pos <= Length(value) then
      begin
        c := value[pos];
        if (c = 'Z') or (c = 'z') then
        begin
          hasZone := true;
          zoneIsUtc := true;
          Inc(pos);
        end
        else if (c = '+') or (c = '-') then
        begin
          hasZone := true;
          if c = '+' then zoneSign := 1 else zoneSign := -1;
          Inc(pos);
          zoneHH := ReadInt(value, pos, 2);
          if (pos <= Length(value)) and (value[pos] = ':') then
            Inc(pos);
          if pos <= Length(value) then
            zoneMM := ReadInt(value, pos, 2);
        end;
      end;
    end;
  end;

  parsedLocal := EncodeDateTime(y, m, d, hh, nn, ss, ms);

  // Normalise to UTC or local depending on what the caller asked for and what
  // the input carried.
  if hasZone then
  begin
    if zoneIsUtc then
    begin
      // parsedLocal is already UTC at this point.
      if returnUtc then
        result := parsedLocal
      else
        result := TTimeZone.Local.ToLocalTime(parsedLocal);
    end
    else
    begin
      // Subtract the offset to get UTC.
      parsedLocal := IncMinute(parsedLocal, -zoneSign * (zoneHH * 60 + zoneMM));
      if returnUtc then
        result := parsedLocal
      else
        result := TTimeZone.Local.ToLocalTime(parsedLocal);
    end;
  end
  else
  begin
    // No zone in the input: treat as the caller's preferred reference frame.
    if returnUtc then
      result := TTimeZone.Local.ToUniversalTime(parsedLocal)
    else
      result := parsedLocal;
  end;
end;
{$IFEND}

class function TDPMDateTimeUtils.TryISO8601ToDate(const value : string;
                                                  out dateValue : TDateTime;
                                                  returnUtc : boolean) : boolean;
begin
  try
    dateValue := ISO8601ToDate(value, returnUtc);
    result := true;
  except
    on Exception do
      result := false;
  end;
end;

end.
