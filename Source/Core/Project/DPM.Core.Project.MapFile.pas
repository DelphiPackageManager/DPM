{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Project.MapFile;

{ Parses Delphi linker MAP files (Windows targets). The format has been stable
  since D2007 / has shipped largely unchanged into 12.x. Two sections matter:

    "Detailed map of segments"
       0001:00400000 0000A000 C=CODE S=.text G=(none) M=System ACBP=A9
                                                       ^^^^^^^^   <- unit
       authoritative list of every unit linked into the binary.
       Win32 uses 8-hex addresses, Win64 uses 16-hex addresses.

    "Line numbers for UnitName(SourcePath) segment .text"
       gives a (UnitName, SourcePath) pair for every unit the linker had
       source-line debug info for. Release builds and units compiled from
       .dcu only may be missing.

  This reader returns the union: every unit name from "Detailed map of segments",
  with its SourcePath filled in if a "Line numbers for ..." section was found
  for it (otherwise empty). The downstream classifier uses SourcePath when
  present and falls back to matching the unit name against cached .dspec unit
  lists when it isn't.

  Linux ELF / Mach-O output use a different format - not supported here. }

interface

uses
  Spring.Collections,
  DPM.Core.Logging;

type
  TMapUnit = record
    UnitName : string;
    SourcePath : string;
  end;

  IMapFileInfo = interface
    ['{0E1A24C4-1AD0-4E55-83A1-AAA8DA3DBED5}']
    function GetFileName : string;
    function GetUnits : IReadOnlyList<TMapUnit>;
    function GetHasSourcePaths : boolean;
    property FileName : string read GetFileName;
    property Units : IReadOnlyList<TMapUnit> read GetUnits;
    //true when at least one unit has a non-empty SourcePath (i.e. the file had
    //"Line numbers for ..." sections). Lets the caller decide if path-based
    //classification is worth attempting.
    property HasSourcePaths : boolean read GetHasSourcePaths;
  end;

  IMapFileReader = interface
    ['{7BB3F6FC-8945-4F8F-A4A3-08F86F1ABA72}']
    function Read(const fileName : string) : IMapFileInfo;
  end;

  TMapFileReader = class(TInterfacedObject, IMapFileReader)
  private
    FLogger : ILogger;
  protected
    function Read(const fileName : string) : IMapFileInfo;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions;

type
  TMapFileInfo = class(TInterfacedObject, IMapFileInfo)
  private
    FFileName : string;
    FUnits : IList<TMapUnit>;
  protected
    function GetFileName : string;
    function GetUnits : IReadOnlyList<TMapUnit>;
    function GetHasSourcePaths : boolean;
  public
    constructor Create(const fileName : string; const units : IList<TMapUnit>);
  end;

{ TMapFileInfo }

constructor TMapFileInfo.Create(const fileName : string; const units : IList<TMapUnit>);
begin
  inherited Create;
  FFileName := fileName;
  FUnits := units;
end;

function TMapFileInfo.GetFileName : string;
begin
  result := FFileName;
end;

function TMapFileInfo.GetUnits : IReadOnlyList<TMapUnit>;
begin
  result := FUnits.AsReadOnly;
end;

function TMapFileInfo.GetHasSourcePaths : boolean;
var
  u : TMapUnit;
begin
  result := false;
  for u in FUnits do
  begin
    if u.SourcePath <> '' then
    begin
      result := true;
      exit;
    end;
  end;
end;

{ TMapFileReader }

constructor TMapFileReader.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

type
  TSection = (None, Segments, OtherKnown);

const
  cSegmentsHeader     = 'Detailed map of segments';
  cPublicsByNameHeader  = 'Address         Publics by Name';
  cPublicsByValueHeader = 'Address         Publics by Value';
  cLineNumbersPrefix    = 'Line numbers for ';
  cSegmentsSuffix       = ' segment .text';

// Strips the UTF-8 BOM (EF BB BF) from the start of a string if present.
// Some Delphi linker .map files in newer versions are UTF-8 encoded and a
// stray BOM would otherwise survive into the first parsed line.
procedure StripUtf8Bom(var s : string);
begin
  if (Length(s) > 0) and (s[1] = #$FEFF) then
    Delete(s, 1, 1);
end;

function ExtractSegmentUnitName(const line : string) : string;
var
  trimmed : string;
  mPos : integer;
  endPos : integer;
  candidate : string;
begin
  result := '';
  trimmed := Trim(line);
  if trimmed = '' then
    exit;
  //Segment lines look like:
  //  0001:00400000 0000A000 C=CODE S=.text G=(none) M=System ACBP=A9
  //  0001:0000000000400000 0000000000050000 C=CODE S=.text G=(none) M=System.SysUtils ACBP=A9
  //We look for the literal "M=" and take the token before the next whitespace.
  //Note: G=(none) precedes M=, so we cannot rely on "G=" being absent. Search forward.
  mPos := Pos(' M=', trimmed);
  if mPos = 0 then
    exit;
  Inc(mPos, 3); // skip past " M="
  endPos := mPos;
  while (endPos <= Length(trimmed)) and (trimmed[endPos] > ' ') do
    Inc(endPos);
  candidate := Copy(trimmed, mPos, endPos - mPos);
  //Empty units (M= with nothing) and the linker's "(none)" placeholder are not real units.
  if (candidate = '') or (candidate = '(none)') then
    exit;
  result := candidate;
end;

procedure ParseLineNumbersHeader(const line : string; out unitName, sourcePath : string);
var
  body : string;
  openParen : integer;
  closeParen : integer;
begin
  unitName := '';
  sourcePath := '';
  //Headers are of the form:
  //  Line numbers for System.SysUtils(C:\Embarcadero\Studio\23.0\source\rtl\sys\System.SysUtils.pas) segment .text
  //  Line numbers for Spring.Collections(Spring.Collections.pas) segment .text
  //We tolerate trailing whitespace and case-insensitive prefix matching.
  if not StartsText(cLineNumbersPrefix, line) then
    exit;
  body := Copy(line, Length(cLineNumbersPrefix) + 1, MaxInt);
  //chop trailing " segment .text" if present.
  if EndsText(cSegmentsSuffix, body) then
    SetLength(body, Length(body) - Length(cSegmentsSuffix));
  body := TrimRight(body);
  //find UnitName(SourcePath)
  openParen := LastDelimiter('(', body);
  closeParen := LastDelimiter(')', body);
  if (openParen = 0) or (closeParen = 0) or (closeParen <= openParen) then
    exit;
  unitName := TrimRight(Copy(body, 1, openParen - 1));
  sourcePath := Trim(Copy(body, openParen + 1, closeParen - openParen - 1));
end;

function TMapFileReader.Read(const fileName : string) : IMapFileInfo;
var
  lines : TStringList;
  units : IList<TMapUnit>;
  unitIndex : IDictionary<string, integer>;
  section : TSection;
  i : integer;
  line : string;
  trimmed : string;
  unitName : string;
  sourcePath : string;
  existingIdx : integer;
  entry : TMapUnit;
begin
  result := nil;
  if not FileExists(fileName) then
  begin
    if FLogger <> nil then
      FLogger.Debug('[MapFile] not found : ' + fileName);
    exit;
  end;

  units := TCollections.CreateList<TMapUnit>;
  unitIndex := TCollections.CreateDictionary<string, integer>;

  lines := TStringList.Create;
  try
    try
      lines.LoadFromFile(fileName);
    except
      on e : Exception do
      begin
        if FLogger <> nil then
          FLogger.Error('[MapFile] failed to load [' + fileName + '] : ' + e.Message);
        exit;
      end;
    end;

    if lines.Count > 0 then
    begin
      line := lines[0];
      StripUtf8Bom(line);
      lines[0] := line;
    end;

    section := TSection.None;
    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      trimmed := Trim(line);
      if trimmed = '' then
        continue;

      //Section headers don't follow a strict format - we recognise them by literal text.
      //"Detailed map of segments" / "Address ... Publics ..." are anchor lines we pivot on.
      if StartsText(cSegmentsHeader, trimmed) then
      begin
        section := TSection.Segments;
        continue;
      end;
      if StartsText('Address ', trimmed) and (Pos('Publics', trimmed) > 0) then
      begin
        section := TSection.OtherKnown;
        continue;
      end;
      //"Line numbers for ..." headers are processed regardless of which section
      //the parser thinks it's in, because the file is sometimes truncated /
      //missing the "Address Publics" sentinel before them.
      if StartsText(cLineNumbersPrefix, trimmed) then
      begin
        ParseLineNumbersHeader(trimmed, unitName, sourcePath);
        if unitName = '' then
          continue;
        if unitIndex.TryGetValue(LowerCase(unitName), existingIdx) then
        begin
          //first non-empty SourcePath wins; subsequent generic instantiations refer to
          //the same source file and don't add new information.
          if (units[existingIdx].SourcePath = '') and (sourcePath <> '') then
          begin
            entry := units[existingIdx];
            entry.SourcePath := sourcePath;
            units[existingIdx] := entry;
          end;
        end
        else
        begin
          entry.UnitName := unitName;
          entry.SourcePath := sourcePath;
          units.Add(entry);
          unitIndex.Add(LowerCase(unitName), units.Count - 1);
        end;
        section := TSection.OtherKnown;
        continue;
      end;

      if section = TSection.Segments then
      begin
        unitName := ExtractSegmentUnitName(trimmed);
        if unitName = '' then
          continue;
        //Deduplicate by case-insensitive unit name. A unit can appear multiple
        //times across segments (e.g. once per .text group) but is still one unit.
        if not unitIndex.ContainsKey(LowerCase(unitName)) then
        begin
          entry.UnitName := unitName;
          entry.SourcePath := '';
          units.Add(entry);
          unitIndex.Add(LowerCase(unitName), units.Count - 1);
        end;
      end;
    end;

    result := TMapFileInfo.Create(fileName, units);
  finally
    lines.Free;
  end;
end;

end.
