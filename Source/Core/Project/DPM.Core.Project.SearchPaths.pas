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

unit DPM.Core.Project.SearchPaths;

{ A unit-name -> file-path resolver built from project and IDE search paths.

  This is a RESOLVER, not an enumerator. It exists to give the SBOM generator
  (and any future caller) a way to find out *where* a specific unit lives, when
  the unit is already known to be linked into the binary (via the MAP file).

  We deliberately do NOT expose any "give me every unit in the index" API.
  Doing so would let a caller dump the entire IDE Library Path into the SBOM
  whether the units are actually used or not. The MAP file is the authority on
  "what was linked"; this index only helps resolve where each of those units
  came from.

  Construction is lazy: directories are not enumerated until the first lookup
  hits them. Each path is scanned at most once per index instance. .pas and
  .dcu files are both considered.

  Origin tagging lets the SBOM classifier decide what to do with a hit:
    - Project   -> project source, do not surface as a component
    - IdeLibrary-> third-party shipped via the IDE Library Path
    - Other     -> third-party from somewhere else (e.g. a non-DPM search path) }

interface

uses
  Spring.Collections;

type
  TUnitOrigin = (Project, IdeLibrary, Other);

  IUnitSearchIndex = interface
    ['{B0F9D2E7-9A1F-4E33-9F2C-6A52DEEDA9C0}']
    procedure AddSearchPath(const path : string; const origin : TUnitOrigin);
    function TryFindUnit(const unitName : string; out filePath : string; out origin : TUnitOrigin) : boolean;
  end;

  TUnitSearchIndex = class(TInterfacedObject, IUnitSearchIndex)
  private
    type
      TEntry = record
        Path : string;
        Origin : TUnitOrigin;
        Scanned : boolean;
      end;
      THit = record
        FilePath : string;
        Origin : TUnitOrigin;
      end;
  private
    FPaths : IList<TEntry>;
    //LowerCase(unitName) -> file path / origin. First path-list entry to claim
    //a unit wins; later paths cannot shadow it (mirrors the compiler).
    FUnits : IDictionary<string, THit>;
    procedure ScanIfNeeded(const idx : integer);
  protected
    procedure AddSearchPath(const path : string; const origin : TUnitOrigin);
    function TryFindUnit(const unitName : string; out filePath : string; out origin : TUnitOrigin) : boolean;
  public
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Types;

{ TUnitSearchIndex }

constructor TUnitSearchIndex.Create;
begin
  inherited Create;
  FPaths := TCollections.CreateList<TEntry>;
  FUnits := TCollections.CreateDictionary<string, THit>;
end;

procedure TUnitSearchIndex.AddSearchPath(const path : string; const origin : TUnitOrigin);
var
  entry : TEntry;
  trimmed : string;
begin
  trimmed := Trim(path);
  if trimmed = '' then
    exit;
  entry.Path := ExcludeTrailingPathDelimiter(trimmed);
  entry.Origin := origin;
  entry.Scanned := false;
  FPaths.Add(entry);
end;

procedure TUnitSearchIndex.ScanIfNeeded(const idx : integer);
var
  entry : TEntry;
  files : TStringDynArray;
  i : integer;
  fileName : string;
  unitKey : string;
  hit : THit;
begin
  entry := FPaths[idx];
  if entry.Scanned then
    exit;
  entry.Scanned := true;
  FPaths[idx] := entry;

  if not DirectoryExists(entry.Path) then
    exit;

  //We glob both .pas and .dcu - either can be the only on-disk artefact
  //(precompiled-only packages ship .dcu; source-mode packages ship .pas).
  try
    files := TDirectory.GetFiles(entry.Path, '*.pas', TSearchOption.soTopDirectoryOnly);
  except
    SetLength(files, 0);
  end;
  for i := 0 to Length(files) - 1 do
  begin
    fileName := files[i];
    unitKey := LowerCase(ChangeFileExt(ExtractFileName(fileName), ''));
    if FUnits.ContainsKey(unitKey) then
      continue;
    hit.FilePath := fileName;
    hit.Origin := entry.Origin;
    FUnits.Add(unitKey, hit);
  end;

  try
    files := TDirectory.GetFiles(entry.Path, '*.dcu', TSearchOption.soTopDirectoryOnly);
  except
    SetLength(files, 0);
  end;
  for i := 0 to Length(files) - 1 do
  begin
    fileName := files[i];
    unitKey := LowerCase(ChangeFileExt(ExtractFileName(fileName), ''));
    if FUnits.ContainsKey(unitKey) then
      continue;
    hit.FilePath := fileName;
    hit.Origin := entry.Origin;
    FUnits.Add(unitKey, hit);
  end;
end;

function TUnitSearchIndex.TryFindUnit(const unitName : string; out filePath : string; out origin : TUnitOrigin) : boolean;
var
  key : string;
  i : integer;
  hit : THit;
begin
  result := false;
  filePath := '';
  origin := TUnitOrigin.Other;
  key := LowerCase(unitName);
  if key = '' then
    exit;

  //Try the already-scanned cache first - cheap. If absent, scan each
  //path lazily in order; first one to produce the unit wins.
  if FUnits.TryGetValue(key, hit) then
  begin
    filePath := hit.FilePath;
    origin := hit.Origin;
    result := true;
    exit;
  end;

  for i := 0 to FPaths.Count - 1 do
  begin
    if FPaths[i].Scanned then
      continue;
    ScanIfNeeded(i);
    if FUnits.TryGetValue(key, hit) then
    begin
      filePath := hit.FilePath;
      origin := hit.Origin;
      result := true;
      exit;
    end;
  end;
end;

end.
