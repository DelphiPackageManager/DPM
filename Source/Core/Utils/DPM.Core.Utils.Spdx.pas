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

unit DPM.Core.Utils.Spdx;

// Shared SPDX license id lookup. The full (non-deprecated) SPDX license list
// is compiled into the host binary as the DPM_SPDX_LICENSES / RCDATA resource
// (one 'id=name,url' line per license). Used to decide whether a free-form
// license string is a known SPDX id - so the IDE can render it as a clickable
// link to the SPDX page - and to populate license pick lists in DSpecCreator.
//
// VCL/GDI free so it can live in Core and be used by the CLI, the IDE plugins
// and DSpecCreator alike.

interface

uses
  System.Classes;

type
  TSpdxLicenses = class
  public
    /// <summary>True when id is a known SPDX license id (case-insensitive).</summary>
    class function IsValidLicenseId(const id : string) : boolean; static;
    /// <summary>Reference url for the id, or '' when the id is unknown.</summary>
    class function GetLicenseUrl(const id : string) : string; static;
    /// <summary>Human readable name for the id, or '' when the id is unknown.</summary>
    class function GetLicenseName(const id : string) : string; static;
    /// <summary>Appends every known SPDX license id to dest. dest is not cleared.</summary>
    class procedure GetLicenseIds(const dest : TStrings); static;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

// Pull in the DPM_SPDX_LICENSES / RCDATA resource. Delphi compiles the .rc on
// demand via brcc32, so simply rebuilding any binary that uses this unit embeds
// the latest spdx-licenses.txt content. The .rc lives alongside dpm.rc in the
// Source/ directory so the CLI, every IDE plugin and DSpecCreator share one
// source. Delphi resolves $R first in the source file's directory, then in the
// project (.dpr) directory - which is Source/ for every consumer here.
{$R '..\..\DPM.Spdx.res'}

const
  //'RCDATA' is a standard resource type, so it is stored numerically as
  //RT_RCDATA (not as a custom string type) - that is what the loader must ask
  //for, matching how the list was originally loaded in DSpecCreator.
  cSpdxResource = 'DPM_SPDX_LICENSES';

var
  GSpdxList : TStringList;
  GLoaded : boolean;

procedure EnsureLoaded;
var
  stream : TResourceStream;
begin
  if GLoaded then
    exit;
  GLoaded := true;
  //GSpdxList created at unit init; tolerate a missing/garbled resource by
  //leaving it empty - callers then treat every license as 'not a known id'.
  try
    stream := TResourceStream.Create(HInstance, cSpdxResource, RT_RCDATA);
    try
      //LoadFromStream with no explicit encoding uses the default ANSI codepage
      //(the resource has no BOM), matching how the list was generated.
      GSpdxList.LoadFromStream(stream);
    finally
      stream.Free;
    end;
  except
    //swallow - GSpdxList stays empty.
  end;
end;

//Returns the 'name,url' value for an id, or '' when not found.
function LookupValue(const id : string) : string;
var
  idx : integer;
  trimmed : string;
begin
  result := '';
  trimmed := Trim(id);
  if trimmed = '' then
    exit;
  EnsureLoaded;
  idx := GSpdxList.IndexOfName(trimmed);
  if idx <> -1 then
    result := GSpdxList.ValueFromIndex[idx];
end;

{ TSpdxLicenses }

class function TSpdxLicenses.IsValidLicenseId(const id : string) : boolean;
begin
  result := LookupValue(id) <> '';
end;

class function TSpdxLicenses.GetLicenseUrl(const id : string) : string;
var
  value : string;
  p : integer;
begin
  //value is 'name,url' - the url is always the final field, so split on the
  //last comma (license names themselves can contain commas).
  result := '';
  value := LookupValue(id);
  if value = '' then
    exit;
  p := LastDelimiter(',', value);
  if p > 0 then
    result := Copy(value, p + 1, MaxInt);
end;

class function TSpdxLicenses.GetLicenseName(const id : string) : string;
var
  value : string;
  p : integer;
begin
  result := '';
  value := LookupValue(id);
  if value = '' then
    exit;
  p := LastDelimiter(',', value);
  if p > 0 then
    result := Copy(value, 1, p - 1)
  else
    result := value;
end;

class procedure TSpdxLicenses.GetLicenseIds(const dest : TStrings);
var
  i : integer;
begin
  if dest = nil then
    exit;
  EnsureLoaded;
  for i := 0 to GSpdxList.Count - 1 do
    dest.Add(GSpdxList.Names[i]);
end;

initialization
  GSpdxList := TStringList.Create;
  GLoaded := false;

finalization
  GSpdxList.Free;

end.
