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

unit DPM.Core.Options.SBOM;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Base;

{$SCOPEDENUMS ON}

type
  //Phase 2: format selection is now multi-value so -format=cyclonedx,html works.
  //The previous 'Both' value made no sense once we support more than two outputs;
  //the alias is preserved at the parser level so existing scripts that pass
  //-format=both keep working (it expands to [CycloneDX, SPDX]).
  TSBOMFormat = (CycloneDX, SPDX, HTML, Markdown);
  TSBOMFormats = set of TSBOMFormat;

const
  cDefaultSBOMFormats : TSBOMFormats = [TSBOMFormat.CycloneDX, TSBOMFormat.SPDX];
  cAllSBOMFormats : TSBOMFormats = [TSBOMFormat.CycloneDX, TSBOMFormat.SPDX, TSBOMFormat.HTML, TSBOMFormat.Markdown];

type
  TSBOMOptions = class(TOptionsBase)
  private
    FProjectPath : string;
    FOutputDir : string;
    FFormats : TSBOMFormats;
    FPlatforms : TDPMPlatforms;
    FConfig : string;
    FMapFile : string;
    FIncludeRuntime : boolean;
    FStrict : boolean;
    FPerProject : boolean;
    class var
      FDefault : TSBOMOptions;
  protected
    constructor CreateClone(const original : TSBOMOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TSBOMOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TSBOMOptions; reintroduce;

    property ProjectPath : string read FProjectPath write FProjectPath;
    property OutputDir : string read FOutputDir write FOutputDir;
    property Formats : TSBOMFormats read FFormats write FFormats;
    property Platforms : TDPMPlatforms read FPlatforms write FPlatforms;
    property Config : string read FConfig write FConfig;
    property MapFile : string read FMapFile write FMapFile;
    property IncludeRuntime : boolean read FIncludeRuntime write FIncludeRuntime;
    property Strict : boolean read FStrict write FStrict;
    /// <summary>
    ///  When the input is a .groupproj: false (default) produces ONE aggregated
    ///  SBOM per platform (group -> project applications -> packages). true
    ///  reverts to legacy per-dproj output (N SBOMs per platform). Ignored when
    ///  the input is a single .dproj.
    /// </summary>
    property PerProject : boolean read FPerProject write FPerProject;
  end;

//Parses a single token to a single format. Raises on unrecognised input.
function StringToSBOMFormat(const value : string) : TSBOMFormat;
//Parses a comma-separated list of tokens to a format set. Accepts aliases
//'both' (= cyclonedx + spdx) and 'all' (every format). Empty input maps to
//the default ([CycloneDX, SPDX]).
function StringToSBOMFormats(const value : string) : TSBOMFormats;
function SBOMFormatToString(const value : TSBOMFormat) : string;

implementation

uses
  System.SysUtils,
  System.Classes;

function StringToSBOMFormat(const value : string) : TSBOMFormat;
var
  v : string;
begin
  v := LowerCase(Trim(value));
  if (v = 'cyclonedx') or (v = 'cdx') then
    result := TSBOMFormat.CycloneDX
  else if (v = 'spdx') then
    result := TSBOMFormat.SPDX
  else if (v = 'html') or (v = 'htm') then
    result := TSBOMFormat.HTML
  else if (v = 'markdown') or (v = 'md') then
    result := TSBOMFormat.Markdown
  else
    raise EArgumentException.Create('Invalid sbom format [' + value + '] - expected cyclonedx | spdx | html | markdown');
end;

function StringToSBOMFormats(const value : string) : TSBOMFormats;
var
  trimmed : string;
  parts : TStringList;
  i : integer;
  token : string;
begin
  result := [];
  trimmed := Trim(value);
  if trimmed = '' then
  begin
    result := cDefaultSBOMFormats;
    exit;
  end;

  parts := TStringList.Create;
  try
    parts.CommaText := trimmed;
    //CommaText preserves order but also accepts semicolons - good enough as a
    //hand-typed CLI list parser. Also tolerate whitespace.
    for i := 0 to parts.Count - 1 do
    begin
      token := LowerCase(Trim(parts[i]));
      if token = '' then
        continue;
      if token = 'all' then
        result := result + cAllSBOMFormats
      else if token = 'both' then
        result := result + cDefaultSBOMFormats
      else if token = 'json' then
        //Convenience alias for 'all the machine-readable formats'.
        result := result + [TSBOMFormat.CycloneDX, TSBOMFormat.SPDX]
      else
        Include(result, StringToSBOMFormat(token));
    end;
  finally
    parts.Free;
  end;

  if result = [] then
    result := cDefaultSBOMFormats;
end;

function SBOMFormatToString(const value : TSBOMFormat) : string;
begin
  case value of
    TSBOMFormat.CycloneDX : result := 'cyclonedx';
    TSBOMFormat.SPDX : result := 'spdx';
    TSBOMFormat.HTML : result := 'html';
    TSBOMFormat.Markdown : result := 'markdown';
  else
    result := '';
  end;
end;

{ TSBOMOptions }

function TSBOMOptions.Clone : TSBOMOptions;
begin
  result := TSBOMOptions.CreateClone(Self);
end;

constructor TSBOMOptions.Create;
begin
  inherited;
  FFormats := cDefaultSBOMFormats;
  FPlatforms := [];
  FIncludeRuntime := true;
  FStrict := false;
  FPerProject := false;
end;

constructor TSBOMOptions.CreateClone(const original : TSBOMOptions);
begin
  inherited CreateClone(original);
  FProjectPath := original.FProjectPath;
  FOutputDir := original.FOutputDir;
  FFormats := original.FFormats;
  FPlatforms := original.FPlatforms;
  FConfig := original.FConfig;
  FMapFile := original.FMapFile;
  FIncludeRuntime := original.FIncludeRuntime;
  FStrict := original.FStrict;
  FPerProject := original.FPerProject;
end;

class constructor TSBOMOptions.CreateDefault;
begin
  FDefault := TSBOMOptions.Create;
end;

function TSBOMOptions.Validate(const logger : ILogger) : Boolean;
begin
  result := inherited Validate(logger);

  if FProjectPath = '' then
  begin
    logger.Error('Project path cannot be empty, must be a .dproj or .groupproj file.');
    result := false;
  end;
end;

end.
