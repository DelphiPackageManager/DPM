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

unit DPM.Core.Options.Scan;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Base,
  DPM.Core.Vuln.Types;

type
  TScanOptions = class(TOptionsBase)
  private
    FInputPath : string;
    FOutputPath : string;
    FSource : string;
    FFailOn : TSeverity;
    FNoCache : boolean;
    FPlatforms : TDPMPlatforms;
    class var
      FDefault : TScanOptions;
  protected
    constructor CreateClone(const original : TScanOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TScanOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : boolean; override;
    function Clone : TScanOptions; reintroduce;

    //An .json / .cdx.json CycloneDX SBOM file, or a .dproj / .groupproj
    //(generates the SBOM in-memory then scans it). Required.
    property InputPath : string read FInputPath write FInputPath;
    //Output .vex.json path. Default: <InputPath>.vex.json next to the input.
    //For groupproj/dproj input, per-platform suffix is added automatically.
    property OutputPath : string read FOutputPath write FOutputPath;
    //Vulnerability database name. v1 only accepts 'osv'.
    property Source : string read FSource write FSource;
    //CI-gate threshold. None (default) = always exit 0. Anything else = exit 1
    //if the max severity found is >= this value.
    property FailOn : TSeverity read FFailOn write FFailOn;
    //Bypass the 24h response cache on reads (still writes fresh responses to
    //the cache).
    property NoCache : boolean read FNoCache write FNoCache;
    //Only honoured when InputPath is a .dproj / .groupproj - forwarded to the
    //SBOM generator. Empty set means all enabled platforms in the project.
    property Platforms : TDPMPlatforms read FPlatforms write FPlatforms;
  end;

implementation

uses
  System.SysUtils;

{ TScanOptions }

function TScanOptions.Clone : TScanOptions;
begin
  result := TScanOptions.CreateClone(Self);
end;

constructor TScanOptions.Create;
begin
  inherited;
  FSource := 'osv';
  FFailOn := TSeverity.None;
  FNoCache := false;
  FPlatforms := [];
end;

constructor TScanOptions.CreateClone(const original : TScanOptions);
begin
  inherited CreateClone(original);
  FInputPath := original.FInputPath;
  FOutputPath := original.FOutputPath;
  FSource := original.FSource;
  FFailOn := original.FFailOn;
  FNoCache := original.FNoCache;
  FPlatforms := original.FPlatforms;
end;

class constructor TScanOptions.CreateDefault;
begin
  FDefault := TScanOptions.Create;
end;

function TScanOptions.Validate(const logger : ILogger) : boolean;
begin
  result := inherited Validate(logger);

  if FInputPath = '' then
  begin
    logger.Error('Input path cannot be empty - expected an SBOM .json file or a .dproj / .groupproj.');
    result := false;
  end;

  if (FSource <> '') and (not SameText(FSource, 'osv')) then
  begin
    logger.Error('Unknown vulnerability source [' + FSource + '] - v1 supports osv only.');
    result := false;
  end;
end;

end.
