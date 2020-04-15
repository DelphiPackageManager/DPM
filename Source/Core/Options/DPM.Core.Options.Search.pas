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

unit DPM.Core.Options.Search;

interface

uses
  DPM.Core.Types,
  DPM.Core.Options.Base;

type
  TSearchOptions = class(TOptionsBase)
  private
    FSources : string;
    FSearchTerms : string;
    FSkip : integer;
    FTake : integer;
    FCompilerVersion : TCompilerVersion;
    FPlatforms : TDPMPlatforms;
    FVersion  : TPackageVersion;
    FAllVersions : boolean;
    FPrerelease : boolean;
    FIncludeDelisted : boolean;
  protected
    FExact : boolean;
    constructor CreateClone(const original : TSearchOptions);reintroduce;

  public
    constructor Create;override;
    function Clone : TSearchOptions;virtual;
    property AllVersions      : boolean read FAllVersions write FAllVersions;
    property Prerelease       : boolean read FPrerelease write FPrerelease;
    //IncludeDelisted not implemented. yet.
    property IncludeDelisted  : boolean read FIncludeDelisted write FIncludeDelisted;
    //comma separated list of sources, empty means all.
    property Sources          : string  read FSources write FSources;
    property SearchTerms      : string  read FSearchTerms write FSearchTerms;
    property Skip             : integer read FSkip write FSkip;
    property Take             : integer read FTake write FTake;
    property CompilerVersion  : TCompilerVersion read FCompilerVersion write FCompilerVersion;
    property Platforms        : TDPMPlatforms read FPlatforms write FPlatforms;
    property Version          : TPackageVersion read FVersion write FVersion;
    property Exact            : boolean read FExact write FExact; //search term is a package id.
  end;

implementation

{ TSearchOptions }

function TSearchOptions.Clone: TSearchOptions;
begin
  result := TSearchOptions.CreateClone(Self);
end;

constructor TSearchOptions.Create;
begin
  inherited;
  FCompilerVersion := TCompilerVersion.UnknownVersion;
  FSkip := 0;
  FTake := 0;
  FPlatforms := [];
  FVersion := TPackageVersion.Empty;
  FExact := false;
end;

constructor TSearchOptions.CreateClone(const original: TSearchOptions);
begin
  inherited CreateClone(original);
  FSources := original.FSources;
  FSearchTerms := original.FSearchTerms;
  FSkip := original.FSkip;
  FTake := original.FTake;
  FCompilerVersion := original.FCompilerVersion;
  FPlatforms := original.FPlatforms;
  FVersion  := original.FVersion;
  FAllVersions := original.FAllVersions;
  FPrerelease := original.FPrerelease;
  FIncludeDelisted := original.FIncludeDelisted;
end;

end.
