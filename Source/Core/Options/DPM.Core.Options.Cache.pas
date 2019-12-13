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

unit DPM.Core.Options.Cache;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  TCacheOptions = class(TSearchOptions)
  private
    FPreRelease     : boolean;
    FVersionString  : string;
    FVersion        : TPackageVersion;
    class var
      FDefault : TCacheOptions;
  protected
    function GetPackageId: string;
    procedure SetPackageId(const Value: string);
    constructor CreateClone(const original : TCacheOptions);reintroduce;

  public
    class constructor CreateDefault;
    class property Default : TCacheOptions read FDefault;
    constructor Create;override;
    function Validate(const logger: ILogger): Boolean; override;
    function Clone : TCacheOptions;reintroduce;

    property PackageId    : string      read GetPackageId write SetPackageId;
    property PreRelease   : boolean     read FPreRelease  write FPreRelease;
    property VersionString: string      read FVersionString  write FVersionString;
    property Version      : TPackageVersion read FVersion  write FVersion;

  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,
  DPM.Core.Constants;

{ TCacheOptions }

function TCacheOptions.Clone: TCacheOptions;
begin
  result := TCacheOptions.CreateClone(self);
end;

constructor TCacheOptions.Create;
begin
  inherited;
  FVersion := TPackageVersion.Empty;
end;

constructor TCacheOptions.CreateClone(const original: TCacheOptions);
begin
  inherited CreateClone(original);
  FPreRelease     := original.FPreRelease;
  FVersionString  := original.FVersionString;
  FVersion        := original.FVersion;
end;

class constructor TCacheOptions.CreateDefault;
begin
  FDefault := TCacheOptions.Create;
end;

function TCacheOptions.GetPackageId: string;
begin
  result := SearchTerms;
end;

procedure TCacheOptions.SetPackageId(const Value: string);
begin
  SearchTerms := value;
end;

function TCacheOptions.Validate(const logger: ILogger): Boolean;
var
  error : string;
begin
  //must call inherited
  result := inherited Validate(logger);

  if TCacheOptions.Default.PackageId = '' then
  begin
    Logger.Error('The <packageId> option must be specified.');
    result := false;
  end;

  if ConfigFile = '' then
  begin
    Logger.Error('No configuration file specified');
    exit;
  end;

  if not TRegEx.IsMatch(PackageId, cPackageIdRegex) then
  begin
    Logger.Error('The specified package Id  [' + PackageId + '] is not a valid Package Id.');
    result := false;
  end;

  if VersionString <> '' then
  begin
    if not TPackageVersion.TryParseWithError(VersionString, FVersion, error) then
    begin
      Logger.Error('The specified package Version  [' + VersionString + '] is not a valid version - ' + error);
      result := false;
    end;
  end;

  if TCacheOptions.Default.CompilerVersion = TCompilerVersion.UnknownVersion then
  begin
    Logger.Error('Compiler option is required');
    result := false;
  end;

  FIsValid := result;
end;

end.
