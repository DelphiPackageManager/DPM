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

unit DPM.Core.Options.Cache;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  // `dpm cache <install|remove|verify>`. Plain names so VSoft's RTTI enum parser
  // maps the positional sub-command argument directly (same pattern as
  // TSourcesSubCommand). Invalid is ordinal 0 / the default.
  TCacheSubCommand = (Invalid, Install, Remove, Verify);

  TCacheOptions = class(TSearchOptions)
  private
    FPreRelease : boolean;
    FVersionString : string;
    FVersion : TPackageVersion;
    FVerifyAll : boolean;
    FCommand : TCacheSubCommand;
    FForce : boolean;
    class var
      FDefault : TCacheOptions;
  protected
    function GetPackageId : string;
    procedure SetPackageId(const Value : string);
    constructor CreateClone(const original : TCacheOptions); reintroduce;

  public
    class constructor CreateDefault;
    class property Default : TCacheOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TCacheOptions; reintroduce;

    property PackageId : string read GetPackageId write SetPackageId;
    property PreRelease : boolean read FPreRelease write FPreRelease;
    property VersionString : string read FVersionString write FVersionString;
    property Version : TPackageVersion read FVersion write FVersion;

    // Which sub-command was requested. The CLI dispatches on this.
    property Command : TCacheSubCommand read FCommand write FCommand;

    // `dpm cache remove --force` — skip the y/N confirmation prompt.
    property Force : boolean read FForce write FForce;

    // `dpm cache verify` — re-hash every cached package against its manifest
    // and re-run signature verification (plan §1.6, V-34). When set, the
    // PackageId / Compiler / Version checks below are skipped. Kept in sync
    // with Command = Verify by the option registration.
    property VerifyAll : boolean read FVerifyAll write FVerifyAll;

  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,
  DPM.Core.Constants;

{ TCacheOptions }

function TCacheOptions.Clone : TCacheOptions;
begin
  result := TCacheOptions.CreateClone(self);
end;

constructor TCacheOptions.Create;
begin
  inherited;
  FVersion := TPackageVersion.Empty;
end;

constructor TCacheOptions.CreateClone(const original : TCacheOptions);
begin
  inherited CreateClone(original);
  FPreRelease := original.FPreRelease;
  FVersionString := original.FVersionString;
  FVersion := original.FVersion;
  FVerifyAll := original.FVerifyAll;
  FCommand := original.FCommand;
  FForce := original.FForce;
end;

class constructor TCacheOptions.CreateDefault;
begin
  FDefault := TCacheOptions.Create;
  FDefault.FCommand := TCacheSubCommand.Invalid;
end;

function TCacheOptions.GetPackageId : string;
begin
  result := SearchTerms;
end;

procedure TCacheOptions.SetPackageId(const Value : string);
begin
  SearchTerms := value;
end;

function TCacheOptions.Validate(const logger : ILogger) : Boolean;
var
  error : string;
begin
  //must call inherited
  result := inherited Validate(logger);

  case FCommand of
    TCacheSubCommand.Invalid :
      begin
        logger.Error('A sub-command is required: install, remove or verify.');
        result := false;
      end;

    // `dpm cache verify` takes no positional args — short-circuit the
    // package-id / compiler / version checks that the other forms require.
    TCacheSubCommand.Verify : ; //nothing else to validate

    TCacheSubCommand.Install :
      begin
        if PackageId = '' then
        begin
          logger.Error('The <packageId> option must be specified.');
          result := false;
        end;

        if ConfigFile = '' then
        begin
          logger.Error('No configuration file specified');
          FIsValid := false;
          result := false;
          exit;
        end;

        if not TRegEx.IsMatch(PackageId, cPackageIdRegex) then
        begin
          logger.Error('The specified package Id  [' + PackageId + '] is not a valid Package Id.');
          result := false;
        end;

        if VersionString <> '' then
        begin
          if not TPackageVersion.TryParseWithError(VersionString, FVersion, error) then
          begin
            logger.Error('The specified package Version  [' + VersionString + '] is not a valid version - ' + error);
            result := false;
          end;
        end;

        if CompilerVersion = TCompilerVersion.UnknownVersion then
        begin
          logger.Error('Compiler option is required');
          result := false;
        end;
      end;

    // `dpm cache remove` needs only a valid package id. Compiler and version
    // are optional filters - omitting them widens the match (all compilers /
    // all versions).
    TCacheSubCommand.Remove :
      begin
        if PackageId = '' then
        begin
          logger.Error('The <packageId> option must be specified.');
          result := false;
        end
        else if not TRegEx.IsMatch(PackageId, cPackageIdRegex) then
        begin
          logger.Error('The specified package Id  [' + PackageId + '] is not a valid Package Id.');
          result := false;
        end;

        if VersionString <> '' then
        begin
          if not TPackageVersion.TryParseWithError(VersionString, FVersion, error) then
          begin
            logger.Error('The specified package Version  [' + VersionString + '] is not a valid version - ' + error);
            result := false;
          end;
        end;
      end;
  end;

  FIsValid := result;
end;

end.

