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
    FPackageFile : string;
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

    // `dpm cache install <file.dpkg>` — when the positional argument is the path
    // to a package file on disk rather than a package id, Validate moves it here
    // (and clears PackageId). The installer extracts this file into the cache and
    // builds it, instead of resolving the id from the configured sources. Empty
    // for the normal id-based form.
    property PackageFile : string read FPackageFile write FPackageFile;
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
  DPM.Core.Packaging.IdValidator,
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
  FPackageFile := original.FPackageFile;
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
  packageString : string;
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

        // The positional argument is either a package id (resolved from the
        // configured sources) or the path to a .dpkg file on disk. Mirror the
        // install command: a value matching the package-id pattern is treated as
        // an id; otherwise it must be an existing package file. For a file the
        // compiler and version come from the file name, so --compiler / --version
        // are not required (and are ignored).
        if (PackageId <> '') and TPackageIdValidator.IsValidPackageId(PackageId) then
        begin
          // Use the canonical id validator (not the looser cPackageIdRegex) so the
          // command fails up front for ids that the package spec loader would later
          // reject anyway - e.g. a too-short org prefix like 'CW.ResourceUtils'.
          // Otherwise the loose check passes here, we contact the sources and cache
          // the package, and the strict check then logs 'Invalid package Id' once
          // per dspec parse without ever stopping the operation.
          FPackageFile := '';

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
        end
        else if (PackageId <> '') and FileExists(PackageId) then
        begin
          FPackageFile := PackageId;
          packageString := ChangeFileExt(ExtractFileName(FPackageFile), '');
          if not TRegEx.IsMatch(packageString, cPackageFileRegex) then
          begin
            logger.Error('The specified package file name [' + packageString + '] is not in the correct format.');
            result := false;
          end;
          // Compiler and version are taken from the file name by the installer.
          PackageId := '';
        end
        else if PackageId <> '' then
        begin
          logger.Error('The specified package Id  [' + PackageId + '] is not a valid Package Id, and no package file of that name exists.');
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

