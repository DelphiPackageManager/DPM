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

unit DPM.Core.Options.Install;

interface


uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  ///<summary> One package to install. An empty Version means the latest available (honouring
  ///  Prerelease). </summary>
  TInstallPackageRequest = record
    PackageId : string;
    Version : TPackageVersion;
  end;

  ///<summary>Options driving an install. Populated differently depending on the caller:
  ///
  ///  CLI path (via the Install command, reading cmdline args):
  ///    PackageId / PackageFile / VersionString / ProjectPath (directory or .dproj/.groupproj file)
  ///    CompilerVersion / Platforms / Sources / Prerelease (from the inherited TSearchOptions)
  ///
  ///  IDE path (from the package manager UI):
  ///    PackageId, Version (TSearchOptions.Version, already-parsed) or AddPackage, Projects (explicit
  ///    list of .dproj files from the project grid), Platforms = [], IsUpgrade, CompilerVersion
  ///    ProjectPath is left blank — the IDE uses the Projects array instead.
  ///
  ///  Either ProjectPath or Projects must be set; Validate rejects the case where both are set
  ///  (ambiguous) or neither is set.
  ///
  ///  Several packages can be installed in one operation (resolved together - e.g. packages that
  ///  pin each other's versions). On the command line PackageId holds the list as
  ///  'id[@version],id[@version]...'; Validate parses it into Packages and leaves PackageId/Version
  ///  mirroring the first entry, so code dealing with a single package keeps working.
  ///</summary>
  TInstallOptions = class(TSearchOptions)
  private
    FPackageFile : string;
    FVersionString : string;
    FNoCache : boolean;
    FProjectPath : string;                   // CLI: directory or single .dproj/.groupproj; IDE: unused
    FProjects : TArray<string>;              // IDE: explicit .dproj list from the project grid; CLI: unused
    FProjectGroup : string;
    FFloat : boolean;
    FIsUpgrade : boolean;                    // upgrading installed packages : only projects that already reference them, plus companion packages
    FPackages : TArray<TInstallPackageRequest>;
    class var
      FDefault : TInstallOptions;
    function ParsePackageList(const logger : ILogger) : boolean;
  protected
    function GetPackageId : string;
    procedure SetPackageId(const Value : string);
    constructor CreateClone(const original : TInstallOptions); reintroduce;
  public
    class constructor CreateDefault;
    class destructor DestroyDefault;
    class property Default : TInstallOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TInstallOptions; reintroduce;
    //adds a package to install - empty version = latest.
    procedure AddPackage(const packageId : string; const version : TPackageVersion);
    property Packages : TArray<TInstallPackageRequest> read FPackages;
    property IsUpgrade : boolean read FIsUpgrade write FIsUpgrade;
    property PackageId : string read GetPackageId write SetPackageId;
    property PackageFile : string read FPackageFile write FPackageFile;
    property ProjectPath : string read FProjectPath write FProjectPath;
    property Projects : TArray<string> read FProjects write FProjects;
    property ProjectGroup : string read FProjectGroup write FProjectGroup;
    property VersionString : string read FVersionString write FVersionString;
  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,
  DPM.Core.Constants,
  DPM.Core.Utils.Strings,
  DPM.Core.Diagnostics;

{ TInstallOptions }

procedure TInstallOptions.AddPackage(const packageId : string; const version : TPackageVersion);
var
  count : integer;
begin
  count := Length(FPackages);
  SetLength(FPackages, count + 1);
  FPackages[count].PackageId := packageId;
  FPackages[count].Version := version;
  if count = 0 then
  begin
    Self.PackageId := packageId;
    Self.Version := version;
  end;
end;

function TInstallOptions.Clone : TInstallOptions;
begin
  result := TInstallOptions.CreateClone(self);
end;

constructor TInstallOptions.Create;
begin
  inherited;
  FExact := true;
end;

constructor TInstallOptions.CreateClone(const original : TInstallOptions);
begin
  inherited CreateClone(original);
  FPackageFile := original.FPackageFile;
  FVersionString := original.FVersionString;
  FNoCache := original.FNoCache;
  FProjectPath := original.FProjectPath;
  FFloat := original.FFloat;
  Force := original.Force;
  FIsUpgrade := original.IsUpgrade;
  FPackages := Copy(original.FPackages);
end;

class constructor TInstallOptions.CreateDefault;
begin
  FDefault := TInstallOptions.Create;
end;

//the singleton goes with the unit - it was never freed, so every run reported it as a leak.
class destructor TInstallOptions.DestroyDefault;
begin
  FDefault.Free;
end;

function TInstallOptions.GetPackageId : string;
begin
  result := SearchTerms;
end;

procedure TInstallOptions.SetPackageId(const Value : string);
begin
  SearchTerms := value;
end;

//Parses PackageId as 'id[@version],id[@version]...' into Packages. Ids can't contain '@' or ',' (see
//cPackageIdRegex) so the separators are unambiguous. A version can come from the item or, when there is
//a single package, from VersionString/Version - not both.
function TInstallOptions.ParsePackageList(const logger : ILogger) : boolean;
var
  items : TArray<string>;
  item : string;
  id : string;
  versionText : string;
  atPos : integer;
  version : TPackageVersion;
  error : string;
  i : integer;
  anyInlineVersion : boolean;
begin
  result := true;
  items := TStringUtils.SplitStr(PackageId, ',', TSplitStringOptions.ExcludeEmpty);
  anyInlineVersion := false;
  for item in items do
  begin
    atPos := Pos('@', item);
    if atPos > 0 then
    begin
      id := Trim(Copy(item, 1, atPos - 1));
      versionText := Trim(Copy(item, atPos + 1, Length(item)));
    end
    else
    begin
      id := Trim(item);
      versionText := '';
    end;

    if not TRegEx.IsMatch(id, cPackageIdRegex) then
    begin
      logger.Error(TErrorCode.E0102_InvalidArgument, '[' + id + '] is not a valid package id or an existing package file.');
      result := false;
      continue;
    end;

    for i := 0 to Length(FPackages) - 1 do
    begin
      if SameText(FPackages[i].PackageId, id) then
      begin
        logger.Error(TErrorCode.E0103_ConflictingArguments, 'The package [' + id + '] is specified more than once.');
        result := false;
      end;
    end;

    version := TPackageVersion.Empty;
    if versionText <> '' then
    begin
      anyInlineVersion := true;
      if not TPackageVersion.TryParseWithError(versionText, version, error) then
      begin
        logger.Error(TErrorCode.E0102_InvalidArgument, 'The version [' + versionText + '] given for the package [' + id + '] is not a valid version - ' + error);
        result := false;
        continue;
      end;
    end;

    SetLength(FPackages, Length(FPackages) + 1);
    FPackages[Length(FPackages) - 1].PackageId := id;
    FPackages[Length(FPackages) - 1].Version := version;
  end;

  if Length(FPackages) = 0 then
  begin
    if result then
      logger.Error(TErrorCode.E0101_MissingArgument, 'No package was specified.');
    result := false;
    exit;
  end;

  if VersionString <> '' then
  begin
    if Length(FPackages) > 1 then
    begin
      logger.Error(TErrorCode.E0103_ConflictingArguments, 'The --version option can only be used with a single package - use id@version for each package instead.');
      result := false;
    end
    else if anyInlineVersion then
    begin
      logger.Error(TErrorCode.E0103_ConflictingArguments, 'The version is specified twice - use either the --version option or id@version.');
      result := false;
    end;
  end;

  //a single package without an inline version takes the version option (or the version the caller set).
  if (Length(FPackages) = 1) and (not anyInlineVersion) then
    FPackages[0].Version := Self.Version;

  //keep PackageId/Version describing the first package for code that deals with a single one.
  PackageId := FPackages[0].PackageId;
  Self.Version := FPackages[0].Version;
end;

function TInstallOptions.Validate(const logger : ILogger) : Boolean;
var
  packageString : string;
  error : string;
  theVersion : TPackageVersion;
begin
  //must call inherited
  result := inherited Validate(logger);

  if ConfigFile = '' then
  begin
    logger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    FIsValid := false;
    exit(false);
  end;

  if VersionString <> '' then
  begin
    if not TPackageVersion.TryParseWithError(VersionString, theVersion, error) then
    begin
      logger.Error(TErrorCode.E0102_InvalidArgument, 'The version [' + VersionString + '] is not a valid version - ' + error);
      result := false;
    end;
    Self.Version := theVersion;
  end;

  FPackageFile := '';
  if (Length(FPackages) = 0) and (PackageId <> '') then
  begin
    //a path to an existing package file (.dpkg) installs that one package - otherwise PackageId is a
    //list of one or more package ids.
    if (not TRegEx.IsMatch(PackageId, cPackageIdRegex)) and FileExists(PackageId) then
    begin
      FPackageFile := PackageId;
      packageString := ChangeFileExt(ExtractFileName(FPackageFile), '');
      if not TRegEx.IsMatch(packageString, cPackageFileRegex) then
      begin
        logger.Error(TErrorCode.E3005_InvalidPackageFile, 'The package file name [' + packageString + '] is not in the correct format (id-compiler-platforms-version).');
        result := false;
      end;
      PackageId := '';
    end
    else if not ParsePackageList(logger) then
      result := false;
  end;

  if (FProjectPath = '') and (Length(FProjects) = 0) then
  begin
    logger.Error(TErrorCode.E0101_MissingArgument, 'The project path is required - a folder or a project file.');
    result := false;
  end
  else if (FProjectPath <> '') and (Length(FProjects) > 0) then
  begin
    //ProjectPath is the CLI's "point at a directory or .dproj"; Projects is the IDE's "here is
    //the explicit list of .dproj files". Both set at once is ambiguous — reject it.
    logger.Error(TErrorCode.E0003_InternalError, 'ProjectPath and Projects cannot both be set - use one or the other (ProjectPath is for the CLI, Projects for the IDE).');
    result := false;
  end;

  FIsValid := result;
end;

end.

