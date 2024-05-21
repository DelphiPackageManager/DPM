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

unit DPM.Core.Options.Install;

interface


uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  TInstallOptions = class(TSearchOptions)
  private
    FPackageFile : string;
    FVersionString : string;
    FNoCache : boolean;
    FProjectPath : string;
    FProjects : TArray<string>;
    FProjectGroup : string;
    FFloat : boolean;
    FIsUpgrade : boolean;
    class var
      FDefault : TInstallOptions;
  protected
    function GetPackageId : string;
    procedure SetPackageId(const Value : string);
    constructor CreateClone(const original : TInstallOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TInstallOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TInstallOptions; reintroduce;
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
  DPM.Core.Constants;

{ TInstallOptions }

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
end;

class constructor TInstallOptions.CreateDefault;
begin
  FDefault := TInstallOptions.Create;
end;

function TInstallOptions.GetPackageId : string;
begin
  result := SearchTerms;
end;

procedure TInstallOptions.SetPackageId(const Value : string);
begin
  SearchTerms := value;
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
    Logger.Error('No configuration file specified');
    exit;
  end;

  PackageFile := PackageId;


  if (PackageId <> '') then
  begin
    if TRegEx.IsMatch(PackageId, cPackageIdRegex) then
    begin
      //      Logger.Error('The specified package Id  [' + PackageId + '] is not a valid Package Id.');
      FPackageFile := ''
    end
    else if not FileExists(FPackageFile) then
    begin
      Logger.Error('The specified packageFile [' + FPackageFile + '] does not exist.');
      result := false;
    end
    else
    begin
      packageString := ChangeFileExt(ExtractFileName(FPackageFile), '');
      if not TRegEx.IsMatch(packageString, cPackageFileRegex) then
      begin
        Logger.Error('The specified packageFile name [' + packageString + '] is not in the correct format.');
        result := false;
      end;
      PackageId := '';
    end;
  end;

  if VersionString <> '' then
  begin
    if not TPackageVersion.TryParseWithError(VersionString, theVersion, error) then
    begin
      Logger.Error('The specified package Version  [' + VersionString + '] is not a valid version - ' + error);
      result := false;
    end;
    Self.Version := theVersion;
  end;

  if (FProjectPath = '') and (Length(FProjects) = 0) then
  begin
    Logger.Error('Project path cannot be empty, must either be a directory or project file.');
    result := false;
  end;

  FIsValid := result;
end;

end.

