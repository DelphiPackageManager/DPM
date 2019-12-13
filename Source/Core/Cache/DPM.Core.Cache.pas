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

unit DPM.Core.Cache;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Spec.Interfaces;

type
  TPackageCache = class(TInterfacedObject, IPackageCache)
  private
    FLogger : ILogger;
    FSpecReader : IPackageSpecReader;
    FLocation : string;
  protected
    procedure SetLocation(const value : string);
    function GetLocation: string;
    function GetPackagesFolder : string;

    function CachePackage(const packageIndentity: IPackageIdentity; const saveFile: Boolean): Boolean;
    function Clean: Boolean;
    function CreatePackagePath(const packageIndentity : IPackageIdentity): string;
    function GetPackagePath(const packageIndentity : IPackageIdentity): string;
    function EnsurePackage(const packageIndentity : IPackageIdentity): Boolean;

    function GetPackageInfo(const packageIdentity : IPackageIdentity) : IPackageInfo;

    function GetPackageMetadata(const packageIdentity : IPackageIdentity) : IPackageMetadata;

    function InstallPackage(const packageIndentity : IPackageIdentity; const saveFile : boolean; const source : string = '' ) : boolean;

    function InstallPackageFromFile(const packageFileName : string; const saveFile : boolean) : boolean;

  public
    constructor Create(const logger : ILogger; const specReader : IPackageSpecReader);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Zip,
  System.RegularExpressions,
  DPM.Core.Constants,
  DPM.Core.Package.Metadata,
  DPM.Core.Utils.Strings;

{ TPackageCache }

function TPackageCache.CachePackage(const packageIndentity: IPackageIdentity; const saveFile: Boolean): Boolean;
begin
  result := false;
end;

function TPackageCache.Clean: Boolean;
begin
  result := false;
end;

constructor TPackageCache.Create(const logger: ILogger; const specReader : IPackageSpecReader);
begin
  FLogger := logger;
  FSpecReader := specReader;
end;

function TPackageCache.CreatePackagePath(const packageIndentity : IPackageIdentity): string;
begin
  result := GetPackagePath(packageIndentity);
  if not ForceDirectories(result) then
  begin
    FLogger.Error('Error creating package folder [' + result + ']' );
    exit;
  end;
end;

function TPackageCache.GetLocation: string;
begin
  result := FLocation;
end;

function TPackageCache.GetPackageInfo(const packageIdentity: IPackageIdentity): IPackageInfo;
var
  packageFolder : string;
  metaDataFile : string;
  spec : IPackageSpec;
begin
  result := nil;
  packageFolder := GetPackagePath(packageIdentity);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageMetaFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  spec :=FSpecReader.ReadSpec(metaDataFile);
  if spec = nil then
    exit;
  Result := TPackageInfo.CreateFromSpec(packageIdentity.SourceName, spec);
end;

function TPackageCache.GetPackageMetadata(const packageIdentity: IPackageIdentity): IPackageMetadata;
var
  packageFolder : string;
  metaDataFile : string;
  spec : IPackageSpec;
begin
  result := nil;
  if not EnsurePackage(packageIdentity) then
  begin
    FLogger.Error('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  packageFolder := GetPackagePath(packageIdentity);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageMetaFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  spec :=FSpecReader.ReadSpec(metaDataFile);
  if spec = nil then
    exit;
  Result := TPackageMetadata.CreateFromSpec(packageIdentity.SourceName, spec);
end;

function TPackageCache.GetPackagePath(const packageIndentity : IPackageIdentity): string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(packageIndentity.CompilerVersion) + PathDelim + DPMPlatformToString(packageIndentity.platform) + PathDelim + packageIndentity.Id + PathDelim + packageIndentity.Version.ToStringNoMeta;
end;

function TPackageCache.GetPackagesFolder: string;
begin
//  result := IncludeTrailingPathDelimiter(FLocation);
  result := TPath.GetFullPath(FLocation)
end;

function TPackageCache.EnsurePackage(const packageIndentity : IPackageIdentity): Boolean;
var
  packageFileName : string;
  packagesFolder : string;
begin
  //check if we have a package folder and manifest.
  packageFileName := GetPackagePath(packageIndentity);
  result := DirectoryExists(packageFileName) and FileExists(IncludeTrailingPathDelimiter(packageFileName) + cPackageMetaFile);
  if not result then
  begin
    packagesFolder := GetPackagesFolder;
    //ok, if we still have the file, try install it again.
    packageFileName := IncludeTrailingPathDelimiter(packagesFolder) + packageIndentity.ToString + cPackageFileExt;
    if FileExists(packageFileName) then
      result := InstallPackageFromFile(packageFileName, true)
  end;
end;

function TPackageCache.InstallPackage(const packageIndentity: IPackageIdentity; const saveFile: boolean; const source: string): boolean;
begin
   result := false;
end;

function TPackageCache.InstallPackageFromFile(const packageFileName: string; const saveFile: boolean): boolean;
var
  packageFilePath : string;
  fileName : string;
  packageIndentity : IPackageIdentity;
  packageFolder : string;
begin
  result := false;
//  FLogger.Debug('[PackageCache] installing from file : ' + packageFileName);
  if not FileExists(packageFileName) then
  begin
    FLogger.Error('Package File [' + packageFileName + '] does not exist');
    exit;
  end;
  if ExtractFileExt(packageFileName) <> '.dpkg' then
  begin
    FLogger.Error('Package File [' + packageFileName + '] is not a valid package file.');
    exit;
  end;

  fileName := ChangeFileExt(ExtractFileName(packageFileName),'');

  //if savefile, copy to cache folder.
  if not TPackageIdentity.TryCreateFromString(FLogger,fileName,'',packageIndentity)  then
    exit;

  //creates the folder
  packageFolder := CreatePackagePath(packageIndentity );

  if saveFile then
  begin
    if (not TStringUtils.StartsWith(packageFileName, GetPackagesFolder)) then
    begin
      packageFilePath := GetPackagesFolder + PathDelim + ExtractFileName(packageFileName);
      try
        TFile.Copy(packageFileName,packageFilePath, true);
      except
        on e : Exception do
        begin
          FLogger.Error('Unable to copy file [' + packageFileName + '] to the package cache');
          FLogger.Error(e.Message);
          exit;
        end;
      end;
    end
    else
      packageFilePath := packageFileName;
  end
  else
    packageFilePath := packageFileName;

  //work with packageFilePath now.

  try
    TZipFile.ExtractZipFile(packageFilePath, packageFolder);
    result := FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageMetaFile);
    if result then
      FLogger.Information('Package  [' + packageFilePath + '] added to cache.');

  except
    on e : exception do
    begin
      FLogger.Error('Unable to copy file [' + packageFileName + '] to the package cache');
      FLogger.Error(e.Message);
      exit;
    end;
  end;
end;

procedure TPackageCache.SetLocation(const value : string);
begin
  FLocation := value;
end;

end.
