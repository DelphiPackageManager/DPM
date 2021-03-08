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
  VSoft.Awaitable,
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
    function GetLocation : string;
    function GetPackagesFolder : string;

    function CachePackage(const packageId : IPackageId; const saveFile : Boolean) : Boolean;
    function Clean : Boolean;
    function CreatePackagePath(const packageId : IPackageId) : string;

    function GetPackagePath(const packageId : IPackageId) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : string;overload;

    function EnsurePackage(const packageId : IPackageId) : Boolean;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;

    function GetPackageMetadata(const packageId : IPackageId) : IPackageMetadata;

    function GetPackageSpec(const packageId : IPackageId) : IPackageSpec;


//    function InstallPackage(const packageId : IPackageId; const saveFile : boolean; const source : string = '') : boolean;

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

function TPackageCache.CachePackage(const packageId : IPackageId; const saveFile : Boolean) : Boolean;
begin
  result := false;
end;

function TPackageCache.Clean : Boolean;
begin
  result := false;
end;

constructor TPackageCache.Create(const logger : ILogger; const specReader : IPackageSpecReader);
begin
  FLogger := logger;
  FSpecReader := specReader;
end;

function TPackageCache.CreatePackagePath(const packageId : IPackageId) : string;
begin
  result := GetPackagePath(packageId);
  if not ForceDirectories(result) then
  begin
    FLogger.Error('Error creating package folder [' + result + ']');
    exit;
  end;
end;

function TPackageCache.GetLocation : string;
begin
  result := FLocation;
end;

function TPackageCache.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
var
  packageFolder : string;
  metaDataFile : string;
  spec : IPackageSpec;
begin
  result := nil;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageMetaFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  spec := FSpecReader.ReadSpec(metaDataFile);
  if spec = nil then
    exit;
  Result := TPackageInfo.CreateFromSpec('', spec);
end;

function TPackageCache.GetPackageMetadata(const packageId : IPackageId) : IPackageMetadata;
var
  spec : IPackageSpec;
begin
  spec := GetPackageSpec(packageId);
  if spec = nil then
    exit;
  Result := TPackageMetadata.CreateFromSpec('', spec);
end;

function TPackageCache.GetPackagePath(const id: string; const version: string; const compilerVersion : TCompilerVersion;const platform: TDPMPlatform): string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + DPMPlatformToString(platform) + PathDelim + Id + PathDelim + Version;
end;

function TPackageCache.GetPackagePath(const packageId : IPackageId) : string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim + DPMPlatformToString(packageId.platform) + PathDelim + packageId.Id + PathDelim + packageId.Version.ToStringNoMeta;
end;

function TPackageCache.GetPackagesFolder : string;
begin
  //  result := IncludeTrailingPathDelimiter(FLocation);
  result := TPath.GetFullPath(FLocation)
end;

function TPackageCache.GetPackageSpec(const packageId: IPackageId): IPackageSpec;
var
  packageFolder : string;
  metaDataFile : string;
begin
  result := nil;
  if not EnsurePackage(packageId) then
  begin
    FLogger.Error('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageMetaFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  result := FSpecReader.ReadSpec(metaDataFile);
end;

function TPackageCache.EnsurePackage(const packageId : IPackageId) : Boolean;
var
  packageFileName : string;
  packagesFolder : string;
begin
  //check if we have a package folder and manifest.
  packageFileName := GetPackagePath(packageId);
  result := DirectoryExists(packageFileName) and FileExists(IncludeTrailingPathDelimiter(packageFileName) + cPackageMetaFile);
  if not result then
  begin
    packagesFolder := GetPackagesFolder;
    //ok, if we still have the file, try install it again.
    packageFileName := IncludeTrailingPathDelimiter(packagesFolder) + packageId.ToString + cPackageFileExt;
    if FileExists(packageFileName) then
      result := InstallPackageFromFile(packageFileName, true)
  end;
end;


function TPackageCache.InstallPackageFromFile(const packageFileName : string; const saveFile : boolean) : boolean;
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

  fileName := ChangeFileExt(ExtractFileName(packageFileName), '');

  //if savefile, copy to cache folder.
  if not TPackageIdentity.TryCreateFromString(FLogger, fileName, '', packageIndentity) then
    exit;

  //creates the folder
  packageFolder := CreatePackagePath(packageIndentity);

  if saveFile then
  begin
    if (not TStringUtils.StartsWith(packageFileName, GetPackagesFolder)) then
    begin
      packageFilePath := GetPackagesFolder + PathDelim + ExtractFileName(packageFileName);
      try
        TFile.Copy(packageFileName, packageFilePath, true);
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
      FLogger.Verbose('Package  [' + packageFilePath + '] added to cache.');

  except
    on e : exception do
    begin
       TDirectory.Delete(packageFolder, true); //just empties it but doesn't delete?
       if not RemoveDir(packageFolder) then
       begin
         FLogger.Error('Unable to cleanup directory : ' + packageFolder);
         FLogger.Error(SysErrorMessage(GetLastError));
       end;
      //raising here, if we don't we end up here again!
      raise Exception.Create('Unable to extract file [' + packageFilePath + '] into the package cache : ' + e.Message);
    end;
  end;
end;

procedure TPackageCache.SetLocation(const value : string);
begin
  FLocation := value;
end;

end.

