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

unit DPM.Core.Cache;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Spec.Interfaces;

type
  TPackageCache = class(TInterfacedObject, IPackageCache)
  private
    FLogger : ILogger;
    FManifestReader : IPackageSpecReader;
    FLocation : string;
  protected
    procedure SetLocation(const value : string);
    function GetLocation : string;
    function GetPackagesFolder : string;

    function CachePackage(const packageId : IPackageIdentity; const saveFile : Boolean) : Boolean;
    function Clean : Boolean;
    function CreatePackagePath(const packageId : IPackageIdentity) : string;

    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;overload;

    function EnsurePackage(const packageId : IPackageIdentity) : Boolean;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;

    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;

    function GetPackageManifest(const packageId : IPackageIdentity) : IPackageSpec;

    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;

//    function InstallPackage(const packageId : IPackageIdentity; const saveFile : boolean; const source : string = '') : boolean;

    function InstallPackageFromFile(const packageFileName : string) : boolean;

  public
    constructor Create(const logger : ILogger; const manifestReader : IPackageSpecReader);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Zip,
  System.RegularExpressions,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Strings;

{ TPackageCache }

function TPackageCache.CachePackage(const packageId : IPackageIdentity; const saveFile : Boolean) : Boolean;
begin
  result := false;
end;

function TPackageCache.Clean : Boolean;
begin
  result := false;
end;

constructor TPackageCache.Create(const logger : ILogger; const manifestReader : IPackageSpecReader);
begin
  FLogger := logger;
  FManifestReader := manifestReader;
end;

function TPackageCache.CreatePackagePath(const packageId : IPackageIdentity) : string;
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

function TPackageCache.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  packageFolder : string;
  metaDataFile : string;
  manifest : IPackageSpec;
begin
  result := nil;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageManifestFile;
  if not FileExists(metaDataFile) then
  begin
    metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cOldPackageManifestFile;
    if not FileExists(metaDataFile) then
    begin
      FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
      exit;
    end;
  end;
  manifest := FManifestReader.ReadSpec(metaDataFile);
  if manifest = nil then
    exit;
  Result := TPackageInfo.CreateFromManifest('', manifest, '', '');
end;

function TPackageCache.GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
var
  manifest : IPackageSpec;
begin
  manifest := GetPackageManifest(packageId);
  if manifest = nil then
    exit;
  Result := TPackageMetadata.CreateFromManifest('', manifest);
end;

function TPackageCache.GetPackagePath(const id: string; const version: string; const compilerVersion : TCompilerVersion): string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + Id + PathDelim + Version;
end;

function TPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim +  packageId.Id + PathDelim + packageId.Version.ToStringNoMeta;
end;

function TPackageCache.GetPackagesFolder : string;
begin
  //  result := IncludeTrailingPathDelimiter(FLocation);
  result := TPath.GetFullPath(FLocation)
end;

function TPackageCache.GetPackageManifest(const packageId: IPackageIdentity): IPackageSpec;
var
  packageFolder : string;
  metaDataFile : string;
begin
  result := nil;
  if not EnsurePackage(packageId) then
  begin
    FLogger.Error('Package metadata file [' + packageId.ToString + '] not found in cache.');
    exit;
  end;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageManifestFile;
  if not FileExists(metaDataFile) then
  begin
    metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cOldPackageManifestFile;
    if not FileExists(metaDataFile) then
    begin
      FLogger.Debug('Package metadata file [' + metaDataFile + '] not found in cache.');
      exit;
    end;
  end;
  result := FManifestReader.ReadSpec(metaDataFile);
end;

function TPackageCache.GetPackagePlatforms(const packageId: IPackageIdentity): TDPMPlatforms;
var
  manifest: IPackageSpec;
begin
  result := [];
  manifest := GetPackageManifest(packageId);
  if (manifest <> nil) and (manifest.TargetPlatform <> nil) then
    result := manifest.TargetPlatform.Platforms;
end;

function TPackageCache.EnsurePackage(const packageId : IPackageIdentity) : Boolean;
var
  packageFolder : string;
  packagesFolder : string;
  manifestFile : string;
  oldManifestFile : string;
  searchPattern : string;
  matchingFiles : TStringDynArray;
begin
  //check if we have a package folder and manifest.
  packageFolder := GetPackagePath(packageId);
  result := DirectoryExists(packageFolder);

  manifestFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageManifestFile;
  oldManifestFile := IncludeTrailingPathDelimiter(packageFolder) + cOldPackageManifestFile;

  result := result and (FileExists(manifestFile) or FileExists(oldManifestFile));
  if not result then
  begin
    packagesFolder := GetPackagesFolder;
    //if a stray .dpkg is sitting in the cache root, try to install it again.
    //The on-disk filename has 4 segments: {Id}-{Compiler}-{BinPlatforms}-{Version}.dpkg.
    //packageId.ToString produces only 3 segments, so we glob for the actual file.
    searchPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) + '-*-' + packageId.Version.ToStringNoMeta + cPackageFileExt;
    if DirectoryExists(packagesFolder) then
    begin
      matchingFiles := TDirectory.GetFiles(packagesFolder, searchPattern, TSearchOption.soTopDirectoryOnly);
      if Length(matchingFiles) > 0 then
        result := InstallPackageFromFile(matchingFiles[0]);
    end;
  end;
end;


function TPackageCache.InstallPackageFromFile(const packageFileName : string) : boolean;
var
  packageFilePath : string;
  fileName : string;
  packageIndentity : IPackageIdentity;
  packageFolder : string;
begin
  result := false;
  FLogger.Debug('[PackageCache] installing from file : ' + packageFileName);
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
  FLogger.Debug('[PackageCache] PackageFolder  : ' + packageFolder);

  if (not TStringUtils.StartsWith(packageFileName, GetPackagesFolder)) then
  begin
    packageFilePath := GetPackagesFolder + PathDelim + ExtractFileName(packageFileName);
    FLogger.Debug('[PackageCache] Copying Package file to   : ' + packageFilePath);

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

  //work with packageFilePath now.

  try
    FLogger.Debug('[PackageCache] Extracting Package file [' + packageFilePath + '] to : ' + packageFolder);
    TZipFile.ExtractZipFile(packageFilePath, packageFolder);
    result := FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageManifestFile);
    if not result then
      result := FileExists(IncludeTrailingPathDelimiter(packageFolder) + cOldPackageManifestFile);
    if result then
      FLogger.Verbose('Package  [' + packageFilePath + '] added to cache.');

  except
    on e : exception do
    begin
       FLogger.Debug('[PackageCache] Error Extracting Package file : ' + e.Message);
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

