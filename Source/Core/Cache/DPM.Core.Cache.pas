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
  System.SyncObjs,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Spec.Interfaces;

type
  TPackageCache = class(TInterfacedObject, IPackageCache)
  private
    FLogger : ILogger;
    FSpecReader : IPackageSpecReader;
    FLocation : string;
    //Process-level memo. Safe because the cached spec is write-once per (id, compiler, version).
    //TPackageCache is registered AsSingleton in DPM.Core.Init.pas so these survive the whole command
    //and dedupe across projects in a group restore as well as IDE operations.
    FInfoCache : IDictionary<string, IPackageInfo>;
    FSpecCache : IDictionary<string, IPackageSpec>;
    FExtractionVerified : ISet<string>;
    FCacheLock : TCriticalSection;
  protected
    procedure SetLocation(const value : string);
    function GetLocation : string;
    function GetPackagesFolder : string;

    function CachePackage(const packageId : IPackageIdentity; const saveFile : Boolean) : Boolean;
    function Clean : Boolean;
    function CreatePackagePath(const packageId : IPackageIdentity) : string;

    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;overload;

    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;

    function EnsurePackage(const packageId : IPackageIdentity) : Boolean;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;

    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;

    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;

    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;

    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;

    function GetPackageHash(const packageId : IPackageIdentity) : string;

    function TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;

//    function InstallPackage(const packageId : IPackageIdentity; const saveFile : boolean; const source : string = '') : boolean;

    function InstallPackageFromFile(const packageFileName : string) : boolean;

  public
    constructor Create(const logger : ILogger; const specReader : IPackageSpecReader);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Zip,
  System.RegularExpressions,
  System.Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Icon,
  DPM.Core.Utils.Hash,
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

constructor TPackageCache.Create(const logger : ILogger; const specReader : IPackageSpecReader);
begin
  FLogger := logger;
  FSpecReader := specReader;
  FInfoCache := TCollections.CreateDictionary<string, IPackageInfo>;
  FSpecCache := TCollections.CreateDictionary<string, IPackageSpec>;
  FExtractionVerified := TCollections.CreateSet<string>;
  FCacheLock := TCriticalSection.Create;
end;

destructor TPackageCache.Destroy;
begin
  FCacheLock.Free;
  inherited;
end;

function MakeCacheKey(const packageId : IPackageIdentity) : string;
begin
  result := LowerCase(packageId.Id) + '|' +
            CompilerToString(packageId.CompilerVersion) + '|' +
            packageId.Version.ToStringNoMeta;
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
  spec : IPackageSpec;
  key : string;
begin
  result := nil;
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FInfoCache.TryGetValue(key, result) then
      exit;
  finally
    FCacheLock.Leave;
  end;

  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package dspec file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  spec := FSpecReader.ReadSpec(metaDataFile);
  if spec = nil then
    exit;
  result := TPackageInfo.CreateFromManifest('', spec, '', '');

  FCacheLock.Enter;
  try
    FInfoCache[key] := result;
  finally
    FCacheLock.Leave;
  end;
end;

function TPackageCache.GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
var
  spec : IPackageSpec;
begin
  spec := GetPackageSpec(packageId);
  if spec = nil then
    exit;
  result := TPackageMetadata.CreateFromManifest('', spec);
end;

function TPackageCache.GetPackagePath(const id: string; const version: string; const compilerVersion : TCompilerVersion): string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + Id + PathDelim + Version;
end;

function TPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim +  packageId.Id + PathDelim + packageId.Version.ToStringNoMeta;
end;

function TPackageCache.GetPackageFileFolder(const packageId : IPackageIdentity) : string;
begin
  //{cache}/{compiler}/{id} - parent of the per-version extraction folders. Raw .dpkg files
  //for all versions of this package id live here side-by-side with the versioned subfolders.
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim + packageId.Id;
end;

function TPackageCache.GetPackagesFolder : string;
begin
  //  result := IncludeTrailingPathDelimiter(FLocation);
  result := TPath.GetFullPath(FLocation)
end;

function TPackageCache.GetPackageSpec(const packageId: IPackageIdentity): IPackageSpec;
var
  packageFolder : string;
  metaDataFile : string;
  key : string;
begin
  result := nil;
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FSpecCache.TryGetValue(key, result) then
      exit;
  finally
    FCacheLock.Leave;
  end;

  if not EnsurePackage(packageId) then
  begin
    FLogger.Error('Package dspec file [' + packageId.ToString + '] not found in cache.');
    exit;
  end;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package dspec file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  result := FSpecReader.ReadSpec(metaDataFile);
  if result = nil then
    exit;

  FCacheLock.Enter;
  try
    FSpecCache[key] := result;
  finally
    FCacheLock.Leave;
  end;
end;

function TPackageCache.GetPackagePlatforms(const packageId: IPackageIdentity): TDPMPlatforms;
var
  spec: IPackageSpec;
begin
  result := [];
  spec := GetPackageSpec(packageId);
  if (spec <> nil) and (spec.TargetPlatform <> nil) then
    result := spec.TargetPlatform.Platforms;
end;

function TPackageCache.GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                                const id : string;
                                                                const compilerVersion : TCompilerVersion;
                                                                const versionRange : TVersionRange;
                                                                const preRelease : boolean) : IList<IPackageInfo>;
var
  packageFileFolder : string;
  versionFolders : TStringDynArray;
  versionFolder : string;
  versionName : string;
  packageVersion : TPackageVersion;
  packageIdentity : IPackageIdentity;
  packageInfo : IPackageInfo;
begin
  result := TCollections.CreateList<IPackageInfo>;

  //{cache}/{compiler}/{id} - reusing GetPackageFileFolder which returns this path
  //without requiring a version. Sibling folders are the per-version extraction folders.
  packageFileFolder := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + id;
  if not DirectoryExists(packageFileFolder) then
    exit;

  versionFolders := TDirectory.GetDirectories(packageFileFolder, '*', TSearchOption.soTopDirectoryOnly);
  for versionFolder in versionFolders do
  begin
    versionName := ExtractFileName(ExcludeTrailingPathDelimiter(versionFolder));
    if not TPackageVersion.TryParse(versionName, packageVersion) then
      continue;
    if not versionRange.IsSatisfiedBy(packageVersion) then
      continue;
    if (not preRelease) and (not packageVersion.IsStable) then
      continue;

    packageIdentity := TPackageIdentity.Create('', id, packageVersion, compilerVersion);
    packageInfo := GetPackageInfo(cancellationToken, packageIdentity);
    if packageInfo <> nil then
      result.Add(packageInfo);
  end;

  //sort descending by version - matches the order produced by TPackageRepositoryManager
  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := Right.Version.CompareTo(Left.Version);
    end));
end;

function TPackageCache.GetPackageHash(const packageId : IPackageIdentity) : string;
var
  folder : string;
  sidecarPattern : string;
  dpkgPattern : string;
  matches : TStringDynArray;
  sidecarPath : string;
  dpkgPath : string;
  lines : TStringList;
begin
  result := '';
  if packageId = nil then
    exit;
  folder := GetPackageFileFolder(packageId);
  if not DirectoryExists(folder) then
    exit;

  //.dpkg filenames follow {id}-{compiler}-{binPlatforms}-{version}.dpkg - the
  //binPlatforms segment is opaque from this side so we wildcard it. The .sha256
  //sidecar shares the .dpkg basename plus the .sha256 extension.
  sidecarPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) + '-*-'
                  + packageId.Version.ToStringNoMeta + cPackageFileExt + cPackageHashAlgorithmExt;
  try
    matches := TDirectory.GetFiles(folder, sidecarPattern, TSearchOption.soTopDirectoryOnly);
  except
    SetLength(matches, 0);
  end;

  if Length(matches) > 0 then
  begin
    sidecarPath := matches[0];
    lines := TStringList.Create;
    try
      try
        lines.LoadFromFile(sidecarPath);
        if lines.Count > 0 then
          result := Trim(lines[0]);
      except
        on e : Exception do
          FLogger.Debug('[Cache] could not read hash sidecar [' + sidecarPath + '] : ' + e.Message);
      end;
    finally
      lines.Free;
    end;
    if result <> '' then
      exit;
  end;

  //Sidecar missing or unreadable - compute from the .dpkg and persist for
  //next time (mirrors TDirectoryPackageRepository.DoGetPackageMetaData's
  //self-heal). Other tooling - SBOM, vulnerability scanners, signature
  //verification - can then assume the sidecar is present.
  dpkgPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) + '-*-'
              + packageId.Version.ToStringNoMeta + cPackageFileExt;
  try
    matches := TDirectory.GetFiles(folder, dpkgPattern, TSearchOption.soTopDirectoryOnly);
  except
    SetLength(matches, 0);
  end;
  if Length(matches) = 0 then
    exit;
  dpkgPath := matches[0];

  try
    result := THashSHA256.GetHashStringFromFile(dpkgPath);
  except
    on e : Exception do
    begin
      FLogger.Debug('[Cache] could not hash .dpkg [' + dpkgPath + '] : ' + e.Message);
      result := '';
    end;
  end;
  if result = '' then
    exit;

  //Persist the sidecar. Best-effort: if the cache directory is somehow
  //read-only we still return the computed hash, we just won't have cached
  //it for the next call.
  try
    TFile.WriteAllText(dpkgPath + cPackageHashAlgorithmExt, result);
  except
    on e : Exception do
      FLogger.Debug('[Cache] could not write hash sidecar [' + dpkgPath + cPackageHashAlgorithmExt + '] : ' + e.Message);
  end;
end;

function TPackageCache.TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
var
  packageFolder : string;
  svgPath : string;
  pngPath : string;
  fileStream : TFileStream;
  stream : TMemoryStream;
begin
  result := false;
  icon := nil;
  if packageId = nil then
    exit;
  //EnsurePackage extracts a stray .dpkg if the folder is missing - cheap no-op once a package
  //has been installed by either the CLI or the IDE.
  if not EnsurePackage(packageId) then
    exit;
  packageFolder := GetPackagePath(packageId);
  svgPath := IncludeTrailingPathDelimiter(packageFolder) + cIconFileSVG;
  pngPath := IncludeTrailingPathDelimiter(packageFolder) + cIconFilePNG;

  if FileExists(svgPath) then
  begin
    fileStream := TFileStream.Create(svgPath, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    //icon now owns the stream.
    icon := CreatePackageIcon(TPackageIconKind.ikSvg, stream);
    result := true;
    exit;
  end;

  if FileExists(pngPath) then
  begin
    fileStream := TFileStream.Create(pngPath, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    icon := CreatePackageIcon(TPackageIconKind.ikPng, stream);
    result := true;
  end;
end;

function TPackageCache.EnsurePackage(const packageId : IPackageIdentity) : Boolean;
var
  packageFolder : string;
  packageFileFolder : string;
  dspecFile : string;
  searchPattern : string;
  matchingFiles : TStringDynArray;
  key : string;
begin
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FExtractionVerified.Contains(key) then
    begin
      result := true;
      exit;
    end;
  finally
    FCacheLock.Leave;
  end;

  //check if we have a package folder and dspec.
  packageFolder := GetPackagePath(packageId);
  result := DirectoryExists(packageFolder);

  dspecFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;

  result := result and FileExists(dspecFile);
  if not result then
  begin
    //fallback: the extracted folder is missing or has no dspec, but the .dpkg may still be
    //on disk from a previous session. On-disk filename is
    //{Id}-{Compiler}-{BinPlatforms}-{Version}.dpkg - BinPlatforms is a DPMPlatformsToBinString
    //bitmask encoding the platforms this .dpkg was packed for. packageId only tells us
    //Id/Compiler/Version so we wildcard the platforms segment.
    packageFileFolder := GetPackageFileFolder(packageId);
    if DirectoryExists(packageFileFolder) then
    begin
      searchPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) +
        '-*-' + packageId.Version.ToStringNoMeta + cPackageFileExt;
      matchingFiles := TDirectory.GetFiles(packageFileFolder, searchPattern, TSearchOption.soTopDirectoryOnly);
      if Length(matchingFiles) > 0 then
        result := InstallPackageFromFile(matchingFiles[0]);
    end;
  end;

  if result then
  begin
    FCacheLock.Enter;
    try
      FExtractionVerified.Add(key);
    finally
      FCacheLock.Leave;
    end;
  end;
end;


function TPackageCache.InstallPackageFromFile(const packageFileName : string) : boolean;
var
  packageFilePath : string;
  fileName : string;
  packageIndentity : IPackageIdentity;
  packageFolder : string;
  packageFileFolder : string;
  key : string;
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

  if not TPackageIdentity.TryCreateFromString(FLogger, fileName, '', packageIndentity) then
    exit;

  //defensive: about to re-extract this id-compiler-version, invalidate any memoised state
  //so the next read picks up the fresh dspec. Contents should be identical for the same
  //(id, compiler, version), but this future-proofs against replacing a corrupt extraction.
  key := MakeCacheKey(packageIndentity);
  FCacheLock.Enter;
  try
    FInfoCache.Remove(key);
    FSpecCache.Remove(key);
    FExtractionVerified.Remove(key);
  finally
    FCacheLock.Leave;
  end;

  //creates the per-version extraction folder
  packageFolder := CreatePackagePath(packageIndentity);
  FLogger.Debug('[PackageCache] PackageFolder  : ' + packageFolder);

  //.dpkg lives in the package-id folder (sibling of the per-version folders) so multiple versions
  //of the same id share a folder rather than cluttering the cache root.
  packageFileFolder := GetPackageFileFolder(packageIndentity);
  if not ForceDirectories(packageFileFolder) then
  begin
    FLogger.Error('Unable to create package file folder [' + packageFileFolder + ']');
    exit;
  end;

  //if the source file isn't already in its canonical cache location, copy it there.
  packageFilePath := IncludeTrailingPathDelimiter(packageFileFolder) + ExtractFileName(packageFileName);
  if not SameText(packageFileName, packageFilePath) then
  begin
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
  end;

  //work with packageFilePath now.

  try
    FLogger.Debug('[PackageCache] Extracting Package file [' + packageFilePath + '] to : ' + packageFolder);
    TZipFile.ExtractZipFile(packageFilePath, packageFolder);
    result := FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile);
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

