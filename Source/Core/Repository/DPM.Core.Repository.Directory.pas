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

unit DPM.Core.Repository.Directory;

interface

uses
  Generics.Defaults,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base,
  DPM.Core.Repository.Directory.Index;

type
  TReadManifestFunc = reference to function(const name : string; const manifest : IPackageSpec; const fileHash :string; const hashAlgorithm : string) : IInterface;

  TDirectoryPackageRepository = class(TBaseRepository, IPackageRepository)
  private
    FPermissionsChecked : boolean;
    FIsWritable : boolean;
    FIndexEnabled : boolean;
    FIndexFolder : string;
    FIndexes : IDictionary<TCompilerVersion, IFolderSourceIndex>;
    procedure SetIndexFolder(const value : string);
    function ReadSpecSidecar(const reader : IPackageSpecReader; const fileName : string; out specText : string) : IPackageSpec;
  protected
    procedure Configure(const source : ISourceConfig); override;
    procedure CheckWritable(const folder : string);

    //What this source's package sidecars held, for compilerVersion (see DPM.Core.Repository.Directory.Index) - nil
    //when the index is turned off.
    function GetIndex(const compilerVersion : TCompilerVersion) : IFolderSourceIndex;
    //The reads of the share the index saves - virtual so a test can count them.
    function ReadHashFile(const fileName : string) : string; virtual;
    function ReadSpecFile(const fileName : string) : string; virtual;
    function HashPackageFile(const fileName : string) : string; virtual;

    //listing and index are given only by the reads that are shown (they ask for the hash) : the index is used when
    //listing saw the package, and nothing else ever reaches it.
    function DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string; readHash : boolean; const readManifestFunc : TReadManifestFunc;
                                  const listing : IFolderListing = nil; const index : IFolderSourceIndex = nil) : IInterface;
    //GetPackageMetaData without saving the index - for callers asking about several packages, who save once.
    function DoGetPackageMetaDataForDisplay(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string;
                                            const compilerVersion : TCompilerVersion; const index : IFolderSourceIndex) : IPackageSearchResultItem;


    function DoList(const searchTerm : string; const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms) : IList<string>;
    function DoExactList(const id : string; const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms; const version : string) : IList<string>;

    function DoGetPackageFeedFiles(const searchTerm : string; const exact : boolean; const compilerVersion : TCompilerVersion) : IList<string>;
    function DoGetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const files : IList<string>;
                              const listing : IFolderListing; const index : IFolderSourceIndex) : IList<IPackageSearchResultItem>;


    function DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;

    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;


    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo; overload;

//    function GetPackageLatestVersions(const cancellationToken : ICancellationToken; const ids : IList<IPackageIdentity>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IDictionary<string, IPackageLatestVersionInfo>;

    function GetPackageLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion) : IPackageLatestVersionInfo;

    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;


    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;

    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;


    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;


  public
    constructor Create(const logger : ILogger); override;
    //Where the index files go - the per-user folder, unless a test says otherwise.
    property IndexFolder : string read FIndexFolder write SetIndexFolder;
  end;

implementation

uses
  VSoft.Uri,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions,
  System.Zip,
  Spring.Collections.Extensions,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Icon,
  DPM.Core.Spec.Reader,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Directory,
  DPM.Core.Package.SearchResults,
  DPM.Core.Utils.DateTime,
  DPM.Core.Package.ListItem, DPM.Core.Package.PackageLatestVersionInfo, DPM.Core.Utils.Hash,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.System;

{ TDirectoryPackageRespository }

//best-effort published date for directory sources: the .dpkg file's last-write
//time, formatted the same way as the http feed. blank if it can't be read.
function GetFilePublishedDate(const fileName : string) : string;
var
  fileDate : TDateTime;
begin
  result := '';
  try
    fileDate := TFile.GetLastWriteTime(fileName);
    result := TDPMDateTimeUtils.FormatPublishedDate(fileDate);
  except
    on Exception do
      result := '';
  end;
end;

function GetSearchRegex(const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms; const version : string) : string;
begin
  // ^(\w+\.\w+)\-([^\-]+)\-([^\-]+)\-(.*)$';
  result := '^((?:\w+)(?:\.\w+)+)\-';
  if (compilerVersion > TCompilerVersion.UnknownVersion) then
    result := result + Format('(%s)\-', [CompilerToString(compilerVersion)])
  else
    result := result + '([^\-]+)\-';

  if platforms <> [] then
    result := result + Format('(%s)\-', [DPMPlatformsToBinString(platforms)])
  else
    result := result + '([^\-]+)\-';

  if version <> '' then
    result := result + Format('(%s)$', [version])
  else
    result := result + '(.*)$';

end;

function CompilerVersionToSearchPart(const compilerVersion : TCompilerVersion) : string;
begin
  case compilerVersion of
    TCompilerVersion.UnknownVersion : result := '*';
  else
    result := CompilerToString(compilerVersion);
  end;
end;

//What a search for searchTerm lists the folder with, up to the file's extension. Ending there, it also matches the
//sidecars beside each package, which share its name.
function FeedPattern(const searchTerm : string; const exact : boolean; const compilerVersion : TCompilerVersion) : string;
begin
  result := searchTerm;
  if (result <> '*') and (not exact) then
    result := '*' + result + '*';
  result := result + '-' + CompilerVersionToSearchPart(compilerVersion) + '-' + '*';
end;

//What a package's files are listed with : {id}-{compiler}-{platforms}-{version}, then extension.
function ExactListPattern(const id : string; const compilerVersion : TCompilerVersion; const version : string; const extension : string) : string;
begin
  result := id + '-' + CompilerVersionToSearchPart(compilerVersion) + '-*';
  if version <> '' then
    result := result + '-' + version + extension
  else
    result := result + '-*' + extension;
end;




procedure TDirectoryPackageRepository.CheckWritable(const folder : string);
begin
  if not FPermissionsChecked then
  begin
    FIsWritable := TDirectoryUtils.IsDirectoryWriteable(folder);
    FPermissionsChecked := true;
  end;
end;

constructor TDirectoryPackageRepository.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FPermissionsChecked := false;
  FIsWritable := false;
  FIndexEnabled := FolderIndexEnabled;
  FIndexFolder := TSystemUtils.ExpandEnvironmentStrings(cDefaultFolderIndexFolder);
  FIndexes := TCollections.CreateDictionary<TCompilerVersion, IFolderSourceIndex>;
end;

procedure TDirectoryPackageRepository.Configure(const source : ISourceConfig);
begin
  inherited Configure(source);
  //an index is of one folder.
  EnterLock;
  try
    FIndexes.Clear;
  finally
    LeaveLock;
  end;
end;

procedure TDirectoryPackageRepository.SetIndexFolder(const value : string);
begin
  EnterLock;
  try
    FIndexFolder := value;
    FIndexes.Clear;
  finally
    LeaveLock;
  end;
end;

function TDirectoryPackageRepository.GetIndex(const compilerVersion : TCompilerVersion) : IFolderSourceIndex;
begin
  result := nil;
  if (not FIndexEnabled) or (compilerVersion = TCompilerVersion.UnknownVersion) or (SourceUri = '') then
    exit;
  EnterLock;
  try
    if FIndexFolder = '' then
      exit;
    if not FIndexes.TryGetValue(compilerVersion, result) then
    begin
      result := TFolderSourceIndex.Create(Logger, FolderIndexFileName(FIndexFolder, SourceUri, compilerVersion), SourceUri, compilerVersion);
      FIndexes[compilerVersion] := result;
    end;
  finally
    LeaveLock;
  end;
end;

function TDirectoryPackageRepository.ReadHashFile(const fileName : string) : string;
begin
  result := Trim(TFile.ReadAllText(fileName));
end;

function TDirectoryPackageRepository.HashPackageFile(const fileName : string) : string;
begin
  result := THashSHA256.GetHashStringFromFile(fileName);
end;

//The text of the spec sidecar, decoded as TYAML.LoadFromFile (which ReadSpec uses) decodes it : by its BOM, or as
//UTF-8 without one.
function TDirectoryPackageRepository.ReadSpecFile(const fileName : string) : string;
var
  bytes : TBytes;
  encoding : TEncoding;
  start : integer;
begin
  bytes := TFile.ReadAllBytes(fileName);
  encoding := nil;
  start := TEncoding.GetBufferEncoding(bytes, encoding);
  if start = 0 then
    encoding := TEncoding.UTF8;
  result := encoding.GetString(bytes, start, Length(bytes) - start);
end;

//The spec sidecar, read as ReadSpec reads it, and the text it held.
function TDirectoryPackageRepository.ReadSpecSidecar(const reader : IPackageSpecReader; const fileName : string; out specText : string) : IPackageSpec;
begin
  result := nil;
  specText := '';
  try
    specText := ReadSpecFile(fileName);
  except
    on e : Exception do
    begin
      Logger.Error(TErrorCode.E6002_SpecUnreadable, 'Unable to read the spec [' + fileName + '] : ' + e.Message);
      exit;
    end;
  end;
  result := reader.ReadSpecString(specText, fileName);
end;

function TDirectoryPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
var
  matchingFiles : IList<string>;
  sourceFile : string;
  sourceHashFile : string;
  destFile : string;
  destHashFile : string;
  tempFile : string;
  sourceHash : string;
  copyHash : string;
  lastError : Cardinal;
begin
  result := false;
  //On-disk filename: {Id}-{Compiler}-{BinPlatforms}-{Version}.dpkg. BinPlatforms encodes the
  //set of platforms the package was packed for; packageInfo identifies Id/Compiler/Version
  //only, so we wildcard the platforms segment via DoExactList.
  matchingFiles := DoExactList(packageInfo.Id, packageInfo.CompilerVersion, [], packageInfo.Version.ToStringNoMeta);
  if matchingFiles.Count = 0 then
  begin
    Logger.Debug('Package not found in repository [' + packageInfo.ToString + ']');
    exit;
  end;
  if matchingFiles.Count > 1 then
    Logger.Warning(TWarningCode.W1206_MultiplePackageFilesMatched, 'More than one package file in [' + Name + '] matched [' + packageInfo.ToString + '] - using the first.');

  sourceFile := matchingFiles[0];
  destFile := IncludeTrailingPathDelimiter(localFolder) + ExtractFileName(sourceFile);
  if TPathUtils.IsRelativePath(destFile) then
    destFile := TPath.GetFullPath(destFile);
  //The copy is what gets checked - the file in the folder can change while it is read - and it only becomes
  //the cached package once it matches, so a bad copy never replaces a good cached one.
  tempFile := TFileUtils.TempFileNameFor(destFile);
  try
    try
      ForceDirectories(ExtractFilePath(destFile));
      Logger.Information('GET ' + sourceFile);
      TFile.Copy(sourceFile, tempFile, true);
      copyHash := THashSHA256.GetHashStringFromFile(tempFile);
      sourceHashFile := sourceFile + cPackageHashAlgorithmExt;

      //if a package hash file exists, verify the package file hasn't changed.
      if FileExists(sourceHashFile) then
      begin
        sourceHash := Trim(TFile.ReadAllText(sourceHashFile));
        //the file has changed since it was pushed - don't use it.
        if not SameText(sourceHash, copyHash) then
        begin
          RequestFailed('has a package file [' + sourceFile + '] that does not match its .sha256 file - it has been modified since it was pushed. ' +
                        'If that was intentional, delete the .sha256 file.', TErrorCode.E1202_PackageHashMismatch);
          exit;
        end;
      end;
      //the hash the package was listed with - it changed after it was found.
      if (packageInfo.Hash <> '') and (not SameText(packageInfo.Hash, copyHash)) then
      begin
        RequestFailed('has a package file [' + sourceFile + '] that has changed since it was listed (sha256 ' + copyHash + ', expected ' +
                      packageInfo.Hash + ').', TErrorCode.E1202_PackageHashMismatch);
        exit;
      end;

      if not TFileUtils.TryReplaceFile(tempFile, destFile, lastError) then
      begin
        RequestFailed('could not copy [' + sourceFile + '] to [' + destFile + '] : ' + SysErrorMessage(lastError), TErrorCode.E3008_CacheWriteFailed);
        exit;
      end;
      fileName := destFile;

      destHashFile := destFile + cPackageHashAlgorithmExt;
      TFileUtils.AtomicWriteAllText(destHashFile, copyHash, TEncoding.ASCII);

      result := true;
      Logger.Information('OK ' + sourceFile);
    except
      on e : Exception do
      begin
        RequestFailed('could not copy [' + sourceFile + '] to [' + destFile + '] : ' + e.Message);
      end;
    end;
  finally
    if FileExists(tempFile) then
      System.SysUtils.DeleteFile(tempFile);
  end;
end;



function TDirectoryPackageRepository.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const includePrerelease : boolean): IPackageInfo;
var
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  i : integer;
  maxVersion : TPackageVersion;
  packageVersion : TPackageVersion;
  maxVersionFile : string;
  readManifestFunc : TReadManifestFunc;
begin
  result := nil;
  if version.IsEmpty then
    searchFiles := DoExactList(id, compilerVersion, [], '')
  else
    searchFiles := DoExactList(id, compilerVersion, [], version.ToStringNoMeta);

  searchRegEx := GetSearchRegex(compilerVersion, [], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);
  maxVersion := TPackageVersion.Empty;
  for i := 0 to searchFiles.Count - 1 do
  begin
    packageFile := searchFiles[i];
    //ensure that the files returned are actually packages
    packageFile := ChangeFileExt(ExtractFileName(packageFile), '');
    match := regex.Match(packageFile);
    if match.Success then
    begin
      if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
        continue;

      if (not includePrerelease) and (not packageVersion.IsStable)  then
        continue;
      //if we wanted a specific version, then we have found it.
      if not version.IsEmpty then
      begin
        maxVersionFile := packageFile;
        maxVersion := version;
        Break;
      end;


      if packageVersion > maxVersion then
      begin
        maxVersion := packageVersion;
        maxVersionFile := packageFile;
      end;
    end;
  end;

  if maxVersion.IsEmpty then
    exit;
  packageFile := IncludeTrailingPathDelimiter(SourceUri) + maxVersionFile + cPackageFileExt;

  //Read manifest from the package so dependencies are populated - the resolver needs them.
  readManifestFunc := function (const name : string; const manifest : IPackageSpec; const fileHash : string; const hashAlgorithm : string) : IInterface
                  begin
                    result := TPackageInfo.CreateFromManifest(name, manifest, fileHash, hashAlgorithm);
                  end;

  result := DoGetPackageMetaData(cancellationToken, packageFile, false, readManifestFunc) as IPackageInfo;
end;

//The icon in fileName. The icon owns the stream; a stream that never reaches it is freed here.
function ReadIconFile(const iconKind : TPackageIconKind; const fileName : string) : IPackageIcon;
var
  stream : TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    stream.LoadFromFile(fileName);
  except
    stream.Free;
    raise;
  end;
  result := CreatePackageIcon(iconKind, stream);
end;

//The icon entry index of zipFile, saved beside the package as cacheFileName so the next request reads it from
//there - when it can be : a folder feed is often read only (a share), and the icon is the point, not the copy.
function ReadIconEntry(const logger : ILogger; const zipFile : TZipFile; const index : integer; const iconKind : TPackageIconKind;
                       const cacheFileName : string) : IPackageIcon;
var
  bytes : TBytes;
  stream : TMemoryStream;
begin
  //the TBytes overload - the stream one is broken for STORED entries in some versions (see DPM.Core.Package.Signing).
  zipFile.Read(index, bytes);
  stream := TMemoryStream.Create;
  try
    if Length(bytes) > 0 then
      stream.WriteBuffer(bytes[0], Length(bytes));
  except
    stream.Free;
    raise;
  end;
  try
    stream.SaveToFile(cacheFileName);
  except
    on e : Exception do
      logger.Debug('Unable to save the icon [' + cacheFileName + '] : ' + e.Message);
  end;
  result := CreatePackageIcon(iconKind, stream);
end;

function TDirectoryPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
var
  matchingFiles : IList<string>;
  zipFile : TZipFile;
  zipIdx : integer;
  packagFileName : string;
  svgIconFileName : string;
  pngIconFileName : string;
begin
  result := nil;
  matchingFiles := DoExactList(packageId, compilerVersion, [], packageVersion);
  if matchingFiles.Count = 0 then
    exit;
  packagFileName := matchingFiles[0];
  svgIconFileName := ChangeFileExt(packagFileName, '.svg');
  pngIconFileName := ChangeFileExt(packagFileName, '.png');

  zipFile := TZipFile.Create;
  try
    try
      //it quite likely already extracted so look for that first
      //icons can be svg or png.
      if FileExists(svgIconFileName) then
        exit(ReadIconFile(TPackageIconKind.ikSvg, svgIconFileName));
      if FileExists(pngIconFileName) then
        exit(ReadIconFile(TPackageIconKind.ikPng, pngIconFileName));

      //not already extracted, so check the package
      zipFile.Open(packagFileName, TZipMode.zmRead);
      zipIdx := zipFile.IndexOf(cIconFileSVG);
      if zipIdx <> -1 then
        exit(ReadIconEntry(Logger, zipFile, zipIdx, TPackageIconKind.ikSvg, svgIconFileName));
      if cancelToken.IsCancelled then
        exit;

      zipIdx := zipFile.IndexOf(cIconFilePNG);
      if zipIdx <> -1 then
        exit(ReadIconEntry(Logger, zipFile, zipIdx, TPackageIconKind.ikPng, pngIconFileName));
    except
      on e : Exception do
      begin
        Logger.Warning(TWarningCode.W1207_PackageEntryUnreadable, 'Unable to read the package [' + packagFileName + '] in [' + Name + '] : ' + e.Message);
        //couldn't look, which is not the same as the package having no icon - see IPackageRepository.GetPackageIcon.
        RequestFailed('could not read the package [' + packagFileName + '] : ' + e.Message);
        exit;
      end;
    end;
  finally
    zipFile.Free;
  end;
end;

function TDirectoryPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  matchingFiles : IList<string>;
  packageFileName : string;
  readManifestFunc : TReadManifestFunc;
begin
  result := nil;
  //The on-disk filename has 4 segments - glob with the platform wildcard.
  matchingFiles := DoExactList(packageId.Id, packageId.CompilerVersion, [], packageId.Version.ToStringNoMeta);
  if matchingFiles.Count = 0 then
    exit;
  packageFileName := matchingFiles[0];

  if cancellationToken.IsCancelled then
    exit;

  readManifestFunc := function (const name : string; const manifest : IPackageSpec; const fileHash :string; const hashAlgorithm : string) : IInterface
                  begin
                    result := TPackageInfo.CreateFromManifest(name, manifest, fileHash, hashAlgorithm);
                  end;

  result := DoGetPackageMetaData(cancellationToken, packageFileName, false, readManifestFunc) as IPackageInfo;
end;


function TDirectoryPackageRepository.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
var
  searchTerms : TArray<string>;
  i : integer;
  results : IList<IPackageSearchResultItem>;
  searchFiles : IList<string>;
  allFiles : IList<string>;
  distinctFiles : IEnumerable<string>;
  first : integer;
  index : IFolderSourceIndex;
  listing : IFolderListing;
  patterns : TArray<string>;
  fullListing : boolean;
begin
  Logger.Debug('TDirectoryPackageRepository.GetPackageFeed');
  result := TDPMPackageSearchResult.Create(options.Skip,0);

  if options.CompilerVersion = TCompilerVersion.UnknownVersion then
    raise EArgumentException.Create('Compiler version must be set');

  if options.SearchTerms <> '' then
    searchTerms := TStringUtils.SplitStr(Trim(options.SearchTerms), ' ');
  //need at least 1 search term, if there are none then search for *
  if length(searchTerms) = 0 then
  begin
    SetLength(searchTerms, 1);
    searchTerms[0] := '*';
  end;

  index := GetIndex(compilerVersion);
  listing := nil;
  fullListing := false;
  if index <> nil then
  begin
    //one pass per term that also sees each package's sidecars, for the index.
    SetLength(patterns, Length(searchTerms));
    for i := 0 to Length(searchTerms) - 1 do
    begin
      patterns[i] := FeedPattern(searchTerms[i], options.Exact, compilerVersion);
      //every package of the compiler is listed.
      if searchTerms[i] = '*' then
        fullListing := true;
    end;
    if cancelToken.IsCancelled then
      exit;
    listing := ListFolder(SourceUri, patterns, cPackageFileExt);
    searchFiles := listing.PackageFiles;
  end
  else
  begin
    allFiles := TCollections.CreateList < string > ;
    for i := 0 to Length(searchTerms) - 1 do
    begin
      if cancelToken.IsCancelled then
        exit;
      allFiles.AddRange(DoGetPackageFeedFiles(searchTerms[i], options.Exact, compilerVersion));
    end;

    distinctFiles := TDistinctIterator<string>.Create(allFiles, TStringComparer.OrdinalIgnoreCase);
    searchFiles := TCollections.CreateList<string>(distinctFiles);
  end;

  //a package per id, from all of its versions' files - the page is of those, in id order so it is the same page
  //each time.
  try
    results := DoGetPackageFeed(cancelToken, options, searchFiles, listing, index);
    //the entries of packages no longer in the folder go - only a listing of all of them can say which.
    if fullListing then
      index.Prune(listing);
  finally
    if index <> nil then
      index.Save;
  end;
  results.Sort(
    function(const Left, Right : IPackageSearchResultItem) : integer
    begin
      result := CompareText(Left.Id, Right.Id);
    end);
  result.TotalCount := results.Count;
  first := options.Skip;
  if first < 0 then
    first := 0;
  for i := first to results.Count - 1 do
  begin
    if (options.Take > 0) and (result.Results.Count >= options.Take) then
      break;
    result.Results.Add(results[i]);
  end;
end;


function TDirectoryPackageRepository.GetPackageLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion ) : IPackageLatestVersionInfo;
var
  i : integer;
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  packageVersion : TPackageVersion;
  latestVersion  : TPackageVersion;
  latestStableVersion : TPackageVersion;
begin
  result := nil;
  searchRegEx := GetSearchRegex(compilerVersion, [], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);

  searchFiles := DoExactList(id, compilerVersion, [], '');
  latestVersion := TPackageVersion.Empty;
  latestStableVersion := TPackageVersion.Empty;
  for i := 0 to searchFiles.Count -1 do
  begin
    packageFile := ChangeFileExt(ExtractFileName(searchFiles[i]), '');
    match := regex.Match(packageFile);
    if match.Success then
    begin
      if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
        continue;

      if packageVersion > latestVersion then
        latestVersion := packageVersion;

      if packageVersion.IsStable and (packageVersion > latestStableVersion) then
        latestStableVersion := packageVersion;
    end;
    //if latestVersion is empt then we didn't find any files!
  end;
  if not latestVersion.IsEmpty then
    result:= TDPMPackageLatestVersionInfo.Create(id,latestStableVersion, latestVersion);
end;

function TDirectoryPackageRepository.GetPackageFeedByIds(const cancellationToken: ICancellationToken; const ids: IList<IPackageIdentity>; const compilerVersion: TCompilerVersion): IPackageSearchResult;
var
  item : IPackageIdentity;
  metaData : IPackageSearchResultItem;
  latestVersionInfo : IPackageLatestVersionInfo;
  index : IFolderSourceIndex;
begin
  result := TDPMPackageSearchResult.Create(0,0);
  index := GetIndex(compilerVersion);
  try
    for item in ids do
    begin
      //Determine the latest version first, independently of whether the installed
      //version is present in this folder. Otherwise a source that only carries a
      //newer (or prerelease) version than the installed one would report nothing,
      //and its update would never reach the repo manager merge.
      latestVersionInfo := GetPackageLatestVersion(cancellationToken, item.id, compilerVersion);
      if latestVersionInfo = nil then
        continue;

      //Prefer the installed version's metadata for the row, but fall back to the
      //latest available version's metadata when the installed .dpkg is not here.
      metaData := DoGetPackageMetaDataForDisplay(cancellationToken, item.Id, item.Version.ToString, compilerVersion, index);
      if metaData = nil then
        metaData := DoGetPackageMetaDataForDisplay(cancellationToken, item.Id, latestVersionInfo.LatestVersion.ToString, compilerVersion, index);

      if metaData <> nil then
      begin
        metaData.LatestVersion := latestVersionInfo.LatestVersion;
        metaData.LatestStableVersion := latestVersionInfo.LatestStableVersion;
        result.Results.Add(metaData);
      end;
    end;
  finally
    //once for them all.
    if index <> nil then
      index.Save;
  end;
end;

//function TDirectoryPackageRepository.GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion): IDictionary<string, IPackageLatestVersionInfo>;
//var
//  i : integer;
//  j : integer;
//  id : string;
//  searchFiles : IList<string>;
//  regex : TRegEx;
//  searchRegex : string;
//  packageFile : string;
//  match : TMatch;
//  packageVersion : TPackageVersion;
//  latestVersion  : TPackageVersion;
//  latestStableVersion : TPackageVersion;
//  info : IPackageLatestVersionInfo;
//begin
//  result := TCollections.CreateDictionary<string, IPackageLatestVersionInfo>;
//
//  searchRegEx := GetSearchRegex(compilerVersion, [], '');
//  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);
//
//  for i := 0 to ids.Count -1 do
//  begin
//    id := ids.Items[i].Id;
//    searchFiles := DoExactList(id, compilerVersion, platform, '');
//    latestVersion := TPackageVersion.Empty;
//    latestStableVersion := TPackageVersion.Empty;
//    for j := 0 to searchFiles.Count -1 do
//    begin
//      packageFile := ChangeFileExt(ExtractFileName(searchFiles[j]), '');
//      match := regex.Match(packageFile);
//      if match.Success then
//      begin
//        if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
//          continue;
//
//        if packageVersion > latestVersion then
//          latestVersion := packageVersion;
//
//        if packageVersion.IsStable and (packageVersion > latestStableVersion) then
//          latestStableVersion := packageVersion;
//      end;
//      //if latestVersion is empt then we didn't find any files!
//      if not latestVersion.IsEmpty then
//      begin
//        info := TDPMPackageLatestVersionInfo.Create(id,latestStableVersion, latestVersion);
//        result[id] := info;
//      end;
//    end;
//  end;
//end;



function TDirectoryPackageRepository.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion): IPackageSearchResultItem;
var
  index : IFolderSourceIndex;
begin
  index := GetIndex(compilerVersion);
  try
    result := DoGetPackageMetaDataForDisplay(cancellationToken, packageId, packageVersion, compilerVersion, index);
  finally
    if index <> nil then
      index.Save;
  end;
end;

function TDirectoryPackageRepository.DoGetPackageMetaDataForDisplay(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string;
                                                                    const compilerVersion : TCompilerVersion; const index : IFolderSourceIndex) : IPackageSearchResultItem;
var
  matchingFiles : IList<string>;
  listing : IFolderListing;
  packageSuffix : string;
  packageFileName : string;
  readManifestFunc : TReadManifestFunc;
  metaData : IPackageMetadata;
  _fileHash :string;
  _hashAlgorithm : string;
begin
  result := nil;
  listing := nil;
  if index <> nil then
  begin
    //the files DoExactList finds, in one pass that also sees their sidecars, for the index.
    if packageVersion <> '' then
      packageSuffix := '-' + packageVersion + cPackageFileExt
    else
      packageSuffix := cPackageFileExt;
    listing := ListFolder(SourceUri, [ExactListPattern(packageId, compilerVersion, packageVersion, '.*')], packageSuffix);
    matchingFiles := listing.PackageFiles;
  end
  else
    matchingFiles := DoExactList(packageId, compilerVersion, [], packageVersion);
  if matchingFiles.Count = 0 then
    exit;
  packageFileName := matchingFiles[0];
  if cancellationToken.IsCancelled then
    exit;

  readManifestFunc := function (const name : string; const manifest : IPackageSpec; const fileHash :string; const hashAlgorithm : string) : IInterface
                  begin
                    result := TPackageMetadata.CreateFromManifest(name, manifest);
                    _fileHash := fileHash;
                    _hashAlgorithm := hashAlgorithm;
                  end;

  metaData := DoGetPackageMetaData(cancellationToken, packageFileName, true, readManifestFunc, listing, index) as IPackageMetaData;



  if metaData <> nil then
  begin
    result := TDPMPackageSearchResultItem.FromMetaData(name, metaData, _fileHash, _hashAlgorithm);
    result.PublishedDate := GetFilePublishedDate(packageFileName);
  end;

end;

function TDirectoryPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
var
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  i : integer;
  packageVersion : TPackageVersion;
begin
  result := TCollections.CreateList<TPackageVersion>;
  searchFiles := DoExactList(id, compilerVersion, [TDPMPlatform.UnknownPlatform], '');

  searchRegEx := GetSearchRegex(compilerVersion, [], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);
  for i := 0 to searchFiles.Count - 1 do
  begin
    //ensure that the files returned are actually packages

    packageFile := ChangeFileExt(ExtractFileName(searchFiles[i]), '');
    match := regex.Match(packageFile);
    if match.Success then
    begin
      if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
        continue;

      if (not preRelease) and (not packageVersion.IsStable)  then
        continue;

      result.Add(packageVersion);
    end;
  end;

  //no point sorting here or making results distinct as the the repo manager will do both.

end;

function TDirectoryPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
var
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  i : integer;
  packageVersion : TPackageVersion;
  packageInfo : IPackageInfo;
  readManifestFunc : TReadManifestFunc;
begin
  result := TCollections.CreateList<IPackageInfo>;

  searchFiles := DoExactList(id, compilerVersion, [], '');

  searchRegEx := GetSearchRegex(compilerVersion, [], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);


  readManifestFunc := function (const name : string; const manifest : IPackageSpec; const fileHash :string; const hashAlgorithm : string) : IInterface
                  begin
                    result := TPackageInfo.CreateFromManifest(name, manifest, fileHash, hashAlgorithm);
                  end;

  for i := 0 to searchFiles.Count - 1 do
  begin
    //ensure that the files returned are actually packages
    packageFile := ChangeFileExt(ExtractFileName(searchFiles[i]), '');
    match := regex.Match(packageFile);
    if match.Success then
    begin
      if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
        continue;

      //todo : check this is correct.
      if not versionRange.IsSatisfiedBy(packageVersion) then
        continue;

      if not prerelease then
        if not packageVersion.IsStable then
          continue;

      packageInfo := DoGetPackageMetadata(cancellationToken, searchFiles[i], false, readManifestFunc) as IPackageInfo;
      result.Add(packageInfo);
    end;
  end;

  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := right.Version.CompareTo(left.Version); ;
    end));


end;



function TDirectoryPackageRepository.DoExactList(const id : string; const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms; const version : string) : IList<string>;
var
  searchTerm : string;
  //files : TStringDynArray;
  fileList : IList<string>;
begin
  result := TCollections.CreateList < string > ;
  searchTerm := ExactListPattern(id, compilerVersion, version, cPackageFileExt);

  try
    fileList := TDirectoryUtils.GetFiles(SourceUri, searchTerm);
    //dedupe
    result.AddRange(fileList.Distinct(TStringComparer.OrdinalIgnoreCase));
  except
    on e : Exception do
    begin
      RequestFailed('could not be searched : ' + e.Message);
    end;
  end;
end;




function TDirectoryPackageRepository.DoGetPackageFeedFiles(const searchTerm : string; const exact : boolean; const compilerVersion : TCompilerVersion) : IList<string>;
var
  files : IList<string>;
  query : string;
begin
  Logger.Debug('TDirectoryPackageRepository.DoGetPackageFeedFiles');
  result := TCollections.CreateList<string>;
  query := FeedPattern(searchTerm, exact, compilerVersion) + cPackageFileExt;
  files := TDirectoryUtils.GetFiles(SourceUri, query);
  result.AddRange(files);

end;

type
  TPackageFind = record
    Id : string;
    PlatformString : string;  // Raw platform segment (binary string or pipe-separated)
    LatestVersion : TPackageVersion;
    LatestStableVersion : TPackageVersion;
  end;


function TDirectoryPackageRepository.DoGetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const files : IList<string>;
                                                      const listing : IFolderListing; const index : IFolderSourceIndex) : IList<IPackageSearchResultItem>;
var
  searchRegEx : string;
  regex : TRegEx;
  match : TMatch;
  i : integer;
  packageFileName : string;
  id : string;
  platformStr : string;
  versionString : string;
  packageVersion : TPackageVersion;
  packageLookup : IDictionary<string, TPackageFind>;
  find : TPackageFind;

  packageMetaData : IPackageMetadata;
  resultItem : IPackageSearchResultItem;

  readManifestFunc : TReadManifestFunc;
  _fileHash :string;
  _hashAlgorithm : string;
begin
  Logger.Debug('TDirectoryPackageRepository.DoGetPackageFeed');
  result := TCollections.CreateList<IPackageSearchResultItem>;

  packageLookup := TCollections.CreateDictionary<string, TPackageFind>  ;

  searchRegEx := GetSearchRegex(options.CompilerVersion, options.Platforms, '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);



  //work out the latest version for each package and what platforms it supports.
  for i := 0 to files.Count - 1 do
  begin
    if cancelToken.IsCancelled then
      exit;

    //ensure that the files returned are actually packages
    packageFileName := ChangeFileExt(ExtractFileName(files[i]), '');
    match := regex.Match(packageFileName);
    if match.Success then
    begin
      id := match.Groups[1].Value;
      //ignore the matched compiler version since we are only ever looking at 1 compiler version
      //cv := StringToCompilerVersion(match.Groups[2].Value);
      // Platform segment can be binary string (e.g., '11') or pipe-separated (e.g., 'win32|win64')
      platformStr := match.Groups[3].Value;
      versionString := match.Groups[4].Value;
      if not TPackageVersion.TryParse(versionString, packageVersion) then
        continue;

      if not options.Prerelease then
        if not packageVersion.IsStable then
          continue;

      if not packageLookup.TryGetValue(LowerCase(id), find) then
      begin
        find.Id := id;
        find.PlatformString := platformStr;
        find.LatestVersion := packageVersion;
        if packageVersion.IsStable then
          find.LatestStableVersion := packageVersion;
        packageLookup[LowerCase(id)] := find;
      end
      else
      begin
        //each version's .dpkg can cover a different platform set, so track the
        //platform string that belongs to the version we're going to load.
        if packageVersion > find.LatestVersion then
        begin
          find.LatestVersion := packageVersion;
          find.PlatformString := platformStr;
        end;

        if packageVersion.IsStable then
          if packageVersion > find.LatestStableVersion then
            find.LatestStableVersion := packageVersion;
        packageLookup[LowerCase(id)] := find;
      end;
    end;
  end;

  readManifestFunc := function (const name : string; const manifest : IPackageSpec; const fileHash :string; const hashAlgorithm : string) : IInterface
                  begin
                    result := nil;
                    //worked out (and written beside the package) for this - it goes in the result.
                    _fileHash := fileHash;
                    _hashAlgorithm := hashAlgorithm;
                    if (manifest = nil) or (manifest.TargetPlatform = nil) then
                      exit;
                    try
                      result := TPackageMetadata.CreateFromManifest(name, manifest);
                    except
                      on e : Exception do
                      begin
                        Logger.Warning(TWarningCode.W1207_PackageEntryUnreadable, 'Unable to read the package details of [' + packageFileName + '] in [' + Name + '] : ' + e.Message);
                        result := nil;
                      end;
                    end;
                  end;

  //now we can use the info collected above to build actual results.
  for find in packageLookup.Values do
  begin
    if cancelToken.IsCancelled then
      exit;

    resultItem := nil;
    packageMetaData := nil;
    if cancelToken.IsCancelled then
      exit;

    packageFileName := Format('%s-%s-%s-%s.dpkg', [find.Id, CompilerToString(options.CompilerVersion), find.PlatformString, find.LatestVersion.ToStringNoMeta]);
    packageFileName := IncludeTrailingPathDelimiter(SourceUri) + packageFileName;
    if not FileExists(packageFileName) then
      continue;

    _fileHash := '';
    _hashAlgorithm := '';
    try
      packageMetadata := DoGetPackageMetaData(cancelToken, packageFileName, true, readManifestFunc, listing, index) as IPackageMetadata;
    except
      on e : Exception do
      begin
        Logger.Warning(TWarningCode.W1207_PackageEntryUnreadable, 'Unable to read the package [' + packageFileName + '] in [' + Name + '] : ' + e.Message);
        packageMetadata := nil;
      end;
    end;

    if packageMetadata <> nil then
    begin
      resultItem := TDPMPackageSearchResultItem.FromMetaData(Self.Name, packageMetadata, _fileHash, _hashAlgorithm);
      resultItem.LatestVersion := find.LatestVersion;
      resultItem.LatestStableVersion := find.LatestStableVersion;
      resultItem.PublishedDate := GetFilePublishedDate(packageFileName);
      result.Add(resultItem);
    end;
  end;
end;

function TDirectoryPackageRepository.DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string; readHash : boolean; const readManifestFunc : TReadManifestFunc;
                                                          const listing : IFolderListing; const index : IFolderSourceIndex) : IInterface;
var
  zipFile : TZipFile;
  metaBytes : TBytes;
  metaString : string;
  manifest : IPackageSpec;
  reader : IPackageSpecReader;
  manifestFileName : string;
  svgIconFileName : string;
  pngIconFileName : string;
  iconBytes : TBytes;
  isSvg : boolean;
  hashFileName : string;
  fileHash : string;
  specText : string;
  useIndex : boolean;
  stamps : TPackageFileStamps;
begin
//  Logger.Debug('TDirectoryPackageRepository.DoGetPackageMetaData');
  result := nil;
  if not FileExists(fileName) then
    exit;

  reader := TPackageSpecReader.Create(Logger);

  //This sidecar is the extracted copy of the in-archive 'package.dspec.yaml', so it keeps the legacy extension to
  //match the archive entry's semantics and to avoid orphaning sidecars in existing caches.
  manifestFileName := ChangeFileExt(fileName, cLegacyPackageSpecExt);

  //Only what is shown comes from the index - those reads ask for the hash, and give the listing that found the
  //package. A package the listing didn't see (its name worked out instead) can't be checked against it.
  useIndex := readHash and (listing <> nil) and (index <> nil);
  if useIndex then
  begin
    stamps := listing.StampsFor(fileName);
    useIndex := stamps.Package.Present;
  end;
  if useIndex and index.TryGet(fileName, stamps, fileHash, specText) then
  begin
    manifest := reader.ReadSpecString(specText, manifestFileName);
    if manifest <> nil then
      exit(readManifestFunc(name, manifest, fileHash, cPackageHashAlgorithm));
    //what was recorded doesn't read any more - forget it, and read the files.
    index.Remove(fileName);
  end;
  fileHash := '';
  specText := '';

  if readHash then
  begin
    hashFileName := fileName + cPackageHashAlgorithmExt;
    if FileExists(hashFileName) then
      fileHash := ReadHashFile(hashFileName)
    else
    begin
      //calulate file hash and save it for next time
      fileHash := HashPackageFile(fileName);
      //we might write to the source folder, so check if writable
      CheckWritable(ExtractFilePath(fileName));
      if FIsWritable then
        TFile.WriteAllText(hashFileName, fileHash);
    end;
  end;

  //first see if the dspec has been extracted already.
  if FileExists(manifestFileName) then
  begin
    if useIndex then
      manifest := ReadSpecSidecar(reader, manifestFileName, specText)
    else
      manifest := reader.ReadSpec(manifestFileName);
  end;

  if manifest = nil then
  begin
    zipFile := TZipFile.Create;
    try
      try
        zipFile.Open(fileName, TZipMode.zmRead);
        if zipFile.IndexOf(cPackageDspecFile) = -1 then
        begin
          Logger.Warning(TWarningCode.W1207_PackageEntryUnreadable, 'The package file [' + fileName + '] in [' + Name + '] does not contain a ' + cPackageDspecFile + '.');
          exit;
        end;
        zipFile.Read(cPackageDspecFile, metaBytes);

        //we will take this opportunity to extract the icon file here
        //for later use.
        isSvg := false;

        svgIconFileName := ChangeFileExt(filename, '.svg');
        if not FileExists(svgIconFileName) then
        begin
          if zipFile.IndexOf(cIconFileSVG) <> -1 then
          begin
            zipFile.Read(cIconFileSVG, iconBytes);
            isSvg := true;
          end;
        end
        else
          isSvg := true;
        pngIconFileName := ChangeFileExt(filename, '.png');
        if (not isSvg) and not FileExists(pngIconFileName) then
        begin
          if zipFile.IndexOf(cIconFilePNG) <> -1 then
          begin
            zipFile.Read(cIconFilePNG, iconBytes);
            isSvg := false;
          end;

        end;
      except
        on e : Exception do
        begin
          Logger.Warning(TWarningCode.W1207_PackageEntryUnreadable, 'Unable to read the package [' + fileName + '] in [' + Name + '] : ' + e.Message);
          exit;
        end;
      end;
    finally
      zipFile.Free;
    end;
    //doing this outside the try/finally to avoid locking the package for too long.
    metaString := TEncoding.UTF8.GetString(metaBytes);
    //metaString is the dspec YAML content extracted from the .dpkg, not a path.
    //Use ReadSpecString and pass the .dpkg path as the diagnostic filename.
    manifest := reader.ReadSpecString(metaString, fileName);
    specText := metaString;
    //we might write to the source folder, so check if writable
    CheckWritable(ExtractFilePath(fileName));
    if FIsWritable then
    begin
      try
        //TODO : Could this cause an issue if multiple users attempt this on a shared folder?
        //may need to use a lock file.
        TFile.WriteAllText(manifestFileName, metaString, TEncoding.UTF8);
        if Length(iconBytes) > 0 then
          if isSvg then
            TFile.WriteAllBytes(svgIconFileName, iconBytes)
          else
            TFile.WriteAllBytes(pngIconFileName, iconBytes)
      except
        //even though we test for write access other errors might occur (eg with network drives disconnected etc).
      end;
    end;
  end;
  if manifest = nil then
    exit;


  result := readManifestFunc(name, manifest, fileHash, cPackageHashAlgorithm);
  //recorded under what the listing saw before these reads - had anything changed since, the next listing differs,
  //and it is read again.
  if useIndex and (result <> nil) then
    index.Put(fileName, stamps, fileHash, specText);
end;

function TDirectoryPackageRepository.DoList(const searchTerm : string; const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms) : IList<string>;
var
//  files : TStringDynArray;
//  fileList : IList<string>;
  term : string;
begin
  term := searchTerm;
  //  result := TCollections.CreateList<string>;
  if term <> '*' then
    term := '*' + term + '*';

  term := term + '-' + CompilerVersionToSearchPart(compilerVersion);

  term := term + '-*-*' + cPackageFileExt;
  result := TDirectoryUtils.GetFiles(SourceUri, term);

  //fileList := TCollections.CreateList<string>(files);

  //TODO : Does this really need to be de-duped?

  //dedupe
  //result.AddRange(TEnumerable.Distinct<string>(fileList, TStringComparer.OrdinalIgnoreCase ));
end;

function TDirectoryPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
var
  searchTerms : TArray<string>;
  searchFiles : IList<string>;
  allFiles : IList<string>;
  i : integer;
  searchRegEx : string;
  regex : TRegEx;
  packageFile : string;
  match : TMatch;
  id : string;
  cv : TCompilerVersion;
  version : string;
  packageVersion : TPackageVersion;

  currentCompiler : TCompilerVersion;
  currentId : string;
  currentVersion : TPackageVersion;
  currentPlatforms : TDPMPlatforms;
  platforms : TDPMPlatforms;
  newItem : IPackageListItem;
  bIsLast : boolean;

  procedure AddCurrent;
  begin
    newItem := TPackageListItem.Create(currentId,currentCompiler, currentVersion, currentPlatforms);
    result.Add(newItem);
  end;

begin
  result := TCollections.CreateList<IPackageListItem>;
  if options.SearchTerms <> '' then
    searchTerms := TStringUtils.SplitStr(Trim(options.SearchTerms), ' ');
  //need at least 1 search term, if there are none then search for *.
  if length(searchTerms) = 0 then
  begin
    SetLength(searchTerms, 1);
    searchTerms[0] := '*';
  end;

  allFiles := TCollections.CreateList < string > ;

  platforms := options.Platforms;


  for i := 0 to Length(searchTerms) - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;

    if options.Exact then
      searchFiles := DoExactList(searchTerms[i], options.CompilerVersion, platforms, options.Version.ToStringNoMeta)
    else
      searchFiles := DoList(searchTerms[i], options.CompilerVersion, platforms);
    allFiles.AddRange(searchFiles);
  end;

  //remove the path from the files
  for i := 0 to allFiles.Count -1 do
    allFiles[i] := ChangeFileExt(ExtractFileName(allFiles[i]), '');

  allFiles.Sort;

  //do we really need to do a regex check here?
  searchRegEx := GetSearchRegex(options.CompilerVersion, options.Platforms, options.Version.ToStringNoMeta);
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);
  currentId := '';
  currentVersion := TPackageVersion.Empty;
  currentCompiler := TCompilerVersion.UnknownVersion;
  currentPlatforms := [];
  platforms := [];
  for i := 0 to allFiles.Count - 1 do
  begin
    bIsLast := i = allFiles.Count -1;
    //ensure that the files returned are actually packages
    packageFile := allFiles[i];
    match := regex.Match(packageFile);
    if match.Success then
    begin
      id := match.Groups[1].Value;
      cv := StringToCompilerVersion(match.Groups[2].Value);
      platforms :=  BinStringToDPMPlatforms(match.Groups[3].Value);
      version := match.Groups[4].Value;

      if not TPackageVersion.TryParse(version, packageVersion) then
        continue;

      if not options.Prerelease then
        if not packageVersion.IsStable then
          continue;
      //if the id, compiler or version change then start a new listitem - same if it's the last entry in the list.
      if (id <> currentId) or (cv <> currentCompiler) or (packageVersion <> currentVersion) or bIsLast then
      begin
        //higher version, start again
        if (id = currentId) and (cv = currentCompiler) and (packageVersion <> currentVersion) then
        begin
          //lower version, just ignore it.
          if packageVersion < currentVersion then
          begin
            if bIsLast then
              AddCurrent;
            continue;
          end;

          //higher version, start again.
          currentId := id;
          currentCompiler := cv;
          currentVersion := packageVersion;
          currentPlatforms := platforms;
          if bIsLast then
            AddCurrent;
          continue;
        end;
        // a new package, so record the last one if we had one.
        if ((currentId <> '') and (currentCompiler <> TCompilerVersion.UnknownVersion)) then
          AddCurrent;

        currentId := id;
        currentCompiler := cv;
        currentVersion := packageVersion;
        currentPlatforms := platforms;
        if bIsLast then
          AddCurrent;
      end
      else
      begin
        //same id+compiler+version - merge platforms (legacy multi-file-per-compiler layout)
        currentPlatforms := currentPlatforms + platforms;
        if bIsLast then
         AddCurrent;
      end;
    end;
  end;
end;

//Raises when fileName is there and can't be removed : a push that leaves the old package's spec beside the new one
//hasn't worked, and the user needs to know which file to delete.
procedure RemoveStaleSidecar(const fileName : string);
begin
  if FileExists(fileName) and (not System.SysUtils.DeleteFile(fileName)) then
    raise EInOutError.CreateFmt('the package was copied, but [%s] (read from the package it replaced) could not be removed : %s - delete it, or the old package''s details are used',
                                [fileName, SysErrorMessage(GetLastError)]);
end;

function TDirectoryPackageRepository.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
var
  targetFile : string;
  hashFile : string;
  hash : string;
begin
  result := false;
  //  if TPath.IsRelativePath(pushOptions.PackagePath) then
  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    Logger.Error(TErrorCode.E3004_PackageFileNotFound, 'The package file [' + pushOptions.PackagePath + '] does not exist.');
    exit;
  end;

  //the repository manager reports these, as for any other source that can't be used.
  if not DirectoryExists(SourceUri) then
  begin
    RequestFailed('folder [' + SourceUri + '] does not exist.');
    exit;
  end;
  try
    targetFile := TPath.Combine(SourceUri, TPath.GetFileName(pushOptions.PackagePath));
    if pushOptions.SkipDuplicate and TFile.Exists(targetFile) then
    begin
      Logger.Information('Package [' + TPath.GetFileName(pushOptions.PackagePath) + '] exists and skipDuplicates specified, skipping');
      exit(true);
    end;


    TFile.Copy(pushOptions.PackagePath, targetFile, true);

    //caclulate file has from original file
    hashFile := targetFile + cPackageHashAlgorithmExt;
    hash := THashSHA256.GetHashStringFromFile(pushOptions.PackagePath);
    //write to source - in one go, so a reader never sees part of it (and fails the package as modified).
    TFileUtils.AtomicWriteAllText(hashFile, hash, TEncoding.ASCII);

    //The spec and icon beside a package are read from it once and kept - those of the package this replaced are
    //wrong now. Removed, the next read takes them from the new one. After the copy, so nothing reading in between
    //takes them from the old package again.
    RemoveStaleSidecar(ChangeFileExt(targetFile, cLegacyPackageSpecExt));
    RemoveStaleSidecar(ChangeFileExt(targetFile, '.svg'));
    RemoveStaleSidecar(ChangeFileExt(targetFile, '.png'));

    Logger.Information('Package pushed ok.', true);
    result := true;
  except
    on e : Exception do
    begin
      RequestFailed('could not take the package : ' + e.Message);
    end;
  end;
end;

end.

