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
  VSoft.Awaitable,
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
  DPM.Core.Repository.Base;

type
  TReadSpecFunc = reference to function(const name : string; const spec : IPackageSpec) : IInterface;

  TDirectoryPackageRepository = class(TBaseRepository, IPackageRepository)
  private
    FPermissionsChecked : boolean;
    FIsWritable : boolean;
  protected
    function DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string; const readSpecFunc : TReadSpecFunc) : IInterface;


    function DoList(searchTerm : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IList<string>;
    function DoExactList(const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const version : string) : IList<string>;

    function DoGetPackageFeedFiles(const searchTerm : string; const exact : boolean; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IList<string>;
    function DoGetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const files : IList<string>) : IList<IPackageSearchResultItem>;


    function DownloadPackage(const cancellationToken : ICancellationToken; const packageMetadata : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;

    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean) : IPackageIdentity;


    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo; overload;

//    function GetPackageLatestVersions(const cancellationToken : ICancellationToken; const ids : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IDictionary<string, IPackageLatestVersionInfo>;

    function GetPackageLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageLatestVersionInfo;

    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResultItem;


    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;

    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageId>; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) :  IPackageSearchResult;


    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;


  public
    constructor Create(const logger : ILogger); override;
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
  DPM.Core.Package.Metadata,
  DPM.Core.Package.Icon,
  DPM.Core.Spec.Reader,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Directory,
  DPM.Core.Package.SearchResults,
  DPM.Core.Package.ListItem, DPM.Core.Package.PackageLatestVersionInfo;

{ TDirectoryPackageRespository }

function GetSearchRegex(const compilerVersion : TCompilerVersion; const platforms : TDPMPlatforms; const version : string) : string;
begin
  // ^(\w+\.\w+)\-([^\-]+)\-([^\-]+)\-(.*)$';
  result := '^(\w+\.\w+)\-';
  if (compilerVersion > TCompilerVersion.UnknownVersion) then
    result := result + Format('(%s)\-', [CompilerToString(compilerVersion)])
  else
    result := result + '([^\-]+)\-';

  if platforms <> [] then
    result := result + Format('(%s)\-', [DPMPlatformsToString(platforms, '|')])
  else
    result := result + '([^\-]+)\-';

  if version <> '' then
    result := result + Format('(%s)$', [version])
  else
    result := result + '(.*)$';

end;




constructor TDirectoryPackageRepository.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FPermissionsChecked := false;
  FIsWritable := false;
end;

function TDirectoryPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageMetadata : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;
var
  sourceFile : string;
  destFile : string;
begin
  result := false;
  sourceFile := IncludeTrailingPathDelimiter(SourceUri) + packageMetadata.ToString + cPackageFileExt;
  if not FileExists(sourceFile) then
  begin
    Logger.Error('File not found in repository [' + sourceFile + ']');
    exit;
  end;
  destFile := IncludeTrailingPathDelimiter(localFolder) + packageMetadata.ToString + cPackageFileExt;
  if TPathUtils.IsRelativePath(destFile) then
    destFile := TPath.GetFullPath(destFile);
  try
    ForceDirectories(ExtractFilePath(destFile));
    Logger.Information('GET ' + sourceFile);
    TFile.Copy(sourceFile, destFile, true);
    fileName := destFile;
    result := true;
    Logger.Information('OK ' + sourceFile);
  except
    on e : Exception do
    begin
      Logger.Error('Error downloading  [' + sourceFile + '] to [' + destFile + ']');
    end;
  end;
end;



function TDirectoryPackageRepository.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform; const includePrerelease : boolean): IPackageIdentity;
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
begin
  result := nil;
  if version.IsEmpty then
    searchFiles := DoExactList(id, compilerVersion, platform, '')
  else
    searchFiles := DoExactList(id, compilerVersion, platform, version.ToStringNoMeta);

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
  result := TPackageIdentity.Create(Self.Name, id, maxVersion, compilerVersion, platform);

end;

function TDirectoryPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
var
  zipFile : TZipFile;
  zipIdx : integer;
  packagFileName : string;
  svgIconFileName : string;
  pngIconFileName : string;
  stream : TMemoryStream;
  DecompressionStream : TStream;
  fileStream : TFileStream;
  zipHeader : TZipHeader;
begin
  result := nil;
  packagFileName := Format('%s-%s-%s-%s.dpkg', [packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform), packageVersion]);
  packagFileName := TPath.Combine(Self.SourceUri, packagFileName);
  svgIconFileName := ChangeFileExt(packagFileName, '.svg');
  pngIconFileName := ChangeFileExt(packagFileName, '.png');

  //it quite likely already extracted so look for that first
  //icons can be svg or png.
  if FileExists(svgIconFileName) then
  begin
    fileStream := TFileStream.Create(svgIconFileName, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    //the icon now owns the stream.
    result := CreatePackageIcon(TPackageIconKind.ikSvg, stream);
    exit;
  end;

  if FileExists(pngIconFileName) then
  begin
    fileStream := TFileStream.Create(pngIconFileName, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    //the icon now owns the stream.
    result := CreatePackageIcon(TPackageIconKind.ikPng, stream);
    exit;
  end;

  //not already extracted, so check the package
  zipFile := TZipFile.Create;
  try
    try
      zipFile.Open(packagFileName, TZipMode.zmRead);
      zipIdx := zipFile.IndexOf(cIconFileSVG);
      if zipIdx <> -1 then
      begin
        stream := TMemoryStream.Create;
        //casting due to broken overload resolution in XE7
        zipFile.Read(zipIdx, DecompressionStream, zipHeader);
        stream.CopyFrom(DecompressionStream, DecompressionStream.Size);
        FreeAndNil(DecompressionStream);
        //save it to speed up things next time.
        stream.SaveToFile(svgIconFileName);
        //result now owns the stream.
        result := CreatePackageIcon(TPackageIconKind.ikSvg, stream);
        exit;
      end;
      if cancelToken.IsCancelled then
        exit;

      zipIdx := zipFile.IndexOf(cIconFilePNG);
      if zipIdx <> -1 then
      begin
        stream := TMemoryStream.Create;
        //casting due to broken overload resolution in XE7
        zipFile.Read(zipIdx, DecompressionStream, zipHeader);
        stream.CopyFrom(DecompressionStream, DecompressionStream.Size);
        FreeAndNil(DecompressionStream);
        stream.SaveToFile(pngIconFileName);
        //result now owns the stream.
        result := CreatePackageIcon(TPackageIconKind.ikPng, stream);
        exit;
      end;
    except
      on e : Exception do
      begin
        Logger.Error('Error opening package file [' + packagFileName + ']');
        exit;
      end;
    end;
  finally
    zipFile.Free;
  end;
end;

function TDirectoryPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
var
  packageFileName : string;
  readSpecFunc : TReadSpecFunc;
begin
  result := nil;
  packageFileName := Format('%s-%s-%s-%s.dpkg', [packageId.Id, CompilerToString(packageId.CompilerVersion), DPMPlatformToString(packageId.Platform), packageId.Version.ToStringNoMeta]);
  packageFileName := IncludeTrailingPathDelimiter(SourceUri) + packageFileName;

  if not FileExists(packageFileName) then
    exit;
  if cancellationToken.IsCancelled then
    exit;

  readSpecFunc := function (const name : string; const spec : IPackageSpec) : IInterface
                  begin
                    result := TPackageInfo.CreateFromSpec(name, spec);
                  end;

  result := DoGetPackageMetaData(cancellationToken, packageFileName, readSpecFunc) as IPackageInfo;
end;


function TDirectoryPackageRepository.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
var
  searchTerms : TArray<string>;
  i : integer;
  results : IList<IPackageSearchResultItem>;
  searchFiles : IList<string>;
  allFiles : IList<string>;
  distinctFiles : IEnumerable<string>;
//  take : integer;
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

  allFiles := TCollections.CreateList < string > ;
  for i := 0 to Length(searchTerms) - 1 do
  begin
    if cancelToken.IsCancelled then
      exit;
    allFiles.AddRange(DoGetPackageFeedFiles(searchTerms[i], options.Exact, compilerVersion, platform));
  end;

  distinctFiles := TDistinctIterator<string>.Create(allFiles, TStringComparer.OrdinalIgnoreCase);
//  if options.Skip > 0 then
//    distinctFiles := distinctFiles.Skip(options.Skip);
//  if options.Take > 0 then
//    distinctFiles := distinctFiles.Take(options.Take);
  searchFiles := TCollections.CreateList<string>(distinctFiles);

  results := DoGetPackageFeed(cancelToken, options, searchFiles);
  result.Results.AddRange(results);

end;


function TDirectoryPackageRepository.GetPackageLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform ) : IPackageLatestVersionInfo;
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

  searchFiles := DoExactList(id, compilerVersion, platform, '');
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

function TDirectoryPackageRepository.GetPackageFeedByIds(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const compilerVersion: TCompilerVersion;
  const platform: TDPMPlatform): IPackageSearchResult;
var
  item : IPackageId;
  metaData : IPackageSearchResultItem;
  latestVersionInfo : IPackageLatestVersionInfo;
begin
  result := TDPMPackageSearchResult.Create(0,0);
  for item in ids do
  begin
      metaData := GetPackageMetaData(cancellationToken,item.Id, item.Version.ToString, compilerVersion, platform);
      if metaData <> nil then
      begin
        latestVersionInfo := GetPackageLatestVersion(cancellationToken, item.id, compilerVersion, platform);
        if latestVersionInfo <> nil then
        begin
          metaData.LatestVersion := latestVersionInfo.LatestVersion;
          metaData.LatestStableVersion := latestVersionInfo.LatestStableVersion;
          Result.Results.Add(metaData);
        end;
      end;
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



function TDirectoryPackageRepository.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform): IPackageSearchResultItem;
var
  packageFileName : string;
  readSpecFunc : TReadSpecFunc;
  metaData : IPackageMetadata;
begin
  result := nil;
  packageFileName := Format('%s-%s-%s-%s.dpkg', [packageId, CompilerToString(compilerVersion), DPMPlatformToString(platform), packageVersion]);
  packageFileName := IncludeTrailingPathDelimiter(SourceUri) + packageFileName;
  if not FileExists(packageFileName) then
    exit;
  if cancellationToken.IsCancelled then
    exit;

  readSpecFunc := function (const name : string; const spec : IPackageSpec) : IInterface
                  begin
                    result := TPackageMetadata.CreateFromSpec(name, spec);
                  end;

  metaData := DoGetPackageMetaData(cancellationToken, packageFileName, readSpecFunc) as IPackageMetaData;
  if metaData <> nil then
    result := TDPMPackageSearchResultItem.FromMetaData(name, metaData);

end;

function TDirectoryPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>;
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
  searchFiles := DoExactList(id, compilerVersion, TDPMPlatform.UnknownPlatform, '');

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

function TDirectoryPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;
var
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  i : integer;
  packageVersion : TPackageVersion;
  packageInfo : IPackageInfo;
  readSpecFunc : TReadSpecFunc;
begin
  result := TCollections.CreateList<IPackageInfo>;

  searchFiles := DoExactList(id, compilerVersion, platform, '');

  searchRegEx := GetSearchRegex(compilerVersion, [platform], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);


  readSpecFunc := function (const name : string; const spec : IPackageSpec) : IInterface
                  begin
                    result := TPackageInfo.CreateFromSpec(name, spec);
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

      packageInfo := DoGetPackageMetadata(cancellationToken, searchFiles[i], readSpecFunc) as IPackageInfo;
      result.Add(packageInfo);
    end;
  end;

  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := right.Version.CompareTo(left.Version); ;
    end));


end;



function CompilerVersionToSearchPart(const compilerVersion : TCompilerVersion) : string;
begin
  case compilerVersion of
    TCompilerVersion.UnknownVersion : result := '*';
  else
    result := CompilerToString(compilerVersion);
  end;
end;

function TDirectoryPackageRepository.DoExactList(const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const version : string) : IList<string>;
var
  searchTerm : string;
  //files : TStringDynArray;
  fileList : IList<string>;
begin
  result := TCollections.CreateList < string > ;
  searchTerm := id;
  searchTerm := searchTerm + '-' + CompilerVersionToSearchPart(compilerVersion);
  if platform = TDPMPlatform.UnknownPlatform then
    searchTerm := searchTerm + '-*'
  else
    searchTerm := searchTerm + '-' + DPMPlatformToString(platform);

  if version <> '' then
    searchTerm := searchTerm + '-' + version + cPackageFileExt
  else
    searchTerm := searchTerm + '-*' + cPackageFileExt;

  try
    fileList := TDirectoryUtils.GetFiles(SourceUri, searchTerm);
    //dedupe
    result.AddRange(fileList.Distinct(TStringComparer.OrdinalIgnoreCase));
  except
    on e : Exception do
    begin
      Logger.Error('Error searching source [' + Name + '] : ' + e.Message);
    end;
  end;
end;




function TDirectoryPackageRepository.DoGetPackageFeedFiles(const searchTerm : string; const exact : boolean; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IList<string>;
var
  files : IList<string>;
  query : string;
begin
  Logger.Debug('TDirectoryPackageRepository.DoGetPackageFeedFiles');
  result := TCollections.CreateList<string>;
  query := searchTerm;
  if (query <> '*') and (not exact) then
    query := '*' + query + '*';
  query := query + '-' + CompilerVersionToSearchPart(compilerVersion) + '-' + DPMPlatformToString(platform) + '-'  + '*' + cPackageFileExt;
  files := TDirectoryUtils.GetFiles(SourceUri, query);
  result.AddRange(files);

end;

type
  TPackageFind = record
    Id : string;
    Platform : TDPMPlatform;
    LatestVersion : TPackageVersion;
    LatestStableVersion : TPackageVersion;
  end;


function TDirectoryPackageRepository.DoGetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const files : IList<string>) : IList<IPackageSearchResultItem>;
var
  searchRegEx : string;
  regex : TRegEx;
  match : TMatch;
  i : integer;
  packageFileName : string;
  id : string;
  platform : TDPMPlatform;
  versionString : string;
  packageVersion : TPackageVersion;
  packageLookup : IDictionary<string, TPackageFind>;
  find : TPackageFind;

  packageMetaData : IPackageMetadata;
  resultItem : IPackageSearchResultItem;

  readSpecFunc : TReadSpecFunc;

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
      platform := StringToDPMPlatform(match.Groups[3].Value);
      versionString := match.Groups[4].Value;
      if not TPackageVersion.TryParse(versionString, packageVersion) then
        continue;

      if not options.Prerelease then
        if not packageVersion.IsStable then
          continue;

      if not packageLookup.TryGetValue(LowerCase(id), find) then
      begin
        find.Id := id;
        find.Platform := platform;
        find.LatestVersion := packageVersion;
        if packageVersion.IsStable then
          find.LatestStableVersion := packageVersion;
        packageLookup[LowerCase(id)] := find;
      end
      else
      begin
        if packageVersion > find.LatestVersion then
          find.LatestVersion := packageVersion;

        if packageVersion.IsStable then
          if packageVersion > find.LatestStableVersion then
            find.LatestStableVersion := packageVersion;
        packageLookup[LowerCase(id)] := find;
      end;
    end;
  end;

  readSpecFunc := function (const name : string; const spec : IPackageSpec) : IInterface
                  begin
                    result := TPackageMetadata.CreateFromSpec(name, spec);
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

    packageFileName := Format('%s-%s-%s-%s.dpkg', [find.Id, CompilerToString(options.CompilerVersion), DPMPlatformToString(find.platform), find.LatestVersion.ToStringNoMeta]);
    packageFileName := IncludeTrailingPathDelimiter(SourceUri) + packageFileName;
    if not FileExists(packageFileName) then
      exit;

    packageMetadata := DoGetPackageMetaData(cancelToken, packageFileName, readSpecFunc) as IPackageMetadata;

    if packageMetadata <> nil then
    begin
      resultItem := TDPMPackageSearchResultItem.FromMetaData(Self.Name, packageMetadata);
      resultItem.LatestVersion := find.LatestVersion;
      resultItem.LatestStableVersion := find.LatestStableVersion;
      result.Add(resultItem);
    end;
  end;
end;

function TDirectoryPackageRepository.DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string; const readSpecFunc : TReadSpecFunc) : IInterface;
var
  zipFile : TZipFile;
  metaBytes : TBytes;
  metaString : string;
  spec : IPackageSpec;
  reader : IPackageSpecReader;
  extractedFile : string;
  svgIconFileName : string;
  pngIconFileName : string;
  iconBytes : TBytes;
  isSvg : boolean;
begin
  Logger.Debug('TDirectoryPackageRepository.DoGetPackageMetaData');
  result := nil;
  reader := TPackageSpecReader.Create(Logger);

  //first see if the spec has been extracted already.
  extractedFile := ChangeFileExt(fileName, '.dspec');
  if FileExists(extractedFile) then
  begin
    spec := reader.ReadSpec(extractedFile);
    if spec <> nil then
    begin
      result := TPackageMetadata.CreateFromSpec(Name, spec);
      exit;
    end;
  end;
  if not FileExists(fileName) then
    exit;

  zipFile := TZipFile.Create;
  try
    try
      zipFile.Open(fileName, TZipMode.zmRead);
      zipFile.Read(cPackageMetaFile, metaBytes);

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
        Logger.Error('Error opening package file [' + fileName + ']');
        exit;
      end;
    end;
  finally
    zipFile.Free;
  end;
  //doing this outside the try/finally to avoid locking the package for too long.
  metaString := TEncoding.UTF8.GetString(metaBytes);
  spec := reader.ReadSpecString(metaString);
  if spec = nil then
    exit;
  result := readSpecFunc(name, spec);
  if not FPermissionsChecked then
  begin
    FIsWritable := TDirectoryUtils.IsDirectoryWriteable(ExtractFilePath(fileName));
    FPermissionsChecked := true;
  end;

  if FIsWritable then
  begin
    try
      //TODO : Could this cause an issue if multiple users attempt this on a shared folder?
      //may need to use a lock file.
      TFile.WriteAllText(extractedFile, metaString, TEncoding.UTF8);
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

function TDirectoryPackageRepository.DoList(searchTerm : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IList<string>;
//var
//  files : TStringDynArray;
//  fileList : IList<string>;
begin
  //  result := TCollections.CreateList<string>;
  if searchTerm <> '*' then
    searchTerm := '*' + searchTerm + '*';

  searchTerm := searchTerm + '-' + CompilerVersionToSearchPart(compilerVersion);

  if platform = TDPMPlatform.UnknownPlatform then
    searchTerm := searchTerm + '-*-*' + cPackageFileExt
  else
    searchTerm := searchTerm + '-' + DPMPlatformToString(platform) + '-*' + cPackageFileExt;

  result := TDirectoryUtils.GetFiles(SourceUri, searchTerm);

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
  platform : TDPMPlatform;
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
  platforms : TDPMPlatforms;
  newItem : IPackageListItem;
  bIsLast : boolean;

  procedure AddCurrent;
  begin
    newItem := TPackageListItem.Create(currentId,currentCompiler, currentVersion, DPMPlatformsToString(platforms));
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

  for i := 0 to Length(searchTerms) - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit;

    if options.Platforms = [] then
    begin
      if options.Exact then
        searchFiles := DoExactList(searchTerms[i], options.CompilerVersion, TDPMPlatform.UnknownPlatform, options.Version.ToStringNoMeta)
      else
        searchFiles := DoList(searchTerms[i], options.CompilerVersion, TDPMPlatform.UnknownPlatform);
      allFiles.AddRange(searchFiles);
    end
    else
    begin
      for platform in options.Platforms do
      begin
        if cancellationToken.IsCancelled then
          exit;

        if options.Exact then
          searchFiles := DoExactList(searchTerms[i], options.CompilerVersion, platform, options.Version.ToStringNoMeta)
        else
          searchFiles := DoList(searchTerms[i], options.CompilerVersion, platform);
        allFiles.AddRange(searchFiles);
      end;
    end;
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
      platform := StringToDPMPlatform(match.Groups[3].Value);
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
          platforms := [platform];
          if bIsLast then
            AddCurrent;
          continue;
        end;
        // a new package, so record the last one if we had one.
        if (currentId <> '') and (currentCompiler <> TCompilerVersion.UnknownVersion) then
        begin
          if bIsLast then
             Include(platforms, platform);
          AddCurrent;
        end;

        currentId := id;
        currentCompiler := cv;
        currentVersion := packageVersion;
        platforms := [platform];
      end
      else
      begin
        Include(platforms, platform);
        if bIsLast then
         AddCurrent;
      end;
    end;
  end;
end;

function TDirectoryPackageRepository.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions): Boolean;
var
  targetFile : string;
begin
  result := false;
  //  if TPath.IsRelativePath(pushOptions.PackagePath) then
  pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    Logger.Error('Package file [' + pushOptions.PackagePath + '] not found.');
    exit;
  end;

  if not DirectoryExists(SourceUri) then
  begin
    Logger.Error('Source uri path does not exist [' + SourceUri + '].');
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
    Logger.Information('Package pushed ok.', true);
    result := true;
  except
    on e : Exception do
    begin
      Logger.Error('Unable to copy file [' + pushOptions.PackagePath + '] to [' + SourceUri + '] : ' + e.Message);
    end;
  end;
end;

end.

