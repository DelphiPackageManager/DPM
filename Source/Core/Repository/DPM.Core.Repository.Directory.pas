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
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base;

//TODO : makes this work for structured folders too
//eg : \Vsoft.AntPattterns\Win32
//     \Vsoft.AntPatterns\Win64
// see https://docs.microsoft.com/en-au/nuget/reference/cli-reference/cli-ref-add


type
  TDirectoryPackageRepository = class(TBaseRepository, IPackageRepository)
  private
    FPermissionsChecked : boolean;
    FIsWritable : boolean;
  protected
    function DoGetPackageInfo(const cancellationToken : ICancellationToken; const fileName : string) : IPackageInfo; overload;
    function DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string) : IPackageMetadata;


    function DoList(searchTerm : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IList<string>;
    function DoExactList(const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const version : string) : IList<string>;

    function DoGetPackageFeedFiles(const options : TSearchOptions; searchTerm : string) : IList<string>;
    function DoGetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const files : IList<string>) : IList<IPackageSearchResultItem>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageIdentity>; overload;

    function DownloadPackage(const cancellationToken : ICancellationToken; const packageMetadata : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : Boolean) : IList<IPackageInfo>;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo; overload;

    function GetPackageLatestVersions(const cancellationToken : ICancellationToken; const ids : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IDictionary<string, TPackageVersion>;


    //ui stuff
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>; overload;
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration) : IList<IPackageSearchResultItem>;
    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;

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
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Directory,
  DPM.Core.Package.SearchResults;

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
  sourceFile := IncludeTrailingPathDelimiter(Source) + packageMetadata.ToString + cPackageFileExt;
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
    TFile.Copy(sourceFile, destFile, true);
    fileName := destFile;
    result := true;
  except
    on e : Exception do
    begin
      Logger.Error('Error downloading  [' + sourceFile + '] to [' + destFile + ']');
    end;
  end;
end;
//
//function TDirectoryPackageRepository.GetPackageMetaData(const packageIdentity : IPackageIdentity): IPackageMetadata;
//begin
//  result := DoGetPackageMetaData(packageIdentity, packageIdentity.Platform);
//end;

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
  packagFileName := TPath.Combine(Self.Source, packagFileName);
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
  packagFileName : string;
begin
  result := nil;
  packagFileName := Format('%s-%s-%s-%s.dpkg', [packageId.Id, CompilerToString(packageId.CompilerVersion), DPMPlatformToString(packageId.Platform), packageId.Version.ToStringNoMeta]);
  packagFileName := IncludeTrailingPathDelimiter(Source) + packagFileName;
  if not FileExists(packagFileName) then
    exit;
  if cancellationToken.IsCancelled then
    exit;

  result := DoGetPackageInfo(cancellationToken, packagFileName);
end;


function TDirectoryPackageRepository.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration) : IList<IPackageSearchResultItem>;
var
  searchTerms : TArray<string>;
  i : integer;
  //  repoResults : IList<IPackageSearchResultItem>;
  searchFiles : IList<string>;
  allFiles : IList<string>;
  distinctFiles : IEnumerable<string>;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;
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
    allFiles.AddRange(DoGetPackageFeedFiles(options, searchTerms[i]));
  end;

  distinctFiles := TDistinctIterator < string > .Create(allFiles, TStringComparer.OrdinalIgnoreCase);
  if options.Skip > 0 then
    distinctFiles := distinctFiles.Skip(options.Skip);
  if options.Take > 0 then
    distinctFiles := distinctFiles.Take(options.Take);
  searchFiles := TCollections.CreateList < string > (distinctFiles);

  result := DoGetPackageFeed(cancelToken, options, searchFiles);


end;


function TDirectoryPackageRepository.DoGetPackageInfo(const cancellationToken : ICancellationToken; const fileName : string) : IPackageInfo;
var
  zipFile : TZipFile;
  metaBytes : TBytes;
  metaString : string;
  spec : IPackageSpec;
  reader : IPackageSpecReader;
  extractedFile : string;
begin
  reader := TPackageSpecReader.Create(Logger);

  //first see if the spec has been extracted already.
  extractedFile := ChangeFileExt(fileName, '.dspec');
  if FileExists(extractedFile) then
  begin
    spec := reader.ReadSpec(extractedFile);
    if spec <> nil then
    begin
      result := TPackageInfo.CreateFromSpec(Name, spec);
      exit;
    end;

  end;
  //failed to read so try extract again

  result := nil;
  zipFile := TZipFile.Create;
  try
    try
      zipFile.Open(fileName, TZipMode.zmRead);
      zipFile.Read(cPackageMetaFile, metaBytes);
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
  result := TPackageInfo.CreateFromSpec(Name, spec);
  if not FPermissionsChecked then
  begin
    FIsWritable := TDirectoryUtils.IsDirectoryWriteable(ExtractFilePath(fileName));
    FPermissionsChecked := true;
  end;

  if FIsWritable then
  begin
    try
      TFile.WriteAllText(extractedFile, metaString, TEncoding.UTF8);
    except
      //even though we test for write access other errors might occur (eg with network drives disconnected etc).
    end;
  end;
end;

function TDirectoryPackageRepository.GetPackageLatestVersions(const cancellationToken: ICancellationToken; const ids: IList<IPackageId>; const platform: TDPMPlatform; const compilerVersion: TCompilerVersion; const preRelease: boolean): IDictionary<string, TPackageVersion>;
var
  i : integer;
  j : integer;
  id : string;
  searchFiles : IList<string>;
  regex : TRegEx;
  searchRegex : string;
  packageFile : string;
  match : TMatch;
  packageVersion : TPackageVersion;
  highestVersion : TPackageVersion;
begin
  result := TCollections.CreateDictionary<string, TPackageVersion>;

  searchRegEx := GetSearchRegex(compilerVersion, [], '');
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);

  for i := 0 to ids.Count -1 do
  begin
    id := ids.Items[i].Id;
    searchFiles := DoExactList(id, compilerVersion, platform, '');
    for j := 0 to searchFiles.Count -1 do
    begin
      packageFile := ChangeFileExt(ExtractFileName(searchFiles[j]), '');
      match := regex.Match(packageFile);
      if match.Success then
      begin
        if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
          continue;

        if ids.Items[i].Version.IsStable then
          if (not preRelease) and (not packageVersion.IsStable)  then
            continue;

        if result.TryGetValue(id, highestVersion) then
        begin
          if packageVersion > highestVersion then
          begin
            highestVersion := packageVersion;
            result[id] := highestVersion;
          end;
        end
        else
        begin
          highestVersion := packageVersion;
          result[id] := highestVersion;
        end;
      end;
    end;
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

//  result.Sort(function(const Left, Right: TPackageVersion): Integer
//              begin
//                   result := right.CompareTo(left);
//              end);

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
begin
  result := TCollections.CreateList<IPackageInfo>;

  searchFiles := DoExactList(id, compilerVersion, platform, '');

  searchRegEx := GetSearchRegex(compilerVersion, [platform], '');
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

      //todo : check this is correct.
      if not versionRange.Satisfies(packageVersion) then
        continue;

      if not prerelease then
        if not packageVersion.IsStable then
          continue;

      packageInfo := DoGetPackageInfo(cancellationToken, searchFiles[i]);
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
    fileList := TDirectoryUtils.GetFiles(Source, searchTerm);
    //dedupe
    result.AddRange(TEnumerable.Distinct<string>(fileList, TStringComparer.OrdinalIgnoreCase));
  except
    on e : Exception do
    begin
      Logger.Error('Error searching source [' + Name + '] : ' + e.Message);
    end;
  end;
end;


type
  TPackageFind = record
    Id : string;
    Platforms : TDPMPlatforms;
    LatestVersion : TPackageVersion;
  end;


function TDirectoryPackageRepository.DoGetPackageFeedFiles(const options : TSearchOptions; searchTerm : string) : IList<string>;
var
  //  files : TStringDynArray;
  platform : TDPMPlatform;
  platformSearchTerm : string;
begin
  result := TCollections.CreateList < string > ;
  if not options.Exact then
  begin
    if searchTerm <> '*' then
      searchTerm := '*' + searchTerm + '*';
  end;
  searchTerm := searchTerm + '-' + CompilerVersionToSearchPart(options.CompilerVersion);
  if options.Platforms = [] then
  begin
    if options.Version.IsEmpty then
      searchTerm := searchTerm + '-*-*' + cPackageFileExt
    else
      searchTerm := searchTerm + '-*-' + options.Version.ToStringNoMeta + cPackageFileExt;

    result := TDirectoryUtils.GetFiles(Source, searchTerm);
    //result.AddRange(files);
  end
  else
  begin
    for platform in options.Platforms do
    begin
      platformSearchTerm := searchTerm + '-' + DPMPlatformToString(platform) + '-';
      if options.Version.IsEmpty then
        platformSearchTerm := platformSearchTerm + '*' + cPackageFileExt
      else
        platformSearchTerm := platformSearchTerm + options.Version.ToStringNoMeta + cPackageFileExt;

      result := TDirectoryUtils.GetFiles(Source, platformSearchTerm);
      //result.AddRange(files);
    end;
  end;

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

  packagePlatformDependencies : IList<IPackagePlatformDependencies>;
  platformDependencies : IPackagePlatformDependencies;
  resultItem : IPackageSearchResultItem;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;

  packageLookup := TCollections.CreateDictionary < string, TPackageFind > ;

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
        find.Platforms := [platform];
        find.LatestVersion := packageVersion;
        packageLookup[LowerCase(id)] := find;
      end
      else
      begin

        if packageVersion = find.LatestVersion then
        begin
          //same version, just add to the platforms
          find.Platforms := find.Platforms + [platform];
        end
        else if packageVersion > find.LatestVersion then
        begin
          //start again with a new latest version
          find.LatestVersion := packageVersion;
          find.Platforms := [platform];
        end
        else
          continue; //lower version, ignore.
        packageLookup[LowerCase(id)] := find;
      end;
    end;
  end;

  //now we can use the info collected above to build actual results.
  for find in packageLookup.Values do
  begin
    if cancelToken.IsCancelled then
      exit;

    packagePlatformDependencies := TCollections.CreateList<IPackagePlatformDependencies>;
    resultItem := nil;
    packageMetaData := nil;
    for platform in find.Platforms do
    begin
      if cancelToken.IsCancelled then
        exit;

      packageFileName := Format('%s-%s-%s-%s.dpkg', [find.Id, CompilerToString(options.CompilerVersion), DPMPlatformToString(platform), find.LatestVersion.ToStringNoMeta]);
      packageFileName := IncludeTrailingPathDelimiter(Source) + packageFileName;
      if not FileExists(packageFileName) then
        exit;
      //Logger.Debug('Checking package file [' + packageFileName + ']');

      packageMetadata := DoGetPackageMetaData(cancelToken, packageFileName);

      if (packageMetadata <> nil) and packageMetadata.Dependencies.Any then
      begin
        platformDependencies := TDPMPackagePlatformDependencies.Create(platform, packageMetadata.Dependencies);
        packagePlatformDependencies.Add(platformDependencies);
      end;
    end;

    if packageMetadata <> nil then
    begin
      resultItem := TDPMPackageSearchResultItem.FromMetaData(Self.Name, packageMetadata, find.Platforms, packagePlatformDependencies);
      result.Add(resultItem);
    end;
  end;
end;

function TDirectoryPackageRepository.DoGetPackageMetaData(const cancellationToken : ICancellationToken; const fileName : string) : IPackageMetadata;
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
  result := TPackageMetadata.CreateFromSpec(Name, spec);
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

  result := TDirectoryUtils.GetFiles(Source, searchTerm);

  //fileList := TCollections.CreateList<string>(files);

  //TODO : Does this really need to be de-duped?

  //dedupe
  //result.AddRange(TEnumerable.Distinct<string>(fileList, TStringComparer.OrdinalIgnoreCase ));
end;

function TDirectoryPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageIdentity>;
var
  searchTerms : TArray<string>;
  searchFiles : IList<string>;
  allFiles : IList<string>;
  distinctFiles : IEnumerable<string>;
  i : integer;
  platform : TDPMPlatform;
  info : IPackageIdentity;
  searchRegEx : string;
  regex : TRegEx;
  packageFile : string;
  match : TMatch;
  id : string;
  cv : TCompilerVersion;
  version : string;
  packageVersion : TPackageVersion;
begin
  result := TCollections.CreateList<IPackageIdentity>;
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
        if options.Exact then
          searchFiles := DoExactList(searchTerms[i], options.CompilerVersion, platform, options.Version.ToStringNoMeta)
        else
          searchFiles := DoList(searchTerms[i], options.CompilerVersion, platform);
        allFiles.AddRange(searchFiles);
      end;
    end;
  end;

  //dedupe
  distinctFiles := TDistinctIterator<string>.Create(allFiles, TStringComparer.OrdinalIgnoreCase);
  if options.Skip > 0 then
    distinctFiles := distinctFiles.Skip(options.Skip);
  if options.Take > 0 then
    distinctFiles := distinctFiles.Take(options.Take);
  searchFiles.Clear;
  searchFiles.AddRange(distinctFiles);


  //do we really need to do a regex check here?
  searchRegEx := GetSearchRegex(options.CompilerVersion, options.Platforms, options.Version.ToStringNoMeta);
  regex := TRegEx.Create(searchRegEx, [roIgnoreCase]);
  for i := 0 to searchFiles.Count - 1 do
  begin
    //ensure that the files returned are actually packages

    packageFile := ChangeFileExt(ExtractFileName(searchFiles[i]), '');
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
      //not using the trycreate here as we need a specific regex.
      info := TPackageIdentity.Create(id, Name, packageVersion, cv, platform, '');
      result.Add(info);
    end;
  end;

end;

end.

