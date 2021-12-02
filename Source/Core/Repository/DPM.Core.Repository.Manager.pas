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

unit DPM.Core.Repository.Manager;

interface

uses
  System.Generics.Defaults,
  Spring.Collections,
  VSoft.Awaitable,
  SVGInterfaces,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TPackageRepositoryManager = class(TInterfacedObject, IPackageRepositoryManager)
  private
    FLogger : ILogger;
    FConfiguration : IConfiguration;
    FRepositories : IList<IPackageRepository>;
    FRepoFactory : IPackageRepositoryFactory;
  protected
    function Initialize(const configuration : IConfiguration) : boolean;
    function HasSources: Boolean;

    function InternalGetLatestVersions(const cancellationToken : ICancellationToken; const installedPackages : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IDictionary<string, TPackageVersion>;


    function GetRepositories : IList<IPackageRepository>;
    function GetRepositoryByName(const value : string) : IPackageRepository;

    function UpdateRepositories : boolean;


    //UI specific stuff
    //TODO : Implement skip/take!
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;


    function GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageId>) : IList<IPackageSearchResultItem>;


    function Find(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform) : IPackageIdentity;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResultItem;
    function GetPackageIcon(const cancelToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                                const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean ) : IList<IPackageInfo>;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;

  public
    constructor Create(const logger : ILogger; const repoFactory : IPackageRepositoryFactory);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Spring.Collections.Extensions,
  VSoft.URI,
  DPM.Core.Constants,
  DPM.Core.Utils.System,
  DPM.Core.Package.SearchResults;

{ TRespositoryManager }

constructor TPackageRepositoryManager.Create(const logger : ILogger; const repoFactory : IPackageRepositoryFactory);
begin
  inherited Create;
  FLogger := logger;
  FRepoFactory := repoFactory;
  FRepositories := TCollections.CreateList<IPackageRepository>;
end;

function TPackageRepositoryManager.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;
var
  repo : IPackageRepository;
begin
  result := false;
  if packageIdentity.SourceName <> '' then
  begin
    repo := GetRepositoryByName(packageIdentity.SourceName);
    if repo = nil then
    begin
      FLogger.Error('Unabled to find repository for source [' + packageIdentity.SourceName + ']');

      exit;
    end;
    FLogger.Information('Downloading package [' + packageIdentity.ToString + '] from [' + repo.Source + ']');
    result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName);
  end
  else
  begin
    for repo in FRepositories do
    begin
      if cancellationToken.IsCancelled then
        exit;
      result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName) or result;
      if result then
        exit;
    end;
  end;
end;

function TPackageRepositoryManager.Find(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform): IPackageIdentity;
begin

end;

function TPackageRepositoryManager.GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageId>) : IList<IPackageSearchResultItem>;
var
  searchOptions : TSearchOptions;
  packageId : IPackageId;
  packageResults : IPackageSearchResult;
  item : IPackageSearchResultItem;
  latestVersions : IDictionary<string, TPackageVersion>;
  latestVersion : TPackageVersion;
  ids : IList<string>;
  platform : TDPMPlatform;
begin
  Assert(FConfiguration <> nil);
  result := TCollections.CreateList<IPackageSearchResultItem>;

  if not installedPackages.Any then
    exit;

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  ids := TCollections.CreateList<string>;

  installedPackages.ForEach(
    procedure(const item : IPackageId)
    begin
      ids.Add(item.Id);
    end);

  platform := installedPackages.First.Platform;

  latestVersions := InternalGetLatestVersions(cancelToken, installedPackages, platform, options.CompilerVersion, options.Prerelease);

  //TODO : This will be really inefficient/slow when using http
  //refactor to make single request to repositories.
//  packageId := installedPackages.FirstOrDefault;
  for packageId in installedPackages do
  begin
    searchOptions := options.Clone;
    searchOptions.SearchTerms := packageId.Id;
    searchOptions.Version := packageId.Version;
    searchOptions.CompilerVersion := packageId.CompilerVersion;
    searchOptions.Platforms := [packageId.Platform];
    searchOptions.Exact := true;

    packageResults := GetPackageFeed(cancelToken, searchOptions, options.CompilerVersion, platform);

    for item in packageResults.Results do
    begin
      item.Installed := true;
//      item.InstalledVersion := packageId.Version.ToStringNoMeta;
      if latestVersions.TryGetValue(item.Id, latestVersion) then
        item.LatestVersion := latestVersion.ToStringNoMeta;
    end;
    result.AddRange(packageResults.Results);
  end;
end;


function TPackageRepositoryManager.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
var
  repo : IPackageRepository;
  sources : TStringList;
  searchResult : IPackageSearchResult;
  allResults : IList<IPackageSearchResultItem>;
  distinctResults : IEnumerable<IPackageSearchResultItem>;
  comparer : IEqualityComparer<IPackageSearchResultItem>;
begin
  Assert(FConfiguration <> nil);
  result := TDPMPackageSearchResult.Create(options.Skip, 0);

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  if options.Sources <> '' then
  begin
    sources := TStringList.Create;
    sources.Delimiter := ',';
    sources.DelimitedText := options.Sources;
  end
  else
    sources := nil;

  allResults := TCollections.CreateList<IPackageSearchResultItem>;

  try
    for repo in FRepositories do
    begin
      if sources <> nil then
      begin
        if sources.IndexOf(repo.Name) = -1 then
          continue;
      end;
      searchResult := repo.GetPackageFeed(cancelToken, options, compilerVersion, platform);
      if options.Take > 0 then
      begin
        if allResults.Count < options.Take then
          allResults.AddRange(searchResult.Results);
      end
      else
        allResults.AddRange(searchResult.Results);
      Result.TotalCount := Result.TotalCount + searchResult.TotalCount;
    end;
  finally
    if sources <> nil then
      sources.Free;
  end;
  comparer := TPackageSearchResultItemComparer.Create;

  distinctResults := TDistinctIterator<IPackageSearchResultItem>.Create(allResults, comparer);
  result.Results.AddRange(distinctResults);


end;

function TPackageRepositoryManager.GetPackageIcon(const cancelToken : ICancellationToken; const source, packageId, packageVersion : string;
                                                  const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
var
  repo : IPackageRepository;
begin
  Assert(FConfiguration <> nil);
  result := nil;

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  repo := GetRepositoryByName(source);
  if repo = nil then
  begin
    FLogger.Error('Unabled to find repository for source [' + source + ']');
    exit;
  end;
  if cancelToken.IsCancelled then
    exit;

  result := repo.GetPackageIcon(cancelToken, packageId, packageVersion, compilerVersion, platform);


end;

function TPackageRepositoryManager.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;
var
  repo : IPackageRepository;
begin
  result := nil;
  Assert(FConfiguration <> nil);

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  for repo in FRepositories do
  begin
    result := repo.GetPackageInfo(cancellationToken, packageId);
    if result <> nil then
      exit;
    if cancellationToken.IsCancelled then
      exit;
  end;
end;


function TPackageRepositoryManager.GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): IPackageSearchResultItem;
var
  repo : IPackageRepository;
begin
  result := nil;
  Assert(FConfiguration <> nil);

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;

  if platform = TDPMPlatform.UnknownPlatform then
  begin
    FLogger.Error('Unabled to search, no platform specified');
    exit;
  end;

  for repo in FRepositories do
  begin
    result := repo.GetPackageMetaData(cancellationToken, packageId, packageVersion, compilerVersion, platform);
    if result <> nil then
      exit;
    if cancellationToken.IsCancelled then
      exit;
  end;
end;

function TPackageRepositoryManager.GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                                      const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>;
var
  repo : IPackageRepository;
  searchResult : IList<TPackageVersion>;
  unfilteredResults : IList<TPackageVersion>;
  distinctResults : IEnumerable<TPackageVersion>;
  comparer : IEqualityComparer<TPackageVersion>;
begin
  Assert(FConfiguration <> nil);
  result := TCollections.CreateList<TPackageVersion>;
  unfilteredResults := TCollections.CreateList<TPackageVersion>;


  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;

  if platform = TDPMPlatform.UnknownPlatform then
  begin
    FLogger.Error('Unabled to search, no platform specified');
    exit;
  end;


  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;


  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;

    searchResult := repo.GetPackageVersions(cancellationToken, packageId, compilerVersion, platform, includePrerelease);
    unfilteredResults.AddRange(searchResult);
  end;

  comparer := TPackageVersionComparer.Create;

  //dedupe
  distinctResults := TDistinctIterator<TPackageVersion>.Create(unfilteredResults, comparer);

  result.AddRange(distinctResults);
  //todo :// which order is this?  want descending.
  result.Sort(function(const Left, Right : TPackageVersion) : Integer
    begin
      result := right.CompareTo(left);
    end);
end;

function TPackageRepositoryManager.InternalGetLatestVersions(const cancellationToken : ICancellationToken; const installedPackages : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IDictionary<string, TPackageVersion>;
var
  repo : IPackageRepository;
  repoResult : IDictionary<string, TPackageVersion>;
  i: Integer;
  repoVersion : TPackageVersion;
  resultVersion : TPackageVersion;
begin
  result := TCollections.CreateDictionary<string, TPackageVersion>;

  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;

    repoResult := repo.GetPackageLatestVersions(cancellationToken, installedPackages, platform, compilerVersion, preRelease);
    if cancellationToken.IsCancelled then
      exit;

    if repoResult.Any then
    begin
      for i := 0 to installedPackages.Count -1 do
      begin
        if repoResult.TryGetValue(installedPackages.Items[i].Id, repoVersion) then
        begin
          if result.TryExtract(installedPackages.Items[i].Id, resultVersion) then
          begin
            if repoVersion > resultVersion then
              result[installedPackages.Items[i].Id] := repoVersion;
          end
          else
            result[installedPackages.Items[i].Id] := repoVersion;
        end;
      end;
    end;
  end;

end;


function TPackageRepositoryManager.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                                      const platform : TDPMPlatform; const packageId : string; const versionRange : TVersionRange;
                                                                      const includePrerelease : boolean) : IList<IPackageInfo>;
var
  repo : IPackageRepository;
  searchResult : IList<IPackageInfo>;
  unfilteredResults : IList<IPackageInfo>;
  distinctResults : IEnumerable<IPackageInfo>;
  comparer : IEqualityComparer<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
  unfilteredResults := TCollections.CreateList<IPackageInfo>;
  Assert(FConfiguration <> nil);

  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;

  if platform = TDPMPlatform.UnknownPlatform then
  begin
    FLogger.Error('Unabled to search, no platform specified');
    exit;
  end;

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;


  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;

    searchResult := repo.GetPackageVersionsWithDependencies(cancellationToken, packageId, compilerVersion, platform, versionRange, includePrerelease);
    unfilteredResults.AddRange(searchResult);
  end;

  comparer := TPackageInfoComparer.Create;

  //dedupe
  distinctResults := TDistinctIterator<IPackageInfo>.Create(unfilteredResults, comparer);

  result.AddRange(distinctResults);
  //todo :// which order is this?  want descending.
  result.Sort(function(const Left, Right : IPackageInfo) : Integer
    begin
      result := right.Version.CompareTo(left.Version); ;
    end);
end;


function TPackageRepositoryManager.GetRepositories : IList<IPackageRepository>;
begin
  result := FRepositories;
end;

function TPackageRepositoryManager.GetRepositoryByName(const value : string) : IPackageRepository;
begin
  result := FRepositories.FirstOrDefault(function(const repo : IPackageRepository) : boolean
    begin
      result := SameText(value, repo.Name);
    end);
end;


function TPackageRepositoryManager.HasSources: Boolean;
begin
  result := FRepositories.Any;
end;

function TPackageRepositoryManager.Initialize(const configuration : IConfiguration) : boolean;
begin
  FConfiguration := configuration;
  result := UpdateRepositories;
end;

function TPackageRepositoryManager.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
var
  repo : IPackageRepository;
  info, prevInfo : IPackageListItem;

  searchResult : IList<IPackageListItem>;
  unfilteredResults : IList<IPackageListItem>;
  distinctResults : IEnumerable<IPackageListItem>;
  sortFunc : TComparison<IPackageListItem>;
  packages : IList<IPackageListItem>;
  sources : TStringList;
begin
  Assert(FConfiguration <> nil);

  result := TCollections.CreateList<IPackageListItem>;

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  if options.Sources <> '' then
  begin
    sources := TStringList.Create;
    sources.Delimiter := ',';
    sources.DelimitedText := options.Sources;
  end
  else
    sources := nil;

  unfilteredResults := TCollections.CreateList<IPackageListItem>;
  //create the list here as if there are no repos it will be nil.
  searchResult := TCollections.CreateList<IPackageListItem>;

  try
    for repo in FRepositories do
    begin
      if sources <> nil then
      begin
        if sources.IndexOf(repo.Name) = -1 then
          continue;
      end;
      searchResult := repo.List(cancellationToken, options);
      unfilteredResults.AddRange(searchResult);
      if cancellationToken.IsCancelled then
        exit;
    end;
  finally
    if sources <> nil then
      sources.Free;
  end;
  if cancellationToken.IsCancelled then
    exit;

  sortFunc := function(const Left, Right : IPackageListItem) : Integer
  begin
    result := CompareText(left.Id, right.Id);
    if result = 0 then
    begin
      result := right.Version.CompareTo(left.Version);
      if result = 0 then
      begin
        result := Ord(left.CompilerVersion) - Ord(right.CompilerVersion);
//        if result = 0 then
//        begin
//          result := Ord(left.Platform) - Ord(right.Platform);
//        end;
      end;
    end;
  end;

  //what does this do? if we don't provide a comparer?
  distinctResults := TDistinctIterator<IPackageListItem>.Create(unfilteredResults, nil);
  searchResult.Clear;
  searchResult.AddRange(distinctResults);

  //Sort by id, version, platform.
  searchResult.Sort(sortFunc);

  packages := TCollections.CreateList<IPackageListItem>;

//  if not options.AllVersions then
//  begin
//    prevInfo := nil;
//    for info in searchResult do
//    begin
//      if cancellationToken.IsCancelled then
//        exit;
//
//      if (prevInfo = nil) or (info.id <> prevInfo.id) then
//      begin
//        prevInfo := info;
//        packages.Add(info);
//        continue;
//      end;
//      //if it's the same id, then compilerversion or platform must be different
//      if (info.id = prevInfo.id) then
//      begin
//        if (info.Version = prevInfo.Version) and ((info.CompilerVersion <> prevInfo.CompilerVersion)) {or (info.Platform <> prevInfo.Platform))} then
//        begin
//          prevInfo := info;
//          packages.Add(info);
//        end;
//      end;
//    end;
//  end
//  else
    packages.AddRange(searchResult);

    //skip and take are not working because of how we are rolling up the results.
//  if (options.Skip > 0) or (options.Take > 0) then
//  begin
//    distinctResults := packages;
//    if options.Skip > 0 then
//      distinctResults := distinctResults.Skip(options.Skip);
//    if options.Take > 0 then
//      distinctResults := distinctResults.Take(options.Take);
//
//    result.AddRange(distinctResults);
//  end
//  else
    result.AddRange(packages);
end;

function TPackageRepositoryManager.Push(const cancellationToken: ICancellationToken; const pushOptions: TPushOptions): Boolean;
var
  repo : IPackageRepository;
begin
  Assert(FConfiguration <> nil);
  result := false;
  if cancellationToken.IsCancelled then
    exit;

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to push, error loading source repositories');
    exit;
  end;

  repo := FRepositories.Where(
    function(const repository : IPackageRepository) : Boolean
    begin
      result := SameText(pushOptions.Source, repository.Name);
    end).FirstOrDefault;

  if repo = nil then
  begin
    FLogger.Error('Unknown Source : ' + pushOptions.Source);
    exit;
  end;

  result := repo.Push(cancellationToken, pushOptions);


end;

function TPackageRepositoryManager.UpdateRepositories : boolean;
var
  uri : IUri;
  error : string;
  source : ISourceConfig;
  repo : IPackageRepository;
begin
  result := false;
  FRepositories.Clear;
  for source in FConfiguration.Sources do
  begin
    if not source.IsEnabled then
      continue;

    if not TUriFactory.TryParseWithError(source.Source, false, uri, error) then
    begin
      FLogger.Error('Invalid source uri : ' + source.Source);
      exit;
    end;

    if not MatchText(uri.Scheme, ['file', 'http', 'https']) then
    begin
      FLogger.Error('Invalid source uri scheme : ' + uri.Scheme);
      exit;
    end;

    repo := FRepoFactory.CreateRepository(source.SourceType);
    repo.Configure(source);
    FRepositories.Add(repo);
  end;
  result := true;
end;

end.

