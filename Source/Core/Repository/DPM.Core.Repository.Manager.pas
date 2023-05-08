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


    function Find(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean; const sources : string) : IPackageIdentity;



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
  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;

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
      if not repo.Enabled then
        continue;
      if cancellationToken.IsCancelled then
        exit;
      result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName) or result;
      if result then
        exit;
    end;
  end;
end;

function TPackageRepositoryManager.Find(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform; const includePrerelease : boolean; const sources : string): IPackageIdentity;
var
  repo : IPackageRepository;
  sourcesList : TStringList;
//  i : integer;
  packages : IList<IPackageIdentity>;
  package : IPackageIdentity;
  currentPackage : IPackageIdentity;

begin
  Assert(FConfiguration <> nil);

  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  if sources <> '' then
  begin
    sourcesList := TStringList.Create;
    sourcesList.Delimiter := ',';
    sourcesList.DelimitedText := sources;
  end
  else
    sourcesList := nil;

  packages := TCollections.CreateList<IPackageIdentity>;
  try
    for repo in FRepositories do
    begin
      if cancellationToken.IsCancelled then
        exit;
      if not repo.Enabled then
        continue;

      if sourcesList <> nil then
      begin
        if sourcesList.IndexOf(repo.Name) = -1 then
          continue;
      end;
      package := repo.Find(cancellationToken, id, compilerVersion, version, platform, includePrerelease);
      if cancellationToken.IsCancelled then
        exit;

      if package <> nil then
      begin
        //if version is empty then we want the latest version - so that's what we have, we're done.
        if version.IsEmpty then
          exit(package);
        if currentPackage <> nil then
        begin
          if package.Version > currentPackage.Version then
            currentPackage := package;
        end
        else
          currentPackage := package;
      end;

      result := currentPackage;
    end;
  finally
    if sourcesList <> nil then
      sourcesList.Free;
  end;



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
  Assert(installedPackages <> nil);

  result := TCollections.CreateList<IPackageSearchResultItem>;
  //nothing to do so justr return empty list.
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
      if latestVersions.TryGetValue(item.Id, latestVersion) then
        item.LatestVersion := latestVersion;
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
      if cancelToken.IsCancelled then
        exit;
      if not repo.Enabled then
        continue;


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
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

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
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

    result := repo.GetPackageMetaData(cancellationToken, packageId, packageVersion, compilerVersion, platform);
    if result <> nil then
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
  result := TCollections.CreateList<TPackageVersion>;
  Assert(FConfiguration <> nil);
  if not UpdateRepositories then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;

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


  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

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
    if not repo.Enabled then
      continue;

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
    if not repo.Enabled then
      continue;

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

  searchResult : IList<IPackageListItem>;
  unfilteredResults : IList<IPackageListItem>;
  sortFunc : TComparison<IPackageListItem>;
  sources : TStringList;
  item, currentItem : IPackageListItem;
  i : integer;
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
      if cancellationToken.IsCancelled then
        exit;
      if not repo.Enabled then
        continue;

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
      result := Ord(left.CompilerVersion) - Ord(right.CompilerVersion);
      if result = 0 then
      begin
        result := right.Version.CompareTo(left.Version);
        if result = 0 then
          result := CompareText(left.Platforms, right.Platforms);
      end;
    end;
  end;

  unfilteredResults.Sort(sortFunc);
  searchResult.Clear;

  //we need to dedupe here.
  currentItem := nil;
  for i := 0 to unfilteredResults.Count -1  do
  begin
    item := unfilteredResults[i];
    if (currentItem <> nil) then
    begin
      if currentItem.IsSamePackageVersion(item) then
        currentItem := currentItem.MergeWith(item)
      else if currentItem.IsSamePackageId(item) then
      begin
        //not the same version but same compiler/packageid, so take highest version
        if item.Version > currentItem.Version then
          currentItem := item;
      end
      else
      begin //different package or compiler verison.
        searchResult.Add(currentItem);
        currentItem := item;
      end;
    end
    else
      currentItem := item;
  end;
  if currentItem <> nil then
    searchResult.Add(currentItem);

  //Sort by id, version, platform.
  searchResult.Sort(sortFunc);
  result.AddRange(searchResult);
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

  if not repo.Enabled then
  begin
    FLogger.Error('Source not enabled : ' + pushOptions.Source);
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
  i : integer;
begin
  result := false;

  //update and remove
  for i := FRepositories.Count -1 downto 0 do
  begin
    repo := FRepositories[i];
    source := FConfiguration.GetSourceByName(repo.Name);
    if source <> nil then
      repo.Configure(source)
    else
      FRepositories.Remove(repo);
  end;

  //add missing repos.
  for source in FConfiguration.Sources do
  begin
    repo := GetRepositoryByName(source.Name);
    if repo = nil then
    begin
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
  end;
  result := true;
end;

end.

