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

unit DPM.Core.Repository.Manager;

interface

uses
  System.Generics.Defaults,
  Spring.Collections,
  VSoft.CancellationToken,
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

//    function InternalGetLatestVersions(const cancellationToken : ICancellationToken; const installedPackages : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IDictionary<string, IPackageLatestVersionInfo>;


    function GetRepositories : IList<IPackageRepository>;
    function GetRepositoryByName(const value : string) : IPackageRepository;

    function UpdateRepositories : boolean;


    //UI specific stuff
    //TODO : Implement skip/take!
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
    function GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
    //
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResultItem;


    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean; const sources : string) : IPackageInfo;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
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

function TPackageRepositoryManager.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.DownloadPackage');
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
    //FLogger.Information('Downloading package [' + packageIdentity.ToString + '] from [' + repo.Name + ']');
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

function TPackageRepositoryManager.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const platform: TDPMPlatform; const includePrerelease : boolean; const sources : string): IPackageInfo;
var
  repo : IPackageRepository;
  sourcesList : TStringList;
//  i : integer;
  packages : IList<IPackageInfo>;
  package : IPackageInfo;
  currentPackage : IPackageInfo;

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

  currentPackage := nil;
  packages := TCollections.CreateList<IPackageInfo>;
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
      package := repo.FindLatestVersion(cancellationToken, id, compilerVersion, version, platform, includePrerelease);
      if cancellationToken.IsCancelled then
        exit;

      if package <> nil then
      begin

        if not version.IsEmpty then
        begin
          if package.Version = version then //we found what we are looking for
            exit(package);
        end;
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

function TPackageRepositoryManager.GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
var
  packageResults : IPackageSearchResult;
  packages : IDictionary<string, IPackageSearchResultItem>;
  package : IPackageSearchResultItem;
  platform : TDPMPlatform;
  repo : IPackageRepository;
  i : integer;
  id : string;
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

  if installedPackages.Count = 0 then
    exit;

  packages := TCollections.CreateDictionary<string, IPackageSearchResultItem>;
    
  //always going to be all the same platform
  platform := installedPackages[0].Platform;


  //loop through enabled repo and get the feed.. then combine to get latest versions
  for repo in FRepositories do
  begin
    if not repo.Enabled then
      continue;
    if cancelToken.IsCancelled then
      exit;
      
    packageResults := repo.GetPackageFeedByIds(cancelToken, installedPackages, options.CompilerVersion, platform ); 
    for i := 0 to packageResults.Results.Count -1 do
    begin
      if cancelToken.IsCancelled then
        exit;

      id := LowerCase(packageResults.Results[i].Id);
      if packages.ContainsKey(id) then
      begin
        package := packages[id]; //don't use extract!
        //if we already have this package from another repo then compare versions
        if packageResults.Results[i].LatestVersion > package.LatestVersion then
          package.LatestVersion := packageResults.Results[i].LatestVersion;
        if packageResults.Results[i].LatestStableVersion > package.LatestStableVersion then
          package.LatestStableVersion := packageResults.Results[i].LatestStableVersion;
      end
      else
        packages[id] := packageResults.Results[i];
    end;
  end;

  result := TCollections.CreateList<IPackageSearchResultItem>;
  result.AddRange(packages.Values);
  result.Sort(
    function(const left, right : IPackageSearchResultItem) : integer
    begin
      result := CompareText(left.Id, right.id);
    end);

end;


function TPackageRepositoryManager.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
var
  repo : IPackageRepository;
  sources : TStringList;
  searchResult : IPackageSearchResult;
  allResults : IList<IPackageSearchResultItem>;
  distinctResults : IList<IPackageSearchResultItem>;
  i: Integer;
  currentResult : IPackageSearchResultItem;
  nextResult : IPackageSearchResultItem;
  count : integer;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageFeed');

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
      allResults.AddRange(searchResult.Results);
    end;
  finally
    if sources <> nil then
      sources.Free;
  end;

  //sort by id so we can pick out the highest version
  allresults.Sort(
    function(const Left, Right : IPackageSearchResultItem) : integer
    begin
      result := CompareText(Left.Id, Right.Id);
    end);


  distinctResults := TCollections.CreateList<IPackageSearchResultItem>;
  currentResult := nil;
  count := allResults.Count;
  for i := 0 to count -1 do
  begin
    nextResult := allResults.Items[i];
    if currentResult = nil then
      currentResult := nextResult
    else if SameText(currentResult.Id, nextResult.Id) then
    begin
      if nextResult.Version > currentResult.Version then
        currentResult := nextResult;
    end
    else
    begin
      //new package id, save current
      distinctResults.Add(currentResult);
      currentResult := nextResult;
    end;
    //last one so just add it
    if i = count -1 then
      distinctResults.Add(currentResult);
  end;

  if options.Take > 0 then
    result.Results.AddRange(distinctResults.Take(options.Take))
  else
    result.Results.AddRange(distinctResults);

  result.TotalCount := result.Results.Count;

end;

function TPackageRepositoryManager.GetPackageIcon(const cancelToken : ICancellationToken; const source, packageId, packageVersion : string;
                                                  const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageIcon');
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

function TPackageRepositoryManager.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageInfo');
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
  FLogger.Debug('TPackageRepositoryManager.GetPackageMetaData');
  result := nil;
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
    FLogger.Error('Unabled to search, error loading repositories');
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
  FLogger.Debug('TPackageRepositoryManager.GetPackageVersions');
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

//function TPackageRepositoryManager.InternalGetLatestVersions(const cancellationToken : ICancellationToken; const installedPackages : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IDictionary<string, IPackageLatestVersionInfo>;
//var
//  repo : IPackageRepository;
//  repoResult : IDictionary<string, IPackageLatestVersionInfo>;
//  i: Integer;
//  repoVersion : IPackageLatestVersionInfo;
//  resultVersion : IPackageLatestVersionInfo;
//begin
//  result := TCollections.CreateDictionary<string, IPackageLatestVersionInfo>;
//
//  for repo in FRepositories do
//  begin
//    if cancellationToken.IsCancelled then
//      exit;
//    if not repo.Enabled then
//      continue;
//
//    repoResult := repo.GetPackageLatestVersions(cancellationToken, installedPackages, platform, compilerVersion);
//    if cancellationToken.IsCancelled then
//      exit;
//
//    if repoResult.Any then
//    begin
//      for i := 0 to installedPackages.Count -1 do
//      begin
//        if repoResult.TryGetValue(installedPackages.Items[i].Id, repoVersion) then
//        begin
//          if result.TryExtract(installedPackages.Items[i].Id, resultVersion) then
//          begin
//            if repoVersion.LatestVersion > resultVersion.LatestVersion then
//              resultVersion.LatestVersion := repoVersion.LatestVersion;
//            if repoVersion.LatestStableVersion > resultVersion.LatestStableVersion then
//              resultVersion.LatestStableVersion := repoVersion.LatestStableVersion;
//          end
//          else
//            result[installedPackages.Items[i].Id] := repoVersion
//        end;
//      end;
//    end;
//  end;
//
//end;


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
  FLogger.Debug('TPackageRepositoryManager.GetPackageVersionsWithDependencies');
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
  //FLogger.Debug('TPackageRepositoryManager.UpdateRepositories');
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

