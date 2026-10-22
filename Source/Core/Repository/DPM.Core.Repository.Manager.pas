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
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.RequestCache;

type
  TPackageRepositoryManager = class(TInterfacedObject, IPackageRepositoryManager)
  private
    FLogger : ILogger;
    FConfiguration : IConfiguration;
    FRepositories : IList<IPackageRepository>;
    FRepoFactory : IPackageRepositoryFactory;
    FRequestCache : TRepositoryRequestCache;
    //the sources the current query could not use - see UnavailableSources.
    FUnavailable : string;
    //starts a query : forgets which sources failed the last one.
    procedure BeginRequest;
    //after asking a repository : a source that failed a query is a warning - another source may have the
    //answer, and the caller decides whether finding nothing is an error.
    procedure NoteUnavailable(const repo : IPackageRepository);
  protected
    function Initialize(const configuration : IConfiguration) : boolean;
    function HasSources: Boolean;
    function UnavailableSources : string;

//    function InternalGetLatestVersions(const cancellationToken : ICancellationToken; const installedPackages : IList<IPackageId>; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IDictionary<string, IPackageLatestVersionInfo>;


    function GetRepositories : IList<IPackageRepository>;
    function GetRepositoryByName(const value : string) : IPackageRepository;

    //report : Initialize says why a source is not valid - every other caller just stops.
    function UpdateRepositories(const report : boolean = false) : boolean;


    //UI specific stuff
    //TODO : Implement skip/take!
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
    //
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;


    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean; const sources : string) : IPackageInfo;



    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
    function TryGetSourceType(const sourceName : string; out sourceType : TSourceType) : boolean;
    function InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageIcon(const cancelToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean ) : IList<IPackageInfo>;

    procedure BeginRequestCache;
    procedure EndRequestCache;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;

  public
    constructor Create(const logger : ILogger; const repoFactory : IPackageRepositoryFactory);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Spring.Collections.Extensions,
  VSoft.URI,
  DPM.Core.Diagnostics,
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
  FRequestCache := TRepositoryRequestCache.Create;
end;

destructor TPackageRepositoryManager.Destroy;
begin
  FRequestCache.Free;
  inherited;
end;

procedure TPackageRepositoryManager.BeginRequestCache;
begin
  FRequestCache.BeginScope;
end;

procedure TPackageRepositoryManager.EndRequestCache;
begin
  FRequestCache.EndScope;
end;

function TPackageRepositoryManager.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
var
  repo : IPackageRepository;
  failures : string;
  failureCode : TErrorCode;
begin
  FLogger.Debug('TPackageRepositoryManager.DownloadPackage');
  result := false;
  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  if packageIdentity.SourceName <> '' then
  begin
    repo := GetRepositoryByName(packageIdentity.SourceName);
    if repo = nil then
    begin
      FLogger.Error(TErrorCode.E1003_UnknownSource, 'Unable to download [' + packageIdentity.ToString + '] - its source [' + packageIdentity.SourceName + '] is not configured.');
      exit;
    end;
    result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName);
    if result or cancellationToken.IsCancelled then
      exit;
    if repo.LastError <> '' then
      FLogger.Error(repo.LastErrorCode, 'Unable to download [' + packageIdentity.ToString + '] - source [' + repo.Name + '] ' + repo.LastError)
    else
      FLogger.Error(TErrorCode.E1201_PackageDownloadFailed, 'Unable to download [' + packageIdentity.ToString + '] - it was not found on source [' + repo.Name + '].');
    exit;
  end;

  //try each source - one that fails only matters if no other has the package.
  failures := '';
  failureCode := TErrorCode.E1201_PackageDownloadFailed;
  for repo in FRepositories do
  begin
    if not repo.Enabled then
      continue;
    if cancellationToken.IsCancelled then
      exit;
    result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName);
    if result then
      break;
    if repo.LastError <> '' then
    begin
      if failures <> '' then
        failures := failures + '; ';
      failures := failures + 'source [' + repo.Name + '] ' + repo.LastError;
      failureCode := repo.LastErrorCode;
    end;
  end;
  if cancellationToken.IsCancelled then
    exit;
  if result then
  begin
    if failures <> '' then
      FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Downloaded [' + packageIdentity.ToString + '], but ' + failures);
    exit;
  end;
  if failures <> '' then
    FLogger.Error(failureCode, 'Unable to download [' + packageIdentity.ToString + '] - ' + failures)
  else
    FLogger.Error(TErrorCode.E1201_PackageDownloadFailed, 'Unable to download [' + packageIdentity.ToString + '] - it was not found on any source.');
end;

function TPackageRepositoryManager.FindLatestVersion(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const version: TPackageVersion; const includePrerelease : boolean; const sources : string): IPackageInfo;
var
  repo : IPackageRepository;
  sourcesList : TStringList;
//  i : integer;
  packages : IList<IPackageInfo>;
  package : IPackageInfo;
  currentPackage : IPackageInfo;

begin
  Assert(FConfiguration <> nil);

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

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
      package := repo.FindLatestVersion(cancellationToken, id, compilerVersion, version, includePrerelease);
      NoteUnavailable(repo);
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

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  if installedPackages.Count = 0 then
    exit;

  packages := TCollections.CreateDictionary<string, IPackageSearchResultItem>;

  //loop through enabled repo and get the feed.. then combine to get latest versions
  for repo in FRepositories do
  begin
    if not repo.Enabled then
      continue;
    if cancelToken.IsCancelled then
      exit;
      
    packageResults := repo.GetPackageFeedByIds(cancelToken, installedPackages, options.CompilerVersion );
    NoteUnavailable(repo);
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


function TPackageRepositoryManager.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
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

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

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
      searchResult := repo.GetPackageFeed(cancelToken, options, compilerVersion);
      NoteUnavailable(repo);
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
                                                  const compilerVersion : TCompilerVersion) : IPackageIcon;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageIcon');
  Assert(FConfiguration <> nil);
  result := nil;

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  repo := GetRepositoryByName(source);
  if repo = nil then
  begin
    FLogger.Debug('No repository for source [' + source + '] - no icon.');
    exit;
  end;
  if cancelToken.IsCancelled then
    exit;

  result := repo.GetPackageIcon(cancelToken, packageId, packageVersion, compilerVersion);


end;

function TPackageRepositoryManager.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageInfo');
  result := nil;
  Assert(FConfiguration <> nil);

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

    result := repo.GetPackageInfo(cancellationToken, packageId);
    NoteUnavailable(repo);
    if result <> nil then
      exit;
    if cancellationToken.IsCancelled then
      exit;
  end;
end;


function TPackageRepositoryManager.GetPackageMetaData(const cancellationToken: ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion): IPackageSearchResultItem;
var
  repo : IPackageRepository;
begin
  FLogger.Debug('TPackageRepositoryManager.GetPackageMetaData');
  result := nil;
  Assert(FConfiguration <> nil);


  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'No compiler version was given for the package query.');
    exit;
  end;

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  
  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

    result := repo.GetPackageMetaData(cancellationToken, packageId, packageVersion, compilerVersion);
    NoteUnavailable(repo);
    if result <> nil then
      exit;
  end;
end;

function TPackageRepositoryManager.GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
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
  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  unfilteredResults := TCollections.CreateList<TPackageVersion>;

  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'No compiler version was given for the package query.');
    exit;
  end;

  if FRequestCache.TryGetVersions(packageId, compilerVersion, includePrerelease, searchResult) then
    exit(searchResult);

  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

    searchResult := repo.GetPackageVersions(cancellationToken, packageId, compilerVersion, includePrerelease);
    NoteUnavailable(repo);
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
  //a cancelled query may be partial - don't remember it.
  if (not cancellationToken.IsCancelled) and (FUnavailable = '') then
    FRequestCache.AddVersions(packageId, compilerVersion, includePrerelease, result);
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
                                                                      const packageId : string; const versionRange : TVersionRange;
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
    FLogger.Error(TErrorCode.E0003_InternalError, 'No compiler version was given for the package query.');
    exit;
  end;

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  if FRequestCache.TryGetVersionsWithDependencies(packageId, compilerVersion, versionRange, includePrerelease, searchResult) then
    exit(searchResult);

  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not repo.Enabled then
      continue;

    searchResult := repo.GetPackageVersionsWithDependencies(cancellationToken, packageId, compilerVersion, versionRange, includePrerelease);
    NoteUnavailable(repo);
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
  //a cancelled query may be partial - don't remember it.
  if (not cancellationToken.IsCancelled) and (FUnavailable = '') then
    FRequestCache.AddVersionsWithDependencies(packageId, compilerVersion, versionRange, includePrerelease, result);
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
  result := UpdateRepositories(true);
end;

function TPackageRepositoryManager.UnavailableSources : string;
begin
  result := FUnavailable;
end;

procedure TPackageRepositoryManager.BeginRequest;
var
  repo : IPackageRepository;
begin
  FUnavailable := '';
  for repo in FRepositories do
    repo.ClearLastError;
end;

procedure TPackageRepositoryManager.NoteUnavailable(const repo : IPackageRepository);
begin
  if repo.LastError = '' then
    exit;
  FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Source [' + repo.Name + '] ' + repo.LastError);
  if FUnavailable <> '' then
    FUnavailable := FUnavailable + ', ';
  FUnavailable := FUnavailable + repo.Name;
  repo.ClearLastError;
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

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

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
      NoteUnavailable(repo);
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
          result := CompareText(DPMPlatformsToString(left.Platforms), DPMPlatformsToString(right.Platforms));
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

  //Initialize reported why.
  if not UpdateRepositories then
    exit;
  BeginRequest;

  repo := FRepositories.Where(
    function(const repository : IPackageRepository) : Boolean
    begin
      result := SameText(pushOptions.Source, repository.Name);
    end).FirstOrDefault;

  if repo = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'Unable to push - the source [' + pushOptions.Source + '] is not configured.');
    exit;
  end;

  if not repo.Enabled then
  begin
    FLogger.Error(TErrorCode.E6203_PushSourceDisabled, 'Unable to push - the source [' + pushOptions.Source + '] is disabled. Enable it with `dpm sources enable`.');
    exit;
  end;

  result := repo.Push(cancellationToken, pushOptions);
  //the repository reports a push the server rejected - a request that could not be made is reported here.
  if (not result) and (not cancellationToken.IsCancelled) and (repo.LastError <> '') then
    FLogger.Error(TErrorCode.E1209_PackagePushFailed, 'Unable to push [' + ExtractFileName(pushOptions.PackagePath) + '] - source [' + repo.Name + '] ' + repo.LastError);
end;

function TPackageRepositoryManager.TryGetSourceType(const sourceName : string; out sourceType : TSourceType) : boolean;
var
  repo : IPackageRepository;
begin
  result := false;
  if not UpdateRepositories then
    exit;
  repo := GetRepositoryByName(sourceName);
  if repo = nil then
    exit;
  sourceType := repo.RepositoryType;
  result := true;
end;

function TPackageRepositoryManager.InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
var
  repo : IPackageRepository;
  gitRepo : IGitRegistryRepository;
begin
  result := false;
  if cancellationToken.IsCancelled then
    exit;
  if not UpdateRepositories then
    exit;
  repo := GetRepositoryByName(packageInfo.SourceName);
  if repo = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'Unable to install [' + packageInfo.ToString + '] - its source [' + packageInfo.SourceName + '] is not configured.');
    exit;
  end;
  if not Supports(repo, IGitRegistryRepository, gitRepo) then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'The source [' + packageInfo.SourceName + '] is not a git registry source.');
    exit;
  end;
  result := gitRepo.InstallPackageInPlace(cancellationToken, packageInfo, targetDir);
end;

function TPackageRepositoryManager.UpdateRepositories(const report : boolean) : boolean;
var
  uri : IUri;
  error : string;
  source : ISourceConfig;
  repo : IPackageRepository;
  gitRepo : IGitRegistryRepository;
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
      //git registry sources can be a git url (including git@/ssh forms) or a local
      //folder, so the strict file/http/https scheme check does not apply to them.
      if source.SourceType <> TSourceType.GitRegistry then
      begin
        if not TUriFactory.TryParseWithError(source.Source, false, uri, error) then
        begin
          if report then
            FLogger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + source.Name + '] is not valid - [' + source.Source + '] is not a valid uri : ' + error);
          exit;
        end;

        if not MatchText(uri.Scheme, ['file', 'http', 'https']) then
        begin
          if report then
            FLogger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + source.Name + '] is not valid - [' + uri.Scheme + '] is not a supported scheme (file, http or https).');
          exit;
        end;
      end;

      repo := FRepoFactory.CreateRepository(source.SourceType);
      repo.Configure(source);
      FRepositories.Add(repo);
    end;
  end;

  //apply the global git-registry refresh interval (from app config) to every git repo.
  for i := 0 to FRepositories.Count - 1 do
    if Supports(FRepositories[i], IGitRegistryRepository, gitRepo) then
      gitRepo.SetRefreshIntervalMinutes(FConfiguration.RegistryRefreshIntervalMinutes);

  result := true;
end;

end.

