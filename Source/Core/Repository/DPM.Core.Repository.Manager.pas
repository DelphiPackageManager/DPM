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
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TPackageRepositoryManager = class(TInterfacedObject, IPackageRepositoryManager)
  private
    FLogger : ILogger;
    FConfigurationManager : IConfigurationManager;
    FRepositories : IList<IPackageRepository>;
    FRepoFactory : IPackageRepositoryFactory;
  protected
    function Initialize( const configuration : IConfiguration) : boolean;

    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string ) : boolean;
    function GetRepositories: IList<IPackageRepository>;
    function GetRepositoryByName(const value: string): IPackageRepository;
    function List(const cancellationToken : ICancellationToken; const options: TSearchOptions): IList<IPackageIdentity>;
    function UpdateRepositories( const configuration : IConfiguration) : boolean;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId): IPackageInfo;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const options : TSearchOptions; const platform : TDPMPlatform; const versionRange : TVersionRange;  const configuration : IConfiguration = nil) : IList<IPackageInfo>;overload;
    //UI specific stuff
    //TODO : Implement skip/take!
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration = nil) : IList<IPackageSearchResultItem>;

    function GetPackageIcon(const cancelToken : ICancellationToken; const source: string; const packageId: string; const packageVersion: string;
                            const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const configuration : IConfiguration): IPackageIcon;

    function GetInstalledPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IEnumerable<IPackageId>; const configuration : IConfiguration = nil) : IList<IPackageSearchResultItem>;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration = nil) : IList<TPackageVersion>;overload;

  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repoFactory : IPackageRepositoryFactory);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Spring.Collections.Extensions,
  VSoft.URI,
  DPM.Core.Constants,
  DPM.Core.Utils.System;

{ TRespositoryManager }

constructor TPackageRepositoryManager.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repoFactory : IPackageRepositoryFactory);
begin
  inherited Create;
  FLogger := logger;
  FConfigurationManager := configurationManager;
  FRepoFactory := repoFactory;
  FRepositories := TCollections.CreateList<IPackageRepository>;
end;

function TPackageRepositoryManager.DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string ) : boolean;
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
    FLogger.Information('Downloading package [' + packageIdentity.ToString + '] from [' + repo.Source +']');
    result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName);
  end
  else
  begin
   for repo in FRepositories do
    begin
      if  cancellationToken.IsCancelled then
        exit;
      result := repo.DownloadPackage(cancellationToken, packageIdentity, localFolder, fileName) or result;
      if result then
        exit;
    end;
  end;
end;

function TPackageRepositoryManager.GetInstalledPackageFeed(const cancelToken: ICancellationToken; const options: TSearchOptions; const installedPackages: IEnumerable<IPackageId>; const configuration: IConfiguration): IList<IPackageSearchResultItem>;
var
  config : IConfiguration;
  searchOptions : TSearchOptions;
  packageId : IPackageId;
  packageResults : IList<IPackageSearchResultItem>;
  item : IPackageSearchResultItem;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;

  config := configuration;
  if config = nil then
    config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if not UpdateRepositories(config) then
  begin
    FLogger.Error('Unabled to search, error loading repositories');
    exit;
  end;

  //TODO : This will be really inefficient/slow when using http
  //refactor to make single request to repositories.
  packageId := installedPackages.FirstOrDefault;
  for packageId in installedPackages do
  begin
    searchOptions := options.Clone;
    searchOptions.SearchTerms := packageId.Id;
    searchOptions.Version := packageId.Version;
    searchOptions.CompilerVersion := packageId.CompilerVersion;
    searchOptions.Platforms := [packageId.Platform];
    searchOptions.Exact := true;

    packageResults := GetPackageFeed(cancelToken, searchOptions, config);
    for item in packageResults do
    begin
      item.Installed := true;
      item.InstalledVersion := packageId.Version.ToStringNoMeta;
    end;
    result.AddRange(packageResults);

  end;


end;

function TPackageRepositoryManager.GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration = nil): IList<IPackageSearchResultItem>;
var
  config : IConfiguration;
  repo : IPackageRepository;
  sources : TStringList;
  searchResult : IList<IPackageSearchResultItem>;
  allResults : IList<IPackageSearchResultItem>;
  distinctResults : IEnumerable<IPackageSearchResultItem>;
  comparer : IEqualityComparer<IPackageSearchResultItem>;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;

  config := configuration;
  if config = nil then
    config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if not UpdateRepositories(config) then
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
      searchResult := repo.GetPackageFeed(cancelToken,options, config);
      allResults.AddRange(searchResult);
    end;
  finally
    if sources <> nil then
      sources.Free;
  end;
  comparer := TPackageSearchResultItemComparer.Create;

  distinctResults := TDistinctIterator<IPackageSearchResultItem>.Create(allResults, comparer);
  result.AddRange(distinctResults);


end;

function TPackageRepositoryManager.GetPackageIcon(const cancelToken : ICancellationToken; const source, packageId, packageVersion: string;
                                                  const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const configuration : IConfiguration): IPackageIcon;
var
  repo : IPackageRepository;
begin
  result := nil;

  if configuration = nil then
    exit;
  if not UpdateRepositories(configuration) then
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

function TPackageRepositoryManager.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId): IPackageInfo;
var
  repo : IPackageRepository;
begin
  result := nil;
  for repo in FRepositories do
  begin
    result := repo.GetPackageInfo(cancellationToken, packageId);
    if result <> nil then
      exit;
  end;
end;


function TPackageRepositoryManager.GetPackageVersions(const cancellationToken: ICancellationToken; const options: TSearchOptions; const configuration: IConfiguration): IList<TPackageVersion>;
var
  config : IConfiguration;
  repo : IPackageRepository;
  searchResult : IList<TPackageVersion>;
  unfilteredResults : IList<TPackageVersion>;
  distinctResults : IEnumerable<TPackageVersion>;
  comparer : IEqualityComparer<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;
  unfilteredResults := TCollections.CreateList<TPackageVersion>;

  config := configuration;
  if config = nil then
  begin
    if options.ConfigFile = '' then
    begin
      FLogger.Error('No configuration file specified');
      exit;
    end;
    config := FConfigurationManager.LoadConfig(options.ConfigFile);
  end;
  if config = nil then
    exit;

  if options.CompilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;


  if not UpdateRepositories(config) then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;

  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;

    searchResult := repo.GetPackageVersions(cancellationToken, options.SearchTerms, options.CompilerVersion);
    unfilteredResults.AddRange(searchResult);
  end;

  comparer := TPackageVersionComparer.Create;

  //dedupe
  distinctResults := TDistinctIterator<TPackageVersion>.Create(unfilteredResults, comparer);

  result.AddRange(distinctResults);
  //todo :// which order is this?  want descending.
  result.Sort(function(const Left, Right: TPackageVersion): Integer
    begin
      result := right.CompareTo(left);
    end);


end;

function TPackageRepositoryManager.GetPackageVersions(const cancellationToken : ICancellationToken; const options : TSearchOptions; const platform : TDPMPlatform; const versionRange : TVersionRange;  const configuration : IConfiguration = nil) : IList<IPackageInfo>;
var
  config : IConfiguration;
  repo : IPackageRepository;
  searchResult : IList<IPackageInfo>;
  unfilteredResults : IList<IPackageInfo>;
  distinctResults : IEnumerable<IPackageInfo>;
  comparer : IEqualityComparer<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
  unfilteredResults := TCollections.CreateList<IPackageInfo>;

  config := configuration;
  if config = nil then
  begin
    if options.ConfigFile = '' then
    begin
      FLogger.Error('No configuration file specified');
      exit;
    end;
    config := FConfigurationManager.LoadConfig(options.ConfigFile);
  end;
  if config = nil then
    exit;

  if options.CompilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;


  if not UpdateRepositories(config) then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;


  for repo in FRepositories do
  begin
    if cancellationToken.IsCancelled then
      exit;

    searchResult := repo.GetPackageVersionsWithDependencies(cancellationToken, options.SearchTerms, options.CompilerVersion,platform,versionRange, options.Prerelease);
    unfilteredResults.AddRange(searchResult);
  end;


  comparer := TPackageInfoComparer.Create;

  //dedupe
  distinctResults := TDistinctIterator<IPackageInfo>.Create(unfilteredResults, comparer);

  result.AddRange(distinctResults);
  //todo :// which order is this?  want descending.
  result.Sort(function(const Left, Right: IPackageInfo): Integer
    begin
      result := right.Version.CompareTo(left.Version);;
    end);
//
end;

(*
function TPackageRepositoryManager.GetPackageVersions(const options: TSearchOptions; const platform: TDPMPlatform; const versionRange: TVersionRange): IList<TPackageVersion>;
var
  config : IConfiguration;
  repo : IPackageRepository;
  searchResult : IList<TPackageVersion>;
  unfilteredResults : IList<TPackageVersion>;
  distinctResults : IEnumerable<TPackageVersion>;
  comparer : IEqualityComparer<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;

  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  if options.CompilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FLogger.Error('Unabled to search, no compiler version specified');
    exit;
  end;


  if options.Platforms = [] then
  begin
    FLogger.Error('Unabled to get package versions, no platform specified');
    exit;
  end;


  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if not UpdateRepositories(config) then
  begin
    FLogger.Error('Unabled to get package versions, error loading repositories');
    exit;
  end;


  for repo in FRepositories do
  begin
    searchResult := repo.GetPackageVersions(options.SearchTerms, options.CompilerVersion,platform,versionRange, options.Prerelease);
    unfilteredResults.AddRange(searchResult);
  end;
  
  comparer := TPackageVersionComparer.Create;

  //dedupe
  distinctResults := TDistinctIterator<TPackageVersion>.Create(unfilteredResults, comparer);
  result.AddRange(distinctResults);
  //todo :// which order is this?  want descending.
  result.Sort(
    function(const Left, Right: TPackageVersion): Integer
    begin
      result := Left.CompareTo(Right);
    end);

end;
*)


function TPackageRepositoryManager.GetRepositories: IList<IPackageRepository>;
begin
  result := FRepositories;
end;

function TPackageRepositoryManager.GetRepositoryByName(const value: string): IPackageRepository;
begin
  result := FRepositories.Where(function(const repo : IPackageRepository) : boolean
                               begin
                                 result := SameText(value, repo.Name);
                               end).FirstOrDefault;
end;


function TPackageRepositoryManager.Initialize(const configuration: IConfiguration): boolean;
begin
  result := UpdateRepositories(configuration);
end;

function TPackageRepositoryManager.List(const cancellationToken : ICancellationToken; const options: TSearchOptions): IList<IPackageIdentity>;
var
  config : IConfiguration;
  repo : IPackageRepository;
  info, prevInfo : IPackageIdentity;

  searchResult : IList<IPackageIdentity>;
  unfilteredResults : IList<IPackageIdentity>;
  distinctResults : IEnumerable<IPackageIdentity>;
  sortFunc : TComparison<IPackageIdentity>;
  packages : IList<IPackageIdentity>;
  sources : TStringList;
begin
  result := TCollections.CreateList<IPackageIdentity>;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if not UpdateRepositories(config) then
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

  unfilteredResults := TCollections.CreateList<IPackageIdentity>;
  //create the list here as if there are no repos it will be nil.
  searchResult := TCollections.CreateList<IPackageIdentity>;

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

  sortFunc := function(const Left, Right: IPackageIdentity): Integer
              begin
                result := CompareText(left.Id, right.Id);
                if result = 0 then
                begin
                   result := right.Version.CompareTo(left.Version);
                   if result = 0 then
                   begin
                     result := Ord(left.CompilerVersion) - Ord(right.CompilerVersion);
                     if result = 0 then
                     begin
                       result := Ord(left.Platform) - Ord(right.Platform);
                     end;
                   end;
                end;
              end;

  //what does this do? if we don't provide a comparer?
  distinctResults := TDistinctIterator<IPackageIdentity>.Create(unfilteredResults, nil);
  searchResult.Clear;
  searchResult.AddRange(distinctResults);

  //Sort by id, version, platform.
  searchResult.Sort(sortFunc);

  packages := TCollections.CreateList<IPackageIdentity>;

  if not options.AllVersions then
  begin
    prevInfo := nil;
    for info in searchResult do
    begin
      if cancellationToken.IsCancelled then
        exit;

      if (prevInfo = nil) or (info.id <> prevInfo.id) then
      begin
        prevInfo := info;
        packages.Add(info);
        continue;
      end;
      //if it's the same id, then compilerversion or platform must be different
      if (info.id = prevInfo.id) then
      begin
        if (info.Version = prevInfo.Version) and ((info.CompilerVersion <> prevInfo.CompilerVersion) or (info.Platform <> prevInfo.Platform)) then
        begin
          prevInfo := info;
          packages.Add(info);
        end;
      end;
    end;
  end
  else
    packages.AddRange(searchResult);

  if (options.Skip > 0) or (options.Take > 0) then
  begin
    distinctResults := packages;
    if options.Skip > 0 then
      distinctResults := distinctResults.Skip(options.Skip);
    if options.Take > 0 then
      distinctResults := distinctResults.Take(options.Take);

    result.AddRange(distinctResults);
  end
  else
    result.AddRange(packages);
end;

function TPackageRepositoryManager.UpdateRepositories( const configuration : IConfiguration) : boolean;
var
  uri : IUri;
  error : string;
  source : ISourceConfig;
  repo : IPackageRepository;
begin
  result := false;
  FRepositories.Clear;
  for source in configuration.Sources do
  begin
    if not source.IsEnabled then
      continue;

    if not TUriFactory.TryParseWithError(source.Source,false, uri, error) then
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
