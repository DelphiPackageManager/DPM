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

unit DPM.Core.Repository.GitRegistry;

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
  DPM.Core.Git.Interfaces,
  DPM.Core.Registry.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base;

type
  //A package repository backed by a git registry (folder-per-id <id>.dspec.yaml).
  //Versions come from the package repo's git tags; metadata/dependencies come from
  //the registry dspec. Installation (clone-in-place + build) is handled by the
  //package installer's git branch, not DownloadPackage.
  TGitRegistryPackageRepository = class(TBaseRepository, IPackageRepository, IGitRegistryRepository)
  private
    FGitClient : IGitClient;
    FCatalog : IRegistryCatalog;
    FRefreshIntervalMinutes : integer;

    //the registry catalog, created lazily from the configured source + current TTL.
    function GetCatalog : IRegistryCatalog;
    //returns the registry dspec for id, or nil.
    function GetSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;
    function SpecSupportsCompiler(const spec : IPackageSpec; const compilerVersion : TCompilerVersion) : boolean;
    function FindTargetPlatform(const spec : IPackageSpec; const compilerVersion : TCompilerVersion) : ISpecTargetPlatform;
    //parses a git tag name (with optional leading v) into a semver version.
    function TryParseTagVersion(const tagName : string; out version : TPackageVersion) : boolean;
    //the package repo's tags as semver versions (filtered for prerelease). When the
    //repo has no semver tags a single stable 0.0.1 (untagged HEAD) entry is returned.
    function EnumerateVersions(const cancellationToken : ICancellationToken; const spec : IPackageSpec; const includePrerelease : boolean) : IList<TPackageVersion>;
    //resolves the git commit a specific version's tag points at ('' if no matching tag).
    function ResolveTagCommit(const cancellationToken : ICancellationToken; const url : string; const version : TPackageVersion) : string;
    //if the cloned repo carries its own dspec, read it (it wins over the registry's).
    function TryReadRepoDspec(const repoDir : string) : IPackageSpec;
    //clones the registry spec reduced to a single targetPlatform (compilerVersion) and version.
    function ReduceSpec(const spec : IPackageSpec; const version : TPackageVersion; const compilerVersion : TCompilerVersion) : IPackageSpec;
    function BuildPackageInfo(const spec : IPackageSpec; const version : TPackageVersion; const compilerVersion : TCompilerVersion) : IPackageInfo;
    function MaxVersion(const versions : IList<TPackageVersion>) : TPackageVersion;
    function MatchesSearch(const id : string; const options : TSearchOptions) : boolean;
  protected
    procedure Configure(const source : ISourceConfig); override;

    //IGitRegistryRepository
    function InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
    procedure SetRefreshIntervalMinutes(const value : integer);

    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;
    function DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>; overload;
    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function GetPackageFeedByIds(const cancellationToken : ICancellationToken; const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>; overload;
  public
    constructor Create(const logger : ILogger; const gitClient : IGitClient); reintroduce;
  end;

implementation

uses
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  DPM.Core.Constants,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.Classes,
  DPM.Core.Package.SearchResults,
  DPM.Core.Package.ListItem,
  DPM.Core.Registry.Catalog;

const
  //Untagged repos have no real version. We surface the default-branch HEAD under a
  //STABLE sentinel so such packages are visible/installable without -prerelease (many
  //mirrored libraries never tag releases). 0.0.1 (not 0.0.0, which equals
  //TPackageVersion.Empty and would be treated as "no version"). HEAD is re-checked on
  //each restore. Whether a version is HEAD-tracked is decided at install time by
  //whether a matching git tag exists, not by this string.
  cUnversionedVersionString = '0.0.1';
  //records the git commit a cached package folder was built from (for HEAD tracking of
  //untagged repos) and marks the folder as a source-built git package for the cache.
  cGitCommitMarker = cGitPackageMarkerFile;

{ TGitRegistryPackageRepository }

constructor TGitRegistryPackageRepository.Create(const logger : ILogger; const gitClient : IGitClient);
begin
  inherited Create(logger);
  FGitClient := gitClient;
  FRefreshIntervalMinutes := cDefaultRegistryRefreshMinutes;
end;

procedure TGitRegistryPackageRepository.Configure(const source : ISourceConfig);
begin
  inherited Configure(source);
  //recreate the catalog lazily from the (possibly new) source + current TTL.
  FCatalog := nil;
end;

procedure TGitRegistryPackageRepository.SetRefreshIntervalMinutes(const value : integer);
begin
  if value <> FRefreshIntervalMinutes then
  begin
    FRefreshIntervalMinutes := value;
    FCatalog := nil; //recreate with the new TTL on next use
  end;
end;

function TGitRegistryPackageRepository.GetCatalog : IRegistryCatalog;
begin
  if FCatalog = nil then
    FCatalog := TRegistryCatalog.Create(Logger, FGitClient, Name, SourceUri, cDefaultRegistriesFolder, FRefreshIntervalMinutes);
  result := FCatalog;
end;

function TGitRegistryPackageRepository.GetSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;
begin
  result := GetCatalog.GetPackageSpec(cancellationToken, id);
end;

function TGitRegistryPackageRepository.FindTargetPlatform(const spec : IPackageSpec; const compilerVersion : TCompilerVersion) : ISpecTargetPlatform;
var
  tp : ISpecTargetPlatform;
begin
  result := nil;
  if spec = nil then
    exit;
  for tp in spec.TargetPlatforms do
  begin
    if (compilerVersion = TCompilerVersion.UnknownVersion) or tp.IsForCompiler(compilerVersion) then
    begin
      result := tp;
      exit;
    end;
  end;
end;

function TGitRegistryPackageRepository.SpecSupportsCompiler(const spec : IPackageSpec; const compilerVersion : TCompilerVersion) : boolean;
begin
  result := FindTargetPlatform(spec, compilerVersion) <> nil;
end;

function TGitRegistryPackageRepository.EnumerateVersions(const cancellationToken : ICancellationToken; const spec : IPackageSpec; const includePrerelease : boolean) : IList<TPackageVersion>;
var
  url : string;
  tags : IDictionary<string, string>;
  tagName : string;
  packageVersion : TPackageVersion;
begin
  result := TCollections.CreateList<TPackageVersion>;
  url := spec.MetaData.RepositoryUrl;
  if url = '' then
  begin
    Logger.Warning('Registry package [' + spec.MetaData.Id + '] has no repositoryUrl - cannot determine versions.');
    exit;
  end;

  if not FGitClient.LsRemoteTags(cancellationToken, url, tags) then
    exit;

  for tagName in tags.Keys do
  begin
    if not TryParseTagVersion(tagName, packageVersion) then
      continue;
    if (not includePrerelease) and (not packageVersion.IsStable) then
      continue;
    result.Add(packageVersion);
  end;

  //no semver tags - fall back to the default branch HEAD under the stable 0.0.0
  //sentinel. It is stable (not a prerelease) so it is surfaced for stable-only
  //callers too, making untagged packages visible/installable by default.
  if result.Count = 0 then
  begin
    Logger.Debug(Format('[git] %s : %d tag(s), no usable versions - using stable 0.0.0 (untagged HEAD).', [spec.MetaData.Id, tags.Count]));
    if TPackageVersion.TryParse(cUnversionedVersionString, packageVersion) then
      result.Add(packageVersion);
  end;
end;

function TGitRegistryPackageRepository.ReduceSpec(const spec : IPackageSpec; const version : TPackageVersion; const compilerVersion : TCompilerVersion) : IPackageSpec;
var
  tp : ISpecTargetPlatform;
  clonedTp : ISpecTargetPlatform;
begin
  result := nil;
  tp := FindTargetPlatform(spec, compilerVersion);
  if tp = nil then
    exit;
  result := spec.Clone;
  clonedTp := tp.Clone;
  clonedTp.Compiler := compilerVersion;
  result.TargetPlatforms.Clear;
  result.TargetPlatforms.Add(clonedTp);
  result.MetaData.Version := version;
end;

function TGitRegistryPackageRepository.BuildPackageInfo(const spec : IPackageSpec; const version : TPackageVersion; const compilerVersion : TCompilerVersion) : IPackageInfo;
var
  reduced : IPackageSpec;
begin
  result := nil;
  reduced := ReduceSpec(spec, version, compilerVersion);
  if reduced = nil then
    exit;
  result := TPackageInfo.CreateFromManifest(Name, reduced, '', '');
end;

function TGitRegistryPackageRepository.MaxVersion(const versions : IList<TPackageVersion>) : TPackageVersion;
var
  i : integer;
begin
  //init from the first element rather than Empty (which is 0.0.0) so a legitimate
  //low version like 0.0.1 isn't lost.
  if versions.Count = 0 then
    exit(TPackageVersion.Empty);
  result := versions[0];
  for i := 1 to versions.Count - 1 do
    if versions[i] > result then
      result := versions[i];
end;

function TGitRegistryPackageRepository.MatchesSearch(const id : string; const options : TSearchOptions) : boolean;
var
  term : string;
  terms : TStringDynArray;
begin
  if (options.SearchTerms = '') or (options.SearchTerms = '*') then
    exit(true);
  if options.Exact then
    exit(SameText(id, options.SearchTerms));
  terms := SplitString(Trim(options.SearchTerms), ' ');
  for term in terms do
  begin
    if term = '' then
      continue;
    if ContainsText(id, term) then
      exit(true);
  end;
  result := false;
end;

function TGitRegistryPackageRepository.GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>;
var
  spec : IPackageSpec;
begin
  result := TCollections.CreateList<TPackageVersion>;
  spec := GetSpec(cancellationToken, id);
  if spec = nil then
    exit;
  if not SpecSupportsCompiler(spec, compilerVersion) then
    exit;
  result := EnumerateVersions(cancellationToken, spec, preRelease);
end;

function TGitRegistryPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;
var
  spec : IPackageSpec;
  versions : IList<TPackageVersion>;
  packageVersion : TPackageVersion;
  packageInfo : IPackageInfo;
begin
  result := TCollections.CreateList<IPackageInfo>;
  spec := GetSpec(cancellationToken, id);
  if spec = nil then
    exit;
  if not SpecSupportsCompiler(spec, compilerVersion) then
    exit;

  versions := EnumerateVersions(cancellationToken, spec, preRelease);
  for packageVersion in versions do
  begin
    if not versionRange.IsSatisfiedBy(packageVersion) then
      continue;
    packageInfo := BuildPackageInfo(spec, packageVersion, compilerVersion);
    if packageInfo <> nil then
      result.Add(packageInfo);
  end;

  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := Right.Version.CompareTo(Left.Version);
    end));
end;

function TGitRegistryPackageRepository.FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean) : IPackageInfo;
var
  spec : IPackageSpec;
  versions : IList<TPackageVersion>;
  chosen : TPackageVersion;
  candidate : TPackageVersion;
  found : boolean;
begin
  result := nil;
  spec := GetSpec(cancellationToken, id);
  if spec = nil then
    exit;
  if not SpecSupportsCompiler(spec, compilerVersion) then
    exit;

  versions := EnumerateVersions(cancellationToken, spec, includePrerelease);
  if versions.Count = 0 then
    exit;

  if not version.IsEmpty then
  begin
    found := false;
    for candidate in versions do
      if candidate = version then
      begin
        found := true;
        break;
      end;
    if not found then
      exit;
    chosen := version;
  end
  else
    chosen := MaxVersion(versions);

  if chosen.IsEmpty then
    exit;
  result := BuildPackageInfo(spec, chosen, compilerVersion);
end;

function TGitRegistryPackageRepository.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  spec : IPackageSpec;
begin
  result := nil;
  spec := GetSpec(cancellationToken, packageId.Id);
  if spec = nil then
    exit;
  result := BuildPackageInfo(spec, packageId.Version, packageId.CompilerVersion);
end;

function TGitRegistryPackageRepository.GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
var
  spec : IPackageSpec;
  reduced : IPackageSpec;
  version : TPackageVersion;
  metaData : IPackageMetadata;
begin
  result := nil;
  spec := GetSpec(cancellationToken, packageId);
  if spec = nil then
    exit;
  if not TPackageVersion.TryParse(packageVersion, version) then
    exit;
  reduced := ReduceSpec(spec, version, compilerVersion);
  if reduced = nil then
    exit;
  metaData := TPackageMetadata.CreateFromManifest(Name, reduced);
  if metaData <> nil then
    result := TDPMPackageSearchResultItem.FromMetaData(Name, metaData, '', '');
end;

function TGitRegistryPackageRepository.GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
var
  ids : IList<string>;
  id : string;
  spec : IPackageSpec;
  versions : IList<TPackageVersion>;
  latest : TPackageVersion;
  reduced : IPackageSpec;
  metaData : IPackageMetadata;
  resultItem : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResult.Create(options.Skip, 0);
  ids := GetCatalog.GetPackageIds(cancellationToken);
  for id in ids do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not MatchesSearch(id, options) then
      continue;
    spec := GetSpec(cancellationToken, id);
    if (spec = nil) or (not SpecSupportsCompiler(spec, compilerVersion)) then
      continue;
    versions := EnumerateVersions(cancellationToken, spec, options.Prerelease);
    latest := MaxVersion(versions);
    if latest.IsEmpty then
      continue;
    reduced := ReduceSpec(spec, latest, compilerVersion);
    if reduced = nil then
      continue;
    metaData := TPackageMetadata.CreateFromManifest(Name, reduced);
    if metaData = nil then
      continue;
    resultItem := TDPMPackageSearchResultItem.FromMetaData(Name, metaData, '', '');
    resultItem.LatestVersion := latest;
    if latest.IsStable then
      resultItem.LatestStableVersion := latest;
    result.Results.Add(resultItem);
  end;
end;

function TGitRegistryPackageRepository.GetPackageFeedByIds(const cancellationToken : ICancellationToken; const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
var
  item : IPackageIdentity;
  metaData : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResult.Create(0, 0);
  for item in ids do
  begin
    metaData := GetPackageMetaData(cancellationToken, item.Id, item.Version.ToStringNoMeta, compilerVersion);
    if metaData <> nil then
      result.Results.Add(metaData);
  end;
end;

function TGitRegistryPackageRepository.List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
var
  ids : IList<string>;
  id : string;
  spec : IPackageSpec;
  tp : ISpecTargetPlatform;
  versions : IList<TPackageVersion>;
  latest : TPackageVersion;
  compiler : TCompilerVersion;
  listItem : IPackageListItem;
begin
  result := TCollections.CreateList<IPackageListItem>;
  ids := GetCatalog.GetPackageIds(cancellationToken);
  for id in ids do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not MatchesSearch(id, options) then
      continue;
    spec := GetSpec(cancellationToken, id);
    if spec = nil then
      continue;
    tp := FindTargetPlatform(spec, options.CompilerVersion);
    if tp = nil then
      continue;
    versions := EnumerateVersions(cancellationToken, spec, options.Prerelease);
    latest := MaxVersion(versions);
    if latest.IsEmpty then
      continue;
    if options.CompilerVersion <> TCompilerVersion.UnknownVersion then
      compiler := options.CompilerVersion
    else
      compiler := tp.Compiler;
    listItem := TPackageListItem.Create(id, compiler, latest, tp.Platforms);
    result.Add(listItem);
  end;
end;

function TGitRegistryPackageRepository.DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;
begin
  //git registry packages are installed in place (clone + build) by the package
  //installer's git branch, not via a downloaded .dpkg. This is intentionally a no-op.
  fileName := '';
  Logger.Error('DownloadPackage is not supported for git registry packages - they install in place.');
  result := false;
end;

function TGitRegistryPackageRepository.GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
begin
  //icons for git packages live in the package repo and require a clone - not yet supported.
  result := nil;
end;

function TGitRegistryPackageRepository.Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
begin
  Logger.Error('Push is not supported for git registry sources.');
  result := false;
end;

function TGitRegistryPackageRepository.TryParseTagVersion(const tagName : string; out version : TPackageVersion) : boolean;
begin
  //tags are commonly prefixed with 'v' (e.g. v1.2.3) - try as-is, then without the prefix.
  result := TPackageVersion.TryParse(tagName, version);
  if (not result) and (Length(tagName) > 1) and CharInSet(tagName[1], ['v', 'V']) then
    result := TPackageVersion.TryParse(Copy(tagName, 2, MaxInt), version);
end;

function TGitRegistryPackageRepository.ResolveTagCommit(const cancellationToken : ICancellationToken; const url : string; const version : TPackageVersion) : string;
var
  tags : IDictionary<string, string>;
  tagName : string;
  tagVersion : TPackageVersion;
begin
  result := '';
  if not FGitClient.LsRemoteTags(cancellationToken, url, tags) then
    exit;
  for tagName in tags.Keys do
  begin
    if TryParseTagVersion(tagName, tagVersion) and (tagVersion = version) then
      exit(tags[tagName]);
  end;
end;

function TGitRegistryPackageRepository.TryReadRepoDspec(const repoDir : string) : IPackageSpec;
var
  reader : IPackageSpecReader;
  files : TStringDynArray;
  f : string;
begin
  result := nil;
  files := TDirectory.GetFiles(repoDir, '*.dspec.yaml');
  for f in files do
  begin
    //ignore the dspec we write into the cache folder.
    if SameText(ExtractFileName(f), cPackageDspecFile) then
      continue;
    reader := TPackageSpecReader.Create(Logger);
    result := reader.ReadSpec(f);
    if result <> nil then
      exit;
  end;
end;

function TGitRegistryPackageRepository.InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
var
  registrySpec : IPackageSpec;
  repoSpec : IPackageSpec;
  finalSpec : IPackageSpec;
  url : string;
  commit : string;
  headTracked : boolean;
  dspecFile : string;
  markerFile : string;
  parentDir : string;
  sYaml : string;
begin
  result := false;

  registrySpec := GetSpec(cancellationToken, packageInfo.Id);
  if registrySpec = nil then
  begin
    Logger.Error('Package [' + packageInfo.Id + '] not found in registry [' + Name + '].');
    exit;
  end;

  url := registrySpec.MetaData.RepositoryUrl;
  if url = '' then
  begin
    Logger.Error('Registry package [' + packageInfo.Id + '] has no repositoryUrl.');
    exit;
  end;

  dspecFile := IncludeTrailingPathDelimiter(targetDir) + cPackageDspecFile;
  markerFile := IncludeTrailingPathDelimiter(targetDir) + cGitCommitMarker;

  //resolve the commit: prefer the matching tag; if there is none (untagged repo, or
  //the 0.0.1 HEAD sentinel) track the default-branch HEAD.
  commit := ResolveTagCommit(cancellationToken, url, packageInfo.Version);
  headTracked := commit = '';
  if headTracked then
  begin
    if not FGitClient.LsRemoteHead(cancellationToken, url, '', commit) then
    begin
      Logger.Error('Could not determine HEAD commit for [' + url + '].');
      exit;
    end;
  end;

  //cache freshness - reuse an existing clone unless we are HEAD-tracking and HEAD moved.
  if DirectoryExists(targetDir) and FileExists(dspecFile) then
  begin
    if not headTracked then
      exit(true);
    if FileExists(markerFile) and SameText(Trim(TFile.ReadAllText(markerFile)), commit) then
      exit(true);
  end;

  //ensure a clean target for git clone (it requires a missing/empty folder).
  if DirectoryExists(targetDir) then
  begin
    try
      TDirectory.Delete(targetDir, true);
    except
      on e : Exception do
      begin
        Logger.Error('Could not clear cached package folder [' + targetDir + '] : ' + e.Message);
        exit;
      end;
    end;
  end;
  parentDir := ExtractFileDir(ExcludeTrailingPathDelimiter(targetDir));
  if parentDir <> '' then
    ForceDirectories(parentDir);

  Logger.Information('Cloning [' + packageInfo.Id + '] from ' + url);
  if not FGitClient.Clone(cancellationToken, url, targetDir) then
    exit;
  if not FGitClient.Checkout(cancellationToken, targetDir, commit) then
    exit;

  //repo dspec wins if present, else use the registry dspec.
  repoSpec := TryReadRepoDspec(targetDir);
  if repoSpec <> nil then
    finalSpec := ReduceSpec(repoSpec, packageInfo.Version, packageInfo.CompilerVersion)
  else
    finalSpec := nil;
  if finalSpec = nil then
    finalSpec := ReduceSpec(registrySpec, packageInfo.Version, packageInfo.CompilerVersion);
  if finalSpec = nil then
  begin
    Logger.Error('No target platform for the requested compiler in dspec for [' + packageInfo.Id + '].');
    exit;
  end;

  //write the dspec into the package folder as a plain dpm spec (single version +
  //single target platform) so the cache/installer treat it like any other package.
  finalSpec.PackageKind := TDPMPackageKind.dpm;
  sYaml := finalSpec.GenerateDspecYAML(packageInfo.Version);
  try
    TFile.WriteAllText(dspecFile, sYaml);
    TFile.WriteAllText(markerFile, commit);
  except
    on e : Exception do
    begin
      Logger.Error('Could not write package dspec [' + dspecFile + '] : ' + e.Message);
      exit;
    end;
  end;

  result := true;
end;

end.
