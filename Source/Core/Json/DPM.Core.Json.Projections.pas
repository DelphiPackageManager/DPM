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

unit DPM.Core.Json.Projections;

interface

uses
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Project.Interfaces;

{
  JSON projections of DPM's package and project types.

  Shape
  -----
  These mirror the camelCase field names the DPM server's search feed already uses (see
  TDPMPackageSearchResultItem.CreateFromJson), NOT the PascalCase keys TPackageMetadata reads
  from a manifest. Staying close to the wire means anyone who has seen the gallery API can read
  this output, and most of it round trips back through the existing FromJson factories.

  Two deliberate departures from the wire format, both type upgrades:
    - `tags` is a JSON array. On the wire it is a single space delimited string.
    - `authors` is a JSON array. On the wire it is a single comma delimited string.
  Both are IList<string>/TStrings on the interfaces, so an array is the honest representation.
  It does mean tags and authors do not survive a byte level round trip through FromJson.

  `hash` is passed through exactly as the source supplied it, with `hashAlgorithm` alongside.
  Be aware the representation is not consistent across sources: the live gallery search feed
  sends base64 (verified against delphi.dev), while TPackageInfo.TryLoadFromJson decodes the
  package info endpoint into hex. Normalising here would mean guessing which one we hold, and
  guessing wrong would corrupt a hash, so callers that compare hashes must not assume an
  encoding. This is a wart in the wire format rather than something to paper over.

  IDE-mutable state on IPackageSearchResultItem (Installed, IsTransitive, LatestVersion,
  LatestStableVersion, VersionRange) is deliberately excluded. The server never sends it; it is
  scratch space the IDE writes into the object after the fact, and emitting it would invent
  facts a CLI or MCP caller has no business trusting.

  Convention
  ----------
  Every projection FILLS a caller supplied mapping rather than creating and returning one, so a
  projection can be nested inside a larger document (an MCP tool result, a search envelope)
  without reparsing. This mirrors FillComponent in DPM.Core.SBOM.Writers.

  Empty strings are omitted rather than emitted as null - see TJsonUtils for why. Empty
  collections ARE emitted, as [].
}

type
  TJsonProjections = record
  public
    /// <summary> id, version, compiler, platforms and signing status. The `list` command's shape. </summary>
    class procedure PackageListItem(const item : IPackageListItem; const target : IYAMLMapping); static;

    /// <summary> A dependency as { packageId, versionRange }, matching the wire format. </summary>
    class procedure PackageDependency(const dependency : IPackageDependency; const target : IYAMLMapping); static;

    /// <summary> Identity, platforms, hash and dependencies. No descriptive metadata. </summary>
    class procedure PackageInfo(const info : IPackageInfo; const target : IYAMLMapping); static;

    /// <summary> The full metadata record - description, authors, licence, urls, dependencies. </summary>
    /// <param name="includeDependencies">
    ///   Search result lists usually want this off; a single package lookup wants it on.
    /// </param>
    class procedure SearchResultItem(const item : IPackageSearchResultItem; const target : IYAMLMapping;
                                     const includeDependencies : boolean); static;

    /// <summary>
    ///   Fills target with a `packages` array holding the root's children as a nested tree.
    ///   Pass the root returned by IProjectEditor.GetPackageReferences - it is a synthetic node
    ///   whose own id and version are not meaningful, so it is never emitted. A nil root yields
    ///   an empty array, which is the correct answer for a project with no packages.
    /// </summary>
    class procedure PackageReferenceTree(const root : IPackageReference; const target : IYAMLMapping); static;

    /// <summary> One reference node and, recursively, its children. </summary>
    class procedure PackageReference(const reference : IPackageReference; const target : IYAMLMapping); static;

    /// <summary>
    ///   Project context - compiler, platforms, and the package tree. The project must already
    ///   be loaded (IProjectEditor.LoadProject) with at least the DPMCompiler, ProjectVersion,
    ///   Platforms and PackageRefs elements.
    /// </summary>
    class procedure ProjectInfo(const editor : IProjectEditor; const target : IYAMLMapping); static;
  end;

implementation

uses
  System.TypInfo,
  DPM.Core.Json.Utils;

function AppTypeToString(const value : TAppType) : string;
begin
  result := GetEnumName(TypeInfo(TAppType), Ord(value));
end;

{ TJsonProjections }

class procedure TJsonProjections.PackageListItem(const item : IPackageListItem; const target : IYAMLMapping);
begin
  target.AddOrSetValue('id', item.Id);
  TJsonUtils.AddVersion(target, 'version', item.Version);
  TJsonUtils.AddCompiler(target, 'compiler', item.CompilerVersion);
  TJsonUtils.AddPlatforms(target, 'platforms', item.Platforms);
  target.AddOrSetValue('isSigned', item.IsSigned);
  TJsonUtils.AddIfNotEmpty(target, 'signedBy', item.SignedBy);
end;

class procedure TJsonProjections.PackageDependency(const dependency : IPackageDependency; const target : IYAMLMapping);
begin
  target.AddOrSetValue('packageId', dependency.Id);
  TJsonUtils.AddVersionRange(target, 'versionRange', dependency.VersionRange);
end;

class procedure TJsonProjections.PackageInfo(const info : IPackageInfo; const target : IYAMLMapping);
var
  dependencies : IYAMLSequence;
  dependency : IPackageDependency;
begin
  target.AddOrSetValue('id', info.Id);
  TJsonUtils.AddVersion(target, 'version', info.Version);
  TJsonUtils.AddCompiler(target, 'compiler', info.CompilerVersion);
  TJsonUtils.AddPlatforms(target, 'platforms', info.SupportedPlatforms);
  TJsonUtils.AddIfNotEmpty(target, 'sourceName', info.SourceName);
  TJsonUtils.AddIfNotEmpty(target, 'hash', info.Hash);
  TJsonUtils.AddIfNotEmpty(target, 'hashAlgorithm', info.HashAlgorithm);
  target.AddOrSetValue('isSigned', info.IsSigned);
  TJsonUtils.AddIfNotEmpty(target, 'signedBy', info.SignedBy);

  dependencies := target.AddOrSetSequence('dependencies');
  if info.Dependencies <> nil then
  begin
    for dependency in info.Dependencies do
      PackageDependency(dependency, dependencies.AddMapping);
  end;
end;

class procedure TJsonProjections.SearchResultItem(const item : IPackageSearchResultItem; const target : IYAMLMapping;
                                                  const includeDependencies : boolean);
var
  dependencies : IYAMLSequence;
  dependency : IPackageDependency;
  frameworks : IYAMLSequence;
  frameworkList : TArray<TDPMUIFrameworkType>;
  i : integer;
begin
  target.AddOrSetValue('id', item.Id);
  TJsonUtils.AddVersion(target, 'version', item.Version);
  TJsonUtils.AddCompiler(target, 'compiler', item.CompilerVersion);
  TJsonUtils.AddPlatforms(target, 'platforms', item.SupportedPlatforms);
  TJsonUtils.AddIfNotEmpty(target, 'sourceName', item.SourceName);

  TJsonUtils.AddIfNotEmpty(target, 'description', item.Description);
  TJsonUtils.AddStrings(target, 'authors', item.Authors);
  TJsonUtils.AddIfNotEmpty(target, 'license', item.License);
  TJsonUtils.AddIfNotEmpty(target, 'copyright', item.Copyright);
  TJsonUtils.AddIfNotEmpty(target, 'icon', item.Icon);
  TJsonUtils.AddStrings(target, 'tags', item.Tags);
  target.AddOrSetValue('isTrial', item.IsTrial);
  target.AddOrSetValue('isCommercial', item.IsCommercial);
  TJsonUtils.AddIfNotEmpty(target, 'projectUrl', item.ProjectUrl);
  TJsonUtils.AddIfNotEmpty(target, 'repositoryUrl', item.RepositoryUrl);
  TJsonUtils.AddIfNotEmpty(target, 'repositoryType', item.RepositoryType);
  TJsonUtils.AddIfNotEmpty(target, 'repositoryBranch', item.RepositoryBranch);
  TJsonUtils.AddIfNotEmpty(target, 'repositoryCommit', item.RepositoryCommit);
  TJsonUtils.AddIfNotEmpty(target, 'reportUrl', item.ReportUrl);
  TJsonUtils.AddIfNotEmpty(target, 'releaseNotes', item.ReleaseNotes);
  TJsonUtils.AddIfNotEmpty(target, 'readme', item.ReadMe);
  TJsonUtils.AddIfNotEmpty(target, 'publishedDate', item.PublishedDate);
  //SearchPaths is deliberately not emitted - the feed never populates it, so an empty array
  //here would read as a fact about the package rather than an absence of data.

  //Downloads is -1 when the source did not report a count - which is not the same as zero.
  if item.Downloads >= 0 then
    target.AddOrSetValue('totalDownloads', item.Downloads);

  target.AddOrSetValue('isReservedPrefix', item.IsReservedPrefix);
  TJsonUtils.AddIfNotEmpty(target, 'hash', item.FileHash);
  TJsonUtils.AddIfNotEmpty(target, 'hashAlgorithm', item.HashAlgorithm);
  target.AddOrSetValue('isSigned', item.IsSigned);
  TJsonUtils.AddIfNotEmpty(target, 'signedBy', item.SignedBy);

  frameworkList := item.Frameworks;
  if Length(frameworkList) > 0 then
  begin
    frameworks := target.AddOrSetSequence('frameworks');
    for i := 0 to Length(frameworkList) - 1 do
      frameworks.AddValue(UIFrameworkTypeToString(frameworkList[i]));
  end;

  if includeDependencies then
  begin
    dependencies := target.AddOrSetSequence('dependencies');
    if item.Dependencies <> nil then
    begin
      for dependency in item.Dependencies do
        PackageDependency(dependency, dependencies.AddMapping);
    end;
  end;
end;

class procedure TJsonProjections.PackageReference(const reference : IPackageReference; const target : IYAMLMapping);
var
  children : IYAMLSequence;
  child : IPackageReference;
begin
  target.AddOrSetValue('id', reference.Id);
  TJsonUtils.AddVersion(target, 'version', reference.Version);
  TJsonUtils.AddVersionRange(target, 'versionRange', reference.VersionRange);
  target.AddOrSetValue('topLevel', reference.IsTopLevel);
  target.AddOrSetValue('transitive', reference.IsTransitive);
  if reference.UseSource then
    target.AddOrSetValue('useSource', true);
  TJsonUtils.AddIfNotEmpty(target, 'manifestHash', reference.ManifestHash);

  //The dproj stores references as nested XML, so this is a tree rather than a general DAG -
  //no cycle guard needed.
  children := target.AddOrSetSequence('dependencies');
  if reference.HasChildren then
  begin
    for child in reference.Children do
      PackageReference(child, children.AddMapping);
  end;
end;

class procedure TJsonProjections.PackageReferenceTree(const root : IPackageReference; const target : IYAMLMapping);
var
  packages : IYAMLSequence;
  child : IPackageReference;
begin
  packages := target.AddOrSetSequence('packages');
  //GetPackageReferences returns nil when the project has no <Packages> node at all.
  if root = nil then
    exit;
  for child in root.Children do
    PackageReference(child, packages.AddMapping);
end;

class procedure TJsonProjections.ProjectInfo(const editor : IProjectEditor; const target : IYAMLMapping);
var
  ambiguousVersions : string;
begin
  TJsonUtils.AddIfNotEmpty(target, 'projectFile', editor.ProjectFile);
  TJsonUtils.AddCompiler(target, 'compiler', editor.CompilerVersion);
  //Deliberately distinct from `compiler` - this is the compiler the project was last managed
  //by DPM under, which differs after an in place IDE upgrade.
  TJsonUtils.AddCompiler(target, 'dpmCompiler', editor.DPMCompilerVersion);
  TJsonUtils.AddIfNotEmpty(target, 'projectVersion', editor.ProjectVersion);

  //Several IDE releases share a ProjectVersion. Say so rather than letting a caller assume the
  //inferred compiler is certain.
  ambiguousVersions := '';
  if IsAmbigousProjectVersion(editor.ProjectVersion, ambiguousVersions) then
    TJsonUtils.AddIfNotEmpty(target, 'ambiguousProjectVersions', ambiguousVersions);

  target.AddOrSetValue('appType', AppTypeToString(editor.AppType));
  TJsonUtils.AddPlatforms(target, 'platforms', editor.Platforms);
  target.AddOrSetValue('hasDpm', editor.HasDPM);
  target.AddOrSetValue('hasPackages', editor.HasPackages);

  PackageReferenceTree(editor.GetPackageReferences, target);
end;

end.
