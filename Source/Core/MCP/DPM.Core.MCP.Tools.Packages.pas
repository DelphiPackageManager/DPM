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

unit DPM.Core.MCP.Tools.Packages;

interface

uses
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.Tools.Common;

type
  TMCPPackageToolBase = class(TMCPToolBase)
  protected
    FContext : IMCPToolContext;
    //These all reach a package feed over the network.
    function GetIsOpenWorld : boolean; override;
  public
    constructor Create(const context : IMCPToolContext);
  end;

  TMCPSearchPackagesTool = class(TMCPPackageToolBase)
  protected
    function GetName : string; override;
    function GetTitle : string; override;
    function GetDescription : string; override;
    procedure BuildInputSchema(const target : IYAMLMapping); override;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; override;
  end;

  TMCPPackageInfoTool = class(TMCPPackageToolBase)
  protected
    function GetName : string; override;
    function GetTitle : string; override;
    function GetDescription : string; override;
    procedure BuildInputSchema(const target : IYAMLMapping); override;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; override;
  end;

  TMCPPackageVersionsTool = class(TMCPPackageToolBase)
  protected
    function GetName : string; override;
    function GetTitle : string; override;
    function GetDescription : string; override;
    procedure BuildInputSchema(const target : IYAMLMapping); override;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; override;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections;

const
  cProjectArgDescription =
    'Optional path to the Delphi project (.dproj) this question is about. When given, the ' +
    'compiler version is read from that project, so you do not have to know it. Prefer this ' +
    'over guessing a compiler.';

  cCompilerArgDescription =
    'Delphi compiler version to target. DPM publishes a separate build of each package for ' +
    'each Delphi release, so results are always scoped to one. Accepts "12.0", "12", "XE2", ' +
    'or the canonical "delphi12.0". If omitted, it is read from projectPath, or from the ' +
    'project this server was started in. Every response states the compiler actually used.';

{ TMCPPackageToolBase }

constructor TMCPPackageToolBase.Create(const context : IMCPToolContext);
begin
  inherited Create;
  FContext := context;
end;

function TMCPPackageToolBase.GetIsOpenWorld : boolean;
begin
  result := true;
end;

///<summary> Fails the call with an actionable message when no sources are configured. </summary>
procedure CheckHasSources(const context : IMCPToolContext);
begin
  if not context.RepositoryManager.HasSources then
    raise EMCPToolError.Create('No DPM package sources are configured, so there is nothing to ' +
      'search. Add one with: dpm sources add -name=DPM -source=https://delphi.dev/api');
end;

{ TMCPSearchPackagesTool }

function TMCPSearchPackagesTool.GetName : string;
begin
  result := 'dpm_search_packages';
end;

function TMCPSearchPackagesTool.GetTitle : string;
begin
  result := 'Search DPM packages';
end;

function TMCPSearchPackagesTool.GetDescription : string;
begin
  result :=
    'Search for Delphi (Object Pascal) libraries published to DPM. DPM is the package manager ' +
    'for Embarcadero Delphi - it is to Delphi what npm is to Node or NuGet is to .NET. Use ' +
    'this to find a library by name or by capability before suggesting it for a Delphi ' +
    'project, for example "json", "http client", "zip", or a known name like "Spring4D". ' +
    'Returns each package''s id, best matching version, description, licence, authors, tags ' +
    'and the target platforms it supports. ' +
    'Packages are published per Delphi compiler version, so results are scoped to one ' +
    'compiler and every response says which was used. ' +
    'This is read only - it never downloads or installs anything. To actually add a package ' +
    'to a project, run the CLI: dpm install <PackageId> <Project.dproj> --compiler=<version>';
end;

procedure TMCPSearchPackagesTool.BuildInputSchema(const target : IYAMLMapping);
var
  properties : IYAMLMapping;
begin
  properties := BeginSchema(target);
  AddStringProperty(properties, 'query',
    'Free text, matched against package id, description and tags. A capability ("json ' +
    'serialization") works as well as a name ("Spring4D").');
  AddStringProperty(properties, 'compiler', cCompilerArgDescription);
  AddStringProperty(properties, 'projectPath', cProjectArgDescription);
  AddStringProperty(properties, 'platform',
    'Optional platform filter, e.g. Win32, Win64, Linux64, Android64, iOS64, MacOS64. ' +
    'Omit to see every platform each package supports.');
  AddBoolProperty(properties, 'includePrerelease',
    'Include pre-release versions such as 1.2.0-beta1. Prefer stable releases unless the ' +
    'user asked otherwise.', false);
  AddIntProperty(properties, 'limit', 'Maximum number of packages to return.', 20, 1, 100);
  AddRequired(target, ['query']);
end;

function TMCPSearchPackagesTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
var
  options : TSearchOptions;
  compiler : TCompilerVersion;
  wantedPlatform : TDPMPlatform;
  platformText : string;
  searchResult : IPackageSearchResult;
  item : IPackageSearchResultItem;
  doc : IYAMLDocument;
  root : IYAMLMapping;
  packages : IYAMLSequence;
begin
  CheckHasSources(FContext);
  compiler := ResolveCompilerArg(arguments, FContext);

  wantedPlatform := TDPMPlatform.UnknownPlatform;
  platformText := OptionalStringArg(arguments, 'platform');
  if platformText <> '' then
    wantedPlatform := ParsePlatformArg(platformText);

  options := TSearchOptions.Create;
  try
    options.SearchTerms := RequireStringArg(arguments, 'query');
    options.CompilerVersion := compiler;
    options.Prerelease := OptionalBoolArg(arguments, 'includePrerelease', false);
    //The base class defaults these to false, which would silently drop every commercial and
    //trial package from the results with no indication anything had been filtered.
    options.Commercial := true;
    options.Trial := true;
    options.Skip := 0;
    options.Take := OptionalIntArg(arguments, 'limit', 20, 1, 100);

    searchResult := FContext.RepositoryManager.GetPackageFeed(cancellationToken, options, compiler);
  finally
    options.Free;
  end;

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  //Echo the compiler back. If it was inferred and inferred wrongly, the caller can see that
  //here instead of concluding the package does not exist.
  TJsonUtils.AddCompiler(root, 'compiler', compiler);
  TJsonUtils.AddIfNotEmpty(root, 'query', RequireStringArg(arguments, 'query'));
  root.AddOrSetValue('totalCount', searchResult.TotalCount);

  packages := root.AddOrSetSequence('packages');
  for item in searchResult.Results do
  begin
    //The feed does not filter by platform for a search, so do it here rather than pretend the
    //filter was applied upstream. Win64/Win64x are interchangeable, hence PlatformSatisfiedBy.
    if (wantedPlatform <> TDPMPlatform.UnknownPlatform) and
       (not PlatformSatisfiedBy(wantedPlatform, item.SupportedPlatforms)) then
      continue;
    TJsonProjections.SearchResultItem(item, packages.AddMapping, false);
  end;

  if platformText <> '' then
    root.AddOrSetValue('platformFilter', DPMPlatformToString(wantedPlatform));

  result := TJsonUtils.ToCompactJson(doc);
end;

{ TMCPPackageInfoTool }

function TMCPPackageInfoTool.GetName : string;
begin
  result := 'dpm_package_info';
end;

function TMCPPackageInfoTool.GetTitle : string;
begin
  result := 'DPM package details';
end;

function TMCPPackageInfoTool.GetDescription : string;
begin
  result :=
    'Get the full details of one DPM package: description, authors, licence, project and ' +
    'repository URLs, the Delphi platforms it supports, and the other packages it depends on ' +
    'with the version range each dependency is allowed to satisfy. ' +
    'Use this after dpm_search_packages when you need to know what a package will pull in, ' +
    'whether it supports the platform the project targets, or what licence applies. ' +
    'Omit "version" to get the newest release for the compiler. Read only.';
end;

procedure TMCPPackageInfoTool.BuildInputSchema(const target : IYAMLMapping);
var
  properties : IYAMLMapping;
begin
  properties := BeginSchema(target);
  AddStringProperty(properties, 'packageId',
    'Exact package id, e.g. "Spring4D.Core" or "VSoft.HttpClient". Case insensitive. Use ' +
    'dpm_search_packages first if you only know part of the name.');
  AddStringProperty(properties, 'version',
    'Exact version, e.g. "2.1.0". Omit for the newest available.');
  AddStringProperty(properties, 'compiler', cCompilerArgDescription);
  AddStringProperty(properties, 'projectPath', cProjectArgDescription);
  AddBoolProperty(properties, 'includePrerelease',
    'Consider pre-release versions when resolving the newest.', false);
  AddRequired(target, ['packageId']);
end;

function TMCPPackageInfoTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
var
  compiler : TCompilerVersion;
  packageId : string;
  versionText : string;
  latest : IPackageInfo;
  metadata : IPackageSearchResultItem;
  doc : IYAMLDocument;
  root : IYAMLMapping;
begin
  CheckHasSources(FContext);
  compiler := ResolveCompilerArg(arguments, FContext);
  packageId := RequireStringArg(arguments, 'packageId');
  versionText := OptionalStringArg(arguments, 'version');

  if versionText = '' then
  begin
    latest := FContext.RepositoryManager.FindLatestVersion(cancellationToken, packageId, compiler,
                TPackageVersion.Empty, OptionalBoolArg(arguments, 'includePrerelease', false), '');
    if latest = nil then
      raise EMCPToolError.Create('No package "' + packageId + '" was found for ' +
        CompilerToString(compiler) + '. Check the id with dpm_search_packages, and note that a ' +
        'package may simply not be published for this Delphi version.');
    versionText := latest.Version.ToStringNoMeta;
  end;

  metadata := FContext.RepositoryManager.GetPackageMetaData(cancellationToken, packageId, versionText, compiler);
  if metadata = nil then
    raise EMCPToolError.Create('No metadata found for "' + packageId + '" version ' + versionText +
      ' on ' + CompilerToString(compiler) + '. Use dpm_package_versions to see which versions exist.');

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  TJsonProjections.SearchResultItem(metadata, root, true);
  result := TJsonUtils.ToCompactJson(doc);
end;

{ TMCPPackageVersionsTool }

function TMCPPackageVersionsTool.GetName : string;
begin
  result := 'dpm_package_versions';
end;

function TMCPPackageVersionsTool.GetTitle : string;
begin
  result := 'DPM package versions';
end;

function TMCPPackageVersionsTool.GetDescription : string;
begin
  result :=
    'List every published version of one DPM package for a given Delphi compiler version, ' +
    'newest first. Use this to choose a version to pin, to check whether a newer release ' +
    'exists than the one a project already references, or to find out whether a package is ' +
    'still published for an older Delphi. Pre-release versions are excluded unless asked for. ' +
    'Read only.';
end;

procedure TMCPPackageVersionsTool.BuildInputSchema(const target : IYAMLMapping);
var
  properties : IYAMLMapping;
begin
  properties := BeginSchema(target);
  AddStringProperty(properties, 'packageId', 'Exact package id, e.g. "Spring4D.Core".');
  AddStringProperty(properties, 'compiler', cCompilerArgDescription);
  AddStringProperty(properties, 'projectPath', cProjectArgDescription);
  AddBoolProperty(properties, 'includePrerelease',
    'Include pre-release versions such as 1.2.0-beta1.', false);
  AddRequired(target, ['packageId']);
end;

function TMCPPackageVersionsTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
var
  compiler : TCompilerVersion;
  packageId : string;
  versions : IList<TPackageVersion>;
  i : integer;
  doc : IYAMLDocument;
  root : IYAMLMapping;
  seq : IYAMLSequence;
begin
  CheckHasSources(FContext);
  compiler := ResolveCompilerArg(arguments, FContext);
  packageId := RequireStringArg(arguments, 'packageId');

  versions := FContext.RepositoryManager.GetPackageVersions(cancellationToken, compiler, packageId,
                OptionalBoolArg(arguments, 'includePrerelease', false));

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  root.AddOrSetValue('packageId', packageId);
  TJsonUtils.AddCompiler(root, 'compiler', compiler);

  seq := root.AddOrSetSequence('versions');
  if versions <> nil then
  begin
    for i := 0 to versions.Count - 1 do
      seq.AddValue(versions[i].ToStringNoMeta);
  end;

  //An empty list is a real answer, not a failure - it usually means the package is not
  //published for this Delphi version. Say so, since a bare [] invites the wrong conclusion.
  if seq.Count = 0 then
    root.AddOrSetValue('note', 'No versions of "' + packageId + '" are published for ' +
      CompilerToString(compiler) + '. It may exist for a different Delphi version.');

  result := TJsonUtils.ToCompactJson(doc);
end;

end.
