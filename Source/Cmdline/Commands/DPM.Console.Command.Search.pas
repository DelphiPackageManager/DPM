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

unit DPM.Console.Command.Search;

interface

uses
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  ///<summary>
  ///  Searches the configured package feeds and reports the full metadata record for each hit.
  ///</summary>
  ///<remarks>
  ///  This differs from 'list' in what it returns, not just how. 'list' is backed by the
  ///  PackageList endpoint and IPackageListItem, which carries only id, version, platforms and
  ///  signing status. 'search' is backed by the PackageSearch feed and IPackageSearchResultItem,
  ///  which carries the description, tags, licence and urls - the things you actually need to
  ///  choose a package.
  ///</remarks>
  TSearchCommand = class(TBaseCommand)
  private
    FRepositoryManager : IPackageRepositoryManager;
    procedure EmitTextResults(const searchResult : IPackageSearchResult);
    procedure EmitJsonResults(const searchResult : IPackageSearchResult);
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                       const repositoryManager : IPackageRepositoryManager); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  VSoft.YAML,
  DPM.Console.RawIO,
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.SearchCmd,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections;

{ TSearchCommand }

constructor TSearchCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                                  const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

procedure TSearchCommand.EmitTextResults(const searchResult : IPackageSearchResult);
var
  item : IPackageSearchResultItem;
  resultString : string;
  signingTag : string;
begin
  if not searchResult.Results.Any then
  begin
    Logger.Information('No packages were found');
    exit;
  end;

  for item in searchResult.Results do
  begin
    // Gallery-reported signing status. Suppress entirely when unsigned;
    // when signed with no extractable CN, render bare "(signed)".
    if not item.IsSigned then
      signingTag := ''
    else if item.SignedBy <> '' then
      signingTag := ' (signed by ' + item.SignedBy + ')'
    else
      signingTag := ' (signed)';

    resultString := item.Id + ' ' + item.Version.ToStringNoMeta +
                    ' [' + DPMPlatformsToString(item.SupportedPlatforms) + ']' + signingTag;
    Logger.Information(resultString);
    if item.Description <> '' then
      Logger.Information('    ' + item.Description);
  end;
end;

procedure TSearchCommand.EmitJsonResults(const searchResult : IPackageSearchResult);
var
  doc : IYAMLDocument;
  root : IYAMLMapping;
  packages : IYAMLSequence;
  item : IPackageSearchResultItem;
  options : TSearchCmdOptions;
begin
  options := TSearchCmdOptions.Default;

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;

  //Echo back what was actually queried. A caller that guessed the compiler wrong should be able
  //to see that from the response rather than from an empty result set.
  TJsonUtils.AddCompiler(root, 'compiler', options.CompilerVersion);
  TJsonUtils.AddIfNotEmpty(root, 'searchTerms', options.SearchTerms);
  root.AddOrSetValue('skip', searchResult.Skip);
  root.AddOrSetValue('totalCount', searchResult.TotalCount);

  //Always emitted, so an empty result is [] rather than a missing key.
  packages := root.AddOrSetSequence('packages');
  for item in searchResult.Results do
    TJsonProjections.SearchResultItem(item, packages.AddMapping, false);

  TStdOut.WriteLine(TJsonUtils.ToCompactJson(doc));
end;

function TSearchCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TSearchCmdOptions;
  searchResult : IPackageSearchResult;
  jsonMode : boolean;
  config : IConfiguration;
begin
  result := TExitCode.Error;

  options := TSearchCmdOptions.Default;
  options.ApplyCommon(TCommonOptions.Default);
  jsonMode := options.OutputFormat = TOutputFormat.Json;

  if not options.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if options.ConfigFile = '' then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);
  FRepositoryManager.Initialize(config);

  searchResult := FRepositoryManager.GetPackageFeed(cancellationToken, options, options.CompilerVersion);
  if cancellationToken.IsCancelled then
    exit;

  if jsonMode then
    EmitJsonResults(searchResult)
  else
    EmitTextResults(searchResult);

  result := TExitCode.OK;
end;

end.
