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

unit DPM.Console.Command.List;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TListCommand = class(TBaseCommand)
  private
    FRepositoryManager : IPackageRepositoryManager;
    procedure EmitJsonResults(const searchResults : IList<IPackageListItem>);
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repositoryManager : IPackageRepositoryManager);reintroduce;
  end;

implementation

uses
  DPM.Core.Types,
  DPM.Core.Utils.Strings,
  DPM.Core.Options.Common,
  DPM.Core.Options.List,
  VSoft.YAML,
  DPM.Console.RawIO,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections,
  DPM.Core.Diagnostics;

//The page of items - skip, then at most take (0 : all).
function PageOf(const items : IList<IPackageListItem>; const skip : integer; const take : integer) : IList<IPackageListItem>;
var
  i : integer;
  first : integer;
begin
  if (skip <= 0) and (take <= 0) then
    exit(items);
  result := TCollections.CreateList<IPackageListItem>;
  first := skip;
  if first < 0 then
    first := 0;
  for i := first to items.Count - 1 do
  begin
    if (take > 0) and (result.Count >= take) then
      break;
    result.Add(items[i]);
  end;
end;

{ TListCommand }

constructor TListCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

procedure TListCommand.EmitJsonResults(const searchResults : IList<IPackageListItem>);
var
  doc : IYAMLDocument;
  packages : IYAMLSequence;
  item : IPackageListItem;
begin
  doc := TYAML.CreateMapping;
  //Envelope rather than a bare array, to match search and project - a caller can then always
  //reach the results the same way, and there is somewhere to add totals later.
  packages := doc.AsMapping.AddOrSetSequence('packages');
  for item in searchResults do
    TJsonProjections.PackageListItem(item, packages.AddMapping);
  TStdOut.WriteLine(TJsonUtils.ToCompactJson(doc));
end;

function TListCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  searchResults : IList<IPackageListItem>;
  item : IPackageListItem;
  resultString : string;
  signingTag : string;
  config : IConfiguration;
  skip : integer;
  take : integer;
begin
  result := TExitCode.Error;
  TListOptions.Default.ApplyCommon(TCommonOptions.Default);
  if not TListOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if TListOptions.Default.ConfigFile = '' then
  begin
    Logger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    result := TExitCode.InvalidArguments;
    exit;
  end;

  //-skip/-take page the merged list - every source is asked for all of it (each paging itself would page only its
  //own). -take was overridden here, so it never applied.
  skip := TListOptions.Default.Skip;
  take := TListOptions.Default.Take;
  TListOptions.Default.Skip := 0;
  TListOptions.Default.Take := MaxInt;

  config := FConfigurationManager.LoadConfig(TListOptions.Default.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);
  //it reported the source that is not valid.
  if not FRepositoryManager.Initialize(config) then
    exit(TExitCode.InitException);

  searchResults := PageOf(FRepositoryManager.List(cancellationToken, TListOptions.Default), skip, take);

  if TListOptions.Default.OutputFormat = TOutputFormat.Json then
  begin
    //An empty result is a legitimate answer in json mode, not a failure - emit an empty
    //array and exit OK. Text mode keeps its existing behaviour. Unless a source couldn't be asked - then
    //nothing found is not the answer, as in text mode.
    EmitJsonResults(searchResults);
    if (not searchResults.Any) and (FRepositoryManager.UnavailableSources <> '') then
    begin
      Logger.Error(TErrorCode.E1102_PackageNotFoundSourcesUnavailable, 'No packages were found - and the source(s) ' +
        FRepositoryManager.UnavailableSources + ' could not be used.');
      exit(TExitCode.Error);
    end;
    result := TExitCode.OK;
    exit;
  end;

  //TODO : re-implement this
  if searchResults.Any then
  begin
    //group by id+version+compiler, collect platforms
    for item in searchResults do
    begin
      if cancellationToken.IsCancelled then
        exit;
      // Gallery-reported signing status. Suppress entirely when unsigned;
      // when signed with no extractable CN, render bare "(signed)".
      if not item.IsSigned then
        signingTag := ''
      else if item.SignedBy <> '' then
        signingTag := ' (signed by ' + item.SignedBy + ')'
      else
        signingTag := ' (signed)';
      resultString := TStringUtils.PadRight(item.Id, 24) + #9+'v'  + TStringUtils.PadRight(item.Version.ToString, 15) + ' [Delphi '  + CompilerToString(item.CompilerVersion) +  ' - ' + DPMPlatformsToString(item.Platforms) + ']' + signingTag;
      Logger.Information(resultString);
    end;
    result := TExitCode.OK;
  end
  else if FRepositoryManager.UnavailableSources <> '' then
    Logger.Error(TErrorCode.E1102_PackageNotFoundSourcesUnavailable, 'No packages were found - and the source(s) ' +
      FRepositoryManager.UnavailableSources + ' could not be used.')
  else
    Logger.Error(TErrorCode.E1101_PackageNotFound, 'No packages were found.');

end;

end.
