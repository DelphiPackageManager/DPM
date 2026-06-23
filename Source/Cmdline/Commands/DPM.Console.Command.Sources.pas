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

unit DPM.Console.Command.Sources;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Git.Interfaces,
  DPM.Core.Sources.Interfaces;

type
  TSourcesCommand = class(TBaseCommand)
  private
    FSourcesManager : ISourcesManager;
    FGitClient : IGitClient;
    //true if path is an existing folder laid out as a registry (folder-per-id <id>.dspec.yaml).
    function LooksLikeRegistryFolder(const path : string) : boolean;
    //force-refresh the cached mirror for git registry sources (all, or the named one).
    function RefreshSources(const cancellationToken : ICancellationToken) : boolean;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
    function ForceNoBanner: Boolean; override;

  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const sourcesManager : ISourcesManager; const gitClient : IGitClient);reintroduce;
  end;


implementation

uses
  System.SysUtils,
  System.IOUtils,
  VSoft.Uri,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Utils.Path,
  DPM.Core.Options.Common,
  DPM.Core.Options.Sources,
  DPM.Core.Sources.Types,
  DPM.Core.Registry.Interfaces,
  DPM.Core.Registry.Catalog;


{ TSourcesCommand }

constructor TSourcesCommand.Create(const logger: ILogger; const configurationManager : IConfigurationManager; const sourcesManager : ISourcesManager; const gitClient : IGitClient);
begin
  inherited Create(logger,configurationManager);
  FSourcesManager := sourcesManager;
  FGitClient := gitClient;
end;

function TSourcesCommand.LooksLikeRegistryFolder(const path : string) : boolean;
var
  subDir : string;
begin
  result := false;
  if not TDirectory.Exists(path) then
    exit;
  //a registry folder has a sub-folder per package id, each containing a dspec file.
  for subDir in TDirectory.GetDirectories(path) do
  begin
    if Length(TPathUtils.FindDspecFiles(subDir)) > 0 then
      exit(true);
  end;
end;

function TSourcesCommand.RefreshSources(const cancellationToken : ICancellationToken) : boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
  catalog : IRegistryCatalog;
  nameFilter : string;
  anyGit : boolean;
begin
  result := true;
  config := FConfigurationManager.LoadConfig(TSourcesOptions.Default.ConfigFile);
  if config = nil then
    exit(false);

  nameFilter := TSourcesOptions.Default.Name;
  anyGit := false;
  for source in config.Sources do
  begin
    if source.SourceType <> TSourceType.GitRegistry then
      continue;
    if (nameFilter <> '') and (not SameText(nameFilter, source.Name)) then
      continue;
    if not source.IsEnabled then
      continue;
    anyGit := true;

    catalog := TRegistryCatalog.Create(Logger, FGitClient, source.Name, source.Source, cDefaultRegistriesFolder, 0);
    if DirectoryExists(source.Source) then
      //a local folder registry is read in place - there is no mirror to pull.
      Logger.Information(Format('Registry [%s] is a local folder (read in place) : %d package(s).',
        [source.Name, catalog.GetPackageIds(cancellationToken).Count]))
    else
    begin
      Logger.Information('Refreshing registry [' + source.Name + '] from ' + source.Source);
      if not catalog.EnsureUpdated(cancellationToken, true) then
      begin
        Logger.Error('Failed to refresh registry [' + source.Name + ']');
        result := false;
      end
      else
        Logger.Information(Format('  %d package(s).', [catalog.GetPackageIds(cancellationToken).Count]));
    end;
  end;

  if not anyGit then
    Logger.Information('No git registry sources to refresh.');
end;

function TSourcesCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  success : boolean;
  newConfig : IConfiguration;
  uri : IUri;
  uriError : string;
begin
  TSourcesOptions.Default.ApplyCommon(TCommonOptions.Default);

  if not (TSourcesOptions.Default.Command in [TSourcesSubCommand.Invalid, TSourcesSubCommand.List, TSourcesSubCommand.Refresh])  then
  begin
    //name is required
    if TSourcesOptions.Default.Name = '' then
    begin
      Logger.Error('Name is required for this command.');
      result := TExitCode.MissingArg;
      exit;
    end;
  end;

  //TODO : Find a way to do this for all commands that might write to a new config file.
  if not FileExists(TSourcesOptions.Default.ConfigFile) then
  begin
    newConfig := FConfigurationManager.NewConfig;
    FConfigurationManager.SaveConfig(newConfig,TSourcesOptions.Default.ConfigFile);
  end;


  Logger.Information('');
  result := TExitCode.OK;
  case TSourcesOptions.Default.Command of
    TSourcesSubCommand.Invalid:
    begin
      Logger.Error('invalid command');
      result := TExitCode.InvalidArguments;
      exit;
    end;

    TSourcesSubCommand.Add    :
    begin
      if TSourcesOptions.Default.Source = '' then
      begin
        Logger.Error('Source is required for this command.');
        result := TExitCode.MissingArg;
        exit;
      end;
      if (TSourcesOptions.Default.SourceType = TSourceType.GitRegistry) or
         IsGitRegistryUri(TSourcesOptions.Default.Source) or
         LooksLikeRegistryFolder(TSourcesOptions.Default.Source) then
      begin
        //git registry source - either a git url or a local registry folder.
        //git handles the url/path, so we don't apply the file/http uri validation;
        //a bad source surfaces a clear error on first use (clone/refresh).
        TSourcesOptions.Default.SourceType := TSourceType.GitRegistry;
      end
      else
      begin
        if not TUriFactory.TryParseWithError(TSourcesOptions.Default.Source, false, uri, uriError) then
        begin
          Logger.Error('Source uri is not valid : ' + uriError);
          result := TExitCode.InvalidArguments;
          exit;
        end;

        //this logic is not quite all there.. more testing required.
        if (uri.Scheme = 'file') and (TSourcesOptions.Default.SourceType <> TSourceType.Folder)  then
        begin
          Logger.Error('Source uri is not valid for folder source');
          result := TExitCode.InvalidArguments;
          exit;
        end
        else if uri.Scheme <> 'file' then
        begin
          if (uri.Scheme = 'http') or (uri.Scheme = 'https') then
            TSourcesOptions.Default.SourceType := TSourceType.DPMServer
          else
          begin
            Logger.Error('Invalid source uri scheme : ' + uri.Scheme);
            result := TExitCode.InvalidArguments;
            exit;
          end;
        end;
      end;

      success := FSourcesManager.AddSource(TSourcesOptions.Default);
    end;
    TSourcesSubCommand.Remove : success := FSourcesManager.RemoveSource(TSourcesOptions.Default);
    TSourcesSubCommand.List   : success := FSourcesManager.ListSources(TSourcesOptions.Default);
    TSourcesSubCommand.Enable : success := FSourcesManager.EnableSource(TSourcesOptions.Default);
    TSourcesSubCommand.Disable: success := FSourcesManager.DisableSource(TSourcesOptions.Default);
    TSourcesSubCommand.Update : success := FSourcesManager.UpdateSource(TSourcesOptions.Default);
    TSourcesSubCommand.Refresh: success := RefreshSources(cancellationToken);
  else
    result := TExitCode.NotImplemented;
    exit;
  end;

  if not success then
    result := TExitCode.Error;
end;

function TSourcesCommand.ForceNoBanner: Boolean;
begin
  result := true;
end;

end.
