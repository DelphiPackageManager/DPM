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

unit DPM.Core.Sources.Manager;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Sources,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Configuration.Interfaces;

type
  TSourcesManager = class(TInterfacedObject, ISourcesManager)
  private
    FLogger : ILogger;
    FConfigManager : IConfigurationManager;
  protected
    function AddSource(const options : TSourcesOptions) : Boolean;
    function DisableSource(const options : TSourcesOptions) : Boolean;
    function EnableSource(const options : TSourcesOptions) : Boolean;
    function ListSources(const options : TSourcesOptions) : Boolean;
    function RemoveSource(const options : TSourcesOptions) : Boolean;
    function UpdateSource(const options : TSourcesOptions) : Boolean;
  public
    constructor Create(const logger : ILogger; const configManager : IConfigurationManager); reintroduce;

    //The type a source is, from what it is - for add and update alike. A git url, an existing registry folder
    //(folder-per-id dspecs) or sourceType GitRegistry is a git registry; git checks those when it first uses them.
    //An http(s) url is a DPM server, a file uri or path a folder (and only a folder). False, having said why
    //(E1002), when source can't be a package source at all.
    class function TryResolveSourceType(const logger : ILogger; const source : string; var sourceType : TSourceType) : boolean; static;
    //true if path is an existing folder laid out as a registry (folder-per-id <id>.dspec.yaml).
    class function LooksLikeRegistryFolder(const path : string) : boolean; static;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  VSoft.Uri,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.System,
  DPM.Core.Sources.Types,
  DPM.Core.Constants,
  DPM.Core.Configuration.Classes,
  DPM.Core.Diagnostics;

{ TSourcesManager }

class function TSourcesManager.LooksLikeRegistryFolder(const path : string) : boolean;
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

class function TSourcesManager.TryResolveSourceType(const logger : ILogger; const source : string; var sourceType : TSourceType) : boolean;
var
  uri : IUri;
  uriError : string;
begin
  result := false;
  if (sourceType = TSourceType.GitRegistry) or IsGitRegistryUri(source) or LooksLikeRegistryFolder(source) then
  begin
    //git handles the url/path, so the file/http uri checks don't apply - a bad one is reported when it is first
    //cloned or refreshed.
    sourceType := TSourceType.GitRegistry;
    exit(true);
  end;

  if not TUriFactory.TryParseWithError(source, false, uri, uriError) then
  begin
    logger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + source + '] is not a valid uri : ' + uriError);
    exit;
  end;

  if uri.Scheme = 'file' then
  begin
    if sourceType <> TSourceType.Folder then
    begin
      logger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + source + '] is a file uri, which is only valid for a folder source.');
      exit;
    end;
  end
  else if (uri.Scheme = 'http') or (uri.Scheme = 'https') then
    sourceType := TSourceType.DPMServer
  else
  begin
    logger.Error(TErrorCode.E1002_InvalidSource, 'The source [' + source + '] is not valid - [' + uri.Scheme + '] is not a supported scheme (file, http or https).');
    exit;
  end;
  result := true;
end;

function TSourcesManager.AddSource(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if options.Name = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --name option is required.');
    exit;
  end;

  if options.Source = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --source option is required.');
    exit;
  end;

  if config.Sources.Any(function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, options.Name);
    end) then
  begin
    FLogger.Error(TErrorCode.E1005_SourceAlreadyExists, 'A source named [' + options.Name + '] already exists - use dpm sources update to change it.');
    exit;
  end;
  source := TSourceConfig.Create(FLogger);
  source.Name := options.Name;
  source.Source := options.Source;
  source.UserName := options.UserName;
  source.Password := options.Password;
  source.SourceType := options.SourceType;
  source.IsEnabled := true;
  config.Sources.Add(source);
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Source [' + source.Name + '] added.');


end;

constructor TSourcesManager.Create(const logger : ILogger; const configManager : IConfigurationManager);
begin
  FLogger := logger;
  FConfigManager := configManager;
end;

function TSourcesManager.DisableSource(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --name option is required.');
    exit;
  end;

  source := config.Sources.FirstOrDefault(function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, options.Name);
    end);
  if source = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'There is no source named [' + options.Name + '].');
    exit;
  end;
  source.IsEnabled := false;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Disabled Source  [' + options.Name + '].', false);
end;


function TSourcesManager.EnableSource(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --name option is required.');
    exit;
  end;

  source := config.Sources.FirstOrDefault(function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, options.Name);
    end);
  if source = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'There is no source named [' + options.Name + '].');
    exit;
  end;
  source.IsEnabled := true;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Enabled Source  [' + options.Name + '].', false);
end;

function TSourcesManager.ListSources(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  i : integer;

  function EnabledToString(const enabled : boolean) : string;
  begin
    if enabled then
      result := 'Enabled'
    else
      result := 'Disabled';
  end;

  function EnabledToChar(const enabled : boolean) : string;
  begin
    if enabled then
      result := 'E'
    else
      result := 'D';
  end;

  function SourceTypeToString(const sourceType : TSourceType) : string;
  begin
    case sourceType of
      TSourceType.Folder : result := 'Folder';
      TSourceType.GitRegistry : result := 'Git Registry';
      TSourceType.DPMServer : result := 'DPM Server';
    else
      result := 'Unknown';
    end;
  end;

begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Format = TSourcesFormat.Short then
  begin
    for i := 0 to config.Sources.Count - 1 do
      FLogger.Information(Format('%s %s', [EnabledToChar(config.Sources[i].IsEnabled), config.Sources[i].Source]));
  end
  else
  begin
    FLogger.Information('Registered Sources:');
    FLogger.Information('', false);
    for i := 0 to config.Sources.Count - 1 do
    begin
      FLogger.Information(Format('  %d.  %s  [%s]  (%s)', [i + 1, config.Sources[i].Name, EnabledToString(config.Sources[i].IsEnabled), SourceTypeToString(config.Sources[i].SourceType)]));
      FLogger.Information(Format('      %s', [config.Sources[i].Source]), false);
    end;
  end;
  result := true;
end;

function TSourcesManager.RemoveSource(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --name option is required.');
    exit;
  end;

  source := config.Sources.FirstOrDefault(function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, options.Name);
    end);
  if source = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'There is no source named [' + options.Name + '].');
    exit;
  end;
  config.Sources.Remove(source);
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Source [' + options.Name + '] removed.', false);
end;

function TSourcesManager.UpdateSource(const options : TSourcesOptions) : Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
  newSource : string;
  newType : TSourceType;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'No configuration file was specified.');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --name option is required.');
    exit;
  end;

  source := config.Sources.FirstOrDefault(function(const item : ISourceConfig) : boolean
    begin
      result := SameText(item.Name, options.Name);
    end);
  if source = nil then
  begin
    FLogger.Error(TErrorCode.E1003_UnknownSource, 'There is no source named [' + options.Name + '].');
    exit;
  end;

  //only what was given changes - and what is saved is a source that can be used, checked as add checks it.
  if options.SourceSpecified then
    newSource := options.Source
  else
    newSource := source.Source;
  if options.SourceTypeSpecified then
    newType := options.SourceType
  else
    newType := source.SourceType;
  if newSource = '' then
  begin
    FLogger.Error(TErrorCode.E0101_MissingArgument, 'The --source option can''t be empty.');
    exit;
  end;
  if not TryResolveSourceType(FLogger, newSource, newType) then
    exit;

  source.Source := newSource;
  source.SourceType := newType;
  if options.UserNameSpecified then
    source.UserName := options.UserName;
  if options.PasswordSpecified then
    source.Password := options.Password;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Source [' + options.Name + '] updated.', false);

end;

end.

