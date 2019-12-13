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

unit DPM.Core.Sources.Manager;

interface

uses
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
    function AddSource(const options: TSourcesOptions): Boolean;
    function DisableSource(const options: TSourcesOptions): Boolean;
    function EnableSource(const options: TSourcesOptions): Boolean;
    function ListSources(const options: TSourcesOptions): Boolean;
    function RemoveSource(const options: TSourcesOptions): Boolean;
    function UpdateSource(const options: TSourcesOptions): Boolean;
  public
    constructor Create(const logger : ILogger; const configManager : IConfigurationManager);reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.System,
  DPM.Core.Sources.Types,
  DPM.Core.Constants,
  DPM.Core.Configuration.Classes;

{ TSourcesManager }

function TSourcesManager.AddSource(const options: TSourcesOptions): Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if options.Name = '' then
  begin
    FLogger.Error('Required parameter [name] is blank.');
    exit;
  end;

  if options.Source = '' then
  begin
    FLogger.Error('Required parameter [source] is blank.');
    exit;
  end;

  if config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, options.Name);
                      end).Any then
  begin
    FLogger.Error('Source with name [' + options.Name + '] already exists');
    exit;
  end;
  source := TSourceConfig.Create(FLogger);
  source.Name := options.Name;
  source.Source := options.Source;
  source.UserName := options.UserName;
  source.Password := options.Password;
  source.IsEnabled := true;
  config.Sources.Add(source);
  result := FConfigManager.SaveConfig(config);

  FLogger.Information('Source [' + source.Name + '] added.');


end;

constructor TSourcesManager.Create(const logger: ILogger; const configManager : IConfigurationManager);
begin
  FLogger := logger;
  FConfigManager := configManager;
end;

function TSourcesManager.DisableSource(const options: TSourcesOptions): Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error('Required parameter [name] is blank.');
    exit;
  end;

  source := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, options.Name);
                      end).FirstOrDefault;
  if source = nil then
  begin
    FLogger.Error('Source with name [' + options.Name + '] not found.');
    exit;
  end;
  source.IsEnabled := false;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Disabled Source  [' + options.Name + '].', false);
end;


function TSourcesManager.EnableSource(const options: TSourcesOptions): Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error('Required parameter [name] is blank.');
    exit;
  end;

  source := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, options.Name);
                      end).FirstOrDefault;
  if source = nil then
  begin
    FLogger.Error('Source with name [' + options.Name + '] not found.');
    exit;
  end;
  source.IsEnabled := true;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Enabled Source  [' + options.Name + '].', false);
end;

function TSourcesManager.ListSources(const options: TSourcesOptions): Boolean;
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

begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Format = TSourcesFormat.Short then
  begin
    for i := 0 to config.Sources.Count -1 do
      FLogger.Information(Format('%s %s',[EnabledToChar(config.Sources[i].IsEnabled), config.Sources[i].Source]));
  end
  else
  begin
    FLogger.Information('Registered Sources:');
    FLogger.Information('', false);
    for i := 0 to config.Sources.Count -1 do
    begin
      FLogger.Information(Format('  %d.  %s  [%s]',[i+1,config.Sources[i].Name,EnabledToString(config.Sources[i].IsEnabled)]));
      FLogger.Information(Format('      %s',[config.Sources[i].Source]) , false);
    end;
  end;
  result := true;
end;

function TSourcesManager.RemoveSource(const options: TSourcesOptions): Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error('Required parameter [name] is blank.');
    exit;
  end;

  source := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, options.Name);
                      end).FirstOrDefault;
  if source = nil then
  begin
    FLogger.Error('Source with name [' + options.Name + '] not found.');
    exit;
  end;
  config.Sources.Remove(source);
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Source [' + options.Name + '] removed.', false);
end;

function TSourcesManager.UpdateSource(const options: TSourcesOptions): Boolean;
var
  config : IConfiguration;
  source : ISourceConfig;
begin
  result := false;
  if options.ConfigFile = '' then
  begin
    FLogger.Error('No configuration file specified');
    exit;
  end;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;
  if options.Name = '' then
  begin
    FLogger.Error('Required parameter [name] is blank.');
    exit;
  end;

  source := config.Sources.Where(function(const item : ISourceConfig):boolean
                      begin
                        result := SameText(item.Name, options.Name);
                      end).FirstOrDefault;
  if source = nil then
  begin
    FLogger.Error('Source with name [' + options.Name + '] not found.');
    exit;
  end;
  source.Name := options.Name;
  source.Source := options.Source;
  source.UserName := options.UserName;
  source.Password := options.Password;
  result := FConfigManager.SaveConfig(config);
  if result then
    FLogger.Information('Source [' + options.Name + '] updated.', false);

end;

end.
