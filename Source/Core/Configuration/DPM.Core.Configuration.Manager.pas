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

unit DPM.Core.Configuration.Manager;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces;

type
  TConfigurationManager = class(TInterfacedObject, IConfigurationManager)
  private
    FLogger : ILogger;
  protected
    function LoadConfig(const configFile : string) : IConfiguration;
    function SaveConfig(const configuration : IConfiguration; const fileName : string = '') : Boolean;
    function NewConfig : IConfiguration;
    function EnsureDefaultConfig : boolean;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Config,
  DPM.Core.Constants,
  DPM.Core.Configuration.Classes;

{ TConfigurationManager }

constructor TConfigurationManager.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

function TConfigurationManager.EnsureDefaultConfig : boolean;
var
  sDefaultConfigFile : string;
  config : IConfiguration;
begin
  sDefaultConfigFile := TConfigUtils.GetDefaultConfigFileName;
  //if it exists, load it to make sure it's valid.
  if FileExists(sDefaultConfigFile) then
  begin
    config := LoadConfig(sDefaultConfigFile); //this will log errors
    if ((config <> nil) and (not config.Sources.Any)) then
      config.AddDefaultSources;

    result := config <> nil;
    if not result then
      FLogger.Error('Unable to load the default config  file : ' + cDefaultConfigFile);

  end
  else
  begin
    config := NewConfig;
    TConfigUtils.EnsureDefaultConfigDir;
    result := SaveConfig(config, sDefaultConfigFile);
    if not result then
      FLogger.Error('No default config file found and unable to create one at : ' + cDefaultConfigFile);
  end;
end;

function TConfigurationManager.LoadConfig(const configFile : string) : IConfiguration;
var
  config : IConfigurationLoadSave;
begin
  result := nil;
  config := TConfiguration.Create(FLogger);

  if config.LoadFromFile(configFile) then
    result := config;
end;

function TConfigurationManager.NewConfig : IConfiguration;

begin
  result := TConfiguration.Create(FLogger);
  result.AddDefaultSources;
  result.PackageCacheLocation := GetEnvironmentVariable(cDPMPackageCacheEnviromentVar);
  if result.PackageCacheLocation = '' then
    result.PackageCacheLocation := TSystemUtils.ExpandEnvironmentStrings(cDefaultPackageCache);

end;

function TConfigurationManager.SaveConfig(const configuration : IConfiguration; const fileName : string) : Boolean;
var
  config : IConfigurationLoadSave;
begin
  result := false;
  config := configuration as IConfigurationLoadSave;
  if config <> nil then
    result := config.SaveToFile(fileName)
  else
    FLogger.Error('configuration does not implement IConfigurationLoadSave');
end;

end.


