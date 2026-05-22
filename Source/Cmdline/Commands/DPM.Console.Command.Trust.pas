{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Console.Command.Trust;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base;


type
  TTrustCommand = class(TBaseCommand)
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  end;


implementation

uses
  System.SysUtils,
  Spring.Collections,
  DPM.Core.Configuration.Classes,
  DPM.Core.Utils.Config,
  DPM.Core.Options.Common,
  DPM.Core.Options.Trust;

function NormaliseSpki(const value : string) : string;
begin
  result := LowerCase(Trim(value));
end;

function FindPublisherIndex(const list : IList<ITrustedPublisherConfig>; const spki : string) : integer;
var
  target : string;
  i : integer;
begin
  target := NormaliseSpki(spki);
  for i := 0 to list.Count - 1 do
    if NormaliseSpki(list[i].Spki) = target then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function FindRepositoryIndex(const list : IList<ITrustedRepositoryConfig>; const spki : string) : integer;
var
  target : string;
  i : integer;
begin
  target := NormaliseSpki(spki);
  for i := 0 to list.Count - 1 do
    if NormaliseSpki(list[i].Spki) = target then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TTrustCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TTrustOptions;
  config : IConfiguration;
  signing : ISigningConfig;
  i : integer;
  configPath : string;
  publisher : ITrustedPublisherConfig;
  repo : ITrustedRepositoryConfig;
  idx : integer;
begin
  TTrustOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TTrustOptions.Default;
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  FConfigurationManager.EnsureDefaultConfig;
  configPath := TConfigUtils.GetDefaultConfigFileName;
  config := FConfigurationManager.LoadConfig(configPath);
  if config = nil then
  begin
    Logger.Error('Could not load configuration file.');
    exit(TExitCode.Error);
  end;
  signing := config.Signing;

  case options.Command of

    tsList, tsShow :
      begin
        Logger.Information('Validation mode         : ' + signing.ValidationMode);
        Logger.Information('Author downgrade policy : ' + signing.AuthorDowngradePolicy);
        Logger.Information('');
        if signing.TrustedPublishers.Count = 0 then
          Logger.Information('Trusted publishers: (none configured)')
        else
        begin
          Logger.Information('Trusted publishers:');
          for i := 0 to signing.TrustedPublishers.Count - 1 do
            Logger.Information('  ' + signing.TrustedPublishers[i].Name + '  ' +
              signing.TrustedPublishers[i].Spki);
        end;
        Logger.Information('');
        if signing.TrustedRepositories.Count = 0 then
          Logger.Information('Trusted repositories: (none configured)')
        else
        begin
          Logger.Information('Trusted repositories:');
          for i := 0 to signing.TrustedRepositories.Count - 1 do
            Logger.Information('  ' + signing.TrustedRepositories[i].Url + '  ' +
              signing.TrustedRepositories[i].Spki);
        end;
        result := TExitCode.OK;
      end;

    tsAdd :
      begin
        if options.Kind = tkPublisher then
        begin
          idx := FindPublisherIndex(signing.TrustedPublishers, options.Spki);
          if idx >= 0 then
            signing.TrustedPublishers[idx].Name := options.Name
          else
          begin
            publisher := TTrustedPublisherConfig.Create;
            publisher.Name := options.Name;
            publisher.Spki := options.Spki;
            signing.TrustedPublishers.Add(publisher);
          end;
        end
        else
        begin
          idx := FindRepositoryIndex(signing.TrustedRepositories, options.Spki);
          if idx >= 0 then
            signing.TrustedRepositories[idx].Url := options.Url
          else
          begin
            repo := TTrustedRepositoryConfig.Create;
            repo.Url := options.Url;
            repo.Spki := options.Spki;
            signing.TrustedRepositories.Add(repo);
          end;
        end;
        if not FConfigurationManager.SaveConfig(config, configPath) then
        begin
          Logger.Error('Failed to save configuration.');
          exit(TExitCode.Error);
        end;
        Logger.Success('Trust entry added.');
        result := TExitCode.OK;
      end;

    tsRemove :
      begin
        if options.Kind = tkPublisher then
        begin
          idx := FindPublisherIndex(signing.TrustedPublishers, options.Spki);
          if idx < 0 then
          begin
            Logger.Error('No trusted publisher found with that SPKI.');
            exit(TExitCode.InvalidArguments);
          end;
          signing.TrustedPublishers.Delete(idx);
        end
        else
        begin
          idx := FindRepositoryIndex(signing.TrustedRepositories, options.Spki);
          if idx < 0 then
          begin
            Logger.Error('No trusted repository found with that SPKI.');
            exit(TExitCode.InvalidArguments);
          end;
          signing.TrustedRepositories.Delete(idx);
        end;
        if not FConfigurationManager.SaveConfig(config, configPath) then
        begin
          Logger.Error('Failed to save configuration.');
          exit(TExitCode.Error);
        end;
        Logger.Success('Trust entry removed.');
        result := TExitCode.OK;
      end;

  else
    Logger.Error('Unknown trust subcommand.');
    result := TExitCode.InvalidArguments;
  end;
end;

end.
