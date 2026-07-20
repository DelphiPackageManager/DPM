{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright (c) 2019 Vincent Parrett and contributors             }
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

unit DPM.Console.Command.Upgrade;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Upgrade.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base;

type
  TUpgradeCommand = class(TBaseCommand)
  private
    FUpgradeService : IUpgradeService;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                       const upgradeService : IUpgradeService); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Version,
  DPM.Core.Options.Common,
  DPM.Core.Options.Upgrade,
  DPM.Console.Prompts;

const
  //Requested wording - when we found nothing newer and the user did not ask for
  //prereleases, point out that there might be one.
  cPreReleaseHint = 'There may be newer pre-release versions, use -pr to check for those';

{ TUpgradeCommand }

constructor TUpgradeCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                                   const upgradeService : IUpgradeService);
begin
  inherited Create(logger, configurationManager);
  FUpgradeService := upgradeService;
end;

function TUpgradeCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TUpgradeOptions;
  currentVersion : TPackageVersion;
  checkResult : TUpgradeCheckResult;
  upgradeInfo : IUpgradeInfo;
  installerFile : string;
  proceed : boolean;
  cancelled : boolean;
  versionDesc : string;
begin
  result := TExitCode.Error;

  options := TUpgradeOptions.Default;
  options.ApplyCommon(TCommonOptions.Default);
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  currentVersion := TDPMVersion.CurrentVersion;
  Logger.Information('Current version : ' + TDPMVersion.CurrentVersionString);

  checkResult := FUpgradeService.CheckForUpgrade(cancellationToken, currentVersion, options.PreRelease, upgradeInfo);

  if cancellationToken.IsCancelled then
    exit(TExitCode.OK);

  case checkResult of
    TUpgradeCheckResult.Error :
      exit(TExitCode.Error); //the service logs the reason

    TUpgradeCheckResult.UpToDate :
      begin
        Logger.Success('dpm is up to date.');
        //Only makes sense when we weren't already including prereleases.
        if not options.PreRelease then
          Logger.Information(cPreReleaseHint);
        exit(TExitCode.OK);
      end;

    TUpgradeCheckResult.UpgradeAvailable :
      begin
        versionDesc := upgradeInfo.Version.ToString;
        if upgradeInfo.IsPrerelease then
          versionDesc := versionDesc + ' (pre-release)';

        Logger.Information('');
        Logger.Success('A newer version is available : ' + versionDesc);
        if upgradeInfo.ReleaseUrl <> '' then
          Logger.Information('Release notes : ' + upgradeInfo.ReleaseUrl);

        if options.CheckOnly then
          exit(TExitCode.OK);

        //NonInteractive covers unattended/CI use - the user has already opted
        //out of being asked anything.
        if options.NonInteractive then
          proceed := true
        else
        begin
          Logger.Information('');
          proceed := PromptYesNo('Download and install ' + upgradeInfo.AssetName + '?', true, cancelled);
          if cancelled then
            exit(TExitCode.OK);
        end;

        if not proceed then
        begin
          Logger.Information('Upgrade cancelled.');
          exit(TExitCode.OK);
        end;

        if not FUpgradeService.DownloadUpgrade(cancellationToken, upgradeInfo, installerFile) then
          exit(TExitCode.Error);

        if cancellationToken.IsCancelled then
          exit(TExitCode.OK);

        if not FUpgradeService.LaunchInstaller(installerFile) then
          exit(TExitCode.Error);

        Logger.Information('');
        Logger.Success('The installer has been started. dpm will now exit so that it can complete.');
        result := TExitCode.OK;
      end;
  end;
end;

end.
