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

unit DPM.Console.Command.Cache;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Package.Installer.Interfaces;

type
  TCacheCommand = class(TBaseCommand)
  private
    FPackageInstaller : IPackageInstaller;
    FPackageCache : IPackageCache;

  protected
    function Execute(const cancellationToken : ICancellationToken): TExitCode; override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const packageInstaller : IPackageInstaller;
                       const packageCache : IPackageCache); reintroduce;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Utils.Config,
  DPM.Core.Options.Common,
  DPM.Core.Options.Cache;

{ TCacheCommand }

constructor TCacheCommand.Create(const logger: ILogger;
                                  const configurationManager: IConfigurationManager;
                                  const packageInstaller: IPackageInstaller;
                                  const packageCache : IPackageCache);
begin
  inherited Create(logger, configurationManager);
  FPackageInstaller := packageInstaller;
  FPackageCache := packageCache;
end;

function TCacheCommand.Execute(const cancellationToken : ICancellationToken): TExitCode;
var
  failureCount : integer;
  config : IConfiguration;
  configPath : string;
begin
  TCacheOptions.Default.ApplyCommon(TCommonOptions.Default);

  // `dpm cache verify` short-circuits the per-package download/cache flow.
  if TCacheOptions.Default.VerifyAll then
  begin
    // Cache instance is shared across commands; for the install/restore
    // flows TPackageInstaller.Init sets Location from the loaded config.
    // The cache verify path bypasses the installer entirely, so do the
    // same minimal init here — otherwise FullReVerify enumerates from an
    // empty path and System.IOUtils raises "Invalid characters in path".
    FConfigurationManager.EnsureDefaultConfig;
    configPath := TCacheOptions.Default.ConfigFile;
    if configPath = '' then
      configPath := TConfigUtils.GetDefaultConfigFileName;
    config := FConfigurationManager.LoadConfig(configPath);
    if config = nil then
    begin
      Logger.Error('Unable to load configuration; cannot verify cache.');
      result := TExitCode.Error;
      exit;
    end;
    FPackageCache.Location := config.PackageCacheLocation;

    failureCount := FPackageCache.FullReVerify;
    if failureCount = 0 then
      result := TExitCode.OK
    else
      result := TExitCode.Error;
    exit;
  end;

  if not TCacheOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if not FPackageInstaller.Cache(cancellationToken, TCacheOptions.Default) then
    result := TExitCode.Error
  else
    result := TExitCode.OK;
end;

end.
