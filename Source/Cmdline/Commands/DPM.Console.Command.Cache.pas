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

    // Loads the configuration and points the (shared) cache at the configured
    // location. The install/restore flows do this via TPackageInstaller.Init,
    // but the verify and remove paths bypass the installer so they must set it
    // themselves - otherwise the cache enumerates from an empty path.
    function EnsureCacheLocation : boolean;
    function ExecuteInstall(const cancellationToken : ICancellationToken) : TExitCode;
    function ExecuteVerify(const cancellationToken : ICancellationToken) : TExitCode;
    function ExecuteRemove : TExitCode;
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
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Utils.Config,
  DPM.Core.Options.Common,
  DPM.Core.Options.Cache,
  DPM.Console.Prompts;

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

function TCacheCommand.EnsureCacheLocation : boolean;
var
  config : IConfiguration;
  configPath : string;
begin
  result := false;
  // Cache instance is shared across commands; for the install/restore flows
  // TPackageInstaller.Init sets Location from the loaded config. The verify and
  // remove paths bypass the installer, so do the same minimal init here -
  // otherwise the cache enumerates from an empty path and System.IOUtils
  // raises "Invalid characters in path".
  FConfigurationManager.EnsureDefaultConfig;
  configPath := TCacheOptions.Default.ConfigFile;
  if configPath = '' then
    configPath := TConfigUtils.GetDefaultConfigFileName;
  config := FConfigurationManager.LoadConfig(configPath);
  if config = nil then
  begin
    Logger.Error('Unable to load configuration; cannot access the package cache.');
    exit;
  end;
  FPackageCache.Location := config.PackageCacheLocation;
  result := true;
end;

function TCacheCommand.ExecuteInstall(const cancellationToken : ICancellationToken) : TExitCode;
begin
  if not TCacheOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  //Installs the package into the cache: resolves its dependency graph, downloads the package
  //and every dependency, then compiles them for the platforms the package supports. One .dpkg
  //per compiler in the new model - it bundles every platform, so a single download covers them
  //all. (The IPackageInstaller method is still named Cache.)
  if not FPackageInstaller.Cache(cancellationToken, TCacheOptions.Default) then
    result := TExitCode.Error
  else
    result := TExitCode.OK;
end;

function TCacheCommand.ExecuteVerify(const cancellationToken : ICancellationToken) : TExitCode;
var
  failureCount : integer;
begin
  if not EnsureCacheLocation then
  begin
    result := TExitCode.Error;
    exit;
  end;

  failureCount := FPackageCache.FullReVerify(cancellationToken);
  if failureCount = 0 then
    result := TExitCode.OK
  else
    result := TExitCode.Error;
end;

function TCacheCommand.ExecuteRemove : TExitCode;
var
  matches : IList<IPackageIdentity>;
  packageId : IPackageIdentity;
  cancelled : boolean;
  proceed : boolean;
  removed : integer;
  failed : integer;
begin
  if not TCacheOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if not EnsureCacheLocation then
  begin
    result := TExitCode.Error;
    exit;
  end;

  matches := FPackageCache.GetCachedPackagesMatching(TCacheOptions.Default.PackageId,
    TCacheOptions.Default.CompilerVersion, TCacheOptions.Default.VersionString);

  if matches.Count = 0 then
  begin
    Logger.Information('No matching packages found in the cache.');
    result := TExitCode.OK;
    exit;
  end;

  Logger.Information('The following cached package(s) will be removed:');
  for packageId in matches do
    Logger.Information('  ' + packageId.Id + ' ' + packageId.Version.ToStringNoMeta +
      ' [' + CompilerToString(packageId.CompilerVersion) + ']');

  if not TCacheOptions.Default.Force then
  begin
    proceed := PromptYesNo('Remove these package(s) from the cache?', false, cancelled);
    if cancelled or (not proceed) then
    begin
      Logger.Information('Cancelled - nothing was removed.');
      result := TExitCode.OK;
      exit;
    end;
  end;

  removed := 0;
  failed := 0;
  for packageId in matches do
  begin
    if FPackageCache.RemovePackage(packageId) then
    begin
      Inc(removed);
      Logger.Information('Removed ' + packageId.Id + ' ' + packageId.Version.ToStringNoMeta +
        ' [' + CompilerToString(packageId.CompilerVersion) + ']');
    end
    else
    begin
      Inc(failed);
      Logger.Error('Failed to remove ' + packageId.Id + ' ' + packageId.Version.ToStringNoMeta +
        ' [' + CompilerToString(packageId.CompilerVersion) + ']');
    end;
  end;

  Logger.Information(Format('Removed %d package(s) from the cache.', [removed]));
  if failed = 0 then
    result := TExitCode.OK
  else
    result := TExitCode.Error;
end;

function TCacheCommand.Execute(const cancellationToken : ICancellationToken): TExitCode;
begin
  TCacheOptions.Default.ApplyCommon(TCommonOptions.Default);

  case TCacheOptions.Default.Command of
    TCacheSubCommand.Install : result := ExecuteInstall(cancellationToken);
    TCacheSubCommand.Verify  : result := ExecuteVerify(cancellationToken);
    TCacheSubCommand.Remove  : result := ExecuteRemove;
  else
    Logger.Error('A sub-command is required: install, remove or verify.');
    result := TExitCode.InvalidArguments;
  end;
end;

end.
