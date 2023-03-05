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

unit DPM.Console.Command.Uninstall;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base,
  DPM.Core.Package.Installer.Interfaces;

type
  TUninstallCommand = class(TBaseCommand)
  private
    FPackageInstaller : IPackageInstaller;
    FContext : IPackageInstallerContext;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const packageInstaller : IPackageInstaller; const context : IPackageInstallerContext);reintroduce;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.UnInstall;

{ TRemoveCommand }

constructor TUninstallCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const packageInstaller: IPackageInstaller; const context : IPackageInstallerContext);
begin
  inherited Create(logger, configurationManager);
  FPackageInstaller := packageInstaller;
  FContext := context;

end;

function TUninstallCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
begin
  TUnInstallOptions.Default.ApplyCommon(TCommonOptions.Default);

  //project path - if it's empty, specify the current directory
  //do this before validation!

  if TUnInstallOptions.Default.ProjectPath = '' then
    TUnInstallOptions.Default.ProjectPath := GetCurrentDir;

  if not TUnInstallOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

 //TODO : This is not ideal, we have no way of returning more specific exit codes here - rethink api?
  if not FPackageInstaller.UnInstall(cancellationToken, TUnInstallOptions.Default, FContext) then
    result := TExitCode.Error
  else
    result := TExitCode.OK;

end;

end.
