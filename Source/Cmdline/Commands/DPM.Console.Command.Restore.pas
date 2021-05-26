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

unit DPM.Console.Command.Restore;

interface

uses
  VSoft.Awaitable,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Package.Installer.Interfaces;

type
  TRestoreCommand = class(TBaseCommand)
  private
    FPackageInstaller : IPackageInstaller;
    FContext : IPackageInstallerContext;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const packageInstaller : IPackageInstaller; const context : IPackageInstallerContext);reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.Restore;

{ TRestoreCommand }

constructor TRestoreCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const packageInstaller: IPackageInstaller; const context : IPackageInstallerContext);
begin
  inherited Create(logger, configurationManager);
  FPackageInstaller := packageInstaller;
  FContext := context;
end;

function TRestoreCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
begin
  TRestoreOptions.Default.ApplyCommon(TCommonOptions.Default);

  //project path - if it's empty, specify the current directory
  //do this before validation!

  if TRestoreOptions.Default.ProjectPath = '' then
    TRestoreOptions.Default.ProjectPath := GetCurrentDir;

  if not TRestoreOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if not FPackageInstaller.Restore(cancellationToken, TRestoreOptions.Default, FContext) then
    result := TExitCode.Error
  else
    result := TExitCode.OK;

end;

end.
