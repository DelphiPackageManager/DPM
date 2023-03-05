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

unit DPM.Console.Command.List;

interface

uses
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TListCommand = class(TBaseCommand)
  private
    FRepositoryManager : IPackageRepositoryManager;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repositoryManager : IPackageRepositoryManager);reintroduce;
  end;

implementation

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Utils.Strings,
  DPM.Core.Options.Common,
  DPM.Core.Options.List,
  DPM.Core.Package.Interfaces;

{ TListCommand }

constructor TListCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

function TListCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  searchResults : IList<IPackageListItem>;
  item : IPackageListItem;
  resultString : string;
  config : IConfiguration;
begin
  result := TExitCode.Error;
  TListOptions.Default.ApplyCommon(TCommonOptions.Default);
  if not TListOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if TListOptions.Default.ConfigFile = '' then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  TListOptions.Default.Take := MaxInt;

  config := FConfigurationManager.LoadConfig(TListOptions.Default.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);
  FRepositoryManager.Initialize(config);

  searchResults := FRepositoryManager.List(cancellationToken, TListOptions.Default);
  //TODO : re-implement this
  if searchResults.Any then
  begin
    //group by id+version+compiler, collect platforms
    for item in searchResults do
    begin
      if cancellationToken.IsCancelled then
        exit;
      resultString := TStringUtils.PadRight(item.Id, 24) + #9+'v'  + TStringUtils.PadRight(item.Version.ToString, 15) + ' [Delphi '  + CompilerToString(item.CompilerVersion) +  ' - ' + item.Platforms + ']';
      Logger.Information(resultString);
    end;
    result := TExitCode.OK;
  end
  else
    Logger.Information('No packages were found');

end;

end.
