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


unit DPM.Console.Command.Feed;

interface

uses
  VSoft.Awaitable,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TFeedCommand = class(TBaseCommand)
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
  DPM.Core.Options.Common,
  DPM.Core.Options.Feed,
  DPM.Core.Package.Interfaces;

{ TListCommand }

constructor TFeedCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

function TFeedCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  searchResults : IList<IPackageSearchResultItem>;
  item : IPackageSearchResultItem;
  resultString : string;
begin
  result := TExitCode.Error;
  TFeedOptions.Default.ApplyCommon(TCommonOptions.Default);
  if not TFeedOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  searchResults := FRepositoryManager.GetPackageFeed(cancellationToken, TFeedOptions.Default);
  if searchResults.Any then
  begin
    //group by id+version+compiler, collect platforms
    for item in searchResults do
    begin
      if cancellationToken.IsCancelled then
        exit;

      resultString := item.Id + '-' + item.Version + ' [' + DPMPlatformsToString(item.Platforms)  + ']';
      Logger.Information(resultString);
    end;
     result := TExitCode.OK;
  end
  else
    Logger.Information('No packages were found');


end;

end.
