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
    function Execute: TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const repositoryManager : IPackageRepositoryManager);reintroduce;
  end;

implementation

uses
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.List,
  DPM.Core.Package.Interfaces;

{ TListCommand }

constructor TListCommand.Create(const logger: ILogger; const configurationManager: IConfigurationManager; const repositoryManager : IPackageRepositoryManager);
begin
  inherited Create(logger, configurationManager);
  FRepositoryManager := repositoryManager;
end;

function TListCommand.Execute: TExitCode;
var
  searchResults : IPackageSearchResult;
  info, prevInfo : IPackageIdentity;
  resultString : string;

begin
  result := TExitCode.Error;
  TListOptions.Default.ApplyCommon(TCommonOptions.Default);
  if not TListOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  searchResults := FRepositoryManager.Search(TListOptions.Default);
  if searchResults.Packages.Any then
  begin
    prevInfo := nil;
    resultString := '';
    //group by id+version+compiler, collect platforms
    for info in searchResults.Packages do
    begin
      if (prevInfo = nil) then
      begin
        prevInfo := info;
        resultString := info.Id  + '-' + info.Version.ToString + '-' + CompilerToString(info.CompilerVersion) +  ' [' + DPMPlatformToString(info.Platform);
        continue;
      end;
      if (info.Id <> prevInfo.Id) or (info.CompilerVersion <> prevInfo.CompilerVersion) or (info.Version <> prevInfo.Version) then
      begin
        Logger.Information(resultString + ']');
        resultString := '';
        resultString := info.Id + '-' + info.Version.ToString + '-' + CompilerToString(info.CompilerVersion) +  ' [' + DPMPlatformToString(info.Platform);
      end
      else
        //different platform.
         resultString := resultString +',' + DPMPlatformToString(info.Platform);
      prevInfo := info;
    end;
    if resultString <> '' then
      Logger.Information(resultString + ']');
     result := TExitCode.OK;
  end
  else
    Logger.Information('No packages were found');


end;

end.
