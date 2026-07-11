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

unit DPM.Console.Command.CopyLocal;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Package.CopyLocal;

type
  TCopyLocalCommand = class(TBaseCommand)
  private
    FCopyLocalService : ICopyLocalService;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
    function ForceNoBanner : boolean; override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const copyLocalService : ICopyLocalService); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Options.Common,
  DPM.Core.Options.CopyLocal;

constructor TCopyLocalCommand.Create(const logger : ILogger;
                                     const configurationManager : IConfigurationManager;
                                     const copyLocalService : ICopyLocalService);
begin
  inherited Create(logger, configurationManager);
  FCopyLocalService := copyLocalService;
end;

function TCopyLocalCommand.ForceNoBanner : boolean;
begin
  result := true;
end;

function TCopyLocalCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  projectPath : string;
begin
  TCopyLocalOptions.Default.ApplyCommon(TCommonOptions.Default);

  projectPath := TCopyLocalOptions.Default.ProjectPath;
  if projectPath = '' then
    projectPath := GetCurrentDir;
  TCopyLocalOptions.Default.ProjectPath := projectPath;

  if not TCopyLocalOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if FCopyLocalService.CopyLocal(cancellationToken, TCopyLocalOptions.Default) then
    result := TExitCode.OK
  else
    result := TExitCode.Error;
end;

end.
