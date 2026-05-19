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

unit DPM.Console.Command.Sbom;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.SBOM.Interfaces;

type
  TSBOMCommand = class(TBaseCommand)
  private
    FGenerator : ISbomGenerator;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const generator : ISbomGenerator); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.Sbom;

constructor TSBOMCommand.Create(const logger : ILogger;
                                 const configurationManager : IConfigurationManager;
                                 const generator : ISbomGenerator);
begin
  inherited Create(logger, configurationManager);
  FGenerator := generator;
end;

function TSBOMCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  projectPath : string;
  projectFiles : TStringDynArray;
begin
  TSBOMOptions.Default.ApplyCommon(TCommonOptions.Default);

  projectPath := TSBOMOptions.Default.ProjectPath;
  if projectPath = '' then
    projectPath := GetCurrentDir;

  //allow a directory shortcut - pick the single .dproj/.groupproj inside it.
  if DirectoryExists(projectPath) then
  begin
    projectFiles := TDirectory.GetFiles(projectPath, '*.groupproj');
    if Length(projectFiles) = 0 then
      projectFiles := TDirectory.GetFiles(projectPath, '*.dproj');
    if Length(projectFiles) = 0 then
    begin
      Logger.Error('No .dproj or .groupproj file found in directory [' + projectPath + ']');
      result := TExitCode.InvalidArguments;
      exit;
    end;
    if Length(projectFiles) > 1 then
    begin
      Logger.Error('Multiple project files found in directory [' + projectPath + '] - specify a single .dproj or .groupproj.');
      result := TExitCode.InvalidArguments;
      exit;
    end;
    projectPath := projectFiles[0];
  end;

  TSBOMOptions.Default.ProjectPath := projectPath;

  if not TSBOMOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if FGenerator.Generate(cancellationToken, TSBOMOptions.Default) then
    result := TExitCode.OK
  else
    result := TExitCode.Error;
end;

end.
