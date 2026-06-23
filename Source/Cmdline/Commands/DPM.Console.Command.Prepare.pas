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

unit DPM.Console.Command.Prepare;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Project.Prepare;

type
  TPrepareCommand = class(TBaseCommand)
  private
    FEngine : IPreparePackageFolders;
  protected
    function ResolveSpecFile(out specFile : string) : boolean;
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                       const engine : IPreparePackageFolders); reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Console.Banner,
  DPM.Core.Utils.Path,
  DPM.Core.Options.Common,
  DPM.Core.Options.Prepare;

{ TPrepareCommand }

constructor TPrepareCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager;
                                   const engine : IPreparePackageFolders);
begin
  inherited Create(logger, configurationManager);
  FEngine := engine;
end;

function TPrepareCommand.ResolveSpecFile(out specFile : string) : boolean;
var
  explicitFile : string;
  candidates : TArray<string>;
  i : integer;
  list : string;
begin
  result := false;
  specFile := '';
  explicitFile := TPrepareOptions.Default.SpecFile;
  if explicitFile <> '' then
  begin
    if not FileExists(explicitFile) then
    begin
      Logger.Error('Spec file not found: ' + explicitFile);
      exit;
    end;
    specFile := explicitFile;
    result := true;
    exit;
  end;

  //auto-discover in current directory. Look for the current .dspec / .dspec.yaml format -
  //the older .dspec (JSON) format is no longer supported by the spec reader.
  candidates := TPathUtils.FindDspecFiles(GetCurrentDir, TSearchOption.soTopDirectoryOnly);
  if Length(candidates) = 0 then
  begin
    Logger.Error('No .dspec or .dspec.yaml file found in current directory: ' + GetCurrentDir);
    Logger.Error('Pass the spec file path explicitly, e.g. "dpm prepare mypackage.dspec".');
    exit;
  end;
  if Length(candidates) > 1 then
  begin
    Logger.Error('Multiple .dspec files found in current directory - please specify one explicitly:');
    list := '';
    for i := 0 to High(candidates) do
      Logger.Error('  ' + TPath.GetFileName(candidates[i]));
    exit;
  end;

  specFile := candidates[0];
  Logger.Information('Using auto-discovered spec: ' + TPath.GetFileName(specFile));
  result := true;
end;

function TPrepareCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  specFile : string;
begin
  TPrepareOptions.Default.ApplyCommon(TCommonOptions.Default);

  if not ResolveSpecFile(specFile) then
  begin
    result := TExitCode.MissingArg;
    exit;
  end;

  try
    if not FEngine.Execute(specFile,
                           TPrepareOptions.Default.Force,
                           TPrepareOptions.Default.DryRun,
                           cancellationToken) then
      exit(TExitCode.Error);
  except
    on e : Exception do
    begin
      Logger.Error('Error preparing packages : ' + e.Message);
      exit(TExitCode.Error);
    end;
  end;

  result := TExitCode.OK;
end;

end.
