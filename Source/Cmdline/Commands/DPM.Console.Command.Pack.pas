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

unit DPM.Console.Command.Pack;

interface

uses
  VSoft.CancellationToken,
  Spring.Container.Common,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Packaging,
  DPM.Core.Constants;

type
  TPackCommand = class(TBaseCommand)
  private
    FWriter : IPackageWriter;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const writer : IPackageWriter);reintroduce;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  VSoft.SemanticVersion,
  DPM.Core.Types,
  DPM.Console.Banner,
  DPM.Console.Options,
  DPM.Core.Options.Common,
  DPM.Core.Options.Pack;

{ TPackCommand }

constructor TPackCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager;  const writer : IPackageWriter);
begin
  inherited Create(logger,configurationManager);
  FWriter := writer;
end;

function TPackCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  mcVer : TSemanticVersion;
  packVer : TPackageVersion;
  error : string;
begin
  TPackOptions.Default.ApplyCommon(TCommonOptions.Default);
  if not TPackOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  //This should just be an api call into the core, don't do all the work here!
  //just check for required args here.

  Logger.Information('Pack Command invoked');
  if TPackOptions.Default.SpecFile = '' then
  begin
    Logger.Error('No Spec file provided!');
    Exit(TExitCode.MissingArg);
  end;


  //does it really make sense to allow this to be overriden from the command line?
  if TPackOptions.Default.MinClientVersion <> cDPMClientVersion then
  begin
    if not TSemanticVersion.TryParseWithError(TPackOptions.Default.MinClientVersion,mcVer,error ) then
    begin
      Logger.Error('MinClientVersion : ' + error);
      Exit(TExitCode.InvalidArguments);
    end;
//    defVer := TSemanticVersion.Parse(cDPMClientVersion);
//    if mcVer > defVer then
//    begin
//      Logger.Error('MinClientVersion must be at less than or equal to : ' + defVer.ToString);
//      Exit(TExitCode.InvalidArguments);
//
//    end;
  end;


  if TPackOptions.Default.Version <> '' then
  begin
    if not TPackageVersion.TryParseWithError(TPackOptions.Default.Version, packVer, error) then
    begin
      Logger.Error('Invalid Version : ' + error);
      Exit(TExitCode.InvalidArguments);
    end;
  end;

  try
    if not FWriter.WritePackageFromSpec(cancellationToken, TPackOptions.Default) then
      exit(TExitCode.Error);

  except
    on e : Exception do
    begin
      Logger.Error('Error creating package : ' + e.Message);
      exit(TExitCode.Error);
    end;
  end;

  result := TExitCode.OK;

end;

end.
