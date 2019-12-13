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

unit DPM.Core.Sources.LocalClient;

interface

uses
  VSoft.Uri,
  DPM.Core.Logging,
  DPM.Core.Options.Push,
  DPM.Core.Sources.Interfaces;

type
  TLocalClient = class(TInterfacedObject, ISourceClient)
  private
    FLogger : ILogger;
    FSourceUri  : IUri;
  protected
    function Push(const pushOptions: TPushOptions): Boolean;
  public
    constructor Create(const logger : ILogger; const sourceUri : IUri);
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils;

{ TLocalClient }

constructor TLocalClient.Create(const logger: ILogger; const sourceUri : IUri);
begin
  FLogger := logger;
  FSourceUri := sourceUri;
end;

function TLocalClient.Push(const pushOptions: TPushOptions): Boolean;
var
  targetFile : string;
begin
  result := false;
//  if TPath.IsRelativePath(pushOptions.PackagePath) then
    pushOptions.PackagePath := TPath.GetFullPath(pushOptions.PackagePath);

  if not FileExists(pushOptions.PackagePath) then
  begin
    FLogger.Error('Package file [' + pushOptions.PackagePath + '] not found.');
    exit;
  end;
  if FSourceUri.LocalPath = '' then
  begin
    FLogger.Error('Source uri local path is empty, likely not a valid uri [' + FSourceUri.ToString + '].');
    exit;
  end;

  if not DirectoryExists(FSourceUri.LocalPath) then
  begin
    FLogger.Error('Source uri local path does not exist [' + FSourceUri.LocalPath + '].');
    exit;
  end;
  try
    targetFile := TPath.Combine(FSourceUri.LocalPath, TPath.GetFileName(pushOptions.PackagePath));
    if pushOptions.SkipDuplicate and TFile.Exists(targetFile) then
    begin
      FLogger.Information('Package [' + TPath.GetFileName(pushOptions.PackagePath)+ '] exists and skipDuplicates specified, skipping');
      exit(true);
    end;

    TFile.Copy(pushOptions.PackagePath, targetFile, false);
    FLogger.Information('Package pushed ok.', true);
    result := true;
  except
    on e : Exception do
    begin
      FLogger.Error('Unable to copy file [' + pushOptions.PackagePath +'] to [' + FSourceUri.LocalPath + '] : ' + e.Message);
    end;
  end;
end;

end.
