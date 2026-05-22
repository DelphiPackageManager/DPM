{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Options.Verify;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Options.Base;

type
  TVerifyOptions = class(TOptionsBase)
  private
    FPackageFile : string;
    FOffline : boolean;
    FJsonOutput : boolean;
    class var FDefault : TVerifyOptions;
  public
    class constructor CreateDefault;
    class property Default : TVerifyOptions read FDefault;
    function Validate(const logger : ILogger) : boolean; override;

    property PackageFile : string read FPackageFile write FPackageFile;
    property Offline : boolean read FOffline write FOffline;
    // P3 §3.5 — when true, dpm verify emits a single JSON object on stdout
    // (no banner, no human-readable lines) so CI pipelines can parse it.
    property JsonOutput : boolean read FJsonOutput write FJsonOutput;
  end;

implementation

uses
  System.SysUtils;

class constructor TVerifyOptions.CreateDefault;
begin
  FDefault := TVerifyOptions.Create;
end;

function TVerifyOptions.Validate(const logger : ILogger) : boolean;
begin
  result := true;
  if FPackageFile = '' then
  begin
    logger.Error('No package file specified (e.g. dpm verify Foo.dpkg).');
    result := false;
  end
  else if not FileExists(FPackageFile) then
  begin
    logger.Error('Package file not found: ' + FPackageFile);
    result := false;
  end;
end;

end.
