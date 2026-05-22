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

unit DPM.Core.Options.Sign;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Options.Base;

type
  TSignStoreLocation = (sslCurrentUser, sslLocalMachine);

  TSignOptions = class(TOptionsBase)
  private
    FPackageFile : string;
    FThumbprint : string;
    FStoreLocation : TSignStoreLocation;
    FPfxFile : string;
    FPfxPasswordEnvVar : string;
    FTimestampUrl : string;
    FDigest : string;
    class var FDefault : TSignOptions;
  public
    class constructor CreateDefault;
    class property Default : TSignOptions read FDefault;
    function Validate(const logger : ILogger) : boolean; override;

    property PackageFile : string read FPackageFile write FPackageFile;
    property Thumbprint : string read FThumbprint write FThumbprint;
    property StoreLocation : TSignStoreLocation read FStoreLocation write FStoreLocation;
    property PfxFile : string read FPfxFile write FPfxFile;
    property PfxPasswordEnvVar : string read FPfxPasswordEnvVar write FPfxPasswordEnvVar;
    property TimestampUrl : string read FTimestampUrl write FTimestampUrl;
    property Digest : string read FDigest write FDigest;
  end;

implementation

uses
  System.SysUtils;

class constructor TSignOptions.CreateDefault;
begin
  FDefault := TSignOptions.Create;
  FDefault.FTimestampUrl := 'http://timestamp.digicert.com';
  FDefault.FDigest := 'sha256';
end;

function TSignOptions.Validate(const logger : ILogger) : boolean;
begin
  result := true;
  if FPackageFile = '' then
  begin
    logger.Error('No package file specified (e.g. dpm sign Foo.dpkg --thumbprint ...).');
    result := false;
  end
  else if not FileExists(FPackageFile) then
  begin
    logger.Error('Package file not found: ' + FPackageFile);
    result := false;
  end;

  if (FThumbprint = '') and (FPfxFile = '') then
  begin
    logger.Error('Either --thumbprint (cert store) or --pfx must be specified.');
    result := false;
  end;
  if (FThumbprint <> '') and (FPfxFile <> '') then
  begin
    logger.Error('--thumbprint and --pfx are mutually exclusive.');
    result := false;
  end;
  if (FPfxFile <> '') and not FileExists(FPfxFile) then
  begin
    logger.Error('PFX file not found: ' + FPfxFile);
    result := false;
  end;
end;

end.
