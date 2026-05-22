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
  // Phase 3 §3.3 — provider selector. Default is the legacy cert-store /
  // PFX path; --provider keyvault / signotaur route to a remote provider.
  TSignProvider = (spLocal, spKeyVault, spSignotaur);

  TSignOptions = class(TOptionsBase)
  private
    FPackageFile : string;
    FThumbprint : string;
    FStoreLocation : TSignStoreLocation;
    FPfxFile : string;
    FPfxPasswordEnvVar : string;
    FTimestampUrl : string;
    FDigest : string;
    // P3 §3.3 — remote-provider options
    FProvider : TSignProvider;
    FVaultUrl : string;
    FCertName : string;
    FKeyVersion : string;
    FTenantId : string;
    FClientId : string;
    FClientSecretEnv : string;
    FSignotaurEndpoint : string;
    FSignotaurApiKeyEnv : string;
    FSignotaurSubject : string;
    FSignotaurLabel : string;
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

    property Provider : TSignProvider read FProvider write FProvider;
    property VaultUrl : string read FVaultUrl write FVaultUrl;
    property CertName : string read FCertName write FCertName;
    property KeyVersion : string read FKeyVersion write FKeyVersion;
    property TenantId : string read FTenantId write FTenantId;
    property ClientId : string read FClientId write FClientId;
    property ClientSecretEnv : string read FClientSecretEnv write FClientSecretEnv;
    property SignotaurEndpoint : string read FSignotaurEndpoint write FSignotaurEndpoint;
    property SignotaurApiKeyEnv : string read FSignotaurApiKeyEnv write FSignotaurApiKeyEnv;
    property SignotaurSubject : string read FSignotaurSubject write FSignotaurSubject;
    property SignotaurLabel : string read FSignotaurLabel write FSignotaurLabel;
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

  case FProvider of
    spLocal :
      begin
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
    spKeyVault :
      begin
        if FVaultUrl = '' then
        begin
          logger.Error('--vault-url is required when --provider=keyvault');
          result := false;
        end;
        if FCertName = '' then
        begin
          logger.Error('--cert-name is required when --provider=keyvault');
          result := false;
        end;
        if (FTenantId = '') or (FClientId = '') then
        begin
          logger.Error('--tenant-id and --client-id are required when --provider=keyvault');
          result := false;
        end;
        if FClientSecretEnv = '' then
        begin
          logger.Error('--client-secret-env is required when --provider=keyvault ' +
                       '(names an env var that holds the client secret)');
          result := false;
        end;
      end;
    spSignotaur :
      begin
        if FSignotaurEndpoint = '' then
        begin
          logger.Error('--endpoint is required when --provider=signotaur');
          result := false;
        end;
        if FSignotaurApiKeyEnv = '' then
        begin
          logger.Error('--api-key-env is required when --provider=signotaur ' +
                       '(names an env var that holds the Signotaur API key)');
          result := false;
        end;
        if (FThumbprint = '') and (FSignotaurSubject = '') and (FSignotaurLabel = '') then
        begin
          logger.Error('Signotaur: one of --thumbprint, --subject, --label is required');
          result := false;
        end;
      end;
  end;
end;

end.
