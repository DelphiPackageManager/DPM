{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Crypto.Provider.Factory;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Options.Sign,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  // Shared between the CLI `dpm sign` command and the DSpecCreator GUI. Turns a
  // populated TSignOptions (+ logger + X509 service) into a concrete
  // ISigningProvider. Returns nil on a configuration error (already logged).
  TSigningProviderFactory = class
  public
    class function CreateProvider(const logger : ILogger;
                                  const x509 : IX509Service;
                                  const options : TSignOptions) : ISigningProvider; static;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DPM.Core.Crypto.Provider,
  DPM.Core.Crypto.Provider.Azure,
  DPM.Core.Crypto.Provider.Signotaur;

{ TSigningProviderFactory }

class function TSigningProviderFactory.CreateProvider(const logger : ILogger;
                                                      const x509 : IX509Service;
                                                      const options : TSignOptions) : ISigningProvider;
var
  store : ICertificateStore;
  pfxBytes : TBytes;
  password : string;
  cert : ICertificate;
  kvOpts : TKeyVaultOptions;
  signotaurOpts : TSignotaurOptions;
  tokenSvc : IAzureAccessTokenService;
begin
  result := nil;

  case options.Provider of
    spKeyVault :
      begin
        kvOpts.VaultUrl := options.VaultUrl;
        kvOpts.CertificateName := options.CertName;
        kvOpts.KeyVersion := options.KeyVersion;
        kvOpts.TenantId := options.TenantId;
        kvOpts.ClientId := options.ClientId;
        // A literal secret (GUI, from the credential store) wins over the env var.
        if options.ClientSecret <> '' then
          kvOpts.ClientSecret := options.ClientSecret
        else
          kvOpts.ClientSecret := GetEnvironmentVariable(options.ClientSecretEnv);
        if kvOpts.ClientSecret = '' then
        begin
          logger.Error('Azure Key Vault client secret is empty (set it directly or via the ' +
                       options.ClientSecretEnv + ' environment variable).');
          exit;
        end;
        tokenSvc := TAzureAccessTokenService.Create(logger);
        result := TKeyVaultSigningProvider.Create(logger, x509, tokenSvc, kvOpts);
      end;
    spSignotaur :
      begin
        signotaurOpts.Endpoint := options.SignotaurEndpoint;
        // --api-key (literal) wins over --api-key-env when both are supplied,
        // since the user explicitly provided one. Validate() guarantees at
        // least one was supplied.
        if options.SignotaurApiKey <> '' then
          signotaurOpts.ApiKey := options.SignotaurApiKey
        else
        begin
          signotaurOpts.ApiKey := GetEnvironmentVariable(options.SignotaurApiKeyEnv);
          if signotaurOpts.ApiKey = '' then
          begin
            logger.Error('Environment variable ' + options.SignotaurApiKeyEnv + ' is not set or empty.');
            exit;
          end;
        end;
        signotaurOpts.Thumbprint := options.Thumbprint;
        signotaurOpts.Subject := options.SignotaurSubject;
        signotaurOpts.Label_ := options.SignotaurLabel;
        signotaurOpts.AllowSelfSignedCertificates := options.SignotaurAllowSelfSigned;
        // Audit metadata (FileName, FileSize) is pushed in per-file by the
        // signing service via ISigningProvider.SetSigningContext, so each
        // file in a batch sign gets its own audit record server-side.
        result := TSignotaurSigningProvider.Create(logger, x509, signotaurOpts);
      end;
  else
    // spLocal � original cert-store + PFX path.
    if options.Thumbprint <> '' then
    begin
      store := x509.OpenSystemStore(TCertStoreLocation(Ord(options.StoreLocation)), 'MY');
      cert := store.FindByThumbprint(options.Thumbprint);
      if cert = nil then
      begin
        logger.Error('Certificate with thumbprint ' + options.Thumbprint + ' not found in store.');
        exit;
      end;
      result := TCertStoreSigningProvider.Create(cert);
    end
    else
    begin
      pfxBytes := TFile.ReadAllBytes(options.PfxFile);
      // A literal password (GUI, from the credential store) wins over the env var.
      if options.PfxPassword <> '' then
        password := options.PfxPassword
      else if options.PfxPasswordEnvVar <> '' then
        password := GetEnvironmentVariable(options.PfxPasswordEnvVar)
      else
        password := '';
      store := x509.OpenPfxStore(pfxBytes, password);
      cert := store.FindByThumbprint('');
      if cert = nil then
      begin
        logger.Error('No certificate found in PFX file.');
        exit;
      end;
      result := TPfxSigningProvider.Create(cert);
    end;
  end;
end;

end.
