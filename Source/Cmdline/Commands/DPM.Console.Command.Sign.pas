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

unit DPM.Console.Command.Sign;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Package.Signing.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base;


type
  TSignCommand = class(TBaseCommand)
  private
    FX509 : IX509Service;
    FSigningService : IPackageSigningService;
    function AcquireProvider : ISigningProvider;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const x509 : IX509Service;
                       const signingService : IPackageSigningService); reintroduce;
  end;


implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Provider,
  DPM.Core.Options.Common,
  DPM.Core.Options.Sign;

constructor TSignCommand.Create(const logger : ILogger;
                                const configurationManager : IConfigurationManager;
                                const x509 : IX509Service;
                                const signingService : IPackageSigningService);
begin
  inherited Create(logger, configurationManager);
  FX509 := x509;
  FSigningService := signingService;
end;

function TSignCommand.AcquireProvider : ISigningProvider;
var
  options : TSignOptions;
  store : ICertificateStore;
  pfxBytes : TBytes;
  password : string;
  cert : ICertificate;
begin
  options := TSignOptions.Default;
  result := nil;

  if options.Thumbprint <> '' then
  begin
    store := FX509.OpenSystemStore(TCertStoreLocation(Ord(options.StoreLocation)), 'MY');
    cert := store.FindByThumbprint(options.Thumbprint);
    if cert = nil then
    begin
      Logger.Error('Certificate with thumbprint ' + options.Thumbprint + ' not found in store.');
      exit;
    end;
    result := TCertStoreSigningProvider.Create(cert);
  end
  else
  begin
    pfxBytes := TFile.ReadAllBytes(options.PfxFile);
    if options.PfxPasswordEnvVar <> '' then
      password := GetEnvironmentVariable(options.PfxPasswordEnvVar)
    else
      password := '';   // The PFX may be unencrypted; if not, OpenPfxStore will error.
    store := FX509.OpenPfxStore(pfxBytes, password);

    // Pick the first cert from the PFX. Multi-cert PFX is rare; if it happens
    // the user should be using cert-store + --thumbprint instead.
    cert := store.FindByThumbprint('');   // empty thumbprint = "first match"
    if cert = nil then
    begin
      Logger.Error('No certificate found in PFX file.');
      exit;
    end;
    result := TPfxSigningProvider.Create(cert);
  end;
end;

function TSignCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TSignOptions;
  provider : ISigningProvider;
  signOpts : ISignOptions;
  alg : THashAlgorithm;
begin
  TSignOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TSignOptions.Default;
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  if not TAlgorithmProfile.ParseHashName(options.Digest, alg) then
  begin
    Logger.Error('Unsupported --digest "' + options.Digest + '" (SHA256/SHA384/SHA512 only).');
    exit(TExitCode.InvalidArguments);
  end;

  provider := AcquireProvider;
  if provider = nil then
    exit(TExitCode.Error);

  signOpts.TimestampUrl := options.TimestampUrl;
  signOpts.DigestAlgorithm := alg;

  try
    // Sign service emits its own per-step log lines (signer, digest,
    // timestamp request/response, blob write).
    FSigningService.SignPackage(options.PackageFile, provider, signOpts);
    result := TExitCode.OK;
  except
    on e : Exception do
    begin
      Logger.Error('Sign failed: ' + e.Message);
      result := TExitCode.Error;
    end;
  end;
end;

end.
