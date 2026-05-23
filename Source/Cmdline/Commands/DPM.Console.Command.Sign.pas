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
  Spring.Collections,
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
    function ExpandTargets(const target : string;
                           const pattern : string;
                           const recursive : boolean) : IList<string>;
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
  DPM.Core.Crypto.Provider.Azure,
  DPM.Core.Crypto.Provider.Signotaur,
  DPM.Core.Options.Common,
  DPM.Core.Options.Sign,
  DPM.Core.Utils.Directory;

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
  kvOpts : TKeyVaultOptions;
  signotaurOpts : TSignotaurOptions;
  tokenSvc : IAzureAccessTokenService;
begin
  options := TSignOptions.Default;
  result := nil;

  case options.Provider of
    spKeyVault :
      begin
        kvOpts.VaultUrl := options.VaultUrl;
        kvOpts.CertificateName := options.CertName;
        kvOpts.KeyVersion := options.KeyVersion;
        kvOpts.TenantId := options.TenantId;
        kvOpts.ClientId := options.ClientId;
        kvOpts.ClientSecret := GetEnvironmentVariable(options.ClientSecretEnv);
        if kvOpts.ClientSecret = '' then
        begin
          Logger.Error('Environment variable ' + options.ClientSecretEnv + ' is not set or empty.');
          exit;
        end;
        tokenSvc := TAzureAccessTokenService.Create(Logger);
        result := TKeyVaultSigningProvider.Create(Logger, FX509, tokenSvc, kvOpts);
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
            Logger.Error('Environment variable ' + options.SignotaurApiKeyEnv + ' is not set or empty.');
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
        result := TSignotaurSigningProvider.Create(Logger, FX509, signotaurOpts);
      end;
  else
    // spLocal — original cert-store + PFX path.
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
        password := '';
      store := FX509.OpenPfxStore(pfxBytes, password);
      cert := store.FindByThumbprint('');
      if cert = nil then
      begin
        Logger.Error('No certificate found in PFX file.');
        exit;
      end;
      result := TPfxSigningProvider.Create(cert);
    end;
  end;
end;

function TSignCommand.ExpandTargets(const target : string;
                                    const pattern : string;
                                    const recursive : boolean) : IList<string>;
var
  effectivePattern : string;
  searchOption : TSearchOption;
  baseDir : string;
  mask : string;
begin
  result := TCollections.CreateList<string>;
  effectivePattern := pattern;
  if effectivePattern = '' then
    effectivePattern := '*.dpkg';
  if recursive then
    searchOption := TSearchOption.soAllDirectories
  else
    searchOption := TSearchOption.soTopDirectoryOnly;

  // Case 1: single existing file. Sign it as-is regardless of name pattern —
  // the user pointed at a specific file.
  if FileExists(target) then
  begin
    result.Add(target);
    exit;
  end;

  // Case 2: existing directory. Walk it with the configured pattern.
  if DirectoryExists(target) then
  begin
    result.AddRange(TDirectoryUtils.GetFiles(target, effectivePattern, searchOption).ToArray);
    exit;
  end;

  // Case 3: wildcard. Split into base dir + mask and walk.
  if (Pos('*', target) > 0) or (Pos('?', target) > 0) then
  begin
    baseDir := ExtractFilePath(target);
    mask := ExtractFileName(target);
    if baseDir = '' then
      baseDir := GetCurrentDir;
    if mask = '' then
      mask := effectivePattern;
    if DirectoryExists(baseDir) then
      result.AddRange(TDirectoryUtils.GetFiles(baseDir, mask, searchOption).ToArray);
    exit;
  end;
  // Falls through with empty list -> caller reports "not found".
end;

function TSignCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TSignOptions;
  provider : ISigningProvider;
  signOpts : ISignOptions;
  alg : THashAlgorithm;
  files : IList<string>;
  packageFile : string;
  okCount : integer;
  failCount : integer;
  sessionOpen : boolean;
begin
  TSignOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TSignOptions.Default;
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  // Empty digest -> haUnknown, signed by the cert-driven auto-select in the
  // signing service. Non-empty must parse to a supported algorithm.
  if options.Digest = '' then
    alg := haUnknown
  else if not TAlgorithmProfile.ParseHashName(options.Digest, alg) then
  begin
    Logger.Error('Unsupported --digest "' + options.Digest + '" (SHA256/SHA384/SHA512 only).');
    exit(TExitCode.InvalidArguments);
  end;

  files := ExpandTargets(options.PackageFile, options.FilePattern, options.Recursive);
  if files.Count = 0 then
  begin
    Logger.Error('No packages to sign — target "' + options.PackageFile + '" is not a file, directory, or matching pattern.');
    exit(TExitCode.InvalidArguments);
  end;

  provider := AcquireProvider;
  if provider = nil then
    exit(TExitCode.Error);

  signOpts.TimestampUrl := options.TimestampUrl;
  signOpts.DigestAlgorithm := alg;

  if files.Count > 1 then
    Logger.Information(Format('Signing %d packages...', [files.Count]));

  // Open a signing session for the full batch. For smart-card / HSM-backed
  // providers this is the difference between one PIN prompt and N. For
  // remote providers it's a no-op.
  provider.BeginSession;
  sessionOpen := true;
  okCount := 0;
  failCount := 0;
  try
    for packageFile in files do
    begin
      if cancellationToken.IsCancelled then
        Break;
      try
        FSigningService.SignPackage(packageFile, provider, signOpts);
        Inc(okCount);
      except
        on e : EProviderFatal do
        begin
          // Unrecoverable provider error (bad credentials, server down).
          // Continuing would just generate one identical failure per file —
          // abort the batch immediately even when --fail-fast is off.
          Inc(failCount);
          Logger.Error('Sign failed for [' + packageFile + ']: ' + e.Message);
          Logger.Error('Provider session unrecoverable — aborting batch (' +
                       IntToStr(files.Count - okCount - failCount) +
                       ' files not signed).');
          Break;
        end;
        on e : Exception do
        begin
          Inc(failCount);
          Logger.Error('Sign failed for [' + packageFile + ']: ' + e.Message);
          if options.FailFast then
            Break;
        end;
      end;
    end;
  finally
    if sessionOpen then
      provider.EndSession;
  end;

  if files.Count > 1 then
  begin
    if failCount = 0 then
      Logger.Success(Format('Signed %d package(s) successfully.', [okCount]))
    else
      Logger.Warning(Format('%d signed, %d failed.', [okCount, failCount]));
  end;

  if failCount > 0 then
    result := TExitCode.Error
  else
    result := TExitCode.OK;
end;

end.
