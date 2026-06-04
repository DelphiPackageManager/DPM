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
  System.IOUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Provider.Factory,
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
begin
  result := TSigningProviderFactory.CreateProvider(Logger, FX509, TSignOptions.Default);
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
