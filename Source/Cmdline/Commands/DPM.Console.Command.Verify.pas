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

unit DPM.Console.Command.Verify;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Core.Package.Signing.Interfaces,
  DPM.Core.Trust.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base;


type
  TVerifyCommand = class(TBaseCommand)
  private
    FSigningService : IPackageSigningService;
    FTrustPolicy : ITrustPolicyService;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const signingService : IPackageSigningService;
                       const trustPolicy : ITrustPolicyService); reintroduce;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Options.Common,
  DPM.Core.Options.Verify;

constructor TVerifyCommand.Create(const logger : ILogger;
                                  const configurationManager : IConfigurationManager;
                                  const signingService : IPackageSigningService;
                                  const trustPolicy : ITrustPolicyService);
begin
  inherited Create(logger, configurationManager);
  FSigningService := signingService;
  FTrustPolicy := trustPolicy;
end;

function ModeToString(mode : TValidationMode) : string;
begin
  case mode of
    vmPermissive          : result := 'permissive';
    vmRequire             : result := 'require';
    vmRepositoryRequired  : result := 'repository-required';
    vmAuthorAndRepository : result := 'author-and-repository';
  else
    result := 'unknown';
  end;
end;

function TVerifyCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TVerifyOptions;
  policy : TTrustPolicy;
  verifyResult : TVerificationResult;
  i : integer;
  sigInfo : TSignatureInfo;
begin
  TVerifyOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TVerifyOptions.Default;
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  policy := FTrustPolicy.GetEffectivePolicy;

  try
    verifyResult := FSigningService.VerifyPackage(options.PackageFile, policy);
  except
    on e : Exception do
    begin
      Logger.Error('Verify exception: ' + e.Message);
      exit(TExitCode.Error);
    end;
  end;

  Logger.Information('');
  Logger.Information('Package: ' + ExtractFileName(options.PackageFile));
  Logger.Information('Mode:    ' + ModeToString(verifyResult.Mode));
  Logger.Information('');

  if Length(verifyResult.Signatures) = 0 then
    Logger.Information('No signatures present.')
  else for i := 0 to High(verifyResult.Signatures) do
  begin
    sigInfo := verifyResult.Signatures[i];
    if sigInfo.Role = srAuthor then
      Logger.Information('Author signature')
    else
      Logger.Information('Repository signature');
    Logger.Information('  Signer:     ' + sigInfo.SignerSubject);
    Logger.Information('  Thumbprint: ' + sigInfo.Thumbprint);
    Logger.Information('  SPKI:       sha256:' + sigInfo.SignerSpkiHex);
    if sigInfo.EffectiveSigningTime > 0 then
      Logger.Information('  Timestamp:  ' + DateTimeToStr(sigInfo.EffectiveSigningTime) + ' UTC');
    if sigInfo.Valid then
    begin
      if sigInfo.Role = srAuthor then
      begin
        if sigInfo.PublisherTrusted then
          Logger.Information('  Result:     valid, publisher trusted')
        else
          Logger.Information('  Result:     valid (publisher not pinned)');
      end
      else if sigInfo.RepositoryTrusted then
        Logger.Information('  Result:     valid, repository trusted')
      else
        Logger.Information('  Result:     valid (repository not trusted; ignored)');
    end
    else
      Logger.Error('  Result:     INVALID — ' + sigInfo.FailureReason);
    Logger.Information('');
  end;

  Logger.Information('Manifest hash: sha256:' + verifyResult.ManifestHashHex);
  Logger.Information('Policy fingerprint: ' + verifyResult.PolicyFingerprint);
  Logger.Information('');

  case verifyResult.Outcome of
    voTrusted :
      begin
        Logger.Success('Trust decision: TRUSTED');
        result := TExitCode.OK;
      end;
    voUnsigned :
      begin
        Logger.Warning('Trust decision: UNSIGNED (permissive mode allows install)');
        result := TExitCode.OK;
      end;
    voUntrustedPublisher :
      begin
        Logger.Warning('Trust decision: UNTRUSTED-PUBLISHER');
        result := TExitCode.OK;
      end;
    voInvalid :
      begin
        Logger.Error('Trust decision: INVALID — ' + verifyResult.Reason);
        result := TExitCode.Error;
      end;
  else
    result := TExitCode.Error;
  end;
end;

end.
