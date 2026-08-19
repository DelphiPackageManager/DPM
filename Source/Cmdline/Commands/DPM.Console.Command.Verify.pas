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
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Package.Signing.Interfaces,
  DPM.Core.Trust.Interfaces,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base;


type
  TVerifyCommand = class(TBaseCommand)
  private
    FSigningService : IPackageSigningService;
    FTrustPolicy : ITrustPolicyService;
    procedure EmitHumanResult(const packageFile : string;
                              const verifyResult : TVerificationResult);
    procedure EmitJsonResult(const packageFile : string;
                             const verifyResult : TVerificationResult);
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode;override;
    //-json-output promises a single JSON object on stdout, so the banner has to go.
    //Options are parsed before the application decides whether to show it, so reading
    //TVerifyOptions.Default here is safe.
    function ForceNoBanner : boolean;override;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const signingService : IPackageSigningService;
                       const trustPolicy : ITrustPolicyService); reintroduce;
  end;


implementation

uses
  System.SysUtils,
  VSoft.YAML,
  DPM.Console.RawIO,
  DPM.Core.Json.Utils,
  DPM.Core.Options.Common,
  DPM.Core.Options.Verify;

function RevocationStatusName(value : TRevocationStatus) : string;
begin
  case value of
    rsGood       : result := 'good';
    rsRevoked    : result := 'revoked';
    rsUnknown    : result := 'unknown';
  else
    result := 'notChecked';
  end;
end;

function RevocationReasonName(value : TRevocationReason) : string;
begin
  case value of
    rrUnspecified           : result := 'unspecified';
    rrKeyCompromise         : result := 'keyCompromise';
    rrCACompromise          : result := 'cACompromise';
    rrAffiliationChanged    : result := 'affiliationChanged';
    rrSuperseded            : result := 'superseded';
    rrCessationOfOperation  : result := 'cessationOfOperation';
    rrCertificateHold       : result := 'certificateHold';
    rrRemoveFromCRL         : result := 'removeFromCRL';
    rrPrivilegeWithdrawn    : result := 'privilegeWithdrawn';
    rrAACompromise          : result := 'aACompromise';
    rrUnknown               : result := 'unknown';
  else
    result := '';   // rrNotApplicable -> omit from JSON
  end;
end;

function OutcomeName(value : TVerificationOutcome) : string;
begin
  case value of
    voTrusted             : result := 'trusted';
    voUnsigned            : result := 'unsigned';
    voUntrustedPublisher  : result := 'untrustedPublisher';
  else
    result := 'invalid';
  end;
end;

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

function TVerifyCommand.ForceNoBanner : boolean;
begin
  result := TVerifyOptions.Default.JsonOutput;
end;

function TVerifyCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TVerifyOptions;
  policy : TTrustPolicy;
  verifyResult : TVerificationResult;
  flags : TVerifyFlags;
begin
  TVerifyOptions.Default.ApplyCommon(TCommonOptions.Default);
  options := TVerifyOptions.Default;
  if not options.Validate(Logger) then
    exit(TExitCode.InvalidArguments);

  policy := FTrustPolicy.GetEffectivePolicy;
  flags.Offline := options.Offline;
  // In JSON mode the only thing on stdout is the JSON object. The console logger
  // writes to stdout too (not stderr), so anything chatty has to be suppressed here
  // rather than relying on stream separation - and ForceNoBanner keeps the banner off.
  if flags.Offline and not options.JsonOutput then
    Logger.Information('Offline mode — revocation will use cached CRL/OCSP responses only.');

  try
    verifyResult := FSigningService.VerifyPackage(options.PackageFile, policy, flags);
  except
    on e : Exception do
    begin
      Logger.Error('Verify exception: ' + e.Message);
      exit(TExitCode.Error);
    end;
  end;

  if options.JsonOutput then
    EmitJsonResult(options.PackageFile, verifyResult)
  else
    EmitHumanResult(options.PackageFile, verifyResult);

  case verifyResult.Outcome of
    voTrusted, voUnsigned, voUntrustedPublisher : result := TExitCode.OK;
  else
    result := TExitCode.Error;
  end;
end;

procedure TVerifyCommand.EmitHumanResult(const packageFile : string;
                                          const verifyResult : TVerificationResult);
var
  i : integer;
  sigInfo : TSignatureInfo;
begin
  Logger.Information('');
  Logger.Information('Package: ' + ExtractFileName(packageFile));
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
    Logger.Information('  Signer     : ' + sigInfo.SignerSubject);
    Logger.Information('  Thumbprint : ' + sigInfo.Thumbprint);
    Logger.Information('  SPKI       : sha256:' + sigInfo.SignerSpkiHex);
    if sigInfo.EffectiveSigningTime > 0 then
      Logger.Information('  Timestamp  :  ' + DateTimeToStr(sigInfo.EffectiveSigningTime) + ' UTC');
    case sigInfo.Revocation of
      rsGood    : Logger.Information('  Revocation : checked, good');
      rsRevoked : Logger.Error      ('  Revocation : REVOKED');
      rsUnknown : Logger.Warning    ('  Revocation : status unknown (CRL/OCSP unreachable)');
      // rsNotChecked: silent — Phase 1 default
    end;
    case sigInfo.CurrentRevocationReason of
      rrKeyCompromise         : Logger.Error  ('  Current status: REVOKED (keyCompromise) — retroactively invalid');
      rrCACompromise          : Logger.Warning('  Current status: revoked (cACompromise)');
      rrSuperseded            : Logger.Warning('  Current status: revoked (superseded)');
      rrAffiliationChanged    : Logger.Warning('  Current status: revoked (affiliationChanged)');
      rrCessationOfOperation  : Logger.Warning('  Current status: revoked (cessationOfOperation)');
      rrCertificateHold       : Logger.Warning('  Current status: revoked (certificateHold)');
      rrPrivilegeWithdrawn    : Logger.Warning('  Current status: revoked (privilegeWithdrawn)');
      rrAACompromise          : Logger.Warning('  Current status: revoked (aACompromise)');
      rrUnspecified           : Logger.Warning('  Current status: revoked (unspecified)');
      rrUnknown               : Logger.Warning('  Current status: revoked (reason undecodable)');
      // rrNotApplicable: silent
    end;
    if sigInfo.Valid then
    begin
      if sigInfo.Role = srAuthor then
      begin
        if sigInfo.PublisherTrusted then
          Logger.Information('  Result     : valid, publisher trusted')
        else
          Logger.Information('  Result     : valid (publisher not pinned)');
      end
      else if sigInfo.RepositoryTrusted then
        Logger.Information('  Result     : valid, repository trusted')
      else
        Logger.Information('  Result     : valid (repository not trusted; ignored)');
    end
    else
      Logger.Error('  Result:     INVALID — ' + sigInfo.FailureReason);

    if (sigInfo.Role = srRepository) and sigInfo.Attestation.Present then
    begin
      Logger.Information('  Attestation:');
      if sigInfo.Attestation.Namespace <> '' then
        Logger.Information('    Namespace:  ' + sigInfo.Attestation.Namespace);
      if sigInfo.Attestation.AuthorSpkiHex <> '' then
        Logger.Information('    Author key: sha256:' + sigInfo.Attestation.AuthorSpkiHex)
      else
        case sigInfo.Attestation.UnsignedReason of
          urAttestNeverSigned :
            Logger.Information('    Author key: (publisher has never author-signed)');
          urAttestAuthorCeasedSigning :
            Logger.Information('    Author key: (publisher previously signed, no longer signs)');
        else
          Logger.Information('    Author key: (unsigned, reason not specified)');
        end;
    end;

    Logger.Information('');
  end;

  Logger.Information('Manifest hash: sha256:' + verifyResult.ManifestHashHex);
  Logger.Information('Policy fingerprint: ' + verifyResult.PolicyFingerprint);
  Logger.Information('');

  case verifyResult.Outcome of
    voTrusted             : Logger.Success('Trust decision: TRUSTED');
    voUnsigned            : Logger.Warning('Trust decision: UNSIGNED (permissive mode allows install)');
    voUntrustedPublisher  : Logger.Warning('Trust decision: UNTRUSTED-PUBLISHER');
    voInvalid             : Logger.Error  ('Trust decision: INVALID — ' + verifyResult.Reason);
  end;
end;

procedure TVerifyCommand.EmitJsonResult(const packageFile : string;
                                         const verifyResult : TVerificationResult);
var
  doc : IYAMLDocument;
  root : IYAMLMapping;
  sigsArr : IYAMLSequence;
  sigObj : IYAMLMapping;
  attObj : IYAMLMapping;
  i : integer;
  sigInfo : TSignatureInfo;
  reasonStr : string;
begin
  // P3 3.5 - single self-describing JSON object on stdout, no banner, no log noise.
  // CI pipelines can `dpm verify ... --json-output` and pipe directly into jq.
  //
  // Built with VSoft.YAML rather than JsonDataObjects, which is being retired from the
  // project. Two things changed with it, both cosmetic: indentation is two spaces rather
  // than tabs, and optional string fields are omitted when empty rather than written as ""
  // (TJsonUtils.AddIfNotEmpty - see that unit for why). The set of fields and their values
  // are unchanged.
  doc := TYAML.CreateMapping;
  root := doc.AsMapping;

  root.AddOrSetValue('package', ExtractFileName(packageFile));
  root.AddOrSetValue('mode', ModeToString(verifyResult.Mode));
  root.AddOrSetValue('outcome', OutcomeName(verifyResult.Outcome));
  TJsonUtils.AddIfNotEmpty(root, 'reason', verifyResult.Reason);
  root.AddOrSetValue('manifestHash', 'sha256:' + verifyResult.ManifestHashHex);
  root.AddOrSetValue('policyFingerprint', verifyResult.PolicyFingerprint);

  sigsArr := root.AddOrSetSequence('signatures');
  for i := 0 to High(verifyResult.Signatures) do
  begin
    sigInfo := verifyResult.Signatures[i];
    sigObj := sigsArr.AddMapping;
    if sigInfo.Role = srAuthor then
      sigObj.AddOrSetValue('role', 'author')
    else
      sigObj.AddOrSetValue('role', 'repository');
    TJsonUtils.AddIfNotEmpty(sigObj, 'signerSubject', sigInfo.SignerSubject);
    sigObj.AddOrSetValue('signerSpki', 'sha256:' + sigInfo.SignerSpkiHex);
    TJsonUtils.AddIfNotEmpty(sigObj, 'thumbprint', sigInfo.Thumbprint);
    sigObj.AddOrSetValue('valid', sigInfo.Valid);
    if not sigInfo.Valid then
      TJsonUtils.AddIfNotEmpty(sigObj, 'failureReason', sigInfo.FailureReason);
    if sigInfo.EffectiveSigningTime > 0 then
      sigObj.AddOrSetValue('effectiveSigningTime',
        FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', sigInfo.EffectiveSigningTime));
    sigObj.AddOrSetValue('revocation', RevocationStatusName(sigInfo.Revocation));
    reasonStr := RevocationReasonName(sigInfo.CurrentRevocationReason);
    TJsonUtils.AddIfNotEmpty(sigObj, 'currentRevocationReason', reasonStr);
    if sigInfo.Role = srAuthor then
      sigObj.AddOrSetValue('publisherTrusted', sigInfo.PublisherTrusted)
    else
    begin
      sigObj.AddOrSetValue('repositoryTrusted', sigInfo.RepositoryTrusted);
      if sigInfo.Attestation.Present then
      begin
        attObj := sigObj.AddOrSetMapping('attestation');
        TJsonUtils.AddIfNotEmpty(attObj, 'namespace', sigInfo.Attestation.Namespace);
        if sigInfo.Attestation.AuthorSpkiHex <> '' then
          attObj.AddOrSetValue('authorSpki', 'sha256:' + sigInfo.Attestation.AuthorSpkiHex)
        else
          case sigInfo.Attestation.UnsignedReason of
            urAttestNeverSigned         : attObj.AddOrSetValue('unsignedReason', 'neverSigned');
            urAttestAuthorCeasedSigning : attObj.AddOrSetValue('unsignedReason', 'authorCeasedSigning');
          end;
      end;
    end;
  end;

  //Raw stdout, bypassing IConsoleWriter, which would indent and word wrap this. TStdOut
  //writes UTF-8 - the previous RTL Write emitted the console ANSI code page, which corrupted
  //any non ASCII signer subject.
  TStdOut.WriteLine(TJsonUtils.ToPrettyJson(doc));
end;

end.
