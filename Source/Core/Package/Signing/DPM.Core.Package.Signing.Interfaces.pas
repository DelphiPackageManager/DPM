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

unit DPM.Core.Package.Signing.Interfaces;

interface

uses
  System.SysUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Trust.Interfaces;

type
  TSignatureRole = (srAuthor, srRepository);

  TUnsignedReason2 = (
    urAttestNotApplicable,
    urAttestNeverSigned,
    urAttestAuthorCeasedSigning
  );

  // Phase 2: contents of a `dpmRepositoryAttestation` signed attribute,
  // read only off a repository signature whose SPKI is in trustedRepositories
  // (V-21). Surfaced on TSignatureInfo so the receipt + IDE can show "author
  // key registered to <namespace>" without re-parsing the CMS blob.
  TRepositoryAttestation = record
    Present         : boolean;
    Namespace       : string;       // e.g. "VSoft.*"
    AuthorSpkiHex   : string;       // empty when UnsignedReason set
    UnsignedReason  : TUnsignedReason2;
  end;

  TSignatureInfo = record
    Role               : TSignatureRole;
    SignerSpkiHex      : string;
    SignerSubject      : string;
    Thumbprint         : string;
    EffectiveSigningTime : TDateTime;
    TimestampAuthority : string;
    ChainTrusted       : boolean;
    PublisherTrusted   : boolean;       // SPKI in trustedPublishers (author only)
    RepositoryTrusted  : boolean;       // SPKI in trustedRepositories (repo only)
    Valid              : boolean;       // CMS verified + chain ok + timestamp ok
    FailureReason      : string;
    Attestation        : TRepositoryAttestation;   // P2: repository attestation
  end;

  TVerificationOutcome = (
    voTrusted,                // mode requirements satisfied
    voUnsigned,               // permissive mode, no signatures (integrity-only)
    voUntrustedPublisher,     // signed but signer not in trust set (permissive ok / require fails)
    voInvalid                 // signature invalid or required signature missing
  );

  TVerificationResult = record
    Outcome             : TVerificationOutcome;
    ManifestHashAlgorithm : THashAlgorithm;
    ManifestHashHex     : string;
    Signatures          : TArray<TSignatureInfo>;
    Reason              : string;
    Mode                : TValidationMode;
    PolicyFingerprint   : string;
  end;

  ISignOptions = record
    TimestampUrl : string;
    DigestAlgorithm : THashAlgorithm;
  end;
  PSignOptions = ^ISignOptions;

  // Phase 2: static helper. The single source of truth for "given this
  // mode and these per-signature facts, what's the outcome?" The verifier
  // and tests both go through this so the rules are exhaustively covered.
  TTrustModeEvaluator = record
  public
    class procedure Evaluate(const policy : TTrustPolicy;
                             hasAnySignature : boolean;
                             hasValidAuthor : boolean;
                             hasValidTrustedRepo : boolean;
                             var outcome : TVerificationOutcome;
                             var reason : string); static;
  end;

  IPackageSigningService = interface
    ['{1A4E5AE5-5F76-491F-AA64-2C0A6F2D7B62}']

    /// <summary>
    /// Sign the manifest in `packageFilePath` and write the result back
    /// as signatures/author-N.p7s. Re-seals the .dpkg in place.
    /// </summary>
    procedure SignPackage(const packageFilePath : string;
                          const provider : ISigningProvider;
                          const options : ISignOptions);

    /// <summary>
    /// Full Verification Workflow — used by `dpm verify` and as the central
    /// gate inside TPackageCache.InstallPackageFromFile.
    /// </summary>
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy) : TVerificationResult;

    /// <summary>
    /// Quick re-check on a cache hit. Re-hashes only the manifest and
    /// compares against the receipt — no CMS, no chain build.
    /// </summary>
    function QuickRecheck(const cacheFolder : string;
                          const policy : TTrustPolicy) : boolean;
  end;

  EPackageSigning = class(Exception);

implementation

class procedure TTrustModeEvaluator.Evaluate(const policy : TTrustPolicy;
                                              hasAnySignature : boolean;
                                              hasValidAuthor : boolean;
                                              hasValidTrustedRepo : boolean;
                                              var outcome : TVerificationOutcome;
                                              var reason : string);
begin
  case policy.ValidationMode of
    vmPermissive :
      begin
        if not hasAnySignature then
          outcome := voUnsigned
        else if hasValidAuthor or hasValidTrustedRepo then
          outcome := voTrusted
        else
          outcome := voUntrustedPublisher;
      end;
    vmRequire :
      begin
        if hasValidAuthor then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          reason := 'require mode: no valid author signature present';
        end;
      end;
    vmRepositoryRequired :
      begin
        if hasValidTrustedRepo then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          reason := 'repository-required mode: no valid signature from a trusted repository';
        end;
      end;
    vmAuthorAndRepository :
      begin
        if hasValidAuthor and hasValidTrustedRepo then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          if not hasValidAuthor and not hasValidTrustedRepo then
            reason := 'author-and-repository mode: no valid author or trusted repository signature'
          else if not hasValidAuthor then
            reason := 'author-and-repository mode: missing valid author signature'
          else
            reason := 'author-and-repository mode: missing valid trusted repository signature';
        end;
      end;
  end;
end;

end.
