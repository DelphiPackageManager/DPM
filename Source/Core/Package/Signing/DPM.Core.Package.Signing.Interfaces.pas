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
  DPM.Core.Crypto.X509.Interfaces,
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
    // SHA-256 (hex) of every author signature blob this repo sig co-signed,
    // as carried in the multi-value dpmVerifiedAuthorSigHash signed
    // attribute on the repository signature. One entry per attested author
    // (CMS attribute values are SET OF — gallery emits one hash per
    // registered-publisher author sig present in the archive).
    //
    // Empty array means the attribute was absent (older gallery versions)
    // or empty. The verifier requires every listed hash to resolve to an
    // author signature actually present in the archive, AND requires the
    // primary AuthorSpkiHex to match the signer of at least one of those
    // resolved sigs — the cryptographic binding between "gallery attested
    // these authors" and "the author signatures actually present came from
    // them".
    BoundAuthorSigHashesHex : TArray<string>;
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
    Revocation         : TRevocationStatus;        // P3: revocation outcome at signing time
    // P3 §3.1 follow-up — *current* revocation reason from a second chain
    // build without pTime. When = rrKeyCompromise, the signature is treated
    // as retroactively invalid unless TTrustPolicy.AllowKeyCompromiseOverride.
    CurrentRevocationReason : TRevocationReason;
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

  // Author-rebuild exemption to the V-24 repository ratchet. A package author
  // rebuilding their own already-published id locally (dpm pack into a local
  // folder, install to test, push to the gallery afterwards) produces a build
  // that legitimately carries no gallery repository signature. Without an
  // exemption the ratchet blocks that install on every release after the id's
  // first — the ratchet is keyed on package id alone and cannot otherwise tell
  // an author's own rebuild from a third party shadowing the id.
  //
  // The exemption is granted only when the new build carries a valid author
  // signature from the *same* key the ratchet last recorded for this id. That
  // key's holder is the one identity entitled to supply an unattested build of
  // it; anyone shadowing the id would need the author's signing key, a strictly
  // higher bar than the repository ratchet enforces on its own.
  //
  // IMPORTANT: `priorAuthor` must be the entry read *before*
  // EvaluateAuthorDowngrade runs for the same install. That call ratchets the
  // author high-water mark forward to the current signer, so comparing against
  // the post-call entry would match any signer at all and void the check.
  TAuthorRebuildExemption = record
  public
    class function Applies(hadPriorAuthor : boolean;
                           const priorAuthor : TAuthorTrustEntry;
                           const signatures : TArray<TSignatureInfo>) : boolean; static;
  end;

  TVerifyFlags = record
    Offline : boolean;   // P3 §3.2 — skip CRL/OCSP network calls
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
                           const policy : TTrustPolicy) : TVerificationResult; overload;
    /// <summary>
    /// P3 §3.2 — variant that takes runtime flags (offline mode etc.) so the
    /// CLI `dpm verify --offline` can skip CRL/OCSP fetches.
    /// </summary>
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy;
                           const flags : TVerifyFlags) : TVerificationResult; overload;

    /// <summary>
    /// Quick re-check on a cache hit. Re-hashes only the manifest and
    /// compares against the receipt — no CMS, no chain build.
    /// </summary>
    function QuickRecheck(const cacheFolder : string;
                          const policy : TTrustPolicy) : boolean;
  end;

  EPackageSigning = class(Exception);

implementation

class function TAuthorRebuildExemption.Applies(hadPriorAuthor : boolean;
                                                const priorAuthor : TAuthorTrustEntry;
                                                const signatures : TArray<TSignatureInfo>) : boolean;
var
  i : integer;
begin
  result := false;

  // Nothing recorded to match against, or the id was last seen unsigned:
  // there is no author identity that could vouch for this build, so the
  // repository ratchet stands. Fail closed.
  if not hadPriorAuthor then
    exit;
  if not priorAuthor.LastSeenAuthorSigned then
    exit;
  if priorAuthor.LastAuthorSpkiHex = '' then
    exit;

  // A permanent user block outranks the exemption. EvaluateAuthorDowngrade
  // already rejects these before the repository ratchet is reached; repeated
  // here so the predicate is correct in isolation.
  if priorAuthor.BlockedPermanently then
    exit;

  // SameText rather than a hex normalisation pass, to stay consistent with the
  // author ratchet's own key-change comparison — both sides of this test come
  // from TSignatureInfo.SignerSpkiHex, so the formats already agree.
  for i := 0 to High(signatures) do
    if (signatures[i].Role = srAuthor) and signatures[i].Valid and
       SameText(signatures[i].SignerSpkiHex, priorAuthor.LastAuthorSpkiHex) then
    begin
      result := true;
      exit;
    end;
end;

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
