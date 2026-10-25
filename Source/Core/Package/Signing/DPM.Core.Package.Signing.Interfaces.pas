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
  DPM.Core.Diagnostics,
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
    // Only set for a signature that verified - an identity nothing vouches for must
    // never reach a trust decision (ratchets, pins, bindings).
    SignerSpkiHex      : string;
    SignerSubject      : string;
    // The certificate a signature that failed to verify claims it was made with -
    // for messages only.
    ClaimedSignerSpkiHex : string;
    ClaimedSubject     : string;
    Thumbprint         : string;
    EffectiveSigningTime : TDateTime;
    TimestampAuthority : string;
    // The timestamp verified, but its authority doesn't chain to a root this machine
    // trusts (e.g. an out of date root store), so its time can't be relied on - the
    // certificate chain was validated as of now, and EffectiveSigningTime is now.
    TimestampAuthorityUntrusted : boolean;
    ChainTrusted       : boolean;
    // The certificate chain is sound but ends at a root this machine doesn't trust - a
    // self-signed or private-CA certificate. The signature is valid; whether its signer
    // is trusted is down to trustedPublishers. Gallery packages never carry these.
    UntrustedRoot      : boolean;
    PublisherTrusted   : boolean;       // SPKI in trustedPublishers (author only)
    RepositoryTrusted  : boolean;       // SPKI in trustedRepositories (repo only)
    Valid              : boolean;       // CMS verified + chain ok (bar UntrustedRoot) + timestamp ok
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
    // The identity the signed manifest declares - '' until it parses. The installer checks it
    // against the identity the package is installed as.
    ManifestPackageId   : string;
    ManifestVersion     : string;
    // Invalid because the package is in a format newer than this client reads (E7109).
    FormatNotSupported  : boolean;
  end;

  ISignOptions = record
    TimestampUrl : string;
    DigestAlgorithm : THashAlgorithm;
  end;
  PSignOptions = ^ISignOptions;

  // What the verified signatures add up to, as TTrustModeEvaluator.Evaluate needs it.
  TSignatureFacts = record
    HasAnySignature : boolean;
    // Why the first signature that failed verification failed - '' when none did. Any
    // failed signature makes the package invalid, in every mode.
    InvalidReason : string;
    // A verified author signature whose signer is trusted : its chain reaches a trusted
    // root, or its SPKI is pinned in trustedPublishers.
    HasValidAuthor : boolean;
    // A verified author signature whose chain ends at an untrusted root and whose SPKI
    // isn't pinned - the SPKI, for the message that says what to pin.
    UntrustedRootAuthorSpki : string;
    HasValidTrustedRepo : boolean;
  end;

  // Phase 2: static helper. The single source of truth for "given this
  // mode and these per-signature facts, what's the outcome?" The verifier
  // and tests both go through this so the rules are exhaustively covered.
  TTrustModeEvaluator = record
  public
    class function GatherFacts(const signatures : TArray<TSignatureInfo>) : TSignatureFacts; static;
    class procedure Evaluate(const policy : TTrustPolicy;
                             const facts : TSignatureFacts;
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

    /// <summary>
    /// Checks an extracted package's files against its dpm-manifest.json : every listed
    /// file must exist with the recorded size and hash. Returns false when any doesn't,
    /// naming them (manifest paths) in mismatched - or with mismatched empty when the
    /// manifest itself can't be read. Project files (.dproj) are only checked for
    /// presence : DPM patches them in the cache before building. Files DPM adds to an
    /// extraction (lib\, bpl\, the receipt, build markers) aren't in the manifest.
    /// </summary>
    function VerifyExtractedFiles(const packageFolder : string;
                                  out mismatched : TArray<string>) : boolean;
  end;

  //raised with E7102 unless said otherwise (see EDPMCodedError).
  EPackageSigning = class(EDPMCodedError)
  protected
    class function DefaultCode : TErrorCode; override;
  end;

implementation

{ EPackageSigning }

class function EPackageSigning.DefaultCode : TErrorCode;
begin
  result := TErrorCode.E7102_SigningFailed;
end;

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

class function TTrustModeEvaluator.GatherFacts(const signatures : TArray<TSignatureInfo>) : TSignatureFacts;
var
  i : integer;
  roleName : string;
begin
  result.HasAnySignature := Length(signatures) > 0;
  result.InvalidReason := '';
  result.HasValidAuthor := false;
  result.UntrustedRootAuthorSpki := '';
  result.HasValidTrustedRepo := false;

  for i := 0 to High(signatures) do
  begin
    if not signatures[i].Valid then
    begin
      if result.InvalidReason = '' then
      begin
        if signatures[i].Role = srRepository then
          roleName := 'repository'
        else
          roleName := 'author';
        result.InvalidReason := Format('the %s signature does not verify : %s', [roleName, signatures[i].FailureReason]);
        if signatures[i].ClaimedSubject <> '' then
          result.InvalidReason := result.InvalidReason + Format(' (claims to be signed by %s)', [signatures[i].ClaimedSubject]);
      end;
      Continue;
    end;

    if signatures[i].Role = srAuthor then
    begin
      if (not signatures[i].UntrustedRoot) or signatures[i].PublisherTrusted then
        result.HasValidAuthor := true
      else if result.UntrustedRootAuthorSpki = '' then
        result.UntrustedRootAuthorSpki := signatures[i].SignerSpkiHex;
    end
    else if signatures[i].RepositoryTrusted then
      result.HasValidTrustedRepo := true;
  end;
end;

class procedure TTrustModeEvaluator.Evaluate(const policy : TTrustPolicy;
                                              const facts : TSignatureFacts;
                                              var outcome : TVerificationOutcome;
                                              var reason : string);

  //for the modes that need an author : why the one present doesn't count.
  function AuthorShortfall(const prefix : string) : string;
  begin
    if facts.UntrustedRootAuthorSpki <> '' then
      result := prefix + ': the author certificate does not chain to a trusted root - add its SPKI (sha256:' +
                facts.UntrustedRootAuthorSpki + ') to signing.trustedPublishers to trust it'
    else
      result := prefix + ': no valid author signature present';
  end;

begin
  //A signature that is present but doesn't verify fails the package whatever the mode - including
  //permissive, which only allows packages that aren't signed at all.
  if facts.InvalidReason <> '' then
  begin
    outcome := voInvalid;
    reason := facts.InvalidReason;
    exit;
  end;

  case policy.ValidationMode of
    vmPermissive :
      begin
        if not facts.HasAnySignature then
          outcome := voUnsigned
        else if facts.HasValidAuthor or facts.HasValidTrustedRepo then
          outcome := voTrusted
        else
          //signatures that verify, from signers nothing here trusts.
          outcome := voUntrustedPublisher;
      end;
    vmRequire :
      begin
        if facts.HasValidAuthor then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          reason := AuthorShortfall('require mode');
        end;
      end;
    vmRepositoryRequired :
      begin
        if facts.HasValidTrustedRepo then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          reason := 'repository-required mode: no valid signature from a trusted repository';
        end;
      end;
    vmAuthorAndRepository :
      begin
        if facts.HasValidAuthor and facts.HasValidTrustedRepo then
          outcome := voTrusted
        else
        begin
          outcome := voInvalid;
          if not facts.HasValidAuthor and not facts.HasValidTrustedRepo then
            reason := 'author-and-repository mode: no valid author or trusted repository signature'
          else if not facts.HasValidAuthor then
            reason := AuthorShortfall('author-and-repository mode')
          else
            reason := 'author-and-repository mode: missing valid trusted repository signature';
        end;
      end;
  end;
end;

end.
