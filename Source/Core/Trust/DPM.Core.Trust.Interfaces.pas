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

unit DPM.Core.Trust.Interfaces;

interface

uses
  System.SysUtils;

type
  TValidationMode = (
    vmPermissive,           // unsigned ok; invalid signature still fails
    vmRequire,              // valid author signature required
    vmRepositoryRequired,   // P2: valid signature from trusted repo required
    vmAuthorAndRepository   // P2: both required
  );

  TAuthorDowngradePolicy = (
    adpPrompt,    // ask the user (CLI = stdin, IDE = modal)
    adpBlock,     // hard fail on downgrade
    adpAllow      // silently accept
  );

  TUnsignedReason = (
    urNotApplicable,
    urNeverSigned,
    urAuthorCeasedSigning
  );

  TTrustedPublisher = record
    Name : string;
    SpkiHex : string;   // SHA-256 of SPKI, hex
  end;

  TTrustedRepository = record
    Url : string;
    SpkiHex : string;
  end;

  TTrustPolicy = record
    ValidationMode         : TValidationMode;
    AuthorDowngradePolicy  : TAuthorDowngradePolicy;
    AllowKeyCompromiseOverride : boolean;   // P3 — read in P1 for fingerprint stability
    TrustedPublishers      : TArray<TTrustedPublisher>;
    TrustedRepositories    : TArray<TTrustedRepository>;
    TrustSetVersion        : integer;
  end;

  ITrustSet = interface
    ['{0E29A3D9-2D0A-4F75-BBE9-7D2A0BBE6F4D}']
    function Version : integer;
    function DefaultValidationMode : TValidationMode;
    function RepositorySpkis : TArray<TTrustedRepository>;
    /// <summary>
    /// P3 §3.4 emergency revocation channel. A repository SPKI present here
    /// is treated as untrusted regardless of any other configuration (the
    /// per-user dpm.config.yaml trustedRepositories list, or its inclusion
    /// in the trust set's own RepositorySpkis). Checked on every verify
    /// to ensure a compromised gallery key can be cut off as soon as the
    /// updated trust set lands.
    /// </summary>
    function RevokedRepositorySpkis : TArray<string>;
  end;

  ITrustPolicyService = interface
    ['{93E7D02E-1547-49E8-9D3E-25A55C2A9B7C}']
    function GetEffectivePolicy : TTrustPolicy;
    function PolicyFingerprint(const policy : TTrustPolicy) : string;   // SHA-256 hex
    function PublisherTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
    function RepositoryTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
  end;

  TAuthorTrustEntry = record
    LastAuthorSpkiHex    : string;
    LastSeenAuthorSigned : boolean;
    LastSeenAt           : TDateTime;
    DowngradeAcknowledged : boolean;
    // If true, the user previously picked "Always block" for this id —
    // subsequent installs are rejected without re-prompting, regardless of
    // any change to the new signer.
    BlockedPermanently   : boolean;
  end;

  // Phase 2 §2.3 (V-24): high-water mark for repository assurance, per
  // package id. Once seen carrying a valid trusted-repository signature
  // attesting namespace N, any future install lacking such a signature
  // (or attesting a different namespace) is a hard failure regardless of
  // the global trust mode. This ratchet is keyed strictly to *trusted*
  // repositories — an attacker running their own repo cannot satisfy it.
  TRepositoryTrustEntry = record
    TrustedRepoSpkiHex   : string;    // the trusted repo SPKI we ratcheted to
    Namespace            : string;    // attested namespace (e.g. "VSoft.*")
    FirstSeenAt          : TDateTime; // UTC
    LastSeenAt           : TDateTime; // UTC
  end;

  ITrustStateService = interface
    ['{2BBA1A1C-9E4F-44D1-8D2A-67F1B3C5D6E7}']
    function TryGetAuthor(const packageId : string; out entry : TAuthorTrustEntry) : boolean;
    procedure RecordAuthor(const packageId : string; const entry : TAuthorTrustEntry);
    procedure AcknowledgeAuthorDowngrade(const packageId : string);
    procedure BlockPermanently(const packageId : string);
    // Drop the author entry for this package id. Used when the user explicitly
    // overrides an author-downgrade prompt — the next install is treated as
    // first-ever (fresh TOFU) rather than another downgrade.
    procedure RemoveAuthor(const packageId : string);

    // P2: repository assurance ratchet.
    function TryGetRepository(const packageId : string; out entry : TRepositoryTrustEntry) : boolean;
    procedure RecordRepository(const packageId : string; const entry : TRepositoryTrustEntry);
    // Drop the repository entry for this package id. Used when the user
    // explicitly overrides a V-24 ratchet failure.
    procedure RemoveRepository(const packageId : string);
  end;

  ETrust = class(Exception);

implementation

end.
