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

unit DPM.Core.Crypto.X509.Interfaces;

interface

uses
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms;

type
  TChainResult = (
    crValid,
    crUntrustedRoot,
    crExpired,
    crRevoked,
    crWrongUsage,
    crIncomplete,
    crUnknownError
  );

  // V-26 — distinct from TChainResult because a chain can be revoked-but-
  // accepted (signing predates revocation) and "unknown" is meaningfully
  // different from "good" or "revoked" for receipts and policy.
  TRevocationStatus = (
    rsNotChecked,    // BuildAtTime called with checkRevocation=false
    rsGood,          // revocation checked, no problems
    rsRevoked,       // CRL/OCSP says revoked at asOfTime
    rsUnknown        // CRL/OCSP unreachable or status indeterminate
  );

  // RFC 5280 §5.3.1 CRL reason codes. V-26: keyCompromise is the only reason
  // we treat as retroactively invalidating an otherwise-timestamped signature.
  // The other reasons are typically administrative (superseded, affiliation
  // changed) and don't invalidate signatures made while the cert was good.
  TRevocationReason = (
    rrNotApplicable,        // cert wasn't revoked, or we didn't / couldn't check
    rrUnspecified,
    rrKeyCompromise,
    rrCACompromise,
    rrAffiliationChanged,
    rrSuperseded,
    rrCessationOfOperation,
    rrCertificateHold,
    rrRemoveFromCRL,
    rrPrivilegeWithdrawn,
    rrAACompromise,
    rrUnknown               // reason field absent or wasn't decodable
  );

  TCertStoreLocation = (
    cslCurrentUser,
    cslLocalMachine
  );

  // What kind of public key the cert carries. Drives both digest pairing
  // (FIPS 186 curve-size matching for ECDSA) and CMS signature-algorithm
  // OID selection. Unknown when we don't recognise the SPKI algorithm OID.
  TCertKeyType = (cktUnknown, cktRsa, cktEcdsa);

  ICertificate = interface
    ['{B1F31C8C-25A7-4F40-A19B-7E40C2E4D8E8}']
    function SubjectCommonName : string;
    function SubjectDistinguishedName : string;
    function IssuerDistinguishedName : string;
    function Thumbprint : string;                     // SHA-1, hex, display only
    function SerialNumberHex : string;
    function SpkiHash(algorithm : THashAlgorithm) : TBytes;
    function NotBefore : TDateTime;                   // UTC
    function NotAfter : TDateTime;                    // UTC
    function HasCodeSigningEku : boolean;
    function RawDerBytes : TBytes;
    /// <summary>
    /// Public-key algorithm carried by the cert's SubjectPublicKeyInfo.
    /// Used by the CMS layer to pick the correct signature-algorithm OID
    /// (RSA vs ECDSA-with-hash) and by PreferredDigest to decide whether
    /// curve-size matching applies.
    /// </summary>
    function PublicKeyType : TCertKeyType;
    /// <summary>
    /// Natural digest pairing for this cert's key. RSA is permissive — any
    /// of SHA-256/384/512 work — and we return SHA-256 as the sensible
    /// default. ECDSA is strict (FIPS 186): digest size MUST match curve
    /// size, otherwise CSP/KSP middleware (YubiKey PIV, Azure KV) will
    /// reject the sign with "algorithm not supported". Returns:
    ///   RSA / unknown → haSha256
    ///   ECDSA P-256   → haSha256
    ///   ECDSA P-384   → haSha384
    ///   ECDSA P-521   → haSha512
    /// Callers should prefer this over a hard-coded default unless the user
    /// explicitly chose a digest on the command line.
    /// </summary>
    function PreferredDigest : THashAlgorithm;
    // Win32 handle access — only the X509 / CMS layer should call this.
    function GetContext : PCCERT_CONTEXT;
  end;

  ICertificateStore = interface
    ['{6B2A7C20-04B6-43FB-A0F1-2C57F6B0E11C}']
    function FindByThumbprint(const thumbprint : string) : ICertificate;
    function FindBySpki(const spkiHash : TBytes; algorithm : THashAlgorithm) : ICertificate;
    function GetHandle : HCERTSTORE;
  end;

  ICertificateChain = interface
    ['{0F0A2A88-04F9-4EC3-83DA-7E0C4D7F0C8D}']
    function Build(const cert : ICertificate;
                   const additionalCerts : array of ICertificate) : TChainResult;
    /// <summary>
    /// Phase 3 §3.1 — timestamp-aware chain build. Passes `asOfTime` to the
    /// Win32 chain engine as the evaluation moment, so a revocation that
    /// post-dates `asOfTime` does NOT fail the chain. When `checkRevocation`
    /// is true, the chain engine is asked to consult CRL/OCSP (excluding the
    /// root). When false, behaves like the basic Build.
    /// </summary>
    function BuildAtTime(const cert : ICertificate;
                         const additionalCerts : array of ICertificate;
                         asOfTime : TDateTime;
                         checkRevocation : boolean) : TChainResult;
    function VerifyForCodeSigning(asOfTime : TDateTime) : TChainResult;
    function ChainCertificates : TArray<ICertificate>;  // leaf first, root last
    function RootCertificate : ICertificate;
    function LastErrorMessage : string;
    /// <summary>
    /// V-26 — revocation outcome captured during the last BuildAtTime call.
    /// rsNotChecked when revocation wasn't requested. Surfaces "unknown"
    /// distinctly from "good"/"revoked" so receipts can record what we did
    /// know and policy can decide what to do with a CRL/OCSP outage.
    /// </summary>
    function RevocationStatus : TRevocationStatus;

    /// <summary>
    /// V-26 — when the chain came back revoked, the CRL/OCSP reason code.
    /// rrNotApplicable when the chain is good or revocation wasn't checked;
    /// rrUnknown when the reason field is missing or undecodable. The
    /// keyCompromise distinction matters for retroactive validity: every
    /// other reason leaves earlier timestamped signatures valid.
    /// </summary>
    function RevocationReason : TRevocationReason;
  end;

  IX509Service = interface
    ['{2A1A0F7C-1F1C-4F3F-91A8-3C71E3AE7BA1}']
    function OpenSystemStore(location : TCertStoreLocation; const storeName : string) : ICertificateStore;
    function OpenPfxStore(const pfxBytes : TBytes; const password : string) : ICertificateStore;
    function LoadCertificateFromDer(const der : TBytes) : ICertificate;
    function CreateChain : ICertificateChain;
  end;

  ECryptoX509 = class(Exception);

implementation

end.
