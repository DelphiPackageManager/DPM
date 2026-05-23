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

unit DPM.Core.Crypto.Provider.Interfaces;

interface

uses
  WinApi.Windows,
  System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces;

type
  // The single abstraction the CMS layer uses for signing.
  //
  // Local providers (cert-store, PFX) own a Win32 key handle and let the
  // OS sign in one shot via CryptSignMessage. They set IsLocal=true and
  // AcquirePrivateKey returns the handle pair.
  //
  // Remote providers (Phase 3 §3.3 — Key Vault, Signotaur) don't have the
  // key locally. They set IsLocal=false and implement SignDigest: the CMS
  // layer pre-computes the digest of the to-be-signed bytes, hands it to
  // the provider, and gets the raw signature back. The provider knows
  // nothing about CMS structure — only "sign these bytes with my key".
  ISigningProvider = interface
    ['{D90DD9C6-44B2-4C32-A3B1-5DD3D6E9D2BC}']
    function Certificate : ICertificate;
    /// <summary>
    /// True when the private key is in-process (cert store, PFX). False
    /// for remote providers — those use SignDigest instead. The CMS layer
    /// picks the code path based on this flag.
    /// </summary>
    function IsLocal : boolean;
    /// <summary>
    /// Local-provider only: hand the CMS layer a Win32 key handle so it
    /// can call CryptSignMessage. Remote providers return false.
    /// </summary>
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    /// <summary>
    /// Remote-provider sign primitive: takes the *digest* of the to-be-signed
    /// bytes and the digest algorithm used to compute it, and returns the
    /// raw signature value (the encryptedDigest of the resulting SignerInfo).
    /// The signature scheme (RSA PKCS#1 / PSS / ECDSA) is determined by the
    /// provider's certificate key type. Local providers raise.
    /// </summary>
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
    /// <summary>
    /// Open a signing session. For smart-card / HSM providers this pre-acquires
    /// the private-key handle and keeps it alive until EndSession, so the card
    /// session (and any cached PIN) survives across multiple sign operations.
    /// Remote providers may use this to warm up auth tokens. Calling Sign
    /// without an explicit BeginSession is still valid — providers acquire
    /// what they need on demand and release immediately.
    /// </summary>
    procedure BeginSession;
    /// <summary>
    /// Close the signing session started by BeginSession. Idempotent; called
    /// automatically on destruction if still open.
    /// </summary>
    procedure EndSession;
    /// <summary>
    /// Provide per-sign-call audit metadata (file name + size) to the provider.
    /// Called by the signing service immediately before each sign operation so
    /// remote providers (e.g. Signotaur) can include the correct name/size in
    /// their server-side audit log even when one provider signs many files in
    /// a batch. Local providers no-op. Pass empty / 0 to clear.
    /// </summary>
    procedure SetSigningContext(const fileName : string; fileSize : Int64);
  end;

  ECryptoProvider = class(Exception);
  ENotImplementedSignDigest = class(ECryptoProvider);
  // Raised when the provider's session has failed in a way that won't be
  // fixed by retrying the next file — bad credentials (401), service down
  // (5xx), trust failures, etc. The batch sign loop treats this as fatal
  // regardless of --fail-fast and aborts the run instead of marching through
  // the remaining files with N more guaranteed failures.
  EProviderFatal = class(ECryptoProvider);

implementation

end.
