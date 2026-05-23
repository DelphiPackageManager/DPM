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

unit DPM.Core.Crypto.Provider;

// Phase 1 signing providers — local certificate store and PFX file. Smart
// cards and HSMs work transparently through the certificate-store provider,
// because CryptAcquireCertificatePrivateKey resolves the CNG KSP for us.
//
// Phase 3 will add TKeyVaultProvider and TSignotaurProvider as siblings.

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  TCertStoreSigningProvider = class(TInterfacedObject, ISigningProvider)
  private
    FCertificate : ICertificate;
    // Session keep-alive. When BeginSession is called, the private key is
    // acquired and held until EndSession. For smart cards / HSMs (YubiKey,
    // PIV minidrivers) this keeps the card session — and the cached PIN —
    // alive across the multiple CryptSignMessage calls a multi-file sign
    // makes, so the user is only prompted once per `dpm sign` invocation.
    FSessionHandle      : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
    FSessionKeySpec     : DWORD;
    FSessionNeedsFree   : boolean;
    FSessionActive      : boolean;
  protected
    function Certificate : ICertificate;
    function IsLocal : boolean;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
    procedure BeginSession;
    procedure EndSession;
    procedure SetSigningContext(const fileName : string; fileSize : Int64);
  public
    constructor Create(const cert : ICertificate);
    destructor Destroy; override;
  end;

  TPfxSigningProvider = class(TCertStoreSigningProvider)
  // PFX is functionally identical at the provider level — once
  // PFXImportCertStore has set the leaf's key spec, CryptAcquireCertificate-
  // PrivateKey resolves it the same way. This subclass exists purely so DI
  // registration / CLI plumbing can distinguish the two.
  end;

implementation

{ TCertStoreSigningProvider }

constructor TCertStoreSigningProvider.Create(const cert : ICertificate);
begin
  if cert = nil then
    raise ECryptoProvider.Create('Signing provider requires a certificate');
  inherited Create;
  FCertificate := cert;
  FSessionHandle := 0;
  FSessionKeySpec := 0;
  FSessionNeedsFree := false;
  FSessionActive := false;
end;

destructor TCertStoreSigningProvider.Destroy;
begin
  EndSession;
  inherited;
end;

function TCertStoreSigningProvider.Certificate : ICertificate;
begin
  result := FCertificate;
end;

function TCertStoreSigningProvider.IsLocal : boolean;
begin
  // Cert-store and PFX providers own a Win32 key handle (smart cards / HSM
  // routed transparently through CNG). The CMS layer signs in-process.
  result := true;
end;

function TCertStoreSigningProvider.SignDigest(const digest : TBytes;
                                               digestAlgorithm : THashAlgorithm) : TBytes;
begin
  // Local providers don't need this — the CMS layer goes through the
  // AcquirePrivateKey + CryptSignMessage path. Surface as a clear error in
  // case a future code path accidentally calls it.
  raise ENotImplementedSignDigest.Create(
    'TCertStoreSigningProvider.SignDigest: local providers sign via CryptSignMessage; ' +
    'use AcquirePrivateKey instead.');
end;

function TCertStoreSigningProvider.AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                                                    out keySpec : DWORD;
                                                    out callerMustFree : boolean) : boolean;
var
  freeProv : BOOL;
const
  cCRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG = $00040000;
  cCRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG  = $00010000;
begin
  keyHandle := 0;
  keySpec := 0;
  callerMustFree := false;

  // If a session is active, hand back the pre-acquired handle and tell the
  // caller NOT to release it — the session owns it.
  if FSessionActive then
  begin
    keyHandle := FSessionHandle;
    keySpec := FSessionKeySpec;
    callerMustFree := false;
    result := true;
    exit;
  end;

  result := CryptAcquireCertificatePrivateKey(
    FCertificate.GetContext,
    cCRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG,
    nil,
    keyHandle,
    keySpec,
    freeProv);
  if not result then
    exit;

  callerMustFree := freeProv;
end;

procedure TCertStoreSigningProvider.BeginSession;
var
  freeProv : BOOL;
const
  cCRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG = $00010000;
begin
  if FSessionActive then
    exit;
  // Pre-acquire the private key. The card driver opens its session and
  // (typically) caches the PIN here. Any CryptSignMessage / SignDigest calls
  // made during the session re-acquire against the same already-open card
  // session, so the PIN is not re-prompted.
  if not CryptAcquireCertificatePrivateKey(
       FCertificate.GetContext,
       cCRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG,
       nil,
       FSessionHandle,
       FSessionKeySpec,
       freeProv) then
    raise ECryptoProvider.CreateFmt(
      'BeginSession: CryptAcquireCertificatePrivateKey failed (error %d)',
      [GetLastError]);
  FSessionNeedsFree := freeProv;
  FSessionActive := true;
end;

procedure TCertStoreSigningProvider.SetSigningContext(const fileName : string;
                                                      fileSize : Int64);
begin
  // No-op. Local CryptSignMessage carries no audit metadata to a remote
  // service. The interface method exists so the signing service can call
  // through uniformly.
end;

procedure TCertStoreSigningProvider.EndSession;
begin
  if not FSessionActive then
    exit;
  if FSessionNeedsFree and (FSessionHandle <> 0) then
  begin
    if FSessionKeySpec = CERT_NCRYPT_KEY_SPEC then
      NCryptFreeObject(FSessionHandle)
    else
      CryptReleaseContext(FSessionHandle, 0);
  end;
  FSessionHandle := 0;
  FSessionKeySpec := 0;
  FSessionNeedsFree := false;
  FSessionActive := false;
end;

end.
