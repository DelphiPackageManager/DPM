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

unit DPM.Core.Crypto.Provider.Signotaur;

// Phase 3 §3.3 — Signotaur signing provider (STUB).
//
// Signotaur (https://signotaur.com) is VSoft's hosted code-signing service.
// The plan calls for it as a sibling of TKeyVaultSigningProvider — same
// shape (download cert, SignDigest), different REST surface. Per the
// session brief: "we will come back to it as signotaur needs modification"
// so this is intentionally a stub. The interface seat is reserved so DI
// registration + CLI plumbing for `--provider signotaur` lights up without
// blocking shipping Key Vault.

interface

uses
  WinApi.Windows,
  System.SysUtils,
  DPM.Core.Logging,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  TSignotaurOptions = record
    Endpoint     : string;
    CertId       : string;
    ApiToken     : string;   // typically sourced from --api-token-env
  end;

  TSignotaurSigningProvider = class(TInterfacedObject, ISigningProvider)
  private
    FLogger : ILogger;
    FX509 : IX509Service;
    FOptions : TSignotaurOptions;
  protected
    function Certificate : ICertificate;
    function IsLocal : boolean;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
  public
    constructor Create(const logger : ILogger;
                       const x509 : IX509Service;
                       const options : TSignotaurOptions);
  end;

  ESignotaur = class(ECryptoProvider);

implementation

constructor TSignotaurSigningProvider.Create(const logger : ILogger;
                                              const x509 : IX509Service;
                                              const options : TSignotaurOptions);
begin
  inherited Create;
  FLogger := logger;
  FX509 := x509;
  FOptions := options;
end;

function TSignotaurSigningProvider.Certificate : ICertificate;
begin
  raise ESignotaur.Create(
    'Signotaur signing provider is not yet implemented. ' +
    'Track Phase 3 §3.3 follow-up.');
end;

function TSignotaurSigningProvider.IsLocal : boolean;
begin
  result := false;
end;

function TSignotaurSigningProvider.AcquirePrivateKey(
  out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  out keySpec : DWORD;
  out callerMustFree : boolean) : boolean;
begin
  keyHandle := 0;
  keySpec := 0;
  callerMustFree := false;
  result := false;
end;

function TSignotaurSigningProvider.SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
begin
  raise ESignotaur.Create(
    'Signotaur signing provider is not yet implemented. ' +
    'Track Phase 3 §3.3 follow-up.');
end;

end.
