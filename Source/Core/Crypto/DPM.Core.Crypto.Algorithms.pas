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

unit DPM.Core.Crypto.Algorithms;

// Mandatory algorithm profile (architecture doc Cryptographic Algorithms,
// conformance V-16 and S-5). Single source of truth for what algorithms the
// signer and verifier accept. SHA-1 and MD5 are *never* accepted, regardless
// of what a manifest or signature declares.
//
// Adding an algorithm in a later phase is one new line; weakening the profile
// requires a phase bump.

interface

uses
  System.SysUtils,
  DPM.Core.Crypto.Win32;

type
  THashAlgorithm = (
    haUnknown,    // sentinel — never accepted
    haSha256,
    haSha384,
    haSha512
  );

  TAlgorithmProfile = record
  public
    // V-16: file-hash algorithms allowed in dpm-manifest.json
    class function FileHashAllowed(algorithm : THashAlgorithm) : boolean; static;

    // V-16: CMS digest algorithms allowed in author / repository signatures
    class function CmsDigestAllowed(algorithm : THashAlgorithm) : boolean; static;

    // V-16: timestamp digest algorithms — currently SHA-256 only (the dominant
    // TSA response algorithm; broaden when justified)
    class function TimestampDigestAllowed(algorithm : THashAlgorithm) : boolean; static;

    // V-16 / S-5: signature algorithms identified by Windows OID strings
    class function SignatureAlgorithmAllowed(const oid : string) : boolean; static;

    // S-5: minimum RSA modulus in bits
    class function MinRsaKeyBits : integer; static;

    // Helpers
    class function ParseHashName(const name : string; out algorithm : THashAlgorithm) : boolean; static;
    class function HashAlgorithmName(algorithm : THashAlgorithm) : string; static;
    class function HashOid(algorithm : THashAlgorithm) : AnsiString; static;
    class function HashOutputSize(algorithm : THashAlgorithm) : integer; static;
    class function HashBCryptAlgorithm(algorithm : THashAlgorithm) : PWideChar; static;
  end;

  ECryptoAlgorithm = class(Exception)
  end;

implementation


{ TAlgorithmProfile }

class function TAlgorithmProfile.FileHashAllowed(algorithm : THashAlgorithm) : boolean;
begin
  result := algorithm in [haSha256, haSha384, haSha512];
end;

class function TAlgorithmProfile.CmsDigestAllowed(algorithm : THashAlgorithm) : boolean;
begin
  result := algorithm in [haSha256, haSha384, haSha512];
end;

class function TAlgorithmProfile.TimestampDigestAllowed(algorithm : THashAlgorithm) : boolean;
begin
  // Architecture doc table: timestamp digest is SHA-256 in Phase 1. Broaden
  // only when a TSA we actually rely on requires SHA-384/512.
  result := algorithm = haSha256;
end;

class function TAlgorithmProfile.SignatureAlgorithmAllowed(const oid : string) : boolean;
const
  // RSA PKCS#1 v1.5 signature OIDs accepted in Phase 1
  cRsaSha256 = '1.2.840.113549.1.1.11';
  cRsaSha384 = '1.2.840.113549.1.1.12';
  cRsaSha512 = '1.2.840.113549.1.1.13';
  // ECDSA signature OIDs (ANSI X9.62)
  cEcdsaSha256 = '1.2.840.10045.4.3.2';
  cEcdsaSha384 = '1.2.840.10045.4.3.3';
  // Bare RSA — discouraged but allowed for legacy CAs; the hash declared on
  // the signed-attribute side still determines actual digest strength.
  cRsa = '1.2.840.113549.1.1.1';
begin
  result :=
    (oid = cRsaSha256) or (oid = cRsaSha384) or (oid = cRsaSha512) or
    (oid = cEcdsaSha256) or (oid = cEcdsaSha384) or
    (oid = cRsa);
end;

class function TAlgorithmProfile.MinRsaKeyBits : integer;
begin
  result := 2048;
end;

class function TAlgorithmProfile.ParseHashName(const name : string; out algorithm : THashAlgorithm) : boolean;
var
  normalised : string;
begin
  algorithm := haUnknown;
  normalised := UpperCase(Trim(name));

  // Strip any "SHA-" hyphen to be lenient on input formatting.
  if (Length(normalised) > 4) and (Copy(normalised, 1, 4) = 'SHA-') then
    normalised := 'SHA' + Copy(normalised, 5, MaxInt);

  if normalised = 'SHA256' then
    algorithm := haSha256
  else if normalised = 'SHA384' then
    algorithm := haSha384
  else if normalised = 'SHA512' then
    algorithm := haSha512;

  // SHA1, MD5, and unknown names all leave algorithm = haUnknown.
  result := algorithm <> haUnknown;
end;

class function TAlgorithmProfile.HashAlgorithmName(algorithm : THashAlgorithm) : string;
begin
  case algorithm of
    haSha256 : result := 'SHA256';
    haSha384 : result := 'SHA384';
    haSha512 : result := 'SHA512';
  else
    result := 'UNKNOWN';
  end;
end;

class function TAlgorithmProfile.HashOid(algorithm : THashAlgorithm) : AnsiString;
begin
  case algorithm of
    haSha256 : result := szOID_NIST_SHA256;
    haSha384 : result := szOID_NIST_SHA384;
    haSha512 : result := szOID_NIST_SHA512;
  else
    raise ECryptoAlgorithm.CreateFmt('No OID for hash algorithm %d', [Ord(algorithm)]);
  end;
end;

class function TAlgorithmProfile.HashOutputSize(algorithm : THashAlgorithm) : integer;
begin
  case algorithm of
    haSha256 : result := 32;
    haSha384 : result := 48;
    haSha512 : result := 64;
  else
    result := 0;
  end;
end;

class function TAlgorithmProfile.HashBCryptAlgorithm(algorithm : THashAlgorithm) : PWideChar;
begin
  case algorithm of
    haSha256 : result := BCRYPT_SHA256_ALGORITHM;
    haSha384 : result := BCRYPT_SHA384_ALGORITHM;
    haSha512 : result := BCRYPT_SHA512_ALGORITHM;
  else
    raise ECryptoAlgorithm.CreateFmt('No BCrypt name for hash algorithm %d', [Ord(algorithm)]);
  end;
end;

end.
