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

unit DPM.Core.Crypto.Timestamping;

// RFC3161 timestamp token request and verification over Win32 crypt32.
// CryptRetrieveTimeStamp signs over a hash of the *signature*, and the
// returned token is attached as a CMS unsigned attribute (counterSignature
// OID 1.3.6.1.4.1.311.3.3.1).

interface

uses
  DPM.Core.Diagnostics,
  Winapi.Windows,
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces;

type
  ITimestampToken = interface
    ['{74A37F1B-1B0C-4D63-9A8F-2C8C3D1B6D02}']
    function SigningTime : TDateTime;            // UTC
    function RawToken : TBytes;
    function TsaCertificate : ICertificate;
  end;

  TTimestampCheck = record
    SigningTime : TDateTime;        // UTC, as the token states it
    Authority : string;             // the timestamp authority certificate's subject
    // The authority's certificate chain reaches a root this machine trusts. When it
    // doesn't, nothing vouches for SigningTime and the caller must not rely on it.
    AuthorityTrusted : boolean;
    FailureReason : string;         // why the token was rejected - '' when it wasn't
  end;

  ITimestamper = interface
    ['{6B3FAB2C-1B5E-4F4E-A6A1-3F9D8E70F02A}']
    function RequestTimestamp(const dataToTimestamp : TBytes;
                              const url : string;
                              digest : THashAlgorithm) : ITimestampToken;
    /// <summary>
    /// Verifies an RFC3161 token over data : the authority's signature and the message
    /// imprint, a SHA-256 imprint, and an authority certificate issued for time stamping
    /// whose chain is sound as of the timestamp. A chain whose only fault is a root this
    /// machine doesn't trust (an out of date root store, say) is reported through
    /// AuthorityTrusted rather than rejected. False - with FailureReason - otherwise.
    /// </summary>
    function Verify(const token : TBytes; const data : TBytes;
                    checkRevocation : boolean;
                    out check : TTimestampCheck) : boolean;
  end;

  TWindowsTimestamper = class(TInterfacedObject, ITimestamper)
  private
    FX509 : IX509Service;
  protected
    function RequestTimestamp(const dataToTimestamp : TBytes;
                              const url : string;
                              digest : THashAlgorithm) : ITimestampToken;
    function Verify(const token : TBytes; const data : TBytes;
                    checkRevocation : boolean;
                    out check : TTimestampCheck) : boolean;
  public
    constructor Create(const x509 : IX509Service);
  end;

  //raised with E7105 unless said otherwise (see EDPMCodedError).
  ECryptoTimestamp = class(EDPMCodedError)
  protected
    class function DefaultCode : TErrorCode; override;
  end;

implementation

uses
  System.DateUtils;

{ ECryptoTimestamp }

class function ECryptoTimestamp.DefaultCode : TErrorCode;
begin
  result := TErrorCode.E7105_TimestampFailed;
end;

const
  cTIMESTAMP_NO_AUTH_RETRIEVAL = $00020000;
  cTIMESTAMP_VERIFY_CONTEXT_SIGNATURE = $00000020;

type
  TTimestampToken = class(TInterfacedObject, ITimestampToken)
  private
    FToken : TBytes;
    FSigningTime : TDateTime;
    FTsaCert : ICertificate;
  protected
    function SigningTime : TDateTime;
    function RawToken : TBytes;
    function TsaCertificate : ICertificate;
  public
    constructor Create(const token : TBytes; signingTime : TDateTime;
                       const tsaCert : ICertificate);
  end;

function FileTimeToUtc(const ft : TFileTime) : TDateTime;
var
  st : TSystemTime;
begin
  if FileTimeToSystemTime(ft, st) then
    result := SystemTimeToDateTime(st)
  else
    result := 0;
end;

{ TTimestampToken }

constructor TTimestampToken.Create(const token : TBytes; signingTime : TDateTime;
                                   const tsaCert : ICertificate);
begin
  inherited Create;
  FToken := token;
  FSigningTime := signingTime;
  FTsaCert := tsaCert;
end;

function TTimestampToken.SigningTime : TDateTime;
begin
  result := FSigningTime;
end;

function TTimestampToken.RawToken : TBytes;
begin
  result := FToken;
end;

function TTimestampToken.TsaCertificate : ICertificate;
begin
  result := FTsaCert;
end;

{ TWindowsTimestamper }

constructor TWindowsTimestamper.Create(const x509 : IX509Service);
begin
  if x509 = nil then
    raise ECryptoTimestamp.Create('TWindowsTimestamper requires an X509 service');
  inherited Create;
  FX509 := x509;
end;

function TWindowsTimestamper.RequestTimestamp(const dataToTimestamp : TBytes;
                                              const url : string;
                                              digest : THashAlgorithm) : ITimestampToken;
var
  para : CRYPT_TIMESTAMP_PARA;
  ctx : PCRYPT_TIMESTAMP_CONTEXT;
  tokenBytes : TBytes;
  signingTime : TDateTime;
begin
  if not TAlgorithmProfile.TimestampDigestAllowed(digest) then
    raise ECryptoTimestamp.CreateFmt('Timestamp digest %s not permitted',
      [TAlgorithmProfile.HashAlgorithmName(digest)]);
  if url = '' then
    raise ECryptoTimestamp.Create('Timestamp URL is empty');

  FillChar(para, SizeOf(para), 0);
  para.fRequestCerts := True;   // request the TSA certificate chain

  ctx := nil;
  if not CryptRetrieveTimeStamp(
       PWideChar(url),
       cTIMESTAMP_NO_AUTH_RETRIEVAL,
       30000,                              // 30s
       PAnsiChar(TAlgorithmProfile.HashOid(digest)),
       @para,
       PByte(@dataToTimestamp[0]),
       Length(dataToTimestamp),
       ctx,
       nil,
       nil) then
    raise ECryptoTimestamp.CreateFmt('CryptRetrieveTimeStamp failed for %s: 0x%.8x',
      [url, GetLastError]);

  try
    SetLength(tokenBytes, ctx.cbEncoded);
    if ctx.cbEncoded > 0 then
      Move(ctx.pbEncoded^, tokenBytes[0], ctx.cbEncoded);
    signingTime := FileTimeToUtc(ctx.pTimeStamp.ftTime);
    // For Phase 1 we don't surface the TSA certificate separately — Verify
    // returns it. The returned token has the TSA cert embedded in its CMS.
    result := TTimestampToken.Create(tokenBytes, signingTime, nil);
  finally
    if ctx <> nil then
      CryptMemFree(ctx);
  end;
end;

function TWindowsTimestamper.Verify(const token : TBytes; const data : TBytes;
                                    checkRevocation : boolean;
                                    out check : TTimestampCheck) : boolean;
var
  ctx : PCRYPT_TIMESTAMP_CONTEXT;
  tsSigner : PCCERT_CONTEXT;
  tsStore : HCERTSTORE;
  hashOid : AnsiString;
  hashAlg : THashAlgorithm;
  tsaCert : ICertificate;
  extras : array of ICertificate;
  enumCtx : PCCERT_CONTEXT;
  chain : ICertificateChain;
  chainResult : TChainResult;
  untrustedRoot : boolean;

  function CertFromContext(const certContext : PCCERT_CONTEXT) : ICertificate;
  var
    der : TBytes;
  begin
    SetLength(der, certContext.cbCertEncoded);
    if certContext.cbCertEncoded > 0 then
      Move(certContext.pbCertEncoded^, der[0], certContext.cbCertEncoded);
    result := FX509.LoadCertificateFromDer(der);
  end;

begin
  check.SigningTime := 0;
  check.Authority := '';
  check.AuthorityTrusted := false;
  check.FailureReason := '';
  result := false;
  if (Length(token) = 0) or (Length(data) = 0) then
  begin
    check.FailureReason := 'the timestamp token is empty';
    exit;
  end;
  ctx := nil;
  tsSigner := nil;
  tsStore := nil;
  try
    // The token's own signature and its message imprint over data. This is the only
    // parser of the token - a strict DER decoder rejects tokens Windows accepts, and
    // the gallery has some.
    if not CryptVerifyTimeStampSignature(
        PByte(@token[0]), Length(token),
        PByte(@data[0]), Length(data),
        nil,
        ctx,
        @tsSigner, @tsStore) then
    begin
      check.FailureReason := 'the timestamp does not verify : ' + SysErrorMessage(GetLastError);
      exit;
    end;
    check.SigningTime := FileTimeToUtc(ctx.pTimeStamp.ftTime);

    hashOid := AnsiString(ctx.pTimeStamp.HashAlgorithm.pszObjId);
    if hashOid = szOID_NIST_SHA256 then
      hashAlg := haSha256
    else if hashOid = szOID_NIST_SHA384 then
      hashAlg := haSha384
    else if hashOid = szOID_NIST_SHA512 then
      hashAlg := haSha512
    else
      hashAlg := haUnknown;
    if not TAlgorithmProfile.TimestampDigestAllowed(hashAlg) then
    begin
      check.FailureReason := Format('the timestamp''s digest algorithm [%s] is not allowed (SHA-256 only)', [string(hashOid)]);
      exit;
    end;

    // Who issued it. Without this anyone could mint a token with any time in it - and a
    // backdated time would make an expired signing certificate look valid.
    if tsSigner = nil then
    begin
      check.FailureReason := 'the timestamp carries no signer certificate';
      exit;
    end;
    tsaCert := CertFromContext(tsSigner);
    check.Authority := tsaCert.SubjectDistinguishedName;
    if not tsaCert.HasEku(szOID_PKIX_KP_TIMESTAMP_SIGNING, true) then
    begin
      check.FailureReason := 'the timestamp authority''s certificate is not issued for time stamping';
      exit;
    end;
    if (tsaCert.PublicKeyType = cktRsa) and (tsaCert.PublicKeyBits < TAlgorithmProfile.MinRsaKeyBits) then
    begin
      check.FailureReason := Format('the timestamp authority''s RSA key is %d bits - at least %d are required',
        [tsaCert.PublicKeyBits, TAlgorithmProfile.MinRsaKeyBits]);
      exit;
    end;

    // the other certificates the token carries - the authority's intermediates.
    SetLength(extras, 0);
    if tsStore <> nil then
    begin
      enumCtx := CertEnumCertificatesInStore(tsStore, nil);
      while enumCtx <> nil do
      begin
        SetLength(extras, Length(extras) + 1);
        extras[High(extras)] := CertFromContext(enumCtx);
        enumCtx := CertEnumCertificatesInStore(tsStore, enumCtx);
      end;
    end;

    chain := FX509.CreateChain;
    chainResult := chain.BuildAtTimeForUsage(tsaCert, extras, check.SigningTime, checkRevocation, szOID_PKIX_KP_TIMESTAMP_SIGNING);
    untrustedRoot := (chainResult = crUntrustedRoot) and chain.EndsOnlyAtUntrustedRoot;
    if (chainResult <> crValid) and (not untrustedRoot) then
    begin
      check.FailureReason := 'the timestamp authority''s certificate chain : ' + chain.LastErrorMessage;
      exit;
    end;
    chainResult := chain.VerifyForCodeSigning(check.SigningTime, untrustedRoot);
    if chainResult <> crValid then
    begin
      check.FailureReason := 'the timestamp authority''s certificate chain : ' + chain.LastErrorMessage;
      exit;
    end;
    check.AuthorityTrusted := not untrustedRoot;
    result := true;
  finally
    if tsSigner <> nil then
      CertFreeCertificateContext(tsSigner);
    if tsStore <> nil then
      CertCloseStore(tsStore, 0);
    if ctx <> nil then
      CryptMemFree(ctx);
  end;
end;

end.
