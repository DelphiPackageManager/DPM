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

unit DPM.Core.Crypto.Cms;

// CMS/PKCS#7 SignedData over Win32 crypt32.dll. Detached mode only.
//
// Sign:   CryptSignMessage with custom signed attributes.
// Verify: CryptVerifyDetachedMessageSignature.
// Decode: CryptMsgOpenToDecode + CryptMsgGetParam (signer info, attrs, cert).
// Patch:  CryptMsgControl(CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR) to add the
//         RFC3161 timestamp token after signing.

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Crypto.Cms.Interfaces;

type
  TCmsService = class(TInterfacedObject, ICmsService)
  private
    FX509 : IX509Service;
  protected
    function Sign(const content : TBytes;
                  const provider : ISigningProvider;
                  const signedAttributes : TCmsAttributes;
                  digest : THashAlgorithm) : TBytes;
    function VerifyDetached(const der : TBytes; const content : TBytes;
                            out signerCert : ICertificate) : boolean;
    function Decode(const der : TBytes) : ICmsSignedData;
    procedure AddUnsignedAttribute(var der : TBytes;
                                   const oid : AnsiString; const value : TBytes);
  public
    constructor Create(const x509 : IX509Service);
  end;

implementation

uses
  DPM.Core.Crypto.Hashing;  // BytesToHex, HexToBytes, BytesEqual

const
  cENCODING_TYPE         = X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;
  cCMSG_SIGNED           = 2;
  cCMSG_DETACHED_FLAG    = 4;

type
  TCmsSignedData = class(TInterfacedObject, ICmsSignedData)
  private
    FDer : TBytes;
    FMsg : HCRYPTMSG;
    FSignerCert : ICertificate;
    FEmbedded : TArray<ICertificate>;
    FSignedAttrs : TCmsAttributes;
    FUnsignedAttrs : TCmsAttributes;
    FDigestAlgorithm : THashAlgorithm;
    FX509 : IX509Service;
    procedure Decode;
    procedure DecodeAttributes(paramType : DWORD; out attrs : TCmsAttributes);
    function DecodeSignerCert : ICertificate;
    procedure DecodeEmbeddedCerts;
    procedure DecodeDigestAlgorithm;
  protected
    function SignerCertificate : ICertificate;
    function EmbeddedCertificates : TArray<ICertificate>;
    function SignedAttributes : TCmsAttributes;
    function UnsignedAttributes : TCmsAttributes;
    function FindSignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
    function FindUnsignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
    function DerBytes : TBytes;
    function DigestAlgorithm : THashAlgorithm;
    function EncryptedDigest : TBytes;
  public
    constructor Create(const der : TBytes; const x509 : IX509Service);
    destructor Destroy; override;
  end;

procedure CheckBool(value : BOOL; const apiName : string);
var
  code : DWORD;
  msg : string;
begin
  if not value then
  begin
    code := GetLastError;
    // SysErrorMessage maps Windows error codes (including HRESULT-style ones
    // like CRYPT_E_*) to readable strings. Falls back to empty for codes the
    // OS doesn't know, in which case the hex code alone is still informative.
    msg := SysErrorMessage(code);
    if msg <> '' then
      raise ECryptoCms.CreateFmt('%s failed: 0x%.8x — %s', [apiName, code, msg])
    else
      raise ECryptoCms.CreateFmt('%s failed: 0x%.8x', [apiName, code]);
  end;
end;

// ---------------------------------------------------------------------------
// Attribute encoding helpers
// ---------------------------------------------------------------------------
//
// CRYPT_SIGN_MESSAGE_PARA.rgAuthAttr expects each attribute's value already
// DER-encoded. Our attribute values are short OCTET STRINGS or UTF8 STRINGS,
// and CryptEncodeObjectEx handles both.

function EncodeOctetString(const raw : TBytes) : TBytes;
const
  cX509_OCTET_STRING : PAnsiChar = PAnsiChar(25);
var
  blob : CRYPT_DATA_BLOB;
  encoded : Pointer;
  size : DWORD;
begin
  blob.cbData := Length(raw);
  if blob.cbData > 0 then
    blob.pbData := @raw[0]
  else
    blob.pbData := nil;

  encoded := nil;
  size := 0;
  CheckBool(
    CryptEncodeObjectEx(cENCODING_TYPE, cX509_OCTET_STRING, @blob,
      CRYPT_ENCODE_ALLOC_FLAG, nil, @encoded, size),
    'CryptEncodeObjectEx(OCTET STRING)');
  try
    SetLength(result, size);
    if size > 0 then
      Move(encoded^, result[0], size);
  finally
    LocalFree(HLOCAL(encoded));
  end;
end;

{ TCmsService }

constructor TCmsService.Create(const x509 : IX509Service);
begin
  if x509 = nil then
    raise ECryptoCms.Create('TCmsService requires an X509 service');
  inherited Create;
  FX509 := x509;
end;

function TCmsService.Sign(const content : TBytes;
                          const provider : ISigningProvider;
                          const signedAttributes : TCmsAttributes;
                          digest : THashAlgorithm) : TBytes;
var
  para : CRYPT_SIGN_MESSAGE_PARA;
  certs : array[0..0] of PCCERT_CONTEXT;
  attrs : array of CRYPT_ATTRIBUTE;
  attrValues : array of CRYPT_INTEGER_BLOB;
  attrBytes : array of TBytes;
  i : integer;
  contentPtr : array[0..0] of Pointer;
  contentSize : array[0..0] of DWORD;
  signedSize : DWORD;
begin
  if not TAlgorithmProfile.CmsDigestAllowed(digest) then
    raise ECryptoCms.CreateFmt('Digest %s not permitted',
      [TAlgorithmProfile.HashAlgorithmName(digest)]);
  if provider = nil then
    raise ECryptoCms.Create('Sign: provider is nil');
  if provider.Certificate = nil then
    raise ECryptoCms.Create('Sign: provider has no certificate');
  // P3 §3.3 — remote providers don't expose a Win32 key handle, so
  // CryptSignMessage can't sign on their behalf. The CMS-assembly path for
  // remote SignDigest is the Phase 3.3 follow-up; for now, give the user
  // a clear actionable error rather than a cryptic CryptSignMessage failure.
  if not provider.IsLocal then
    raise ECryptoCms.Create(
      'Sign: remote signing providers (Key Vault / Signotaur) are not yet ' +
      'wired into the CMS assembly path. Auth + cert download work end-to-end; ' +
      'the manual CMS assembly that consumes provider.SignDigest is tracked as ' +
      'a Phase 3.3 follow-up.');

  // Pre-encode every signed attribute as an OCTET STRING so the OS will
  // accept them. Build the CRYPT_ATTRIBUTE table on top.
  SetLength(attrs, Length(signedAttributes));
  SetLength(attrValues, Length(signedAttributes));
  SetLength(attrBytes, Length(signedAttributes));
  for i := 0 to High(signedAttributes) do
  begin
    attrBytes[i] := EncodeOctetString(signedAttributes[i].Value);
    attrValues[i].cbData := Length(attrBytes[i]);
    if attrValues[i].cbData > 0 then
      attrValues[i].pbData := @attrBytes[i][0]
    else
      attrValues[i].pbData := nil;
    attrs[i].pszObjId := PAnsiChar(signedAttributes[i].Oid);
    attrs[i].cValue := 1;
    attrs[i].rgValue := @attrValues[i];
  end;

  certs[0] := provider.Certificate.GetContext;

  FillChar(para, SizeOf(para), 0);
  para.cbSize := SizeOf(para);
  para.dwMsgEncodingType := cENCODING_TYPE;
  para.pSigningCert := certs[0];
  para.HashAlgorithm.pszObjId := PAnsiChar(TAlgorithmProfile.HashOid(digest));
  para.cMsgCert := 1;
  para.rgpMsgCert := @certs[0];
  if Length(attrs) > 0 then
  begin
    para.cAuthAttr := Length(attrs);
    para.rgAuthAttr := @attrs[0];
  end;

  contentPtr[0] := nil;
  if Length(content) > 0 then
    contentPtr[0] := @content[0];
  contentSize[0] := Length(content);

  signedSize := 0;
  CheckBool(
    CryptSignMessage(@para, True, 1, @contentPtr[0], @contentSize[0], nil, signedSize),
    'CryptSignMessage(size query)');

  SetLength(result, signedSize);
  if signedSize > 0 then
    CheckBool(
      CryptSignMessage(@para, True, 1, @contentPtr[0], @contentSize[0],
        PByte(@result[0]), signedSize),
      'CryptSignMessage');
  SetLength(result, signedSize);
end;

function TCmsService.VerifyDetached(const der : TBytes; const content : TBytes;
                                    out signerCert : ICertificate) : boolean;
var
  para : CRYPT_VERIFY_MESSAGE_PARA;
  contentPtr : array[0..0] of Pointer;
  contentSize : array[0..0] of DWORD;
  ctx : PCCERT_CONTEXT;
begin
  signerCert := nil;
  result := false;
  if Length(der) = 0 then
    exit;

  FillChar(para, SizeOf(para), 0);
  para.cbSize := SizeOf(para);
  para.dwMsgAndCertEncodingType := cENCODING_TYPE;

  contentPtr[0] := nil;
  if Length(content) > 0 then
    contentPtr[0] := @content[0];
  contentSize[0] := Length(content);

  ctx := nil;
  result := CryptVerifyDetachedMessageSignature(
    @para, 0,
    PByte(@der[0]), Length(der),
    1, @contentPtr[0], @contentSize[0],
    @ctx);

  if result and (ctx <> nil) then
    signerCert := FX509.LoadCertificateFromDer(
      Self.Decode(der).EmbeddedCertificates[0].RawDerBytes);
  // The cleaner path is to wrap ctx via X509 — but we don't own a public
  // CertContext-wrapping constructor on the interface. The DER round-trip
  // above keeps lifetime ownership clean. The verifier already has the
  // signer's leaf cert embedded in the CMS, so this is cheap.

  if ctx <> nil then
    CertFreeCertificateContext(ctx);
end;

function TCmsService.Decode(const der : TBytes) : ICmsSignedData;
begin
  result := TCmsSignedData.Create(der, FX509);
end;

procedure TCmsService.AddUnsignedAttribute(var der : TBytes;
                                           const oid : AnsiString; const value : TBytes);
var
  msg : HCRYPTMSG;
  attr : CRYPT_ATTRIBUTE;
  attrValue : CRYPT_INTEGER_BLOB;
  encodedAttr : Pointer;
  encodedAttrSize : DWORD;
  para : CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA;
  encodedSize : DWORD;
  newDer : TBytes;
begin
  msg := CryptMsgOpenToDecode(cENCODING_TYPE, 0, 0, 0, nil, nil);
  if msg = nil then
    raise ECryptoCms.CreateFmt('CryptMsgOpenToDecode failed: %d', [GetLastError]);
  try
    CheckBool(CryptMsgUpdate(msg, PByte(@der[0]), Length(der), True),
      'CryptMsgUpdate');

    // The attribute value for an RFC3161 counter-signature is the raw DER
    // TimeStampToken (a ContentInfo) — NOT wrapped in OCTET STRING. The
    // attribute value bytes are used as-is.
    attrValue.cbData := Length(value);
    if attrValue.cbData > 0 then
      attrValue.pbData := @value[0]
    else
      attrValue.pbData := nil;
    attr.pszObjId := PAnsiChar(oid);
    attr.cValue := 1;
    attr.rgValue := @attrValue;

    // CryptMsgControl(ADD_SIGNER_UNAUTH_ATTR) takes a struct whose `blob`
    // is the *DER-encoded* CRYPT_ATTRIBUTE — not the raw struct in memory.
    // Hand the attribute to CryptEncodeObjectEx(PKCS_ATTRIBUTE) first, then
    // pass the resulting bytes to CryptMsgControl.
    encodedAttr := nil;
    encodedAttrSize := 0;
    CheckBool(
      CryptEncodeObjectEx(cENCODING_TYPE, PKCS_ATTRIBUTE, @attr,
        CRYPT_ENCODE_ALLOC_FLAG, nil, @encodedAttr, encodedAttrSize),
      'CryptEncodeObjectEx(PKCS_ATTRIBUTE)');
    try
      FillChar(para, SizeOf(para), 0);
      para.cbSize := SizeOf(para);
      para.dwSignerIndex := 0;
      para.blob.cbData := encodedAttrSize;
      para.blob.pbData := encodedAttr;

      CheckBool(CryptMsgControl(msg, 0, CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR, @para),
        'CryptMsgControl(ADD_UNAUTH_ATTR)');
    finally
      LocalFree(HLOCAL(encodedAttr));
    end;

    // Read the rewritten CMS bytes back out.
    encodedSize := 0;
    CheckBool(CryptMsgGetParam(msg, 29 {CMSG_ENCODED_MESSAGE}, 0, nil, encodedSize), 'CryptMsgGetParam(ENCODED_MESSAGE size)');
    SetLength(newDer, encodedSize);
    CheckBool(CryptMsgGetParam(msg, 29 {CMSG_ENCODED_MESSAGE}, 0, @newDer[0], encodedSize), 'CryptMsgGetParam(ENCODED_MESSAGE)');
    SetLength(newDer, encodedSize);
    der := newDer;
  finally
    CryptMsgClose(msg);
  end;
end;

{ TCmsSignedData }

constructor TCmsSignedData.Create(const der : TBytes; const x509 : IX509Service);
begin
  inherited Create;
  FDer := der;
  FX509 := x509;
  FMsg := CryptMsgOpenToDecode(cENCODING_TYPE, 0, 0, 0, nil, nil);
  if FMsg = nil then
    raise ECryptoCms.CreateFmt('CryptMsgOpenToDecode failed: %d', [GetLastError]);
  if Length(FDer) > 0 then
    CheckBool(CryptMsgUpdate(FMsg, PByte(@FDer[0]), Length(FDer), True),
      'CryptMsgUpdate');
  Decode;
end;

destructor TCmsSignedData.Destroy;
begin
  if FMsg <> nil then
    CryptMsgClose(FMsg);
  inherited;
end;

procedure TCmsSignedData.Decode;
begin
  FSignerCert := DecodeSignerCert;
  DecodeEmbeddedCerts;
  DecodeAttributes(CMSG_SIGNER_AUTH_ATTR_PARAM, FSignedAttrs);
  DecodeAttributes(CMSG_SIGNER_UNAUTH_ATTR_PARAM, FUnsignedAttrs);
  DecodeDigestAlgorithm;
end;

function TCmsSignedData.DecodeSignerCert : ICertificate;
var
  count : DWORD;
  countSize : DWORD;
  i : integer;
  size : DWORD;
  der : TBytes;
begin
  result := nil;
  countSize := SizeOf(count);
  if not CryptMsgGetParam(FMsg, CMSG_CERT_COUNT_PARAM, 0, @count, countSize) then
    exit;
  for i := 0 to Integer(count) - 1 do
  begin
    size := 0;
    if not CryptMsgGetParam(FMsg, CMSG_CERT_PARAM, i, nil, size) then
      Continue;
    SetLength(der, size);
    if not CryptMsgGetParam(FMsg, CMSG_CERT_PARAM, i, @der[0], size) then
      Continue;
    SetLength(der, size);
    // For Phase 1 we take the first certificate; if there were multiple
    // SignerInfos we would match by SignerInfo.IssuerAndSerialNumber.
    result := FX509.LoadCertificateFromDer(der);
    if i = 0 then
      exit;
  end;
end;

procedure TCmsSignedData.DecodeEmbeddedCerts;
var
  count : DWORD;
  countSize : DWORD;
  i : integer;
  size : DWORD;
  der : TBytes;
begin
  countSize := SizeOf(count);
  if not CryptMsgGetParam(FMsg, CMSG_CERT_COUNT_PARAM, 0, @count, countSize) then
    exit;
  SetLength(FEmbedded, count);
  for i := 0 to Integer(count) - 1 do
  begin
    size := 0;
    CryptMsgGetParam(FMsg, CMSG_CERT_PARAM, i, nil, size);
    SetLength(der, size);
    CryptMsgGetParam(FMsg, CMSG_CERT_PARAM, i, @der[0], size);
    SetLength(der, size);
    FEmbedded[i] := FX509.LoadCertificateFromDer(der);
  end;
end;

procedure TCmsSignedData.DecodeAttributes(paramType : DWORD; out attrs : TCmsAttributes);
var
  size : DWORD;
  buf : TBytes;
  pAttrs : PCRYPT_ATTRIBUTES;
  i : integer;
  attr : PCRYPT_ATTRIBUTE;
begin
  attrs := nil;
  size := 0;
  if not CryptMsgGetParam(FMsg, paramType, 0, nil, size) or (size = 0) then
    exit;
  SetLength(buf, size);
  if not CryptMsgGetParam(FMsg, paramType, 0, @buf[0], size) then
    exit;
  pAttrs := PCRYPT_ATTRIBUTES(@buf[0]);
  SetLength(attrs, pAttrs.cAttr);
  attr := pAttrs.rgAttr;
  for i := 0 to Integer(pAttrs.cAttr) - 1 do
  begin
    attrs[i].Oid := AnsiString(attr.pszObjId);
    if (attr.cValue > 0) and (attr.rgValue.cbData > 0) then
    begin
      SetLength(attrs[i].Value, attr.rgValue.cbData);
      Move(attr.rgValue.pbData^, attrs[i].Value[0], attr.rgValue.cbData);
    end;
    Inc(attr);
  end;
end;

procedure TCmsSignedData.DecodeDigestAlgorithm;
var
  size : DWORD;
  buf : TBytes;
  alg : PCRYPT_ALGORITHM_IDENTIFIER;
  oid : AnsiString;
begin
  FDigestAlgorithm := haUnknown;
  size := 0;
  if not CryptMsgGetParam(FMsg, CMSG_SIGNER_HASH_ALGORITHM_PARAM, 0, nil, size) or (size = 0) then
    exit;
  SetLength(buf, size);
  if not CryptMsgGetParam(FMsg, CMSG_SIGNER_HASH_ALGORITHM_PARAM, 0, @buf[0], size) then
    exit;
  alg := PCRYPT_ALGORITHM_IDENTIFIER(@buf[0]);
  oid := AnsiString(alg.pszObjId);
  if oid = szOID_NIST_SHA256 then
    FDigestAlgorithm := haSha256
  else if oid = szOID_NIST_SHA384 then
    FDigestAlgorithm := haSha384
  else if oid = szOID_NIST_SHA512 then
    FDigestAlgorithm := haSha512;
end;

function TCmsSignedData.SignerCertificate : ICertificate;
begin
  result := FSignerCert;
end;

function TCmsSignedData.EmbeddedCertificates : TArray<ICertificate>;
begin
  result := FEmbedded;
end;

function TCmsSignedData.SignedAttributes : TCmsAttributes;
begin
  result := FSignedAttrs;
end;

function TCmsSignedData.UnsignedAttributes : TCmsAttributes;
begin
  result := FUnsignedAttrs;
end;

function TCmsSignedData.FindSignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
var
  i : integer;
begin
  for i := 0 to High(FSignedAttrs) do
    if FSignedAttrs[i].Oid = oid then
    begin
      value := FSignedAttrs[i].Value;
      result := true;
      exit;
    end;
  value := nil;
  result := false;
end;

function TCmsSignedData.FindUnsignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
var
  i : integer;
begin
  for i := 0 to High(FUnsignedAttrs) do
    if FUnsignedAttrs[i].Oid = oid then
    begin
      value := FUnsignedAttrs[i].Value;
      result := true;
      exit;
    end;
  value := nil;
  result := false;
end;

function TCmsSignedData.DerBytes : TBytes;
begin
  result := FDer;
end;

function TCmsSignedData.DigestAlgorithm : THashAlgorithm;
begin
  result := FDigestAlgorithm;
end;

function TCmsSignedData.EncryptedDigest : TBytes;
var
  msg : HCRYPTMSG;
  size : DWORD;
begin
  SetLength(result, 0);
  if Length(FDer) = 0 then
    exit;
  msg := CryptMsgOpenToDecode(cENCODING_TYPE, 0, 0, 0, nil, nil);
  if msg = nil then
    raise ECryptoCms.CreateFmt('CryptMsgOpenToDecode failed: 0x%.8x', [GetLastError]);
  try
    CheckBool(CryptMsgUpdate(msg, PByte(@FDer[0]), Length(FDer), True),
      'CryptMsgUpdate(EncryptedDigest)');
    size := 0;
    CheckBool(CryptMsgGetParam(msg, CMSG_ENCRYPTED_DIGEST, 0, nil, size),
      'CryptMsgGetParam(ENCRYPTED_DIGEST size)');
    SetLength(result, size);
    if size > 0 then
      CheckBool(CryptMsgGetParam(msg, CMSG_ENCRYPTED_DIGEST, 0, @result[0], size),
        'CryptMsgGetParam(ENCRYPTED_DIGEST)');
    SetLength(result, size);
  finally
    CryptMsgClose(msg);
  end;
end;

end.
