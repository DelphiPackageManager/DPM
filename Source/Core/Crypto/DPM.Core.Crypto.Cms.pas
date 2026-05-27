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
  DPM.Core.Logging,
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
    FHashing : IHashingService;
    FLogger : ILogger;
    /// <summary>
    /// P3 §3.3 v2 — assemble a detached CMS SignedData when the provider
    /// can't expose a Win32 key handle. Computes the digest of the signed
    /// attributes locally, delegates the actual signature to provider.SignDigest
    /// (single remote call), then hand-encodes the SignerInfo + outer
    /// SignedData using CryptEncodeObjectEx + manual DER for the wrappers
    /// that Windows doesn't expose an encoder for.
    /// </summary>
    function SignRemote(const content : TBytes;
                        const provider : ISigningProvider;
                        const signedAttributes : TCmsAttributes;
                        digest : THashAlgorithm) : TBytes;
  protected
    function Sign(const content : TBytes;
                  const provider : ISigningProvider;
                  const signedAttributes : TCmsAttributes;
                  digest : THashAlgorithm) : TBytes;
    function VerifyDetached(const der : TBytes; const content : TBytes;
                            out signerCert : ICertificate) : boolean;
    function Decode(const der : TBytes) : ICmsSignedData;
    function ExtractEncryptedDigest(const der : TBytes) : TBytes;
    procedure AddUnsignedAttribute(var der : TBytes;
                                   const oid : AnsiString; const value : TBytes);
  public
    constructor Create(const logger : ILogger;
                       const x509 : IX509Service;
                       const hashing : IHashingService);
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
    function FindSignedAttributeValues(const oid : AnsiString; out values : TArray<TBytes>) : boolean;
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

constructor TCmsService.Create(const logger : ILogger;
                               const x509 : IX509Service;
                               const hashing : IHashingService);
begin
  if logger = nil then
    raise ECryptoCms.Create('TCmsService requires a logger');
  if x509 = nil then
    raise ECryptoCms.Create('TCmsService requires an X509 service');
  if hashing = nil then
    raise ECryptoCms.Create('TCmsService requires a hashing service');
  inherited Create;
  FLogger := logger;
  FX509 := x509;
  FHashing := hashing;
end;

// ---------------------------------------------------------------------------
// DER hand-encoding primitives — used by SignRemote to assemble the outer
// SignedData SEQUENCE / SET wrappers that Windows doesn't expose a struct
// encoder for. Each helper takes already-encoded content and returns the
// tag+length+content bytes.
// ---------------------------------------------------------------------------

function DerLength(len : integer) : TBytes;
var
  buf : array[0..4] of byte;
  numBytes, i : integer;
begin
  if len < $80 then
  begin
    SetLength(result, 1);
    result[0] := byte(len);
  end
  else
  begin
    numBytes := 0;
    i := len;
    while i > 0 do
    begin
      buf[numBytes] := byte(i and $FF);
      i := i shr 8;
      Inc(numBytes);
    end;
    SetLength(result, numBytes + 1);
    result[0] := $80 or byte(numBytes);
    // Length octets are big-endian.
    for i := 0 to numBytes - 1 do
      result[numBytes - i] := buf[i];
  end;
end;

function DerWrapTag(tag : byte; const content : TBytes) : TBytes;
var
  lenBytes : TBytes;
begin
  lenBytes := DerLength(Length(content));
  SetLength(result, 1 + Length(lenBytes) + Length(content));
  result[0] := tag;
  Move(lenBytes[0], result[1], Length(lenBytes));
  if Length(content) > 0 then
    Move(content[0], result[1 + Length(lenBytes)], Length(content));
end;

function DerSequence(const content : TBytes) : TBytes;
begin
  result := DerWrapTag($30, content);
end;

function DerSet(const content : TBytes) : TBytes;
begin
  result := DerWrapTag($31, content);
end;

function DerContext0Implicit(const content : TBytes) : TBytes;
// [0] IMPLICIT (constructed) — context-specific tag with constructed bit.
// Used for the certificates[0] in SignedData.
begin
  result := DerWrapTag($A0, content);
end;

function DerInteger(value : integer) : TBytes;
var
  buf : array[0..3] of byte;
  i, numBytes : integer;
  needsLeadingZero : boolean;
  payload : TBytes;
begin
  if value = 0 then
  begin
    SetLength(payload, 1);
    payload[0] := 0;
  end
  else
  begin
    numBytes := 0;
    i := value;
    while i > 0 do
    begin
      buf[numBytes] := byte(i and $FF);
      i := i shr 8;
      Inc(numBytes);
    end;
    needsLeadingZero := (buf[numBytes - 1] and $80) <> 0;
    if needsLeadingZero then
    begin
      SetLength(payload, numBytes + 1);
      payload[0] := 0;
      for i := 0 to numBytes - 1 do
        payload[numBytes - i] := buf[i];
    end
    else
    begin
      SetLength(payload, numBytes);
      for i := 0 to numBytes - 1 do
        payload[numBytes - 1 - i] := buf[i];
    end;
  end;
  result := DerWrapTag($02, payload);
end;

function ConcatBytes(const parts : array of TBytes) : TBytes;
var
  i, total, offset : integer;
begin
  total := 0;
  for i := 0 to High(parts) do
    Inc(total, Length(parts[i]));
  SetLength(result, total);
  offset := 0;
  for i := 0 to High(parts) do
  begin
    if Length(parts[i]) > 0 then
    begin
      Move(parts[i][0], result[offset], Length(parts[i]));
      Inc(offset, Length(parts[i]));
    end;
  end;
end;

// CryptEncodeObjectEx wrapper that returns the bytes as TBytes and frees
// the OS-allocated buffer for us.
function EncodeWith(structType : PAnsiChar; structInfo : Pointer; const apiName : string) : TBytes;
var
  buf : Pointer;
  size : DWORD;
begin
  buf := nil;
  size := 0;
  CheckBool(
    CryptEncodeObjectEx(cENCODING_TYPE, structType, structInfo,
      CRYPT_ENCODE_ALLOC_FLAG, nil, @buf, size),
    apiName);
  try
    SetLength(result, size);
    if size > 0 then
      Move(buf^, result[0], size);
  finally
    LocalFree(HLOCAL(buf));
  end;
end;

function EncodeAttributeOctet(const oid : AnsiString; const value : TBytes) : TBytes;
var
  octet : TBytes;
  blob : CRYPT_INTEGER_BLOB;
  attr : CRYPT_ATTRIBUTE;
begin
  // Wrap the value in OCTET STRING — caller's convention for our signed
  // attribute payloads (e.g. dpmSignatureRole bytes).
  octet := EncodeOctetString(value);
  blob.cbData := Length(octet);
  if blob.cbData > 0 then
    blob.pbData := @octet[0]
  else
    blob.pbData := nil;
  attr.pszObjId := PAnsiChar(oid);
  attr.cValue := 1;
  attr.rgValue := @blob;
  result := EncodeWith(PKCS_ATTRIBUTE, @attr, 'CryptEncodeObjectEx(PKCS_ATTRIBUTE)');
end;

function EncodeAttributeRaw(const oid : AnsiString; const valueDer : TBytes) : TBytes;
var
  blob : CRYPT_INTEGER_BLOB;
  attr : CRYPT_ATTRIBUTE;
begin
  // Value already DER-encoded (e.g. OBJECT IDENTIFIER for id-data).
  blob.cbData := Length(valueDer);
  if blob.cbData > 0 then
    blob.pbData := @valueDer[0]
  else
    blob.pbData := nil;
  attr.pszObjId := PAnsiChar(oid);
  attr.cValue := 1;
  attr.rgValue := @blob;
  result := EncodeWith(PKCS_ATTRIBUTE, @attr, 'CryptEncodeObjectEx(PKCS_ATTRIBUTE)');
end;

function EncodeOidValue(const oid : AnsiString) : TBytes;
var
  p : PAnsiChar;
begin
  // X509_OBJECT_IDENTIFIER struct type is declared in DPM.Core.Crypto.Win32
  // alongside the other CryptEncodeObjectEx struct-type ids.
  p := PAnsiChar(oid);
  result := EncodeWith(X509_OBJECT_IDENTIFIER, @p, 'CryptEncodeObjectEx(X509_OBJECT_IDENTIFIER)');
end;

function EncodeAlgorithmIdentifier(const oid : AnsiString) : TBytes;
var
  alg : CRYPT_ALGORITHM_IDENTIFIER;
begin
  FillChar(alg, SizeOf(alg), 0);
  alg.pszObjId := PAnsiChar(oid);
  // Parameters field left empty — Windows omits the optional NULL for some
  // OIDs and writes ASN.1 NULL for others as appropriate.
  result := EncodeWith(X509_ALGORITHM_IDENTIFIER, @alg,
    'CryptEncodeObjectEx(X509_ALGORITHM_IDENTIFIER)');
end;

// CMS SignerInfo.HashEncryptionAlgorithm OID lookup. The CMS field name is
// historical — it's the signature algorithm, not encryption. RSA reuses
// rsaEncryption regardless of digest; ECDSA uses the ecdsa-with-<hash>
// OIDs from RFC 5758.
function SignatureAlgOidFor(keyType : TCertKeyType; digest : THashAlgorithm) : AnsiString;
begin
  case keyType of
    cktEcdsa :
      case digest of
        haSha256 : result := szOID_ECDSA_SHA256;
        haSha384 : result := szOID_ECDSA_SHA384;
        haSha512 : result := szOID_ECDSA_SHA512;
      else
        raise ECryptoCms.CreateFmt(
          'No ECDSA signature OID known for digest %d', [Ord(digest)]);
      end;
  else
    // cktRsa or cktUnknown: rsaEncryption is the safe default and what
    // pre-existing RSA signing code shipped.
    result := szOID_RSA_RSA;
  end;
end;

// ---------------------------------------------------------------------------
// DER parsing helpers — paired with the DER encoding helpers above. Only
// what's needed to extract the [0] IMPLICIT signedAttrs from a SignerInfo
// for the sign-time diagnostic that compares (a) the digest the client
// sent to the remote signer against (b) the digest a verifier would
// recompute from the assembled CMS. If those differ we have a client-side
// encoding divergence; if they agree the bug is in transit / server.
// ---------------------------------------------------------------------------

type
  TDerElement = record
    Tag           : byte;     // raw ASN.1 tag octet (e.g. 0x30, 0x31, 0xA0)
    ContentStart  : integer;  // index of first content byte
    ContentLength : integer;  // length of content
    TotalLength   : integer;  // tag + length + content
  end;

function ParseDerTLV(const data : TBytes; offset : integer; out element : TDerElement) : boolean;
var
  lenByte : byte;
  i, numLen, len : integer;
begin
  result := false;
  if (offset < 0) or (offset + 2 > Length(data)) then
    exit;
  element.Tag := data[offset];
  lenByte := data[offset + 1];
  if lenByte < $80 then
  begin
    element.ContentStart := offset + 2;
    element.ContentLength := lenByte;
    element.TotalLength := 2 + lenByte;
  end
  else
  begin
    numLen := lenByte and $7F;
    if (numLen = 0) or (numLen > 4) or (offset + 2 + numLen > Length(data)) then
      exit;
    len := 0;
    for i := 0 to numLen - 1 do
      len := (len shl 8) or data[offset + 2 + i];
    element.ContentStart := offset + 2 + numLen;
    element.ContentLength := len;
    element.TotalLength := 2 + numLen + len;
  end;
  if element.ContentStart + element.ContentLength > Length(data) then
    exit;
  result := true;
end;

// Walk a CMS ContentInfo and return the byte range covering the SignerInfo's
// [0] IMPLICIT signedAttrs (tag 0xA0). Returns false if the structure isn't
// shaped how we expect.
function FindSignedAttrsRange(const cmsDer : TBytes; out attrsOffset, attrsTotalLen : integer) : boolean;
var
  contentInfo, ctx0, signedData, ver, digAlgs, encap, siSet, signerInfo : TDerElement;
  fieldStart : integer;
  field : TDerElement;
begin
  result := false;
  attrsOffset := 0;
  attrsTotalLen := 0;
  // Local records aren't auto-initialised in Delphi — explicitly zero siSet
  // so the post-loop tag check sees a sentinel (not stack garbage) on the
  // path where the signerInfos SET isn't found.
  FillChar(siSet, SizeOf(siSet), 0);
  // ContentInfo SEQUENCE { OID, [0] EXPLICIT SignedData }
  if not ParseDerTLV(cmsDer, 0, contentInfo) then exit;
  if contentInfo.Tag <> $30 then exit;
  // Skip the contentType OID inside ContentInfo
  if not ParseDerTLV(cmsDer, contentInfo.ContentStart, ver) then exit;
  // [0] EXPLICIT
  if not ParseDerTLV(cmsDer, ver.ContentStart + ver.ContentLength, ctx0) then exit;
  if ctx0.Tag <> $A0 then exit;
  // SignedData SEQUENCE
  if not ParseDerTLV(cmsDer, ctx0.ContentStart, signedData) then exit;
  if signedData.Tag <> $30 then exit;
  // SignedData fields: version, digestAlgs SET, encapContent, [certs[0]], [crls[1]], signerInfos SET
  fieldStart := signedData.ContentStart;
  // version INTEGER
  if not ParseDerTLV(cmsDer, fieldStart, ver) then exit;
  fieldStart := ver.ContentStart + ver.ContentLength;
  // digestAlgorithms SET (0x31)
  if not ParseDerTLV(cmsDer, fieldStart, digAlgs) then exit;
  fieldStart := digAlgs.ContentStart + digAlgs.ContentLength;
  // encapContentInfo SEQUENCE
  if not ParseDerTLV(cmsDer, fieldStart, encap) then exit;
  fieldStart := encap.ContentStart + encap.ContentLength;
  // Optional [0] certificates and [1] CRLs may appear; skip any context tags
  // until we find the signerInfos SET (0x31).
  while fieldStart < signedData.ContentStart + signedData.ContentLength do
  begin
    if not ParseDerTLV(cmsDer, fieldStart, field) then exit;
    if field.Tag = $31 then
    begin
      siSet := field;
      Break;
    end;
    if (field.Tag = $A0) or (field.Tag = $A1) then
    begin
      // certs[0] / crls[1] — skip
      fieldStart := field.ContentStart + field.ContentLength;
      Continue;
    end;
    // Unexpected element — bail
    exit;
  end;
  if siSet.Tag <> $31 then exit;
  // signerInfos SET → SignerInfo SEQUENCE
  if not ParseDerTLV(cmsDer, siSet.ContentStart, signerInfo) then exit;
  if signerInfo.Tag <> $30 then exit;
  // SignerInfo fields: version, sid (issuerAndSerial or [0] subjectKeyId),
  //   digestAlgorithm, [0] IMPLICIT signedAttrs?, signatureAlgorithm,
  //   signature, [1] IMPLICIT unsignedAttrs?
  fieldStart := signerInfo.ContentStart;
  // version
  if not ParseDerTLV(cmsDer, fieldStart, ver) then exit;
  fieldStart := ver.ContentStart + ver.ContentLength;
  // sid (SEQUENCE for issuerAndSerial, or [0] for SubjectKeyIdentifier)
  if not ParseDerTLV(cmsDer, fieldStart, field) then exit;
  fieldStart := field.ContentStart + field.ContentLength;
  // digestAlgorithm
  if not ParseDerTLV(cmsDer, fieldStart, field) then exit;
  fieldStart := field.ContentStart + field.ContentLength;
  // Next is either [0] IMPLICIT signedAttrs (0xA0) or signatureAlgorithm
  if not ParseDerTLV(cmsDer, fieldStart, field) then exit;
  if field.Tag <> $A0 then
    exit;  // No signedAttrs present — not our use case
  attrsOffset := fieldStart;
  attrsTotalLen := field.TotalLength;
  result := true;
end;

// Decode an ASN.1 OBJECT IDENTIFIER (already framed by tag 0x06) into the
// dotted-decimal text form. Returns '' if the bytes don't decode as an OID.
function DecodeOidText(const data : TBytes; offset, length : integer) : string;
var
  i, value : integer;
  first : boolean;
begin
  result := '';
  if length <= 0 then
    exit;
  // First two arcs are packed: byte0 = 40*arc1 + arc2
  value := data[offset];
  result := IntToStr(value div 40) + '.' + IntToStr(value mod 40);
  i := offset + 1;
  value := 0;
  first := true;
  while i < offset + length do
  begin
    if first then
    begin
      value := 0;
      first := false;
    end;
    value := (value shl 7) or (data[i] and $7F);
    if (data[i] and $80) = 0 then
    begin
      result := result + '.' + IntToStr(value);
      first := true;
    end;
    Inc(i);
  end;
end;

// Walk the just-assembled CMS to the SignerInfo's signatureAlgorithm OID
// (the AlgorithmIdentifier sitting right after the [0] IMPLICIT signedAttrs).
// Returns the dotted-decimal OID text, or '' if the structure isn't shaped
// how we expect. Used as a sign-time diagnostic to confirm Windows' encoder
// preserved the ecdsa-with-shaXXX OID we asked for, instead of writing the
// generic ECC OID 1.2.840.10045.2.1 (a known issue that breaks .NET
// SignedCms.CheckSignature — see dotnet/runtime#91168).
function FindSignatureAlgorithmOid(const cmsDer : TBytes) : string;
var
  attrsOffset, attrsTotalLen : integer;
  algSeq, oid : TDerElement;
begin
  result := '';
  if not FindSignedAttrsRange(cmsDer, attrsOffset, attrsTotalLen) then
    exit;
  // Next field after the [0] IMPLICIT signedAttrs is the signatureAlgorithm
  // AlgorithmIdentifier (SEQUENCE { OID, parameters? }).
  if not ParseDerTLV(cmsDer, attrsOffset + attrsTotalLen, algSeq) then
    exit;
  if algSeq.Tag <> $30 then
    exit;
  if not ParseDerTLV(cmsDer, algSeq.ContentStart, oid) then
    exit;
  if oid.Tag <> $06 then
    exit;
  result := DecodeOidText(cmsDer, oid.ContentStart, oid.ContentLength);
end;

// Compute the SHA-of-canonical-SignedAttrs-SET as a verifier would: take
// the [0] IMPLICIT bytes, replace the implicit tag (0xA0) with the SET tag
// (0x31), hash. The verifier does this internally; we mirror it here to
// catch encoding divergence at sign time.
function ComputeCanonicalSignedAttrsHash(const cmsDer : TBytes;
                                          const hashing : IHashingService;
                                          digest : THashAlgorithm) : TBytes;
var
  attrsOffset, attrsTotalLen : integer;
  canonical : TBytes;
begin
  SetLength(result, 0);
  if not FindSignedAttrsRange(cmsDer, attrsOffset, attrsTotalLen) then
    exit;
  // Copy the [0] IMPLICIT bytes verbatim, then overwrite the leading tag
  // with the SET tag 0x31. Length and content are identical between the
  // two forms (RFC 5652 §5.3, RFC 5652 §5.4 — signed attrs are hashed as
  // a SET OF Attribute regardless of how they're tagged on the wire).
  SetLength(canonical, attrsTotalLen);
  Move(cmsDer[attrsOffset], canonical[0], attrsTotalLen);
  canonical[0] := $31;
  result := hashing.HashBytes(canonical, digest);
end;

// DER SET ordering rule: elements must be sorted byte-wise ascending of
// their encoded form. CompareDerBytes is the lexicographic predicate.
function CompareDerBytes(const a, b : TBytes) : integer;
var
  i, minLen : integer;
begin
  minLen := Length(a);
  if Length(b) < minLen then
    minLen := Length(b);
  for i := 0 to minLen - 1 do
  begin
    if a[i] < b[i] then exit(-1);
    if a[i] > b[i] then exit(1);
  end;
  if Length(a) < Length(b) then exit(-1);
  if Length(a) > Length(b) then exit(1);
  result := 0;
end;

procedure SortDerBytes(var items : array of TBytes);
var
  i, j : integer;
  tmp : TBytes;
begin
  // Insertion sort — N is tiny (3..5 attributes typically).
  for i := 1 to High(items) do
  begin
    tmp := items[i];
    j := i - 1;
    while (j >= 0) and (CompareDerBytes(items[j], tmp) > 0) do
    begin
      items[j + 1] := items[j];
      Dec(j);
    end;
    items[j + 1] := tmp;
  end;
end;

function TCmsService.SignRemote(const content : TBytes;
                                const provider : ISigningProvider;
                                const signedAttributes : TCmsAttributes;
                                digest : THashAlgorithm) : TBytes;
var
  messageDigest : TBytes;
  encodedAttrs : array of TBytes;
  attrsSorted : array of TBytes;
  signedAttrsSetContent : TBytes;
  signedAttrsSetDer : TBytes;
  attrsHash : TBytes;
  signatureBytes : TBytes;
  cert : ICertificate;
  certInfo : PCERT_INFO;
  certDer : TBytes;
  authAttrsArr : array of CRYPT_ATTRIBUTE;
  authAttrValues : array of CRYPT_INTEGER_BLOB;
  authAttrPayloads : array of TBytes;
  signerInfo : CMSG_SIGNER_INFO;
  signerInfoDer : TBytes;
  // Both OIDs are pulled into local AnsiStrings BEFORE being aliased into
  // signerInfo via PAnsiChar(...) — otherwise the cast captures a pointer
  // into a function-return temporary that's freed at end-of-statement,
  // leaving signerInfo.*.pszObjId dangling when CryptEncodeObjectEx reads
  // it a few lines later. That corrupts the heap and crashes much later
  // (typically inside an unrelated BCrypt allocation, status c0000374).
  hashAlgOid : AnsiString;
  sigAlgOid : AnsiString;
  digestAlgDer, sigAlgDer : TBytes;
  digestAlgsSetDer : TBytes;
  encapContentInfoDer : TBytes;
  embeddedHash : TBytes;
  embeddedSig : TBytes;
  embeddedSigAlgOid : string;
  encapContentTypeOidValue : TBytes;
  certificatesField : TBytes;
  signerInfosSetDer : TBytes;
  signedDataInner : TBytes;
  contentInfo : CRYPT_CONTENT_INFO;
  contentBlob : CRYPT_INTEGER_BLOB;
  i, n, idx : integer;
begin
  FLogger.Verbose('  SignRemote: resolving signer certificate');
  cert := provider.Certificate;
  if cert = nil then
    raise ECryptoCms.Create('SignRemote: provider has no certificate');

  // 1. Compute message digest of the content (the manifest, for detached).
  FLogger.Verbose(Format('  SignRemote: hashing content (%d bytes, %s)',
    [Length(content), TAlgorithmProfile.HashAlgorithmName(digest)]));
  messageDigest := FHashing.HashBytes(content, digest);

  // 2. Build the full signed-attribute set: contentType + messageDigest +
  //    every caller-supplied attribute. These end up both (a) encoded as
  //    a SET for the digest input, and (b) carried in the SignerInfo's
  //    [0] IMPLICIT signedAttrs.
  n := Length(signedAttributes) + 2;
  SetLength(authAttrsArr, n);
  SetLength(authAttrValues, n);
  SetLength(authAttrPayloads, n);

  // contentType = id-data (detached: encapContentInfo carries id-data with no eContent)
  encapContentTypeOidValue := EncodeOidValue(szOID_RSA_data);
  authAttrPayloads[0] := encapContentTypeOidValue;
  authAttrValues[0].cbData := Length(authAttrPayloads[0]);
  authAttrValues[0].pbData := @authAttrPayloads[0][0];
  authAttrsArr[0].pszObjId := PAnsiChar(szOID_RSA_contentType);
  authAttrsArr[0].cValue := 1;
  authAttrsArr[0].rgValue := @authAttrValues[0];

  // messageDigest = OCTET STRING(hash-of-content)
  authAttrPayloads[1] := EncodeOctetString(messageDigest);
  authAttrValues[1].cbData := Length(authAttrPayloads[1]);
  authAttrValues[1].pbData := @authAttrPayloads[1][0];
  authAttrsArr[1].pszObjId := PAnsiChar(szOID_RSA_messageDigest);
  authAttrsArr[1].cValue := 1;
  authAttrsArr[1].rgValue := @authAttrValues[1];

  // Caller-supplied attrs (each value wrapped in OCTET STRING, matching the
  // local-sign path so the verifier sees them identically).
  for i := 0 to High(signedAttributes) do
  begin
    idx := 2 + i;
    authAttrPayloads[idx] := EncodeOctetString(signedAttributes[i].Value);
    authAttrValues[idx].cbData := Length(authAttrPayloads[idx]);
    authAttrValues[idx].pbData := @authAttrPayloads[idx][0];
    authAttrsArr[idx].pszObjId := PAnsiChar(signedAttributes[i].Oid);
    authAttrsArr[idx].cValue := 1;
    authAttrsArr[idx].rgValue := @authAttrValues[idx];
  end;

  // 3. Encode each attribute on its own, sort byte-wise (DER SET rule),
  //    concatenate, and wrap in a SET tag. This is what the digest covers.
  SetLength(encodedAttrs, n);
  for i := 0 to n - 1 do
    encodedAttrs[i] := EncodeAttributeRaw(
      AnsiString(authAttrsArr[i].pszObjId),
      // value bytes: each rgValue[0] is a CRYPT_INTEGER_BLOB pointing at the
      // already-DER value payload — wrap-once via PKCS_ATTRIBUTE encoder.
      // We re-encode here rather than reuse Windows' attribute encoding for
      // the array form because PKCS_ATTRIBUTE encodes one attr at a time.
      Copy(authAttrPayloads[i], 0, Length(authAttrPayloads[i])));

  FLogger.Verbose(Format('  SignRemote: encoded %d signed attributes; sorting + SET-wrapping', [n]));
  SetLength(attrsSorted, n);
  for i := 0 to n - 1 do
    attrsSorted[i] := encodedAttrs[i];
  SortDerBytes(attrsSorted);

  signedAttrsSetContent := ConcatBytes(attrsSorted);
  signedAttrsSetDer := DerSet(signedAttrsSetContent);
  FLogger.Verbose(Format('  SignRemote: SignedAttrs SET = %d bytes', [Length(signedAttrsSetDer)]));

  // 4. Hash the SET-encoded signed attrs. That hash is what the remote
  //    signer signs over.
  attrsHash := FHashing.HashBytes(signedAttrsSetDer, digest);
  FLogger.Verbose(Format('  SignRemote: SignedAttrs digest = %d bytes; calling provider.SignDigest', [Length(attrsHash)]));
  // Diagnostic — full hex of the digest that's about to be sent over the
  // wire. When post-sign verify fails, the user can correlate this against
  // (a) the digest the server logged for the same request, and (b) the
  // SHA-of-canonical-SignedAttrs reconstructed from the assembled CMS, to
  // pinpoint whether the bad signature came from a client encoding bug, an
  // HTTP-transport corruption, or a server signing bug. Pretty cheap (one
  // hex line, only at Verbose).
  FLogger.Verbose('  SignRemote: digest hex = ' + BytesToHex(attrsHash));

  // 5. Single remote call — provider.SignDigest returns the raw signature
  //    value (RSA PKCS#1 v1.5 over the digest, or ECDSA equivalent).
  signatureBytes := provider.SignDigest(attrsHash, digest);
  if Length(signatureBytes) = 0 then
    raise ECryptoCms.Create('SignRemote: provider returned empty signature');
  FLogger.Verbose(Format('  SignRemote: signature returned = %d bytes', [Length(signatureBytes)]));
  FLogger.Verbose('  SignRemote: signature hex = ' + BytesToHex(signatureBytes));

  // 6. Build CMSG_SIGNER_INFO and let Windows encode the SignerInfo
  //    structure for us (PKCS7_SIGNER_INFO). The encoder takes the same
  //    CRYPT_ATTRIBUTE array we used for hashing and writes it out as
  //    [0] IMPLICIT signedAttrs — bit-identical content, different tag.
  certInfo := cert.GetContext.pCertInfo;
  FillChar(signerInfo, SizeOf(signerInfo), 0);
  signerInfo.dwVersion := 1;   // IssuerAndSerial form
  signerInfo.Issuer := certInfo.Issuer;
  signerInfo.SerialNumber := certInfo.SerialNumber;
  // Stash both OIDs in locals first; see the comment near the var block —
  // PAnsiChar(<function-return AnsiString>) leaves a dangling pointer.
  hashAlgOid := TAlgorithmProfile.HashOid(digest);
  signerInfo.HashAlgorithm.pszObjId := PAnsiChar(hashAlgOid);
  // CMS "HashEncryptionAlgorithm" is really the *signature* algorithm. RSA
  // uses the rsaEncryption OID for any hash; ECDSA uses ecdsa-with-<hash>
  // OIDs (RFC 5758). Mismatching this against the actual signature bytes
  // makes verifiers reject the signature outright.
  sigAlgOid := SignatureAlgOidFor(cert.PublicKeyType, digest);
  signerInfo.HashEncryptionAlgorithm.pszObjId := PAnsiChar(sigAlgOid);
  signerInfo.EncryptedHash.cbData := Length(signatureBytes);
  signerInfo.EncryptedHash.pbData := @signatureBytes[0];
  signerInfo.AuthAttrs.cAttr := n;
  signerInfo.AuthAttrs.rgAttr := @authAttrsArr[0];
  signerInfo.UnauthAttrs.cAttr := 0;
  signerInfo.UnauthAttrs.rgAttr := nil;

  signerInfoDer := EncodeWith(PKCS7_SIGNER_INFO, @signerInfo, 'CryptEncodeObjectEx(PKCS7_SIGNER_INFO)');

  // 7. Hand-assemble the SignedData SEQUENCE.
  //
  //   SignedData ::= SEQUENCE {
  //       version           INTEGER,                   -- 1
  //       digestAlgorithms  SET OF AlgorithmIdentifier,
  //       encapContentInfo  EncapsulatedContentInfo,   -- {id-data} (detached)
  //       certificates  [0] IMPLICIT CertificateSet,
  //       signerInfos       SET OF SignerInfo
  //   }
  digestAlgDer := EncodeAlgorithmIdentifier(TAlgorithmProfile.HashOid(digest));
  digestAlgsSetDer := DerSet(digestAlgDer);

  sigAlgDer := EncodeAlgorithmIdentifier(szOID_RSA_RSA);   // not embedded directly; computed for symmetry / future use
  // sigAlgDer is currently unused at the SignedData level — the algorithm
  // identifier travels with the SignerInfo. Suppress "unused" via reference:
  if Length(sigAlgDer) = 0 then ; // intentionally noop

  // EncapsulatedContentInfo for a *detached* signature: SEQUENCE { id-data }
  // (no [0] EXPLICIT eContent OCTET STRING).
  encapContentInfoDer := DerSequence(EncodeOidValue(szOID_RSA_data));

  // certificates [0] IMPLICIT — exactly one cert in our bag.
  certDer := cert.RawDerBytes;
  certificatesField := DerContext0Implicit(certDer);

  signerInfosSetDer := DerSet(signerInfoDer);

  signedDataInner := DerSequence(ConcatBytes([
    DerInteger(1),               // version
    digestAlgsSetDer,
    encapContentInfoDer,
    certificatesField,
    signerInfosSetDer
  ]));

  // 8. Wrap in ContentInfo via Windows' helper. CryptEncodeObjectEx wraps
  //    in: SEQUENCE { contentType, [0] EXPLICIT content }.
  contentInfo.pszObjId := PAnsiChar(szOID_RSA_signedData);
  contentBlob.cbData := Length(signedDataInner);
  contentBlob.pbData := @signedDataInner[0];
  contentInfo.Content := contentBlob;

  result := EncodeWith(PKCS_CONTENT_INFO, @contentInfo,
    'CryptEncodeObjectEx(PKCS_CONTENT_INFO)');

  // Diagnostic — recover the canonical SignedAttrs hash a verifier would
  // recompute from the assembled CMS, and compare against the digest we
  // actually sent to the remote signer. We've seen post-sign verify failures
  // where the messageDigest attribute is correct and the remote signer's
  // own verify passes, but the CMS still fails to verify locally — this
  // check distinguishes the two possible causes:
  //   - hashes match → our encoded SignedAttrs match what Windows embedded,
  //                    so the signature bytes themselves are wrong / corrupt
  //                    (HTTP transport, server bug, etc.)
  //   - hashes differ → Windows' PKCS7_SIGNER_INFO encoder produced different
  //                     bytes than our manual signedAttrsSetDer construction,
  //                     i.e. a client-side encoding divergence.
  // Cheap to run on every sign: one DER walk + one hash over ~120 bytes.
  embeddedHash := ComputeCanonicalSignedAttrsHash(result, FHashing, digest);
  if Length(embeddedHash) = 0 then
    FLogger.Verbose('  SignRemote: could not extract embedded SignedAttrs for self-check')
  else if BytesEqual(embeddedHash, attrsHash) then
    FLogger.Verbose('  SignRemote: embedded SignedAttrs hash matches sent digest (OK)')
  else
  begin
    FLogger.Error(
      '  SignRemote: ENCODING DIVERGENCE — the SignedAttrs hash a verifier ' +
      'will recompute from the assembled CMS does NOT match the digest we ' +
      'sent to the remote signer. The remote signer signed our digest ' +
      'correctly, but the bytes Windows embedded in the CMS hash to a ' +
      'different value, so the signature won''t verify.');
    FLogger.Error('  SignRemote: sent digest       = ' + BytesToHex(attrsHash));
    FLogger.Error('  SignRemote: embedded SET hash = ' + BytesToHex(embeddedHash));
  end;

  // Second self-check: confirm the signature value Windows' PKCS7_SIGNER_INFO
  // encoder put into the CMS's encryptedDigest is byte-identical to what the
  // remote signer returned. If they differ, the encoder corrupted the
  // signature on the way in — distinct from the encoding-divergence case
  // above. The two checks together fully cover the "received signature →
  // embedded CMS" hop: if both pass and verify still fails, the bug is
  // beyond DPM's control (the signature genuinely doesn't match the digest
  // under this key per Windows' verifier, even though server-side ECDsa
  // verify accepted it — typically signatureAlgorithm OID mismatches or
  // CryptoAPI ECDSA strictness peculiarities, see ExtractEncryptedDigest
  // diagnostic in SignPackage).
  embeddedSig := ExtractEncryptedDigest(result);
  if Length(embeddedSig) = 0 then
    FLogger.Verbose('  SignRemote: could not extract embedded signature value for self-check')
  else if BytesEqual(embeddedSig, signatureBytes) then
    FLogger.Verbose('  SignRemote: embedded signature value matches received signature (OK)')
  else
  begin
    FLogger.Error(
      '  SignRemote: SIGNATURE CORRUPTION — Windows'' PKCS7_SIGNER_INFO ' +
      'encoder embedded a different signature value than the one returned ' +
      'by the remote signer.');
    FLogger.Error('  SignRemote: received sig = ' + BytesToHex(signatureBytes));
    FLogger.Error('  SignRemote: embedded sig = ' + BytesToHex(embeddedSig));
  end;

  // Third self-check: log the signatureAlgorithm OID Windows wrote into the
  // CMS, so we can spot the dotnet/runtime#91168-class issue where Windows'
  // PKCS7_SIGNER_INFO encoder substitutes the generic ECC OID 1.2.840.10045.2.1
  // for the specific ecdsa-with-shaXXX OID we asked for. Verifiers that key
  // off the signatureAlgorithm OID to pick the hash function then fail with
  // a confusing "digest algorithm not valid for signature algorithm" error.
  embeddedSigAlgOid := FindSignatureAlgorithmOid(result);
  if embeddedSigAlgOid = '' then
    FLogger.Verbose('  SignRemote: could not extract embedded signatureAlgorithm OID')
  else
    FLogger.Verbose('  SignRemote: embedded signatureAlgorithm OID = ' + embeddedSigAlgOid +
                    ' (expected ' + string(sigAlgOid) + ')');
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
  hashAlgOid : AnsiString;
begin
  if not TAlgorithmProfile.CmsDigestAllowed(digest) then
    raise ECryptoCms.CreateFmt('Digest %s not permitted', [TAlgorithmProfile.HashAlgorithmName(digest)]);
  if provider = nil then
    raise ECryptoCms.Create('Sign: provider is nil');
  if provider.Certificate = nil then
    raise ECryptoCms.Create('Sign: provider has no certificate');
  // P3 §3.3 v2 — remote providers don't expose a Win32 key handle, so
  // CryptSignMessage can't sign on their behalf. Branch into the manual
  // CMS-assembly path that does the single remote SignDigest call.
  if not provider.IsLocal then
  begin
    result := SignRemote(content, provider, signedAttributes, digest);
    exit;
  end;

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
  // Pin the OID string in a local AnsiString so PAnsiChar(...) doesn't
  // capture a function-return temporary that gets freed before
  // CryptSignMessage reads it (heap-corruption trap — see SignRemote).
  hashAlgOid := TAlgorithmProfile.HashOid(digest);
  para.HashAlgorithm.pszObjId := PAnsiChar(hashAlgOid);
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

function TCmsService.ExtractEncryptedDigest(const der : TBytes) : TBytes;
var
  msg : HCRYPTMSG;
  size : DWORD;
begin
  // Minimal decode: open the message, feed the bytes, fetch CMSG_ENCRYPTED_
  // DIGEST, close. No pointer-chasing into signer-info / attribute arrays —
  // avoids AVs on hand-assembled CMS that Windows can still parse far
  // enough to expose the signature value but trips up on deeper traversal.
  SetLength(result, 0);
  if Length(der) = 0 then
    exit;
  msg := CryptMsgOpenToDecode(cENCODING_TYPE, 0, 0, 0, nil, nil);
  if msg = nil then
    raise ECryptoCms.CreateFmt('CryptMsgOpenToDecode failed: 0x%.8x', [GetLastError]);
  try
    CheckBool(CryptMsgUpdate(msg, PByte(@der[0]), Length(der), True),
      'CryptMsgUpdate(ExtractEncryptedDigest)');
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
  FLogger.Verbose(Format('    AddUnsignedAttribute: opening message decoder (cms=%d bytes, attr=%d bytes)',
    [Length(der), Length(value)]));
  msg := CryptMsgOpenToDecode(cENCODING_TYPE, 0, 0, 0, nil, nil);
  if msg = nil then
    raise ECryptoCms.CreateFmt('CryptMsgOpenToDecode failed: %d', [GetLastError]);
  try
    FLogger.Verbose('    AddUnsignedAttribute: feeding existing CMS to decoder');
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
    FLogger.Verbose('    AddUnsignedAttribute: encoding PKCS_ATTRIBUTE');
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

      FLogger.Verbose(Format('    AddUnsignedAttribute: CryptMsgControl(ADD_UNAUTH_ATTR) with %d-byte blob',
        [encodedAttrSize]));
      CheckBool(CryptMsgControl(msg, 0, CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR, @para),
        'CryptMsgControl(ADD_UNAUTH_ATTR)');
    finally
      LocalFree(HLOCAL(encodedAttr));
    end;

    // Read the rewritten CMS bytes back out.
    encodedSize := 0;
    FLogger.Verbose('    AddUnsignedAttribute: re-reading encoded message');
    CheckBool(CryptMsgGetParam(msg, 29 {CMSG_ENCODED_MESSAGE}, 0, nil, encodedSize), 'CryptMsgGetParam(ENCODED_MESSAGE size)');
    SetLength(newDer, encodedSize);
    CheckBool(CryptMsgGetParam(msg, 29 {CMSG_ENCODED_MESSAGE}, 0, @newDer[0], encodedSize), 'CryptMsgGetParam(ENCODED_MESSAGE)');
    SetLength(newDer, encodedSize);
    der := newDer;
    FLogger.Verbose(Format('    AddUnsignedAttribute: done (new CMS = %d bytes)', [encodedSize]));
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
  i, j : integer;
  attr : PCRYPT_ATTRIBUTE;
  blob : PCRYPT_INTEGER_BLOB;
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
    // CRYPT_ATTRIBUTE.rgValue points at a contiguous array of cValue
    // CRYPT_ATTR_BLOB structures. Walk the whole array — most DPM
    // attributes are single-valued, but dpmVerifiedAuthorSigHash carries
    // one hash value per attested author signature.
    SetLength(attrs[i].Values, attr.cValue);
    for j := 0 to Integer(attr.cValue) - 1 do
    begin
      blob := PCRYPT_INTEGER_BLOB(NativeUInt(attr.rgValue) +
                                  NativeUInt(j) * SizeOf(CRYPT_INTEGER_BLOB));
      if blob.cbData > 0 then
      begin
        SetLength(attrs[i].Values[j], blob.cbData);
        Move(blob.pbData^, attrs[i].Values[j][0], blob.cbData);
      end;
    end;
    // Convenience shortcut for the common single-value case.
    if Length(attrs[i].Values) > 0 then
      attrs[i].Value := attrs[i].Values[0];
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

function TCmsSignedData.FindSignedAttributeValues(const oid : AnsiString;
                                                   out values : TArray<TBytes>) : boolean;
var
  i : integer;
begin
  for i := 0 to High(FSignedAttrs) do
    if FSignedAttrs[i].Oid = oid then
    begin
      values := FSignedAttrs[i].Values;
      result := true;
      exit;
    end;
  values := nil;
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
