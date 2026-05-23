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

unit DPM.Core.Crypto.X509;

// X509 certificate and chain wrappers around Win32 crypt32.dll. All
// CERT_CONTEXT / HCERTSTORE / CERT_CHAIN_CONTEXT lifetime is owned by these
// interface-based wrappers.

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.X509.Interfaces;

type
  TX509Service = class(TInterfacedObject, IX509Service)
  private
    FHashingService : IHashingService;
  protected
    function OpenSystemStore(location : TCertStoreLocation; const storeName : string) : ICertificateStore;
    function OpenPfxStore(const pfxBytes : TBytes; const password : string) : ICertificateStore;
    function LoadCertificateFromDer(const der : TBytes) : ICertificate;
    function CreateChain : ICertificateChain;
  public
    constructor Create(const hashingService : IHashingService);
  end;

implementation

uses
  System.DateUtils;

const
  cCERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
  cCERT_X500_NAME_STR            = 3;
  cCERT_SIMPLE_NAME_STR          = 1;

  cCERT_SHA1_HASH_PROP_ID        = 3;

  cCRYPT_ASN_ENCODING            = X509_ASN_ENCODING;
  cENCODING_TYPE                 = X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;

type
  TCertificate = class(TInterfacedObject, ICertificate)
  private
    FContext : PCCERT_CONTEXT;
    FOwnsContext : boolean;
    FHashingService : IHashingService;
    function GetCertInfo : PCERT_INFO;
    function NameBlobToString(const blob : CERT_NAME_BLOB; strType : DWORD) : string;
    function ExtractCommonName(const dn : string) : string;
    function FindExtension(const oid : AnsiString) : PCERT_EXTENSION;
  protected
    function SubjectCommonName : string;
    function SubjectDistinguishedName : string;
    function IssuerDistinguishedName : string;
    function Thumbprint : string;
    function SerialNumberHex : string;
    function SpkiHash(algorithm : THashAlgorithm) : TBytes;
    function NotBefore : TDateTime;
    function NotAfter : TDateTime;
    function HasCodeSigningEku : boolean;
    function RawDerBytes : TBytes;
    function PublicKeyType : TCertKeyType;
    function PreferredDigest : THashAlgorithm;
    function GetContext : PCCERT_CONTEXT;
  public
    constructor Create(context : PCCERT_CONTEXT; ownsContext : boolean; const hashingService : IHashingService);
    destructor Destroy; override;
  end;

  TCertificateStore = class(TInterfacedObject, ICertificateStore)
  private
    FHandle : HCERTSTORE;
    FHashingService : IHashingService;
  protected
    function FindByThumbprint(const thumbprint : string) : ICertificate;
    function FindBySpki(const spkiHash : TBytes; algorithm : THashAlgorithm) : ICertificate;
    function GetHandle : HCERTSTORE;
  public
    constructor Create(handle : HCERTSTORE; const hashingService : IHashingService);
    destructor Destroy; override;
  end;

  TCertificateChain = class(TInterfacedObject, ICertificateChain)
  private
    FChain : PCERT_CHAIN_CONTEXT;
    FLeaf  : ICertificate;
    FAdditionalStore : HCERTSTORE;
    FLastError : string;
    FHashingService : IHashingService;
    FRevocationStatus : TRevocationStatus;
    FRevocationReason : TRevocationReason;
    procedure CloseHandles;
    procedure CaptureLeafRevocationDetail;
  protected
    function Build(const cert : ICertificate;
                   const additionalCerts : array of ICertificate) : TChainResult;
    function BuildAtTime(const cert : ICertificate;
                         const additionalCerts : array of ICertificate;
                         asOfTime : TDateTime;
                         checkRevocation : boolean) : TChainResult;
    function VerifyForCodeSigning(asOfTime : TDateTime) : TChainResult;
    function ChainCertificates : TArray<ICertificate>;
    function RootCertificate : ICertificate;
    function LastErrorMessage : string;
    function RevocationStatus : TRevocationStatus;
    function RevocationReason : TRevocationReason;
  public
    constructor Create(const hashingService : IHashingService);
    destructor Destroy; override;
  end;

function HexEncode(const bytes : TBytes) : string;
const
  cHex : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  i : integer;
  p : PChar;
begin
  SetLength(result, Length(bytes) * 2);
  if Length(bytes) = 0 then
    exit;
  p := PChar(result);
  for i := 0 to Length(bytes) - 1 do
  begin
    p^ := cHex[bytes[i] shr 4]; Inc(p);
    p^ := cHex[bytes[i] and $0F]; Inc(p);
  end;
end;

function FileTimeToUtcDateTime(const ft : TFileTime) : TDateTime;
var
  st : TSystemTime;
begin
  if FileTimeToSystemTime(ft, st) then
    result := SystemTimeToDateTime(st)
  else
    result := 0;
end;

function UtcDateTimeToFileTime(const value : TDateTime) : TFileTime;
var
  st : TSystemTime;
  localFt : TFileTime;
begin
  DateTimeToSystemTime(value, st);
  if not SystemTimeToFileTime(st, localFt) then
    raise ECryptoX509.Create('SystemTimeToFileTime failed');
  // The caller-provided value is treated as UTC, so localFt is already UTC.
  result := localFt;
end;

{ TCertificate }

constructor TCertificate.Create(context : PCCERT_CONTEXT; ownsContext : boolean;
                                const hashingService : IHashingService);
begin
  if context = nil then
    raise ECryptoX509.Create('TCertificate.Create: context is nil');
  inherited Create;
  FContext := context;
  FOwnsContext := ownsContext;
  FHashingService := hashingService;
end;

destructor TCertificate.Destroy;
begin
  if FOwnsContext and (FContext <> nil) then
    CertFreeCertificateContext(FContext);
  FContext := nil;
  inherited;
end;

function TCertificate.GetCertInfo : PCERT_INFO;
begin
  result := FContext.pCertInfo;
end;

function TCertificate.GetContext : PCCERT_CONTEXT;
begin
  result := FContext;
end;

function TCertificate.NameBlobToString(const blob : CERT_NAME_BLOB; strType : DWORD) : string;
var
  needed : DWORD;
  buf : array of WideChar;
begin
  needed := CertNameToStrW(cCRYPT_ASN_ENCODING, @blob, strType, nil, 0);
  if needed = 0 then
  begin
    result := '';
    exit;
  end;
  SetLength(buf, needed);
  CertNameToStrW(cCRYPT_ASN_ENCODING, @blob, strType, @buf[0], needed);
  // Strip the terminating NUL.
  if (needed > 0) and (buf[needed - 1] = #0) then
    SetString(result, PWideChar(@buf[0]), needed - 1)
  else
    SetString(result, PWideChar(@buf[0]), needed);
end;

function TCertificate.ExtractCommonName(const dn : string) : string;
const
  cCnPrefix = 'CN=';
var
  startPos : integer;
  endPos : integer;
  inQuotes : boolean;
  c : Char;
begin
  // CERT_SIMPLE_NAME_STR may already give us the CN — fall back to a manual
  // sweep through the DN for cases where it does not.
  result := '';
  startPos := Pos(cCnPrefix, dn);
  if startPos = 0 then
    exit;
  Inc(startPos, Length(cCnPrefix));
  inQuotes := false;
  endPos := startPos;
  while endPos <= Length(dn) do
  begin
    c := dn[endPos];
    if c = '"' then
      inQuotes := not inQuotes
    else if (c = ',') and not inQuotes then
      Break;
    Inc(endPos);
  end;
  result := Trim(Copy(dn, startPos, endPos - startPos));
end;

function TCertificate.SubjectDistinguishedName : string;
begin
  result := NameBlobToString(GetCertInfo.Subject, cCERT_X500_NAME_STR);
end;

function TCertificate.IssuerDistinguishedName : string;
begin
  result := NameBlobToString(GetCertInfo.Issuer, cCERT_X500_NAME_STR);
end;

function TCertificate.SubjectCommonName : string;
var
  simple : string;
begin
  simple := NameBlobToString(GetCertInfo.Subject, cCERT_SIMPLE_NAME_STR);
  if simple <> '' then
  begin
    result := simple;
    exit;
  end;
  result := ExtractCommonName(SubjectDistinguishedName);
end;

function TCertificate.Thumbprint : string;
var
  size : DWORD;
  buf : TBytes;
begin
  size := 0;
  if not CertGetCertificateContextProperty(FContext, cCERT_SHA1_HASH_PROP_ID, nil, size) or (size = 0) then
  begin
    result := '';
    exit;
  end;
  SetLength(buf, size);
  if not CertGetCertificateContextProperty(FContext, cCERT_SHA1_HASH_PROP_ID, @buf[0], size) then
  begin
    result := '';
    exit;
  end;
  result := HexEncode(buf);
end;

function TCertificate.SerialNumberHex : string;
var
  info : PCERT_INFO;
  buf : TBytes;
  i : integer;
begin
  info := GetCertInfo;
  // The serial number is stored little-endian; reverse for the conventional
  // big-endian display order.
  SetLength(buf, info.SerialNumber.cbData);
  for i := 0 to Integer(info.SerialNumber.cbData) - 1 do
    buf[i] := (info.SerialNumber.pbData + (Integer(info.SerialNumber.cbData) - 1 - i))^;
  result := HexEncode(buf);
end;

function TCertificate.SpkiHash(algorithm : THashAlgorithm) : TBytes;
var
  info : PCERT_INFO;
  encodedSize : DWORD;
  encoded : Pointer;
  bytes : TBytes;
begin
  info := GetCertInfo;
  encoded := nil;
  encodedSize := 0;

  // Re-encode the SubjectPublicKeyInfo using the OS — never parse ASN.1 by
  // hand. The CRYPT_ENCODE_ALLOC_FLAG makes CryptEncodeObjectEx allocate
  // the output buffer; we free it with LocalFree.
  if not CryptEncodeObjectEx(
       cCRYPT_ASN_ENCODING,
       X509_PUBLIC_KEY_INFO,
       @info.SubjectPublicKeyInfo,
       CRYPT_ENCODE_ALLOC_FLAG,
       nil,
       @encoded,
       encodedSize) then
    raise ECryptoX509.CreateFmt('CryptEncodeObjectEx(SubjectPublicKeyInfo) failed: %d', [GetLastError]);

  try
    SetLength(bytes, encodedSize);
    if encodedSize > 0 then
      Move(encoded^, bytes[0], encodedSize);
    result := FHashingService.HashBytes(bytes, algorithm);
  finally
    if encoded <> nil then
      LocalFree(HLOCAL(encoded));
  end;
end;

function TCertificate.NotBefore : TDateTime;
begin
  result := FileTimeToUtcDateTime(GetCertInfo.NotBefore);
end;

function TCertificate.NotAfter : TDateTime;
begin
  result := FileTimeToUtcDateTime(GetCertInfo.NotAfter);
end;

function TCertificate.FindExtension(const oid : AnsiString) : PCERT_EXTENSION;
var
  info : PCERT_INFO;
  i : integer;
  ext : PCERT_EXTENSION;
begin
  info := GetCertInfo;
  ext := info.rgExtension;
  for i := 0 to Integer(info.cExtension) - 1 do
  begin
    if AnsiString(ext.pszObjId) = oid then
    begin
      result := ext;
      exit;
    end;
    Inc(ext);
  end;
  result := nil;
end;

function TCertificate.HasCodeSigningEku : boolean;
var
  ext : PCERT_EXTENSION;
  decoded : Pointer;
  decodedSize : DWORD;
  usage : PCERT_ENHKEY_USAGE;
  i : integer;
  oid : AnsiString;
begin
  result := false;
  ext := FindExtension('2.5.29.37');
  if ext = nil then
  begin
    // No EKU extension at all means the certificate is implicitly valid for
    // any usage. Chain policy enforcement decides at validation time; for
    // display purposes we report "true" to avoid false-negative UI.
    result := true;
    exit;
  end;

  decoded := nil;
  decodedSize := 0;
  if not CryptDecodeObjectEx(cCRYPT_ASN_ENCODING, X509_ENHANCED_KEY_USAGE,
      ext.Value.pbData, ext.Value.cbData,
      CRYPT_DECODE_ALLOC_FLAG, nil, @decoded, decodedSize) then
    exit;

  try
    usage := PCERT_ENHKEY_USAGE(decoded);
    for i := 0 to Integer(usage.cUsageIdentifier) - 1 do
    begin
      oid := AnsiString(PPAnsiChar(NativeUInt(usage.rgpszUsageIdentifier) + NativeUInt(i * SizeOf(Pointer)))^);
      if oid = szOID_PKIX_KP_CODE_SIGNING then
      begin
        result := true;
        exit;
      end;
    end;
  finally
    LocalFree(HLOCAL(decoded));
  end;
end;

function TCertificate.RawDerBytes : TBytes;
begin
  SetLength(result, FContext.cbCertEncoded);
  if FContext.cbCertEncoded > 0 then
    Move(FContext.pbCertEncoded^, result[0], FContext.cbCertEncoded);
end;

function TCertificate.PublicKeyType : TCertKeyType;
var
  info : PCERT_INFO;
  algOid : AnsiString;
begin
  result := cktUnknown;
  info := GetCertInfo;
  if info = nil then
    exit;
  algOid := AnsiString(string(info.SubjectPublicKeyInfo.Algorithm.pszObjId));
  if algOid = szOID_RSA_RSA then
    result := cktRsa
  else if algOid = szOID_ECC_PUBLIC_KEY then
    result := cktEcdsa;
end;

function TCertificate.PreferredDigest : THashAlgorithm;
var
  info : PCERT_INFO;
  params : CRYPT_OBJID_BLOB;
  oidLen : byte;
  oidBytes : TBytes;
  i : integer;

  function CurveOidEquals(const expected : AnsiString) : boolean;
  var
    // Raw Pointer (NOT TBytes). CryptEncodeObjectEx with CRYPT_ENCODE_ALLOC_FLAG
    // writes back a LocalAlloc'd pointer; using a Delphi-managed TBytes here
    // makes the RTL read/write Delphi dynamic-array header bytes (refcount,
    // length) from inside the LocalAlloc heap block — that corrupts the heap
    // and crashes a later allocator with STATUS_HEAP_CORRUPTION (c0000374).
    expectedBuf : Pointer;
    expectedLen : DWORD;
    j : integer;
    p : PAnsiChar;
  begin
    // Encode the expected OID via the OS so we don't hand-encode dotted-OID
    // -> DER. expected -> X509_OBJECT_IDENTIFIER -> { 06 LL bb bb bb ... }.
    expectedBuf := nil;
    expectedLen := 0;
    p := PAnsiChar(expected);
    if not CryptEncodeObjectEx(cENCODING_TYPE, X509_OBJECT_IDENTIFIER, @p,
             CRYPT_ENCODE_ALLOC_FLAG, nil, @expectedBuf, expectedLen) then
    begin
      result := false;
      exit;
    end;
    try
      // Skip the tag (06) and length byte to get just the OID body bytes.
      if (expectedLen < 2) or (Length(oidBytes) <> Integer(expectedLen) - 2) then
      begin
        result := false;
        exit;
      end;
      result := true;
      for j := 0 to Length(oidBytes) - 1 do
        if oidBytes[j] <> PByte(NativeUInt(expectedBuf) + 2 + NativeUInt(j))^ then
        begin
          result := false;
          exit;
        end;
    finally
      LocalFree(HLOCAL(expectedBuf));
    end;
  end;

begin
  // Default: SHA-256. Always safe for RSA; safe for ECDSA P-256; the only
  // case we need to override is when the cert is ECDSA P-384 / P-521 (FIPS
  // 186 requires digest size = curve size).
  result := haSha256;
  if PublicKeyType <> cktEcdsa then
    exit;     // RSA or unknown — SHA-256 default stands

  info := GetCertInfo;
  if info = nil then
    exit;

  // ECDSA: the curve is in Algorithm.Parameters (DER-encoded OBJECT
  // IDENTIFIER for named curves: tag 06, length, body bytes).
  params := info.SubjectPublicKeyInfo.Algorithm.Parameters;
  if (params.cbData < 2) or (params.pbData = nil) or (PByte(params.pbData)^ <> $06) then
    exit;
  oidLen := PByte(NativeUInt(params.pbData) + 1)^;
  if (oidLen = 0) or (oidLen > params.cbData - 2) then
    exit;
  SetLength(oidBytes, oidLen);
  for i := 0 to oidLen - 1 do
    oidBytes[i] := PByte(NativeUInt(params.pbData) + 2 + NativeUInt(i))^;

  if CurveOidEquals(szOID_ECC_CURVE_P384) then
    result := haSha384
  else if CurveOidEquals(szOID_ECC_CURVE_P521) then
    result := haSha512;
  // P-256 and unknown curves fall through to the SHA-256 default.
end;

{ TCertificateStore }

constructor TCertificateStore.Create(handle : HCERTSTORE; const hashingService : IHashingService);
begin
  if handle = nil then
    raise ECryptoX509.Create('TCertificateStore.Create: handle is nil');
  inherited Create;
  FHandle := handle;
  FHashingService := hashingService;
end;

destructor TCertificateStore.Destroy;
begin
  if FHandle <> nil then
    CertCloseStore(FHandle, 0);
  FHandle := nil;
  inherited;
end;

function TCertificateStore.GetHandle : HCERTSTORE;
begin
  result := FHandle;
end;

function TCertificateStore.FindByThumbprint(const thumbprint : string) : ICertificate;
var
  cleaned : string;
  bytes : TBytes;
  blob : CRYPT_HASH_BLOB;
  ctx : PCCERT_CONTEXT;
begin
  result := nil;
  cleaned := StringReplace(thumbprint, ' ', '', [rfReplaceAll]);
  cleaned := StringReplace(cleaned, ':', '', [rfReplaceAll]);
  bytes := HexToBytes(cleaned);
  blob.cbData := Length(bytes);
  if blob.cbData > 0 then
    blob.pbData := @bytes[0]
  else
    blob.pbData := nil;

  ctx := CertFindCertificateInStore(FHandle, cENCODING_TYPE, 0,
    CERT_FIND_SHA1_HASH, @blob, nil);
  if ctx <> nil then
    result := TCertificate.Create(ctx, true, FHashingService);
end;

function TCertificateStore.FindBySpki(const spkiHash : TBytes; algorithm : THashAlgorithm) : ICertificate;
var
  ctx : PCCERT_CONTEXT;
  candidate : ICertificate;
begin
  result := nil;
  // CertEnumCertificatesInStore frees the previous context automatically when
  // we pass it as the second arg, so we never own the enumerator's pointer.
  // When we want to keep a match alive we duplicate before exiting the loop.
  ctx := CertEnumCertificatesInStore(FHandle, nil);
  while ctx <> nil do
  begin
    candidate := TCertificate.Create(CertDuplicateCertificateContext(ctx), true, FHashingService);
    if BytesEqual(candidate.SpkiHash(algorithm), spkiHash) then
    begin
      result := candidate;
      exit;
    end;
    candidate := nil;
    ctx := CertEnumCertificatesInStore(FHandle, ctx);
  end;
end;

{ TCertificateChain }

constructor TCertificateChain.Create(const hashingService : IHashingService);
begin
  inherited Create;
  FHashingService := hashingService;
end;

destructor TCertificateChain.Destroy;
begin
  CloseHandles;
  inherited;
end;

procedure TCertificateChain.CloseHandles;
begin
  if FChain <> nil then
  begin
    CertFreeCertificateChain(FChain);
    FChain := nil;
  end;
  if FAdditionalStore <> nil then
  begin
    CertCloseStore(FAdditionalStore, 0);
    FAdditionalStore := nil;
  end;
end;

function TCertificateChain.Build(const cert : ICertificate;
                                 const additionalCerts : array of ICertificate) : TChainResult;
begin
  // Legacy: no time, no revocation. Phase 1 / Phase 2 behaviour preserved.
  result := BuildAtTime(cert, additionalCerts, 0, false);
end;

function TCertificateChain.RevocationStatus : TRevocationStatus;
begin
  result := FRevocationStatus;
end;

function TCertificateChain.RevocationReason : TRevocationReason;
begin
  result := FRevocationReason;
end;

procedure TCertificateChain.CaptureLeafRevocationDetail;
var
  simple : PCERT_SIMPLE_CHAIN;
  leafElement : PCERT_CHAIN_ELEMENT;
  revInfo : PCERT_REVOCATION_INFO;
  crlInfo : PCERT_REVOCATION_CRL_INFO;
  entry : PCRL_ENTRY;
  ext : PCERT_EXTENSION;
  i : integer;
  decoded : Pointer;
  decodedSize : DWORD;
  reasonCode : Word;
begin
  FRevocationReason := rrNotApplicable;
  if FChain = nil then
    exit;
  if FRevocationStatus <> rsRevoked then
    exit;   // nothing to surface unless we have a revocation hit

  if FChain.cChain = 0 then
    exit;
  simple := PPCERT_SIMPLE_CHAIN(FChain.rgpChain)^;
  if (simple = nil) or (simple.cElement = 0) then
    exit;
  leafElement := PPCERT_CHAIN_ELEMENT(simple.rgpElement)^;
  if leafElement = nil then
    exit;

  revInfo := leafElement.pRevocationInfo;
  if (revInfo = nil) or (revInfo.pCrlInfo = nil) then
  begin
    // Revocation came back as revoked but we have no CRL detail to inspect —
    // e.g. revoked via an OCSP responder that didn't expose the reason.
    FRevocationReason := rrUnknown;
    exit;
  end;
  crlInfo := revInfo.pCrlInfo;
  entry := crlInfo.pCrlEntry;
  if entry = nil then
  begin
    FRevocationReason := rrUnknown;
    exit;
  end;

  // Walk the CRL entry extensions for 2.5.29.21 (id-ce-cRLReasons).
  ext := entry.rgExtension;
  for i := 0 to Integer(entry.cExtension) - 1 do
  begin
    if (ext.pszObjId <> nil) and (AnsiString(ext.pszObjId) = szOID_CRL_REASON_CODE) then
    begin
      decoded := nil;
      decodedSize := 0;
      if CryptDecodeObjectEx(cCRYPT_ASN_ENCODING, X509_CRL_REASON_CODE,
          ext.Value.pbData, ext.Value.cbData,
          CRYPT_DECODE_ALLOC_FLAG, nil, @decoded, decodedSize) then
      begin
        try
          // The decoded value is a single byte-sized enum.
          if (decoded <> nil) and (decodedSize >= 1) then
          begin
            reasonCode := PWord(decoded)^ and $FF;
            case reasonCode of
              CRL_REASON_UNSPECIFIED            : FRevocationReason := rrUnspecified;
              CRL_REASON_KEY_COMPROMISE         : FRevocationReason := rrKeyCompromise;
              CRL_REASON_CA_COMPROMISE          : FRevocationReason := rrCACompromise;
              CRL_REASON_AFFILIATION_CHANGED    : FRevocationReason := rrAffiliationChanged;
              CRL_REASON_SUPERSEDED             : FRevocationReason := rrSuperseded;
              CRL_REASON_CESSATION_OF_OPERATION : FRevocationReason := rrCessationOfOperation;
              CRL_REASON_CERTIFICATE_HOLD       : FRevocationReason := rrCertificateHold;
              CRL_REASON_REMOVE_FROM_CRL        : FRevocationReason := rrRemoveFromCRL;
              CRL_REASON_PRIVILEGE_WITHDRAWN    : FRevocationReason := rrPrivilegeWithdrawn;
              CRL_REASON_AA_COMPROMISE          : FRevocationReason := rrAACompromise;
            else
              FRevocationReason := rrUnknown;
            end;
          end;
        finally
          LocalFree(HLOCAL(decoded));
        end;
      end;
      exit;
    end;
    Inc(ext);
  end;

  // No reason extension on the CRL entry. RFC 5280 treats this as
  // "unspecified" rather than "unknown" — the cert is revoked, just without
  // a stated reason.
  FRevocationReason := rrUnspecified;
end;

function TCertificateChain.BuildAtTime(const cert : ICertificate;
                                       const additionalCerts : array of ICertificate;
                                       asOfTime : TDateTime;
                                       checkRevocation : boolean) : TChainResult;
var
  para : CERT_CHAIN_PARA;
  usage : array[0..0] of PAnsiChar;
  i : integer;
  filetime : TFileTime;
  pTimeArg : PFileTime;
  flags : DWORD;
begin
  CloseHandles;
  FRevocationStatus := rsNotChecked;
  FRevocationReason := rrNotApplicable;

  if cert = nil then
    raise ECryptoX509.Create('BuildAtTime: cert is nil');
  FLeaf := cert;

  // Build an in-memory store holding any additional certs (intermediates
  // pulled from the signature blob).
  if Length(additionalCerts) > 0 then
  begin
    FAdditionalStore := CertOpenStore(CERT_STORE_PROV_MEMORY, cENCODING_TYPE, 0, 0, nil);
    if FAdditionalStore = nil then
      raise ECryptoX509.CreateFmt('CertOpenStore(MEMORY) failed: %d', [GetLastError]);
    for i := Low(additionalCerts) to High(additionalCerts) do
    begin
      if additionalCerts[i] = nil then
        Continue;
      CertAddCertificateContextToStore(FAdditionalStore,
        additionalCerts[i].GetContext, CERT_STORE_ADD_USE_EXISTING, nil);
    end;
  end;

  FillChar(para, SizeOf(para), 0);
  para.cbSize := SizeOf(para);
  para.RequestedUsage.dwType := USAGE_MATCH_TYPE_AND;
  para.RequestedUsage.Usage.cUsageIdentifier := 1;
  usage[0] := PAnsiChar(szOID_PKIX_KP_CODE_SIGNING);
  para.RequestedUsage.Usage.rgpszUsageIdentifier := @usage[0];

  // V-26: when asOfTime is set, evaluate the chain *as of* the signing
  // timestamp. A revocation that post-dates asOfTime does not fail the
  // chain — the signature was valid when it was made.
  if asOfTime > 0 then
  begin
    filetime := UtcDateTimeToFileTime(asOfTime);
    pTimeArg := @filetime;
  end
  else
    pTimeArg := nil;

  flags := 0;
  if checkRevocation then
    // Exclude the root because trusted roots aren't on CRLs and we don't want
    // a missing root-CRL to mask the leaf/intermediate result.
    flags := flags or CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT;

  if not CertGetCertificateChain(nil, cert.GetContext, pTimeArg, FAdditionalStore,
    @para, flags, nil, FChain) then
  begin
    FLastError := SysErrorMessage(GetLastError);
    FChain := nil;
    result := crUnknownError;
    exit;
  end;

  // V-26 — translate the chain's revocation-related bits into our enum,
  // distinct from the overall TChainResult so we can record "checked, good"
  // vs "checked, unreachable" on the receipt.
  if checkRevocation then
  begin
    if (FChain.TrustStatus.dwErrorStatus and CERT_TRUST_IS_REVOKED) <> 0 then
      FRevocationStatus := rsRevoked
    else if (FChain.TrustStatus.dwErrorStatus and CERT_TRUST_REVOCATION_STATUS_UNKNOWN) <> 0 then
      FRevocationStatus := rsUnknown
    else
      FRevocationStatus := rsGood;
    // V-26 — when revoked, dig into the leaf chain element's pRevocationInfo
    // to pull out the CRL reason code so the verifier can distinguish
    // keyCompromise (retroactively invalidating) from administrative reasons.
    CaptureLeafRevocationDetail;
  end;

  // Map the trust status onto our enum.
  if FChain.TrustStatus.dwErrorStatus = CERT_TRUST_NO_ERROR then
    result := crValid
  else if (FChain.TrustStatus.dwErrorStatus and CERT_TRUST_IS_REVOKED) <> 0 then
    result := crRevoked
  else if (FChain.TrustStatus.dwErrorStatus and CERT_TRUST_IS_NOT_TIME_VALID) <> 0 then
    result := crExpired
  else if (FChain.TrustStatus.dwErrorStatus and (CERT_TRUST_IS_UNTRUSTED_ROOT or CERT_TRUST_IS_PARTIAL_CHAIN)) <> 0 then
    result := crUntrustedRoot
  else if (FChain.TrustStatus.dwErrorStatus and CERT_TRUST_IS_NOT_VALID_FOR_USAGE) <> 0 then
    result := crWrongUsage
  else if (FChain.TrustStatus.dwErrorStatus = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) then
    // A CRL/OCSP outage by itself shouldn't fail the chain — surface as valid
    // with rsUnknown so policy can decide what to do.
    result := crValid
  else
    result := crUnknownError;
end;

function TCertificateChain.VerifyForCodeSigning(asOfTime : TDateTime) : TChainResult;
var
  policyPara : CERT_CHAIN_POLICY_PARA;
  policyStatus : CERT_CHAIN_POLICY_STATUS;
begin
  if FChain = nil then
  begin
    FLastError := 'Chain has not been built';
    result := crIncomplete;
    exit;
  end;

  FillChar(policyPara, SizeOf(policyPara), 0);
  policyPara.cbSize := SizeOf(policyPara);

  FillChar(policyStatus, SizeOf(policyStatus), 0);
  policyStatus.cbSize := SizeOf(policyStatus);

  if not CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_BASE, FChain,
    @policyPara, @policyStatus) then
  begin
    FLastError := SysErrorMessage(GetLastError);
    result := crUnknownError;
    exit;
  end;

  if policyStatus.dwError = 0 then
  begin
    // Confirm the leaf was valid at the requested signing time.
    if asOfTime > 0 then
    begin
      if (asOfTime < FLeaf.NotBefore) or (asOfTime > FLeaf.NotAfter) then
      begin
        FLastError := Format('Certificate not valid at %s (valid %s .. %s)',
          [DateTimeToStr(asOfTime), DateTimeToStr(FLeaf.NotBefore), DateTimeToStr(FLeaf.NotAfter)]);
        result := crExpired;
        exit;
      end;
    end;
    result := crValid;
  end
  else
  begin
    FLastError := Format('Chain policy error 0x%.8x', [policyStatus.dwError]);
    case policyStatus.dwError of
      $80092010 : result := crRevoked;           // CRYPT_E_REVOKED
      $800B0101 : result := crExpired;           // CERT_E_EXPIRED
      $800B010A : result := crIncomplete;        // CERT_E_CHAINING
      $800B0109 : result := crUntrustedRoot;     // CERT_E_UNTRUSTEDROOT
      $800B0110 : result := crWrongUsage;        // CERT_E_WRONG_USAGE
    else
      result := crUnknownError;
    end;
  end;
end;

function TCertificateChain.ChainCertificates : TArray<ICertificate>;
var
  simple : PCERT_SIMPLE_CHAIN;
  i : integer;
  element : PCERT_CHAIN_ELEMENT;
begin
  if (FChain = nil) or (FChain.cChain = 0) then
  begin
    result := nil;
    exit;
  end;
  simple := PPCERT_SIMPLE_CHAIN(FChain.rgpChain)^;
  SetLength(result, simple.cElement);
  for i := 0 to Integer(simple.cElement) - 1 do
  begin
    element := PPCERT_CHAIN_ELEMENT(NativeUInt(simple.rgpElement) + NativeUInt(i * SizeOf(Pointer)))^;
    result[i] := TCertificate.Create(
      CertDuplicateCertificateContext(element.pCertContext),
      true,
      FHashingService);
  end;
end;

function TCertificateChain.RootCertificate : ICertificate;
var
  chainCerts : TArray<ICertificate>;
begin
  chainCerts := ChainCertificates;
  if Length(chainCerts) = 0 then
    result := nil
  else
    result := chainCerts[High(chainCerts)];
end;

function TCertificateChain.LastErrorMessage : string;
begin
  result := FLastError;
end;

{ TX509Service }

constructor TX509Service.Create(const hashingService : IHashingService);
begin
  if hashingService = nil then
    raise ECryptoX509.Create('TX509Service requires a hashing service');
  inherited Create;
  FHashingService := hashingService;
end;

function TX509Service.OpenSystemStore(location : TCertStoreLocation; const storeName : string) : ICertificateStore;
var
  flags : DWORD;
  handle : HCERTSTORE;
begin
  case location of
    cslCurrentUser  : flags := CERT_SYSTEM_STORE_CURRENT_USER;
    cslLocalMachine : flags := CERT_SYSTEM_STORE_LOCAL_MACHINE;
  else
    raise ECryptoX509.Create('Unknown certificate store location');
  end;

  handle := CertOpenStore(
    CERT_STORE_PROV_SYSTEM_W,
    cENCODING_TYPE,
    0,
    flags,
    PWideChar(storeName));
  if handle = nil then
    raise ECryptoX509.CreateFmt('Failed to open system store "%s" (location %d): %d',
      [storeName, Ord(location), GetLastError]);

  result := TCertificateStore.Create(handle, FHashingService);
end;

function TX509Service.OpenPfxStore(const pfxBytes : TBytes; const password : string) : ICertificateStore;
var
  blob : CRYPT_DATA_BLOB;
  pwd : PWideChar;
  handle : HCERTSTORE;
const
  cCRYPT_USER_KEYSET = $00001000;
  cPKCS12_NO_PERSIST_KEY = $00008000;
begin
  if Length(pfxBytes) = 0 then
    raise ECryptoX509.Create('PFX bytes empty');
  blob.cbData := Length(pfxBytes);
  blob.pbData := @pfxBytes[0];

  if password = '' then
    pwd := nil
  else
    pwd := PWideChar(password);

  handle := PFXImportCertStore(@blob, pwd, cCRYPT_USER_KEYSET);
  if handle = nil then
    raise ECryptoX509.CreateFmt('PFXImportCertStore failed: %d (wrong password or malformed PFX)',
      [GetLastError]);
  result := TCertificateStore.Create(handle, FHashingService);
end;

function TX509Service.LoadCertificateFromDer(const der : TBytes) : ICertificate;
var
  ctx : PCCERT_CONTEXT;
begin
  if Length(der) = 0 then
    raise ECryptoX509.Create('LoadCertificateFromDer: empty input');
  ctx := CertCreateCertificateContext(cENCODING_TYPE, @der[0], Length(der));
  if ctx = nil then
    raise ECryptoX509.CreateFmt('CertCreateCertificateContext failed: %d', [GetLastError]);
  result := TCertificate.Create(ctx, true, FHashingService);
end;

function TX509Service.CreateChain : ICertificateChain;
begin
  result := TCertificateChain.Create(FHashingService);
end;

end.
