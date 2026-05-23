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

unit DPM.Core.Crypto.Win32;

// Flat Win32 imports for the cryptography layer: crypt32.dll, bcrypt.dll,
// ncrypt.dll. This is the single API surface for the whole crypto layer —
// no other DPM unit imports these DLLs directly.
//
// Conventions:
//   * Structures use Windows naming (e.g. CERT_CONTEXT) to keep MSDN searches
//     productive.
//   * Pointer aliases follow the conventional 'P' / 'PP' prefix.
//   * Functions are declared `stdcall`. crypt32 / bcrypt / ncrypt all use it.
//   * Only the subset of constants and structs we use is declared. Add more
//     as further phases need them.

interface

uses
  Winapi.Windows;

const
  cCrypt32 = 'crypt32.dll';
  cBCrypt  = 'bcrypt.dll';
  cNCrypt  = 'ncrypt.dll';

// ---------------------------------------------------------------------------
// BCrypt (CNG) — used by DPM.Core.Crypto.Hashing
// ---------------------------------------------------------------------------

type
  NTSTATUS         = LongInt;
  BCRYPT_HANDLE    = THandle;
  BCRYPT_ALG_HANDLE  = THandle;
  BCRYPT_HASH_HANDLE = THandle;
  BCRYPT_KEY_HANDLE  = THandle;

const
  STATUS_SUCCESS : NTSTATUS = 0;

  BCRYPT_SHA256_ALGORITHM = 'SHA256';
  BCRYPT_SHA384_ALGORITHM = 'SHA384';
  BCRYPT_SHA512_ALGORITHM = 'SHA512';

  BCRYPT_OBJECT_LENGTH    = 'ObjectLength';
  BCRYPT_HASH_LENGTH      = 'HashDigestLength';

  BCRYPT_HASH_REUSABLE_FLAG = $00000020;

function BCryptOpenAlgorithmProvider(out phAlgorithm : BCRYPT_ALG_HANDLE;
                                     pszAlgId, pszImplementation : PWideChar;
                                     dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptCloseAlgorithmProvider(hAlgorithm : BCRYPT_ALG_HANDLE;
                                      dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptGetProperty(hObject : BCRYPT_HANDLE; pszProperty : PWideChar;
                           pbOutput : PByte; cbOutput : ULONG;
                           var pcbResult : ULONG; dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptCreateHash(hAlgorithm : BCRYPT_ALG_HANDLE;
                          out phHash : BCRYPT_HASH_HANDLE;
                          pbHashObject : PByte; cbHashObject : ULONG;
                          pbSecret : PByte; cbSecret : ULONG;
                          dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptHashData(hHash : BCRYPT_HASH_HANDLE; pbInput : PByte;
                        cbInput : ULONG; dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptFinishHash(hHash : BCRYPT_HASH_HANDLE; pbOutput : PByte;
                          cbOutput : ULONG; dwFlags : ULONG) : NTSTATUS; stdcall;
  external cBCrypt;

function BCryptDestroyHash(hHash : BCRYPT_HASH_HANDLE) : NTSTATUS; stdcall;
  external cBCrypt;


// ---------------------------------------------------------------------------
// Crypt32 — certificates, certificate stores, chains, CMS
// ---------------------------------------------------------------------------

type
  HCERTSTORE      = Pointer;
  HCRYPTMSG       = Pointer;
  HCRYPTPROV      = ULONG_PTR;
  HCRYPTPROV_LEGACY = ULONG_PTR;
  HCRYPTPROV_OR_NCRYPT_KEY_HANDLE = ULONG_PTR;
  HCERTCHAINENGINE = Pointer;

  PCERT_CONTEXT   = ^CERT_CONTEXT;
  PCCERT_CONTEXT  = PCERT_CONTEXT;
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;

  CRYPT_INTEGER_BLOB = record
    cbData : DWORD;
    pbData : PByte;
  end;
  PCRYPT_INTEGER_BLOB = ^CRYPT_INTEGER_BLOB;
  CRYPT_OBJID_BLOB = CRYPT_INTEGER_BLOB;
  PCRYPT_OBJID_BLOB = PCRYPT_INTEGER_BLOB;
  CRYPT_DATA_BLOB  = CRYPT_INTEGER_BLOB;
  PCRYPT_DATA_BLOB = PCRYPT_INTEGER_BLOB;
  CERT_NAME_BLOB   = CRYPT_INTEGER_BLOB;
  PCERT_NAME_BLOB  = PCRYPT_INTEGER_BLOB;
  CRYPT_HASH_BLOB  = CRYPT_INTEGER_BLOB;
  PCRYPT_HASH_BLOB = PCRYPT_INTEGER_BLOB;
  CRYPT_DER_BLOB   = CRYPT_INTEGER_BLOB;

  CRYPT_BIT_BLOB = record
    cbData      : DWORD;
    pbData      : PByte;
    cUnusedBits : DWORD;
  end;
  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;

  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId   : PAnsiChar;
    Parameters : CRYPT_OBJID_BLOB;
  end;
  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;

  CERT_PUBLIC_KEY_INFO = record
    Algorithm : CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey : CRYPT_BIT_BLOB;
  end;
  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;

  CERT_EXTENSION = record
    pszObjId  : PAnsiChar;
    fCritical : BOOL;
    Value     : CRYPT_OBJID_BLOB;
  end;
  PCERT_EXTENSION = ^CERT_EXTENSION;

  CERT_INFO = record
    dwVersion              : DWORD;
    SerialNumber           : CRYPT_INTEGER_BLOB;
    SignatureAlgorithm     : CRYPT_ALGORITHM_IDENTIFIER;
    Issuer                 : CERT_NAME_BLOB;
    NotBefore              : TFileTime;
    NotAfter               : TFileTime;
    Subject                : CERT_NAME_BLOB;
    SubjectPublicKeyInfo   : CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId         : CRYPT_BIT_BLOB;
    SubjectUniqueId        : CRYPT_BIT_BLOB;
    cExtension             : DWORD;
    rgExtension            : PCERT_EXTENSION;
  end;
  PCERT_INFO = ^CERT_INFO;

  CERT_CONTEXT = record
    dwCertEncodingType : DWORD;
    pbCertEncoded      : PByte;
    cbCertEncoded      : DWORD;
    pCertInfo          : PCERT_INFO;
    hCertStore         : HCERTSTORE;
  end;

  CTL_USAGE = record
    cUsageIdentifier     : DWORD;
    rgpszUsageIdentifier : PPAnsiChar;
  end;
  PCTL_USAGE = ^CTL_USAGE;
  CERT_ENHKEY_USAGE = CTL_USAGE;
  PCERT_ENHKEY_USAGE = PCTL_USAGE;

  CERT_USAGE_MATCH = record
    dwType  : DWORD;
    Usage   : CERT_ENHKEY_USAGE;
  end;
  PCERT_USAGE_MATCH = ^CERT_USAGE_MATCH;

  CERT_CHAIN_PARA = record
    cbSize             : DWORD;
    RequestedUsage     : CERT_USAGE_MATCH;
    // Two further (optional) fields exist on later SDKs (RequestedIssuancePolicy,
    // dwUrlRetrievalTimeout, fCheckRevocationFreshnessTime, etc.) — we use the
    // shorter form that's available since XP.
  end;
  PCERT_CHAIN_PARA = ^CERT_CHAIN_PARA;

  CERT_CHAIN_POLICY_PARA = record
    cbSize           : DWORD;
    dwFlags          : DWORD;
    pvExtraPolicyPara : Pointer;
  end;
  PCERT_CHAIN_POLICY_PARA = ^CERT_CHAIN_POLICY_PARA;

  CERT_CHAIN_POLICY_STATUS = record
    cbSize             : DWORD;
    dwError            : DWORD;
    lChainIndex        : LongInt;
    lElementIndex      : LongInt;
    pvExtraPolicyStatus : Pointer;
  end;
  PCERT_CHAIN_POLICY_STATUS = ^CERT_CHAIN_POLICY_STATUS;

  CERT_TRUST_STATUS = record
    dwErrorStatus : DWORD;
    dwInfoStatus  : DWORD;
  end;

  // Stub for PCCRL_CONTEXT — we don't dereference it; we just need a typed
  // pointer to match the SDK layout.
  PCCRL_CONTEXT = Pointer;

  // CRL_ENTRY (RFC 5280 §5.1) — describes one revoked certificate. RevocationDate
  // tells us when it was revoked; rgExtension may carry the reason code under
  // OID 2.5.29.21 (id-ce-cRLReasons).
  PCRL_ENTRY = ^CRL_ENTRY;
  CRL_ENTRY = record
    SerialNumber    : CRYPT_INTEGER_BLOB;
    RevocationDate  : TFileTime;
    cExtension      : DWORD;
    rgExtension     : PCERT_EXTENSION;
  end;

  PCERT_REVOCATION_CRL_INFO = ^CERT_REVOCATION_CRL_INFO;
  CERT_REVOCATION_CRL_INFO = record
    cbSize             : DWORD;
    pBaseCrlContext    : PCCRL_CONTEXT;
    pDeltaCrlContext   : PCCRL_CONTEXT;
    pCrlEntry          : PCRL_ENTRY;
    fDeltaCrlEntry     : BOOL;
  end;

  PCERT_REVOCATION_INFO = ^CERT_REVOCATION_INFO;
  CERT_REVOCATION_INFO = record
    cbSize              : DWORD;
    dwRevocationResult  : DWORD;       // HRESULT from the revocation provider
    pszRevocationOid    : PAnsiChar;
    pvOidSpecificInfo   : Pointer;
    fHasFreshnessTime   : BOOL;
    dwFreshnessTime     : DWORD;
    pCrlInfo            : PCERT_REVOCATION_CRL_INFO;
  end;

  // The full CERT_CHAIN_ELEMENT / CERT_SIMPLE_CHAIN / CERT_CHAIN_CONTEXT
  // shapes are large; we only need to read trust status and walk the chain
  // to its root, so the partial declarations below match the layout MSDN
  // documents and are sufficient for our uses.
  PCERT_CHAIN_ELEMENT  = ^CERT_CHAIN_ELEMENT;
  PPCERT_CHAIN_ELEMENT = ^PCERT_CHAIN_ELEMENT;
  CERT_CHAIN_ELEMENT = record
    cbSize             : DWORD;
    pCertContext       : PCCERT_CONTEXT;
    TrustStatus        : CERT_TRUST_STATUS;
    pRevocationInfo    : PCERT_REVOCATION_INFO;
    pIssuanceUsage     : Pointer;
    pApplicationUsage  : Pointer;
    pwszExtendedErrorInfo : PWideChar;
  end;

  PCERT_SIMPLE_CHAIN = ^CERT_SIMPLE_CHAIN;
  PPCERT_SIMPLE_CHAIN = ^PCERT_SIMPLE_CHAIN;
  CERT_SIMPLE_CHAIN = record
    cbSize         : DWORD;
    TrustStatus    : CERT_TRUST_STATUS;
    cElement       : DWORD;
    rgpElement     : PPCERT_CHAIN_ELEMENT;
    pTrustListInfo : Pointer;
    fHasRevocationFreshnessTime : BOOL;
    dwRevocationFreshnessTime   : DWORD;
  end;

  PCERT_CHAIN_CONTEXT  = ^CERT_CHAIN_CONTEXT;
  CERT_CHAIN_CONTEXT = record
    cbSize              : DWORD;
    TrustStatus         : CERT_TRUST_STATUS;
    cChain              : DWORD;
    rgpChain            : PPCERT_SIMPLE_CHAIN;
    cLowerQualityChainContext : DWORD;
    rgpLowerQualityChainContext : Pointer;
    fHasRevocationFreshnessTime : BOOL;
    dwRevocationFreshnessTime   : DWORD;
  end;

const
  // Encoding types
  X509_ASN_ENCODING   = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;

  // Cert store provider IDs — CertOpenStore takes lpszStoreProvider as
  // either a string pointer or a small integer cast to PAnsiChar.
  CERT_STORE_PROV_MEMORY     = PAnsiChar(2);
  CERT_STORE_PROV_FILENAME_A = PAnsiChar(7);
  CERT_STORE_PROV_FILENAME_W = PAnsiChar(8);
  CERT_STORE_PROV_SYSTEM_A   = PAnsiChar(9);
  CERT_STORE_PROV_SYSTEM_W   = PAnsiChar(10);

  // Open-store flags
  CERT_SYSTEM_STORE_CURRENT_USER  = $00010000;
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;

  // CertAddCertificateContextToStore disposition values
  CERT_STORE_ADD_USE_EXISTING = 2;
  CERT_STORE_ADD_ALWAYS       = 4;

  // CertFindCertificateInStore find type
  CERT_FIND_HASH         = $00010000;  // by SHA-1 thumbprint
  CERT_FIND_SUBJECT_NAME = $00020007;
  CERT_FIND_SHA1_HASH    = CERT_FIND_HASH;

  // CertCloseStore flags
  CERT_CLOSE_STORE_FORCE_FLAG = $00000001;
  CERT_CLOSE_STORE_CHECK_FLAG = $00000002;

  // Cert info / encoding helpers (used by CryptEncodeObjectEx)
  X509_PUBLIC_KEY_INFO    = PAnsiChar(8);
  X509_ENHANCED_KEY_USAGE = PAnsiChar(36);
  CRYPT_ENCODE_ALLOC_FLAG = $8000;
  CRYPT_DECODE_ALLOC_FLAG = $8000;

  // CertGetCertificateChain usage type
  USAGE_MATCH_TYPE_AND = $00000000;
  USAGE_MATCH_TYPE_OR  = $00000001;

  // szOID constants
  szOID_PKIX_KP_CODE_SIGNING : AnsiString = '1.3.6.1.5.5.7.3.3';
  szOID_RSA_RSA              : AnsiString = '1.2.840.113549.1.1.1';
  // id-ecPublicKey — the SubjectPublicKeyInfo algorithm OID for any ECDSA
  // certificate. The actual curve is identified by the algorithm's parameters
  // field, which (for named curves) is itself an OBJECT IDENTIFIER.
  szOID_ECC_PUBLIC_KEY       : AnsiString = '1.2.840.10045.2.1';
  // Named-curve OIDs we recognise. Used to pair a digest with the curve size
  // (FIPS 186 / NIST SP 800-57: digest size should match curve size for ECDSA).
  szOID_ECC_CURVE_P256       : AnsiString = '1.2.840.10045.3.1.7';
  szOID_ECC_CURVE_P384       : AnsiString = '1.3.132.0.34';
  szOID_ECC_CURVE_P521       : AnsiString = '1.3.132.0.35';
  szOID_RSA_SHA256RSA        : AnsiString = '1.2.840.113549.1.1.11';
  szOID_RSA_SHA384RSA        : AnsiString = '1.2.840.113549.1.1.12';
  szOID_RSA_SHA512RSA        : AnsiString = '1.2.840.113549.1.1.13';
  // ECDSA-with-hash signature algorithm OIDs (ANSI X9.62 / RFC 5758).
  // Used in CMS SignerInfo.HashEncryptionAlgorithm when the signer key is
  // ECDSA. Confusingly, "HashEncryptionAlgorithm" is the CMS field name —
  // it's actually the *signature algorithm*, not an encryption algorithm.
  szOID_ECDSA_SHA256         : AnsiString = '1.2.840.10045.4.3.2';
  szOID_ECDSA_SHA384         : AnsiString = '1.2.840.10045.4.3.3';
  szOID_ECDSA_SHA512         : AnsiString = '1.2.840.10045.4.3.4';
  szOID_OIWSEC_SHA1          : AnsiString = '1.3.14.3.2.26';
  szOID_NIST_SHA256          : AnsiString = '2.16.840.1.101.3.4.2.1';
  szOID_NIST_SHA384          : AnsiString = '2.16.840.1.101.3.4.2.2';
  szOID_NIST_SHA512          : AnsiString = '2.16.840.1.101.3.4.2.3';
  szOID_RSA_signingTime      : AnsiString = '1.2.840.113549.1.9.5';
  szOID_RSA_messageDigest    : AnsiString = '1.2.840.113549.1.9.4';
  szOID_RSA_contentType      : AnsiString = '1.2.840.113549.1.9.3';
  szOID_RSA_data             : AnsiString = '1.2.840.113549.1.7.1';
  szOID_RSA_signedData       : AnsiString = '1.2.840.113549.1.7.2';
  szOID_RFC3161_counterSign  : AnsiString = '1.3.6.1.4.1.311.3.3.1';
  // RFC 5280 §5.3.1 — CRL entry extension carrying the revocation reason.
  szOID_CRL_REASON_CODE      : AnsiString = '2.5.29.21';

  // CRL reason code values (RFC 5280 §5.3.1). The X509_CRL_REASON_CODE
  // decoder returns a single DWORD whose low byte is the enumerated value.
  CRL_REASON_UNSPECIFIED              = 0;
  CRL_REASON_KEY_COMPROMISE           = 1;
  CRL_REASON_CA_COMPROMISE            = 2;
  CRL_REASON_AFFILIATION_CHANGED      = 3;
  CRL_REASON_SUPERSEDED               = 4;
  CRL_REASON_CESSATION_OF_OPERATION   = 5;
  CRL_REASON_CERTIFICATE_HOLD         = 6;
  CRL_REASON_REMOVE_FROM_CRL          = 8;
  CRL_REASON_PRIVILEGE_WITHDRAWN      = 9;
  CRL_REASON_AA_COMPROMISE            = 10;

  // CryptDecodeObjectEx struct type for the CRL reason enum.
  X509_CRL_REASON_CODE       : PAnsiChar = PAnsiChar(76);

  // Chain policy identifiers (passed as LPCSTR via @-cast).
  CERT_CHAIN_POLICY_BASE              = PAnsiChar(1);
  CERT_CHAIN_POLICY_AUTHENTICODE      = PAnsiChar(2);
  CERT_CHAIN_POLICY_AUTHENTICODE_TS   = PAnsiChar(3);
  CERT_CHAIN_POLICY_SSL               = PAnsiChar(4);
  CERT_CHAIN_POLICY_BASIC_CONSTRAINTS = PAnsiChar(5);

  // Chain build flags
  CERT_CHAIN_REVOCATION_CHECK_END_CERT             = $10000000;
  CERT_CHAIN_REVOCATION_CHECK_CHAIN                = $20000000;
  CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT   = $40000000;

  // Trust-status info bits
  CERT_TRUST_NO_ERROR                              = $00000000;
  CERT_TRUST_IS_NOT_TIME_VALID                     = $00000001;
  CERT_TRUST_IS_REVOKED                            = $00000004;
  CERT_TRUST_IS_NOT_SIGNATURE_VALID                = $00000008;
  CERT_TRUST_IS_NOT_VALID_FOR_USAGE                = $00000010;
  CERT_TRUST_IS_UNTRUSTED_ROOT                     = $00000020;
  CERT_TRUST_REVOCATION_STATUS_UNKNOWN             = $00000040;
  CERT_TRUST_IS_PARTIAL_CHAIN                      = $00010000;

  // CryptMsg control codes
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR = 8;

  // CryptEncodeObjectEx struct type for a PKCS#7 Attribute. Defined in
  // wincrypt.h as `(LPCSTR)22`. Used to DER-encode the (oid, values) pair
  // that becomes the `blob` of CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA.
  PKCS_ATTRIBUTE          : PAnsiChar = PAnsiChar(22);
  // Outer ContentInfo wrapper for CMS messages.
  PKCS_CONTENT_INFO       : PAnsiChar = PAnsiChar(33);
  // Encodes a CMSG_SIGNER_INFO struct as a PKCS#7 SignerInfo SEQUENCE.
  PKCS7_SIGNER_INFO       : PAnsiChar = PAnsiChar(500);
  // Encodes an AlgorithmIdentifier (OID + optional params).
  X509_ALGORITHM_IDENTIFIER : PAnsiChar = PAnsiChar(74);
  // Encodes a bare OBJECT IDENTIFIER. pvStructInfo points to an LPSTR
  // (i.e. PAnsiChar*) whose target is the dotted-OID AnsiString. Defined
  // in wincrypt.h as `(LPCSTR)73`. NB: 7 is X509_NAME — passing 7 here
  // makes CryptEncodeObjectEx mis-parse the input and fail with E_OUTOFMEMORY.
  X509_OBJECT_IDENTIFIER  : PAnsiChar = PAnsiChar(73);

  // CryptMsgGetParam types we care about
  CMSG_TYPE_PARAM                = 1;
  CMSG_CONTENT_PARAM             = 2;
  CMSG_BARE_CONTENT_PARAM        = 3;
  CMSG_INNER_CONTENT_TYPE_PARAM  = 4;
  CMSG_SIGNER_COUNT_PARAM        = 5;
  CMSG_SIGNER_INFO_PARAM         = 6;
  CMSG_SIGNER_CERT_INFO_PARAM    = 7;
  CMSG_SIGNER_HASH_ALGORITHM_PARAM = 8;
  CMSG_SIGNER_AUTH_ATTR_PARAM    = 9;
  CMSG_SIGNER_UNAUTH_ATTR_PARAM  = 10;
  CMSG_CERT_COUNT_PARAM          = 11;
  CMSG_CERT_PARAM                = 12;
  CMSG_CONTENT_ENCRYPTED_PARAM   = 18;
  // The signer info's encryptedDigest (i.e. the per-signer signature value).
  // This is the byte sequence that an RFC3161 timestamp counter-signature
  // covers; adding/removing unsigned attributes does NOT change it.
  CMSG_ENCRYPTED_DIGEST          = 27;

type
  CRYPT_ATTRIBUTE = record
    pszObjId : PAnsiChar;
    cValue   : DWORD;
    rgValue  : PCRYPT_INTEGER_BLOB;
  end;
  PCRYPT_ATTRIBUTE = ^CRYPT_ATTRIBUTE;

  // Parameter struct for CryptMsgControl(CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR).
  // The `blob` field carries the already-DER-encoded CRYPT_ATTRIBUTE, NOT
  // the raw struct. Encoding happens via CryptEncodeObjectEx(PKCS_ATTRIBUTE).
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = record
    cbSize        : DWORD;
    dwSignerIndex : DWORD;
    blob          : CRYPT_INTEGER_BLOB;
  end;
  PCMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = ^CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA;

  CRYPT_ATTRIBUTES = record
    cAttr  : DWORD;
    rgAttr : PCRYPT_ATTRIBUTE;
  end;
  PCRYPT_ATTRIBUTES = ^CRYPT_ATTRIBUTES;

  CMSG_SIGNER_ENCODE_INFO = record
    cbSize              : DWORD;
    pCertInfo           : PCERT_INFO;
    hCryptProvOrNCryptKey : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
    dwKeySpec           : DWORD;
    HashAlgorithm       : CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo       : Pointer;
    cAuthAttr           : DWORD;
    rgAuthAttr          : PCRYPT_ATTRIBUTE;
    cUnauthAttr         : DWORD;
    rgUnauthAttr        : PCRYPT_ATTRIBUTE;
  end;
  PCMSG_SIGNER_ENCODE_INFO = ^CMSG_SIGNER_ENCODE_INFO;

  // CMSG_SIGNER_INFO — accepted by CryptEncodeObjectEx(PKCS7_SIGNER_INFO).
  // Fully describes a SignerInfo: signer identity (Issuer + Serial),
  // hash algorithm, signature algorithm + value, and the signed +
  // unsigned attribute sets.
  CMSG_SIGNER_INFO = record
    dwVersion               : DWORD;
    Issuer                  : CERT_NAME_BLOB;
    SerialNumber            : CRYPT_INTEGER_BLOB;
    HashAlgorithm           : CRYPT_ALGORITHM_IDENTIFIER;
    HashEncryptionAlgorithm : CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedHash           : CRYPT_INTEGER_BLOB;       // the signature value
    AuthAttrs               : CRYPT_ATTRIBUTES;
    UnauthAttrs             : CRYPT_ATTRIBUTES;
  end;
  PCMSG_SIGNER_INFO = ^CMSG_SIGNER_INFO;

  // CryptEncodeObjectEx(PKCS_CONTENT_INFO) parameter struct. Wraps an
  // inner DER blob with its content-type OID into a ContentInfo SEQUENCE.
  CRYPT_CONTENT_INFO = record
    pszObjId : PAnsiChar;
    Content  : CRYPT_INTEGER_BLOB;
  end;
  PCRYPT_CONTENT_INFO = ^CRYPT_CONTENT_INFO;

  CRYPT_SIGN_MESSAGE_PARA = record
    cbSize                    : DWORD;
    dwMsgEncodingType         : DWORD;
    pSigningCert              : PCCERT_CONTEXT;
    HashAlgorithm             : CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo             : Pointer;
    cMsgCert                  : DWORD;
    rgpMsgCert                : PPCCERT_CONTEXT;
    cMsgCrl                   : DWORD;
    rgpMsgCrl                 : Pointer;
    cAuthAttr                 : DWORD;
    rgAuthAttr                : PCRYPT_ATTRIBUTE;
    cUnauthAttr               : DWORD;
    rgUnauthAttr              : PCRYPT_ATTRIBUTE;
    dwFlags                   : DWORD;
    dwInnerContentType        : DWORD;
  end;
  PCRYPT_SIGN_MESSAGE_PARA = ^CRYPT_SIGN_MESSAGE_PARA;

  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize                : DWORD;
    dwMsgAndCertEncodingType : DWORD;
    hCryptProv            : HCRYPTPROV_LEGACY;
    pfnGetSignerCertificate : Pointer;
    pvGetArg              : Pointer;
  end;
  PCRYPT_VERIFY_MESSAGE_PARA = ^CRYPT_VERIFY_MESSAGE_PARA;

  // RFC3161 timestamp parameters
  CRYPT_TIMESTAMP_PARA = record
    pszTSAPolicyId : PAnsiChar;
    fRequestCerts  : BOOL;
    Nonce          : CRYPT_INTEGER_BLOB;
    cExtension     : DWORD;
    rgExtension    : Pointer;
  end;
  PCRYPT_TIMESTAMP_PARA = ^CRYPT_TIMESTAMP_PARA;

  CRYPT_TIMESTAMP_ACCURACY = record
    dwSeconds : DWORD;
    dwMillis  : DWORD;
    dwMicros  : DWORD;
  end;

  CRYPT_TIMESTAMP_INFO = record
    dwVersion          : DWORD;
    pszTSAPolicyId     : PAnsiChar;
    HashAlgorithm      : CRYPT_ALGORITHM_IDENTIFIER;
    HashedMessage      : CRYPT_INTEGER_BLOB;
    SerialNumber       : CRYPT_INTEGER_BLOB;
    ftTime             : TFileTime;
    pvAccuracy         : ^CRYPT_TIMESTAMP_ACCURACY;
    fOrdering          : BOOL;
    Nonce              : CRYPT_INTEGER_BLOB;
    Tsa                : CRYPT_INTEGER_BLOB;
    cExtension         : DWORD;
    rgExtension        : Pointer;
  end;
  PCRYPT_TIMESTAMP_INFO = ^CRYPT_TIMESTAMP_INFO;

  CRYPT_TIMESTAMP_CONTEXT = record
    cbEncoded      : DWORD;
    pbEncoded      : PByte;
    pTimeStamp     : PCRYPT_TIMESTAMP_INFO;
  end;
  PCRYPT_TIMESTAMP_CONTEXT = ^CRYPT_TIMESTAMP_CONTEXT;

// ---- crypt32 functions ----------------------------------------------------

function CertOpenStore(lpszStoreProvider : PAnsiChar; dwEncodingType : DWORD;
                       hCryptProv : HCRYPTPROV_LEGACY; dwFlags : DWORD;
                       pvPara : Pointer) : HCERTSTORE; stdcall; external cCrypt32;

function CertOpenSystemStoreW(hProv : HCRYPTPROV_LEGACY;
                              szSubsystemProtocol : PWideChar) : HCERTSTORE; stdcall; external cCrypt32 name 'CertOpenSystemStoreW';

function CertCloseStore(hCertStore : HCERTSTORE; dwFlags : DWORD) : BOOL; stdcall; external cCrypt32;

function CertFindCertificateInStore(hCertStore : HCERTSTORE;
                                    dwCertEncodingType, dwFindFlags, dwFindType : DWORD;
                                    pvFindPara : Pointer;
                                    pPrevCertContext : PCCERT_CONTEXT) : PCCERT_CONTEXT; stdcall; external cCrypt32;

function CertEnumCertificatesInStore(hCertStore : HCERTSTORE;
                                     pPrevCertContext : PCCERT_CONTEXT) : PCCERT_CONTEXT; stdcall; external cCrypt32;

function CertDuplicateCertificateContext(pCertContext : PCCERT_CONTEXT) : PCCERT_CONTEXT; stdcall; external cCrypt32;

function CertFreeCertificateContext(pCertContext : PCCERT_CONTEXT) : BOOL; stdcall; external cCrypt32;

function CertCreateCertificateContext(dwCertEncodingType : DWORD;
                                      pbCertEncoded : PByte;
                                      cbCertEncoded : DWORD) : PCCERT_CONTEXT; stdcall; external cCrypt32;

function CertAddCertificateContextToStore(hCertStore : HCERTSTORE;
                                          pCertContext : PCCERT_CONTEXT;
                                          dwAddDisposition : DWORD;
                                          ppStoreContext : PPCCERT_CONTEXT) : BOOL; stdcall; external cCrypt32;

function CryptDecodeObjectEx(dwCertEncodingType : DWORD;
                             lpszStructType : PAnsiChar;
                             pbEncoded : PByte; cbEncoded : DWORD;
                             dwFlags : DWORD; pDecodePara : Pointer;
                             pvStructInfo : Pointer;
                             var pcbStructInfo : DWORD) : BOOL; stdcall; external cCrypt32;

function CertGetCertificateContextProperty(pCertContext : PCCERT_CONTEXT;
                                           dwPropId : DWORD; pvData : Pointer;
                                           var pcbData : DWORD) : BOOL; stdcall; external cCrypt32;

function CertNameToStrW(dwCertEncodingType : DWORD; pName : PCERT_NAME_BLOB;
                        dwStrType : DWORD; psz : PWideChar; csz : DWORD) : DWORD; stdcall;
                        external cCrypt32 name 'CertNameToStrW';

function CryptEncodeObjectEx(dwCertEncodingType : DWORD; lpszStructType : PAnsiChar;
                             pvStructInfo : Pointer; dwFlags : DWORD;
                             pEncodePara : Pointer; pvEncoded : Pointer;
                             var pcbEncoded : DWORD) : BOOL; stdcall; external cCrypt32;

function CryptAcquireCertificatePrivateKey(pCert : PCCERT_CONTEXT; dwFlags : DWORD;
                                           pvParameters : Pointer;
                                           out phCryptProvOrNCryptKey : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                                           out pdwKeySpec : DWORD;
                                           out pfCallerFreeProv : BOOL) : BOOL; stdcall; external cCrypt32;

// Used to release a handle acquired by CryptAcquireCertificatePrivateKey.
// The keySpec out-parameter from the acquire call tells us which release API
// to use: CERT_NCRYPT_KEY_SPEC -> NCryptFreeObject, otherwise CryptReleaseContext.
const
  CERT_NCRYPT_KEY_SPEC = $FFFFFFFF;

function CryptReleaseContext(hProv : HCRYPTPROV; dwFlags : DWORD) : BOOL;
  stdcall; external 'advapi32.dll';

function NCryptFreeObject(hObject : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE) : LongInt;
  stdcall; external cNCrypt;

function CertGetCertificateChain(hChainEngine : HCERTCHAINENGINE;
                                 pCertContext : PCCERT_CONTEXT;
                                 pTime : PFileTime; hAdditionalStore : HCERTSTORE;
                                 pChainPara : PCERT_CHAIN_PARA; dwFlags : DWORD;
                                 pvReserved : Pointer;
                                 var ppChainContext : PCERT_CHAIN_CONTEXT) : BOOL; stdcall; external cCrypt32;

procedure CertFreeCertificateChain(pChainContext : PCERT_CHAIN_CONTEXT); stdcall; external cCrypt32;

function CertVerifyCertificateChainPolicy(pszPolicyOID : PAnsiChar;
                                          pChainContext : PCERT_CHAIN_CONTEXT;
                                          pPolicyPara : PCERT_CHAIN_POLICY_PARA;
                                          pPolicyStatus : PCERT_CHAIN_POLICY_STATUS) : BOOL; stdcall; external cCrypt32;

// PFX support
function PFXImportCertStore(pPFX : PCRYPT_DATA_BLOB; szPassword : PWideChar;
                            dwFlags : DWORD) : HCERTSTORE; stdcall; external cCrypt32;

// CMS / SignedData
function CryptSignMessage(pSignPara : PCRYPT_SIGN_MESSAGE_PARA;
                          fDetachedSignature : BOOL;
                          cToBeSigned : DWORD;
                          rgpbToBeSigned : Pointer;
                          rgcbToBeSigned : Pointer;
                          pbSignedBlob : PByte;
                          var pcbSignedBlob : DWORD) : BOOL; stdcall; external cCrypt32;

function CryptVerifyDetachedMessageSignature(pVerifyPara : PCRYPT_VERIFY_MESSAGE_PARA;
                                             dwSignerIndex : DWORD;
                                             pbDetachedSignBlob : PByte;
                                             cbDetachedSignBlob : DWORD;
                                             cToBeSigned : DWORD;
                                             rgpbToBeSigned : Pointer;
                                             rgcbToBeSigned : Pointer;
                                             ppSignerCert : PPCCERT_CONTEXT) : BOOL; stdcall; external cCrypt32;

function CryptMsgOpenToDecode(dwMsgEncodingType, dwFlags, dwMsgType : DWORD;
                              hCryptProv : HCRYPTPROV_LEGACY;
                              pRecipientInfo : Pointer;
                              pStreamInfo : Pointer) : HCRYPTMSG; stdcall; external cCrypt32;

function CryptMsgUpdate(hCryptMsg : HCRYPTMSG; pbData : PByte; cbData : DWORD;
                        fFinal : BOOL) : BOOL; stdcall; external cCrypt32;

function CryptMsgGetParam(hCryptMsg : HCRYPTMSG; dwParamType, dwIndex : DWORD;
                          pvData : Pointer; var pcbData : DWORD) : BOOL; stdcall; external cCrypt32;

function CryptMsgClose(hCryptMsg : HCRYPTMSG) : BOOL; stdcall; external cCrypt32;

function CryptMsgControl(hCryptMsg : HCRYPTMSG; dwFlags : DWORD;
                         dwCtrlType : DWORD; pvCtrlPara : Pointer) : BOOL; stdcall; external cCrypt32;

// RFC3161 timestamping
function CryptRetrieveTimeStamp(wszUrl : PWideChar; dwRetrievalFlags : DWORD;
                                dwTimeout : DWORD; pszHashId : PAnsiChar;
                                pPara : PCRYPT_TIMESTAMP_PARA;
                                pbData : PByte; cbData : DWORD;
                                var ppTsContext : PCRYPT_TIMESTAMP_CONTEXT;
                                ppTsSigner : PPCCERT_CONTEXT;
                                phStore : Pointer) : BOOL; stdcall; external cCrypt32;

function CryptVerifyTimeStampSignature(pbTSContentInfo : PByte; cbTSContentInfo : DWORD;
                                       pbData : PByte; cbData : DWORD;
                                       hAdditionalStore : HCERTSTORE;
                                       var ppTsContext : PCRYPT_TIMESTAMP_CONTEXT;
                                       ppTsSigner : PPCCERT_CONTEXT;
                                       phStore : Pointer) : BOOL; stdcall; external cCrypt32;

procedure CryptMemFree(pv : Pointer); stdcall; external cCrypt32;

// ---------------------------------------------------------------------------
// Normaliz — Unicode normalization (used by manifest path validation)
// ---------------------------------------------------------------------------

const
  cNormaliz = 'normaliz.dll';

  // NORM_FORM
  NormalizationOther : Integer = 0;
  NormalizationC     : Integer = 1;
  NormalizationD     : Integer = 2;
  NormalizationKC    : Integer = 5;
  NormalizationKD    : Integer = 6;

function NormalizeString(NormForm : Integer; lpSrcString : PWideChar;
                         cwSrcLength : Integer; lpDstString : PWideChar;
                         cwDstLength : Integer) : Integer; stdcall;
  external cNormaliz;

function IsNormalizedString(NormForm : Integer; lpString : PWideChar;
                            cwLength : Integer) : BOOL; stdcall;
  external cNormaliz;

implementation

end.
