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

unit DPM.Core.Crypto.Cms.Interfaces;

interface

uses
  System.SysUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  TCmsAttribute = record
    Oid    : AnsiString;
    // CMS attributes are `Attribute SEQUENCE { OID, SET OF AttributeValue }`
    // — i.e. the value field is intrinsically a SET that may carry one or
    // many values. Most DPM attributes are single-valued, so Value is kept
    // as a convenience pointing at Values[0]. Multi-valued attributes
    // (e.g. dpmVerifiedAuthorSigHash carrying one hash per attested author
    // signature) need to walk Values themselves — use FindSignedAttributeValues
    // for that.
    Value  : TBytes;             // shortcut: Values[0] if any, else nil
    Values : TArray<TBytes>;     // every AttributeValue under this OID
  end;
  TCmsAttributes = TArray<TCmsAttribute>;

  ICmsSignedData = interface
    ['{2E1B57A9-B61F-4DC0-8FB1-2A7FED8E4DEE}']
    function SignerCertificate : ICertificate;
    function EmbeddedCertificates : TArray<ICertificate>;
    function SignedAttributes : TCmsAttributes;
    function UnsignedAttributes : TCmsAttributes;
    function FindSignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
    function FindUnsignedAttribute(const oid : AnsiString; out value : TBytes) : boolean;
    /// <summary>
    /// Returns every AttributeValue carried by the named signed attribute.
    /// CMS attributes are SET OF AttributeValue — most DPM attributes have a
    /// single value (use FindSignedAttribute then), but the author<->repo
    /// binding (dpmVerifiedAuthorSigHash) carries one hash per attested
    /// author signature, so the verifier walks the whole set.
    /// </summary>
    function FindSignedAttributeValues(const oid : AnsiString; out values : TArray<TBytes>) : boolean;
    function DerBytes : TBytes;
    function DigestAlgorithm : THashAlgorithm;
    // Per RFC3161 + szOID_RFC3161_counterSign convention, the timestamp
    // covers the signer info's `encryptedDigest` (signature value) — not
    // the whole CMS. Adding/removing unsigned attributes does NOT change
    // this field, so sign-time and verify-time hashes match.
    function EncryptedDigest : TBytes;
  end;

  ICmsService = interface
    ['{50D27E5F-7CD9-4D88-9D8A-78D8E0D7D8FE}']
    function Sign(const content : TBytes;
                  const provider : ISigningProvider;
                  const signedAttributes : TCmsAttributes;
                  digest : THashAlgorithm) : TBytes;

    function VerifyDetached(const der : TBytes; const content : TBytes;
                            out signerCert : ICertificate) : boolean;

    function Decode(const der : TBytes) : ICmsSignedData;

    /// <summary>
    /// Pulls just the SignerInfo's encryptedDigest out of a CMS blob, without
    /// constructing an ICmsSignedData or walking signer-certs / attributes.
    /// Used for RFC3161 timestamp imprint computation, where a full decode
    /// is wasteful and exposes us to pointer-fixup edge cases on
    /// hand-assembled CMS structures (the remote-signer path).
    /// </summary>
    function ExtractEncryptedDigest(const der : TBytes) : TBytes;

    procedure AddUnsignedAttribute(var der : TBytes;
                                   const oid : AnsiString; const value : TBytes);
  end;

  ECryptoCms = class(Exception);

implementation

end.
