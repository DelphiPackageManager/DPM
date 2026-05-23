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
    Oid   : AnsiString;
    Value : TBytes;       // raw DER for the attribute value (OCTET STRING typically)
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

    procedure AddUnsignedAttribute(var der : TBytes;
                                   const oid : AnsiString; const value : TBytes);
  end;

  ECryptoCms = class(Exception);

implementation

end.
