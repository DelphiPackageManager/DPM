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

unit DPM.Core.Crypto.X509.Interfaces;

interface

uses
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms;

type
  TChainResult = (
    crValid,
    crUntrustedRoot,
    crExpired,
    crRevoked,
    crWrongUsage,
    crIncomplete,
    crUnknownError
  );

  TCertStoreLocation = (
    cslCurrentUser,
    cslLocalMachine
  );

  ICertificate = interface
    ['{B1F31C8C-25A7-4F40-A19B-7E40C2E4D8E8}']
    function SubjectCommonName : string;
    function SubjectDistinguishedName : string;
    function IssuerDistinguishedName : string;
    function Thumbprint : string;                     // SHA-1, hex, display only
    function SerialNumberHex : string;
    function SpkiHash(algorithm : THashAlgorithm) : TBytes;
    function NotBefore : TDateTime;                   // UTC
    function NotAfter : TDateTime;                    // UTC
    function HasCodeSigningEku : boolean;
    function RawDerBytes : TBytes;
    // Win32 handle access — only the X509 / CMS layer should call this.
    function GetContext : PCCERT_CONTEXT;
  end;

  ICertificateStore = interface
    ['{6B2A7C20-04B6-43FB-A0F1-2C57F6B0E11C}']
    function FindByThumbprint(const thumbprint : string) : ICertificate;
    function FindBySpki(const spkiHash : TBytes; algorithm : THashAlgorithm) : ICertificate;
    function GetHandle : HCERTSTORE;
  end;

  ICertificateChain = interface
    ['{0F0A2A88-04F9-4EC3-83DA-7E0C4D7F0C8D}']
    function Build(const cert : ICertificate;
                   const additionalCerts : array of ICertificate) : TChainResult;
    function VerifyForCodeSigning(asOfTime : TDateTime) : TChainResult;
    function ChainCertificates : TArray<ICertificate>;  // leaf first, root last
    function RootCertificate : ICertificate;
    function LastErrorMessage : string;
  end;

  IX509Service = interface
    ['{2A1A0F7C-1F1C-4F3F-91A8-3C71E3AE7BA1}']
    function OpenSystemStore(location : TCertStoreLocation; const storeName : string) : ICertificateStore;
    function OpenPfxStore(const pfxBytes : TBytes; const password : string) : ICertificateStore;
    function LoadCertificateFromDer(const der : TBytes) : ICertificate;
    function CreateChain : ICertificateChain;
  end;

  ECryptoX509 = class(Exception);

implementation

end.
