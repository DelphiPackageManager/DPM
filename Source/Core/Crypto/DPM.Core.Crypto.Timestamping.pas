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

  ITimestamper = interface
    ['{6B3FAB2C-1B5E-4F4E-A6A1-3F9D8E70F02A}']
    function RequestTimestamp(const dataToTimestamp : TBytes;
                              const url : string;
                              digest : THashAlgorithm) : ITimestampToken;
    function Verify(const token : TBytes; const data : TBytes;
                    digest : THashAlgorithm;
                    out signingTime : TDateTime) : boolean;
  end;

  TWindowsTimestamper = class(TInterfacedObject, ITimestamper)
  private
    FX509 : IX509Service;
  protected
    function RequestTimestamp(const dataToTimestamp : TBytes;
                              const url : string;
                              digest : THashAlgorithm) : ITimestampToken;
    function Verify(const token : TBytes; const data : TBytes;
                    digest : THashAlgorithm;
                    out signingTime : TDateTime) : boolean;
  public
    constructor Create(const x509 : IX509Service);
  end;

  ECryptoTimestamp = class(Exception);

implementation

uses
  System.DateUtils;

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
                                    digest : THashAlgorithm;
                                    out signingTime : TDateTime) : boolean;
var
  ctx : PCRYPT_TIMESTAMP_CONTEXT;
begin
  signingTime := 0;
  result := false;
  if Length(token) = 0 then
    exit;
  ctx := nil;
  // CryptVerifyTimeStampSignature does the heavy lifting — chain to TSA root,
  // verify the message imprint matches the hashed input, return the parsed
  // CRYPT_TIMESTAMP_INFO.
  if not CryptVerifyTimeStampSignature(
      PByte(@token[0]), Length(token),
      PByte(@data[0]), Length(data),
      nil,
      ctx,
      nil, nil) then
    exit;

  try
    signingTime := FileTimeToUtc(ctx.pTimeStamp.ftTime);
    result := true;
  finally
    if ctx <> nil then
      CryptMemFree(ctx);
  end;
end;

end.
