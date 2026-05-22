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

unit DPM.Core.Crypto.Provider.Signotaur;

// Phase 3 §3.3 v2 — Signotaur signing provider.
//
// Signotaur (https://www.finalbuilder.com/signotaur) is VSoft's code-signing
// server application.
// The applicatgion currently exposes gRPC; REST endpoints are being added.
// This provider implements against the REST shape that mirrors the gRPC protos:
//
//   POST {endpoint}/api/v1/cert/get   (GetCertRequest  -> GetCertResponse)
//   POST {endpoint}/api/v1/sign       (SignRequest     -> SignResponse)
//
// JSON field names mirror the proto field names verbatim (PascalCase, e.g.
// "Thumbprint", "Certificate", "Digest"). Bytes fields are base64-encoded
// (standard, NOT base64url — different from Azure KV).
//
// The certificate selector is one of: Thumbprint, Subject, Label (exactly
// one). The server picks based on whichever is non-empty. We surface a
// CLI option per selector.

interface

uses
  WinApi.Windows,
  System.SysUtils,
  DPM.Core.Logging,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  TSignotaurOptions = record
    Endpoint     : string;     // e.g. https://signotaur.example.com
    ApiKey       : string;     // sourced from --api-key-env
    // Exactly one of the three selectors should be non-empty. If multiple
    // are set, server preference is Thumbprint > Label > Subject.
    Thumbprint   : string;
    Subject      : string;
    Label_       : string;     // underscore — `Label` is a Delphi reserved word in some contexts
    // Optional metadata the server logs / audits alongside the sign call.
    FileName     : string;
    FileSizeBytes : Int64;
    FileVersion  : string;
  end;

  TSignotaurSigningProvider = class(TInterfacedObject, ISigningProvider)
  private
    FLogger : ILogger;
    FX509 : IX509Service;
    FOptions : TSignotaurOptions;
    FCert : ICertificate;       // lazy-loaded
    FResolvedThumbprint : string;   // server-returned thumbprint, used for SignDigest
    function GetCertificate : ICertificate;
    function HashAlgName(alg : THashAlgorithm) : string;
  protected
    function Certificate : ICertificate;
    function IsLocal : boolean;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
  public
    constructor Create(const logger : ILogger;
                       const x509 : IX509Service;
                       const options : TSignotaurOptions);
  end;

  ESignotaur = class(ECryptoProvider);

const
  cSignotaurClientVersion = 'DPM/0.6.0';
  cSignotaurApiBase       = '/api/v1';

implementation

uses
  System.Classes,
  JsonDataObjects,
  VSoft.Base64,
  VSoft.HttpClient,
  VSoft.CancellationToken,
  DPM.Core.Utils.Base64Url;

// Signotaur uses standard base64 (not base64url) for binary payloads.
function StdBase64Encode(const bytes : TBytes) : string;
begin
  result := TBase64.Encode(bytes, false);
end;

function StdBase64Decode(const value : string) : TBytes;
begin
  result := TBase64.Decode(value);
end;

constructor TSignotaurSigningProvider.Create(const logger : ILogger;
                                              const x509 : IX509Service;
                                              const options : TSignotaurOptions);
begin
  if (logger = nil) or (x509 = nil) then
    raise ESignotaur.Create('TSignotaurSigningProvider: missing dependencies');
  if options.Endpoint = '' then
    raise ESignotaur.Create('Signotaur: --endpoint is required');
  if options.ApiKey = '' then
    raise ESignotaur.Create('Signotaur: API key is required (set via --api-key-env)');
  if (options.Thumbprint = '') and (options.Subject = '') and (options.Label_ = '') then
    raise ESignotaur.Create('Signotaur: one of --thumbprint, --subject, --label is required');
  inherited Create;
  FLogger := logger;
  FX509 := x509;
  FOptions := options;
end;

function TSignotaurSigningProvider.HashAlgName(alg : THashAlgorithm) : string;
begin
  // Signotaur expects "SHA256" / "SHA384" / "SHA512" per the gRPC field doc.
  case alg of
    haSha256 : result := 'SHA256';
    haSha384 : result := 'SHA384';
    haSha512 : result := 'SHA512';
  else
    raise ESignotaur.CreateFmt('Unsupported digest algorithm: %d', [Ord(alg)]);
  end;
end;

function TSignotaurSigningProvider.GetCertificate : ICertificate;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  cancellationToken : ICancellationToken;
  reqDoc, respDoc : TJsonObject;
  reqBody : string;
  cerBase64 : string;
  derBytes : TBytes;
  resultCode : integer;
begin
  if FCert <> nil then
  begin
    result := FCert;
    exit;
  end;

  // GetCertRequest: only non-empty selectors are sent. ApiKey + ClientVersion
  // are always present.
  reqDoc := TJsonObject.Create;
  try
    reqDoc.S['ApiKey'] := FOptions.ApiKey;
    reqDoc.S['ClientVersion'] := cSignotaurClientVersion;
    if FOptions.Thumbprint <> '' then
      reqDoc.S['Thumbprint'] := FOptions.Thumbprint;
    if FOptions.Subject <> '' then
      reqDoc.S['Subject'] := FOptions.Subject;
    if FOptions.Label_ <> '' then
      reqDoc.S['Label'] := FOptions.Label_;
    reqBody := reqDoc.ToJSON(False);
  finally
    reqDoc.Free;
  end;

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(FOptions.Endpoint);
  request := httpClient.CreateRequest(cSignotaurApiBase + '/cert/get')
                .WithHeader('Accept', 'application/json')
                .WithBody(reqBody, TEncoding.UTF8)
                .WithContentType('application/json', 'utf-8');

  FLogger.Verbose('Signotaur POST ' + FOptions.Endpoint + cSignotaurApiBase + '/cert/get');
  response := httpClient.Post(request, cancellationToken);
  if response.StatusCode <> 200 then
    raise ESignotaur.CreateFmt(
      'Signotaur cert lookup failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  respDoc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  try
    if respDoc.Contains('Result') then
      resultCode := respDoc.I['Result']
    else
      resultCode := -1;
    if resultCode <> 0 then
      raise ESignotaur.CreateFmt(
        'Signotaur cert lookup returned error code %d: %s',
        [resultCode, respDoc.S['Message']]);

    if not respDoc.Contains('Certificate') then
      raise ESignotaur.Create('Signotaur cert response missing Certificate field');

    cerBase64 := respDoc.S['Certificate'];
    derBytes := StdBase64Decode(cerBase64);
    FCert := FX509.LoadCertificateFromDer(derBytes);

    // Use the server's authoritative thumbprint for the SignRequest, even
    // if the caller selected by Subject/Label. Otherwise the server would
    // re-resolve and we'd lose audit-trail accuracy.
    if respDoc.Contains('Thumbprint') then
      FResolvedThumbprint := respDoc.S['Thumbprint'];
    if FResolvedThumbprint = '' then
      FResolvedThumbprint := FOptions.Thumbprint;

    FLogger.Verbose('Signotaur cert loaded: ' + FCert.SubjectDistinguishedName +
      ' (thumbprint=' + FResolvedThumbprint + ')');
  finally
    respDoc.Free;
  end;
  result := FCert;
end;

function TSignotaurSigningProvider.Certificate : ICertificate;
begin
  result := GetCertificate;
end;

function TSignotaurSigningProvider.IsLocal : boolean;
begin
  result := false;
end;

function TSignotaurSigningProvider.AcquirePrivateKey(
  out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  out keySpec : DWORD;
  out callerMustFree : boolean) : boolean;
begin
  keyHandle := 0;
  keySpec := 0;
  callerMustFree := false;
  result := false;
end;

function TSignotaurSigningProvider.SignDigest(const digest : TBytes;
                                               digestAlgorithm : THashAlgorithm) : TBytes;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  cancellationToken : ICancellationToken;
  reqDoc, respDoc : TJsonObject;
  reqBody : string;
  sigBase64 : string;
  resultCode : integer;
begin
  // Ensure cert + resolved thumbprint are populated before signing.
  GetCertificate;
  if FResolvedThumbprint = '' then
    raise ESignotaur.Create('Signotaur: cert lookup did not yield a thumbprint for signing');

  reqDoc := TJsonObject.Create;
  try
    reqDoc.S['ApiKey'] := FOptions.ApiKey;
    reqDoc.S['Thumbprint'] := FResolvedThumbprint;
    reqDoc.S['Digest'] := StdBase64Encode(digest);
    reqDoc.S['DigestHashAlgorithm'] := HashAlgName(digestAlgorithm);
    reqDoc.S['ClientVersion'] := cSignotaurClientVersion;
    if FOptions.FileName <> '' then
      reqDoc.S['FileName'] := FOptions.FileName;
    if FOptions.FileSizeBytes > 0 then
      reqDoc.L['FileSizeInBytes'] := FOptions.FileSizeBytes;
    if FOptions.FileVersion <> '' then
      reqDoc.S['FileVersion'] := FOptions.FileVersion;
    reqBody := reqDoc.ToJSON(False);
  finally
    reqDoc.Free;
  end;

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(FOptions.Endpoint);
  request := httpClient.CreateRequest(cSignotaurApiBase + '/sign')
                .WithHeader('Accept', 'application/json')
                .WithBody(reqBody, TEncoding.UTF8)
                .WithContentType('application/json', 'utf-8');

  FLogger.Information('Signing digest via Signotaur (' + HashAlgName(digestAlgorithm) + ')');
  FLogger.Verbose('Signotaur POST ' + FOptions.Endpoint + cSignotaurApiBase + '/sign');
  response := httpClient.Post(request, cancellationToken);
  if response.StatusCode <> 200 then
    raise ESignotaur.CreateFmt(
      'Signotaur sign failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  respDoc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  try
    if respDoc.Contains('Result') then
      resultCode := respDoc.I['Result']
    else
      resultCode := -1;
    if resultCode <> 0 then
      raise ESignotaur.CreateFmt(
        'Signotaur sign returned error code %d: %s',
        [resultCode, respDoc.S['Message']]);
    if not respDoc.Contains('SignedData') then
      raise ESignotaur.Create('Signotaur sign response missing SignedData field');
    sigBase64 := respDoc.S['SignedData'];
    result := StdBase64Decode(sigBase64);
  finally
    respDoc.Free;
  end;
end;

end.
