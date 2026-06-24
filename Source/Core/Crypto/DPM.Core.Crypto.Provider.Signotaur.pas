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
// JSON field names are camelCase — the Signotaur server's REST surface
// canonicalises camelCase regardless of the gRPC proto's PascalCase, because
// the underlying .NET JsonNamingPolicy.CamelCase serializer rewrites
// PascalCase property names on emit. JsonDataObjects lookups are
// case-sensitive so the names below MUST match the wire format exactly.
// Bytes fields are base64-encoded (standard, NOT base64url — different from
// Azure KV).
//
// Authentication: the API key is sent in the Authorization request header, not
// in the JSON body. Keeps it out of any body-logging the server / proxies do.
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
    // Trust TLS chains that fail validation (self-signed, untrusted CA,
    // hostname mismatch). For dev / on-prem Signotaur deployments only —
    // production endpoints should always present a chained-to-trusted-CA cert.
    AllowSelfSignedCertificates : boolean;
  end;

  TSignotaurSigningProvider = class(TInterfacedObject, ISigningProvider)
  private
    FLogger : ILogger;
    FX509 : IX509Service;
    FOptions : TSignotaurOptions;
    FCert : ICertificate;       // lazy-loaded
    FResolvedThumbprint : string;   // server-returned thumbprint, used for SignDigest
    // Per-call audit metadata pushed in by the signing service immediately
    // before each SignDigest. Empty/0 means fall back to FOptions defaults
    // (e.g. single-file CLI use that still pre-populates options).
    FCallFileName : string;
    FCallFileSize : Int64;
    function GetCertificate : ICertificate;
    function HashAlgName(alg : THashAlgorithm) : string;
  protected
    function Certificate : ICertificate;
    function IsLocal : boolean;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
    // Signotaur signs server-side under an API key; no client-side session
    // state to maintain. BeginSession/EndSession are no-ops.
    procedure BeginSession;
    procedure EndSession;
    // Per-call audit context. Stored until the next SignDigest call so each
    // file in a batch sign gets its own FileName / FileSize on the server-
    // side audit log.
    procedure SetSigningContext(const fileName : string; fileSize : Int64);
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
  // Strip any trailing slash so endpoint + cSignotaurApiBase doesn't produce a
  // double slash (e.g. https://host:82//api/v1/...). Harmless on most servers
  // but it muddies the logs and some proxies reject it.
  while (FOptions.Endpoint <> '') and (FOptions.Endpoint[Length(FOptions.Endpoint)] = '/') do
    FOptions.Endpoint := Copy(FOptions.Endpoint, 1, Length(FOptions.Endpoint) - 1);
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

  // GetCertRequest: only non-empty selectors are sent. ApiKey travels in the
  // Authorization header (see WithHeader below); ClientVersion is part of the body.
  reqDoc := TJsonObject.Create;
  try
    reqDoc.S['clientVersion'] := cSignotaurClientVersion;
    if FOptions.Thumbprint <> '' then
      reqDoc.S['thumbprint'] := FOptions.Thumbprint;
    if FOptions.Subject <> '' then
      reqDoc.S['subject'] := FOptions.Subject;
    if FOptions.Label_ <> '' then
      reqDoc.S['label'] := FOptions.Label_;
    reqBody := reqDoc.ToJSON(False);
  finally
    reqDoc.Free;
  end;

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(FOptions.Endpoint);
  if FOptions.AllowSelfSignedCertificates then
  begin
    httpClient.AllowSelfSignedCertificates := true;
//    FLogger.Warning('Signotaur: TLS certificate validation is disabled ' +
//                    '(--allow-untrusted). Use only for dev / on-prem.');
  end;
  request := httpClient.CreateRequest(cSignotaurApiBase + '/cert/get')
                .WithHeader('Accept', 'application/json')
                .WithHeader('Authorization', 'Bearer ' + FOptions.ApiKey)
                .WithBody(reqBody, TEncoding.UTF8)
                .WithContentType('application/json', 'utf-8');

  FLogger.Debug('Signotaur POST ' + FOptions.Endpoint + cSignotaurApiBase + '/cert/get');
  try
    response := httpClient.Post(request, cancellationToken);
  except
    on e : Exception do
    begin
      FLogger.Error(Format('Signotaur POST raised %s: %s', [e.ClassName, e.Message]));
      raise EProviderFatal.CreateFmt(
        'Signotaur cert lookup HTTP call failed (%s): %s',
        [e.ClassName, e.Message]);
    end;
  end;
  FLogger.Debug(Format('Signotaur HTTP %d response (%d bytes)', [response.StatusCode, Length(response.Response)]));
  if Length(response.Response) > 0 then
    FLogger.Verbose('Signotaur response body: ' + response.Response);
  // A 405 (Method Not Allowed) on the REST endpoint means the server does not
  // expose the /api/v1 REST surface at all — that is Signotaur v1, which only
  // speaks gRPC. DPM signs over REST, so the user needs to upgrade.
  if response.StatusCode = 405 then
    raise EProviderFatal.Create(
      'Signotaur returned HTTP 405 (Method Not Allowed) for the REST endpoint. ' +
      'This Signotaur server does not support the REST signing API. ' +
      'Signotaur v2 or later is required to sign with DPM.');
  if response.StatusCode <> 200 then
    // HTTP-level failure: bad endpoint, bad API key, server down. None of
    // these get better by trying the next file — raise as fatal so the
    // batch loop aborts instead of generating N more guaranteed failures.
    raise EProviderFatal.CreateFmt(
      'Signotaur cert lookup failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  try
    respDoc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
      raise ESignotaur.CreateFmt(
        'Signotaur cert response is not valid JSON (%s). Raw body: %s',
        [e.Message, response.Response]);
  end;
  try
    if respDoc.Contains('result') then
      resultCode := respDoc.I['result']
    else
      resultCode := -1;
    if resultCode <> 0 then
      raise ESignotaur.CreateFmt(
        'Signotaur cert lookup returned error code %d: %s. Raw body: %s',
        [resultCode, respDoc.S['message'], response.Response]);

    if not respDoc.Contains('certificate') then
      raise ESignotaur.CreateFmt(
        'Signotaur cert response missing certificate field. Raw body: %s',
        [response.Response]);

    cerBase64 := respDoc.S['certificate'];
    derBytes := StdBase64Decode(cerBase64);
    FCert := FX509.LoadCertificateFromDer(derBytes);

    // Use the server's authoritative thumbprint for the SignRequest, even
    // if the caller selected by Subject/Label. Otherwise the server would
    // re-resolve and we'd lose audit-trail accuracy.
    if respDoc.Contains('thumbprint') then
      FResolvedThumbprint := respDoc.S['thumbprint'];
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

procedure TSignotaurSigningProvider.BeginSession;
begin
  // No-op. Each REST call carries its own API-key header.
end;

procedure TSignotaurSigningProvider.EndSession;
begin
  // No-op.
end;

procedure TSignotaurSigningProvider.SetSigningContext(const fileName : string;
                                                       fileSize : Int64);
begin
  FCallFileName := fileName;
  FCallFileSize := fileSize;
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
  effectiveFileName : string;
  effectiveFileSize : Int64;
begin
  FLogger.Verbose(Format('  Signotaur.SignDigest: entered (digest=%d bytes, alg=%s)',
    [Length(digest), HashAlgName(digestAlgorithm)]));
  // Ensure cert + resolved thumbprint are populated before signing.
  GetCertificate;
  if FResolvedThumbprint = '' then
    raise ESignotaur.Create('Signotaur: cert lookup did not yield a thumbprint for signing');

  // Prefer per-call context (set by the signing service for each file in a
  // batch); fall back to the constructor-time defaults from FOptions if the
  // caller didn't supply anything.
  if FCallFileName <> '' then
    effectiveFileName := FCallFileName
  else
    effectiveFileName := FOptions.FileName;
  if FCallFileSize > 0 then
    effectiveFileSize := FCallFileSize
  else
    effectiveFileSize := FOptions.FileSizeBytes;

  reqDoc := TJsonObject.Create;
  try
    // ApiKey travels in the Authorization header, not the body.
    reqDoc.S['thumbprint'] := FResolvedThumbprint;
    reqDoc.S['digest'] := StdBase64Encode(digest);
    reqDoc.S['digestHashAlgorithm'] := HashAlgName(digestAlgorithm);
    reqDoc.S['clientVersion'] := cSignotaurClientVersion;
    if effectiveFileName <> '' then
      reqDoc.S['fileName'] := effectiveFileName;
    if effectiveFileSize > 0 then
      reqDoc.L['fileSizeInBytes'] := effectiveFileSize;
    if FOptions.FileVersion <> '' then
      reqDoc.S['fileVersion'] := FOptions.FileVersion;
    reqBody := reqDoc.ToJSON(False);
  finally
    reqDoc.Free;
  end;

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(FOptions.Endpoint);
  // Self-signed flag must be reapplied per client instance — the cert/get
  // call already warned the user; no need to warn again on every sign.
  if FOptions.AllowSelfSignedCertificates then
    httpClient.AllowSelfSignedCertificates := true;
  request := httpClient.CreateRequest(cSignotaurApiBase + '/sign')
                .WithHeader('Accept', 'application/json')
                .WithHeader('Authorization', 'Bearer ' +FOptions.ApiKey)
                .WithBody(reqBody, TEncoding.UTF8)
                .WithContentType('application/json', 'utf-8');

  FLogger.Information('Signing digest via Signotaur (' + HashAlgName(digestAlgorithm) + ')');
  FLogger.Verbose('Signotaur POST ' + FOptions.Endpoint + cSignotaurApiBase + '/sign');
  // Wrap the POST itself so any HTTP-layer exception (TLS failure, timeout,
  // connection drop) is logged with full diagnostics before it bubbles up.
  // Several batch-sign hangs traced back to exceptions raised here that
  // looked like silent process exits in higher-level logs.
  try
    response := httpClient.Post(request, cancellationToken);
  except
    on e : Exception do
    begin
      FLogger.Error(Format('Signotaur POST raised %s: %s', [e.ClassName, e.Message]));
      raise EProviderFatal.CreateFmt(
        'Signotaur sign HTTP call failed (%s): %s',
        [e.ClassName, e.Message]);
    end;
  end;
  // Log status separately from body — body logging on a huge / binary
  // response has crashed the console writer in the past, and we always
  // want to know the status came back even if the body log fails.
  FLogger.Debug(Format('Signotaur HTTP %d response (%d bytes)',
    [response.StatusCode, Length(response.Response)]));
  if Length(response.Response) > 0 then
    FLogger.Verbose('Signotaur response body: ' + response.Response);
  // 405 on the REST endpoint => Signotaur v1 (gRPC only). See note in GetCertificate.
  if response.StatusCode = 405 then
    raise EProviderFatal.Create(
      'Signotaur returned HTTP 405 (Method Not Allowed) for the REST endpoint. ' +
      'This Signotaur server does not support the REST signing API. ' +
      'Signotaur v2 or later is required to sign with DPM.');
  if response.StatusCode <> 200 then
    // HTTP-level failure: see note on the matching raise in GetCertificate.
    raise EProviderFatal.CreateFmt(
      'Signotaur sign failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  try
    respDoc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  except
    on e : Exception do
      raise ESignotaur.CreateFmt(
        'Signotaur sign response is not valid JSON (%s). Raw body: %s',
        [e.Message, response.Response]);
  end;
  try
    if respDoc.Contains('result') then
      resultCode := respDoc.I['result']
    else
      resultCode := -1;
    if resultCode <> 0 then
      raise ESignotaur.CreateFmt(
        'Signotaur sign returned error code %d: %s. Raw body: %s',
        [resultCode, respDoc.S['message'], response.Response]);
    if not respDoc.Contains('signedData') then
      raise ESignotaur.CreateFmt(
        'Signotaur sign response missing signedData field. Raw body: %s',
        [response.Response]);
    sigBase64 := respDoc.S['signedData'];
    result := StdBase64Decode(sigBase64);
  finally
    respDoc.Free;
  end;
end;

end.
