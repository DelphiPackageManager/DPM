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

unit DPM.Core.Crypto.Provider.Azure;

// Phase 3 §3.3 — Azure Key Vault signing provider.
//
// Three pieces:
//   1. TAzureAccessTokenService  — AAD client-credentials flow + on-disk
//      token cache under %APPDATA%\.dpm\azure-token-cache\.
//   2. TKeyVaultClient            — thin REST wrapper over the KV data plane:
//      GET  /certificates/{name}?api-version=7.4
//      POST /keys/{name}/sign?api-version=7.4
//   3. TKeyVaultSigningProvider   — ISigningProvider implementation. Downloads
//      the leaf cert on first use, delegates SignDigest to the REST call.
//
// AcquirePrivateKey returns false: KV-stored keys are never exposed to the
// client. The CMS layer routes through SignDigest instead (see Phase 3.3
// follow-up for the manual-assembly path that lets this actually produce a
// signed package).

interface

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  DPM.Core.Logging,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces;

type
  TKeyVaultOptions = record
    VaultUrl      : string;   // e.g. https://vsoft.vault.azure.net
    CertificateName : string; // KV certificate name (the cert + key pair)
    KeyVersion    : string;   // optional — empty means latest
    TenantId      : string;
    ClientId      : string;
    // The CLI reads the secret from an env var rather than the command line.
    ClientSecret  : string;
  end;

  TAzureAccessToken = record
    AccessToken : string;
    ExpiresAt   : TDateTime;   // UTC; treat as expired ~60s before this
  end;

  IAzureAccessTokenService = interface
    ['{0B6F9F4E-7D7C-4E3A-9E0B-7E1E9F0C3A11}']
    /// <summary>
    /// Returns a bearer token for the given tenant+client+scope. Reads from
    /// the on-disk cache when fresh; otherwise calls AAD and refreshes.
    /// </summary>
    function GetToken(const opts : TKeyVaultOptions; const scope : string) : TAzureAccessToken;
  end;

  TAzureAccessTokenService = class(TInterfacedObject, IAzureAccessTokenService)
  private
    FLogger : ILogger;
    FLock : TCriticalSection;
    function CacheFilePath(const opts : TKeyVaultOptions; const scope : string) : string;
    function TryReadCache(const path : string; out token : TAzureAccessToken) : boolean;
    procedure WriteCache(const path : string; const token : TAzureAccessToken);
    function FetchTokenFromAAD(const opts : TKeyVaultOptions; const scope : string) : TAzureAccessToken;
  protected
    function GetToken(const opts : TKeyVaultOptions; const scope : string) : TAzureAccessToken;
  public
    constructor Create(const logger : ILogger);
    destructor Destroy; override;
  end;

  TKeyVaultClient = class
  private
    FLogger : ILogger;
    FTokens : IAzureAccessTokenService;
  public
    constructor Create(const logger : ILogger; const tokens : IAzureAccessTokenService);

    /// <summary>
    /// Downloads the leaf certificate (DER bytes) for the named KV certificate.
    /// </summary>
    function DownloadCertificateDer(const opts : TKeyVaultOptions) : TBytes;

    /// <summary>
    /// Signs the digest with the named KV key. `keyVaultAlg` is the KV JWA
    /// name (e.g. "RS256", "ES256"). Returns the raw signature bytes.
    /// </summary>
    function SignDigest(const opts : TKeyVaultOptions;
                        const digest : TBytes;
                        const keyVaultAlg : string) : TBytes;
  end;

  TKeyVaultSigningProvider = class(TInterfacedObject, ISigningProvider)
  private
    FLogger : ILogger;
    FX509 : IX509Service;
    FOptions : TKeyVaultOptions;
    FClient : TKeyVaultClient;
    FCert : ICertificate;       // lazy-loaded on first Certificate call
    function GetCertificate : ICertificate;
    function MapAlgToJwa(digestAlgorithm : THashAlgorithm) : string;
  protected
    function Certificate : ICertificate;
    function IsLocal : boolean;
    function AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                               out keySpec : DWORD;
                               out callerMustFree : boolean) : boolean;
    function SignDigest(const digest : TBytes; digestAlgorithm : THashAlgorithm) : TBytes;
    // KV-backed keys live on the service side; there's no smart-card session
    // to keep alive locally. AAD tokens are already cached by the token
    // service, so a no-op here is correct.
    procedure BeginSession;
    procedure EndSession;
    procedure SetSigningContext(const fileName : string; fileSize : Int64);
  public
    constructor Create(const logger : ILogger;
                       const x509 : IX509Service;
                       const tokens : IAzureAccessTokenService;
                       const options : TKeyVaultOptions);
    destructor Destroy; override;
  end;

  EAzureKeyVault = class(ECryptoProvider);

implementation

uses
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  JsonDataObjects,
  VSoft.Base64,
  VSoft.HttpClient,
  VSoft.CancellationToken,
  DPM.Core.Utils.Base64Url,
  DPM.Core.Utils.DateTime;

const
  cKeyVaultApiVersion = '7.4';
  cAadAuthorityBase   = 'https://login.microsoftonline.com';
  cKeyVaultScope      = 'https://vault.azure.net/.default';

// base64url codec lives in DPM.Core.Utils.Base64Url so the encoding is
// testable in isolation and the helper isn't tied to the Azure provider.

// RFC 3986 percent-encoding for application/x-www-form-urlencoded values.
// Unreserved: ALPHA / DIGIT / "-" / "." / "_" / "~". Everything else escapes
// as %HH over the UTF-8 byte sequence. AAD accepts %20 for spaces (we don't
// use '+' substitution).
function UrlEncodeForm(const value : string) : string;
const
  cHex : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  utf8 : TBytes;
  i : integer;
  b : byte;
  sb : TStringBuilder;
begin
  utf8 := TEncoding.UTF8.GetBytes(value);
  sb := TStringBuilder.Create;
  try
    for i := 0 to Length(utf8) - 1 do
    begin
      b := utf8[i];
      if ((b >= Ord('A')) and (b <= Ord('Z'))) or
         ((b >= Ord('a')) and (b <= Ord('z'))) or
         ((b >= Ord('0')) and (b <= Ord('9'))) or
         (b = Ord('-')) or (b = Ord('_')) or
         (b = Ord('.')) or (b = Ord('~')) then
        sb.Append(Char(b))
      else
      begin
        sb.Append('%');
        sb.Append(cHex[(b shr 4) and $0F]);
        sb.Append(cHex[b and $0F]);
      end;
    end;
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ TAzureAccessTokenService }

constructor TAzureAccessTokenService.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FLock := TCriticalSection.Create;
end;

destructor TAzureAccessTokenService.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TAzureAccessTokenService.CacheFilePath(const opts : TKeyVaultOptions;
                                                 const scope : string) : string;
var
  dir : string;
  fileName : string;
begin
  // %APPDATA%\.dpm\azure-token-cache\{tenant}-{client}-{scopeHash}.json
  dir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) +
         '.dpm' + PathDelim + 'azure-token-cache';
  if not DirectoryExists(dir) then
    ForceDirectories(dir);
  // Filename uses tenant+client only; scope is short enough that we collapse
  // to the dominant one (vault.azure.net) for the cache key. Multi-scope
  // callers can refine later.
  fileName := LowerCase(StringReplace(opts.TenantId, '/', '_', [rfReplaceAll])) +
              '-' +
              LowerCase(StringReplace(opts.ClientId, '/', '_', [rfReplaceAll])) +
              '.json';
  result := IncludeTrailingPathDelimiter(dir) + fileName;
end;

function TAzureAccessTokenService.TryReadCache(const path : string; out token : TAzureAccessToken) : boolean;
var
  doc : TJsonObject;
  text : string;
  expiresIso : string;
begin
  result := false;
  token.AccessToken := '';
  token.ExpiresAt := 0;
  if not FileExists(path) then
    exit;
  try
    text := TFile.ReadAllText(path, TEncoding.UTF8);
    doc := TJsonBaseObject.Parse(text) as TJsonObject;
    try
      if doc.Contains('accessToken') then
        token.AccessToken := doc.S['accessToken'];
      if doc.Contains('expiresAt') then
      begin
        expiresIso := doc.S['expiresAt'];
        TDPMDateTimeUtils.TryISO8601ToDate(expiresIso, token.ExpiresAt, True);
      end;
    finally
      doc.Free;
    end;
    result := (token.AccessToken <> '') and (token.ExpiresAt > 0);
  except
    on Exception do
      // Corrupt cache — treat as miss; will be overwritten next fetch.
      result := false;
  end;
end;

procedure TAzureAccessTokenService.WriteCache(const path : string; const token : TAzureAccessToken);
var
  doc : TJsonObject;
  tempPath : string;
begin
  doc := TJsonObject.Create;
  try
    doc.S['accessToken'] := token.AccessToken;
    doc.S['expiresAt'] := TDPMDateTimeUtils.DateToISO8601(token.ExpiresAt, True);
    tempPath := path + '.tmp';
    TFile.WriteAllText(tempPath, doc.ToJSON(False), TEncoding.UTF8);
    if FileExists(path) then
      DeleteFile(path);
    if not RenameFile(tempPath, path) then
      raise EAzureKeyVault.CreateFmt('Failed to write token cache: %s', [path]);
  finally
    doc.Free;
  end;
end;

function TAzureAccessTokenService.FetchTokenFromAAD(const opts : TKeyVaultOptions;
                                                     const scope : string) : TAzureAccessToken;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  body : string;
  cancellationToken : ICancellationToken;
  doc : TJsonObject;
  expiresIn : integer;
begin
  result.AccessToken := '';
  result.ExpiresAt := 0;

  if (opts.TenantId = '') or (opts.ClientId = '') or (opts.ClientSecret = '') then
    raise EAzureKeyVault.Create(
      'Azure Key Vault: tenant id, client id and client secret are required ' +
      '(set via env var named by --client-secret-env)');

  // client_credentials flow per RFC 6749 §4.4 + AAD v2 token endpoint.
  body :=
    'grant_type=client_credentials' +
    '&client_id=' + UrlEncodeForm(opts.ClientId) +
    '&client_secret=' + UrlEncodeForm(opts.ClientSecret) +
    '&scope=' + UrlEncodeForm(scope);

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(cAadAuthorityBase);
  request := httpClient.CreateRequest('/' + opts.TenantId + '/oauth2/v2.0/token')
                .WithHeader('Accept', 'application/json')
                .WithBody(body, TEncoding.UTF8)
                .WithContentType('application/x-www-form-urlencoded', '');

  FLogger.Verbose('AAD POST ' + cAadAuthorityBase + '/' + opts.TenantId + '/oauth2/v2.0/token');
  response := httpClient.Post(request, cancellationToken);
  if response.StatusCode <> 200 then
    raise EAzureKeyVault.CreateFmt(
      'AAD token request failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  doc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  try
    if not doc.Contains('access_token') then
      raise EAzureKeyVault.Create('AAD response missing access_token');
    result.AccessToken := doc.S['access_token'];
    if doc.Contains('expires_in') then
      expiresIn := doc.I['expires_in']
    else
      expiresIn := 3600;
    // Treat the token as expiring 60s early to avoid races near boundary.
    result.ExpiresAt := IncSecond(TTimeZone.Local.ToUniversalTime(Now), expiresIn - 60);
  finally
    doc.Free;
  end;
end;

function TAzureAccessTokenService.GetToken(const opts : TKeyVaultOptions;
                                            const scope : string) : TAzureAccessToken;
var
  path : string;
  cached : TAzureAccessToken;
begin
  FLock.Enter;
  try
    path := CacheFilePath(opts, scope);
    if TryReadCache(path, cached) and
       (cached.ExpiresAt > TTimeZone.Local.ToUniversalTime(Now)) then
    begin
      FLogger.Verbose('AAD token cache hit (expires ' +
        TDPMDateTimeUtils.DateToISO8601(cached.ExpiresAt, True) + ')');
      result := cached;
      exit;
    end;

    result := FetchTokenFromAAD(opts, scope);
    WriteCache(path, result);
    FLogger.Verbose('AAD token cached (expires ' +
      TDPMDateTimeUtils.DateToISO8601(result.ExpiresAt, True) + ')');
  finally
    FLock.Leave;
  end;
end;

{ TKeyVaultClient }

constructor TKeyVaultClient.Create(const logger : ILogger;
                                    const tokens : IAzureAccessTokenService);
begin
  inherited Create;
  FLogger := logger;
  FTokens := tokens;
end;

function TKeyVaultClient.DownloadCertificateDer(const opts : TKeyVaultOptions) : TBytes;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  cancellationToken : ICancellationToken;
  token : TAzureAccessToken;
  doc : TJsonObject;
  cerBase64 : string;
begin
  if opts.VaultUrl = '' then
    raise EAzureKeyVault.Create('Key Vault URL is required (--vault-url)');
  if opts.CertificateName = '' then
    raise EAzureKeyVault.Create('Key Vault certificate name is required (--cert-name)');

  token := FTokens.GetToken(opts, cKeyVaultScope);
  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(opts.VaultUrl);
  request := httpClient.CreateRequest('/certificates/' + opts.CertificateName +
                                       '?api-version=' + cKeyVaultApiVersion)
                .WithHeader('Authorization', 'Bearer ' + token.AccessToken)
                .WithHeader('Accept', 'application/json');

  FLogger.Verbose('KV GET ' + opts.VaultUrl + '/certificates/' + opts.CertificateName);
  response := httpClient.Get(request, cancellationToken);
  if response.StatusCode <> 200 then
    raise EAzureKeyVault.CreateFmt(
      'Key Vault certificate fetch failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  doc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  try
    if not doc.Contains('cer') then
      raise EAzureKeyVault.Create('Key Vault certificate response missing "cer" field');
    // The "cer" field is base64-encoded DER (NOT base64url).
    cerBase64 := doc.S['cer'];
    result := TBase64.Decode(cerBase64);
  finally
    doc.Free;
  end;
end;

function TKeyVaultClient.SignDigest(const opts : TKeyVaultOptions;
                                     const digest : TBytes;
                                     const keyVaultAlg : string) : TBytes;
var
  httpClient : IHttpClient;
  request : IHttpRequest;
  response : IHttpResponse;
  cancellationToken : ICancellationToken;
  token : TAzureAccessToken;
  reqDoc : TJsonObject;
  reqBody : string;
  respDoc : TJsonObject;
  endpoint : string;
begin
  token := FTokens.GetToken(opts, cKeyVaultScope);

  reqDoc := TJsonObject.Create;
  try
    reqDoc.S['alg'] := keyVaultAlg;
    reqDoc.S['value'] := TBase64Url.Encode(digest);
    reqBody := reqDoc.ToJSON(False);
  finally
    reqDoc.Free;
  end;

  endpoint := '/keys/' + opts.CertificateName;
  if opts.KeyVersion <> '' then
    endpoint := endpoint + '/' + opts.KeyVersion;
  endpoint := endpoint + '/sign?api-version=' + cKeyVaultApiVersion;

  cancellationToken := TCancellationTokenSourceFactory.Create.Token;
  httpClient := THttpClientFactory.CreateClient(opts.VaultUrl);
  request := httpClient.CreateRequest(endpoint)
                .WithHeader('Authorization', 'Bearer ' + token.AccessToken)
                .WithHeader('Accept', 'application/json')
                .WithBody(reqBody, TEncoding.UTF8)
                .WithContentType('application/json', 'utf-8');

  FLogger.Verbose('KV POST ' + opts.VaultUrl + endpoint + ' (alg=' + keyVaultAlg +
    ', digest=' + IntToStr(Length(digest)) + 'B)');
  response := httpClient.Post(request, cancellationToken);
  if response.StatusCode <> 200 then
    raise EAzureKeyVault.CreateFmt(
      'Key Vault sign failed (HTTP %d): %s',
      [response.StatusCode, response.Response]);

  respDoc := TJsonBaseObject.Parse(response.Response) as TJsonObject;
  try
    if not respDoc.Contains('value') then
      raise EAzureKeyVault.Create('Key Vault sign response missing "value" field');
    result := TBase64Url.Decode(respDoc.S['value']);
  finally
    respDoc.Free;
  end;
end;

{ TKeyVaultSigningProvider }

constructor TKeyVaultSigningProvider.Create(const logger : ILogger;
                                             const x509 : IX509Service;
                                             const tokens : IAzureAccessTokenService;
                                             const options : TKeyVaultOptions);
begin
  if (logger = nil) or (x509 = nil) or (tokens = nil) then
    raise EAzureKeyVault.Create('TKeyVaultSigningProvider: missing dependencies');
  inherited Create;
  FLogger := logger;
  FX509 := x509;
  FOptions := options;
  FClient := TKeyVaultClient.Create(logger, tokens);
end;

destructor TKeyVaultSigningProvider.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TKeyVaultSigningProvider.GetCertificate : ICertificate;
var
  derBytes : TBytes;
begin
  if FCert = nil then
  begin
    derBytes := FClient.DownloadCertificateDer(FOptions);
    FCert := FX509.LoadCertificateFromDer(derBytes);
    FLogger.Verbose('KV cert loaded: ' + FCert.SubjectDistinguishedName);
  end;
  result := FCert;
end;

function TKeyVaultSigningProvider.Certificate : ICertificate;
begin
  result := GetCertificate;
end;

function TKeyVaultSigningProvider.IsLocal : boolean;
begin
  result := false;
end;

function TKeyVaultSigningProvider.AcquirePrivateKey(out keyHandle : HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
                                                     out keySpec : DWORD;
                                                     out callerMustFree : boolean) : boolean;
begin
  // KV-backed keys are never released to the client. The CMS layer should
  // see IsLocal=false and route through SignDigest.
  keyHandle := 0;
  keySpec := 0;
  callerMustFree := false;
  result := false;
end;

procedure TKeyVaultSigningProvider.BeginSession;
begin
  // No-op. AAD token caching is handled inside the token service; each
  // SignDigest reuses the cached token automatically.
end;

procedure TKeyVaultSigningProvider.EndSession;
begin
  // No-op.
end;

procedure TKeyVaultSigningProvider.SetSigningContext(const fileName : string;
                                                      fileSize : Int64);
begin
  // No-op. Key Vault's sign endpoint takes no audit-metadata fields — the
  // KV audit log records the caller's AAD identity + key version, not the
  // file being signed.
end;

function TKeyVaultSigningProvider.MapAlgToJwa(digestAlgorithm : THashAlgorithm) : string;
var
  cert : ICertificate;
  isEcdsa : boolean;
begin
  // Map (key type, digest) -> JWA name. KV distinguishes RSA / RSA-PSS / ECDSA;
  // we use RSA PKCS#1 v1.5 by default unless the cert is ECDSA. PSS is a
  // future option exposed via a CLI flag if needed.
  cert := GetCertificate;
  // Heuristic: ECDSA certs use the id-ecPublicKey OID family. We don't yet
  // surface the public-key OID on ICertificate; default to RSA for now and
  // expose an explicit override in a follow-up when ECDSA test material exists.
  isEcdsa := false;
  if isEcdsa then
    case digestAlgorithm of
      haSha256 : result := 'ES256';
      haSha384 : result := 'ES384';
      haSha512 : result := 'ES512';
    else
      raise EAzureKeyVault.Create('Unsupported ECDSA digest');
    end
  else
    case digestAlgorithm of
      haSha256 : result := 'RS256';
      haSha384 : result := 'RS384';
      haSha512 : result := 'RS512';
    else
      raise EAzureKeyVault.Create('Unsupported RSA digest');
    end;
end;

function TKeyVaultSigningProvider.SignDigest(const digest : TBytes;
                                              digestAlgorithm : THashAlgorithm) : TBytes;
var
  jwaAlg : string;
begin
  jwaAlg := MapAlgToJwa(digestAlgorithm);
  FLogger.Information('Signing digest via Azure Key Vault (' + jwaAlg + ')');
  result := FClient.SignDigest(FOptions, digest, jwaAlg);
end;

end.
