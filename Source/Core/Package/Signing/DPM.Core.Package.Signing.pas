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

unit DPM.Core.Package.Signing;

// Sign and verify orchestration. The only unit that knows about both
// packages and crypto.
//
// Sign:
//   1. open dpkg, find dpm-manifest.json bytes
//   2. CMS-sign them with the dpmSignatureRole=author attribute
//   3. RFC3161 timestamp
//   4. write signatures/author-<n>.p7s back into the dpkg
//
// Verify:
//   1. archive-format rules (V-9..V-13)
//   2. extract manifest bytes, parse with hardened parser (V-1, V-2)
//   3. file-set equality + integrity (V-3, V-4)
//   4. every signatures/* must be a well-formed CMS blob (V-14)
//   5..7. CMS verify, chain build, timestamp verify (V-15..V-17)
//   8..9. apply trust policy (V-18..V-22)

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.Cms.Interfaces,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Crypto.Timestamping,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Archive,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Package.Signing.Interfaces;

type
  TPackageSigningService = class(TInterfacedObject, IPackageSigningService)
  private
    FLogger : ILogger;
    FHashing : IHashingService;
    FX509 : IX509Service;
    FCms : ICmsService;
    FTimestamper : ITimestamper;
    FManifest : IManifestService;
    FArchive : IArchiveValidator;
    FReceipt : IReceiptService;
    FTrustPolicy : ITrustPolicyService;
    // P3 §3.2 — current-call runtime flags. Set by the overloaded entry
    // point before delegating to the shared VerifyPackage path so the
    // inner VerifyOneSignature can read e.g. Offline without changing every
    // intermediate signature.
    FActiveFlags : TVerifyFlags;
    function ReadManifestBytes(const archivePath : string; out bytes : TBytes) : boolean;
    function ReadSignatureBlobs(const archivePath : string;
                                out blobs : IList<TPair<string, TBytes>>) : boolean;
    procedure WriteSignatureToArchive(const archivePath : string;
                                      const blobName : string;
                                      const blobBytes : TBytes);
    function VerifyOneSignature(const der : TBytes; const manifestBytes : TBytes;
                                out info : TSignatureInfo) : boolean;
    procedure VerifyFileSetIntegrity(const archivePath : string;
                                     const manifest : IPackageManifest);
    // Phase 2: parse the dpmRepositoryAttestation signed attribute off a
    // trusted repository signature and attach to info.Attestation.
    procedure ReadRepositoryAttestation(const der : TBytes; var info : TSignatureInfo);
    // Phase 2: central truth table that turns (mode, hasAuthor, hasRepo)
    // into the verification outcome. Pulled out so the same function is
    // exhaustively covered by tests.
    procedure EvaluateRequirements(const policy : TTrustPolicy;
                                    hasAnySignature : boolean;
                                    hasValidAuthor : boolean;
                                    hasValidTrustedRepo : boolean;
                                    var outcome : TVerificationOutcome;
                                    var reason : string);
  protected
    procedure SignPackage(const packageFilePath : string;
                          const provider : ISigningProvider;
                          const options : ISignOptions);
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy) : TVerificationResult; overload;
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy;
                           const flags : TVerifyFlags) : TVerificationResult; overload;
    function QuickRecheck(const cacheFolder : string;
                          const policy : TTrustPolicy) : boolean;
  public
    constructor Create(const logger : ILogger;
                       const hashing : IHashingService;
                       const x509 : IX509Service;
                       const cms : ICmsService;
                       const timestamper : ITimestamper;
                       const manifest : IManifestService;
                       const archive : IArchiveValidator;
                       const receipt : IReceiptService;
                       const trustPolicy : ITrustPolicyService);
  end;

const
  // INTERIM PRIVATE OID ARC.
  //
  // The plan originally called for 2.25.<UUID-as-integer> under ITU-T X.667,
  // but Microsoft's CryptEncodeObject caps each OID sub-arc at 31 bits
  // (CRYPT_E_OID_FORMAT, 0x80091003). A 128-bit UUID sub-arc never reaches
  // the signer.
  //
  // We squat on 1.3.6.1.4.1.95860 — distinctively higher than the current
  // highest-issued IANA PEN (~65860 as of 2026-05-23) to make collision
  // with a future IANA assignment very unlikely. When our own PEN is
  // assigned, the migration plan is dual-emit + always-accept (see
  // docs/package-signing-oid-migration.md). The constants below MUST NEVER
  // be removed — old packages embed them literally in their signed
  // attributes and will keep verifying via the alias path forever.
  //
  // Future PEN flips define a parallel cDpmOidArcCanonical + the three
  // canonical OIDs. The signer emits both; the verifier matches by
  // membership in {legacy, canonical}.
  cDpmOidArc                   : AnsiString = '1.3.6.1.4.1.95860';
  cOidDpmSignatureRole         : AnsiString = '1.3.6.1.4.1.95860.1';
  cOidDpmRepositoryAttestation : AnsiString = '1.3.6.1.4.1.95860.2';
  cOidDpmVerifiedAuthorSigHash : AnsiString = '1.3.6.1.4.1.95860.3';

  cSigRoleAuthor     = 'author';
  cSigRoleRepository = 'repository';
  cSignatureFolder   = 'signatures/';
  cTimestampUrlDefault = 'http://timestamp.digicert.com';

implementation

uses
  System.DateUtils,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Hashing;

constructor TPackageSigningService.Create(const logger : ILogger;
                                          const hashing : IHashingService;
                                          const x509 : IX509Service;
                                          const cms : ICmsService;
                                          const timestamper : ITimestamper;
                                          const manifest : IManifestService;
                                          const archive : IArchiveValidator;
                                          const receipt : IReceiptService;
                                          const trustPolicy : ITrustPolicyService);
begin
  if (logger = nil) or (hashing = nil) or (x509 = nil) or (cms = nil) or
     (timestamper = nil) or (manifest = nil) or (archive = nil) or
     (receipt = nil) or (trustPolicy = nil) then
    raise EPackageSigning.Create('TPackageSigningService dependencies missing');
  inherited Create;
  FLogger := logger;
  FHashing := hashing;
  FX509 := x509;
  FCms := cms;
  FTimestamper := timestamper;
  FManifest := manifest;
  FArchive := archive;
  FReceipt := receipt;
  FTrustPolicy := trustPolicy;
end;

function TPackageSigningService.ReadManifestBytes(const archivePath : string;
                                                  out bytes : TBytes) : boolean;
var
  zip : TZipFile;
  i : integer;
  ms : TStream;
  header : TZipHeader;
begin
  result := false;
  bytes := nil;
  zip := TZipFile.Create;
  try
    zip.Open(archivePath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      if SameText(zip.FileName[i], cManifestFileName) then
      begin
        zip.Read(i, ms, header);
        try
          SetLength(bytes, ms.Size);
          ms.Position := 0;
          if ms.Size > 0 then
            ms.Read(bytes[0], ms.Size);
          result := true;
        finally
          ms.Free;
        end;
        exit;
      end;
    end;
  finally
    zip.Free;
  end;
end;

function TPackageSigningService.ReadSignatureBlobs(const archivePath : string;
  out blobs : IList<TPair<string, TBytes>>) : boolean;
var
  zip : TZipFile;
  i : integer;
  ms : TStream;
  header : TZipHeader;
  bytes : TBytes;
  name : string;
begin
  blobs := TCollections.CreateList<TPair<string, TBytes>>;
  result := true;
  zip := TZipFile.Create;
  try
    try
      zip.Open(archivePath, zmRead);
    except
      result := false;
      exit;
    end;
    for i := 0 to zip.FileCount - 1 do
    begin
      name := zip.FileName[i];
      if not StartsText(cSignatureFolder, StringReplace(name, '\', '/', [rfReplaceAll])) then
        Continue;
      if (Length(name) > 0) and (name[Length(name)] = '/') then
        Continue;   // directory placeholder
      zip.Read(i, ms, header);
      try
        SetLength(bytes, ms.Size);
        ms.Position := 0;
        if ms.Size > 0 then
          ms.Read(bytes[0], ms.Size);
        blobs.Add(TPair<string, TBytes>.Create(name, bytes));
      finally
        ms.Free;
      end;
    end;
  finally
    zip.Free;
  end;
end;

procedure TPackageSigningService.WriteSignatureToArchive(const archivePath : string;
                                                          const blobName : string;
                                                          const blobBytes : TBytes);
var
  zip : TZipFile;
  ms : TMemoryStream;
begin
  zip := TZipFile.Create;
  ms := TMemoryStream.Create;
  try
    if Length(blobBytes) > 0 then
      ms.WriteBuffer(blobBytes[0], Length(blobBytes));
    ms.Position := 0;
    // Append-only: zmReadWrite preserves existing entries and adds new ones.
    zip.Open(archivePath, zmReadWrite);
    zip.Add(ms, blobName, zcDeflate);
  finally
    ms.Free;
    zip.Free;
  end;
end;

procedure TPackageSigningService.SignPackage(const packageFilePath : string;
                                             const provider : ISigningProvider;
                                             const options : ISignOptions);
var
  manifestBytes : TBytes;
  signedAttrs : TCmsAttributes;
  cmsDer : TBytes;
  signatureBytes : TBytes;
  timestampToken : ITimestampToken;
  timestampUrl : string;
  digest : THashAlgorithm;
  nextIndex : integer;
  blobName : string;
  i : integer;
  existingBlobs : IList<TPair<string, TBytes>>;
  existingName : string;
  imprintBytes : TBytes;
  signerCert : ICertificate;
begin
  FLogger.Information('Signing [' + packageFilePath + ']');
  if not FileExists(packageFilePath) then
    raise EPackageSigning.CreateFmt('Package "%s" does not exist', [packageFilePath]);
  if not ReadManifestBytes(packageFilePath, manifestBytes) then
    raise EPackageSigning.Create('Package contains no dpm-manifest.json');
  FLogger.Verbose(Format('  Manifest read: %d bytes', [Length(manifestBytes)]));

  if (provider <> nil) and (provider.Certificate <> nil) then
  begin
    signerCert := provider.Certificate;
    FLogger.Information('  Signer: ' + signerCert.SubjectDistinguishedName);
    FLogger.Verbose('  Thumbprint: ' + signerCert.Thumbprint);
    FLogger.Verbose('  SPKI (sha256): ' + BytesToHex(signerCert.SpkiHash(haSha256)));
  end;

  // Build signed attributes: dpmSignatureRole=author.
  SetLength(signedAttrs, 1);
  signedAttrs[0].Oid := cOidDpmSignatureRole;
  signedAttrs[0].Value := TEncoding.UTF8.GetBytes(cSigRoleAuthor);

  digest := options.DigestAlgorithm;
  if digest = haUnknown then
    digest := haSha256;
  FLogger.Information('  Digest: ' + TAlgorithmProfile.HashAlgorithmName(digest));

  FLogger.Verbose('  Calling signer (may prompt for token PIN)...');
  cmsDer := FCms.Sign(manifestBytes, provider, signedAttrs, digest);
  FLogger.Information(Format('  CMS signature produced: %d bytes', [Length(cmsDer)]));

  // RFC3161 timestamp the CMS blob.
  timestampUrl := options.TimestampUrl;
  if timestampUrl = '' then
  begin
    timestampUrl := cTimestampUrlDefault;
    FLogger.Verbose('  Timestamp URL (default): ' + timestampUrl);
  end
  else
    FLogger.Verbose('  Timestamp URL: ' + timestampUrl);

  // Imprint covers the signer's encryptedDigest, NOT the full CMS. The
  // unsigned attribute we attach below changes the CMS bytes but doesn't
  // touch encryptedDigest, so verify sees the same bytes we signed over.
  imprintBytes := FCms.Decode(cmsDer).EncryptedDigest;
  FLogger.Verbose(Format('  Timestamp imprint covers encryptedDigest (%d bytes)',
    [Length(imprintBytes)]));
  FLogger.Information('  Requesting RFC3161 timestamp from ' + timestampUrl);
  try
    timestampToken := FTimestamper.RequestTimestamp(imprintBytes, timestampUrl, haSha256);
  except
    on e : Exception do
    begin
      FLogger.Error('  Timestamp request failed: ' + e.Message);
      raise;
    end;
  end;
  if timestampToken = nil then
    raise EPackageSigning.Create('Timestamp authority returned no token');
  FLogger.Information(Format('  Timestamp received: signing time %s UTC (%d bytes)',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', timestampToken.SigningTime),
     Length(timestampToken.RawToken)]));
  if timestampToken.TsaCertificate <> nil then
    FLogger.Verbose('  Timestamp authority: ' +
      timestampToken.TsaCertificate.SubjectDistinguishedName);

  signatureBytes := cmsDer;
  // Attach the timestamp token as an unsigned attribute on the signer info.
  FLogger.Verbose('  Attaching timestamp token to CMS signer info...');
  FCms.AddUnsignedAttribute(signatureBytes, szOID_RFC3161_counterSign, timestampToken.RawToken);
  FLogger.Verbose(Format('  Final signature blob: %d bytes', [Length(signatureBytes)]));

  // Pick the next available signature file name.
  ReadSignatureBlobs(packageFilePath, existingBlobs);
  nextIndex := 1;
  for i := 0 to existingBlobs.Count - 1 do
  begin
    existingName := ExtractFileName(existingBlobs[i].Key);
    if StartsText('author-', existingName) then
      Inc(nextIndex);
  end;

  blobName := Format('signatures/author-%d.p7s', [nextIndex]);
  FLogger.Information('  Writing ' + blobName + ' into the .dpkg');
  WriteSignatureToArchive(packageFilePath, blobName, signatureBytes);
  FLogger.Success('Signed [' + ExtractFileName(packageFilePath) + ']');
end;

procedure TPackageSigningService.VerifyFileSetIntegrity(const archivePath : string;
                                                         const manifest : IPackageManifest);
var
  zip : TZipFile;
  i, j : integer;
  archivePaths : IDictionary<string, integer>;
  manifestPaths : IDictionary<string, integer>;
  name : string;
  normalised : string;
  reason : string;
  entry : TManifestFileEntry;
  ms : TStream;
  header : TZipHeader;
  bytes : TBytes;
  actualHash : TBytes;
  found : integer;
begin
  archivePaths := TCollections.CreateDictionary<string, integer>;
  manifestPaths := TCollections.CreateDictionary<string, integer>;
  zip := TZipFile.Create;
  try
    zip.Open(archivePath, zmRead);

    for i := 0 to zip.FileCount - 1 do
    begin
      name := StringReplace(zip.FileName[i], '\', '/', [rfReplaceAll]);
      // Strip trailing slash on directory entries before comparison.
      if (Length(name) > 0) and (name[Length(name)] = '/') then
        Continue;
      // Manifest itself and signatures/ are excluded from manifest.files
      if SameText(name, cManifestFileName) then
        Continue;
      if StartsText(cSignatureFolder, name) then
        Continue;
      normalised := FManifest.NormalizeToNfc(name);
      archivePaths[normalised] := i;
    end;

    // Walk manifest entries — each must be in the archive with matching hash + size.
    for j := 0 to High(manifest.Files) do
    begin
      entry := manifest.Files[j];
      if not FManifest.ValidatePath(entry.Path, reason) then
        raise EPackageSigning.CreateFmt('Manifest path "%s" invalid: %s',
          [entry.Path, reason]);

      if not archivePaths.TryGetValue(entry.Path, found) then
        raise EPackageSigning.CreateFmt('Manifest lists "%s" but archive does not contain it',
          [entry.Path]);

      if zip.FileInfo[found].UncompressedSize <> entry.Size then
        raise EPackageSigning.CreateFmt('Size mismatch for "%s": manifest=%d archive=%d',
          [entry.Path, entry.Size, zip.FileInfo[found].UncompressedSize]);

      zip.Read(found, ms, header);
      try
        SetLength(bytes, ms.Size);
        ms.Position := 0;
        if ms.Size > 0 then
          ms.Read(bytes[0], ms.Size);
      finally
        ms.Free;
      end;
      actualHash := FHashing.HashBytes(bytes, manifest.HashAlgorithm);
      if not BytesEqual(actualHash, entry.Hash) then
        raise EPackageSigning.CreateFmt('Hash mismatch for "%s"', [entry.Path]);

      manifestPaths[entry.Path] := 1;
    end;

    // Any archive entry (excluding manifest + signatures/) not in manifestPaths
    // is an unlisted file (V-3).
    for name in archivePaths.Keys do
      if not manifestPaths.ContainsKey(name) then
        raise EPackageSigning.CreateFmt('Archive contains unlisted file "%s"', [name]);
  finally
    zip.Free;
  end;
end;

function TPackageSigningService.VerifyOneSignature(const der : TBytes; const manifestBytes : TBytes;
                                                    out info : TSignatureInfo) : boolean;
var
  decoded : ICmsSignedData;
  signerCert : ICertificate;
  chain : ICertificateChain;
  chainResult : TChainResult;
  roleBytes : TBytes;
  roleText : string;
  tsToken : TBytes;
  tsSigningTime : TDateTime;
  embedded : TArray<ICertificate>;
  intermediates : array of ICertificate;
  i : integer;
begin
  result := false;
  FillChar(info, SizeOf(info), 0);
  info.Role := srAuthor;
  info.Valid := false;

  // V-15: detached CMS signature verifies against the manifest bytes.
  if not FCms.VerifyDetached(der, manifestBytes, signerCert) then
  begin
    info.FailureReason := 'CMS signature does not verify against manifest bytes';
    exit;
  end;

  decoded := FCms.Decode(der);

  // Find the role attribute. Default to author if absent (back-compat for
  // legacy author-only blobs that pre-date the role attribute — Phase 1
  // tightens this to require the attribute once Phase 1 ships).
  if decoded.FindSignedAttribute(cOidDpmSignatureRole, roleBytes) then
  begin
    roleText := LowerCase(Trim(TEncoding.UTF8.GetString(roleBytes)));
    if roleText = cSigRoleRepository then
      info.Role := srRepository
    else if roleText = cSigRoleAuthor then
      info.Role := srAuthor
    else
    begin
      info.FailureReason := 'unknown dpmSignatureRole value "' + roleText + '"';
      exit;
    end;
  end;

  signerCert := decoded.SignerCertificate;
  if signerCert = nil then
  begin
    info.FailureReason := 'signature has no signer certificate';
    exit;
  end;

  info.SignerSubject := signerCert.SubjectDistinguishedName;
  info.Thumbprint := signerCert.Thumbprint;
  info.SignerSpkiHex := BytesToHex(signerCert.SpkiHash(haSha256));

  // V-17: RFC3161 timestamp — establish effective signing time
  if decoded.FindUnsignedAttribute(szOID_RFC3161_counterSign, tsToken) then
  begin
    // The imprint covers the signer's encryptedDigest — the same bytes we
    // signed at pack time. Adding the unsigned attribute does not change
    // encryptedDigest, so the round-trip matches.
    if FTimestamper.Verify(tsToken, decoded.EncryptedDigest, haSha256,
        tsSigningTime) then
      info.EffectiveSigningTime := tsSigningTime
    else
    begin
      info.FailureReason := 'RFC3161 timestamp verification failed';
      exit;
    end;
  end
  else
  begin
    info.FailureReason := 'signature has no RFC3161 timestamp';
    exit;
  end;

  // V-15 / V-16 chain build — gather embedded certs as intermediates and
  // build to a trusted root.
  embedded := decoded.EmbeddedCertificates;
  SetLength(intermediates, 0);
  for i := 0 to High(embedded) do
  begin
    if (embedded[i] <> nil) and
       not BytesEqual(embedded[i].SpkiHash(haSha256), signerCert.SpkiHash(haSha256)) then
    begin
      SetLength(intermediates, Length(intermediates) + 1);
      intermediates[High(intermediates)] := embedded[i];
    end;
  end;

  chain := FX509.CreateChain;
  // V-26: build the chain *as of* the RFC3161 signing time, with online
  // revocation checks enabled. A revocation that post-dates the signing
  // timestamp does NOT invalidate the signature (the cert was good when the
  // signature was made). A revocation effective at signing time still fails.
  // P3 §3.2 — when the caller passed --offline, ask the chain engine to
  // skip CRL/OCSP fetches. The engine still uses its on-disk cache, so a
  // recently fetched response is consulted. Unreachable becomes rsUnknown
  // rather than failing the verify.
  chainResult := chain.BuildAtTime(signerCert, intermediates,
    info.EffectiveSigningTime, not FActiveFlags.Offline);
  if chainResult <> crValid then
  begin
    info.FailureReason := Format('chain build: %s', [chain.LastErrorMessage]);
    exit;
  end;
  chainResult := chain.VerifyForCodeSigning(info.EffectiveSigningTime);
  if chainResult <> crValid then
  begin
    info.FailureReason := Format('chain verify: %s', [chain.LastErrorMessage]);
    exit;
  end;

  info.ChainTrusted := true;
  info.Revocation := chain.RevocationStatus;

  // V-26 follow-up — second chain build at "now" (no pTime) to detect a
  // post-signing keyCompromise revocation. The timestamp-aware build above
  // uses the CRL valid at signing time, which won't carry a revocation
  // published later. Without this second pass we'd miss keyCompromise
  // disclosures that arrive after the package shipped.
  //
  // Skip in offline mode — we'd just get the same (possibly stale) cached
  // CRL as the first pass.
  info.CurrentRevocationReason := rrNotApplicable;
  if not FActiveFlags.Offline then
  begin
    chain := FX509.CreateChain;
    if chain.BuildAtTime(signerCert, intermediates, 0, True) = crRevoked then
      info.CurrentRevocationReason := chain.RevocationReason;
  end;

  info.Valid := true;
  result := true;
end;

procedure TPackageSigningService.ReadRepositoryAttestation(const der : TBytes;
                                                            var info : TSignatureInfo);
var
  decoded : ICmsSignedData;
  attrBytes : TBytes;
  text : string;
  eqPos : integer;
  parts : TStringList;
  i : integer;
  key, value : string;
  authorSpki, namespace : string;
  unsignedTag : string;
begin
  info.Attestation.Present := false;
  info.Attestation.Namespace := '';
  info.Attestation.AuthorSpkiHex := '';
  info.Attestation.UnsignedReason := urAttestNotApplicable;

  try
    decoded := FCms.Decode(der);
  except
    exit;
  end;
  if decoded = nil then
    exit;
  if not decoded.FindSignedAttribute(cOidDpmRepositoryAttestation, attrBytes) then
    exit;

  // The attestation payload is a UTF-8 KV pair list, semicolon-separated:
  //   namespace=VSoft.*;authorSpki=hex;unsigned=neverSigned
  // unsigned is omitted when the author SPKI is present. Conformance-doc
  // wire format; the producer (gallery) writes it the same way.
  text := TEncoding.UTF8.GetString(attrBytes);
  parts := TStringList.Create;
  try
    parts.Delimiter := ';';
    parts.StrictDelimiter := true;
    parts.DelimitedText := text;
    namespace := '';
    authorSpki := '';
    unsignedTag := '';
    for i := 0 to parts.Count - 1 do
    begin
      eqPos := Pos('=', parts[i]);
      if eqPos <= 0 then
        Continue;
      key := Trim(Copy(parts[i], 1, eqPos - 1));
      value := Trim(Copy(parts[i], eqPos + 1, MaxInt));
      if SameText(key, 'namespace') then
        namespace := value
      else if SameText(key, 'authorSpki') then
        authorSpki := value
      else if SameText(key, 'unsigned') then
        unsignedTag := value;
    end;
  finally
    parts.Free;
  end;

  info.Attestation.Present := true;
  info.Attestation.Namespace := namespace;
  info.Attestation.AuthorSpkiHex := authorSpki;
  if authorSpki <> '' then
    info.Attestation.UnsignedReason := urAttestNotApplicable
  else if SameText(unsignedTag, 'neverSigned') then
    info.Attestation.UnsignedReason := urAttestNeverSigned
  else if SameText(unsignedTag, 'authorCeasedSigning') then
    info.Attestation.UnsignedReason := urAttestAuthorCeasedSigning
  else
    info.Attestation.UnsignedReason := urAttestNotApplicable;
end;

procedure TPackageSigningService.EvaluateRequirements(const policy : TTrustPolicy;
                                                       hasAnySignature : boolean;
                                                       hasValidAuthor : boolean;
                                                       hasValidTrustedRepo : boolean;
                                                       var outcome : TVerificationOutcome;
                                                       var reason : string);
begin
  TTrustModeEvaluator.Evaluate(policy, hasAnySignature, hasValidAuthor,
                               hasValidTrustedRepo, outcome, reason);
end;

function TPackageSigningService.VerifyPackage(const packageFilePath : string;
                                              const policy : TTrustPolicy;
                                              const flags : TVerifyFlags) : TVerificationResult;
begin
  FActiveFlags := flags;
  try
    result := VerifyPackage(packageFilePath, policy);
  finally
    // Reset so a subsequent VerifyPackage() call without flags isn't sticky.
    FActiveFlags.Offline := false;
  end;
end;

function TPackageSigningService.VerifyPackage(const packageFilePath : string;
                                              const policy : TTrustPolicy) : TVerificationResult;
var
  archiveResult : TArchiveValidationResult;
  manifestBytes : TBytes;
  manifest : IPackageManifest;
  blobs : IList<TPair<string, TBytes>>;
  i : integer;
  sigInfo : TSignatureInfo;
  hasValidAuthor : boolean;
  hasAnySignature : boolean;
  hasValidTrustedRepo : boolean;
begin
  FillChar(result, SizeOf(result), 0);
  result.Outcome := voInvalid;
  result.Mode := policy.ValidationMode;
  SetLength(result.Signatures, 0);
  // Strings need explicit init after FillChar.
  result.ManifestHashHex := '';
  result.Reason := '';
  result.PolicyFingerprint := FTrustPolicy.PolicyFingerprint(policy);

  // 1. archive-format rules
  archiveResult := FArchive.Validate(packageFilePath);
  if not archiveResult.Ok then
  begin
    result.Reason := Format('Archive rejected: %s (entry "%s")',
      [archiveResult.Reason, archiveResult.Entry]);
    exit;
  end;

  // 2. read manifest bytes
  if not ReadManifestBytes(packageFilePath, manifestBytes) then
  begin
    result.Reason := 'Package contains no dpm-manifest.json';
    exit;
  end;
  try
    manifest := FManifest.Parse(manifestBytes);
  except
    on e : Exception do
    begin
      result.Reason := 'Manifest parse failed: ' + e.Message;
      exit;
    end;
  end;
  result.ManifestHashAlgorithm := haSha256;
  result.ManifestHashHex := BytesToHex(FHashing.HashBytes(manifestBytes, haSha256));

  // 3. file-set equality + integrity
  try
    VerifyFileSetIntegrity(packageFilePath, manifest);
  except
    on e : EPackageSigning do
    begin
      result.Reason := e.Message;
      exit;
    end;
  end;

  // 4. read all signatures/* blobs; each must be a well-formed CMS blob
  if not ReadSignatureBlobs(packageFilePath, blobs) then
  begin
    result.Reason := 'Failed to enumerate signatures/';
    exit;
  end;
  hasAnySignature := blobs.Count > 0;
  hasValidAuthor := false;
  hasValidTrustedRepo := false;

  SetLength(result.Signatures, blobs.Count);
  for i := 0 to blobs.Count - 1 do
  begin
    try
      VerifyOneSignature(blobs[i].Value, manifestBytes, sigInfo);
    except
      on e : Exception do
      begin
        sigInfo.Valid := false;
        sigInfo.FailureReason := e.Message;
      end;
    end;

    if sigInfo.Role = srAuthor then
      sigInfo.PublisherTrusted := FTrustPolicy.PublisherTrusted(policy, sigInfo.SignerSpkiHex)
    else
    begin
      sigInfo.RepositoryTrusted := FTrustPolicy.RepositoryTrusted(policy, sigInfo.SignerSpkiHex);
      // V-21: only read the attestation off a trusted repository signature.
      // An attestation present on an untrusted repo signature is ignored
      // entirely — never read, never displayed.
      if sigInfo.Valid and sigInfo.RepositoryTrusted then
        ReadRepositoryAttestation(blobs[i].Value, sigInfo);
    end;

    // V-26 follow-up — retroactive invalidation on keyCompromise. The
    // second-pass build inside VerifyOneSignature surfaces the current
    // CRL reason; if it's keyCompromise we treat the signature as invalid
    // unless the administrator has explicitly accepted the residual risk.
    if sigInfo.Valid and (sigInfo.CurrentRevocationReason = rrKeyCompromise)
       and not policy.AllowKeyCompromiseOverride then
    begin
      sigInfo.Valid := false;
      sigInfo.FailureReason :=
        'certificate revoked for keyCompromise — signature retroactively invalid';
    end;

    result.Signatures[i] := sigInfo;
    if sigInfo.Valid and (sigInfo.Role = srAuthor) then
      hasValidAuthor := true;
    if sigInfo.Valid and (sigInfo.Role = srRepository) and sigInfo.RepositoryTrusted then
      hasValidTrustedRepo := true;
  end;

  EvaluateRequirements(policy, hasAnySignature, hasValidAuthor, hasValidTrustedRepo,
                       result.Outcome, result.Reason);
end;

function TPackageSigningService.QuickRecheck(const cacheFolder : string;
                                             const policy : TTrustPolicy) : boolean;
var
  receipt : TVerificationReceipt;
  manifestPath : string;
  manifestBytes : TBytes;
  recomputedHash : string;
  policyFingerprint : string;
begin
  result := false;
  if not FReceipt.TryRead(cacheFolder, receipt) then
    exit;

  policyFingerprint := FTrustPolicy.PolicyFingerprint(policy);
  if not SameText(policyFingerprint, receipt.TrustPolicyFingerprint) then
    exit;   // policy changed; force full re-verify

  manifestPath := IncludeTrailingPathDelimiter(cacheFolder) + cManifestFileName;
  if not FileExists(manifestPath) then
    exit;
  manifestBytes := TFile.ReadAllBytes(manifestPath);
  recomputedHash := BytesToHex(FHashing.HashBytes(manifestBytes, haSha256));
  result := SameText(recomputedHash, receipt.ManifestHashHex);
end;

end.
