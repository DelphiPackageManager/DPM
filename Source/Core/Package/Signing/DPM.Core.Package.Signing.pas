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
    // Filename-only scan of the signatures/ folder. No entry contents are
    // read — just enumerates the central directory and returns the next
    // free author-N index. Raises on read failure so we never silently
    // collide on author-1.p7s.
    function NextAuthorSignatureIndex(const archivePath : string) : integer;
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
    // Cryptographic binding check: confirm the repo sig's
    // dpmVerifiedAuthorSigHash attribute matches a present author signature
    // whose signer SPKI equals the attestation's authorSpki. Marks the repo
    // sigInfo invalid if the binding is declared but doesn't resolve. Soft-
    // warns (does not fail) when the binding attribute is absent, to allow
    // older gallery versions to keep working until they catch up.
    procedure EnforceAuthorSigBinding(const blobs : IList<TPair<string, TBytes>>;
                                       const earlierSignatures : TArray<TSignatureInfo>;
                                       var repoSigInfo : TSignatureInfo);
    // Inverse of EnforceAuthorSigBinding. Attestation declares no verified
    // author (authorMode = never_author_signed / author_acknowledged_unsigned)
    // — but if the archive carries any signatures/author-*.p7s blob, the
    // upload's contents disagree with what the gallery signed. Either an
    // attacker swapped a verified-author blob for an unverified one (so the
    // gallery saw no verified signer) or the gallery's attestation generator
    // is out of sync with the archive. Both cases must refuse the package:
    // an unverified author sig in the same archive as a "no signer" repo
    // attestation is exactly the bait-and-switch we're trying to detect.
    procedure EnforceUnsignedAttestation(const blobs : IList<TPair<string, TBytes>>;
                                          var repoSigInfo : TSignatureInfo);
    // Strip a single OCTET STRING (tag 04) or UTF8String (tag 0C) wrapper
    // from `value`, returning its content bytes. If the bytes don't look
    // like a recognised wrapper (or claim a length larger than available)
    // the input is returned unchanged — caller can still attempt to parse,
    // just from an unexpected shape.
    function UnwrapOctetString(const value : TBytes) : TBytes;
    // Tolerant UTF-8 → string conversion. Falls back to byte-by-byte ASCII
    // when TEncoding.UTF8.GetString raises (strict-UTF8 configurations).
    // Never raises — the caller for attestation parsing relies on us
    // returning *something* parseable so the KV walker can spot a malformed
    // payload and the binding-enforcement security check can still run.
    function Utf8BytesToStringSafe(const bytes : TBytes) : string;
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
  // PRIVATE ENTERPRISE NUMBER.
  //
  // We were assigned on 1.3.6.1.4.1.65863
  // The constants below MUST NEVER
  // be removed — old packages embed them literally in their signed
  // attributes and will keep verifying via the alias path forever.
  //

  cDpmOidArc                   : AnsiString = '1.3.6.1.4.1.65863';
  cOidDpmSignatureRole         : AnsiString = '1.3.6.1.4.1.65863.1.1';
  cOidDpmRepositoryAttestation : AnsiString = '1.3.6.1.4.1.65863.1.2';
  cOidDpmVerifiedAuthorSigHash : AnsiString = '1.3.6.1.4.1.65863.1.3';

  cSigRoleAuthor     = 'author';
  cSigRoleRepository = 'repository';
  cSignatureFolder   = 'signatures/';
  cTimestampUrlDefault = 'http://timestamp.digicert.com';

implementation

uses
  System.DateUtils,
  System.IOUtils,
  System.StrUtils,
  JsonDataObjects,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Utils.Files;

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
        // Use the RTL TBytes overload — it reads UncompressedSize bytes from the
        // entry stream's correct position. See ReadSignatureBlobs for why the
        // stream overload + ms.Size/ms.Position is unusable on Delphi 10.2.
        zip.Read(i, bytes);
        result := true;
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
      // Use the RTL TBytes overload — it reads UncompressedSize bytes from the
      // entry stream's correct position. The stream overload + ms.Size/Position
      // is unusable on Delphi 10.2: for a STORED entry (a .p7s is incompressible
      // so it is stored, not deflated) ms.Size is reported as bytes-to-end-of-
      // archive AND ms.Position:=0 seeks to the start of the ARCHIVE rather than
      // the entry, so the CMS blob is read as garbage and CryptVerifyDetached-
      // MessageSignature fails with CRYPT_E_ASN1_BADTAG. Deflated entries return
      // a fresh decompression stream, which is why they were never affected.
      zip.Read(i, bytes);
      blobs.Add(TPair<string, TBytes>.Create(name, bytes));
    end;
  finally
    zip.Free;
  end;
end;

function TPackageSigningService.NextAuthorSignatureIndex(const archivePath : string) : integer;
const
  cPrefix = 'signatures/author-';
  cSuffix = '.p7s';
var
  zip : TZipFile;
  i, value, highest : integer;
  name, normalised, stem : string;
begin
  highest := 0;
  zip := TZipFile.Create;
  try
    // No try/except — a read failure here MUST surface, not be swallowed
    // into nextIndex=1 and append a duplicate author-1.p7s.
    zip.Open(archivePath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      name := zip.FileName[i];
      normalised := StringReplace(name, '\', '/', [rfReplaceAll]);
      if not StartsText(cPrefix, normalised) then
        Continue;
      if not EndsText(cSuffix, normalised) then
        Continue;
      stem := Copy(normalised, Length(cPrefix) + 1,
                   Length(normalised) - Length(cPrefix) - Length(cSuffix));
      if TryStrToInt(stem, value) and (value > highest) then
        highest := value;
    end;
  finally
    zip.Free;
  end;
  result := highest + 1;
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
    // Caller computes a unique blobName via NextAuthorSignatureIndex; we
    // never get asked to write a name that already exists.
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
  blobName : string;
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
    FLogger.Information('  Signer : ' + signerCert.SubjectDistinguishedName);
    FLogger.Verbose('  Thumbprint : ' + signerCert.Thumbprint);
    FLogger.Verbose('  SPKI (sha256) : ' + BytesToHex(signerCert.SpkiHash(haSha256)));
  end;

  // Push per-file audit metadata down to the provider. Remote providers
  // (Signotaur) use this to record the correct file name + size in their
  // server-side audit log; local providers no-op.
  if provider <> nil then
    provider.SetSigningContext(ExtractFileName(packageFilePath),
                               TFileUtils.GetSize(packageFilePath));

  // Build signed attributes: dpmSignatureRole=author.
  SetLength(signedAttrs, 1);
  signedAttrs[0].Oid := cOidDpmSignatureRole;
  signedAttrs[0].Value := TEncoding.UTF8.GetBytes(cSigRoleAuthor);

  digest := options.DigestAlgorithm;
  if digest = haUnknown then
  begin
    // Auto-select from the cert's key parameters. RSA -> SHA-256; ECDSA
    // P-256 -> SHA-256, P-384 -> SHA-384, P-521 -> SHA-512. This matches
    // what HSM / smart-card middleware actually accepts (FIPS 186: digest
    // size = curve size for ECDSA).
    if (signerCert <> nil) then
      digest := signerCert.PreferredDigest
    else
      digest := haSha256;
    FLogger.Information('  Digest Algorithm : ' + TAlgorithmProfile.HashAlgorithmName(digest) +
                        ' (auto-selected from cert key)');
  end
  else
    FLogger.Information('  Digest Algorithm : ' + TAlgorithmProfile.HashAlgorithmName(digest));

  FLogger.Verbose('  Calling signer (may prompt for token PIN)...');
  cmsDer := FCms.Sign(manifestBytes, provider, signedAttrs, digest);
  FLogger.Information(Format('  CMS signature produced: %d bytes', [Length(cmsDer)]));

  // Sanity-check the signature locally before we ship it. We've observed
  // sporadic (~0.4%) signing failures where the produced CMS is structurally
  // valid and the messageDigest attribute correctly equals SHA(manifest), but
  // the signature value itself doesn't verify against the signer cert's
  // public key. Root cause could be on either side — a client-side digest
  // corruption (TBytes lifetime, HTTP-body truncation, etc.) or a remote
  // signing service returning a bad signature — and we can't distinguish
  // them after the fact from the artifact alone. What we CAN do is refuse
  // to ship the bad package: it would silently fail verification on every
  // downstream consumer otherwise.
  //
  // Re-run with --verbose to capture the digest bytes sent to the signer
  // and the signature bytes returned (logged by SignRemote). Comparing the
  // sent-digest hex against SHA-of-canonical-SignedAttrs reconstructed from
  // the bad CMS tells you which side corrupted: equal => server returned a
  // bad signature, different => the client sent a wrong digest.
  if not FCms.VerifyDetached(cmsDer, manifestBytes, signerCert) then
    raise EPackageSigning.CreateFmt(
      'Post-sign verification failed for "%s": the produced CMS signature ' +
      'does not verify against the signed manifest bytes. The messageDigest ' +
      'attribute is correct, so the manifest itself was hashed right, but ' +
      'the signature value is wrong for the signer cert''s key. Re-running ' +
      'the sign command usually succeeds. Re-run with --verbose for the ' +
      'SignRemote self-check log lines (encoding hash, signature bytes, ' +
      'signatureAlgorithm OID) to pinpoint which leg of the pipeline ' +
      'corrupted.',
      [ExtractFileName(packageFilePath)]);
  FLogger.Verbose('  Local verify against signed manifest bytes: OK');

  // RFC3161 timestamp the CMS blob.
  timestampUrl := options.TimestampUrl;
  if timestampUrl = '' then
  begin
    timestampUrl := cTimestampUrlDefault;
    FLogger.Verbose('  Timestamp URL (default) : ' + timestampUrl);
  end
  else
    FLogger.Verbose('  Timestamp URL : ' + timestampUrl);

  // Imprint covers the signer's encryptedDigest, NOT the full CMS. The
  // unsigned attribute we attach below changes the CMS bytes but doesn't
  // touch encryptedDigest, so verify sees the same bytes we signed over.
  // ExtractEncryptedDigest is the minimal Win32 decode — no signer/cert/
  // attribute traversal — to avoid AV exits on hand-assembled CMS (remote
  // provider path).
  imprintBytes := FCms.ExtractEncryptedDigest(cmsDer);
  FLogger.Verbose(Format('  Timestamp imprint covers encryptedDigest (%d bytes)', [Length(imprintBytes)]));
  FLogger.Information('  Requesting RFC3161 timestamp from : ' + timestampUrl);
  try
    timestampToken := FTimestamper.RequestTimestamp(imprintBytes, timestampUrl, haSha256);
  except
    on e : Exception do
    begin
      FLogger.Error('  Timestamp request failed : ' + e.Message);
      raise;
    end;
  end;
  if timestampToken = nil then
    raise EPackageSigning.Create('Timestamp authority returned no token');
  FLogger.Information(Format('  Timestamp received : signing time %s UTC (%d bytes)', [FormatDateTime('yyyy-mm-dd hh:nn:ss', timestampToken.SigningTime),
     Length(timestampToken.RawToken)]));
  if timestampToken.TsaCertificate <> nil then
    FLogger.Verbose('  Timestamp authority : ' + timestampToken.TsaCertificate.SubjectDistinguishedName);

  signatureBytes := cmsDer;
  // Attach the timestamp token as an unsigned attribute on the signer info.
  FLogger.Verbose('  Attaching timestamp token to CMS signer info...');
  FCms.AddUnsignedAttribute(signatureBytes, szOID_RFC3161_counterSign, timestampToken.RawToken);
  FLogger.Verbose(Format('  Final signature blob: %d bytes', [Length(signatureBytes)]));

  // Filename-only scan of the central directory yields the next free
  // author-N index. Re-signing accumulates: first run writes author-1.p7s,
  // a second run sees it and writes author-2.p7s, etc. The archive is
  // append-only.
  blobName := Format('signatures/author-%d.p7s', [NextAuthorSignatureIndex(packageFilePath)]);
  FLogger.Information('  Writing ' + blobName + ' into the .dpkg');
  WriteSignatureToArchive(packageFilePath, blobName, signatureBytes);
  FLogger.Verbose('  Signature file written; sealing archive');
  FLogger.Success('Signed [' + ExtractFileName(packageFilePath) + ']');
  FLogger.NewLine;
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

      // RTL TBytes overload — reads from the entry's correct position. See
      // ReadSignatureBlobs: the stream overload + ms.Size/ms.Position corrupts
      // STORED entries on Delphi 10.2 (wrong Size, and Position:=0 seeks to the
      // archive start), so a stored file would otherwise fail integrity there.
      zip.Read(found, bytes);
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
  embeddedSigner : ICertificate;
begin
  result := false;
  FillChar(info, SizeOf(info), 0);
  info.Role := srAuthor;
  info.Valid := false;

  // V-15: detached CMS signature verifies against the manifest bytes.
  if not FCms.VerifyDetached(der, manifestBytes, signerCert) then
  begin
    info.FailureReason := 'CMS signature does not verify against manifest bytes';
    // Surface the cert the CMS *claims* to be signed by, even though the
    // signature is invalid. Without this, the verify output shows empty
    // Signer / Thumbprint / SPKI and a user staring at "INVALID" has no way
    // to know which cert produced the bad signature (and therefore which
    // batch / signer to re-run).
    try
      decoded := FCms.Decode(der);
      if decoded <> nil then
      begin
        embeddedSigner := decoded.SignerCertificate;
        if embeddedSigner <> nil then
        begin
          info.SignerSubject := embeddedSigner.SubjectDistinguishedName;
          info.Thumbprint := embeddedSigner.Thumbprint;
          info.SignerSpkiHex := BytesToHex(embeddedSigner.SpkiHash(haSha256));
        end;
      end;
    except
      // Decode itself failed — leave signer fields blank; the FailureReason
      // already tells the user the signature is invalid.
      on Exception do ;
    end;
    exit;
  end;

  decoded := FCms.Decode(der);

  // Find the role attribute. Default to author if absent (back-compat for
  // legacy author-only blobs that pre-date the role attribute — Phase 1
  // tightens this to require the attribute once Phase 1 ships).
  //
  // Producers vary on whether they DER-wrap the attribute value in an
  // OCTET STRING. Our local signer does; some peers write the bytes bare.
  // UnwrapOctetString is a no-op on bare input and strips the wrapper on
  // wrapped input, so role detection works either way.
  if decoded.FindSignedAttribute(cOidDpmSignatureRole, roleBytes) then
  begin
    roleBytes := UnwrapOctetString(roleBytes);
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

  // Repository signatures are authenticated by SPKI pinning against the
  // trust set — NOT chain validation. Per docs/package-signing.md
  // §Trust Bootstrapping: repository certs are essentially self-issued and
  // ship with the client as built-in SPKI pins. The CMS verify above plus
  // the RepositoryTrusted(SPKI) check that happens after VerifyOneSignature
  // returns are the full authentication for repo sigs. Skip the public-CA
  // chain build, code-signing EKU check, and the second-pass revocation
  // build, all of which would fail on a self-issued cert that intentionally
  // has no public-CA parent.
  if info.Role = srRepository then
  begin
    info.ChainTrusted := false;          // not applicable — SPKI-pinned, not chained
    info.Revocation := rsNotChecked;
    info.CurrentRevocationReason := rrNotApplicable;
    info.Valid := true;
    result := true;
    exit;
  end;

  // V-15 / V-16 chain build — gather embedded certs as intermediates and
  // build to a trusted root. Author signatures only.
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

function TPackageSigningService.UnwrapOctetString(const value : TBytes) : TBytes;
const
  // Tags producers reasonably use to wrap a string-shaped AttributeValue.
  // CMS AttributeValue is ANY, so the producer picks. OCTET STRING (04) is
  // what the DPM client emits; .NET / Node CMS libraries often pick
  // UTF8String (0C) for string payloads — both wrap the same way, and we
  // want to unwrap either before handing the bytes to a UTF-8 decoder.
  // Other string-ish tags (PrintableString 13, IA5String 16, …) get added
  // here if we ever see them in the wild.
  cTagOctetString = $04;
  cTagUtf8String  = $0C;
var
  tag : byte;
  contentLen, lenFieldBytes, contentStart : integer;
  lenOf : byte;
begin
  result := value;
  if Length(value) < 2 then
    exit;
  tag := value[0];
  if (tag <> cTagOctetString) and (tag <> cTagUtf8String) then
    exit;

  if value[1] < $80 then
  begin
    // Short form — length fits in one byte.
    contentLen := value[1];
    contentStart := 2;
  end
  else
  begin
    // Long form — value[1] = 0x80 | <number-of-length-bytes>.
    // Max length we care about: a few KB, so 1..4 length bytes is plenty.
    lenOf := value[1] and $7F;
    if (lenOf = 0) or (lenOf > 4) or (Length(value) < 2 + lenOf) then
      exit;
    contentLen := 0;
    for lenFieldBytes := 0 to lenOf - 1 do
      contentLen := (contentLen shl 8) or value[2 + lenFieldBytes];
    contentStart := 2 + lenOf;
  end;

  if contentLen > Length(value) - contentStart then
    exit;     // declared length overruns the buffer — leave as-is

  SetLength(result, contentLen);
  if contentLen > 0 then
    Move(value[contentStart], result[0], contentLen);
end;

function TPackageSigningService.Utf8BytesToStringSafe(const bytes : TBytes) : string;
var
  i : integer;
  sb : TStringBuilder;
begin
  if Length(bytes) = 0 then
  begin
    result := '';
    exit;
  end;
  try
    result := TEncoding.UTF8.GetString(bytes);
    exit;
  except
    on Exception do
    begin
      // Strict UTF-8 configuration rejected an invalid sequence. Fall back
      // to a byte-by-byte conversion: keep printable ASCII (0x20..0x7E)
      // plus CR/LF/TAB, replace everything else with '?'. The attestation
      // KV format is pure ASCII, so any real content survives; corruption
      // becomes visible '?' chars that won't match expected keys.
    end;
  end;
  sb := TStringBuilder.Create;
  try
    for i := 0 to Length(bytes) - 1 do
      if (bytes[i] = $09) or (bytes[i] = $0A) or (bytes[i] = $0D) or
         ((bytes[i] >= $20) and (bytes[i] <= $7E)) then
        sb.Append(Char(bytes[i]))
      else
        sb.Append('?');
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TPackageSigningService.ReadRepositoryAttestation(const der : TBytes;
                                                            var info : TSignatureInfo);
var
  decoded : ICmsSignedData;
  attrBytes : TBytes;
  hashValues : TArray<TBytes>;
  hashBytes : TBytes;
  text : string;
  doc : TJsonObject;
  authorMode : string;
  authorSpki : string;
  namespace : string;
  i, collected : integer;
begin
  info.Attestation.Present := false;
  info.Attestation.Namespace := '';
  info.Attestation.AuthorSpkiHex := '';
  info.Attestation.UnsignedReason := urAttestNotApplicable;
  SetLength(info.Attestation.BoundAuthorSigHashesHex, 0);

  try
    decoded := FCms.Decode(der);
  except
    exit;
  end;
  if decoded = nil then
    exit;
  if not decoded.FindSignedAttribute(cOidDpmRepositoryAttestation, attrBytes) then
    exit;

  // Canonical wire form (docs/package-signing.md §CMS / PKCS#7 Format and
  // the gallery spec): UTF8String value containing canonical JSON:
  //   { "authorMode": "verified" | "never_author_signed" | "author_acknowledged_unsigned",
  //     "authorSpki": <hex>|null,
  //     "galleryVersion": "1.0",
  //     "namespace": <string>,
  //     "namespaceType": "user"|"org" }
  // Unknown keys are ignored for forward-compat (gallery may extend the
  // schema without breaking older verifiers).
  //
  // FindSignedAttribute returns the raw attribute-value bytes Windows hands
  // back from CryptMsgGetParam — the DER-wrapped form. UnwrapOctetString
  // strips a OCTET STRING (04) or UTF8String (0C) wrapper if present so we
  // can hand the inner JSON bytes to the decoder.
  attrBytes := UnwrapOctetString(attrBytes);
  // Tolerant decode — strict UTF-8 RTL builds raise on invalid sequences,
  // which would otherwise tunnel out and (before the hard-fail wiring in
  // VerifyPackage) silently skip the binding check.
  text := Utf8BytesToStringSafe(attrBytes);

  doc := nil;
  try
    try
      doc := TJsonBaseObject.Parse(text) as TJsonObject;
    except
      on e : Exception do
        raise EPackageSigning.Create(
          'dpmRepositoryAttestation JSON parse failed: ' + e.Message);
    end;
    if doc = nil then
      raise EPackageSigning.Create(
        'dpmRepositoryAttestation payload is not a JSON object');

    authorMode := doc.S['authorMode'];
    namespace := doc.S['namespace'];
    // TJsonObject.S returns '' for both a missing key and a JSON null —
    // both legitimately mean "no verified author signer", so we don't
    // distinguish.
    authorSpki := doc.S['authorSpki'];

    info.Attestation.Present := true;
    info.Attestation.Namespace := namespace;

    if SameText(authorMode, 'verified') then
    begin
      info.Attestation.AuthorSpkiHex := LowerCase(authorSpki);
      info.Attestation.UnsignedReason := urAttestNotApplicable;
    end
    else if SameText(authorMode, 'never_author_signed') then
    begin
      info.Attestation.AuthorSpkiHex := '';
      info.Attestation.UnsignedReason := urAttestNeverSigned;
    end
    else if SameText(authorMode, 'author_acknowledged_unsigned') then
    begin
      info.Attestation.AuthorSpkiHex := '';
      info.Attestation.UnsignedReason := urAttestAuthorCeasedSigning;
    end
    else
      raise EPackageSigning.CreateFmt(
        'dpmRepositoryAttestation has unknown authorMode "%s"', [authorMode]);
  finally
    doc.Free;
  end;

  // Pull the author-sig binding hashes off the same repo signature. The
  // attribute is multi-value (CMS SET OF), one OCTET STRING per attested
  // author signature; surface each as hex for symmetry with SignerSpkiHex.
  // Absence is enforced (or not) by EnforceAuthorSigBinding /
  // EnforceUnsignedAttestation based on what authorMode declared.
  if decoded.FindSignedAttributeValues(cOidDpmVerifiedAuthorSigHash, hashValues) then
  begin
    SetLength(info.Attestation.BoundAuthorSigHashesHex, Length(hashValues));
    collected := 0;
    for i := 0 to High(hashValues) do
    begin
      hashBytes := UnwrapOctetString(hashValues[i]);
      if Length(hashBytes) > 0 then
      begin
        info.Attestation.BoundAuthorSigHashesHex[collected] := BytesToHex(hashBytes);
        Inc(collected);
      end;
    end;
    SetLength(info.Attestation.BoundAuthorSigHashesHex, collected);
  end;
end;

procedure TPackageSigningService.EnforceAuthorSigBinding(
  const blobs : IList<TPair<string, TBytes>>;
  const earlierSignatures : TArray<TSignatureInfo>;
  var repoSigInfo : TSignatureInfo);
var
  h, k : integer;
  name : string;
  blobHashHex : string;
  boundHash : string;
  matchedThisHash : boolean;
  matchedSpkis : TArray<string>;
  primaryMatched : boolean;
begin
  // Empty / absent binding attribute is a hard failure when the attestation
  // declares a signed author. Without the binding an attacker could strip
  // or swap the author signature without detection — the repo signature
  // covers the manifest, not the author sig blob. Gallery is expected to
  // emit dpmVerifiedAuthorSigHash (OID 1.3.6.1.4.1.95860.1.3) for every
  // attested author signature.
  if Length(repoSigInfo.Attestation.BoundAuthorSigHashesHex) = 0 then
  begin
    repoSigInfo.Valid := false;
    repoSigInfo.FailureReason :=
      'repository attestation declares a signed-author SPKI but the ' +
      'dpmVerifiedAuthorSigHash binding attribute (OID ' +
      string(cOidDpmVerifiedAuthorSigHash) + ') is missing — gallery did ' +
      'not bind the repository signature to any author signature, so the ' +
      'author<->repo link cannot be verified.';
    exit;
  end;

  // Every bound hash must resolve to an author signature actually present
  // in the archive. Collect the matched sigs' SPKIs while we walk, so the
  // primary-SPKI check below has the full set.
  SetLength(matchedSpkis, 0);
  for h := 0 to High(repoSigInfo.Attestation.BoundAuthorSigHashesHex) do
  begin
    boundHash := repoSigInfo.Attestation.BoundAuthorSigHashesHex[h];
    matchedThisHash := false;
    for k := 0 to blobs.Count - 1 do
    begin
      name := LowerCase(StringReplace(blobs[k].Key, '\', '/', [rfReplaceAll]));
      if not StartsText('signatures/author-', name) then
        Continue;
      blobHashHex := BytesToHex(FHashing.HashBytes(blobs[k].Value, haSha256));
      if not SameText(blobHashHex, boundHash) then
        Continue;
      matchedThisHash := true;
      // earlierSignatures is populated in the same iteration order as blobs.
      // Author sigs sort before repo sigs (author-N.p7s < repository-N.p7s
      // alphabetically), so the SPKI for any matched author has already
      // been recorded by the time we reach the repo sig.
      if (k < Length(earlierSignatures)) and
         (earlierSignatures[k].SignerSpkiHex <> '') then
      begin
        SetLength(matchedSpkis, Length(matchedSpkis) + 1);
        matchedSpkis[High(matchedSpkis)] := earlierSignatures[k].SignerSpkiHex;
      end;
      Break;
    end;
    if not matchedThisHash then
    begin
      repoSigInfo.Valid := false;
      repoSigInfo.FailureReason := Format(
        'repository attestation binds to %d author signature(s); hash %s... ' +
        'has no matching blob in the archive. The author signature was ' +
        'stripped or modified after the gallery signed it.',
        [Length(repoSigInfo.Attestation.BoundAuthorSigHashesHex),
         Copy(boundHash, 1, 16)]);
      exit;
    end;
  end;

  // The primary attestation SPKI (the gallery's registered publisher for
  // this namespace) must be the signer of at least one of the bound author
  // sigs we just matched. Co-signers / co-publishers may also be bound and
  // are vouched for by the gallery, but the primary SPKI anchors the
  // namespace identity — without that link, the attestation's authorSpki
  // field is unsupported by the actual blobs in the package.
  primaryMatched := false;
  for k := 0 to High(matchedSpkis) do
    if SameText(matchedSpkis[k], repoSigInfo.Attestation.AuthorSpkiHex) then
    begin
      primaryMatched := true;
      Break;
    end;
  if not primaryMatched then
  begin
    repoSigInfo.Valid := false;
    repoSigInfo.FailureReason := Format(
      'repository attestation declares primary author SPKI %s... but no ' +
      'bound author signature in the archive is signed by that key — the ' +
      'author signature was swapped for one from a different certificate.',
      [Copy(repoSigInfo.Attestation.AuthorSpkiHex, 1, 16)]);
    exit;
  end;
end;

procedure TPackageSigningService.EnforceUnsignedAttestation(
  const blobs : IList<TPair<string, TBytes>>;
  var repoSigInfo : TSignatureInfo);
var
  k : integer;
  name : string;
  authorBlobCount : integer;
  modeLabel : string;
begin
  authorBlobCount := 0;
  for k := 0 to blobs.Count - 1 do
  begin
    name := LowerCase(StringReplace(blobs[k].Key, '\', '/', [rfReplaceAll]));
    if StartsText('signatures/author-', name) and EndsText('.p7s', name) then
      Inc(authorBlobCount);
  end;
  if authorBlobCount = 0 then
    exit;

  case repoSigInfo.Attestation.UnsignedReason of
    urAttestNeverSigned         : modeLabel := 'never_author_signed';
    urAttestAuthorCeasedSigning : modeLabel := 'author_acknowledged_unsigned';
  else
    modeLabel := 'unspecified';
  end;

  repoSigInfo.Valid := false;
  repoSigInfo.FailureReason := Format(
    'repository attestation declares no verified author signer (authorMode=%s) ' +
    'but the archive contains %d author signature blob(s) — the archive ' +
    'contents do not match what the gallery attested at publish time. An ' +
    'author signature was added, swapped, or the gallery''s attestation is ' +
    'out of date.',
    [modeLabel, authorBlobCount]);
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
      begin
        try
          ReadRepositoryAttestation(blobs[i].Value, sigInfo);
        except
          on e : Exception do
          begin
            // Hard fail — a parse exception on a trusted repo sig MUST NOT
            // bypass the binding check. Silently ignoring would let an
            // attacker neuter author<->repo binding by corrupting the
            // attestation: parser raises -> Attestation.Present stays
            // false -> binding check skipped -> stripped/swapped author
            // sig goes undetected.
            sigInfo.Valid := false;
            sigInfo.FailureReason := Format(
              'repository attestation could not be parsed (%s: %s) — ' +
              'cannot verify the gallery''s claim about who signed this ' +
              'package',
              [e.ClassName, e.Message]);
            sigInfo.Attestation.Present := false;
          end;
        end;
        // A trusted repo sig is REQUIRED to carry an attestation. Gallery
        // never repo-signs without one; if Attestation.Present is false
        // here, the producer is broken or the attribute was stripped.
        if sigInfo.Valid and not sigInfo.Attestation.Present then
        begin
          sigInfo.Valid := false;
          sigInfo.FailureReason :=
            'repository signature has no dpmRepositoryAttestation attribute ' +
            '— gallery did not declare the registered publisher for this ' +
            'package, so the author<->repo binding cannot be verified';
        end;
        // Enforce the author-sig binding when the attestation declares a
        // signed publisher. Without this check, an attacker could strip the
        // author signature (or swap it for one from a different cert) and
        // the package would still verify — the attestation only proves the
        // gallery *said* "publisher X signed this", not that the author sig
        // actually present is from X.
        if sigInfo.Valid
           and sigInfo.Attestation.Present
           and (sigInfo.Attestation.AuthorSpkiHex <> '') then
          EnforceAuthorSigBinding(blobs, result.Signatures, sigInfo);

        // Inverse: gallery says "no verified author" but archive carries an
        // author-N.p7s blob. The verifier from §Client verification contract
        // requires this case to fail — otherwise an attacker could ship an
        // unverified author sig under an attestation that doesn't bind it,
        // and a downstream IDE rendering "Signed by …" off the author cert
        // would mislead the user.
        if sigInfo.Valid
           and sigInfo.Attestation.Present
           and (sigInfo.Attestation.AuthorSpkiHex = '') then
          EnforceUnsignedAttestation(blobs, sigInfo);
      end;
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
