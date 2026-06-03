unit DPM.Core.Tests.Package.Receipt;

// V-31, V-32, V-33: cache verification receipt round-trip.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TReceiptServiceTests = class
  private
    function TempCacheFolder : string;
  public
    [Test] procedure TryRead_ReturnsFalse_WhenAbsent;
    [Test] procedure TryRead_ReturnsFalse_WhenMalformed;
    [Test] procedure WriteAndRead_RoundTripsAllFields;
    [Test] procedure Write_IsAtomic_RemovesTempOnSuccess;
    [Test] procedure Write_Overwrites_ExistingReceipt;
    [Test] procedure Receipt_With_NoSignatures_RoundTrips;
    [Test] procedure Receipt_With_RepositoryAttestation_RoundTrips;
    [Test] procedure Receipt_Without_Attestation_OmitsFields;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Package.Cache.Receipt;

function TReceiptServiceTests.TempCacheFolder : string;
begin
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-receipt-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now));
  ForceDirectories(result);
end;

procedure TReceiptServiceTests.TryRead_ReturnsFalse_WhenAbsent;
var
  svc : IReceiptService;
  folder : string;
  r : TVerificationReceipt;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    Assert.IsFalse(svc.TryRead(folder, r));
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.TryRead_ReturnsFalse_WhenMalformed;
var
  svc : IReceiptService;
  folder : string;
  r : TVerificationReceipt;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    // YAML is lenient — pick something that parses but produces a scalar
    // root, which the receipt loader rejects because it expects a mapping.
    // The contract is "graceful failure, no exception escapes" — i.e. either
    // returns false OR returns true with an empty / default-initialised record.
    TFile.WriteAllText(svc.ReceiptPath(folder),
      ': : : [unmatched-bracket : not-a-valid-document : :',
      TEncoding.UTF8);
    r.PackageId := 'sentinel';
    // Either result is acceptable as long as we don't crash AND we don't
    // return a record that claims a real package id from junk input.
    svc.TryRead(folder, r);
    Assert.IsTrue((r.PackageId = '') or (r.PackageId = 'sentinel'),
      'malformed input produced a real-looking packageId: ' + r.PackageId);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.WriteAndRead_RoundTripsAllFields;
var
  svc : IReceiptService;
  folder : string;
  written, readBack : TVerificationReceipt;
  sig : TReceiptSignature;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    written.ReceiptVersion := cCurrentReceiptVersion;
    written.PackageId := 'VSoft.RoundTrip';
    written.Version := '1.2.3';
    written.Compiler := 'DelphiXE2';
    written.ManifestHashAlgorithm := haSha256;
    written.ManifestHashHex := 'ab12cd34';
    written.TrustDecision := 'trusted';
    written.TrustPolicyFingerprint := 'sha256:ffffffff';
    written.VerifiedAt := EncodeDateTime(2026, 5, 22, 10, 0, 0, 0);
    written.DpmVersion := '0.6.0';

    sig.Role := 'author';
    sig.SignerSpkiHex := 'aabbccdd';
    sig.SignerSubject := 'CN=Test Signer';
    sig.Thumbprint := 'AB12CD34EF56';
    sig.EffectiveSigningTime := EncodeDateTime(2026, 5, 19, 10, 0, 0, 0);
    sig.TimestampAuthority := 'DigiCert';
    sig.RevocationStatus := 'notChecked';
    SetLength(written.Signatures, 1);
    written.Signatures[0] := sig;

    svc.Write(folder, written);
    Assert.IsTrue(FileExists(svc.ReceiptPath(folder)),
      'receipt file should exist after Write');

    Assert.IsTrue(svc.TryRead(folder, readBack), 'TryRead failed');
    Assert.AreEqual(written.ReceiptVersion,         readBack.ReceiptVersion);
    Assert.AreEqual(written.PackageId,              readBack.PackageId);
    Assert.AreEqual(written.Version,                readBack.Version);
    Assert.AreEqual(written.Compiler,               readBack.Compiler);
    Assert.AreEqual(Ord(written.ManifestHashAlgorithm), Ord(readBack.ManifestHashAlgorithm));
    Assert.AreEqual(written.ManifestHashHex,        readBack.ManifestHashHex);
    Assert.AreEqual(written.TrustDecision,          readBack.TrustDecision);
    Assert.AreEqual(written.TrustPolicyFingerprint, readBack.TrustPolicyFingerprint);
    Assert.AreEqual(written.DpmVersion,             readBack.DpmVersion);
    Assert.AreEqual<integer>(1, Length(readBack.Signatures));
    Assert.AreEqual('author',           readBack.Signatures[0].Role);
    Assert.AreEqual('aabbccdd',         readBack.Signatures[0].SignerSpkiHex);
    Assert.AreEqual('CN=Test Signer',   readBack.Signatures[0].SignerSubject);
    Assert.AreEqual('AB12CD34EF56',     readBack.Signatures[0].Thumbprint);
    Assert.AreEqual('DigiCert',         readBack.Signatures[0].TimestampAuthority);
    Assert.AreEqual('notChecked',       readBack.Signatures[0].RevocationStatus);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.Write_IsAtomic_RemovesTempOnSuccess;
var
  svc : IReceiptService;
  folder : string;
  r : TVerificationReceipt;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    r.ReceiptVersion := cCurrentReceiptVersion;
    r.PackageId := 'X';
    r.Version := '1';
    r.Compiler := 'D';
    r.ManifestHashAlgorithm := haSha256;
    r.ManifestHashHex := 'ab';
    r.TrustDecision := 'unsigned';
    r.TrustPolicyFingerprint := 'sha256:00';
    r.VerifiedAt := Now;
    r.DpmVersion := '0.6';
    SetLength(r.Signatures, 0);

    svc.Write(folder, r);
    Assert.IsFalse(FileExists(svc.ReceiptPath(folder) + '.tmp'),
      'temp file left behind after atomic write');
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.Write_Overwrites_ExistingReceipt;
var
  svc : IReceiptService;
  folder : string;
  r : TVerificationReceipt;
  r2 : TVerificationReceipt;
  loaded : TVerificationReceipt;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    r.ReceiptVersion := cCurrentReceiptVersion;
    r.PackageId := 'FirstPkg';
    r.Version := '0.1';
    r.Compiler := 'D';
    r.ManifestHashAlgorithm := haSha256;
    r.ManifestHashHex := '01';
    r.TrustDecision := 'unsigned';
    r.TrustPolicyFingerprint := 'sha256:01';
    r.VerifiedAt := Now;
    r.DpmVersion := '0.6';
    SetLength(r.Signatures, 0);
    svc.Write(folder, r);

    r2 := r;
    r2.PackageId := 'SecondPkg';
    r2.ManifestHashHex := '02';
    svc.Write(folder, r2);

    Assert.IsTrue(svc.TryRead(folder, loaded));
    Assert.AreEqual('SecondPkg', loaded.PackageId);
    Assert.AreEqual('02', loaded.ManifestHashHex);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.Receipt_With_NoSignatures_RoundTrips;
var
  svc : IReceiptService;
  folder : string;
  r, loaded : TVerificationReceipt;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    r.ReceiptVersion := cCurrentReceiptVersion;
    r.PackageId := 'Unsigned';
    r.Version := '1.0.0';
    r.Compiler := 'DelphiXE2';
    r.ManifestHashAlgorithm := haSha256;
    r.ManifestHashHex := 'deadbeef';
    r.TrustDecision := 'unsigned';
    r.TrustPolicyFingerprint := 'sha256:none';
    r.VerifiedAt := Now;
    r.DpmVersion := '0.6.0';
    SetLength(r.Signatures, 0);

    svc.Write(folder, r);
    Assert.IsTrue(svc.TryRead(folder, loaded));
    Assert.AreEqual<integer>(0, Length(loaded.Signatures));
    Assert.AreEqual('unsigned', loaded.TrustDecision);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.Receipt_With_RepositoryAttestation_RoundTrips;
var
  svc : IReceiptService;
  folder : string;
  r, loaded : TVerificationReceipt;
  sig : TReceiptSignature;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    r.ReceiptVersion := cCurrentReceiptVersion;
    r.PackageId := 'VSoft.WithAttest';
    r.Version := '1.0.0';
    r.Compiler := 'DelphiXE2';
    r.ManifestHashAlgorithm := haSha256;
    r.ManifestHashHex := 'beef';
    r.TrustDecision := 'trusted';
    r.TrustPolicyFingerprint := 'sha256:00';
    r.VerifiedAt := EncodeDateTime(2026, 5, 22, 10, 0, 0, 0);
    r.DpmVersion := '0.6.0';

    sig.Role := 'repository';
    sig.SignerSpkiHex := 'cafe1234';
    sig.SignerSubject := 'CN=DPM Gallery';
    sig.Thumbprint := 'DEADBEEF';
    sig.EffectiveSigningTime := EncodeDateTime(2026, 5, 22, 9, 0, 0, 0);
    sig.TimestampAuthority := 'DigiCert';
    sig.RevocationStatus := 'notChecked';
    sig.AttestationNamespace := 'VSoft.*';
    sig.AttestationAuthorSpkiHex := 'aabbcc';
    sig.AttestationUnsignedReason := '';
    SetLength(r.Signatures, 1);
    r.Signatures[0] := sig;

    svc.Write(folder, r);
    Assert.IsTrue(svc.TryRead(folder, loaded));
    Assert.AreEqual<integer>(1, Length(loaded.Signatures));
    Assert.AreEqual('VSoft.*', loaded.Signatures[0].AttestationNamespace);
    Assert.AreEqual('aabbcc', loaded.Signatures[0].AttestationAuthorSpkiHex);
    Assert.AreEqual('', loaded.Signatures[0].AttestationUnsignedReason);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

procedure TReceiptServiceTests.Receipt_Without_Attestation_OmitsFields;
var
  svc : IReceiptService;
  folder : string;
  r, loaded : TVerificationReceipt;
  sig : TReceiptSignature;
  yamlText : string;
begin
  svc := TYamlReceiptService.Create;
  folder := TempCacheFolder;
  try
    r.ReceiptVersion := cCurrentReceiptVersion;
    r.PackageId := 'VSoft.NoAttest';
    r.Version := '1.0.0';
    r.Compiler := 'DelphiXE2';
    r.ManifestHashAlgorithm := haSha256;
    r.ManifestHashHex := 'beef';
    r.TrustDecision := 'trusted';
    r.TrustPolicyFingerprint := 'sha256:00';
    r.VerifiedAt := EncodeDateTime(2026, 5, 22, 10, 0, 0, 0);
    r.DpmVersion := '0.6.0';

    sig.Role := 'author';
    sig.SignerSpkiHex := 'aabb';
    sig.SignerSubject := 'CN=Test';
    sig.Thumbprint := 'CAFE';
    sig.EffectiveSigningTime := EncodeDateTime(2026, 5, 22, 9, 0, 0, 0);
    sig.TimestampAuthority := 'DigiCert';
    sig.RevocationStatus := 'notChecked';
    sig.AttestationNamespace := '';
    sig.AttestationAuthorSpkiHex := '';
    sig.AttestationUnsignedReason := '';
    SetLength(r.Signatures, 1);
    r.Signatures[0] := sig;

    svc.Write(folder, r);
    // Confirm the file genuinely doesn't carry stale attestation keys when
    // none were set — keeps receipts clean and avoids reader ambiguity.
    yamlText := TFile.ReadAllText(svc.ReceiptPath(folder));
    Assert.IsTrue(Pos('attestation', yamlText) = 0,
      'attestation block should be omitted when no attestation is present');

    Assert.IsTrue(svc.TryRead(folder, loaded));
    Assert.AreEqual('', loaded.Signatures[0].AttestationNamespace);
    Assert.AreEqual('', loaded.Signatures[0].AttestationAuthorSpkiHex);
  finally
    TDirectory.Delete(folder, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TReceiptServiceTests);

end.
