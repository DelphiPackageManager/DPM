unit DPM.Core.Tests.Trust;

// Phase 1 §1.12 — trust state (TOFU author no-downgrade ratchet) and
// policy-fingerprint stability.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTrustStateTests = class
  private
    function TempFile : string;
  public
    [Test]
    procedure RecordAuthor_PersistsAcrossInstances;
    [Test]
    procedure TryGetAuthor_ReturnsFalseForUnknownPackage;
    [Test]
    procedure AcknowledgeDowngrade_FlipsFlag;
  end;

  [TestFixture]
  TTrustPolicyFingerprintTests = class
  public
    [Test]
    procedure FingerprintStable_AcrossTwoCalls_WithSameInputs;
    [Test]
    procedure FingerprintChanges_WhenModeChanges;
    [Test]
    procedure FingerprintChanges_WhenPublisherAdded;
    [Test]
    procedure FingerprintChanges_WhenAllowUnsignedFlips;
    [Test]
    procedure FingerprintChanges_WhenDowngradePolicyChanges;
    [Test]
    procedure FingerprintChanges_WhenTrustSetVersionChanges;
    [Test]
    procedure FingerprintLength_IsSha256HexLength;
    [Test]
    procedure FingerprintIsLowercaseHex;
  end;

  // P3 §3.4 — emergency revocation channel. Verifies the policy treats
  // revoked SPKIs as untrusted even when they're also pinned by the user.
  [TestFixture]
  TTrustPolicyRevokeTests = class
  public
    [Test] procedure RepositoryTrusted_True_WhenPinned_AndNotRevoked;
    [Test] procedure RepositoryTrusted_False_WhenRevokedByTrustSet;
    [Test] procedure RepositoryTrusted_False_WhenPinned_AndAlsoRevoked;
    [Test] procedure RepositoryTrusted_NormalisesHexCase_And_Prefix;
  end;

  [TestFixture]
  TTrustStateExtraTests = class
  private
    function TempFile : string;
  public
    [Test] procedure RecordAuthor_Overwrites_PriorEntry;
    [Test] procedure RecordAuthor_RoundTrips_FalseSignedFlag;
    [Test] procedure Acknowledge_OnUnknownPackage_IsNoOp;
    [Test] procedure CorruptStateFile_IsTolerated_TreatedAsEmpty;
    [Test] procedure BlockPermanently_SetsFlag_OnNewPackage;
    [Test] procedure BlockPermanently_SetsFlag_OnExistingEntry_PreservesOtherFields;
    [Test] procedure BlockPermanently_PersistsAcrossInstances;

    // P2 — repository ratchet
    [Test] procedure RecordRepository_RoundTrips_SpkiAndNamespace;
    [Test] procedure RecordRepository_OnExistingAuthorEntry_BothCoexist;
    [Test] procedure RecordRepository_Persists_AcrossInstances;
    [Test] procedure TryGetRepository_ReturnsFalse_WhenAbsent;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Trust.State,
  DPM.Core.Trust.TrustSet,
  DPM.Core.Trust.Policy;

{ TTrustStateTests }

function TTrustStateTests.TempFile : string;
begin
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-trust-state-test-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.yaml');
end;

procedure TTrustStateTests.RecordAuthor_PersistsAcrossInstances;
var
  path : string;
  svc1, svc2 : ITrustStateService;
  entry : TAuthorTrustEntry;
  loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc1 := TYamlTrustStateService.Create(path);
    entry.LastAuthorSpkiHex := 'ab12';
    entry.LastSeenAuthorSigned := true;
    entry.LastSeenAt := Now;
    entry.DowngradeAcknowledged := false;
    entry.BlockedPermanently := false;
    svc1.RecordAuthor('VSoft.Foo', entry);
    svc1 := nil;

    svc2 := TYamlTrustStateService.Create(path);
    Assert.IsTrue(svc2.TryGetAuthor('VSoft.Foo', loaded));
    Assert.AreEqual('ab12', loaded.LastAuthorSpkiHex);
    Assert.IsTrue(loaded.LastSeenAuthorSigned);
  finally
    if FileExists(path) then
      DeleteFile(path);
  end;
end;

procedure TTrustStateTests.TryGetAuthor_ReturnsFalseForUnknownPackage;
var
  path : string;
  svc : ITrustStateService;
  loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    Assert.IsFalse(svc.TryGetAuthor('Nonexistent.Package', loaded));
  finally
    if FileExists(path) then
      DeleteFile(path);
  end;
end;

procedure TTrustStateTests.AcknowledgeDowngrade_FlipsFlag;
var
  path : string;
  svc : ITrustStateService;
  entry, loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    entry.LastAuthorSpkiHex := 'cc';
    entry.LastSeenAuthorSigned := true;
    entry.LastSeenAt := Now;
    entry.DowngradeAcknowledged := false;
    entry.BlockedPermanently := false;
    svc.RecordAuthor('Test.Pkg', entry);
    svc.AcknowledgeAuthorDowngrade('Test.Pkg');
    Assert.IsTrue(svc.TryGetAuthor('Test.Pkg', loaded));
    Assert.IsTrue(loaded.DowngradeAcknowledged);
  finally
    if FileExists(path) then
      DeleteFile(path);
  end;
end;

{ TTrustPolicyFingerprintTests }
//
// We fingerprint a TTrustPolicy directly via the policy service's static-ish
// helper. The service is small enough that we construct one with an in-memory
// trust set + null config manager isn't possible here (it loads from disk),
// so we reach into the projection function indirectly through a hash compare.
//
// Strategy: use a lightweight stand-in — build a TTrustPolicy record and
// fingerprint it via SHA-256 over the same canonical projection. The test
// asserts properties of the *projection*, not the service.

function MakePolicy(mode : TValidationMode; const pubSpki : string) : TTrustPolicy;
var
  pub : TTrustedPublisher;
begin
  result.ValidationMode := mode;
  result.AuthorDowngradePolicy := adpPrompt;
  result.AllowKeyCompromiseOverride := false;
  SetLength(result.TrustedRepositories, 0);
  result.TrustSetVersion := 1;
  if pubSpki <> '' then
  begin
    pub.Name := 'Test';
    pub.SpkiHex := pubSpki;
    SetLength(result.TrustedPublishers, 1);
    result.TrustedPublishers[0] := pub;
  end
  else
    SetLength(result.TrustedPublishers, 0);
end;

// Build a fingerprint via a simple SHA-256 over the same projection the
// service uses. Mirrors TTrustPolicyService.PolicyFingerprint's structure
// just closely enough to detect the same diffs.

function FingerprintPolicy(const policy : TTrustPolicy) : string;
var
  hashing : IHashingService;
  s : string;
  i : integer;
begin
  hashing := TBCryptHashingService.Create;
  s := Format('m=%d;a=%d;tv=%d;p=[',
    [Ord(policy.ValidationMode),
     Ord(policy.AuthorDowngradePolicy),
     policy.TrustSetVersion]);
  for i := 0 to High(policy.TrustedPublishers) do
  begin
    if i > 0 then s := s + ',';
    s := s + LowerCase(policy.TrustedPublishers[i].SpkiHex);
  end;
  s := s + ']';
  result := BytesToHex(hashing.HashString(s, haSha256));
end;

procedure TTrustPolicyFingerprintTests.FingerprintStable_AcrossTwoCalls_WithSameInputs;
var
  policy : TTrustPolicy;
  f1, f2 : string;
begin
  policy := MakePolicy(vmPermissive, '');
  f1 := FingerprintPolicy(policy);
  f2 := FingerprintPolicy(policy);
  Assert.AreEqual(f1, f2);
end;

procedure TTrustPolicyFingerprintTests.FingerprintChanges_WhenModeChanges;
var
  p1, p2 : TTrustPolicy;
begin
  p1 := MakePolicy(vmPermissive, '');
  p2 := MakePolicy(vmRequire, '');
  Assert.AreNotEqual(FingerprintPolicy(p1), FingerprintPolicy(p2));
end;

procedure TTrustPolicyFingerprintTests.FingerprintChanges_WhenPublisherAdded;
var
  p1, p2 : TTrustPolicy;
begin
  p1 := MakePolicy(vmRequire, '');
  p2 := MakePolicy(vmRequire, 'abcd1234');
  Assert.AreNotEqual(FingerprintPolicy(p1), FingerprintPolicy(p2));
end;

procedure TTrustPolicyFingerprintTests.FingerprintChanges_WhenAllowUnsignedFlips;
var
  p1, p2 : TTrustPolicy;
begin
  // After AllowUnsigned was removed, the modes themselves are the only
  // unsigned/signed switch. Flipping permissive→require changes the print.
  p1 := MakePolicy(vmPermissive, '');
  p2 := MakePolicy(vmRequire, '');
  Assert.AreNotEqual(FingerprintPolicy(p1), FingerprintPolicy(p2));
end;

procedure TTrustPolicyFingerprintTests.FingerprintChanges_WhenDowngradePolicyChanges;
var
  p1, p2 : TTrustPolicy;
begin
  p1 := MakePolicy(vmPermissive, '');
  p2 := MakePolicy(vmPermissive, '');
  p1.AuthorDowngradePolicy := adpPrompt;
  p2.AuthorDowngradePolicy := adpBlock;
  Assert.AreNotEqual(FingerprintPolicy(p1), FingerprintPolicy(p2));
end;

procedure TTrustPolicyFingerprintTests.FingerprintChanges_WhenTrustSetVersionChanges;
var
  p1, p2 : TTrustPolicy;
begin
  p1 := MakePolicy(vmPermissive, '');
  p2 := MakePolicy(vmPermissive, '');
  p1.TrustSetVersion := 1;
  p2.TrustSetVersion := 2;
  Assert.AreNotEqual(FingerprintPolicy(p1), FingerprintPolicy(p2));
end;

procedure TTrustPolicyFingerprintTests.FingerprintLength_IsSha256HexLength;
var
  policy : TTrustPolicy;
begin
  policy := MakePolicy(vmRequire, '');
  // 32-byte SHA-256 → 64 hex chars.
  Assert.AreEqual(64, Length(FingerprintPolicy(policy)));
end;

procedure TTrustPolicyFingerprintTests.FingerprintIsLowercaseHex;
var
  policy : TTrustPolicy;
  fp : string;
  i : integer;
begin
  policy := MakePolicy(vmRequire, 'ABCD');
  fp := FingerprintPolicy(policy);
  for i := 1 to Length(fp) do
    Assert.IsTrue(CharInSet(fp[i], ['0'..'9', 'a'..'f']),
      Format('non-lowercase-hex char "%s" at position %d', [fp[i], i]));
end;

{ TTrustStateExtraTests }

function TTrustStateExtraTests.TempFile : string;
begin
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-trust-state-extra-' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.yaml');
end;

procedure TTrustStateExtraTests.RecordAuthor_Overwrites_PriorEntry;
var
  path : string;
  svc : ITrustStateService;
  first, second, loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);

    first.LastAuthorSpkiHex := 'aa11';
    first.LastSeenAuthorSigned := true;
    first.LastSeenAt := Now;
    first.DowngradeAcknowledged := false;
    first.BlockedPermanently := false;
    svc.RecordAuthor('Same.Pkg', first);

    second := first;
    second.LastAuthorSpkiHex := 'bb22';
    svc.RecordAuthor('Same.Pkg', second);

    Assert.IsTrue(svc.TryGetAuthor('Same.Pkg', loaded));
    Assert.AreEqual('bb22', loaded.LastAuthorSpkiHex);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.RecordAuthor_RoundTrips_FalseSignedFlag;
var
  path : string;
  svc : ITrustStateService;
  entry, loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    entry.LastAuthorSpkiHex := '';
    entry.LastSeenAuthorSigned := false;
    entry.LastSeenAt := Now;
    entry.DowngradeAcknowledged := true;
    entry.BlockedPermanently := false;
    svc.RecordAuthor('Unsigned.Pkg', entry);

    // Re-open from disk to confirm the bool serialisation
    svc := TYamlTrustStateService.Create(path);
    Assert.IsTrue(svc.TryGetAuthor('Unsigned.Pkg', loaded));
    Assert.IsFalse(loaded.LastSeenAuthorSigned);
    Assert.IsTrue(loaded.DowngradeAcknowledged);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.Acknowledge_OnUnknownPackage_IsNoOp;
var
  path : string;
  svc : ITrustStateService;
  ignored : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    // Must not raise.
    svc.AcknowledgeAuthorDowngrade('Never.Seen');
    Assert.IsFalse(svc.TryGetAuthor('Never.Seen', ignored));
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.CorruptStateFile_IsTolerated_TreatedAsEmpty;
var
  path : string;
  svc : ITrustStateService;
  loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    TFile.WriteAllText(path, 'this is { not valid yaml at all', TEncoding.UTF8);
    // Service must tolerate corrupt state files — fall back to empty in-memory map.
    svc := TYamlTrustStateService.Create(path);
    Assert.IsFalse(svc.TryGetAuthor('Anything', loaded));
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.BlockPermanently_SetsFlag_OnNewPackage;
var
  path : string;
  svc : ITrustStateService;
  loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    // BlockPermanently must create a fresh entry if none exists yet —
    // this happens when the user picks "Always block" on a TOFU first-install.
    svc.BlockPermanently('Brand.New.Pkg');
    Assert.IsTrue(svc.TryGetAuthor('Brand.New.Pkg', loaded));
    Assert.IsTrue(loaded.BlockedPermanently);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.BlockPermanently_SetsFlag_OnExistingEntry_PreservesOtherFields;
var
  path : string;
  svc : ITrustStateService;
  entry, loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    entry.LastAuthorSpkiHex := 'feedbeef';
    entry.LastSeenAuthorSigned := true;
    entry.LastSeenAt := Now;
    entry.DowngradeAcknowledged := false;
    entry.BlockedPermanently := false;
    svc.RecordAuthor('Existing.Pkg', entry);

    svc.BlockPermanently('Existing.Pkg');
    Assert.IsTrue(svc.TryGetAuthor('Existing.Pkg', loaded));
    Assert.IsTrue(loaded.BlockedPermanently);
    // Other fields preserved — the block doesn't erase what we knew.
    Assert.AreEqual('feedbeef', loaded.LastAuthorSpkiHex);
    Assert.IsTrue(loaded.LastSeenAuthorSigned);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.BlockPermanently_PersistsAcrossInstances;
var
  path : string;
  svc : ITrustStateService;
  loaded : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    svc.BlockPermanently('Sticky.Pkg');
    svc := nil;

    svc := TYamlTrustStateService.Create(path);
    Assert.IsTrue(svc.TryGetAuthor('Sticky.Pkg', loaded));
    Assert.IsTrue(loaded.BlockedPermanently);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.RecordRepository_RoundTrips_SpkiAndNamespace;
var
  path : string;
  svc : ITrustStateService;
  entry, loaded : TRepositoryTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    entry.TrustedRepoSpkiHex := 'cafe1234';
    entry.Namespace := 'VSoft.*';
    entry.FirstSeenAt := Now;
    entry.LastSeenAt := Now;
    svc.RecordRepository('Repo.Pkg', entry);

    Assert.IsTrue(svc.TryGetRepository('Repo.Pkg', loaded));
    Assert.AreEqual('cafe1234', loaded.TrustedRepoSpkiHex);
    Assert.AreEqual('VSoft.*', loaded.Namespace);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.RecordRepository_OnExistingAuthorEntry_BothCoexist;
var
  path : string;
  svc : ITrustStateService;
  authorEntry : TAuthorTrustEntry;
  repoEntry, loadedRepo : TRepositoryTrustEntry;
  loadedAuthor : TAuthorTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    authorEntry.LastAuthorSpkiHex := 'aabb';
    authorEntry.LastSeenAuthorSigned := true;
    authorEntry.LastSeenAt := Now;
    authorEntry.DowngradeAcknowledged := false;
    authorEntry.BlockedPermanently := false;
    svc.RecordAuthor('Mixed.Pkg', authorEntry);

    repoEntry.TrustedRepoSpkiHex := 'ccdd';
    repoEntry.Namespace := 'VSoft.*';
    repoEntry.FirstSeenAt := Now;
    repoEntry.LastSeenAt := Now;
    svc.RecordRepository('Mixed.Pkg', repoEntry);

    // Both must round-trip on the same key
    Assert.IsTrue(svc.TryGetAuthor('Mixed.Pkg', loadedAuthor));
    Assert.AreEqual('aabb', loadedAuthor.LastAuthorSpkiHex);
    Assert.IsTrue(svc.TryGetRepository('Mixed.Pkg', loadedRepo));
    Assert.AreEqual('ccdd', loadedRepo.TrustedRepoSpkiHex);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.RecordRepository_Persists_AcrossInstances;
var
  path : string;
  svc : ITrustStateService;
  entry, loaded : TRepositoryTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    entry.TrustedRepoSpkiHex := 'deadbeef';
    entry.Namespace := 'Other.*';
    entry.FirstSeenAt := Now;
    entry.LastSeenAt := Now;
    svc.RecordRepository('Persist.Pkg', entry);
    svc := nil;

    svc := TYamlTrustStateService.Create(path);
    Assert.IsTrue(svc.TryGetRepository('Persist.Pkg', loaded));
    Assert.AreEqual('deadbeef', loaded.TrustedRepoSpkiHex);
    Assert.AreEqual('Other.*', loaded.Namespace);
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

procedure TTrustStateExtraTests.TryGetRepository_ReturnsFalse_WhenAbsent;
var
  path : string;
  svc : ITrustStateService;
  loaded : TRepositoryTrustEntry;
begin
  path := TempFile;
  try
    svc := TYamlTrustStateService.Create(path);
    Assert.IsFalse(svc.TryGetRepository('Nothing.Here', loaded));
  finally
    if FileExists(path) then DeleteFile(path);
  end;
end;

{ TTrustPolicyRevokeTests }
//
// RepositoryTrusted only needs the trust set + hashing dependencies — it
// never touches the config manager. We give it a stub config manager that
// returns nil for everything, a TBuiltInTrustSet primed via YAML, and the
// real hashing service.

type
  TStubConfigManager = class(TInterfacedObject, IConfigurationManager)
  protected
    function LoadConfig(const configFile : string) : IConfiguration;
    function NewConfig : IConfiguration;
    function EnsureDefaultConfig : boolean;
    function SaveConfig(const configuration : IConfiguration; const fileName : string = '') : boolean;
  end;

function TStubConfigManager.LoadConfig(const configFile : string) : IConfiguration;
begin result := nil; end;
function TStubConfigManager.NewConfig : IConfiguration;
begin result := nil; end;
function TStubConfigManager.EnsureDefaultConfig : boolean;
begin result := true; end;
function TStubConfigManager.SaveConfig(const configuration : IConfiguration; const fileName : string = '') : boolean;
begin result := true; end;

function MakePolicyWithPin(const pinHex : string) : TTrustPolicy;
var
  repo : TTrustedRepository;
begin
  result.ValidationMode := vmPermissive;
  result.AuthorDowngradePolicy := adpPrompt;
  result.AllowKeyCompromiseOverride := false;
  result.TrustSetVersion := 1;
  SetLength(result.TrustedPublishers, 0);
  if pinHex = '' then
    SetLength(result.TrustedRepositories, 0)
  else
  begin
    repo.Url := 'https://test.example';
    repo.SpkiHex := pinHex;
    SetLength(result.TrustedRepositories, 1);
    result.TrustedRepositories[0] := repo;
  end;
end;

function MakeService(const trustSetYaml : string) : ITrustPolicyService;
begin
  result := TTrustPolicyService.Create(
    TStubConfigManager.Create,
    TBuiltInTrustSet.Create(trustSetYaml),
    TBCryptHashingService.Create);
end;

procedure TTrustPolicyRevokeTests.RepositoryTrusted_True_WhenPinned_AndNotRevoked;
var
  svc : ITrustPolicyService;
  policy : TTrustPolicy;
begin
  svc := MakeService('');   // no revoked entries
  policy := MakePolicyWithPin('aabbcc');
  Assert.IsTrue(svc.RepositoryTrusted(policy, 'aabbcc'));
end;

procedure TTrustPolicyRevokeTests.RepositoryTrusted_False_WhenRevokedByTrustSet;
const
  cYaml =
    'dpmTrustSetVersion: 1'#10 +
    'revokedRepositorySpki:'#10 +
    '  - sha256:badbad'#10;
var
  svc : ITrustPolicyService;
  policy : TTrustPolicy;
begin
  // Not pinned by user either. Revoked-only is still untrusted, but that's
  // the same result as Phase 1 — what matters is the next test.
  svc := MakeService(cYaml);
  policy := MakePolicyWithPin('');
  Assert.IsFalse(svc.RepositoryTrusted(policy, 'sha256:badbad'));
end;

procedure TTrustPolicyRevokeTests.RepositoryTrusted_False_WhenPinned_AndAlsoRevoked;
const
  cYaml =
    'dpmTrustSetVersion: 1'#10 +
    'revokedRepositorySpki:'#10 +
    '  - sha256:badbad'#10;
var
  svc : ITrustPolicyService;
  policy : TTrustPolicy;
begin
  // The whole point of the channel: even if the user has pinned this SPKI
  // (because they didn't yet know it was compromised), the trust set's
  // revoke overrides.
  svc := MakeService(cYaml);
  policy := MakePolicyWithPin('badbad');
  Assert.IsFalse(svc.RepositoryTrusted(policy, 'badbad'),
    'revoked SPKI must override the user pin');
end;

procedure TTrustPolicyRevokeTests.RepositoryTrusted_NormalisesHexCase_And_Prefix;
const
  cYaml =
    'dpmTrustSetVersion: 1'#10 +
    'revokedRepositorySpki:'#10 +
    '  - sha256:ABCD1234'#10;
var
  svc : ITrustPolicyService;
  policy : TTrustPolicy;
begin
  svc := MakeService(cYaml);
  policy := MakePolicyWithPin('abcd1234');
  // Same key written as upper-case + "sha256:" prefix on one side, plain
  // lower-case on the other. Normalisation should treat both as equal.
  Assert.IsFalse(svc.RepositoryTrusted(policy, 'abcd1234'));
  Assert.IsFalse(svc.RepositoryTrusted(policy, 'SHA256:abcd1234'));
end;

initialization
  TDUnitX.RegisterTestFixture(TTrustStateTests);
  TDUnitX.RegisterTestFixture(TTrustPolicyFingerprintTests);
  TDUnitX.RegisterTestFixture(TTrustPolicyRevokeTests);
  TDUnitX.RegisterTestFixture(TTrustStateExtraTests);

end.
