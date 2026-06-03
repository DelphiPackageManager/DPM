unit DPM.Core.Tests.Trust.TrustSet;

// TBuiltInTrustSet — verify the YAML loader's defaults and field handling.
// The default constructor reads from an RC resource; this test exe has no
// such resource, so we cover both the safe-defaults path and the explicit-
// YAML constructor.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBuiltInTrustSetTests = class
  public
    // Default constructor reads the DPM_TRUST_SET resource that DPM.TrustSet.rc
    // bakes into every consumer binary. The test exe gets it too via
    // DPM.Core.Trust.TrustSet's $R directive, so the test asserts what we
    // ship rather than the (Phase 1) safe-empty fallback.
    [Test] procedure DefaultConstructor_LoadsBuiltInResource;
    [Test] procedure EmptyYaml_YieldsSafeDefaults;

    [Test] procedure LoadsVersion_FromYaml;
    [Test] procedure LoadsDefaultMode_Permissive;
    [Test] procedure LoadsDefaultMode_Require;
    [Test] procedure LoadsDefaultMode_RepositoryRequired;
    [Test] procedure LoadsDefaultMode_AuthorAndRepository;
    [Test] procedure UnknownDefaultMode_FallsBackToPermissive;

    [Test] procedure LoadsMultipleRepositorySpkiEntries;
    [Test] procedure MalformedYaml_Tolerated_FallsBackToDefaults;
    [Test] procedure NonMappingRoot_Tolerated;

    // P3 §3.4 — revoked-repository channel
    [Test] procedure RevokedRepositorySpkis_Default_IsEmpty;
    [Test] procedure RevokedRepositorySpkis_LoadedFromYaml;
    [Test] procedure RevokedRepositorySpkis_Tolerates_MissingSequence;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Trust.TrustSet;

procedure TBuiltInTrustSetTests.DefaultConstructor_LoadsBuiltInResource;
var
  ts : ITrustSet;
  pins : TArray<TTrustedRepository>;
begin
  // Linked-in resource from DPM.TrustSet.rc / dpm-trust-set.yaml. Locked
  // to the shipped values so a change to the YAML that isn't matched here
  // is caught — this is the canonical source of trust for every binary.
  ts := TBuiltInTrustSet.Create;
  Assert.AreEqual(1, ts.Version, 'shipped trust-set version');
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));

  pins := ts.RepositorySpkis;
  Assert.AreEqual<integer>(1, Length(pins), 'should ship exactly one trusted repo');
  Assert.AreEqual('DPM Official Gallery', pins[0].Url);
  Assert.AreEqual(
    'sha256:c2d7996a815b4d2061789f367b2fcfbb08afcc4c31af47c4efce91a4413a39f8',
    pins[0].SpkiHex);

  // Revocation channel ships empty; entries are added in an emergency.
  Assert.AreEqual<integer>(0, Length(ts.RevokedRepositorySpkis));
end;

procedure TBuiltInTrustSetTests.EmptyYaml_YieldsSafeDefaults;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create('');
  Assert.AreEqual(0, ts.Version);
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
  Assert.AreEqual<integer>(0, Length(ts.RepositorySpkis));
end;

procedure TBuiltInTrustSetTests.LoadsVersion_FromYaml;
const
  cYaml = 'dpmTrustSetVersion: 42'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(42, ts.Version);
end;

procedure TBuiltInTrustSetTests.LoadsDefaultMode_Permissive;
const
  cYaml = 'defaultValidationMode: permissive'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
end;

procedure TBuiltInTrustSetTests.LoadsDefaultMode_Require;
const
  cYaml = 'defaultValidationMode: require'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(Ord(vmRequire), Ord(ts.DefaultValidationMode));
end;

procedure TBuiltInTrustSetTests.LoadsDefaultMode_RepositoryRequired;
const
  cYaml = 'defaultValidationMode: repository-required'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(Ord(vmRepositoryRequired), Ord(ts.DefaultValidationMode));
end;

procedure TBuiltInTrustSetTests.LoadsDefaultMode_AuthorAndRepository;
const
  cYaml = 'defaultValidationMode: author-and-repository'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(Ord(vmAuthorAndRepository), Ord(ts.DefaultValidationMode));
end;

procedure TBuiltInTrustSetTests.UnknownDefaultMode_FallsBackToPermissive;
const
  cYaml = 'defaultValidationMode: nonsense-mode'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
end;

procedure TBuiltInTrustSetTests.LoadsMultipleRepositorySpkiEntries;
const
  cYaml =
    'dpmTrustSetVersion: 1'#10 +
    'repositorySpki:'#10 +
    '  - name: First'#10 +
    '    spki: sha256:aaaa'#10 +
    '  - name: Second'#10 +
    '    spki: sha256:bbbb'#10;
var
  ts : ITrustSet;
  pins : TArray<TTrustedRepository>;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  pins := ts.RepositorySpkis;
  Assert.AreEqual<integer>(2, Length(pins));
  Assert.AreEqual('First',        pins[0].Url);
  Assert.AreEqual('sha256:aaaa',  pins[0].SpkiHex);
  Assert.AreEqual('Second',       pins[1].Url);
  Assert.AreEqual('sha256:bbbb',  pins[1].SpkiHex);
end;

procedure TBuiltInTrustSetTests.MalformedYaml_Tolerated_FallsBackToDefaults;
const
  cBad = '{ this is not [ valid yaml';
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cBad);
  // Should silently fall back to defaults — no exception escapes.
  Assert.AreEqual(0, ts.Version);
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
  Assert.AreEqual<integer>(0, Length(ts.RepositorySpkis));
end;

procedure TBuiltInTrustSetTests.NonMappingRoot_Tolerated;
const
  // Top-level sequence — not a valid trust-set shape, but must not crash.
  cYaml = '- 1'#10 + '- 2'#10;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual(0, ts.Version);
  Assert.AreEqual<integer>(0, Length(ts.RepositorySpkis));
end;

procedure TBuiltInTrustSetTests.RevokedRepositorySpkis_Default_IsEmpty;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create('');
  Assert.AreEqual<integer>(0, Length(ts.RevokedRepositorySpkis));
end;

procedure TBuiltInTrustSetTests.RevokedRepositorySpkis_LoadedFromYaml;
const
  cYaml =
    'dpmTrustSetVersion: 7'#10 +
    'revokedRepositorySpki:'#10 +
    '  - sha256:abcd1234'#10 +
    '  - sha256:cafebabe'#10;
var
  ts : ITrustSet;
  revoked : TArray<string>;
begin
  ts := TBuiltInTrustSet.Create(cYaml);
  revoked := ts.RevokedRepositorySpkis;
  Assert.AreEqual<integer>(2, Length(revoked));
  Assert.AreEqual('sha256:abcd1234', revoked[0]);
  Assert.AreEqual('sha256:cafebabe', revoked[1]);
end;

procedure TBuiltInTrustSetTests.RevokedRepositorySpkis_Tolerates_MissingSequence;
const
  cYaml =
    'dpmTrustSetVersion: 1'#10 +
    'repositorySpki:'#10 +
    '  - name: Only'#10 +
    '    spki: sha256:aa'#10;
var
  ts : ITrustSet;
begin
  // No revokedRepositorySpki key at all — must yield an empty array, not raise.
  ts := TBuiltInTrustSet.Create(cYaml);
  Assert.AreEqual<integer>(0, Length(ts.RevokedRepositorySpkis));
end;

initialization
  TDUnitX.RegisterTestFixture(TBuiltInTrustSetTests);

end.
