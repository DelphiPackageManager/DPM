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
    [Test] procedure DefaultConstructor_NoResource_YieldsSafeDefaults;
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

procedure TBuiltInTrustSetTests.DefaultConstructor_NoResource_YieldsSafeDefaults;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create;
  Assert.AreEqual(0, ts.Version, 'no resource: version is 0');
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
  Assert.AreEqual(0, Length(ts.RepositorySpkis));
end;

procedure TBuiltInTrustSetTests.EmptyYaml_YieldsSafeDefaults;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create('');
  Assert.AreEqual(0, ts.Version);
  Assert.AreEqual(Ord(vmPermissive), Ord(ts.DefaultValidationMode));
  Assert.AreEqual(0, Length(ts.RepositorySpkis));
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
  Assert.AreEqual(2, Length(pins));
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
  Assert.AreEqual(0, Length(ts.RepositorySpkis));
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
  Assert.AreEqual(0, Length(ts.RepositorySpkis));
end;

procedure TBuiltInTrustSetTests.RevokedRepositorySpkis_Default_IsEmpty;
var
  ts : ITrustSet;
begin
  ts := TBuiltInTrustSet.Create('');
  Assert.AreEqual(0, Length(ts.RevokedRepositorySpkis));
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
  Assert.AreEqual(2, Length(revoked));
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
  Assert.AreEqual(0, Length(ts.RevokedRepositorySpkis));
end;

initialization
  TDUnitX.RegisterTestFixture(TBuiltInTrustSetTests);

end.
