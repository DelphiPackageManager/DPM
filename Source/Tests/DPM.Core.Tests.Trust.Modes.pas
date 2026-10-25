unit DPM.Core.Tests.Trust.Modes;

// Phase 2 §2.4 — exhaustive truth table for TTrustModeEvaluator.Evaluate
// across the four validation modes × (hasAnySignature, hasValidAuthor,
// hasValidTrustedRepo). Sixteen combinations covered explicitly so a
// regression in any mode shows up as a named failure.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTrustModeTests = class
  public
    [Test] procedure Permissive_NoSig_Unsigned;
    [Test] procedure Permissive_SigButUntrusted_UntrustedPublisher;
    [Test] procedure Permissive_ValidAuthor_Trusted;
    [Test] procedure Permissive_ValidTrustedRepo_Trusted;
    [Test] procedure Permissive_BothValid_Trusted;

    [Test] procedure Require_NoSig_Invalid;
    [Test] procedure Require_ValidAuthor_Trusted;
    [Test] procedure Require_ValidTrustedRepoOnly_Invalid;

    [Test] procedure RepositoryRequired_NoSig_Invalid;
    [Test] procedure RepositoryRequired_ValidAuthorOnly_Invalid;
    [Test] procedure RepositoryRequired_ValidTrustedRepo_Trusted;
    [Test] procedure RepositoryRequired_BothValid_Trusted;

    [Test] procedure AuthorAndRepository_NoSig_Invalid;
    [Test] procedure AuthorAndRepository_ValidAuthorOnly_Invalid;
    [Test] procedure AuthorAndRepository_ValidTrustedRepoOnly_Invalid;
    [Test] procedure AuthorAndRepository_BothValid_Trusted;

    [Test] procedure AuthorAndRepository_AuthorOnly_ReasonMentionsRepo;
    [Test] procedure AuthorAndRepository_RepoOnly_ReasonMentionsAuthor;

    // A signature that doesn't verify fails the package in every mode - even next to
    // signatures that do.
    [Test] procedure EveryMode_InvalidSignature_Invalid;
    // Untrusted root (self-signed / private CA) : valid, signer not trusted unless pinned.
    [Test] procedure Permissive_UntrustedRootAuthor_UntrustedPublisher;
    [Test] procedure Permissive_UntrustedRootAuthorWithTrustedRepo_Trusted;
    [Test] procedure Require_UntrustedRootAuthor_InvalidNamingTheSpki;
    [Test] procedure AuthorAndRepository_UntrustedRootAuthor_ReasonNamesTheSpki;

    [Test] procedure GatherFacts_NoSignatures;
    [Test] procedure GatherFacts_FailedSignatureGivesReason;
    [Test] procedure GatherFacts_ChainTrustedAuthorCounts;
    [Test] procedure GatherFacts_UntrustedRootAuthor_CountsOnlyWhenPinned;
    [Test] procedure GatherFacts_RepositoryCountsOnlyWhenTrusted;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Package.Signing.Interfaces;

function MakePolicy(mode : TValidationMode) : TTrustPolicy;
begin
  result.ValidationMode := mode;
  result.AuthorDowngradePolicy := adpBlock;
  result.AllowKeyCompromiseOverride := false;
  result.TrustSetVersion := 1;
  SetLength(result.TrustedPublishers, 0);
  SetLength(result.TrustedRepositories, 0);
end;

function MakeFacts(hasAnySig, hasValidAuthor, hasValidRepo : boolean) : TSignatureFacts;
begin
  result.HasAnySignature := hasAnySig;
  result.InvalidReason := '';
  result.HasValidAuthor := hasValidAuthor;
  result.UntrustedRootAuthorSpki := '';
  result.HasValidTrustedRepo := hasValidRepo;
end;

function RunFacts(mode : TValidationMode; const facts : TSignatureFacts; out reason : string) : TVerificationOutcome;
var
  policy : TTrustPolicy;
begin
  policy := MakePolicy(mode);
  result := voInvalid;
  reason := '';
  TTrustModeEvaluator.Evaluate(policy, facts, result, reason);
end;

//every signature present verified - the cases below are about who signed.
function RunMode(mode : TValidationMode;
                 hasAnySig, hasValidAuthor, hasValidRepo : boolean;
                 out reason : string) : TVerificationOutcome;
begin
  result := RunFacts(mode, MakeFacts(hasAnySig, hasValidAuthor, hasValidRepo), reason);
end;

function MakeSig(role : TSignatureRole; valid : boolean) : TSignatureInfo;
begin
  FillChar(result, SizeOf(result), 0);
  result.Role := role;
  result.Valid := valid;
  result.SignerSpkiHex := 'aa11';
  result.ChainTrusted := valid;
  if not valid then
  begin
    result.SignerSpkiHex := '';
    result.ClaimedSignerSpkiHex := 'aa11';
    result.ClaimedSubject := 'CN=Someone';
    result.FailureReason := 'CMS signature does not verify against manifest bytes';
  end;
end;

function UntrustedRootAuthor(pinned : boolean) : TSignatureInfo;
begin
  result := MakeSig(srAuthor, true);
  result.ChainTrusted := false;
  result.UntrustedRoot := true;
  result.SignerSpkiHex := 'bb22';
  result.PublisherTrusted := pinned;
end;

{ TTrustModeTests — permissive }

procedure TTrustModeTests.Permissive_NoSig_Unsigned;
var reason : string;
begin
  Assert.AreEqual(Ord(voUnsigned), Ord(RunMode(vmPermissive, false, false, false, reason)));
end;

procedure TTrustModeTests.Permissive_SigButUntrusted_UntrustedPublisher;
var reason : string;
begin
  Assert.AreEqual(Ord(voUntrustedPublisher),
    Ord(RunMode(vmPermissive, true, false, false, reason)));
end;

procedure TTrustModeTests.Permissive_ValidAuthor_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted), Ord(RunMode(vmPermissive, true, true, false, reason)));
end;

procedure TTrustModeTests.Permissive_ValidTrustedRepo_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted), Ord(RunMode(vmPermissive, true, false, true, reason)));
end;

procedure TTrustModeTests.Permissive_BothValid_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted), Ord(RunMode(vmPermissive, true, true, true, reason)));
end;

{ require }

procedure TTrustModeTests.Require_NoSig_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid), Ord(RunMode(vmRequire, false, false, false, reason)));
end;

procedure TTrustModeTests.Require_ValidAuthor_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted), Ord(RunMode(vmRequire, true, true, false, reason)));
end;

procedure TTrustModeTests.Require_ValidTrustedRepoOnly_Invalid;
var reason : string;
begin
  // Require is author-anchored: a repo signature alone doesn't satisfy it.
  Assert.AreEqual(Ord(voInvalid), Ord(RunMode(vmRequire, true, false, true, reason)));
end;

{ repository-required }

procedure TTrustModeTests.RepositoryRequired_NoSig_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid),
    Ord(RunMode(vmRepositoryRequired, false, false, false, reason)));
end;

procedure TTrustModeTests.RepositoryRequired_ValidAuthorOnly_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid),
    Ord(RunMode(vmRepositoryRequired, true, true, false, reason)));
end;

procedure TTrustModeTests.RepositoryRequired_ValidTrustedRepo_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted),
    Ord(RunMode(vmRepositoryRequired, true, false, true, reason)));
end;

procedure TTrustModeTests.RepositoryRequired_BothValid_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted),
    Ord(RunMode(vmRepositoryRequired, true, true, true, reason)));
end;

{ author-and-repository }

procedure TTrustModeTests.AuthorAndRepository_NoSig_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid),
    Ord(RunMode(vmAuthorAndRepository, false, false, false, reason)));
end;

procedure TTrustModeTests.AuthorAndRepository_ValidAuthorOnly_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid),
    Ord(RunMode(vmAuthorAndRepository, true, true, false, reason)));
end;

procedure TTrustModeTests.AuthorAndRepository_ValidTrustedRepoOnly_Invalid;
var reason : string;
begin
  Assert.AreEqual(Ord(voInvalid),
    Ord(RunMode(vmAuthorAndRepository, true, false, true, reason)));
end;

procedure TTrustModeTests.AuthorAndRepository_BothValid_Trusted;
var reason : string;
begin
  Assert.AreEqual(Ord(voTrusted),
    Ord(RunMode(vmAuthorAndRepository, true, true, true, reason)));
end;

procedure TTrustModeTests.AuthorAndRepository_AuthorOnly_ReasonMentionsRepo;
var reason : string;
begin
  RunMode(vmAuthorAndRepository, true, true, false, reason);
  Assert.IsTrue(Pos('repository', LowerCase(reason)) > 0,
    'reason should mention the missing repository signature, got: ' + reason);
end;

procedure TTrustModeTests.AuthorAndRepository_RepoOnly_ReasonMentionsAuthor;
var reason : string;
begin
  RunMode(vmAuthorAndRepository, true, false, true, reason);
  Assert.IsTrue(Pos('author', LowerCase(reason)) > 0,
    'reason should mention the missing author signature, got: ' + reason);
end;

procedure TTrustModeTests.EveryMode_InvalidSignature_Invalid;
var
  mode : TValidationMode;
  facts : TSignatureFacts;
  reason : string;
begin
  facts := MakeFacts(true, true, true);
  facts.InvalidReason := 'the author signature does not verify : bad';
  for mode := Low(TValidationMode) to High(TValidationMode) do
  begin
    Assert.AreEqual(Ord(voInvalid), Ord(RunFacts(mode, facts, reason)), 'mode ' + IntToStr(Ord(mode)));
    Assert.AreEqual(facts.InvalidReason, reason);
  end;
end;

procedure TTrustModeTests.Permissive_UntrustedRootAuthor_UntrustedPublisher;
var
  facts : TSignatureFacts;
  reason : string;
begin
  facts := MakeFacts(true, false, false);
  facts.UntrustedRootAuthorSpki := 'bb22';
  Assert.AreEqual(Ord(voUntrustedPublisher), Ord(RunFacts(vmPermissive, facts, reason)));
end;

procedure TTrustModeTests.Permissive_UntrustedRootAuthorWithTrustedRepo_Trusted;
var
  facts : TSignatureFacts;
  reason : string;
begin
  facts := MakeFacts(true, false, true);
  facts.UntrustedRootAuthorSpki := 'bb22';
  Assert.AreEqual(Ord(voTrusted), Ord(RunFacts(vmPermissive, facts, reason)));
end;

procedure TTrustModeTests.Require_UntrustedRootAuthor_InvalidNamingTheSpki;
var
  facts : TSignatureFacts;
  reason : string;
begin
  facts := MakeFacts(true, false, false);
  facts.UntrustedRootAuthorSpki := 'bb22';
  Assert.AreEqual(Ord(voInvalid), Ord(RunFacts(vmRequire, facts, reason)));
  Assert.IsTrue(Pos('sha256:bb22', reason) > 0, 'the reason should say which SPKI to pin, got: ' + reason);
  Assert.IsTrue(Pos('trustedPublishers', reason) > 0, 'the reason should say where to pin it, got: ' + reason);
end;

procedure TTrustModeTests.AuthorAndRepository_UntrustedRootAuthor_ReasonNamesTheSpki;
var
  facts : TSignatureFacts;
  reason : string;
begin
  facts := MakeFacts(true, false, true);
  facts.UntrustedRootAuthorSpki := 'bb22';
  Assert.AreEqual(Ord(voInvalid), Ord(RunFacts(vmAuthorAndRepository, facts, reason)));
  Assert.IsTrue(Pos('sha256:bb22', reason) > 0, 'got: ' + reason);
end;

procedure TTrustModeTests.GatherFacts_NoSignatures;
var
  facts : TSignatureFacts;
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 0);
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsFalse(facts.HasAnySignature);
  Assert.AreEqual('', facts.InvalidReason);
end;

procedure TTrustModeTests.GatherFacts_FailedSignatureGivesReason;
var
  facts : TSignatureFacts;
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 2);
  sigs[0] := MakeSig(srAuthor, false);
  sigs[1] := MakeSig(srRepository, true);
  sigs[1].RepositoryTrusted := true;
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsTrue(Pos('author signature does not verify', facts.InvalidReason) > 0, facts.InvalidReason);
  Assert.IsTrue(Pos('CN=Someone', facts.InvalidReason) > 0, 'the reason should say who it claims to be from: ' + facts.InvalidReason);
  Assert.IsTrue(facts.HasValidTrustedRepo, 'the facts about the other signatures are still gathered');
end;

procedure TTrustModeTests.GatherFacts_ChainTrustedAuthorCounts;
var
  facts : TSignatureFacts;
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, true);
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsTrue(facts.HasValidAuthor, 'a chain to a trusted root makes the author count, pinned or not');
  Assert.AreEqual('', facts.UntrustedRootAuthorSpki);
end;

procedure TTrustModeTests.GatherFacts_UntrustedRootAuthor_CountsOnlyWhenPinned;
var
  facts : TSignatureFacts;
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := UntrustedRootAuthor(false);
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsFalse(facts.HasValidAuthor, 'unpinned');
  Assert.AreEqual('bb22', facts.UntrustedRootAuthorSpki);
  Assert.AreEqual('', facts.InvalidReason, 'an untrusted root is not a verification failure');

  sigs[0] := UntrustedRootAuthor(true);
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsTrue(facts.HasValidAuthor, 'pinned in trustedPublishers');
  Assert.AreEqual('', facts.UntrustedRootAuthorSpki);
end;

procedure TTrustModeTests.GatherFacts_RepositoryCountsOnlyWhenTrusted;
var
  facts : TSignatureFacts;
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srRepository, true);
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsFalse(facts.HasValidTrustedRepo, 'not in trustedRepositories');
  Assert.IsTrue(facts.HasAnySignature);

  sigs[0].RepositoryTrusted := true;
  facts := TTrustModeEvaluator.GatherFacts(sigs);
  Assert.IsTrue(facts.HasValidTrustedRepo);
end;

initialization
  TDUnitX.RegisterTestFixture(TTrustModeTests);

end.
