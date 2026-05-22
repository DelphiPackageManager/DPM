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

function RunMode(mode : TValidationMode;
                 hasAnySig, hasValidAuthor, hasValidRepo : boolean;
                 out reason : string) : TVerificationOutcome;
var
  policy : TTrustPolicy;
begin
  policy := MakePolicy(mode);
  result := voInvalid;
  reason := '';
  TTrustModeEvaluator.Evaluate(policy, hasAnySig, hasValidAuthor, hasValidRepo, result, reason);
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

initialization
  TDUnitX.RegisterTestFixture(TTrustModeTests);

end.
