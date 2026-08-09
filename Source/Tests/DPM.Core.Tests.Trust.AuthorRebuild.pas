unit DPM.Core.Tests.Trust.AuthorRebuild;

// Truth table for TAuthorRebuildExemption.Applies — the escape hatch that lets a
// package author install a locally built copy of an id they have already
// published, without the V-24 repository ratchet blocking it.
//
// The exemption must hold in exactly one situation: the prior author high-water
// mark is a signed key, and the current build carries a valid author signature
// from that same key. Every other combination has to fail closed, because the
// only thing standing between "author rebuilding their own package" and "third
// party shadowing a published id" is possession of the author's signing key.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAuthorRebuildExemptionTests = class
  public
    // Granted.
    [Test] procedure Applies_WhenValidAuthorSig_MatchesPriorSpki;
    [Test] procedure Applies_IsCaseInsensitive_OnSpkiHex;
    [Test] procedure Applies_WhenMatchingSigIsNotFirstInArray;

    // Denied — nothing recorded to match against.
    [Test] procedure Denied_WhenNoPriorAuthorEntry;
    [Test] procedure Denied_WhenPriorEntryWasUnsigned;
    [Test] procedure Denied_WhenPriorSpkiIsEmpty;

    // Denied — the current build cannot prove the author identity.
    [Test] procedure Denied_WhenBuildHasNoSignatures;
    [Test] procedure Denied_WhenAuthorSigIsInvalid;
    [Test] procedure Denied_WhenAuthorSigIsFromDifferentKey;
    [Test] procedure Denied_WhenOnlyRepoSigMatchesPriorSpki;

    // Denied — user decision outranks the exemption.
    [Test] procedure Denied_WhenPackagePermanentlyBlocked;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Package.Signing.Interfaces;

const
  cAuthorSpki = 'aabbccdd11223344';
  cOtherSpki  = '99887766ffeeddcc';

function MakePriorAuthor(const spkiHex : string; signed : boolean;
                         blocked : boolean = false) : TAuthorTrustEntry;
begin
  result.LastAuthorSpkiHex    := spkiHex;
  result.LastSeenAuthorSigned := signed;
  result.LastSeenAt           := 0;
  result.DowngradeAcknowledged := false;
  result.BlockedPermanently   := blocked;
end;

function MakeSig(role : TSignatureRole; const spkiHex : string;
                 valid : boolean) : TSignatureInfo;
begin
  // Only the three fields the predicate reads need setting; the rest stay at
  // their zero-initialised defaults.
  result := Default(TSignatureInfo);
  result.Role          := role;
  result.SignerSpkiHex := spkiHex;
  result.Valid         := valid;
end;

{ granted }

procedure TAuthorRebuildExemptionTests.Applies_WhenValidAuthorSig_MatchesPriorSpki;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cAuthorSpki, true);
  Assert.IsTrue(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Applies_IsCaseInsensitive_OnSpkiHex;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, UpperCase(cAuthorSpki), true);
  Assert.IsTrue(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(LowerCase(cAuthorSpki), true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Applies_WhenMatchingSigIsNotFirstInArray;
var
  sigs : TArray<TSignatureInfo>;
begin
  // A build can carry several author signatures; any one valid match is enough.
  SetLength(sigs, 3);
  sigs[0] := MakeSig(srRepository, cOtherSpki, true);
  sigs[1] := MakeSig(srAuthor, cOtherSpki, true);
  sigs[2] := MakeSig(srAuthor, cAuthorSpki, true);
  Assert.IsTrue(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

{ denied — nothing recorded to match against }

procedure TAuthorRebuildExemptionTests.Denied_WhenNoPriorAuthorEntry;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cAuthorSpki, true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(false,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Denied_WhenPriorEntryWasUnsigned;
var
  sigs : TArray<TSignatureInfo>;
begin
  // The id was last seen unsigned, so no author identity was ever established
  // for it — a signature now proves nothing about who published it before.
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cAuthorSpki, true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, false), sigs));
end;

procedure TAuthorRebuildExemptionTests.Denied_WhenPriorSpkiIsEmpty;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, '', true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor('', true), sigs));
end;

{ denied — current build cannot prove the author identity }

procedure TAuthorRebuildExemptionTests.Denied_WhenBuildHasNoSignatures;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 0);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Denied_WhenAuthorSigIsInvalid;
var
  sigs : TArray<TSignatureInfo>;
begin
  // Right key, but the signature did not verify — it proves nothing.
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cAuthorSpki, false);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Denied_WhenAuthorSigIsFromDifferentKey;
var
  sigs : TArray<TSignatureInfo>;
begin
  // This is the shadowing case the ratchet exists to catch.
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cOtherSpki, true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

procedure TAuthorRebuildExemptionTests.Denied_WhenOnlyRepoSigMatchesPriorSpki;
var
  sigs : TArray<TSignatureInfo>;
begin
  // Role matters: a repository signature carrying the author's SPKI must not
  // satisfy an author-identity test.
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srRepository, cAuthorSpki, true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true), sigs));
end;

{ denied — user decision outranks the exemption }

procedure TAuthorRebuildExemptionTests.Denied_WhenPackagePermanentlyBlocked;
var
  sigs : TArray<TSignatureInfo>;
begin
  SetLength(sigs, 1);
  sigs[0] := MakeSig(srAuthor, cAuthorSpki, true);
  Assert.IsFalse(TAuthorRebuildExemption.Applies(true,
    MakePriorAuthor(cAuthorSpki, true, true), sigs));
end;

initialization
  TDUnitX.RegisterTestFixture(TAuthorRebuildExemptionTests);

end.
