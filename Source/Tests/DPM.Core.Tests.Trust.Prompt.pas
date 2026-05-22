unit DPM.Core.Tests.Trust.Prompt;

// Tests for the ITrustPromptStrategy implementations. Console TTY paths
// can't be reliably driven from a test (stdin is whatever the harness
// provides) so we focus on the deterministic non-interactive default and
// confirm the chain behaviour.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTrustPromptTests = class
  public
    [Test] procedure NonInteractive_AlwaysReturnsBlockOnce;
    [Test] procedure NonInteractive_NeverReturnsTrue;
    [Test] procedure CustomStrategy_HonoursReturnedDecision;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Trust.Prompt;

type
  // Captures the strategy's view of the world for inspection.
  TFakeTrustPromptStrategy = class(TInterfacedObject, ITrustPromptStrategy)
  private
    FDecision : TTrustPromptDecision;
    FAccept   : boolean;
    FCallCount : integer;
    FLastContext : TTrustPromptContext;
  protected
    function PromptAuthorDowngrade(const context : TTrustPromptContext;
                                   out decision : TTrustPromptDecision) : boolean;
  public
    constructor Create(decision : TTrustPromptDecision; accept : boolean);
    property CallCount : integer read FCallCount;
    property LastContext : TTrustPromptContext read FLastContext;
  end;

constructor TFakeTrustPromptStrategy.Create(decision : TTrustPromptDecision; accept : boolean);
begin
  inherited Create;
  FDecision := decision;
  FAccept := accept;
end;

function TFakeTrustPromptStrategy.PromptAuthorDowngrade(
  const context : TTrustPromptContext;
  out decision : TTrustPromptDecision) : boolean;
begin
  Inc(FCallCount);
  FLastContext := context;
  decision := FDecision;
  result := FAccept;
end;

procedure TTrustPromptTests.NonInteractive_AlwaysReturnsBlockOnce;
var
  strategy : ITrustPromptStrategy;
  ctx : TTrustPromptContext;
  decision : TTrustPromptDecision;
begin
  strategy := TNonInteractiveTrustPromptStrategy.Create;
  ctx.PackageId := 'X';
  ctx.Version := '1.0.0';
  ctx.PreviousSpkiHex := 'aa';
  ctx.NewSigned := false;
  ctx.NewSpkiHex := '';
  strategy.PromptAuthorDowngrade(ctx, decision);
  Assert.AreEqual(Ord(tpdBlockOnce), Ord(decision));
end;

procedure TTrustPromptTests.NonInteractive_NeverReturnsTrue;
var
  strategy : ITrustPromptStrategy;
  ctx : TTrustPromptContext;
  decision : TTrustPromptDecision;
begin
  strategy := TNonInteractiveTrustPromptStrategy.Create;
  ctx.PackageId := 'Y';
  ctx.Version := '2.0.0';
  ctx.NewSigned := true;
  ctx.NewSpkiHex := 'bb';
  Assert.IsFalse(strategy.PromptAuthorDowngrade(ctx, decision));
end;

procedure TTrustPromptTests.CustomStrategy_HonoursReturnedDecision;
var
  fake : TFakeTrustPromptStrategy;
  strategy : ITrustPromptStrategy;
  ctx : TTrustPromptContext;
  decision : TTrustPromptDecision;
  ok : boolean;
begin
  fake := TFakeTrustPromptStrategy.Create(tpdTrustOnce, true);
  strategy := fake;
  ctx.PackageId := 'Test.Pkg';
  ctx.Version := '3.0.0';
  ctx.PreviousSpkiHex := 'aa11';
  ctx.NewSigned := true;
  ctx.NewSpkiHex := 'bb22';

  ok := strategy.PromptAuthorDowngrade(ctx, decision);

  Assert.IsTrue(ok);
  Assert.AreEqual(Ord(tpdTrustOnce), Ord(decision));
  Assert.AreEqual(1, fake.CallCount);
  Assert.AreEqual('Test.Pkg', fake.LastContext.PackageId);
  Assert.AreEqual('aa11', fake.LastContext.PreviousSpkiHex);
  Assert.AreEqual('bb22', fake.LastContext.NewSpkiHex);
end;

initialization
  TDUnitX.RegisterTestFixture(TTrustPromptTests);

end.
