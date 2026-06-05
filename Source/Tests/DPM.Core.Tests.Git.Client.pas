unit DPM.Core.Tests.Git.Client;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TGitClientTests = class
  public
    [Test]
    procedure ParseTags_LightweightTag_UsesOwnSha;

    [Test]
    procedure ParseTags_AnnotatedTag_UsesPeeledCommit;

    [Test]
    procedure ParseTags_IgnoresNonTagRefs;

    [Test]
    procedure ParseTags_EmptyInput_ReturnsEmpty;

    [Test]
    procedure ParseRefCommit_ReturnsFirstSha;

    [Test]
    procedure ParseRefCommit_NoTab_ReturnsEmpty;
  end;

implementation

uses
  System.Classes,
  Spring.Collections,
  DPM.Core.Git.Client;

const
  cShaA = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  cShaB = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
  cShaC = 'cccccccccccccccccccccccccccccccccccccccc';
  cShaD = 'dddddddddddddddddddddddddddddddddddddddd';
  cShaE = 'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee';

function MakeLines(const lines : array of string) : TStringList;
var
  i : integer;
begin
  result := TStringList.Create;
  for i := Low(lines) to High(lines) do
    result.Add(lines[i]);
end;

{ TGitClientTests }

procedure TGitClientTests.ParseTags_LightweightTag_UsesOwnSha;
var
  lines : TStringList;
  tags : IDictionary<string, string>;
begin
  lines := MakeLines([cShaA + #9 + 'refs/tags/v1.0.0']);
  try
    tags := TGitClient.ParseTags(lines);
    Assert.AreEqual(1, tags.Count);
    Assert.IsTrue(tags.ContainsKey('v1.0.0'));
    Assert.AreEqual(cShaA, tags['v1.0.0']);
  finally
    lines.Free;
  end;
end;

procedure TGitClientTests.ParseTags_AnnotatedTag_UsesPeeledCommit;
var
  lines : TStringList;
  tags : IDictionary<string, string>;
begin
  //annotated tag - the tag object sha is cShaB, the peeled (^{}) commit is cShaC.
  lines := MakeLines([
    cShaB + #9 + 'refs/tags/v2.0.0',
    cShaC + #9 + 'refs/tags/v2.0.0^{}']);
  try
    tags := TGitClient.ParseTags(lines);
    Assert.AreEqual(1, tags.Count);
    //the commit (peeled) must win, not the tag object sha.
    Assert.AreEqual(cShaC, tags['v2.0.0']);
  finally
    lines.Free;
  end;
end;

procedure TGitClientTests.ParseTags_IgnoresNonTagRefs;
var
  lines : TStringList;
  tags : IDictionary<string, string>;
begin
  lines := MakeLines([
    cShaA + #9 + 'HEAD',
    cShaA + #9 + 'refs/tags/v1.0.0',
    cShaD + #9 + 'refs/tags/v1.5.0',
    cShaE + #9 + 'refs/tags/v1.5.0^{}',
    cShaB + #9 + 'refs/heads/main']);
  try
    tags := TGitClient.ParseTags(lines);
    Assert.AreEqual(2, tags.Count);
    Assert.AreEqual(cShaA, tags['v1.0.0']);
    Assert.AreEqual(cShaE, tags['v1.5.0']);
    Assert.IsFalse(tags.ContainsKey('main'));
  finally
    lines.Free;
  end;
end;

procedure TGitClientTests.ParseTags_EmptyInput_ReturnsEmpty;
var
  lines : TStringList;
  tags : IDictionary<string, string>;
begin
  lines := TStringList.Create;
  try
    tags := TGitClient.ParseTags(lines);
    Assert.AreEqual(0, tags.Count);
  finally
    lines.Free;
  end;
end;

procedure TGitClientTests.ParseRefCommit_ReturnsFirstSha;
var
  lines : TStringList;
begin
  lines := MakeLines([cShaC + #9 + 'HEAD']);
  try
    Assert.AreEqual(cShaC, TGitClient.ParseRefCommit(lines));
  finally
    lines.Free;
  end;
end;

procedure TGitClientTests.ParseRefCommit_NoTab_ReturnsEmpty;
var
  lines : TStringList;
begin
  lines := MakeLines(['no-tab-here']);
  try
    Assert.AreEqual('', TGitClient.ParseRefCommit(lines));
  finally
    lines.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TGitClientTests);

end.
