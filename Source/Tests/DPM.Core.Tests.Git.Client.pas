unit DPM.Core.Tests.Git.Client;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TGitClientTests = class
  private
    FTempFolder : string;
    function MakeFolder(const name : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    //git runs by full path - found on PATH as DPM started, git.exe only, never from the current folder. A
    //git registry checkout runs git in a freshly cloned repository, which could commit a git.cmd or git.bat.
    [Test]
    procedure FindGitExe_Takes_The_First_Folder_With_GitExe;
    [Test]
    procedure FindGitExe_Ignores_Scripts_Named_Git;
    [Test]
    procedure FindGitExe_Ignores_Relative_Entries;
    [Test]
    procedure FindGitExe_Handles_Quoted_Entries;
    [Test]
    procedure FindGitExe_Empty_Path_Finds_Nothing;
    [Test]
    procedure GitExe_Ignores_Folders_Added_To_PATH_Later;
    [Test]
    procedure Checkout_Does_Not_Run_A_Git_Script_From_The_Repository;
    //the commit can't be put after '--' (checkout takes paths there), so one git would read as an option is refused.
    [Test]
    procedure Checkout_Refuses_A_Commit_Git_Would_Read_As_An_Option;
    //git's output (stdout and stderr) still reaches the parsers and the error messages - needs git installed.
    [Test]
    procedure LsRemoteTags_Reads_The_Tags_Of_A_Local_Repository;
    [Test]
    procedure Failed_Command_Reports_What_Git_Said;

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

    //TGitUtils.NormalizeCommitId - published packages routinely carry a commit id that
    //still has its VCS keyword decoration attached.
    [Test]
    [TestCase('IdPrefix',        'Id:0fe5fa486aae1f4f99b9ea39d29a1915a568d822,0fe5fa486aae1f4f99b9ea39d29a1915a568d822')]
    [TestCase('IdPrefixSpaced',  'Id: 0fe5fa486aae,0fe5fa486aae')]
    [TestCase('LowerCasePrefix', 'id:0fe5fa486aae,0fe5fa486aae')]
    [TestCase('DollarKeyword',   '$Id: 0fe5fa486aae $,0fe5fa486aae')]
    [TestCase('AlreadyClean',    '0fe5fa486aae,0fe5fa486aae')]
    [TestCase('Surrounding',     '  0fe5fa486aae  ,0fe5fa486aae')]
    //'Id' only counts as the keyword when a colon follows it
    [TestCase('NotAKeyword',     'Identity,Identity')]
    //the unreplaced placeholder is meaningful to pack - leave it alone
    [TestCase('HashPlaceholder', '#HASH#,#HASH#')]
    procedure NormalizeCommitId(const input, expected : string);

    [Test]
    procedure NormalizeCommitId_EmptyAndDecorationOnly_ReturnEmpty;
  end;

implementation

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Utils.Process,
  DPM.Core.Git.Interfaces,
  DPM.Core.Git.Client,
  TestLogger;

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

//an empty file is all FindGitExe looks for.
procedure TouchFile(const fileName : string);
begin
  TFile.WriteAllText(fileName, '');
end;

{ TGitClientTests }

procedure TGitClientTests.FixtureSetup;
begin
  FTempFolder := TPath.Combine(TPath.GetTempPath, 'dpm-git-client-' + IntToHex(Random(MaxInt), 8));
  ForceDirectories(FTempFolder);
end;

procedure TGitClientTests.FixtureTearDown;
begin
  if DirectoryExists(FTempFolder) then
    TDirectory.Delete(FTempFolder, true);
end;

function TGitClientTests.MakeFolder(const name : string) : string;
begin
  result := TPath.Combine(FTempFolder, name);
  ForceDirectories(result);
end;

procedure TGitClientTests.FindGitExe_Takes_The_First_Folder_With_GitExe;
var
  noGit : string;
  first : string;
  second : string;
begin
  noGit := MakeFolder('first-nogit');
  first := MakeFolder('first-git');
  second := MakeFolder('first-git2');
  TouchFile(TPath.Combine(first, 'git.exe'));
  TouchFile(TPath.Combine(second, 'git.exe'));
  Assert.AreEqual(TPath.Combine(first, 'git.exe'), TGitClient.FindGitExe(noGit + ';' + first + ';' + second), true);
end;

procedure TGitClientTests.FindGitExe_Ignores_Scripts_Named_Git;
var
  scripts : string;
  realGit : string;
begin
  scripts := MakeFolder('scripts');
  realGit := MakeFolder('scripts-real');
  TouchFile(TPath.Combine(scripts, 'git.cmd'));
  TouchFile(TPath.Combine(scripts, 'git.bat'));
  TouchFile(TPath.Combine(scripts, 'git.com'));
  TouchFile(TPath.Combine(scripts, 'git'));
  TouchFile(TPath.Combine(realGit, 'git.exe'));
  Assert.AreEqual(TPath.Combine(realGit, 'git.exe'), TGitClient.FindGitExe(scripts + ';' + realGit), true);
  Assert.AreEqual('', TGitClient.FindGitExe(scripts), 'only git.exe counts');
end;

procedure TGitClientTests.FindGitExe_Ignores_Relative_Entries;
var
  current : string;
  savedDir : string;
begin
  //git.exe in the current folder, and in a sub folder of it - neither may be found.
  current := MakeFolder('relative');
  TouchFile(TPath.Combine(current, 'git.exe'));
  ForceDirectories(TPath.Combine(current, 'bin'));
  TouchFile(TPath.Combine(current, 'bin\git.exe'));
  savedDir := GetCurrentDir;
  SetCurrentDir(current);
  try
    Assert.AreEqual('', TGitClient.FindGitExe('.;bin;.\bin;..\relative'));
  finally
    SetCurrentDir(savedDir);
  end;
end;

procedure TGitClientTests.FindGitExe_Handles_Quoted_Entries;
var
  spaced : string;
begin
  spaced := MakeFolder('Program Files\Git\cmd');
  TouchFile(TPath.Combine(spaced, 'git.exe'));
  Assert.AreEqual(TPath.Combine(spaced, 'git.exe'), TGitClient.FindGitExe('"' + spaced + '"'), true);
  Assert.AreEqual(TPath.Combine(spaced, 'git.exe'), TGitClient.FindGitExe(IncludeTrailingPathDelimiter(spaced)), true);
end;

procedure TGitClientTests.FindGitExe_Empty_Path_Finds_Nothing;
begin
  Assert.AreEqual('', TGitClient.FindGitExe(''));
  Assert.AreEqual('', TGitClient.FindGitExe(';;'));
end;

procedure TGitClientTests.GitExe_Ignores_Folders_Added_To_PATH_Later;
var
  later : string;
  savedPath : string;
  gitExe : string;
begin
  //what a package's bpl folder or PATH variable does in the IDE - after DPM started.
  later := MakeFolder('later');
  TouchFile(TPath.Combine(later, 'git.exe'));
  savedPath := GetEnvironmentVariable('PATH');
  WinApi.Windows.SetEnvironmentVariable('PATH', PChar(later + ';' + savedPath));
  try
    gitExe := TGitClient.GitExe;
  finally
    WinApi.Windows.SetEnvironmentVariable('PATH', PChar(savedPath));
  end;
  Assert.IsFalse(SameText(TPath.Combine(later, 'git.exe'), gitExe), 'git was looked for on the PATH DPM started with');
  //no git installed is fine - but when there is one it is a full path to a file that exists.
  if gitExe <> '' then
    Assert.IsTrue(TFile.Exists(gitExe) and (ExtractFileDrive(gitExe) <> ''), gitExe);
end;

procedure TGitClientTests.Checkout_Does_Not_Run_A_Git_Script_From_The_Repository;
const
  cNoCurrentDir = 'NoDefaultCurrentDirectoryInExePath';
var
  repoDir : string;
  marker : string;
  client : IGitClient;
  logger : ILogger;
  noCurrentDir : string;
begin
  //cmd.exe looks in the current folder before PATH, so 'cmd /c git ...' run in this folder used to run
  //the script. Whether or not git is installed, the checkout fails here (this isn't a repository) - all
  //that matters is that the script never ran.
  repoDir := MakeFolder('cloned-repo');
  marker := TPath.Combine(FTempFolder, 'script-ran.txt');
  //no BOM - cmd would read one as part of the first command, and the script would fail without anyone
  //noticing. The marker is written next to the repository folder, which is the script's current folder.
  TFile.WriteAllText(TPath.Combine(repoDir, 'git.cmd'), '@echo ran > ..\script-ran.txt' + sLineBreak, TEncoding.ASCII);
  TFile.WriteAllText(TPath.Combine(repoDir, 'git.bat'), '@echo ran > ..\script-ran.txt' + sLineBreak, TEncoding.ASCII);
  logger := TTestLogger.Create;
  client := TGitClient.Create(logger);
  //cmd only skips the current folder when this is set - make sure it isn't, as on most machines.
  noCurrentDir := GetEnvironmentVariable(cNoCurrentDir);
  WinApi.Windows.SetEnvironmentVariable(cNoCurrentDir, nil);
  try
    Assert.IsFalse(client.Checkout(TCancellationTokenSourceFactory.Create.Token, repoDir, 'main'));
  finally
    if noCurrentDir <> '' then
      WinApi.Windows.SetEnvironmentVariable(cNoCurrentDir, PChar(noCurrentDir));
  end;
  Assert.IsFalse(TFile.Exists(marker), 'a git script in the repository was run');
  Assert.IsTrue(client.LastError <> '', 'the failure is still explained');
end;

//runs git directly, to set up a repository - fails the test if git does.
procedure RunGitForSetup(const args : string; const workingDir : string);
var
  exitCode : Cardinal;
begin
  exitCode := TProcess.Execute2(TCancellationTokenSourceFactory.Create.Token, TGitClient.GitExe, args, workingDir);
  Assert.IsTrue(exitCode = 0, 'git ' + args + ' exited with ' + IntToStr(exitCode));
end;

procedure TGitClientTests.LsRemoteTags_Reads_The_Tags_Of_A_Local_Repository;
var
  repoDir : string;
  client : IGitClient;
  tags : IDictionary<string, string>;
begin
  if TGitClient.GitExe = '' then
    Assert.Pass('git is not installed');
  repoDir := MakeFolder('tagged-repo');
  RunGitForSetup('init --quiet', repoDir);
  RunGitForSetup('-c user.email=dpm@example.com -c user.name=dpm commit --quiet --allow-empty -m init', repoDir);
  RunGitForSetup('tag v1.2.3', repoDir);
  client := TGitClient.Create(TTestLogger.Create);
  Assert.IsTrue(client.LsRemoteTags(TCancellationTokenSourceFactory.Create.Token, repoDir, tags), client.LastError);
  Assert.IsTrue(tags.ContainsKey('v1.2.3'), 'the tag is read from what git printed');
  Assert.AreEqual(40, Length(tags['v1.2.3']), tags['v1.2.3']);
end;

procedure TGitClientTests.Failed_Command_Reports_What_Git_Said;
var
  client : IGitClient;
begin
  if TGitClient.GitExe = '' then
    Assert.Pass('git is not installed');
  client := TGitClient.Create(TTestLogger.Create);
  Assert.IsFalse(client.Checkout(TCancellationTokenSourceFactory.Create.Token, MakeFolder('not-a-repo'), 'main'));
  //git says this on stderr.
  Assert.IsTrue(Pos('not a git repository', client.LastError) > 0, client.LastError);
end;

procedure TGitClientTests.Checkout_Refuses_A_Commit_Git_Would_Read_As_An_Option;
var
  client : IGitClient;
begin
  client := TGitClient.Create(TTestLogger.Create);
  Assert.IsFalse(client.Checkout(TCancellationTokenSourceFactory.Create.Token, MakeFolder('option-repo'), '--orphan=x'));
  Assert.IsTrue(Pos('refused', client.LastError) > 0, client.LastError);
end;

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

procedure TGitClientTests.NormalizeCommitId(const input, expected : string);
begin
  //Trim the TestCase values, not the result - leading/trailing spaces in the attribute
  //are eaten by the parser, so 'Surrounding' is exercised via the explicit test below.
  Assert.AreEqual(Trim(expected), TGitUtils.NormalizeCommitId(Trim(input)), false);
end;

procedure TGitClientTests.NormalizeCommitId_EmptyAndDecorationOnly_ReturnEmpty;
begin
  Assert.AreEqual('', TGitUtils.NormalizeCommitId(''));
  Assert.AreEqual('', TGitUtils.NormalizeCommitId('   '));
  //nothing left once the keyword is stripped - better empty than a stray 'Id:'
  Assert.AreEqual('', TGitUtils.NormalizeCommitId('Id:'));
  Assert.AreEqual('', TGitUtils.NormalizeCommitId('$Id: $'));
  //whitespace around a real value is removed
  Assert.AreEqual('0fe5fa486aae', TGitUtils.NormalizeCommitId('  0fe5fa486aae  '), false);
end;

initialization
  TDUnitX.RegisterTestFixture(TGitClientTests);

end.
