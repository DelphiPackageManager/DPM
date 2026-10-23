unit DPM.Core.Tests.Project.CopyLocalTargets;

// The DPM.CopyLocal.targets file every DPM managed project imports from the machine's package cache. It
// records where dpm.exe is on this machine - projects don't, so they don't differ between machines - and
// only ever runs dpm by a full path: msbuild's Exec task runs its command through cmd.exe in the project's
// folder, so a bare 'dpm' would run a dpm.bat or dpm.cmd sitting next to the project (or anywhere on PATH).

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TCopyLocalTargetsTests = class
  private
    FTempFolder : string;
    function MakeFolder(const name : string) : string;
    function MakeProject(const name : string; out cacheFolder : string) : string;
  public
    [SetupFixture]
    procedure FixtureSetup;
    [TearDownFixture]
    procedure FixtureTearDown;

    [Test]
    procedure Never_Runs_A_Bare_Dpm;
    [Test]
    procedure Records_The_Dpm_Exe_Path;
    [Test]
    procedure Escapes_The_Recorded_Path;
    //a missing dpm.exe must not break the build - copy-local is skipped with a warning.
    [Test]
    procedure Warns_And_Skips_When_No_Dpm_Exe_Is_Found;
    [Test]
    procedure Rewrites_The_File_When_Dpm_Moves;
    [Test]
    procedure Keeps_The_Recorded_Path_When_Dpm_Is_Not_Known;

    //the targets file run by msbuild - passes as 'not checked' where the .NET Framework msbuild isn't installed.
    [Test]
    procedure MSBuild_Runs_The_Recorded_Dpm_When_DPMExe_Is_Wrong;
    [Test]
    procedure MSBuild_Warns_And_Builds_When_No_Dpm_Is_Found;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.IOUtils,
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Utils.Process,
  DPM.Core.Project.CopyLocalTargets,
  TestLogger;

const
  CRLF = #13#10;

  //the least msbuild project that builds and imports the targets the way a DPM managed dproj does.
  cStubProject =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003" DefaultTargets="Build">' + CRLF +
    '  <PropertyGroup>' + CRLF +
    '    <DPMCache>%s</DPMCache>' + CRLF +
    '    <BuildDependsOn>Compile</BuildDependsOn>' + CRLF +
    '  </PropertyGroup>' + CRLF +
    '  <ItemGroup>' + CRLF +
    '    <OutputFile Include="$(MSBuildProjectDirectory)\out\App.exe" />' + CRLF +
    '  </ItemGroup>' + CRLF +
    '  <Target Name="Compile" />' + CRLF +
    '  <Target Name="Build" DependsOnTargets="$(BuildDependsOn)" />' + CRLF +
    '  <Import Project="' + cCopyLocalImportProject + '" Condition="Exists(''' + cCopyLocalImportProject + ''')" />' + CRLF +
    '</Project>' + CRLF;

function MSBuildExe : string;
begin
  result := TPath.Combine(GetEnvironmentVariable('WINDIR'), 'Microsoft.NET\Framework\v4.0.30319\MSBuild.exe');
end;

//no BOM - cmd would read one as part of the first command.
procedure WriteScript(const fileName : string; const line : string);
begin
  TFile.WriteAllText(fileName, line + CRLF, TEncoding.ASCII);
end;

function RunMSBuild(const projectFile : string; const extraArgs : string; out output : string) : Cardinal;
var
  env : IEnvironmentBlock;
  logFile : string;
begin
  env := TEnvironmentBlockFactory.Create(nil, true);
  //cmd only skips the current folder when this is set - make sure it isn't, as in an IDE.
  env.RemoveVariable('NoDefaultCurrentDirectoryInExePath');
  logFile := TPath.GetTempFileName;
  try
    result := TProcess.Execute2(TCancellationTokenSourceFactory.Create.Token, MSBuildExe,
                                '"' + projectFile + '" /nologo /nr:false /v:minimal ' + extraArgs,
                                ExtractFileDir(projectFile), env, 120000, logFile);
    output := TFile.ReadAllText(logFile);
  finally
    TFile.Delete(logFile);
  end;
end;

procedure EnsureTargets(const cacheFolder : string; const dpmExePath : string);
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  EnsureCopyLocalTargets(cacheFolder, dpmExePath, logger);
end;

{ TCopyLocalTargetsTests }

procedure TCopyLocalTargetsTests.FixtureSetup;
begin
  FTempFolder := TPath.Combine(TPath.GetTempPath, 'dpm-copylocal-' + IntToHex(Random(MaxInt), 8));
  ForceDirectories(FTempFolder);
end;

procedure TCopyLocalTargetsTests.FixtureTearDown;
begin
  if DirectoryExists(FTempFolder) then
    TDirectory.Delete(FTempFolder, true);
end;

function TCopyLocalTargetsTests.MakeFolder(const name : string) : string;
begin
  result := TPath.Combine(FTempFolder, name);
  ForceDirectories(result);
end;

//a project, with a hostile dpm.cmd and dpm.bat next to it that leave hostile-ran.txt if they run.
function TCopyLocalTargetsTests.MakeProject(const name : string; out cacheFolder : string) : string;
var
  projectFolder : string;
begin
  cacheFolder := MakeFolder(name + '\cache');
  projectFolder := MakeFolder(name + '\project');
  result := TPath.Combine(projectFolder, 'App.dproj');
  TFile.WriteAllText(result, Format(cStubProject, [cacheFolder]), TEncoding.UTF8);
  WriteScript(TPath.Combine(projectFolder, 'dpm.cmd'), '@echo hostile > "%~dp0hostile-ran.txt"');
  WriteScript(TPath.Combine(projectFolder, 'dpm.bat'), '@echo hostile > "%~dp0hostile-ran.txt"');
end;

procedure TCopyLocalTargetsTests.Never_Runs_A_Bare_Dpm;
var
  content : string;
begin
  content := GetCopyLocalTargetsContent('');
  Assert.IsFalse(ContainsText(content, '>dpm<'), content);
  Assert.IsFalse(ContainsText(content, '>dpm.exe<'), content);
  //run only when one of the full paths exists.
  Assert.IsTrue(ContainsText(content, '<Exec Condition="''$(_DPMExe)'' != ''''" ContinueOnError="true" Command="&quot;$(_DPMExe)&quot; copylocal '), content);
  Assert.IsTrue(ContainsText(content, '<_DPMExe Condition="''$(DPMExe)'' != '''' and Exists(''$(DPMExe)'')">$(DPMExe)</_DPMExe>'), content);
end;

procedure TCopyLocalTargetsTests.Records_The_Dpm_Exe_Path;
var
  content : string;
begin
  content := GetCopyLocalTargetsContent('c:\tools\dpm.exe');
  Assert.IsTrue(ContainsText(content, '<_DPMRecordedExe>c:\tools\dpm.exe</_DPMRecordedExe>'), content);
end;

procedure TCopyLocalTargetsTests.Escapes_The_Recorded_Path;
var
  content : string;
begin
  //an apostrophe, an ampersand, a $( and a ; are all fine in a folder name.
  content := GetCopyLocalTargetsContent('c:\Bob''s & $(x);y\dpm.exe');
  Assert.IsTrue(ContainsText(content, '<_DPMRecordedExe>c:\Bob%27s &amp; %24(x)%3By\dpm.exe</_DPMRecordedExe>'), content);
end;

procedure TCopyLocalTargetsTests.Warns_And_Skips_When_No_Dpm_Exe_Is_Found;
var
  content : string;
begin
  content := GetCopyLocalTargetsContent('c:\tools\dpm.exe');
  Assert.IsTrue(ContainsText(content, '<Warning Condition="''$(_DPMExe)'' == ''''"'), content);
  Assert.IsFalse(ContainsText(content, '<Error'), 'a missing dpm.exe must not fail the build : ' + content);
end;

procedure TCopyLocalTargetsTests.Rewrites_The_File_When_Dpm_Moves;
var
  cacheFolder : string;
  targetsFile : string;
begin
  cacheFolder := MakeFolder('moves');
  targetsFile := TPath.Combine(cacheFolder, cCopyLocalTargetsFileName);
  EnsureTargets(cacheFolder, 'c:\old\dpm.exe');
  EnsureTargets(cacheFolder, 'c:\new\dpm.exe');
  Assert.AreEqual(GetCopyLocalTargetsContent('c:\new\dpm.exe'), TFile.ReadAllText(targetsFile), false);
end;

procedure TCopyLocalTargetsTests.Keeps_The_Recorded_Path_When_Dpm_Is_Not_Known;
var
  cacheFolder : string;
  targetsFile : string;
begin
  cacheFolder := MakeFolder('unknown');
  targetsFile := TPath.Combine(cacheFolder, cCopyLocalTargetsFileName);
  //the IDE plugin with no dpm.exe beside it restores after the command line did.
  EnsureTargets(cacheFolder, 'c:\tools\dpm.exe');
  EnsureTargets(cacheFolder, '');
  Assert.AreEqual(GetCopyLocalTargetsContent('c:\tools\dpm.exe'), TFile.ReadAllText(targetsFile), false);

  //but a file an older version wrote is replaced.
  TFile.WriteAllText(targetsFile, '<!-- DPM.CopyLocal.targets v2 - generated by DPM, do not edit. -->', TEncoding.UTF8);
  EnsureTargets(cacheFolder, '');
  Assert.AreEqual(GetCopyLocalTargetsContent(''), TFile.ReadAllText(targetsFile), false);
end;

procedure TCopyLocalTargetsTests.MSBuild_Runs_The_Recorded_Dpm_When_DPMExe_Is_Wrong;
var
  projectFile : string;
  cacheFolder : string;
  toolsFolder : string;
  output : string;
  exitCode : Cardinal;
begin
  if not FileExists(MSBuildExe) then
    Assert.Pass('msbuild was not found at ' + MSBuildExe + ' - not checked');
  projectFile := MakeProject('wrong', cacheFolder);
  //what the restore on this machine recorded - in a folder msbuild would mangle unescaped.
  toolsFolder := MakeFolder('wrong\Bob''s & $(x) tools');
  WriteScript(TPath.Combine(toolsFolder, 'dpm.cmd'), '@echo real > "%~dp0real-ran.txt"');
  EnsureTargets(cacheFolder, TPath.Combine(toolsFolder, 'dpm.cmd'));

  //the dpm.exe of the machine the project was last restored on.
  exitCode := RunMSBuild(projectFile, '"/p:DPMExe=c:\somebody\else\dpm.exe"', output);

  Assert.IsTrue(exitCode = 0, 'the build failed : ' + output);
  Assert.IsTrue(TFile.Exists(TPath.Combine(toolsFolder, 'real-ran.txt')), 'the recorded dpm.exe did not run : ' + output);
  Assert.IsFalse(TFile.Exists(TPath.Combine(ExtractFileDir(projectFile), 'hostile-ran.txt')), 'a dpm script next to the project ran : ' + output);
end;

procedure TCopyLocalTargetsTests.MSBuild_Warns_And_Builds_When_No_Dpm_Is_Found;
var
  projectFile : string;
  cacheFolder : string;
  output : string;
  exitCode : Cardinal;
begin
  if not FileExists(MSBuildExe) then
    Assert.Pass('msbuild was not found at ' + MSBuildExe + ' - not checked');
  projectFile := MakeProject('missing', cacheFolder);
  //recorded, then uninstalled.
  EnsureTargets(cacheFolder, TPath.Combine(FTempFolder, 'missing\gone\dpm.exe'));

  exitCode := RunMSBuild(projectFile, '', output);

  Assert.IsTrue(exitCode = 0, 'a missing dpm.exe must not fail the build : ' + output);
  Assert.IsTrue(ContainsText(output, 'DPM copy-local was skipped'), 'it says so : ' + output);
  Assert.IsFalse(TFile.Exists(TPath.Combine(ExtractFileDir(projectFile), 'hostile-ran.txt')), 'a dpm script next to the project ran : ' + output);
end;

initialization
  TDUnitX.RegisterTestFixture(TCopyLocalTargetsTests);

end.
