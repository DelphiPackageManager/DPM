unit DPM.Core.Tests.Compiler.MSBuild;

// What DPM runs to compile a package : cmd.exe and MSBuild.exe by their full paths - never a cmd.exe or an
// msbuild.bat sitting in the current folder - and msbuild given, as global properties, everything that decides what
// the CodeGear targets import.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMSBuildCompilerTests = class
  private
    FRoot : string;
    FRsVars : string;
    FMSBuild : string;
    FProject : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure CommandLine_PinsWhatDecidesTheImports;
    [Test]
    procedure CommandLine_RunsEverythingByItsFullPath;
    [Test]
    procedure Build_DoesNotRunACmdOrMSBuildInTheCurrentFolder;
  end;

implementation

uses
  WinApi.Windows,
  WinApi.ActiveX,
  System.SysUtils,
  System.IOUtils,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.MSBuild,
  TestLogger;

type
  //a compiler 'installed' at root, whose rsvars.bat and MSBuild.exe are the files given.
  TFakeEnvironment = class(TInterfacedObject, ICompilerEnvironmentProvider)
  private
    FRoot : string;
    FRsVars : string;
    FMSBuild : string;
  protected
    function GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
    function GetRootDir(const compilerVersion : TCompilerVersion) : string;
    function GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
  public
    constructor Create(const root, rsVars, msBuild : string);
  end;

  //the command line is protected.
  TMSBuildCompilerAccess = class(TMSBuildCompiler);

constructor TFakeEnvironment.Create(const root, rsVars, msBuild : string);
begin
  inherited Create;
  FRoot := root;
  FRsVars := rsVars;
  FMSBuild := msBuild;
end;

function TFakeEnvironment.GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
begin
  result := FRsVars;
end;

function TFakeEnvironment.GetRootDir(const compilerVersion : TCompilerVersion) : string;
begin
  result := FRoot;
end;

function TFakeEnvironment.GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
begin
  result := FMSBuild;
end;

const
  cProject =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10 +
    '    <PropertyGroup Condition="''$(Config)''==''Base'' or ''$(Base)''!=''''">'#13#10 +
    '        <Base>true</Base>'#13#10 +
    '    </PropertyGroup>'#13#10 +
    '    <ItemGroup>'#13#10 +
    '        <BuildConfiguration Include="Base">'#13#10 +
    '            <Key>Base</Key>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '        <BuildConfiguration Include="Release">'#13#10 +
    '            <Key>Cfg_1</Key>'#13#10 +
    '            <CfgParent>Base</CfgParent>'#13#10 +
    '        </BuildConfiguration>'#13#10 +
    '    </ItemGroup>'#13#10 +
    '</Project>'#13#10;

function NewCompiler(const env : ICompilerEnvironmentProvider; const cacheFolder : string) : ICompiler;
var
  logger : ILogger;
begin
  logger := TTestLogger.Create;
  result := TMSBuildCompilerAccess.Create(logger, TCompilerVersion.Delphi12_0, TDPMPlatform.Win32, env, cacheFolder);
end;

procedure WriteBatch(const fileName : string; const text : string);
begin
  //no BOM - cmd would try to run it as part of the first command.
  TFile.WriteAllText(fileName, text, TEncoding.ASCII);
end;

{ TMSBuildCompilerTests }

procedure TMSBuildCompilerTests.Setup;
var
  g : TGUID;
begin
  //the project is read with MSXML.
  CoInitialize(nil);
  CreateGUID(g);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm compiler test ' + LowerCase(Copy(GUIDToString(g), 2, 8)));
  ForceDirectories(TPath.Combine(FRoot, 'Studio\bin'));
  ForceDirectories(TPath.Combine(FRoot, 'work'));
  FRsVars := TPath.Combine(FRoot, 'Studio\bin\rsvars.bat');
  WriteBatch(FRsVars, '@echo off'#13#10);
  //stands in for MSBuild.exe - records that it was what ran.
  FMSBuild := TPath.Combine(FRoot, 'Studio\fake msbuild.bat');
  WriteBatch(FMSBuild, '@echo off'#13#10'echo real>"' + TPath.Combine(FRoot, 'ran-real.txt') + '"'#13#10'exit /b 0'#13#10);
  FProject := TPath.Combine(FRoot, 'work\Pkg.dproj');
  TFile.WriteAllText(FProject, cProject, TEncoding.UTF8);
end;

procedure TMSBuildCompilerTests.TearDown;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
  CoUninitialize;
end;

procedure TMSBuildCompilerTests.CommandLine_PinsWhatDecidesTheImports;
var
  commandLine : string;
begin
  commandLine := (NewCompiler(TFakeEnvironment.Create(TPath.Combine(FRoot, 'Studio'), FRsVars, FMSBuild), 'c:\cache root') as TMSBuildCompilerAccess)
                   .GetCommandLine(TDPMPlatform.Win32, FProject, 'Release', TPackageVersion.Parse('1.0.0'));
  Assert.Contains(commandLine, ' /p:GlobalOptionFile= ');
  Assert.Contains(commandLine, ' /p:BDSAppDataBaseDir=BDS ');
  Assert.Contains(commandLine, ' /p:APPDATA="' + ExcludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + '"');
  Assert.Contains(commandLine, ' /p:BDS="' + TPath.Combine(FRoot, 'Studio') + '"');
  Assert.Contains(commandLine, ' /p:DPMCache="c:\cache root"');
  Assert.Contains(commandLine, ' /p:ImportEnvOptions=false');
end;

procedure TMSBuildCompilerTests.CommandLine_RunsEverythingByItsFullPath;
var
  commandLine : string;
begin
  commandLine := (NewCompiler(TFakeEnvironment.Create(TPath.Combine(FRoot, 'Studio'), FRsVars, FMSBuild), '') as TMSBuildCompilerAccess)
                   .GetCommandLine(TDPMPlatform.Win32, FProject, 'Release', TPackageVersion.Parse('1.0.0'));
  Assert.Contains(commandLine, 'call "' + FRsVars + '" && "' + FMSBuild + '" "' + FProject + '"');
  //every path quoted - the folder has a space in it - and errors go to the log as well.
  Assert.Contains(commandLine, ' 2>&1"');
  Assert.DoesNotContain(commandLine, ' msbuild ', true);
  Assert.IsFalse(commandLine.EndsWith(';'), commandLine);
end;

procedure TMSBuildCompilerTests.Build_DoesNotRunACmdOrMSBuildInTheCurrentFolder;
var
  compiler : ICompiler;
  oldDir : string;
  oldNoCurrentDir : string;
  hadNoCurrentDir : boolean;
  work : string;
  planted : string;
begin
  work := TPath.Combine(FRoot, 'work');
  planted := TPath.Combine(FRoot, 'ran-planted.txt');
  //what a package's folder or a checkout could hold.
  WriteBatch(TPath.Combine(work, 'msbuild.bat'), '@echo off'#13#10'echo planted>"' + planted + '"'#13#10);
  WriteBatch(TPath.Combine(work, 'msbuild.cmd'), '@echo off'#13#10'echo planted>"' + planted + '"'#13#10);
  //a cmd.exe that is not cmd - if it runs, the real msbuild stand in never does.
  TFile.Copy(TPath.Combine(TPath.GetDirectoryName(GetEnvironmentVariable('ComSpec')), 'hostname.exe'), TPath.Combine(work, 'cmd.exe'));
  //the test environment sets this, which would hide the planted files from cmd whatever DPM does.
  hadNoCurrentDir := GetEnvironmentVariable('NoDefaultCurrentDirectoryInExePath') <> '';
  oldNoCurrentDir := GetEnvironmentVariable('NoDefaultCurrentDirectoryInExePath');
  SetEnvironmentVariable('NoDefaultCurrentDirectoryInExePath', nil);
  oldDir := GetCurrentDir;
  SetCurrentDir(work);
  try
    compiler := NewCompiler(TFakeEnvironment.Create(TPath.Combine(FRoot, 'Studio'), FRsVars, FMSBuild), '');
    compiler.BuildProject(TCancellationTokenSourceFactory.Create.Token, TDPMPlatform.Win32, FProject, 'Release', TPackageVersion.Parse('1.0.0'));
  finally
    SetCurrentDir(oldDir);
    if hadNoCurrentDir then
      SetEnvironmentVariable('NoDefaultCurrentDirectoryInExePath', PChar(oldNoCurrentDir));
  end;
  Assert.IsFalse(FileExists(planted), 'an msbuild.bat in the current folder ran');
  Assert.IsTrue(FileExists(TPath.Combine(FRoot, 'ran-real.txt')), 'the compiler''s own MSBuild did not run - a cmd.exe in the current folder may have');
end;

initialization
  TDUnitX.RegisterTestFixture(TMSBuildCompilerTests);

end.
