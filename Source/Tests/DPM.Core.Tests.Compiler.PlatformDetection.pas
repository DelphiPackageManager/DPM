{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Tests.Compiler.PlatformDetection;

interface

uses
  DUnitX.TestFramework;

type
  //What decides whether this machine can build a platform - its compiler and runtime library installed under
  //$(BDS), and the SDK the IDE's SDK Manager registered for it. Checked against a fake install folder and a scratch
  //registry key, and once against the newest Delphi installed here.
  [TestFixture]
  TPlatformDetectionTests = class
  private
    FRoot : string;
    FTestKey : string;
    FSDKKey : string;
    procedure AddFile(const relativePath : string);
    //exeName in exeFolder (bin or bin64), and lib\<libFolder>\release\rtl.dcp when libFolder isn't ''.
    procedure InstallPlatform(const exeFolder : string; const exeName : string; const libFolder : string);
    //valueName (Default_<platform>) in the scratch PlatformSDKs key names sdkName - with a key of its own when registerSDK.
    procedure SetDefaultSDK(const valueName : string; const sdkName : string; const registerSDK : boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Installed_Platform_Can_Be_Built;
    [Test]
    procedure Missing_Compiler_Is_Not_Installed;
    [Test]
    procedure Compiler_Only_In_Bin64_Is_Installed;
    [Test]
    procedure Compiler_Without_Runtime_Library_Is_Not_Installed;
    //Delphi 10.4 leaves lib\<platform>\release folders holding System.dcu and the FireDAC units, but not the rtl
    //package, for Android, Android64 and iOSDevice64 when they aren't installed - a package can't be built there.
    [Test]
    procedure Partial_Lib_Folder_Without_The_Rtl_Package_Is_Not_Installed;
    //Win64x has no compiler of its own - dcc64 builds it - but it has its own runtime library (from 12.1 on).
    [Test]
    procedure Win64x_Is_Built_By_Dcc64_With_Its_Own_Runtime_Library;
    //XE4 to XE7 called the iOS device lib folder iOSDevice.
    [Test]
    procedure iOS32_Accepts_The_Pre_XE8_Lib_Folder;

    [Test]
    procedure SDK_Selected_And_Registered_Is_Configured;
    [Test]
    procedure No_Selected_SDK_Is_Not_Configured;
    [Test]
    procedure Empty_Selected_SDK_Is_Not_Configured;
    [Test]
    procedure Selected_SDK_That_Is_Not_Registered_Is_Not_Configured;
    [Test]
    procedure No_PlatformSDKs_Key_Is_Not_Configured;
    //Windows needs no SDK, and iOS packages are static libraries - nothing is linked against an SDK.
    [Test]
    procedure Only_Linked_Platforms_Need_An_SDK;

    //What the provider found out is remembered - each platform is looked up once.
    [Test]
    procedure Installed_Delphi_Can_Build_Win32_And_Remembers_It;
    //An SDK added in the IDE's SDK Manager (no IDE restart) is seen by the next restore - a missing SDK isn't
    //remembered. A platform that isn't installed is (installing one restarts the IDE), and so is one that can be built.
    [Test]
    procedure SDK_Added_Later_Is_Seen_But_A_Missing_Platform_Is_Remembered;

    //FireMonkey packages can be built where fmx.dcp is - in the platform's lib folder (everywhere but Linux), or on the
    //IDE's library path for the platform, which is where FMX for Linux (a GetIt package) puts it.
    [Test]
    procedure FMX_In_The_Platform_Lib_Folder_Is_Available;
    [Test]
    procedure FMX_On_The_Library_Path_Is_Available;
    [Test]
    procedure No_FMX_For_Linux_Says_To_Install_It;
    //eg the IDE still names the folder of an FMX for Linux version that was replaced - the build would fail there too.
    [Test]
    procedure Library_Path_Naming_A_Folder_Without_FMX_Says_So;
    //FMX for Linux can be installed from GetIt while the IDE is open - the next operation sees it.
    [Test]
    procedure FMX_Installed_Later_Is_Seen;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Win.Registry,
  WinApi.Windows,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.EnvironmentProvider,
  TestLogger;

const
  cCompiler = TCompilerVersion.Delphi13_0;

type
  //The provider over a fake $(BDS), a scratch PlatformSDKs key and the library path given.
  TFakeInstallProvider = class(TCompilerEnvironmentProvider)
  private
    FFakeRoot : string;
    FFakeSDKKey : string;
    FFakeLibraryPath : string;
  protected
    function TryGetRootDir(const compilerVersion : TCompilerVersion; out rootDir : string) : boolean; override;
    function GetPlatformSDKsKey(const compilerVersion : TCompilerVersion) : string; override;
    function GetIDELibraryPath(const compilerVersion : TCompilerVersion; const rootDir : string; const platform : TDPMPlatform) : string; override;
  public
    constructor Create(const logger : ILogger; const root : string; const sdkKey : string; const libraryPath : string = '');
  end;

constructor TFakeInstallProvider.Create(const logger : ILogger; const root : string; const sdkKey : string; const libraryPath : string);
begin
  inherited Create(logger);
  FFakeRoot := root;
  FFakeSDKKey := sdkKey;
  FFakeLibraryPath := libraryPath;
end;

function TFakeInstallProvider.GetIDELibraryPath(const compilerVersion : TCompilerVersion; const rootDir : string; const platform : TDPMPlatform) : string;
begin
  result := FFakeLibraryPath;
end;

function TFakeInstallProvider.TryGetRootDir(const compilerVersion : TCompilerVersion; out rootDir : string) : boolean;
begin
  rootDir := FFakeRoot;
  result := true;
end;

function TFakeInstallProvider.GetPlatformSDKsKey(const compilerVersion : TCompilerVersion) : string;
begin
  result := FFakeSDKKey;
end;

//Deletes key (under HKEY_CURRENT_USER) when nothing is left in it.
procedure DeleteKeyIfEmpty(const key : string);
var
  reg : TRegistry;
  info : TRegKeyInfo;
  empty : boolean;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(key, false) then
      exit;
    empty := reg.GetKeyInfo(info) and (info.NumSubKeys = 0) and (info.NumValues = 0);
    reg.CloseKey;
    if empty then
      reg.DeleteKey(key);
  finally
    reg.Free;
  end;
end;

{ TPlatformDetectionTests }

procedure TPlatformDetectionTests.Setup;
var
  guid : TGUID;
  id : string;
begin
  CreateGUID(guid);
  id := Copy(GUIDToString(guid), 2, 36);
  FRoot := TPath.Combine(TPath.GetTempPath, 'dpm-platform-detection-' + id);
  ForceDirectories(FRoot);
  FTestKey := 'Software\DPM\Tests\' + id;
  FSDKKey := FTestKey + '\PlatformSDKs';
end;

procedure TPlatformDetectionTests.TearDown;
var
  reg : TRegistry;
begin
  if DirectoryExists(FRoot) then
    TDirectory.Delete(FRoot, true);
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(FTestKey) then
      reg.DeleteKey(FTestKey);
  finally
    reg.Free;
  end;
  DeleteKeyIfEmpty('Software\DPM\Tests');
  DeleteKeyIfEmpty('Software\DPM');
end;

procedure TPlatformDetectionTests.AddFile(const relativePath : string);
var
  fileName : string;
begin
  fileName := TPath.Combine(FRoot, relativePath);
  ForceDirectories(ExtractFilePath(fileName));
  TFile.WriteAllText(fileName, 'test');
end;

procedure TPlatformDetectionTests.InstallPlatform(const exeFolder : string; const exeName : string; const libFolder : string);
begin
  AddFile(exeFolder + '\' + exeName);
  if libFolder <> '' then
    AddFile('lib\' + libFolder + '\release\rtl.dcp');
end;

procedure TPlatformDetectionTests.SetDefaultSDK(const valueName : string; const sdkName : string; const registerSDK : boolean);
var
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(FSDKKey, true) then
      Assert.Fail('Unable to create the registry key ' + FSDKKey);
    reg.WriteString(valueName, sdkName);
    reg.CloseKey;
    if registerSDK then
    begin
      if not reg.OpenKey(FSDKKey + '\' + sdkName, true) then
        Assert.Fail('Unable to create the registry key ' + FSDKKey + '\' + sdkName);
      reg.WriteString('PlatformName', 'Test');
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TPlatformDetectionTests.Installed_Platform_Can_Be_Built;
var
  reason : string;
begin
  InstallPlatform('bin', 'dccaarm64.exe', 'Android64');
  Assert.IsTrue(CheckPlatformInstalled(FRoot, cCompiler, TDPMPlatform.Android64, reason), reason);
  Assert.AreEqual('', reason);
end;

procedure TPlatformDetectionTests.Missing_Compiler_Is_Not_Installed;
var
  reason : string;
begin
  //the 32 bit Android compiler, and the 64 bit runtime library - but not the 64 bit compiler.
  InstallPlatform('bin', 'dccaarm.exe', 'Android');
  AddFile('lib\Android64\release\rtl.dcp');
  Assert.IsFalse(CheckPlatformInstalled(FRoot, cCompiler, TDPMPlatform.Android64, reason));
  Assert.IsTrue(Pos('dccaarm64.exe', reason) > 0, 'it names the compiler that is missing : ' + reason);
  Assert.IsTrue(Pos('android64 compiler', reason) > 0, reason);
end;

procedure TPlatformDetectionTests.Compiler_Only_In_Bin64_Is_Installed;
var
  reason : string;
begin
  InstallPlatform('bin64', 'dccarm64ec.exe', 'WinARM64EC');
  Assert.IsTrue(CheckPlatformInstalled(FRoot, cCompiler, TDPMPlatform.WinARM64EC, reason), reason);
end;

procedure TPlatformDetectionTests.Compiler_Without_Runtime_Library_Is_Not_Installed;
var
  reason : string;
begin
  InstallPlatform('bin', 'dcclinux64.exe', '');
  Assert.IsFalse(CheckPlatformInstalled(FRoot, cCompiler, TDPMPlatform.Linux64, reason));
  Assert.IsTrue(Pos('lib\Linux64\release\rtl.dcp', reason) > 0, 'it names the runtime library that is missing : ' + reason);
end;

procedure TPlatformDetectionTests.Partial_Lib_Folder_Without_The_Rtl_Package_Is_Not_Installed;
var
  reason : string;
begin
  InstallPlatform('bin', 'dcciosarm64.exe', '');
  AddFile('lib\iOSDevice64\release\System.dcu');
  AddFile('lib\iOSDevice64\release\FireDAC.dcp');
  Assert.IsFalse(CheckPlatformInstalled(FRoot, TCompilerVersion.Delphi10_4, TDPMPlatform.iOS64, reason),
                 'without rtl.dcp the platform is not installed');
  Assert.IsTrue(Pos('lib\iOSDevice64\release\rtl.dcp', reason) > 0, 'it names the runtime package that is missing : ' + reason);
end;

procedure TPlatformDetectionTests.Win64x_Is_Built_By_Dcc64_With_Its_Own_Runtime_Library;
var
  reason : string;
begin
  InstallPlatform('bin', 'dcc64.exe', 'Win64');
  //12.0 - dcc64, but no Win64x runtime library yet.
  Assert.IsFalse(CheckPlatformInstalled(FRoot, TCompilerVersion.Delphi12_0, TDPMPlatform.Win64x, reason),
                 'Win64x needs its own runtime library');
  AddFile('lib\Win64x\release\rtl.dcp');
  Assert.IsTrue(CheckPlatformInstalled(FRoot, TCompilerVersion.Delphi12_0, TDPMPlatform.Win64x, reason), reason);
end;

procedure TPlatformDetectionTests.iOS32_Accepts_The_Pre_XE8_Lib_Folder;
var
  reason : string;
begin
  InstallPlatform('bin', 'dcciosarm.exe', 'iOSDevice');
  Assert.IsTrue(CheckPlatformInstalled(FRoot, TCompilerVersion.DelphiXE7, TDPMPlatform.iOS32, reason), reason);
end;

procedure TPlatformDetectionTests.SDK_Selected_And_Registered_Is_Configured;
var
  reason : string;
begin
  SetDefaultSDK('Default_Android64', 'AndroidSDK-Test', true);
  Assert.IsTrue(CheckPlatformSDK(HKEY_CURRENT_USER, FSDKKey, cCompiler, TDPMPlatform.Android64, reason), reason);
  Assert.AreEqual('', reason);
end;

procedure TPlatformDetectionTests.No_Selected_SDK_Is_Not_Configured;
var
  reason : string;
begin
  //another platform has one.
  SetDefaultSDK('Default_Linux64', 'ubuntu-test.sdk', true);
  Assert.IsFalse(CheckPlatformSDK(HKEY_CURRENT_USER, FSDKKey, cCompiler, TDPMPlatform.Android64, reason));
  Assert.IsTrue(Pos('no android64 SDK is configured', reason) > 0, reason);
  Assert.IsTrue(Pos('SDK Manager', reason) > 0, 'it says where to add one : ' + reason);
end;

procedure TPlatformDetectionTests.Empty_Selected_SDK_Is_Not_Configured;
var
  reason : string;
begin
  //what EnvOptions.proj shows as <DefaultPlatformSDK/>.
  SetDefaultSDK('Default_OSX64', '', false);
  Assert.IsFalse(CheckPlatformSDK(HKEY_CURRENT_USER, FSDKKey, cCompiler, TDPMPlatform.MacOS64, reason));
  Assert.IsTrue(Pos('no macos64 SDK is configured', reason) > 0, reason);
end;

procedure TPlatformDetectionTests.Selected_SDK_That_Is_Not_Registered_Is_Not_Configured;
var
  reason : string;
begin
  SetDefaultSDK('Default_OSXARM64', 'MacOSX99.0.sdk', false);
  Assert.IsFalse(CheckPlatformSDK(HKEY_CURRENT_USER, FSDKKey, cCompiler, TDPMPlatform.MacOSARM64, reason));
  Assert.IsTrue(Pos('[MacOSX99.0.sdk]', reason) > 0, 'it names the SDK that is missing : ' + reason);
end;

procedure TPlatformDetectionTests.No_PlatformSDKs_Key_Is_Not_Configured;
var
  reason : string;
begin
  Assert.IsFalse(CheckPlatformSDK(HKEY_CURRENT_USER, FSDKKey, cCompiler, TDPMPlatform.Linux64, reason));
  Assert.IsTrue(Pos('no linux64 SDK is configured', reason) > 0, reason);
end;

procedure TPlatformDetectionTests.Only_Linked_Platforms_Need_An_SDK;
var
  platform : TDPMPlatform;
  needSDK : TDPMPlatforms;
begin
  needSDK := [TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64, TDPMPlatform.Android, TDPMPlatform.Android64, TDPMPlatform.Linux64];
  for platform := Succ(TDPMPlatform.UnknownPlatform) to High(TDPMPlatform) do
    Assert.IsTrue((platform in needSDK) = PlatformNeedsSDK(platform), DPMPlatformToString(platform));
end;

//How many times the provider looked platform up - it logs each lookup.
function LookupCount(const logger : TTestLogger; const platform : TDPMPlatform) : integer;
var
  i : integer;
  line : string;
begin
  result := 0;
  for i := 0 to logger.Messages.Count - 1 do
  begin
    line := logger.Messages[i];
    if (Pos('Checking whether', line) > 0) and (Pos('can build for ' + DPMPlatformToString(platform), line) > 0) then
      Inc(result);
  end;
end;

procedure TPlatformDetectionTests.Installed_Delphi_Can_Build_Win32_And_Remembers_It;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  provider : ICompilerEnvironmentProvider;
  version : TCompilerVersion;
  installed : TCompilerVersion;
  platform : TDPMPlatform;
  unsupported : TDPMPlatform;
  firstResult : boolean;
  firstReason : string;
  reason : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  provider := TCompilerEnvironmentProvider.Create(loggerIntf);
  installed := TCompilerVersion.UnknownVersion;
  for version := DelphiMax downto TCompilerVersion.DelphiXE2 do
  begin
    try
      provider.GetRootDir(version);
    except
      //not installed.
      continue;
    end;
    installed := version;
    break;
  end;
  if installed = TCompilerVersion.UnknownVersion then
  begin
    Assert.Pass('No Delphi is installed - nothing to check');
    exit;
  end;

  Assert.IsTrue(provider.CanBuildPlatform(installed, TDPMPlatform.Win32, reason), 'every Delphi builds Win32 : ' + reason);
  Assert.IsTrue(provider.CanBuildPlatform(installed, TDPMPlatform.Win32, reason), reason);
  Assert.AreEqual(1, LookupCount(logger, TDPMPlatform.Win32), 'the second answer is the remembered one : ' + logger.Messages.Text);

  //a platform this version doesn't have - the answer (whatever it is) and its reason are remembered too.
  unsupported := TDPMPlatform.UnknownPlatform;
  for platform := Succ(TDPMPlatform.UnknownPlatform) to High(TDPMPlatform) do
    if not (platform in AllPlatforms(installed)) then
    begin
      unsupported := platform;
      break;
    end;
  if unsupported = TDPMPlatform.UnknownPlatform then
    exit;
  firstResult := provider.CanBuildPlatform(installed, unsupported, firstReason);
  Assert.IsTrue(firstResult = provider.CanBuildPlatform(installed, unsupported, reason), 'the same answer both times');
  Assert.AreEqual(firstReason, reason, false);
  Assert.AreEqual(1, LookupCount(logger, unsupported), 'the second answer is the remembered one : ' + logger.Messages.Text);
end;

procedure TPlatformDetectionTests.SDK_Added_Later_Is_Seen_But_A_Missing_Platform_Is_Remembered;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  provider : ICompilerEnvironmentProvider;
  reason : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  provider := TFakeInstallProvider.Create(loggerIntf, FRoot, FSDKKey);

  //Android64 is installed, but no SDK is configured for it yet.
  InstallPlatform('bin', 'dccaarm64.exe', 'Android64');
  Assert.IsFalse(provider.CanBuildPlatform(cCompiler, TDPMPlatform.Android64, reason));
  Assert.IsTrue(Pos('no android64 SDK is configured', reason) > 0, reason);
  //one is added in the SDK Manager - the next operation can build the platform.
  SetDefaultSDK('Default_Android64', 'AndroidSDK-Test', true);
  Assert.IsTrue(provider.CanBuildPlatform(cCompiler, TDPMPlatform.Android64, reason), 'an SDK added later must be seen : ' + reason);
  Assert.AreEqual(2, LookupCount(logger, TDPMPlatform.Android64), 'the missing SDK was looked for again : ' + logger.Messages.Text);
  //and that is remembered.
  Assert.IsTrue(provider.CanBuildPlatform(cCompiler, TDPMPlatform.Android64, reason), reason);
  Assert.AreEqual(2, LookupCount(logger, TDPMPlatform.Android64), 'can build was remembered : ' + logger.Messages.Text);

  //A platform that isn't installed stays that way for the process, even when its files appear later.
  Assert.IsFalse(provider.CanBuildPlatform(cCompiler, TDPMPlatform.Linux64, reason));
  InstallPlatform('bin', 'dcclinux64.exe', 'Linux64');
  Assert.IsFalse(provider.CanBuildPlatform(cCompiler, TDPMPlatform.Linux64, reason), 'not installed is remembered');
  Assert.IsTrue(Pos('dcclinux64.exe', reason) > 0, 'with its reason : ' + reason);
  Assert.AreEqual(1, LookupCount(logger, TDPMPlatform.Linux64), logger.Messages.Text);
end;

procedure TPlatformDetectionTests.FMX_In_The_Platform_Lib_Folder_Is_Available;
var
  reason : string;
begin
  AddFile('lib\OSX64\release\fmx.dcp');
  Assert.IsTrue(CheckFMXAvailable(FRoot, '', cCompiler, TDPMPlatform.MacOS64, reason), reason);
  Assert.AreEqual('', reason);
end;

procedure TPlatformDetectionTests.FMX_On_The_Library_Path_Is_Available;
var
  reason : string;
  libraryPath : string;
begin
  AddFile('FmxLinux\1.84\Lib\Release\fmx.dcp');
  //the shape FMX for Linux leaves - a macro we can't expand is passed over, and the folder has a trailing \.
  libraryPath := '$(UNKNOWNMACRO)\Lib;' + TPath.Combine(FRoot, 'FmxLinux\1.84\Lib\Release') + '\;' + TPath.Combine(FRoot, 'FmxLinux\1.84\Redist') + '\';
  Assert.IsTrue(CheckFMXAvailable(FRoot, libraryPath, cCompiler, TDPMPlatform.Linux64, reason), reason);
end;

procedure TPlatformDetectionTests.No_FMX_For_Linux_Says_To_Install_It;
var
  reason : string;
begin
  //Linux64 itself is installed - but not FMX for it.
  InstallPlatform('bin', 'dcclinux64.exe', 'Linux64');
  Assert.IsFalse(CheckFMXAvailable(FRoot, TPath.Combine(FRoot, 'lib\Linux64\release'), cCompiler, TDPMPlatform.Linux64, reason));
  Assert.IsTrue(Pos('no fmx.dcp for linux64', reason) > 0, reason);
  Assert.IsTrue(Pos('install FMX for Linux (FMXLinux) from GetIt', reason) > 0, 'it says how to get it : ' + reason);
end;

procedure TPlatformDetectionTests.Library_Path_Naming_A_Folder_Without_FMX_Says_So;
var
  reason : string;
  staleFolder : string;
begin
  AddFile('FmxLinux\1.84\Lib\Release\fmx.dcp');
  staleFolder := TPath.Combine(FRoot, 'FmxLinux\1.83\Lib\Release') + '\';
  Assert.IsFalse(CheckFMXAvailable(FRoot, staleFolder, cCompiler, TDPMPlatform.Linux64, reason),
                 'what the build would look in has no fmx.dcp');
  Assert.IsTrue(Pos('[' + staleFolder + ']', reason) > 0, 'it names the folder : ' + reason);
  Assert.IsTrue(Pos('reinstall FMX for Linux from GetIt', reason) > 0, reason);
end;

//How many times the provider looked for FMX for platform - it logs each look.
function FMXLookupCount(const logger : TTestLogger; const platform : TDPMPlatform) : integer;
var
  i : integer;
  line : string;
begin
  result := 0;
  for i := 0 to logger.Messages.Count - 1 do
  begin
    line := logger.Messages[i];
    if (Pos('Checking whether', line) > 0) and (Pos('has FMX for ' + DPMPlatformToString(platform), line) > 0) then
      Inc(result);
  end;
end;

procedure TPlatformDetectionTests.FMX_Installed_Later_Is_Seen;
var
  logger : TTestLogger;
  loggerIntf : ILogger;
  provider : ICompilerEnvironmentProvider;
  reason : string;
begin
  logger := TTestLogger.Create;
  loggerIntf := logger;
  provider := TFakeInstallProvider.Create(loggerIntf, FRoot, FSDKKey, TPath.Combine(FRoot, 'FmxLinux\1.84\Lib\Release'));
  Assert.IsFalse(provider.CanBuildFMX(cCompiler, TDPMPlatform.Linux64, reason));
  //installed from GetIt.
  AddFile('FmxLinux\1.84\Lib\Release\fmx.dcp');
  Assert.IsTrue(provider.CanBuildFMX(cCompiler, TDPMPlatform.Linux64, reason), 'FMX installed later must be seen : ' + reason);
  Assert.IsTrue(provider.CanBuildFMX(cCompiler, TDPMPlatform.Linux64, reason), reason);
  Assert.AreEqual(2, FMXLookupCount(logger, TDPMPlatform.Linux64), 'missing is looked for again, found is remembered : ' + logger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TPlatformDetectionTests);

end.
