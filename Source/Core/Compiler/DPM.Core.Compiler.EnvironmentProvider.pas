{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Compiler.EnvironmentProvider;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes,
  WinApi.Windows,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces;

type
  //singleton, inject where needed.
  TCompilerEnvironmentProvider = class(TInterfacedObject, ICompilerEnvironmentProvider)
  private
    type
      TStatus = (unknown, found, notfound);
  private
    FLogger : ILogger;
    FFound : array[TCompilerVersion.UnknownVersion..DelphiMax] of TStatus;
    FRootDirs : array[TCompilerVersion.UnknownVersion..DelphiMax] of string;
    FMSBuildPaths : array[TCompilerVersion.UnknownVersion..DelphiMax] of string;
    //what CanBuildPlatform found - found = it can be built, notfound = the platform isn't installed (FPlatformReasons
    //says why). A missing SDK isn't remembered - see CanBuildPlatform.
    FPlatformStatus : array[TCompilerVersion.UnknownVersion..DelphiMax, TDPMPlatform] of TStatus;
    FPlatformReasons : array[TCompilerVersion.UnknownVersion..DelphiMax, TDPMPlatform] of string;
    //what CanBuildFMX found - only that it can, see CanBuildFMX.
    FFMXFound : array[TCompilerVersion.UnknownVersion..DelphiMax, TDPMPlatform] of boolean;
  protected
    //the IDE's library path for platform, its macros expanded.
    function GetIDELibraryPath(const compilerVersion : TCompilerVersion; const rootDir : string; const platform : TDPMPlatform) : string; virtual;
    function CanBuildFMX(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out reason : string) : boolean;
    function FoundCompilerInfo(const compilerVersion : TCompilerVersion) : Boolean;
    //the compiler's install folder - false when it isn't installed.
    function TryGetRootDir(const compilerVersion : TCompilerVersion; out rootDir : string) : boolean; virtual;
    //the HKEY_CURRENT_USER key the IDE's SDK Manager records the compiler's SDKs under.
    function GetPlatformSDKsKey(const compilerVersion : TCompilerVersion) : string; virtual;
    function GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
    function GetRootDir(const compilerVersion : TCompilerVersion) : string;
    function GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
    function CanBuildPlatform(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out reason : string) : boolean;

  public
    constructor Create(const logger : ILogger);
    destructor Destroy; override;
  end;

//The command line compiler that builds platform - '' for one we don't know. Win64x has no compiler of its own,
//msbuild builds it with dcc64.
function PlatformCompilerExe(const platform : TDPMPlatform) : string;

//Whether building platform needs a platform SDK configured in the IDE (its SDK Manager) - the platforms whose
//packages are linked against the SDK's libraries. Windows needs none, and iOS packages are static libraries, so
//nothing is linked. Where we can't be sure (OSX32, from before the SDK Manager) the build is left to try.
function PlatformNeedsSDK(const platform : TDPMPlatform) : boolean;

//Whether the compiler for platform (in bin or bin64) and its runtime library (lib\<platform>\release\rtl.dcp) are
//installed under rootDir - $(BDS). reason says what is missing.
function CheckPlatformInstalled(const rootDir : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                out reason : string) : boolean;

//Whether a default SDK for platform is configured under platformSDKsKey (the compiler's PlatformSDKs key) - its
//Default_<platform> value names the SDK, and the SDK's own key exists. reason says what is missing.
function CheckPlatformSDK(const rootKey : HKEY; const platformSDKsKey : string; const compilerVersion : TCompilerVersion;
                          const platform : TDPMPlatform; out reason : string) : boolean;

//Whether FireMonkey packages can be built for platform - fmx.dcp is in rootDir's lib folder for it, or in one of the
//folders of libraryPath (the IDE's library path for platform, its macros expanded), which is where FMX for Linux (a
//GetIt package) puts it. reason says why not.
function CheckFMXAvailable(const rootDir : string; const libraryPath : string; const compilerVersion : TCompilerVersion;
                           const platform : TDPMPlatform; out reason : string) : boolean;

//Expands the MSBuild macros of an IDE Library Path (or project search path) value - the IDE's own (BDSLIB, BDS,
//BDSCOMMONDIR, BDSCatalogRepository, ...) and the user's Environment Variables overrides (name -> value), which
//win as they do in the IDE - and then any %VARIABLE%. Exposed so it can be unit tested.
function ExpandBDSTokens(const value, delphiRoot : string; const platform : TDPMPlatform; const bdsVersion : string;
                         const overrides : IDictionary<string, string> = nil) : string;

//The IDE's Environment Variables overrides for bdsVersion (HKCU), name -> value - empty when there are none.
function ReadIDEEnvironmentOverrides(const bdsVersion : string) : IDictionary<string, string>;

//The IDE's Library Path for platform (HKCU ...\Library\<platform>, 'Search Path') - '' when there is none. Its
//macros are not expanded - see ExpandBDSTokens.
function ReadIDELibraryPath(const bdsVersion : string; const platform : TDPMPlatform) : string;

implementation

uses
  System.Win.Registry,
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Diagnostics,
  DPM.Core.Utils.System;

const
  cSDKManagerHint = ' - add one in the IDE''s SDK Manager (Tools > Options > Deployment > SDK Manager)';

function PlatformCompilerExe(const platform : TDPMPlatform) : string;
begin
  case platform of
    TDPMPlatform.Win32        : result := 'dcc32.exe';
    TDPMPlatform.Win64,
    TDPMPlatform.Win64x       : result := 'dcc64.exe';
    TDPMPlatform.WinARM64EC   : result := 'dccarm64ec.exe';
    TDPMPlatform.MacOS32      : result := 'dccosx.exe';
    TDPMPlatform.MacOS64      : result := 'dccosx64.exe';
    TDPMPlatform.MacOSARM64   : result := 'dccosxarm64.exe';
    TDPMPlatform.Android      : result := 'dccaarm.exe';
    TDPMPlatform.Android64    : result := 'dccaarm64.exe';
    TDPMPlatform.iOS32        : result := 'dcciosarm.exe';
    TDPMPlatform.iOS64        : result := 'dcciosarm64.exe';
    TDPMPlatform.iOSSimulator : result := 'dccios32.exe';
    TDPMPlatform.iOSSimARM64  : result := 'dcciossimarm64.exe';
    TDPMPlatform.Linux64      : result := 'dcclinux64.exe';
  else
    result := '';
  end;
end;

function PlatformNeedsSDK(const platform : TDPMPlatform) : boolean;
begin
  result := platform in [TDPMPlatform.MacOS64, TDPMPlatform.MacOSARM64, TDPMPlatform.Android, TDPMPlatform.Android64,
                         TDPMPlatform.Linux64];
end;

//$(BDS)\lib\<folder>\release\rtl.dcp - the folder is the platform's name in the IDE ($(Platform)). The rtl package,
//which every package requires, not System.dcu - Delphi 10.4 leaves partial lib folders (System.dcu and the FireDAC
//units, but no rtl.dcp) for Android, Android64 and iOSDevice64 when those platforms aren't installed, and a package
//build there fails with 'Required package 'rtl' not found'.
function RuntimePackagePath(const rootDir : string; const platformFolder : string) : string;
begin
  result := IncludeTrailingPathDelimiter(rootDir) + 'lib\' + platformFolder + '\release\rtl.dcp';
end;

//$(BDS)\lib\<folder>\release\fmx.dcp - FireMonkey's runtime package, which every FMX package requires.
function FMXPackagePath(const rootDir : string; const platformFolder : string) : string;
begin
  result := IncludeTrailingPathDelimiter(rootDir) + 'lib\' + platformFolder + '\release\fmx.dcp';
end;

function CheckPlatformInstalled(const rootDir : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                out reason : string) : boolean;
var
  root : string;
  exeName : string;
  libFound : boolean;
begin
  result := false;
  reason := '';
  exeName := PlatformCompilerExe(platform);
  //a platform we don't know the compiler of - the build decides.
  if exeName = '' then
    exit(true);
  root := IncludeTrailingPathDelimiter(rootDir);
  //some compilers are only in bin64 (dccarm64ec), the ones the 64 bit IDE uses are in both.
  if not (FileExists(root + 'bin\' + exeName) or FileExists(root + 'bin64\' + exeName)) then
  begin
    reason := 'the ' + DPMPlatformToString(platform) + ' compiler for ' + CompilerToString(compilerVersion) + ' is not installed (no ' +
              exeName + ' in ' + root + 'bin or bin64)';
    exit;
  end;
  libFound := FileExists(RuntimePackagePath(root, DPMPlatformToBDString(platform)));
  //XE4 to XE7 called the iOS device platform (and its lib folder) iOSDevice - iOSDevice32 from XE8 on.
  if (not libFound) and (platform = TDPMPlatform.iOS32) then
    libFound := FileExists(RuntimePackagePath(root, 'iOSDevice'));
  if not libFound then
  begin
    reason := 'the ' + DPMPlatformToString(platform) + ' runtime library for ' + CompilerToString(compilerVersion) + ' is not installed (no ' +
              RuntimePackagePath(root, DPMPlatformToBDString(platform)) + ')';
    exit;
  end;
  result := true;
end;

function CheckPlatformSDK(const rootKey : HKEY; const platformSDKsKey : string; const compilerVersion : TCompilerVersion;
                          const platform : TDPMPlatform; out reason : string) : boolean;
var
  reg : TRegistry;
  valueName : string;
  sdkName : string;
  sdkExists : boolean;
begin
  result := false;
  reason := '';
  //the value the IDE records the platform's selected SDK in - Default_Android64, Default_OSXARM64 etc.
  valueName := 'Default_' + DPMPlatformToBDString(platform);
  sdkName := '';
  sdkExists := false;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := rootKey;
    if reg.OpenKey(platformSDKsKey, false) then
    begin
      if reg.ValueExists(valueName) then
        sdkName := Trim(reg.ReadString(valueName));
      if sdkName <> '' then
        sdkExists := reg.KeyExists(sdkName);
    end;
  finally
    reg.Free;
  end;
  if sdkName = '' then
  begin
    reason := 'no ' + DPMPlatformToString(platform) + ' SDK is configured for ' + CompilerToString(compilerVersion) + cSDKManagerHint;
    exit;
  end;
  if not sdkExists then
  begin
    reason := 'the ' + DPMPlatformToString(platform) + ' SDK [' + sdkName + '] selected for ' + CompilerToString(compilerVersion) +
              ' is not registered' + cSDKManagerHint;
    exit;
  end;
  result := true;
end;

function CheckFMXAvailable(const rootDir : string; const libraryPath : string; const compilerVersion : TCompilerVersion;
                           const platform : TDPMPlatform; out reason : string) : boolean;
var
  folders : TStringList;
  folder : string;
  staleFolder : string;
  i : integer;
begin
  result := false;
  reason := '';
  //Shipped with the platform everywhere but Linux.
  if FileExists(FMXPackagePath(rootDir, DPMPlatformToBDString(platform))) then
    exit(true);
  //XE4 to XE7 called the iOS device lib folder iOSDevice.
  if (platform = TDPMPlatform.iOS32) and FileExists(FMXPackagePath(rootDir, 'iOSDevice')) then
    exit(true);
  //Added by a package - FMX for Linux adds its folder to the Linux64 library path, which the build uses too.
  staleFolder := '';
  folders := TStringList.Create;
  try
    folders.StrictDelimiter := true;
    folders.Delimiter := ';';
    folders.DelimitedText := libraryPath;
    for i := 0 to folders.Count - 1 do
    begin
      folder := Trim(folders[i]);
      //a macro we couldn't expand - not a folder we can look in.
      if (folder = '') or (Pos('$(', folder) > 0) then
        continue;
      if FileExists(IncludeTrailingPathDelimiter(folder) + 'fmx.dcp') then
        exit(true);
      //eg the IDE still names the folder of an FMX for Linux version that has since been replaced.
      if (staleFolder = '') and ContainsText(folder, 'fmx') then
        staleFolder := folder;
    end;
  finally
    folders.Free;
  end;
  reason := 'there is no fmx.dcp for ' + DPMPlatformToString(platform) + ' in ' +
            ExcludeTrailingPathDelimiter(ExtractFilePath(FMXPackagePath(rootDir, DPMPlatformToBDString(platform)))) +
            ' or on the IDE''s ' + DPMPlatformToString(platform) + ' library path for ' + CompilerToString(compilerVersion);
  if staleFolder <> '' then
  begin
    reason := reason + ' - the library path names [' + staleFolder + '], which has no fmx.dcp';
    if platform = TDPMPlatform.Linux64 then
      reason := reason + ' (reinstall FMX for Linux from GetIt, or correct the IDE''s Linux64 library path)';
  end
  else if platform = TDPMPlatform.Linux64 then
    reason := reason + ' - install FMX for Linux (FMXLinux) from GetIt';
end;

//Expand the BDS-family MSBuild macros that show up in IDE Library Path values
//('$(BDSLIB)\$(PLATFORM)\release' is the canonical Win32/Win64 entry).
//Without this expansion the registry-derived paths point at literal "$(BDSLIB)\..."
//directories that don't exist.
function ExpandBDSTokens(const value, delphiRoot : string; const platform : TDPMPlatform; const bdsVersion : string;
                         const overrides : IDictionary<string, string>) : string;

  //as long as the value is - a fixed 1024 char buffer left a long library path unexpanded.
  function ExpandEnv(const s : string) : string;
  begin
    if Pos('%', s) = 0 then
      exit(s);
    result := TSystemUtils.ExpandEnvironmentStrings(s);
    if result = '' then
      result := s;
  end;

var
  s : string;
  publicDocs : string;
  userDocs : string;
  pair : TPair<string, string>;
begin
  s := value;
  //the user's overrides first - an override of one of the IDE's own (BDSCOMMONDIR, say) is the value the IDE uses.
  if overrides <> nil then
  begin
    for pair in overrides do
      s := StringReplace(s, '$(' + pair.Key + ')', pair.Value, [rfReplaceAll, rfIgnoreCase]);
  end;
  if delphiRoot <> '' then
  begin
    s := StringReplace(s, '$(BDSLIB)', IncludeTrailingPathDelimiter(delphiRoot) + 'lib', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSBIN)', IncludeTrailingPathDelimiter(delphiRoot) + 'bin', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSINCLUDE)', IncludeTrailingPathDelimiter(delphiRoot) + 'include', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDS)', delphiRoot, [rfReplaceAll, rfIgnoreCase]);
  end;
  if bdsVersion <> '' then
  begin
    publicDocs := ExpandEnv('%PUBLIC%\Documents\Embarcadero\Studio\' + bdsVersion);
    userDocs := ExpandEnv('%USERPROFILE%\Documents\Embarcadero\Studio\' + bdsVersion);
    s := StringReplace(s, '$(BDSCOMMONDIR)', publicDocs, [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSUSERDIR)', userDocs, [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSPROJECTSDIR)', IncludeTrailingPathDelimiter(userDocs) + 'Projects', [rfReplaceAll, rfIgnoreCase]);
    //where GetIt puts what it installs - the library paths of those packages are under it.
    s := StringReplace(s, '$(BDSCatalogRepositoryAllUsers)', IncludeTrailingPathDelimiter(publicDocs) + 'CatalogRepository', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSCatalogRepository)', IncludeTrailingPathDelimiter(userDocs) + 'CatalogRepository', [rfReplaceAll, rfIgnoreCase]);
  end;
  //$(PLATFORM) - uppercase variant the IDE uses in Library Path entries (the project uses $(Platform))
  s := StringReplace(s, '$(PLATFORM)', DPMPlatformToBDString(platform), [rfReplaceAll, rfIgnoreCase]);
  //Generic environment-variable expansion catches anything else (e.g. %ProgramFiles%)
  result := ExpandEnv(s);
end;

function ReadIDEEnvironmentOverrides(const bdsVersion : string) : IDictionary<string, string>;
var
  reg : TRegistry;
  names : TStringList;
  i : integer;
begin
  result := TCollections.CreateDictionary<string, string>;
  if bdsVersion = '' then
    exit;
  reg := TRegistry.Create(KEY_READ);
  names := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKeyReadOnly('Software\Embarcadero\BDS\' + bdsVersion + '\Environment Variables') then
      exit;
    reg.GetValueNames(names);
    for i := 0 to names.Count - 1 do
    begin
      try
        result[names[i]] := reg.ReadString(names[i]);
      except
        //not a string - the IDE only writes strings here.
      end;
    end;
  finally
    names.Free;
    reg.Free;
  end;
end;

function ReadIDELibraryPath(const bdsVersion : string; const platform : TDPMPlatform) : string;

  function ReadSearchPath(const platformKey : string; out value : string) : boolean;
  var
    reg : TRegistry;
  begin
    result := false;
    value := '';
    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_CURRENT_USER;
      //the value name is literally "Search Path" (with a space). Semicolon-separated.
      if reg.OpenKeyReadOnly('Software\Embarcadero\BDS\' + bdsVersion + '\Library\' + platformKey) and reg.ValueExists('Search Path') then
      begin
        try
          value := reg.ReadString('Search Path');
          result := true;
        except
          value := '';
        end;
      end;
    finally
      reg.Free;
    end;
  end;

begin
  result := '';
  if bdsVersion = '' then
    exit;
  if ReadSearchPath(DPMPlatformToBDString(platform), result) then
    exit;
  //the IDE's own names for these keys aren't always the platform's - Android32 once there was an Android64, and
  //iOSDevice before XE8.
  if platform = TDPMPlatform.Android then
    ReadSearchPath('Android32', result)
  else if platform = TDPMPlatform.iOS32 then
    ReadSearchPath('iOSDevice', result);
end;

function NotInstalledMessage(const compilerVersion : TCompilerVersion) : string;
begin
  result := 'Delphi ' + CompilerToString(compilerVersion) + ' is not installed - unable to find its install location ' +
            '(RootDir under HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE \Software\Embarcadero\BDS\' + CompilerToBDSVersion(compilerVersion) + ').';
end;

//RootDir under the compiler's key - '' when there is none.
function ReadRootDir(const rootKey : HKEY; const access : LongWord; const key : string) : string;
var
  reg : TRegistry;
begin
  result := '';
  reg := TRegistry.Create(access);
  try
    reg.RootKey := rootKey;
    if reg.OpenKey(key, false) and reg.ValueExists('RootDir') then
      result := Trim(reg.ReadString('RootDir'));
  finally
    reg.Free;
  end;
end;


{ TCompilerEnvironmentProvider }
constructor TCompilerEnvironmentProvider.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

destructor TCompilerEnvironmentProvider.Destroy;
begin

  inherited;
end;

function TCompilerEnvironmentProvider.FoundCompilerInfo(const compilerVersion : TCompilerVersion) : Boolean;
begin
  result := FFound[compilerVersion] = TStatus.found;
end;

function TCompilerEnvironmentProvider.TryGetRootDir(const compilerVersion : TCompilerVersion; out rootDir : string) : boolean;
var
  key : string;
begin
  rootDir := '';
  case FFound[compilerVersion] of
    TStatus.found :
      begin
        rootDir := FRootDirs[compilerVersion];
        exit(true);
      end;
    TStatus.notfound :
      exit(false);
  end;
  key := Format('Software\Embarcadero\BDS\%s', [CompilerToBDSVersion(compilerVersion)]);
  FLogger.Debug('Reading RootDir from ' + key);
  //the user's own, where the IDE records it when it first runs - else the machine's, where the installer does (the
  //32 bit view - Delphi's installer is a 32 bit program), as on a build machine where no one has opened the IDE.
  rootDir := ReadRootDir(HKEY_CURRENT_USER, KEY_READ, key);
  if rootDir = '' then
    rootDir := ReadRootDir(HKEY_LOCAL_MACHINE, KEY_READ or KEY_WOW64_32KEY, key);
  FLogger.Debug('RootDir : ' + rootDir);
  if rootDir = '' then
  begin
    FFound[compilerVersion] := TStatus.notfound;
    exit(false);
  end;
  rootDir := ExcludeTrailingPathDelimiter(rootDir);
  FRootDirs[compilerVersion] := rootDir;
  FFound[compilerVersion] := TStatus.found;
  result := true;
end;

function TCompilerEnvironmentProvider.GetRootDir(const compilerVersion : TCompilerVersion) : string;
begin
  if not TryGetRootDir(compilerVersion, result) then
    raise EDPMError.Create(TErrorCode.E5001_CompilerNotInstalled, NotInstalledMessage(compilerVersion));
end;

function TCompilerEnvironmentProvider.GetPlatformSDKsKey(const compilerVersion : TCompilerVersion) : string;
begin
  result := Format('Software\Embarcadero\BDS\%s\PlatformSDKs', [CompilerToBDSVersion(compilerVersion)]);
end;

function TCompilerEnvironmentProvider.CanBuildPlatform(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out reason : string) : boolean;
var
  rootDir : string;
  sdkMissing : boolean;
begin
  result := true;
  reason := '';
  if (compilerVersion = TCompilerVersion.UnknownVersion) or (platform = TDPMPlatform.UnknownPlatform) then
    exit;
  case FPlatformStatus[compilerVersion, platform] of
    TStatus.found :
      exit;
    TStatus.notfound :
      begin
        reason := FPlatformReasons[compilerVersion, platform];
        exit(false);
      end;
  end;
  //The compiler version itself not being installed is the build's to report (E5001) - it is not a platform to skip.
  //Not recorded here - GetRootDir remembers it.
  if not TryGetRootDir(compilerVersion, rootDir) then
    exit;
  FLogger.Debug('Checking whether ' + CompilerToString(compilerVersion) + ' can build for ' + DPMPlatformToString(platform));
  result := CheckPlatformInstalled(rootDir, compilerVersion, platform, reason);
  sdkMissing := false;
  if result and PlatformNeedsSDK(platform) then
  begin
    //the IDE's SDK Manager records the SDKs per user.
    result := CheckPlatformSDK(HKEY_CURRENT_USER, GetPlatformSDKsKey(compilerVersion), compilerVersion, platform, reason);
    sdkMissing := not result;
  end;
  if result then
  begin
    FPlatformStatus[compilerVersion, platform] := TStatus.found;
    FLogger.Debug(CompilerToString(compilerVersion) + ' can build for ' + DPMPlatformToString(platform));
  end
  else if sdkMissing then
    //Not remembered - an SDK can be added in the IDE's SDK Manager without restarting the IDE, and the next restore
    //should then build the platform. Looking again is a few file and registry reads.
    FLogger.Debug(CompilerToString(compilerVersion) + ' can''t build for ' + DPMPlatformToString(platform) + ' yet : ' + reason)
  else
  begin
    //Remembered - installing a platform restarts the IDE.
    FPlatformStatus[compilerVersion, platform] := TStatus.notfound;
    FPlatformReasons[compilerVersion, platform] := reason;
    FLogger.Debug(CompilerToString(compilerVersion) + ' can''t build for ' + DPMPlatformToString(platform) + ' : ' + reason);
  end;
end;

function TCompilerEnvironmentProvider.GetIDELibraryPath(const compilerVersion : TCompilerVersion; const rootDir : string; const platform : TDPMPlatform) : string;
var
  bdsVersion : string;
begin
  bdsVersion := CompilerToBDSVersion(compilerVersion);
  result := ExpandBDSTokens(ReadIDELibraryPath(bdsVersion, platform), rootDir, platform, bdsVersion, ReadIDEEnvironmentOverrides(bdsVersion));
end;

function TCompilerEnvironmentProvider.CanBuildFMX(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out reason : string) : boolean;
var
  rootDir : string;
begin
  result := true;
  reason := '';
  if (compilerVersion = TCompilerVersion.UnknownVersion) or (platform = TDPMPlatform.UnknownPlatform) then
    exit;
  if FFMXFound[compilerVersion, platform] then
    exit;
  //the compiler version not being installed is the build's to report (E5001).
  if not TryGetRootDir(compilerVersion, rootDir) then
    exit;
  FLogger.Debug('Checking whether ' + CompilerToString(compilerVersion) + ' has FMX for ' + DPMPlatformToString(platform));
  result := CheckFMXAvailable(rootDir, GetIDELibraryPath(compilerVersion, rootDir, platform), compilerVersion, platform, reason);
  if result then
    FFMXFound[compilerVersion, platform] := true
  else
    //Not remembered - FMX for Linux is a GetIt package, installed while the IDE is open, and the next restore should
    //then build the FMX packages.
    FLogger.Debug(CompilerToString(compilerVersion) + ' has no FMX for ' + DPMPlatformToString(platform) + ' yet : ' + reason);
end;

function TCompilerEnvironmentProvider.GetRsVarsFilePath(const platform : TDPMPlatform;  const compilerVersion : TCompilerVersion) : string;
begin
  if TSystemUtils.Is64BitIDE then
    result := GetRootDir(compilerVersion) + '\bin64\rsvars64.bat'
  else
    result := GetRootDir(compilerVersion) + '\bin\rsvars.bat';
end;

function TCompilerEnvironmentProvider.GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
var
  rsVarsFile : string;
  lines : TStringList;
  line : string;
  frameworkDir : string;
  i : integer;
begin
  result := FMSBuildPaths[compilerVersion];
  if result <> '' then
    exit;
  rsVarsFile := GetRsVarsFilePath(platform, compilerVersion);
  //@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319 - the folder, in every release from XE2 on.
  frameworkDir := '';
  lines := TStringList.Create;
  try
    try
      lines.LoadFromFile(rsVarsFile);
    except
      on e : Exception do
        raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to read [' + rsVarsFile + '] : ' + e.Message);
    end;
    for i := 0 to lines.Count - 1 do
    begin
      line := Trim(lines[i]);
      if (line <> '') and (line[1] = '@') then
        Delete(line, 1, 1);
      if StartsText('SET FrameworkDir=', line) then
        frameworkDir := Trim(Copy(line, Length('SET FrameworkDir=') + 1, MaxInt));
    end;
  finally
    lines.Free;
  end;
  if frameworkDir = '' then
    raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find MSBuild - [' + rsVarsFile + '] does not set FrameworkDir.');
  result := IncludeTrailingPathDelimiter(TSystemUtils.ExpandEnvironmentStrings(frameworkDir)) + 'MSBuild.exe';
  if not FileExists(result) then
    raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find MSBuild - [' + result + '], named by [' + rsVarsFile + '], does not exist.');
  FMSBuildPaths[compilerVersion] := result;
end;

end.


