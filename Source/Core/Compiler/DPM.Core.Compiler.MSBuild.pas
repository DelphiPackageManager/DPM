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

unit DPM.Core.Compiler.MSBuild;

interface

uses
  System.Classes,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces;

type
  //Do not create directly, inject a compilerfactory.
  TMSBuildCompiler = class(TInterfacedObject, ICompiler)
  private
    FLogger : ILogger;
    FEnv : ICompilerEnvironmentProvider;
    FCompilerLogFile : string;

    FBPLOutput : string;
    FLibOutput : string;

    FCompilerVersion : TCompilerVersion;
    FConfiguration : string;
    FProjectFile : string;
    FPlatform : TDPMPlatform;
    FSearchPaths : IList<string>;
    FVerbosity : TCompilerVerbosity;

    FCompilerOutput : TStringList;

    FBuildForDesign : boolean;
    //the package cache root - what a package's dproj sees as $(DPMCache).
    FPackageCacheFolder : string;
  protected
    function GetBPLOutput : string;
    function GetCompilerVersion : TCompilerVersion;
    function GetConfiguration : string;

    function GetLibOutput : string;
    function GetPlatform : TDPMPlatform;
    function GetSearchPaths : IList<string>;

    procedure SetConfiguration(const value : string);

    procedure SetLibOutput(const value : string);
    procedure SetBPLOutput(const value : string);

    procedure SetSearchPaths(const value : IList<string>);
    function GetVerbosity : TCompilerVerbosity;
    procedure SetVerbosity(const value : TCompilerVerbosity);


//  function GetPlatformName : string;
    function GetProjectSearchPath(const configName : string) : string;

    function GetCompilerOutput : TStrings;

    function GetMSBuildParameters(const configName : string; const packageVersion : TPackageVersion) : string;
    function GetCommandLine(const platform : TDPMPlatform; const projectFile : string; const configName : string; const packageVersion : TPackageVersion) : string;

    function BuildProject(const cancellationToken : ICancellationToken; const platform : TDPMPlatform; const projectFile : string; const configName : string; const packageVersion : TPackageVersion; const forDesign : boolean) : Boolean;
  public
    constructor Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider;
                       const packageCacheFolder : string);
    destructor Destroy; override;

  end;

implementation

uses
  WinApi.Windows,
  WinApi.ShlObj,
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Diagnostics,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Process,
  DPM.Core.Compiler.ProjectSettings;


const
  //The only platforms whose build needs nothing from the IDE environment - everything the compiler
  //and linker need lives under $(BDSLIB)\<platform>\release, which CodeGear.Delphi.Targets adds by
  //itself. See the ImportEnvOptions note in BuildProject.
  cWindowsPlatforms : TDPMPlatforms = [TDPMPlatform.Win32, TDPMPlatform.Win64,
                                       TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC];

//cmd.exe by its full path - never a cmd.exe in DPM's folder or the current one.
function CmdExePath : string;
var
  buffer : array[0..MAX_PATH] of Char;
  len : UINT;
begin
  len := GetSystemDirectory(buffer, Length(buffer));
  if (len = 0) or (len > MAX_PATH) then
    raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find the system folder (for cmd.exe) : ' + SysErrorMessage(GetLastError));
  SetString(result, buffer, len);
  result := IncludeTrailingPathDelimiter(result) + 'cmd.exe';
end;

//the user's roaming profile folder - $(APPDATA).
function AppDataFolder : string;
var
  buffer : array[0..MAX_PATH] of Char;
begin
  result := GetEnvironmentVariable('APPDATA');
  if result = '' then
  begin
    if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @buffer[0]) <> S_OK then
      raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find the user''s application data folder (APPDATA).');
    result := buffer;
  end;
  result := ExcludeTrailingPathDelimiter(result);
end;

//a path as a global property - quoted, and with no trailing \, which would escape the closing quote.
function PathProperty(const name : string; const path : string) : string;
begin
  result := ' /p:' + name + '=' + TPathUtils.QuotePath(ExcludeTrailingPathDelimiter(path), true);
end;

//The log is ours to tidy up - not something a build fails over.
procedure DeleteCompilerLog(const logger : ILogger; const fileName : string);
begin
  try
    if TFile.Exists(fileName) then
      TFile.Delete(fileName);
  except
    on e : Exception do
      logger.Debug('Unable to delete the compiler log [' + fileName + '] : ' + e.Message);
  end;
end;

{ TMSBuildCompiler }

function TMSBuildCompiler.BuildProject(const cancellationToken : ICancellationToken; const platform : TDPMPlatform; const projectFile : string; const configName : string; const packageVersion : TPackageVersion; const forDesign : boolean) : Boolean;
var
  commandLine : string;
  env : IEnvironmentBlock;
  i : integer;
  exitCode : Cardinal;
  oemEncoding : TEncoding;
begin

  result := false;
  FPlatform := platform;
  FBuildForDesign := forDesign;
  FCompilerOutput.Clear;

  FCompilerLogFile := TPath.GetTempFileName;
  FProjectFile := projectFile;

  try
    commandLine := GetCommandLine(platform, projectFile, configName, packageVersion);
  except
    //eg the compiler isn't installed - an EDPMError that says so, reported with its own code.
    on e : Exception do
    begin
      LogException(FLogger, e);
      DeleteCompilerLog(FLogger, FCompilerLogFile);
      exit;
    end;
  end;

  env := TEnvironmentBlockFactory.Create(nil, true);
  {$IFDEF DEBUG}
  //THE IDE set these, which makes it difficult to debug the command line version.
  env.RemoveVariable('BDS');
  env.RemoveVariable('BDSLIB');
  env.RemoveVariable('BDSINCLUDE');
  env.RemoveVariable('BDSCOMMONDIR');
  {$ENDIF}
  env.RemoveVariable('PLATFORM');
  //cmd looks for a command in the current folder before the PATH unless this is set - and a package's own folder
  //could hold an msbuild.bat. Everything is run by its full path anyway; this covers what msbuild itself starts.
  env.AddOrSet('NoDefaultCurrentDirectoryInExePath', '1');

  //EnvOptions.proj is the IDE's environment - the per platform library paths and the selected
  //platform SDK. CodeGear.Common.Targets imports it unless ImportEnvOptions is exactly 'false'.
  //
  //For Windows we keep it out on purpose : it drags the user's whole IDE library path (third party
  //libs, catalog repository, madExcept etc) into what is meant to be an isolated package build, and
  //on build machines those paths don't exist at all.
  //
  //For every other platform it is NOT optional. The SDK selection (--sysroot) and the linker
  //library paths - $(BDS)\redist\<platform>, $(BDSCOMMONDIR)\Dcp\<platform>, Bpl\<platform>, and the
  //SDK's own lib dirs - come from nowhere else, so suppressing it fails the link with
  //'cannot find -lgcc_s / -lc / -ldl / -lpthread' long after the compile itself succeeded.
  //Note the unit search path stays under our control either way - we pass DCC_UnitSearchPath as a
  //global (command line) property, which wins over anything the project or EnvOptions sets.
  //(ImportEnvOptions=false is passed as a global property for these - see GetMSBuildParameters.)
  FLogger.Debug('Compler - cmdline : ' + commandLine);
  try
    exitCode := TProcess.Execute2(cancellationToken, CmdExePath, commandLine,'',env);
  except
    on e : Exception do
    begin
      FLogger.Error(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to run msbuild to compile [' + projectFile + '] : ' + e.Message);
      DeleteCompilerLog(FLogger, FCompilerLogFile);
      exit;
    end;
  end;
  result := exitCode = 0;
  //TODO : Doesn't give realtime feedback, remove when we have a CreateProcess version of TProcess.
  if TFile.Exists(FCompilerLogFile) then
  begin
    if (not result) and (not cancellationToken.IsCancelled) then
    begin
      //what cmd redirected - console output, in the OEM code page.
      oemEncoding := TEncoding.GetEncoding(GetOEMCP);
      try
        FCompilerOutput.LoadFromFile(FCompilerLogFile, oemEncoding);
      finally
        oemEncoding.Free;
      end;
      for i := 0 to FCompilerOutput.Count -1 do
        FLogger.Information(FCompilerOutput.Strings[i]);
      FCompilerOutput.Clear;
    end;
    DeleteCompilerLog(FLogger, FCompilerLogFile);
  end;
  //after the output, so the error is the last thing shown. A cancelled build is not a failure to
  //report - the operation reports the cancellation.
  if (not result) and (not cancellationToken.IsCancelled) then
    FLogger.Error(TErrorCode.E5003_PackageCompileFailed, 'Compiling [' + projectFile + '] for ' + DPMPlatformToString(platform) +
                  ' failed (msbuild exit code ' + IntToStr(exitCode) + ') - see the compiler output above.');
end;

constructor TMSBuildCompiler.Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider;
                                    const packageCacheFolder : string);
begin
  FLogger := logger;
  FPackageCacheFolder := packageCacheFolder;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FEnv := env;
  FSearchPaths := TCollections.CreateList<string>;
  FCompilerOutput := TStringList.Create;
end;

destructor TMSBuildCompiler.Destroy;
begin
  FCompilerOutput.Free;
  inherited;
end;

function TMSBuildCompiler.GetLibOutput: string;
begin
  result := FLibOutput;
end;

function TMSBuildCompiler.GetBPLOutput : string;
begin
  result := FBPLOutput;
end;

function TMSBuildCompiler.GetCommandLine(const platform : TDPMPlatform; const projectFile, configName : string; const packageVersion : TPackageVersion) : string;
begin
  FProjectFile := projectFile;
  //cmd.exe runs rsvars.bat - the compiler's environment - then MSBuild.exe by its full path, never an 'msbuild' looked
  //up on the PATH (cmd looks in the current folder first). Both go to the log, errors included. /s /c "..." makes cmd
  //strip just the outer quotes, so every path inside can be quoted.
  result := '/s /c "(call ' + TPathUtils.QuotePath(FEnv.GetRsVarsFilePath(platform, FCompilerVersion), true) +
            ' && ' + TPathUtils.QuotePath(FEnv.GetMSBuildPath(platform, FCompilerVersion), true) +
            ' ' + TPathUtils.QuotePath(projectFile, true) + ' ' + GetMSBuildParameters(configName, packageVersion) +
            ') > ' + TPathUtils.QuotePath(FCompilerLogFile, true) + ' 2>&1"';
end;

function TMSBuildCompiler.GetCompilerOutput : TStrings;
begin
  result := FCompilerOutput;
end;

function TMSBuildCompiler.GetCompilerVersion : TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TMSBuildCompiler.GetConfiguration : string;
begin
  result := FConfiguration;
end;


function TMSBuildCompiler.GetMSBuildParameters(const configName : string; const packageVersion : TPackageVersion) : string;
var
  libPath : string;
  bplPath : string;
begin
  //build version info resource first, then bpl.
  //this assumes the dproj has the correct version info.
  //We should investigate updating the dproj.
  result := '/target:BuildVersionResource;Build';
  result := result + ' /p:Config=' + configName;
  result := result + ' /p:Platform=' + DPMPlatformToBDString(FPlatform);

  //THIS DOES NOT WORK!!!
  //attempting to update version info
//  result := result + ' /p:VerInfo_IncludeVerInfo=true';
//  result := result + ' /p:VerInfo_MajorVer=' + IntToStr(packageVersion.Major);
//  result := result + ' /p:VerInfo_MinorVer=' + IntToStr(packageVersion.Minor);
//  result := result + ' /p:VerInfo_Release=' + IntToStr(packageVersion.Patch);
//
//  if packageVersion.IsStable then
//    result := result + ' /p:VerInfo_Build=1'
//  else
//    result := result + ' /p:VerInfo_Build=0';


  //TODO : Check that these props are correctly named for all supported compiler versions.

  if FLibOutput <> '' then
  begin
    libPath := TPathUtils.QuotePath(ExcludeTrailingPathDelimiter(FLibOutput)); //msbuild is fussy about trailing path delimeters!
    result := result + ' /p:DCC_DcpOutput=' + libPath;
    result := result + ' /p:DCC_DcuOutput=' + libPath;
    result := result + ' /p:DCC_ObjOutput=' + libPath;
    result := result + ' /p:DCC_HppOutput=' + libPath;
    result := result + ' /p:DCC_BpiOutput=' + libPath;
  end;

  if FBPLOutput <> '' then
  begin
    bplPath := TPathUtils.QuotePath(ExcludeTrailingPathDelimiter(FBPLOutput));
    result := result + ' /p:DCC_BplOutput=' + bplPath;
  end;

  //implict rebuild off - stops the E2466 Never-build package 'X' requires always-build package 'Y'
  //Note that some third party libs like omnithreadlibrary have always-build/implicitbuild on
  result := result + ' /p:DCC_OutputNeverBuildDcps=true';

  //Package dprojs are whatever the package author happened to ship - DPM compiles them, it does
  //not run them. Every hook below is a *property* the CodeGear targets test before executing
  //something, so passing it empty as a global (command line) property beats anything the dproj -
  //or a file the dproj imports - sets, leaving the guard false and the step skipped :
  //
  //  PreBuildEvent / PostBuildEvent - CodeGear.Common.Targets, <Exec> gated on
  //                                   '$(PreBuildEvent)'!='' etc. Condition identical XE2..13.
  //  PreLinkEvent                   - the C++ equivalent in CodeGear.Cpp.Targets. A no-op for a
  //                                   dproj, blocked now so cbproj support can't quietly reopen it.
  //  CustomToolCommand              - the command __RunUserTool hands to UserTask for a per file
  //                                   custom build tool. Empty makes _RunUserTools warn and skip.
  //  _PreCompileTargets /           - target *names* spliced into CoreBuildDependsOn. No shipped
  //  _PostCompileTargets              .Targets defines either, so a value could only be the
  //                                   project's own, injecting arbitrary targets into the build.
  //
  //Not a sandbox, and can't be - a dproj is an msbuild file, so it can still hang a target off
  //Build with AfterTargets, or <Import> one that does. What this shuts is the IDE level hooks,
  //which is all a package that only wants to be compiled has any business using.
  result := result + ' /p:PreBuildEvent= /p:PostBuildEvent= /p:PreLinkEvent=';
  result := result + ' /p:CustomToolCommand= /p:_PreCompileTargets= /p:_PostCompileTargets=';

  //DPM.CopyLocal.targets is DPM's own consumer side import - it appends a target to BuildDependsOn
  //that execs dpm.exe. Authors who use DPM sometimes ship a dproj still carrying the import, and
  //running copylocal against a project sitting in the package cache is meaningless work. The
  //generated targets file gates itself on this property, so setting it is the supported off switch
  //- and it is what lets TBuildHookValidator treat that import as harmless rather than reject it.
  result := result + ' /p:DPMCopyLocalDisable=true';

  //The imports CodeGear's own targets make are chosen by properties : GlobalOptionFile (imported whenever it is set),
  //APPDATA and BDSAppDataBaseDir (environment.proj, EnvOptions.proj), BDS (the targets themselves and Profiles), and
  //DPMCache for DPM's copylocal import. A dproj - or a file it imports - that sets one of them could point the
  //import at a file of its own, inside the package. Global (command line) properties win over anything a project
  //sets, so it can't. EnvironmentSettings, EnvOptions, Profiles and ProductVersion are set unconditionally by
  //CodeGear.Common.Targets from these, and imported at once. TBuildHookValidator refuses a dproj that sets any of
  //them as well.
  result := result + ' /p:GlobalOptionFile=';
  result := result + ' /p:BDSAppDataBaseDir=BDS';
  result := result + PathProperty('APPDATA', AppDataFolder);
  result := result + PathProperty('BDS', FEnv.GetRootDir(FCompilerVersion));
  if FPackageCacheFolder <> '' then
    result := result + PathProperty('DPMCache', FPackageCacheFolder);
  //see BuildProject - the IDE's environment stays out of a Windows package build.
  if FPlatform in cWindowsPlatforms then
    result := result + ' /p:ImportEnvOptions=false';

  result := result + ' /p:DCC_UnitSearchPath=' +  GetProjectSearchPath(configName);

  //result := result +  ' /v:diag';
 end;


function TMSBuildCompiler.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

//function TMSBuildCompiler.GetPlatformName: string;
//begin
//  if FBuildForDesign then
//  begin
//    result := DPMPlatformToBDString(TDPMPlatform.Win32)
//  end
//  else
//    result := DPMPlatformToBDString(FPlatform);
//end;

function TMSBuildCompiler.GetProjectSearchPath(const configName: string): string;
var
  s : string;
  settingsLoader : IProjectSettingsLoader;
  platform : TDPMPlatform;
begin
  result := '';// '$(BDSLIB)\$(PLATFORM)\release;$(BDS)\include';
  if FSearchPaths.Any then
  begin
    for s in FSearchPaths do
    begin
      if result <> '' then
        result := result + ';';
      result := result + ExcludeTrailingPathDelimiter(s);
    end;
  end;

  platform := FPlatform;

  FLogger.Debug('Loading project to get search path : ' + FProjectFile);
  settingsLoader := TDPMProjectSettingsLoader.Create(FLogger, FProjectFile, configName, platform);
  s := settingsLoader.GetSearchPath;
  FLogger.Debug('Project Search Path : ' + s);

  if s <> '' then
  begin
    if result <> '' then
      result := s + ';' + result
    else
      result := s;
  end;

  FLogger.Debug('SearchPath : ' + result);

  result := TPathUtils.QuotePath(result, true);
end;

function TMSBuildCompiler.GetSearchPaths : IList<string>;
begin
  result := FSearchPaths;
end;

function TMSBuildCompiler.GetVerbosity : TCompilerVerbosity;
begin
  result := FVerbosity;
end;

procedure TMSBuildCompiler.SetLibOutput(const value: string);
begin
  FLibOutput := value;
end;

procedure TMSBuildCompiler.SetBPLOutput(const value : string);
begin
  FBPLOutput := value;
end;

procedure TMSBuildCompiler.SetConfiguration(const value : string);
begin
  FConfiguration := value;
end;


procedure TMSBuildCompiler.SetSearchPaths(const value : IList<string> );
begin
  FSearchPaths.Clear;
  if value <> nil then
    FSearchPaths.AddRange(value);
end;

procedure TMSBuildCompiler.SetVerbosity(const value : TCompilerVerbosity);
begin
  FVerbosity := value;
end;

end.

