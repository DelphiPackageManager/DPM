{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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
    constructor Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Process,
  DPM.Core.Utils.System,
  DPM.Core.Compiler.ProjectSettings;


{ TMSBuildCompiler }

function TMSBuildCompiler.BuildProject(const cancellationToken : ICancellationToken; const platform : TDPMPlatform; const projectFile : string; const configName : string; const packageVersion : TPackageVersion; const forDesign : boolean) : Boolean;
var
  commandLine : string;
  env : IEnvironmentBlock;
  i : integer;
  exitCode : Cardinal;
begin

  result := false;
  FBuildForDesign := forDesign;
  FCompilerOutput.Clear;

  FCompilerLogFile := TPath.GetTempFileName;
  FProjectFile := projectFile;

  try
    commandLine := GetCommandLine(platform, projectFile, configName, packageVersion);
  except
    on e : Exception do
    begin
      FLogger.Error('Error generating command line : ' + e.Message);
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
  //envoptions causes problems on our build machines, haven't figured out why yet.
  env.AddOrSet('ImportEnvOptions','false');
  FLogger.Debug('Compler - cmdline : ' + commandLine);
  try
    exitCode := TProcess.Execute2(cancellationToken, 'cmd.exe', commandLine,'',env);
  except
    on e : Exception do
    begin
      FLogger.Error('Error executing compiler : ' + e.Message);
      exit;
    end;
  end;
  result := exitCode = 0;
  if not result then
  FLogger.Error('Package compilation failed - exit code : ' + IntToStr(exitCode));
  //TODO : Doesn't give realtime feedback, remove when we have a CreateProcess version of TProcess.
  if TFile.Exists(FCompilerLogFile) then
  begin
    if not result then
    begin
      FCompilerOutput.LoadFromFile(FCompilerLogFile);
      for i := 0 to FCompilerOutput.Count -1 do
        FLogger.Information(FCompilerOutput.Strings[i]);
      FCompilerOutput.Clear;
    end;
    TFile.Delete(FCompilerLogFile);
  end;
end;

constructor TMSBuildCompiler.Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);
begin
  FLogger := logger;
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
  //I don't like this... but it will do for a start.

  result := 'call "' + FEnv.GetRsVarsFilePath(platform, FCompilerVersion) + '"';
  result := result + ' & msbuild "' + projectfile + '" ' + GetMSBuildParameters(configName, packageVersion);
  result := ' cmd /c ' + result + ' > ' + FCompilerLogFile;
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
  if FBuildForDesign and (not TSystemUtils.Is64bitIDE) then
    result := result + ' /p:Platform=' + DPMPlatformToBDString(TDPMPlatform.Win32)
  else
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

  if FBuildForDesign then
  begin
    if TSystemUtils.Is64BitIDE then
      platform := TDPMPlatform.Win64
    else
      platform := TDPMPlatform.Win32
  end
  else
    platform := FPlatform;

  FLogger.Debug('Loading project to get search path : ' + FProjectFile);
  settingsLoader := TDPMProjectSettingsLoader.Create(FLogger, FProjectFile, configName, platform);
  s := settingsLoader.GetSearchPath;
  FLogger.Debug('Project Search Path : ' + s);

  if s <> '' then
    result := s + ';' + result;

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

