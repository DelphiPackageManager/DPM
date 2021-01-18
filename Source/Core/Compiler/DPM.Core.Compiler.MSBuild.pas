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


    function GetCompilerOutput : TStrings;

    function GetMSBuildParameters(const configName : string) : string;
    function GetCommandLine(const projectFile : string; const configName : string) : string;

    function BuildProject(const cancellationToken : ICancellationToken; const projectFile : string; const configName : string; const forDesign : boolean) : Boolean;
  public
    constructor Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Process;


{ TMSBuildCompiler }

function TMSBuildCompiler.BuildProject(const cancellationToken : ICancellationToken; const projectFile : string; const configName : string; const forDesign : boolean) : Boolean;
var
  commandLine : string;
begin
  result := false;
  FBuildForDesign := forDesign;
  FCompilerOutput.Clear;

  FCompilerLogFile := TPath.GetTempFileName;

  commandLine := GetCommandLine(projectFile, configName);
  FLogger.Debug('Compler - cmdline : ' + commandLine);
  try

    result := TProcess.Execute(cancellationToken, 'cmd.exe', commandLine) = 0;
  except
    on e : Exception do
    begin
      FLogger.Error('Error executing compiler : ' + e.Message);
      exit;
    end;
  end;

  //TODO : Doesn't give realtime feedback, remove when we have a CreateProcess version of TProcess.
  if TFile.Exists(FCompilerLogFile) then
  begin
    FCompilerOutput.LoadFromFile(FCompilerLogFile);
    TFile.Delete(FCompilerLogFile);
    FLogger.Information(FCompilerOutput.Text);
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

function TMSBuildCompiler.GetCommandLine(const projectFile, configName : string) : string;
begin
  //I don't like this... but it will do for a start.

  result := 'call "' + FEnv.GetRsVarsFilePath(FCompilerVersion) + '"';
  result := result + '& msbuild "' + projectfile + '" ' + GetMSBuildParameters(configName);
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


function TMSBuildCompiler.GetMSBuildParameters(const configName : string) : string;
var
  s: string;
  searchPath : string;
  libPath : string;
  bplPath : string;
begin
  result := '/target:Build';
  result := result + ' /p:config=' + configName;
  if FBuildForDesign then
    result := result + ' /p:platform=' + DPMPlatformToBDString(TDPMPlatform.Win32)
  else
    result := result + ' /p:platform=' + DPMPlatformToBDString(FPlatform);


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


  result := result + ' /p:DCC_UnitSearchPath=';
  searchPath := '$(BDSLIB)\$(PLATFORM)\release;$(BDS)\include';
  // $(BDSLIB)\$(Platform)\release;$(BDSUSERDIR)\Imports;$(BDS)\Imports;$(BDSCOMMONDIR)\Dcp\$(Platform);$(BDS)\include
  if FSearchPaths.Any then
  begin
    for s in FSearchPaths do
    begin
      if searchPath <> '' then
        searchPath := searchPath + ';';
      searchPath := searchPath + ExcludeTrailingPathDelimiter(s);
    end;
  end;
  result := result + TPathUtils.QuotePath(searchPath, true);

end;


function TMSBuildCompiler.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
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

