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
    FDCPOutput : string;
    FDCUOutput : string;
    FHPPOutput : string;
    FOBJOutput : string;


    FCompilerVersion : TCompilerVersion;
    FConfiguration : string;
    FPlatform : TDPMPlatform;
    FSearchPaths : IList<string>;
    FVerbosity : TCompilerVerbosity;

    FCompilerOutput : TStringList;
  protected
    function GetBPLOutput : string;
    function GetCompilerVersion : TCompilerVersion;
    function GetConfiguration : string;
    function GetDCPOutput : string;
    function GetDCUOutput : string;
    function GetHPPOutput : string;
    function GetOBJOutput : string;
    function GetPlatform : TDPMPlatform;
    function GetSearchPaths : IList<string>;
    procedure SetBPLOutput(const value : string);
    procedure SetConfiguration(const value : string);
    procedure SetDCPOutput(const value : string);
    procedure SetDCUOutput(const value : string);
    procedure SetHPPOutput(const value : string);
    procedure SetOBJOutput(const value : string);
    procedure SetSearchPaths(const value : IList<string>);
    function GetVerbosity : TCompilerVerbosity;
    procedure SetVerbosity(const value : TCompilerVerbosity);


    function GetCompilerOutput : TStrings;

    function GetMSBuildParameters(const configName : string) : string;
    function GetCommandLine(const projectFile : string; const configName : string) : string;

    function BuildProject(const cancellationToken : ICancellationToken; const projectFile : string; const configName : string) : Boolean;
  public
    constructor Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Utils.Process;


{ TMSBuildCompiler }

function TMSBuildCompiler.BuildProject(const cancellationToken : ICancellationToken; const projectFile : string; const configName : string) : Boolean;
var
  commandLine                 : string;
begin
  result := false;
  FCompilerOutput.Clear;

  FCompilerLogFile := TPath.GetTempFileName;

  commandLine := GetCommandLine(projectFile, configName);

  try
    result := TProcess.Execute(cancellationToken, 'cmd.exe', commandLine) = 0;
  except
    on e : Exception do
    begin
      FLogger.Error('Error executing compiler : ' + e.Message);
    end;
  end;

  //TODO : Doesn't give realtime feedback, remove when we have a CreateProcess version of TProcess.
  if TFile.Exists(FCompilerLogFile) then
  begin
    FCompilerOutput.LoadFromFile(FCompilerLogFile);
    TFile.Delete(FCompilerLogFile);
  end;

end;

constructor TMSBuildCompiler.Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);
begin
  FLogger := logger;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FEnv := env;
  FSearchPaths := TCollections.CreateList <string> ;
  FCompilerOutput := TStringList.Create;
end;

destructor TMSBuildCompiler.Destroy;
begin
  FCompilerOutput.Free;
  inherited;
end;

function TMSBuildCompiler.GetBPLOutput : string;
begin
  result := FBPLOutput;
end;

function TMSBuildCompiler.GetCommandLine(const projectFile, configName : string) : string;
var
  cmd                         : string;
begin
  //I don't like this... but it will do for a start.

  result := 'call "' + FEnv.GetRsVarsFilePath(FCompilerVersion) + '\rsvars.bat"';
  result := result + '& msbuild "' + projectfile + '" ' + GetMSBuildParameters(configName);
  result := 'cmd.exe /c ' + cmd + ' > ' + FCompilerLogFile;
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

function TMSBuildCompiler.GetDCPOutput : string;
begin
  result := FDCPOutput;
end;

function TMSBuildCompiler.GetDCUOutput : string;
begin
  result := FDCUOutput;
end;

function TMSBuildCompiler.GetHPPOutput : string;
begin
  result := FHPPOutput;
end;

function TMSBuildCompiler.GetMSBuildParameters(const configName : string) : string;
begin
  result := '/target:Build';
  result := result + ' /p:config=' + configName;
  result := result + ' /p:platform=' + DPMPlatformToBDString(FPlatform);

  //TODO : Check that these props are correctly named for all supported compiler versions.

  if FDCPOutput <> '' then
    result := result + ' /p:DCC_DcpOutput' + ExcludeTrailingPathDelimiter(FDCPOutput); //msbuild is fussy!

  if FDCUOutput <> '' then
    result := result + ' /p:DCC_DcuOutput' + ExcludeTrailingPathDelimiter(FDCUOutput);

  if FBPLOutput <> '' then
    result := result + ' /p:DCC_BplOutput' + ExcludeTrailingPathDelimiter(FBPLOutput);

  if FOBJOutput <> '' then
    result := result + ' /p:DCC_ObjOutput' + ExcludeTrailingPathDelimiter(FOBJOutput);

  if FHPPOutput <> '' then
    result := result + ' /p:DCC_HppOutput' + ExcludeTrailingPathDelimiter(FHPPOutput);

end;

function TMSBuildCompiler.GetOBJOutput : string;
begin
  result := FOBJOutput;
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

procedure TMSBuildCompiler.SetBPLOutput(const value : string);
begin
  FBPLOutput := value;
end;

procedure TMSBuildCompiler.SetConfiguration(const value : string);
begin
  FConfiguration := value;
end;

procedure TMSBuildCompiler.SetDCPOutput(const value : string);
begin
  FDCPOutput := value;
end;

procedure TMSBuildCompiler.SetDCUOutput(const value : string);
begin
  FDCUOutput := value;
end;

procedure TMSBuildCompiler.SetHPPOutput(const value : string);
begin
  FHPPOutput := value;
end;

procedure TMSBuildCompiler.SetOBJOutput(const value : string);
begin
  FOBJOutput := value;
end;

procedure TMSBuildCompiler.SetSearchPaths(const value : IList<string> );
begin
  FSearchPaths := value;
end;

procedure TMSBuildCompiler.SetVerbosity(const value : TCompilerVerbosity);
begin
  FVerbosity := value;
end;

end.

