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
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces;

type
  //Do not create directly, inject a compilerfactory.
  TMSBuildCompiler = class(TInterfacedObject, ICompiler)
  private
    FLogger : ILogger;
    FEnv : ICompilerEnvironmentProvider;

    FBPLOutput: string;
    FCompilerVersion : TCompilerVersion;
    FConfiguration: string;
    FDCPOutput: string;
    FDCUOutput: string;
    FHPPOutput: string;
    FOBJOutput: string;
    FPlatform: TDPMPlatform;
    FSearchPaths: IList<string>;

  protected

    function GetBPLOutput: string;
    function GetCompilerVersion: TCompilerVersion;
    function GetConfiguration: string;
    function GetDCPOutput: string;
    function GetDCUOutput: string;
    function GetHPPOutput: string;
    function GetOBJOutput: string;
    function GetPlatform: TDPMPlatform;
    function GetSearchPaths: IList<string>;
    procedure SetBPLOutput(const value: string);
    procedure SetConfiguration(const value: string);
    procedure SetDCPOutput(const value: string);
    procedure SetDCUOutput(const value: string);
    procedure SetHPPOutput(const value: string);
    procedure SetOBJOutput(const value: string);
    procedure SetSearchPaths(const value: IList<string>);

    function BuildProject(const projectFile: string): Boolean;
  public
    constructor Create(const logger : ILogger; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const env : ICompilerEnvironmentProvider);

  end;

implementation

{ TMSBuildCompiler }

function TMSBuildCompiler.BuildProject(const projectFile: string): Boolean;
begin
  result := false;

end;

constructor TMSBuildCompiler.Create(const logger: ILogger; const compilerVersion: TCompilerVersion; const platform : TDPMPlatform; const env: ICompilerEnvironmentProvider);
begin
  FLogger := logger;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FEnv := env;
  FSearchPaths := TCollections.CreateList<string>;
end;

function TMSBuildCompiler.GetBPLOutput: string;
begin
  result := FBPLOutput;
end;

function TMSBuildCompiler.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TMSBuildCompiler.GetConfiguration: string;
begin
  result := FConfiguration;
end;

function TMSBuildCompiler.GetDCPOutput: string;
begin
  result := FDCPOutput;
end;

function TMSBuildCompiler.GetDCUOutput: string;
begin
  result := FDCUOutput;
end;

function TMSBuildCompiler.GetHPPOutput: string;
begin
  result := FHPPOutput;
end;

function TMSBuildCompiler.GetOBJOutput: string;
begin
  result := FOBJOutput;
end;

function TMSBuildCompiler.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TMSBuildCompiler.GetSearchPaths: IList<string>;
begin
  result := FSearchPaths;
end;

procedure TMSBuildCompiler.SetBPLOutput(const value: string);
begin
  FBPLOutput := value;
end;

procedure TMSBuildCompiler.SetConfiguration(const value: string);
begin
  FConfiguration := value;
end;

procedure TMSBuildCompiler.SetDCPOutput(const value: string);
begin
  FDCPOutput := value;
end;

procedure TMSBuildCompiler.SetDCUOutput(const value: string);
begin
  FDCUOutput := value;
end;

procedure TMSBuildCompiler.SetHPPOutput(const value: string);
begin
  FHPPOutput := value;
end;

procedure TMSBuildCompiler.SetOBJOutput(const value: string);
begin
  FOBJOutput := value;
end;

procedure TMSBuildCompiler.SetSearchPaths(const value: IList<string>);
begin
  FSearchPaths := value;
end;

end.
