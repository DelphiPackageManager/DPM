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


unit DPM.Core.Compiler.Factory;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces;

type
  TCompilerFactory = class(TInterfacedObject,ICompilerFactory)
  private
    FLogger : ILogger;
    FEnv    : ICompilerEnvironmentProvider;
  protected
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler;
  public
    constructor Create(const logger: ILogger; const env : ICompilerEnvironmentProvider);
  end;

implementation

uses
  DPM.Core.Compiler.MSBuild;

{ TCompilerFactory }

constructor TCompilerFactory.Create(const logger: ILogger; const env: ICompilerEnvironmentProvider);
begin
  FLogger := logger;
  FEnv := env;
end;

function TCompilerFactory.CreateCompiler(const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): ICompiler;
begin
  //if we have different compiler implementations then work that out here.
  result := TMSBuildCompiler.Create(FLogger, compilerVersion, platform, FEnv);

end;

end.
