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
  DPM.Core.Cache.Interfaces,
  DPM.Core.Compiler.Interfaces;

type
  TCompilerFactory = class(TInterfacedObject, ICompilerFactory)
  private
    FLogger : ILogger;
    FEnv : ICompilerEnvironmentProvider;
    FPackageCache : IPackageCache;
  protected
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler; overload;
    function CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler; overload;
  public
    constructor Create(const logger : ILogger; const env : ICompilerEnvironmentProvider; const packageCache : IPackageCache);
  end;

implementation

uses
  DPM.Core.Compiler.MSBuild;

{ TCompilerFactory }

constructor TCompilerFactory.Create(const logger : ILogger; const env : ICompilerEnvironmentProvider; const packageCache : IPackageCache);
begin
  FLogger := logger;
  FEnv := env;
  FPackageCache := packageCache;
end;

function TCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : ICompiler;
begin
  result := CreateCompiler(compilerVersion, platform, FLogger);
end;

function TCompilerFactory.CreateCompiler(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const logger : ILogger) : ICompiler;
begin
  //if we have different compiler implementations then work that out here.
  //the cache root a package's dproj sees as $(DPMCache) - read now, it is set from the configuration.
  result := TMSBuildCompiler.Create(logger, compilerVersion, platform, FEnv, FPackageCache.PackagesFolder);
end;

end.

