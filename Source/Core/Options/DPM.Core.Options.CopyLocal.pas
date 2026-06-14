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

unit DPM.Core.Options.CopyLocal;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Base;

type
  //Options for the 'dpm copylocal' command, invoked by DPM.CopyLocal.targets from an AfterBuild
  //MSBuild target. It copies package binaries from the cache into the project's build output folder.
  TCopyLocalOptions = class(TOptionsBase)
  private
    FProjectPath : string;
    FPlatform : TDPMPlatform;
    FConfig : string;
    FOutputDir : string;
    FCompilerVersion : TCompilerVersion;
    FUsePackages : boolean;
    FRuntimePackages : string;
    class var
      FDefault : TCopyLocalOptions;
  protected
    constructor CreateClone(const original : TCopyLocalOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TCopyLocalOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TCopyLocalOptions; reintroduce;

    //The .dproj being built (msbuild passes $(MSBuildProjectFullPath)).
    property ProjectPath : string read FProjectPath write FProjectPath;
    //The single platform being built (msbuild passes $(Platform)).
    property Platform : TDPMPlatform read FPlatform write FPlatform;
    //The build config being built (msbuild passes $(Config)).
    property Config : string read FConfig write FConfig;
    //The resolved build output dir to copy into (msbuild passes $(OutputPath)).
    property OutputDir : string read FOutputDir write FOutputDir;
    //The compiler version the project targets (msbuild passes $(DPMCompiler)).
    property CompilerVersion : TCompilerVersion read FCompilerVersion write FCompilerVersion;
    //True when the build links with runtime packages (msbuild passes $(UsePackages)).
    property UsePackages : boolean read FUsePackages write FUsePackages;
    //The project's runtime package link set (msbuild passes $(DCC_UsePackage)). Used to decide
    //whether a runtimeOnly copyLocal entry's bpl is actually referenced by this build.
    property RuntimePackages : string read FRuntimePackages write FRuntimePackages;
  end;

implementation

uses
  System.SysUtils;

{ TCopyLocalOptions }

function TCopyLocalOptions.Clone : TCopyLocalOptions;
begin
  result := TCopyLocalOptions.CreateClone(Self);
end;

constructor TCopyLocalOptions.Create;
begin
  inherited;
  FPlatform := TDPMPlatform.UnknownPlatform;
  FCompilerVersion := TCompilerVersion.UnknownVersion;
  FUsePackages := false;
end;

constructor TCopyLocalOptions.CreateClone(const original : TCopyLocalOptions);
begin
  inherited CreateClone(original);
  FProjectPath := original.FProjectPath;
  FPlatform := original.FPlatform;
  FConfig := original.FConfig;
  FOutputDir := original.FOutputDir;
  FCompilerVersion := original.FCompilerVersion;
  FUsePackages := original.FUsePackages;
  FRuntimePackages := original.FRuntimePackages;
end;

class constructor TCopyLocalOptions.CreateDefault;
begin
  FDefault := TCopyLocalOptions.Create;
end;

function TCopyLocalOptions.Validate(const logger : ILogger) : Boolean;
begin
  result := inherited Validate(logger);

  if FProjectPath = '' then
  begin
    logger.Error('Project path cannot be empty, must be a .dproj file.');
    result := false;
  end;

  if FPlatform = TDPMPlatform.UnknownPlatform then
  begin
    logger.Error('A valid -platform is required.');
    result := false;
  end;

  if FOutputDir = '' then
  begin
    logger.Error('An -outputDir is required.');
    result := false;
  end;
end;

end.
