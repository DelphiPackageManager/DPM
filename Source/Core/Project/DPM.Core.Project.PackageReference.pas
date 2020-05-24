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

unit DPM.Core.Project.PackageReference;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces;

type
  TPackageReference = class(TInterfacedObject, IPackageReference, IPackageId)
  private
    FId: string;
    FVersion : TPackageVersion;
    FPlatform : TDPMPlatform;
    FCompilerVersion : TCompilerVersion;
    FRange    : TVersionRange;
    FIsTransitive : boolean;
    FDependencies : IList<IPackageReference>;
  protected
    function GetId: string;
    function GetVersion: TPackageVersion;
    function GetPlatform: TDPMPlatform;
    procedure SetVersion(const value : TPackageVersion);
    function GetDependencies: IList<IPackageReference>;
    function GetIsTransitive: Boolean;
    function GetRange: TVersionRange;
    function GetCompilerVersion: TCompilerVersion;

  public
    constructor Create(const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const range : TVersionRange; const isTransitive : boolean);
    function ToIdVersionString: string;
  end;

implementation

{ TPackageRefence }

constructor TPackageReference.Create(const id : string; const version : TPackageVersion; const platform : TDPMPlatform; const compilerVersion : TCompilerVersion; const range : TVersionRange; const isTransitive : boolean);
begin
  FId := id;
  FVersion := version;
  FPlatform := platform;
  FCompilerVersion := compilerVersion;
  FRange := range;
  FIsTransitive := isTransitive;
  FDependencies := TCollections.CreateList<IPackageReference>;
end;

function TPackageReference.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageReference.GetDependencies: IList<IPackageReference>;
begin
  result := FDependencies;
end;

function TPackageReference.GetId: string;
begin
  result := FId;
end;

function TPackageReference.GetIsTransitive: Boolean;
begin
  result := FIsTransitive;
end;

function TPackageReference.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageReference.GetRange: TVersionRange;
begin
  result := FRange;
end;

function TPackageReference.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

procedure TPackageReference.SetVersion(const value: TPackageVersion);
begin
  FVersion := value;
end;

function TPackageReference.ToIdVersionString: string;
begin
  result := FId +' [' + FVersion.ToStringNoMeta + ']';
end;

end.
