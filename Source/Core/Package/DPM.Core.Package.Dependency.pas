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

unit DPM.Core.Package.Dependency;

interface

uses
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces;

type
  TPackageDependency = class(TInterfacedObject, IPackageDependency)
  private
    FDependencyVersion: TVersionRange;
    FId: string;
    FPlatform: TDPMPlatform;
  protected
    function GeTVersionRange: TVersionRange;
    function GetId: string;
    function GetPlatform: TDPMPlatform;
    procedure SetVersionRange(const value : TVersionRange);
  public
     constructor Create(const id : string; const version : TVersionRange; const platform : TDPMPlatform);
  end;

implementation

{ TPackageDependency }

constructor TPackageDependency.Create(const id: string; const version: TVersionRange; const platform: TDPMPlatform);
begin
  FId := id;
  FDependencyVersion := version;
  FPlatform := platform;
end;

function TPackageDependency.GeTVersionRange: TVersionRange;
begin
  result := FDependencyVersion;
end;

procedure TPackageDependency.SetVersionRange(const value: TVersionRange);
begin
  FDependencyVersion := value;
end;

function TPackageDependency.GetId: string;
begin
  result := FId;
end;

function TPackageDependency.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;


end.
