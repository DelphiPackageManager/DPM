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

unit DPM.Core.Dependency.Resolution;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces;

type
  TResolution = class(TInterfacedObject, IResolution)
  private
    FPackage :  IPackageInfo;
    FDependency : IPackageDependency;
    FParentId : string;
  protected
    function GetPackage :  IPackageInfo;
    function GetDependency : IPackageDependency;
    function GetParentId : string;
  public
     constructor Create( const package :  IPackageInfo; const dependency : IPackageDependency; const parentId : string);
  end;

implementation

{ TResolution }

constructor TResolution.Create(const package: IPackageInfo; const dependency: IPackageDependency; const parentId: string);
begin
  FPackage := package;
  FDependency := dependency;
  FParentId := parentId;
end;

function TResolution.GetDependency: IPackageDependency;
begin
  result := FDependency;
end;

function TResolution.GetPackage: IPackageInfo;
begin
  result := FPackage;
end;

function TResolution.GetParentId: string;
begin
  result := FParentId;
end;

end.
