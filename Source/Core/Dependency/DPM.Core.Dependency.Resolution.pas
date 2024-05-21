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
  TResolvedPackage = class(TInterfacedObject, IResolvedPackage)
  private
    FPackage : IPackageInfo;
    FParentId : string;
    FVersionRange : TVersionRange;
    FProject : string;
  protected
    function GetIsTopLevel : boolean;
    function GetPackage : IPackageInfo;
    function GetParentId : string;
    function GetProject : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function Clone(const parentId : string) : IResolvedPackage;
  public
    constructor Create(const package : IPackageInfo; const range : TVersionRange; const parentId : string; const project : string);
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Constants;

{ TResolvedPackage }

function TResolvedPackage.Clone(const parentId: string): IResolvedPackage;
begin
  result := TResolvedPackage.Create(FPackage, FVersionRange, parentId, FProject);
end;

constructor TResolvedPackage.Create(const package : IPackageInfo; const range : TVersionRange; const parentId : string; const project : string);
begin
  FPackage := package;
  FVersionRange := range;
  if FVersionRange.IsEmpty then
    raise EArgumentOutOfRangeException.Create('Empty version range provided for resolution');
  FParentId := parentId;
  FProject := project;
end;


function TResolvedPackage.GetIsTopLevel: boolean;
begin
  result := FParentId = cRootNode;
end;

function TResolvedPackage.GetPackage : IPackageInfo;
begin
  result := FPackage;
end;

function TResolvedPackage.GetParentId : string;
begin
  result := FParentId;
end;

function TResolvedPackage.GetProject: string;
begin
  result := FProject;
end;

function TResolvedPackage.GetVersionRange : TVersionRange;
begin
  result := FVersionRange;
end;

procedure TResolvedPackage.SetVersionRange(const value : TVersionRange);
begin
  FVersionRange := value;
end;

end.

