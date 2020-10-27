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
    FPackage : IPackageInfo;
    FParentId : string;
    FVersionRange : TVersionRange;
  protected
    function GetPackage : IPackageInfo;
    function GetParentId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);


  public
    constructor Create(const package : IPackageInfo; const range : TVersionRange; const parentId : string);
  end;

implementation

{ TResolution }

constructor TResolution.Create(const package : IPackageInfo; const range : TVersionRange; const parentId : string);
begin
  FPackage := package;
  FVersionRange := range;
  FParentId := parentId;
end;


function TResolution.GetPackage : IPackageInfo;
begin
  result := FPackage;
end;

function TResolution.GetParentId : string;
begin
  result := FParentId;
end;

function TResolution.GetVersionRange : TVersionRange;
begin
  result := FVersionRange;
end;

procedure TResolution.SetVersionRange(const value : TVersionRange);
begin
  FVersionRange := value;
end;

end.

