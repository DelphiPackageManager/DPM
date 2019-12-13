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
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces;

type
  TPackageDependency = class(TInterfacedObject, IPackageDependency)
  private
    FId : string;
    FRange : TVersionRange;
  protected
    function GetId: string;
    function GetVersionRange: TVersionRange;
    procedure SetVersionRange(const value: TVersionRange);
  public
    constructor Create(const id, range : string);
    function ToString : string;override;

  end;

implementation

constructor TPackageDependency.Create(const id, range: string);
begin
  FId := id;
  FRange := TVersionRange.Parse(range);
end;

function TPackageDependency.GetId: string;
begin
  result := FId;
end;

function TPackageDependency.GetVersionRange: TVersionRange;
begin
  result := FRange;
end;

procedure TPackageDependency.SetVersionRange(const value: TVersionRange);
begin
  FRange := value;
end;

function TPackageDependency.ToString: string;
begin
  result := FId + ' ' + FRange.ToString;
end;


end.
