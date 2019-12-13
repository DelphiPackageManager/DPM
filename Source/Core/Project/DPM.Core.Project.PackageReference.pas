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
  DPM.Core.Types,
  DPM.Core.Project.Interfaces;

type
  TPackageReference = class(TInterfacedObject, IPackageReference)
  private
    FId: string;
    FVersion : TPackageVersion;
    FPlatform : TDPMPlatform;
  protected
    function GetId: string;
    function GetVersion: TPackageVersion;
    function GetPlatform: TDPMPlatform;
    procedure SetVersion(const value : TPackageVersion);
  public
    constructor Create(const id : string; const version : TPackageVersion; const platform : TDPMPlatform);
  end;

implementation

{ TPackageRefence }

constructor TPackageReference.Create(const id: string; const version: TPackageVersion; const platform : TDPMPlatform);
begin
  FId := id;
  FVersion := version;
  FPlatform := platform;
end;

function TPackageReference.GetId: string;
begin
  result := FId;
end;

function TPackageReference.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageReference.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

procedure TPackageReference.SetVersion(const value: TPackageVersion);
begin
  FVersion := value;
end;

end.
