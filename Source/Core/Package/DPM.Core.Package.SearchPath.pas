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

unit DPM.Core.Package.SearchPath;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces;

type
  TPackageSearchPath = class(TInterfacedObject, IPackageSearchPath)
  private
    FBinariesOnly: Boolean;
    FPath: string;
    FSourceOnly: Boolean;
  protected
    function GetBinariesOnly: Boolean;
    function GetPath: string;
    function GetSourceOnly: Boolean;
  public
    constructor Create(const path : string; const sourceOnly : boolean; const binOnly : boolean);
  end;

implementation

{ TPackageSearchPath }

constructor TPackageSearchPath.Create(const path: string; const sourceOnly, binOnly: boolean);
begin
  FPath := path;
  FBinariesOnly := binOnly;
  FSourceOnly := sourceOnly;
end;

function TPackageSearchPath.GetBinariesOnly: Boolean;
begin
  result := FBinariesOnly;
end;

function TPackageSearchPath.GetPath: string;
begin
  result := FPath;
end;

function TPackageSearchPath.GetSourceOnly: Boolean;
begin
  result := FSourceOnly;
end;

end.
