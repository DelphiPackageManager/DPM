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
    FPath: string;
  protected
    function GetPath: string;
  public
    constructor Create(const path : string);
  end;

implementation

{ TPackageSearchPath }

constructor TPackageSearchPath.Create(const path: string);
begin
  FPath := path;
end;

function TPackageSearchPath.GetPath: string;
begin
  result := FPath;
end;

end.
