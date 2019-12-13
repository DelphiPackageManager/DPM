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

unit DPM.Core.Repository.SearchResult;

interface

uses
  Spring.Collections,
  DPM.Core.Package.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TPackageSearchResult = class(TInterfacedObject, IPackageSearchResult)
  private
    FPackages : IList<IPackageIdentity>;
  protected
    function GetPackages: IList<IPackageIdentity>;
  public
    constructor Create;
  end;

implementation

{ TPackageSearchResult }

constructor TPackageSearchResult.Create;
begin
  FPackages := TCollections.CreateList<IPackageIdentity>;
end;

function TPackageSearchResult.GetPackages: IList<IPackageIdentity>;
begin
  result := FPackages;
end;

end.
