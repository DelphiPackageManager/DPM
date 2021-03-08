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

unit DPM.Core.Project.Configuration;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Project.Interfaces;

type
  TProjectConfiguration = class(TInterfacedObject, IProjectConfiguration)
  private
    FName : string;
    FOutputDir : string;
    FPlatform : TDPMPlatform;
    FUsesRuntimePackages : Boolean;
    FPackages : IList<string>;
  protected
    function GetUsesRuntimePackages : Boolean;
    function GetName : string;
    function GetOutputDir : string;
    function GetPlatform : TDPMPlatform;
    function GetPackages : IList<string>;
  public
    constructor Create(const name, outputdir : string; const platform : TDPMPlatform; const usesRuntimePackages : boolean; const packages : IEnumerable<string>);
  end;

implementation

{ TProjectConfiguration }

constructor TProjectConfiguration.Create(const name, outputdir : string; const platform : TDPMPlatform; const usesRuntimePackages : boolean; const packages : IEnumerable<string>);
begin
  FName := name;
  FOutputDir := outputdir;
  FPlatform := platform;
  FUsesRuntimePackages := usesRuntimePackages;
  FPackages := TCollections.CreateList <string> ;
  if packages <> nil then
    FPackages.AddRange(packages);
end;

function TProjectConfiguration.GetUsesRuntimePackages : Boolean;
begin
  result := FUsesRuntimePackages;
end;

function TProjectConfiguration.GetName : string;
begin
  result := FName;
end;

function TProjectConfiguration.GetOutputDir : string;
begin
  result := FOutputDir;
end;

function TProjectConfiguration.GetPackages : IList<string>;
begin
  result := FPackages;
end;

function TProjectConfiguration.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

end.

