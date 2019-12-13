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

unit DPM.Core.Project.Interfaces;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version;

{$SCOPEDENUMS ON}

type
  //represents a package reference in the dproj
  IPackageReference = interface
  ['{FC4548EF-A449-4E78-9D9B-C01B5BE0E389}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetPlatform : TDPMPlatform;
    procedure SetVersion(const value : TPackageVersion);
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion write SetVersion;
    property Platform : TDPMPlatform read GetPlatform;
  end;


  IProjectConfiguration = interface
  ['{2A251BA1-1D1E-4D0A-95B0-BD0036A14D94}']
    function GetPlatform : TDPMPlatform;
    function GetName     : string;
    function GetOutputDir : string;
    function GetLinkWithRuntime : boolean;
    function GetPackages : IList<string>;

    property Platform   : TDPMPlatform read GetPlatform;
    property Name       : string read GetName;
    property OutputDir  : string read GetOutputDir;
    property LinkWithRuntime : boolean read GetLinkWithRuntime;
    property Packages   : IList<string> read GetPackages;
  end;


  TAppType = (Application, Package,Lib, Unknown);

  //used by the package installer
  IProjectEditor = interface
  ['{CFF241F9-8B5B-44FC-95FC-6C1A015637E9}']
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatforms : TDPMPlatforms;
    function GetProjectVersion : string;
    function GetPackageReferences : IList<IPackageReference>;
    function GetAppType : TAppType;

    function LoadProject(const filename : string) : boolean;
    procedure Reset;

    function AddSearchPaths(const platform : TDPMPlatform; const searchPaths : IList<string>; const packageCacheLocation : string) : boolean;
    function AddOrUpdatePackageReference(const packageReference : IPackageReference) : boolean;

    function SaveProject(const fileName : string = '') : boolean;

    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property ProjectVersion  : string read GetProjectVersion;
    property Platforms : TDPMPlatforms read GetPlatforms;
    property PackageReferences : IList<IPackageReference> read GetPackageReferences;
    property AppType : TAppType read GetAppType;

  end;


  IGroupProjectReader = interface
  ['{D7E07C8A-A9C1-44B3-9306-F00EDFB9825D}']
    function LoadGroupProj(const groupProjFile : string) : boolean;
    procedure Close;
    function ExtractProjects(const list : IList<string>) : boolean;
  end;


implementation

end.
