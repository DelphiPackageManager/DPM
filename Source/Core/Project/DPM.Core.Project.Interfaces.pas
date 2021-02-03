{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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
  System.Generics.Defaults,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Package.Interfaces;

{$SCOPEDENUMS ON}

type
  //represents a package reference in the dproj
  IPackageReference = interface(IPackageId)
  ['{FC4548EF-A449-4E78-9D9B-C01B5BE0E389}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetPlatform : TDPMPlatform;
    function GetIsTransitive : boolean;
    function GetRange : TVersionRange;
    function GetDependencies : IList<IPackageReference>;
    procedure SetVersion(const value : TPackageVersion);
    function GetHasDependencies : boolean;
    function GetUseSource : boolean;

    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion write SetVersion;
    property Platform : TDPMPlatform read GetPlatform;
    property Range : TVersionRange read GetRange;
    property IsTransitive : boolean read GetIsTransitive;
    property Dependencies : IList<IPackageReference> read GetDependencies;
    property HasDependencies : boolean read GetHasDependencies;
    property UseSource : boolean read GetUseSource;
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
    //function GetPackageReferences : IList<IGraphNode>;
    function GetAppType : TAppType;
    function GetHasPackages : boolean;
    procedure SetCompiler(const value : TCompilerVersion);

    function LoadProject(const filename : string) : boolean;
    procedure Reset;

    function AddSearchPaths(const platform : TDPMPlatform; const searchPaths : IList<string>; const packageCacheLocation : string) : boolean;
    procedure UpdatePackageReferences(const dependencyGraph : IGraphNode; const platform : TDPMPlatform);

    function GetPackageReferences(const platform : TDPMPlatform) : IGraphNode;

    function SaveProject(const fileName : string = '') : boolean;

    property CompilerVersion : TCompilerVersion read GetCompilerVersion write SetCompiler;
    property ProjectVersion  : string read GetProjectVersion;
    property Platforms : TDPMPlatforms read GetPlatforms;
//    property PackageReferences : IList<IGraphNode> read GetPackageReferences;
    property AppType : TAppType read GetAppType;
    property HasPackages : boolean read GetHasPackages;

  end;


  IGroupProjectReader = interface
  ['{D7E07C8A-A9C1-44B3-9306-F00EDFB9825D}']
    function LoadGroupProj(const groupProjFile : string) : boolean;
    procedure Close;
    function ExtractProjects(const list : IList<string>) : boolean;
  end;

  //just compares id's, not version!
  TPackageRefenceComparer = class(TInterfacedObject,IEqualityComparer<IPackageReference>)
  protected
    function Equals(const Left, Right: IPackageReference): Boolean;reintroduce;
    function GetHashCode(const Value: IPackageReference): Integer; reintroduce;
  end;



implementation

// For Delphi XE3 and up:
{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}


uses
  {$IF CompilerVersion >= 29.0}
  System.Hash,
  {$IFEND}
  System.SysUtils;

{ TPackageRefenceComparer }

function TPackageRefenceComparer.Equals(const Left, Right: IPackageReference): Boolean;
begin
  result := SameText(Left.Id, Right.Id);
end;

function TPackageRefenceComparer.GetHashCode(const Value: IPackageReference): Integer;
var
  s : string;
begin
  s := Value.Id;
  {$IF CompilerVersion >= 29.0}
  Result := System.Hash.THashBobJenkins.GetHashValue(s);
  {$ELSE}
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
  {$IFEND}
end;

end.
