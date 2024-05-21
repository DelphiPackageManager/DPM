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

unit DPM.Core.Dependency.Interfaces;

interface

uses
  Spring.Collections,
  System.Classes,
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Options.Search,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version;

type
  IPackageReference = interface;

  TNodeVisitProc = reference to procedure(const packageReference : IPackageReference);

  //a directed asyclic graph (DAG).
  IPackageReference = interface(IPackageIdentity)
    ['{20055C26-8E63-4936-8249-ACF8514A37E7}']
    function GetId : string;
    function GetParent : IPackageReference;
    function GetParentId : string;
    procedure SetParent(const value : IPackageReference);
    function GetVersion : TPackageVersion;
    procedure SetVersion(const value : TPackageVersion);

    function GetSelectedOn : TVersionRange;
    procedure SetSelectedOn(const value : TVersionRange);

    function GetSearchPaths : IList<string>;
    function GetLibPath : string;
    procedure SetLibPath(const value : string);
    function GetBplPath : string;
    procedure SetBplPath(const value : string);
    procedure SetUseSource(const value : boolean);
    procedure SetProjectFile(const value : string);

    function GetDependencies : IEnumerable<IPackageReference>;

    function GetPlatform : TDPMPlatform;
    function GetUseSource : boolean;
    function GetIsTransitive : boolean;
    function GetIsTopLevel : boolean;
    function GetProjectFile : string;

    function GetPackageInfo : IPackageInfo;
    procedure SetPackageInfo(const value : IPackageInfo);

    //these will be added when we read the package manifest
//    procedure AddDesignBPL(const platform : TDPMPlatform; const bplFile : string);
//
//    //returns true if we have already loaded the bpls
//    function GetDesignBPLsLoaded(platform : TDPMPlatform) : boolean;
//
//    procedure SetDesignBPLsLoaded(platform : TDPMPlatform; const value : boolean);
//
//    function GetDesignBPLs(platform : TDPMPlatform) : IList<string>;

    function AddChild(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IPackageReference;

    procedure AddExistingChild(const id : string; const packageReference : IPackageReference);
    /// <summary>
    /// Finds the first reference to a package [id] using a breadth first search
    ///  </summary>
    function FindFirstChild(const id : string) : IPackageReference;

    /// <summary>
    /// Finds all references to a package [id] using a breadth first search
    ///  </summary>
    function FindChildren(const id : string) : IList<IPackageReference>;

    /// <summary>
    ///  Find a non transitive dependency
    /// </summary>
    function FindTopLevelChild(const id : string) : IPackageReference;

    /// <summary>
    /// Returns true if node has a top level dependency on [id]
    /// </summary>
    function HasTopLevelChild(const id : string) : boolean;

    /// <summary>
    /// Returns true if we have a dependency on [id] at any level in the graph
    /// </summary>
    function HasAnyChild(const id : string) : boolean;

    /// <summary>
    ///  Clones the node without any dependencies.
    ///  </summary>
    function Clone : IPackageReference;

    /// <summary>
    /// Removes any child with id recursively (and it's children)
    /// </summary>
    function RemoveChild(const packageReference : IPackageReference) : boolean;

    /// <summary>
    /// Removes a top level dependency, returns true if removed
    /// </summary>
    function RemoveTopLevelChild(const id : string) : boolean;

    function IsRoot : boolean;
    function HasChildren : boolean;

    /// <summary>
    /// Visits child nodes in depth first mode and calls the visitor proc with each node
    /// </summary>
    procedure VisitDFS(const visitor : TNodeVisitProc);

    function ToIdVersionString : string;

    /// <summary>
    ///  Compares nodes including their children
    ///  used by BOM check
    /// </summary>
    function AreEqual(const otherPackageReference : IPackageReference; const depth : integer = 1) : boolean;

    property Id : string read GetId;
    property VersionRange : TVersionRange read GetSelectedOn write SetSelectedOn;
    property IsTransitive : boolean read GetIsTransitive;
    property IsTopLevel : boolean read GetIsTopLevel;
    property Children : IEnumerable<IPackageReference>read GetDependencies;
    property Parent : IPackageReference read GetParent;
    property Platform : TDPMPlatform read GetPlatform;
    property UseSource : boolean read GetUseSource write SetUseSource;
    property ProjectFile : string read GetProjectFile write SetProjectFile;

    //build support
    property SearchPaths : IList<string> read GetSearchPaths;
    property LibPath : string read GetLibPath write SetLibPath;
    property BplPath : string read GetBplPath write SetBplPath;

    //resolution support
    property PackageInfo : IPackageInfo read GetPackageInfo write SetPackageInfo;
    property ParentId : string read GetParentId;

    //design support
//    property DesignBpls[platform : TDPMPlatform] : IList<string> read GetDesignBPLs;
//
//    property DesignBplsLoaded[platform : TDPMPlatform] : boolean read GetDesignBplsLoaded write SetDesignBplsLoaded;

  end;


  IResolvedPackage = interface
    ['{CC4F63AA-80F7-46AC-8C42-0F8725B59579}']
    function GetPackage : IPackageInfo;
    function GetParentId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function GetProject : string;
    function GetIsTopLevel : boolean;

    function Clone(const parentId : string) : IResolvedPackage;

    property IsTopLevel : boolean read GetIsTopLevel;
    property PackageInfo : IPackageInfo read GetPackage;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
    property ParentId : string read GetParentId;
    property ProjectFile : string read GetProject;
  end;

  IDependencyResolver = interface
    ['{B187F0DB-FEA1-48B4-81F2-CECF073C2FB0}']
    function Initialize(const config : IConfiguration) : boolean;
    //returns true if all dependencies were resolved. If true, the graph is fully populated and can be serialized.
    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageReference>; var dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>) : boolean;
    function ResolveForRestore(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectFile : string; const options : TSearchOptions; const projectReferences : IList<IPackageReference>; var dependencyGraph : IPackageReference; out resolved : IList<IPackageInfo>) : boolean;
  end;


implementation

end.

