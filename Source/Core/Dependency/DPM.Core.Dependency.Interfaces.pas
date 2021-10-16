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
  VSoft.Awaitable,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Options.Search,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version;

type
  IGraphNode = interface;

  TNodeVisitProc = reference to procedure(const node : IGraphNode);

  //a directed asyclic graph (DAG).
  IGraphNode = interface(IPackageId)
    ['{20055C26-8E63-4936-8249-ACF8514A37E7}']
    function GetId : string;
    function GetParent : IGraphNode;
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

    function GetChildNodes : IEnumerable<IGraphNode>;

    function GetPlatform : TDPMPlatform;
    function GetUseSource : boolean;
    function GetIsTransitive : boolean;
    function GetProjectFile : string;

    function AddPackageChildNode(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IGraphNode;
    procedure AddExistingNode(const id : string; const node : IGraphNode);
    ///
    /// Breadth first search
    function FindFirstNode(const id : string) : IGraphNode;
    function FindNodes(const id : string) : IList<IGraphNode>;

    /// <summary> Searches this node only
    /// </summary>
    function FindChild(const id : string) : IGraphNode;

    //removes any child with id recursively (and it's children)
    function RemoveNode(const node : IGraphNode) : boolean;
    function IsRoot : boolean;
    function HasChildren : boolean;
    procedure VisitDFS(const visitor : TNodeVisitProc);

    function ToIdVersionString : string;
    //used by BOM check
    function AreEqual(const otherNode : IGraphNode; const depth : integer = 1) : boolean;

    property Id : string read GetId;
    property SelectedOn : TVersionRange read GetSelectedOn write SetSelectedOn;
    property IsTransitive : boolean read GetIsTransitive;
    property ChildNodes : IEnumerable<IGraphNode>read GetChildNodes;
    property Parent : IGraphNode read GetParent;
    property Platform : TDPMPlatform read GetPlatform;
    property UseSource : boolean read GetUseSource write SetUseSource;
    property ProjectFile : string read GetProjectFile write SetProjectFile;

    //build support
    property SearchPaths : IList<string> read GetSearchPaths;
    property LibPath : string read GetLibPath write SetLibPath;
    property BplPath : string read GetBplPath write SetBplPath;
  end;


  IResolution = interface
    ['{CC4F63AA-80F7-46AC-8C42-0F8725B59579}']
    function GetPackage : IPackageInfo;
    function GetParentId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function GetProject : string;

    function Clone(const parentId : string) : IResolution;

    property Package : IPackageInfo read GetPackage;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
    property ParentId : string read GetParentId;
    property Project : string read GetProject;
  end;

  TProjectReference = record
    Package : IPackageInfo;
    VersionRange : TVersionRange;
    ParentId : string;
  end;

  IDependencyResolver = interface
    ['{B187F0DB-FEA1-48B4-81F2-CECF073C2FB0}']
    procedure Initialize(const config : IConfiguration);
    //returns true if all dependencies were resolved. If true, the graph is fully populated and can be serialized.
    function ResolveForInstall(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectFile : string; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; out resolved : IList<IPackageInfo>) : boolean;
    function ResolveForRestore(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectFile : string; const options : TSearchOptions; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; out resolved : IList<IPackageInfo>) : boolean;
  end;


implementation

end.

