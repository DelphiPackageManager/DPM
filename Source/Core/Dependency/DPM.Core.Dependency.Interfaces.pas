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
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version;

type
  IGraphNode = interface;

  TNodeVisitProc = reference to procedure(const node : IGraphNode);

  //a directed asyclic graph (DAG).
  IGraphNode = interface
    ['{20055C26-8E63-4936-8249-ACF8514A37E7}']
    function GetLevel : integer;
    function GetId : string;
    function GetParent : IGraphNode;
    function GetSelectedVersion : TPackageVersion;
    procedure SetSelectedVersion(const value : TPackageVersion);

    function GetSelectedOn : TVersionRange;
    procedure SetSelectedOn(const value : TVersionRange);

    function GetSearchPaths : IList<string>;
    function GetLibPath : string;
    procedure SetLibPath(const value : string);
    function GetBplPath : string;
    procedure SetBplPath(const value : string);


    function GetChildNodes : IEnumerable<IGraphNode>;

    function GetPlatform : TDPMPlatform;

    function AddChildNode(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange) : IGraphNode;
    //Breadth first search
    function FindFirstNode(const id : string) : IGraphNode;
    function FindNodes(const id : string) : IList<IGraphNode>;
    //searches this node only
    function FindChild(const id : string) : IGraphNode;

    //removes any child with id recursively (and it's children)
    procedure Prune(const id : string);
    function RemoveNode(const node : IGraphNode) : boolean;
    function IsRoot : boolean;
    function IsTopLevel : boolean;
    function HasChildren : boolean;
    procedure VisitDFS(const visitor : TNodeVisitProc);

    //used by BOM check
    function AreEqual(const otherNode : IGraphNode; const depth : integer = 1) : boolean;

    property Id : string read GetId;
    property SelectedVersion : TPackageVersion read GetSelectedVersion write SetSelectedVersion;
    property SelectedOn : TVersionRange read GetSelectedOn write SetSelectedOn;
    property Level : integer read GetLevel;
    property ChildNodes : IEnumerable<IGraphNode>read GetChildNodes;
    property Parent : IGraphNode read GetParent;
    property Platform : TDPMPlatform read GetPlatform;

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

    property Package : IPackageInfo read GetPackage;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
    property ParentId : string read GetParentId;
  end;

  TProjectReference = record
    Package : IPackageInfo;
    VersionRange : TVersionRange;
    ParentId : string;
  end;

  IDependencyResolver = interface
    ['{B187F0DB-FEA1-48B4-81F2-CECF073C2FB0}']
    //returns true if all dependencies were resolved. If true, the graph is fully populated and can be serialized.
    function ResolveForInstall(const cancellationToken : ICancellationToken; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
    function ResolveForRestore(const cancellationToken : ICancellationToken; const options : TSearchOptions; const projectReferences : IList<TProjectReference>; var dependencyGraph : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
  end;


implementation

end.

