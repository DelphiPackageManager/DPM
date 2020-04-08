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
  DPM.Core.Project.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version;

type
  IGraphNode = interface;

  TNodeVisitProc = reference to procedure(const node : IGraphNode);

  TGraphNodeState = (
    Unknown,
    Rejected,
    Selected,
    Candidate,
    Failure
  );

  //a directed asyclic graph (DAG).
  IGraphNode = interface
  ['{20055C26-8E63-4936-8249-ACF8514A37E7}']
    function GetLevel : integer;
    function GetId : string;
    function GetParent : IGraphNode;
    function GetState : TGraphNodeState;
    function GetSelectedVersion : TPackageVersion;
    function GetDependencies : IDictionary<string, IPackageDependency>;

    procedure SetState(const value : TGraphNodeState);
    procedure SetSelectedVersion(const value : TPackageVersion);

    function GetChildNodes : IEnumerable<IGraphNode>;

    function AddChildNode(const id : string; const version : TPackageVersion; const selectedOn : TVersionRange; const dependencies : IEnumerable<IPackageDependency>) : IGraphNode;
    function RemoveChildNode(const  id : string) : boolean;

    //Breadth first search
    function FindNode(const id : string ) : IGraphNode;

    //searches this node only
    function FindChild(const id : string) : IGraphNode;

    function IsRoot : boolean;
    function IsTopLevel : boolean;
    function HasChildren : boolean;
    procedure VisitDFS(const visitor : TNodeVisitProc);

    property Id : string read GetId;
    property State : TGraphNodeState read GetState write SetState;
    property SelectedVersion : TPackageVersion read GetSelectedVersion write SetSelectedVersion;
    property Level : integer read GetLevel;
    property ChildNodes : IEnumerable<IGraphNode> read GetChildNodes;
    property Parent : IGraphNode read GetParent;
    property Dependencies : IDictionary<string, IPackageDependency> read GetDependencies;
  end;


  ILockFile = interface
  ['{777456CA-44C2-4DC8-901A-5332091BD531}']
    function IsValid(const topLevel : IEnumerable<IPackageDependency>) : boolean;
    function GetFileName : string;
    function GetGraph : IGraphNode;
    function CommitToFile(const newFileName : string = '') : boolean;
    function CommitToStream(const stream : TStream) : boolean;
    property FileName : string read GetFileName;
    property Graph : IGraphNode read GetGraph;
  end;

  ILockFileReader = interface
  ['{12D9989C-070C-4872-A56E-CA2EF80B4664}']
    function TryLoadFromString(const value : string; out lockFile : ILockFile) : boolean;
    function TryLoadFromFile(const fileName : string; out lockFile : ILockFile) : boolean;
    function CreateNew(const fileName : string) : ILockFile;
  end;

  IResolution = interface
  ['{CC4F63AA-80F7-46AC-8C42-0F8725B59579}']
    function GetPackage :  IPackageInfo;
    function GetDependency : IPackageDependency;
    function GetParentId : string;

    property Package : IPackageInfo read GetPackage;
    property Dependency : IPackageDependency read GetDependency;
    property ParentId : string read GetParentId;
  end;


  IDependencyResolver = interface
  ['{B187F0DB-FEA1-48B4-81F2-CECF073C2FB0}']
    //returns true if all dependencies were resolved. If true, the graph is fully populated and can be serialized.
    function ResolveForInstall(const cancellationToken : ICancellationToken; const options : TSearchOptions; const newPackage : IPackageInfo; const projectReferences : IList<IPackageInfo>; const lockFile : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
    function ResolveForRestore(const cancellationToken : ICancellationToken; const options : TSearchOptions; const projectReferences : IList<IPackageInfo>; const lockFile : IGraphNode; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; out resolved : IList<IPackageInfo>) : boolean;
  end;


implementation

end.
