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

unit DPM.Core.SBOM.Types;

interface

uses
  Spring.Collections,
  DPM.Core.Types;

{$SCOPEDENUMS ON}

type
  ///<summary>The origin/kind of a component, drives type/supplier defaults in the writers
  /// and lets the generator's classifier route MAP-file evidence to the right component.</summary>
  TSBOMComponentKind = (
    Application,        // the project being analysed itself
    DpmPackage,         // a package installed via DPM
    DelphiRuntime,      // RTL / VCL / FMX (single component per platform)
    ThirdParty,         // discovered via MAP file - not a DPM package, not Delphi runtime
    Unidentified        // MAP-file unit we could not place anywhere
  );

  ///<summary>A key/value pair the writers emit verbatim into CycloneDX `properties[]`
  /// and SPDX `annotations[]`. Used for DPM-specific fields that don't map to a first-class
  /// spec field (e.g. `dpm:framework`, `dpm:useSource`).</summary>
  TSBOMProperty = record
    Name : string;
    Value : string;
  end;

  ///<summary>One unit-of-evidence — typically a source file path discovered in the linker MAP file
  /// that places this component in the binary. Surfaces as `evidence.occurrences[]` in CycloneDX
  /// and as an `annotations[]` entry in SPDX.</summary>
  TSBOMEvidence = record
    Location : string;  // file path or other locator
    Note : string;      // optional - e.g. "from map file" / "from dspec"
  end;

  ///<summary>One component in the SBOM. Plain class (not interfaced) — the report owns
  /// the list and disposes them. Most string fields are optional and emit only when non-empty.
  ///</summary>
  TSBOMComponent = class
  private
    FKind : TSBOMComponentKind;
    FBomRef : string;            // stable id used to link dependency edges within the SBOM
    FId : string;                // package id / display name
    FVersion : string;
    FDescription : string;
    FAuthors : IList<string>;
    FSupplier : string;
    FLicense : string;
    FCopyright : string;
    FHashAlgorithm : string;
    FHashValue : string;
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
    FDownloadUrl : string;
    FPurl : string;
    FTags : IList<string>;
    FProperties : IList<TSBOMProperty>;
    FEvidence : IList<TSBOMEvidence>;
  public
    constructor Create(const kind : TSBOMComponentKind);
    procedure AddProperty(const name, value : string);
    procedure AddEvidence(const location, note : string);

    property Kind : TSBOMComponentKind read FKind;
    property BomRef : string read FBomRef write FBomRef;
    property Id : string read FId write FId;
    property Version : string read FVersion write FVersion;
    property Description : string read FDescription write FDescription;
    property Authors : IList<string> read FAuthors;
    property Supplier : string read FSupplier write FSupplier;
    property License : string read FLicense write FLicense;
    property Copyright : string read FCopyright write FCopyright;
    property HashAlgorithm : string read FHashAlgorithm write FHashAlgorithm;
    property HashValue : string read FHashValue write FHashValue;
    property ProjectUrl : string read FProjectUrl write FProjectUrl;
    property RepositoryUrl : string read FRepositoryUrl write FRepositoryUrl;
    property RepositoryType : string read FRepositoryType write FRepositoryType;
    property RepositoryBranch : string read FRepositoryBranch write FRepositoryBranch;
    property RepositoryCommit : string read FRepositoryCommit write FRepositoryCommit;
    property DownloadUrl : string read FDownloadUrl write FDownloadUrl;
    property Purl : string read FPurl write FPurl;
    property Tags : IList<string> read FTags;
    property Properties : IList<TSBOMProperty> read FProperties;
    property Evidence : IList<TSBOMEvidence> read FEvidence;
  end;

  ///<summary>A dependency edge in the dependency graph: ParentBomRef depends on ChildBomRef.
  /// CycloneDX emits `dependencies[]`; SPDX emits `relationships[]` with `relationshipType: DEPENDS_ON`.</summary>
  TSBOMRelationship = record
    ParentBomRef : string;
    ChildBomRef : string;
  end;

  ///<summary>The whole SBOM for one (project, platform) pair. Owns all components.</summary>
  TSBOMReport = class
  private
    FSerialNumber : string;       // urn:uuid:...
    FTimestampUtc : string;       // ISO-8601 in Z
    FToolName : string;
    FToolVersion : string;
    FProjectName : string;
    FProjectVersion : string;
    FPlatform : TDPMPlatform;
    FCompilerVersion : TCompilerVersion;
    FRootComponent : TSBOMComponent;
    FComponents : IList<TSBOMComponent>;
    FRelationships : IList<TSBOMRelationship>;
    FMetaProperties : IList<TSBOMProperty>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddComponent(const kind : TSBOMComponentKind) : TSBOMComponent;
    procedure AddRelationship(const parentBomRef, childBomRef : string);
    procedure AddMetaProperty(const name, value : string);
    function FindComponentByPurl(const purl : string) : TSBOMComponent;
    function FindComponentById(const id : string) : TSBOMComponent;

    property SerialNumber : string read FSerialNumber write FSerialNumber;
    property TimestampUtc : string read FTimestampUtc write FTimestampUtc;
    property ToolName : string read FToolName write FToolName;
    property ToolVersion : string read FToolVersion write FToolVersion;
    property ProjectName : string read FProjectName write FProjectName;
    property ProjectVersion : string read FProjectVersion write FProjectVersion;
    property Platform : TDPMPlatform read FPlatform write FPlatform;
    property CompilerVersion : TCompilerVersion read FCompilerVersion write FCompilerVersion;
    //the component representing the project itself - parent of every top-level dependency edge
    property RootComponent : TSBOMComponent read FRootComponent;
    property Components : IList<TSBOMComponent> read FComponents;
    property Relationships : IList<TSBOMRelationship> read FRelationships;
    property MetaProperties : IList<TSBOMProperty> read FMetaProperties;
  end;


implementation

uses
  System.SysUtils;

{ TSBOMComponent }

constructor TSBOMComponent.Create(const kind : TSBOMComponentKind);
begin
  inherited Create;
  FKind := kind;
  FAuthors := TCollections.CreateList<string>;
  FTags := TCollections.CreateList<string>;
  FProperties := TCollections.CreateList<TSBOMProperty>;
  FEvidence := TCollections.CreateList<TSBOMEvidence>;
end;

procedure TSBOMComponent.AddProperty(const name, value : string);
var
  p : TSBOMProperty;
begin
  p.Name := name;
  p.Value := value;
  FProperties.Add(p);
end;

procedure TSBOMComponent.AddEvidence(const location, note : string);
var
  e : TSBOMEvidence;
begin
  e.Location := location;
  e.Note := note;
  FEvidence.Add(e);
end;

{ TSBOMReport }

constructor TSBOMReport.Create;
begin
  inherited Create;
  //Spring's owned-object list disposes children on free / list reassignment.
  FComponents := TCollections.CreateObjectList<TSBOMComponent>(true);
  FRelationships := TCollections.CreateList<TSBOMRelationship>;
  FMetaProperties := TCollections.CreateList<TSBOMProperty>;
  FRootComponent := TSBOMComponent.Create(TSBOMComponentKind.Application);
end;

destructor TSBOMReport.Destroy;
begin
  FRootComponent.Free;
  inherited;
end;

function TSBOMReport.AddComponent(const kind : TSBOMComponentKind) : TSBOMComponent;
begin
  result := TSBOMComponent.Create(kind);
  FComponents.Add(result);
end;

procedure TSBOMReport.AddRelationship(const parentBomRef, childBomRef : string);
var
  rel : TSBOMRelationship;
begin
  rel.ParentBomRef := parentBomRef;
  rel.ChildBomRef := childBomRef;
  FRelationships.Add(rel);
end;

procedure TSBOMReport.AddMetaProperty(const name, value : string);
var
  p : TSBOMProperty;
begin
  p.Name := name;
  p.Value := value;
  FMetaProperties.Add(p);
end;

function TSBOMReport.FindComponentByPurl(const purl : string) : TSBOMComponent;
var
  comp : TSBOMComponent;
begin
  result := nil;
  if purl = '' then
    exit;
  for comp in FComponents do
  begin
    if SameText(comp.Purl, purl) then
    begin
      result := comp;
      exit;
    end;
  end;
end;

function TSBOMReport.FindComponentById(const id : string) : TSBOMComponent;
var
  comp : TSBOMComponent;
begin
  result := nil;
  if id = '' then
    exit;
  for comp in FComponents do
  begin
    if SameText(comp.Id, id) then
    begin
      result := comp;
      exit;
    end;
  end;
end;

end.
