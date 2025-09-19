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

unit DPM.Core.Spec.Template;

interface

uses
  System.Classes,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.TargetPlatform,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecTemplate = class(TSpecNode, ISpecTemplate)
  protected
    FName : string;
    FDependencies : IList<ISpecDependency>;
    FSourceFiles : IList<ISpecSourceEntry>;
    FBuildEntries : IList<ISpecBuildEntry>;
    FDesignFiles : IList<ISpecDesignEntry>;

    FSourceComments : TStringList;
    FDependenciesComments : TStringList;
    FBuildComments : TStringList;
    FDesignComments : TStringList;
    FPackageDefComments : TStringList;
  protected
    function IsTemplate : boolean; virtual;
    function GetDependencies : IList<ISpecDependency>;
    function GetDesignFiles : IList<ISpecDesignEntry>;
    function GetSourceFiles : IList<ISpecSourceEntry>;
    function GetBuildEntries : IList<ISpecBuildEntry>;
    function GetName : string;
    procedure SetName(const templateName: string);

    function GetSourceComments : TStrings;
    function GetDependenciesComments : TStrings;
    function GetBuildComments : TStrings;
    function GetDesignComments : TStrings;
    function GetPackageDefComments : TStrings;


    function NewDependency(const id : string) : ISpecDependency;
    function NewSource(const src: string): ISpecSourceEntry;
    function NewDesignEntry(const project : string) : ISpecDesignEntry;
    function NewBuildEntry(const project : string) : ISpecBuildEntry;

    procedure DeleteDependency(const id : string);
    procedure DeleteSource(const src: string);
    procedure DeleteBuildEntry(const project : string);
    procedure DeleteDesignEntry(const project : string);

    function FindDependency(const id : string) : ISpecDependency;
    function FindSourceEntry(const src : string) : ISpecSourceEntry;
    function FindBuildEntry(const project : string) : ISpecBuildEntry;
    function FindDesignEntry(const project : string) : ISpecDesignEntry;

    function LoadDependenciesFromYAML(const dependenciesArray : IYAMLSequence) : Boolean;
    function LoadSourceFromYAML(const sourceArray : IYAMLSequence) : Boolean;
    function LoadBuildEntriesFromYAML(const buildArray : IYAMLSequence) : Boolean;
    function LoadDesignFromYAML(const designArray : IYAMLSequence) : Boolean;


    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;


    function Clone : ISpecTemplate;

    constructor CreateClone(const logger : ILogger; const source : ISpecTemplate);

    property SourceEntries : IList<ISpecSourceEntry>read GetSourceFiles;
    property DesignFiles : IList<ISpecDesignEntry> read GetDesignFiles;
    property Dependencies : IList<ISpecDependency>read GetDependencies;
    property BuildEntries : IList<ISpecBuildEntry>read GetBuildEntries;

  public
    constructor Create(const logger : ILogger); override;
    destructor Destroy;override;
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Spec.SourceEntry,
  DPM.Core.Spec.DesignEntry,
  DPM.Core.Spec.Dependency,
  DPM.Core.Spec.BuildEntry;


{ TSpecTemplate }


function TSpecTemplate.Clone: ISpecTemplate;
begin
  result := TSpecTemplate.CreateClone(Logger, Self);
end;

constructor TSpecTemplate.Create(const logger : ILogger);
begin
  inherited Create(Logger);
  FDependencies := TCollections.CreateList<ISpecDependency>;
  FSourceFiles := TCollections.CreateList<ISpecSourceEntry>;
  FBuildEntries := TCollections.CreateList<ISpecBuildEntry>;
  FDesignFiles := TCollections.CreateList<ISpecDesignEntry>;
end;

constructor TSpecTemplate.CreateClone(const logger : ILogger; const source : ISpecTemplate);
var
  newDependency : ISpecDependency;
  newSourceEntry : ISpecSourceEntry;
  newBuildEntry : ISpecBuildEntry;
  newDesignEntry : ISpecDesignEntry;
  i : integer;
begin
  Create(logger);
  FName := source.Name;
  for i := 0 to source.Dependencies.Count -1 do
  begin
    newDependency := source.Dependencies[i].Clone;
    FDependencies.Add(newDependency);
  end;

  for i := 0 to source.SourceEntries.Count -1 do
  begin
    newSourceEntry := source.SourceEntries[i].Clone;
    FSourceFiles.Add(newSourceEntry);
  end;

  for i := 0 to source.BuildEntries.Count -1 do
  begin
    newBuildEntry := source.BuildEntries[i].Clone;
    FBuildEntries.Add(newBuildEntry);
  end;

  for i := 0 to source.DesignEntries.Count -1 do
  begin
    newDesignEntry := source.DesignEntries[i].Clone;
    FDesignFiles.Add(newDesignEntry);
  end;


  //TODO : Add packagedef cloning here!
end;

procedure TSpecTemplate.DeleteBuildEntry(const project: string);
var
  build : ISpecBuildEntry;
begin
  build := FindBuildEntry(project);
  if build <> nil then
    FBuildEntries.Remove(build);
end;

procedure TSpecTemplate.DeleteDependency(const id: string);
var
  dependency : ISpecDependency;
begin
  dependency := Self.FindDependency(id);
  if dependency <> nil then
    FDependencies.Remove(dependency);
end;

procedure TSpecTemplate.DeleteDesignEntry(const project: string);
var
  design : ISpecDesignEntry;
begin
  design := FindDesignEntry(project);
  if design <> nil then
    FDesignFiles.Remove(design);
end;


procedure TSpecTemplate.DeleteSource(const src: string);
var
  source : ISpecSourceEntry;
begin
  source := FindSourceEntry(src);
  if source <> nil then
    FSourceFiles.Remove(source);
end;

destructor TSpecTemplate.Destroy;
begin
  FDependencies := nil;
  FDesignFiles := nil;
  FSourceFiles := nil;
  FBuildEntries := nil;
  inherited;
end;

function TSpecTemplate.NewSource(const src: string): ISpecSourceEntry;
var
  sourceFilesEntry : ISpecSourceEntry;
begin
  sourceFilesEntry := TSpecSourceEntry.Create(Logger);
  sourceFilesEntry.Source := src;
  FSourceFiles.Add(sourceFilesEntry);
  Result := sourceFilesEntry;
end;


procedure TSpecTemplate.SetName(const templateName: string);
begin
  FName := templateName;
end;


procedure TSpecTemplate.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  template : IYAMLMapping;
  i : integer;
  seq : IYAMLSequence;
begin
  template := parent.AsSequence.AddMapping;
  template.S['name'] := FName;
  if FDependencies.Any then
  begin
    seq := template.A['dependencies'];
    for i := 0 to FDependencies.Count -1 do
      FDependencies[i].ToYAML(seq, packageKind);
  end;

  if FSourceFiles.Any then
  begin
    seq := template.A['source'];
    for i := 0 to FSourceFiles.Count -1 do
      FSourceFiles[i].ToYAML(seq, packageKind);
  end;
  if FBuildEntries.Any then
  begin
    seq := template.A['build'];
    for i := 0 to FBuildEntries.Count -1 do
      FBuildEntries[i].ToYAML(seq, packageKind);
  end;
  if FDesignFiles.Any then
  begin
    seq := template.A['design'];
    for i := 0 to FDesignFiles.Count -1 do
      FDesignFiles[i].ToYAML(seq, packageKind);
  end;

  //TODO : package definitions

end;

function TSpecTemplate.NewDependency(const id: string): ISpecDependency;
var
  dependency : ISpecDependency;
begin
  dependency := TSpecDependency.Create(Logger);
  dependency.Id := id;
  FDependencies.Add(dependency);
  Result := dependency;
end;

function TSpecTemplate.NewDesignEntry(const project : string) : ISpecDesignEntry;
var
  designFilesEntry : ISpecDesignEntry;
begin
  designFilesEntry := TSpecDesignEntry.Create(Logger);
  designFilesEntry.Project := project;
  FDesignFiles.Add(designFilesEntry);
  Result := designFilesEntry;
end;

function TSpecTemplate.NewBuildEntry(const project : string) : ISpecBuildEntry;
var
  buildEntry : ISpecBuildEntry;
begin
  buildEntry := TSpecBuildEntry.Create(Logger);
  buildEntry.Project := project;
  FBuildEntries.Add(buildEntry);
  Result := buildEntry;
end;


function TSpecTemplate.FindBuildEntry(const project : string) : ISpecBuildEntry;
begin
  result := FBuildEntries.FirstOrDefault(
    function(const item : ISpecBuildEntry) : boolean
    begin
      result := SameText(item.Project, project);
    end);
end;

function TSpecTemplate.FindDependency(const id : string) : ISpecDependency;
begin
  result := FDependencies.FirstOrDefault(
    function(const item : ISpecDependency) : boolean
    begin
      result := SameText(item.Id, id);
    end);
end;

function TSpecTemplate.FindDesignEntry(const project : string) : ISpecDesignEntry;
begin
  result := FDesignFiles.FirstOrDefault(
    function(const item : ISpecDesignEntry) : boolean
    begin
      result := SameText(item.Project, project);
    end);
end;

function TSpecTemplate.FindSourceEntry(const src : string) : ISpecSourceEntry;
begin
  result := FSourceFiles.FirstOrDefault(
    function(const item : ISpecSourceEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);
end;

function TSpecTemplate.GetBuildComments: TStrings;
begin
  if FBuildComments = nil then
    FBuildComments := TStringList.Create;
  result := FBuildComments;
end;

function TSpecTemplate.GetBuildEntries : IList<ISpecBuildEntry>;
begin
  result := FBuildEntries;
end;

function TSpecTemplate.GetDependencies : IList<ISpecDependency>;
begin
  result := FDependencies;
end;


function TSpecTemplate.GetDependenciesComments: TStrings;
begin
  if FDependenciesComments = nil then
    FDependenciesComments := TStringList.Create;
  result := FDependenciesComments;
end;

function TSpecTemplate.GetDesignComments: TStrings;
begin
  if FDesignComments = nil then
    FDesignComments := TStringList.Create;
  result := FDesignComments;
end;

function TSpecTemplate.GetDesignFiles : IList<ISpecDesignEntry>;
begin
  result := FDesignFiles;
end;

function TSpecTemplate.GetName: string;
begin
  result := FName;
end;


function TSpecTemplate.GetPackageDefComments: TStrings;
begin
  if FPackageDefComments = nil then
    FPackageDefComments := TStringList.Create;
  result := FPackageDefComments;
end;

function TSpecTemplate.GetSourceComments: TStrings;
begin
  if FSourceComments = nil then
    FSourceComments := TStringList.Create;
  result := FSourceComments;
end;

function TSpecTemplate.GetSourceFiles : IList<ISpecSourceEntry>;
begin
  result := FSourceFiles;
end;


function TSpecTemplate.IsTemplate : boolean;
begin
  result := false;
end;



function TSpecTemplate.LoadBuildEntriesFromYAML(const buildArray: IYAMLSequence): Boolean;
begin
  result := LoadYAMLCollection(buildArray, TSpecBuildEntry,
    procedure(const value : IInterface)
    begin
      FBuildEntries.Add(value as ISpecBuildEntry);
    end);
end;


function TSpecTemplate.LoadDependenciesFromYAML(const dependenciesArray: IYAMLSequence): Boolean;
var
  i : integer;
  dependencyObj : IYAMLMapping;
  dependency : ISpecDependency;
begin
  result := true;
  if dependenciesArray.Count = 0 then
    exit;
  for i := 0 to dependenciesArray.Count - 1 do
  begin
    dependencyObj := dependenciesArray.O[i];
    dependency := TSpecDependency.Create(Logger);
    FDependencies.Add(dependency);
    result := dependency.LoadFromYAML(dependencyObj) and result;
  end;

end;



function TSpecTemplate.LoadDesignFromYAML(const designArray: IYAMLSequence): Boolean;
begin
  result := LoadYAMLCollection(designArray, TSpecDesignEntry,
    procedure(const value : IInterface)
    begin
      FDesignFiles.Add(value as ISpecDesignEntry);
    end);
end;

function TSpecTemplate.LoadSourceFromYAML(const sourceArray: IYAMLSequence): Boolean;
begin
  result := LoadYAMLCollection(sourceArray, TSpecSourceEntry,
    procedure(const value : IInterface)
    begin
      FSourceFiles.Add(value as ISpecSourceEntry);
    end);
end;

function TSpecTemplate.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  collectionObj : IYAMLSequence;
begin
  result := true;
  LoadComments(yamlObject);

  FName := yamlObject.S['name'];
  Logger.Debug('[template] name : ' + FName);

  collectionObj := yamlObject.A['dependencies'];
  if collectionObj.Count > 0 then
    result := LoadDependenciesFromYAML(collectionObj) and result;


  collectionObj := yamlObject.A['source'];
  if collectionObj.Count > 0 then
    result := LoadSourceFromYAML(collectionObj) and result;

  collectionObj := yamlObject.A['build'];
  if collectionObj.Count > 0 then
    result := LoadBuildEntriesFromYAML(collectionObj) and result;

  collectionObj := yamlObject.A['design'];
  if collectionObj.Count > 0 then
    result := LoadDesignFromYAML(collectionObj) and result;


end;

end.

