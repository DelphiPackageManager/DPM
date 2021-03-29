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

unit DPM.Core.Spec.TemplateBase;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.TargetPlatform,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecTemplateBase = class(TSpecNode, ISpecTemplateBase)
  protected
    FDependencies : IList<ISpecDependency>;
    FDesignFiles : IList<ISpecBPLEntry>;
    FFiles : IList<ISpecFileEntry>;
    FLibFiles : IList<ISpecFileEntry>;
    FRuntimeFiles : IList<ISpecBPLEntry>;
    FSourceFiles : IList<ISpecFileEntry>;
    FSearchPaths : IList<ISpecSearchPath>;
    FBuildEntries : IList<ISpecBuildEntry>;
  protected
    function IsTemplate : boolean; virtual;
    function AllowDependencyGroups : boolean; virtual;
    function AllowSearchPathGroups : boolean; virtual;
    function GetDependencies : IList<ISpecDependency>;
    function GetDesignFiles : IList<ISpecBPLEntry>;
    function GetFiles : IList<ISpecFileEntry>;
    function GetLibFiles : IList<ISpecFileEntry>;
    function GetRuntimeFiles : IList<ISpecBPLEntry>;
    function GetSourceFiles : IList<ISpecFileEntry>;
    function GetSearchPaths : IList<ISpecSearchPath>;
    function GetBuildEntries : IList<ISpecBuildEntry>;

    function FindDependencyById(const id : string) : ISpecDependency;
    function FindDependencyGroupByTargetPlatform(const targetPlatform : TTargetPlatform) : ISpecDependencyGroup;
    function FindSearchPathByPath(const path : string) : ISpecSearchPath;
    function FindRuntimeBplBySrc(const src : string) : ISpecBPLEntry;
    function FindDesignBplBySrc(const src : string) : ISpecBPLEntry;
    function FindLibFileBySrc(const src : string) : ISpecFileEntry;
    function FindSourceFileBySrc(const src : string) : ISpecFileEntry;
    function FindOtherFileBySrc(const src : string) : ISpecFileEntry;
    function FindBuildEntryById(const id : string) : ISpecBuildEntry;


    //    function LoadCollection(const rootElement : IXMLDOMElement; const collectionPath : string; const nodeClass : TSpecNodeClass; const action : TConstProc<ISpecNode>) : boolean;

    function LoadDependenciesFromJson(const dependenciesArray : TJsonArray) : Boolean;
    function LoadLibraryFromJson(const libArray : TJsonArray) : Boolean;
    function LoadSourceFromJson(const sourceArray : TJsonArray) : Boolean;
    function LoadRuntimeFromJson(const runtimeArray : TJsonArray) : Boolean;
    function LoadDesignFromJson(const designArray : TJsonArray) : Boolean;
    function LoadFilesFromJson(const filesArray : TJsonArray) : Boolean;
    function LoadSearchPathsFromJson(const searchPathsArray : TJsonArray) : Boolean;
    function LoadBuildEntriesFromJson(const buildArray : TJsonArray) : Boolean;

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;


    constructor CreateClone(const logger : ILogger; const deps : IList<ISpecDependency>; const design, runtime : IList<ISpecBPLEntry>;
      const source, lib, files : IList<ISpecFileEntry>; const search : IList<ISpecSearchPath>);
  public
    constructor Create(const logger : ILogger); override;


  end;



implementation

uses
  System.SysUtils,
  DPM.Core.Spec.FileEntry,
  DPM.Core.Spec.BPLEntry,
  DPM.Core.Spec.Dependency,
  DPM.Core.Spec.DependencyGroup,
  DPM.Core.Spec.SearchPath,
  DPM.Core.Spec.SearchPathGroup, DPM.Core.Spec.BuildEntry;


{ TSpecTemplateBase }

function TSpecTemplateBase.AllowDependencyGroups : boolean;
begin
  result := false;
end;

function TSpecTemplateBase.AllowSearchPathGroups : boolean;
begin
  result := false;
end;

constructor TSpecTemplateBase.Create(const logger : ILogger);
begin
  inherited Create(Logger);
  FDependencies := TCollections.CreateList<ISpecDependency>;
  FDesignFiles := TCollections.CreateList<ISpecBPLEntry>;
  FFiles := TCollections.CreateList<ISpecFileEntry>;
  FLibFiles := TCollections.CreateList<ISpecFileEntry>;
  FRuntimeFiles := TCollections.CreateList<ISpecBPLEntry>;
  FSourceFiles := TCollections.CreateList<ISpecFileEntry>;
  FSearchPaths := TCollections.CreateList<ISpecSearchPath>;
  FBuildEntries := TCollections.CreateList<ISpecBuildEntry>;
end;

constructor TSpecTemplateBase.CreateClone(const logger : ILogger; const deps : IList<ISpecDependency>; const design, runtime : IList<ISpecBPLEntry>; const source, lib, files : IList<ISpecFileEntry>;
                                          const search : IList<ISpecSearchPath>);
begin
  Create(logger);
  FDependencies.AddRange(deps);
  FDesignFiles.AddRange(design);
  FFiles.AddRange(files);
  FLibFiles.AddRange(lib);
  FRuntimeFiles.AddRange(runtime);
  FSourceFiles.AddRange(source);
  FSearchPaths.AddRange(search);

end;

function TSpecTemplateBase.FindBuildEntryById(const id : string) : ISpecBuildEntry;
begin
  result := FBuildEntries.FirstOrDefault(
    function(const item : ISpecBuildEntry) : boolean
    begin
      result := SameText(item.Id, id);
    end);
end;

function TSpecTemplateBase.FindDependencyById(const id : string) : ISpecDependency;
begin
  result := FDependencies.FirstOrDefault(
    function(const item : ISpecDependency) : boolean
    begin
      result := SameText(item.Id, id);
    end);
end;

function TSpecTemplateBase.FindDependencyGroupByTargetPlatform(const targetPlatform : TTargetPlatform) : ISpecDependencyGroup;
begin
  result := FDependencies.FirstOrDefault(
    function(const item : ISpecDependency) : boolean
    begin
      result := false;
      if item.IsGroup then
        result := (item as ISpecDependencyGroup).TargetPlatform = targetPlatform;
    end) as ISpecDependencyGroup;

end;

function TSpecTemplateBase.FindDesignBplBySrc(const src : string) : ISpecBPLEntry;
begin
  result := FDesignFiles.FirstOrDefault(
    function(const item : ISpecBPLEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);
end;

function TSpecTemplateBase.FindLibFileBySrc(const src : string) : ISpecFileEntry;
begin
  result := FLibFiles.FirstOrDefault(
    function(const item : ISpecFileEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);

end;

function TSpecTemplateBase.FindOtherFileBySrc(const src : string) : ISpecFileEntry;
begin
  result := FFiles.FirstOrDefault(
    function(const item : ISpecFileEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);

end;

function TSpecTemplateBase.FindRuntimeBplBySrc(const src : string) : ISpecBPLEntry;
begin
  result := FRuntimeFiles.FirstOrDefault(
    function(const item : ISpecBPLEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);

end;

function TSpecTemplateBase.FindSearchPathByPath(const path : string) : ISpecSearchPath;
begin
  result := FSearchPaths.FirstOrDefault(
    function(const item : ISpecSearchPath) : boolean
    begin
      result := SameText(item.Path, path);
    end);

end;

function TSpecTemplateBase.FindSourceFileBySrc(const src : string) : ISpecFileEntry;
begin
  result := FSourceFiles.FirstOrDefault(
    function(const item : ISpecFileEntry) : boolean
    begin
      result := SameText(item.Source, src);
    end);
end;

function TSpecTemplateBase.GetBuildEntries : IList<ISpecBuildEntry>;
begin
  result := FBuildEntries;
end;

function TSpecTemplateBase.GetDependencies : IList<ISpecDependency>;
begin
  result := FDependencies;
end;

function TSpecTemplateBase.GetDesignFiles : IList<ISpecBPLEntry>;
begin
  result := FDesignFiles;
end;

function TSpecTemplateBase.GetFiles : IList<ISpecFileEntry>;
begin
  result := FFiles;
end;

function TSpecTemplateBase.GetLibFiles : IList<ISpecFileEntry>;
begin
  result := FLibFiles;
end;

function TSpecTemplateBase.GetRuntimeFiles : IList<ISpecBPLEntry>;
begin
  result := FRuntimeFiles;
end;

function TSpecTemplateBase.GetSearchPaths : IList<ISpecSearchPath>;
begin
  result := FSearchPaths;
end;

function TSpecTemplateBase.GetSourceFiles : IList<ISpecFileEntry>;
begin
  result := FSourceFiles;
end;


function TSpecTemplateBase.IsTemplate : boolean;
begin
  result := false;
end;


function TSpecTemplateBase.LoadBuildEntriesFromJson(const buildArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(buildArray, TSpecBuildEntry,
    procedure(const value : IInterface)
    begin
      FBuildEntries.Add(value as ISpecBuildEntry);
    end);
end;

function TSpecTemplateBase.LoadDependenciesFromJson(const dependenciesArray : TJsonArray) : Boolean;
var
  i : integer;
  dependencyObj : TJsonObject;
  dependency : ISpecDependency;
begin
  result := true;
  if dependenciesArray.Count = 0 then
    exit;
  for i := 0 to dependenciesArray.Count - 1 do
  begin
    dependencyObj := dependenciesArray[i];
    if dependencyObj.Contains('targetPlatform') then
    begin
      if not IsTemplate then
      begin
        result := false;
        Logger.Error('Dependency groups not valid in targetPlatform, for use in templates only');
        continue;
      end;

      dependency := TSpecDependencyGroup.Create(Logger);
    end
    else
      dependency := TSpecDependency.Create(Logger);
    FDependencies.Add(dependency);
    result := dependency.LoadFromJson(dependencyObj) and result;
  end;
end;

function TSpecTemplateBase.LoadDesignFromJson(const designArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(designArray, TSpecBPLEntry,
    procedure(const value : IInterface)
    begin
      FDesignFiles.Add(value as ISpecBPLEntry);
    end);

end;


function TSpecTemplateBase.LoadFilesFromJson(const filesArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(filesArray, TSpecFileEntry,
    procedure(const value : IInterface)
    begin
      FFiles.Add(value as ISpecFileEntry);
    end);

end;


function TSpecTemplateBase.LoadLibraryFromJson(const libArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(libArray, TSpecFileEntry,
    procedure(const value : IInterface)
    begin
      FLibFiles.Add(value as ISpecFileEntry);
    end);

end;

function TSpecTemplateBase.LoadRuntimeFromJson(const runtimeArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(runtimeArray, TSpecBPLEntry,
    procedure(const value : IInterface)
    begin
      FRuntimeFiles.Add(value as ISpecBPLEntry);
    end);
end;


function TSpecTemplateBase.LoadSearchPathsFromJson(const searchPathsArray : TJsonArray) : Boolean;
var
  i : integer;
  searchPath : ISpecSearchPath;
  searchPathGroup : ISpecSearchPathGroup;
  searchPathObj : TJsonObject;
begin
  result := true;
  if searchPathsArray.Count = 0 then
    exit;

  for i := 0 to searchPathsArray.Count - 1 do
  begin
    searchPathObj := searchPathsArray.O[i];
    //detect if it's a group or not.
    if searchPathObj.Contains('targetPlatform') then
    begin
      if not IsTemplate then
      begin
        result := false;
        Logger.Error('searchPath groups not valid in targetPlatform, for use in templates only');
        continue;
      end;
      searchPath := TSpecSearchPathGroup.Create(Logger)
    end
    else
      searchPath := TSpecSearchPath.Create(Logger);

    FSearchPaths.Add(searchPath);
    result := searchPath.LoadFromJson(searchPathObj) and result;
    if searchPath.IsGroup then
    begin
      searchPathGroup := searchPath as ISpecSearchPathGroup;
      if not searchPathGroup.SearchPaths.Any then
      begin
        result := false;
        Logger.Error('Empty searchPath/group not allowed [' + searchPathGroup.TargetPlatform.ToString + ']');
      end;
    end;
  end;

  if not FSearchPaths.Any then
  begin
    Logger.Error('No searchPaths were specified');
    result := false;
  end;
end;


function TSpecTemplateBase.LoadSourceFromJson(const sourceArray : TJsonArray) : Boolean;
begin
  result := LoadJsonCollection(sourceArray, TSpecFileEntry,
    procedure(const value : IInterface)
    begin
      FSourceFiles.Add(value as ISpecFileEntry);
    end);
end;


function TSpecTemplateBase.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  hasChildren : boolean;
  sValue : string;
  collectionObj : TJsonArray;
begin
  result := true;
  if IsTemplate then
  begin
    if jsonObject.Contains('template') then
    begin
      result := false;
      Logger.Error('template property not valid in a template!');
      exit;
    end;
    hasChildren := jsonObject.Count > 1; //name +
    if not hasChildren then
    begin
      if jsonObject.Contains('name') then
        sValue := jsonObject.S['name']
      else
        sValue := 'unamed';
      result := false;
      Logger.Error('Empty template [' + sValue + ']');
      exit;
    end;
  end
  else
  begin
    //if we point to a template and have props other than compiler + platforms then fail
    if jsonObject.Contains('template') then
    begin
      if jsonObject.Count > 4 then //compiler + platforms + template + variables
      begin
        Logger.Error('targetPlatform cannot specify a template and it''s own definition, pick one');
        result := false;
      end;
      // if it's 3 props, then variables must be one of them
      if not jsonObject.Contains('variables') then
      begin
        Logger.Error('targetPlatform cannot specify a template and it''s own definition, pick one');
        result := false;
      end;
      exit;
    end;
  end;

  collectionObj := jsonObject.A['dependencies'];
  if collectionObj.Count > 0 then
    result := LoadDependenciesFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['lib'];
  if collectionObj.Count > 0 then
    result := LoadLibraryFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['source'];
  if collectionObj.Count > 0 then
    result := LoadSourceFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['runtime'];
  if collectionObj.Count > 0 then
    result := LoadRuntimeFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['design'];
  if collectionObj.Count > 0 then
    result := LoadDesignFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['files'];
  if collectionObj.Count > 0 then
    result := LoadFilesFromJson(collectionObj) and result;

  collectionObj := jsonObject.A['searchPaths'];
  if collectionObj.Count > 0 then
    result := LoadSearchPathsFromJson(collectionObj) and result
  else
  begin
    result := false;
    Logger.Error('Required field [searchPaths] not found');
  end;

  collectionObj := jsonObject.A['build'];
  if collectionObj.Count > 0 then
    result := LoadBuildEntriesFromJson(collectionObj) and result;


end;



end.

