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

unit DPM.Core.Spec.BuildEntry;

interface
uses
  JsonDataObjects,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecCopyEntry = class(TSpecNode, ISpecCopyEntry)
  private
    FSource : string;
    FFlatten : boolean;


  protected
    function GetSource : string;
    function GetFlatten : boolean;
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function Clone : ISpecCopyEntry;
  public
    constructor CreateClone(const logger : ILogger; const source : string; const flatten : boolean); reintroduce;
  public
    constructor Create(const logger : ILogger); override;
  end;


  TSpecBuildEntry = class(TSpecNode, ISpecBuildEntry)
  private
    FBplOutputDir : string;
    FConfig : string;
    FLibOutputDir : string;
    FId : string;
    FProject : string;
    FDesignOnly : boolean;
    FBuildForDesign : boolean;
    FCopyFiles : IList<ISpecCopyEntry>;
  protected
    function GetConfig : string;

    function GetBplOutputDir : string;
    function GetLibOutputDir : string;
    function GetId : string;
    function GetProject : string;
    function GetDesignOnly : boolean;
    function GetBuildForDesign : boolean;
    function GetCopyFiles: IList<ISpecCopyEntry>;

    procedure SetProject(const value : string);
    procedure SetLibOutputDir(const value : string);
    procedure SetBplOutputDir(const value : string);
    procedure SetId(const value : string);
    procedure SetDesignOnly(const value : boolean);
    procedure SetBuildForDesign(const value : boolean);

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function Clone : ISpecBuildEntry;
    function ToJSON: string; override;

  public
    constructor CreateClone(const logger : ILogger; const id, project, config, bpldir, libdir : string; const designOnly : boolean; const buildForDesign : boolean; const copyFiles : IList<ISpecCopyEntry>); reintroduce;
  public
    constructor Create(const logger : ILogger); override;

  end;

implementation

uses
  System.SysUtils;

{ TSpecBuildEntry }

function TSpecBuildEntry.Clone : ISpecBuildEntry;
begin
  result := TSpecBuildEntry.CreateClone(logger, FId, FProject, FConfig, FBplOutputDir, FLibOutputDir, FDesignOnly, FBuildForDesign, FCopyFiles);
end;

constructor TSpecBuildEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FCopyFiles := TCollections.CreateList<ISpecCopyEntry>;
end;

constructor TSpecBuildEntry.CreateClone(const logger : ILogger; const id, project, config, bpldir, libdir : string; const designOnly : boolean; const buildForDesign : boolean; const copyFiles : IList<ISpecCopyEntry>);
var
  copyEntry : ISpecCopyEntry;
begin
  inherited Create(logger);
  FId := id;
  FProject := project;
  FConfig := config;
  FBplOutputDir := bpldir;
  FLibOutputDir := libdir;
  FDesignOnly   := designOnly;
  FBuildForDesign := buildForDesign;
  FCopyFiles := TCollections.CreateList<ISpecCopyEntry>;
  for copyEntry in copyFiles do
    FCopyFiles.Add(copyEntry.Clone);
end;

function TSpecBuildEntry.GetLibOutputDir: string;
begin
  result := FLibOutputDir;
end;

function TSpecBuildEntry.GetBplOutputDir : string;
begin
  result := FBplOutputDir;
end;

function TSpecBuildEntry.GetBuildForDesign: boolean;
begin
  result := FBuildForDesign;
end;

function TSpecBuildEntry.GetConfig : string;
begin
  result := FConfig;
end;


function TSpecBuildEntry.GetCopyFiles: IList<ISpecCopyEntry>;
begin
  result := FCopyFiles;
end;

function TSpecBuildEntry.GetDesignOnly: boolean;
begin
  result := FDesignOnly;
end;

function TSpecBuildEntry.GetId : string;
begin
  result := FId;
end;



function TSpecBuildEntry.GetProject : string;
begin
  result := FProject;
end;

function TSpecBuildEntry.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  copyFilesArray : TJsonArray;
begin
  result := true;
  FId := jsonObject.S['id'];
  if FId = '' then
  begin
    Logger.Error('Build Entry is missing required [id] property.');
    result := false;
  end;

  FProject := jsonObject.S['project'];
  if FProject = '' then
  begin
    Logger.Error('Build Entry is missing required [project] property.');
    result := false;
  end;

  FConfig := jsonObject.S['config'];
  if FConfig = '' then
    FConfig := 'Release';

  FBplOutputDir := jsonObject.S['bplOutputDir'];
  if FBplOutputDir = '' then
    FBplOutputDir := 'bin';

  FLibOutputDir := jsonObject.S['libOutputDir'];
  if FLibOutputDir = '' then
    FLibOutputDir := 'lib';

  FDesignOnly     := jsonObject.B['designOnly'];
  FBuildForDesign := jsonObject.B['buildForDesign'];

  if jsonObject.Contains('copyFiles') then
  begin
    copyFilesArray :=   jsonObject.A['copyFiles'];
    LoadJsonCollection(copyFilesArray, TSpecCopyEntry,
    procedure(const value : IInterface)
    begin
      FCopyFiles.Add(value as ISpecCopyEntry);
    end);
  end;

end;

procedure TSpecBuildEntry.SetLibOutputDir(const value: string);
begin
  FLibOutputDir := value;
end;

procedure TSpecBuildEntry.SetBplOutputDir(const value : string);
begin
  FBplOutputDir := value;
end;

procedure TSpecBuildEntry.SetBuildForDesign(const value: boolean);
begin
  FBuildForDesign := value;
end;


procedure TSpecBuildEntry.SetDesignOnly(const value: boolean);
begin
  FDesignOnly := value;
end;

procedure TSpecBuildEntry.SetId(const value : string);
begin
  FId := value;
end;

procedure TSpecBuildEntry.SetProject(const value : string);
begin
  FProject := value;
end;

function TSpecBuildEntry.ToJSON: string;
var
  json : TJSONObject;
begin
  json := TJSONObject.Create;
  try
    json.S['id'] := FId;
    json.S['project'] := FProject;
    if FDesignOnly then
      json.B['designOnly'] := FDesignOnly;
    json.S['config'] := FConfig;

    if FBuildForDesign then
      json.B['buildForDesign'] := FBuildForDesign;

    Result := json.ToJSON;
  finally
    FreeAndNil(json);
  end;
end;

{ TSpecCopyEntry }

function TSpecCopyEntry.Clone: ISpecCopyEntry;
begin
  result := TSpecCopyEntry.CreateClone(Logger, FSource, FFlatten)
end;

constructor TSpecCopyEntry.Create(const logger: ILogger);
begin
  inherited Create(logger);

end;

constructor TSpecCopyEntry.CreateClone(const logger: ILogger;  const source: string; const flatten: boolean);
begin
  inherited Create(logger);
  FSource := source;
  FFlatten := flatten;
end;

function TSpecCopyEntry.GetFlatten: boolean;
begin
  result := FFlatten;
end;

function TSpecCopyEntry.GetSource: string;
begin
  result := FSource;
end;

function TSpecCopyEntry.LoadFromJson(const jsonObject: TJsonObject): Boolean;
begin
  result := true;
  FSource := jsonObject.S['src'];
  if FSource = '' then
  begin
    Logger.Error('Copy Entry is missing required [src] property.');
    result := false;
  end;
  FFlatten := jsonObject.B['flatten'];
end;

end.

