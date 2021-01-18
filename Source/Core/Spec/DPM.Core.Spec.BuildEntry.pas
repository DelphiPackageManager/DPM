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
  TSpecBuildEntry = class(TSpecNode, ISpecBuildEntry)
  private
    FLogger : ILogger;
    FBplOutputDir : string;
    FConfig : string;
    FLibOutputDir : string;
    FId : string;
    FProject : string;
    FDesignOnly : boolean;
    FBuildForDesign : boolean;
  protected
    function GetConfig : string;

    function GetBplOutputDir : string;
    function GetLibOutputDir : string;
    function GetId : string;
    function GetProject : string;
    function GetDesignOnly : boolean;
    function GetBuildForDesign : boolean;
    procedure SetProject(const value : string);

    procedure SetLibOutputDir(const value : string);
    procedure SetBplOutputDir(const value : string);
    procedure SetId(const value : string);
    procedure SetDesignOnly(const value : boolean);
    procedure SetBuildForDesign(const value : boolean);
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function Clone : ISpecBuildEntry;

  public
    constructor CreateClone(const logger : ILogger; const id, project, config, bpldir, libdir : string; const designOnly : boolean; const buildForDesign : boolean); reintroduce;
  public
    constructor Create(const logger : ILogger); override;

  end;

implementation

{ TSpecBuildEntry }

function TSpecBuildEntry.Clone : ISpecBuildEntry;
begin
  result := TSpecBuildEntry.CreateClone(logger, FId, FProject, FConfig, FBplOutputDir, FLibOutputDir, FDesignOnly, FBuildForDesign);
end;

constructor TSpecBuildEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

constructor TSpecBuildEntry.CreateClone(const logger : ILogger; const id, project, config, bpldir, libdir : string; const designOnly : boolean; const buildForDesign : boolean);
begin
  inherited Create(logger);
  FId := id;
  FProject := project;
  FConfig := config;
  FBplOutputDir := bpldir;
  FLibOutputDir := libdir;
  FDesignOnly   := designOnly;
  FBuildForDesign := buildForDesign;
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
begin
  result := true;
  FId := jsonObject.S['id'];
  if FId = '' then
  begin
    FLogger.Error('Build Entry is missing required [id] property.');
    result := false;
  end;

  FProject := jsonObject.S['project'];
  if FProject = '' then
  begin
    FLogger.Error('Build Entry is missing required [project] property.');
    result := false;
  end;

  FConfig := jsonObject.S['config'];
  if FConfig = '' then
    FConfig := 'release';

  FBplOutputDir := jsonObject.S['bplOutputDir'];
  if FBplOutputDir = '' then
    FBplOutputDir := 'bin';

  FLibOutputDir := jsonObject.S['libOutputDir'];
  if FLibOutputDir = '' then
    FLibOutputDir := 'lib';

  FDesignOnly     := jsonObject.B['designOnly'];
  FBuildForDesign := jsonObject.B['buildForDesign'];

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

end.

