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

unit DPM.Core.MCP.Tools.Project;

interface

uses
  VSoft.CancellationToken,
  VSoft.YAML,
  DPM.Core.Project.Interfaces,
  DPM.Core.MCP.Interfaces,
  DPM.Core.MCP.Tools.Common;

type
  TMCPProjectToolBase = class(TMCPToolBase)
  protected
    FContext : IMCPToolContext;
  public
    constructor Create(const context : IMCPToolContext);
  end;

  TMCPProjectInfoTool = class(TMCPProjectToolBase)
  protected
    function GetName : string; override;
    function GetTitle : string; override;
    function GetDescription : string; override;
    procedure BuildInputSchema(const target : IYAMLMapping); override;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; override;
  end;

  TMCPWhyPackageTool = class(TMCPProjectToolBase)
  protected
    function GetName : string; override;
    function GetTitle : string; override;
    function GetDescription : string; override;
    procedure BuildInputSchema(const target : IYAMLMapping); override;
    function Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string; override;
  end;

implementation

uses
  System.SysUtils,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections;

const
  cProjectPathArgDescription =
    'Path to a Delphi project (.dproj). May be absolute or relative to the working directory. ' +
    'Omit to use the project this server was started with, or the single .dproj in the ' +
    'working directory.';

{ TMCPProjectToolBase }

constructor TMCPProjectToolBase.Create(const context : IMCPToolContext);
begin
  inherited Create;
  FContext := context;
end;

{ TMCPProjectInfoTool }

function TMCPProjectInfoTool.GetName : string;
begin
  result := 'dpm_project_info';
end;

function TMCPProjectInfoTool.GetTitle : string;
begin
  result := 'Delphi project info';
end;

function TMCPProjectInfoTool.GetDescription : string;
begin
  result :=
    'Read a Delphi project file (.dproj) and report which Delphi compiler version it targets, ' +
    'which platforms it builds for, whether DPM is enabled for it, and its complete package ' +
    'dependency tree. Each package in the tree is marked as top level (added to the project ' +
    'deliberately) or transitive (pulled in by another package), with the version range each ' +
    'reference resolved against. ' +
    'Call this FIRST whenever you need to understand or change a Delphi project''s ' +
    'dependencies - it tells you the compiler version the other tools should use, which is ' +
    'otherwise easy to get wrong. Read only: the project file is never modified.';
end;

procedure TMCPProjectInfoTool.BuildInputSchema(const target : IYAMLMapping);
var
  properties : IYAMLMapping;
begin
  properties := BeginSchema(target);
  AddStringProperty(properties, 'projectPath', cProjectPathArgDescription);
end;

function TMCPProjectInfoTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
var
  projectFile : string;
  editor : IProjectEditor;
  doc : IYAMLDocument;
begin
  projectFile := RequireProjectFile(arguments, FContext);
  editor := LoadProjectEditor(projectFile, FContext);

  doc := TYAML.CreateMapping;
  TJsonProjections.ProjectInfo(editor, doc.AsMapping);
  result := TJsonUtils.ToCompactJson(doc);
end;

{ TMCPWhyPackageTool }

function TMCPWhyPackageTool.GetName : string;
begin
  result := 'dpm_why_package';
end;

function TMCPWhyPackageTool.GetTitle : string;
begin
  result := 'Explain a Delphi dependency';
end;

function TMCPWhyPackageTool.GetDescription : string;
begin
  result :=
    'Explain why a package is present in a Delphi project''s dependency tree. Returns every ' +
    'chain from the project down to that package, so you can see which top level package ' +
    'pulls it in and at what version. ' +
    'Use this when a project depends on something unexpected, when deciding whether a package ' +
    'can safely be removed, or when two packages disagree about the version of a shared ' +
    'dependency. Read only.';
end;

procedure TMCPWhyPackageTool.BuildInputSchema(const target : IYAMLMapping);
var
  properties : IYAMLMapping;
begin
  properties := BeginSchema(target);
  AddStringProperty(properties, 'packageId',
    'The package to explain, e.g. "Spring4D.Base". Case insensitive.');
  AddStringProperty(properties, 'projectPath', cProjectPathArgDescription);
  AddRequired(target, ['packageId']);
end;

function TMCPWhyPackageTool.Invoke(const cancellationToken : ICancellationToken; const arguments : IYAMLMapping) : string;
var
  projectFile : string;
  packageId : string;
  editor : IProjectEditor;
  graph : IPackageReference;
  matches : IList<IPackageReference>;
  node : IPackageReference;
  stack : IList<IPackageReference>;
  doc : IYAMLDocument;
  root : IYAMLMapping;
  chains : IYAMLSequence;
  chain : IYAMLSequence;
  entry : IYAMLMapping;
  i : integer;
  j : integer;
begin
  projectFile := RequireProjectFile(arguments, FContext);
  packageId := RequireStringArg(arguments, 'packageId');
  editor := LoadProjectEditor(projectFile, FContext);

  doc := TYAML.CreateMapping;
  root := doc.AsMapping;
  root.AddOrSetValue('packageId', packageId);
  root.AddOrSetValue('projectFile', ExtractFileName(projectFile));

  graph := editor.GetPackageReferences;
  matches := nil;
  if graph <> nil then
    matches := graph.FindChildren(packageId);

  //Not found is an answer, not an error - the model asked a reasonable question and "it is
  //not there" is the correct reply.
  root.AddOrSetValue('found', (matches <> nil) and (matches.Count > 0));

  chains := root.AddOrSetSequence('chains');
  if matches <> nil then
  begin
    for i := 0 to matches.Count - 1 do
    begin
      stack := TCollections.CreateList<IPackageReference>;
      node := matches[i];
      while (node <> nil) and (not node.IsRoot) do
      begin
        stack.Insert(0, node);
        node := node.Parent;
      end;

      chain := chains.AddSequence;
      for j := 0 to stack.Count - 1 do
      begin
        entry := chain.AddMapping;
        entry.AddOrSetValue('id', stack[j].Id);
        TJsonUtils.AddVersion(entry, 'version', stack[j].Version);
        TJsonUtils.AddVersionRange(entry, 'versionRange', stack[j].VersionRange);
      end;
    end;
  end;

  result := TJsonUtils.ToCompactJson(doc);
end;

end.
