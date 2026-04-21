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

unit DPM.Console.Command.Why;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.Writer,
  DPM.Console.ExitCodes,
  DPM.Console.Command,
  DPM.Console.Command.Base;

type
  TWhyCommand = class(TBaseCommand)
  private
    FConsole : IConsoleWriter;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager; const console : IConsoleWriter); reintroduce;
  end;


implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Generics.Collections,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor,
  DPM.Core.Options.Common,
  DPM.Core.Options.Why;

type
  TWhyNode = class
  strict private
    FId : string;
    FVersion : string;
    FChildren : TObjectList<TWhyNode>;
  public
    constructor Create(const id : string; const version : string);
    destructor Destroy; override;
    function FindOrAddChild(const id : string; const version : string) : TWhyNode;
    property Id : string read FId;
    property Version : string read FVersion;
    property Children : TObjectList<TWhyNode> read FChildren;
  end;

{ TWhyNode }

constructor TWhyNode.Create(const id : string; const version : string);
begin
  FId := id;
  FVersion := version;
  FChildren := TObjectList<TWhyNode>.Create(True);
end;

destructor TWhyNode.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TWhyNode.FindOrAddChild(const id : string; const version : string) : TWhyNode;
var
  child : TWhyNode;
begin
  for child in FChildren do
    if SameText(child.FId, id) and (child.FVersion = version) then
      exit(child);
  result := TWhyNode.Create(id, version);
  FChildren.Add(result);
end;


{ TWhyCommand }

constructor TWhyCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager; const console : IConsoleWriter);
begin
  inherited Create(logger, configurationManager);
  FConsole := console;
end;

function TWhyCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  packageId : string;
  projectPath : string;
  projectFiles : TStringDynArray;
  config : IConfiguration;
  projectEditor : IProjectEditor;
  graph : IPackageReference;
  matches : IList<IPackageReference>;
  node : IPackageReference;
  chain : IList<IPackageReference>;
  i : integer;
  j : integer;
  root : TWhyNode;
  current : TWhyNode;

  procedure RenderChildren(const parent : TWhyNode; const prefix : string);
  var
    k : integer;
    isLast : boolean;
    isMatch : boolean;
    branch : string;
    childPrefix : string;
    child : TWhyNode;
  begin
    for k := 0 to parent.Children.Count - 1 do
    begin
      child := parent.Children[k];
      isLast := k = parent.Children.Count - 1;
      if isLast then
      begin
        branch := '└─ ';
        childPrefix := prefix + '   ';
      end
      else
      begin
        branch := '├─ ';
        childPrefix := prefix + '│  ';
      end;
      isMatch := SameText(child.Id, packageId);
      FConsole.Write(prefix + branch, ccGrey);
      if isMatch then
        FConsole.Write(child.Id, ccBrightAqua)
      else
        FConsole.Write(child.Id, ccWhite);
      FConsole.WriteLine(' (v' + child.Version + ')', ccDarkAqua);
      RenderChildren(child, childPrefix);
    end;
  end;

begin
  TWhyOptions.Default.ApplyCommon(TCommonOptions.Default);

  projectPath := TWhyOptions.Default.ProjectPath;
  if projectPath = '' then
    projectPath := GetCurrentDir;

  if DirectoryExists(projectPath) then
  begin
    projectFiles := TDirectory.GetFiles(projectPath, '*.dproj');
    if Length(projectFiles) = 0 then
    begin
      Logger.Error('No .dproj file found in directory [' + projectPath + ']');
      result := TExitCode.InvalidArguments;
      exit;
    end;
    if Length(projectFiles) > 1 then
    begin
      Logger.Error('Multiple .dproj files found in directory [' + projectPath + '] - specify a single .dproj file.');
      result := TExitCode.InvalidArguments;
      exit;
    end;
    projectPath := projectFiles[0];
  end;

  TWhyOptions.Default.ProjectPath := projectPath;

  if not TWhyOptions.Default.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  packageId := TWhyOptions.Default.PackageId;

  config := FConfigurationManager.LoadConfig(TWhyOptions.Default.ConfigFile);
  if config = nil then
  begin
    result := TExitCode.InitException;
    exit;
  end;

  projectEditor := TProjectEditor.Create(Logger, config, TWhyOptions.Default.CompilerVersion);
  if not projectEditor.LoadProject(projectPath,
    [TProjectElement.PackageRefs, TProjectElement.DPMCompiler, TProjectElement.ProjectVersion]) then
  begin
    result := TExitCode.Error;
    exit;
  end;

  graph := projectEditor.GetPackageReferences;
  if graph = nil then
  begin
    Logger.Information('Project has no package references.');
    result := TExitCode.OK;
    exit;
  end;

  matches := graph.FindChildren(packageId);
  if (matches = nil) or (matches.Count = 0) then
  begin
    Logger.Information('Package ''' + packageId + ''' is not in the dependency graph of ' +
                       ExtractFileName(projectPath) + '.');
    result := TExitCode.OK;
    exit;
  end;

  root := TWhyNode.Create(ExtractFileName(projectPath), '');
  try
    for i := 0 to matches.Count - 1 do
    begin
      //build chain from root-child down to the match (exclusive of root)
      chain := TCollections.CreateList<IPackageReference>;
      node := matches[i];
      while (node <> nil) and (not node.IsRoot) do
      begin
        chain.Insert(0, node);
        node := node.Parent;
      end;

      //graft chain into the unified tree, collapsing shared prefixes
      current := root;
      for j := 0 to chain.Count - 1 do
        current := current.FindOrAddChild(chain[j].Id, chain[j].Version.ToStringNoMeta);
    end;

    FConsole.WriteLine;
    FConsole.Write('The following dependency graph references ''', ccWhite);
    FConsole.Write(packageId, ccBrightAqua);
    FConsole.WriteLine(''':', ccWhite);
    FConsole.WriteLine;
    FConsole.WriteLine('[' + root.Id + ']', ccBrightWhite);
    RenderChildren(root, '  ');
    FConsole.WriteLine;
  finally
    root.Free;
  end;

  result := TExitCode.OK;
end;

end.
