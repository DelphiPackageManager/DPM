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

unit DPM.Console.Command.Project;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Project.Interfaces;

type
  ///<summary>
  ///  Reports what a .dproj or .groupproj targets and which DPM packages it references.
  ///</summary>
  ///<remarks>
  ///  Read only - the project file is never written. Nothing else in the CLI surfaces this,
  ///  and it is the context anything reasoning about a project needs before it can give a
  ///  compiler-correct answer.
  ///</remarks>
  TProjectCommand = class(TBaseCommand)
  private
    function ResolveProjectFiles(const inputPath : string; out projectFiles : IList<string>) : boolean;
    function LoadEditor(const projectFile : string; const config : IConfiguration; out editor : IProjectEditor) : boolean;
    procedure EmitText(const editors : IList<IProjectEditor>);
    procedure EmitJson(const editors : IList<IProjectEditor>);
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
  public
    constructor Create(const logger : ILogger; const configurationManager : IConfigurationManager); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  VSoft.YAML,
  DPM.Console.RawIO,
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.Project,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Json.Utils,
  DPM.Core.Json.Projections;

{ TProjectCommand }

constructor TProjectCommand.Create(const logger : ILogger; const configurationManager : IConfigurationManager);
begin
  inherited Create(logger, configurationManager);
end;

function TProjectCommand.ResolveProjectFiles(const inputPath : string; out projectFiles : IList<string>) : boolean;
var
  path : string;
  found : TArray<string>;
  groupReader : IGroupProjectReader;
  groupProjects : IList<string>;
  groupDir : string;
  i : integer;
begin
  result := false;
  projectFiles := TCollections.CreateList<string>;

  path := inputPath;
  if path = '' then
    path := GetCurrentDir;

  //A bare directory is only unambiguous when it holds exactly one project - say which files
  //were found rather than silently picking one.
  if DirectoryExists(path) then
  begin
    found := TDirectory.GetFiles(path, '*.groupproj');
    if Length(found) = 0 then
      found := TDirectory.GetFiles(path, '*.dproj');

    if Length(found) = 0 then
    begin
      Logger.Error('No .dproj or .groupproj file found in directory [' + path + ']');
      exit;
    end;
    if Length(found) > 1 then
    begin
      Logger.Error('Multiple project files found in directory [' + path + '] - specify one:');
      for i := 0 to Length(found) - 1 do
        Logger.Error('  ' + ExtractFileName(found[i]));
      exit;
    end;
    path := found[0];
  end;

  if not FileExists(path) then
  begin
    Logger.Error('Project file not found [' + path + ']');
    exit;
  end;

  if SameText(ExtractFileExt(path), '.groupproj') then
  begin
    groupReader := TGroupProjectReader.Create(Logger);
    if not groupReader.LoadGroupProj(path) then
      exit;
    groupProjects := TCollections.CreateList<string>;
    if not groupReader.ExtractProjects(groupProjects) then
      exit;
    //ExtractProjects returns the Include attributes verbatim, which are relative to the
    //groupproj - resolve them here rather than leaving the caller to guess.
    groupDir := ExtractFilePath(path);
    for i := 0 to groupProjects.Count - 1 do
      projectFiles.Add(TPath.GetFullPath(groupDir + groupProjects[i]));
  end
  else
    projectFiles.Add(path);

  result := projectFiles.Count > 0;
end;

function TProjectCommand.LoadEditor(const projectFile : string; const config : IConfiguration; out editor : IProjectEditor) : boolean;
begin
  editor := TProjectEditor.Create(Logger, config, TProjectOptions.Default.CompilerVersion);
  result := editor.LoadProject(projectFile, [TProjectElement.PackageRefs, TProjectElement.DPMCompiler,
                                             TProjectElement.ProjectVersion, TProjectElement.Platforms,
                                             TProjectElement.AppType]);
  if not result then
    Logger.Error('Unable to load project [' + projectFile + ']');
end;

procedure TProjectCommand.EmitText(const editors : IList<IProjectEditor>);
var
  editor : IProjectEditor;
  graph : IPackageReference;
  topLevel : IPackageReference;

  procedure WriteNode(const reference : IPackageReference; const indent : string);
  var
    child : IPackageReference;
  begin
    Logger.Information(indent + reference.Id + ' ' + reference.Version.ToStringNoMeta);
    if reference.HasChildren then
    begin
      for child in reference.Children do
        WriteNode(child, indent + '  ');
    end;
  end;

begin
  for editor in editors do
  begin
    Logger.Information(ExtractFileName(editor.ProjectFile));
    Logger.Information('  Compiler   : ' + CompilerToString(editor.CompilerVersion));
    Logger.Information('  Platforms  : ' + DPMPlatformsToString(editor.Platforms));
    Logger.Information('  DPM        : ' + BoolToStr(editor.HasDPM, true));

    graph := editor.GetPackageReferences;
    if (graph = nil) or (not graph.HasChildren) then
      Logger.Information('  Packages   : none')
    else
    begin
      Logger.Information('  Packages   :');
      //Skip the graph root itself - it is a synthetic node whose id and version are not
      //meaningful (TPackageReference.CreateRoot), so only its children are real packages.
      for topLevel in graph.Children do
        WriteNode(topLevel, '    ');
    end;
    Logger.Information('');
  end;
end;

procedure TProjectCommand.EmitJson(const editors : IList<IProjectEditor>);
var
  doc : IYAMLDocument;
  root : IYAMLMapping;
  projects : IYAMLSequence;
  editor : IProjectEditor;
begin
  doc := TYAML.CreateMapping;
  root := doc.AsMapping;

  //Always an array, even for a single .dproj, so a caller does not have to branch on whether
  //it passed a project or a project group.
  projects := root.AddOrSetSequence('projects');
  for editor in editors do
    TJsonProjections.ProjectInfo(editor, projects.AddMapping);

  TStdOut.WriteLine(TJsonUtils.ToCompactJson(doc));
end;

function TProjectCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  options : TProjectOptions;
  config : IConfiguration;
  projectFiles : IList<string>;
  editors : IList<IProjectEditor>;
  editor : IProjectEditor;
  projectFile : string;
  jsonMode : boolean;
begin
  result := TExitCode.Error;

  options := TProjectOptions.Default;
  options.ApplyCommon(TCommonOptions.Default);
  jsonMode := options.OutputFormat = TOutputFormat.Json;

  if not options.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if not ResolveProjectFiles(options.ProjectPath, projectFiles) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  config := FConfigurationManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit(TExitCode.InitException);

  editors := TCollections.CreateList<IProjectEditor>;
  for projectFile in projectFiles do
  begin
    if cancellationToken.IsCancelled then
      exit;
    if not LoadEditor(projectFile, config, editor) then
      exit;
    editors.Add(editor);
  end;

  if jsonMode then
    EmitJson(editors)
  else
    EmitText(editors);

  result := TExitCode.OK;
end;

end.
