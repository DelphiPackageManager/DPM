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

unit DPM.Core.Project.PackageGenerator;

// Generates the dpk/dproj package projects declared by a template's
// `package definitions` into the package cache version folder, so a source-only
// library (one that ships no package projects of its own) can still be compiled
// at install time. The generated projects are picked up and built by the
// installer's existing build/design-entry compile loop - the build entry's
// `project` path must match the package definition's `project` path so both
// resolve to the same file under the cache folder.

interface

uses
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces;

type
  IPackageProjectGenerator = interface
    ['{1B9F2C77-3A4D-4E61-8F0A-7C5D2E9B4A18}']
    //Generate every `package definitions` project declared by the given template into
    //packagePath (the cache version folder). compiler/targetPlatforms come from the
    //resolved targetPlatform. Returns false on a hard error (a definition whose file
    //globs matched nothing, or a write failure); true when there's nothing to do or all
    //definitions were written.
    function Generate(const cancellationToken : ICancellationToken; const spec : IPackageSpec;
                      const template : ISpecTemplate; const packagePath : string;
                      const compiler : TCompilerVersion; const targetPlatforms : TDPMPlatforms) : boolean;
  end;

  TPackageProjectGenerator = class(TInterfacedObject, IPackageProjectGenerator)
  private
    FLogger : ILogger;
  protected
    function Generate(const cancellationToken : ICancellationToken; const spec : IPackageSpec;
                      const template : ISpecTemplate; const packagePath : string;
                      const compiler : TCompilerVersion; const targetPlatforms : TDPMPlatforms) : boolean;
  public
    constructor Create(const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Project.Prepare.Templates,
  DPM.Core.Project.Prepare.SourceFiles;

{ TPackageProjectGenerator }

constructor TPackageProjectGenerator.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
end;

//explicit kind wins; otherwise infer design when the requires list mentions designide.
function ResolveKind(const def : ISpecPackageDefinition) : TPrepareProjectKind;
var
  k : string;
  i : integer;
begin
  k := LowerCase(Trim(def.Kind));
  if k = 'design' then
    exit(pkDesign);
  if k = 'runtime' then
    exit(pkRuntime);
  if def.Requires <> nil then
    for i := 0 to def.Requires.Count - 1 do
      if SameText(Trim(def.Requires[i]), 'designide') then
        exit(pkDesign);
  result := pkRuntime;
end;

//rtl is always emitted by the renderer, and designide for design packages - filter both
//out of the author-declared requires so they aren't duplicated in the dpk/dproj.
function FilterRequires(const def : ISpecPackageDefinition) : IList<string>;
var
  i : integer;
  name : string;
begin
  result := TCollections.CreateList<string>;
  if def.Requires = nil then
    exit;
  for i := 0 to def.Requires.Count - 1 do
  begin
    name := Trim(def.Requires[i]);
    if name = '' then
      continue;
    if SameText(name, 'rtl') or SameText(name, 'designide') then
      continue;
    result.Add(name);
  end;
end;

function TPackageProjectGenerator.Generate(const cancellationToken : ICancellationToken; const spec : IPackageSpec;
                                           const template : ISpecTemplate; const packagePath : string;
                                           const compiler : TCompilerVersion; const targetPlatforms : TDPMPlatforms) : boolean;
var
  i, k : integer;
  def : ISpecPackageDefinition;
  variables : IDictionary<string, string>;
  projectPath : string;
  dprojPath : string;
  folderPath : string;
  projectName : string;
  absoluteSources : IList<TPrepareSourceFile>;
  relativeSources : IList<TPrepareSourceFile>;
  relEntry : TPrepareSourceFile;
  kind : TPrepareProjectKind;
  effectivePlatforms : TDPMPlatforms;
  requires : IList<string>;
  dpkPath : string;
begin
  result := true;
  if (template = nil) or (template.PackageDefinitions = nil) or (not template.PackageDefinitions.Any) then
    exit;

  variables := TPrepareSourceGatherer.BuildVariables(spec, compiler);

  for i := 0 to template.PackageDefinitions.Count - 1 do
  begin
    if cancellationToken.IsCancelled then
      exit(false);

    def := template.PackageDefinitions[i];
    if (def = nil) or (Trim(def.Project) = '') then
      continue;

    //resolve the project path (variables + separators) and split into folder + name.
    //combined against packagePath the same way the installer's build loop combines
    //the build entry's project path, so they land on the same file.
    projectPath := TPrepareSourceGatherer.ResolveVariables(def.Project, variables);
    projectPath := StringReplace(projectPath, '/', PathDelim, [rfReplaceAll]);
    dprojPath := TPath.GetFullPath(TPath.Combine(packagePath, projectPath));
    projectName := ChangeFileExt(ExtractFileName(dprojPath), '');
    folderPath := ExtractFilePath(dprojPath);
    if projectName = '' then
    begin
      FLogger.Error('Package definition has an invalid project path: ' + def.Project);
      result := false;
      continue;
    end;

    //expand the definition's file globs (relative to the package cache root) applying
    //its exclude patterns, then make each path relative to the generated dproj's folder.
    absoluteSources := TPrepareSourceGatherer.GatherFromPatterns(FLogger, packagePath, def.Files, def.Exclude, variables);
    if absoluteSources.Count = 0 then
    begin
      FLogger.Error('Package definition [' + def.Project + '] matched no source files - cannot generate package project.');
      result := false;
      continue;
    end;

    relativeSources := TCollections.CreateList<TPrepareSourceFile>;
    for k := 0 to absoluteSources.Count - 1 do
    begin
      relEntry.Path := TPrepareSourceGatherer.MakeRelativePath(folderPath, absoluteSources[k].Path);
      relEntry.FormName := absoluteSources[k].FormName;
      relativeSources.Add(relEntry);
    end;

    kind := ResolveKind(def);
    requires := FilterRequires(def);

    //entry override intersected with the targetPlatform set; empty override = use the
    //targetPlatform set as-is. An empty intersection means the override only named
    //platforms the targetPlatform doesn't support - skip + warn rather than emit a
    //dproj with no platforms.
    if def.Platforms <> [] then
      effectivePlatforms := targetPlatforms * def.Platforms
    else
      effectivePlatforms := targetPlatforms;

    if effectivePlatforms = [] then
    begin
      FLogger.Warning('Package definition [' + def.Project + '] has no platforms after intersecting the override with the targetPlatform - skipping.');
      continue;
    end;

    TDirectory.CreateDirectory(folderPath);
    dpkPath := TPath.Combine(folderPath, projectName + '.dpk');

    try
      TFile.WriteAllText(dpkPath, TPrepareTemplates.RenderDpk(projectName, compiler, relativeSources, requires, kind), TEncoding.UTF8);
      TFile.WriteAllText(dprojPath, TPrepareTemplates.RenderDproj(projectName, compiler, relativeSources, requires, kind, effectivePlatforms), TEncoding.UTF8);
      FLogger.Information('Generated package project: ' + dprojPath + ' (' + DPMPlatformsToString(effectivePlatforms, ', ') + ')');
    except
      on e : Exception do
      begin
        FLogger.Error('Failed to generate package project [' + def.Project + '] : ' + e.Message);
        result := false;
      end;
    end;
  end;
end;

end.
