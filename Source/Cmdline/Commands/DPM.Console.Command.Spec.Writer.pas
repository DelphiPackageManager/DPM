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

unit DPM.Console.Command.Spec.Writer;

//Generates a .dspec.yaml text file from the discovered + prompted scaffold data.
//Output style follows the hand-authored VSoftTechnologies specs: inline author
//lists, $packageSource$ variables with per-target overrides, grouped
//`compiler from`/`compiler to` ranges when possible.

interface

uses
  DPM.Core.Types;

type
  TSpecTargetInfo = record
    Compiler : TCompilerVersion;
    Platforms : TDPMPlatforms;
    PackageSourceTemplate : string;   //e.g. 'Rad Studio $compilernoprefix$'
    PackageSourceLiteral : string;    //original folder name for reference
  end;

  TSpecDependency = record
    Id : string;
    Version : string;                 //empty string when unknown; writer will emit '' and flag TODO
  end;
  TSpecDependencies = TArray<TSpecDependency>;

  TSpecScaffold = record
    PackageId : string;
    Version : string;
    Description : string;
    Author : string;
    Copyright : string;        //emitted verbatim; use 'X and contributors' convention
    License : string;
    ProjectUrl : string;
    RepositoryUrl : string;
    Tags : TArray<string>;

    //All paths below are relative to the project root with forward slashes and
    //no leading `./` - the writer prepends `./` as needed.
    SourceGlobs : TArray<string>;     //e.g. ['source/*.pas', 'source/*.inc']
    HasPackagesFolder : boolean;
    PackagesFolderRel : string;       //e.g. 'packages'
    Targets : TArray<TSpecTargetInfo>; //ordered by compiler enum
    BuildDProjs : TArray<string>;     //leaf filenames only, will be joined with $packageSource$
    DesignDProjs : TArray<string>;
    BuildPlatforms : TDPMPlatforms;   //platforms for every build entry
    DesignPlatforms : TDPMPlatforms;  //platforms for every design entry (schema limits to win32/win64)
    Dependencies : TSpecDependencies; //emitted under templates[0].dependencies
  end;

function DerivePackageSourceTemplate(const compiler : TCompilerVersion; const folderName : string) : string;
function BuildSpecYaml(const scaffold : TSpecScaffold) : string;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes;

function DerivePackageSourceTemplate(const compiler : TCompilerVersion; const folderName : string) : string;
var
  codeName : string;
  cnp : string;
  cnpDotZero : string;
begin
  result := folderName;
  if result = '' then
    exit;

  codeName := CompilerCodeName(compiler);
  if codeName <> '' then
    result := StringReplace(result, codeName, '$compilerCodeName$', [rfIgnoreCase, rfReplaceAll]);

  cnp := CompilerNoPrefix(compiler);
  if cnp = '' then
    exit;
  cnpDotZero := cnp + '.0';

  if Pos(cnpDotZero, result) > 0 then
    result := StringReplace(result, cnpDotZero, '$compilernoprefix$.0', [rfReplaceAll])
  else
    result := StringReplace(result, cnp, '$compilernoprefix$', [rfReplaceAll]);
end;

function JoinRelPath(const parts : array of string) : string;
var
  i : integer;
  part : string;
begin
  result := '';
  for i := Low(parts) to High(parts) do
  begin
    part := parts[i];
    if part = '' then
      continue;
    if result = '' then
      result := part
    else
      result := result + '/' + part;
  end;
end;

function RelWithDot(const path : string) : string;
begin
  if path = '' then
    exit('');
  if StartsStr('./', path) or StartsStr('/', path) then
    result := path
  else
    result := './' + path;
end;

function YamlEscape(const value : string) : string;
var
  needsQuotes : boolean;
  i : integer;
begin
  //quote when the value contains `:` or `#` or would otherwise be ambiguous.
  //A simple rule: if it contains any of these chars or starts/ends with whitespace, quote.
  needsQuotes := false;
  if (value = '') then
    exit('""');
  for i := 1 to Length(value) do
  begin
    if CharInSet(value[i], [':', '#', '"', '''', '$', '[', ']', '{', '}', ',', '&', '*', '!', '|', '>',
      '%', '@', '`', '\']) then
    begin
      needsQuotes := true;
      break;
    end;
  end;
  if (value[1] = ' ') or (value[Length(value)] = ' ') then
    needsQuotes := true;
  if not needsQuotes then
  begin
    result := value;
    exit;
  end;
  //we only need to escape embedded double quotes
  result := '"' + StringReplace(value, '"', '\"', [rfReplaceAll]) + '"';
end;

function FormatPlatformList(const platforms : TDPMPlatforms) : string;
var
  p : TDPMPlatform;
  first : boolean;
begin
  result := '[ ';
  first := true;
  for p := Low(TDPMPlatform) to High(TDPMPlatform) do
  begin
    if p = TDPMPlatform.UnknownPlatform then
      continue;
    if not (p in platforms) then
      continue;
    if not first then
      result := result + ', ';
    result := result + LowerCase(DPMPlatformToString(p));
    first := false;
  end;
  result := result + ' ]';
end;

function FormatTags(const tags : TArray<string>) : string;
var
  i : integer;
begin
  result := '[ ';
  for i := 0 to High(tags) do
  begin
    if i > 0 then
      result := result + ', ';
    result := result + tags[i];
  end;
  result := result + ' ]';
end;

type
  TTargetRange = record
    StartCompiler : TCompilerVersion;
    EndCompiler : TCompilerVersion;
    Platforms : TDPMPlatforms;
    PackageSourceTemplate : string;
  end;
  TTargetRanges = TArray<TTargetRange>;

function GroupTargetsIntoRanges(const targets : TArray<TSpecTargetInfo>) : TTargetRanges;
var
  i : integer;
  working : TTargetRange;
  list : TArray<TTargetRange>;
begin
  SetLength(list, 0);
  if Length(targets) = 0 then
  begin
    result := list;
    exit;
  end;

  working.StartCompiler := targets[0].Compiler;
  working.EndCompiler := targets[0].Compiler;
  working.Platforms := targets[0].Platforms;
  working.PackageSourceTemplate := targets[0].PackageSourceTemplate;

  for i := 1 to High(targets) do
  begin
    //extend the current run if the next compiler is the immediate successor and
    //shares the same platforms and packageSource template
    if (Ord(targets[i].Compiler) = Ord(working.EndCompiler) + 1) and
       (targets[i].Platforms = working.Platforms) and
       (targets[i].PackageSourceTemplate = working.PackageSourceTemplate) then
    begin
      working.EndCompiler := targets[i].Compiler;
    end
    else
    begin
      SetLength(list, Length(list) + 1);
      list[High(list)] := working;
      working.StartCompiler := targets[i].Compiler;
      working.EndCompiler := targets[i].Compiler;
      working.Platforms := targets[i].Platforms;
      working.PackageSourceTemplate := targets[i].PackageSourceTemplate;
    end;
  end;
  SetLength(list, Length(list) + 1);
  list[High(list)] := working;
  result := list;
end;

function MostCommonTemplate(const ranges : TTargetRanges; out bestTemplate : string) : boolean;
var
  i, j : integer;
  currentTpl : string;
  bestCount : integer;
  currentCount : integer;
begin
  //pick the template that covers the most compilers (weighted by range length)
  bestCount := 0;
  bestTemplate := '';
  for i := 0 to High(ranges) do
  begin
    currentTpl := ranges[i].PackageSourceTemplate;
    currentCount := 0;
    for j := 0 to High(ranges) do
    begin
      if ranges[j].PackageSourceTemplate = currentTpl then
        currentCount := currentCount + (Ord(ranges[j].EndCompiler) - Ord(ranges[j].StartCompiler) + 1);
    end;
    if currentCount > bestCount then
    begin
      bestCount := currentCount;
      bestTemplate := currentTpl;
    end;
  end;
  result := bestTemplate <> '';
end;

procedure AppendLine(var buffer : string; const line : string);
begin
  buffer := buffer + line + #13#10;
end;

procedure AppendMetadata(var buffer : string; const scaffold : TSpecScaffold);
begin
  AppendLine(buffer, 'metadata:');
  AppendLine(buffer, '  id: ' + scaffold.PackageId);
  AppendLine(buffer, '  version: ' + scaffold.Version);
  AppendLine(buffer, '  description: ' + YamlEscape(scaffold.Description));
  AppendLine(buffer, '  authors: [ ' + scaffold.Author + ' ]');
  if scaffold.ProjectUrl <> '' then
    AppendLine(buffer, '  projectUrl: ' + scaffold.ProjectUrl);
  if scaffold.RepositoryUrl <> '' then
    AppendLine(buffer, '  repositoryUrl: ' + scaffold.RepositoryUrl);
  if scaffold.License <> '' then
    AppendLine(buffer, '  license: ' + scaffold.License);
  if scaffold.Copyright <> '' then
    AppendLine(buffer, '  copyright: ' + scaffold.Copyright);
  if Length(scaffold.Tags) > 0 then
    AppendLine(buffer, '  tags: ' + FormatTags(scaffold.Tags));
end;

procedure AppendTargetPlatforms(var buffer : string; const ranges : TTargetRanges;
  const defaultTemplate : string; const hasPackagesFolder : boolean);
var
  i : integer;
  r : TTargetRange;
begin
  AppendLine(buffer, 'targetPlatforms:');
  for i := 0 to High(ranges) do
  begin
    r := ranges[i];
    if r.StartCompiler = r.EndCompiler then
      AppendLine(buffer, '  - compiler: ' + CompilerToString(r.StartCompiler))
    else
    begin
      AppendLine(buffer, '  - compiler from: ' + CompilerToString(r.StartCompiler));
      AppendLine(buffer, '    compiler to: ' + CompilerToString(r.EndCompiler));
    end;
    AppendLine(buffer, '    platforms: ' + FormatPlatformList(r.Platforms));
    if hasPackagesFolder and (r.PackageSourceTemplate <> defaultTemplate) then
    begin
      AppendLine(buffer, '    variables:');
      AppendLine(buffer, '      packageSource: ' + YamlEscape(r.PackageSourceTemplate));
    end;
  end;
end;

procedure AppendProjectEntries(var buffer : string; const dprojs : TArray<string>;
  const platforms : TDPMPlatforms; const hasPackagesFolder : boolean; const packagesFolderRel : string);
var
  i : integer;
  projectPath : string;
begin
  for i := 0 to High(dprojs) do
  begin
    if hasPackagesFolder then
      projectPath := RelWithDot(JoinRelPath([packagesFolderRel, '$packageSource$', dprojs[i]]))
    else
      projectPath := RelWithDot(dprojs[i]);
    AppendLine(buffer, '      - project: ' + projectPath);
    if platforms <> [] then
      AppendLine(buffer, '        platforms: ' + FormatPlatformList(platforms));
  end;
end;

procedure AppendDependencies(var buffer : string; const deps : TSpecDependencies);
var
  i : integer;
  versionText : string;
begin
  if Length(deps) = 0 then
    exit;
  AppendLine(buffer, '    dependencies:');
  for i := 0 to High(deps) do
  begin
    AppendLine(buffer, '      - id: ' + deps[i].Id);
    //Emit an empty double-quoted value when we don't know the version so the
    //YAML stays valid and users see an obvious TODO to fill in.
    if deps[i].Version = '' then
      versionText := '""'
    else
      versionText := deps[i].Version;
    AppendLine(buffer, '        version: ' + versionText);
  end;
end;

procedure AppendTemplates(var buffer : string; const scaffold : TSpecScaffold);
var
  i : integer;
  pkgRel : string;
begin
  AppendLine(buffer, 'templates:');
  AppendLine(buffer, '  - name: default');
  AppendDependencies(buffer, scaffold.Dependencies);
  AppendLine(buffer, '    source:');
  for i := 0 to High(scaffold.SourceGlobs) do
    AppendLine(buffer, '      - src: ' + RelWithDot(scaffold.SourceGlobs[i]));

  if scaffold.HasPackagesFolder then
  begin
    //dest is only required when it differs from the src folder, which is the
    //default. Omit it so the generated file stays minimal.
    pkgRel := RelWithDot(JoinRelPath([scaffold.PackagesFolderRel, '$packageSource$']));
    AppendLine(buffer, '      - src: ' + pkgRel + '/*.dpk');
    AppendLine(buffer, '      - src: ' + pkgRel + '/*.dproj');
  end;

  if Length(scaffold.BuildDProjs) > 0 then
  begin
    AppendLine(buffer, '    build:');
    AppendProjectEntries(buffer, scaffold.BuildDProjs, scaffold.BuildPlatforms,
      scaffold.HasPackagesFolder, scaffold.PackagesFolderRel);
  end;

  if Length(scaffold.DesignDProjs) > 0 then
  begin
    AppendLine(buffer, '    design:');
    AppendProjectEntries(buffer, scaffold.DesignDProjs, scaffold.DesignPlatforms,
      scaffold.HasPackagesFolder, scaffold.PackagesFolderRel);
  end;
end;

function BuildSpecYaml(const scaffold : TSpecScaffold) : string;
var
  buffer : string;
  ranges : TTargetRanges;
  defaultTemplate : string;
begin
  buffer := '';
  AppendLine(buffer, 'min client version: 1.0');
  AppendMetadata(buffer, scaffold);
  AppendLine(buffer, '');

  if scaffold.HasPackagesFolder and (Length(scaffold.Targets) > 0) then
  begin
    ranges := GroupTargetsIntoRanges(scaffold.Targets);
    MostCommonTemplate(ranges, defaultTemplate);
    AppendLine(buffer, 'variables:');
    AppendLine(buffer, '  packageSource: ' + YamlEscape(defaultTemplate));
    AppendLine(buffer, '');
    AppendTargetPlatforms(buffer, ranges, defaultTemplate, true);
  end
  else if Length(scaffold.Targets) > 0 then
  begin
    ranges := GroupTargetsIntoRanges(scaffold.Targets);
    AppendTargetPlatforms(buffer, ranges, '', false);
  end;
  AppendLine(buffer, '');

  AppendTemplates(buffer, scaffold);

  result := buffer;
end;

end.
