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

unit DPM.Core.Spec.PackageDefinition;

interface
uses
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  //A package project (dpk/dproj pair) that DPM should generate - used for source-only
  //libraries that don't ship their own package projects. The generated project is written
  //into the package cache version folder at install time and then compiled by the build
  //entries. See ISpecPackageDefinition for the field semantics.
  TSpecPackageDefinition = class(TSpecNode, ISpecPackageDefinition)
  private
    FProject : string;
    FFiles : IList<string>;
    FExclude : IList<string>;
    FRequires : IList<string>;
    FPlatforms : TDPMPlatforms;
    FKind : string;
  protected
    function GetProject : string;
    function GetFiles : IList<string>;
    function GetExclude : IList<string>;
    function GetRequires : IList<string>;
    function GetPlatforms : TDPMPlatforms;
    function GetKind : string;

    procedure SetProject(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetKind(const value : string);

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function Clone : ISpecPackageDefinition;
    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;

    property Project : string read GetProject write SetProject;
    property Files : IList<string> read GetFiles;
    property Exclude : IList<string> read GetExclude;
    property Requires : IList<string> read GetRequires;
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    property Kind : string read GetKind write SetKind;

  public
    constructor CreateClone(const logger : ILogger; const project, kind : string; const platforms : TDPMPlatforms;
                            const files, exclude, requires : IList<string>); reintroduce;
  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils;

{ TSpecPackageDefinition }

function TSpecPackageDefinition.Clone : ISpecPackageDefinition;
begin
  result := TSpecPackageDefinition.CreateClone(logger, FProject, FKind, FPlatforms, FFiles, FExclude, FRequires);
end;

constructor TSpecPackageDefinition.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FFiles := TCollections.CreateList<string>;
  FExclude := TCollections.CreateList<string>;
  FRequires := TCollections.CreateList<string>;
end;

constructor TSpecPackageDefinition.CreateClone(const logger : ILogger; const project, kind : string; const platforms : TDPMPlatforms;
                                               const files, exclude, requires : IList<string>);
begin
  inherited Create(logger);
  FProject := project;
  FKind := kind;
  FPlatforms := platforms;
  FFiles := TCollections.CreateList<string>;
  FExclude := TCollections.CreateList<string>;
  FRequires := TCollections.CreateList<string>;
  if files <> nil then
    FFiles.AddRange(files);
  if exclude <> nil then
    FExclude.AddRange(exclude);
  if requires <> nil then
    FRequires.AddRange(requires);
end;

function TSpecPackageDefinition.GetExclude : IList<string>;
begin
  result := FExclude;
end;

function TSpecPackageDefinition.GetFiles : IList<string>;
begin
  result := FFiles;
end;

function TSpecPackageDefinition.GetKind : string;
begin
  result := FKind;
end;

function TSpecPackageDefinition.GetPlatforms : TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TSpecPackageDefinition.GetProject : string;
begin
  result := FProject;
end;

function TSpecPackageDefinition.GetRequires : IList<string>;
begin
  result := FRequires;
end;

function TSpecPackageDefinition.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  filesSeq : IYAMLSequence;
  excludeSeq : IYAMLSequence;
  requiresSeq : IYAMLSequence;
  platformsSeq : IYAMLSequence;
  i : integer;
  platform : TDPMPlatform;
  sPlatform : string;
  entry : string;
begin
  result := true;
  FProject := yamlObject.S['project'];
  if FProject = '' then
  begin
    Logger.Error('Package definition is missing required [project] property.');
    result := false;
  end;

  //files is required - a package with no units can't produce a usable bpl.
  if yamlObject.ContainsKey('files') then
  begin
    filesSeq := yamlObject.A['files'];
    for i := 0 to filesSeq.Count - 1 do
    begin
      entry := Trim(filesSeq.S[i]);
      if entry <> '' then
        FFiles.Add(entry);
    end;
  end;
  if FFiles.Count = 0 then
  begin
    Logger.Error('Package definition [' + FProject + '] is missing required [files] entries.');
    result := false;
  end;

  if yamlObject.ContainsKey('exclude') then
  begin
    excludeSeq := yamlObject.A['exclude'];
    for i := 0 to excludeSeq.Count - 1 do
    begin
      entry := Trim(excludeSeq.S[i]);
      if entry <> '' then
        FExclude.Add(entry);
    end;
  end;

  if yamlObject.ContainsKey('requires') then
  begin
    requiresSeq := yamlObject.A['requires'];
    for i := 0 to requiresSeq.Count - 1 do
    begin
      entry := Trim(requiresSeq.S[i]);
      if entry <> '' then
        FRequires.Add(entry);
    end;
  end;

  FPlatforms := [];
  platformsSeq := yamlObject.A['platforms'];
  if platformsSeq.Count > 0 then
  begin
    for i := 0 to platformsSeq.Count - 1 do
    begin
      sPlatform := platformsSeq.S[i];
      platform := StringToDPMPlatform(sPlatform);
      if platform <> TDPMPlatform.UnknownPlatform then
        FPlatforms := FPlatforms + [platform];
    end;
  end;

  FKind := yamlObject.S['kind'];
end;

procedure TSpecPackageDefinition.SetKind(const value : string);
begin
  FKind := value;
end;

procedure TSpecPackageDefinition.SetPlatforms(const value : TDPMPlatforms);
begin
  FPlatforms := value;
end;

procedure TSpecPackageDefinition.SetProject(const value : string);
begin
  FProject := value;
end;

procedure TSpecPackageDefinition.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  mapping : IYAMLMapping;
  seq : IYAMLSequence;
  platform : TDPMPlatform;
  i : integer;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['project'] := FProject;

  if FKind <> '' then
    mapping.S['kind'] := FKind;

  if FFiles.Count > 0 then
  begin
    seq := mapping.A['files'];
    for i := 0 to FFiles.Count - 1 do
      seq.AddValue(FFiles[i]);
  end;

  if FExclude.Count > 0 then
  begin
    seq := mapping.A['exclude'];
    for i := 0 to FExclude.Count - 1 do
      seq.AddValue(FExclude[i]);
  end;

  if FRequires.Count > 0 then
  begin
    seq := mapping.A['requires'];
    for i := 0 to FRequires.Count - 1 do
      seq.AddValue(FRequires[i]);
  end;

  if FPlatforms <> [] then
  begin
    seq := mapping.A['platforms'];
    for platform in FPlatforms do
      seq.AddValue(DPMPlatformToString(platform));
  end;
end;

end.
