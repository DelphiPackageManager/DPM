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

unit DPM.Core.Spec.BuildEntry;

interface
uses
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecBuildEntry = class(TSpecNode, ISpecBuildEntry)
  private
    FProject : string;
    FPlatforms : TDPMPlatforms;
    FDefines : string;
    FReferences : IList<string>;
    FSearchPaths : IList<string>;
  protected
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;
    function GetReferences : IList<string>;
    function GetSearchPaths : IList<string>;

    procedure SetProject(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetDefines(const value : string);

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function Clone : ISpecBuildEntry;
    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;

    //here for design entry to use
    property Project : string read GetProject write SetProject;
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    property Defines : string read GetDefines write SetDefines;
    property References : IList<string> read GetReferences;
    property SearchPaths : IList<string> read GetSearchPaths;

  public
    constructor CreateClone(const logger : ILogger; const project : string; const defines : string; const platforms : TDPMPlatforms; const references : IList<string>; const searchPaths : IList<string>); reintroduce;
  public
    constructor Create(const logger : ILogger); override;


  end;

implementation

uses
  System.SysUtils;

{ TSpecBuildEntry }

function TSpecBuildEntry.Clone : ISpecBuildEntry;
begin
  result := TSpecBuildEntry.CreateClone(logger, FProject, FDefines, FPlatforms, FReferences, FSearchPaths);
end;

constructor TSpecBuildEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FReferences := TCollections.CreateList<string>;
  FSearchPaths := TCollections.CreateList<string>;
end;

constructor TSpecBuildEntry.CreateClone(const logger : ILogger; const project, defines : string; const platforms : TDPMPlatforms; const references : IList<string>; const searchPaths : IList<string>);
begin
  inherited Create(logger);
  FProject := project;
  FDefines := defines;
  FPlatforms := platforms;
  FReferences := TCollections.CreateList<string>;
  if references <> nil then
    FReferences.AddRange(references);
  FSearchPaths := TCollections.CreateList<string>;
  if searchPaths <> nil then
    FSearchPaths.AddRange(searchPaths);
end;



function TSpecBuildEntry.GetDefines: string;
begin
  result := FDefines;
end;


function TSpecBuildEntry.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TSpecBuildEntry.GetProject : string;
begin
  result := FProject;
end;

function TSpecBuildEntry.GetReferences : IList<string>;
begin
  result := FReferences;
end;

function TSpecBuildEntry.GetSearchPaths : IList<string>;
begin
  result := FSearchPaths;
end;



function TSpecBuildEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  platformsSeq : IYAMLSequence;
  refsSeq : IYAMLSequence;
  searchSeq : IYAMLSequence;
  i : integer;
  platform  : TDPMPlatform;
  sPlatform : string;
  refName : string;
  searchPath : string;
begin
  result := true;
  FProject := yamlObject.S['project'];
  if FProject = '' then
  begin
    Logger.Error('Build Entry is missing required [project] property.');
    result := false;
  end;

  FDefines := yamlObject.S['defines'];
  platformsSeq := yamlObject.A['platforms'];
  FPlatforms := [];
  if platformsSeq.Count > 0 then
  begin
    for i := 0 to platformsSeq.Count -1 do
    begin
      sPlatform := platformsSeq.S[i];
      platform := StringToDPMPlatform(sPlatform);
      if platform <> TDPMPlatform.UnknownPlatform then
        FPlatforms := FPlatforms + [platform];
    end;
  end;

  if yamlObject.Contains('references') then
  begin
    refsSeq := yamlObject.A['references'];
    for i := 0 to refsSeq.Count - 1 do
    begin
      refName := Trim(refsSeq.S[i]);
      if refName <> '' then
        FReferences.Add(refName);
    end;
  end;

  if yamlObject.Contains('searchPaths') then
  begin
    searchSeq := yamlObject.A['searchPaths'];
    for i := 0 to searchSeq.Count - 1 do
    begin
      searchPath := Trim(searchSeq.S[i]);
      if searchPath <> '' then
        FSearchPaths.Add(searchPath);
    end;
  end;
end;


procedure TSpecBuildEntry.SetDefines(const value: string);
begin
  FDefines := value;
end;


procedure TSpecBuildEntry.SetPlatforms(const value: TDPMPlatforms);
begin
  FPlatforms := value;
end;

procedure TSpecBuildEntry.SetProject(const value : string);
begin
  FProject := value;
end;


procedure TSpecBuildEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  mapping : IYAMLMapping;
  platformsSeq : IYAMLSequence;
  refsSeq : IYAMLSequence;
  searchSeq : IYAMLSequence;
  platform : TDPMPlatform;
  sPlatform : string;
  i : integer;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['project'] := FProject;
  if FPlatforms <> [] then
  begin
    platformsSeq := mapping.A['platforms'];
    for platform in FPlatforms do
    begin
      sPlatform := DPMPlatformToString(platform);
      platformsSeq.AddValue(sPlatform);
    end;
  end;

  if FDefines <> '' then
    mapping.S['defines'] := FDefines;

  if FReferences.Count > 0 then
  begin
    refsSeq := mapping.A['references'];
    for i := 0 to FReferences.Count - 1 do
      refsSeq.AddValue(FReferences[i]);
  end;

  if FSearchPaths.Count > 0 then
  begin
    searchSeq := mapping.A['searchPaths'];
    for i := 0 to FSearchPaths.Count - 1 do
      searchSeq.AddValue(FSearchPaths[i]);
  end;
end;


end.
