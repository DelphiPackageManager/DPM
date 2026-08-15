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

unit DPM.Core.Spec.DesignEntry;

interface

uses
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecDesignEntry = class(TSpecNode, ISpecDesignEntry)
  private
    FProject : string;
    FPlatforms : TDPMPlatforms;
    FDefines : string;
    FReferences : IList<string>;
    FSearchPaths : IList<string>;

    FLibSuffix : string;
    FLibPrefix : string;
    FLibVersion : string;
  protected
    function GetLibSuffix : string;
    function GetLibPrefix : string;
    function GetLibVersion : string;
    function GetIsPrebuilt : boolean;
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;
    function GetReferences : IList<string>;
    function GetSearchPaths : IList<string>;

    procedure SetProject(const value : string);
    procedure SetDefines(const value : string);
    procedure SetLibSuffix(const value : string);
    procedure SetLibPrefix(const value : string);
    procedure SetLibVersion(const value : string);
    procedure SetPlatforms(const value: TDPMPlatforms);


    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;


    function ResolvePrebuiltPlatform(out designPlatform : TDPMPlatform; out error : string) : boolean;

    function Clone : ISpecDesignEntry;reintroduce;
    constructor CreateClone(const logger : ILogger; const project : string; const defines : string; platforms : TDPMPlatforms;
                            const libSuffix, libPrefix, libVersion : string; const references : IList<string>; const searchPaths : IList<string> ); reintroduce;

  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils;

{ TSpecDesignEntry }

function TSpecDesignEntry.Clone : ISpecDesignEntry;
begin
  result := TSpecDesignEntry.CreateClone(logger, FProject, FDefines, FPlatforms, FLibSuffix, FLibPrefix, FLibVersion, FReferences, FSearchPaths);
end;

constructor TSpecDesignEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
  //leave empty so the installer can tell "author did not specify" from "author explicitly declared"
  FPlatforms := [];
  FReferences := TCollections.CreateList<string>;
  FSearchPaths := TCollections.CreateList<string>;
end;

constructor TSpecDesignEntry.CreateClone(const logger : ILogger; const project : string; const defines : string; platforms : TDPMPlatforms;
                                         const libSuffix, libPrefix, libVersion : string; const references : IList<string>; const searchPaths : IList<string>);
begin
  inherited Create(logger);
  FProject := project;
  FDefines := defines;
  FPlatforms := platforms;

  FLibSuffix := libSuffix;
  FLibPrefix := libPrefix;
  FLibVersion := libVersion;

  FReferences := TCollections.CreateList<string>;
  if references <> nil then
    FReferences.AddRange(references);
  FSearchPaths := TCollections.CreateList<string>;
  if searchPaths <> nil then
    FSearchPaths.AddRange(searchPaths);
end;


function TSpecDesignEntry.GetDefines: string;
begin
  result := FDefines;
end;

function TSpecDesignEntry.GetLibPrefix: string;
begin
  result := FLibPrefix;
end;

function TSpecDesignEntry.GetLibSuffix: string;
begin
  result := FLibSuffix;
end;

function TSpecDesignEntry.GetLibVersion: string;
begin
  result := FLibVersion;
end;


//A design entry normally names a dproj/dpk for DPM to compile. Packages that ship precompiled
//design-time binaries have no project to build - they point the entry straight at the .bpl that
//is packed in the archive. A .bpl is a build *output*, never something msbuild can load, so the
//extension alone is an unambiguous marker and no extra dspec property is needed. Install skips
//the dproj patch and the msbuild call for these and treats the shipped bpl as already built.
function TSpecDesignEntry.GetIsPrebuilt : boolean;
begin
  result := SameText(ExtractFileExt(Trim(FProject)), '.bpl');
end;

function TSpecDesignEntry.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TSpecDesignEntry.GetProject: string;
begin
  result := FProject;
end;

function TSpecDesignEntry.GetReferences : IList<string>;
begin
  result := FReferences;
end;

function TSpecDesignEntry.GetSearchPaths : IList<string>;
begin
  result := FSearchPaths;
end;



function TSpecDesignEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
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

  FLibSuffix := yamlObject.S['libSuffix'];
  FLibPrefix := yamlObject.S['libPrefix'];
  FLibVersion := yamlObject.S['libVersion'];
  //platforms left empty when not in the file - installer defers to the design dproj in that case
end;


//For a compiled design entry DPM builds the bpl itself, once per platform, into bpl\{platform} - so
//one entry can serve both IDE bitnesses and the platform list is only ever a filter.
//
//A prebuilt entry is the opposite: one binary, one fixed path in the archive, and a bpl can only load
//into an IDE of its own bitness (32 bit IDE -> Win32 bpl, 64 bit IDE -> Win64 bpl). That is a fact
//about the binary, NOT about what the package compiles for - a package targeting only Win32 still
//needs a Win64 design bpl to appear in a 64 bit IDE. Since targetPlatforms therefore can't tell us the
//answer, the entry has to state it, and authors ship one entry per bitness. Guessing here would mean
//silently offering the wrong bitness to the IDE, which can only end in a failed load.
function TSpecDesignEntry.ResolvePrebuiltPlatform(out designPlatform : TDPMPlatform; out error : string) : boolean;
var
  candidate : TDPMPlatform;
  candidateCount : integer;
begin
  result := false;
  designPlatform := TDPMPlatform.UnknownPlatform;
  error := '';

  candidateCount := 0;
  for candidate in FPlatforms do
  begin
    Inc(candidateCount);
    designPlatform := candidate;
  end;

  if candidateCount = 0 then
  begin
    error := 'Prebuilt design entry [' + FProject + '] must declare a platform : Win32 for the 32 bit IDE, Win64 for the 64 bit IDE. ' +
             'A .bpl only loads into an IDE of its own bitness, which is independent of the platforms this package targets - ' +
             'add one design entry per bitness, each pointing at that bitness''s own .bpl.';
    exit;
  end;

  if candidateCount > 1 then
  begin
    designPlatform := TDPMPlatform.UnknownPlatform;
    error := 'Prebuilt design entry [' + FProject + '] declares ' + IntToStr(candidateCount) +
             ' platforms. A .bpl is a single-platform binary - declare one platform per entry, and add a separate design entry pointing at each bitness''s own .bpl.';
    exit;
  end;

  //Design packages are loaded by the IDE itself, and the IDE only ever comes in these two bitnesses.
  if not (designPlatform in [TDPMPlatform.Win32, TDPMPlatform.Win64]) then
  begin
    error := 'Prebuilt design entry [' + FProject + '] declares platform ' + DPMPlatformToString(designPlatform) +
             '. Design packages load into the IDE, so the platform must be Win32 or Win64.';
    designPlatform := TDPMPlatform.UnknownPlatform;
    exit;
  end;

  result := true;
end;

procedure TSpecDesignEntry.SetDefines(const value: string);
begin
  FDefines := value;
end;

procedure TSpecDesignEntry.SetLibPrefix(const value: string);
begin
  FLibPrefix := value;
end;

procedure TSpecDesignEntry.SetLibSuffix(const value: string);
begin
  FLibSuffix := value;
end;

procedure TSpecDesignEntry.SetLibVersion(const value: string);
begin
  FLibVersion := value;
end;



procedure TSpecDesignEntry.SetPlatforms(const value: TDPMPlatforms);
var
  platforms : TDPMPlatforms;
begin
  //Design only supports Win32/Win64
  if (TDPMPlatform.Win32 in value) then
    Include(platforms,TDPMPlatform.win32);
  if (TDPMPlatform.Win32 in value) then
    Include(platforms,TDPMPlatform.win64);
end;

procedure TSpecDesignEntry.SetProject(const value: string);
begin
  FProject := value;
end;


procedure TSpecDesignEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
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
  //FPlatforms is empty when the author did not state platforms; write whatever is set.
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

  if FLibSuffix <> '' then
    mapping.S['libSuffix'] := FLibSuffix;
  if FLibPrefix <> '' then
    mapping.S['libPrefix'] := FLibPrefix;
  if FLibVersion <> '' then
    mapping.S['libVersion'] := FLibVersion;

end;

end.

