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
  protected
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;

    procedure SetProject(const value : string);
    //virtual so we can override in design to limit to Win32/Win64
    procedure SetPlatforms(const value : TDPMPlatforms);virtual;
    procedure SetDefines(const value : string);

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function Clone : ISpecBuildEntry;
    function ToJSON: string; override;
    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;

    //here for design entry to use
    property Project : string read GetProject write SetProject;
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    property Defines : string read GetDefines write SetDefines;

  public
    constructor CreateClone(const logger : ILogger; const project : string; const defines : string; const platforms : TDPMPlatforms); reintroduce;
  public
    constructor Create(const logger : ILogger); override;


  end;

implementation

uses
  System.SysUtils;

{ TSpecBuildEntry }

function TSpecBuildEntry.Clone : ISpecBuildEntry;
begin
  result := TSpecBuildEntry.CreateClone(logger, FProject, FDefines, FPlatforms);
end;

constructor TSpecBuildEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
end;

constructor TSpecBuildEntry.CreateClone(const logger : ILogger; const project, defines : string; const platforms : TDPMPlatforms);
begin
  inherited Create(logger);
  FProject := project;
  FDefines := defines;
  FPlatforms := platforms;
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

function TSpecBuildEntry.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  platformsArray : TJsonArray;
  i : integer;
  platform  : TDPMPlatform;
  sPlatform : string;
begin
  result := true;
  FProject := jsonObject.S['project'];
  if FProject = '' then
  begin
    Logger.Error('Build Entry is missing required [project] property.');
    result := false;
  end;
  FDefines := jsonObject.S['defines'];
  platformsArray := jsonObject.A['platforms'];
  FPlatforms := [];
  if platformsArray.Count > 0 then
  begin
    for i := 0 to platformsArray.Count -1 do
    begin
      sPlatform := platformsArray.S[i];
      platform := StringToDPMPlatform(sPlatform);
      if platform <> TDPMPlatform.UnknownPlatform then
        FPlatforms := FPlatforms + [platform];
    end;
  end;
end;

function TSpecBuildEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  platformsSeq : IYAMLSequence;
  i : integer;
  platform  : TDPMPlatform;
  sPlatform : string;
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

function TSpecBuildEntry.ToJSON: string;
begin
  raise ENotImplemented.Create('ToJSON not implented, will be removed');
end;

procedure TSpecBuildEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
begin

end;


end.

