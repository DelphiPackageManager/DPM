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

    FLibSuffix : string;
    FLibPrefix : string;
    FLibVersion : string;
  protected
    function GetLibSuffix : string;
    function GetLibPrefix : string;
    function GetLibVersion : string;
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;

    procedure SetProject(const value : string);
    procedure SetDefines(const value : string);
    procedure SetLibSuffix(const value : string);
    procedure SetLibPrefix(const value : string);
    procedure SetLibVersion(const value : string);
    procedure SetPlatforms(const value: TDPMPlatforms);


    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;


    function Clone : ISpecDesignEntry;reintroduce;
    constructor CreateClone(const logger : ILogger; const project : string; const defines : string; platforms : TDPMPlatforms;
                            const libSuffix, libPrefix, libVersion : string ); reintroduce;

  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils;

{ TSpecDesignEntry }

function TSpecDesignEntry.Clone : ISpecDesignEntry;
begin
  result := TSpecDesignEntry.CreateClone(logger, FProject, FDefines, FPlatforms, FLibSuffix, FLibPrefix, FLibVersion );
end;

constructor TSpecDesignEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FPlatforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
end;

constructor TSpecDesignEntry.CreateClone(const logger : ILogger; const project : string; const defines : string; platforms : TDPMPlatforms;
                                         const libSuffix, libPrefix, libVersion : string);
begin
  inherited Create(logger);
  FProject := project;
  FDefines := defines;
  FPlatforms := platforms;

  FLibSuffix := libSuffix;
  FLibPrefix := libPrefix;
  FLibVersion := libVersion;
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


function TSpecDesignEntry.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TSpecDesignEntry.GetProject: string;
begin
  result := FProject;
end;



function TSpecDesignEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
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

  FLibSuffix := yamlObject.S['libSuffix'];
  FLibPrefix := yamlObject.S['libPrefix'];
  FLibVersion := yamlObject.S['libVersion'];

  //apply the default if not in the file.
  if FPlatforms = [] then
    FPlatforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];


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
  platform : TDPMPlatform;
  sPlatform : string;
  platforms : TDPMPlatforms;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['project'] := FProject;
  //we want to avoid the platforms being written out if they are default value
  if FPlatforms = [TDPMPlatform.Win32, TDPMPlatform.Win64] then
    platforms := []
  else
    platforms := FPlatforms;

  if platforms <> [] then
  begin
    platformsSeq := mapping.A['platforms'];
    for platform in platforms do
    begin
      sPlatform := DPMPlatformToString(platform);
      platformsSeq.AddValue(sPlatform);
    end;
  end;

  if FDefines <> '' then
    mapping.S['defines'] := FDefines;

  if FLibSuffix <> '' then
    parent.AsMapping.S['libSuffix'] := FLibSuffix;
  if FLibPrefix <> '' then
    parent.AsMapping.S['libPrefix'] := FLibPrefix;
  if FLibVersion <> '' then
    parent.AsMapping.S['libVersion'] := FLibVersion;

end;

end.

