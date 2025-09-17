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

unit DPM.Core.Spec.DesignEntry;

interface

uses
  JsonDataObjects,
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.BuildEntry;

type
  TSpecDesignEntry = class(TSpecBuildEntry, ISpecDesignEntry)
  private
    FLibSuffix : string;
    FLibPrefix : string;
    FLibVersion : string;
  protected
    function GetLibSuffix : string;
    function GetLibPrefix : string;
    function GetLibVersion : string;


    procedure SetLibSuffix(const value : string);
    procedure SetLibPrefix(const value : string);
    procedure SetLibVersion(const value : string);
    procedure SetPlatforms(const value: TDPMPlatforms); override;


    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);override;
    function ToJSON : string; override;


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
  result := TSpecDesignEntry.CreateClone(logger, Project, Defines, Platforms, FLibSuffix, FLibPrefix, FLibVersion );
end;

constructor TSpecDesignEntry.Create(const logger : ILogger);
begin
  inherited Create(logger);
  Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
end;

constructor TSpecDesignEntry.CreateClone(const logger : ILogger; const project : string; const defines : string; platforms : TDPMPlatforms;
                                         const libSuffix, libPrefix, libVersion : string);
begin
  inherited CreateClone(logger,project, defines, platforms);

  FLibSuffix := libSuffix;
  FLibPrefix := libPrefix;
  FLibVersion := libVersion;
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


function TSpecDesignEntry.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
begin
  result := inherited LoadFromJson(jsonObject);
  FLibSuffix := jsonObject.S['libSuffix'];
  FLibPrefix := jsonObject.S['libPrefix'];
  FLibVersion := jsonObject.S['libVersion'];

  //apply the default if not in the file.
  if Platforms = [] then
    Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];


end;


function TSpecDesignEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
begin
  result := inherited LoadFromYAML(yamlObject);
  FLibSuffix := yamlObject.S['libSuffix'];
  FLibPrefix := yamlObject.S['libPrefix'];
  FLibVersion := yamlObject.S['libVersion'];

  //apply the default if not in the file.
  if Platforms = [] then
    Platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];


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
  inherited SetPlatforms(platforms);
end;

function TSpecDesignEntry.ToJSON: string;
//var
//  json : TJSONObject;
begin
  raise ENotImplemented.Create('should be removed');
//  json := TJSONObject.Create;
//  try
//    json.S['buildId'] := FBuildId;
//    json.S['src'] := FSource;
//    if FCopyLocal then
//      json.B['copyLocal'] := FCopyLocal;
//    if FInstall then
//      json.B['install'] := FInstall;
//    Result := json.ToJSON;
//  finally
//    FreeAndNil(json);
//  end;
end;

procedure TSpecDesignEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  platforms : TDPMPlatforms;
begin
  //we want to avoid the platforms being written out if they are default value - so blank them out then restor the
  if Self.Platforms = [TDPMPlatform.Win32, TDPMPlatform.Win64] then
  begin
    //save
    platforms := [TDPMPlatform.Win32, TDPMPlatform.Win64];
    inherited SetPlatforms([]);
  end;
  inherited ToYAML(parent, packageKind);
  if platforms <> [] then
    //restore
    inherited SetPlatforms(platforms);

  if FLibSuffix <> '' then
    parent.AsMapping.S['libSuffix'] := FLibSuffix;
  if FLibPrefix <> '' then
    parent.AsMapping.S['libPrefix'] := FLibPrefix;
  if FLibVersion <> '' then
    parent.AsMapping.S['libVersion'] := FLibVersion;

end;

end.

