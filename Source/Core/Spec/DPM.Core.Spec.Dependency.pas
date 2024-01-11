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

unit DPM.Core.Spec.Dependency;

interface

uses
  JsonDataObjects,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node,
  DPM.Core.Dependency.Version;

type
  TSpecDependency = class(TSpecNode, ISpecDependency)
  private
    FId : string;
    FVersion : TVersionRange;
    FVersionString : string;
  protected
    function GetId : string;
    procedure SetId(Id: string);
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function GetVersionString : string;
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function IsGroup : Boolean; virtual;
    constructor CreateClone(const logger : ILogger; const id : string; const version : TVersionRange; const versionString : string);
    function Clone : ISpecDependency; virtual;
    function ToJSON: string; override;
  public
    constructor Create(const logger : ILogger); override;
  end;

implementation

uses
  System.SysUtils,
  VSoft.SemanticVersion,
  DPM.Core.Packaging.IdValidator;

{ TSpecDependency }

function TSpecDependency.Clone : ISpecDependency;
begin
  result := TSpecDependency.CreateClone(Logger, FId, FVersion.Clone, FVersionString);
end;

constructor TSpecDependency.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

function TSpecDependency.GetId : string;
begin
  result := FId;
end;

constructor TSpecDependency.CreateClone(const logger : ILogger; const id : string; const version : TVersionRange; const versionString : string);
begin
  inherited Create(logger);
  FId := id;
  FVersion := version;
  FVersionString := versionString;
end;

function TSpecDependency.GetVersionRange : TVersionRange;
begin
  result := FVersion;
end;

function TSpecDependency.GetVersionString : string;
begin
  result := FVersionString;
end;

function TSpecDependency.IsGroup : Boolean;
begin
  result := false;
end;

function TSpecDependency.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sValue : string;
  sError : string;
begin
  result := true;
  FId := jsonObject.S['id'];
  if FId = '' then
  begin
    Logger.Error('dependency has no id attribute');
    result := false;
  end
  else if not TPackageIdValidator.IsValidPackageId(FId) then
  begin
    Logger.Error('Invalid dependency Id [' + FId + ']');
    result := false;
  end;
  sValue := jsonObject.S['version'];
  if sValue = '' then
  begin
    Logger.Error('dependency has no version');
    result := false;

  end
  else if sValue = '$version$' then
    FVersionString := sValue
  else if not TVersionRange.TryParseWithError(sValue, FVersion, sError) then
  begin
    Logger.Error('Invalid dependency version attribute [' + sValue + '] - ' + sError);
    result := false;
  end;

end;

procedure TSpecDependency.SetId(Id: string);
begin
  FId := Id;
end;

procedure TSpecDependency.SetVersionRange(const value : TVersionRange);
begin
  FVersion := value;
end;

function TSpecDependency.ToJSON: string;
var
  json : TJsonObject;
begin
  json := TJsonObject.Create;
  try
    json.S['id'] := FId;
    json.S['version'] := FVersion.ToString;
    Result := json.ToJSON;
  finally
    FreeAndNil(json);
  end;
end;

end.

