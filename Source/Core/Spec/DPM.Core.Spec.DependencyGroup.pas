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

unit DPM.Core.Spec.DependencyGroup;

interface

uses
  JsonDataObjects,
  DPM.Core.Logging,
  DPM.Core.TargetPlatform,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Dependency,
  Spring.Collections;


type
  TSpecDependencyGroup = class(TSpecDependency, ISpecDependencyGroup)
  private
    FTargetPlatform : TTargetPlatform;
    FDependencies : IList<ISpecDependency>;
  protected
    function GetDependencies: IList<ISpecDependency>;
    function GetTargetPlatform : TTargetPlatform;
    function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;

    function IsGroup: Boolean;override;
    function Clone: ISpecDependency;override;
    constructor CreateClone(const logger : ILogger; const targetPlatform : TTargetPlatform; const dependencies : IList<ISpecDependency>);
  public
    constructor Create(const logger : ILogger);override;

  end;

implementation

uses
  DPM.Core.Constants;

{ TSpecDependencyGroup }

function TSpecDependencyGroup.Clone: ISpecDependency;
var
  dependencies : IList<ISpecDependency>;
  dep : ISpecDependency;
  cloneDep : ISpecDependency;
begin
  dependencies := TCollections.CreateList<ISpecDependency>;
  for dep in FDependencies do
  begin
    cloneDep := dep.Clone;
    dependencies.Add(cloneDep)
  end;
  result := TSpecDependencyGroup.CreateClone(logger, FTargetPlatform.Clone,dependencies );
end;

constructor TSpecDependencyGroup.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FDependencies := TCollections.CreateList<ISpecDependency>;
end;

function TSpecDependencyGroup.GetTargetPlatform: TTargetPlatform;
begin
   result := FTargetPlatform;
end;

constructor TSpecDependencyGroup.CreateClone(const logger: ILogger; const targetPlatform: TTargetPlatform; const dependencies: IList<ISpecDependency>);
var
  dependency : ISpecDependency;
begin
  inherited Create(logger);
  FTargetPlatform := targetPlatform;
  FDependencies := TCollections.CreateList<ISpecDependency>;
  for dependency in dependencies do
    FDependencies.Add(dependency.Clone);
end;

function TSpecDependencyGroup.GetDependencies: IList<ISpecDependency>;
begin
  result := FDependencies;
end;

function TSpecDependencyGroup.IsGroup: Boolean;
begin
  result := true;
end;

function TSpecDependencyGroup.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  sValue : string;
  i : integer;
  dependencies : TJsonArray;
  dependency : ISpecDependency;
begin
  result := true;
  sValue := jsonObject.S[cTargetPlatformAttribute];
  if sValue = '' then
  begin
    result := false;
    Logger.Error('Required property [' + cTargetPlatformAttribute + '] is missing.' );
  end
  else
  begin
    if not TTargetPlatform.TryParse(sValue, FTargetPlatform) then
    begin
      result := false;
      Logger.Error('Invalid targetPlatform [' + sValue + ']');
    end;
  end;

  dependencies := jsonObject.A['dependencies'];

  if dependencies.Count = 0 then
    exit;

  for i := 0 to dependencies.Count -1 do
  begin
    dependency := TSpecDependency.Create(Logger);
    FDependencies.Add(dependency);
    result := dependency.LoadFromJson(dependencies.O[i]) and result;
  end;
end;

end.
