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

unit DPM.Core.Spec.SearchPathGroup;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.TargetPlatform,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.SearchPath;

type
  TSpecSearchPathGroup = class(TSpecSearchPath, ISpecSearchPathGroup)
  private
    FTargetPlatform : TTargetPlatform;
    FSearchPaths : IList<ISpecSearchPath>;
  protected
    function GetTargetPlatform : TTargetPlatform;
    function GetSearchPaths: IList<ISpecSearchPath>;
    function Clone: ISpecSearchPath;override;
    function IsGroup: Boolean;override;
    function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;

    constructor CreateClone(const logger: ILogger; const targetPlatform : TTargetPlatform; const searchPaths : IList<ISpecSearchPath>);
  public
    constructor Create(const logger: ILogger); override;
  end;

implementation

uses
  DPM.Core.Constants;

{ TSpecSearchPathGroup }

function TSpecSearchPathGroup.Clone: ISpecSearchPath;
var
  searchPaths : IList<ISpecSearchPath>;
  clonePath, path : ISpecSearchPath;
begin
  searchPaths := TCollections.CreateList<ISpecSearchPath>;
  for path in FSearchPaths do
  begin
    clonePath := path.Clone;
    searchPaths.Add(clonePath);
  end;
  result := TSpecSearchPathGroup.CreateClone(logger, FTargetPlatform.Clone, searchPaths);
end;

constructor TSpecSearchPathGroup.Create(const logger: ILogger);
begin
  inherited Create(logger);
  FTargetPlatform := TTargetPlatform.Empty;
  FSearchPaths := TCollections.CreateList<ISpecSearchPath>;

end;

constructor TSpecSearchPathGroup.CreateClone(const logger: ILogger; const targetPlatform: TTargetPlatform; const searchPaths: IList<ISpecSearchPath>);
begin
  inherited Create(logger);
  FTargetPlatform := targetPlatform;
  FSearchPaths := TCollections.CreateList<ISpecSearchPath>;
  FSearchPaths.AddRange(searchPaths);
end;

function TSpecSearchPathGroup.GetSearchPaths: IList<ISpecSearchPath>;
begin
  result := FSearchPaths;
end;

function TSpecSearchPathGroup.GetTargetPlatform: TTargetPlatform;
begin
  result := FTargetPlatform;
end;

function TSpecSearchPathGroup.IsGroup: Boolean;
begin
  result := true;
end;

function TSpecSearchPathGroup.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  i : integer;
  searchPath : ISpecSearchPath;
  sValue : string;
  searchPathsArray : TJsonArray;
begin
  result := true;
  //don't call inherited
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

  searchPathsArray := jsonObject.A['searchPaths'];
  if searchPathsArray.Count = 0 then
    exit;

  for i := 0 to searchPathsArray.Count -1 do
  begin
    searchPath := TSpecSearchPath.Create(Logger);
    FSearchPaths.Add(searchPath);
    result := searchPath.LoadFromJson(searchPathsArray.O[i]) and result;
  end;
end;


end.
