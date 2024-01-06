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


unit DPM.Core.Spec.Node;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  JsonDataObjects,
  DPM.Core.Spec.Interfaces;

type
  TSpecNodeClass = class of TSpecNode;
  TSpecNode = class(TInterfacedObject, ISpecNode)
  private
    FLogger : ILogger;
  protected
    property Logger : ILogger read FLogger;
    function LoadFromJson(const jsonObject : TJsonObject) : boolean; virtual; abstract;
    function ToJSON: string; virtual; abstract;
    function LoadJsonCollection(const collection : TJSonArray; const nodeClass : TSpecNodeClass; const action : TConstProc<IInterface>) : boolean;
    function LoadObjectList(list: Spring.Collections.IList<ISpecNode>): TJsonArray;
  public
    constructor Create(const logger : ILogger); virtual;
  end;



implementation

{ TSpecNode }

constructor TSpecNode.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

function TSpecNode.LoadJsonCollection(const collection : TJSonArray; const nodeClass : TSpecNodeClass; const action : TConstProc<IInterface>) : boolean;
var
  item : ISpecNode;
  i : Integer;
  obj : TJsonObject;
begin
  result := true;
  if collection.Count = 0 then
    exit;

  for i := 0 to collection.Count - 1 do
  begin
    item := nodeClass.Create(Logger) as ISpecNode;
    obj := collection.O[i];
    item.LoadFromJson(obj);
    action(item);
  end;
end;

function TSpecNode.LoadObjectList(list: Spring.Collections.IList<ISpecNode>): TJsonArray;
var
  i: Integer;
  json : TJSONObject;
begin
  Result := TJsonArray.Create;
  for i := 0 to list.Count - 1 do
  begin
    json := TJsonObject.Parse(list[i].ToJson) as TJsonObject;
    Result.Add(json);
  end;
end;

end.

