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


unit DPM.Core.Spec.Node;

interface

uses
  System.Classes,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  JsonDataObjects,
  VSoft.YAML,
  DPM.Core.Spec.Interfaces;

type
  TSpecNodeClass = class of TSpecNode;
  TSpecNode = class(TInterfacedObject, ISpecNode)
  private
    FLogger : ILogger;
    FComments : TStringList;
  protected
    procedure AddToArray<T>(arr : TArray<T>; value : T);

    function GetComments : TStrings;
    function HasComments : boolean;
    procedure SetComments(const value : TStrings);

    //takes any comments available on the yamlobject
    procedure LoadComments(const yamlObject : IYAMLValue);

    function LoadFromJson(const jsonObject : TJsonObject) : boolean; virtual; abstract;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;virtual;abstract;

    function ToJSON: string; virtual; abstract;
    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);virtual;abstract;
    function LoadJsonCollection(const collection : TJSonArray; const nodeClass : TSpecNodeClass; const action : TConstProc<IInterface>) : boolean;
    function LoadYAMLCollection(const collection : IYAMLSequence; const nodeClass : TSpecNodeClass; const action : TConstProc<IInterface>) : boolean;

    function LoadObjectList(const list: Spring.Collections.IList<ISpecNode>): TJsonArray;

    //for internal use
    property Comments : TStrings read GetComments write SetComments;
    property Logger : ILogger read FLogger;
  public
    constructor Create(const logger : ILogger); virtual;
    destructor Destroy;override;
  end;



implementation

uses
  System.SysUtils;

{ TSpecNode }

procedure TSpecNode.LoadComments(const yamlObject: IYAMLValue);
var
  collection : IYAMLCollection;
begin
  if yamlObject.IsSequence or yamlObject.IsMapping then
  begin
    collection := yamlObject.AsCollection;
    if collection.HasComments then
      SetComments(collection.Comments);
  end;
end;

procedure TSpecNode.AddToArray<T>(arr: TArray<T>; value: T);
begin
  SetLength(arr, Length(arr)+1);
  arr[Length(arr)-1] := value;
end;

constructor TSpecNode.Create(const logger : ILogger);
begin
  inherited Create;
  FLogger := logger;
  FComments := nil;
end;

destructor TSpecNode.Destroy;
begin
  FLogger := nil;
  FreeAndNil(FComments);
  inherited;
end;

function TSpecNode.GetComments: TStrings;
begin
  if FComments = nil then
    FComments := TStringList.Create;
  result := FComments;
end;

function TSpecNode.HasComments: boolean;
begin
  result := (FComments <> nil) and (FComments.Count > 0);
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

function TSpecNode.LoadObjectList(const list: Spring.Collections.IList<ISpecNode>): TJsonArray;
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

function TSpecNode.LoadYAMLCollection(const collection: IYAMLSequence; const nodeClass: TSpecNodeClass; const action: TConstProc<IInterface>): boolean;
var
  item : ISpecNode;
  i : Integer;
  obj : IYAMLMapping;
begin
  result := true;
  if collection.Count = 0 then
    exit;

  for i := 0 to collection.Count - 1 do
  begin
    item := nodeClass.Create(Logger) as ISpecNode;
    obj := collection.O[i];
    item.LoadFromYAML(obj);
    action(item);
  end;

end;

procedure TSpecNode.SetComments(const value: TStrings);
begin
  if value <> nil then
  begin
    if FComments = nil then
      FComments := TStringList.Create;
    FComments.Assign(value);
  end;
end;

end.

