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
  VSoft.YAML,
  DPM.Core.Spec.Interfaces;

type
  TSpecNodeClass = class of TSpecNode;
  TSpecNode = class(TInterfacedObject, ISpecNode)
  private
    FLogger : ILogger;
    FComments : TStringList;
  protected
    procedure AddToArray<T>(var arr : TArray<T>; value : T);

    function GetComments : TStrings;
    function HasComments : boolean;
    procedure SetComments(const value : TStrings);

    //takes any comments available on the yamlobject
    procedure LoadComments(const yamlObject : IYAMLValue);

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;virtual;abstract;

    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);virtual;abstract;
    function LoadYAMLCollection(const collection : IYAMLSequence; const nodeClass : TSpecNodeClass; const action : TConstProc<IInterface>) : boolean;

    //Reads the compiler names under key - a sequence, or a single scalar - into compilers. An unknown
    //name is logged and fails the load rather than being skipped, since the caller acts on what is
    //missing from the set. owner describes the node for the error message.
    function LoadCompilersFromYAML(const yamlObject : IYAMLMapping; const key : string; const owner : string; out compilers : TCompilerVersions) : boolean;
    //Writes compilers as a sequence under key, in compiler order. Writes nothing for an empty set.
    procedure CompilersToYAML(const mapping : IYAMLMapping; const key : string; const compilers : TCompilerVersions);


    //for internal use
    property Comments : TStrings read GetComments write SetComments;
    property Logger : ILogger read FLogger;
  public
    constructor Create(const logger : ILogger); virtual;
    destructor Destroy;override;
  end;



implementation

uses
  System.SysUtils,
  DPM.Core.Diagnostics;

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

procedure TSpecNode.AddToArray<T>(var arr: TArray<T>; value: T);
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
    //keep loading the rest so every error is reported, but an item that failed fails the collection.
    result := item.LoadFromYAML(obj) and result;
    action(item);
  end;

end;

function TSpecNode.LoadCompilersFromYAML(const yamlObject : IYAMLMapping; const key : string; const owner : string; out compilers : TCompilerVersions) : boolean;
var
  values : TStringList;
  compilersSeq : IYAMLSequence;
  i : integer;
  sCompiler : string;
  compiler : TCompilerVersion;
begin
  result := true;
  compilers := [];
  if not yamlObject.Contains(key) then
    exit;

  values := TStringList.Create;
  try
    if yamlObject.Values[key].IsSequence then
    begin
      compilersSeq := yamlObject.A[key];
      for i := 0 to compilersSeq.Count - 1 do
        values.Add(compilersSeq.S[i]);
    end
    else
      values.Add(yamlObject.S[key]);

    for i := 0 to values.Count - 1 do
    begin
      sCompiler := Trim(values[i]);
      if sCompiler = '' then
        continue;
      compiler := StringToCompilerVersion(sCompiler);
      if compiler = TCompilerVersion.UnknownVersion then
      begin
        Logger.Error(TErrorCode.E6004_SpecValueInvalid, owner + ' has an invalid compiler value [' + sCompiler + '] in [' + key + ']');
        result := false;
      end
      else
        Include(compilers, compiler);
    end;
  finally
    values.Free;
  end;
end;

procedure TSpecNode.CompilersToYAML(const mapping : IYAMLMapping; const key : string; const compilers : TCompilerVersions);
var
  compilersSeq : IYAMLSequence;
  compiler : TCompilerVersion;
begin
  if compilers = [] then
    exit;
  compilersSeq := mapping.A[key];
  for compiler in compilers do
    compilersSeq.AddValue(CompilerToString(compiler));
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

