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

unit DPM.Core.Spec.FileEntry;

interface

uses
  JsonDataObjects,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecFileEntry = class(TSpecNode, ISpecFileEntry)
  private
  protected
    //making these protected to simplify clone;
    FSource  : string;
    FDestination : string;
    FExclude : IList<string>;
    FFlatten : boolean;
    FIgnore : boolean;

    function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;
    function GetSource: string;
    function GetDestination: string;
    function GetExclude: IList<string>;
    function GetFlatten: Boolean;
    procedure SetSource(const value : string);
    procedure SetDestination(const value : string);
    function GetIgnore : boolean;

    constructor CreateClone(const logger: ILogger; const src : string; const dest : string; const exclude : IList<string>; const flatten : boolean; const ignore : boolean);virtual;
    function Clone: ISpecFileEntry;overload;
  public
    constructor Create(const logger: ILogger); override;

  end;

implementation

uses
  System.SysUtils;

{ TSpecFileEntry }

function TSpecFileEntry.Clone: ISpecFileEntry;
begin
  result := TSpecFileEntry.CreateClone(logger,FSource, FDestination, FExclude, FFlatten, FIgnore );
end;

constructor TSpecFileEntry.Create(const logger: ILogger);
begin
  inherited Create(Logger);
  FExclude := TCollections.CreateList<string>;
end;

constructor TSpecFileEntry.CreateClone(const logger: ILogger; const src, dest : string; const  exclude: IList<string>; const flatten: boolean; const ignore : boolean);
begin
  inherited Create(logger);
  FSource := src;
  FDestination := dest;
  FExclude := TCollections.CreateList<string>;
  FExclude.AddRange(exclude);
  FFlatten := flatten;
  FIgnore := ignore;
end;

function TSpecFileEntry.GetExclude: IList<string>;
begin
  result := FExclude;
end;

function TSpecFileEntry.GetFlatten: Boolean;
begin
  result := FFlatten;
end;

function TSpecFileEntry.GetIgnore: boolean;
begin
  result := FIgnore;
end;

function TSpecFileEntry.GetSource: string;
begin
  result := FSource;
end;

function TSpecFileEntry.GetDestination: string;
begin
  result := FDestination;
end;

function TSpecFileEntry.LoadFromJson(const jsonObject: TJsonObject): Boolean;
var
  excludeArray : TJsonArray;
  entry : string;
  i: Integer;
begin
  result := true;
  FSource := jsonObject.S['src'];
  if FSource = '' then
  begin
    result := false;
    Logger.Error('Required attribute [src] is missing');
  end;
  FDestination := jsonObject.S['dest'];
  if FDestination = '' then
  begin
    result := false;
    Logger.Error('Required attribute [dest] is missing');
  end;

  FFlatten := jsonObject.B['flatten'];
  FIgnore := jsonObject.B['ignore'];

  if jsonObject.Contains('exclude') then
  begin
    excludeArray := jsonObject.A['exclude']; //this is causing an av!

    for i := 0 to excludeArray.Count -1 do
    begin
      entry := excludeArray.S[i];
      if entry <> '' then
        FExclude.Add(entry);
    end;
  end;
end;

procedure TSpecFileEntry.SetSource(const value: string);
begin
  FSource := value;
end;

procedure TSpecFileEntry.SetDestination(const value: string);
begin
  FDestination := value;
end;

end.
