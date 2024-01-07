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

unit DPM.Core.Spec.SearchPath;

interface

uses
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecSearchPath = class(TSpecNode, ISpecSearchPath)
  private
    FPath : string;
  protected
    function GetPath : string;
    procedure SetPath(const value : string);

    function IsGroup : Boolean; virtual;
    function Clone : ISpecSearchPath; virtual;
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function ToJSON: string; override;

    constructor CreateClone(const logger : ILogger; const path : string);
  public
    constructor Create(const logger : ILogger); override;

  end;

implementation

uses
  Variants,
  System.SysUtils;

{ TSpecSearchPath }

function TSpecSearchPath.Clone : ISpecSearchPath;
begin
  result := TSpecSearchPath.CreateClone(logger, FPath);
end;

constructor TSpecSearchPath.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

constructor TSpecSearchPath.CreateClone(const logger : ILogger; const path : string );
begin
  inherited Create(logger);
  FPath := path;

end;


function TSpecSearchPath.GetPath : string;
begin
  result := FPath;
end;


function TSpecSearchPath.IsGroup : Boolean;
begin
  result := false;
end;


function TSpecSearchPath.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
begin
  result := true;
  FPath := jsonObject.S['path'];
  if FPath = '' then
  begin
    result := false;
    Logger.Error('Required property [path] not found');
  end;
end;

procedure TSpecSearchPath.SetPath(const value : string);
begin
  FPath := value;
end;

function TSpecSearchPath.ToJSon: string;
var
  json : TJsonObject;
begin
  json := TJsonObject.Create;
  try
    json.S['path'] := FPath;
    Result := json.ToJSON;
  finally
    FreeAndNil(json);
  end;
end;

end.

