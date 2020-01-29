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

unit DPM.Core.Spec.BPLEntry;

interface

uses
  JsonDataObjects,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.FileEntry;

type
  TSpecBPLEntry = class(TSpecFileEntry, ISpecBPLEntry)
  private
    FCopyLocal : boolean;
    FInstall   : boolean;
  protected
    function LoadFromJson(const jsonObject: TJsonObject): Boolean;override;

    function GetCopyLocal: Boolean;
    function GetInstall: Boolean;
    function Clone: ISpecBPLEntry;overload;
    constructor CreateClone(const logger: ILogger; const src : string; const dest : string; const exclude : IList<string>; const flatten, copyLocal, install : boolean);reintroduce;
  public
    constructor Create(const logger: ILogger); override;
  end;

implementation

uses
  System.SysUtils;

{ TSpecBPLEntry }

function TSpecBPLEntry.Clone: ISpecBPLEntry;
begin
  result := TSpecBPLEntry.CreateClone(logger, Self.GetSource, Self.GetDestination,Self.GetExclude,Self.GetFlatten,FCopyLocal, FInstall);
end;

constructor TSpecBPLEntry.Create(const logger: ILogger);
begin
  inherited Create(logger);

end;

constructor TSpecBPLEntry.CreateClone(const logger: ILogger; const src, dest : string; const exclude: IList<string>; const flatten, copyLocal, install: boolean);
begin
  inherited CreateClone(logger, src, dest, exclude, flatten, false);
  FCopyLocal := copyLocal;
  FInstall := install;
end;

function TSpecBPLEntry.GetCopyLocal: Boolean;
begin
  result := FCopyLocal;
end;

function TSpecBPLEntry.GetInstall: Boolean;
begin
  result := FInstall;
end;

function TSpecBPLEntry.LoadFromJson(const jsonObject: TJsonObject): Boolean;
begin
  result := inherited LoadFromJson(jsonObject);
  FCopyLocal := jsonObject.B['copylocal'];
  FInstall := jsonObject.B['install'];
end;


end.
