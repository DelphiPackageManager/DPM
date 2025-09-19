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

unit DPM.Core.Spec.SourceEntry;

interface

uses
  VSoft.YAML,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecSourceEntry = class(TSpecNode, ISpecSourceEntry)
  private
  protected
    //making these protected to simplify clone;
    FSource : string;
    FDestination : string;
    FExclude : IList<string>;

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function GetSource : string;
    function GetDestination : string;
    function GetExclude : IList<string>;
    procedure SetSource(const value : string);
    procedure SetDestination(const value : string);

    constructor CreateClone(const logger : ILogger; const src : string; const dest : string; const exclude : IList<string>); virtual;
    function Clone : ISpecSourceEntry;

    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;

  public
    constructor Create(const logger : ILogger); override;

  end;

implementation

uses
  System.SysUtils;

{ TSpecSourceEntry }

function TSpecSourceEntry.Clone : ISpecSourceEntry;
begin
  result := TSpecSourceEntry.CreateClone(logger, FSource, FDestination, FExclude);
end;

constructor TSpecSourceEntry.Create(const logger : ILogger);
begin
  inherited Create(Logger);
  FExclude := TCollections.CreateList < string > ;
end;

constructor TSpecSourceEntry.CreateClone(const logger : ILogger; const src, dest : string; const exclude : IList<string>);
begin
  inherited Create(logger);
  FSource := src;
  FDestination := dest;
  FExclude := TCollections.CreateList < string > ;
  FExclude.AddRange(exclude);
end;

function TSpecSourceEntry.GetExclude : IList<string>;
begin
  result := FExclude;
end;

function TSpecSourceEntry.GetSource : string;
begin
  result := FSource;
end;

function TSpecSourceEntry.GetDestination : string;
begin
  result := FDestination;
end;


function TSpecSourceEntry.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  excludeArray : IYAMLSequence;
  entry : string;
  i : Integer;
begin
  result := true;
  FSource := yamlObject.S['src'];
  if FSource = '' then
  begin
    result := false;
    Logger.Error('Required attribute [src] is missing');
  end;
  FDestination := yamlObject.S['dest'];

  if yamlObject.Contains('exclude') then
  begin
    excludeArray := yamlObject.A['exclude'];

    for i := 0 to excludeArray.Count - 1 do
    begin
      entry := excludeArray.S[i];
      if entry <> '' then
        FExclude.Add(entry);
    end;
  end;
end;

procedure TSpecSourceEntry.SetSource(const value : string);
begin
  FSource := value;
end;


procedure TSpecSourceEntry.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  exludeSeq : IYAMLSequence;
  i : integer;
  mapping : IYAMLMapping;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['src'] := FSource;
  if FDestination <> '' then
    mapping.S['dest'] := FDestination;
  if FExclude.Count > 0 then
  begin
    exludeSeq := mapping.A['exclude'];
    for i := 0 to FExclude.Count -1 do
      exludeSeq.AddValue(FExclude[i]);
  end;


end;

procedure TSpecSourceEntry.SetDestination(const value : string);
begin
  FDestination := value;
end;


end.

