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

unit DPM.Core.SBOM.ComponentRules;

{ Rules that name the libraries behind units the build links but no DPM package owns - a commercial
  component suite, a library on the search path, source vendored into the project. Without a rule,
  such units are grouped by the folder they were found in, or not identified at all.

  Rules come from the components: list in <project>.sbom.yaml, plus its ownUnits and ownUnitPrefixes.
  An exact unit name beats an own-code unit name, which beats a unit name prefix, which beats an
  own-code prefix - so a precise rule always wins over a broad one - and the first rule wins inside
  each kind. Names compare ignoring case. }

interface

uses
  Spring.Collections,
  DPM.Core.Logging;

{$SCOPEDENUMS ON}

type
  ///<summary>One library, and the units that belong to it.</summary>
  TSBOMComponentRule = record
    Name : string;
    Version : string;
    Supplier : string;
    SupplierUrl : string;     // a URL, or an email address
    License : string;         // SPDX identifier or expression, or a licence name
    Purl : string;
    ComponentType : string;   // 'library', 'framework' or 'application' - '' means library
    Units : TArray<string>;          // exact unit names
    UnitPrefixes : TArray<string>;   // unit name prefixes
  end;

  TSBOMUnitMatch = (
    None,        // no rule says anything about the unit
    Component,   // a rule claims it - ruleIndex says which
    OwnCode      // the project's own code, not a component
  );

  ISBOMComponentRules = interface
    ['{6E0B1F4C-7A52-4C1B-9F0D-3B8E2C5A1D47}']
    function GetCount : integer;
    function GetRule(index : integer) : TSBOMComponentRule;
    function GetOwnUnits : TArray<string>;
    function GetOwnUnitPrefixes : TArray<string>;
    procedure Add(const rule : TSBOMComponentRule);
    procedure AddOwnUnit(const unitName : string);
    procedure AddOwnUnitPrefix(const prefix : string);
    ///<summary>Appends other's rules and own-code names after these, so these keep priority.</summary>
    procedure AddAll(const other : ISBOMComponentRules);
    function Match(const unitName : string; out ruleIndex : integer) : TSBOMUnitMatch;
    property Count : integer read GetCount;
    property Rules[index : integer] : TSBOMComponentRule read GetRule;
  end;

  TSBOMComponentRules = class(TInterfacedObject, ISBOMComponentRules)
  private
    FRules : IList<TSBOMComponentRule>;
    FOwnUnits : IList<string>;
    FOwnUnitPrefixes : IList<string>;
  protected
    function GetCount : integer;
    function GetRule(index : integer) : TSBOMComponentRule;
    function GetOwnUnits : TArray<string>;
    function GetOwnUnitPrefixes : TArray<string>;
    procedure Add(const rule : TSBOMComponentRule);
    procedure AddOwnUnit(const unitName : string);
    procedure AddOwnUnitPrefix(const prefix : string);
    procedure AddAll(const other : ISBOMComponentRules);
    function Match(const unitName : string; out ruleIndex : integer) : TSBOMUnitMatch;
  public
    constructor Create;
  end;

///<summary>Checks a rule read from a file before it is added : false, with a warning, when it has
/// no name. Normalises ComponentType, and warns about a type it doesn't know and about prefixes
/// short enough to claim units they shouldn't. context names the row in warnings.</summary>
function ValidateComponentRule(const logger : ILogger; const context : string; var rule : TSBOMComponentRule) : boolean;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  DPM.Core.Diagnostics;

const
  //a shorter prefix claims far more than the library it was written for
  cMinPrefixLength = 3;

function ContainsText(const values : array of string; const value : string) : boolean;
var
  i : integer;
begin
  for i := 0 to Length(values) - 1 do
    if SameText(values[i], value) then
      exit(true);
  result := false;
end;

function ContainsPrefixOf(const prefixes : array of string; const value : string) : boolean;
var
  i : integer;
begin
  for i := 0 to Length(prefixes) - 1 do
    if (prefixes[i] <> '') and StartsText(prefixes[i], value) then
      exit(true);
  result := false;
end;

{ TSBOMComponentRules }

constructor TSBOMComponentRules.Create;
begin
  inherited Create;
  FRules := TCollections.CreateList<TSBOMComponentRule>;
  FOwnUnits := TCollections.CreateList<string>;
  FOwnUnitPrefixes := TCollections.CreateList<string>;
end;

function TSBOMComponentRules.GetCount : integer;
begin
  result := FRules.Count;
end;

function TSBOMComponentRules.GetRule(index : integer) : TSBOMComponentRule;
begin
  result := FRules[index];
end;

function TSBOMComponentRules.GetOwnUnits : TArray<string>;
begin
  result := FOwnUnits.ToArray;
end;

function TSBOMComponentRules.GetOwnUnitPrefixes : TArray<string>;
begin
  result := FOwnUnitPrefixes.ToArray;
end;

procedure TSBOMComponentRules.Add(const rule : TSBOMComponentRule);
begin
  FRules.Add(rule);
end;

procedure TSBOMComponentRules.AddOwnUnit(const unitName : string);
begin
  if Trim(unitName) <> '' then
    FOwnUnits.Add(Trim(unitName));
end;

procedure TSBOMComponentRules.AddOwnUnitPrefix(const prefix : string);
begin
  if Trim(prefix) <> '' then
    FOwnUnitPrefixes.Add(Trim(prefix));
end;

procedure TSBOMComponentRules.AddAll(const other : ISBOMComponentRules);
var
  i : integer;
  name : string;
begin
  if other = nil then
    exit;
  for i := 0 to other.Count - 1 do
    FRules.Add(other.Rules[i]);
  for name in other.GetOwnUnits do
    FOwnUnits.Add(name);
  for name in other.GetOwnUnitPrefixes do
    FOwnUnitPrefixes.Add(name);
end;

function TSBOMComponentRules.Match(const unitName : string; out ruleIndex : integer) : TSBOMUnitMatch;
var
  i : integer;
begin
  ruleIndex := -1;
  result := TSBOMUnitMatch.None;
  if Trim(unitName) = '' then
    exit;

  for i := 0 to FRules.Count - 1 do
    if ContainsText(FRules[i].Units, unitName) then
    begin
      ruleIndex := i;
      exit(TSBOMUnitMatch.Component);
    end;

  if ContainsText(FOwnUnits.ToArray, unitName) then
    exit(TSBOMUnitMatch.OwnCode);

  for i := 0 to FRules.Count - 1 do
    if ContainsPrefixOf(FRules[i].UnitPrefixes, unitName) then
    begin
      ruleIndex := i;
      exit(TSBOMUnitMatch.Component);
    end;

  if ContainsPrefixOf(FOwnUnitPrefixes.ToArray, unitName) then
    result := TSBOMUnitMatch.OwnCode;
end;

function ValidateComponentRule(const logger : ILogger; const context : string; var rule : TSBOMComponentRule) : boolean;
var
  i : integer;
begin
  rule.Name := Trim(rule.Name);
  if rule.Name = '' then
  begin
    logger.Warning(TWarningCode.W8104_SBOMComponentRuleInvalid, '[SBOM] ' + context + ' has no name and was skipped');
    exit(false);
  end;

  rule.ComponentType := LowerCase(Trim(rule.ComponentType));
  if (rule.ComponentType <> '') and (rule.ComponentType <> 'library') and
     (rule.ComponentType <> 'framework') and (rule.ComponentType <> 'application') then
  begin
    logger.Warning(TWarningCode.W8104_SBOMComponentRuleInvalid, '[SBOM] ' + context + ' (' + rule.Name + ') : type [' + rule.ComponentType +
                   '] is not library, framework or application - library is used');
    rule.ComponentType := '';
  end;

  for i := 0 to Length(rule.UnitPrefixes) - 1 do
    if Length(rule.UnitPrefixes[i]) < cMinPrefixLength then
      logger.Warning(TWarningCode.W8104_SBOMComponentRuleInvalid, '[SBOM] ' + context + ' (' + rule.Name + ') : the unit prefix [' + rule.UnitPrefixes[i] +
                     '] is shorter than ' + IntToStr(cMinPrefixLength) + ' characters and may claim units of other libraries');
  result := true;
end;

end.
