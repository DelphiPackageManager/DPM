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

unit DPM.Core.Spec.Dependency;

interface

uses
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node,
  DPM.Core.Dependency.Version;

type
  TSpecDependency = class(TSpecNode, ISpecDependency)
  private
    FId : string;
    FVersion : TVersionRange;
    FVersionString : string;
  protected
    function GetId : string;
    procedure SetId(const Id: string);
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function GetVersionString : string;
    procedure SetVersionString(const value : string);
    procedure ResolveVersionToken(const version : TPackageVersion);
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    constructor CreateClone(const logger : ILogger; const id : string; const version : TVersionRange; const versionString : string);
    function Clone : ISpecDependency; virtual;
    procedure ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);override;
  public
    constructor Create(const logger : ILogger); override;


  end;

implementation

uses
  System.SysUtils,
  VSoft.SemanticVersion,
  DPM.Core.Constants,
  DPM.Core.Packaging.IdValidator;

{ TSpecDependency }

function TSpecDependency.Clone : ISpecDependency;
begin
  result := TSpecDependency.CreateClone(Logger, FId, FVersion.Clone, FVersionString);
end;

constructor TSpecDependency.Create(const logger : ILogger);
begin
  inherited Create(logger);

end;

function TSpecDependency.GetId : string;
begin
  result := FId;
end;

constructor TSpecDependency.CreateClone(const logger : ILogger; const id : string; const version : TVersionRange; const versionString : string);
begin
  inherited Create(logger);
  FId := id;
  FVersion := version;
  FVersionString := versionString;
end;

function TSpecDependency.GetVersionRange : TVersionRange;
begin
  result := FVersion;
end;

function TSpecDependency.GetVersionString : string;
begin
  //the authored text: the unresolved token, else the range (empty string when nothing is set) so
  //the UI can display/round-trip $version$ rather than the empty range that FVersion would show.
  if FVersionString = cVersionToken then
    result := cVersionToken
  else if FVersion.IsEmpty then
    result := ''
  else
    result := FVersion.ToString;
end;

procedure TSpecDependency.SetVersionString(const value : string);
var
  range : TVersionRange;
begin
  if SameText(Trim(value), cVersionToken) then
    //defer - resolved to the package's own version at pack time (ResolveVersionToken).
    FVersionString := cVersionToken
  else
  begin
    FVersionString := '';
    if TVersionRange.TryParse(value, range) then
      FVersion := range;
  end;
end;


function TSpecDependency.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  sValue : string;
  sError : string;
begin
  result := true;
  FId := yamlObject.S['id'];
  if FId = '' then
  begin
    Logger.Error('dependency has no id attribute');
    result := false;
  end
  else if not TPackageIdValidator.IsValidPackageId(FId) then
  begin
    Logger.Error('Invalid dependency Id [' + FId + ']');
    result := false;
  end;
  sValue := yamlObject.S['version'];
  if sValue = '' then
  begin
    Logger.Error('dependency has no version');
    result := false;

  end
  else if Trim(sValue) = cVersionToken then
    //deferred - resolved to this package's own version at pack time (ResolveVersionToken).
    FVersionString := cVersionToken
  else if SameText(Trim(sValue), cBundledDependencyToken) then
    //friendly alias for a dependency on an IDE-bundled library (e.g. Indy)
    FVersion := TVersionRange.Parse(cBundledDependencyVersion)
  else if not TVersionRange.TryParseWithError(sValue, FVersion, sError) then
  begin
    Logger.Error('Invalid dependency version attribute [' + sValue + '] - ' + sError);
    result := false;
  end;
end;

procedure TSpecDependency.ResolveVersionToken(const version : TPackageVersion);
begin
  //only the $version$ token is deferred at parse time; every other version is already a concrete
  //range in FVersion. Resolve to a fixed range on this package's version and clear the token so
  //ToYAML writes the resolved version.
  if FVersionString = cVersionToken then
  begin
    FVersion := TVersionRange.Create(version);
    FVersionString := '';
  end;
end;

procedure TSpecDependency.SetId(const Id: string);
begin
  FId := Id;
end;

procedure TSpecDependency.SetVersionRange(const value : TVersionRange);
begin
  FVersion := value;
  //a concrete range supersedes any pending $version$ token (e.g. the user edited the version away
  //from the token in the UI) so ToYAML / resolution don't keep treating it as the token.
  FVersionString := '';
end;

procedure TSpecDependency.ToYAML(const parent: IYAMLValue; const packageKind: TDPMPackageKind);
var
  mapping : IYAMLMapping;
begin
  mapping := parent.AsSequence.AddMapping;
  mapping.S['id'] := FId;
  //preserve an unresolved $version$ token (editor round-trip); once resolved at pack time
  //FVersionString is cleared and FVersion holds the concrete version.
  if FVersionString = cVersionToken then
    mapping.S['version'] := FVersionString
  else
    mapping.S['version'] := FVersion.ToString;
end;

end.

