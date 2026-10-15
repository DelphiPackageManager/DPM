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

unit DPM.Core.SBOM.ProductInfo;

{ Product metadata for the SBOM root - who supplies the product, under which licence, which
  version, and who produced the SBOM. The CRA (Annex II) and BSI TR-03183-2 (5.2.1, 5.2.2) expect
  all of these, and none of them can be read from a build.

  The values come from three layers, most specific first : the sbom command's options, the
  <project>.sbom.yaml sidecar next to the .dproj / .groupproj (read by the CLI and the IDE alike),
  and the project's own version info. }

interface

uses
  DPM.Core.Logging,
  DPM.Core.SBOM.ComponentRules;

type
  ///<summary>Any field may be empty - an empty field is "not known here", and Merge fills it from
  /// the next layer down.</summary>
  TSBOMProductInfo = record
    Version : string;
    Supplier : string;
    SupplierUrl : string;   // a URL, or an email address
    License : string;       // SPDX identifier or expression, or a licence name
    SbomCreator : string;   // email address or http(s) URL of whoever produced the SBOM
    ///<summary>Field by field : this record's value where it has one, otherwise fallback's.</summary>
    function Merge(const fallback : TSBOMProductInfo) : TSBOMProductInfo;
  end;

///<summary>The sidecar for a project : MyApp.dproj -> MyApp.sbom.yaml in the same folder.</summary>
function SBOMSidecarFileName(const projectPath : string) : string;

///<summary>Reads the sidecar next to projectPath : the product metadata into info, and its
/// components:, ownUnits and ownUnitPrefixes into rules (skipped when rules is nil).
/// Returns true when the project has no sidecar (info and rules stay empty) or it was read.
/// Returns false, having logged an error, when the sidecar exists but cannot be parsed - the
/// caller should not emit an SBOM that silently lacks what the user asked for.</summary>
function LoadSBOMSidecar(const logger : ILogger; const projectPath : string;
                         out info : TSBOMProductInfo; const rules : ISBOMComponentRules) : boolean;

///<summary>A plain address - local@domain.tld, no spaces, no scheme. Not RFC 5322, just enough to
/// tell an email address apart from a URL or a company name.</summary>
function IsEmailAddress(const value : string) : boolean;

function IsHttpUrl(const value : string) : boolean;

///<summary>The gaps in the product metadata that the CRA (Annex II) and BSI TR-03183-2 (5.2.1,
/// 5.2.2) expect filled. Each warning names the sidecar key that fills it. versionIsExplicit is
/// true when the version came from the options or the sidecar rather than the version info -
/// 1.0.0.0 is only suspicious when it is the Delphi default for a new project.</summary>
function GetProductInfoWarnings(const info : TSBOMProductInfo; const versionIsExplicit : boolean;
                                const sidecarName : string) : TArray<string>;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  VSoft.YAML;

const
  cSidecarExt = '.sbom.yaml';

{ TSBOMProductInfo }

function TSBOMProductInfo.Merge(const fallback : TSBOMProductInfo) : TSBOMProductInfo;

  function Pick(const preferred, other : string) : string;
  begin
    if preferred <> '' then
      result := preferred
    else
      result := other;
  end;

begin
  result.Version := Pick(Version, fallback.Version);
  result.Supplier := Pick(Supplier, fallback.Supplier);
  result.SupplierUrl := Pick(SupplierUrl, fallback.SupplierUrl);
  result.License := Pick(License, fallback.License);
  result.SbomCreator := Pick(SbomCreator, fallback.SbomCreator);
end;

function SBOMSidecarFileName(const projectPath : string) : string;
begin
  result := ChangeFileExt(projectPath, cSidecarExt);
end;

function IsEmailAddress(const value : string) : boolean;
var
  atPos : integer;
  domain : string;
begin
  result := false;
  if (value = '') or (Pos(' ', value) > 0) or (Pos('://', value) > 0) then
    exit;
  atPos := Pos('@', value);
  if (atPos <= 1) or (PosEx('@', value, atPos + 1) > 0) then
    exit;
  domain := Copy(value, atPos + 1, MaxInt);
  //a dot inside the domain, not at either end of it
  result := (Pos('.', domain) > 1) and (domain[Length(domain)] <> '.');
end;

function IsHttpUrl(const value : string) : boolean;
begin
  result := ((Length(value) > Length('http://')) and StartsText('http://', value)) or
            ((Length(value) > Length('https://')) and StartsText('https://', value));
end;

function GetProductInfoWarnings(const info : TSBOMProductInfo; const versionIsExplicit : boolean;
                                const sidecarName : string) : TArray<string>;

  procedure Add(const text : string);
  begin
    SetLength(result, Length(result) + 1);
    result[Length(result) - 1] := 'CRA/BSI: ' + text;
  end;

begin
  result := nil;
  if info.Supplier = '' then
    Add('the SBOM names no supplier for the product (CRA Annex II 1, BSI TR-03183-2 5.2.2). ' +
        'Set supplier: in ' + sidecarName + ', pass --supplier, or fill CompanyName in the project version info.')
  else if (info.SupplierUrl = '') and not IsEmailAddress(info.Supplier) then
    Add('the supplier [' + info.Supplier + '] has no contact - CRA Annex II 1 asks for an email address or other ' +
        'digital contact, BSI TR-03183-2 5.2.2 for an email address or URL. Set supplierUrl: in ' + sidecarName +
        ' or pass --supplier-url.');

  if info.SbomCreator = '' then
    Add('the SBOM does not say who created it (BSI TR-03183-2 5.2.1). Set sbomCreator: in ' + sidecarName +
        ' to an email address or URL, or pass --sbom-creator.');

  if info.Version = '' then
    Add('the product has no version (CRA Annex II 3, BSI TR-03183-2 5.2.2). Set version: in ' + sidecarName +
        ', pass --version, or include version info in the project.')
  else if (info.Version = '1.0.0.0') and not versionIsExplicit then
    Add('the product version is 1.0.0.0, the default for a new Delphi project. If that is not the real release, ' +
        'set version: in ' + sidecarName + ', pass --version, or update the project version info.');

  if info.License = '' then
    Add('the product has no distribution licence (BSI TR-03183-2 5.2.2). Set license: in ' + sidecarName +
        ' to an SPDX identifier or expression, or pass --license.');
end;

//RawValue rather than AsString for anything but a string - 'version: 2.1' is a float to YAML, but
//we want its text
function ScalarText(const value : IYAMLValue) : string;
begin
  if value.IsString then
    result := Trim(value.AsString)
  else
    result := Trim(value.RawValue);
end;

//A list of names - a sequence, or a single scalar for a list of one.
function ScalarList(const value : IYAMLValue) : TArray<string>;
var
  sequence : IYAMLSequence;
  i : integer;
begin
  result := nil;
  if (value = nil) or value.IsNull then
    exit;
  if value.IsScalar then
  begin
    SetLength(result, 1);
    result[0] := ScalarText(value);
    exit;
  end;
  if not value.IsSequence then
    exit;
  sequence := value.AsSequence;
  for i := 0 to sequence.Count - 1 do
  begin
    if (sequence[i] = nil) or not sequence[i].IsScalar or (ScalarText(sequence[i]) = '') then
      continue;
    SetLength(result, Length(result) + 1);
    result[Length(result) - 1] := ScalarText(sequence[i]);
  end;
end;

//One entry of the components: list - a library's identity, and the units that belong to it.
procedure ReadSidecarComponent(const logger : ILogger; const context : string; const row : IYAMLMapping;
                               const rules : ISBOMComponentRules);
var
  rule : TSBOMComponentRule;
  i : integer;
  key : string;
  value : IYAMLValue;
begin
  for i := 0 to row.Count - 1 do
  begin
    key := row.Keys[i];
    value := row.Values[key];
    if (value = nil) or value.IsNull then
      continue;
    if SameText(key, 'units') then
      rule.Units := ScalarList(value)
    else if SameText(key, 'unitPrefixes') then
      rule.UnitPrefixes := ScalarList(value)
    else if not value.IsScalar then
      logger.Warning('[SBOM] ' + context + ' : ignoring [' + key + '] - expected a single value')
    else if SameText(key, 'name') then
      rule.Name := ScalarText(value)
    else if SameText(key, 'version') then
      rule.Version := ScalarText(value)
    else if SameText(key, 'supplier') then
      rule.Supplier := ScalarText(value)
    else if SameText(key, 'supplierUrl') then
      rule.SupplierUrl := ScalarText(value)
    else if SameText(key, 'license') then
      rule.License := ScalarText(value)
    else if SameText(key, 'purl') then
      rule.Purl := ScalarText(value)
    else if SameText(key, 'type') then
      rule.ComponentType := ScalarText(value)
    else if not SameText(key, 'notes') then
      logger.Warning('[SBOM] ' + context + ' : ignoring unknown key [' + key + '] - expected name, version, supplier, ' +
                     'supplierUrl, license, purl, type, units or unitPrefixes');
  end;
  if ValidateComponentRule(logger, context, rule) then
    rules.Add(rule);
end;

function LoadSBOMSidecar(const logger : ILogger; const projectPath : string;
                         out info : TSBOMProductInfo; const rules : ISBOMComponentRules) : boolean;
var
  fileName : string;
  doc : IYAMLDocument;
  mapping : IYAMLMapping;
  components : IYAMLSequence;
  i : integer;
  j : integer;
  key : string;
  value : IYAMLValue;
  name : string;
begin
  info.Version := '';
  info.Supplier := '';
  info.SupplierUrl := '';
  info.License := '';
  info.SbomCreator := '';
  result := false;

  fileName := SBOMSidecarFileName(projectPath);
  if FileExists(fileName) then
  begin
    try
      doc := TYAML.LoadFromFile(fileName);
    except
      on e : Exception do
      begin
        logger.Error('[SBOM] could not read [' + fileName + '] : ' + e.Message);
        exit;
      end;
    end;

    //an empty file is a sidecar with nothing in it yet
    if (doc <> nil) and (doc.Root <> nil) and not doc.Root.IsNull then
    begin
      if not doc.Root.IsMapping then
      begin
        logger.Error('[SBOM] [' + fileName + '] must be a mapping of key: value pairs');
        exit;
      end;

      logger.Verbose('[SBOM] reading product metadata from ' + fileName);
      mapping := doc.Root.AsMapping;
      for i := 0 to mapping.Count - 1 do
      begin
        key := mapping.Keys[i];
        value := mapping.Values[key];
        if (value = nil) or value.IsNull then
          continue;

        if SameText(key, 'components') then
        begin
          if not value.IsSequence then
          begin
            logger.Warning('[SBOM] ignoring [components] in [' + fileName + '] - expected a list');
            continue;
          end;
          if rules = nil then
            continue;
          components := value.AsSequence;
          for j := 0 to components.Count - 1 do
          begin
            if (components[j] = nil) or not components[j].IsMapping then
            begin
              logger.Warning('[SBOM] ' + ExtractFileName(fileName) + ' component ' + IntToStr(j) + ' is not a mapping and was skipped');
              continue;
            end;
            ReadSidecarComponent(logger, ExtractFileName(fileName) + ' component ' + IntToStr(j), components[j].AsMapping, rules);
          end;
          continue;
        end;
        if SameText(key, 'ownUnits') or SameText(key, 'ownUnitPrefixes') then
        begin
          if rules <> nil then
            for name in ScalarList(value) do
              if SameText(key, 'ownUnits') then
                rules.AddOwnUnit(name)
              else
                rules.AddOwnUnitPrefix(name);
          continue;
        end;

        if not value.IsScalar then
        begin
          logger.Warning('[SBOM] ignoring [' + key + '] in [' + fileName + '] - expected a single value');
          continue;
        end;
        if SameText(key, 'version') then
          info.Version := ScalarText(value)
        else if SameText(key, 'supplier') then
          info.Supplier := ScalarText(value)
        else if SameText(key, 'supplierUrl') then
          info.SupplierUrl := ScalarText(value)
        else if SameText(key, 'license') then
          info.License := ScalarText(value)
        else if SameText(key, 'sbomCreator') then
          info.SbomCreator := ScalarText(value)
        else
          logger.Warning('[SBOM] ignoring unknown key [' + key + '] in [' + fileName + '] - expected version, supplier, ' +
                         'supplierUrl, license, sbomCreator, components, ownUnits or ownUnitPrefixes');
      end;
    end;
  end;
  result := true;
end;

end.
