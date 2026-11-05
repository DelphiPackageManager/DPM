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

  {$SCOPEDENUMS ON}
  ///<summary>A gap in the product metadata that the CRA / BSI TR-03183-2 expect filled.
  /// NoSupplierContact is only reported for a supplier that is there; DefaultVersion is a version
  /// of 1.0.0.0 that came from the project's version info.</summary>
  TSBOMProductInfoGap = (NoSupplier, NoSupplierContact, NoSbomCreator, NoVersion, DefaultVersion, NoLicense);
  TSBOMProductInfoGaps = set of TSBOMProductInfoGap;
  {$SCOPEDENUMS OFF}

///<summary>The sidecar for a project : MyApp.dproj -> MyApp.sbom.yaml in the same folder.</summary>
function SBOMSidecarFileName(const projectPath : string) : string;

///<summary>Reads the sidecar next to projectPath : the product metadata into info, and its
/// components:, ownUnits and ownUnitPrefixes into rules (skipped when rules is nil).
/// Returns true when the project has no sidecar (info and rules stay empty) or it was read.
/// Returns false, having logged an error, when the sidecar exists but cannot be parsed - the
/// caller should not emit an SBOM that silently lacks what the user asked for.</summary>
function LoadSBOMSidecar(const logger : ILogger; const projectPath : string;
                         out info : TSBOMProductInfo; const rules : ISBOMComponentRules) : boolean;

///<summary>Writes info's five product fields into the sidecar next to projectPath, keeping every
/// other key (components, ownUnits, ownUnitPrefixes, anything unknown). A blank field is written as
/// null where the key already exists and left out where it does not. Creates the file when there is
/// none and info has at least one value. Leaves the file untouched when nothing changes.
/// Returns false, having logged an error, when the existing sidecar cannot be read or the file
/// cannot be written - an unreadable sidecar is never overwritten.</summary>
function SaveSBOMSidecar(const logger : ILogger; const projectPath : string;
                         const info : TSBOMProductInfo) : boolean;

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

///<summary>The same gaps GetProductInfoWarnings reports, as a set - for a caller that words them its
/// own way, such as the IDE's SBOM dialog.</summary>
function GetProductInfoGaps(const info : TSBOMProductInfo; const versionIsExplicit : boolean) : TSBOMProductInfoGaps;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  VSoft.YAML,
  DPM.Core.Diagnostics;

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

var
  gaps : TSBOMProductInfoGaps;
begin
  result := nil;
  gaps := GetProductInfoGaps(info, versionIsExplicit);
  if TSBOMProductInfoGap.NoSupplier in gaps then
    Add('the SBOM names no supplier for the product (CRA Annex II 1, BSI TR-03183-2 5.2.2). ' +
        'Set supplier: in ' + sidecarName + ', pass --supplier, or fill CompanyName in the project version info.');
  if TSBOMProductInfoGap.NoSupplierContact in gaps then
    Add('the supplier [' + info.Supplier + '] has no contact - CRA Annex II 1 asks for an email address or other ' +
        'digital contact, BSI TR-03183-2 5.2.2 for an email address or URL. Set supplierUrl: in ' + sidecarName +
        ' or pass --supplier-url.');

  if TSBOMProductInfoGap.NoSbomCreator in gaps then
    Add('the SBOM does not say who created it (BSI TR-03183-2 5.2.1). Set sbomCreator: in ' + sidecarName +
        ' to an email address or URL, or pass --sbom-creator.');

  if TSBOMProductInfoGap.NoVersion in gaps then
    Add('the product has no version (CRA Annex II 3, BSI TR-03183-2 5.2.2). Set version: in ' + sidecarName +
        ', pass --version, or include version info in the project.');
  if TSBOMProductInfoGap.DefaultVersion in gaps then
    Add('the product version is 1.0.0.0, the default for a new Delphi project. If that is not the real release, ' +
        'set version: in ' + sidecarName + ', pass --version, or update the project version info.');

  if TSBOMProductInfoGap.NoLicense in gaps then
    Add('the product has no distribution licence (BSI TR-03183-2 5.2.2). Set license: in ' + sidecarName +
        ' to an SPDX identifier or expression, or pass --license.');
end;

function GetProductInfoGaps(const info : TSBOMProductInfo; const versionIsExplicit : boolean) : TSBOMProductInfoGaps;
begin
  result := [];
  if info.Supplier = '' then
    Include(result, TSBOMProductInfoGap.NoSupplier)
  //an email address is a contact in itself
  else if (info.SupplierUrl = '') and not IsEmailAddress(info.Supplier) then
    Include(result, TSBOMProductInfoGap.NoSupplierContact);

  if info.SbomCreator = '' then
    Include(result, TSBOMProductInfoGap.NoSbomCreator);

  if info.Version = '' then
    Include(result, TSBOMProductInfoGap.NoVersion)
  //a new Delphi project starts at 1.0.0.0, so from the version info it is probably not the real release
  else if (info.Version = '1.0.0.0') and not versionIsExplicit then
    Include(result, TSBOMProductInfoGap.DefaultVersion);

  if info.License = '' then
    Include(result, TSBOMProductInfoGap.NoLicense);
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
      logger.Warning(TWarningCode.W8103_SBOMProductInfoIgnored, '[SBOM] ' + context + ' : ignoring [' + key + '] - expected a single value')
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
      logger.Warning(TWarningCode.W8103_SBOMProductInfoIgnored, '[SBOM] ' + context + ' : ignoring unknown key [' + key + '] - expected name, version, supplier, ' +
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
        logger.Error(TErrorCode.E8102_SBOMProductInfoUnreadable, '[SBOM] could not read [' + fileName + '] : ' + e.Message);
        exit;
      end;
    end;

    //an empty file is a sidecar with nothing in it yet
    if (doc <> nil) and (doc.Root <> nil) and not doc.Root.IsNull then
    begin
      if not doc.Root.IsMapping then
      begin
        logger.Error(TErrorCode.E8101_SBOMProductInfoInvalid, '[SBOM] [' + fileName + '] must be a mapping of key: value pairs');
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
            logger.Warning(TWarningCode.W8103_SBOMProductInfoIgnored, '[SBOM] ignoring [components] in [' + fileName + '] - expected a list');
            continue;
          end;
          if rules = nil then
            continue;
          components := value.AsSequence;
          for j := 0 to components.Count - 1 do
          begin
            if (components[j] = nil) or not components[j].IsMapping then
            begin
              logger.Warning(TWarningCode.W8104_SBOMComponentRuleInvalid, '[SBOM] ' + ExtractFileName(fileName) + ' component ' + IntToStr(j) + ' is not a mapping and was skipped');
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
          logger.Warning(TWarningCode.W8103_SBOMProductInfoIgnored, '[SBOM] ignoring [' + key + '] in [' + fileName + '] - expected a single value');
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
          logger.Warning(TWarningCode.W8103_SBOMProductInfoIgnored, '[SBOM] ignoring unknown key [' + key + '] in [' + fileName + '] - expected version, supplier, ' +
                         'supplierUrl, license, sbomCreator, components, ownUnits or ownUnitPrefixes');
      end;
    end;
  end;
  result := true;
end;

//The key's spelling in mapping when it is there in any case - the reader matches keys ignoring case,
//so 'Supplier:' is updated rather than joined by a second 'supplier:'. '' when it is not there.
function FindKey(const mapping : IYAMLMapping; const key : string) : string;
var
  i : integer;
begin
  result := '';
  for i := 0 to mapping.Count - 1 do
    if SameText(mapping.Keys[i], key) then
    begin
      result := mapping.Keys[i];
      exit;
    end;
end;

function SaveSBOMSidecar(const logger : ILogger; const projectPath : string;
                         const info : TSBOMProductInfo) : boolean;
const
  cKeys : array[0..4] of string = ('version', 'supplier', 'supplierUrl', 'license', 'sbomCreator');
var
  fileName : string;
  doc : IYAMLDocument;
  mapping : IYAMLMapping;
  values : array[0..4] of string;
  i : integer;
  key : string;
  current : IYAMLValue;
  currentValue : string;
  changed : boolean;
begin
  result := false;
  values[0] := Trim(info.Version);
  values[1] := Trim(info.Supplier);
  values[2] := Trim(info.SupplierUrl);
  values[3] := Trim(info.License);
  values[4] := Trim(info.SbomCreator);

  fileName := SBOMSidecarFileName(projectPath);
  doc := nil;
  if FileExists(fileName) then
  begin
    try
      doc := TYAML.LoadFromFile(fileName);
    except
      on e : Exception do
      begin
        logger.Error(TErrorCode.E8102_SBOMProductInfoUnreadable, '[SBOM] could not read [' + fileName + '] : ' + e.Message +
                     ' - it was not changed');
        exit;
      end;
    end;
    //an empty file is a sidecar with nothing in it yet
    if (doc = nil) or (doc.Root = nil) or doc.Root.IsNull then
      doc := nil
    else if not doc.Root.IsMapping then
    begin
      logger.Error(TErrorCode.E8101_SBOMProductInfoInvalid, '[SBOM] [' + fileName + '] must be a mapping of key: value pairs - it was not changed');
      exit;
    end;
  end;
  if doc = nil then
    doc := TYAML.CreateMapping;
  mapping := doc.Root.AsMapping;

  changed := false;
  for i := Low(cKeys) to High(cKeys) do
  begin
    key := FindKey(mapping, cKeys[i]);
    if key = '' then
    begin
      //a blank value is already what an absent key means
      if values[i] <> '' then
      begin
        mapping.AddOrSetValue(cKeys[i], values[i]);
        changed := true;
      end;
      continue;
    end;

    //compared the way LoadSBOMSidecar reads it, so 'version: 2.1' (a float to YAML) matches '2.1'.
    //The reader ignores a value that isn't a single value, so that counts as blank.
    current := mapping.Values[key];
    if (current = nil) or current.IsNull or not current.IsScalar then
      currentValue := ''
    else
      currentValue := ScalarText(current);
    if currentValue = values[i] then
      continue;

    //VSoft.YAML can't remove a key; the reader treats a null as unset
    if values[i] = '' then
      mapping.AddOrSetNull(key)
    else
      mapping.AddOrSetValue(key, values[i]);
    changed := true;
  end;

  //nothing to say - leave the file (or its absence) exactly as it was
  if not changed then
  begin
    result := true;
    exit;
  end;

  doc.Options.WriteByteOrderMark := false;
  try
    TYAML.WriteToFile(doc, fileName);
  except
    on e : Exception do
    begin
      logger.Error(TErrorCode.E8117_SBOMProductInfoWriteFailed, '[SBOM] could not write [' + fileName + '] : ' + e.Message);
      exit;
    end;
  end;
  result := true;
end;

end.
