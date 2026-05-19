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

unit DPM.Core.SBOM.Writers;

{ CycloneDX 1.5 JSON and SPDX 2.3 JSON writers.

  Both formats are described at https://cyclonedx.org/specification/overview/
  and https://spdx.github.io/spdx-spec/v2.3/. JSON emission uses
  JsonDataObjects throughout (the rest of DPM uses it; System.JSON has
  known bugs on the older Delphi versions DPM still supports). }

interface

uses
  DPM.Core.Options.Sbom,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Types;

type
  TCycloneDXWriter = class(TInterfacedObject, ISbomWriter)
  protected
    function GetFormat : TSBOMFormat;
    function GetFileExtension : string;
    procedure Write(const report : TSBOMReport; const fileName : string);
  end;

  TSPDXWriter = class(TInterfacedObject, ISbomWriter)
  protected
    function GetFormat : TSBOMFormat;
    function GetFileExtension : string;
    procedure Write(const report : TSBOMReport; const fileName : string);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Generics.Collections,
  JsonDataObjects,
  DPM.Core.Types;

const
  cNoAssertion = 'NOASSERTION';

//Normalises a hash algorithm name to the CycloneDX hashAlg enum. CycloneDX 1.5 hashes[].alg
//must be one of MD5, SHA-1, SHA-256, SHA-384, SHA-512, SHA3-256, ... — exact spelling matters
//for downstream validation.
function CycloneDXHashAlg(const value : string) : string;
var
  v : string;
begin
  v := UpperCase(StringReplace(value, '-', '', [rfReplaceAll]));
  if v = 'MD5' then
    result := 'MD5'
  else if v = 'SHA1' then
    result := 'SHA-1'
  else if v = 'SHA256' then
    result := 'SHA-256'
  else if v = 'SHA384' then
    result := 'SHA-384'
  else if v = 'SHA512' then
    result := 'SHA-512'
  else if v = 'SHA3256' then
    result := 'SHA3-256'
  else if v = 'SHA3512' then
    result := 'SHA3-512'
  else
    result := 'SHA-256';
end;

function SpdxChecksumAlg(const value : string) : string;
var
  v : string;
begin
  v := UpperCase(StringReplace(value, '-', '', [rfReplaceAll]));
  if v = 'SHA256' then
    result := 'SHA256'
  else if v = 'SHA1' then
    result := 'SHA1'
  else if v = 'SHA384' then
    result := 'SHA384'
  else if v = 'SHA512' then
    result := 'SHA512'
  else if v = 'MD5' then
    result := 'MD5'
  else
    result := 'SHA256';
end;

//Heuristic: does this look like an SPDX licence expression / id? We accept a tight character
//set (alphanumerics, dot, plus, hyphen, and the keywords AND / OR / WITH plus parens / spaces).
//This is a fallback for cases where the spec carries a free-form licence string. When in doubt
//we emit it as a name (CycloneDX licenses[].license.name) instead of an id.
function LooksLikeSpdxExpression(const value : string) : boolean;
const
  cAllowed = ['A'..'Z','a'..'z','0'..'9','.','+','-','(',')',' '];
var
  i : integer;
  c : char;
begin
  result := false;
  if Trim(value) = '' then
    exit;
  for i := 1 to Length(value) do
  begin
    c := value[i];
    if not CharInSet(c, cAllowed) then
      exit;
  end;
  result := true;
end;

function Slugify(const value : string) : string;
var
  i : integer;
  c : char;
  sb : TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    for i := 1 to Length(value) do
    begin
      c := value[i];
      if CharInSet(c, ['A'..'Z','a'..'z','0'..'9','.','-']) then
        sb.Append(c)
      else
        sb.Append('-');
    end;
    result := sb.ToString;
    if result = '' then
      result := 'x';
  finally
    sb.Free;
  end;
end;

procedure WriteUtf8(const fileName, content : string);
var
  bytes : TBytes;
  fs : TFileStream;
begin
  bytes := TEncoding.UTF8.GetBytes(content);
  fs := TFileStream.Create(fileName, fmCreate);
  try
    if Length(bytes) > 0 then
      fs.WriteBuffer(bytes[0], Length(bytes));
  finally
    fs.Free;
  end;
end;

//Set a string property only when there's something to say. Spec readers tolerate missing
//optional fields better than empty strings (which often fail schema "minLength" assertions).
procedure SetStringIfNotEmpty(const obj : TJsonObject; const name, value : string);
begin
  if value <> '' then
    obj.S[name] := value;
end;

{ TCycloneDXWriter }

function TCycloneDXWriter.GetFormat : TSBOMFormat;
begin
  result := TSBOMFormat.CycloneDX;
end;

function TCycloneDXWriter.GetFileExtension : string;
begin
  result := '.cdx.json';
end;

procedure TCycloneDXWriter.Write(const report : TSBOMReport; const fileName : string);

  procedure FillLicenses(const componentObj : TJsonObject; const license : string);
  var
    licArr : TJsonArray;
    licWrap : TJsonObject;
    inner : TJsonObject;
  begin
    if license = '' then
      exit;
    licArr := componentObj.A['licenses'];
    licWrap := licArr.AddObject;
    inner := licWrap.O['license'];
    if LooksLikeSpdxExpression(license) then
      inner.S['id'] := license
    else
      inner.S['name'] := license;
  end;

  procedure FillComponent(const obj : TJsonObject; const comp : TSBOMComponent);
  var
    typeName : string;
    authorsArr : TJsonArray;
    author : string;
    hashesArr : TJsonArray;
    hashObj : TJsonObject;
    extRefs : TJsonArray;
    extObj : TJsonObject;
    propsArr : TJsonArray;
    propObj : TJsonObject;
    prop : TSBOMProperty;
    tagsArr : TJsonArray;
    tag : string;
    evidenceObj : TJsonObject;
    occurArr : TJsonArray;
    ev : TSBOMEvidence;
  begin
    case comp.Kind of
      TSBOMComponentKind.Application : typeName := 'application';
      TSBOMComponentKind.DelphiRuntime : typeName := 'framework';
    else
      typeName := 'library';
    end;
    obj.S['type'] := typeName;
    SetStringIfNotEmpty(obj, 'bom-ref', comp.BomRef);
    SetStringIfNotEmpty(obj, 'name', comp.Id);
    SetStringIfNotEmpty(obj, 'version', comp.Version);
    SetStringIfNotEmpty(obj, 'description', comp.Description);

    if comp.Authors.Count > 0 then
    begin
      authorsArr := obj.A['authors'];
      for author in comp.Authors do
      begin
        if Trim(author) = '' then
          continue;
        authorsArr.AddObject.S['name'] := author;
      end;
    end;

    if comp.Supplier <> '' then
      obj.O['supplier'].S['name'] := comp.Supplier;

    FillLicenses(obj, comp.License);
    SetStringIfNotEmpty(obj, 'copyright', comp.Copyright);

    if (comp.HashValue <> '') and (comp.HashAlgorithm <> '') then
    begin
      hashesArr := obj.A['hashes'];
      hashObj := hashesArr.AddObject;
      hashObj.S['alg'] := CycloneDXHashAlg(comp.HashAlgorithm);
      hashObj.S['content'] := comp.HashValue;
    end;

    SetStringIfNotEmpty(obj, 'purl', comp.Purl);

    if (comp.ProjectUrl <> '') or (comp.RepositoryUrl <> '') or (comp.DownloadUrl <> '') then
    begin
      extRefs := obj.A['externalReferences'];
      if comp.ProjectUrl <> '' then
      begin
        extObj := extRefs.AddObject;
        extObj.S['type'] := 'website';
        extObj.S['url'] := comp.ProjectUrl;
      end;
      if comp.RepositoryUrl <> '' then
      begin
        extObj := extRefs.AddObject;
        extObj.S['type'] := 'vcs';
        extObj.S['url'] := comp.RepositoryUrl;
        if (comp.RepositoryBranch <> '') or (comp.RepositoryCommit <> '') then
          extObj.S['comment'] := Trim(comp.RepositoryBranch + ' ' + comp.RepositoryCommit);
      end;
      if comp.DownloadUrl <> '' then
      begin
        extObj := extRefs.AddObject;
        extObj.S['type'] := 'distribution';
        extObj.S['url'] := comp.DownloadUrl;
      end;
    end;

    if comp.Tags.Count > 0 then
    begin
      tagsArr := obj.A['tags'];
      for tag in comp.Tags do
        if Trim(tag) <> '' then
          tagsArr.Add(tag);
    end;

    if comp.Properties.Count > 0 then
    begin
      propsArr := obj.A['properties'];
      for prop in comp.Properties do
      begin
        propObj := propsArr.AddObject;
        propObj.S['name'] := prop.Name;
        propObj.S['value'] := prop.Value;
      end;
    end;

    if comp.Evidence.Count > 0 then
    begin
      evidenceObj := obj.O['evidence'];
      occurArr := evidenceObj.A['occurrences'];
      for ev in comp.Evidence do
        occurArr.AddObject.S['location'] := ev.Location;
    end;
  end;

var
  root : TJsonObject;
  meta : TJsonObject;
  toolsArr : TJsonArray;
  toolObj : TJsonObject;
  metaPropsArr : TJsonArray;
  metaPropObj : TJsonObject;
  metaProp : TSBOMProperty;
  componentsArr : TJsonArray;
  comp : TSBOMComponent;
  depsArr : TJsonArray;
  depMap : TDictionary<string, TJsonArray>;
  refList : TJsonArray;
  rel : TSBOMRelationship;
  refObj : TJsonObject;
  parent : string;
  jsonText : string;
begin
  root := TJsonObject.Create;
  try
    depMap := TDictionary<string, TJsonArray>.Create;
    try
      root.S['bomFormat'] := 'CycloneDX';
      root.S['specVersion'] := '1.5';
      root.I['version'] := 1;
      SetStringIfNotEmpty(root, 'serialNumber', report.SerialNumber);

      meta := root.O['metadata'];
      SetStringIfNotEmpty(meta, 'timestamp', report.TimestampUtc);

      toolsArr := meta.A['tools'];
      toolObj := toolsArr.AddObject;
      toolObj.S['vendor'] := 'DPM';
      toolObj.S['name'] := report.ToolName;
      toolObj.S['version'] := report.ToolVersion;

      FillComponent(meta.O['component'], report.RootComponent);

      if report.MetaProperties.Count > 0 then
      begin
        metaPropsArr := meta.A['properties'];
        for metaProp in report.MetaProperties do
        begin
          metaPropObj := metaPropsArr.AddObject;
          metaPropObj.S['name'] := metaProp.Name;
          metaPropObj.S['value'] := metaProp.Value;
        end;
      end;

      componentsArr := root.A['components'];
      for comp in report.Components do
        FillComponent(componentsArr.AddObject, comp);

      //CycloneDX dependencies[] is a flat array of {ref, dependsOn[]} entries - one per parent.
      //We group child edges by parent here so the writer doesn't have to be order-sensitive.
      if report.Relationships.Count > 0 then
      begin
        depsArr := root.A['dependencies'];
        for rel in report.Relationships do
        begin
          parent := rel.ParentBomRef;
          if parent = '' then
            continue;
          if not depMap.TryGetValue(parent, refList) then
          begin
            refObj := depsArr.AddObject;
            refObj.S['ref'] := parent;
            refList := refObj.A['dependsOn'];
            depMap.Add(parent, refList);
          end;
          refList.Add(rel.ChildBomRef);
        end;
      end;

      jsonText := root.ToJSON(false);
      WriteUtf8(fileName, jsonText);
    finally
      depMap.Free;
    end;
  finally
    root.Free;
  end;
end;

{ TSPDXWriter }

function TSPDXWriter.GetFormat : TSBOMFormat;
begin
  result := TSBOMFormat.SPDX;
end;

function TSPDXWriter.GetFileExtension : string;
begin
  result := '.spdx.json';
end;

procedure TSPDXWriter.Write(const report : TSBOMReport; const fileName : string);

  function CoalesceNoAssertion(const value : string) : string;
  begin
    if Trim(value) = '' then
      result := cNoAssertion
    else
      result := value;
  end;

  function SpdxIdFor(const comp : TSBOMComponent;
                     const usedIds : TDictionary<string, integer>) : string;
  var
    candidate : string;
    next : integer;
  begin
    candidate := 'SPDXRef-Package-' + Slugify(comp.Id) + '-' + Slugify(comp.Version);
    if not usedIds.ContainsKey(candidate) then
    begin
      usedIds.Add(candidate, 1);
      result := candidate;
      exit;
    end;
    next := usedIds[candidate] + 1;
    usedIds[candidate] := next;
    result := candidate + '-' + IntToStr(next);
  end;

  procedure FillPackage(const obj : TJsonObject; const comp : TSBOMComponent; const spdxId : string);
  var
    checksumsArr : TJsonArray;
    checksumObj : TJsonObject;
    extRefs : TJsonArray;
    extObj : TJsonObject;
    suppLine : string;
    annArr : TJsonArray;
    annObj : TJsonObject;
    prop : TSBOMProperty;
    ev : TSBOMEvidence;
    tag : string;

    procedure AddAnnotation(const comment : string);
    begin
      annObj := annArr.AddObject;
      annObj.S['annotator'] := 'Tool: dpm';
      annObj.S['annotationDate'] := report.TimestampUtc;
      annObj.S['annotationType'] := 'OTHER';
      annObj.S['comment'] := comment;
    end;

  begin
    obj.S['SPDXID'] := spdxId;
    obj.S['name'] := comp.Id;
    SetStringIfNotEmpty(obj, 'versionInfo', comp.Version);

    if comp.Supplier <> '' then
      suppLine := 'Organization: ' + comp.Supplier
    else if comp.Authors.Count > 0 then
      suppLine := 'Organization: ' + String.Join(', ', comp.Authors.ToArray)
    else
      suppLine := cNoAssertion;
    obj.S['supplier'] := suppLine;

    obj.S['downloadLocation'] := CoalesceNoAssertion(comp.DownloadUrl);
    obj.B['filesAnalyzed'] := false;

    if (comp.License <> '') and LooksLikeSpdxExpression(comp.License) then
      obj.S['licenseDeclared'] := comp.License
    else
      obj.S['licenseDeclared'] := cNoAssertion;
    obj.S['licenseConcluded'] := cNoAssertion;

    obj.S['copyrightText'] := CoalesceNoAssertion(comp.Copyright);
    SetStringIfNotEmpty(obj, 'description', comp.Description);
    SetStringIfNotEmpty(obj, 'homepage', comp.ProjectUrl);

    if (comp.HashValue <> '') and (comp.HashAlgorithm <> '') then
    begin
      checksumsArr := obj.A['checksums'];
      checksumObj := checksumsArr.AddObject;
      checksumObj.S['algorithm'] := SpdxChecksumAlg(comp.HashAlgorithm);
      checksumObj.S['checksumValue'] := comp.HashValue;
    end;

    if comp.Purl <> '' then
    begin
      extRefs := obj.A['externalRefs'];
      extObj := extRefs.AddObject;
      extObj.S['referenceCategory'] := 'PACKAGE-MANAGER';
      extObj.S['referenceType'] := 'purl';
      extObj.S['referenceLocator'] := comp.Purl;
    end;

    //SPDX has no first-class properties bag like CycloneDX, so DPM-specific fields, tags and
    //evidence locations all become annotations[] entries with annotationType=OTHER.
    if (comp.Properties.Count > 0) or (comp.Tags.Count > 0) or (comp.Evidence.Count > 0) then
    begin
      annArr := obj.A['annotations'];
      for prop in comp.Properties do
        AddAnnotation(prop.Name + '=' + prop.Value);
      for tag in comp.Tags do
        if Trim(tag) <> '' then
          AddAnnotation('tag=' + tag);
      for ev in comp.Evidence do
        AddAnnotation('evidence=' + ev.Location);
    end;
  end;

const
  cDocSpdxId = 'SPDXRef-DOCUMENT';
var
  root : TJsonObject;
  creationInfo : TJsonObject;
  creatorsArr : TJsonArray;
  uuidPart : string;
  packagesArr : TJsonArray;
  relsArr : TJsonArray;
  relObj : TJsonObject;
  rootSpdxId : string;
  bomRefToSpdxId : TDictionary<string, string>;
  usedIds : TDictionary<string, integer>;
  comp : TSBOMComponent;
  spdxId : string;
  rel : TSBOMRelationship;
  parentSpdx : string;
  childSpdx : string;
  jsonText : string;
begin
  root := TJsonObject.Create;
  bomRefToSpdxId := TDictionary<string, string>.Create;
  usedIds := TDictionary<string, integer>.Create;
  try
    root.S['spdxVersion'] := 'SPDX-2.3';
    root.S['dataLicense'] := 'CC0-1.0';
    root.S['SPDXID'] := cDocSpdxId;
    root.S['name'] := report.ProjectName + '-' + DPMPlatformToString(report.Platform);

    uuidPart := report.SerialNumber;
    if StartsText('urn:uuid:', uuidPart) then
      Delete(uuidPart, 1, Length('urn:uuid:'));
    if uuidPart = '' then
      uuidPart := 'unknown';
    root.S['documentNamespace'] :=
      'https://dpm-cli/' + report.ProjectName + '/' + DPMPlatformToString(report.Platform) + '/' + uuidPart;

    creationInfo := root.O['creationInfo'];
    creationInfo.S['created'] := report.TimestampUtc;
    creatorsArr := creationInfo.A['creators'];
    creatorsArr.Add('Tool: ' + report.ToolName + '-' + report.ToolVersion);

    //Root component (the project) is a Package too. Without it the SPDX DOCUMENT DESCRIBES
    //relationship has nothing to point at.
    rootSpdxId := SpdxIdFor(report.RootComponent, usedIds);
    bomRefToSpdxId.Add(report.RootComponent.BomRef, rootSpdxId);

    packagesArr := root.A['packages'];
    FillPackage(packagesArr.AddObject, report.RootComponent, rootSpdxId);

    for comp in report.Components do
    begin
      spdxId := SpdxIdFor(comp, usedIds);
      bomRefToSpdxId.Add(comp.BomRef, spdxId);
      FillPackage(packagesArr.AddObject, comp, spdxId);
    end;

    relsArr := root.A['relationships'];

    //Mandatory: DOCUMENT DESCRIBES root
    relObj := relsArr.AddObject;
    relObj.S['spdxElementId'] := cDocSpdxId;
    relObj.S['relationshipType'] := 'DESCRIBES';
    relObj.S['relatedSpdxElement'] := rootSpdxId;

    for rel in report.Relationships do
    begin
      if not bomRefToSpdxId.TryGetValue(rel.ParentBomRef, parentSpdx) then
        continue;
      if not bomRefToSpdxId.TryGetValue(rel.ChildBomRef, childSpdx) then
        continue;
      relObj := relsArr.AddObject;
      relObj.S['spdxElementId'] := parentSpdx;
      relObj.S['relationshipType'] := 'DEPENDS_ON';
      relObj.S['relatedSpdxElement'] := childSpdx;
    end;

    jsonText := root.ToJSON(false);
    WriteUtf8(fileName, jsonText);
  finally
    bomRefToSpdxId.Free;
    usedIds.Free;
    root.Free;
  end;
end;

end.
