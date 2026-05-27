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

unit DPM.Core.SBOM.Reader;

{ Inverse of TCycloneDXWriter - reads a CycloneDX 1.5 JSON document back into
  a TSBOMReport. Used by dpm scan when the user hands us an existing SBOM file
  instead of a project. Best-effort kind inference for SBOMs not produced by
  DPM (we look for the dpm:component-kind property first; fall back to mapping
  CycloneDX `type` to kind). }

interface

uses
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Types;

type
  TCycloneDXReader = class(TInterfacedObject, ISBOMReader)
  protected
    function ReadFromFile(const fileName : string) : TSBOMReport;
    function ReadFromString(const json : string) : TSBOMReport;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  JsonDataObjects,
  DPM.Core.Types;

function StringToSbomKind(const value : string) : TSBOMComponentKind;
var
  v : string;
begin
  v := LowerCase(Trim(value));
  if (v = 'application') then
    result := TSBOMComponentKind.Application
  else if (v = 'dpm-package') then
    result := TSBOMComponentKind.DpmPackage
  else if (v = 'delphi-runtime') then
    result := TSBOMComponentKind.DelphiRuntime
  else if (v = 'third-party') then
    result := TSBOMComponentKind.ThirdParty
  else if (v = 'unidentified') then
    result := TSBOMComponentKind.Unidentified
  else
    result := TSBOMComponentKind.Unidentified;
end;

//Fallback when the SBOM wasn't produced by DPM (or the property is missing):
//map CycloneDX `type` -> our kind. `application` -> Application, `framework` ->
//DelphiRuntime, and `library` is ambiguous so we use the purl to disambiguate:
//pkg:generic/dpm/... -> DpmPackage, else ThirdParty. No purl at all -> Unidentified.
function InferKind(const cdxType, purl : string) : TSBOMComponentKind;
var
  t : string;
  p : string;
begin
  t := LowerCase(Trim(cdxType));
  p := LowerCase(Trim(purl));
  if t = 'application' then
    result := TSBOMComponentKind.Application
  else if t = 'framework' then
    result := TSBOMComponentKind.DelphiRuntime
  else if t = 'library' then
  begin
    if (p <> '') and (Pos('pkg:generic/dpm/', p) = 1) then
      result := TSBOMComponentKind.DpmPackage
    else if p <> '' then
      result := TSBOMComponentKind.ThirdParty
    else
      result := TSBOMComponentKind.Unidentified;
  end
  else
    result := TSBOMComponentKind.Unidentified;
end;

//Read the dpm:component-kind value out of a CycloneDX properties[] array if
//present. Returns true and sets value when found, false otherwise. The reader
//prefers this over InferKind because it's a deterministic round-trip when the
//SBOM was produced by DPM.
function TryReadKindProperty(const componentObj : TJsonObject; out kind : TSBOMComponentKind) : boolean;
var
  propsArr : TJsonArray;
  i : integer;
  propObj : TJsonObject;
  propName : string;
begin
  result := false;
  if not componentObj.Contains('properties') then
    exit;
  if componentObj.IsNull('properties') then
    exit;
  propsArr := componentObj.A['properties'];
  for i := 0 to propsArr.Count - 1 do
  begin
    propObj := propsArr.O[i];
    if not propObj.Contains('name') then
      continue;
    propName := propObj.S['name'];
    if SameText(propName, 'dpm:component-kind') then
    begin
      kind := StringToSbomKind(propObj.S['value']);
      result := true;
      exit;
    end;
  end;
end;

procedure ReadProperties(const componentObj : TJsonObject; const comp : TSBOMComponent);
var
  propsArr : TJsonArray;
  i : integer;
  propObj : TJsonObject;
  propName : string;
begin
  if not componentObj.Contains('properties') then
    exit;
  if componentObj.IsNull('properties') then
    exit;
  propsArr := componentObj.A['properties'];
  for i := 0 to propsArr.Count - 1 do
  begin
    propObj := propsArr.O[i];
    if not propObj.Contains('name') then
      continue;
    propName := propObj.S['name'];
    //dpm:component-kind is the writer's own discriminator - we already used it
    //in TryReadKindProperty and don't want to round-trip it as a user property.
    if SameText(propName, 'dpm:component-kind') then
      continue;
    comp.AddProperty(propName, propObj.S['value']);
  end;
end;

procedure ReadEvidence(const componentObj : TJsonObject; const comp : TSBOMComponent);
var
  evidenceObj : TJsonObject;
  occurArr : TJsonArray;
  i : integer;
  occObj : TJsonObject;
begin
  if not componentObj.Contains('evidence') then
    exit;
  if componentObj.IsNull('evidence') then
    exit;
  evidenceObj := componentObj.O['evidence'];
  if not evidenceObj.Contains('occurrences') then
    exit;
  if evidenceObj.IsNull('occurrences') then
    exit;
  occurArr := evidenceObj.A['occurrences'];
  for i := 0 to occurArr.Count - 1 do
  begin
    occObj := occurArr.O[i];
    if occObj.Contains('location') then
      comp.AddEvidence(occObj.S['location'], '');
  end;
end;

procedure ReadHashes(const componentObj : TJsonObject; const comp : TSBOMComponent);
var
  hashesArr : TJsonArray;
  hashObj : TJsonObject;
begin
  if not componentObj.Contains('hashes') then
    exit;
  if componentObj.IsNull('hashes') then
    exit;
  hashesArr := componentObj.A['hashes'];
  if hashesArr.Count = 0 then
    exit;
  //Only round-trip the first hash. The CycloneDX writer only emits one anyway.
  hashObj := hashesArr.O[0];
  if hashObj.Contains('alg') then
    comp.HashAlgorithm := hashObj.S['alg'];
  if hashObj.Contains('content') then
    comp.HashValue := hashObj.S['content'];
end;

procedure ReadExternalRefs(const componentObj : TJsonObject; const comp : TSBOMComponent);
var
  refsArr : TJsonArray;
  i : integer;
  refObj : TJsonObject;
  refType : string;
  refUrl : string;
begin
  if not componentObj.Contains('externalReferences') then
    exit;
  if componentObj.IsNull('externalReferences') then
    exit;
  refsArr := componentObj.A['externalReferences'];
  for i := 0 to refsArr.Count - 1 do
  begin
    refObj := refsArr.O[i];
    if not refObj.Contains('type') then
      continue;
    refType := LowerCase(refObj.S['type']);
    refUrl := refObj.S['url'];
    if refType = 'website' then
      comp.ProjectUrl := refUrl
    else if refType = 'vcs' then
      comp.RepositoryUrl := refUrl
    else if refType = 'distribution' then
      comp.DownloadUrl := refUrl;
  end;
end;

procedure FillComponent(const componentObj : TJsonObject; const comp : TSBOMComponent;
                       const inferredType, inferredPurl : string);
var
  authorsArr : TJsonArray;
  i : integer;
  authorObj : TJsonObject;
  licensesArr : TJsonArray;
  licWrap : TJsonObject;
  licInner : TJsonObject;
  tagsArr : TJsonArray;
begin
  if componentObj.Contains('bom-ref') then
    comp.BomRef := componentObj.S['bom-ref'];
  if componentObj.Contains('name') then
    comp.Id := componentObj.S['name'];
  if componentObj.Contains('version') then
    comp.Version := componentObj.S['version'];
  if componentObj.Contains('description') then
    comp.Description := componentObj.S['description'];
  if componentObj.Contains('purl') then
    comp.Purl := componentObj.S['purl']
  else
    comp.Purl := inferredPurl;
  if componentObj.Contains('copyright') then
    comp.Copyright := componentObj.S['copyright'];

  if componentObj.Contains('supplier') and (not componentObj.IsNull('supplier')) then
  begin
    if componentObj.O['supplier'].Contains('name') then
      comp.Supplier := componentObj.O['supplier'].S['name'];
  end;

  if componentObj.Contains('authors') and (not componentObj.IsNull('authors')) then
  begin
    authorsArr := componentObj.A['authors'];
    for i := 0 to authorsArr.Count - 1 do
    begin
      authorObj := authorsArr.O[i];
      if authorObj.Contains('name') then
        comp.Authors.Add(authorObj.S['name']);
    end;
  end;

  if componentObj.Contains('licenses') and (not componentObj.IsNull('licenses')) then
  begin
    licensesArr := componentObj.A['licenses'];
    if licensesArr.Count > 0 then
    begin
      licWrap := licensesArr.O[0];
      if licWrap.Contains('license') then
      begin
        licInner := licWrap.O['license'];
        if licInner.Contains('id') then
          comp.License := licInner.S['id']
        else if licInner.Contains('name') then
          comp.License := licInner.S['name'];
      end;
    end;
  end;

  if componentObj.Contains('tags') and (not componentObj.IsNull('tags')) then
  begin
    tagsArr := componentObj.A['tags'];
    for i := 0 to tagsArr.Count - 1 do
      comp.Tags.Add(tagsArr.S[i]);
  end;

  ReadHashes(componentObj, comp);
  ReadExternalRefs(componentObj, comp);
  ReadProperties(componentObj, comp);
  ReadEvidence(componentObj, comp);
end;

procedure ReadRelationships(const root : TJsonObject; const report : TSBOMReport);
var
  depsArr : TJsonArray;
  i, j : integer;
  depObj : TJsonObject;
  parentRef : string;
  childrenArr : TJsonArray;
begin
  if not root.Contains('dependencies') then
    exit;
  if root.IsNull('dependencies') then
    exit;
  depsArr := root.A['dependencies'];
  for i := 0 to depsArr.Count - 1 do
  begin
    depObj := depsArr.O[i];
    if not depObj.Contains('ref') then
      continue;
    parentRef := depObj.S['ref'];
    if not depObj.Contains('dependsOn') then
      continue;
    childrenArr := depObj.A['dependsOn'];
    for j := 0 to childrenArr.Count - 1 do
      report.AddRelationship(parentRef, childrenArr.S[j]);
  end;
end;

procedure ReadMetaProperties(const metaObj : TJsonObject; const report : TSBOMReport);
var
  propsArr : TJsonArray;
  i : integer;
  propObj : TJsonObject;
  propName : string;
  propValue : string;
begin
  if not metaObj.Contains('properties') then
    exit;
  if metaObj.IsNull('properties') then
    exit;
  propsArr := metaObj.A['properties'];
  for i := 0 to propsArr.Count - 1 do
  begin
    propObj := propsArr.O[i];
    if not propObj.Contains('name') then
      continue;
    propName := propObj.S['name'];
    propValue := propObj.S['value'];
    //Recover platform / compiler from the writer's hidden discriminators so the
    //round-tripped report knows which (project, platform) it represents. Don't
    //also stash them as user properties - that would cause them to re-emit a
    //second time on a write -> read -> write cycle.
    if SameText(propName, 'dpm:platform') then
      report.Platform := StringToDPMPlatform(propValue)
    else if SameText(propName, 'dpm:compilerVersion') then
      report.CompilerVersion := StringToCompilerVersion(propValue)
    else
      report.AddMetaProperty(propName, propValue);
  end;
end;

{ TCycloneDXReader }

function TCycloneDXReader.ReadFromFile(const fileName : string) : TSBOMReport;
var
  json : string;
begin
  if not FileExists(fileName) then
    raise EFileNotFoundException.Create('SBOM file not found: ' + fileName);
  json := TFile.ReadAllText(fileName, TEncoding.UTF8);
  result := ReadFromString(json);
end;

function TCycloneDXReader.ReadFromString(const json : string) : TSBOMReport;
var
  root : TJsonObject;
  meta : TJsonObject;
  rootCompObj : TJsonObject;
  componentsArr : TJsonArray;
  i : integer;
  compObj : TJsonObject;
  comp : TSBOMComponent;
  cdxType : string;
  purl : string;
  kind : TSBOMComponentKind;
  toolsArr : TJsonArray;
  toolObj : TJsonObject;
  bomFormat : string;
  specVersion : string;
begin
//  result := nil;
  root := nil;
  try
    try
      root := TJsonBaseObject.Parse(json) as TJsonObject;
    except
      on e : Exception do
        raise EArgumentException.Create('Could not parse SBOM JSON: ' + e.Message);
    end;

    if root.Contains('bomFormat') then
    begin
      bomFormat := root.S['bomFormat'];
      if not SameText(bomFormat, 'CycloneDX') then
        raise EArgumentException.Create('Expected bomFormat=CycloneDX, got [' + bomFormat + ']');
    end;
    if root.Contains('specVersion') then
    begin
      specVersion := root.S['specVersion'];
      //Be lenient: 1.4, 1.5, 1.6 all use the same component shape we need.
      if (Pos('1.', specVersion) <> 1) then
        raise EArgumentException.Create('Unsupported CycloneDX specVersion [' + specVersion + '] - expected 1.x');
    end;

    result := TSBOMReport.Create;
    try
      if root.Contains('serialNumber') then
        result.SerialNumber := root.S['serialNumber'];

      if root.Contains('metadata') and (not root.IsNull('metadata')) then
      begin
        meta := root.O['metadata'];
        if meta.Contains('timestamp') then
          result.TimestampUtc := meta.S['timestamp'];

        if meta.Contains('tools') and (not meta.IsNull('tools')) then
        begin
          toolsArr := meta.A['tools'];
          if toolsArr.Count > 0 then
          begin
            toolObj := toolsArr.O[0];
            if toolObj.Contains('name') then
              result.ToolName := toolObj.S['name'];
            if toolObj.Contains('version') then
              result.ToolVersion := toolObj.S['version'];
          end;
        end;

        if meta.Contains('component') and (not meta.IsNull('component')) then
        begin
          rootCompObj := meta.O['component'];
          if rootCompObj.Contains('type') then
            cdxType := rootCompObj.S['type']
          else
            cdxType := 'application';
          if rootCompObj.Contains('purl') then
            purl := rootCompObj.S['purl']
          else
            purl := '';
          if not TryReadKindProperty(rootCompObj, kind) then
            kind := InferKind(cdxType, purl);
          //The TSBOMReport constructor pre-creates a root component as Application.
          //We can't change its kind after the fact (it's read-only), but for SBOM
          //files we just round-trip into it as-is - the root is always the project.
          FillComponent(rootCompObj, result.RootComponent, cdxType, purl);
          if rootCompObj.Contains('name') then
            result.ProjectName := rootCompObj.S['name'];
          if rootCompObj.Contains('version') then
            result.ProjectVersion := rootCompObj.S['version'];
        end;

        ReadMetaProperties(meta, result);
      end;

      if root.Contains('components') and (not root.IsNull('components')) then
      begin
        componentsArr := root.A['components'];
        for i := 0 to componentsArr.Count - 1 do
        begin
          compObj := componentsArr.O[i];
          if compObj.Contains('type') then
            cdxType := compObj.S['type']
          else
            cdxType := 'library';
          if compObj.Contains('purl') then
            purl := compObj.S['purl']
          else
            purl := '';
          if not TryReadKindProperty(compObj, kind) then
            kind := InferKind(cdxType, purl);
          comp := result.AddComponent(kind);
          FillComponent(compObj, comp, cdxType, purl);
        end;
      end;

      ReadRelationships(root, result);
    except
      result.Free;
      raise;
    end;
  finally
    root.Free;
  end;
end;

end.
