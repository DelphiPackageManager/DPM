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

unit DPM.Core.Vuln.Writer.CycloneDX;

(* CycloneDX 1.5 VEX writer.

  Emits a CycloneDX 1.5 JSON document carrying:
  - The source SBOM's identity in metadata
  - The vulnerabilities[] array, one entry per finding, with affects[].ref
    pointing back into the source SBOM's component bom-refs

  Spec: https://cyclonedx.org/docs/1.5/json/#vulnerabilities *)

interface

uses
  DPM.Core.SBOM.Types,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Types;

const
  cVulnWriterCycloneDX = 'vuln.writer.cyclonedx';

type
  TCycloneDxVexWriter = class(TInterfacedObject, IVulnWriter)
  protected
    function GetFormatId : string;
    function GetFileExtension : string;
    procedure Write(const sbomReport : TSBOMReport;
                    const vulnReport : TVulnReport;
                    const fileName : string);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  JsonDataObjects;

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

procedure SetStringIfNotEmpty(const obj : TJsonObject; const name, value : string);
begin
  if value <> '' then
    obj.S[name] := value;
end;

//Severity -> CycloneDX VEX rating.severity enum: 'none' | 'low' | 'medium' |
//'high' | 'critical' | 'unknown'. Matches our enum string verbatim.
function SeverityToCycloneDX(const value : TSeverity) : string;
begin
  case value of
    TSeverity.None : result := 'none';
    TSeverity.Low : result := 'low';
    TSeverity.Medium : result := 'medium';
    TSeverity.High : result := 'high';
    TSeverity.Critical : result := 'critical';
  else
    //CycloneDX accepts 'unknown' as a rating.severity value.
    result := 'unknown';
  end;
end;

//Aliases get split into a 'source' field for the canonical id and a
//references[].source for the rest. We pick the canonical from the id's prefix:
//GHSA-... -> GitHub Advisory Database; CVE-... -> NVD; else -> osv.dev.
function SourceNameForId(const id : string) : string;
begin
  if Pos('GHSA-', id) = 1 then
    result := 'GitHub Advisory Database'
  else if Pos('CVE-', id) = 1 then
    result := 'NVD'
  else
    result := 'osv.dev';
end;

function SourceUrlForId(const id : string) : string;
begin
  if Pos('GHSA-', id) = 1 then
    result := 'https://github.com/advisories/' + id
  else if Pos('CVE-', id) = 1 then
    result := 'https://nvd.nist.gov/vuln/detail/' + id
  else
    result := 'https://osv.dev/vulnerability/' + id;
end;

{ TCycloneDxVexWriter }

function TCycloneDxVexWriter.GetFormatId : string;
begin
  result := cVulnWriterCycloneDX;
end;

function TCycloneDxVexWriter.GetFileExtension : string;
begin
  result := '.vex.json';
end;

procedure TCycloneDxVexWriter.Write(const sbomReport : TSBOMReport;
                                    const vulnReport : TVulnReport;
                                    const fileName : string);
var
  root : TJsonObject;
  meta : TJsonObject;
  toolsArr : TJsonArray;
  toolObj : TJsonObject;
  vulnsArr : TJsonArray;
  vulnObj : TJsonObject;
  vuln : TVulnerability;
  ratingsArr : TJsonArray;
  ratingObj : TJsonObject;
  sourceObj : TJsonObject;
  affectsArr : TJsonArray;
  affObj : TJsonObject;
  aff : TVulnAffected;
  refsArr : TJsonArray;
  refObj : TJsonObject;
  refSource : TJsonObject;
  refUrl : string;
  alias : TVulnAlias;
  advisoriesArr : TJsonArray;
  advObj : TJsonObject;
  jsonText : string;
begin
  root := TJsonObject.Create;
  try
    root.S['bomFormat'] := 'CycloneDX';
    root.S['specVersion'] := '1.5';
    root.I['version'] := 1;
    SetStringIfNotEmpty(root, 'serialNumber', vulnReport.SerialNumber);

    meta := root.O['metadata'];
    SetStringIfNotEmpty(meta, 'timestamp', vulnReport.TimestampUtc);

    toolsArr := meta.A['tools'];
    toolObj := toolsArr.AddObject;
    toolObj.S['vendor'] := 'DPM';
    toolObj.S['name'] := vulnReport.ToolName;
    toolObj.S['version'] := vulnReport.ToolVersion;

    //Reference the source SBOM by serialNumber so a downstream tool can
    //correlate this VEX with the SBOM it scanned. The root component echoes
    //the project the SBOM described.
    if sbomReport.RootComponent <> nil then
    begin
      meta.O['component'].S['type'] := 'application';
      meta.O['component'].S['bom-ref'] := sbomReport.RootComponent.BomRef;
      SetStringIfNotEmpty(meta.O['component'], 'name', sbomReport.RootComponent.Id);
      SetStringIfNotEmpty(meta.O['component'], 'version', sbomReport.RootComponent.Version);
    end;
    SetStringIfNotEmpty(meta, 'sourceSbomSerialNumber', vulnReport.SourceSbomSerial);

    if vulnReport.Vulnerabilities.Count = 0 then
    begin
      //Schema allows an empty vulnerabilities[] - emit it explicitly so
      //downstream "is this scanned?" checks succeed.
      root.A['vulnerabilities'];
    end
    else
    begin
      vulnsArr := root.A['vulnerabilities'];
      for vuln in vulnReport.Vulnerabilities do
      begin
        vulnObj := vulnsArr.AddObject;
        vulnObj.S['id'] := vuln.Id;

        sourceObj := vulnObj.O['source'];
        sourceObj.S['name'] := SourceNameForId(vuln.Id);
        sourceObj.S['url'] := SourceUrlForId(vuln.Id);

        SetStringIfNotEmpty(vulnObj, 'description', vuln.Summary);
        SetStringIfNotEmpty(vulnObj, 'detail', vuln.Details);
        SetStringIfNotEmpty(vulnObj, 'published', vuln.Published);
        SetStringIfNotEmpty(vulnObj, 'updated', vuln.Modified);

        //ratings[] - one entry covering CVSS score + severity.
        ratingsArr := vulnObj.A['ratings'];
        ratingObj := ratingsArr.AddObject;
        ratingObj.O['source'].S['name'] := SourceNameForId(vuln.Id);
        if vuln.CvssScore > 0.0 then
          ratingObj.F['score'] := vuln.CvssScore;
        ratingObj.S['severity'] := SeverityToCycloneDX(vuln.Severity);
        if vuln.CvssVector <> '' then
        begin
          if Pos('CVSS:3', vuln.CvssVector) > 0 then
            ratingObj.S['method'] := 'CVSSv3'
          else if Pos('CVSS:2', vuln.CvssVector) > 0 then
            ratingObj.S['method'] := 'CVSSv2';
          ratingObj.S['vector'] := vuln.CvssVector;
        end;

        //advisories[] - external advisory URLs
        if vuln.References.Count > 0 then
        begin
          advisoriesArr := vulnObj.A['advisories'];
          for refUrl in vuln.References do
          begin
            advObj := advisoriesArr.AddObject;
            advObj.S['url'] := refUrl;
          end;
        end;

        //references[] - alias ids, each carrying a source object
        if vuln.Aliases.Count > 0 then
        begin
          refsArr := vulnObj.A['references'];
          for alias in vuln.Aliases do
          begin
            if SameText(alias.Id, vuln.Id) then
              continue;
            refObj := refsArr.AddObject;
            refObj.S['id'] := alias.Id;
            refSource := refObj.O['source'];
            refSource.S['name'] := alias.Source;
            refSource.S['url'] := SourceUrlForId(alias.Id);
          end;
        end;

        //affects[] - one entry per affected component in the source SBOM
        affectsArr := vulnObj.A['affects'];
        for aff in vuln.Affects do
        begin
          affObj := affectsArr.AddObject;
          //ref points to a bom-ref in the source SBOM. Leave it dangling if
          //the source SBOM didn't carry bom-refs (legitimate for hand-edited
          //SBOMs) - downstream tooling can fall back to component/purl matching.
          if aff.BomRef <> '' then
            affObj.S['ref'] := aff.BomRef
          else if aff.Purl <> '' then
            affObj.S['ref'] := aff.Purl
          else
            affObj.S['ref'] := aff.ComponentId;
        end;
      end;
    end;

    jsonText := root.ToJSON(false);
    WriteUtf8(fileName, jsonText);
  finally
    root.Free;
  end;
end;

end.
