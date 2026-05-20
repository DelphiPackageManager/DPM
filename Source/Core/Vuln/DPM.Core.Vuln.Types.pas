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

unit DPM.Core.Vuln.Types;

{ Vulnerability scan domain types. These are the post-database, pre-writer
  shapes — the OSV client maps the raw OSV record into these, the scanner
  groups them into a TVulnReport, and writers serialise to CycloneDX VEX
  (or future formats). }

interface

uses
  Spring.Collections;

{$SCOPEDENUMS ON}

type
  //CVSS severity buckets, ordered. Comparisons against the -fail-on threshold
  //rely on ordinal ordering (Critical > High > Medium > Low > None).
  //Unknown maps numerically to None for threshold purposes but is reported
  //distinctly so users can see "we found a vuln but don't have a CVSS score for it".
  TSeverity = (
    None,
    Low,
    Medium,
    High,
    Critical,
    Unknown
  );

  //One advisory reference (CVE, GHSA, etc.) attached to a vulnerability.
  //OSV records typically carry multiple aliases - the primary id plus references
  //to the same hole in other databases.
  TVulnAlias = record
    Source : string;   // e.g. 'CVE' / 'GHSA' / 'OSV'
    Id : string;       // e.g. 'CVE-2021-44228'
  end;

  //One affected-component citation. Carries the source SBOM bomRef so the
  //CycloneDX VEX writer can emit `affects[].ref` pointing back into the SBOM
  //the user fed us. Also carries componentId/version for human-readable output
  //formats that aren't bomRef-aware.
  TVulnAffected = record
    BomRef : string;
    ComponentId : string;
    ComponentVersion : string;
    Purl : string;
    FixedInVersion : string;  // empty if no fix is listed in the advisory
  end;

  //One vulnerability. The id is the canonical OSV/GHSA/CVE id; aliases enumerate
  //the same hole's other ids; affects[] lists every component in the input SBOM
  //that this hole hits (a single advisory frequently affects multiple package
  //versions, so one Vulnerability can carry many TVulnAffected rows).
  TVulnerability = class
  private
    FId : string;
    FAliases : IList<TVulnAlias>;
    FSummary : string;
    FDetails : string;
    FSeverity : TSeverity;
    FCvssScore : Double;
    FCvssVector : string;
    FPublished : string;     // ISO-8601
    FModified : string;      // ISO-8601
    FReferences : IList<string>;  // URLs
    FAffects : IList<TVulnAffected>;
  public
    constructor Create;
    procedure AddAlias(const source, id : string);
    procedure AddReference(const url : string);
    procedure AddAffected(const affected : TVulnAffected);

    property Id : string read FId write FId;
    property Aliases : IList<TVulnAlias> read FAliases;
    property Summary : string read FSummary write FSummary;
    property Details : string read FDetails write FDetails;
    property Severity : TSeverity read FSeverity write FSeverity;
    property CvssScore : Double read FCvssScore write FCvssScore;
    property CvssVector : string read FCvssVector write FCvssVector;
    property Published : string read FPublished write FPublished;
    property Modified : string read FModified write FModified;
    property References : IList<string> read FReferences;
    property Affects : IList<TVulnAffected> read FAffects;
  end;

  //The full scan report. Owns the vulnerabilities. Carries forward enough
  //identity from the source SBOM that the writer can reference it.
  TVulnReport = class
  private
    FSerialNumber : string;
    FTimestampUtc : string;
    FToolName : string;
    FToolVersion : string;
    FSourceSbomSerial : string;   // serialNumber of the SBOM that was scanned
    FProjectName : string;
    FProjectVersion : string;
    FPlatform : string;
    FVulnerabilities : IList<TVulnerability>;
    FComponentsScanned : Integer;
    FComponentsAffected : Integer;
    FMaxSeverity : TSeverity;
    function ComputeMaxSeverity : TSeverity;
  public
    constructor Create;
    function AddVulnerability(const id : string) : TVulnerability;
    procedure Finalize;

    property SerialNumber : string read FSerialNumber write FSerialNumber;
    property TimestampUtc : string read FTimestampUtc write FTimestampUtc;
    property ToolName : string read FToolName write FToolName;
    property ToolVersion : string read FToolVersion write FToolVersion;
    property SourceSbomSerial : string read FSourceSbomSerial write FSourceSbomSerial;
    property ProjectName : string read FProjectName write FProjectName;
    property ProjectVersion : string read FProjectVersion write FProjectVersion;
    property Platform : string read FPlatform write FPlatform;
    property Vulnerabilities : IList<TVulnerability> read FVulnerabilities;
    property ComponentsScanned : Integer read FComponentsScanned write FComponentsScanned;
    property ComponentsAffected : Integer read FComponentsAffected write FComponentsAffected;
    property MaxSeverity : TSeverity read FMaxSeverity;
  end;


//Parse a -fail-on CLI token to a TSeverity. Accepts 'none', 'low', 'medium',
//'high', 'critical'. Unrecognised input raises EArgumentException. The default
//for the CLI ('none') means "never fail," not "filter out low/medium/high."
function StringToSeverity(const value : string) : TSeverity;
function SeverityToString(const value : TSeverity) : string;
function CvssScoreToSeverity(const score : Double) : TSeverity;
//Map an OSV severity vector string ('CVSS:3.1/AV:N/...') to a numeric score.
//Returns 0 and severity=Unknown if it can't be parsed.
function ParseCvssBaseScore(const vector : string; out score : Double) : boolean;

implementation

uses
  System.SysUtils;

function StringToSeverity(const value : string) : TSeverity;
var
  v : string;
begin
  v := LowerCase(Trim(value));
  if (v = 'none') or (v = '') then
    result := TSeverity.None
  else if v = 'low' then
    result := TSeverity.Low
  else if v = 'medium' then
    result := TSeverity.Medium
  else if v = 'high' then
    result := TSeverity.High
  else if v = 'critical' then
    result := TSeverity.Critical
  else if v = 'unknown' then
    result := TSeverity.Unknown
  else
    raise EArgumentException.Create('Invalid severity [' + value + '] - expected none | low | medium | high | critical');
end;

function SeverityToString(const value : TSeverity) : string;
begin
  case value of
    TSeverity.None : result := 'none';
    TSeverity.Low : result := 'low';
    TSeverity.Medium : result := 'medium';
    TSeverity.High : result := 'high';
    TSeverity.Critical : result := 'critical';
    TSeverity.Unknown : result := 'unknown';
  else
    result := 'unknown';
  end;
end;

function CvssScoreToSeverity(const score : Double) : TSeverity;
begin
  //Standard CVSS 3.x severity bands.
  if score >= 9.0 then
    result := TSeverity.Critical
  else if score >= 7.0 then
    result := TSeverity.High
  else if score >= 4.0 then
    result := TSeverity.Medium
  else if score > 0.0 then
    result := TSeverity.Low
  else
    result := TSeverity.None;
end;

function ParseCvssBaseScore(const vector : string; out score : Double) : boolean;
var
  v : string;
  posScore : integer;
  trailing : string;
  i : integer;
  numStr : string;
  fs : TFormatSettings;
begin
  //OSV publishes per-severity entries like:
  //  { "type": "CVSS_V3", "score": "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" }
  //The vector string itself doesn't carry the base score; we'd have to compute
  //it from the metrics. For v1 we settle for a simpler shape: OSV often *also*
  //publishes a numeric score alongside the vector. The caller should prefer
  //that. This helper exists for the fallback case where we only see the vector
  //and want a coarse bucket - we look for an embedded /Base:N.N marker if
  //present (some OSV records carry that), otherwise we fail to parse.
  result := false;
  score := 0.0;
  v := vector;
  posScore := Pos('/Base:', v);
  if posScore = 0 then
    posScore := Pos('CVSS:', v);
  if posScore = 0 then
    exit;
  trailing := Copy(v, posScore, MaxInt);
  posScore := Pos(':', trailing);
  if posScore = 0 then
    exit;
  Delete(trailing, 1, posScore);
  numStr := '';
  for i := 1 to Length(trailing) do
  begin
    if CharInSet(trailing[i], ['0'..'9','.']) then
      numStr := numStr + trailing[i]
    else
      break;
  end;
  if numStr = '' then
    exit;
  fs := FormatSettings;
  fs.DecimalSeparator := '.';
  result := TryStrToFloat(numStr, score, fs);
end;

{ TVulnerability }

constructor TVulnerability.Create;
begin
  inherited Create;
  FAliases := TCollections.CreateList<TVulnAlias>;
  FReferences := TCollections.CreateList<string>;
  FAffects := TCollections.CreateList<TVulnAffected>;
  FSeverity := TSeverity.Unknown;
end;

procedure TVulnerability.AddAlias(const source, id : string);
var
  a : TVulnAlias;
begin
  a.Source := source;
  a.Id := id;
  FAliases.Add(a);
end;

procedure TVulnerability.AddReference(const url : string);
begin
  if Trim(url) <> '' then
    FReferences.Add(url);
end;

procedure TVulnerability.AddAffected(const affected : TVulnAffected);
begin
  FAffects.Add(affected);
end;

{ TVulnReport }

constructor TVulnReport.Create;
begin
  inherited Create;
  FVulnerabilities := TCollections.CreateObjectList<TVulnerability>(true);
  FMaxSeverity := TSeverity.None;
end;

function TVulnReport.AddVulnerability(const id : string) : TVulnerability;
begin
  result := TVulnerability.Create;
  result.Id := id;
  FVulnerabilities.Add(result);
end;

function TVulnReport.ComputeMaxSeverity : TSeverity;
var
  vuln : TVulnerability;
  bestOrd : integer;
  candidateOrd : integer;
begin
  bestOrd := Ord(TSeverity.None);
  for vuln in FVulnerabilities do
  begin
    //Unknown is reported but does not raise the threshold - the user has no
    //information to act on, so failing CI on Unknown would be noisy.
    if vuln.Severity = TSeverity.Unknown then
      continue;
    candidateOrd := Ord(vuln.Severity);
    if candidateOrd > bestOrd then
      bestOrd := candidateOrd;
  end;
  result := TSeverity(bestOrd);
end;

procedure TVulnReport.Finalize;
var
  vuln : TVulnerability;
  affectedBomRefs : IDictionary<string, integer>;
  aff : TVulnAffected;
begin
  affectedBomRefs := TCollections.CreateDictionary<string, integer>;
  for vuln in FVulnerabilities do
    for aff in vuln.Affects do
      if aff.BomRef <> '' then
        affectedBomRefs[aff.BomRef] := 1;
  FComponentsAffected := affectedBomRefs.Count;
  FMaxSeverity := ComputeMaxSeverity;
end;

end.
