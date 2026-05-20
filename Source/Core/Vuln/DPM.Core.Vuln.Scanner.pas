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

unit DPM.Core.Vuln.Scanner;

{ Orchestrator: walks the components of a TSBOMReport, asks the configured
  vulnerability database about the purls, builds a TVulnReport. Stateless -
  one scanner instance can scan many reports. }

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Logging,
  DPM.Core.SBOM.Types,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Types;

type
  TVulnScanner = class(TInterfacedObject, IVulnScanner)
  private
    FLogger : ILogger;
    FDatabase : IVulnDatabase;
  protected
    function Scan(const cancellationToken : ICancellationToken;
                  const report : TSBOMReport) : TVulnReport;
  public
    constructor Create(const logger : ILogger; const database : IVulnDatabase);
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  Spring.Collections,
  DPM.Core.Types;

const
  cScanToolName = 'dpm-scan';
  cScanToolVersion = '1.0.0';

{ TVulnScanner }

constructor TVulnScanner.Create(const logger : ILogger; const database : IVulnDatabase);
begin
  inherited Create;
  FLogger := logger;
  FDatabase := database;
end;

function TVulnScanner.Scan(const cancellationToken : ICancellationToken;
                           const report : TSBOMReport) : TVulnReport;
var
  purls : IList<string>;
  purlToComp : IDictionary<string, TSBOMComponent>;
  comp : TSBOMComponent;
  lookups : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
  byVulnId : IDictionary<string, TVulnerability>;
  perPurl : IList<IVulnDatabaseAdvisory>;
  adv : IVulnDatabaseAdvisory;
  vuln : TVulnerability;
  affected : TVulnAffected;
  alias : TVulnAlias;
  refUrl : string;
  scanned : integer;
begin
  result := TVulnReport.Create;
  try
    result.ToolName := cScanToolName;
    result.ToolVersion := cScanToolVersion;
    result.TimestampUtc := DateToISO8601(TTimeZone.Local.ToUniversalTime(Now), true);
    result.SourceSbomSerial := report.SerialNumber;
    result.ProjectName := report.ProjectName;
    result.ProjectVersion := report.ProjectVersion;
    result.Platform := DPMPlatformToString(report.Platform);

    purls := TCollections.CreateList<string>;
    purlToComp := TCollections.CreateDictionary<string, TSBOMComponent>;
    scanned := 0;
    for comp in report.Components do
    begin
      //Only scan components that carry a purl. Application root, runtime
      //framework (no purl), and unidentified entries (kind=Unidentified, no
      //purl) are skipped silently - there's nothing to look up.
      if comp.Purl = '' then
        continue;
      //A purl can legitimately appear twice in a groupproj aggregation - the
      //dedupe in the generator keeps one component per (id, version) but we
      //defensively skip duplicates here too in case the input SBOM was edited.
      if purlToComp.ContainsKey(comp.Purl) then
        continue;
      purls.Add(comp.Purl);
      purlToComp[comp.Purl] := comp;
      Inc(scanned);
    end;
    result.ComponentsScanned := scanned;

    if purls.Count = 0 then
    begin
      result.Finalize;
      exit;
    end;

    if Assigned(FLogger) then
      FLogger.Information('[scan] Querying ' + FDatabase.SourceName + ' for ' + IntToStr(purls.Count) + ' component(s)');

    lookups := FDatabase.Query(cancellationToken, purls);
    if cancellationToken.IsCancelled then
    begin
      result.Finalize;
      exit;
    end;

    byVulnId := TCollections.CreateDictionary<string, TVulnerability>;
    for comp in report.Components do
    begin
      if comp.Purl = '' then
        continue;
      if not lookups.ContainsKey(comp.Purl) then
        continue;
      perPurl := lookups[comp.Purl];
      for adv in perPurl do
      begin
        if adv = nil then
          continue;
        //Group repeated finds of the same advisory across multiple components
        //into one TVulnerability with many affects[] entries. This matches the
        //CycloneDX VEX schema where one vuln entry references multiple
        //affects[].ref values.
        if not byVulnId.TryGetValue(adv.Id, vuln) then
        begin
          vuln := result.AddVulnerability(adv.Id);
          vuln.Summary := adv.Summary;
          vuln.Details := adv.Details;
          vuln.Severity := adv.Severity;
          vuln.CvssScore := adv.CvssScore;
          vuln.CvssVector := adv.CvssVector;
          vuln.Published := adv.Published;
          vuln.Modified := adv.Modified;
          for alias in adv.Aliases do
            vuln.AddAlias(alias.Source, alias.Id);
          for refUrl in adv.References do
            vuln.AddReference(refUrl);
          byVulnId[adv.Id] := vuln;
        end;

        affected.BomRef := comp.BomRef;
        affected.ComponentId := comp.Id;
        affected.ComponentVersion := comp.Version;
        affected.Purl := comp.Purl;
        affected.FixedInVersion := adv.FixedVersion;
        vuln.AddAffected(affected);
      end;
    end;

    result.Finalize;
    if Assigned(FLogger) then
      FLogger.Information('[scan] Found ' + IntToStr(result.Vulnerabilities.Count) +
                          ' vulnerability(ies) affecting ' +
                          IntToStr(result.ComponentsAffected) + ' component(s); max severity ' +
                          SeverityToString(result.MaxSeverity));
  except
    result.Free;
    raise;
  end;
end;

end.
