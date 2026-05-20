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

unit DPM.Core.Vuln.Interfaces;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.SBOM.Types,
  DPM.Core.Vuln.Types;

type
  //One advisory record from an upstream vulnerability database. Owned by the
  //caller of IVulnDatabase.Query; the database does not retain a reference.
  //The database is responsible for normalising upstream-format quirks - the
  //scanner sees a uniform shape regardless of source.
  IVulnDatabaseAdvisory = interface
    ['{1B6D8C12-2A7C-4B91-9E47-3D5F9AAB58E1}']
    function GetId : string;
    function GetAliases : IList<TVulnAlias>;
    function GetSummary : string;
    function GetDetails : string;
    function GetSeverity : TSeverity;
    function GetCvssScore : Double;
    function GetCvssVector : string;
    function GetPublished : string;
    function GetModified : string;
    function GetReferences : IList<string>;
    function GetFixedVersion : string;

    property Id : string read GetId;
    property Aliases : IList<TVulnAlias> read GetAliases;
    property Summary : string read GetSummary;
    property Details : string read GetDetails;
    property Severity : TSeverity read GetSeverity;
    property CvssScore : Double read GetCvssScore;
    property CvssVector : string read GetCvssVector;
    property Published : string read GetPublished;
    property Modified : string read GetModified;
    property References : IList<string> read GetReferences;
    property FixedVersion : string read GetFixedVersion;
  end;

  ///<summary>A vulnerability data source. v1 has one implementation (OSV);
  /// GHSA-direct / NVD / Sonatype can be slotted in later as additional
  /// implementations without changes to the scanner.</summary>
  IVulnDatabase = interface
    ['{9B57F1A8-2C0E-4F1F-9BBC-7E14B79D2D6A}']
    function GetSourceName : string;
    //Look up vulnerabilities for the given purls. The returned dictionary is
    //keyed by input purl; missing keys mean no vulns found (or query failed -
    //implementations are expected to log and skip rather than throw).
    function Query(const cancellationToken : ICancellationToken;
                   const purls : IList<string>) : IDictionary<string, IList<IVulnDatabaseAdvisory>>;
    property SourceName : string read GetSourceName;
  end;

  ///<summary>Disk-backed response cache. Wraps an IVulnDatabase so repeat scans
  /// against the same SBOM don't re-hit the network. TTL is configured at
  /// construction (default 24h). Cache miss / stale entry falls through to the
  /// wrapped database and persists the fresh response. Set BypassReads to true
  /// to force fresh fetches for this run (still writes fresh responses).</summary>
  IVulnResponseCache = interface
    ['{D3B7A4F5-9C2B-4E08-AF6E-1A8B4F6D2C7E}']
    function TryGet(const purl : string; const sourceName : string;
                    out cachedJson : string) : boolean;
    procedure Put(const purl : string; const sourceName : string; const json : string);
    procedure Clear;
    function GetCacheDir : string;
    function GetBypassReads : boolean;
    procedure SetBypassReads(const value : boolean);
    property CacheDir : string read GetCacheDir;
    property BypassReads : boolean read GetBypassReads write SetBypassReads;
  end;

  ///<summary>Orchestrator. Walks an SBOM, queries the configured database,
  /// builds a TVulnReport. Does not write — the writer is a separate stage so
  /// the same scanner output can drive multiple output formats.</summary>
  IVulnScanner = interface
    ['{3F8C1A2B-4D6E-4F7A-8B9C-2E0D5A6C8F1A}']
    function Scan(const cancellationToken : ICancellationToken;
                  const report : TSBOMReport) : TVulnReport;
  end;

  ///<summary>One output-format writer (CycloneDX VEX, later OpenVEX, etc).
  /// Mirrors the ISbomWriter shape so format registration follows the same
  /// container pattern.</summary>
  IVulnWriter = interface
    ['{6E7D4B2A-3F5C-4D8E-9A1B-5C7F8E2D4B0A}']
    function GetFormatId : string;
    function GetFileExtension : string;
    procedure Write(const sbomReport : TSBOMReport;
                    const vulnReport : TVulnReport;
                    const fileName : string);
    property FormatId : string read GetFormatId;
    property FileExtension : string read GetFileExtension;
  end;

implementation

end.
