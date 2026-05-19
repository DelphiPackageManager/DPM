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

unit DPM.Core.SBOM.Writers.Reports;

{ Human-readable SBOM writers - HTML (single self-contained file with embedded
  CSS) and GitHub-Flavoured Markdown. Both consume TSBOMReport directly so the
  presentation never lags the machine-readable CycloneDX/SPDX outputs.

  Designed for SBOM audit reviewers who don't want to parse JSON: a one-page
  summary of every component in the binary, classified by origin (DPM package,
  Delphi runtime, third-party, unidentified), with the most relevant column for
  each (license, version, supplier, hash).

  Writers are intentionally minimal - no JS, no external CSS, no images. The
  HTML output renders correctly when emailed, printed, or attached to a ticket. }

interface

uses
  DPM.Core.Options.SBOM,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Types;

type
  THTMLReportWriter = class(TInterfacedObject, ISbomWriter)
  protected
    function GetFormat : TSBOMFormat;
    function GetFileExtension : string;
    procedure Write(const report : TSBOMReport; const fileName : string);
  end;

  TMarkdownReportWriter = class(TInterfacedObject, ISbomWriter)
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
  Spring.Collections,
  DPM.Core.Types;

const
  //Inlined so the HTML output is self-contained and survives email/attachment
  //transit without dead links. Dark theme tuned to match audit-tool aesthetics
  //(GitHub-dark-style palette - high contrast, low eye strain, prints legibly
  //if the user prefers a light printout via the @media print override).
  cEmbeddedCSS =
    ':root {' + sLineBreak +
    '  --bg: #0d1117; --bg-card: #161b22; --bg-card-alt: #1c232c; --bg-hover: #1f2933;' + sLineBreak +
    '  --border: #30363d; --border-accent: #484f58;' + sLineBreak +
    '  --text: #e6edf3; --text-muted: #8b949e; --text-strong: #f0f6fc;' + sLineBreak +
    '  --accent: #58a6ff; --accent-dpm: #3fb950; --accent-runtime: #58a6ff;' + sLineBreak +
    '  --accent-third: #d29922; --accent-unknown: #f85149;' + sLineBreak +
    '  --code-bg: #1f2428; --code-fg: #79c0ff;' + sLineBreak +
    '}' + sLineBreak +
    '* { box-sizing: border-box; }' + sLineBreak +
    'body {' + sLineBreak +
    '  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Inter", Roboto, sans-serif;' + sLineBreak +
    '  background: var(--bg); color: var(--text);' + sLineBreak +
    '  max-width: 1280px; margin: 0 auto; padding: 2.5em 2em;' + sLineBreak +
    '  line-height: 1.55; font-size: 15px;' + sLineBreak +
    '}' + sLineBreak +
    'h1 { font-size: 2em; color: var(--text-strong); font-weight: 700; margin: 0 0 0.4em; }' + sLineBreak +
    'h2 { font-size: 1.35em; color: var(--text-strong); font-weight: 600;' + sLineBreak +
    '     margin: 2.5em 0 0.8em; padding-bottom: 0.4em; border-bottom: 1px solid var(--border); }' + sLineBreak +
    'h3 { font-size: 1.05em; color: var(--text-strong); margin: 1.4em 0 0.6em; }' + sLineBreak +
    'p.lead { color: var(--text-muted); margin: 0 0 2em; }' + sLineBreak +
    'a { color: var(--accent); text-decoration: none; }' + sLineBreak +
    'a:hover { text-decoration: underline; }' + sLineBreak +
    'code, .hash {' + sLineBreak +
    '  font-family: ui-monospace, SFMono-Regular, "JetBrains Mono", Menlo, Consolas, monospace;' + sLineBreak +
    '  font-size: 0.88em; background: var(--code-bg); color: var(--code-fg);' + sLineBreak +
    '  padding: 0.1em 0.4em; border-radius: 4px;' + sLineBreak +
    '}' + sLineBreak +
    '.hash { word-break: break-all; }' + sLineBreak +
    'table {' + sLineBreak +
    '  border-collapse: separate; border-spacing: 0; width: 100%; margin: 1em 0;' + sLineBreak +
    '  font-size: 0.92em; background: var(--bg-card);' + sLineBreak +
    '  border: 1px solid var(--border); border-radius: 8px; overflow: hidden;' + sLineBreak +
    '}' + sLineBreak +
    'th, td { padding: 0.7em 1em; text-align: left; vertical-align: top;' + sLineBreak +
    '         border-bottom: 1px solid var(--border); }' + sLineBreak +
    'th { background: #11161d; color: var(--text-strong); font-weight: 600;' + sLineBreak +
    '     text-transform: uppercase; font-size: 0.78em; letter-spacing: 0.04em; }' + sLineBreak +
    'tbody tr:last-child td { border-bottom: none; }' + sLineBreak +
    'tbody tr:nth-child(even) td { background: var(--bg-card-alt); }' + sLineBreak +
    'tbody tr:hover td { background: var(--bg-hover); }' + sLineBreak +
    'tbody tr td:first-child { white-space: nowrap; }' + sLineBreak +
    '.meta-table td:first-child {' + sLineBreak +
    '  width: 12em; font-weight: 600; color: var(--text-strong);' + sLineBreak +
    '  background: #11161d;' + sLineBreak +
    '}' + sLineBreak +
    '.stat-grid {' + sLineBreak +
    '  display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));' + sLineBreak +
    '  gap: 1em; margin: 1.5em 0 2.5em;' + sLineBreak +
    '}' + sLineBreak +
    '.stat-card {' + sLineBreak +
    '  background: var(--bg-card); border: 1px solid var(--border);' + sLineBreak +
    '  border-left-width: 4px; border-radius: 8px;' + sLineBreak +
    '  padding: 1em 1.2em; transition: border-color 0.15s ease;' + sLineBreak +
    '}' + sLineBreak +
    '.stat-card:hover { border-color: var(--border-accent); }' + sLineBreak +
    '.stat-card .label { color: var(--text-muted); font-size: 0.85em;' + sLineBreak +
    '                    text-transform: uppercase; letter-spacing: 0.05em; }' + sLineBreak +
    '.stat-card .value { font-size: 2em; font-weight: 700; margin-top: 0.2em;' + sLineBreak +
    '                    color: var(--text-strong); line-height: 1.1; }' + sLineBreak +
    '.stat-card.dpm { border-left-color: var(--accent-dpm); }' + sLineBreak +
    '.stat-card.dpm .value { color: var(--accent-dpm); }' + sLineBreak +
    '.stat-card.runtime { border-left-color: var(--accent-runtime); }' + sLineBreak +
    '.stat-card.runtime .value { color: var(--accent-runtime); }' + sLineBreak +
    '.stat-card.third { border-left-color: var(--accent-third); }' + sLineBreak +
    '.stat-card.third .value { color: var(--accent-third); }' + sLineBreak +
    '.stat-card.unknown { border-left-color: var(--accent-unknown); }' + sLineBreak +
    '.stat-card.unknown .value { color: var(--accent-unknown); }' + sLineBreak +
    '.kind-dpm { color: var(--accent-dpm); font-weight: 600; }' + sLineBreak +
    '.kind-runtime { color: var(--accent-runtime); font-weight: 600; }' + sLineBreak +
    '.kind-third { color: var(--accent-third); font-weight: 600; }' + sLineBreak +
    '.kind-unknown { color: var(--accent-unknown); font-weight: 600; }' + sLineBreak +
    '.muted { color: var(--text-muted); font-style: italic; }' + sLineBreak +
    '.count-badge { color: var(--text-muted); font-weight: 400; font-size: 0.85em; margin-left: 0.4em; }' + sLineBreak +
    'details { background: var(--bg-card); border: 1px solid var(--border);' + sLineBreak +
    '          border-radius: 8px; padding: 0.8em 1.2em; margin: 1em 0; }' + sLineBreak +
    'details summary { cursor: pointer; font-weight: 600; color: var(--text-strong); padding: 0.3em 0; }' + sLineBreak +
    'details[open] summary { margin-bottom: 0.8em; }' + sLineBreak +
    'details ul { margin: 0.3em 0 1em; padding-left: 1.4em; }' + sLineBreak +
    'details li { margin: 0.25em 0; }' + sLineBreak +
    '@media print {' + sLineBreak +
    '  body { background: white; color: #1f2328; max-width: none; padding: 1cm; }' + sLineBreak +
    '  h1, h2, h3 { color: #1f2328; }' + sLineBreak +
    '  th, .meta-table td:first-child { background: #f6f8fa; color: #1f2328; }' + sLineBreak +
    '  tbody tr:nth-child(even) td { background: #fafbfc; }' + sLineBreak +
    '  table, .stat-card, details { background: white; border-color: #d0d7de; }' + sLineBreak +
    '  code, .hash { background: #f6f8fa; color: #1f2328; }' + sLineBreak +
    '  .stat-card .value { color: #1f2328; }' + sLineBreak +
    '  .muted, .stat-card .label { color: #656d76; }' + sLineBreak +
    '}';

//Escape characters that have meaning in HTML attribute / text content.
function HtmlEscape(const s : string) : string;
begin
  result := StringReplace(s, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
end;

//Escape characters with meaning in Markdown. We only escape the ones that matter
//inside table cells - the table separator `|` and the line-break, which would
//otherwise split a cell vertically.
function MdEscapeCell(const s : string) : string;
begin
  result := StringReplace(s, '|', '\|', [rfReplaceAll]);
  result := StringReplace(result, #13#10, ' ', [rfReplaceAll]);
  result := StringReplace(result, #10, ' ', [rfReplaceAll]);
  result := StringReplace(result, #13, ' ', [rfReplaceAll]);
end;

function CoalesceDash(const s : string) : string;
begin
  if Trim(s) = '' then
    result := '-'
  else
    result := s;
end;

function KindLabel(const kind : TSBOMComponentKind) : string;
begin
  case kind of
    TSBOMComponentKind.Application : result := 'Application';
    TSBOMComponentKind.DpmPackage : result := 'DPM package';
    TSBOMComponentKind.DelphiRuntime : result := 'Delphi runtime';
    TSBOMComponentKind.ThirdParty : result := 'Third-party';
    TSBOMComponentKind.Unidentified : result := 'Unidentified';
  else
    result := '';
  end;
end;

function KindCssClass(const kind : TSBOMComponentKind) : string;
begin
  case kind of
    TSBOMComponentKind.DpmPackage : result := 'kind-dpm';
    TSBOMComponentKind.DelphiRuntime : result := 'kind-runtime';
    TSBOMComponentKind.ThirdParty : result := 'kind-third';
    TSBOMComponentKind.Unidentified : result := 'kind-unknown';
  else
    result := '';
  end;
end;

//Joins authors into a single display string; empty -> '-'. Lives at unit scope
//because both writers compose author cells the same way.
function JoinAuthors(const comp : TSBOMComponent) : string;
var
  i : integer;
begin
  result := '';
  if (comp = nil) or (comp.Authors = nil) then
    exit;
  for i := 0 to comp.Authors.Count - 1 do
  begin
    if Trim(comp.Authors[i]) = '' then
      continue;
    if result = '' then
      result := comp.Authors[i]
    else
      result := result + ', ' + comp.Authors[i];
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

//Bucket components by kind so each writer can render them grouped. Application
//(the project root) is not in report.Components - it's the RootComponent.
type
  TComponentsByKind = record
    Dpm : IList<TSBOMComponent>;
    Runtime : IList<TSBOMComponent>;
    ThirdParty : IList<TSBOMComponent>;
    Unidentified : IList<TSBOMComponent>;
  end;

function BucketByKind(const report : TSBOMReport) : TComponentsByKind;
var
  comp : TSBOMComponent;
begin
  result.Dpm := TCollections.CreateList<TSBOMComponent>;
  result.Runtime := TCollections.CreateList<TSBOMComponent>;
  result.ThirdParty := TCollections.CreateList<TSBOMComponent>;
  result.Unidentified := TCollections.CreateList<TSBOMComponent>;
  for comp in report.Components do
  begin
    case comp.Kind of
      TSBOMComponentKind.DpmPackage : result.Dpm.Add(comp);
      TSBOMComponentKind.DelphiRuntime : result.Runtime.Add(comp);
      TSBOMComponentKind.ThirdParty : result.ThirdParty.Add(comp);
      TSBOMComponentKind.Unidentified : result.Unidentified.Add(comp);
    end;
  end;
end;

{ THTMLReportWriter }

function THTMLReportWriter.GetFormat : TSBOMFormat;
begin
  result := TSBOMFormat.HTML;
end;

function THTMLReportWriter.GetFileExtension : string;
begin
  result := '.html';
end;

procedure THTMLReportWriter.Write(const report : TSBOMReport; const fileName : string);

  function ExternalLinks(const comp : TSBOMComponent) : string;
  begin
    result := '';
    if comp.ProjectUrl <> '' then
      result := result + '<a href="' + HtmlEscape(comp.ProjectUrl) + '">project</a>';
    if comp.RepositoryUrl <> '' then
    begin
      if result <> '' then
        result := result + ' &middot; ';
      result := result + '<a href="' + HtmlEscape(comp.RepositoryUrl) + '">repo</a>';
    end;
    if result = '' then
      result := '<span class="muted">-</span>';
  end;

  function ComponentRow(const comp : TSBOMComponent) : string;
  var
    hashCell : string;
  begin
    if (comp.HashAlgorithm <> '') and (comp.HashValue <> '') then
      //Truncate the visible hash to 16 chars (fits the column without wrapping)
      //but stuff the full <algorithm>:<hash> into the title attribute so a
      //hover reveals what was packed - matches what reviewers paste into
      //external tools (purl checksum=, OSV lookups, etc.).
      hashCell := '<span class="hash" title="' + HtmlEscape(comp.HashAlgorithm + ':' + comp.HashValue) + '">'
                + HtmlEscape(Copy(comp.HashValue, 1, 16)) + '&hellip;</span>'
    else
      hashCell := '<span class="muted">-</span>';
    result :=
      '<tr>' +
        '<td><code>' + HtmlEscape(comp.Id) + '</code></td>' +
        '<td>' + HtmlEscape(CoalesceDash(comp.Version)) + '</td>' +
        '<td>' + HtmlEscape(CoalesceDash(comp.License)) + '</td>' +
        '<td>' + HtmlEscape(CoalesceDash(comp.Supplier)) + '</td>' +
        '<td>' + HtmlEscape(CoalesceDash(JoinAuthors(comp))) + '</td>' +
        '<td>' + ExternalLinks(comp) + '</td>' +
        '<td>' + hashCell + '</td>' +
      '</tr>' + sLineBreak;
  end;

  procedure AppendSection(const sb : TStringBuilder; const title : string; const list : IList<TSBOMComponent>);
  var
    comp : TSBOMComponent;
  begin
    if list.Count = 0 then
      exit;
    sb.Append('<h2>' + HtmlEscape(title) + '<span class="count-badge">(' + IntToStr(list.Count) + ')</span></h2>' + sLineBreak);
    sb.Append('<table><thead><tr>' +
              '<th>Id</th><th>Version</th><th>License</th><th>Supplier</th><th>Authors</th><th>Links</th><th>Hash</th>' +
              '</tr></thead><tbody>' + sLineBreak);
    for comp in list do
      sb.Append(ComponentRow(comp));
    sb.Append('</tbody></table>' + sLineBreak);
  end;

  procedure AppendStatCard(const sb : TStringBuilder; const cssClass, label_, value : string);
  begin
    sb.Append('<div class="stat-card ' + cssClass + '">' + sLineBreak);
    sb.Append('  <div class="label">' + HtmlEscape(label_) + '</div>' + sLineBreak);
    sb.Append('  <div class="value">' + HtmlEscape(value) + '</div>' + sLineBreak);
    sb.Append('</div>' + sLineBreak);
  end;

var
  buckets : TComponentsByKind;
  sb : TStringBuilder;
  evidenceCount : integer;
  comp : TSBOMComponent;
  ev : TSBOMEvidence;
begin
  buckets := BucketByKind(report);
  sb := TStringBuilder.Create;
  try
    sb.Append('<!DOCTYPE html>' + sLineBreak);
    sb.Append('<html lang="en"><head><meta charset="UTF-8">' + sLineBreak);
    sb.Append('<title>SBOM: ' + HtmlEscape(report.ProjectName) + ' (' + HtmlEscape(DPMPlatformToString(report.Platform)) + ')</title>' + sLineBreak);
    sb.Append('<style>' + cEmbeddedCSS + '</style>' + sLineBreak);
    sb.Append('</head><body>' + sLineBreak);

    sb.Append('<h1>Software Bill of Materials</h1>' + sLineBreak);
    sb.Append('<p class="lead">Machine- and human-readable inventory of every component linked into ' +
              '<strong>' + HtmlEscape(report.ProjectName) + '</strong> for the <strong>' +
              HtmlEscape(DPMPlatformToString(report.Platform)) +
              '</strong> platform. Generated from the project''s dependency graph and the linker MAP file.</p>' + sLineBreak);

    //Card row: at-a-glance counts per component kind. Mirrors the audit-style
    //summary panels in tools like DX.Comply so a reviewer can see "what's in
    //this binary" without scrolling.
    sb.Append('<div class="stat-grid">' + sLineBreak);
    AppendStatCard(sb, 'dpm', 'DPM packages', IntToStr(buckets.Dpm.Count));
    AppendStatCard(sb, 'runtime', 'Delphi runtime', IntToStr(buckets.Runtime.Count));
    AppendStatCard(sb, 'third', 'Third-party', IntToStr(buckets.ThirdParty.Count));
    AppendStatCard(sb, 'unknown', 'Unidentified', IntToStr(buckets.Unidentified.Count));
    sb.Append('</div>' + sLineBreak);

    sb.Append('<h2>Project</h2>' + sLineBreak);
    sb.Append('<table class="meta-table"><tbody>' + sLineBreak);
    sb.Append('<tr><td>Project</td><td>' + HtmlEscape(report.ProjectName) + '</td></tr>' + sLineBreak);
    sb.Append('<tr><td>Version</td><td>' + HtmlEscape(CoalesceDash(report.ProjectVersion)) + '</td></tr>' + sLineBreak);
    sb.Append('<tr><td>Platform</td><td>' + HtmlEscape(DPMPlatformToString(report.Platform)) + '</td></tr>' + sLineBreak);
    sb.Append('<tr><td>Generated</td><td>' + HtmlEscape(report.TimestampUtc) + '</td></tr>' + sLineBreak);
    sb.Append('<tr><td>Serial</td><td><code>' + HtmlEscape(report.SerialNumber) + '</code></td></tr>' + sLineBreak);
    sb.Append('<tr><td>Tool</td><td>' + HtmlEscape(report.ToolName) + ' ' + HtmlEscape(report.ToolVersion) + '</td></tr>' + sLineBreak);
    sb.Append('</tbody></table>' + sLineBreak);

    AppendSection(sb, 'DPM packages', buckets.Dpm);
    AppendSection(sb, 'Delphi runtime', buckets.Runtime);
    AppendSection(sb, 'Third-party', buckets.ThirdParty);
    AppendSection(sb, 'Unidentified', buckets.Unidentified);

    //Evidence summary in a collapsed <details> block - usually noisy but useful for audits.
    evidenceCount := 0;
    for comp in report.Components do
      evidenceCount := evidenceCount + comp.Evidence.Count;
    if evidenceCount > 0 then
    begin
      sb.Append('<h2>Evidence<span class="count-badge">(' + IntToStr(evidenceCount) + ' occurrences)</span></h2>' + sLineBreak);
      sb.Append('<details><summary>Show MAP-file evidence per component</summary>' + sLineBreak);
      for comp in report.Components do
      begin
        if comp.Evidence.Count = 0 then
          continue;
        sb.Append('<h3>' + HtmlEscape(comp.Id) + '</h3>' + sLineBreak);
        sb.Append('<ul>' + sLineBreak);
        for ev in comp.Evidence do
          sb.Append('<li><code>' + HtmlEscape(ev.Location) + '</code></li>' + sLineBreak);
        sb.Append('</ul>' + sLineBreak);
      end;
      sb.Append('</details>' + sLineBreak);
    end;

    sb.Append('</body></html>' + sLineBreak);
    WriteUtf8(fileName, sb.ToString);
  finally
    sb.Free;
  end;
end;

{ TMarkdownReportWriter }

function TMarkdownReportWriter.GetFormat : TSBOMFormat;
begin
  result := TSBOMFormat.Markdown;
end;

function TMarkdownReportWriter.GetFileExtension : string;
begin
  result := '.md';
end;

procedure TMarkdownReportWriter.Write(const report : TSBOMReport; const fileName : string);

  function LinksCell(const comp : TSBOMComponent) : string;
  begin
    result := '';
    if comp.ProjectUrl <> '' then
      result := '[project](' + comp.ProjectUrl + ')';
    if comp.RepositoryUrl <> '' then
    begin
      if result <> '' then
        result := result + ' · ';
      result := result + '[repo](' + comp.RepositoryUrl + ')';
    end;
    if result = '' then
      result := '-';
  end;

  function ComponentRow(const comp : TSBOMComponent) : string;
  var
    hashCell : string;
  begin
    if (comp.HashAlgorithm <> '') and (comp.HashValue <> '') then
      hashCell := '`' + Copy(comp.HashValue, 1, 16) + '…`'
    else
      hashCell := '-';
    result :=
      '| `' + MdEscapeCell(comp.Id) + '` ' +
      '| ' + MdEscapeCell(CoalesceDash(comp.Version)) + ' ' +
      '| ' + MdEscapeCell(CoalesceDash(comp.License)) + ' ' +
      '| ' + MdEscapeCell(CoalesceDash(comp.Supplier)) + ' ' +
      '| ' + MdEscapeCell(CoalesceDash(JoinAuthors(comp))) + ' ' +
      '| ' + MdEscapeCell(LinksCell(comp)) + ' ' +
      '| ' + hashCell + ' |' + sLineBreak;
  end;

  procedure AppendSection(const sb : TStringBuilder; const title : string; const list : IList<TSBOMComponent>);
  var
    comp : TSBOMComponent;
  begin
    if list.Count = 0 then
      exit;
    sb.Append('## ' + title + ' (' + IntToStr(list.Count) + ')' + sLineBreak + sLineBreak);
    sb.Append('| Id | Version | License | Supplier | Authors | Links | Hash |' + sLineBreak);
    sb.Append('|---|---|---|---|---|---|---|' + sLineBreak);
    for comp in list do
      sb.Append(ComponentRow(comp));
    sb.Append(sLineBreak);
  end;

var
  buckets : TComponentsByKind;
  sb : TStringBuilder;
  evidenceCount : integer;
  comp : TSBOMComponent;
  ev : TSBOMEvidence;
begin
  buckets := BucketByKind(report);
  sb := TStringBuilder.Create;
  try
    sb.Append('# Software Bill of Materials' + sLineBreak + sLineBreak);

    sb.Append('| Field | Value |' + sLineBreak);
    sb.Append('|---|---|' + sLineBreak);
    sb.Append('| Project | ' + MdEscapeCell(report.ProjectName) + ' |' + sLineBreak);
    sb.Append('| Version | ' + MdEscapeCell(CoalesceDash(report.ProjectVersion)) + ' |' + sLineBreak);
    sb.Append('| Platform | ' + MdEscapeCell(DPMPlatformToString(report.Platform)) + ' |' + sLineBreak);
    sb.Append('| Generated | ' + MdEscapeCell(report.TimestampUtc) + ' |' + sLineBreak);
    sb.Append('| Serial | `' + MdEscapeCell(report.SerialNumber) + '` |' + sLineBreak);
    sb.Append('| Tool | ' + MdEscapeCell(report.ToolName + ' ' + report.ToolVersion) + ' |' + sLineBreak);
    sb.Append(sLineBreak);

    sb.Append('## Summary' + sLineBreak + sLineBreak);
    sb.Append('- DPM packages: **' + IntToStr(buckets.Dpm.Count) + '**' + sLineBreak);
    sb.Append('- Delphi runtime: **' + IntToStr(buckets.Runtime.Count) + '**' + sLineBreak);
    sb.Append('- Third-party: **' + IntToStr(buckets.ThirdParty.Count) + '**' + sLineBreak);
    sb.Append('- Unidentified: **' + IntToStr(buckets.Unidentified.Count) + '**' + sLineBreak);
    sb.Append(sLineBreak);

    AppendSection(sb, 'DPM packages', buckets.Dpm);
    AppendSection(sb, 'Delphi runtime', buckets.Runtime);
    AppendSection(sb, 'Third-party', buckets.ThirdParty);
    AppendSection(sb, 'Unidentified', buckets.Unidentified);

    evidenceCount := 0;
    for comp in report.Components do
      evidenceCount := evidenceCount + comp.Evidence.Count;
    if evidenceCount > 0 then
    begin
      sb.Append('## Evidence (' + IntToStr(evidenceCount) + ' occurrences)' + sLineBreak + sLineBreak);
      sb.Append('<details><summary>MAP-file evidence per component</summary>' + sLineBreak + sLineBreak);
      for comp in report.Components do
      begin
        if comp.Evidence.Count = 0 then
          continue;
        sb.Append('### ' + comp.Id + sLineBreak + sLineBreak);
        for ev in comp.Evidence do
          sb.Append('- `' + ev.Location + '`' + sLineBreak);
        sb.Append(sLineBreak);
      end;
      sb.Append('</details>' + sLineBreak);
    end;

    WriteUtf8(fileName, sb.ToString);
  finally
    sb.Free;
  end;
end;

end.
