unit DPM.Core.Tests.SBOM.Reports;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSBOMReportsTests = class
  private
    function NewTempPath(const ext : string) : string;
    function ReadAllText(const fileName : string) : string;
    function BuildSampleReport : Pointer;  //returns TSBOMReport disguised
    function ContainsAll(const haystack : string; const needles : array of string) : boolean;
  public
    [Test]
    procedure HTML_HasDoctypeAndProjectTitle;
    [Test]
    procedure HTML_HasComponentSectionsAndCounts;
    [Test]
    procedure HTML_EscapesAngleBracketsInUserData;
    [Test]
    procedure HTML_HasEmbeddedCSS;
    [Test]
    procedure Markdown_HasFrontMatterTable;
    [Test]
    procedure Markdown_HasComponentTables;
    [Test]
    procedure Markdown_EscapesPipeInCellContent;
    [Test]
    procedure BothWriters_OmitEmptyComponentSections;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  DPM.Core.Types,
  DPM.Core.SBOM.Types,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.Options.SBOM,
  DPM.Core.SBOM.Writers.Reports;

function TSBOMReportsTests.NewTempPath(const ext : string) : string;
var
  g : TGUID;
  guidStr : string;
begin
  CreateGUID(g);
  guidStr := LowerCase(Copy(GUIDToString(g), 2, 36));
  result := TPath.Combine(TPath.GetTempPath, 'dpm-sbom-report-' + guidStr + ext);
end;

function TSBOMReportsTests.ReadAllText(const fileName : string) : string;
var
  bytes : TBytes;
begin
  bytes := TFile.ReadAllBytes(fileName);
  result := TEncoding.UTF8.GetString(bytes);
end;

function TSBOMReportsTests.ContainsAll(const haystack : string; const needles : array of string) : boolean;
var
  i : integer;
begin
  result := true;
  for i := Low(needles) to High(needles) do
  begin
    if Pos(needles[i], haystack) = 0 then
    begin
      result := false;
      exit;
    end;
  end;
end;

function TSBOMReportsTests.BuildSampleReport : Pointer;
var
  report : TSBOMReport;
  dpm : TSBOMComponent;
  runtime : TSBOMComponent;
  third : TSBOMComponent;
begin
  report := TSBOMReport.Create;
  report.SerialNumber := 'urn:uuid:abcdef01-2345-6789-abcd-ef0123456789';
  report.TimestampUtc := '2026-05-19T10:00:00Z';
  report.ToolName := 'dpm';
  report.ToolVersion := '1.0.0';
  report.ProjectName := 'DemoProject';
  report.ProjectVersion := '0.1.0';
  report.Platform := TDPMPlatform.Win64;
  report.CompilerVersion := TCompilerVersion.Delphi12_0;

  report.RootComponent.BomRef := 'project:DemoProject:win64';
  report.RootComponent.Id := 'DemoProject';
  report.RootComponent.Version := '0.1.0';

  dpm := report.AddComponent(TSBOMComponentKind.DpmPackage);
  dpm.BomRef := 'dpm:Sample<Package>@1.0';
  dpm.Id := 'Sample<Package>';  //angle brackets to exercise HTML escaping
  dpm.Version := '1.0';
  dpm.License := 'Apache-2.0';
  dpm.Supplier := 'Example | Corp';  //pipe to exercise Markdown escaping
  dpm.ProjectUrl := 'https://example.com';
  dpm.RepositoryUrl := 'https://github.com/example/sample';
  dpm.HashAlgorithm := 'sha256';
  dpm.HashValue := '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef';
  dpm.Authors.Add('Alice');
  dpm.Authors.Add('Bob');

  runtime := report.AddComponent(TSBOMComponentKind.DelphiRuntime);
  runtime.BomRef := 'embarcadero:delphi@23.0';
  runtime.Id := 'Delphi RTL/VCL/FMX';
  runtime.Version := '29.0.55362.2017';
  runtime.Supplier := 'Embarcadero Technologies';

  third := report.AddComponent(TSBOMComponentKind.ThirdParty);
  third.BomRef := 'thirdparty:somelib';
  third.Id := 'somelib';
  third.Version := 'unknown';

  result := report;
end;

procedure TSBOMReportsTests.HTML_HasDoctypeAndProjectTitle;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := THTMLReportWriter.Create;
    outPath := NewTempPath('.html');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(ContainsAll(text, ['<!DOCTYPE html>', '<html', 'Software Bill of Materials',
                                       'DemoProject', 'win64']),
                    'HTML missing structural / project markers');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.HTML_HasComponentSectionsAndCounts;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := THTMLReportWriter.Create;
    outPath := NewTempPath('.html');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(ContainsAll(text, ['DPM packages', 'Delphi runtime', 'Third-party']),
                    'expected section headings missing');
      //Stat cards: dpm count = 1, third-party count = 1, runtime count = 1.
      Assert.IsTrue(ContainsAll(text, ['stat-card dpm', 'stat-card runtime',
                                       'stat-card third', 'stat-card unknown']),
                    'expected stat-card markup missing');
      Assert.IsTrue(Pos('<div class="value">1</div>', text) > 0,
                    'expected at least one stat value of 1');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.HTML_EscapesAngleBracketsInUserData;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := THTMLReportWriter.Create;
    outPath := NewTempPath('.html');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      //Sample<Package> must be escaped - literal "Sample<Package>" would inject markup
      Assert.IsFalse(Pos('Sample<Package>', text) > 0, 'angle brackets not escaped');
      Assert.IsTrue(Pos('Sample&lt;Package&gt;', text) > 0, 'expected escaped form not present');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.HTML_HasEmbeddedCSS;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := THTMLReportWriter.Create;
    outPath := NewTempPath('.html');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('<style>', text) > 0, 'CSS block missing');
      Assert.IsTrue(Pos('border-collapse', text) > 0, 'CSS rules missing');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.Markdown_HasFrontMatterTable;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := TMarkdownReportWriter.Create;
    outPath := NewTempPath('.md');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(ContainsAll(text, ['# Software Bill of Materials',
                                       '| Project | DemoProject |',
                                       '| Platform | win64 |']),
                    'front-matter table is malformed or missing');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.Markdown_HasComponentTables;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := TMarkdownReportWriter.Create;
    outPath := NewTempPath('.md');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('## DPM packages (1)', text) > 0);
      Assert.IsTrue(Pos('## Delphi runtime (1)', text) > 0);
      Assert.IsTrue(Pos('## Third-party (1)', text) > 0);
      Assert.IsTrue(Pos('| Id | Version | License |', text) > 0,
                    'expected component table header missing');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.Markdown_EscapesPipeInCellContent;
var
  report : TSBOMReport;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  report := BuildSampleReport;
  try
    writer := TMarkdownReportWriter.Create;
    outPath := NewTempPath('.md');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      //Supplier was 'Example | Corp' - the inner pipe would break the table row
      //if it wasn't escaped to '\|'.
      Assert.IsTrue(Pos('Example \| Corp', text) > 0,
                    'pipe character in cell content was not escaped');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

procedure TSBOMReportsTests.BothWriters_OmitEmptyComponentSections;
var
  report : TSBOMReport;
  dpm : TSBOMComponent;
  writer : ISbomWriter;
  outPath : string;
  text : string;
begin
  //Project with only a DPM component - no runtime, no third-party, no unidentified.
  //The reports should NOT emit empty section headings for the missing kinds (that's noise).
  report := TSBOMReport.Create;
  try
    report.ProjectName := 'Tiny';
    report.RootComponent.Id := 'Tiny';
    report.RootComponent.Version := '0';
    report.TimestampUtc := '2026-05-19T00:00:00Z';
    report.SerialNumber := 'urn:uuid:000';
    report.ToolName := 'dpm';
    report.ToolVersion := '1.0';
    report.Platform := TDPMPlatform.Win32;
    dpm := report.AddComponent(TSBOMComponentKind.DpmPackage);
    dpm.BomRef := 'dpm:OnlyPkg@1';
    dpm.Id := 'OnlyPkg';
    dpm.Version := '1';

    writer := THTMLReportWriter.Create;
    outPath := NewTempPath('.html');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('<h2>DPM packages<span class="count-badge">(1)', text) > 0,
                    'DPM section heading missing for the one component');
      Assert.IsFalse(Pos('<h2>Delphi runtime<span class="count-badge">(', text) > 0,
                    'should not emit empty runtime section');
      Assert.IsFalse(Pos('<h2>Third-party<span class="count-badge">(', text) > 0,
                    'should not emit empty third-party section');
      Assert.IsFalse(Pos('<h2>Unidentified<span class="count-badge">(', text) > 0,
                    'should not emit empty unidentified section');
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;

    writer := TMarkdownReportWriter.Create;
    outPath := NewTempPath('.md');
    try
      writer.Write(report, outPath);
      text := ReadAllText(outPath);
      Assert.IsTrue(Pos('## DPM packages (1)', text) > 0);
      Assert.IsFalse(Pos('## Delphi runtime', text) > 0);
      Assert.IsFalse(Pos('## Third-party', text) > 0);
      Assert.IsFalse(Pos('## Unidentified', text) > 0);
    finally
      if FileExists(outPath) then
        DeleteFile(outPath);
    end;
  finally
    report.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSBOMReportsTests);

end.
