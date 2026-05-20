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

unit DPM.Console.Command.Scan;

interface

uses
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Logging,
  DPM.Console.ExitCodes,
  DPM.Console.Command.Base,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.Vuln.Interfaces,
  DPM.Core.Vuln.Types;

type
  TScanCommand = class(TBaseCommand)
  private
    FSbomReader : ISBOMReader;
    FSbomGenerator : ISbomGenerator;
    FScanner : IVulnScanner;
    FVexWriter : IVulnWriter;
    FCache : IVulnResponseCache;
  protected
    function Execute(const cancellationToken : ICancellationToken) : TExitCode; override;
    function ScanSbomFile(const cancellationToken : ICancellationToken;
                          const sbomPath, outputPath : string;
                          const failOn : TSeverity;
                          const writeToDir : boolean) : TExitCode;
    function ScanProject(const cancellationToken : ICancellationToken;
                         const projectPath : string;
                         const platforms : TDPMPlatforms;
                         const outputBase : string;
                         const failOn : TSeverity) : TExitCode;
  public
    constructor Create(const logger : ILogger;
                       const configurationManager : IConfigurationManager;
                       const sbomReader : ISBOMReader;
                       const sbomGenerator : ISbomGenerator;
                       const scanner : IVulnScanner;
                       const vexWriter : IVulnWriter;
                       const cache : IVulnResponseCache); reintroduce;
  end;

implementation

uses
  System.Types,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  DPM.Core.Options.Common,
  DPM.Core.Options.Scan,
  DPM.Core.Options.Sbom,
  DPM.Core.SBOM.Types;

constructor TScanCommand.Create(const logger : ILogger;
                                const configurationManager : IConfigurationManager;
                                const sbomReader : ISBOMReader;
                                const sbomGenerator : ISbomGenerator;
                                const scanner : IVulnScanner;
                                const vexWriter : IVulnWriter;
                                const cache : IVulnResponseCache);
begin
  inherited Create(logger, configurationManager);
  FSbomReader := sbomReader;
  FSbomGenerator := sbomGenerator;
  FScanner := scanner;
  FVexWriter := vexWriter;
  FCache := cache;
end;

function IsSbomFile(const path : string) : boolean;
var
  ext : string;
begin
  ext := LowerCase(ExtractFileExt(path));
  result := (ext = '.json');
end;

function IsProjectFile(const path : string) : boolean;
var
  ext : string;
begin
  ext := LowerCase(ExtractFileExt(path));
  result := (ext = '.dproj') or (ext = '.groupproj');
end;

function DefaultOutputPath(const inputPath : string) : string;
var
  ext : string;
  baseName : string;
  projectDir : string;
begin
  ext := LowerCase(ExtractFileExt(inputPath));
  if (ext = '.dproj') or (ext = '.groupproj') then
  begin
    //Project input -> default is "write per-platform .vex.json files beside
    //the project itself" (not "create a new subdir named after the project").
    //Use the project's directory verbatim; fall back to the cwd if the user
    //just typed the filename from inside the project folder.
    projectDir := ExtractFilePath(inputPath);
    if projectDir = '' then
      projectDir := GetCurrentDir;
    result := ExcludeTrailingPathDelimiter(projectDir);
  end
  else
  begin
    baseName := ChangeFileExt(inputPath, '');
    //If input is foo.cdx.json or foo.spdx.json keep that suffix off.
    if SameText(ExtractFileExt(baseName), '.cdx') or SameText(ExtractFileExt(baseName), '.spdx') then
      baseName := ChangeFileExt(baseName, '');
    result := baseName + '.vex.json';
  end;
end;

function TScanCommand.ScanSbomFile(const cancellationToken : ICancellationToken;
                                    const sbomPath, outputPath : string;
                                    const failOn : TSeverity;
                                    const writeToDir : boolean) : TExitCode;
var
  sbomReport : TSBOMReport;
  vulnReport : TVulnReport;
  outBase : string;
begin
  result := TExitCode.OK;
  try
    sbomReport := FSbomReader.ReadFromFile(sbomPath);
  except
    on e : Exception do
    begin
      Logger.Error('Could not read SBOM [' + sbomPath + ']: ' + e.Message);
      result := TExitCode.Error;
      exit;
    end;
  end;

  try
    vulnReport := nil;
    try
      try
        vulnReport := FScanner.Scan(cancellationToken, sbomReport);
      except
        on e : Exception do
        begin
          Logger.Error('Scan failed: ' + e.Message);
          result := TExitCode.Error;
          exit;
        end;
      end;

      if writeToDir then
        //outputPath is a directory; per-platform suffix is encoded in the SBOM
        //we just read (TSBOMReport.Platform), so build the filename here.
        outBase := IncludeTrailingPathDelimiter(outputPath) +
                   sbomReport.ProjectName + '-' + DPMPlatformToString(sbomReport.Platform) + '.vex.json'
      else
        outBase := outputPath;

      try
        FVexWriter.Write(sbomReport, vulnReport, outBase);
      except
        on e : Exception do
        begin
          Logger.Error('Could not write VEX report: ' + e.Message);
          result := TExitCode.Error;
          exit;
        end;
      end;

      Logger.Information(Format('[scan] %s: %d components scanned, %d vulnerable, max severity %s -> %s',
        [ExtractFileName(sbomPath), vulnReport.ComponentsScanned, vulnReport.ComponentsAffected,
         SeverityToString(vulnReport.MaxSeverity), outBase]));

      if (failOn <> TSeverity.None) and
         (Ord(vulnReport.MaxSeverity) >= Ord(failOn)) and
         (vulnReport.MaxSeverity <> TSeverity.Unknown) then
      begin
        Logger.Warning('[scan] max severity ' + SeverityToString(vulnReport.MaxSeverity) +
                       ' meets -fail-on=' + SeverityToString(failOn));
        result := TExitCode.Error;
      end;
    finally
      vulnReport.Free;
    end;
  finally
    sbomReport.Free;
  end;
end;

function TScanCommand.ScanProject(const cancellationToken : ICancellationToken;
                                  const projectPath : string;
                                  const platforms : TDPMPlatforms;
                                  const outputBase : string;
                                  const failOn : TSeverity) : TExitCode;
var
  tempRoot : string;
  sbomOpts : TSBOMOptions;
  generated : TArray<string>;
  sbomFile : string;
  outDir : string;
  worstExitCode : TExitCode;
  fileExit : TExitCode;
begin
  result := TExitCode.OK;
  tempRoot := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'dpm-scan-' + IntToHex(Random(MaxInt), 8);
  if not ForceDirectories(tempRoot) then
  begin
    Logger.Error('Could not create temp working folder for SBOM generation: ' + tempRoot);
    result := TExitCode.Error;
    exit;
  end;

  //outputBase is either a directory the user specified (we honour it),
  //or - for the default - the project's folder. Per-platform .vex.json files
  //land beside the corresponding SBOMs.
  if outputBase = '' then
  begin
    outDir := ExtractFilePath(projectPath);
    if outDir = '' then
      outDir := GetCurrentDir;
  end
  else if DirectoryExists(outputBase) or
          (ExtractFileExt(outputBase) = '') then
  begin
    outDir := outputBase;
    if not DirectoryExists(outDir) then
      if not ForceDirectories(outDir) then
      begin
        Logger.Error('Could not create output directory: ' + outDir);
        result := TExitCode.Error;
        exit;
      end;
  end
  else
  begin
    Logger.Error('-output for project input must be a directory (or omitted), not a file: ' + outputBase);
    result := TExitCode.InvalidArguments;
    exit;
  end;

  try
    sbomOpts := TSBOMOptions.Default.Clone;
    try
      //TSBOMOptions.Default is a separate singleton from TScanOptions.Default;
      //we have to seed its config-file etc from the common options too, otherwise
      //the SBOM generator's call to LoadConfig('') will fail.
      sbomOpts.ApplyCommon(TCommonOptions.Default);
      sbomOpts.ProjectPath := projectPath;
      sbomOpts.OutputDir := tempRoot;
      //Force just the CycloneDX format - we don't need SPDX/HTML/Markdown for the scan.
      sbomOpts.Formats := [TSBOMFormat.CycloneDX];
      if platforms <> [] then
        sbomOpts.Platforms := platforms;

      if not FSbomGenerator.Generate(cancellationToken, sbomOpts) then
      begin
        Logger.Error('SBOM generation failed - cannot scan.');
        result := TExitCode.Error;
        exit;
      end;
    finally
      sbomOpts.Free;
    end;

    generated := TDirectory.GetFiles(tempRoot, '*.cdx.json', TSearchOption.soAllDirectories);
    if Length(generated) = 0 then
    begin
      Logger.Warning('No SBOMs were produced - nothing to scan.');
      exit;
    end;

    worstExitCode := TExitCode.OK;
    for sbomFile in generated do
    begin
      if cancellationToken.IsCancelled then
        break;
      fileExit := ScanSbomFile(cancellationToken, sbomFile, outDir, failOn, true);
      //Propagate Error (1) but don't downgrade a worse code.
      if Ord(fileExit) > Ord(worstExitCode) then
        worstExitCode := fileExit;
    end;
    result := worstExitCode;
  finally
    //Best-effort cleanup.
    try
      if DirectoryExists(tempRoot) then
        TDirectory.Delete(tempRoot, true);
    except
      //swallow - leaving temp files behind is preferable to crashing the command.
    end;
  end;
end;

function TScanCommand.Execute(const cancellationToken : ICancellationToken) : TExitCode;
var
  inputPath : string;
  outputBase : string;
  options : TScanOptions;
  isProject : boolean;
begin
  options := TScanOptions.Default;
  options.ApplyCommon(TCommonOptions.Default);

  inputPath := options.InputPath;
  if inputPath = '' then
    inputPath := GetCurrentDir;
  if DirectoryExists(inputPath) then
  begin
    Logger.Error('Input must be an SBOM .json file or a .dproj / .groupproj, not a directory.');
    result := TExitCode.InvalidArguments;
    exit;
  end;
  //Normalise to an absolute path. The SBOM generator resolves the project's
  //relative MAP / output paths against ExtractFilePath(projectPath) - if that
  //comes in empty (because the user typed just the filename from inside the
  //project folder) it falls back to the drive root and we end up looking for
  //the MAP file at e.g. I:\Win32\Release\Foo.map.
  inputPath := TPath.GetFullPath(inputPath);
  options.InputPath := inputPath;

  if not options.Validate(Logger) then
  begin
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if not FileExists(inputPath) then
  begin
    Logger.Error('Input file not found: ' + inputPath);
    result := TExitCode.InvalidArguments;
    exit;
  end;

  isProject := IsProjectFile(inputPath);

  if (not isProject) and (not IsSbomFile(inputPath)) then
  begin
    Logger.Error('Unsupported input extension - expected .json (CycloneDX SBOM), .dproj or .groupproj.');
    result := TExitCode.InvalidArguments;
    exit;
  end;

  if options.OutputPath <> '' then
    outputBase := options.OutputPath
  else
    outputBase := DefaultOutputPath(inputPath);

  if Assigned(FCache) then
    FCache.BypassReads := options.NoCache;

  if isProject then
    result := ScanProject(cancellationToken, inputPath, options.Platforms, outputBase, options.FailOn)
  else
    result := ScanSbomFile(cancellationToken, inputPath, outputBase, options.FailOn, false);
end;

end.
