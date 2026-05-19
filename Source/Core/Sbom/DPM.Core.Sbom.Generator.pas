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

unit DPM.Core.SBOM.Generator;

interface

uses
  VSoft.CancellationToken,
  Spring.Collections,
  Spring.Container,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Options.Sbom,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Types,
  DPM.Core.Project.MapFile,
  DPM.Core.Project.SearchPaths;

const
  cSBOMWriterCycloneDX = 'sbom.writer.cyclonedx';
  cSBOMWriterSPDX      = 'sbom.writer.spdx';
  cSBOMWriterHTML      = 'sbom.writer.html';
  cSBOMWriterMarkdown  = 'sbom.writer.markdown';

type
  TSBOMGenerator = class(TInterfacedObject, ISbomGenerator)
  private
    FContainer : TContainer;
    FLogger : ILogger;
    FConfigManager : IConfigurationManager;
    FPackageCache : IPackageCache;
    FMapFileReader : IMapFileReader;
    //caches the per-compiler Delphi RootDir (registry read) for the lifetime
    //of one Generate call - keyed by ord(TCompilerVersion)
    FDelphiRootCache : IDictionary<integer, string>;
    //LowerCase(unitName) -> bomRef of the DPM component that ships it. Populated by
    //EnumeratePackages, consumed by ApplyMapFileEvidence when MAP entries lack a source path.
    //Cleared per-platform; survives across multiple dprojs when aggregating a .groupproj
    //so a package shared by two dprojs in a group only contributes once.
    FUnitOwnership : IDictionary<string, string>;
    //LowerCase(packageId + '@' + version) -> bomRef. Tracks which packages have already
    //been added to the current report so a second visit (same package from a different
    //dproj in an aggregated group) emits a fresh relationship edge but not a duplicate
    //component. Cleared per-platform.
    FVisitedPackages : IDictionary<string, string>;
  protected
    function Generate(const cancellationToken : ICancellationToken; const options : TSBOMOptions) : boolean;
    function GenerateForProject(const projectPath : string; const options : TSBOMOptions; const config : IConfiguration) : boolean;
    function GenerateForPlatform(const editor : IProjectEditor;
                                 const projectPath, projectName, outDir, configName : string;
                                 const compilerVersion : TCompilerVersion;
                                 const platform : TDPMPlatform;
                                 const projectVersion : string;
                                 const graphRoot : IPackageReference;
                                 const options : TSBOMOptions) : boolean;
    function GenerateForGroupAggregated(const groupProjPath : string; const dprojPaths : array of string;
                                        const options : TSBOMOptions; const config : IConfiguration;
                                        const cancellationToken : ICancellationToken) : boolean;
    procedure EnumeratePackages(const report : TSBOMReport; const graphRoot : IPackageReference;
                                const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                const parentBomRef : string);
    procedure AddDelphiRuntime(const report : TSBOMReport; const compilerVersion : TCompilerVersion);
    procedure ApplyMapFileEvidence(const report : TSBOMReport; const editor : IProjectEditor;
                                   const projectPath, dprojName, configName : string;
                                   const platform : TDPMPlatform; const options : TSBOMOptions;
                                   const compilerVersion : TCompilerVersion;
                                   const searchIndex : IUnitSearchIndex);
    function BuildSearchIndex(const editor : IProjectEditor; const dprojDir, configName : string;
                              const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IUnitSearchIndex;
    function GetIDELibraryPath(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : string;
    function WriteReport(const report : TSBOMReport; const options : TSBOMOptions; const outDir, projectName : string) : boolean;
    function GetDelphiRootDir(const compilerVersion : TCompilerVersion) : string;
    function ResolveWriter(const name : string) : ISbomWriter;
  public
    constructor Create(const container : TContainer;
                       const logger : ILogger;
                       const configManager : IConfigurationManager;
                       const packageCache : IPackageCache;
                       const mapFileReader : IMapFileReader);
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.StrUtils,
  System.DateUtils,
  System.Win.Registry,
  WinApi.Windows,
  DPM.Core.Constants,
  DPM.Core.Package.Classes,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Compiler.ProjectSettings,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Utils.Strings;

const
  //purl scheme used for all DPM-managed components. `pkg:dpm/` is not registered with
  //the purl spec, so we use the registered `pkg:generic/` escape hatch and tag the
  //origin via a property. Downstream SCA tools (Grype, OWASP Dependency-Track) accept
  //generic purls.
  cPurlNamespaceDpm = 'pkg:generic/dpm/';
  cPurlEmbarcaderoDelphi = 'pkg:generic/embarcadero/delphi@';

function NowUtcIso8601 : string;
var
  utc : TDateTime;
begin
  utc := TTimeZone.Local.ToUniversalTime(Now);
  result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', utc);
end;

function NewGuidString : string;
var
  g : TGUID;
begin
  CreateGUID(g);
  //GUIDToString returns "{...}" - strip braces and lowercase for uuid friendliness.
  result := LowerCase(Copy(GUIDToString(g), 2, 36));
end;

function PathStartsWith(const path, prefix : string) : boolean;
var
  p : string;
  q : string;
begin
  result := false;
  if (path = '') or (prefix = '') then
    exit;
  p := IncludeTrailingPathDelimiter(LowerCase(prefix));
  q := LowerCase(path);
  result := (Length(q) >= Length(p)) and (Copy(q, 1, Length(p)) = p);
end;

function ExpandProjectTokens(const value : string; const platform : TDPMPlatform; const configName : string) : string;
var
  s : string;
begin
  //OutputDir from IProjectConfiguration commonly carries $(Platform) and $(Config) tokens
  //("e.g. .\$(Platform)\$(Config)"). We do not aim to expand every MSBuild macro - only
  //the two that matter for locating per-(platform,config) build artefacts.
  s := value;
  s := StringReplace(s, '$(Platform)', DPMPlatformToBDString(platform), [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '$(Config)', configName, [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, '$(Configuration)', configName, [rfReplaceAll, rfIgnoreCase]);
  result := s;
end;

//Expand the BDS-family MSBuild macros that show up in IDE Library Path values
//('$(BDSLIB)\$(PLATFORM)\release' is the canonical Win32/Win64 entry).
//Without this expansion the registry-derived paths point at literal "$(BDSLIB)\..."
//directories that don't exist, the search index can't scan them, and we lose every
//IDE-installed third-party library as a source of unit-name evidence.
function ExpandBDSTokens(const value, delphiRoot : string;
                        const platform : TDPMPlatform;
                        const bdsVersion : string) : string;

  function ExpandEnv(const s : string) : string;
  var
    buf : array[0..1023] of Char;
    n : DWORD;
  begin
    if Pos('%', s) = 0 then
      exit(s);
    n := ExpandEnvironmentStrings(PChar(s), @buf[0], Length(buf));
    if (n = 0) or (n > DWORD(Length(buf))) then
      exit(s);
    SetString(result, PChar(@buf[0]), n - 1);
  end;

var
  s : string;
  publicDocs : string;
  userDocs : string;
begin
  s := value;
  if delphiRoot <> '' then
  begin
    s := StringReplace(s, '$(BDSLIB)', IncludeTrailingPathDelimiter(delphiRoot) + 'lib', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSBIN)', IncludeTrailingPathDelimiter(delphiRoot) + 'bin', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSINCLUDE)', IncludeTrailingPathDelimiter(delphiRoot) + 'include', [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDS)', delphiRoot, [rfReplaceAll, rfIgnoreCase]);
  end;
  if bdsVersion <> '' then
  begin
    publicDocs := ExpandEnv('%PUBLIC%\Documents\Embarcadero\Studio\' + bdsVersion);
    userDocs := ExpandEnv('%USERPROFILE%\Documents\Embarcadero\Studio\' + bdsVersion);
    s := StringReplace(s, '$(BDSCOMMONDIR)', publicDocs, [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSUSERDIR)', userDocs, [rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s, '$(BDSPROJECTSDIR)', IncludeTrailingPathDelimiter(userDocs) + 'Projects', [rfReplaceAll, rfIgnoreCase]);
  end;
  //$(PLATFORM) - uppercase variant the IDE uses in Library Path entries (the project uses $(Platform))
  s := StringReplace(s, '$(PLATFORM)', DPMPlatformToBDString(platform), [rfReplaceAll, rfIgnoreCase]);
  //Generic environment-variable expansion catches anything else (e.g. %ProgramFiles%)
  result := ExpandEnv(s);
end;

function ResolveAgainstDproj(const dprojPath, relativeOrAbsolute : string) : string;
var
  base : string;
begin
  if relativeOrAbsolute = '' then
  begin
    result := ExtractFilePath(dprojPath);
    exit;
  end;
  if TPath.IsPathRooted(relativeOrAbsolute) then
  begin
    result := relativeOrAbsolute;
    exit;
  end;
  base := IncludeTrailingPathDelimiter(ExtractFilePath(dprojPath));
  result := TPath.GetFullPath(base + relativeOrAbsolute);
end;

function IsWindowsPlatform(const platform : TDPMPlatform) : boolean;
begin
  result := platform in [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.WinARM64EC];
end;

function FirstPathSegmentUnder(const root, fullPath : string) : string;
var
  rootNorm : string;
  rest : string;
  sepPos : integer;
begin
  result := '';
  rootNorm := IncludeTrailingPathDelimiter(LowerCase(root));
  if (LowerCase(Copy(fullPath, 1, Length(rootNorm))) <> rootNorm) then
    exit;
  rest := Copy(fullPath, Length(rootNorm) + 1, MaxInt);
  if rest = '' then
    exit;
  sepPos := Pos(PathDelim, rest);
  if sepPos = 0 then
  begin
    //file directly under root - use its directory (likely the root itself), fall back to the
    //filename without extension as a stable component-id.
    result := ChangeFileExt(rest, '');
    exit;
  end;
  result := Copy(rest, 1, sepPos - 1);
end;

//Delegates to IPackageCache.GetPackageHash, which reads the sidecar and
//self-heals if missing. Kept as a thin wrapper to localise the cache-nil
//guard - the cache instance is optional for unit-tested generator scenarios.
function ReadPackageSidecarHash(const cache : IPackageCache; const identity : IPackageIdentity) : string;
begin
  result := '';
  if cache = nil then
    exit;
  result := cache.GetPackageHash(identity);
end;

//Returns true if the unit name is part of Delphi's shipped RTL/VCL/FMX/runtime
//distribution. Matches either a reserved namespace prefix (System., Vcl., Fmx., ...)
//or one of the bare legacy unit names that ship without a namespace (System, SysInit).
//
//Prefix is a strong signal but not absolute proof - a third-party could ship a unit
//called e.g. System.MyStuff. Callers that want to corroborate can additionally check
//the resolved file path against the Delphi RootDir; see ApplyMapFileEvidence.
function IsEmbarcaderoRuntimeUnit(const unitName : string) : boolean;
const
  cPrefixes : array[0..16] of string = (
    'System.', 'Vcl.', 'Fmx.', 'Winapi.', 'Data.', 'Datasnap.', 'Soap.',
    'Web.', 'Xml.', 'Bde.', 'IBX.', 'REST.', 'Posix.', 'Macapi.', 'IOSapi.',
    'Androidapi.', 'Bluetooth.'
  );
  //Bare unit names that ship as part of Delphi without a namespace. The big two are
  //System and SysInit (truly namespace-free RTL). Older units (Classes, Types, ...) get
  //resolved by the compiler's namespace search to System.Classes / System.Types and emit
  //the namespaced name in the MAP file, so we don't need them here for modern Delphi.
  cBareNames : array[0..1] of string = (
    'System', 'SysInit'
  );
var
  i : integer;
begin
  result := false;
  for i := Low(cPrefixes) to High(cPrefixes) do
  begin
    if StartsText(cPrefixes[i], unitName) then
    begin
      result := true;
      exit;
    end;
  end;
  for i := Low(cBareNames) to High(cBareNames) do
  begin
    if SameText(cBareNames[i], unitName) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function BuildDpmPurl(const id, version, repoUrl, repoCommit, hash : string) : string;
var
  qs : string;

  procedure Append(const key, value : string);
  begin
    if value = '' then
      exit;
    if qs = '' then
      qs := '?' + key + '=' + value
    else
      qs := qs + '&' + key + '=' + value;
  end;

begin
  qs := '';
  if hash <> '' then
    Append('checksum', 'sha256:' + hash);
  if repoUrl <> '' then
  begin
    if repoCommit <> '' then
      Append('vcs_url', repoUrl + '@' + repoCommit)
    else
      Append('vcs_url', repoUrl);
  end;
  result := cPurlNamespaceDpm + id + '@' + version + qs;
end;

{ TSBOMGenerator }

constructor TSBOMGenerator.Create(const container : TContainer;
                                   const logger : ILogger;
                                   const configManager : IConfigurationManager;
                                   const packageCache : IPackageCache;
                                   const mapFileReader : IMapFileReader);
begin
  inherited Create;
  FContainer := container;
  FLogger := logger;
  FConfigManager := configManager;
  FPackageCache := packageCache;
  FMapFileReader := mapFileReader;
  FDelphiRootCache := TCollections.CreateDictionary<integer, string>;
  FUnitOwnership := TCollections.CreateDictionary<string, string>;
  FVisitedPackages := TCollections.CreateDictionary<string, string>;
end;

function TSBOMGenerator.GetIDELibraryPath(const compilerVersion : TCompilerVersion;
                                          const platform : TDPMPlatform) : string;
var
  bdsVersion : string;
  key : string;
  reg : TRegistry;
begin
  result := '';
  if compilerVersion = TCompilerVersion.UnknownVersion then
    exit;
  try
    bdsVersion := CompilerToBDSVersion(compilerVersion);
  except
    exit;
  end;
  //IDE Library Path is per-platform, under HKCU\Software\Embarcadero\BDS\{bdsVer}\Library\{Platform}.
  //The value name is literally "Search Path" (with a space). Semicolon-separated.
  key := Format('Software\Embarcadero\BDS\%s\Library\%s', [bdsVersion, DPMPlatformToBDString(platform)]);
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(key) then
    begin
      try
        result := reg.ReadString('Search Path');
      except
        result := '';
      end;
    end;
  finally
    reg.Free;
  end;
end;

function TSBOMGenerator.BuildSearchIndex(const editor : IProjectEditor; const dprojDir, configName : string;
                                         const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IUnitSearchIndex;

  procedure AddSemicolonPaths(const index : IUnitSearchIndex; const value : string; const defaultOrigin : TUnitOrigin);
  var
    parts : TArray<string>;
    i : integer;
    item : string;
    resolved : string;
    origin : TUnitOrigin;
  begin
    if Trim(value) = '' then
      exit;
    parts := TStringUtils.SplitStr(value, ';');
    for i := 0 to Length(parts) - 1 do
    begin
      item := Trim(parts[i]);
      if item = '' then
        continue;
      //Resolve relative paths against the dproj dir before scanning.
      if TPath.IsPathRooted(item) then
        resolved := item
      else
        resolved := TPath.GetFullPath(IncludeTrailingPathDelimiter(dprojDir) + item);
      //Tag paths physically inside the project tree as Project; everything else inherits
      //the default origin (typically IdeLibrary or Other).
      if PathStartsWith(resolved, dprojDir) then
        origin := TUnitOrigin.Project
      else
        origin := defaultOrigin;
      index.AddSearchPath(resolved, origin);
    end;
  end;

var
  loader : IProjectSettingsLoader;
  projectSearchPath : string;
  ideSearchPath : string;
  delphiRoot : string;
  bdsVersion : string;
  sourceFile : string;
  resolvedSourcePath : string;
  sourceFolder : string;
begin
  result := TUnitSearchIndex.Create;

  delphiRoot := GetDelphiRootDir(compilerVersion);
  bdsVersion := '';
  try
    bdsVersion := CompilerToBDSVersion(compilerVersion);
  except
    //unknown compiler - leave bdsVersion blank; ExpandBDSTokens will just no-op the BDSCOMMONDIR/BDSUSERDIR substitutions
  end;

  //The dproj's own directory is implicitly on the compiler's search path - any
  //.pas / .dpr sitting next to the dproj resolves without being explicitly listed
  //in DCC_UnitSearchPath. Add it FIRST with Project origin so MAP entries that
  //lack a source path (e.g. release builds, default-named form units like
  //Unit1 / Form1) are correctly attributed to the project and filtered out of
  //the SBOM rather than landing in 'unidentified-third-party'.
  if dprojDir <> '' then
    result.AddSearchPath(dprojDir, TUnitOrigin.Project);

  //Pick up every folder that holds a project source file referenced from the
  //dproj's ItemGroup (subfolders like Forms\, relative siblings like ..\Common\).
  //First-path-wins inside the search index, so this preserves Project origin
  //for anything also reachable via DCC_UnitSearchPath / IDE Library Path.
  if editor <> nil then
  begin
    try
      for sourceFile in editor.GetSourceFiles do
      begin
        if Trim(sourceFile) = '' then
          continue;
        if TPath.IsPathRooted(sourceFile) then
          resolvedSourcePath := sourceFile
        else
          resolvedSourcePath := TPath.GetFullPath(IncludeTrailingPathDelimiter(dprojDir) + sourceFile);
        sourceFolder := ExtractFilePath(resolvedSourcePath);
        if sourceFolder = '' then
          continue;
        sourceFolder := ExcludeTrailingPathDelimiter(sourceFolder);
        result.AddSearchPath(sourceFolder, TUnitOrigin.Project);
      end;
    except
      on e : Exception do
        FLogger.Debug('[SBOM] could not enumerate project source files : ' + e.Message);
    end;

    try
      loader := TDPMProjectSettingsLoader.Create(FLogger, editor.ProjectFile, configName, platform);
      projectSearchPath := loader.GetSearchPath;
      projectSearchPath := ExpandProjectTokens(projectSearchPath, platform, configName);
      projectSearchPath := ExpandBDSTokens(projectSearchPath, delphiRoot, platform, bdsVersion);
      AddSemicolonPaths(result, projectSearchPath, TUnitOrigin.Other);
    except
      on e : Exception do
        FLogger.Warning('[SBOM] could not read project search path : ' + e.Message);
    end;
  end;

  ideSearchPath := GetIDELibraryPath(compilerVersion, platform);
  //The registry value is what the IDE writes; macros are unexpanded literals
  //("$(BDSLIB)\$(PLATFORM)\release"). Expansion is essential here or every IDE
  //library path becomes a non-existent directory and the scan finds nothing.
  ideSearchPath := ExpandBDSTokens(ideSearchPath, delphiRoot, platform, bdsVersion);
  AddSemicolonPaths(result, ideSearchPath, TUnitOrigin.IdeLibrary);
end;

function TSBOMGenerator.ResolveWriter(const name : string) : ISbomWriter;
begin
  result := nil;
  if FContainer = nil then
    exit;
  try
    result := FContainer.Resolve<ISbomWriter>(name);
  except
    on e : Exception do
      FLogger.Error('[SBOM] could not resolve writer [' + name + '] : ' + e.Message);
  end;
end;

function TSBOMGenerator.GetDelphiRootDir(const compilerVersion : TCompilerVersion) : string;
var
  cached : string;
  bdsVersion : string;
  key : string;
  reg : TRegistry;
begin
  result := '';
  if FDelphiRootCache.TryGetValue(Ord(compilerVersion), cached) then
  begin
    result := cached;
    exit;
  end;

  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    FDelphiRootCache.Add(Ord(compilerVersion), '');
    exit;
  end;

  try
    bdsVersion := CompilerToBDSVersion(compilerVersion);
  except
    FDelphiRootCache.Add(Ord(compilerVersion), '');
    exit;
  end;

  key := Format('Software\Embarcadero\BDS\%s', [bdsVersion]);
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(key) then
    begin
      try
        result := reg.ReadString('RootDir');
      except
        result := '';
      end;
    end;
  finally
    reg.Free;
  end;
  if result <> '' then
    result := ExcludeTrailingPathDelimiter(result);
  FDelphiRootCache.Add(Ord(compilerVersion), result);
end;

function TSBOMGenerator.Generate(const cancellationToken : ICancellationToken; const options : TSBOMOptions) : boolean;
var
  config : IConfiguration;
  projPath : string;
  ext : string;
  projects : IList<string>;
  groupReader : IGroupProjectReader;
  projectsArray : TArray<string>;
  i : integer;
  child : string;
  resolvedChild : string;
  ok : boolean;
begin
  result := false;
  if options = nil then
    exit;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
  begin
    FLogger.Error('Unable to load configuration.');
    exit;
  end;

  if FPackageCache.Location = '' then
    FPackageCache.Location := config.PackageCacheLocation;

  projPath := options.ProjectPath;
  if not FileExists(projPath) then
  begin
    FLogger.Error('Project file not found : ' + projPath);
    exit;
  end;

  ext := LowerCase(ExtractFileExt(projPath));
  if ext = '.groupproj' then
  begin
    projects := TCollections.CreateList<string>;
    groupReader := TGroupProjectReader.Create(FLogger);
    if not groupReader.LoadGroupProj(projPath) then
      exit;
    if not groupReader.ExtractProjects(projects) then
      exit;
    projectsArray := projects.ToArray;
    //Resolve all dproj paths relative to the .groupproj's directory up front so both
    //the legacy and aggregated paths can pass canonical absolute paths down.
    for i := 0 to Length(projectsArray) - 1 do
      projectsArray[i] := ResolveAgainstDproj(projPath, projectsArray[i]);

    if options.PerProject then
    begin
      //Legacy behaviour: one SBOM per dproj per platform. Useful for CI flows that
      //consume per-project artefacts (e.g. a per-binary attestation pipeline).
      ok := true;
      for i := 0 to Length(projectsArray) - 1 do
      begin
        if (cancellationToken <> nil) and cancellationToken.IsCancelled then
        begin
          ok := false;
          break;
        end;
        resolvedChild := projectsArray[i];
        ok := GenerateForProject(resolvedChild, options, config) and ok;
      end;
      result := ok;
    end
    else
    begin
      //Default: ONE aggregated SBOM per platform, with the group as root and
      //each dproj as an Application sub-component. Mirrors how a release engineer
      //thinks about a solution: 'these N projects ship together, here's what
      //they collectively pull in.'
      result := GenerateForGroupAggregated(projPath, projectsArray, options, config, cancellationToken);
    end;
  end
  else if ext = '.dproj' then
  begin
    result := GenerateForProject(projPath, options, config);
  end
  else
  begin
    FLogger.Error('Unsupported project type [' + ext + '] - expected .dproj or .groupproj');
    exit;
  end;
end;

function TSBOMGenerator.GenerateForProject(const projectPath : string; const options : TSBOMOptions; const config : IConfiguration) : boolean;
var
  editor : IProjectEditor;
  compilerVersion : TCompilerVersion;
  enabledPlatforms : TDPMPlatforms;
  selectedPlatforms : TDPMPlatforms;
  platform : TDPMPlatform;
  configNames : IReadOnlyList<string>;
  configName : string;
  outDir : string;
  projectName : string;
  projectVersion : string;
  graphRoot : IPackageReference;
  ok : boolean;

  function PickConfig : string;
  var
    name : string;
  begin
    result := '';
    if options.Config <> '' then
    begin
      for name in configNames do
        if SameText(name, options.Config) then
        begin
          result := name;
          exit;
        end;
      FLogger.Warning('Requested config [' + options.Config + '] not found in project - falling back to first available.');
    end;
    for name in configNames do
      if SameText(name, 'Release') then
      begin
        result := name;
        exit;
      end;
    for name in configNames do
      if SameText(name, 'Debug') then
      begin
        result := name;
        exit;
      end;
    if configNames.Count > 0 then
      result := configNames[0];
  end;

begin
  result := false;
  FLogger.Information('Generating SBOM for project : ' + projectPath);

  editor := TProjectEditor.Create(FLogger, config, TCompilerVersion.UnknownVersion);
  if not editor.LoadProject(projectPath, [TProjectElement.All]) then
    exit;

  compilerVersion := editor.CompilerVersion;
  if compilerVersion = TCompilerVersion.UnknownVersion then
  begin
    //fall back to the project version - useful for older dprojs without <DPMCompiler>.
    compilerVersion := ProjectVersionToCompilerVersion(editor.ProjectVersion);
    if compilerVersion = TCompilerVersion.UnknownVersion then
    begin
      FLogger.Error('Unable to determine compiler version for project : ' + projectPath);
      exit;
    end;
  end;

  enabledPlatforms := editor.Platforms;
  if enabledPlatforms = [] then
  begin
    FLogger.Error('Project has no enabled platforms : ' + projectPath);
    exit;
  end;
  if options.Platforms = [] then
    selectedPlatforms := enabledPlatforms
  else
    selectedPlatforms := options.Platforms * enabledPlatforms;

  if selectedPlatforms = [] then
  begin
    FLogger.Error('No requested platform is enabled in the project [' + DPMPlatformsToString(options.Platforms) + ']');
    exit;
  end;

  configNames := editor.GetConfigNames;
  configName := PickConfig;
  if configName = '' then
    configName := 'Release';

  outDir := options.OutputDir;
  if outDir = '' then
    outDir := ExtractFilePath(projectPath);
  outDir := ExcludeTrailingPathDelimiter(outDir);
  if not ForceDirectories(outDir) then
  begin
    FLogger.Error('Unable to create output directory : ' + outDir);
    exit;
  end;

  projectName := ChangeFileExt(ExtractFileName(projectPath), '');
  projectVersion := editor.ProjectVersion;
  graphRoot := editor.GetPackageReferences;

  ok := true;
  for platform in selectedPlatforms do
  begin
    if not GenerateForPlatform(editor, projectPath, projectName, outDir, configName,
                               compilerVersion, platform, projectVersion, graphRoot, options) then
      ok := false;
  end;
  result := ok;
end;

function TSBOMGenerator.GenerateForGroupAggregated(const groupProjPath : string; const dprojPaths : array of string;
                                                    const options : TSBOMOptions; const config : IConfiguration;
                                                    const cancellationToken : ICancellationToken) : boolean;
type
  TDprojCtx = record
    Path : string;
    Name : string;
    Editor : IProjectEditor;
    CompilerVersion : TCompilerVersion;
    EnabledPlatforms : TDPMPlatforms;
    ConfigName : string;
    GraphRoot : IPackageReference;
    AppBomRef : string;  //bomRef of the Application sub-component added per platform
    ConfigNames : IReadOnlyList<string>;
  end;
var
  contexts : TArray<TDprojCtx>;
  i : integer;
  ctx : TDprojCtx;
  groupName : string;
  outDir : string;
  unionPlatforms : TDPMPlatforms;
  selectedPlatforms : TDPMPlatforms;
  platform : TDPMPlatform;
  report : TSBOMReport;
  dprojApp : TSBOMComponent;
  runtimeCompiler : TCompilerVersion;
  ok : boolean;

  function PickConfig(const names : IReadOnlyList<string>) : string;
  var
    name : string;
  begin
    result := '';
    if options.Config <> '' then
    begin
      for name in names do
        if SameText(name, options.Config) then
          exit(name);
    end;
    for name in names do
      if SameText(name, 'Release') then
        exit(name);
    for name in names do
      if SameText(name, 'Debug') then
        exit(name);
    if names.Count > 0 then
      result := names[0];
  end;

begin
  result := false;
  if Length(dprojPaths) = 0 then
  begin
    FLogger.Error('Group project [' + groupProjPath + '] contains no projects.');
    exit;
  end;

  FLogger.Information('Generating aggregated SBOM for group : ' + groupProjPath);

  //First pass: load every dproj and capture its compiler / platforms / config.
  //Doing this once up front lets us iterate platforms in the outer loop and
  //compute the union of platforms across the whole group.
  SetLength(contexts, Length(dprojPaths));
  unionPlatforms := [];
  for i := 0 to Length(dprojPaths) - 1 do
  begin
    contexts[i].Path := dprojPaths[i];
    contexts[i].Name := ChangeFileExt(ExtractFileName(dprojPaths[i]), '');
    contexts[i].Editor := TProjectEditor.Create(FLogger, config, TCompilerVersion.UnknownVersion);
    if not contexts[i].Editor.LoadProject(dprojPaths[i], [TProjectElement.All]) then
    begin
      FLogger.Error('[SBOM] could not load dproj [' + dprojPaths[i] + '] - skipping in group aggregation.');
      continue;
    end;
    contexts[i].CompilerVersion := contexts[i].Editor.CompilerVersion;
    if contexts[i].CompilerVersion = TCompilerVersion.UnknownVersion then
      contexts[i].CompilerVersion := ProjectVersionToCompilerVersion(contexts[i].Editor.ProjectVersion);
    contexts[i].EnabledPlatforms := contexts[i].Editor.Platforms;
    contexts[i].ConfigNames := contexts[i].Editor.GetConfigNames;
    contexts[i].ConfigName := PickConfig(contexts[i].ConfigNames);
    if contexts[i].ConfigName = '' then
      contexts[i].ConfigName := 'Release';
    contexts[i].GraphRoot := contexts[i].Editor.GetPackageReferences;
    unionPlatforms := unionPlatforms + contexts[i].EnabledPlatforms;
  end;

  if unionPlatforms = [] then
  begin
    FLogger.Error('No enabled platforms across any dproj in [' + groupProjPath + ']');
    exit;
  end;

  if options.Platforms = [] then
    selectedPlatforms := unionPlatforms
  else
    selectedPlatforms := options.Platforms * unionPlatforms;
  if selectedPlatforms = [] then
  begin
    FLogger.Error('No requested platform is enabled in any dproj of the group [' + DPMPlatformsToString(options.Platforms) + ']');
    exit;
  end;

  groupName := ChangeFileExt(ExtractFileName(groupProjPath), '');
  outDir := options.OutputDir;
  if outDir = '' then
    outDir := ExtractFilePath(groupProjPath);
  outDir := ExcludeTrailingPathDelimiter(outDir);
  if not ForceDirectories(outDir) then
  begin
    FLogger.Error('Unable to create output directory : ' + outDir);
    exit;
  end;

  //Per-platform outer loop: build one report containing every dproj's contribution.
  //Components dedup via FVisitedPackages (DPM packages keyed by id@version) and the
  //idempotent AddDelphiRuntime; the resulting tree is
  //   <group>
  //     +-- <dproj1 app> -> <pkg> -> ...
  //     +-- <dproj2 app> -> <pkg> -> ...
  //     +-- <Delphi runtime>
  ok := true;
  for platform in selectedPlatforms do
  begin
    if (cancellationToken <> nil) and cancellationToken.IsCancelled then
    begin
      ok := false;
      break;
    end;

    report := TSBOMReport.Create;
    try
      report.SerialNumber := 'urn:uuid:' + NewGuidString;
      report.TimestampUtc := NowUtcIso8601;
      report.ToolName := 'dpm';
      report.ToolVersion := cDPMClientVersion;
      report.ProjectName := groupName;
      report.ProjectVersion := '';
      report.Platform := platform;

      report.RootComponent.BomRef := 'group:' + groupName + ':' + DPMPlatformToString(platform);
      report.RootComponent.Id := groupName;
      report.RootComponent.Version := '0.0.0';
      report.AddMetaProperty('dpm:groupProject', ExtractFileName(groupProjPath));
      report.AddMetaProperty('dpm:projectCount', IntToStr(Length(contexts)));

      //Per-platform classifier state (see GenerateForPlatform note).
      FUnitOwnership.Clear;
      FVisitedPackages.Clear;
      runtimeCompiler := TCompilerVersion.UnknownVersion;

      for i := 0 to Length(contexts) - 1 do
      begin
        ctx := contexts[i];
        if ctx.Editor = nil then
          continue;  //failed to load above
        //Skip dprojs that don't target this platform - we don't want noise from
        //a Win32-only utility appearing on the Linux64 SBOM.
        if not (platform in ctx.EnabledPlatforms) then
          continue;

        //Application sub-component per dproj. Becomes the parent for all of this
        //dproj's package edges, so the dependency tree shows which packages each
        //dproj pulls in (vs. just a flat list under the group root).
        dprojApp := report.AddComponent(TSBOMComponentKind.Application);
        ctx.AppBomRef := 'project:' + ctx.Name + ':' + DPMPlatformToString(platform);
        dprojApp.BomRef := ctx.AppBomRef;
        dprojApp.Id := ctx.Name;
        if ctx.Editor.ProjectVersion <> '' then
          dprojApp.Version := ctx.Editor.ProjectVersion
        else
          dprojApp.Version := '0.0.0';
        report.AddRelationship(report.RootComponent.BomRef, ctx.AppBomRef);
        contexts[i].AppBomRef := ctx.AppBomRef;

        try
          EnumeratePackages(report, ctx.GraphRoot, ctx.CompilerVersion, platform, ctx.AppBomRef);
        except
          on e : Exception do
            FLogger.Error('[SBOM] error enumerating packages for ' + ctx.Name + ' : ' + e.Message);
        end;

        //First dproj wins the compiler-version vote for the runtime component.
        //Cross-compiler groups are unusual; if it happens we still emit one
        //runtime per distinct compiler version (AddDelphiRuntime is idempotent
        //per bom-ref).
        if runtimeCompiler = TCompilerVersion.UnknownVersion then
          runtimeCompiler := ctx.CompilerVersion;
        if options.IncludeRuntime then
          AddDelphiRuntime(report, ctx.CompilerVersion);

        if IsWindowsPlatform(platform) then
        begin
          try
            ApplyMapFileEvidence(report, ctx.Editor, ctx.Path, ctx.Name, ctx.ConfigName,
                                 platform, options, ctx.CompilerVersion,
                                 BuildSearchIndex(ctx.Editor, ExcludeTrailingPathDelimiter(ExtractFilePath(ctx.Path)),
                                                  ctx.ConfigName, platform, ctx.CompilerVersion));
          except
            on e : Exception do
              FLogger.Error('[SBOM] error processing MAP file for ' + ctx.Name + ' : ' + e.Message);
          end;
        end
        else
          report.AddMetaProperty('dpm:evidence', 'platform-not-supported:' + DPMPlatformToString(platform));
      end;

      if not WriteReport(report, options, outDir, groupName) then
        ok := false;
    finally
      report.Free;
    end;
  end;
  result := ok;
end;

function TSBOMGenerator.GenerateForPlatform(const editor : IProjectEditor;
                                            const projectPath, projectName, outDir, configName : string;
                                            const compilerVersion : TCompilerVersion;
                                            const platform : TDPMPlatform;
                                            const projectVersion : string;
                                            const graphRoot : IPackageReference;
                                            const options : TSBOMOptions) : boolean;
var
  report : TSBOMReport;
begin
  report := TSBOMReport.Create;
  try
    report.SerialNumber := 'urn:uuid:' + NewGuidString;
    report.TimestampUtc := NowUtcIso8601;
    report.ToolName := 'dpm';
    report.ToolVersion := cDPMClientVersion;
    report.ProjectName := projectName;
    report.ProjectVersion := projectVersion;
    report.Platform := platform;
    report.CompilerVersion := compilerVersion;

    report.RootComponent.BomRef := 'project:' + projectName + ':' + DPMPlatformToString(platform);
    report.RootComponent.Id := projectName;
    if projectVersion <> '' then
      report.RootComponent.Version := projectVersion
    else
      report.RootComponent.Version := '0.0.0';

    //Fresh per-platform: the unit-ownership map, the package-visited map, and the
    //search index are all keyed on the unit / package name and would be wrong if
    //reused across platforms (different lib folder per platform, different IDE
    //Library Path). In aggregation mode they're cleared per-platform too but kept
    //across all dprojs within that platform.
    FUnitOwnership.Clear;
    FVisitedPackages.Clear;

    try
      EnumeratePackages(report, graphRoot, compilerVersion, platform, report.RootComponent.BomRef);
    except
      on e : Exception do
        FLogger.Error('[SBOM] error enumerating packages : ' + e.Message);
    end;

    if options.IncludeRuntime then
      AddDelphiRuntime(report, compilerVersion);

    if IsWindowsPlatform(platform) then
    begin
      try
        ApplyMapFileEvidence(report, editor, projectPath, projectName, configName, platform, options,
                             compilerVersion,
                             BuildSearchIndex(editor, ExcludeTrailingPathDelimiter(ExtractFilePath(projectPath)),
                                              configName, platform, compilerVersion));
      except
        on e : Exception do
          FLogger.Error('[SBOM] error processing MAP file : ' + e.Message);
      end;
    end
    else
    begin
      report.AddMetaProperty('dpm:evidence', 'platform-not-supported:' + DPMPlatformToString(platform));
      FLogger.Information('[SBOM] MAP-file parsing not yet supported on platform [' + DPMPlatformToString(platform) + '] - emitting package + runtime SBOM only.');
    end;

    result := WriteReport(report, options, outDir, projectName);
  finally
    report.Free;
  end;
end;

procedure TSBOMGenerator.EnumeratePackages(const report : TSBOMReport;
                                            const graphRoot : IPackageReference;
                                            const compilerVersion : TCompilerVersion;
                                            const platform : TDPMPlatform;
                                            const parentBomRef : string);

  //Glob *.pas / *.dcu in [folder] and register each as owned by [ownerBomRef] in
  //FUnitOwnership. Silent on missing folders or glob errors - this is best-effort
  //evidence-attribution, never blocks SBOM emission.
  procedure HarvestUnits(const folder, ownerBomRef : string; const pattern : string);
  var
    files : TStringDynArray;
    i : integer;
    unitKey : string;
  begin
    if (folder = '') or (not DirectoryExists(folder)) then
      exit;
    try
      files := TDirectory.GetFiles(folder, pattern, TSearchOption.soTopDirectoryOnly);
    except
      SetLength(files, 0);
    end;
    for i := 0 to Length(files) - 1 do
    begin
      unitKey := LowerCase(ChangeFileExt(ExtractFileName(files[i]), ''));
      //First package to claim a unit wins - subsequent packages don't shadow it
      //(the dependency graph already deduplicates by id, so a real collision
      //is rare and would indicate a packaging mistake).
      if not FUnitOwnership.ContainsKey(unitKey) then
        FUnitOwnership.Add(unitKey, ownerBomRef);
    end;
  end;

  procedure PopulateUnitOwnership(const identity : IPackageIdentity; const ownerBomRef : string);
  var
    packageFolder : string;
    libFolder : string;
  begin
    packageFolder := IncludeTrailingPathDelimiter(FPackageCache.GetPackagePath(identity));
    //Compile-on-install mode: .dcu files written into lib\<BDSPlatform> by the installer
    //(see DPM.Core.Package.Installer.pas:388-389 - Compiler.LibOutputDir is set there).
    libFolder := packageFolder + 'lib' + PathDelim + DPMPlatformToBDString(platform);
    HarvestUnits(libFolder, ownerBomRef, '*.dcu');
    //Source-mode: walk the well-known package source roots. We don't have a per-package
    //"give me the source folder" API on IPackageMetadata, so we glob the conventional
    //roots a DPM package commonly ships: source / src / lib. First package to claim each
    //unit wins, so order matters only for collisions across packages, not for these three.
    HarvestUnits(packageFolder + 'source', ownerBomRef, '*.pas');
    HarvestUnits(packageFolder + 'src', ownerBomRef, '*.pas');
    HarvestUnits(packageFolder + 'lib', ownerBomRef, '*.pas');
  end;

  procedure Visit(const node : IPackageReference; const parentBomRef : string);
  var
    key : string;
    bomRef : string;
    identity : IPackageIdentity;
    spec : IPackageSpec;
    metadata : ISpecMetaData;
    comp : TSBOMComponent;
    hash : string;
    author : string;
    tag : string;
    child : IPackageReference;
    framework : TDPMUIFrameworkType;
  begin
    if node = nil then
      exit;
    if node.IsRoot then
    begin
      //Top-level children attach to whatever the caller specified as the parent
      //(report root for single-dproj reports, dproj application sub-component
      //for aggregated groupproj reports).
      for child in node.Children do
        Visit(child, parentBomRef);
      exit;
    end;

    key := LowerCase(node.Id + '@' + node.Version.ToStringNoMeta);
    if FVisitedPackages.TryGetValue(key, bomRef) then
    begin
      //already a component - just add a fresh edge. In aggregation mode this is
      //how a shared package surfaces under two parent dprojs.
      if parentBomRef <> '' then
        report.AddRelationship(parentBomRef, bomRef);
      for child in node.Children do
        Visit(child, bomRef);
      exit;
    end;

    identity := TPackageIdentity.Create('', node.Id, node.Version, compilerVersion);
    spec := nil;
    try
      spec := FPackageCache.GetPackageManifest(identity);
    except
      on e : Exception do
        FLogger.Warning('[SBOM] could not load manifest for ' + node.Id + ' ' + node.Version.ToStringNoMeta + ' : ' + e.Message);
    end;

    comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
    bomRef := 'dpm:' + node.Id + '@' + node.Version.ToStringNoMeta;
    comp.BomRef := bomRef;
    comp.Id := node.Id;
    comp.Version := node.Version.ToStringNoMeta;
    FVisitedPackages.Add(key, bomRef);

    hash := ReadPackageSidecarHash(FPackageCache, identity);
    if hash <> '' then
    begin
      comp.HashAlgorithm := cPackageHashAlgorithm;
      comp.HashValue := hash;
    end;

    if spec <> nil then
    begin
      metadata := spec.MetaData;
      if metadata <> nil then
      begin
        comp.Description := metadata.Description;
        comp.License := metadata.License;
        comp.Copyright := metadata.Copyright;
        comp.ProjectUrl := metadata.ProjectUrl;
        comp.RepositoryUrl := metadata.RepositoryUrl;
        comp.RepositoryType := metadata.RepositoryType;
        comp.RepositoryBranch := metadata.RepositoryBranch;
        comp.RepositoryCommit := metadata.RepositoryCommit;
        if metadata.Authors <> nil then
          for author in metadata.Authors do
            if Trim(author) <> '' then
              comp.Authors.Add(author);
        if metadata.Tags <> nil then
          for tag in metadata.Tags do
            if Trim(tag) <> '' then
              comp.Tags.Add(tag);
        for framework in metadata.Frameworks do
          comp.AddProperty('dpm:framework', UIFrameworkTypeToString(framework));
        if metadata.IsTrial then
          comp.AddProperty('dpm:trial', 'true');
        if metadata.IsCommercial then
          comp.AddProperty('dpm:commercial', 'true');
      end;
    end;

    if node.UseSource then
      comp.AddProperty('dpm:useSource', 'true');
    comp.AddProperty('dpm:purl-type', 'dpm');

    comp.Purl := BuildDpmPurl(comp.Id, comp.Version, comp.RepositoryUrl, comp.RepositoryCommit, comp.HashValue);

    //Populate the unit-ownership map so ApplyMapFileEvidence can attribute
    //MAP entries that lack a source path to the right DPM component.
    try
      PopulateUnitOwnership(identity, bomRef);
    except
      on e : Exception do
        FLogger.Debug('[SBOM] could not harvest units for ' + node.Id + ' : ' + e.Message);
    end;

    if parentBomRef <> '' then
      report.AddRelationship(parentBomRef, bomRef);

    for child in node.Children do
      Visit(child, bomRef);
  end;

begin
  if graphRoot = nil then
    exit;
  //FVisitedPackages persists across multiple calls within one platform pass so the
  //aggregated-groupproj path can dedup packages shared between dprojs. The caller
  //(GenerateForPlatform / GenerateForGroupAggregated) clears it once per platform.
  Visit(graphRoot, parentBomRef);
end;

//Returns the file-version string of the named exe (e.g. '23.0.55321.6840' for dcc32.exe
//on Delphi 12 Update 1). Empty on any failure - caller falls back to the BDS major.minor.
function GetExeFileVersion(const fileName : string) : string;
var
  size : DWORD;
  handle : DWORD;
  buf : array of byte;
  fixed : PVSFixedFileInfo;
  fixedLen : UINT;
  hi, lo : DWORD;
begin
  result := '';
  if (fileName = '') or (not FileExists(fileName)) then
    exit;
  size := GetFileVersionInfoSize(PChar(fileName), handle);
  if size = 0 then
    exit;
  SetLength(buf, size);
  if not GetFileVersionInfo(PChar(fileName), 0, size, @buf[0]) then
    exit;
  fixed := nil;
  if not VerQueryValue(@buf[0], '\', Pointer(fixed), fixedLen) then
    exit;
  if (fixed = nil) or (fixedLen < SizeOf(TVSFixedFileInfo)) then
    exit;
  hi := fixed.dwFileVersionMS;
  lo := fixed.dwFileVersionLS;
  result := Format('%d.%d.%d.%d',
    [(hi shr 16) and $FFFF, hi and $FFFF, (lo shr 16) and $FFFF, lo and $FFFF]);
end;

procedure TSBOMGenerator.AddDelphiRuntime(const report : TSBOMReport; const compilerVersion : TCompilerVersion);
var
  comp : TSBOMComponent;
  bds : string;
  delphiRoot : string;
  dccPath : string;
  fileVersion : string;
  existing : TSBOMComponent;
  bomRefCandidate : string;
begin
  //Idempotent: in aggregation mode this is called once per dproj. All dprojs in
  //a group normally target the same compiler so they'd produce identical runtime
  //components. Skip if one with the same bom-ref is already present.
  try
    bds := CompilerToBDSVersion(compilerVersion);
  except
    bds := '0.0';
  end;
  bomRefCandidate := 'embarcadero:delphi@' + bds;
  for existing in report.Components do
    if (existing.Kind = TSBOMComponentKind.DelphiRuntime) and (existing.BomRef = bomRefCandidate) then
    begin
      //Already added by an earlier dproj in the group - relationship from root
      //is already in place, nothing more to do.
      exit;
    end;

  comp := report.AddComponent(TSBOMComponentKind.DelphiRuntime);
  comp.BomRef := bomRefCandidate;
  comp.Id := 'Delphi RTL/VCL/FMX';
  //Prefer the actual file version of dcc32.exe (e.g. '23.0.55321.6840') over the BDS
  //major.minor - it pins the SBOM to the exact compiler build the project was compiled
  //against, which is what matters for vulnerability cross-referencing later.
  delphiRoot := GetDelphiRootDir(compilerVersion);
  if delphiRoot <> '' then
  begin
    dccPath := IncludeTrailingPathDelimiter(delphiRoot) + 'bin' + PathDelim + 'dcc32.exe';
    fileVersion := GetExeFileVersion(dccPath);
    if fileVersion <> '' then
    begin
      comp.Version := fileVersion;
      comp.AddProperty('dpm:bdsVersion', bds);
      comp.AddProperty('dpm:compilerExe', dccPath);
    end;
  end;
  if comp.Version = '' then
    comp.Version := bds;
  comp.Supplier := 'Embarcadero Technologies';
  comp.ProjectUrl := 'https://www.embarcadero.com/products/delphi';
  comp.Purl := cPurlEmbarcaderoDelphi + comp.Version;
  comp.AddProperty('dpm:compilerName', CompilerWithCodeName(compilerVersion));

  report.AddRelationship(report.RootComponent.BomRef, comp.BomRef);
end;

procedure TSBOMGenerator.ApplyMapFileEvidence(const report : TSBOMReport; const editor : IProjectEditor;
                                              const projectPath, dprojName, configName : string;
                                              const platform : TDPMPlatform; const options : TSBOMOptions;
                                              const compilerVersion : TCompilerVersion;
                                              const searchIndex : IUnitSearchIndex);
var
  dprojDir : string;
  delphiRoot : string;
  cacheRoot : string;
  mapPath : string;
  outputDir : string;
  projectConfig : IProjectConfiguration;
  info : IMapFileInfo;
  u : TMapUnit;
  comp : TSBOMComponent;
  runtimeComp : TSBOMComponent;
  thirdParty : IDictionary<string, TSBOMComponent>;
  unidentified : TSBOMComponent;
  evidenceLoc : string;
  groupId : string;
  groupKey : string;
  bomRef : string;
  dpkgPackageId : string;

  //Lazy: created on first hit so SBOMs with full evidence don't contain
  //an empty 'unidentified-third-party' component.
  procedure EnsureUnidentified;
  begin
    if unidentified <> nil then
      exit;
    unidentified := report.AddComponent(TSBOMComponentKind.Unidentified);
    unidentified.BomRef := 'unidentified:third-party';
    unidentified.Id := 'unidentified-third-party';
    unidentified.Version := 'unknown';
    report.AddRelationship(report.RootComponent.BomRef, unidentified.BomRef);
  end;

  //Look up [groupKey] in thirdParty or create a new ThirdParty component for it.
  function FindOrCreateThirdParty(const aGroupKey, aGroupId, aBomRefPrefix, aOriginTag : string) : TSBOMComponent;
  var
    existing : TSBOMComponent;
    refStr : string;
  begin
    if thirdParty.TryGetValue(aGroupKey, existing) then
    begin
      result := existing;
      exit;
    end;
    result := report.AddComponent(TSBOMComponentKind.ThirdParty);
    refStr := aBomRefPrefix + LowerCase(aGroupId);
    result.BomRef := refStr;
    result.Id := aGroupId;
    result.Version := 'unknown';
    result.AddProperty('dpm:origin', aOriginTag);
    thirdParty.Add(aGroupKey, result);
    report.AddRelationship(report.RootComponent.BomRef, refStr);
  end;

  function FindComponentByBomRef(const aBomRef : string) : TSBOMComponent;
  var
    i : integer;
  begin
    result := nil;
    if aBomRef = '' then
      exit;
    for i := 0 to report.Components.Count - 1 do
    begin
      if report.Components[i].BomRef = aBomRef then
      begin
        result := report.Components[i];
        exit;
      end;
    end;
  end;

  //Classify a MAP entry that has no SourcePath. Returns true if we attached
  //evidence somewhere (DPM, runtime, third-party); false to fall back to
  //unidentified.
  function ClassifyNoSourcePathUnit(const unitName : string) : boolean;
  var
    ownerBomRef : string;
    ownerComp : TSBOMComponent;
    foundPath : string;
    origin : TUnitOrigin;
    folderName : string;
    folderKey : string;
    thirdPartyComp : TSBOMComponent;
  begin
    result := false;

    //1) Did one of the DPM packages we already enumerated ship this unit?
    if FUnitOwnership.TryGetValue(LowerCase(unitName), ownerBomRef) then
    begin
      ownerComp := FindComponentByBomRef(ownerBomRef);
      if ownerComp <> nil then
      begin
        ownerComp.AddEvidence('unit:' + unitName, 'map-no-source');
        result := true;
        exit;
      end;
    end;

    //2) Namespace / bare-name match against Delphi's reserved runtime unit set.
    //   We try to corroborate with a path check (file under Delphi RootDir) - that's
    //   the strongest signal - but a namespace match alone is itself a very strong
    //   signal that the unit is part of Delphi's shipped RTL/VCL/FMX. In practice
    //   third-parties almost never ship units in the System.* / Vcl.* / Fmx.*
    //   namespaces, and demanding a path corroboration here would dump the entire
    //   RTL into 'unidentified' whenever the IDE Library Path expansion fails
    //   (e.g. when $(BDSLIB) is unset). When we fall back to the prefix-only
    //   classification we attach an evidence note so downstream consumers can tell
    //   the difference.
    if (runtimeComp <> nil) and IsEmbarcaderoRuntimeUnit(unitName) then
    begin
      if (searchIndex <> nil) and searchIndex.TryFindUnit(unitName, foundPath, origin) then
      begin
        if (delphiRoot <> '') and PathStartsWith(foundPath, delphiRoot) then
        begin
          runtimeComp.AddEvidence(foundPath, 'map-no-source');
          result := true;
          exit;
        end;
      end;
      runtimeComp.AddEvidence('unit:' + unitName, 'map-no-source:namespace-prefix');
      result := true;
      exit;
    end;

    //3) General search-index lookup for non-Embarcadero-prefixed units.
    if (searchIndex <> nil) and searchIndex.TryFindUnit(unitName, foundPath, origin) then
    begin
      if (delphiRoot <> '') and PathStartsWith(foundPath, delphiRoot) and (runtimeComp <> nil) then
      begin
        runtimeComp.AddEvidence(foundPath, 'map-no-source');
        result := true;
        exit;
      end;
      if origin = TUnitOrigin.Project then
      begin
        //Project source - intentionally not surfaced as a component, but we did resolve it.
        result := true;
        exit;
      end;
      //Third-party: group by the parent folder name (last segment).
      folderName := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(foundPath)));
      if folderName = '' then
        folderName := 'third-party';
      folderKey := LowerCase(ExtractFilePath(foundPath));
      thirdPartyComp := FindOrCreateThirdParty(folderKey, folderName, 'thirdparty:', 'search-index');
      thirdPartyComp.AddEvidence(foundPath, 'map-no-source');
      result := true;
    end;
  end;

begin
  dprojDir := ExcludeTrailingPathDelimiter(ExtractFilePath(projectPath));
  delphiRoot := GetDelphiRootDir(compilerVersion);
  cacheRoot := FPackageCache.PackagesFolder;

  if options.MapFile <> '' then
    mapPath := options.MapFile
  else
  begin
    //Prefer the OutputDir recorded in the project's per-(config,platform) configuration.
    //Fall back to the default Delphi shape .\$(Platform)\$(Config) only if the editor has
    //no entry for this pair (rare - happens with mis-keyed configs).
    outputDir := '';
    if editor <> nil then
    begin
      projectConfig := editor.GetProjectConfiguration(configName, platform);
      if projectConfig <> nil then
        outputDir := projectConfig.OutputDir;
    end;
    if outputDir = '' then
      outputDir := '.\$(Platform)\$(Config)';
    outputDir := ExpandProjectTokens(outputDir, platform, configName);
    mapPath := IncludeTrailingPathDelimiter(dprojDir) + outputDir;
    mapPath := IncludeTrailingPathDelimiter(mapPath) + dprojName + '.map';
    mapPath := TPath.GetFullPath(mapPath);
  end;

  if not FileExists(mapPath) then
  begin
    if options.Strict then
      raise Exception.Create('MAP file not found: ' + mapPath + ' (re-run with the linker option "Map file = Detailed" enabled, or supply -map=<path>)');
    report.AddMetaProperty('dpm:evidence', 'map-file-not-found');
    report.AddMetaProperty('dpm:evidence.mapPath', mapPath);
    FLogger.Warning('[SBOM] MAP file not found at ' + mapPath
                    + ' - SBOM will contain DPM packages + runtime only.'
                    + ' Enable the linker option "Map file = Detailed" to include non-DPM evidence.');
    exit;
  end;

  FLogger.Verbose('[SBOM] reading MAP file: ' + mapPath);
  info := FMapFileReader.Read(mapPath);
  if info = nil then
  begin
    report.AddMetaProperty('dpm:evidence', 'map-file-unreadable');
    exit;
  end;

  //Runtime component is one if include-runtime; otherwise nil and we just skip evidence for it.
  runtimeComp := nil;
  for comp in report.Components do
    if comp.Kind = TSBOMComponentKind.DelphiRuntime then
    begin
      runtimeComp := comp;
      break;
    end;

  //Logged at Debug so it's available when diagnosing (-verbosity=detailed) but
  //doesn't add noise to normal runs once classification is working.
  FLogger.Debug(Format('[SBOM] classifier state: runtimeComp=%s, FUnitOwnership=%d units, delphiRoot=%s',
    [BoolToStr(runtimeComp <> nil, true), FUnitOwnership.Count, delphiRoot]));

  thirdParty := TCollections.CreateDictionary<string, TSBOMComponent>;
  unidentified := nil;

  for u in info.Units do
  begin
    evidenceLoc := u.SourcePath;
    if evidenceLoc <> '' then
    begin
      //classify by source-path origin.
      if (cacheRoot <> '') and PathStartsWith(evidenceLoc, cacheRoot) then
      begin
        //which DPM component does this path belong to? The cache layout is
        //{cache}/{compiler}/{id}/{version}/... so id is the third segment after root.
        dpkgPackageId := FirstPathSegmentUnder(IncludeTrailingPathDelimiter(cacheRoot) + CompilerToString(compilerVersion), evidenceLoc);
        comp := report.FindComponentById(dpkgPackageId);
        if comp <> nil then
          comp.AddEvidence(evidenceLoc, 'map')
        //if we couldn't match it, fall through to the unidentified bucket.
        else
        begin
          if unidentified = nil then
          begin
            unidentified := report.AddComponent(TSBOMComponentKind.Unidentified);
            unidentified.BomRef := 'unidentified:third-party';
            unidentified.Id := 'unidentified-third-party';
            unidentified.Version := 'unknown';
            report.AddRelationship(report.RootComponent.BomRef, unidentified.BomRef);
          end;
          unidentified.AddEvidence(evidenceLoc, 'map');
        end;
        continue;
      end;

      if (delphiRoot <> '') and PathStartsWith(evidenceLoc, delphiRoot) then
      begin
        if runtimeComp <> nil then
          runtimeComp.AddEvidence(evidenceLoc, 'map');
        continue;
      end;

      if PathStartsWith(evidenceLoc, dprojDir) then
      begin
        //own project source - intentionally not surfaced as a component.
        continue;
      end;

      //Third-party. Group by the first directory component above the source file. The id is the
      //last segment of the parent directory - a reasonable, deterministic stand-in for a library
      //name. Two files in the same dir collapse to one component.
      groupId := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(evidenceLoc)));
      groupKey := LowerCase(ExtractFilePath(evidenceLoc));
      if not thirdParty.TryGetValue(groupKey, comp) then
      begin
        comp := report.AddComponent(TSBOMComponentKind.ThirdParty);
        if groupId = '' then
          groupId := 'third-party';
        bomRef := 'thirdparty:' + LowerCase(groupId);
        comp.BomRef := bomRef;
        comp.Id := groupId;
        comp.Version := 'unknown';
        comp.AddProperty('dpm:origin', 'map-file');
        thirdParty.Add(groupKey, comp);
        report.AddRelationship(report.RootComponent.BomRef, bomRef);
      end;
      comp.AddEvidence(evidenceLoc, 'map');
    end
    else
    begin
      //The .dpr's program unit shows up in the MAP under the project name; that's
      //our own source and shouldn't surface as a component at all. In aggregation
      //mode dprojName is the *current* dproj's name (not the group name in
      //report.ProjectName) so each contained .dpr is filtered as its turn comes.
      if SameText(u.UnitName, dprojName) then
        continue;
      //No source path (release build w/o debug info, or .dcu-only library).
      //Tiered classifier: per-package unit map -> prefix+path -> search index ->
      //unidentified. See ClassifyNoSourcePathUnit for the full ladder.
      if not ClassifyNoSourcePathUnit(u.UnitName) then
      begin
        EnsureUnidentified;
        unidentified.AddEvidence('unit:' + u.UnitName, 'map-no-source');
      end;
    end;
  end;

  if not info.HasSourcePaths then
    report.AddMetaProperty('dpm:evidence.note', 'map-file-no-source-paths');
end;

function TSBOMGenerator.WriteReport(const report : TSBOMReport; const options : TSBOMOptions; const outDir, projectName : string) : boolean;
var
  baseName : string;
  emittedAny : boolean;

  procedure RunWriter(const writer : ISbomWriter);
  var
    target : string;
  begin
    if writer = nil then
      exit;
    target := IncludeTrailingPathDelimiter(outDir) + baseName + writer.FileExtension;
    try
      writer.Write(report, target);
      emittedAny := true;
      FLogger.Information('[SBOM] wrote ' + target);
    except
      on e : Exception do
        FLogger.Error('[SBOM] failed writing ' + target + ' : ' + e.Message);
    end;
  end;

begin
  baseName := projectName + '-' + DPMPlatformToString(report.Platform);
  emittedAny := false;

  //Iterate the format set. Each writer is resolved by name from the container
  //so adding a new format is purely a registration step in DPM.Core.Init.pas.
  if TSBOMFormat.CycloneDX in options.Formats then
    RunWriter(ResolveWriter(cSBOMWriterCycloneDX));
  if TSBOMFormat.SPDX in options.Formats then
    RunWriter(ResolveWriter(cSBOMWriterSPDX));
  if TSBOMFormat.HTML in options.Formats then
    RunWriter(ResolveWriter(cSBOMWriterHTML));
  if TSBOMFormat.Markdown in options.Formats then
    RunWriter(ResolveWriter(cSBOMWriterMarkdown));

  result := emittedAny;
end;

end.
