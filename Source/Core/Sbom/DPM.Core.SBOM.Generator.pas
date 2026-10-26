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
  DPM.Core.SBOM.ProductInfo,
  DPM.Core.SBOM.ComponentRules,
  DPM.Core.Project.MapFile,
  DPM.Core.Project.SearchPaths,
  DPM.Core.Crypto.Hashing.Interfaces;

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
    FHashingService : IHashingService;
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
    //Components found in the build rather than the package graph, keyed 'folder:' + LowerCase(source
    //folder) for a ThirdParty component grouping MAP units, 'bpl:' + LowerCase(file name) for a runtime
    //package. Cleared per-platform alongside FVisitedPackages, so in an aggregated .groupproj a
    //library linked by several dprojs is one component with an edge from each of them.
    FComponentsByKey : IDictionary<string, TSBOMComponent>;
    //LowerCase(bpl file name) -> bomRef of the DPM component whose bpl\<platform> folder ships it.
    //Populated by EnumeratePackages, consumed by ApplyBinaryImports. Cleared per-platform.
    FBplOwnership : IDictionary<string, string>;
    //LowerCase(name) of every component rule that claimed a unit, and whether a MAP file was read,
    //for the current platform pass - a rule that claims nothing when there was a MAP file to match
    //against is stale. Cleared per-platform.
    FMatchedRules : ISet<string>;
    FMapFileRead : boolean;
  protected
    function Generate(const cancellationToken : ICancellationToken; const options : TSBOMOptions) : boolean;
    function GenerateForProject(const cancellationToken : ICancellationToken; const projectPath : string; const options : TSBOMOptions;
                                const config : IConfiguration) : boolean;
    function GenerateForPlatform(const cancellationToken : ICancellationToken; const editor : IProjectEditor;
                                 const projectPath, projectName, outDir, configName : string;
                                 const compilerVersion : TCompilerVersion;
                                 const platform : TDPMPlatform;
                                 const graphRoot : IPackageReference;
                                 const explicitInfo : TSBOMProductInfo;
                                 const rules : ISBOMComponentRules;
                                 const options : TSBOMOptions) : boolean;
    function GenerateForGroupAggregated(const groupProjPath : string; const dprojPaths : array of string;
                                        const options : TSBOMOptions; const config : IConfiguration;
                                        const cancellationToken : ICancellationToken) : boolean;
    procedure EnumeratePackages(const cancellationToken : ICancellationToken; const report : TSBOMReport; const graphRoot : IPackageReference;
                                const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                const parentBomRef : string);
    procedure AddDelphiRuntime(const report : TSBOMReport; const compilerVersion : TCompilerVersion);
    //parentBomRef is the component that links the binary this MAP file describes - the report
    //root for a single dproj, the dproj's application component in an aggregated group.
    procedure ApplyMapFileEvidence(const report : TSBOMReport; const editor : IProjectEditor;
                                   const projectPath, dprojName, configName, parentBomRef : string;
                                   const platform : TDPMPlatform; const options : TSBOMOptions;
                                   const compilerVersion : TCompilerVersion;
                                   const searchIndex : IUnitSearchIndex;
                                   const rules : ISBOMComponentRules);
    function BuildSearchIndex(const editor : IProjectEditor; const dprojDir, configName : string;
                              const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : IUnitSearchIndex;
    function GetIDELibraryPath(const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : string;
    function GetVersionInfo(const projectPath, configName : string; const platform : TDPMPlatform) : TSBOMProductInfo;
    function LoadExplicitProductInfo(const projectPath : string; const options : TSBOMOptions; out info : TSBOMProductInfo;
                                     out rules : ISBOMComponentRules) : boolean;
    function FindOrCreateRuleComponent(const report : TSBOMReport; const parentBomRef : string;
                                       const rule : TSBOMComponentRule) : TSBOMComponent;
    procedure ApplyDeclaredComponents(const report : TSBOMReport; const parentBomRef : string; const rules : ISBOMComponentRules);
    procedure WarnUnmatchedRules(const rules : ISBOMComponentRules; const warned : ISet<string>);
    procedure ReportProductInfoWarnings(const report : TSBOMReport; const info : TSBOMProductInfo;
                                        const versionIsExplicit : boolean; const projectPath : string);
    function ApplyBuiltBinary(const report : TSBOMReport; const comp : TSBOMComponent; const editor : IProjectEditor;
                              const projectPath, configName : string; const platform : TDPMPlatform;
                              const compilerVersion : TCompilerVersion) : string;
    procedure ApplyBinaryImports(const report : TSBOMReport; const parentBomRef, binaryPath : string;
                                 const compilerVersion : TCompilerVersion);
    function WriteReport(const report : TSBOMReport; const options : TSBOMOptions; const outDir, projectName : string) : boolean;
    //virtual so tests can supply an install folder rather than read the registry
    function GetDelphiRootDir(const compilerVersion : TCompilerVersion) : string; virtual;
    function ResolveWriter(const name : string) : ISbomWriter;
  public
    constructor Create(const container : TContainer;
                       const logger : ILogger;
                       const configManager : IConfigurationManager;
                       const packageCache : IPackageCache;
                       const mapFileReader : IMapFileReader;
                       const hashingService : IHashingService);
  end;

//Where to look for a file the build writes next to the binary (the binary itself, its MAP file),
//in probe order. Exposed (and free of any filesystem access) so the path rules can be unit tested
//without a build tree.
function GetOutputFileCandidates(const dprojDir, outputDir, fileName, configName : string;
                                 const platform : TDPMPlatform) : TArray<string>;

//GetOutputFileCandidates for <projectName>.map
function GetMapFileCandidates(const dprojDir, outputDir, projectName, configName : string;
                              const platform : TDPMPlatform) : TArray<string>;

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
  DPM.Core.Version,
  DPM.Core.Package.Classes,
  DPM.Core.Project.Editor,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Compiler.ProjectSettings,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.PEImports,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Diagnostics;

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

//Probe order:
//  1) the project's configured output dir (DCC_ExeOutput, or DCC_BplOutput for packages).
//     It may be relative to the dproj, absolute (D:\Ajur2000) or UNC (\\build\drops) - so
//     it goes through ToAbsolutePath rather than being concatenated onto the dproj dir.
//  2) the dproj folder itself. dcc with no -E switch writes the binary and its map next to
//     the project, which is exactly what an absent or empty output dir element means.
//  3) the IDE's default .\$(Platform)\$(Config) shape - a last guess for projects whose
//     configuration we couldn't read (mis-keyed config, missing BuildConfiguration entry).
function GetOutputFileCandidates(const dprojDir, outputDir, fileName, configName : string;
                                 const platform : TDPMPlatform) : TArray<string>;

  procedure AddCandidate(const folder : string);
  var
    fullPath : string;
    i : integer;
  begin
    if Trim(folder) = '' then
      exit;
    fullPath := TPathUtils.ToAbsolutePath(ExpandProjectTokens(folder, platform, configName), dprojDir);
    if fullPath = '' then
      exit;
    fullPath := IncludeTrailingPathDelimiter(fullPath) + fileName;
    for i := 0 to Length(result) - 1 do
      if SameText(result[i], fullPath) then
        exit;
    SetLength(result, Length(result) + 1);
    result[Length(result) - 1] := fullPath;
  end;

begin
  result := nil;
  AddCandidate(outputDir);
  AddCandidate(dprojDir);
  AddCandidate('.\$(Platform)\$(Config)');
end;

function GetMapFileCandidates(const dprojDir, outputDir, projectName, configName : string;
                              const platform : TDPMPlatform) : TArray<string>;
begin
  result := GetOutputFileCandidates(dprojDir, outputDir, projectName + '.map', configName, platform);
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
  result := platform in [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.Win64x, TDPMPlatform.WinARM64EC];
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

//Value of one key in a VerInfo_Keys list ('CompanyName=Acme;FileVersion=1.0.0.0;...'). A group that
//appends to an inherited list can repeat a key - the later, more specific one wins.
function GetVerInfoKey(const keys, keyName : string) : string;
var
  parts : TArray<string>;
  i : integer;
  sepPos : integer;
begin
  result := '';
  parts := TStringUtils.SplitStr(keys, ';');
  for i := 0 to Length(parts) - 1 do
  begin
    sepPos := Pos('=', parts[i]);
    if (sepPos > 0) and SameText(Trim(Copy(parts[i], 1, sepPos - 1)), keyName) then
      result := Trim(Copy(parts[i], sepPos + 1, MaxInt));
  end;
end;

type
  TFileVersionResource = record
    Version : string;       // the fixed FileVersion, else the ProductVersion string
    CompanyName : string;
  end;

//What a binary's version resource says about it - empty fields when it has none.
function ReadVersionResource(const fileName : string) : TFileVersionResource;
var
  size : DWORD;
  handle : DWORD;
  buf : TBytes;
  fixed : PVSFixedFileInfo;
  fixedLen : UINT;
  translation : PDWORD;
  translationLen : UINT;
  langCodePage : string;

  function QueryString(const name : string) : string;
  var
    value : PChar;
    valueLen : UINT;
  begin
    result := '';
    value := nil;
    if VerQueryValue(@buf[0], PChar('\StringFileInfo\' + langCodePage + '\' + name), Pointer(value), valueLen) and
       (value <> nil) and (valueLen > 0) then
      result := Trim(string(value));
  end;

begin
  result.Version := '';
  result.CompanyName := '';
  size := GetFileVersionInfoSize(PChar(fileName), handle);
  if size = 0 then
    exit;
  SetLength(buf, size);
  if not GetFileVersionInfo(PChar(fileName), 0, size, @buf[0]) then
    exit;

  //the strings live in a block named after the first language / code page pair the file lists
  langCodePage := '040904B0';
  translation := nil;
  if VerQueryValue(@buf[0], '\VarFileInfo\Translation', Pointer(translation), translationLen) and
     (translation <> nil) and (translationLen >= SizeOf(DWORD)) then
    langCodePage := IntToHex(LoWord(translation^), 4) + IntToHex(HiWord(translation^), 4);

  fixed := nil;
  if VerQueryValue(@buf[0], '\', Pointer(fixed), fixedLen) and (fixed <> nil) and
     (fixedLen >= SizeOf(TVSFixedFileInfo)) and ((fixed.dwFileVersionMS <> 0) or (fixed.dwFileVersionLS <> 0)) then
    result.Version := Format('%d.%d.%d.%d', [(fixed.dwFileVersionMS shr 16) and $FFFF, fixed.dwFileVersionMS and $FFFF,
                                             (fixed.dwFileVersionLS shr 16) and $FFFF, fixed.dwFileVersionLS and $FFFF])
  else
    result.Version := QueryString('ProductVersion');
  result.CompanyName := QueryString('CompanyName');
end;

//Part of Windows rather than of the product : an API set name (api-ms-win-*, ext-ms-*), which the
//loader maps onto one of its own DLLs, or a DLL in a system folder whose version resource names
//Microsoft. The second check keeps a third-party DLL that was installed into System32 (it happens)
//in the SBOM. Sysnative is how a 32 bit process sees the real System32 on 64 bit Windows.
function IsWindowsSystemDll(const moduleName : string) : boolean;
const
  cSystemFolders : array[0..2] of string = ('System32', 'SysWOW64', 'Sysnative');
var
  systemRoot : string;
  candidate : string;
  i : integer;
begin
  result := StartsText('api-ms-', moduleName) or StartsText('ext-ms-', moduleName);
  if result then
    exit;
  systemRoot := GetEnvironmentVariable('SystemRoot');
  if systemRoot = '' then
    exit;
  for i := Low(cSystemFolders) to High(cSystemFolders) do
  begin
    candidate := IncludeTrailingPathDelimiter(systemRoot) + cSystemFolders[i] + PathDelim + moduleName;
    if FileExists(candidate) and (Pos('microsoft', LowerCase(ReadVersionResource(candidate).CompanyName)) > 0) then
    begin
      result := true;
      exit;
    end;
  end;
end;

{ TSBOMGenerator }

constructor TSBOMGenerator.Create(const container : TContainer;
                                   const logger : ILogger;
                                   const configManager : IConfigurationManager;
                                   const packageCache : IPackageCache;
                                   const mapFileReader : IMapFileReader;
                                   const hashingService : IHashingService);
begin
  inherited Create;
  FContainer := container;
  FLogger := logger;
  FConfigManager := configManager;
  FPackageCache := packageCache;
  FMapFileReader := mapFileReader;
  FHashingService := hashingService;
  FDelphiRootCache := TCollections.CreateDictionary<integer, string>;
  FUnitOwnership := TCollections.CreateDictionary<string, string>;
  FVisitedPackages := TCollections.CreateDictionary<string, string>;
  FComponentsByKey := TCollections.CreateDictionary<string, TSBOMComponent>;
  FBplOwnership := TCollections.CreateDictionary<string, string>;
  FMatchedRules := TCollections.CreateSet<string>;
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

//What the project stamps into its binary's version resource for one (config, platform) - the
//lowest layer of product metadata. Version is ProductVersion from the version info keys, else
//FileVersion; not the dproj's ProjectVersion element, which is the MSBuild project format version.
//Supplier is CompanyName. Empty when the project doesn't include version info, since the dproj's
//values then describe nothing that ships.
function TSBOMGenerator.GetVersionInfo(const projectPath, configName : string; const platform : TDPMPlatform) : TSBOMProductInfo;
var
  loader : IProjectSettingsLoader;
  keys : string;
begin
  result.Version := '';
  result.Supplier := '';
  result.SupplierUrl := '';
  result.License := '';
  result.SbomCreator := '';
  try
    loader := TDPMProjectSettingsLoader.Create(FLogger, projectPath, configName, platform);
    if not SameText(loader.GetStringProperty('VerInfo_IncludeVerInfo', ''), 'true') then
      exit;
    keys := loader.GetStringProperty('VerInfo_Keys', '');
  except
    on e : Exception do
    begin
      FLogger.Warning(TWarningCode.W8109_ProjectSettingsUnreadable, '[SBOM] could not read the project version info : ' + e.Message);
      exit;
    end;
  end;
  result.Version := GetVerInfoKey(keys, 'ProductVersion');
  if result.Version = '' then
    result.Version := GetVerInfoKey(keys, 'FileVersion');
  result.Supplier := GetVerInfoKey(keys, 'CompanyName');
end;

//The explicit layer of product metadata for projectPath - the sbom options over its sidecar - and
//the sidecar's component rules.
//False, having logged why, when the sidecar can't be read or the SBOM creator isn't a contact.
function TSBOMGenerator.LoadExplicitProductInfo(const projectPath : string; const options : TSBOMOptions;
                                                out info : TSBOMProductInfo; out rules : ISBOMComponentRules) : boolean;
var
  sidecar : TSBOMProductInfo;
  fromOptions : TSBOMProductInfo;
begin
  result := false;
  rules := TSBOMComponentRules.Create;
  if not LoadSBOMSidecar(FLogger, projectPath, sidecar, rules) then
    exit;
  fromOptions.Version := Trim(options.ProductVersion);
  fromOptions.Supplier := Trim(options.Supplier);
  fromOptions.SupplierUrl := Trim(options.SupplierUrl);
  fromOptions.License := Trim(options.License);
  fromOptions.SbomCreator := Trim(options.SbomCreator);
  info := fromOptions.Merge(sidecar);
  //BSI TR-03183-2 5.2.1 : the creator is given as an email address, or a URL when there is none.
  //A company name is not a way to contact anyone.
  if (info.SbomCreator <> '') and not (IsEmailAddress(info.SbomCreator) or IsHttpUrl(info.SbomCreator)) then
  begin
    FLogger.Error(TErrorCode.E8101_SBOMProductInfoInvalid, '[SBOM] the SBOM creator [' + info.SbomCreator + '] must be an email address or an http(s) URL');
    exit;
  end;
  result := true;
end;

//The component a rule describes, created the first time something claims it. In an aggregated group
//every dproj that links the library adds an edge to the one component.
function TSBOMGenerator.FindOrCreateRuleComponent(const report : TSBOMReport; const parentBomRef : string;
                                                  const rule : TSBOMComponentRule) : TSBOMComponent;
var
  key : string;
begin
  key := 'rule:' + LowerCase(rule.Name);
  if not FComponentsByKey.TryGetValue(key, result) then
  begin
    result := report.AddComponent(TSBOMComponentKind.ThirdParty);
    result.BomRef := report.MakeUniqueBomRef('component:' + LowerCase(rule.Name));
    result.Id := rule.Name;
    result.Version := rule.Version;
    result.Supplier := rule.Supplier;
    result.SupplierUrl := rule.SupplierUrl;
    result.License := rule.License;
    result.Purl := rule.Purl;
    result.ComponentType := rule.ComponentType;
    result.AddProperty('dpm:origin', 'component-rule');
    FComponentsByKey.Add(key, result);
  end;
  FMatchedRules.Add(LowerCase(rule.Name));
  report.AddRelationship(parentBomRef, result.BomRef);
end;

//Rules with no units to match declare a component outright - something the build can't show, such
//as a DLL the program loads by name at runtime.
procedure TSBOMGenerator.ApplyDeclaredComponents(const report : TSBOMReport; const parentBomRef : string;
                                                 const rules : ISBOMComponentRules);
var
  i : integer;
begin
  if rules = nil then
    exit;
  for i := 0 to rules.Count - 1 do
    if (Length(rules.Rules[i].Units) = 0) and (Length(rules.Rules[i].UnitPrefixes) = 0) then
      FindOrCreateRuleComponent(report, parentBomRef, rules.Rules[i]);
end;

//A rule with units to match that matched none, although a MAP file was read - the library was
//dropped, or the rule is wrong. warned collects the names already reported, so a group shares it.
procedure TSBOMGenerator.WarnUnmatchedRules(const rules : ISBOMComponentRules; const warned : ISet<string>);
var
  i : integer;
  rule : TSBOMComponentRule;
begin
  if (rules = nil) or not FMapFileRead then
    exit;
  for i := 0 to rules.Count - 1 do
  begin
    rule := rules.Rules[i];
    if (Length(rule.Units) = 0) and (Length(rule.UnitPrefixes) = 0) then
      continue;
    if FMatchedRules.Contains(LowerCase(rule.Name)) or not warned.Add(LowerCase(rule.Name)) then
      continue;
    FLogger.Warning(TWarningCode.W8105_SBOMComponentRuleUnmatched, '[SBOM] the component rule [' + rule.Name + '] matched no unit in the MAP file - ' +
                    'the library may no longer be used, or its units / unitPrefixes are wrong');
  end;
end;

//CRA / BSI gaps in the root's metadata - logged, and kept on the report for the HTML / Markdown
//reports. They never stop the SBOM being written.
procedure TSBOMGenerator.ReportProductInfoWarnings(const report : TSBOMReport; const info : TSBOMProductInfo;
                                                   const versionIsExplicit : boolean; const projectPath : string);
var
  warning : string;
begin
  for warning in GetProductInfoWarnings(info, versionIsExplicit, ExtractFileName(SBOMSidecarFileName(projectPath))) do
  begin
    FLogger.Warning(TWarningCode.W8106_SBOMProductInfoIncomplete, '[SBOM] ' + warning);
    report.Warnings.Add(warning);
  end;
end;

//The file the project builds for (config, platform) - <name>.exe, <name>.dll, or <name><suffix>.bpl
//with the DllSuffix resolved - looked for where the build writes it. On a hit comp gets its SHA-256
//(primary) and SHA-512; BSI TR-03183-2 5.2.2 wants the SHA-512 of the deployable component. On a miss
//the report gets a warning - the SBOM is still useful, it just can't be tied to a binary.
//Returns the binary's path, '' when there is none.
function TSBOMGenerator.ApplyBuiltBinary(const report : TSBOMReport; const comp : TSBOMComponent; const editor : IProjectEditor;
                                         const projectPath, configName : string; const platform : TDPMPlatform;
                                         const compilerVersion : TCompilerVersion) : string;
var
  projectName : string;
  loader : IProjectSettingsLoader;
  suffix : string;
  fileName : string;
  outputDir : string;
  projectConfig : IProjectConfiguration;
  candidates : TArray<string>;
  i : integer;
  binaryPath : string;
  warning : string;
begin
  result := '';
  //non-Windows targets build bundles and shared objects under other names - not handled yet
  if (editor = nil) or not IsWindowsPlatform(platform) then
    exit;

  projectName := ChangeFileExt(ExtractFileName(projectPath), '');
  case editor.AppType of
    TAppType.Application : fileName := projectName + '.exe';
    TAppType.Lib : fileName := projectName + '.dll';
    TAppType.Package :
    begin
      suffix := '';
      try
        loader := TDPMProjectSettingsLoader.Create(FLogger, projectPath, configName, platform);
        suffix := loader.GetStringProperty('DllSuffix', '');
      except
        on e : Exception do
          FLogger.Debug('[SBOM] could not read DllSuffix : ' + e.Message);
      end;
      //$(Auto) is the compiler's package version - 290 for 12.x, 370 for 13.x
      if SameText(suffix, '$(Auto)') then
      begin
        try
          suffix := CompilerToLibSuffix(compilerVersion);
        except
          suffix := '';
        end;
      end;
      fileName := projectName + suffix + '.bpl';
    end;
  else
    exit;
  end;

  outputDir := '';
  projectConfig := editor.GetProjectConfiguration(configName, platform);
  if projectConfig <> nil then
    outputDir := projectConfig.OutputDir;
  candidates := GetOutputFileCandidates(ExcludeTrailingPathDelimiter(ExtractFilePath(projectPath)), outputDir,
                                        fileName, configName, platform);
  binaryPath := '';
  for i := 0 to Length(candidates) - 1 do
  begin
    if FileExists(candidates[i]) then
    begin
      binaryPath := candidates[i];
      break;
    end;
  end;

  if binaryPath = '' then
  begin
    warning := 'CRA/BSI: the built file ' + fileName + ' was not found, so the SBOM has no hash for it ' +
               '(BSI TR-03183-2 5.2.2 asks for the SHA-512 of the deployable component). Build the ' +
               configName + ' ' + DPMPlatformToString(platform) + ' configuration first, or check --config and --platforms.';
    FLogger.Warning(TWarningCode.W8107_BuiltBinaryNotFound, '[SBOM] ' + warning);
    report.Warnings.Add(warning);
    exit;
  end;

  try
    comp.HashAlgorithm := 'SHA-256';
    comp.HashValue := BytesToHex(FHashingService.HashFile(binaryPath, haSha256));
    comp.AddHash('SHA-512', BytesToHex(FHashingService.HashFile(binaryPath, haSha512)));
    FLogger.Verbose('[SBOM] hashed ' + binaryPath);
    result := binaryPath;
  except
    on e : Exception do
    begin
      comp.HashAlgorithm := '';
      comp.HashValue := '';
      comp.Hashes.Clear;
      warning := 'CRA/BSI: could not hash the built file ' + binaryPath + ' : ' + e.Message;
      FLogger.Warning(TWarningCode.W8108_BinaryUnreadable, '[SBOM] ' + warning);
      report.Warnings.Add(warning);
    end;
  end;
end;

//The runtime packages and DLLs binaryPath loads, from its PE import table - only what the linker
//actually referenced, unlike the DCC_UsePackage list, which names every package the IDE knows.
// - a bpl one of the DPM packages built is evidence on that package
// - a bpl or DLL from Delphi itself (found in the install's bin / bin64) is evidence on the runtime
//   component, or dropped with --no-runtime
// - an API set name, or a DLL Windows ships (in a system folder, version resource names Microsoft),
//   is part of the operating system, not of the product
// - anything else is a component of its own, linked from parentBomRef. When the file is next to the
//   binary - the copy that ships with it - it is hashed, and its version resource gives the version
//   and the supplier.
procedure TSBOMGenerator.ApplyBinaryImports(const report : TSBOMReport; const parentBomRef, binaryPath : string;
                                            const compilerVersion : TCompilerVersion);
var
  imports : TArray<TPEImport>;
  errorMessage : string;
  i : integer;
  moduleName : string;
  key : string;
  isPackage : boolean;
  ownerBomRef : string;
  owner : TSBOMComponent;
  runtimeComp : TSBOMComponent;
  comp : TSBOMComponent;
  delphiRoot : string;
  binaryDir : string;
  shippedCopy : string;
  versionResource : TFileVersionResource;

  function IsDelphiFile(const fileName : string) : boolean;
  begin
    result := (delphiRoot <> '') and
              (FileExists(IncludeTrailingPathDelimiter(delphiRoot) + 'bin' + PathDelim + fileName) or
               FileExists(IncludeTrailingPathDelimiter(delphiRoot) + 'bin64' + PathDelim + fileName));
  end;

begin
  if binaryPath = '' then
    exit;
  if not TryReadPEImports(binaryPath, imports, errorMessage) then
  begin
    FLogger.Warning(TWarningCode.W8108_BinaryUnreadable, '[SBOM] could not read the imports of ' + binaryPath + ' : ' + errorMessage);
    exit;
  end;

  runtimeComp := nil;
  for comp in report.Components do
    if comp.Kind = TSBOMComponentKind.DelphiRuntime then
    begin
      runtimeComp := comp;
      break;
    end;
  delphiRoot := GetDelphiRootDir(compilerVersion);
  binaryDir := ExtractFilePath(binaryPath);

  for i := 0 to Length(imports) - 1 do
  begin
    moduleName := imports[i].ModuleName;
    key := LowerCase(moduleName);
    isPackage := SameText(ExtractFileExt(moduleName), '.bpl');

    if isPackage and FBplOwnership.TryGetValue(key, ownerBomRef) then
    begin
      owner := report.FindComponentByBomRef(ownerBomRef);
      if owner <> nil then
      begin
        owner.AddEvidence('runtime-package:' + moduleName, 'pe-import');
        continue;
      end;
    end;

    if IsDelphiFile(moduleName) then
    begin
      if runtimeComp <> nil then
      begin
        if isPackage then
          runtimeComp.AddEvidence('runtime-package:' + moduleName, 'pe-import')
        else
          runtimeComp.AddEvidence('dll:' + moduleName, 'pe-import');
      end;
      continue;
    end;

    if (not isPackage) and IsWindowsSystemDll(moduleName) then
      continue;

    if isPackage then
      key := 'bpl:' + key
    else
      key := 'dll:' + key;
    if not FComponentsByKey.TryGetValue(key, comp) then
    begin
      comp := report.AddComponent(TSBOMComponentKind.ThirdParty);
      comp.Id := moduleName;
      if isPackage then
      begin
        comp.BomRef := report.MakeUniqueBomRef('runtime-package:' + LowerCase(moduleName));
        comp.AddProperty('dpm:origin', 'runtime-package');
      end
      else
      begin
        comp.BomRef := report.MakeUniqueBomRef('dll:' + LowerCase(moduleName));
        comp.AddProperty('dpm:origin', 'dll-import');
        //bound on first call, not at startup - it may never be loaded at all
        if imports[i].Delayed then
          comp.AddProperty('dpm:delayLoad', 'true');
      end;

      shippedCopy := binaryDir + moduleName;
      if FileExists(shippedCopy) then
      begin
        try
          comp.HashAlgorithm := 'SHA-256';
          comp.HashValue := BytesToHex(FHashingService.HashFile(shippedCopy, haSha256));
          comp.AddHash('SHA-512', BytesToHex(FHashingService.HashFile(shippedCopy, haSha512)));
        except
          on e : Exception do
          begin
            comp.HashAlgorithm := '';
            comp.HashValue := '';
            comp.Hashes.Clear;
            FLogger.Warning(TWarningCode.W8108_BinaryUnreadable, '[SBOM] could not hash ' + shippedCopy + ' : ' + e.Message);
          end;
        end;
        versionResource := ReadVersionResource(shippedCopy);
        comp.Version := versionResource.Version;
        comp.Supplier := versionResource.CompanyName;
      end;
      FComponentsByKey.Add(key, comp);
    end;
    report.AddRelationship(parentBomRef, comp.BomRef);
  end;
end;

//Puts layered product metadata on a root or application component.
procedure ApplyProductInfo(const comp : TSBOMComponent; const info : TSBOMProductInfo);
begin
  comp.Version := info.Version;
  comp.Supplier := info.Supplier;
  comp.SupplierUrl := info.SupplierUrl;
  comp.License := info.License;
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
        FLogger.Warning(TWarningCode.W8109_ProjectSettingsUnreadable, '[SBOM] could not read the project search path : ' + e.Message);
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
      FLogger.Error(TErrorCode.E0003_InternalError, '[SBOM] could not resolve the writer [' + name + '] : ' + e.Message);
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
  resolvedChild : string;
  ok : boolean;
begin
  result := false;
  if options = nil then
    exit;

  config := FConfigManager.LoadConfig(options.ConfigFile);
  if config = nil then
    exit;

  if FPackageCache.Location = '' then
    FPackageCache.Location := config.PackageCacheLocation;

  //Normalise up front: everything downstream (ExtractFileName for the output base name,
  //ExtractFilePath for the default output dir, ResolveAgainstDproj for group members)
  //assumes an absolute path using PathDelim. A command line 'sbom ../Source/X.dproj' or
  //'sbom i:/src/X.dproj' otherwise leaks its separators into the output file name.
  projPath := TPathUtils.ToAbsolutePath(options.ProjectPath);
  if not FileExists(projPath) then
  begin
    FLogger.Error(TErrorCode.E4001_ProjectNotFound, 'The project file [' + projPath + '] does not exist.');
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
        ok := GenerateForProject(cancellationToken, resolvedChild, options, config) and ok;
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
    result := GenerateForProject(cancellationToken, projPath, options, config);
  end
  else
  begin
    FLogger.Error(TErrorCode.E0102_InvalidArgument, 'The project type [' + ext + '] is not supported - expected a .dproj or a .groupproj.');
    exit;
  end;
end;

function TSBOMGenerator.GenerateForProject(const cancellationToken : ICancellationToken; const projectPath : string; const options : TSBOMOptions;
                                          const config : IConfiguration) : boolean;
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
  graphRoot : IPackageReference;
  explicitInfo : TSBOMProductInfo;
  rules : ISBOMComponentRules;
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
      FLogger.Warning(TWarningCode.W8110_BuildConfigNotFound, 'The configuration [' + options.Config + '] is not in the project - using the first available.');
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

  if not LoadExplicitProductInfo(projectPath, options, explicitInfo, rules) then
    exit;

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
      FLogger.Error(TErrorCode.E4007_ProjectCompilerUnknown, 'Unable to determine the compiler version of the project [' + projectPath + '].');
      exit;
    end;
  end;

  enabledPlatforms := editor.Platforms;
  if enabledPlatforms = [] then
  begin
    FLogger.Error(TErrorCode.E4014_ProjectHasNoPlatforms, 'The project [' + projectPath + '] has no enabled platforms.');
    exit;
  end;
  if options.Platforms = [] then
    selectedPlatforms := enabledPlatforms
  else
    selectedPlatforms := options.Platforms * enabledPlatforms;

  if selectedPlatforms = [] then
  begin
    FLogger.Error(TErrorCode.E4015_RequestedPlatformsNotTargeted, 'None of the requested platforms [' + DPMPlatformsToString(options.Platforms) + '] is enabled in the project [' + projectPath + '].');
    exit;
  end;

  configNames := editor.GetConfigNames;
  configName := PickConfig;
  if configName = '' then
    configName := 'Release';

  //--outdir is user supplied, so it gets the same treatment as the project path. A relative
  //outdir resolves against the current directory, not the project directory.
  outDir := TPathUtils.ToAbsolutePath(options.OutputDir);
  if outDir = '' then
    outDir := ExtractFilePath(projectPath);
  outDir := ExcludeTrailingPathDelimiter(outDir);
  if not ForceDirectories(outDir) then
  begin
    FLogger.Error(TErrorCode.E0005_FolderCreateFailed, 'Unable to create the output folder [' + outDir + '].');
    exit;
  end;

  projectName := ChangeFileExt(ExtractFileName(projectPath), '');
  graphRoot := editor.GetPackageReferences;

  ok := true;
  for platform in selectedPlatforms do
  begin
    if not GenerateForPlatform(cancellationToken, editor, projectPath, projectName, outDir, configName,
                               compilerVersion, platform, graphRoot, explicitInfo, rules, options) then
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
    Sidecar : TSBOMProductInfo;  //the dproj's own <name>.sbom.yaml
    Rules : ISBOMComponentRules; //its sidecar's component rules, then the group's
  end;
var
  groupInfo : TSBOMProductInfo;
  groupRules : ISBOMComponentRules;
  warnedRules : ISet<string>;
  groupFallback : TSBOMProductInfo;
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
  binaryPath : string;
  runtimeCompiler : TCompilerVersion;
  ok : boolean;
  editor : IProjectEditor;
  loadFailed : boolean;
  platformFailed : boolean;

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
    FLogger.Error(TErrorCode.E4002_NoProjectsFound, 'The project group [' + groupProjPath + '] contains no projects.');
    exit;
  end;

  FLogger.Information('Generating aggregated SBOM for group : ' + groupProjPath);

  //The options and the group's sidecar describe the group root and the SBOM as a whole.
  if not LoadExplicitProductInfo(groupProjPath, options, groupInfo, groupRules) then
    exit;
  //...and are the fallback for each dproj's supplier and licence, but not its version - a dproj's
  //own version info says more about it than a version given for the group.
  groupFallback := groupInfo;
  groupFallback.Version := '';

  loadFailed := false;
  //First pass: load every dproj and capture its compiler / platforms / config.
  //Doing this once up front lets us iterate platforms in the outer loop and
  //compute the union of platforms across the whole group.
  SetLength(contexts, Length(dprojPaths));
  unionPlatforms := [];
  for i := 0 to Length(dprojPaths) - 1 do
  begin
    contexts[i].Path := dprojPaths[i];
    contexts[i].Name := ChangeFileExt(ExtractFileName(dprojPaths[i]), '');
    contexts[i].Editor := nil;
    if not FileExists(dprojPaths[i]) then
    begin
      FLogger.Warning(TWarningCode.W4011_ProjectNotFoundInList, '[SBOM] skipping [' + dprojPaths[i] + '] - the group lists it but it does not exist.');
      continue;
    end;
    editor := TProjectEditor.Create(FLogger, config, TCompilerVersion.UnknownVersion);
    if not editor.LoadProject(dprojPaths[i], [TProjectElement.All]) then
    begin
      FLogger.Information('[SBOM] skipping [' + dprojPaths[i] + '] in the group SBOM.', true);
      loadFailed := true;
      continue;
    end;
    contexts[i].Editor := editor;
    contexts[i].CompilerVersion := contexts[i].Editor.CompilerVersion;
    if contexts[i].CompilerVersion = TCompilerVersion.UnknownVersion then
      contexts[i].CompilerVersion := ProjectVersionToCompilerVersion(contexts[i].Editor.ProjectVersion);
    contexts[i].EnabledPlatforms := contexts[i].Editor.Platforms;
    contexts[i].ConfigNames := contexts[i].Editor.GetConfigNames;
    contexts[i].ConfigName := PickConfig(contexts[i].ConfigNames);
    if contexts[i].ConfigName = '' then
      contexts[i].ConfigName := 'Release';
    contexts[i].GraphRoot := contexts[i].Editor.GetPackageReferences;
    contexts[i].Rules := TSBOMComponentRules.Create;
    if not LoadSBOMSidecar(FLogger, dprojPaths[i], contexts[i].Sidecar, contexts[i].Rules) then
      exit;
    //a dproj's own rules first, so it can name a library differently from the rest of the group
    contexts[i].Rules.AddAll(groupRules);
    unionPlatforms := unionPlatforms + contexts[i].EnabledPlatforms;
  end;

  if unionPlatforms = [] then
  begin
    //the loads that failed said why.
    if not loadFailed then
      FLogger.Error(TErrorCode.E4014_ProjectHasNoPlatforms, 'None of the projects in [' + groupProjPath + '] has an enabled platform.');
    exit;
  end;

  if options.Platforms = [] then
    selectedPlatforms := unionPlatforms
  else
    selectedPlatforms := options.Platforms * unionPlatforms;
  if selectedPlatforms = [] then
  begin
    FLogger.Error(TErrorCode.E4015_RequestedPlatformsNotTargeted, 'None of the requested platforms [' + DPMPlatformsToString(options.Platforms) + '] is enabled in any project in [' + groupProjPath + '].');
    exit;
  end;

  groupName := ChangeFileExt(ExtractFileName(groupProjPath), '');
  outDir := TPathUtils.ToAbsolutePath(options.OutputDir);
  if outDir = '' then
    outDir := ExtractFilePath(groupProjPath);
  outDir := ExcludeTrailingPathDelimiter(outDir);
  if not ForceDirectories(outDir) then
  begin
    FLogger.Error(TErrorCode.E0005_FolderCreateFailed, 'Unable to create the output folder [' + outDir + '].');
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
      //see the note in GenerateForPlatform - the host binary's product version, not the
      //minimum-client-version constant.
      report.ToolVersion := TDPMVersion.CurrentVersionString;
      report.ProjectName := groupName;
      report.ProjectVersion := groupInfo.Version;
      report.SbomCreator := groupInfo.SbomCreator;
      report.Platform := platform;

      report.RootComponent.BomRef := 'group:' + groupName + ':' + DPMPlatformToString(platform);
      report.RootComponent.Id := groupName;
      //the group is exactly the projects the .groupproj lists for this platform, and their runtime
      report.RootComponent.DependencyCompleteness := TSBOMCompleteness.Complete;
      //a group project has no version info of its own - unless one was given, Version stays
      //empty and is omitted
      ApplyProductInfo(report.RootComponent, groupInfo);
      ReportProductInfoWarnings(report, groupInfo, true, groupProjPath);
      report.AddMetaProperty('dpm:groupProject', ExtractFileName(groupProjPath));
      report.AddMetaProperty('dpm:projectCount', IntToStr(Length(contexts)));

      //Per-platform classifier state (see GenerateForPlatform note).
      FUnitOwnership.Clear;
      FVisitedPackages.Clear;
      FComponentsByKey.Clear;
      FBplOwnership.Clear;
      FMatchedRules.Clear;
      FMapFileRead := false;
      runtimeCompiler := TCompilerVersion.UnknownVersion;
      platformFailed := false;

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
        //as for a single dproj's root - runtime loads by name can't be seen
        dprojApp.DependencyCompleteness := TSBOMCompleteness.Incomplete;
        ApplyProductInfo(dprojApp, ctx.Sidecar.Merge(groupFallback).Merge(GetVersionInfo(ctx.Path, ctx.ConfigName, platform)));
        binaryPath := ApplyBuiltBinary(report, dprojApp, ctx.Editor, ctx.Path, ctx.ConfigName, platform, ctx.CompilerVersion);
        report.AddRelationship(report.RootComponent.BomRef, ctx.AppBomRef);
        contexts[i].AppBomRef := ctx.AppBomRef;

        //an SBOM without the packages would be wrong - nothing is written for the platform.
        try
          EnumeratePackages(cancellationToken, report, ctx.GraphRoot, ctx.CompilerVersion, platform, ctx.AppBomRef);
        except
          on e : Exception do
          begin
            FLogger.Error(ErrorCodeOf(e, TErrorCode.E8112_PackageEnumerationFailed), '[SBOM] unable to list the packages of ' + ctx.Name +
              ' for ' + DPMPlatformToString(platform) + ' : ' + e.Message);
            platformFailed := true;
          end;
        end;

        //First dproj wins the compiler-version vote for the runtime component.
        //Cross-compiler groups are unusual; if it happens we still emit one
        //runtime per distinct compiler version (AddDelphiRuntime is idempotent
        //per bom-ref).
        if runtimeCompiler = TCompilerVersion.UnknownVersion then
          runtimeCompiler := ctx.CompilerVersion;
        if options.IncludeRuntime then
          AddDelphiRuntime(report, ctx.CompilerVersion);

        //after the packages and the runtime component - those are what an import is matched against
        ApplyBinaryImports(report, ctx.AppBomRef, binaryPath, ctx.CompilerVersion);
        ApplyDeclaredComponents(report, ctx.AppBomRef, ctx.Rules);

        if IsWindowsPlatform(platform) then
        begin
          try
            ApplyMapFileEvidence(report, ctx.Editor, ctx.Path, ctx.Name, ctx.ConfigName, ctx.AppBomRef,
                                 platform, options, ctx.CompilerVersion,
                                 BuildSearchIndex(ctx.Editor, ExcludeTrailingPathDelimiter(ExtractFilePath(ctx.Path)),
                                                  ctx.ConfigName, platform, ctx.CompilerVersion), ctx.Rules);
          except
            on e : Exception do
            begin
              if options.Strict then
              begin
                FLogger.Error(ErrorCodeOf(e, TErrorCode.E8113_MapFileRequired), '[SBOM] ' + ctx.Name + ' : ' + e.Message);
                platformFailed := true;
              end
              else
                FLogger.Warning(TWarningCode.W8116_MapFileUnreadable, '[SBOM] the MAP file of ' + ctx.Name + ' could not be read : ' + e.Message +
                  ' - its part of the SBOM lists the DPM packages and the runtime only.');
            end;
          end;
        end
        else
          report.AddMetaProperty('dpm:evidence', 'platform-not-supported:' + DPMPlatformToString(platform));
      end;

      //once the whole group has been matched - a group rule only one dproj uses is not stale
      warnedRules := TCollections.CreateSet<string>;
      for i := 0 to Length(contexts) - 1 do
        if (contexts[i].Editor <> nil) and (platform in contexts[i].EnabledPlatforms) then
          WarnUnmatchedRules(contexts[i].Rules, warnedRules);

      if platformFailed then
        ok := false
      else if not WriteReport(report, options, outDir, groupName) then
        ok := false;
    finally
      report.Free;
    end;
  end;
  result := ok and not loadFailed;
end;

function TSBOMGenerator.GenerateForPlatform(const cancellationToken : ICancellationToken; const editor : IProjectEditor;
                                            const projectPath, projectName, outDir, configName : string;
                                            const compilerVersion : TCompilerVersion;
                                            const platform : TDPMPlatform;
                                            const graphRoot : IPackageReference;
                                            const explicitInfo : TSBOMProductInfo;
                                            const rules : ISBOMComponentRules;
                                            const options : TSBOMOptions) : boolean;
var
  report : TSBOMReport;
  productInfo : TSBOMProductInfo;
  binaryPath : string;
begin
  //version info is per (config, platform) - Win32 and Win64 builds can carry different versions
  productInfo := explicitInfo.Merge(GetVersionInfo(projectPath, configName, platform));
  report := TSBOMReport.Create;
  try
    report.SerialNumber := 'urn:uuid:' + NewGuidString;
    report.TimestampUtc := NowUtcIso8601;
    report.ToolName := 'dpm';
    //The product version of the host binary, not cDPMClientVersion - that constant is the
    //minimum-client-version floor written into dspec files, and reporting it here would
    //name the wrong tool build in every SBOM we emit.
    report.ToolVersion := TDPMVersion.CurrentVersionString;
    report.ProjectName := projectName;
    report.ProjectVersion := productInfo.Version;
    report.SbomCreator := productInfo.SbomCreator;
    report.Platform := platform;
    report.CompilerVersion := compilerVersion;

    report.RootComponent.BomRef := 'project:' + projectName + ':' + DPMPlatformToString(platform);
    report.RootComponent.Id := projectName;
    //what the program loads by name at runtime (LoadLibrary, LoadPackage) is in neither the MAP
    //file nor its import table
    report.RootComponent.DependencyCompleteness := TSBOMCompleteness.Incomplete;
    //anything still empty is unknown - the writers omit it rather than invent a value
    ApplyProductInfo(report.RootComponent, productInfo);
    ReportProductInfoWarnings(report, productInfo, explicitInfo.Version <> '', projectPath);
    //for a single dproj the root is the binary it builds
    binaryPath := ApplyBuiltBinary(report, report.RootComponent, editor, projectPath, configName, platform, compilerVersion);

    //Fresh per-platform: the unit-ownership map, the package-visited map, and the
    //search index are all keyed on the unit / package name and would be wrong if
    //reused across platforms (different lib folder per platform, different IDE
    //Library Path). In aggregation mode they're cleared per-platform too but kept
    //across all dprojs within that platform.
    FUnitOwnership.Clear;
    FVisitedPackages.Clear;
    FComponentsByKey.Clear;
    FBplOwnership.Clear;
    FMatchedRules.Clear;
    FMapFileRead := false;

    //an SBOM without the packages would be wrong - nothing is written for the platform.
    try
      EnumeratePackages(cancellationToken, report, graphRoot, compilerVersion, platform, report.RootComponent.BomRef);
    except
      on e : Exception do
      begin
        FLogger.Error(ErrorCodeOf(e, TErrorCode.E8112_PackageEnumerationFailed), '[SBOM] unable to list the packages of ' + projectName +
          ' for ' + DPMPlatformToString(platform) + ' : ' + e.Message);
        exit(false);
      end;
    end;

    if options.IncludeRuntime then
      AddDelphiRuntime(report, compilerVersion);

    //after the packages and the runtime component - those are what an import is matched against
    ApplyBinaryImports(report, report.RootComponent.BomRef, binaryPath, compilerVersion);
    ApplyDeclaredComponents(report, report.RootComponent.BomRef, rules);

    if IsWindowsPlatform(platform) then
    begin
      try
        ApplyMapFileEvidence(report, editor, projectPath, projectName, configName,
                             report.RootComponent.BomRef, platform, options, compilerVersion,
                             BuildSearchIndex(editor, ExcludeTrailingPathDelimiter(ExtractFilePath(projectPath)),
                                              configName, platform, compilerVersion), rules);
      except
        on e : Exception do
        begin
          if options.Strict then
          begin
            FLogger.Error(ErrorCodeOf(e, TErrorCode.E8113_MapFileRequired), '[SBOM] ' + e.Message);
            exit(false);
          end;
          FLogger.Warning(TWarningCode.W8116_MapFileUnreadable, '[SBOM] the MAP file could not be read : ' + e.Message +
            ' - the SBOM lists the DPM packages and the runtime only.');
        end;
      end;
    end
    else
    begin
      report.AddMetaProperty('dpm:evidence', 'platform-not-supported:' + DPMPlatformToString(platform));
      FLogger.Information('[SBOM] MAP-file parsing not yet supported on platform [' + DPMPlatformToString(platform) + '] - emitting package + runtime SBOM only.');
    end;
    WarnUnmatchedRules(rules, TCollections.CreateSet<string>);

    result := WriteReport(report, options, outDir, projectName);
  finally
    report.Free;
  end;
end;

procedure TSBOMGenerator.EnumeratePackages(const cancellationToken : ICancellationToken; const report : TSBOMReport;
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

  procedure HarvestBpls(const folder, ownerBomRef : string);
  var
    files : TStringDynArray;
    i : integer;
    bplKey : string;
  begin
    if not DirectoryExists(folder) then
      exit;
    try
      files := TDirectory.GetFiles(folder, '*.bpl', TSearchOption.soTopDirectoryOnly);
    except
      SetLength(files, 0);
    end;
    for i := 0 to Length(files) - 1 do
    begin
      bplKey := LowerCase(ExtractFileName(files[i]));
      if not FBplOwnership.ContainsKey(bplKey) then
        FBplOwnership.Add(bplKey, ownerBomRef);
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
    //Runtime packages the installer compiled into bpl\<BDSPlatform>, so a project linking with
    //runtime packages can be tied back to this package by the bpl names its binary imports.
    HarvestBpls(packageFolder + 'bpl' + PathDelim + DPMPlatformToBDString(platform), ownerBomRef);
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
      spec := FPackageCache.GetPackageSpec(cancellationToken, identity);
    except
      on e : Exception do
        FLogger.Warning(TWarningCode.W3013_PackageSpecUnreadable, '[SBOM] could not load the spec for ' + node.Id + ' ' + node.Version.ToStringNoMeta + ' : ' + e.Message);
    end;

    comp := report.AddComponent(TSBOMComponentKind.DpmPackage);
    bomRef := 'dpm:' + node.Id + '@' + node.Version.ToStringNoMeta;
    comp.BomRef := bomRef;
    comp.Id := node.Id;
    comp.Version := node.Version.ToStringNoMeta;
    //its dependencies are declared in its dspec and resolved into the dproj - the children below
    comp.DependencyCompleteness := TSBOMCompleteness.Complete;
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
                                              const projectPath, dprojName, configName, parentBomRef : string;
                                              const platform : TDPMPlatform; const options : TSBOMOptions;
                                              const compilerVersion : TCompilerVersion;
                                              const searchIndex : IUnitSearchIndex;
                                              const rules : ISBOMComponentRules);
const
  cUnidentifiedBomRef = 'unidentified:third-party';
var
  dprojDir : string;
  delphiRoot : string;
  cacheRoot : string;
  mapPath : string;
  candidates : TArray<string>;
  triedPaths : string;
  i : integer;
  outputDir : string;
  projectConfig : IProjectConfiguration;
  info : IMapFileInfo;
  u : TMapUnit;
  comp : TSBOMComponent;
  runtimeComp : TSBOMComponent;
  unidentified : TSBOMComponent;
  evidenceLoc : string;
  dpkgPackageId : string;
  ruleIndex : integer;

  //Lazy: created on first hit so SBOMs with full evidence don't contain an empty
  //'unidentified-third-party' component. In an aggregated group an earlier dproj may
  //already have created it - reuse that one and just link it from this dproj too.
  procedure EnsureUnidentified;
  begin
    if unidentified = nil then
    begin
      unidentified := report.AddComponent(TSBOMComponentKind.Unidentified);
      unidentified.BomRef := cUnidentifiedBomRef;
      unidentified.Id := 'unidentified-third-party';
    end;
    report.AddRelationship(parentBomRef, unidentified.BomRef);
  end;

  //The ThirdParty component grouping the units in sourceFile's folder, created on first use.
  //The folder name is a deterministic stand-in for the library name. Unrelated libraries often
  //share one ('Source', 'src') so the bom-ref is made unique - the name is left as it is.
  function FindOrCreateThirdParty(const sourceFile, originTag : string) : TSBOMComponent;
  var
    folderKey : string;
    groupId : string;
  begin
    folderKey := 'folder:' + LowerCase(ExtractFilePath(sourceFile));
    if not FComponentsByKey.TryGetValue(folderKey, result) then
    begin
      groupId := ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(sourceFile)));
      if groupId = '' then
        groupId := 'third-party';
      result := report.AddComponent(TSBOMComponentKind.ThirdParty);
      result.BomRef := report.MakeUniqueBomRef('thirdparty:' + LowerCase(groupId));
      result.Id := groupId;
      //no Version - nothing in a MAP file tells us which release of the library this is, and a
      //made up value ('unknown') can be mistaken for a real one by vulnerability matching
      result.AddProperty('dpm:origin', originTag);
      FComponentsByKey.Add(folderKey, result);
    end;
    report.AddRelationship(parentBomRef, result.BomRef);
  end;

  //A rule written for a library that is now installed with DPM : the package claims the unit, but
  //the rule is not stale - the library is linked.
  procedure NoteRuleForDpmUnit(const unitName : string);
  begin
    if (rules <> nil) and (rules.Match(unitName, ruleIndex) = TSBOMUnitMatch.Component) then
      FMatchedRules.Add(LowerCase(rules.Rules[ruleIndex].Name));
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
  begin
    result := false;

    //1) Did one of the DPM packages we already enumerated ship this unit?
    if FUnitOwnership.TryGetValue(LowerCase(unitName), ownerBomRef) then
    begin
      ownerComp := report.FindComponentByBomRef(ownerBomRef);
      if ownerComp <> nil then
      begin
        ownerComp.AddEvidence('unit:' + unitName, 'map-no-source');
        NoteRuleForDpmUnit(unitName);
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

    //3) A component rule - the user's word for which library a unit belongs to, or that it is
    //   the project's own code.
    if rules <> nil then
    begin
      case rules.Match(unitName, ruleIndex) of
        TSBOMUnitMatch.Component :
        begin
          FindOrCreateRuleComponent(report, parentBomRef, rules.Rules[ruleIndex]).AddEvidence('unit:' + unitName, 'map-no-source');
          result := true;
          exit;
        end;
        TSBOMUnitMatch.OwnCode :
        begin
          result := true;
          exit;
        end;
      end;
    end;

    //4) General search-index lookup for non-Embarcadero-prefixed units.
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
      //Third-party: grouped by the folder the unit was found in.
      FindOrCreateThirdParty(foundPath, 'search-index').AddEvidence(foundPath, 'map-no-source');
      result := true;
    end;
  end;

begin
  dprojDir := ExcludeTrailingPathDelimiter(ExtractFilePath(projectPath));
  delphiRoot := GetDelphiRootDir(compilerVersion);
  cacheRoot := FPackageCache.PackagesFolder;

  if options.MapFile <> '' then
  begin
    //--map is user supplied too; resolve it against the dproj directory rather than the
    //current directory, which is what someone typing a relative map path means.
    mapPath := TPathUtils.ToAbsolutePath(options.MapFile, dprojDir);
    triedPaths := mapPath;
  end
  else
  begin
    //The OutputDir recorded in the project's per-(config,platform) configuration is the
    //primary location, but it isn't the only one worth looking at - see the notes on
    //GetMapFileCandidates. Take the first candidate that exists.
    outputDir := '';
    if editor <> nil then
    begin
      projectConfig := editor.GetProjectConfiguration(configName, platform);
      if projectConfig <> nil then
        outputDir := projectConfig.OutputDir;
    end;
    candidates := GetMapFileCandidates(dprojDir, outputDir, dprojName, configName, platform);
    mapPath := '';
    triedPaths := '';
    for i := 0 to Length(candidates) - 1 do
    begin
      if triedPaths <> '' then
        triedPaths := triedPaths + ', ';
      triedPaths := triedPaths + candidates[i];
      if (mapPath = '') and FileExists(candidates[i]) then
        mapPath := candidates[i];
    end;
    //Nothing found - report against the primary location, the one the user most likely expects.
    if (mapPath = '') and (Length(candidates) > 0) then
      mapPath := candidates[0];
  end;

  if (mapPath = '') or (not FileExists(mapPath)) then
  begin
    if options.Strict then
      raise EDPMError.Create(TErrorCode.E8113_MapFileRequired, 'The MAP file was not found - looked in: ' + triedPaths + ' (re-run with the linker option "Map file = Detailed" enabled, or supply --map=<path>)');
    report.AddMetaProperty('dpm:evidence', 'map-file-not-found');
    report.AddMetaProperty('dpm:evidence.mapPath', mapPath);
    FLogger.Warning(TWarningCode.W8114_MapFileNotFound, '[SBOM] MAP file not found - looked in: ' + triedPaths
                    + ' - SBOM will contain DPM packages + runtime only.'
                    + ' Enable the linker option "Map file = Detailed" to include non-DPM evidence.');
    exit;
  end;

  FLogger.Verbose('[SBOM] reading MAP file: ' + mapPath);
  info := FMapFileReader.Read(mapPath);
  if info <> nil then
    FMapFileRead := true;
  if info = nil then
  begin
    //the reader said why.
    if options.Strict then
      raise EDPMError.Create(TErrorCode.E8113_MapFileRequired, 'The MAP file [' + mapPath + '] could not be read.');
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

  //an earlier dproj of an aggregated group may already have created it
  unidentified := report.FindComponentByBomRef(cUnidentifiedBomRef);

  for u in info.Units do
  begin
    evidenceLoc := u.SourcePath;
    //dcc records some source paths as a bare file name, relative to nothing we can rely on - classify
    //those by unit name, which looks the unit up on the project's search path
    if (evidenceLoc <> '') and not TPath.IsPathRooted(evidenceLoc) then
      evidenceLoc := '';
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
        begin
          comp.AddEvidence(evidenceLoc, 'map');
          NoteRuleForDpmUnit(u.UnitName);
        end
        //if we couldn't match it, fall through to the unidentified bucket.
        else
        begin
          EnsureUnidentified;
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

      //Before the project folder check - a rule also claims library source vendored into the project.
      if rules <> nil then
      begin
        case rules.Match(u.UnitName, ruleIndex) of
          TSBOMUnitMatch.Component :
          begin
            FindOrCreateRuleComponent(report, parentBomRef, rules.Rules[ruleIndex]).AddEvidence(evidenceLoc, 'map');
            continue;
          end;
          TSBOMUnitMatch.OwnCode : continue;
        end;
      end;

      if PathStartsWith(evidenceLoc, dprojDir) then
      begin
        //own project source - intentionally not surfaced as a component.
        continue;
      end;

      //Third-party. Two files in the same dir collapse to one component.
      FindOrCreateThirdParty(evidenceLoc, 'map-file').AddEvidence(evidenceLoc, 'map');
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
      //Tiered classifier: per-package unit map -> prefix+path -> component rules ->
      //search index -> unidentified. See ClassifyNoSourcePathUnit for the full ladder.
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
  writeFailed : boolean;

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
      begin
        FLogger.Error(ErrorCodeOf(e, TErrorCode.E8115_SBOMWriteFailed), '[SBOM] unable to write ' + target + ' : ' + e.Message);
        writeFailed := true;
      end;
    end;
  end;

begin
  baseName := projectName + '-' + DPMPlatformToString(report.Platform);
  emittedAny := false;
  writeFailed := false;

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

  result := emittedAny and not writeFailed;
end;

end.
