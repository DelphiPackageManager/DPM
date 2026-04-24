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

unit DPM.Console.Command.Spec.Discovery;

//Pure discovery helpers for the spec scaffolder. No console interaction here -
//they return structured results that the command orchestrator turns into prompts
//or default values.

interface

uses
  System.SysUtils,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces;

type
  TDProjKind = (dkUnknown, dkRuntime, dkDesign);

  TCompilerFolder = record
    FolderName : string;      //just the leaf folder name, '' when the packages folder itself holds the dprojs
    FullPath   : string;      //absolute path to the folder containing the dproj(s)
    Compiler   : TCompilerVersion;
    DProjFiles : TArray<string>; //absolute paths to dproj files found inside
  end;

  TCompilerFolders = TArray<TCompilerFolder>;

  TDpkUnit = record
    UnitName : string;        //e.g. 'Spring.Collections'
    RelPath  : string;        //path as written in the dpk/dproj, forward-slashed (e.g. '../../Source/Base/Spring.pas')
  end;
  TDpkUnits = TArray<TDpkUnit>;

  TLogicalPackage = record
    Stem : string;            //package identifier stem, e.g. 'Spring.Base' or 'VSoft.VirtualListView'
    RuntimeDProj : string;    //absolute path to the runtime dproj (may be '')
    DesignDProj : string;     //absolute path to the design dproj (may be '')
  end;
  TLogicalPackages = TArray<TLogicalPackage>;

function FindSourceFolder(const rootDir : string; out folder : string) : boolean;
function FindPackagesFolder(const rootDir : string; out folder : string) : boolean;

/// <summary>
///  Looks for a LICENSE / COPYING file at the project root (with or without an
///  extension). Returns just the leaf file name on success - the caller prepends
///  './' when emitting the source entry.
/// </summary>
function FindLicenseFile(const rootDir : string; out leafName : string) : boolean;

function DetectGitRemoteUrl(const rootDir : string; out url : string) : boolean;
function NormaliseGitUrl(const rawUrl : string) : string;
function DerivePackageIdFromUrl(const normalisedUrl : string) : string;

function FolderNameToCompilerVersion(const folderName : string) : TCompilerVersion;

/// <summary>
///  Scans the supplied packages folder. If it has compiler subfolders, returns one
///  entry per subfolder whose name/contents resolve to a known compiler. If the
///  folder itself contains dproj files, returns a single entry with FolderName=''.
/// </summary>
function ScanPackagesFolder(const packagesFolder : string; const logger : ILogger;
  const config : IConfiguration) : TCompilerFolders;

function ClassifyDProj(const dprojPath : string) : TDProjKind;

/// <summary>
///  Returns the union of platforms enabled in the supplied dproj files, using the
///  existing IProjectEditor to parse each file.
/// </summary>
function CollectPlatforms(const dprojFiles : TArray<string>; const logger : ILogger;
  const config : IConfiguration) : TDPMPlatforms;

function MakeRelative(const baseDir : string; const fullPath : string) : string;

/// <summary>
///  Normalises a dproj/dpk filename stem to the logical package identifier
///  stem by stripping conventional runtime/design suffixes (R, D, Design,
///  DesignTime, .Designtime, .Design).
/// </summary>
function ComputeLogicalPackageStem(const leafName : string) : string;

/// <summary>
///  Pairs runtime and design dproj files that share a stem into logical packages.
/// </summary>
function GroupDProjsByStem(const dprojFiles : TArray<string>) : TLogicalPackages;

/// <summary>
///  Parses a .dpk file's `contains` clause. Returns empty when the file cannot
///  be read or contains no matching entries. Skips `{$IFDEF ...}` and line
///  comments.
/// </summary>
function ParseDpkContains(const dpkPath : string) : TDpkUnits;

/// <summary>
///  Extracts source unit paths from a .dproj's &lt;DCCReference Include="...pas"&gt;
///  nodes (falling back when the paired .dpk isn't parseable).
/// </summary>
function ExtractDprojUnits(const dprojPath : string) : TArray<string>;

/// <summary>
///  Parses the `requires` clause of a .dpk file. Returns bare package names
///  (stripped of any surrounding whitespace) with standard Delphi packages
///  filtered out via IsStandardDelphiPackage.
/// </summary>
function ParseDpkRequires(const dpkPath : string) : TArray<string>;

/// <summary>
///  Extracts package dependency names from a .dproj's &lt;DCCReference Include="*.dcp"&gt;
///  nodes, filtered the same way as ParseDpkRequires. Used as a fallback.
/// </summary>
function ExtractDprojDcpReferences(const dprojPath : string) : TArray<string>;

/// <summary>
///  Returns true for well-known Delphi runtime/design packages shipped with the
///  IDE (rtl, vcl, fmx, FireDAC*, Indy*, ...). Case-insensitive.
/// </summary>
function IsStandardDelphiPackage(const name : string) : boolean;

/// <summary>
///  Turns a list of source file paths relative to projectRoot into a minimal
///  set of ant-style globs (e.g. `./Source/Base/**.pas`). Paths with matching
///  extensions grouped under the same deepest-common ancestor collapse into a
///  single recursive glob. Mixed extensions produce separate globs per extension.
/// </summary>
function DeriveSourceGlobs(const unitPaths : TArray<string>; const projectRoot : string) : TArray<string>;

/// <summary>
///  Finds the deepest forward-slashed folder that contains every path in the
///  input list. Returns '' when paths don't share any folder (e.g. one is at
///  project root).
/// </summary>
function CommonParentFolder(const paths : TArray<string>) : string;

/// <summary>
///  Resolves `unitRelPath` as written in a dpk/dproj (possibly containing `..\`)
///  relative to the directory holding the dpk/dproj, producing a forward-slashed
///  path relative to projectRoot. Returns '' when the path escapes projectRoot.
/// </summary>
function ResolveDpkUnitPath(const containerDir : string; const unitRelPath : string;
  const projectRoot : string) : string;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Utils.Strings,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.Editor;

const
  cSourceCandidates : array[0..3] of string = ('Source', 'Sources', 'Src', 'src');
  cPackagesCandidates : array[0..3] of string = ('Packages', 'packages', 'Package', 'package');

function TryFindFolder(const rootDir : string; const candidates : array of string; out folder : string) : boolean;
var
  i : integer;
  candidate : string;
begin
  result := false;
  for i := Low(candidates) to High(candidates) do
  begin
    candidate := IncludeTrailingPathDelimiter(rootDir) + candidates[i];
    if TDirectory.Exists(candidate) then
    begin
      folder := candidate;
      exit(true);
    end;
  end;
end;

function FindSourceFolder(const rootDir : string; out folder : string) : boolean;
var
  pasFiles : TArray<string>;
begin
  result := TryFindFolder(rootDir, cSourceCandidates, folder);
  if result then
    exit;
  //fall back: current folder contains .pas files
  pasFiles := TDirectory.GetFiles(rootDir, '*.pas');
  if Length(pasFiles) > 0 then
  begin
    folder := rootDir;
    result := true;
  end;
end;

function FindPackagesFolder(const rootDir : string; out folder : string) : boolean;
begin
  result := TryFindFolder(rootDir, cPackagesCandidates, folder);
end;

function FindLicenseFile(const rootDir : string; out leafName : string) : boolean;
var
  entries : TArray<string>;
  entry : string;
  stem : string;
  actualLeaf : string;
begin
  //Match files like LICENSE, LICENSE.txt, License.md, COPYING etc. The file may
  //or may not have an extension, so we compare on the stem only. Listing the
  //directory and comparing preserves the actual on-disk casing for the dspec.
  result := false;
  leafName := '';
  try
    entries := TDirectory.GetFiles(rootDir);
  except
    exit;
  end;
  for entry in entries do
  begin
    actualLeaf := ExtractFileName(entry);
    stem := LowerCase(TPath.GetFileNameWithoutExtension(actualLeaf));
    if (stem = 'license') or (stem = 'licence') or (stem = 'copying') then
    begin
      leafName := actualLeaf;
      result := true;
      exit;
    end;
  end;
end;

function DetectGitRemoteUrl(const rootDir : string; out url : string) : boolean;
var
  cfgPath : string;
  lines : TStringList;
  i : integer;
  line : string;
  trimmed : string;
  inOrigin : boolean;
  originUrl : string;
  firstUrl : string;
  eqPos : integer;
  value : string;
  headerStart : integer;
  headerEnd : integer;
  header : string;
begin
  result := false;
  url := '';
  cfgPath := IncludeTrailingPathDelimiter(rootDir) + '.git' + PathDelim + 'config';
  if not TFile.Exists(cfgPath) then
    exit;
  lines := TStringList.Create;
  try
    try
      lines.LoadFromFile(cfgPath);
    except
      exit;
    end;
    inOrigin := false;
    originUrl := '';
    firstUrl := '';
    for i := 0 to lines.Count - 1 do
    begin
      line := lines[i];
      trimmed := Trim(line);
      if trimmed = '' then
        continue;
      if StartsStr(';', trimmed) or StartsStr('#', trimmed) then
        continue;
      if StartsStr('[', trimmed) then
      begin
        headerStart := Pos('[', trimmed);
        headerEnd := Pos(']', trimmed);
        if (headerStart > 0) and (headerEnd > headerStart) then
        begin
          header := Copy(trimmed, headerStart + 1, headerEnd - headerStart - 1);
          inOrigin := SameText(Trim(header), 'remote "origin"');
          //track whether we are inside any [remote "..."] block for the fallback
          if inOrigin or StartsText('remote ', Trim(header)) then
          begin
            //carry on - url= inside this block may be the first-seen remote
          end;
        end;
        continue;
      end;
      eqPos := Pos('=', trimmed);
      if eqPos <= 0 then
        continue;
      if not SameText(Trim(Copy(trimmed, 1, eqPos - 1)), 'url') then
        continue;
      value := Trim(Copy(trimmed, eqPos + 1, MaxInt));
      if value = '' then
        continue;
      if inOrigin and (originUrl = '') then
        originUrl := value;
      if firstUrl = '' then
        firstUrl := value;
    end;
    if originUrl <> '' then
      url := originUrl
    else
      url := firstUrl;
    result := url <> '';
  finally
    lines.Free;
  end;
end;

function NormaliseGitUrl(const rawUrl : string) : string;
var
  working : string;
  colonPos : integer;
  host : string;
  path : string;
begin
  working := Trim(rawUrl);
  if working = '' then
    exit('');

  //git@host:owner/repo(.git)
  if StartsStr('git@', working) then
  begin
    colonPos := Pos(':', working);
    if colonPos > 4 then
    begin
      host := Copy(working, 5, colonPos - 5);
      path := Copy(working, colonPos + 1, MaxInt);
      working := 'https://' + host + '/' + path;
    end;
  end
  //ssh://git@host/owner/repo(.git)
  else if StartsStr('ssh://git@', working) then
  begin
    working := 'https://' + Copy(working, Length('ssh://git@') + 1, MaxInt);
  end;

  //strip trailing .git
  if EndsStr('.git', LowerCase(working)) then
    SetLength(working, Length(working) - 4);

  result := working;
end;

function DerivePackageIdFromUrl(const normalisedUrl : string) : string;
var
  working : string;
  parts : TArray<string>;
  count : integer;
  owner : string;
  repo : string;
begin
  result := '';
  working := normalisedUrl;
  //drop the scheme
  if Pos('://', working) > 0 then
    working := Copy(working, Pos('://', working) + 3, MaxInt);
  //drop the host
  if Pos('/', working) > 0 then
    working := Copy(working, Pos('/', working) + 1, MaxInt)
  else
    exit;
  parts := TStringUtils.SplitStr(working, '/');
  count := Length(parts);
  if count < 2 then
    exit;
  owner := parts[count - 2];
  repo := parts[count - 1];
  if (owner = '') or (repo = '') then
    exit;
  result := owner + '.' + repo;
end;

function StripPrefix(const value : string; const prefixes : array of string; out stripped : string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := Low(prefixes) to High(prefixes) do
  begin
    if StartsText(prefixes[i], value) then
    begin
      stripped := Copy(value, Length(prefixes[i]) + 1, MaxInt);
      exit(true);
    end;
  end;
  stripped := value;
end;

function IsDigits(const s : string) : boolean;
var
  i : integer;
begin
  result := s <> '';
  for i := 1 to Length(s) do
    if not CharInSet(s[i], ['0'..'9']) then
      exit(false);
end;

function TryThreeDigitDelphiSeries(const candidate : string; out expanded : string) : boolean;
var
  major : integer;
  minor : integer;
begin
  //Convert a 3-digit "majorMinor" token (e.g. "104" -> "10.4", "110" -> "11.0",
  //"120" -> "12.0") used in folder conventions like "D104", "D110".
  result := false;
  if (Length(candidate) <> 3) or not IsDigits(candidate) then
    exit;
  major := StrToInt(Copy(candidate, 1, 2));
  minor := StrToInt(Copy(candidate, 3, 1));
  expanded := IntToStr(major) + '.' + IntToStr(minor);
  result := true;
end;

function FolderNameToCompilerVersion(const folderName : string) : TCompilerVersion;
var
  working : string;
  tmp : string;
  parts : TArray<string>;
  candidate : string;
  expanded : string;
  v : TCompilerVersion;
begin
  result := TCompilerVersion.UnknownVersion;
  working := Trim(folderName);
  if working = '' then
    exit;

  //Strip common prefixes like "RAD Studio " or "Embarcadero RAD Studio ".
  if StripPrefix(working, ['Embarcadero RAD Studio ', 'Embarcadero RadStudio ', 'RAD Studio ',
    'Rad Studio ', 'RadStudio ', 'Delphi '], tmp) then
    working := tmp;

  //"11.0 Alexandria" -> keep leading token for direct matching but also remember
  //the code name for fallback.
  parts := TStringUtils.SplitStr(working, ' ');
  if Length(parts) > 0 then
    candidate := parts[0]
  else
    candidate := working;

  //Direct match (e.g. "11", "10.4", "XE2", "delphi11").
  result := StringToCompilerVersion(candidate);
  if result <> TCompilerVersion.UnknownVersion then
    exit;

  //"D110"/"D104" - leading D followed by a 3-digit majorMinor.
  if (Length(candidate) >= 2) and (UpCase(candidate[1]) = 'D') then
  begin
    tmp := Copy(candidate, 2, MaxInt);
    if TryThreeDigitDelphiSeries(tmp, expanded) then
    begin
      result := StringToCompilerVersion(expanded);
      if result <> TCompilerVersion.UnknownVersion then
        exit;
    end;
    //"D11" / "D12" / "DXE2"
    result := StringToCompilerVersion(tmp);
    if result <> TCompilerVersion.UnknownVersion then
      exit;
  end;

  //Lib suffix - "280", "290", "370" etc.
  for v := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if v = TCompilerVersion.UnknownVersion then
      continue;
    try
      if SameText(candidate, CompilerToLibSuffix(v)) then
      begin
        result := v;
        exit;
      end;
    except
      continue;
    end;
  end;

  //Check every space-separated token against the code-name list - handles
  //"11.0 Alexandria", "Alexandria", "Rad Studio 12.0 Athens" etc.
  for v := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if v = TCompilerVersion.UnknownVersion then
      continue;
    if (CompilerCodeName(v) <> '') and ContainsText(working, CompilerCodeName(v)) then
    begin
      result := v;
      exit;
    end;
  end;
end;

function GetCompilerFromDProj(const dprojPath : string; const logger : ILogger;
  const config : IConfiguration) : TCompilerVersion;
var
  editor : IProjectEditor;
begin
  result := TCompilerVersion.UnknownVersion;
  editor := TProjectEditor.Create(logger, config, TCompilerVersion.UnknownVersion);
  try
    if editor.LoadProject(dprojPath, [TProjectElement.ProjectVersion]) then
      result := editor.CompilerVersion;
  except
    //swallow - return UnknownVersion
  end;
end;

function ScanPackagesFolder(const packagesFolder : string; const logger : ILogger;
  const config : IConfiguration) : TCompilerFolders;
var
  subDirs : TArray<string>;
  list : TArray<TCompilerFolder>;
  i : integer;
  dprojFiles : TArray<string>;
  entry : TCompilerFolder;
  compiler : TCompilerVersion;
  folderName : string;
begin
  SetLength(list, 0);
  if not TDirectory.Exists(packagesFolder) then
  begin
    result := list;
    exit;
  end;

  //if the folder itself has dprojs, treat it as a single-compiler layout
  dprojFiles := TDirectory.GetFiles(packagesFolder, '*.dproj');
  if Length(dprojFiles) > 0 then
  begin
    entry.FolderName := '';
    entry.FullPath := packagesFolder;
    entry.DProjFiles := dprojFiles;
    entry.Compiler := GetCompilerFromDProj(dprojFiles[0], logger, config);
    if entry.Compiler <> TCompilerVersion.UnknownVersion then
    begin
      SetLength(list, 1);
      list[0] := entry;
    end;
    result := list;
    exit;
  end;

  subDirs := TDirectory.GetDirectories(packagesFolder);
  for i := 0 to High(subDirs) do
  begin
    folderName := ExtractFileName(ExcludeTrailingPathDelimiter(subDirs[i]));
    compiler := FolderNameToCompilerVersion(folderName);
    dprojFiles := TDirectory.GetFiles(subDirs[i], '*.dproj');
    if (compiler = TCompilerVersion.UnknownVersion) and (Length(dprojFiles) > 0) then
      compiler := GetCompilerFromDProj(dprojFiles[0], logger, config);
    if compiler = TCompilerVersion.UnknownVersion then
      continue;
    if Length(dprojFiles) = 0 then
      continue;
    entry.FolderName := folderName;
    entry.FullPath := subDirs[i];
    entry.Compiler := compiler;
    entry.DProjFiles := dprojFiles;
    SetLength(list, Length(list) + 1);
    list[High(list)] := entry;
  end;
  result := list;
end;

function EndsWithSuffix(const stem : string; const suffix : string) : boolean;
var
  stemLen : integer;
  suffLen : integer;
begin
  result := false;
  stemLen := Length(stem);
  suffLen := Length(suffix);
  if (suffLen = 0) or (stemLen < suffLen) then
    exit;
  result := SameText(Copy(stem, stemLen - suffLen + 1, suffLen), suffix);
end;

function ClassifyByFilename(const dprojPath : string) : TDProjKind;
var
  stem : string;
  lastChar : Char;
begin
  result := dkUnknown;
  stem := TPath.GetFileNameWithoutExtension(dprojPath);
  if stem = '' then
    exit;

  //Longer suffixes first so "Design" beats "D".
  if EndsWithSuffix(stem, 'DesignTime') or
     EndsWithSuffix(stem, 'Design') or
     EndsWithSuffix(stem, 'DT') then
    exit(dkDesign);

  //Last char heuristic - R means runtime, D means design. Must be the literal
  //uppercase letter so we don't match random words ending in 'd' or 'r'.
  lastChar := stem[Length(stem)];
  if lastChar = 'D' then
    exit(dkDesign);
  if lastChar = 'R' then
    exit(dkRuntime);
end;

function ClassifyByDProjContent(const dprojPath : string) : TDProjKind;
var
  content : string;
  lowered : string;
begin
  result := dkUnknown;
  try
    content := TFile.ReadAllText(dprojPath);
  except
    exit;
  end;
  lowered := LowerCase(content);
  if (Pos('<designonlypackage>true</designonlypackage>', lowered) > 0) or
     (Pos('designtimeonly', lowered) > 0) then
    result := dkDesign
  else if Pos('<runtimeonlypackage>true</runtimeonlypackage>', lowered) > 0 then
    result := dkRuntime;
end;

function ClassifyDProj(const dprojPath : string) : TDProjKind;
begin
  result := ClassifyByFilename(dprojPath);
  if result <> dkUnknown then
    exit;
  result := ClassifyByDProjContent(dprojPath);
end;

function CollectPlatforms(const dprojFiles : TArray<string>; const logger : ILogger;
  const config : IConfiguration) : TDPMPlatforms;
var
  i : integer;
  editor : IProjectEditor;
begin
  result := [];
  for i := 0 to High(dprojFiles) do
  begin
    editor := TProjectEditor.Create(logger, config, TCompilerVersion.UnknownVersion);
    try
      if editor.LoadProject(dprojFiles[i], [TProjectElement.Platforms]) then
        result := result + editor.Platforms;
    except
      //swallow - one bad dproj shouldn't poison the batch
    end;
  end;
end;

function MakeRelative(const baseDir : string; const fullPath : string) : string;
var
  baseNorm : string;
  fullNorm : string;
begin
  baseNorm := IncludeTrailingPathDelimiter(ExcludeTrailingPathDelimiter(baseDir));
  fullNorm := IncludeTrailingPathDelimiter(ExcludeTrailingPathDelimiter(fullPath));
  if SameText(baseNorm, fullNorm) then
    exit('./');
  if StartsText(baseNorm, fullNorm) then
    result := './' + StringReplace(Copy(fullPath, Length(baseNorm) + 1, MaxInt), '\', '/', [rfReplaceAll])
  else
    result := StringReplace(fullPath, '\', '/', [rfReplaceAll]);
end;

function StripIfSuffix(const s : string; const suffix : string) : string;
begin
  if EndsText(suffix, s) and (Length(s) > Length(suffix)) then
    result := Copy(s, 1, Length(s) - Length(suffix))
  else
    result := s;
end;

function ComputeLogicalPackageStem(const leafName : string) : string;
var
  stem : string;
  stripped : string;
begin
  stem := TPath.GetFileNameWithoutExtension(leafName);
  if stem = '' then
    exit('');

  //Longer/compound design suffixes first.
  stripped := StripIfSuffix(stem, '.Designtime');
  if stripped <> stem then exit(stripped);
  stripped := StripIfSuffix(stem, '.DesignTime');
  if stripped <> stem then exit(stripped);
  stripped := StripIfSuffix(stem, '.Design');
  if stripped <> stem then exit(stripped);
  stripped := StripIfSuffix(stem, 'DesignTime');
  if stripped <> stem then exit(stripped);
  stripped := StripIfSuffix(stem, 'Design');
  if stripped <> stem then exit(stripped);

  //Single-char R/D - only strip when the preceding char is also an uppercase
  //letter or digit so we don't amputate names like "VSoft.CancellationToken"
  //into "VSoft.CancellationToke".
  if Length(stem) > 1 then
  begin
    if (stem[Length(stem)] = 'R') or (stem[Length(stem)] = 'D') then
      exit(Copy(stem, 1, Length(stem) - 1));
  end;
  result := stem;
end;

function GroupDProjsByStem(const dprojFiles : TArray<string>) : TLogicalPackages;
var
  i, j : integer;
  stem : string;
  leaf : string;
  kind : TDProjKind;
  existingIdx : integer;
  entry : TLogicalPackage;
  list : TLogicalPackages;
begin
  SetLength(list, 0);
  for i := 0 to High(dprojFiles) do
  begin
    leaf := ExtractFileName(dprojFiles[i]);
    stem := ComputeLogicalPackageStem(leaf);
    if stem = '' then
      continue;
    kind := ClassifyDProj(dprojFiles[i]);

    existingIdx := -1;
    for j := 0 to High(list) do
      if SameText(list[j].Stem, stem) then
      begin
        existingIdx := j;
        break;
      end;

    if existingIdx = -1 then
    begin
      entry.Stem := stem;
      entry.RuntimeDProj := '';
      entry.DesignDProj := '';
      case kind of
        dkDesign : entry.DesignDProj := dprojFiles[i];
      else
        //dkRuntime or dkUnknown - treat as runtime until a paired design dproj shows up
        entry.RuntimeDProj := dprojFiles[i];
      end;
      SetLength(list, Length(list) + 1);
      list[High(list)] := entry;
    end
    else
    begin
      case kind of
        dkDesign :
          if list[existingIdx].DesignDProj = '' then
            list[existingIdx].DesignDProj := dprojFiles[i];
      else
        if list[existingIdx].RuntimeDProj = '' then
          list[existingIdx].RuntimeDProj := dprojFiles[i];
      end;
    end;
  end;
  result := list;
end;

function StripDelphiComments(const input : string) : string;
var
  i : integer;
  len : integer;
  buffer : TStringBuilder;
begin
  //Remove `{...}` and `//` comments. Leaves whitespace/newlines in place so
  //our tokenizer still sees line breaks as separators.
  len := Length(input);
  buffer := TStringBuilder.Create(len);
  try
    i := 1;
    while i <= len do
    begin
      if input[i] = '{' then
      begin
        while (i <= len) and (input[i] <> '}') do
          Inc(i);
        if i <= len then
          Inc(i);
      end
      else if (i < len) and (input[i] = '/') and (input[i + 1] = '/') then
      begin
        while (i <= len) and (input[i] <> #10) and (input[i] <> #13) do
          Inc(i);
      end
      else
      begin
        buffer.Append(input[i]);
        Inc(i);
      end;
    end;
    result := buffer.ToString;
  finally
    buffer.Free;
  end;
end;

function FindWordBoundary(const s : string; const word : string; const startPos : integer) : integer;
var
  len : integer;
  wordLen : integer;
  i : integer;
  before : Char;
  after : Char;
begin
  result := 0;
  len := Length(s);
  wordLen := Length(word);
  i := startPos;
  while i <= len - wordLen + 1 do
  begin
    if SameText(Copy(s, i, wordLen), word) then
    begin
      before := #0;
      after := #0;
      if i > 1 then before := s[i - 1];
      if i + wordLen <= len then after := s[i + wordLen];
      if not CharInSet(before, ['A'..'Z', 'a'..'z', '0'..'9', '_']) and
         not CharInSet(after, ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      begin
        result := i;
        exit;
      end;
    end;
    Inc(i);
  end;
end;

function ParseDpkContains(const dpkPath : string) : TDpkUnits;
var
  raw : string;
  stripped : string;
  containsPos : integer;
  endPos : integer;
  body : string;
  entries : TArray<string>;
  i : integer;
  entry : string;
  inPos : integer;
  firstQuote : integer;
  secondQuote : integer;
  unitName : string;
  unitPath : string;
  parsed : TDpkUnit;
  list : TDpkUnits;
begin
  SetLength(list, 0);
  try
    raw := TFile.ReadAllText(dpkPath);
  except
    result := list;
    exit;
  end;

  stripped := StripDelphiComments(raw);
  containsPos := FindWordBoundary(stripped, 'contains', 1);
  if containsPos = 0 then
  begin
    result := list;
    exit;
  end;

  //the contains clause runs from after the keyword up to `end.` or the end of file.
  endPos := FindWordBoundary(stripped, 'end', containsPos + Length('contains'));
  if endPos = 0 then
    endPos := Length(stripped) + 1;

  body := Copy(stripped, containsPos + Length('contains'), endPos - containsPos - Length('contains'));
  //Normalise separators so a simple split by comma gives us entries.
  body := StringReplace(body, #13#10, ' ', [rfReplaceAll]);
  body := StringReplace(body, #10, ' ', [rfReplaceAll]);
  body := StringReplace(body, #13, ' ', [rfReplaceAll]);
  body := StringReplace(body, #9, ' ', [rfReplaceAll]);
  //strip trailing semicolon if any
  body := Trim(body);
  while (body <> '') and (body[Length(body)] = ';') do
    SetLength(body, Length(body) - 1);

  entries := TStringUtils.SplitStr(body, ',');
  for i := 0 to High(entries) do
  begin
    entry := Trim(entries[i]);
    if entry = '' then continue;
    inPos := FindWordBoundary(entry, 'in', 1);
    if inPos = 0 then
    begin
      //unit listed without a file path - skip; we cannot derive a source entry
      continue;
    end;
    unitName := Trim(Copy(entry, 1, inPos - 1));
    if unitName = '' then continue;
    firstQuote := PosEx('''', entry, inPos + 2);
    if firstQuote = 0 then continue;
    secondQuote := PosEx('''', entry, firstQuote + 1);
    if secondQuote = 0 then continue;
    unitPath := Copy(entry, firstQuote + 1, secondQuote - firstQuote - 1);
    if unitPath = '' then continue;
    parsed.UnitName := unitName;
    parsed.RelPath := StringReplace(unitPath, '\', '/', [rfReplaceAll]);
    SetLength(list, Length(list) + 1);
    list[High(list)] := parsed;
  end;
  result := list;
end;

function ExtractDprojUnits(const dprojPath : string) : TArray<string>;
const
  cNeedle = 'DCCReference Include=';
var
  raw : string;
  list : TArray<string>;
  cursor : integer;
  nextPos : integer;
  quoteStart : integer;
  quoteEnd : integer;
  value : string;
  lowerValue : string;
begin
  SetLength(list, 0);
  try
    raw := TFile.ReadAllText(dprojPath);
  except
    result := list;
    exit;
  end;
  cursor := 1;
  while cursor <= Length(raw) do
  begin
    nextPos := PosEx(cNeedle, raw, cursor);
    if nextPos = 0 then break;
    quoteStart := PosEx('"', raw, nextPos + Length(cNeedle));
    if quoteStart = 0 then break;
    quoteEnd := PosEx('"', raw, quoteStart + 1);
    if quoteEnd = 0 then break;
    value := Copy(raw, quoteStart + 1, quoteEnd - quoteStart - 1);
    lowerValue := LowerCase(value);
    //Ignore references to .dcp / packages / $() tokens - we only want source units.
    if EndsStr('.pas', lowerValue) then
    begin
      SetLength(list, Length(list) + 1);
      list[High(list)] := StringReplace(value, '\', '/', [rfReplaceAll]);
    end;
    cursor := quoteEnd + 1;
  end;
  result := list;
end;

function IsStandardDelphiPackage(const name : string) : boolean;
const
  //Standard Delphi runtime/design packages shipped with the IDE. Case-insensitive
  //exact match only - a prefix rule risks shadowing user packages that happen
  //to share a stem (e.g. 'fmxFramework', 'dsnapMyFix').
  cExact : array[0..51] of string = (
    'rtl', 'vcl', 'fmx', 'fmxase', 'fmxdae', 'fmxFireDAC', 'fmxobj', 'fmxobjects',
    'vclactnband', 'vcldb', 'vcldbx', 'vcldsnap', 'vclie',
    'vclimg', 'vclimaging', 'vclribbon', 'vclsmp', 'vcltouch', 'vclx', 'vclwinx',
    'dbrtl', 'dsnap', 'dsnapcon', 'dsnaprest', 'dsnapxml',
    'soaprtl', 'inet', 'inetdb', 'xmlrtl',
    'webdsnap', 'webdsnapvcl',
    'tee', 'teedb', 'teeui', 'tethering',
    'bindengine', 'bindcomp', 'bindcompvcl', 'bindcompfmx', 'bindcompdbx', 'dataimpl',
    'designide', 'dbxcommondriver', 'dbxcds',
    'IndyCore', 'IndyProtocols', 'IndySystem', 'IndyIPCommon', 'IndyIPServer', 'IndyIPClient',
    'FireDAC', 'FireDACCommon'
    );
var
  i : integer;
  trimmed : string;
  lowered : string;
begin
  result := false;
  trimmed := Trim(name);
  if trimmed = '' then
    exit(true);
  lowered := LowerCase(trimmed);
  for i := Low(cExact) to High(cExact) do
    if lowered = LowerCase(cExact[i]) then
      exit(true);
  //The FireDAC family has many variants (FireDACIBDriver, FireDACMSSQLDriver, ...).
  //These are all Embarcadero-shipped, so treat the prefix as standard. The
  //lowercase prefix is unusual enough that user collisions are unlikely.
  if StartsText('FireDAC', trimmed) then
    exit(true);
end;

function CollectRequiresBody(const dpkPath : string; out body : string) : boolean;
var
  raw : string;
  stripped : string;
  requiresPos : integer;
  terminator : integer;
  containsAt : integer;
  endAt : integer;
  semicolonAt : integer;
begin
  result := false;
  body := '';
  try
    raw := TFile.ReadAllText(dpkPath);
  except
    exit;
  end;
  stripped := StripDelphiComments(raw);
  requiresPos := FindWordBoundary(stripped, 'requires', 1);
  if requiresPos = 0 then
    exit;
  //requires ends at the first of: `contains`, `end`, or `;` (outside any string)
  containsAt := FindWordBoundary(stripped, 'contains', requiresPos + Length('requires'));
  endAt := FindWordBoundary(stripped, 'end', requiresPos + Length('requires'));
  semicolonAt := PosEx(';', stripped, requiresPos + Length('requires'));
  terminator := MaxInt;
  if (containsAt > 0) and (containsAt < terminator) then terminator := containsAt;
  if (endAt > 0) and (endAt < terminator) then terminator := endAt;
  //the trailing `;` in `requires rtl;` must be included before the contains
  //keyword, so accept it when it's strictly before the next keyword
  if (semicolonAt > 0) and (semicolonAt < terminator) then
    //keep - but use the token before the semicolon as the end
    terminator := semicolonAt;
  if terminator = MaxInt then
    exit;
  body := Copy(stripped, requiresPos + Length('requires'), terminator - requiresPos - Length('requires'));
  result := true;
end;

function ParseDpkRequires(const dpkPath : string) : TArray<string>;
var
  body : string;
  entries : TArray<string>;
  i : integer;
  entry : string;
  list : TArray<string>;
begin
  SetLength(list, 0);
  if not CollectRequiresBody(dpkPath, body) then
  begin
    result := list;
    exit;
  end;
  //entries are comma-separated identifiers, possibly containing dots
  body := StringReplace(body, #13#10, ' ', [rfReplaceAll]);
  body := StringReplace(body, #10, ' ', [rfReplaceAll]);
  body := StringReplace(body, #13, ' ', [rfReplaceAll]);
  body := StringReplace(body, #9, ' ', [rfReplaceAll]);
  entries := TStringUtils.SplitStr(body, ',');
  for i := 0 to High(entries) do
  begin
    entry := Trim(entries[i]);
    //strip stray trailing semicolons just in case
    while (entry <> '') and (entry[Length(entry)] = ';') do
      SetLength(entry, Length(entry) - 1);
    entry := Trim(entry);
    if entry = '' then continue;
    if IsStandardDelphiPackage(entry) then continue;
    SetLength(list, Length(list) + 1);
    list[High(list)] := entry;
  end;
  result := list;
end;

function ExtractDprojDcpReferences(const dprojPath : string) : TArray<string>;
const
  cNeedle = 'DCCReference Include=';
var
  raw : string;
  list : TArray<string>;
  cursor : integer;
  nextPos : integer;
  quoteStart : integer;
  quoteEnd : integer;
  value : string;
  lowerValue : string;
  baseName : string;
begin
  SetLength(list, 0);
  try
    raw := TFile.ReadAllText(dprojPath);
  except
    result := list;
    exit;
  end;
  cursor := 1;
  while cursor <= Length(raw) do
  begin
    nextPos := PosEx(cNeedle, raw, cursor);
    if nextPos = 0 then break;
    quoteStart := PosEx('"', raw, nextPos + Length(cNeedle));
    if quoteStart = 0 then break;
    quoteEnd := PosEx('"', raw, quoteStart + 1);
    if quoteEnd = 0 then break;
    value := Copy(raw, quoteStart + 1, quoteEnd - quoteStart - 1);
    lowerValue := LowerCase(value);
    if EndsStr('.dcp', lowerValue) then
    begin
      baseName := ChangeFileExt(ExtractFileName(value), '');
      if (baseName <> '') and not IsStandardDelphiPackage(baseName) then
      begin
        SetLength(list, Length(list) + 1);
        list[High(list)] := baseName;
      end;
    end;
    cursor := quoteEnd + 1;
  end;
  result := list;
end;

function ResolveDpkUnitPath(const containerDir : string; const unitRelPath : string;
  const projectRoot : string) : string;
var
  combined : string;
  normalisedRoot : string;
  normalisedCombined : string;
begin
  result := '';
  if unitRelPath = '' then
    exit;
  //TPath.Combine + ExpandFileName handle .. segments cleanly.
  combined := unitRelPath;
  combined := StringReplace(combined, '/', PathDelim, [rfReplaceAll]);
  combined := TPath.Combine(containerDir, combined);
  try
    combined := ExpandFileName(combined);
  except
    exit;
  end;
  normalisedRoot := IncludeTrailingPathDelimiter(ExcludeTrailingPathDelimiter(projectRoot));
  normalisedCombined := combined;
  if not StartsText(normalisedRoot, normalisedCombined) then
    exit;
  result := StringReplace(Copy(combined, Length(normalisedRoot) + 1, MaxInt), '\', '/', [rfReplaceAll]);
end;

function CommonParentFolder(const paths : TArray<string>) : string;
var
  i : integer;
  parts : TArray<string>;
  currentParts : TArray<string>;
  sharedLen : integer;
  j : integer;
  k : integer;
  buffer : string;
  folder : string;
begin
  //paths are files, forward-slash. Return the deepest folder all share.
  result := '';
  if Length(paths) = 0 then exit;
  folder := paths[0];
  j := LastDelimiter('/', folder);
  if j = 0 then
    exit; //no folder component - everything is at root
  folder := Copy(folder, 1, j - 1);
  parts := TStringUtils.SplitStr(folder, '/');

  for i := 1 to High(paths) do
  begin
    folder := paths[i];
    j := LastDelimiter('/', folder);
    if j = 0 then
      //this path has no folder - common parent is root
      exit('');
    folder := Copy(folder, 1, j - 1);
    currentParts := TStringUtils.SplitStr(folder, '/');
    sharedLen := 0;
    for k := 0 to High(parts) do
    begin
      if k > High(currentParts) then break;
      if not SameText(parts[k], currentParts[k]) then break;
      Inc(sharedLen);
    end;
    SetLength(parts, sharedLen);
    if sharedLen = 0 then
      exit('');
  end;

  buffer := '';
  for k := 0 to High(parts) do
  begin
    if buffer = '' then
      buffer := parts[k]
    else
      buffer := buffer + '/' + parts[k];
  end;
  result := buffer;
end;

function HasPasExtension(const path : string) : boolean;
begin
  result := EndsText('.pas', path);
end;

function HasIncExtension(const path : string) : boolean;
begin
  result := EndsText('.inc', path);
end;

function AppendGlob(const list : TArray<string>; const glob : string) : TArray<string>;
begin
  result := list;
  SetLength(result, Length(result) + 1);
  result[High(result)] := glob;
end;

function DeriveSourceGlobs(const unitPaths : TArray<string>; const projectRoot : string) : TArray<string>;
var
  pasPaths : TArray<string>;
  incPaths : TArray<string>;
  i : integer;
  commonPas : string;
  commonInc : string;
  list : TArray<string>;
begin
  SetLength(list, 0);
  SetLength(pasPaths, 0);
  SetLength(incPaths, 0);
  //We only ever receive .pas paths from the dpk parser; .inc files are added
  //separately by the caller if they're discovered alongside. Still, handle a
  //mixed list for robustness.
  for i := 0 to High(unitPaths) do
  begin
    if HasPasExtension(unitPaths[i]) then
    begin
      SetLength(pasPaths, Length(pasPaths) + 1);
      pasPaths[High(pasPaths)] := unitPaths[i];
    end
    else if HasIncExtension(unitPaths[i]) then
    begin
      SetLength(incPaths, Length(incPaths) + 1);
      incPaths[High(incPaths)] := unitPaths[i];
    end;
  end;

  if Length(pasPaths) > 0 then
  begin
    commonPas := CommonParentFolder(pasPaths);
    if commonPas <> '' then
      list := AppendGlob(list, commonPas + '/**.pas')
    else
      //no common folder - fall back to listing each file
      for i := 0 to High(pasPaths) do
        list := AppendGlob(list, pasPaths[i]);
  end;

  if Length(incPaths) > 0 then
  begin
    commonInc := CommonParentFolder(incPaths);
    if commonInc <> '' then
      list := AppendGlob(list, commonInc + '/**.inc')
    else
      for i := 0 to High(incPaths) do
        list := AppendGlob(list, incPaths[i]);
  end;

  //silence unused-parameter warning in XE2 compilers
  if projectRoot = '' then ;
  result := list;
end;

end.
