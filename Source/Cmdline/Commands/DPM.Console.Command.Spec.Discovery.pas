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

end.
