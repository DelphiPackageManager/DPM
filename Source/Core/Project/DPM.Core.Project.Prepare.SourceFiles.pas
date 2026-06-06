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

unit DPM.Core.Project.Prepare.SourceFiles;

// Shared, stateless helpers for expanding source-file globs into the
// TPrepareSourceFile list consumed by the dpk/dproj template renderer. Used by
// both `dpm prepare` (scaffolding into the author's source tree) and the
// install-time package generator (scaffolding into the package cache). Kept in
// one place so the glob expansion, .dfm form detection, variable substitution
// and relative-path logic don't drift between the two callers.

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Project.Prepare.Templates;

type
  TPrepareSourceGatherer = class
  public
    //matches $name$ tokens used by Pack for variable substitution. Source paths still
    //carrying one after resolution refer to a per-version artefact that doesn't exist yet
    //and isn't a valid DCCReference target.
    class function ContainsUnresolvedVariable(const value : string) : boolean; static;

    //Whitelist of extensions valid as DCCReferences in a Delphi package dproj. .dfm files
    //are pulled in implicitly by the {FormName} reference on their owning .pas unit.
    class function ShouldIncludeSourceFile(const filePath : string) : boolean; static;

    //Best-effort form identifier from a text-form .dfm's first line. '' for binary dfm.
    class function ExtractFormNameFromDfm(const dfmPath : string) : string; static;

    //compute relative path from fromDir to toPath. Both must be absolute and on the same drive.
    class function MakeRelativePath(const fromDir, toPath : string) : string; static;

    //builds the variable dictionary used to expand $name$ tokens. Mirrors the Pack-time
    //variable set so dspec authors see consistent behaviour between Pack and Prepare.
    class function BuildVariables(const spec : IPackageSpec; const compiler : TCompilerVersion) : IDictionary<string, string>; static;

    //iterative $name$ token substitution; returns the input verbatim if no tokens are
    //present, or after a fixed point (also bounded to detect circular references).
    class function ResolveVariables(const value : string; const variables : IDictionary<string, string>) : string; static;

    //Expand each glob in patterns (relative to baseDir) into absolute source files,
    //filter to package-able extensions, dedupe, attach form names, and drop anything
    //matching an exclude glob (matched against the file name). exclude may be nil/empty.
    class function GatherFromPatterns(const logger : ILogger; const baseDir : string;
                                      const patterns, exclude : IList<string>;
                                      const variables : IDictionary<string, string>) : IList<TPrepareSourceFile>; static;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Masks,
  System.RegularExpressions,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Strings;

class function TPrepareSourceGatherer.ContainsUnresolvedVariable(const value : string) : boolean;
begin
  result := TRegEx.IsMatch(value, '\$[A-Za-z_][A-Za-z0-9_]*\$');
end;

class function TPrepareSourceGatherer.ShouldIncludeSourceFile(const filePath : string) : boolean;
var
  ext : string;
begin
  //.dfm files come in via the {FormName} annotation on their owning .pas, so they're
  //deliberately excluded here. Everything else (.txt, .md, .json, accidental .dproj/.dpk
  //glob hits) should not be referenced by the dproj at all.
  ext := LowerCase(ExtractFileExt(filePath));
  result := (ext = '.pas') or (ext = '.inc') or (ext = '.rc') or (ext = '.res');
end;

class function TPrepareSourceGatherer.ExtractFormNameFromDfm(const dfmPath : string) : string;
var
  fs : TFileStream;
  buffer : TBytes;
  bytesRead : integer;
  text : string;
  newlineIdx : integer;
  firstLine : string;
  match : TMatch;
begin
  //Text-form .dfm files start with `object Name : TClassName` (or `inherited`).
  //Binary .dfm files don't parse this way - we silently skip them. Reading just
  //the first 512 bytes keeps the cost negligible.
  result := '';
  if not FileExists(dfmPath) then
    exit;
  try
    fs := TFileStream.Create(dfmPath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(buffer, 512);
      bytesRead := fs.Read(buffer[0], Length(buffer));
      if bytesRead <= 0 then
        exit;
      SetLength(buffer, bytesRead);
    finally
      fs.Free;
    end;
    text := TEncoding.UTF8.GetString(buffer);
    //skip a UTF-8 BOM if present.
    if (Length(text) > 0) and (text[1] = #$FEFF) then
      Delete(text, 1, 1);
    newlineIdx := Pos(#10, text);
    if newlineIdx > 0 then
      firstLine := Copy(text, 1, newlineIdx - 1)
    else
      firstLine := text;
    firstLine := Trim(StringReplace(firstLine, #13, '', [rfReplaceAll]));
    match := TRegEx.Match(firstLine, '^(object|inherited)\s+(\w+)\s*:', [roIgnoreCase]);
    if match.Success and (match.Groups.Count >= 3) then
      result := match.Groups.Item[2].Value;
  except
    on e : Exception do
      result := ''; //best-effort; don't fail the scaffold over an unreadable dfm.
  end;
end;

class function TPrepareSourceGatherer.MakeRelativePath(const fromDir, toPath : string) : string;
var
  fromSegments : TArray<string>;
  toSegments : TArray<string>;
  i : integer;
  commonLen : integer;
  upCount : integer;
  k : integer;
  normalizedFrom : string;
  normalizedTo : string;
begin
  //normalize both paths to absolute and use a single separator so segment comparison
  //is straightforward. ExcludeTrailingPathDelimiter avoids a phantom empty trailing
  //segment when fromDir was supplied with a trailing slash.
  normalizedFrom := ExcludeTrailingPathDelimiter(TPath.GetFullPath(fromDir));
  normalizedTo := TPath.GetFullPath(toPath);
  normalizedFrom := StringReplace(normalizedFrom, '/', PathDelim, [rfReplaceAll]);
  normalizedTo := StringReplace(normalizedTo, '/', PathDelim, [rfReplaceAll]);

  fromSegments := TStringUtils.SplitStr(normalizedFrom, PathDelim);
  toSegments := TStringUtils.SplitStr(normalizedTo, PathDelim);

  commonLen := 0;
  while (commonLen < Length(fromSegments)) and (commonLen < Length(toSegments)) and
        SameText(fromSegments[commonLen], toSegments[commonLen]) do
    Inc(commonLen);

  //if there's no common root (e.g. different drives) fall back to the absolute path.
  if commonLen = 0 then
  begin
    result := normalizedTo;
    exit;
  end;

  upCount := Length(fromSegments) - commonLen;
  result := '';
  for i := 0 to upCount - 1 do
    result := result + '..' + PathDelim;
  for k := commonLen to High(toSegments) do
  begin
    result := result + toSegments[k];
    if k < High(toSegments) then
      result := result + PathDelim;
  end;
  if result = '' then
    result := '.';
end;

class function TPrepareSourceGatherer.BuildVariables(const spec : IPackageSpec; const compiler : TCompilerVersion) : IDictionary<string, string>;
var
  pair : TPair<string, string>;
begin
  result := TCollections.CreateDictionary<string, string>;

  //compiler-derived variables - mirrors TPackageWriter.PopulateVariables so dspec
  //authors see consistent behaviour between Pack and Prepare.
  result['compiler'] := CompilerToString(compiler);
  result['target'] := CompilerToString(compiler);
  result['compilernoprefix'] := CompilerNoPrefix(compiler);
  result['compilernopoint'] := CompilerToStringNoPoint(compiler);
  result['compilershortversion'] := CompilerToShortVersion(compiler);
  result['compilercodename'] := CompilerCodeName(compiler);
  result['compilerwithcodename'] := CompilerWithCodeName(compiler);
  result['compilermajornoprefix'] := CompilerMajorNoPrefix(compiler);

  result['compilerversion'] := CompilerToCompilerVersionIntStr(compiler);
  result['libsuffix'] := CompilerToLibSuffix(compiler);
  result['libsuffixshort'] := CompilerToLibSuffixShort(compiler);

  result['bdsversion'] := CompilerToBDSVersion(compiler);

  //spec-declared variables overlay compiler-derived ones. Values may themselves contain
  //$name$ tokens - ResolveVariables iterates to a fixed point so chains work.
  if (spec <> nil) and (spec.Variables <> nil) then
    for pair in spec.Variables do
      result[LowerCase(pair.Key)] := pair.Value;
end;

class function TPrepareSourceGatherer.ResolveVariables(const value : string; const variables : IDictionary<string, string>) : string;
const
  cMaxIterations = 10; //circular-reference guard
var
  pair : TPair<string, string>;
  previous : string;
  iterations : integer;
begin
  result := value;
  if Pos('$', result) = 0 then
    exit;
  iterations := 0;
  repeat
    previous := result;
    for pair in variables do
      result := StringReplace(result, '$' + pair.Key + '$', pair.Value, [rfReplaceAll, rfIgnoreCase]);
    Inc(iterations);
  until (result = previous) or (iterations >= cMaxIterations);
end;

class function TPrepareSourceGatherer.GatherFromPatterns(const logger : ILogger; const baseDir : string;
                                                         const patterns, exclude : IList<string>;
                                                         const variables : IDictionary<string, string>) : IList<TPrepareSourceFile>;

  function IsExcluded(const fileName : string) : boolean;
  var
    e : integer;
    pattern : string;
    mask : string;
  begin
    result := false;
    if exclude = nil then
      exit;
    for e := 0 to exclude.Count - 1 do
    begin
      pattern := ResolveVariables(exclude[e], variables);
      pattern := StringReplace(pattern, '/', PathDelim, [rfReplaceAll]);
      //exclude entries are file-name masks (e.g. *.Tests.pas, Test*.pas). Match against
      //the leaf name so a directory-qualified pattern still excludes by its mask.
      mask := ExtractFileName(pattern);
      if mask = '' then
        continue;
      if MatchesMask(fileName, mask) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

var
  j : integer;
  pattern : string;
  rawPattern : string;
  searchDir : string;
  mask : string;
  files : TArray<string>;
  k : integer;
  abs : string;
  dfmPath : string;
  seen : ISet<string>;
  entry : TPrepareSourceFile;
  fileName : string;
begin
  result := TCollections.CreateList<TPrepareSourceFile>;
  seen := TCollections.CreateSet<string>;
  if patterns = nil then
    exit;

  for j := 0 to patterns.Count - 1 do
  begin
    rawPattern := patterns[j];
    if Trim(rawPattern) = '' then
      continue;

    //resolve $name$ tokens against the variable set. Anything unresolved after this is
    //a genuine dspec authoring issue.
    pattern := ResolveVariables(rawPattern, variables);
    if ContainsUnresolvedVariable(pattern) then
    begin
      logger.Warning('Source entry has unresolvable variable - skipping: ' + rawPattern);
      continue;
    end;

    //normalize separators and split off the file mask.
    pattern := StringReplace(pattern, '/', PathDelim, [rfReplaceAll]);
    mask := ExtractFileName(pattern);
    searchDir := ExtractFilePath(pattern);
    if mask = '' then
      mask := '*.pas';

    if TPathUtils.IsPathRooted(searchDir) then
      searchDir := ExcludeTrailingPathDelimiter(searchDir)
    else
      searchDir := ExcludeTrailingPathDelimiter(TPath.GetFullPath(TPath.Combine(baseDir, searchDir)));

    if not TDirectory.Exists(searchDir) then
    begin
      logger.Warning('Source path does not exist (skipping): ' + searchDir);
      continue;
    end;

    files := TArray<string>(TDirectory.GetFiles(searchDir, mask, TSearchOption.soTopDirectoryOnly));
    if rawPattern = pattern then
      logger.Information(Format('Source pattern %s matched %d file(s)', [rawPattern, Length(files)]))
    else
      logger.Information(Format('Source pattern %s (resolved: %s) matched %d file(s)', [rawPattern, pattern, Length(files)]));

    for k := 0 to High(files) do
    begin
      abs := TPath.GetFullPath(files[k]);
      //Skip extensions that shouldn't be in a dpk/dproj. .dfm files come in via
      //the {FormName} annotation on their owning .pas.
      if not ShouldIncludeSourceFile(abs) then
        continue;
      fileName := ExtractFileName(abs);
      if IsExcluded(fileName) then
      begin
        logger.Information('  excluded: ' + abs);
        continue;
      end;
      if seen.Contains(LowerCase(abs)) then
        continue;
      seen.Add(LowerCase(abs));

      entry.Path := abs;
      entry.FormName := '';
      //Form detection: if this is a .pas file with a sibling .dfm, lift the form
      //identifier from the .dfm's first line. Other extensions get no FormName.
      if SameText(ExtractFileExt(abs), '.pas') then
      begin
        dfmPath := ChangeFileExt(abs, '.dfm');
        if FileExists(dfmPath) then
          entry.FormName := ExtractFormNameFromDfm(dfmPath);
      end;

      result.Add(entry);
      if entry.FormName <> '' then
        logger.Information('  ' + abs + ' (form: ' + entry.FormName + ')')
      else
        logger.Information('  ' + abs);
    end;
  end;
end;

end.
