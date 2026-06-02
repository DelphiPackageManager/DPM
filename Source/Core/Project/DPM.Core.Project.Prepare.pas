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

unit DPM.Core.Project.Prepare;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Project.Transformer,
  DPM.Core.Project.Prepare.Templates;

type
  TPreparePair = record
    DpkFile : string;
    DprojFile : string;
    BaseName : string;
  end;

  //A single dpk/dproj pair to scaffold, derived from one entry in the dspec's
  //templates.build or templates.design lists. ProjectName is the file basename
  //(no extension), Kind determines runtime-only vs design-only flags in the dproj.
  //Platforms is the entry-level override - empty means "use the targetPlatform's
  //full list"; non-empty intersects with the targetPlatform's set at scaffold time.
  //References are the author-declared extra requires (vcl, fmx, sibling runtime
  //packages) that the prepare command should emit in the dpk's requires clause
  //and as DCCReferences in the dproj. nil/empty means "no extra references".
  TPrepareScaffoldItem = record
    ProjectName : string;
    Kind : TPrepareProjectKind;
    Platforms : TDPMPlatforms;
    References : IList<string>;
  end;

  TPrepareFolderState = record
    Compiler : TCompilerVersion;
    FolderPath : string;
    Pairs : IList<TPreparePair>;
    NewestDprojTime : TDateTime;
  end;

  IPreparePackageFolders = interface
    ['{8C3D9A57-4E5F-4BA6-9C71-1E2D3F4A5B6C}']
    //returns true if the operation completed (including the scaffold case which exits
    //asking the user to configure and re-run); false on hard error. When dryRun is true,
    //no folders are created and no files are written - the engine logs what it would do.
    function Execute(const specFile : string; const force : boolean; const dryRun : boolean;
                     const cancellationToken : ICancellationToken) : boolean;
  end;

  TPreparePackageFolders = class(TInterfacedObject, IPreparePackageFolders)
  private
    FLogger : ILogger;
    FSpecReader : IPackageSpecReader;
    FTransformer : IProjectTransformer;
    FDpkTransformer : IDpkTransformer;
    //set by Execute, consumed by helper methods. Avoids threading it through every signature.
    FDryRun : boolean;
  protected
    function GatherSupportedCompilers(const spec : IPackageSpec) : TCompilerVersions;
    function FindPairsInFolder(const folderPath : string) : IList<TPreparePair>;
    function PickSourceFolder(const folderStates : IList<TPrepareFolderState>; out source : TPrepareFolderState) : boolean;
    procedure EnsureFolders(const packagesRoot : string; const compilers : TCompilerVersions);
    function ScaffoldInitialPair(const packagesRoot : string; const specDir : string;
                                 const spec : IPackageSpec; const packageId : string;
                                 const compiler : TCompilerVersion; const force : boolean) : boolean;
    //Write one dpk/dproj pair for a single project name + kind into folderPath.
    procedure ScaffoldOnePair(const folderPath, projectName : string;
                              const kind : TPrepareProjectKind;
                              const compiler : TCompilerVersion;
                              const relativeSources : IList<TPrepareSourceFile>;
                              const requiredPackages : IList<string>;
                              const platforms : TDPMPlatforms;
                              const force : boolean);
    function PropagatePair(const targetCompiler : TCompilerVersion;
                           const targetFolder : string;
                           const pair : TPreparePair; const force : boolean) : boolean;
    //expand all spec.Templates[].Sources[] globs into absolute file paths, deduped.
    //compiler is used to resolve $compiler*$ variables for the scaffold target.
    function GatherSourceFiles(const specDir : string; const spec : IPackageSpec;
                               const compiler : TCompilerVersion) : IList<TPrepareSourceFile>;
    //compute relative path from fromDir to toPath. Both must be absolute and on the same drive.
    function MakeRelativePath(const fromDir, toPath : string) : string;
    //builds the variable dictionary used to expand $name$ tokens in source paths.
    //Mirrors the Pack-time variable set so users authoring dspecs see consistent behaviour.
    function BuildVariables(const spec : IPackageSpec; const compiler : TCompilerVersion) : IDictionary<string, string>;
    //iterative $name$ token substitution against the supplied variable dict; returns
    //the input verbatim if no tokens are present, or after no further substitutions
    //can be made (also limits iterations to detect circular references).
    function ResolveVariables(const value : string; const variables : IDictionary<string, string>) : string;
    //Collect every (project name, kind) pair the dspec wants scaffolded: one per
    //distinct templates.build[].project (kind = pkRuntime) and templates.design[].project
    //(kind = pkDesign). Variables in project paths are resolved first. Returns an empty
    //list when the dspec declares no build/design entries; the caller then falls back
    //to a single packageId + 'R' runtime scaffold.
    function GatherScaffoldItems(const spec : IPackageSpec; const variables : IDictionary<string, string>) : IList<TPrepareScaffoldItem>;
    //consistent prefix on log lines when dry-run is active.
    function LogPrefix : string;
  public
    constructor Create(const logger : ILogger; const specReader : IPackageSpecReader;
                       const transformer : IProjectTransformer; const dpkTransformer : IDpkTransformer);

    function Execute(const specFile : string; const force : boolean; const dryRun : boolean;
                     const cancellationToken : ICancellationToken) : boolean;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  System.RegularExpressions,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Strings;

function ContainsUnresolvedVariable(const value : string) : boolean;
begin
  //matches $name$ tokens used by Pack for variable substitution (e.g. $packageSource$,
  //$compilernoprefix$). For prepare-time scaffolding these stay literal, so any source
  //path containing them refers to a per-version artefact that doesn't yet exist and
  //isn't a valid DCCReference target anyway.
  result := TRegEx.IsMatch(value, '\$[A-Za-z_][A-Za-z0-9_]*\$');
end;

function ShouldIncludeSourceFile(const filePath : string) : boolean;
var
  ext : string;
begin
  //Whitelist of extensions that make sense as DCCReferences in a Delphi package
  //dproj. .dfm files are pulled in implicitly by the {FormName} reference on
  //their owning .pas unit (so they're deliberately excluded here). Everything
  //else - .txt READMEs, .md docs, .json, accidental .dproj/.dpk glob hits -
  //should not be referenced by the dproj at all.
  ext := LowerCase(ExtractFileExt(filePath));
  result := (ext = '.pas') or (ext = '.inc') or (ext = '.rc') or (ext = '.res');
end;

function ExtractFormNameFromDfm(const dfmPath : string) : string;
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

const
  cPackagesFolder = 'packages';

{ TPreparePackageFolders }

constructor TPreparePackageFolders.Create(const logger : ILogger; const specReader : IPackageSpecReader;
                                           const transformer : IProjectTransformer; const dpkTransformer : IDpkTransformer);
begin
  inherited Create;
  FLogger := logger;
  FSpecReader := specReader;
  FTransformer := transformer;
  FDpkTransformer := dpkTransformer;
end;

function TPreparePackageFolders.GatherSupportedCompilers(const spec : IPackageSpec) : TCompilerVersions;
var
  i : integer;
  tp : ISpecTargetPlatform;
  expanded : TCompilerVersions;
  c : TCompilerVersion;
begin
  result := [];
  if (spec = nil) or (spec.TargetPlatforms = nil) then
    exit;
  for i := 0 to spec.TargetPlatforms.Count - 1 do
  begin
    tp := spec.TargetPlatforms[i];
    expanded := ExpandedCompilersOf(tp);
    for c := Low(TCompilerVersion) to High(TCompilerVersion) do
      if c in expanded then
        Include(result, c);
  end;
end;

function TPreparePackageFolders.FindPairsInFolder(const folderPath : string) : IList<TPreparePair>;
var
  dprojFiles : TArray<string>;
  dpkPath : string;
  i : integer;
  baseName : string;
  pair : TPreparePair;
begin
  result := TCollections.CreateList<TPreparePair>;
  if not TDirectory.Exists(folderPath) then
    exit;

  dprojFiles := TArray<string>(TDirectory.GetFiles(folderPath, '*.dproj', TSearchOption.soTopDirectoryOnly));
  for i := 0 to High(dprojFiles) do
  begin
    baseName := TPath.GetFileNameWithoutExtension(dprojFiles[i]);
    dpkPath := TPath.Combine(folderPath, baseName + '.dpk');
    if FileExists(dpkPath) then
    begin
      pair.DpkFile := dpkPath;
      pair.DprojFile := dprojFiles[i];
      pair.BaseName := baseName;
      result.Add(pair);
    end;
  end;
end;

function TPreparePackageFolders.PickSourceFolder(const folderStates : IList<TPrepareFolderState>; out source : TPrepareFolderState) : boolean;
var
  i : integer;
  bestIdx : integer;
  bestTime : TDateTime;
  candidate : TPrepareFolderState;
begin
  result := false;
  bestIdx := -1;
  bestTime := 0;
  for i := 0 to folderStates.Count - 1 do
  begin
    candidate := folderStates[i];
    if candidate.Pairs.Count = 0 then
      continue;
    //higher mtime wins. ties broken by lower compiler ordinal (which is enumeration order).
    if (bestIdx = -1) or (candidate.NewestDprojTime > bestTime) then
    begin
      bestIdx := i;
      bestTime := candidate.NewestDprojTime;
    end;
  end;
  if bestIdx < 0 then
    exit;
  source := folderStates[bestIdx];
  result := true;
end;

function TPreparePackageFolders.LogPrefix : string;
begin
  if FDryRun then
    result := '[DRY RUN] '
  else
    result := '';
end;

procedure TPreparePackageFolders.EnsureFolders(const packagesRoot : string; const compilers : TCompilerVersions);
var
  c : TCompilerVersion;
  folderPath : string;
begin
  for c := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if not (c in compilers) then
      continue;
    if c = TCompilerVersion.UnknownVersion then
      continue;
    folderPath := TPath.Combine(packagesRoot, CompilerToRADStudioFolderName(c));
    if not TDirectory.Exists(folderPath) then
    begin
      if FDryRun then
        FLogger.Information(LogPrefix + 'Would create folder: ' + folderPath)
      else
      begin
        TDirectory.CreateDirectory(folderPath);
        FLogger.Information('Created folder: ' + folderPath);
      end;
    end;
  end;
end;

function TPreparePackageFolders.BuildVariables(const spec : IPackageSpec; const compiler : TCompilerVersion) : IDictionary<string, string>;
var
  pair : TPair<string, string>;
begin
  result := TCollections.CreateDictionary<string, string>;

  //compiler-derived variables - mirrors TPackageWriter.PopulateVariables so dspec
  //authors see consistent behaviour between Pack and Prepare.
  //TODO : this is code dupe, refactor
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
  result['bdsversion'] := CompilerToBDSVersion(compiler);

  //spec-declared variables overlay compiler-derived ones (allows authors to override
  //e.g. compilernoprefix if they really want to). Values may themselves contain
  //$name$ tokens - ResolveVariables iterates to a fixed point so chains like
  //$packageSource$ -> "Rad Studio $compilernoprefix$" -> "Rad Studio XE2" work.
  if (spec <> nil) and (spec.Variables <> nil) then
    for pair in spec.Variables do
      result[LowerCase(pair.Key)] := pair.Value;
end;

function TPreparePackageFolders.ResolveVariables(const value : string; const variables : IDictionary<string, string>) : string;
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

function TPreparePackageFolders.GatherSourceFiles(const specDir : string; const spec : IPackageSpec;
                                                   const compiler : TCompilerVersion) : IList<TPrepareSourceFile>;
var
  i, j : integer;
  template : ISpecTemplate;
  sourceEntry : ISpecSourceEntry;
  pattern : string;
  rawPattern : string;
  searchDir : string;
  mask : string;
  files : TArray<string>;
  k : integer;
  abs : string;
  dfmPath : string;
  seen : ISet<string>;
  variables : IDictionary<string, string>;
  entry : TPrepareSourceFile;
begin
  result := TCollections.CreateList<TPrepareSourceFile>;
  seen := TCollections.CreateSet<string>;
  if (spec = nil) or (spec.Templates = nil) then
    exit;

  variables := BuildVariables(spec, compiler);

  for i := 0 to spec.Templates.Count - 1 do
  begin
    template := spec.Templates[i];
    if (template = nil) or (template.SourceEntries = nil) then
      continue;
    for j := 0 to template.SourceEntries.Count - 1 do
    begin
      sourceEntry := template.SourceEntries[j];
      if sourceEntry = nil then
        continue;
      rawPattern := sourceEntry.Source;
      if Trim(rawPattern) = '' then
        continue;

      //resolve $name$ tokens against the spec + compiler variable set. Anything
      //unresolved after this is a genuine dspec authoring issue.
      pattern := ResolveVariables(rawPattern, variables);
      if ContainsUnresolvedVariable(pattern) then
      begin
        FLogger.Warning('Source entry has unresolvable variable - skipping: ' + rawPattern);
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
        searchDir := ExcludeTrailingPathDelimiter(TPath.GetFullPath(TPath.Combine(specDir, searchDir)));

      if not TDirectory.Exists(searchDir) then
      begin
        FLogger.Warning('Source path does not exist (skipping): ' + searchDir);
        continue;
      end;

      files := TArray<string>(TDirectory.GetFiles(searchDir, mask, TSearchOption.soTopDirectoryOnly));
      if rawPattern = pattern then
        FLogger.Information(Format('Source pattern %s matched %d file(s)', [rawPattern, Length(files)]))
      else
        FLogger.Information(Format('Source pattern %s (resolved: %s) matched %d file(s)', [rawPattern, pattern, Length(files)]));

      for k := 0 to High(files) do
      begin
        abs := TPath.GetFullPath(files[k]);
        //Skip extensions that shouldn't be in a dpk/dproj. .dfm files come in via
        //the {FormName} annotation on their owning .pas.
        if not ShouldIncludeSourceFile(abs) then
          continue;
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
          FLogger.Information('  ' + abs + ' (form: ' + entry.FormName + ')')
        else
          FLogger.Information('  ' + abs);
      end;
    end;
  end;
end;

function TPreparePackageFolders.MakeRelativePath(const fromDir, toPath : string) : string;
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

function RuntimeProjectName(const packageId : string) : string;
begin
  //Delphi convention: runtime packages end in 'R' (design-time end in 'D'). Don't
  //double-append if the dspec id already follows the convention.
  result := packageId;
  if (Length(result) > 0) and (result[Length(result)] <> 'R') and (result[Length(result)] <> 'r') then
    result := result + 'R';
end;

function TPreparePackageFolders.GatherScaffoldItems(const spec : IPackageSpec;
                                                    const variables : IDictionary<string, string>) : IList<TPrepareScaffoldItem>;

  function ProjectNameFromPath(const projectPath : string) : string;
  var
    normalized : string;
  begin
    //ExtractFileName on Windows only splits on '\'. dspec authors typically use '/',
    //so normalize before extracting the basename or the whole path leaks through.
    normalized := StringReplace(projectPath, '/', PathDelim, [rfReplaceAll]);
    result := ChangeFileExt(ExtractFileName(normalized), '');
  end;

var
  i, j : integer;
  template : ISpecTemplate;
  buildEntry : ISpecBuildEntry;
  designEntry : ISpecDesignEntry;
  projectPath : string;
  projectName : string;
  item : TPrepareScaffoldItem;
  seen : ISet<string>;
begin
  result := TCollections.CreateList<TPrepareScaffoldItem>;
  seen := TCollections.CreateSet<string>;
  if (spec = nil) or (spec.Templates = nil) then
    exit;

  for i := 0 to spec.Templates.Count - 1 do
  begin
    template := spec.Templates[i];
    if template = nil then
      continue;

    //runtime packages from build entries
    if template.BuildEntries <> nil then
    begin
      for j := 0 to template.BuildEntries.Count - 1 do
      begin
        buildEntry := template.BuildEntries[j];
        if buildEntry = nil then
          continue;
        projectPath := buildEntry.Project;
        if Trim(projectPath) = '' then
          continue;
        projectPath := ResolveVariables(projectPath, variables);
        projectName := ProjectNameFromPath(projectPath);
        if (projectName = '') or seen.Contains(LowerCase(projectName)) then
          continue;
        seen.Add(LowerCase(projectName));
        item.ProjectName := projectName;
        item.Kind := pkRuntime;
        //buildEntry.Platforms is empty when the dspec didn't declare an override.
        //ScaffoldInitialPair intersects with the targetPlatform's set; empty here
        //means "no override - use the targetPlatform's full set as-is".
        item.Platforms := buildEntry.Platforms;
        //Author-declared extra requires (e.g. vcl, fmx, sibling runtime packages).
        //Empty list when the dspec didn't declare any; the engine still appends
        //the auto-required runtime siblings for design entries.
        item.References := buildEntry.References;
        result.Add(item);
      end;
    end;

    //design packages from design entries
    if template.DesignEntries <> nil then
    begin
      for j := 0 to template.DesignEntries.Count - 1 do
      begin
        designEntry := template.DesignEntries[j];
        if designEntry = nil then
          continue;
        projectPath := designEntry.Project;
        if Trim(projectPath) = '' then
          continue;
        projectPath := ResolveVariables(projectPath, variables);
        projectName := ProjectNameFromPath(projectPath);
        if (projectName = '') or seen.Contains(LowerCase(projectName)) then
          continue;
        seen.Add(LowerCase(projectName));
        item.ProjectName := projectName;
        item.Kind := pkDesign;
        //designEntry.Platforms - same empty-means-no-override semantics as build.
        item.Platforms := designEntry.Platforms;
        item.References := designEntry.References;
        result.Add(item);
      end;
    end;
  end;
end;

procedure TPreparePackageFolders.ScaffoldOnePair(const folderPath, projectName : string;
                                                 const kind : TPrepareProjectKind;
                                                 const compiler : TCompilerVersion;
                                                 const relativeSources : IList<TPrepareSourceFile>;
                                                 const requiredPackages : IList<string>;
                                                 const platforms : TDPMPlatforms;
                                                 const force : boolean);
var
  dpkPath : string;
  dprojPath : string;
  kindLabel : string;
  platformsLabel : string;
begin
  dpkPath := TPath.Combine(folderPath, projectName + '.dpk');
  dprojPath := TPath.Combine(folderPath, projectName + '.dproj');
  if kind = pkDesign then
    kindLabel := 'design-only'
  else
    kindLabel := 'runtime-only';
  //Short summary of the platforms the dproj will be scaffolded for, surfaced in
  //the log so the user can see at a glance which entry-level overrides took
  //effect (especially during --dryrun).
  platformsLabel := DPMPlatformsToString(platforms, ', ');

  if FileExists(dpkPath) and not force then
    FLogger.Warning(LogPrefix + 'Scaffold target already exists, skipping: ' + dpkPath)
  else
  begin
    if FDryRun then
      FLogger.Information(LogPrefix + 'Would scaffold ' + kindLabel + ' package: ' + dpkPath + ' (' + IntToStr(relativeSources.Count) + ' source file(s))')
    else
    begin
      TFile.WriteAllText(dpkPath, TPrepareTemplates.RenderDpk(projectName, compiler, relativeSources, requiredPackages, kind), TEncoding.UTF8);
      FLogger.Information('Created (' + kindLabel + '): ' + dpkPath);
    end;
  end;

  if FileExists(dprojPath) and not force then
    FLogger.Warning(LogPrefix + 'Scaffold target already exists, skipping: ' + dprojPath)
  else
  begin
    if FDryRun then
      FLogger.Information(LogPrefix + 'Would scaffold ' + kindLabel + ' project: ' + dprojPath + ' (platforms: ' + platformsLabel + ')')
    else
    begin
      TFile.WriteAllText(dprojPath, TPrepareTemplates.RenderDproj(projectName, compiler, relativeSources, requiredPackages, kind, platforms), TEncoding.UTF8);
      FLogger.Information('Created (' + kindLabel + ', platforms: ' + platformsLabel + '): ' + dprojPath);
    end;
  end;
end;

function TPreparePackageFolders.ScaffoldInitialPair(const packagesRoot : string; const specDir : string;
                                                    const spec : IPackageSpec; const packageId : string;
                                                    const compiler : TCompilerVersion; const force : boolean) : boolean;

  function ContainsIgnoreCase(const list : IList<string>; const value : string) : boolean;
  var
    k : integer;
  begin
    result := false;
    for k := 0 to list.Count - 1 do
      if SameText(list[k], value) then
      begin
        result := true;
        exit;
      end;
  end;

var
  folderName : string;
  folderPath : string;
  absoluteSources : IList<TPrepareSourceFile>;
  relativeSources : IList<TPrepareSourceFile>;
  i, j : integer;
  variables : IDictionary<string, string>;
  items : IList<TPrepareScaffoldItem>;
  fallback : TPrepareScaffoldItem;
  compilerPlatforms : TDPMPlatforms;
  itemPlatforms : TDPMPlatforms;
  relEntry : TPrepareSourceFile;
  runtimePackageNames : IList<string>;
  itemRequires : IList<string>;
  refName : string;
begin
  folderName := CompilerToRADStudioFolderName(compiler);
  folderPath := TPath.Combine(packagesRoot, folderName);
  //EnsureFolders has already announced (or created) this folder above. Just make
  //sure it exists on disk when we're really executing; suppress the duplicate
  //dry-run log entry.
  if not FDryRun then
    TDirectory.CreateDirectory(folderPath);

  //expand globs from spec.Templates and compute relative paths from the new folder.
  //pass the target compiler so variables like $compilernoprefix$ resolve correctly.
  absoluteSources := GatherSourceFiles(specDir, spec, compiler);
  relativeSources := TCollections.CreateList<TPrepareSourceFile>;
  for i := 0 to absoluteSources.Count - 1 do
  begin
    relEntry.Path := MakeRelativePath(folderPath, absoluteSources[i].Path);
    relEntry.FormName := absoluteSources[i].FormName;
    relativeSources.Add(relEntry);
  end;

  //Resolve the platforms the dspec declares for this compiler. PlatformsForCompiler
  //returns the empty set when the compiler isn't matched (shouldn't happen because
  //GatherSupportedCompilers filters by membership) - fall back to Win32 with a warning.
  compilerPlatforms := PlatformsForCompiler(spec, compiler);
  if compilerPlatforms = [] then
  begin
    FLogger.Warning(Format('Spec declares no platforms for %s - falling back to Win32 only.',
                           [CompilerToRADStudioFolderName(compiler)]));
    compilerPlatforms := [TDPMPlatform.Win32];
  end;

  //Collect every project the dspec wants scaffolded (one per build entry as runtime,
  //one per design entry as design-only). If the dspec declares none, fall back to a
  //single runtime package named <packageId>R so an unconfigured dspec still produces
  //something usable.
  variables := BuildVariables(spec, compiler);
  items := GatherScaffoldItems(spec, variables);

  if items.Count = 0 then
  begin
    fallback.ProjectName := RuntimeProjectName(packageId);
    fallback.Kind := pkRuntime;
    fallback.Platforms := []; //no override - inherits compilerPlatforms below.
    fallback.References := nil; //no dspec, no author references.
    items.Add(fallback);
  end;

  //Collect every runtime project name in this set. A design package in the same
  //dspec naturally wraps the runtime one(s) - we wire that up automatically by
  //requiring the runtime bpls from the design's dpk + dproj. Runtime packages
  //don't require their siblings (they'd cycle if they did).
  runtimePackageNames := TCollections.CreateList<string>;
  for i := 0 to items.Count - 1 do
    if items[i].Kind = pkRuntime then
      runtimePackageNames.Add(items[i].ProjectName);

  for i := 0 to items.Count - 1 do
  begin
    //Build the per-item requires list. Design entries auto-require the runtime
    //siblings declared elsewhere in the same dspec (existing behaviour). On top
    //of that, append any author-declared references (vcl, fmx, extra sibling
    //packages, etc.) - deduped case-insensitively against what's already there.
    //For runtime entries the auto-require step is skipped (runtimes don't depend
    //on each other by default), so references are the only source of extras.
    itemRequires := TCollections.CreateList<string>;
    if items[i].Kind = pkDesign then
      for j := 0 to runtimePackageNames.Count - 1 do
        itemRequires.Add(runtimePackageNames[j]);
    if items[i].References <> nil then
      for j := 0 to items[i].References.Count - 1 do
      begin
        refName := Trim(items[i].References[j]);
        if (refName <> '') and not ContainsIgnoreCase(itemRequires, refName) then
          itemRequires.Add(refName);
      end;

    //Per-item effective platforms = entry override intersected with the
    //targetPlatform's declared platforms. Empty override means "use the
    //targetPlatform's full set as-is". An empty intersection means the entry
    //declared only platforms the targetPlatform doesn't support - skip + warn
    //rather than emit a dproj with no <Platforms>.
    if items[i].Platforms <> [] then
      itemPlatforms := compilerPlatforms * items[i].Platforms
    else
      itemPlatforms := compilerPlatforms;

    if itemPlatforms = [] then
    begin
      FLogger.Warning(Format('Item %s in %s has no platforms after intersecting the entry override with the targetPlatform - skipping.',
                             [items[i].ProjectName, CompilerToRADStudioFolderName(compiler)]));
      continue;
    end;

    ScaffoldOnePair(folderPath, items[i].ProjectName, items[i].Kind, compiler,
                    relativeSources, itemRequires, itemPlatforms, force);
  end;

  result := true;
end;

function TPreparePackageFolders.PropagatePair(const targetCompiler : TCompilerVersion;
                                              const targetFolder : string;
                                              const pair : TPreparePair; const force : boolean) : boolean;
var
  targetDpk : string;
  targetDproj : string;
  writeDpk : boolean;
  writeDproj : boolean;
begin
  result := false;
  targetDpk := TPath.Combine(targetFolder, pair.BaseName + '.dpk');
  targetDproj := TPath.Combine(targetFolder, pair.BaseName + '.dproj');

  writeDpk := force or not FileExists(targetDpk);
  writeDproj := force or not FileExists(targetDproj);

  if not writeDpk then
    FLogger.Information(LogPrefix + 'Skipping (exists): ' + targetDpk)
  else if FDryRun then
    FLogger.Information(LogPrefix + 'Would rewrite LIBSUFFIX and write: ' + targetDpk)
  else
  begin
    try
      FDpkTransformer.RewriteLibSuffix(pair.DpkFile, targetDpk, targetCompiler);
      FLogger.Information('Wrote: ' + targetDpk);
    except
      on e : Exception do
      begin
        FLogger.Error('Failed to write ' + targetDpk + ' : ' + e.Message);
        exit;
      end;
    end;
  end;

  if not writeDproj then
    FLogger.Information(LogPrefix + 'Skipping (exists): ' + targetDproj)
  else if FDryRun then
    FLogger.Information(LogPrefix + 'Would transform dproj for ' + CompilerToRADStudioFolderName(targetCompiler) + ' and write: ' + targetDproj)
  else
  begin
    try
      if not FTransformer.LoadFromFile(pair.DprojFile) then
      begin
        FLogger.Error('Failed to load source dproj: ' + pair.DprojFile);
        exit;
      end;
      FTransformer.ApplyForCompiler(targetCompiler);
      if not FTransformer.SaveToFile(targetDproj) then
      begin
        FLogger.Error('Failed to save target dproj: ' + targetDproj);
        exit;
      end;
      FLogger.Information('Wrote: ' + targetDproj);
    except
      on e : Exception do
      begin
        FLogger.Error('Failed to transform ' + pair.DprojFile + ' -> ' + targetDproj + ' : ' + e.Message);
        exit;
      end;
    end;
  end;

  result := true;
end;

function TPreparePackageFolders.Execute(const specFile : string; const force : boolean; const dryRun : boolean;
                                         const cancellationToken : ICancellationToken) : boolean;
var
  spec : IPackageSpec;
  specDir : string;
  packagesRoot : string;
  packageId : string;
  compilers : TCompilerVersions;
  c : TCompilerVersion;
  folderStates : IList<TPrepareFolderState>;
  state : TPrepareFolderState;
  source : TPrepareFolderState;
  hasSource : boolean;
  pair : TPreparePair;
  i : integer;
  pairMTime : TDateTime;
begin
  result := false;
  FDryRun := dryRun;

  if FDryRun then
    FLogger.Information('[DRY RUN] No files will be created or modified.');

  if not FileExists(specFile) then
  begin
    FLogger.Error('Spec file does not exist: ' + specFile);
    exit;
  end;

  spec := FSpecReader.ReadSpec(specFile);
  if (spec = nil) or not spec.IsValid then
  begin
    FLogger.Error('Failed to read spec file: ' + specFile);
    exit;
  end;

  if (spec.MetaData = nil) or (Trim(spec.MetaData.Id) = '') then
  begin
    FLogger.Error('Spec is missing required metadata.id');
    exit;
  end;
  packageId := spec.MetaData.Id;

  compilers := GatherSupportedCompilers(spec);
  if compilers = [] then
  begin
    FLogger.Error('Spec does not declare any supported compilers in targetPlatforms.');
    exit;
  end;

  specDir := ExtractFilePath(specFile);
  if specDir = '' then
    specDir := GetCurrentDir;
  packagesRoot := TPath.Combine(specDir, cPackagesFolder);
  if not TDirectory.Exists(packagesRoot) then
  begin
    if FDryRun then
      FLogger.Information(LogPrefix + 'Would create packages root: ' + packagesRoot)
    else
    begin
      TDirectory.CreateDirectory(packagesRoot);
      FLogger.Information('Created packages root: ' + packagesRoot);
    end;
  end;

  EnsureFolders(packagesRoot, compilers);

  if cancellationToken.IsCancelled then
    exit;

  //gather state for every supported compiler folder.
  folderStates := TCollections.CreateList<TPrepareFolderState>;
  for c := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if not (c in compilers) then
      continue;
    if c = TCompilerVersion.UnknownVersion then
      continue;
    state.Compiler := c;
    state.FolderPath := TPath.Combine(packagesRoot, CompilerToRADStudioFolderName(c));
    state.Pairs := FindPairsInFolder(state.FolderPath);
    state.NewestDprojTime := 0;
    for i := 0 to state.Pairs.Count - 1 do
    begin
      pair := state.Pairs[i];
      pairMTime := TFile.GetLastWriteTime(pair.DprojFile);
      if pairMTime > state.NewestDprojTime then
        state.NewestDprojTime := pairMTime;
    end;
    folderStates.Add(state);
  end;

  hasSource := PickSourceFolder(folderStates, source);

  if not hasSource then
  begin
    //One-shot: scaffold each supported compiler directly from the template. Each
    //compiler gets a fresh dpk/dproj with the correct ProjectVersion, libsuffix,
    //DllSuffix, DPMCompiler etc. for its version - no propagation step needed.
    for c := Low(TCompilerVersion) to High(TCompilerVersion) do
    begin
      if not (c in compilers) then
        continue;
      if c = TCompilerVersion.UnknownVersion then
        continue;
      if cancellationToken.IsCancelled then
        exit;
      if not ScaffoldInitialPair(packagesRoot, specDir, spec, packageId, c, force) then
        exit;
    end;
    FLogger.Information('');
    FLogger.Information('Scaffolded packages for all supported Delphi versions.');
    FLogger.Information('Review the generated dpk/dproj files and adjust platforms / sources as needed.');
    result := true;
    exit;
  end;

  FLogger.Information(Format('Source folder: %s (%d pair(s))', [source.FolderPath, source.Pairs.Count]));

  //propagate each pair in the source set to every target compiler folder (including
  //the source folder itself for in-place transforms).
  for c := Low(TCompilerVersion) to High(TCompilerVersion) do
  begin
    if not (c in compilers) then
      continue;
    if c = TCompilerVersion.UnknownVersion then
      continue;
    if cancellationToken.IsCancelled then
      exit;

    for i := 0 to source.Pairs.Count - 1 do
    begin
      pair := source.Pairs[i];
      if not PropagatePair(c,
                           TPath.Combine(packagesRoot, CompilerToRADStudioFolderName(c)),
                           pair, force) then
      begin
        FLogger.Error('Aborting due to earlier error.');
        exit;
      end;
    end;
  end;

  result := true;
end;

end.
