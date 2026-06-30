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


unit DPM.Core.Packaging.Writer;

interface

uses
  System.Classes,
  System.RegularExpressions,
  Spring.Container,
  Spring.Collections,
  VSoft.AntPatterns,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Pack,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.TargetPlatform,
  DPM.Core.Packaging,
  DPM.Core.Packaging.Archive,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Archive,
  DPM.Core.Utils.Masks;

/// This only processes specs for the Pack command. For git style packages we will need to do something else.
type
  TPackageWriter = class(TInterfacedObject, IPackageWriter)
  private
    FLogger : ILogger;
    FArchiveWriter : IPackageArchiveWriter;
    FSpecReader : IPackageSpecReader;
    FManifestService : IManifestService;
    FArchiveValidator : IArchiveValidator;

    FCurrentTokens : TStringList;

    FVariables : IVariables;
    FTokenRegEx : TRegEx;

    //Lowercase archive paths added during source processing. Used to verify that every
    //build/design entry project will be present in the package, so we catch missing source
    //patterns at pack time rather than at install time on every consumer's machine.
    FArchivePaths : IList<string>;

    //The package's internal root. Normally the dspec's folder, but when the dspec lives in a
    //sub-folder and references content above it via '..\', this is the deepest common ancestor of
    //every referenced path so the archive mirrors the on-disk layout without any '..' segments.
    FEffectiveBasePath : string;

    //Canonical (forward-slash, NFC, exact-case) archive paths of every PE binary (.bpl/.dll/.exe)
    //added to the package. Auto-derived during source processing and emitted into the package's
    //dspec as 'precompiledBinaries' so the gallery can cross-check shipped binaries against the
    //author's declaration.
    FPrecompiledBinaries : IList<string>;

  protected

    /// <summary> Normalises a pack-time archive path (which uses backslashes) to the canonical form
    /// used inside the .dpkg and its manifest: forward slashes + NFC, preserving case. </summary>
    function CanonicalArchivePath(const archivePath : string) : string;

    procedure ProcessPattern(const basePath, dest : string; const pattern : IFileSystemPattern; const excludeMatcher : IFileMatcher; var fileCount : integer);
    procedure ProcessEntry(const basePath : string; const antPattern : IAntPattern; const source, dest : string; const exclude : IList<string>);

    /// <summary> Determines the package's internal root: the deepest folder that is a common ancestor
    /// of the dspec folder and every path the spec references (source globs, readme, icon, build/design/
    /// packagedef projects). Archive paths are then computed relative to this so '..\' references resolve
    /// to clean, root-relative entries. </summary>
    function ComputeEffectiveBasePath(const basePath : string; const spec : IPackageSpec; const template : ISpecTemplate) : string;

    /// <summary> Resolves an authored (possibly '..\'-relative) path against basePath and expresses it
    /// relative to FEffectiveBasePath - the archive-relative form. Returns forward-slash separators. </summary>
    function ToEffectiveRelative(const basePath, value : string) : string;

    /// <summary> Rewrites a source entry's glob into the archive-relative form its files were packed to,
    /// so install can re-expand it against the extracted package. Mirrors ProcessEntry's placement:
    /// no dest -> files mirror the source tree under the effective root (use ToEffectiveRelative);
    /// explicit dest -> files sit under dest keeping their position relative to the glob's base folder
    /// (use dest + the glob's wildcard tail). Returns forward-slash separators. </summary>
    function BuildCopyToLibGlob(const basePath, source, dest : string) : string;

    procedure ValidateBuildEntries(const template : ISpecTemplate; const antPattern : IAntPattern);

    /// <summary> Rejects any package that declares a reserved/banned IDE environment variable name. </summary>
    function ValidateEnvironmentVariables(const spec : IPackageSpec) : boolean;

    procedure PopulateVariables(const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const externalVariables : TStringList);

    function ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const variables: TStringList) : boolean;
    function TokenMatchEvaluator(const match : TMatch) : string;
    function EnvVarTokenMatchEvaluator(const match : TMatch) : string;



    /// <summary>
    /// Writes a Package per compiler version.
    /// </summary>
    function InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion;
                                  const basePath : string; const variables : TStringList) : boolean;

    function WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
  public
    constructor Create(const logger : ILogger;
                       const archiveWriter : IPackageArchiveWriter;
                       const specReader : IPackageSpecReader;
                       const manifestService : IManifestService;
                       const archiveValidator : IArchiveValidator);
  end;



implementation

uses
  System.Diagnostics,
  System.SysUtils,
  System.StrUtils,
  System.Types,
  System.IOUtils,
  System.Masks,
  DPM.Core.Constants,
  DPM.Core.Packaging.EnvironmentVariableValidator,
  DPM.Core.Spec,
  DPM.Core.Utils.PE,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path;

{ TPackageWriter }

constructor TPackageWriter.Create(const logger : ILogger;
                                   const archiveWriter : IPackageArchiveWriter;
                                   const specReader : IPackageSpecReader;
                                   const manifestService : IManifestService;
                                   const archiveValidator : IArchiveValidator);
begin
  FLogger := logger;
  FArchiveWriter := archiveWriter;
  FSpecReader := specReader;
  FManifestService := manifestService;
  FArchiveValidator := archiveValidator;
  FCurrentTokens := nil;
  FTokenRegEx := TRegEx.Create('\$(\w+)\$');
end;


function TPackageWriter.CanonicalArchivePath(const archivePath : string) : string;
begin
  result := StringReplace(archivePath, '\', '/', [rfReplaceAll]);
  //match the manifest's canonicalisation so precompiledBinaries paths are byte-identical to the
  //file entries in dpm-manifest.json. NormalizeToNfc is a no-op on pure ASCII paths.
  if FManifestService <> nil then
    result := FManifestService.NormalizeToNfc(result);
end;

function ApplyBase(const basePath : string; const value : string) : string;
begin
  result := value;
  if TStringUtils.StartsWith(result, '.\') then
  begin
    Delete(result, 1, 1);
    result := TPath.Combine(basePath, result);
  end;
end;

function StripCurrent(const value : string) : string;
begin
  result := value;
  if TStringUtils.StartsWith(result, '.\') then
    Delete(result, 1, 2);
end;

//Longest common directory prefix (by path segment, case-insensitive) of a set of absolute
//directory paths. Returns '' when there is no common prefix (eg different drives). Used to find
//the package's effective root when paths reach above the dspec folder via '..\'.
function LongestCommonDirPath(const dirs : TArray<string>) : string;
var
  i, j : integer;
  refSegs : TArray<string>;
  curSegs : TArray<string>;
  commonLen : integer;

  function SegsOf(const path : string) : TArray<string>;
  begin
    //XE2 treats TStringDynArray (SplitString's result) and TArray<string> as distinct - hard-cast.
    result := TArray<string>(SplitString(StringReplace(ExcludeTrailingPathDelimiter(path), '/', '\', [rfReplaceAll]), '\'));
  end;

begin
  result := '';
  if Length(dirs) = 0 then
    exit;

  refSegs := SegsOf(dirs[0]);
  commonLen := Length(refSegs);
  for i := 1 to High(dirs) do
  begin
    curSegs := SegsOf(dirs[i]);
    if Length(curSegs) < commonLen then
      commonLen := Length(curSegs);
    j := 0;
    while (j < commonLen) and SameText(refSegs[j], curSegs[j]) do
      Inc(j);
    commonLen := j;
    if commonLen = 0 then
      break;
  end;

  for j := 0 to commonLen - 1 do
    if result = '' then
      result := refSegs[j]
    else
      result := result + PathDelim + refSegs[j];

  if result <> '' then
    result := IncludeTrailingPathDelimiter(result);
end;


function GetNonWildcardPath(const value : string) : string;
var
  i : integer;
begin
  result := '';
  i := Pos('*', value);
  if i > 0 then
    result := Copy(value, 1, i - 1);
end;

procedure TPackageWriter.ProcessPattern(const basePath : string; const dest : string; const pattern : IFileSystemPattern; const excludeMatcher : IFileMatcher; var fileCount : integer);
var
  files : TStringDynArray;
  f : string;
  archivePath : string;

  function IsFileExcluded(const fileName : string) : boolean;
  begin
    result := excludeMatcher.Matches(fileName);
  end;
begin
  if not TDirectory.Exists(pattern.Directory) then
      raise Exception.Create('Directory not found : ' + pattern.Directory);
  files := TDirectory.GetFiles(pattern.Directory, pattern.FileMask, TSearchOption.soTopDirectoryOnly);
  for f in files do
  begin
    if IsFileExcluded(f) then
      continue;

    if not TFile.Exists(f) then
      raise Exception.Create('File not found : ' + f);
    archivePath := dest + '\' + TPathUtils.StripBase(basePath, f);
    if TStringUtils.StartsWith(archivePath, '\') then
      Delete(archivePath, 1, 1);
    Inc(fileCount);
    // FLogger.Debug('Writing file [' + archivePath + '] to package.');
  //  FLogger.Debug('Adding file : ' + archivePath);

    FArchiveWriter.AddFile(f, archivePath);
    if FArchivePaths <> nil then
      FArchivePaths.Add(LowerCase(archivePath));

    //Record any real PE binary so it can be declared in the package's dspec. We sniff the on-disk
    //file by content (not extension) so a renamed binary is still caught. The path stored here must
    //match the package.dspec.yaml/manifest exactly: forward slashes, NFC, original case.
    if (FPrecompiledBinaries <> nil) and TPEUtils.IsPE(f) then
      FPrecompiledBinaries.Add(CanonicalArchivePath(archivePath));
  end;
end;


function TPackageWriter.ValidateEnvironmentVariables(const spec : IPackageSpec) : boolean;
var
  template : ISpecTemplate;
  pair : TPair<string,string>;
  reason : string;
begin
  result := true;
  for template in spec.Templates do
  begin
    for pair in template.EnvironmentVariables do
    begin
      if not TEnvironmentVariableValidator.IsAllowed(pair.Key, reason) then
      begin
        FLogger.Error('Template [' + template.Name + '] : ' + reason);
        result := false;
      end;
    end;
  end;
end;

function TPackageWriter.ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const variables: TStringList): boolean;
var
  evaluator : TMatchEvaluator;
  envVarEvaluator : TMatchEvaluator;
  regEx : TRegEx;
  i, j : integer;
  template : ISpecTemplate;
  source : ISpecSourceEntry;
  build  : ISpecBuildEntry;
  design : ISpecDesignEntry;
  envVarKeys : TArray<string>;
begin
  result := true;
  FVariables := TCollections.CreateDictionary<string,string>;
  regEx := FTokenRegEx;
  evaluator := TokenMatchEvaluator; //work around for compiler overload resolution issue.
  try
    try
      PopulateVariables(spec, targetPlatform, version, variables);

      spec.MetaData.Id := regEx.Replace(spec.MetaData.Id, evaluator);
      spec.MetaData.Description := regEx.Replace(spec.MetaData.Description, evaluator);
      for i := 0 to spec.MetaData.Authors.Count -1 do
        spec.MetaData.Authors[i] := regEx.Replace(spec.MetaData.Authors[i], evaluator);

      if spec.MetaData.ProjectUrl <> '' then
        spec.MetaData.ProjectUrl := regEx.Replace(spec.MetaData.ProjectUrl, evaluator);

      if spec.MetaData.RepositoryUrl <> '' then
          spec.MetaData.RepositoryUrl := regEx.Replace(spec.MetaData.RepositoryUrl, evaluator);

      if spec.MetaData.RepositoryType <> '' then
        spec.MetaData.RepositoryType := regEx.Replace(spec.MetaData.RepositoryType, evaluator);

      if spec.MetaData.RepositoryBranch <> '' then
        spec.MetaData.RepositoryBranch := regEx.Replace(spec.MetaData.RepositoryBranch, evaluator);

      if spec.MetaData.RepositoryCommit <> '' then
        spec.MetaData.RepositoryCommit := regEx.Replace(spec.MetaData.RepositoryCommit, evaluator);

      spec.MetaData.License := regEx.Replace(spec.MetaData.License, evaluator);

      if spec.MetaData.Icon <> '' then
        spec.MetaData.Icon := regEx.Replace(spec.MetaData.Icon, evaluator);

      if spec.MetaData.Copyright <> '' then
        spec.MetaData.Copyright := regEx.Replace(spec.MetaData.Copyright, evaluator);

      if spec.MetaData.Tags.count > 0 then
      begin
        for i := 0 to spec.MetaData.Tags.Count -1 do
          spec.MetaData.Tags[i] := regEx.Replace(spec.MetaData.Tags[i], evaluator);
      end;

      template := spec.FindTemplate(targetPlatform.TemplateName);
      if template = nil then
        raise Exception.Create('Template not found : ' + targetPlatform.TemplateName);

      for i := 0 to template.SourceEntries.Count -1 do
      begin
        source := template.SourceEntries[i];
        source.Source := regEx.Replace(source.Source, evaluator);
        if source.Destination <> '' then
          source.Destination := regEx.Replace(source.Destination, evaluator);
        if source.Exclude.Count > 0 then
        begin
          for j := 0 to source.Exclude.Count -1 do
            source.Exclude[j] := regEx.Replace(source.Exclude[j], evaluator);
        end;
      end;

      for i := 0 to template.BuildEntries.Count -1 do
      begin
        build := template.BuildEntries[i];
        //Trim stray whitespace - a trailing space on the project path (easy to leave in a
        //dspec) would otherwise propagate into the manifest and break MSBuild at install time.
        build.Project := Trim(regEx.Replace(build.Project, evaluator));
        if build.Defines <> '' then
          build.Defines := regEx.Replace(build.Defines, evaluator);
      end;

      for i := 0 to template.DesignEntries.Count -1 do
      begin
        design := template.DesignEntries[i];
        design.Project := Trim(regEx.Replace(design.Project, evaluator));
        if design.Defines <> '' then
          design.Defines := regEx.Replace(design.Defines, evaluator);

        if design.LibSuffix <> '' then
          design.LibSuffix := regEx.Replace(design.LibSuffix, evaluator);

        if design.LibPrefix <> '' then
          design.LibPrefix := regEx.Replace(design.LibPrefix, evaluator);

        if design.LibVersion <> '' then
          design.LibVersion := regEx.Replace(design.LibVersion, evaluator);
      end;

      //resolve any dependency authored with the $version$ token to this package's own version.
      for i := 0 to template.Dependencies.Count -1 do
        template.Dependencies[i].ResolveVersionToken(version);

      //environment variable values are the ONLY place install-time processing happens. They get the
      //normal pack-time tokens (spec/targetPlatform variables + built-in compiler tokens) via a
      //dedicated evaluator that additionally preserves the install-time $packageDir$ token for the
      //IDE to resolve when the package loads. $packageDir$ is NOT valid anywhere else - in any other
      //field the normal evaluator raises 'Unknown token'. Keys (names) are never tokenized.
      envVarEvaluator := EnvVarTokenMatchEvaluator; //work around for compiler overload resolution issue.
      envVarKeys := template.EnvironmentVariables.Keys.ToArray;
      for i := 0 to High(envVarKeys) do
        template.EnvironmentVariables[envVarKeys[i]] := regEx.Replace(template.EnvironmentVariables[envVarKeys[i]], envVarEvaluator);

      //TODO : Add packagedefs

    except
      on e : Exception do
      begin
        FLogger.Error('Error replacing variables : ' + e.Message);
        result := false;

      end;

    end;


  finally
    FVariables := nil;
  end;
end;

function TPackageWriter.TokenMatchEvaluator(const match: TMatch): string;
var
  key : string;
begin
  if match.Success and (match.Groups.Count = 2) then
  begin
    key := LowerCase(match.Groups.Item[1].Value);
    if FVariables.ContainsKey(key) then
      result := FVariables[key]
    else
      raise Exception.Create('Unknown token [' + match.Groups.Item[1].Value + ']');
  end
  else
    result := match.Value;
end;

function TPackageWriter.EnvVarTokenMatchEvaluator(const match: TMatch): string;
var
  key : string;
begin
  //Used only for environmentVariables values. Behaves like TokenMatchEvaluator but additionally
  //preserves two install-time tokens, left intact in the manifest for the IDE installer to resolve
  //on the consumer machine:
  //  $packageDir$ - the package's cache folder.
  //  $platform$   - the IDE host platform (Win32/Win64). NOT expanded to the build platform here,
  //                 since the value takes effect in the IDE process, whose bitness is unknown at
  //                 pack time. (Note 'platform' is deliberately not a pack-time variable - see
  //                 PopulateVariables - so without this it would raise 'Unknown token'.)
  //Both are meaningful ONLY in environment variable values - everywhere else they are unknown tokens.
  if match.Success and (match.Groups.Count = 2) then
  begin
    key := LowerCase(match.Groups.Item[1].Value);
    if key = 'packagedir' then
      result := '$packageDir$'
    else if key = 'platform' then
      result := '$platform$'
    else if FVariables.ContainsKey(key) then
      result := FVariables[key]
    else
      raise Exception.Create('Unknown token [' + match.Groups.Item[1].Value + ']');
  end
  else
    result := match.Value;
end;

procedure TPackageWriter.PopulateVariables(const spec: IPackageSpec; const targetPlatform: ISpecTargetPlatform; const version: TPackageVersion; const externalVariables: TStringList);
var
  pair : TPair<string,string>;
  regex : TRegex;
  evaluator : TMatchEvaluator;
  i : integer;
  replacedVariables : IVariables;
begin
  for pair in spec.Variables do
    FVariables[LowerCase(pair.Key)] := pair.Value;

  //apply targetPlatform overrides;
  for pair in targetPlatform.Variables do
    FVariables[LowerCase(pair.Key)] := pair.Value;

  //we don't want to write out variables into the manifest
  //this is safe because we are working with a clone of the spec
  spec.Variables.Clear;
  targetPlatform.Variables.Clear;

  if not version.IsEmpty then
    FVariables['version'] := version.ToString
  else
    FVariables['version'] := spec.MetaData.Version.ToString;
  FVariables['target'] := CompilerToString(targetPlatform.Compiler);
  FVariables['compiler'] := CompilerToString(targetPlatform.Compiler);
  FVariables['compilernoprefix'] := CompilerNoPrefix(targetPlatform.Compiler);
  FVariables['compilernopoint'] := CompilerToStringNoPoint(targetPlatform.Compiler);
  FVariables['compilershortversion'] := CompilerToShortVersion(targetPlatform.Compiler);
  FVariables['compilercodename'] := CompilerCodeName(targetPlatform.Compiler);
  FVariables['compilerwithcodename'] := CompilerWithCodeName(targetPlatform.Compiler);
//  FVariables['compilerwithcodename'] := CompilerNoPrefix(targetPlatform.Compiler);
  FVariables['compilermajornoprefix'] := CompilerMajorNoPrefix(targetPlatform.Compiler);

  //do not replace plaform here - we'll do that during install
//  list.Add('platform=' + DPMPlatformToString(targetPlatform.Platforms[0]));
  FVariables['compilerversion'] := CompilerToCompilerVersionIntStr(targetPlatform.Compiler);
  FVariables['libsuffix'] := CompilerToLibSuffix(targetPlatform.Compiler);
  FVariables['libsuffixshort'] := CompilerToLibSuffixShort(targetPlatform.Compiler);
  FVariables['bdsversion'] := CompilerToBDSVersion(targetPlatform.Compiler);
//  FVariables['bitness'] := DPMPlatformBitness(targetPlatform.Platforms[0]);


  //apply external props passed in on command line.
  if externalVariables.Count > 0 then
  begin
    for i := 0 to externalVariables.Count -1 do
      FVariables[LowerCase(externalVariables.Names[i])] := externalVariables.ValueFromIndex[i];
  end;

  regEx := FTokenRegEx;
  evaluator := TokenMatchEvaluator;

  replacedVariables := TCollections.CreateDictionary<string, string>;

  //variables from the spec and external may reference existing variables.
  for pair in FVariables do
  begin
    if TStringUtils.Contains(pair.Value, '$') then
      replacedVariables[pair.Key] := Trim(regEx.Replace(pair.Value, evaluator))
    else
      replacedVariables[pair.Key] := Trim(pair.Value);
  end;

  FVariables.Clear;
  FVariables.AddRange(replacedVariables);

end;

procedure TPackageWriter.ProcessEntry(const basePath : string; const antPattern : IAntPattern; const source, dest : string; const exclude : IList<string>);
var
  fsPatterns : TArray<IFileSystemPattern>;
  fsPattern : IFileSystemPattern;
  searchBasePath : string;
  stripBasePath : string;
  fileCount : integer;
  actualDest : string;
  actualSource : string;
  i, j : integer;
  newPatterns : TArray<IFileSystemPattern>;
  excludeAntPattern : IAntPattern;
  fileMatcher : IFileMatcher;

  procedure ValidateDestinationPath(const source, dest : string);
  begin
    if (pos(#10, dest) > 0) or (pos(#13, dest) > 0) then
      raise Exception.Create('Entry [' + source + '] has invalid characters in destination [' + dest + ']');
  end;


begin
  ValidateDestinationPath(source, dest);

  actualSource := StripCurrent(source);

  searchBasePath := TPathUtils.StripWildCard(TPathUtils.CompressRelativePath(basePath, actualSource));
  searchBasePath := ExtractFilePath(searchBasePath);

  if dest = '' then
  begin
    //No explicit destination: mirror the source tree under the package's effective root, so a
    //'..\Sources\Core\Foo.pas' lands at 'Sources\Core\Foo.pas' rather than escaping with '..'.
    //Strip relative to the effective root and let dest be empty.
    stripBasePath := IncludeTrailingPathDelimiter(FEffectiveBasePath);
    actualDest := '';
  end
  else
  begin
    //Explicit destination: files keep their position relative to the glob's base folder, placed
    //under dest. This is the long-standing behaviour for authored dest values.
    stripBasePath := searchBasePath;
    actualDest := StripCurrent(dest);
    actualDest := ExcludeTrailingPathDelimiter(actualDest);
  end;

  fsPatterns := antPattern.Expand(actualSource);
  fileCount := 0;

  fileMatcher := TFileMatcher.Create;

  excludeAntPattern := TAntPattern.Create(basePath);
  for i := 0 to exclude.Count -1 do
  begin
    newPatterns := excludeAntPattern.Expand(ApplyBase(basePath, exclude[i]));
    for j := 0 to High(newPatterns) do
      fileMatcher.AddMask(newPatterns[j].Directory + newPatterns[j].FileMask);
  end;

  for fsPattern in fsPatterns do
    ProcessPattern(stripBasePath, actualDest, fsPattern, fileMatcher, fileCount);

  if fileCount = 0 then
    FLogger.Warning('No files were found for pattern [' + source + ']');
end;


function TPackageWriter.ComputeEffectiveBasePath(const basePath : string; const spec : IPackageSpec; const template : ISpecTemplate) : string;
var
  dirs : IList<string>;
  sourceEntry : ISpecSourceEntry;
  buildEntry : ISpecBuildEntry;
  designEntry : ISpecDesignEntry;
  packageDef : ISpecPackageDefinition;
  fileGlob : string;

  procedure AddPathDir(const value : string; const isGlob : boolean);
  var
    resolved : string;
  begin
    if Trim(value) = '' then
      exit;
    resolved := TPathUtils.CompressRelativePath(basePath, StringReplace(Trim(value), '/', '\', [rfReplaceAll]));
    if isGlob then
      resolved := TPathUtils.StripWildCard(resolved);
    dirs.Add(ExtractFilePath(resolved));
  end;

begin
  dirs := TCollections.CreateList<string>;
  //Always include the dspec folder so the effective root never descends below it.
  dirs.Add(IncludeTrailingPathDelimiter(basePath));

  AddPathDir(spec.MetaData.Icon, false);
  AddPathDir(spec.MetaData.ReadMe, false);

  for sourceEntry in template.SourceEntries do
    AddPathDir(sourceEntry.Source, true);

  for buildEntry in template.BuildEntries do
    AddPathDir(buildEntry.Project, false);

  for designEntry in template.DesignEntries do
    AddPathDir(designEntry.Project, false);

  for packageDef in template.PackageDefinitions do
  begin
    AddPathDir(packageDef.Project, false);
    for fileGlob in packageDef.Files do
      AddPathDir(fileGlob, true);
  end;

  result := LongestCommonDirPath(dirs.ToArray);
  if result = '' then
    result := IncludeTrailingPathDelimiter(basePath);
end;


function TPackageWriter.ToEffectiveRelative(const basePath, value : string) : string;
var
  resolved : string;
begin
  if Trim(value) = '' then
    exit('');
  resolved := TPathUtils.CompressRelativePath(basePath, StringReplace(Trim(value), '/', '\', [rfReplaceAll]));
  //StripBase returns the root-relative remainder when the path is under the effective root, or just
  //the file name when it sits outside it (which then lands at the archive root).
  result := TPathUtils.StripBase(IncludeTrailingPathDelimiter(FEffectiveBasePath), resolved);
  result := StringReplace(result, '\', '/', [rfReplaceAll]);
  if TStringUtils.StartsWith(result, '/') then
    Delete(result, 1, 1);
end;


function TPackageWriter.BuildCopyToLibGlob(const basePath, source, dest : string) : string;
var
  normalisedSource : string;
  baseDir : string;
  tail : string;
  normalisedDest : string;
begin
  if Trim(dest) = '' then
  begin
    //No dest: ProcessEntry mirrors the source tree under the effective root, so the files land at
    //their effective-base-relative path - the same transform ToEffectiveRelative applies (wildcards
    //survive CompressRelativePath/StripBase unchanged).
    result := ToEffectiveRelative(basePath, source);
    exit;
  end;

  //Explicit dest: files keep their position relative to the glob's base folder, placed under dest.
  //So the archive glob is dest + the wildcard tail of the source (the part after its base folder).
  normalisedSource := StripCurrent(StringReplace(Trim(source), '/', '\', [rfReplaceAll]));
  baseDir := TPathUtils.GlobBaseDir(source);
  if baseDir <> '' then
    tail := TPathUtils.StripBase(IncludeTrailingPathDelimiter(baseDir), normalisedSource)
  else
    tail := normalisedSource;
  if TStringUtils.StartsWith(tail, '\') then
    Delete(tail, 1, 1);

  normalisedDest := ExcludeTrailingPathDelimiter(StripCurrent(StringReplace(Trim(dest), '/', '\', [rfReplaceAll])));
  result := StringReplace(normalisedDest + '\' + tail, '\', '/', [rfReplaceAll]);
  if TStringUtils.StartsWith(result, '/') then
    Delete(result, 1, 1);
end;


procedure TPackageWriter.ValidateBuildEntries(const template : ISpecTemplate; const antPattern : IAntPattern);
var
  buildEntry  : ISpecBuildEntry;
  designEntry : ISpecDesignEntry;
  sourcePatternRegexes : TArray<TRegEx>;
  sourceEntry : ISpecSourceEntry;
  patternIndex : integer;

  function NormalisedProjectPath(const value : string) : string;
  begin
    //Trim first - ConvertAntToRegexString trims source patterns and anchors them with ^..$,
    //so a trailing/leading space on a build or design project path would never match an
    //otherwise-covering source entry (and would also miss the exact-path FArchivePaths compare).
    result := Trim(value);
    result := StringReplace(result, '/', '\', [rfReplaceAll]);
    result := TPathUtils.CompressRelativePath(result);
    result := LowerCase(result);
  end;

  function MatchesAnySourcePattern(const normalisedPath : string) : boolean;
  var
    i : integer;
  begin
    for i := 0 to High(sourcePatternRegexes) do
      if sourcePatternRegexes[i].IsMatch(normalisedPath) then
        exit(true);
    result := false;
  end;

  procedure CheckProjectIncluded(const entryKind, projectPath : string);
  var
    normalised : string;
  begin
    if projectPath = '' then
      exit;
    normalised := NormalisedProjectPath(projectPath);
    //Fast path: source entry materialised this exact archive path during ProcessEntry.
    if FArchivePaths.Contains(normalised) then
      exit;
    //Wildcard fallback: build entry is covered by a source entry's ant pattern even if
    //the wildcard expansion didn't materialise this exact file (e.g. .dproj generated
    //by an upstream step, or path differences that don't survive exact-string compare).
    if MatchesAnySourcePattern(normalised) then
      exit;
    raise Exception.Create(
      entryKind + ' project [' + projectPath + '] is not covered by any source entry. ' +
      'Add a source entry that includes it (e.g. one matching its containing folder), ' +
      'otherwise install will fail when this package is consumed.');
  end;

begin
  //Pre-compile each source entry's ant pattern into a regex. ConvertAntToRegexString
  //handles `**`, `*`, `?`, and ambiguous path separators consistently so callers don't
  //need to care about slash style or escaping.
  SetLength(sourcePatternRegexes, template.SourceEntries.Count);
  patternIndex := 0;
  for sourceEntry in template.SourceEntries do
  begin
    sourcePatternRegexes[patternIndex] := TRegEx.Create(
      antPattern.ConvertAntToRegexString(NormalisedProjectPath(sourceEntry.Source)),
      [TRegExOption.roIgnoreCase]);
    Inc(patternIndex);
  end;

  for buildEntry in template.BuildEntries do
    CheckProjectIncluded('Build entry', buildEntry.Project);

  for designEntry in template.DesignEntries do
    CheckProjectIncluded('Design entry', designEntry.Project);
end;

function TPackageWriter.InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion;
                                             const basePath : string; const variables : TStringList) : boolean;
var
  reducedSpec : IPackageSpec;
  sDspec : string;
  packageFileName : string;
  sStream : TStringStream;
  platforms: TDPMPlatforms;
  antPattern : IAntPattern;

  template : ISpecTemplate;
  sourceEntry : ISpecSourceEntry;
  copyToLibEntry : ISpecSourceEntry;
  copyToBinEntry : ISpecSourceEntry;
  copyLocalEntry : ISpecCopyLocalEntry;
  buildEntry : ISpecBuildEntry;
  designEntry : ISpecDesignEntry;
  packageDef : ISpecPackageDefinition;
  i : integer;
  archiveValidation : TArchiveValidationResult;
  manifest : IPackageManifest;
  iconPath : string;
  readmePath : string;
  readmeArchiveName : string;
begin
  result := false;

  //create fresh copy of the spec with a single targetPlatform
  reducedSpec := spec.Clone;

  //the passed in targetPlatform is a clone with a single compilerVersion set.
  reducedSpec.TargetPlatforms.Clear;
  reducedSpec.TargetPlatforms.Add(targetPlatform);

  template := reducedSpec.FindTemplate(targetPlatform.TemplateName);
  if template = nil then
  begin
    FLogger.Error('Could not find Template : ' + targetPlatform.TemplateName);
    exit;
  end;

  ReplaceTokens(version, reducedSpec, targetPlatform, variables);
  platforms := targetPlatform.Platforms;
  packageFileName := reducedSpec.MetaData.Id + '-' + CompilerToString(targetPlatform.Compiler) + '-' + DPMPlatformsToBinString(platforms) + '-' + version.ToStringNoMeta + cPackageFileExt;
  packageFileName := IncludeTrailingPathDelimiter(outputFolder) + packageFileName;

  //Work out the package's internal root. When the dspec sits in a sub-folder and reaches above it
  //via '..\', this resolves to the common ancestor (eg the repo root) so every archive entry is a
  //clean, root-relative path. For a dspec at the root of its content this is just basePath.
  FEffectiveBasePath := ComputeEffectiveBasePath(basePath, reducedSpec, template);
  FArchiveWriter.SetBasePath(FEffectiveBasePath);
  if not FArchiveWriter.Open(packageFileName) then
  begin
    FLogger.Warning('Could not open package file [' + packageFileName + '] - skipping');
    exit;
  end;
  FLogger.Information('Writing package to file : ' + packageFileName);
  FArchivePaths := TCollections.CreateList<string>;
  FPrecompiledBinaries := TCollections.CreateList<string>;
  try
    //Trim stray whitespace on path values - the YAML reader preserves trailing spaces in
    //quoted scalars, which would break file lookups (icon/readme) and pattern matching (source).
    reducedSpec.MetaData.Icon := Trim(reducedSpec.MetaData.Icon);
    if reducedSpec.MetaData.Icon <> '' then
    begin
      //Resolve relative to the dspec's folder (basePath). The dspec usually lives in a sub-folder,
      //so icon/readme paths are commonly authored as '..\something' - resolve those here rather than
      //leaving them relative to the process working directory.
      iconPath := TPathUtils.CompressRelativePath(basePath, reducedSpec.MetaData.Icon);
      if not FileExists(iconPath) then
      begin
        FLogger.Error('Icon file not found : ' + iconPath);
        exit;
      end;
      if FArchiveWriter.AddIcon(iconPath) then
        reducedSpec.MetaData.Icon := GetIconArchiveFileName(iconPath);
    end;

    reducedSpec.MetaData.ReadMe := Trim(reducedSpec.MetaData.ReadMe);
    if reducedSpec.MetaData.ReadMe <> '' then
    begin
      readmePath := TPathUtils.CompressRelativePath(basePath, reducedSpec.MetaData.ReadMe);
      if not FileExists(readmePath) then
      begin
        FLogger.Error('Readme file not found : ' + readmePath);
        exit;
      end;
      //Store the readme relative to the package's effective root (mirroring its on-disk location),
      //so a '..\README.md' lands at 'README.md' rather than escaping the archive with a '..' segment.
      readmeArchiveName := ToEffectiveRelative(basePath, reducedSpec.MetaData.ReadMe);
      FArchiveWriter.AddFile(readmePath, readmeArchiveName);
      reducedSpec.MetaData.ReadMe := readmeArchiveName;
    end;

    antPattern := TAntPattern.Create(basePath);

    for sourceEntry in template.SourceEntries do
    begin
      sourceEntry.Source := StringReplace(Trim(sourceEntry.Source), '/','\',[rfReplaceAll]);
      sourceEntry.Destination := StringReplace(Trim(sourceEntry.Destination), '/','\',[rfReplaceAll]);
      for i := 0 to sourceEntry.Exclude.Count -1 do
        sourceEntry.Exclude[i] := StringReplace(Trim(sourceEntry.Exclude[i]), '/','\',[rfReplaceAll]);

      ProcessEntry(basePath,antPattern, sourceEntry.Source, sourceEntry.Destination, sourceEntry.Exclude);
    end;

    //Every build/design entry's dproj must be covered by a source entry,
    //otherwise install will fail on every consumer when MSBuild tries to load the missing file.
    ValidateBuildEntries(template, antPattern);

    //Rewrite build/design/packagedef project (and packagedef file) paths into the same archive-relative
    //form used when packing the source files, so install can find the dproj/files inside the package
    //regardless of how they were authored ('..\Sources\X.dproj' -> 'Sources/X.dproj').
    for buildEntry in template.BuildEntries do
      buildEntry.Project := ToEffectiveRelative(basePath, buildEntry.Project);
    for designEntry in template.DesignEntries do
      designEntry.Project := ToEffectiveRelative(basePath, designEntry.Project);
    for packageDef in template.PackageDefinitions do
    begin
      packageDef.Project := ToEffectiveRelative(basePath, packageDef.Project);
      for i := 0 to packageDef.Files.Count - 1 do
        packageDef.Files[i] := ToEffectiveRelative(basePath, packageDef.Files[i]);
    end;

    //Re-home any copyToLib-flagged source entries into the template-level copyToLib list before we
    //drop the source entries. The matched files are already in the archive (packed by their own source
    //entry above); install re-expands these globs against the extracted package and copies the matches
    //into lib\{platform}. Store only the archive-relative src - excluded files never made it into the
    //archive, so excludes are redundant at install time, and dest is baked into the rewritten glob.
    for sourceEntry in template.SourceEntries do
    begin
      if not sourceEntry.CopyToLib then
        continue;
      copyToLibEntry := sourceEntry.Clone;
      copyToLibEntry.Source := BuildCopyToLibGlob(basePath, sourceEntry.Source, sourceEntry.Destination);
      copyToLibEntry.Destination := '';
      copyToLibEntry.CopyToLib := false;
      copyToLibEntry.Exclude.Clear;
      template.CopyToLibEntries.Add(copyToLibEntry);
    end;

    //Re-home any copyToBin-flagged source entries into the template-level copyToBin list, same as
    //copyToLib above but the entry's CopyToBin platform is preserved - install copies the matches
    //into bpl\{platform} only when installing for that platform.
    for sourceEntry in template.SourceEntries do
    begin
      if sourceEntry.CopyToBin = TDPMPlatform.UnknownPlatform then
        continue;
      copyToBinEntry := sourceEntry.Clone;
      copyToBinEntry.Source := BuildCopyToLibGlob(basePath, sourceEntry.Source, sourceEntry.Destination);
      copyToBinEntry.Destination := '';
      copyToBinEntry.CopyToLib := false;
      copyToBinEntry.Exclude.Clear;
      template.CopyToBinEntries.Add(copyToBinEntry);
    end;

    //copyLocal entries are authored directly (not derived from source entries) - they target
    //build output produced during install (e.g. a runtime .bpl in bpl\{platform}), so there's no
    //source file to pack. Normalise the authored glob to forward slashes so the packed spec is
    //canonical; 'dpm copylocal' re-expands it (substituting $platform$) against the extracted package.
    for copyLocalEntry in template.CopyLocalEntries do
      copyLocalEntry.Source := StringReplace(Trim(copyLocalEntry.Source), '\', '/', [rfReplaceAll]);

    //we don't want these in the dspec
    template.SourceEntries.Clear;

    //Declare the precompiled PE binaries we actually shipped, with archive paths that match the
    //manifest exactly. Sorted for deterministic dspec output. The gallery rejects an author-signed
    //package whose archive contains an undeclared PE, so this list must reflect what was packed.
    template.PrecompiledBinaries.Clear;
    FPrecompiledBinaries.Sort;
    for i := 0 to FPrecompiledBinaries.Count - 1 do
      template.PrecompiledBinaries.Add(FPrecompiledBinaries[i]);

    //write the dspec last as we need to clear out stuff not needed in the spec
    sDspec := reducedSpec.GenerateDspecYAML(version);
    Assert(sDspec <> '');
    sStream := TStringStream.Create(sDspec, TEncoding.UTF8);
    try
      FArchiveWriter.WriteMetaDataFile(sStream);
    finally
      sStream.Free;
    end;


    result := true;
  finally
    FArchiveWriter.Close;
    FArchivePaths := nil;
    FPrecompiledBinaries := nil;
  end;

  // Post-process: emit dpm-manifest.json from the just-closed archive and
  // verify the archive satisfies V-9..V-13 + M-12. Failures here mean the
  // package is malformed and we don't want to ship it.
  if result and (FArchiveValidator <> nil) then
  begin
    archiveValidation := FArchiveValidator.Validate(packageFileName);
    if not archiveValidation.Ok then
    begin
      FLogger.Error(Format('Archive validation failed: %s (entry "%s")',
        [archiveValidation.Reason, archiveValidation.Entry]));
      DeleteFile(packageFileName);
      result := false;
      exit;
    end;
  end;

  if result and (FManifestService <> nil) then
  begin
    try
      manifest := FManifestService.GenerateFromArchive(packageFileName,
        reducedSpec.MetaData.Id, version.ToStringNoMeta, haSha256);
      FManifestService.InjectIntoArchive(packageFileName, manifest);
      FLogger.Debug(Format('Emitted dpm-manifest.json (%d entries)',
        [Length(manifest.Files)]));
    except
      on e : Exception do
      begin
        FLogger.Error('Manifest emission failed: ' + e.Message);
        DeleteFile(packageFileName);
        result := false;
        exit;
      end;
    end;
  end;
end;

function TPackageWriter.WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
var
  spec : IPackageSpec;
  targetPlatform : ISpecTargetPlatform;
  clonedTargetPlatform : ISpecTargetPlatform;
  version : TPackageVersion;
  error : string;
  variables : TStringList;
  props : TArray<string>;
  prop : string;
  currentCompiler : TCompilerVersion;
  expandedCompilers : TCompilerVersions;
  stopwatch : TStopWatch;
begin
  result := false;

  //expand relative paths
  options.SpecFile := TPath.GetFullPath(options.SpecFile);
  if not FileExists(options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - does not exist!');
  if not TPathUtils.IsDspecFile(options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - is likely not a spec file, incorrect extension, should be [' + cPackageSpecExt + '] or [' + cLegacyPackageSpecExt + ']');

  //output and base path default to current folder if not set
  if options.OutputFolder = '' then
    options.OutputFolder := GetCurrentDir
  else
    options.OutputFolder := TPath.GetFullPath(options.OutputFolder);

  if options.BasePath = '' then
    options.BasePath := TPath.GetDirectoryName(options.SpecFile)
  else
    options.BasePath := TPath.GetFullPath(options.BasePath);

  if not DirectoryExists(options.BasePath) then
    raise EArgumentException.Create('Base Path : ' + options.BasePath + ' - does not exist!');

  if options.Version <> '' then
  begin
    if not TPackageVersion.TryParseWithError(options.Version, version, error) then
    begin
      FLogger.Error('Invalid Version : ' + error);
      Exit(false);
    end;
  end
  else
    version := TPackageVersion.Empty;


  ForceDirectories(options.OutputFolder);
  FLogger.Information('Reading package spec from file : ' + options.SpecFile);
  stopwatch := TStopwatch.StartNew;
  spec := FSpecReader.ReadSpec(options.specFile);
  stopWatch.Stop;
  FLogger.Debug(Format('Loaded spec in : %dms',[stopWatch.ElapsedMilliseconds]));
  if spec = nil then
  begin
    FLogger.Information('An error occured reading the spec file, package writing failed');
    exit
  end;
  if not spec.IsValid then
  begin
    FLogger.Error('Spec is not valid, exiting.');
    exit;
  end;

  if spec.PackageKind <> TDPMPackageKind.dpm then
  begin
    FLogger.Error('Invalid package kind - git packages cannot be packed.');
    exit;
  end;

  //Reject reserved/banned IDE environment variable names before doing any real work.
  if not ValidateEnvironmentVariables(spec) then
    exit;

  variables := TStringList.Create;
  try
    if options.Variables <> '' then
    begin
      props := TStringUtils.SplitStr(options.Variables, ';');
      for prop in props do
      begin
        if pos('=', prop) > 0 then
          variables.Add(prop);
      end;
    end;

    FLogger.Information('Spec is valid, writing package files...');

    if version.IsEmpty then
       version := spec.MetaData.Version;

    result := true;
    for targetPlatform in spec.TargetPlatforms do
    begin
      if cancellationToken.IsCancelled then
        exit(false);

      //ExpandedCompilersOf collapses the three authoring forms (Compiler, Compilers, MinCompiler..MaxCompiler)
      //into one set so the expansion logic lives in exactly one place.
      expandedCompilers := ExpandedCompilersOf(targetPlatform);
      if expandedCompilers = [] then
      begin
        FLogger.Error('No compiler version found for target platform');
        result := false;
        exit;
      end;

      for currentCompiler in expandedCompilers do
      begin
        clonedTargetPlatform := targetPlatform.Clone;
        clonedTargetPlatform.Compiler := currentCompiler;
        result := InternalWritePackage(options.OutputFolder, clonedTargetPlatform, spec, version, options.BasePath, variables) and result;
      end;
    end;
    FLogger.Information('Done.');

  finally
    variables.Free;
  end;
end;

end.

