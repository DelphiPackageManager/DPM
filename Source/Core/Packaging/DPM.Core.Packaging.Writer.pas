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
  DPM.Core.Packaging,
  DPM.Core.Packaging.Archive,
  DPM.Core.Utils.Masks;

/// This only processes specs for the Pack command. For git style packages we will need to do something else.
type
  TPackageWriter = class(TInterfacedObject, IPackageWriter)
  private
    FLogger : ILogger;
    FArchiveWriter : IPackageArchiveWriter;
    FSpecReader : IPackageSpecReader;

    FCurrentTokens : TStringList;

    FVariables : IVariables;

  protected

    procedure ProcessPattern(const basePath, dest : string; const pattern : IFileSystemPattern; const excludeMatcher : IFileMatcher; var fileCount : integer);
    procedure ProcessEntry(const basePath : string; const antPattern : IAntPattern; const source, dest : string; const exclude : IList<string>);



    procedure PopulateVariables(const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const externalProps : TStringList);

    function ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const properties: TStringList) : boolean;
    function TokenMatchEvaluator(const match : TMatch) : string;



    /// <summary>
    /// Writes a Package per compiler version.
    /// </summary>
    function InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion;
                                  const basePath : string; const properties : TStringList) : boolean;

    function WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
  public
    constructor Create(const logger : ILogger; const archiveWriter : IPackageArchiveWriter; const specReader : IPackageSpecReader);
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
  DPM.Core.Spec,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path;

{ TPackageWriter }

constructor TPackageWriter.Create(const logger : ILogger; const archiveWriter : IPackageArchiveWriter; const specReader : IPackageSpecReader);
begin
  FLogger := logger;
  FArchiveWriter := archiveWriter;
  FSpecReader := specReader;
  FCurrentTokens := nil;
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
  end;
end;


function TPackageWriter.ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const properties: TStringList): boolean;
var
  evaluator : TMatchEvaluator;
  regEx : TRegEx;
  i, j : integer;
  template : ISpecTemplate;
  source : ISpecSourceEntry;
  build  : ISpecBuildEntry;
  design : ISpecDesignEntry;
begin
  result := true;
  FVariables := TCollections.CreateDictionary<string,string>;
  regEx := TRegEx.Create('\$(\w+)\$');
  evaluator := TokenMatchEvaluator; //work around for compiler overload resolution issue.
  try
    try
      PopulateVariables(spec, targetPlatform, version, properties);

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
        build.Project := regEx.Replace(build.Project, evaluator);
        if build.Defines <> '' then
          build.Defines := regEx.Replace(build.Defines, evaluator);
      end;

      for i := 0 to template.DesignEntries.Count -1 do
      begin
        design := template.DesignEntries[i];
        design.Project := regEx.Replace(design.Project, evaluator);
        if design.Defines <> '' then
          design.Defines := regEx.Replace(design.Defines, evaluator);

        if design.LibSuffix <> '' then
          design.LibSuffix := regEx.Replace(design.LibSuffix, evaluator);

        if design.LibPrefix <> '' then
          design.LibPrefix := regEx.Replace(design.LibPrefix, evaluator);

        if design.LibVersion <> '' then
          design.LibVersion := regEx.Replace(design.LibVersion, evaluator);
      end;

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

procedure TPackageWriter.PopulateVariables(const spec: IPackageSpec; const targetPlatform: ISpecTargetPlatform; const version: TPackageVersion; const externalProps: TStringList);
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
  FVariables['compilercodename'] := CompilerCodeName(targetPlatform.Compiler);
  FVariables['compilerwithcodename'] := CompilerWithCodeName(targetPlatform.Compiler);
  FVariables['compilerwithcodename'] := CompilerNoPrefix(targetPlatform.Compiler);

  //do not replace plaform here - we'll do that during install
//  list.Add('platform=' + DPMPlatformToString(targetPlatform.Platforms[0]));
  FVariables['compilerversion'] := CompilerToCompilerVersionIntStr(targetPlatform.Compiler);
  FVariables['libsuffix'] := CompilerToLibSuffix(targetPlatform.Compiler);
  FVariables['bdsversion'] := CompilerToBDSVersion(targetPlatform.Compiler);
  FVariables['bitness'] := DPMPlatformBitness(targetPlatform.Platforms[0]);


  //apply external props passed in on command line.
  if externalProps.Count > 0 then
  begin
    for i := 0 to externalProps.Count -1 do
      FVariables[LowerCase(externalProps.Names[i])] := externalProps.ValueFromIndex[i];
  end;

  regEx := TRegEx.Create('\$(\w+)\$');
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

  if dest = '' then
    actualDest := ExtractFilePath(actualSource)
  else
    actualDest := dest;


  actualDest := StripCurrent(actualDest);
  actualDest := ExcludeTrailingPathDelimiter(actualDest);

  searchBasePath := TPathUtils.StripWildCard(TPathUtils.CompressRelativePath(basePath, actualSource));
  searchBasePath := ExtractFilePath(searchBasePath);

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
    ProcessPattern(searchBasePath, actualDest, fsPattern, fileMatcher, fileCount);

  if fileCount = 0 then
    FLogger.Warning('No files were found for pattern [' + source + ']');
end;


function TPackageWriter.InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion;
                                             const basePath : string; const properties : TStringList) : boolean;
var
  reducedSpec : IPackageSpec;
  sManifest : string;
  packageFileName : string;
  sStream : TStringStream;
  platforms: TDPMPlatforms;
  antPattern : IAntPattern;

  template : ISpecTemplate;
  sourceEntry : ISpecSourceEntry;
  i : integer;
  stopWatch : TStopWatch;
begin
  result := false;

  //create fresh copy of the spec with a single targetPlatform
  stopWatch := TStopWatch.StartNew;
  reducedSpec := spec.Clone;
  stopWatch.Stop;
  FLogger.Debug(Format('cloned spec in : %dms', [stopWatch.ElapsedMilliseconds]));

  //the passed in targetPlatform is a clone with a single compilerVersion set.
  reducedSpec.TargetPlatforms.Clear;
  reducedSpec.TargetPlatforms.Add(targetPlatform);

  template := reducedSpec.FindTemplate(targetPlatform.TemplateName);
  if template = nil then
  begin
    FLogger.Error('Could not find Template : ' + targetPlatform.TemplateName);
    exit;
  end;

  ReplaceTokens(version, reducedSpec, targetPlatform, properties);
  platforms := DPMPlatformsArrayToPlatforms(targetPlatform.Platforms);
  packageFileName := reducedSpec.MetaData.Id + '-' + CompilerToString(targetPlatform.Compiler) + '-' + DPMPlatformsToBinString(platforms) + '-' + version.ToStringNoMeta + cPackageFileExt;
  packageFileName := IncludeTrailingPathDelimiter(outputFolder) + packageFileName;
  FArchiveWriter.SetBasePath(basePath);
  if not FArchiveWriter.Open(packageFileName) then
  begin
    FLogger.Warning('Could not open package file [' + packageFileName + '] - skipping');
    exit;
  end;
  FLogger.Information('Writing package to file : ' + packageFileName);
  try
    if reducedSpec.MetaData.Icon <> '' then
      FArchiveWriter.AddIcon(reducedSpec.MetaData.Icon);

    if reducedSpec.MetaData.ReadMe <> '' then
      FArchiveWriter.AddFile(ApplyBase(basePath,reducedSpec.MetaData.ReadMe));

    antPattern := TAntPattern.Create(basePath);

    for sourceEntry in template.SourceEntries do
    begin
      sourceEntry.Source := StringReplace(sourceEntry.Source, '/','\',[rfReplaceAll]);
      sourceEntry.Destination := StringReplace(sourceEntry.Destination, '/','\',[rfReplaceAll]);
      for i := 0 to sourceEntry.Exclude.Count -1 do
        sourceEntry.Exclude[i] := StringReplace(sourceEntry.Exclude[i], '/','\',[rfReplaceAll]);

      ProcessEntry(basePath,antPattern, sourceEntry.Source, sourceEntry.Destination, sourceEntry.Exclude);
    end;
    //we don't want these in the manifest
    template.SourceEntries.Clear;
    //write the manifest last as we need to clear out stuff not needed in the spec
    sManifest := reducedSpec.GenerateManifestYAML(version);
    Assert(sManifest <> '');
    sStream := TStringStream.Create(sManifest, TEncoding.UTF8);
    try
      FArchiveWriter.WriteMetaDataFile(sStream);
    finally
      sStream.Free;
    end;


    result := true;
  finally
    FArchiveWriter.Close;
  end;
end;

function TPackageWriter.WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
var
  spec : IPackageSpec;
  targetPlatform : ISpecTargetPlatform;
  clonedTargetPlatform : ISpecTargetPlatform;
  version : TPackageVersion;
  error : string;
  properties : TStringList;
  props : TArray<string>;
  prop : string;
  minCompiler, maxCompiler,
  currentCompiler : TCompilerVersion;
  compilers : TArray<TCompilerVersion>;
  stopwatch : TStopWatch;
begin
  result := false;

  //expand relative paths
  options.SpecFile := TPath.GetFullPath(options.SpecFile);
  if not FileExists(options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - does not exist!');
  if not EndsText(cPackageSpecExt, options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - is likely not a spec file, incorrect extension, should be [' + cPackageSpecExt + ']');

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

  //where is this used?
  properties := TStringList.Create;
  try
    if options.Properties <> '' then
    begin
      props := TStringUtils.SplitStr(options.Properties, ';');
      for prop in props do
      begin
        if pos('=', prop) > 0 then
          properties.Add(prop);
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

      // TargetPlatforms can specify compiler version 3 ways
      if targetPlatform.Compiler <> TCompilerVersion.UnknownVersion then
      begin
        clonedTargetPlatform := targetPlatform.Clone;
        result := InternalWritePackage(options.OutputFolder, clonedTargetPlatform, spec, version, options.BasePath, properties) and result;
      end
      else if (targetPlatform.MinCompiler <> TCompilerVersion.UnknownVersion ) and (targetPlatform.MaxCompiler <> TCompilerVersion.UnknownVersion) then
      begin
        minCompiler := targetPlatform.MinCompiler;
        maxCompiler := targetPlatform.MaxCompiler;

        for currentCompiler := minCompiler to maxCompiler do
        begin
          clonedTargetPlatform := targetPlatform.Clone;
          clonedTargetPlatform.Compiler := currentCompiler;
          result := InternalWritePackage(options.OutputFolder, clonedTargetPlatform, spec, version, options.BasePath, properties) and result;
        end;
      end
      else if Length(targetPlatform.Compilers) > 0 then
      begin
        compilers := targetPlatform.Compilers;
        for currentCompiler in targetPlatform.Compilers do
        begin
          clonedTargetPlatform := targetPlatform.Clone;
          clonedTargetPlatform.Compiler := currentCompiler;
          result := InternalWritePackage(options.OutputFolder, clonedTargetPlatform, spec, version, options.BasePath, properties) and result;
        end;
      end
      else
      begin
        FLogger.Error('No compiler version found for target platform');
        result := false;
        exit;
      end;
    end;
    FLogger.Information('Done.');

  finally
    properties.Free;
  end;
end;

end.

