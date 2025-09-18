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
  DPM.Core.Packaging.Archive;

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

    procedure ProcessPattern(const basePath, dest : string; const pattern : IFileSystemPattern; const flatten : boolean; const excludePatterns : IList<string> ; const ignore : boolean; var fileCount : integer);
    procedure ProcessEntry(const basePath : string; const antPattern : IAntPattern; const source, dest : string; const flatten : boolean; const exclude : IList<string> ; const ignore : boolean);


    procedure GetTokensForTargetPlatform(const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const version : TPackageVersion; const list : TStringList; const externalProps : TStringList);
    function ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const properties: TStringList) : boolean;
    function TokenMatchEvaluator(const match : TMatch) : string;



    /// <summary>
    /// Writes a Package per compiler version.
    /// </summary>
    function InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion; const basePath : string) : boolean;

    function WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
  public
    constructor Create(const logger : ILogger; const archiveWriter : IPackageArchiveWriter; const specReader : IPackageSpecReader);
  end;



implementation

uses
  System.SysUtils,
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

procedure TPackageWriter.GetTokensForTargetPlatform(const spec: IPackageSpec; const targetPlatform: ISpecTargetPlatform; const version: TPackageVersion; const list, externalProps: TStringList);
var
  i: Integer;
  regEx : TRegEx;
  evaluator : TMatchEvaluator;
begin
  list.Clear;
  if not version.IsEmpty then
    list.Add('version=' + version.ToString)
  else
    list.Add('version=' +  spec.MetaData.Version.ToString);
  list.Add('target=' + CompilerToString(targetPlatform.Compiler));
  list.Add('compiler=' + CompilerToString(targetPlatform.Compiler));
  list.Add('compilerNoPoint=' + CompilerToStringNoPoint(targetPlatform.Compiler));
  list.Add('compilerCodeName=' + CompilerCodeName(targetPlatform.Compiler));
  list.Add('compilerWithCodeName=' + CompilerWithCodeName(targetPlatform.Compiler));
  //do not replace plaform here - we'll do that during install
//  list.Add('platform=' + DPMPlatformToString(targetPlatform.Platforms[0]));
  list.Add('compilerVersion=' + CompilerToCompilerVersionIntStr(targetPlatform.Compiler));
  list.Add('libSuffix=' + CompilerToLibSuffix(targetPlatform.Compiler));
  list.Add('bdsVersion=' + CompilerToBDSVersion(targetPlatform.Compiler));
  list.Add('bitness=' + DPMPlatformBitness(targetPlatform.Platforms[0]));
  if DPMPlatformBitness(targetPlatform.Platforms[0]) = '64' then
    list.Add('bitness64Only=' + DPMPlatformBitness(targetPlatform.Platforms[0]))
  else
    list.Add('bitness64Only=');

  if targetPlatform.Variables.Count = 0 then
    exit;

  //override the values with values from the template.
  for i := 0 to targetPlatform.Variables.Count -1 do
  begin
    //setting a value to '' removes it from the list!!!!!
    list.Values[targetPlatform.Variables.Items[i].Key] := '';
    list.Add(targetPlatform.Variables.Items[i].Key + '=' + targetPlatform.Variables.Items[i].Value);
  end;

  //fix up some variable overrides
  if list.Values['compilerCodeName'] = '' then
  begin
    list.Values['compilerWithCodeName'] := '';
    list.Add('compilerWithCodeName=' + CompilerToString(targetPlatform.Compiler));
  end;


  //apply external props passed in on command line.
  if externalProps.Count > 0 then
  begin
    for i := 0 to externalProps.Count -1 do
      list.Values[externalProps.Names[i]] := externalProps.ValueFromIndex[i];
  end;

  regEx := TRegEx.Create('\$(\w+)\$');
  evaluator := TokenMatchEvaluator;

  //variables from the spec and external may reference existing variables.
  for i := 0 to list.Count -1 do
  begin
    if TStringUtils.Contains(list.ValueFromIndex[i], '$') then
      list.ValueFromIndex[i] := regEx.Replace(list.ValueFromIndex[i], evaluator);
  end;

end;

procedure TPackageWriter.ProcessPattern(const basePath : string; const dest : string; const pattern : IFileSystemPattern; const flatten : boolean; const excludePatterns : IList<string> ; const ignore : boolean; var fileCount : integer);
var
  files : TStringDynArray;
  f : string;
  archivePath : string;

  function IsFileExcluded(const fileName : string) : boolean;
  var
    excludePatten : string;
  begin
    result := false;
    if excludePatterns.Count > 0 then
    begin
      for excludePatten in excludePatterns do
      begin
        //this might be slow.. Creates/Destroys TMask each time
        //if it is then create a record based mask type to do the job.
        if MatchesMask(fileName, excludePatten) then
          exit(true);
      end;
    end;
  end;
begin

  if not TDirectory.Exists(pattern.Directory) then
    if ignore then
      exit
    else
      raise Exception.Create('Directory not found : ' + pattern.Directory);
  f := TPath.Combine(pattern.Directory, pattern.FileMask); //TODO : What whas this meant to do???
  files := TDirectory.GetFiles(pattern.Directory, pattern.FileMask, TSearchOption.soTopDirectoryOnly);
  for f in files do
  begin
    if IsFileExcluded(f) then
      continue;

    if not TFile.Exists(f) then
      raise Exception.Create('File not found : ' + f);


    if flatten then
      archivePath := dest + '\' + ExtractFileName(f)
    else
      archivePath := dest + '\' + TPathUtils.StripBase(basePath, f);
    if TStringUtils.StartsWith(archivePath, '\') then
      Delete(archivePath, 1, 1);
    Inc(fileCount);
    // FLogger.Debug('Writing file [' + archivePath + '] to package.');
    FArchiveWriter.AddFile(f, archivePath);
  end;
end;


function TPackageWriter.ReplaceTokens(const version: TPackageVersion; const spec : IPackageSpec; const targetPlatform : ISpecTargetPlatform; const properties: TStringList): boolean;
var
  pair : TPair<string,string>;
  evaluator : TMatchEvaluator;
  regEx : TRegEx;

begin
  FVariables := TCollections.CreateDictionary<string,string>;

  for pair in spec.Variables do
    FVariables[LowerCase(pair.Key)] := pair.Value;

  //apply targetPlatform overrides;
  for pair in targetPlatform.Variables do
    FVariables[LowerCase(pair.Key)] := pair.Value;

  //we don't want to write out variables into the manifest
  spec.Variables.Clear;
  targetPlatform.Variables.Clear;


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

procedure TPackageWriter.ProcessEntry(const basePath : string; const antPattern : IAntPattern; const source, dest : string; const flatten : boolean; const exclude : IList<string> ; const ignore : boolean);
var
  fsPatterns : TArray<IFileSystemPattern>;
  fsPattern : IFileSystemPattern;
  searchBasePath : string;
  fileCount : integer;
  actualDest : string;

  procedure ValidateDestinationPath(const source, dest : string);
  begin
    if (pos(#10, dest) > 0) or (pos(#13, dest) > 0) then
      raise Exception.Create('Entry [' + source + '] has invalid characters in destination [' + dest + ']');
  end;


begin
  ValidateDestinationPath(source, dest);
  if dest = '' then
    actualDest := ExtractFilePath(source)
  else
    actualDest := dest;
  actualDest := ExcludeTrailingPathDelimiter(actualDest);

  searchBasePath := TPathUtils.StripWildCard(TPathUtils.CompressRelativePath(basePath, source));
  searchBasePath := ExtractFilePath(searchBasePath);
  fsPatterns := antPattern.Expand(source);
  fileCount := 0;
  for fsPattern in fsPatterns do
    ProcessPattern(searchBasePath, actualDest, fsPattern, flatten, exclude, ignore, fileCount);

  if (not ignore) and (fileCount = 0) then
    FLogger.Warning('No files were found for pattern [' + source + ']');
end;


function TPackageWriter.InternalWritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion; const basePath : string) : boolean;
var
  reducedSpec : IPackageSpec;
  actualTargetPlatform : ISpecTargetPlatform;

  sManifest : string;
  packageFileName : string;
  sStream : TStringStream;
  platforms: TDPMPlatforms;



begin
  result := false;

  //create fresh copy of the spec with a single targetPlatform
  reducedSpec := spec.Clone;

  //the passed in targetPlatform already has a single compilerVersion set.
  actualTargetPlatform := targetPlatform.Clone;
  reducedSpec.TargetPlatforms.Clear;
  reducedSpec.TargetPlatforms.Add(targetPlatform.Clone);



  sManifest := spec.GenerateManifestJson(version, targetPlatform);
  platforms := DPMPlatformsArrayToPlatforms(targetPlatform.Platforms);

  packageFileName := spec.MetaData.Id + '-' + CompilerToString(targetPlatform.Compiler) + '-' + DPMPlatformsToBinString(platforms) + '-' + version.ToStringNoMeta + cPackageFileExt;
  packageFileName := IncludeTrailingPathDelimiter(outputFolder) + packageFileName;
  FArchiveWriter.SetBasePath(basePath);
  if not FArchiveWriter.Open(packageFileName) then
  begin
    FLogger.Warning('Could not open package file [' + packageFileName + '] - skipping');
    exit;
  end;
  FLogger.Information('Writing package to file : ' + packageFileName);
  try
    if spec.MetaData.Icon <> '' then
      FArchiveWriter.AddIcon(spec.MetaData.Icon);

    if spec.MetaData.ReadMe <> '' then
      FArchiveWriter.AddFile(spec.MetaData.ReadMe);

    sStream := TStringStream.Create(sManifest, TEncoding.UTF8);
    try
      FArchiveWriter.WriteMetaDataFile(sStream);
    finally
      sStream.Free;
    end;






//    result := true;
  finally
    FArchiveWriter.Close;
  end;
end;

function TPackageWriter.WritePackageFromSpec(const cancellationToken : ICancellationToken; const options : TPackOptions) : boolean;
var
  spec : IPackageSpec;
  compilerVersion : TCompilerVersion;
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
begin
  result := false;

  //expand relative paths
  options.SpecFile := TPath.GetFullPath(options.SpecFile);
  if not FileExists(options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - does not exist!');
  if ExtractFileExt(options.SpecFile) <> cPackageSpecExt then
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
  spec := FSpecReader.ReadSpec(options.specFile);
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


    result := true;
    for targetPlatform in spec.TargetPlatforms do
    begin
      if cancellationToken.IsCancelled then
        exit(false);

      if version.IsEmpty then
        version := spec.MetaData.Version;

      // TargetPlatforms can specify compiler version 3 ways
      if targetPlatform.Compiler <> TCompilerVersion.UnknownVersion then
      begin
        clonedTargetPlatform := targetPlatform.Clone;
        result := InternalWritePackage(options.OutputFolder, clonedTargetPlatform, spec, version, options.BasePath) and result;
      end
      else if (targetPlatform.MinCompiler <> TCompilerVersion.UnknownVersion ) and (targetPlatform.MinCompiler <> TCompilerVersion.UnknownVersion) then
      begin
        minCompiler := targetPlatform.MinCompiler;
        maxCompiler := targetPlatform.MaxCompiler;

        for currentCompiler := minCompiler to maxCompiler do
        begin
          clonedTargetPlatform := targetPlatform.Clone;
          clonedTargetPlatform.Compiler := currentCompiler;
          result := InternalWritePackage(options.OutputFolder, targetPlatform, spec, version, options.BasePath) and result;
        end;
      end
      else if Length(targetPlatform.Compilers) > 0 then
      begin
        compilers := targetPlatform.Compilers;
        for currentCompiler in targetPlatform.Compilers do
        begin
          clonedTargetPlatform := targetPlatform.Clone;
          targetPlatform.Compiler := currentCompiler;
          result := InternalWritePackage(options.OutputFolder, targetPlatform, spec, version, options.BasePath) and result;
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

