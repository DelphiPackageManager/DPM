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


unit DPM.Core.Packaging.Writer;

interface

uses
  Spring.Container,
  VSoft.AntPatterns,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Pack,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Packaging,
  DPM.Core.Packaging.Archive;

type
  TPackageWriter = class(TInterfacedObject,IPackageWriter)
  private
    FLogger : ILogger;
    FArchiveWriter : IPackageArchiveWriter;
    FSpecReader : IPackageSpecReader;
    procedure ProcessPattern(const basePath, dest: string; const pattern: IFileSystemPattern; const searchPath: string; const flatten: boolean; const excludePatterns : TArray<IFileSystemPattern>;  var fileCount: integer);
  protected

    function WritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion; const basePath : string) : boolean;

    //Generate zip file and xml metadata file.
    function WritePackageFromSpec(const options : TPackOptions) : boolean;
  public
    constructor Create(const logger : ILogger; const archiveWriter : IPackageArchiveWriter; const specReader : IPackageSpecReader);
  end;



implementation

uses
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.Classes,
  System.Masks,
  DPM.Core.Constants,
  DPM.Core.Spec,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.Path;

{ TPackageWriter }

constructor TPackageWriter.Create(const logger: ILogger; const archiveWriter : IPackageArchiveWriter; const specReader : IPackageSpecReader);
begin
  FLogger := logger;
  FArchiveWriter := archiveWriter;
  FSpecReader := specReader;
end;

function StripBase(const base : string; const fileName : string) : string;
begin
  if TStringUtils.StartsWith(fileName, base) then
    result := Copy(fileName, Length(base) + 1, Length(fileName))
  else
    //it's below or outside the base path,
    //there's no way to know what else to do
    //but just use the filename, so it will
    //end up in the root folder??
    result := ExtractFileName(fileName);
end;

function GetNonWildcardPath(const value : string) : string;
var
  i : integer;
begin
  result := '';
  i :=  Pos('*', value);
  if i > 0 then
    result := Copy(value,1, i -1);
end;

procedure TPackageWriter.ProcessPattern(const basePath: string; const dest : string; const pattern : IFileSystemPattern; const searchPath : string; const flatten : boolean; const excludePatterns : TArray<IFileSystemPattern>; var fileCount : integer);
var
  files : TStringDynArray;
  f : string;
  archivePath : string;

  function IsFileExcluded(const fileName : string) : boolean;
  var
    excludePatten : IFileSystemPattern;
    mask : string;
  begin
    result := false;
    if Length(excludePatterns) > 0 then
    begin
      for excludePatten in excludePatterns do
      begin
        mask := excludePatten.FileMask;
        if TPathUtils.IsRelativePath(mask) then
          mask := TPath.Combine(basePath, mask);

        //this might be slow.. Creates/Destroys TMask each time
        //if it is then create a record based mask type to do the job.
        if MatchesMask(fileName,  mask) then
          exit(true);
      end;
    end;

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


    if flatten then
      archivePath := dest + '\' + ExtractFileName(f)
    else
      archivePath := dest + '\' + StripBase(IncludeTrailingPathDelimiter(basePath) + searchPath , f);
      if TStringUtils.StartsWith(archivePath, '\') then
        Delete(archivePath,1,1);
    Inc(fileCount);
   // FLogger.Debug('Writing file [' + archivePath + '] to package.');
    FArchiveWriter.AddFile(f,archivePath);
  end;
end;


function TPackageWriter.WritePackage(const outputFolder : string; const targetPlatform : ISpecTargetPlatform; const spec : IPackageSpec; const version : TPackageVersion; const basePath : string): boolean;
var
  sManifest : string;
  packageFileName : string;
  sStream : TStringStream;
  antPattern : IAntPattern;
  fileEntry : ISpecFileEntry;
  bplEntry : ISpecBPLEntry;

  procedure ProcessEntry(const source, dest : string; const flatten : boolean; const exclude : string);
  var
    fsPatterns : TArray<IFileSystemPattern>;
    fsPattern : IFileSystemPattern;
    fsExcludePatterns : TArray<IFileSystemPattern>;
    nonWildcardPath : string;
    fileCount : integer;

  begin
    nonWildcardPath := GetNonWildcardPath(source);
    fsPatterns := antPattern.Expand(source);
    if exclude <> '' then
      fsExcludePatterns := antPattern.Expand(exclude);
    fileCount := 0;

    for fsPattern in fsPatterns do
     ProcessPattern(basePath, dest, fsPattern, nonWildcardPath, flatten, fsExcludePatterns, fileCount);

    if fileCount = 0 then
     FLogger.Warning('No files were found for pattern [' + source + ']');
  end;


begin
  result := false;
  sManifest := spec.GenerateManifestJson(version, targetPlatform);
  //going with json
//  sManifest := spec.GenerateManifestXML(version, targetPlatform);
  packageFileName := spec.MetaData.Id + '-' + CompilerToString(targetPlatform.Compiler) + '-' +  DPMPlatformToString(targetPlatform.Platforms[0]) + '-' +  version.ToStringNoMeta + cPackageFileExt;
  packageFileName := IncludeTrailingPathDelimiter(outputFolder) + packageFileName;
  FArchiveWriter.SetBasePath(basePath);

  if not FArchiveWriter.Open(packageFileName) then
  begin
    FLogger.Warning('Could not open package file [' + packageFileName + '] - skipping');
    exit;
  end;
//  FLogger.Debug('Writing file : ' + packageFileName);
  try
    sStream := TStringStream.Create(sManifest,TEncoding.UTF8);
    try
      FArchiveWriter.WriteMetaDataFile(sStream);
    finally
      sStream.Free;
    end;
    antPattern := TAntPattern.Create(basePath);

    for fileEntry in targetPlatform.SourceFiles do
      ProcessEntry(fileEntry.Source, fileEntry.Destination, fileEntry.Flatten, fileEntry.Exclude);

    for fileEntry in targetPlatform.Files do
      ProcessEntry(fileEntry.Source, fileEntry.Destination, fileEntry.Flatten, fileEntry.Exclude);

    for fileEntry in targetPlatform.LibFiles do
      ProcessEntry(fileEntry.Source, fileEntry.Destination, fileEntry.Flatten, fileEntry.Exclude);

    for bplEntry in targetPlatform.RuntimeFiles do
      ProcessEntry(bplEntry.Source, bplEntry.Destination, bplEntry.Flatten, bplEntry.Exclude);

    for bplEntry in targetPlatform.DesignFiles do
      ProcessEntry(bplEntry.Source, bplEntry.Destination, bplEntry.Flatten, bplEntry.Exclude);

  finally
    FArchiveWriter.Close;
  end;
end;

function TPackageWriter.WritePackageFromSpec(const options : TPackOptions) : boolean;
var
  spec : IPackageSpec;
  targetPlatform : ISpecTargetPlatform;
  version : TPackageVersion;
  error : string;
  properties : TStringList;
  props : TArray<string>;
  prop : string;
begin
  result := false;

  //expand relative paths
  options.SpecFile := TPath.GetFullPath(options.SpecFile);
  if not FileExists(options.SpecFile) then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - does not exist!');
  if ExtractFileExt(options.SpecFile) <> cPackageSpecExt then
    raise EArgumentException.Create('Spec file : ' + options.SpecFile + ' - is likely not a spec file, incorrect extension, should be [' +  cPackageSpecExt + ']');

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

    if not spec.PreProcess(version, properties)  then
    begin
      FLogger.Error('Spec is not valid, exiting.');
      exit;
    end;

    FLogger.Information('Spec is valid, writing package files...');
    for targetPlatform in spec.TargetPlatforms do
    begin
      if version.IsEmpty then
        version := spec.MetaData.Version;
      result := WritePackage(options.OutputFolder, targetPlatform, spec, version, options.BasePath) and result;
    end;
    FLogger.Information('Done.');

  finally
    properties.Free;
  end;



end;

end.
