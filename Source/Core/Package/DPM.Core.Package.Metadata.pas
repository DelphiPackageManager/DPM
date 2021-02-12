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

unit DPM.Core.Package.Metadata;

interface

uses
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.Interfaces;

type
  TPackageId = class(TInterfacedObject, IPackageId)
  private
    FCompilerVersion : TCompilerVersion;
    FId : string;
    FPlatform : TDPMPlatform;
    FVersion : TPackageVersion;
  protected
    function GetCompilerVersion : TCompilerVersion;
    function GetId : string;
    function GetPlatform : TDPMPlatform;
    function GetVersion : TPackageVersion;
    function ToIdVersionString : string; virtual;
  public
    constructor Create(const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform); overload; virtual;
    function ToString : string; override;
  end;


  TPackageIdentity = class(TPackageId, IPackageIdentity, IPackageId)
  private
    FSourceName : string;
    FProjectUrl : string;
  protected
    function GetProjectUrl : string;
    function GetSourceName : string;
    constructor Create(const sourceName : string; const spec : IPackageSpec); overload; virtual;
  public
    constructor Create(const id, source : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectUrl : string); overload; virtual;
    class function TryCreateFromString(const logger : ILogger; const value : string; const source : string; out packageIdentity : IPackageIdentity) : boolean;
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageIdentity;
  end;

  TPackageInfo = class(TPackageIdentity, IPackageInfo, IPackageIdentity, IPackageId)
  private
    FDependencies : IList<IPackageDependency>;
    FUseSource : boolean;
  protected
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);
    constructor Create(const sourceName : string; const spec : IPackageSpec); override;
  public
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageInfo;
  end;

  TPackageMetadata = class(TPackageInfo, IPackageMetadata, IPackageInfo, IPackageIdentity, IPackageId)
  private
    FAuthors : string;
    FCopyright : string;
    FDescription : string;
    FIcon : string;
    FIsCommercial : Boolean;
    FIsTrial : Boolean;
    FLicense : string;
    FTags : string;
    FSearchPaths : IList<IPackageSearchPath>;
  protected
    function GetAuthors : string;
    function GetCopyright : string;
    function GetDescription : string;
    function GetIcon : string;
    function GetIsCommercial : Boolean;
    function GetIsTrial : Boolean;
    function GetLicense : string;
    function GetTags : string;
    function GetSearchPaths : IList<IPackageSearchPath>;
    constructor Create(const sourceName : string; const spec : IPackageSpec); reintroduce;
  public
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageMetadata;
  end;



  TPackageMetadataExtractor = class
  public
    //these will read the manifest
    class function TryExtractFull(const logger : ILogger; const fileName : string; out metadata : IPackageMetadata; const source : string = '') : boolean;
    class function TryExtractInfo(const logger : ILogger; const fileName : string; out info : IPackageInfo; const source : string = '') : boolean;
    //this just parses the filename
    class function TryExtractIdentity(const logger : ILogger; const fileName : string; out identity : IPackageIdentity; const source : string = '') : boolean;
  end;


  //  TPackageMetadataComparer = class

implementation

uses
  DPM.Core.Constants,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.SearchPath,
  DPM.Core.Package.Dependency,
  System.SysUtils,
  System.RegularExpressions,
  System.Zip;

{ TPackageMetadata }

constructor TPackageIdentity.Create(const id, source : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const projectUrl : string);
begin
  inherited Create(id, version, compilerVersion, platform);
  FSourceName := source;
end;

class function TPackageIdentity.CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageIdentity;
begin
  result := TPackageIdentity.Create(sourceName, spec);
end;

constructor TPackageIdentity.Create(const sourceName : string; const spec : IPackageSpec);
begin
  inherited Create(spec.MetaData.Id, spec.MetaData.Version, spec.TargetPlatform.Compiler, spec.TargetPlatform.Platforms[0]);
  FSourceName := sourceName;
end;

function TPackageIdentity.GetProjectUrl : string;
begin
  result := FProjectUrl;
end;

function TPackageIdentity.GetSourceName : string;
begin
  result := FSourceName;
end;

class function TPackageIdentity.TryCreateFromString(const logger : ILogger; const value : string; const source : string; out packageIdentity : IPackageIdentity) : boolean;
var
  match : TMatch;
  id : string;
  cv : TCompilerVersion;
  platform : TDPMPlatform;
  packageVersion : TPackageVersion;
begin
  result := false;
  match := TRegEx.Match(value, cPackageFileRegex, [roIgnoreCase]);
  if not match.Success then
  begin
    logger.Error('Package name is not a valid package [' + value + ']');
    exit;
  end;
  id := match.Groups[1].Value;
  cv := StringToCompilerVersion(match.Groups[2].Value);
  if cv = TCompilerVersion.UnknownVersion then
  begin
    logger.Error('Compiler version segment is not a valid version [' + match.Groups[2].Value + ']');
    exit;
  end;
  platform := StringToDPMPlatform(match.Groups[3].Value);
  if platform = TDPMPlatform.UnknownPlatform then
  begin
    logger.Error('Platform segment is not a valid platform [' + match.Groups[3].Value + ']');
    exit;
  end;
  if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
  begin
    logger.Error('Version segment is not a valid version [' + match.Groups[4].Value + ']');
    exit;
  end;

  //we dont' have a source.
  packageIdentity := TPackageIdentity.Create(id, '', packageVersion, cv, platform, '');
  result := true;


end;

{ TPackageMetaDataWithDependencies }


constructor TPackageInfo.Create(const sourceName : string; const spec : IPackageSpec);
var
  dep : ISpecDependency;
  newDep : IPackageDependency;
begin
  inherited Create(sourceName, spec);
  FDependencies := TCollections.CreateList<IPackageDependency>;

  for dep in spec.TargetPlatform.Dependencies do
  begin
    newDep := TPackageDependency.Create(dep.Id, dep.Version, FPlatform);
    FDependencies.Add(newDep);
  end;

end;

class function TPackageInfo.CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageInfo;
begin
  result := TPackageInfo.Create(sourceName, spec);
end;

function TPackageInfo.GetDependencies : IList<IPackageDependency>;
begin
  result := FDependencies;
end;


function TPackageInfo.GetUseSource: boolean;
begin
  result := FUseSource;
end;

procedure TPackageInfo.SetUseSource(const value: boolean);
begin
  FUseSource := value;
end;

{ TPackageMetadataFull }

constructor TPackageMetadata.Create(const sourceName : string; const spec : IPackageSpec);
var
  specSearchPath : ISpecSearchPath;
  searchPath : IPackageSearchPath;
begin
  inherited Create(sourceName, spec);
  FSearchPaths := TCollections.CreateList<IPackageSearchPath>;
  FAuthors := spec.MetaData.Authors;
  FCopyright := spec.MetaData.Copyright;
  FDescription := spec.MetaData.Description;
  FIcon := spec.MetaData.Icon;
  FIsCommercial := spec.MetaData.IsCommercial;
  FIsTrial := spec.MetaData.IsTrial;
  FLicense := spec.MetaData.License;
  FProjectUrl := spec.MetaData.ProjectUrl;
  FTags := spec.MetaData.Tags;

  for specSearchPath in spec.TargetPlatform.SearchPaths do
  begin
    searchPath := TPackageSearchPath.Create(specSearchPath.Path);
    FSearchPaths.Add(searchPath);
  end;
end;

class function TPackageMetadata.CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageMetadata;
begin
  result := TPackageMetadata.Create(sourceName, spec);
end;

function TPackageMetadata.GetAuthors : string;
begin
  result := FAuthors;
end;

function TPackageMetadata.GetCopyright : string;
begin
  result := FCopyright;
end;

function TPackageMetadata.GetDescription : string;
begin
  result := FDescription;
end;

function TPackageMetadata.GetIcon : string;
begin
  result := FIcon;
end;

function TPackageMetadata.GetIsCommercial : Boolean;
begin
  result := FIsCommercial;
end;

function TPackageMetadata.GetIsTrial : Boolean;
begin
  result := FIsTrial;
end;

function TPackageMetadata.GetLicense : string;
begin
  result := FLicense;
end;

function TPackageMetadata.GetSearchPaths : IList<IPackageSearchPath>;
begin
  result := FSearchPaths;
end;

function TPackageMetadata.GetTags : string;
begin
  result := FTags;
end;

{ TPackageMetadataExtractor }

class function TPackageMetadataExtractor.TryExtractIdentity(const logger : ILogger; const fileName : string; out identity : IPackageIdentity; const source : string) : boolean;
var
  match : TMatch;
  id : string;
  cv : TCompilerVersion;
  platform : TDPMPlatform;
  packageVersion : TPackageVersion;
  value : string;
begin
  identity := nil;
  value := ExtractFileName(fileName);
  result := false;
  match := TRegEx.Match(filename, cPackageFileRegex, [roIgnoreCase]);
  if not match.Success then
  begin
    logger.Error('Package name is not a valid package [' + value + ']');
    exit;
  end;
  id := match.Groups[1].Value;
  cv := StringToCompilerVersion(match.Groups[2].Value);
  if cv = TCompilerVersion.UnknownVersion then
  begin
    logger.Error('Compiler version segment is not a valid version [' + match.Groups[2].Value + ']');
    exit;
  end;
  platform := StringToDPMPlatform(match.Groups[3].Value);
  if platform = TDPMPlatform.UnknownPlatform then
  begin
    logger.Error('Platform segment is not a valid platform [' + match.Groups[3].Value + ']');
    exit;
  end;
  if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
  begin
    logger.Error('Version segment is not a valid version [' + match.Groups[4].Value + ']');
    exit;
  end;
  //we dont' have a source.
  identity := TPackageIdentity.Create(id, '', packageVersion, cv, platform, '');
  result := true;
end;

class function TPackageMetadataExtractor.TryExtractInfo(const logger : ILogger; const fileName : string; out info : IPackageInfo; const source : string) : boolean;
var
  zipFile : TZipFile;
  metaBytes : TBytes;
  metaString : string;
  spec : IPackageSpec;
  reader : IPackageSpecReader;
begin
  result := false;
  zipFile := TZipFile.Create;
  try
    try
      zipFile.Open(fileName, TZipMode.zmRead);
      zipFile.Read(cPackageMetaFile, metaBytes);
    except
      on e : Exception do
      begin
        Logger.Error('Error opening package file [' + fileName + ']');
        exit;
      end;
    end;
  finally
    zipFile.Free;
  end;
  //doing this outside the try/finally to avoid locking the package for too long.
  metaString := TEncoding.UTF8.GetString(metaBytes);
  reader := TPackageSpecReader.Create(Logger);
  spec := reader.ReadSpecString(metaString);
  if spec = nil then
    exit;
  info := TPackageMetadata.CreateFromSpec(source, spec);
  result := true;

end;

class function TPackageMetadataExtractor.TryExtractFull(const logger : ILogger; const fileName : string; out metadata : IPackageMetadata; const source : string = '') : boolean;
var
  zipFile : TZipFile;
  metaBytes : TBytes;
  metaString : string;
  spec : IPackageSpec;
  reader : IPackageSpecReader;
begin
  result := false;
  zipFile := TZipFile.Create;
  try
    try
      zipFile.Open(fileName, TZipMode.zmRead);
      zipFile.Read(cPackageMetaFile, metaBytes);
    except
      on e : Exception do
      begin
        Logger.Error('Error opening package file [' + fileName + ']');
        exit;
      end;
    end;
  finally
    zipFile.Free;
  end;
  //doing this outside the try/finally to avoid locking the package for too long.
  metaString := TEncoding.UTF8.GetString(metaBytes);
  reader := TPackageSpecReader.Create(Logger);
  spec := reader.ReadSpecString(metaString);
  if spec = nil then
    exit;
  metadata := TPackageMetadata.CreateFromSpec(source, spec);
  result := true;
end;

{ TPackageId }

constructor TPackageId.Create(const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform);
begin
  FId := id;
  FVersion := version;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
end;

function TPackageId.GetCompilerVersion : TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageId.GetId : string;
begin
  result := FId;
end;

function TPackageId.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageId.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;

function TPackageId.ToIdVersionString : string;
begin
  result := FId + ' [' + FVersion.ToStringNoMeta + ']';
end;

function TPackageId.ToString : string;
begin
  result := FId + '-' + CompilerToString(FCompilerVersion) + '-' + DPMPlatformToString(FPlatform) + '-' + FVersion.ToStringNoMeta;
end;

end.

