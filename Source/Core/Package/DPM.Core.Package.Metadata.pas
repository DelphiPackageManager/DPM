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
  JsonDataObjects,
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
  protected
    function GetSourceName : string;
    constructor Create(const sourceName : string; const spec : IPackageSpec); overload; virtual;
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);overload;virtual;
  public
    constructor Create(const sourceName : string; const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform); overload; virtual;
    class function TryCreateFromString(const logger : ILogger; const value : string; const source : string; out packageIdentity : IPackageIdentity) : boolean;
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageIdentity;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageIdentity : IPackageIdentity) : boolean;
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
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);override;
  public
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageInfo;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageInfo : IPackageInfo) : boolean;
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
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
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
    function GetProjectUrl : string;
    function GetRepositoryUrl: string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    constructor Create(const sourceName : string; const spec : IPackageSpec); override;
  public
    constructor Create(const sourceName : string; const jsonObj : TJsonObject); override;
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageMetadata;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageMetadata : IPackageMetadata) : boolean;
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
  System.Zip,
  System.Classes;

{ TPackageMetadata }


constructor TPackageIdentity.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  id : string;
  stmp : string;
  cv : TCompilerVersion;
  platform : TDPMPlatform;
  packageVersion : TPackageVersion;
begin
  id := jsonObj.S['id'];
  stmp := jsonObj.S['compiler'];
  cv := StringToCompilerVersion(stmp);
  if cv = TCompilerVersion.UnknownVersion then
    raise Exception.Create('Compiler segment is not a valid version [' + stmp+ ']');
  stmp := jsonObj.S['platform'];
  platform := StringToDPMPlatform(stmp);
  if platform = TDPMPlatform.UnknownPlatform then
    raise Exception.Create('Platform is not a valid platform [' + stmp+ ']');

  stmp := jsonObj.S['version'];
  if not TPackageVersion.TryParse(stmp, packageVersion) then
    raise Exception.Create('Version is not a valid version [' + stmp + ']');

  inherited Create(id, packageVersion, cv, platform);
  FSourceName := sourceName;


end;

constructor TPackageIdentity.Create(const sourceName : string; const id: string; const version: TPackageVersion; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform);
begin
  inherited Create(id, version, compilerVersion, platform);
  FSourceName := sourceName;
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


function TPackageIdentity.GetSourceName : string;
begin
  result := FSourceName;
end;

class function TPackageIdentity.TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageIdentity : IPackageIdentity) : boolean;
begin
  result := false;
  try
    packageIdentity := TPackageIdentity.Create(source, jsonObj);
  except
    on e : Exception do
    begin
      logger.Error(e.Message);
      exit
    end;
  end;
  result := true;

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

  packageIdentity := TPackageIdentity.Create(source, id,  packageVersion, cv, platform);
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


constructor TPackageInfo.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  depArr : TJsonArray;
  depId : string;
  depVersion : string;
  i: Integer;
  range : TVersionRange;
  dependency : IPackageDependency;
begin
  inherited Create(sourceName, jsonObj);
  FDependencies := TCollections.CreateList<IPackageDependency>;
  if jsonObj.Contains('dependencies') then
  begin
    depArr := jsonObj.A['dependencies'];
    for i := 0 to depArr.Count -1 do
    begin
      depId := depArr.O[i].S['packageId'];
      depVersion := depArr.O[i].S['versionRange'];
    end;

    if TVersionRange.TryParse(depVersion, range) then
    begin
      dependency := TPackageDependency.Create(depId, range, FPlatform);
      FDependencies.Add(dependency);
    end;
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

class function TPackageInfo.TryLoadFromJson(const logger: ILogger; const jsonObj: TJsonObject; const source: string; out packageInfo: IPackageInfo): boolean;
begin
  result := false;
  try
    packageInfo := TPackageInfo.Create(source, jsonObj);
  except
    on e : Exception do
    begin
      logger.Error(e.Message);
      exit;
    end;
  end;
  result := true;
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
  FProjectUrl := spec.MetaData.ProjectUrl;
  FRepositoryUrl := spec.MetaData.RepositoryUrl;
  FRepositoryType := spec.MetaData.RepositoryType;
  FRepositoryBranch := spec.MetaData.RepositoryBranch;
  FRepositoryCommit := spec.MetaData.RepositoryCommit;

  for specSearchPath in spec.TargetPlatform.SearchPaths do
  begin
    searchPath := TPackageSearchPath.Create(specSearchPath.Path);
    FSearchPaths.Add(searchPath);
  end;
end;


constructor TPackageMetadata.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  searchPaths : string;
  sList : TStringList;
  i: Integer;
  searchPath : IPackageSearchPath;
begin
  inherited Create(sourceName, jsonObj);
  FSearchPaths := TCollections.CreateList<IPackageSearchPath>;

  FAuthors        := jsonObj.S['authors'];;
  FCopyright      := jsonObj.S['copyright'];
  FDescription    := jsonObj.S['description'];
  FIcon           := jsonObj.S['icon'];
  FIsCommercial   := jsonObj.B['isCommercial'];
  FIsTrial        := jsonObj.B['isTrial'];
  FLicense        := jsonObj.S['License'];
  FProjectUrl     := jsonObj.S['ProjectUrl'];
  FTags           := jsonObj.S['Tags'];
  FProjectUrl     := jsonObj.S['ProjectUrl'];
  FRepositoryUrl  := jsonObj.S['RepositoryUrl'];
  FRepositoryType := jsonObj.S['RepositoryType'];
  FRepositoryBranch := jsonObj.S['RepositoryBranch'];
  FRepositoryCommit := jsonObj.S['RepositoryCommit'];
  FRepositoryCommit := jsonObj.S['RepositoryCommit'];
  searchPaths       := jsonObj.S['searchPaths'];

  if searchPaths <> '' then
  begin
    sList := TStringList.Create;
    try
      sList.Delimiter := ';';
      sList.DelimitedText := searchPaths;
      for i := 0 to sList.Count -1 do
      begin
        searchPath := TPackageSearchPath.Create(sList.Strings[i]);
        FSearchPaths.Add(searchPath);
      end;
    finally
      sList.Free;
    end;
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

function TPackageMetadata.GetProjectUrl: string;
begin
  result := FProjectUrl;
end;

function TPackageMetadata.GetRepositoryBranch: string;
begin
  result := FRepositoryBranch;
end;

function TPackageMetadata.GetRepositoryCommit: string;
begin
  result := FRepositoryCommit;
end;

function TPackageMetadata.GetRepositoryType: string;
begin
  result := FRepositoryBranch;
end;

function TPackageMetadata.GetRepositoryUrl: string;
begin
  result := FRepositoryUrl;
end;

function TPackageMetadata.GetSearchPaths : IList<IPackageSearchPath>;
begin
  result := FSearchPaths;
end;

function TPackageMetadata.GetTags : string;
begin
  result := FTags;
end;

class function TPackageMetadata.TryLoadFromJson(const logger: ILogger; const jsonObj: TJsonObject; const source: string; out packageMetadata: IPackageMetadata): boolean;
begin
  result := false;
  try
    packageMetadata := TPackageMetadata.Create(source, jsonObj);
  except
    on e : Exception do
    begin
      logger.Error(e.Message);
      exit;

    end;
  end;
  result := true;
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
  identity := TPackageIdentity.Create(id, source, packageVersion, cv, platform);
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

