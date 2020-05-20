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
  TPackageIdentity = class(TInterfacedObject, IPackageIdentity)
  private
    FCompilerVersion: TCompilerVersion;
    FId: string;
    FPlatform: TDPMPlatform;
    FSourceName: string;
    FVersion: TPackageVersion;
    FProjectUrl : string;
  protected
    function GetCompilerVersion: TCompilerVersion;
    function GetId: string;
    function GetPlatform: TDPMPlatform;
    function GetProjectUrl: string;

    function GetSourceName: string;
    function GetVersion: TPackageVersion;
    function ToIdVersionString : string;
    constructor Create(const sourceName : string; const spec : IPackageSpec);overload;virtual;
  public
    function ToString : string;override;
    constructor Create(const id, source : string; const version : TPackageVersion; const compilerVersion: TCompilerVersion; const platform : TDPMPlatform; const projectUrl : string);overload;virtual;
    class function TryCreateFromString(const logger : ILogger; const value : string; const source : string; out packageIdentity : IPackageIdentity) : boolean;
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageIdentity;
  end;

  TPackageInfo = class(TPackageIdentity, IPackageInfo)
  private
    FDependencies : IList<IPackageDependency>;
  protected
    function GetDependencies: IList<IPackageDependency>;
    constructor Create(const sourceName : string; const spec : IPackageSpec);override;
  public
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageInfo;
  end;

  TPackageMetadata = class(TPackageInfo, IPackageMetadata)
  private
    FAuthors: string;
    FOwners : string;
    FCopyright: string;
    FDescription: string;
    FIcon: string;
    FIsCommercial: Boolean;
    FIsTrial: Boolean;
    FLicense: string;
    FTags: string;
    FSearchPaths : IList<IPackageSearchPath>;
  protected
    function GetOwners: string;
    function GetAuthors: string;
    function GetCopyright: string;
    function GetDescription: string;
    function GetIcon: string;
    function GetIsCommercial: Boolean;
    function GetIsTrial: Boolean;
    function GetLicense: string;
    function GetTags: string;
    function GetSearchPaths: IList<IPackageSearchPath>;
    constructor Create(const sourceName : string; const spec : IPackageSpec);reintroduce;
  public
    class function CreateFromSpec(const sourceName : string; const spec : IPackageSpec) : IPackageMetadata;
  end;

  TPackageDependency = class(TInterfacedObject, IPackageDependency)
  private
    FDependencyVersion: TVersionRange;
    FId: string;
    FPlatform: TDPMPlatform;
  protected
    function GeTVersionRange: TVersionRange;
    function GetId: string;
    function GetPlatform: TDPMPlatform;
    procedure SetVersionRange(const value : TVersionRange);
  public
     constructor Create(const id : string; const version : TVersionRange; const platform : TDPMPlatform);
  end;

  TPackageMetadataExtractor = class
  public
    //these will read the manifest
    class function TryExtractFull(const logger : ILogger; const fileName : string; out metadata : IPackageMetadata; const source : string = ''): boolean;
    class function TryExtractInfo(const logger : ILogger; const fileName : string; out info : IPackageInfo; const source : string = ''): boolean;
    //this just parses the filename
    class function TryExtractIdentity(const logger : ILogger; const fileName : string; out identity : IPackageIdentity; const source : string = ''): boolean;
  end;


//  TPackageMetadataComparer = class

implementation

uses
  DPM.Core.Constants,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.SearchPath,
  System.SysUtils,
  System.RegularExpressions,
  System.Zip;

{ TPackageMetadata }

constructor TPackageIdentity.Create(const id, source : string; const version : TPackageVersion; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const projectUrl : string);
begin
  FId := id;
  FVersion := version;
  FSourceName := source;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
end;

class function TPackageIdentity.CreateFromSpec(const sourceName: string; const spec: IPackageSpec): IPackageIdentity;
begin
  result := TPackageIdentity.Create(sourceName, spec);
end;

constructor TPackageIdentity.Create(const sourceName: string; const spec: IPackageSpec);
begin
  FSourceName := sourceName;
  FId := spec.MetaData.Id;
  FVersion := spec.MetaData.Version;
  FPlatform := spec.TargetPlatforms[0].Platforms[0];
  FCompilerVersion := spec.TargetPlatforms[0].Compiler;
end;

function TPackageIdentity.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageIdentity.GetId: string;
begin
  result := FId;
end;

function TPackageIdentity.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageIdentity.GetProjectUrl: string;
begin
  result := FProjectUrl;
end;

function TPackageIdentity.GetSourceName: string;
begin
  result := FSourceName;
end;

function TPackageIdentity.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

function TPackageIdentity.ToIdVersionString: string;
begin
  result := FId +' [' + FVersion.ToStringNoMeta + ']';
end;

function TPackageIdentity.ToString: string;
begin
  result := FId +'-' + CompilerToString(FCompilerVersion) + '-' + DPMPlatformToString(FPlatform) + '-' + FVersion.ToStringNoMeta;
end;

class function TPackageIdentity.TryCreateFromString(const logger: ILogger; const value: string; const source : string; out packageIdentity: IPackageIdentity): boolean;
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


constructor TPackageInfo.Create(const sourceName: string; const spec: IPackageSpec);
var
  dep : ISpecDependency;
  newDep : IPackageDependency;
begin
  inherited Create(sourceName, spec);
  FDependencies := TCollections.CreateList<IPackageDependency>;

  for dep in spec.TargetPlatforms[0].Dependencies do
  begin
    newDep := TPackageDependency.Create(dep.Id, dep.Version, FPlatform);
    FDependencies.Add(newDep);
  end;

end;

class function TPackageInfo.CreateFromSpec(const sourceName: string; const spec: IPackageSpec): IPackageInfo;
begin
  result := TPackageInfo.Create(sourceName, spec);
end;

function TPackageInfo.GetDependencies: IList<IPackageDependency>;
begin
  result := FDependencies;
end;

{ TPackageDependency }

constructor TPackageDependency.Create(const id: string; const version: TVersionRange; const platform: TDPMPlatform);
begin
  FId := id;
  FDependencyVersion := version;
  FPlatform := platform;
end;

function TPackageDependency.GeTVersionRange: TVersionRange;
begin
  result := FDependencyVersion;
end;

procedure TPackageDependency.SetVersionRange(const value: TVersionRange);
begin
  FDependencyVersion := value;
end;

function TPackageDependency.GetId: string;
begin
  result := FId;
end;

function TPackageDependency.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

{ TPackageMetadataFull }

constructor TPackageMetadata.Create(const sourceName : string; const spec: IPackageSpec);
var
  specSearchPath : ISpecSearchPath;
  searchPath : IPackageSearchPath;
begin
  inherited Create(sourceName, spec);
  FSearchPaths  :=  TCollections.CreateList<IPackageSearchPath>;
  FAuthors      := spec.MetaData.Authors;
  FOwners       := spec.MetaData.Owners;
  FCopyright    := spec.MetaData.Copyright;
  FDescription  := spec.MetaData.Description;
  FIcon         := spec.MetaData.Icon;
  FIsCommercial := spec.MetaData.IsCommercial;
  FIsTrial      := spec.MetaData.IsTrial;
  FLicense      := spec.MetaData.License;
  FProjectUrl   := spec.MetaData.ProjectUrl;
  FTags         := spec.MetaData.Tags;

  for specSearchPath in spec.TargetPlatforms[0].SearchPaths do
  begin
    searchPath := TPackageSearchPath.Create(specSearchPath.Path, specSearchPath.BinariesOnly, specSearchPath.SourceOnly);
    FSearchPaths.Add(searchPath);
  end;
end;

class function TPackageMetadata.CreateFromSpec(const sourceName: string; const spec: IPackageSpec): IPackageMetadata;
begin
  result := TPackageMetadata.Create(sourceName, spec);
end;

function TPackageMetadata.GetAuthors: string;
begin
  result := FAuthors;
end;

function TPackageMetadata.GetCopyright: string;
begin
  result := FCopyright;
end;

function TPackageMetadata.GetDescription: string;
begin
  result := FDescription;
end;

function TPackageMetadata.GetIcon: string;
begin
  result := FIcon;
end;

function TPackageMetadata.GetIsCommercial: Boolean;
begin
  result := FIsCommercial;
end;

function TPackageMetadata.GetIsTrial: Boolean;
begin
  result := FIsTrial;
end;

function TPackageMetadata.GetLicense: string;
begin
  result := FLicense;
end;


function TPackageMetadata.GetOwners: string;
begin
  result := FOwners;
end;

function TPackageMetadata.GetSearchPaths: IList<IPackageSearchPath>;
begin
  result := FSearchPaths;
end;

function TPackageMetadata.GetTags: string;
begin
  result := FTags;
end;

{ TPackageMetadataExtractor }

class function TPackageMetadataExtractor.TryExtractIdentity(const logger : ILogger; const fileName : string; out identity : IPackageIdentity; const source : string): boolean;
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

class function TPackageMetadataExtractor.TryExtractInfo(const logger: ILogger; const fileName: string; out info: IPackageInfo; const source: string): boolean;
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
  result:= true;

end;

class function TPackageMetadataExtractor.TryExtractFull(const logger : ILogger; const fileName : string; out metadata : IPackageMetadata; const source : string = ''): boolean;
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
  result:= true;
end;

end.
