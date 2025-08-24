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

unit DPM.Core.Package.Classes;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Manifest.Interfaces;

type
  TPackageIdentity = class(TInterfacedObject, IPackageIdentity)
  private
    FCompilerVersion : TCompilerVersion;
    FId : string;
    FPlatform : TDPMPlatform;
    FVersion : TPackageVersion;
    FSourceName : string;
  protected
    function GetCompilerVersion : TCompilerVersion;
    function GetId : string;
    function GetPlatform : TDPMPlatform;
    function GetVersion : TPackageVersion;
    function ToIdVersionString : string; virtual;
    function GetSourceName : string;
    constructor Create(const sourceName : string; const manifest : IPackageManifest); overload;virtual;
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);overload;virtual;

  public
    constructor Create(const sourceName : string; const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform); overload; virtual;
    class function TryCreateFromString(const logger : ILogger; const value : string; const source : string; out packageIdentity : IPackageIdentity) : boolean;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageIdentity : IPackageIdentity) : boolean;
    function ToString : string; override; //note - do not override below - we rely on this. should probably create a new property
  end;

  TPackageInfo = class(TPackageIdentity, IPackageInfo, IPackageIdentity)
  private
    FDependencies : IList<IPackageDependency>;
    FUseSource : boolean;
    FHash : string;
    FHashAlgorithm : string;
  protected
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);
    function GetHash : string;
    function GetHashAlgorithm : string;

    constructor Create(const sourceName : string; const manifest : IPackageManifest; const hash : string; const hashAlgorithm : string);overload;
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);override;
  public
    destructor Destroy;override;
    constructor Create(const sourceName : string; const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const hash : string; const hashAlgorithm : string); overload; virtual;
    class function CreateFromManifest(const sourceName : string; const manifest : IPackageManifest; const hash : string; const hashAlgorith : string) : IPackageInfo;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageInfo : IPackageInfo) : boolean;

  end;

  TPackageMetadata = class(TPackageInfo, IPackageMetadata, IPackageInfo, IPackageIdentity)
  private
    FAuthors : string;
    FCopyright : string;
    FDescription : string;
    FIcon : string;
    FIsCommercial : Boolean;
    FIsTrial : Boolean;
    FLicense : string;
    FTags : string;
    FSearchPaths : IList<string>;
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
    function GetSearchPaths : IList<string>;
    function GetProjectUrl : string;
    function GetRepositoryUrl: string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    constructor Create(const sourceName : string; const manifest : IPackageManifest); override;
  public
    constructor Create(const sourceName : string; const jsonObj : TJsonObject); override;
    destructor Destroy;override;
    class function CreateFromManifest(const sourceName : string; const manifest : IPackageManifest) : IPackageMetadata;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageMetadata : IPackageMetadata) : boolean;
  end;





implementation

uses
  DPM.Core.Constants,
  DPM.Core.Manifest.Reader,
  DPM.Core.Package.Dependency,
  VSoft.Base64,
  System.SysUtils,
  System.RegularExpressions,
  System.Zip,
  System.Classes, DPM.Core.Utils.Hash;

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


  Create(sourceName,  id, packageVersion, cv, platform);


end;

constructor TPackageIdentity.Create(const sourceName : string; const id: string; const version: TPackageVersion; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform);
begin
  inherited Create;
  FId := id;
  FVersion := version;
  FCompilerVersion := compilerVersion;
  FPlatform := platform;
  FSourceName := sourceName;
end;

constructor TPackageIdentity.Create(const sourceName: string; const manifest: IPackageManifest);
begin
  Create(sourceName, manifest.MetaData.Id, manifest.MetaData.Version, manifest.TargetPlatform.Compiler, manifest.TargetPlatform.Platforms[0]);
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
  logger.Debug('TRegEx.Match : [' + value + '] regex : ' + cPackageFileRegex);
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


constructor TPackageInfo.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  depArr : TJsonArray;
  depId : string;
  depVersion : string;
  i: Integer;
  range : TVersionRange;
  dependency : IPackageDependency;
  bytes : TBytes;
begin
  inherited Create(sourceName, jsonObj);
  FDependencies := TCollections.CreateList<IPackageDependency>;

  FHash := jsonObj.S['hash']; //base64 - need ot convert it to hex.
  FHashAlgorithm := jsonObj.S['hashAlgorithm'];

  if FHash <> '' then
  begin
    bytes := TBase64.Decode(FHash);
    FHash := THashSHA256.DigestAsString(bytes);
  end;

  //check for isnull is needed due to jd barfing on nulls
  if jsonObj.Contains('dependencies') and (not jsonObj.IsNull('dependencies')) then
  begin
    depArr := jsonObj.A['dependencies'];
    for i := 0 to depArr.Count -1 do
    begin
      depId := depArr.O[i].S['packageId'];
      depVersion := depArr.O[i].S['versionRange'];
      if TVersionRange.TryParse(depVersion, range) then
      begin
        dependency := TPackageDependency.Create(depId, range, FPlatform);
        FDependencies.Add(dependency);
      end;
    end;
  end;

end;

constructor TPackageInfo.Create(const sourceName: string; const manifest: IPackageManifest; const hash : string; const hashAlgorithm : string);
var
  dep : ISpecDependency;
  newDep : IPackageDependency;
begin
  inherited Create(sourceName, manifest);
  FDependencies := TCollections.CreateList<IPackageDependency>;
  FHash := hash;
  FHashAlgorithm := hashAlgorithm;

  for dep in manifest.TargetPlatform.Dependencies do
  begin
    newDep := TPackageDependency.Create(dep.Id, dep.Version, FPlatform);
    FDependencies.Add(newDep);
  end;

end;

constructor TPackageInfo.Create(const sourceName, id: string; const version: TPackageVersion; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const hash, hashAlgorithm: string);
begin
  inherited Create(sourceName, id, version, compilerVersion, platform);
  FHash := hash;
  FHashAlgorithm := hashAlgorithm;

end;

class function TPackageInfo.CreateFromManifest(const sourceName: string; const manifest : IPackageManifest; const hash : string; const hashAlgorith : string): IPackageInfo;
begin
  result := TPackageInfo.Create(sourceName, manifest, hash, hashAlgorith);
end;

destructor TPackageInfo.Destroy;
begin
  FDependencies := nil;
  inherited;
end;

function TPackageInfo.GetDependencies : IList<IPackageDependency>;
begin
  result := FDependencies;
end;


function TPackageInfo.GetHash: string;
begin
  result := FHash;
end;

function TPackageInfo.GetHashAlgorithm: string;
begin
  result := FHashAlgorithm;
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
      packageInfo := nil;
      logger.Error(e.Message);
      exit;
    end;
  end;
  result := true;
end;


constructor TPackageMetadata.Create(const sourceName : string; const manifest : IPackageManifest);
var
  specSearchPath : ISpecSearchPath;
begin
  inherited Create(sourceName, manifest);
  FSearchPaths := TCollections.CreateList<string>;
  FAuthors := manifest.MetaData.Authors;
  FCopyright := manifest.MetaData.Copyright;
  FDescription := manifest.MetaData.Description;
  FIcon := manifest.MetaData.Icon;
  FIsCommercial := manifest.MetaData.IsCommercial;
  FIsTrial := manifest.MetaData.IsTrial;
  FLicense := manifest.MetaData.License;
  FProjectUrl := manifest.MetaData.ProjectUrl;
  FTags := manifest.MetaData.Tags;
  FProjectUrl := manifest.MetaData.ProjectUrl;
  FRepositoryUrl := manifest.MetaData.RepositoryUrl;
  FRepositoryType := manifest.MetaData.RepositoryType;
  FRepositoryBranch := manifest.MetaData.RepositoryBranch;
  FRepositoryCommit := manifest.MetaData.RepositoryCommit;

  for specSearchPath in manifest.TargetPlatform.SearchPaths do
    FSearchPaths.Add(specSearchPath.Path);
end;



constructor TPackageMetadata.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  searchPaths : string;
  sList : TStringList;
  i: Integer;
begin
  inherited Create(sourceName, jsonObj);
  FSearchPaths := TCollections.CreateList<string>;

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
  searchPaths       := jsonObj.S['searchPaths'];

  if searchPaths <> '' then
  begin
    sList := TStringList.Create;
    try
      sList.Delimiter := ';';
      sList.DelimitedText := searchPaths;
      for i := 0 to sList.Count -1 do
        FSearchPaths.Add(sList.Strings[i]);
    finally
      sList.Free;
    end;
  end;
end;

class function TPackageMetadata.CreateFromManifest(const sourceName: string; const manifest : IPackageManifest): IPackageMetadata;
begin
  result := TPackageMetadata.Create(sourceName, manifest );
end;


destructor TPackageMetadata.Destroy;
begin
  FSearchPaths := nil;
  inherited;
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

function TPackageMetadata.GetSearchPaths : IList<string>;
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

function TPackageIdentity.GetCompilerVersion : TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TPackageIdentity.GetId : string;
begin
  result := FId;
end;

function TPackageIdentity.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

function TPackageIdentity.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;

function TPackageIdentity.ToIdVersionString : string;
begin
  result := FId + ' [' + FVersion.ToStringNoMeta + ']';
end;

function TPackageIdentity.ToString : string;
begin
  result := FId + '-' + CompilerToString(FCompilerVersion) + '-' + DPMPlatformToString(FPlatform) + '-' + FVersion.ToStringNoMeta;
end;


end.

