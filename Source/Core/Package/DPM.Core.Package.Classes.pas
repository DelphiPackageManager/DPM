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
  System.Classes,
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.Interfaces;

type
  TPackageIdentity = class(TInterfacedObject, IPackageIdentity)
  private
    FCompilerVersion : TCompilerVersion;
    FId : string;
    FVersion : TPackageVersion;
    FSourceName : string;
  protected
    function GetCompilerVersion : TCompilerVersion;
    function GetId : string;
    function GetVersion : TPackageVersion;
    function ToIdVersionString : string; virtual;
    function GetSourceName : string;
    constructor Create(const sourceName : string; const manifest : IPackageSpec); overload;virtual;
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);overload;virtual;

  public
    constructor Create(const sourceName : string; const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion); overload; virtual;
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
    FSupportedPlatforms : TDPMPlatforms;
  protected
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);
    function GetHash : string;
    function GetHashAlgorithm : string;
    function GetSupportedPlatforms : TDPMPlatforms;
    procedure SetSupportedPlatforms(const value : TDPMPlatforms);

    constructor Create(const sourceName : string; const manifest : IPackageSpec; const hash : string; const hashAlgorithm : string);overload;
    constructor Create(const sourceName : string; const jsonObj : TJsonObject);override;
  public
    destructor Destroy;override;
    constructor Create(const sourceName : string; const id : string; const version : TPackageVersion; const compilerVersion : TCompilerVersion; const hash : string; const hashAlgorithm : string); overload; virtual;
    class function CreateFromManifest(const sourceName : string; const manifest : IPackageSpec; const hash : string; const hashAlgorith : string) : IPackageInfo;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageInfo : IPackageInfo) : boolean;

  end;

  TPackageMetadata = class(TPackageInfo, IPackageMetadata, IPackageInfo, IPackageIdentity)
  private
    FAuthors : IList<string>;
    FCopyright : string;
    FDescription : string;
    FIcon : string;
    FIsCommercial : Boolean;
    FIsTrial : Boolean;
    FLicense : string;
    FTags : TStringList;
    FSearchPaths : IList<string>;
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
    FReleaseNotes : string;
    FReadMe : string;
    FFrameworks : TArray<TDPMUIFrameworkType>;
  protected
    function GetAuthors : IList<string>;
    function GetCopyright : string;
    function GetDescription : string;
    function GetIcon : string;
    function GetIsCommercial : Boolean;
    function GetIsTrial : Boolean;
    function GetLicense : string;
    function GetTags : TStrings;
    function GetSearchPaths : IList<string>;
    function GetProjectUrl : string;
    function GetRepositoryUrl: string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetReleaseNotes : string;
    function GetReadMe : string;
    function GetFrameworks : TArray<TDPMUIFrameworkType>;
    ///<summary>Populates this package's metadata fields from the spec's metadata — the single place
    /// where ISpecMetaData → IPackageMetadata field-copying lives. Allocates FAuthors/FTags first so
    /// it is also safe to call on an instance created via the json constructor.</summary>
    procedure AssignFromSpecMetaData(const source : ISpecMetaData);
    constructor Create(const sourceName : string; const manifest : IPackageSpec); override;
  public
    constructor Create(const sourceName : string; const jsonObj : TJsonObject); override;
    destructor Destroy;override;
    class function CreateFromManifest(const sourceName : string; const manifest : IPackageSpec) : IPackageMetadata;
    class function TryLoadFromJson(const logger : ILogger; const jsonObj : TJsonObject; const source : string; out packageMetadata : IPackageMetadata) : boolean;
  end;





implementation

uses
  DPM.Core.Constants,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.Dependency,
  VSoft.Base64,
  System.SysUtils,
  System.RegularExpressions,
  System.Zip,
  DPM.Core.Utils.Hash;

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


  Create(sourceName,  id, packageVersion, cv);


end;

constructor TPackageIdentity.Create(const sourceName : string; const id: string; const version: TPackageVersion; const compilerVersion: TCompilerVersion);
begin
  inherited Create;
  FId := id;
  FVersion := version;
  FCompilerVersion := compilerVersion;
  FSourceName := sourceName;
  
end;

constructor TPackageIdentity.Create(const sourceName: string; const manifest: IPackageSpec);
begin
  inherited Create;
  if manifest = nil then
    raise Exception.Create('Package manifest is nil');
  if manifest.TargetPlatform = nil then
    raise Exception.Create('Package manifest [' + manifest.MetaData.Id + '] has no target platforms');

  FSourceName := sourceName;
  FId := manifest.MetaData.Id;
  FVersion := manifest.MetaData.version;
  FCompilerVersion := manifest.TargetPlatform.Compiler;
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
  // Platform segment (Group 3) can be binary string (e.g., '11') or pipe-separated (e.g., 'win32|win64')
  // IPackageIdentity doesn't store platform, so we just validate it's non-empty
  if match.Groups[3].Value = '' then
  begin
    logger.Error('Platform segment is empty in package name [' + value + ']');
    exit;
  end;
  if not TPackageVersion.TryParse(match.Groups[4].Value, packageVersion) then
  begin
    logger.Error('Version segment is not a valid version [' + match.Groups[4].Value + ']');
    exit;
  end;

  packageIdentity := TPackageIdentity.Create(source, id,  packageVersion, cv);
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

  //new per-compiler JSON shape: server returns a comma/pipe-separated 'platforms' list.
  //missing/empty is tolerated so feeds that haven't adopted the field yet don't break clients.
  if jsonObj.Contains('platforms') then
    FSupportedPlatforms := StringToDPMPlatforms(jsonObj.S['platforms']);

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
        dependency := TPackageDependency.Create(depId, range);
        FDependencies.Add(dependency);
      end;
    end;
  end;

end;

constructor TPackageInfo.Create(const sourceName: string; const manifest: IPackageSpec; const hash : string; const hashAlgorithm : string);
var
  template: ISpecTemplate;
  dep: ISpecDependency;
  newDep: IPackageDependency;
  bytes: TBytes;
  i: integer;
begin
  inherited Create(sourceName, manifest);
  FDependencies := TCollections.CreateList<IPackageDependency>;
  FHash := hash;
  FHashAlgorithm := hashAlgorithm;
  if FHash <> '' then
  begin
    bytes := TBase64.Decode(FHash);
    FHash := THashSHA256.DigestAsString(bytes);
  end;

  // Get dependencies from the template referenced by TargetPlatform
  if manifest.TargetPlatform <> nil then
  begin
    FSupportedPlatforms := manifest.TargetPlatform.Platforms;
    template := manifest.FindTemplate(manifest.TargetPlatform.TemplateName);
    if template <> nil then
    begin
      for i := 0 to template.Dependencies.Count - 1 do
      begin
        dep := template.Dependencies[i];
        newDep := TPackageDependency.Create(dep.Id, dep.Version);
        FDependencies.Add(newDep);
      end;
    end;
  end;
end;

constructor TPackageInfo.Create(const sourceName, id: string; const version: TPackageVersion; const compilerVersion: TCompilerVersion; const hash, hashAlgorithm: string);
begin
  inherited Create(sourceName, id, version, compilerVersion);
  FHash := hash;
  FHashAlgorithm := hashAlgorithm;

end;

class function TPackageInfo.CreateFromManifest(const sourceName: string; const manifest : IPackageSpec; const hash : string; const hashAlgorith : string): IPackageInfo;
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

function TPackageInfo.GetSupportedPlatforms : TDPMPlatforms;
begin
  result := FSupportedPlatforms;
end;

procedure TPackageInfo.SetSupportedPlatforms(const value : TDPMPlatforms);
begin
  FSupportedPlatforms := value;
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


constructor TPackageMetadata.Create(const sourceName : string; const manifest : IPackageSpec);
begin
  inherited Create(sourceName, manifest, '', '');
  FSearchPaths := TCollections.CreateList<string>;
  FTags := TStringList.Create;
  FAuthors := TCollections.CreateList<string>;
  AssignFromSpecMetaData(manifest.MetaData);
end;

procedure TPackageMetadata.AssignFromSpecMetaData(const source : ISpecMetaData);
begin
  FAuthors.Clear;
  if source.Authors <> nil then
    FAuthors.AddRange(source.Authors);
  FCopyright := source.Copyright;
  FDescription := source.Description;
  FIcon := source.Icon;
  FIsCommercial := source.IsCommercial;
  FIsTrial := source.IsTrial;
  FLicense := source.License;
  FProjectUrl := source.ProjectUrl;
  FTags.Assign(source.Tags);
  FRepositoryUrl := source.RepositoryUrl;
  FRepositoryType := source.RepositoryType;
  FRepositoryBranch := source.RepositoryBranch;
  FRepositoryCommit := source.RepositoryCommit;
  FReleaseNotes := source.ReleaseNotes;
  FReadMe := source.ReadMe;
  FFrameworks := source.Frameworks;
end;


constructor TPackageMetadata.Create(const sourceName: string; const jsonObj: TJsonObject);
var
  searchPaths : string;
  sList : TStringList;
  i: Integer;
  authorsStr : string;
begin
  inherited Create(sourceName, jsonObj);
  FSearchPaths := TCollections.CreateList<string>;
  FAuthors := TCollections.CreateList<string>;
  FTags := TStringList.Create;

  //server JSON has historically sent authors as a comma-joined string; keep parsing that way
  //so existing feeds still work but expose the list shape through the interface.
  authorsStr := jsonObj.S['authors'];
  if authorsStr <> '' then
  begin
    sList := TStringList.Create;
    try
      sList.Delimiter := ',';
      sList.StrictDelimiter := true;
      sList.DelimitedText := authorsStr;
      for i := 0 to sList.Count - 1 do
        FAuthors.Add(Trim(sList[i]));
    finally
      sList.Free;
    end;
  end;

  FCopyright      := jsonObj.S['copyright'];
  FDescription    := jsonObj.S['description'];
  FIcon           := jsonObj.S['icon'];
  FIsCommercial   := jsonObj.B['isCommercial'];
  FIsTrial        := jsonObj.B['isTrial'];
  FLicense        := jsonObj.S['License'];
  FProjectUrl     := jsonObj.S['ProjectUrl'];
  if jsonObj.Contains('Tags')  then
    FTags.Text    := jsonObj.S['Tags'];
  FRepositoryUrl  := jsonObj.S['RepositoryUrl'];
  FRepositoryType := jsonObj.S['RepositoryType'];
  FRepositoryBranch := jsonObj.S['RepositoryBranch'];
  FRepositoryCommit := jsonObj.S['RepositoryCommit'];
  FReleaseNotes   := jsonObj.S['releaseNotes'];
  FReadMe         := jsonObj.S['readme'];
  searchPaths     := jsonObj.S['searchPaths'];

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

class function TPackageMetadata.CreateFromManifest(const sourceName: string; const manifest : IPackageSpec): IPackageMetadata;
begin
  result := TPackageMetadata.Create(sourceName, manifest);
end;


destructor TPackageMetadata.Destroy;
begin
  FreeAndNil(FTags);
  FSearchPaths := nil;
  inherited;
end;

function TPackageMetadata.GetAuthors : IList<string>;
begin
  result := FAuthors;
end;

function TPackageMetadata.GetCopyright : string;
begin
  result := FCopyright;
end;

function TPackageMetadata.GetFrameworks : TArray<TDPMUIFrameworkType>;
begin
  result := FFrameworks;
end;

function TPackageMetadata.GetReadMe : string;
begin
  result := FReadMe;
end;

function TPackageMetadata.GetReleaseNotes : string;
begin
  result := FReleaseNotes;
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
  result := FRepositoryType;
end;

function TPackageMetadata.GetRepositoryUrl: string;
begin
  result := FRepositoryUrl;
end;

function TPackageMetadata.GetSearchPaths : IList<string>;
begin
  result := FSearchPaths;
end;

function TPackageMetadata.GetTags : TStrings;
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
  result := FId + '-' + CompilerToString(FCompilerVersion) + '-' + FVersion.ToStringNoMeta;
end;


end.

