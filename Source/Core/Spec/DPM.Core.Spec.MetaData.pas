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

unit DPM.Core.Spec.MetaData;

interface

uses
  System.Classes,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecMetaData = class(TSpecNode, ISpecMetaData)
  private
    FPackageKind : TDPMPackageKind;
    FId : string;
    FVersion : TPackageVersion;
    FDescription : string;
    FAuthors : IList<string>;
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
    FReleaseNotes : string;
    FLicense : string;
    FIcon : string;
    FCopyright : string;
    FTags : TStringList;
    FIsTrial : boolean;
    FIsCommercial : boolean;
    FReadme : string;
    FFrameworks : TArray<TDPMUIFrameworkType>;
  protected
    function GetVersion : TPackageVersion;
    function GetId : string;
    function GetDescription : string;
    function GetAuthors : IList<string>;
    function GetProjectUrl : string;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetReleaseNotes : string;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : TStrings;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetReadMe : string;
    function GetFrameworks : TArray<TDPMUIFrameworkType>;
    function GetPackageKind : TDPMPackageKind;

    procedure SetPackageKind(const value : TDPMPackageKind);
    procedure SetId(const value : string);
    procedure SetDescription(const value : string);
    procedure SetProjectUrl(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetReleaseNotes(const value : string);
    procedure SetLicense(const value : string);
    procedure SetIcon(const value : string);
    procedure SetCopyright(const value : string);
    procedure SetTags(const value : TStrings);
    procedure SetIsTrial(const value : boolean);
    procedure SetIsCommercial(const value : boolean);
    procedure SetVersion(const value : TPackageVersion);
    procedure SetReadMe(const value : string);
    procedure SetFrameworks(const value : TArray<TDPMUIFrameworkType>);

    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    procedure ToYAML(const parentObj : IYAMLValue; const packageKind : TDPMPackageKind);override;

    function Clone : ISpecMetaData;

    public constructor CreateClone(const logger : ILogger; const source : ISpecMetadata);
  public
    constructor Create(const logger : ILogger; const packageKind : TDPMPackageKind);reintroduce;
    destructor Destroy;override;

  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Packaging.IdValidator;

{ TSpecMetaData }

function TSpecMetaData.Clone: ISpecMetaData;
begin
  result := TSpecMetaData.CreateClone(Logger, self);
end;

constructor TSpecMetaData.Create(const logger : ILogger; const packageKind : TDPMPackageKind);
begin
  inherited Create(logger);
  FPackageKind := packageKind;
  FVersion := TPackageVersion.Empty;
  FFrameworks := nil;
  FRepositoryCommit := '#HASH#'; //git replacment.
  FTags := TStringList.Create;
  FAuthors := TCollections.CreateList<string>;
end;


constructor TSpecMetaData.CreateClone(const logger: ILogger; const source: ISpecMetadata);
begin
  inherited Create(logger);
  FPackageKind := source.PackageKind;
  FId := source.Id;
  FVersion := source.Version;
  FDescription := source.Description;
  FAuthors := TCollections.CreateList<string>;
  FAuthors.AddRange(source.Authors);
  FProjectUrl := source.ProjectUrl;
  FRepositoryUrl := source.RepositoryUrl;
  FRepositoryType := source.RepositoryType;
  FRepositoryBranch := source.RepositoryBranch;
  FRepositoryCommit := source.RepositoryCommit;
  FReleaseNotes := source.ReleaseNotes;
  FLicense := source.License;
  FIcon := source.Icon;
  FCopyright := source.Copyright;

  FTags := TStringList.Create;
  FTags.Assign(source.Tags);
  FIsTrial := source.IsTrial;
  FIsCommercial := source.IsCommercial;
  FReadme := source.ReadMe;
  FFrameworks := source.Frameworks;
end;

destructor TSpecMetaData.Destroy;
begin
  FreeAndNil(FTags);
  inherited;
end;

function TSpecMetaData.GetAuthors : IList<string>;
begin
  result := FAuthors;
end;

function TSpecMetaData.GetCopyright : string;
begin
  result := FCopyright;
end;

function TSpecMetaData.GetDescription : string;
begin
  result := FDescription;
end;



function TSpecMetaData.GetFrameworks: TArray<TDPMUIFrameworkType>;
begin
  result := FFrameworks;
end;

function TSpecMetaData.GetIcon : string;
begin
  result := FIcon;
end;

function TSpecMetaData.GetId : string;
begin
  result := FId;
end;

function TSpecMetaData.GetIsCommercial : boolean;
begin
  result := FIsCommercial;
end;

function TSpecMetaData.GetIsTrial : boolean;
begin
  result := FIsTrial;
end;

function TSpecMetaData.GetLicense : string;
begin
  result := FLicense;
end;


function TSpecMetaData.GetPackageKind: TDPMPackageKind;
begin
  result := FPackageKind;
end;

function TSpecMetaData.GetProjectUrl : string;
begin
  result := FProjectUrl;
end;

function TSpecMetaData.GetReadMe: string;
begin
  result := FReadme;
end;

function TSpecMetaData.GetReleaseNotes: string;
begin
  result := FReleaseNotes;
end;

function TSpecMetaData.GetRepositoryBranch: string;
begin
  result := FRepositoryBranch;
end;

function TSpecMetaData.GetRepositoryCommit: string;
begin
  result := FRepositoryCommit;
end;

function TSpecMetaData.GetRepositoryType: string;
begin
  result := FRepositoryType;
end;

function TSpecMetaData.GetRepositoryUrl: string;
begin
  result := FRepositoryUrl;
end;

function TSpecMetaData.GetTags : TStrings;
begin
  result := FTags;
end;


function TSpecMetaData.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;

function TSpecMetaData.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  sVersion : string;
  sError : string;
  sUI : string;
  authorsSeq : IYAMLSequence;
  tagsSeq : IYAMLSequence;
  i: Integer;
  frameworksSeq : IYAMLSequence;
  framework : TDPMUIFrameworkType;
begin
  result := true;
  //preserve comments;
  LoadComments(yamlObject);

  FId := yamlObject.S['id'];
  if not TPackageIdValidator.IsValidPackageId(FId) then
  begin
    Logger.Error('Invalid package Id [' + FId + ']');
    result := false;
  end;

  //version is required for dpm packages. For git-registry packages the resolvable versions come
  //from the repo's git tags (the dspec's version is set per-tag at resolve time), so the field is
  //optional here - we still read it when present.
  sVersion := yamlObject.S['version'];
  if (sVersion = '') and (FPackageKind = TDPMPackageKind.dpm) then
  begin
    logger.Error('Required field [version] not found.');
    result := false;
  end;
  if sVersion <> '' then
  begin
    if not TPackageVersion.TryParseWithError(sVersion, FVersion, sError) then
    begin
      logger.Error('Invalid Package Version : ' + sError);
      result := false;
    end;
  end;

  FDescription := yamlObject.S['description'];
  if FDescription = '' then
  begin
    logger.Error('Required field [description] not found.');
    result := false;
  end;

  authorsSeq := yamlObject.A['authors'];
  if authorsSeq.Count = 0 then
  begin
    logger.Error('Required field [authors] not found.');
    result := false;
  end;

  for i := 0 to authorsSeq.Count -1 do
    FAuthors.Add(authorsSeq.S[i]);

  FProjectUrl := yamlObject.S['projectUrl'];
  FRepositoryUrl := yamlObject.S['repositoryUrl'];
  FRepositoryType := yamlObject.S['repositoryType'];
  FRepositoryBranch := yamlObject.S['repositoryBranch'];
  FRepositoryCommit := yamlObject.S['repositoryCommit'];
  FReleaseNotes := yamlObject.S['releaseNotes'];
  FLicense := yamlObject.S['license'];
  FIcon := yamlObject.S['icon'];
  FCopyright := yamlObject.S['copyright'];
  FReadme := yamlObject.S['readme'];

  tagsSeq := yamlObject.A['tags'];
  for i := 0 to tagsSeq.Count - 1 do
    FTags.Add(tagsSeq.S[i]);

  FIsTrial := yamlObject.B['isTrial'];
  FIsCommercial := yamlObject.B['isCommercial'];

  frameworksSeq := yamlObject.A['frameworks'];
  if frameworksSeq.Count > 0 then
  begin
    for i := 0 to frameworksSeq.Count -1 do
    begin
      sUI := frameworksSeq.S[i];
      frameWork := StringToUIFrameworkType(sUI);
      if framework <> TDPMUIFrameworkType.None then
      begin
        SetLength(FFrameworks, Length(FFrameworks) + 1);
        FFrameworks[Length(FFrameworks) - 1] := framework;
      end;
    end;
  end;


//  sLicenseType := yamlObject.S['licenseType'];
//  if sLicenseType <> '' then
//    FLicenseType := StringToLicenseType(sLicenseType);



end;


procedure TSpecMetaData.SetCopyright(const value : string);
begin
  FCopyright := value;
end;

procedure TSpecMetaData.SetDescription(const value : string);
begin
  FDescription := value;
end;

procedure TSpecMetaData.SetFrameworks(const value: TArray<TDPMUIFrameworkType>);
begin
  FFrameworks := value;
end;

procedure TSpecMetaData.SetIcon(const value : string);
begin
  FIcon := value;
end;

procedure TSpecMetaData.SetId(const value : string);
begin
  FId := value;
end;

procedure TSpecMetaData.SetIsCommercial(const value : boolean);
begin
  FIsCommercial := value;
end;

procedure TSpecMetaData.SetIsTrial(const value : boolean);
begin
  FIsTrial := value;
end;

procedure TSpecMetaData.SetLicense(const value : string);
begin
  FLicense := value;
end;


procedure TSpecMetaData.SetPackageKind(const value: TDPMPackageKind);
begin
  FPackageKind := value;
end;

procedure TSpecMetaData.SetProjectUrl(const value : string);
begin
  FProjectUrl := value;
end;

procedure TSpecMetaData.SetReadMe(const value: string);
begin
  FReadme := value;
end;

procedure TSpecMetaData.SetReleaseNotes(const value: string);
begin
  FReleaseNotes := value;
end;

procedure TSpecMetaData.SetRepositoryBranch(const value: string);
begin
  FRepositoryBranch := value;
end;

procedure TSpecMetaData.SetRepositoryCommit(const value: string);
begin
  FRepositoryCommit := value;
end;

procedure TSpecMetaData.SetRepositoryType(const value: string);
begin
  FRepositoryType := value;
end;

procedure TSpecMetaData.SetRepositoryUrl(const value: string);
begin
  FRepositoryUrl := value;
end;

procedure TSpecMetaData.SetTags(const value : TStrings);
begin
  FTags.Assign(value);
end;

procedure TSpecMetaData.SetVersion(const value : TPackageVersion);
begin
  FVersion := value;
end;

procedure TSpecMetaData.ToYAML(const parentObj: IYAMLValue; const packageKind : TDPMPackageKind);
var
  metaData : IYAMLMapping;
  tagsSeq : IYAMLSequence;
  authorsSeq : IYAMLSequence;
  frameworksSeq : IYAMLSequence;
  i: Integer;
begin
  metaData := parentObj.AsMapping.O['metadata'];
  metaData.S['id'] := FId;

  //Both dpm and git-registry packages carry a single semantic version. For git packages the
  //resolvable versions come from the repo's git tags; the dspec version is informational and is
  //overwritten per-tag at resolve time.
  metaData.S['version'] := FVersion.ToString;

  metaData.S['description'] := FDescription;
  if Length(FIcon) > 0 then
    metaData.S['icon'] := FIcon;

  authorsSeq := metaData.A['authors'];
  for i := 0 to FAuthors.Count -1 do
    authorsSeq.AddValue(FAuthors[i]);

  if FProjectUrl <> '' then
    metaData.S['projectUrl'] := FProjectUrl;
  //these were previously either guarded by the wrong field (repositoryUrl was only written when a
  //commit was set) or not written at all (repositoryType/Branch, releaseNotes, frameworks), so the
  //DSpecCreator silently dropped them on save. The read side already expects these keys.
  if FRepositoryUrl <> '' then
    metaData.S['repositoryUrl'] := FRepositoryUrl;
  if FRepositoryType <> '' then
    metaData.S['repositoryType'] := FRepositoryType;
  if FRepositoryBranch <> '' then
    metaData.S['repositoryBranch'] := FRepositoryBranch;
  if FRepositoryCommit <> '' then
    metaData.S['repositoryCommit'] := FRepositoryCommit;
  metaData.S['license'] := FLicense;
  if FCopyright <> '' then
    metaData.S['copyright'] := FCopyright;
  if FReleaseNotes <> '' then
    metaData.S['releaseNotes'] := FReleaseNotes;

  if FTags.Count > 0 then
  begin
    tagsSeq := metaData.AddOrSetSequence('tags');
    for i := 0 to FTags.Count -1 do
      tagsSeq.AddValue(FTags.Strings[i]);
  end;

  if Length(FFrameworks) > 0 then
  begin
    frameworksSeq := metaData.AddOrSetSequence('frameworks');
    for i := 0 to Length(FFrameworks) - 1 do
      frameworksSeq.AddValue(UIFrameworkTypeToString(FFrameworks[i]));
  end;

  //read side uses the camelCase keys (isTrial/isCommercial) - the lowercase keys written before
  //never round-tripped.
  if FIsTrial then
    metaData.B['isTrial'] := FIsTrial;
  if FIsCommercial then
    metaData.B['isCommercial'] := FIsCommercial;

  if Length(FReadme) > 0 then
    metaData.S['readme'] := FReadme;

end;

end.

