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
  JsonDataObjects,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecVersion = class(TInterfacedObject, ISpecVersion)
  private
    FVersion : TPackageVersion;
    FCommit : string;
  protected
    function GetVersion : TPackageVersion;
    procedure SetVersion(version : TPackageVersion);
    function GetCommit  : string;
    procedure SetCommit(const value : string);
  public
    constructor Create(version : TPackageVersion; const commit : string);
  end;

  TSpecMetaData = class(TSpecNode, ISpecMetaData)
  private
    FPackageKind : TDPMPackageKind;
    FId : string;
    FVersion : TPackageVersion;
    FVersions : IList<ISpecVersion>;
    FDescription : string;
    FAuthors : string;
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
    function GetVersions : IList<ISpecVersion>;
    function GetVersion : TPackageVersion;
    function GetId : string;
    function GetDescription : string;
    function GetAuthors : string;
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
    procedure SetAuthors(const value : string);
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

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;override;

    function ToJSON : string; override;
    procedure ToYAML(const parentObj : IYAMLValue; const packageKind : TDPMPackageKind);override;

  public
    constructor Create(const logger : ILogger; const packageKind : TDPMPackageKind);reintroduce;
    destructor Destroy;override;

  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Packaging.IdValidator;

{ TSpecMetaData }

constructor TSpecMetaData.Create(const logger : ILogger; const packageKind : TDPMPackageKind);
begin
  inherited Create(logger);
  FPackageKind := packageKind;
  FVersion := TPackageVersion.Empty;
  FFrameworks := [];
  FRepositoryCommit := '#HASH#'; //git replacment.
  FTags := TStringList.Create;
end;

destructor TSpecMetaData.Destroy;
begin
  FreeAndNil(FTags);
  inherited;
end;

function TSpecMetaData.GetAuthors : string;
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


function TSpecMetaData.GetVersions: IList<ISpecVersion>;
begin
  if FVersions = nil then
    FVersions := TCollections.CreateList<ISpecVersion>;
  result := FVersions;
end;

function TSpecMetaData.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sVersion : string;
  sError : string;
//  sLicenseType : string;
begin
  result := true;
  FId := jsonObject.S['id'];

  if not TPackageIdValidator.IsValidPackageId(FId) then
  begin
    Logger.Error('Invalid package Id [' + FId + ']');
    result := false;
  end;

  sVersion := jsonObject.S['version'];
  if sVersion = '' then
  begin
    logger.Error('Required field [version] not found.');
    result := false;
  end;

  if not TPackageVersion.TryParseWithError(sVersion, FVersion, sError) then
  begin
    logger.Error('Invalid Package Version : ' + sError);
    result := false;
  end;

  FDescription := jsonObject.S['description'];
  if FDescription = '' then
  begin
    logger.Error('Required field [description] not found.');
    result := false;
  end;

  FAuthors := jsonObject.S['authors'];
  if FAuthors = '' then
  begin
    logger.Error('Required field [authors] not found.');
    result := false;
  end;

  FProjectUrl := jsonObject.S['projectUrl'];
  FRepositoryUrl := jsonObject.S['repositoryUrl'];
  FRepositoryType := jsonObject.S['repositoryType'];
  FRepositoryBranch := jsonObject.S['repositoryBranch'];
  FRepositoryCommit := jsonObject.S['repositoryCommit'];
  FReleaseNotes := jsonObject.S['releaseNotes'];
  FLicense := jsonObject.S['license'];
  FIcon := jsonObject.S['icon'];
  FCopyright := jsonObject.S['copyright'];
  FReadme := jsonObject.S['readme'];
  FTags.Delimiter := ' ';
  FTags.DelimitedText := jsonObject.S['tags'];
  FTags.Delimiter := #0;
  FIsTrial := jsonObject.B['isTrial'];
  FIsCommercial := jsonObject.B['isCommercial'];
//  sUI := jsonObject.S['uiFramework'];
//  if sUI <> '' then
//    FUIFrameworkType := StringToUIFrameworkType(sUI);
//  sLicenseType := jsonObject.S['licenseType'];
//  if sLicenseType <> '' then
//    FLicenseType := StringToLicenseType(sLicenseType);
end;


function TSpecMetaData.LoadFromYAML(const yamlObject: IYAMLMapping): boolean;
var
  sVersion : string;
  sError : string;
  sUI : string;
  versionsSeq : IYAMLSequence;
  tagsSeq : IYAMLSequence;
  sCommit : string;
  i: Integer;
  versionObj : IYAMLMapping;
  version : TPackageVersion;
  specVersion : ISpecVersion;
  frameworksSeq : IYAMLSequence;
  framework : TDPMUIFrameworkType;

  procedure EnsureVersions;
  begin
    if FVersions = nil then
      FVersions := TCollections.CreateList<ISpecVersion>;
  end;

begin
  result := true;

  //preserve comments;
  AddComments(yamlObject);

  FId := yamlObject.S['id'];
  if not TPackageIdValidator.IsValidPackageId(FId) then
  begin
    Logger.Error('Invalid package Id [' + FId + ']');
    result := false;
  end;

  case FPackageKind of
    TDPMPackageKind.dpm:
    begin
      sVersion := yamlObject.S['version'];
      if sVersion = '' then
      begin
        logger.Error('Required field [version] not found.');
        result := false;
      end;
      if not TPackageVersion.TryParseWithError(sVersion, FVersion, sError) then
      begin
        logger.Error('Invalid Package Version : ' + sError);
        result := false;
      end;

    end;
    TDPMPackageKind.git:
    begin
      versionsSeq := yamlObject.A['versions'];
      if versionsSeq.Count > 0 then
      begin
        for i := 0 to versionsSeq.Count -1 do
        begin
          versionObj := versionsSeq.Items[i].AsMapping;
          sVersion := versionObj.S['version'];
          if sVersion = '' then
          begin
            logger.Error('Required field [version] not found.');
            result := false;
            continue;
          end;
          if not TPackageVersion.TryParseWithError(sVersion, version, sError) then
          begin
            logger.Error('Invalid Package Version : ' + sError);
            result := false;
            continue;
          end;
          sCommit := versionObj.S['commit'];
          if sCommit = '' then
          begin
            logger.Error('Required field [commit] not found.');
            result := false;
            continue;
          end;
          EnsureVersions;
          specVersion := TSpecVersion.Create(version, sCommit);
          FVersions.Add(specVersion);
        end;

        if (FVersions = nil) or (FVersions.Count = 0) then
        begin
          logger.Error('At least 1 version is required.');
          result := false;
        end;
      end
      else
      begin
        logger.Error('Required field [versions] for git package kind not found.');
        result := false;
      end;
    end;
  end;


  FDescription := yamlObject.S['description'];
  if FDescription = '' then
  begin
    logger.Error('Required field [description] not found.');
    result := false;
  end;

  FAuthors := yamlObject.S['authors'];
  if FAuthors = '' then
  begin
    logger.Error('Required field [authors] not found.');
    result := false;
  end;

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

procedure TSpecMetaData.SetAuthors(const value : string);
begin
  FAuthors := value;
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

function TSpecMetaData.ToJSON: string;
var
  json : TJSONObject;
begin
  json := TJSONObject.Create;
  try
    json.S['id'] := FId;
    json.S['version'] := FVersion.ToString;
    json.S['description'] := FDescription;
    if Length(FIcon) > 0 then
      json.S['icon'] := FIcon;
    json.S['authors'] := FAuthors;
    json.S['projectUrl'] := FProjectUrl;
    json.S['repositoryUrl'] := FRepositoryUrl;
    json.S['repositoryCommit'] := FRepositoryCommit;
    json.S['license'] := FLicense;
    json.S['copyright'] := FCopyright;
    if FTags.Count > 0 then
    begin
      FTags.Delimiter := ' ';
      json.S['tags'] := FTags.DelimitedText;
      FTags.Delimiter := ' ';
    end;

    if FIsTrial then
      json.B['istrial'] := FIsTrial;
    if FIsCommercial then
      json.b['iscommercial'] := FIsCommercial;

    if Length(FReadme) > 0 then
      json.S['readme'] := FReadme;

    Result := json.ToJSON;
  finally
    FreeAndNil(json);
  end;
end;

procedure TSpecMetaData.ToYAML(const parentObj: IYAMLValue; const packageKind : TDPMPackageKind);
var
  metaData : IYAMLMapping;
  versions : IYAMLSequence;
  version : IYAMLMapping;
  tagsSeq : IYAMLSequence;
  i: Integer;
begin
  metaData := parentObj.AsMapping.O['metdata'];
  metaData.S['id'] := FId;

  case packageKind of
    TDPMPackageKind.dpm: 
    begin
      metaData.S['version'] := FVersion.ToString;
    end;
    TDPMPackageKind.git:
    begin
      if (FVersions = nil) or (not FVersions.Any) then
        raise Exception.Create('Git style packages must use "versions" with at least 1 version');
      versions := metaData.A['versions'];
      for i := 0 to FVersions.Count -1 do
      begin
        if FVersions[i].Commit = '' then
          raise Exception.Create('commit must be a commit hash');
        if FVersions[i].Version.IsEmpty then
          raise Exception.Create('version must be a valid semantic version');
       
        version := versions.AddMapping;
        version.S['version'] := FVersions[i].Version.ToString;       
        version.S['commit'] := FVersions[i].Commit; 
      end;
    end;
  end;

  metaData.S['description'] := FDescription;
  if Length(FIcon) > 0 then
    metaData.S['icon'] := FIcon;
  metaData.S['authors'] := FAuthors;
  if FProjectUrl <> '' then
    metaData.S['projectUrl'] := FProjectUrl;
  if FRepositoryCommit <> '' then
    metaData.S['repositoryUrl'] := FRepositoryUrl;
  if FRepositoryCommit <> '' then
    metaData.S['repositoryCommit'] := FRepositoryCommit;
  metaData.S['license'] := FLicense;
  if FCopyright <> '' then
    metaData.S['copyright'] := FCopyright;


  if FTags.Count > 0 then
  begin
    tagsSeq := metaData.AddOrSetSequence('tags');
    for i := 0 to FTags.Count -1 do
      tagsSeq.AddValue(FTags.Strings[i]);
  end;
  if FIsTrial then
    metaData.B['istrial'] := FIsTrial;
  if FIsCommercial then
    metaData.b['iscommercial'] := FIsCommercial;

  if Length(FReadme) > 0 then
    metaData.S['readme'] := FReadme;

end;

{ TSpecVersion }

constructor TSpecVersion.Create(version: TPackageVersion; const commit: string);
begin
  FVersion := version;
  FCommit := commit;
end;

function TSpecVersion.GetCommit: string;
begin
  result := FCommit;
end;

function TSpecVersion.GetVersion: TPackageVersion;
begin
  result := FVersion;
end;

procedure TSpecVersion.SetCommit(const value: string);
begin
  FCommit := value;
end;

procedure TSpecVersion.SetVersion(version: TPackageVersion);
begin
  FVersion := version;
end;

end.

