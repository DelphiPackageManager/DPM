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
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Node;

type
  TSpecMetaData = class(TSpecNode, ISpecMetaData)
  private
    FId : string;
    FVersion : TPackageVersion;
    FDescription : string;
    FAuthors : string;
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
    FReleaseNotes : string;
    FLicense : string;
    FLicenseType : TDPMLicenseType;
    FIcon : string;
    FCopyright : string;
    FTags : string;
    FIsTrial : boolean;
    FIsCommercial : boolean;
    FReadme : string;
    FUIFrameworkType : TDPMUIFrameworkType;
  protected
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
    function GetLicenseType : TDPMLicenseType;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : string;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetReadMe : string;
    function GetUIFrameworkType: TDPMUIFrameworkType;


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
    procedure SetLicenseType(const value : TDPMLicenseType);
    procedure SetIcon(const value : string);
    procedure SetCopyright(const value : string);
    procedure SetTags(const value : string);
    procedure SetIsTrial(const value : boolean);
    procedure SetIsCommercial(const value : boolean);
    procedure SetVersion(const value : TPackageVersion);
    procedure SetReadMe(const value : string);
    procedure SetUIFrameworkType(const value: TDPMUIFrameworkType);

    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
    function ToJSON : string; override;

  public
    constructor Create(const logger : ILogger); override;
    destructor Destroy;override;

  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Packaging.IdValidator;

{ TSpecMetaData }

constructor TSpecMetaData.Create(const logger : ILogger);
begin
  inherited Create(logger);
  FVersion := TPackageVersion.Empty;
  FUIFrameworkType := TDPMUIFrameworkType.None;
  FLicenseType := TDPMLicenseType.SPDX;
  FRepositoryCommit := '#HASH#'; //git replacment.
end;

destructor TSpecMetaData.Destroy;
begin
//  FId := '';
//  FDescription := '';
//  FAuthors := '';
//  FProjectUrl := '';
//  FRepositoryUrl := '';
//  FRepositoryType := '';
//  FRepositoryBranch := '';
//  FRepositoryCommit := '';
//  FReleaseNotes := '';
//  FLicense := '';
//  FIcon := '';
//  FCopyright := '';
//  FTags := '';
//  FReadme := '';
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

function TSpecMetaData.GetLicenseType: TDPMLicenseType;
begin
  result := FLicenseType;
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

function TSpecMetaData.GetTags : string;
begin
  result := FTags;
end;

function TSpecMetaData.GetUIFrameworkType: TDPMUIFrameworkType;
begin
  result := FUIFrameworkType;
end;

function TSpecMetaData.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;


function TSpecMetaData.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sVersion : string;
  sError : string;
  sUI : string;
  sLicenseType : string;
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
  FTags := jsonObject.S['tags'];
  FIsTrial := jsonObject.B['isTrial'];
  FIsCommercial := jsonObject.B['isCommercial'];
  sUI := jsonObject.S['uiFramework'];
  if sUI <> '' then
    FUIFrameworkType := StringToUIFrameworkType(sUI);
  sLicenseType := jsonObject.S['licenseType'];
  if sLicenseType <> '' then
    FLicenseType := StringToLicenseType(sLicenseType);
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

procedure TSpecMetaData.SetLicenseType(const value: TDPMLicenseType);
begin
  FLicenseType := value;
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

procedure TSpecMetaData.SetTags(const value : string);
begin
  FTags := value;
end;

procedure TSpecMetaData.SetUIFrameworkType(const value: TDPMUIFrameworkType);
begin
  FUIFrameworkType := value;
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
    json.S['tags'] := FTags;
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

end.

