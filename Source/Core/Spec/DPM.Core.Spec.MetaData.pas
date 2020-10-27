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
    FOwners : string;
    FProjectUrl : string;
    FLicense : string;
    FIcon : string;
    FCopyright : string;
    FTags : string;
    FIsTrial : boolean;
    FIsCommercial : boolean;
  protected
    function GetVersion : TPackageVersion;
    function GetId : string;
    function GetDescription : string;
    function GetAuthors : string;
    function GetOwners : string;
    function GetProjectUrl : string;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : string;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;

    procedure SetId(const value : string);
    procedure SetDescription(const value : string);
    procedure SetAuthors(const value : string);
    procedure SetOwners(const value : string);
    procedure SetProjectUrl(const value : string);
    procedure SetLicense(const value : string);
    procedure SetIcon(const value : string);
    procedure SetCopyright(const value : string);
    procedure SetTags(const value : string);
    procedure SetIsTrial(const value : boolean);
    procedure SetIsCommercial(const value : boolean);
    procedure SetVersion(const value : TPackageVersion);
    function LoadFromJson(const jsonObject : TJsonObject) : Boolean; override;
  public
    constructor Create(const logger : ILogger); override;

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

function TSpecMetaData.GetOwners : string;
begin
  result := FOwners;
end;

function TSpecMetaData.GetProjectUrl : string;
begin
  result := FProjectUrl;
end;

function TSpecMetaData.GetTags : string;
begin
  result := FTags;
end;

function TSpecMetaData.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;


function TSpecMetaData.LoadFromJson(const jsonObject : TJsonObject) : Boolean;
var
  sVersion : string;
  sError : string;
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

  FOwners := jsonObject.S['owners'];
  if FOwners = '' then
    FOwners := FAuthors;

  FProjectUrl := jsonObject.S['projectUrl'];
  FLicense := jsonObject.S['license'];
  FIcon := jsonObject.S['icon'];
  FCopyright := jsonObject.S['copyright'];
  FTags := jsonObject.S['tags'];
  FIsTrial := jsonObject.B['isTrial'];
  FIsCommercial := jsonObject.B['isCommercial'];
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

procedure TSpecMetaData.SetOwners(const value : string);
begin
  FOwners := value;
end;

procedure TSpecMetaData.SetProjectUrl(const value : string);
begin
  FProjectUrl := value;
end;

procedure TSpecMetaData.SetTags(const value : string);
begin
  FTags := value;
end;

procedure TSpecMetaData.SetVersion(const value : TPackageVersion);
begin
  FVersion := value;
end;

end.

