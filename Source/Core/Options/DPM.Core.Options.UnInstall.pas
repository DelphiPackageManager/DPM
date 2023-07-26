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

unit DPM.Core.Options.UnInstall;

interface
uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  TUninstallOptions = class(TSearchOptions)
  private
    FProjectPath : string;
    FProjects : TArray<string>;
    class var
      FDefault : TUninstallOptions;
  protected
    function GetPackageId : string;
    procedure SetPackageId(const Value : string);
    constructor CreateClone(const original : TUninstallOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TUninstallOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TUninstallOptions; reintroduce;

    property PackageId : string read GetPackageId write SetPackageId;
    property ProjectPath : string read FProjectPath write FProjectPath;
    property Projects : TArray<string> read FProjects write FProjects;
  end;

implementation

{ TRemoveOptions }

function TUninstallOptions.Clone : TUninstallOptions;
begin
  result := TUninstallOptions.CreateClone(self);
end;

constructor TUninstallOptions.Create;
begin
  inherited;

end;

constructor TUninstallOptions.CreateClone(const original : TUninstallOptions);
begin
  inherited CreateClone(original);
  FProjectPath := original.FProjectPath;
end;

class constructor TUninstallOptions.CreateDefault;
begin
  FDefault := TUninstallOptions.Create;
end;

function TUninstallOptions.GetPackageId : string;
begin
  result := SearchTerms;
end;

procedure TUninstallOptions.SetPackageId(const Value : string);
begin
  SearchTerms := value;
end;

function TUninstallOptions.Validate(const logger : ILogger) : Boolean;
begin
  //must call inherited
  result := inherited Validate(logger);

  if (FProjectPath = '') and (Length(FProjects) = 0) then
  begin
    Logger.Error('Project path cannot be empty, must either be a directory or project file.');
    result := false;
  end;

  if PackageId = '' then
  begin
    Logger.Error('The <packageId> option must be specified.');
    result := false;
  end;

  if ConfigFile = '' then
  begin
    Logger.Error('No configuration file specified');
    exit;
  end;

end;

end.

