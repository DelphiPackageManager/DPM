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

unit DPM.Core.Options.Sources;

interface

uses
  DPM.Core.Types,
  DPM.Core.Sources.Types,
  DPM.Core.Options.Base;

type
  TSourcesOptions = class(TOptionsBase)
  private
    FSubCommand : TSourcesSubCommand;
    FName : string;
    FSource : string;
    FUserName : string;
    FPassword : string;
    FFormat : TSourcesFormat;
    FSourceType : TSourceType;
    FSourceSpecified : boolean;
    FUserNameSpecified : boolean;
    FPasswordSpecified : boolean;
    FSourceTypeSpecified : boolean;
    procedure SetSource(const value : string);
    procedure SetUserName(const value : string);
    procedure SetPassword(const value : string);
    procedure SetSourceType(const value : TSourceType);
    class var
      FDefault : TSourcesOptions;
  public
    class constructor CreateDefault;
    class destructor DestroyDefault;
    class property Default : TSourcesOptions read FDefault;
    constructor Create; override;

    property Command : TSourcesSubCommand read FSubCommand write FSubCommand;
    property Name : string read FName write FName;
    property Source : string read FSource write SetSource;
    property SourceType : TSourceType read FSourceType write SetSourceType;
    property Format : TSourcesFormat read FFormat write FFormat;
    property UserName : string read FUserName write SetUserName;
    property Password : string read FPassword write SetPassword;
    //whether each was given (set) - update changes only those.
    property SourceSpecified : boolean read FSourceSpecified;
    property SourceTypeSpecified : boolean read FSourceTypeSpecified;
    property UserNameSpecified : boolean read FUserNameSpecified;
    property PasswordSpecified : boolean read FPasswordSpecified;


  end;

implementation

{ TSourcesOptions }

constructor TSourcesOptions.Create;
begin
  inherited;
  FSubCommand := TSourcesSubCommand.List;
  FSourceType := TSourceType.Folder;
end;

class constructor TSourcesOptions.CreateDefault;
begin
  FDefault := TSourcesOptions.Create;
end;

//the singleton goes with the unit - it was never freed, so every run reported it as a leak.
class destructor TSourcesOptions.DestroyDefault;
begin
  FDefault.Free;
end;

procedure TSourcesOptions.SetPassword(const value : string);
begin
  FPassword := value;
  FPasswordSpecified := true;
end;

procedure TSourcesOptions.SetSource(const value : string);
begin
  FSource := value;
  FSourceSpecified := true;
end;

procedure TSourcesOptions.SetSourceType(const value : TSourceType);
begin
  FSourceType := value;
  FSourceTypeSpecified := true;
end;

procedure TSourcesOptions.SetUserName(const value : string);
begin
  FUserName := value;
  FUserNameSpecified := true;
end;

end.

