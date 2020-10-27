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
    class var
      FDefault : TSourcesOptions;
  public
    class constructor CreateDefault;
    class property Default : TSourcesOptions read FDefault;
    constructor Create; override;

    property Command : TSourcesSubCommand read FSubCommand write FSubCommand;
    property Name : string read FName write FName;
    property Source : string read FSource write FSource;
    property SourceType : TSourceType read FSourceType write FSourceType;
    property Format : TSourcesFormat read FFormat write FFormat;
    property UserName : string read FUserName write FUserName;
    property Password : string read FPassword write FPassword;


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

end.

