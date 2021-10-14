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

unit DPM.Core.Options.Restore;

interface

uses
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Options.Search;

type
  TRestoreOptions = class(TSearchOptions)
  private
    FProjectPath : string;
    class var
      FDefault : TRestoreOptions;
  protected
    constructor CreateClone(const original : TRestoreOptions); reintroduce;

  public
    class constructor CreateDefault;
    class property Default : TRestoreOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TRestoreOptions; reintroduce;

    property ProjectPath : string read FProjectPath write FProjectPath;
  end;

implementation

{ TRestoreOptions }

function TRestoreOptions.Clone : TRestoreOptions;
begin
  result := TRestoreOptions.CreateClone(Self);
end;

constructor TRestoreOptions.Create;
begin
  inherited;
  Prerelease := true;
end;

constructor TRestoreOptions.CreateClone(const original : TRestoreOptions);
begin
  inherited CreateClone(original);
  FProjectPath := original.ProjectPath;

end;

class constructor TRestoreOptions.CreateDefault;
begin
  FDefault := TRestoreOptions.Create;
end;

function TRestoreOptions.Validate(const logger : ILogger) : Boolean;
begin
  //must call inherited
  result := inherited Validate(logger);

  if FProjectPath = '' then
  begin
    Logger.Error('Project path cannot be empty, must either be a directory or project file.');
    result := false;
  end;


end;

end.

