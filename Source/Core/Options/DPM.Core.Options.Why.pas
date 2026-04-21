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

unit DPM.Core.Options.Why;

interface

uses
  DPM.Core.Logging,
  DPM.Core.Options.Restore;

type
  TWhyOptions = class(TRestoreOptions)
  private
    FPackageId : string;
    class var
      FDefault : TWhyOptions;
  protected
    constructor CreateClone(const original : TWhyOptions); reintroduce;
  public
    class constructor CreateDefault;
    class property Default : TWhyOptions read FDefault;
    constructor Create; override;
    function Validate(const logger : ILogger) : Boolean; override;
    function Clone : TWhyOptions; reintroduce;

    property PackageId : string read FPackageId write FPackageId;
  end;

implementation

{ TWhyOptions }

function TWhyOptions.Clone : TWhyOptions;
begin
  result := TWhyOptions.CreateClone(Self);
end;

constructor TWhyOptions.Create;
begin
  inherited;
end;

constructor TWhyOptions.CreateClone(const original : TWhyOptions);
begin
  inherited CreateClone(original);
  FPackageId := original.PackageId;
end;

class constructor TWhyOptions.CreateDefault;
begin
  FDefault := TWhyOptions.Create;
end;

function TWhyOptions.Validate(const logger : ILogger) : Boolean;
begin
  //must call inherited
  result := inherited Validate(logger);

  if FPackageId = '' then
  begin
    logger.Error('Package id cannot be empty.');
    result := false;
  end;
end;

end.
