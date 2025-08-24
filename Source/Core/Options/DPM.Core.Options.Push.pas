{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Options.Push;

interface


uses
  DPM.Core.Types,
  DPM.Core.Options.Base;

type
  TPushOptions = class(TOptionsBase)
  private
    FPackagePath : string;
    FSource : string;
    FApiKey : string;
    FTimeout : integer;
    FSkipDuplicate : boolean;

    class var
      FDefault : TPushOptions;
  public
    class constructor CreateDefault;
    class property Default : TPushOptions read FDefault;
    constructor Create; override;

    property ApiKey : string read FApiKey write FApiKey;
    property PackagePath : string read FPackagePath write FPackagePath;
    property SkipDuplicate : boolean read FSkipDuplicate write FSkipDuplicate;
    property Source : string read FSource write FSource;
    property Timeout : integer read FTimeout write FTimeout;
  end;


implementation

{ TPushOptions }

constructor TPushOptions.Create;
begin
  inherited;
  FTimeout := -1;
end;

class constructor TPushOptions.CreateDefault;
begin
  FDefault := TPushOptions.Create;
end;

end.

