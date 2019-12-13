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

unit DPM.Core.Options.Config;

interface

uses
  DPM.Core.Options.Base;

type
  TConfigOptions = class(TOptionsBase)
  private
    FUseSymLinks : boolean; //use symlinks to package cache when installing package
    FSetValues : string;
    class var
      FDefault : TConfigOptions;
  public
    class constructor CreateDefault;
    class property Default : TConfigOptions read FDefault;

    property UseSymLinks : Boolean read FUseSymLinks write FUseSymLinks;
    property SetValues : string read FSetValues write FSetValues;
  end;

implementation

{ TConfigOptions }

class constructor TConfigOptions.CreateDefault;
begin
  FDefault := TConfigOptions.Create;
end;

end.
