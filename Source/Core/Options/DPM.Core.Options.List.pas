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

unit DPM.Core.Options.List;

interface


uses
  DPM.Core.Types,
  DPM.Core.Options.Search;

type
  TListOptions = class(TSearchOptions)
  private
    class var
      FDefault : TListOptions;
  public
    class constructor CreateDefault;
    class property Default : TListOptions read FDefault;
    constructor Create; override;

  end;

implementation

{ TListOptions }

constructor TListOptions.Create;
begin
  inherited;
  Prerelease := false;
  Commercial := true;
  Trial := true;
  Skip := 0;
  Take := 200;

end;

class constructor TListOptions.CreateDefault;
begin
  FDefault := TListOptions.Create;
end;

end.

