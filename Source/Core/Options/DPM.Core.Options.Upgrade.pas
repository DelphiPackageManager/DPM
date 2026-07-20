{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright (c) 2019 Vincent Parrett and contributors             }
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

unit DPM.Core.Options.Upgrade;

interface

uses
  DPM.Core.Options.Base;

type
  //Descends from TOptionsBase rather than TSearchOptions - upgrading the client
  //has nothing to do with package sources, compilers or platforms.
  TUpgradeOptions = class(TOptionsBase)
  private
    FPreRelease : boolean;
    FCheckOnly : boolean;
    class var
      FDefault : TUpgradeOptions;
  public
    class constructor CreateDefault;
    class property Default : TUpgradeOptions read FDefault;
    constructor Create; override;

    /// <summary>
    ///  Include pre-release versions when looking for a newer version.
    /// </summary>
    property PreRelease : boolean read FPreRelease write FPreRelease;

    /// <summary>
    ///  Only report whether a newer version exists - do not download or install
    ///  anything. Useful in scripts, and the shape the IDE plugin will want.
    /// </summary>
    property CheckOnly : boolean read FCheckOnly write FCheckOnly;
  end;

implementation

{ TUpgradeOptions }

constructor TUpgradeOptions.Create;
begin
  inherited;
  FPreRelease := false;
  FCheckOnly := false;
end;

class constructor TUpgradeOptions.CreateDefault;
begin
  FDefault := TUpgradeOptions.Create;
end;

end.
