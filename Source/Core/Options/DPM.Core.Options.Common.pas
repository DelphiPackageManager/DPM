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

unit DPM.Core.Options.Common;

interface

uses
  DPM.Core.Types;

type
  TCommonOptions = class
  private
    FNoBanner : boolean;
    FVerbosity : TVerbosity;
    FHelp : boolean;
    FConfigFile : string;
    FNonInteractive : boolean;
    class var
      FDefault : TCommonOptions;
  public
    class constructor CreateDefault;
    class property Default : TCommonOptions read FDefault;
    constructor Create;

    property NonInteractive : boolean read FNonInteractive write FNonInteractive;
    property NoBanner : boolean read FNoBanner write FNoBanner;
    property Verbosity : TVerbosity read FVerbosity write FVerbosity;
    property Help : boolean read FHelp write FHelp;
    property ConfigFile : string read FConfigFile write FConfigFile;
  end;
implementation

{ TCommonOptions }

constructor TCommonOptions.Create;
begin
  {$IFDEF DEBUG}
  FVerbosity := TVerbosity.Debug;
  {$ELSE}
  FVerbosity := TVerbosity.Normal;
  {$ENDIF}
end;

class constructor TCommonOptions.CreateDefault;
begin
  FDefault := TCommonOptions.Create;
end;

end.

