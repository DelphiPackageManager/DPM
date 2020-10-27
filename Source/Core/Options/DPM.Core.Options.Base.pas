{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Options.Base;

interface

uses
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Logging;

type
  TOptionsBase = class
  private
    FVerbosity : TVerbosity;
    FConfigFile : string;
    FNonInteractive : boolean;
  protected
    FValidated : boolean;
    FIsValid : boolean;
    constructor CreateClone(const original : TOptionsBase); virtual;
  public
    constructor Create; virtual;
    procedure ApplyCommon(const options : TCommonOptions);
    function Validate(const logger : ILogger) : boolean; virtual;
    property ConfigFile : string read FConfigFile write FConfigFile;
    property NonInteractive : boolean read FNonInteractive write FNonInteractive;
    property Verbosity : TVerbosity read FVerbosity write FVerbosity;
    property Validated : boolean read FValidated;
    property IsValid : boolean read FIsValid;
  end;

implementation

uses
  DPM.Core.Constants,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Config,
  System.SysUtils;

{ TOptionsBase }

procedure TOptionsBase.ApplyCommon(const options : TCommonOptions);
var
  sConfigFile : string;
begin
  FVerbosity := options.Verbosity;
  FConfigFile := options.ConfigFile;
  FNonInteractive := options.NonInteractive;
  //check if there is a config file in the curent folder.
  if FConfigFile = '' then
  begin
    //check the current directory
    sConfigFile := IncludeTrailingPathDelimiter(GetCurrentDir) + cDPMConfigFileName;
    if FileExists(sConfigFile) then
      FConfigFile := sConfigFile;
    //if it's still empty, use the profile default.
    if FConfigFile = '' then
    begin
      //ensure our .dpm folder exists.
      TConfigUtils.EnsureDefaultConfigDir;
      FConfigFile := TConfigUtils.GetDefaultConfigFileName;
    end;
  end;
end;

constructor TOptionsBase.Create;
begin
  FVerbosity := TVerbosity.Normal;
  FValidated := false;
  FIsValid := false;
end;

constructor TOptionsBase.CreateClone(const original : TOptionsBase);
begin
  FVerbosity := original.Verbosity;
  FConfigFile := original.FConfigFile;
  FNonInteractive := original.FNonInteractive;
  FValidated := original.FValidated;
  FIsValid := original.FIsValid;
end;

function TOptionsBase.Validate(const logger : ILogger) : boolean;
begin
  result := true;
  FValidated := true;
  FIsValid := true;
end;

end.

