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

unit DPM.Core.Utils.Config;

interface

type
  TConfigUtils = class
  public
    class procedure EnsureDefaultConfigDir;
    class function GetDefaultConfigFileName : string;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.System,
  DPM.Core.Constants;

{ TConfigUtils }

class procedure TConfigUtils.EnsureDefaultConfigDir;
var
  sConfigFolder : string;
begin
    sConfigFolder := ExtractFilePath(TSystemUtils.ExpandEnvironmentStrings(cDefaultConfigFile));
    if not DirectoryExists(sConfigFolder) then
    begin
      //ensure our .dpm folder exists.
      ForceDirectories(sConfigFolder);
    end;

end;

class function TConfigUtils.GetDefaultConfigFileName: string;
begin
  result := TSystemUtils.ExpandEnvironmentStrings(cDefaultConfigFile);
end;

end.
