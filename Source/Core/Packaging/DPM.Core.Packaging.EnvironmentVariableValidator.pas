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

unit DPM.Core.Packaging.EnvironmentVariableValidator;

interface

// Validates IDE environment variable names that a package wants to set when its design-time
// components are loaded. A package may create/override most variables, but a small set is banned
// because overriding them could compromise system stability or be used to redirect the IDE (or a
// process it spawns) to a shadow executable. PATH is allowed but treated specially (append-only) by
// the IDE installer, so it is NOT banned here.

type
  TEnvironmentVariableValidator = class
  public
    // Returns false when the name is reserved/banned and must not be set by a package.
    // reason is filled with a human readable explanation when result is false.
    class function IsAllowed(const name : string; out reason : string) : boolean;
    // True for the PATH variable, which the IDE installer appends to rather than replacing.
    class function IsPathVariable(const name : string) : boolean;
  end;

implementation

uses
  System.SysUtils;

const
  // Compared case-insensitively (Windows env var names are case-insensitive). PATH is intentionally
  // NOT in this list - it is allowed and append-only.
  cBannedEnvVars : array[0..43] of string = (
    //Executable-hijack / redirect risks
    'PATHEXT',
    'COMSPEC',
    'SYSTEMROOT',
    'WINDIR',
    'SYSTEMDRIVE',
    //OS / user profile (stability + redirect)
    'TEMP',
    'TMP',
    'USERPROFILE',
    'PUBLIC',
    'HOMEDRIVE',
    'HOMEPATH',
    'APPDATA',
    'LOCALAPPDATA',
    'PROGRAMDATA',
    'ALLUSERSPROFILE',
    'PROGRAMFILES',
    'PROGRAMFILES(X86)',
    'PROGRAMW6432',
    'COMMONPROGRAMFILES',
    'COMMONPROGRAMFILES(X86)',
    'COMMONPROGRAMW6432',
    'USERNAME',
    'USERDOMAIN',
    'COMPUTERNAME',
    'LOGONSERVER',
    'OS',
    'NUMBER_OF_PROCESSORS',
    'PROCESSOR_ARCHITECTURE',
    'PROCESSOR_ARCHITEW6432',
    'PROCESSOR_IDENTIFIER',
    //RAD Studio built-ins - overriding these breaks the IDE
    'BDS',
    'BDSBIN',
    'BDSINCLUDE',
    'BDSLIB',
    'BDSCOMMONDIR',
    'BDSUSERDIR',
    'BDSPROJECTSDIR',
    'BDSPLATFORMSDKSDIR',
    'BDSCATALOGREPOSITORY',
    'BDSCATALOGREPOSITORYALLUSERS',
    'DELPHI',
    'BCB',
    'FRAMEWORKDIR',
    'FRAMEWORKVERSION'
  );

{ TEnvironmentVariableValidator }

class function TEnvironmentVariableValidator.IsPathVariable(const name : string) : boolean;
begin
  result := SameText(Trim(name), 'PATH');
end;

class function TEnvironmentVariableValidator.IsAllowed(const name : string; out reason : string) : boolean;
var
  upperName : string;
  i : integer;
begin
  reason := '';
  upperName := UpperCase(Trim(name));

  if upperName = '' then
  begin
    reason := 'environment variable name is empty';
    exit(false);
  end;

  for i := Low(cBannedEnvVars) to High(cBannedEnvVars) do
  begin
    if upperName = cBannedEnvVars[i] then
    begin
      reason := 'environment variable [' + name + '] is reserved (system stability / executable hijack risk) and cannot be set by a package. Use a package specific variable name instead.';
      exit(false);
    end;
  end;

  result := true;
end;

end.
