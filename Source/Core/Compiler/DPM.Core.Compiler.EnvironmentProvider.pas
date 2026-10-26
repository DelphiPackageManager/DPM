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

unit DPM.Core.Compiler.EnvironmentProvider;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Compiler.Interfaces;

type
  //singleton, inject where needed.
  TCompilerEnvironmentProvider = class(TInterfacedObject, ICompilerEnvironmentProvider)
  private
    type
      TStatus = (unknown, found, notfound);
  private
    FLogger : ILogger;
    FFound : array[TCompilerVersion.UnknownVersion..DelphiMax] of TStatus;
    FRootDirs : array[TCompilerVersion.UnknownVersion..DelphiMax] of string;
    FMSBuildPaths : array[TCompilerVersion.UnknownVersion..DelphiMax] of string;
  protected
    function FoundCompilerInfo(const compilerVersion : TCompilerVersion) : Boolean;
    function GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
    function GetRootDir(const compilerVersion : TCompilerVersion) : string;
    function GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;

  public
    constructor Create(const logger : ILogger);
    destructor Destroy; override;
  end;

implementation

uses
  System.Win.Registry,
  System.SysUtils,
  System.StrUtils,
  WinApi.Windows,
  DPM.Core.Diagnostics,
  DPM.Core.Utils.System;

function NotInstalledMessage(const compilerVersion : TCompilerVersion) : string;
begin
  result := 'Delphi ' + CompilerToString(compilerVersion) + ' is not installed - unable to find its install location ' +
            '(RootDir under HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE \Software\Embarcadero\BDS\' + CompilerToBDSVersion(compilerVersion) + ').';
end;

//RootDir under the compiler's key - '' when there is none.
function ReadRootDir(const rootKey : HKEY; const access : LongWord; const key : string) : string;
var
  reg : TRegistry;
begin
  result := '';
  reg := TRegistry.Create(access);
  try
    reg.RootKey := rootKey;
    if reg.OpenKey(key, false) and reg.ValueExists('RootDir') then
      result := Trim(reg.ReadString('RootDir'));
  finally
    reg.Free;
  end;
end;


{ TCompilerEnvironmentProvider }
constructor TCompilerEnvironmentProvider.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

destructor TCompilerEnvironmentProvider.Destroy;
begin

  inherited;
end;

function TCompilerEnvironmentProvider.FoundCompilerInfo(const compilerVersion : TCompilerVersion) : Boolean;
begin
  result := FFound[compilerVersion] = TStatus.found;
end;

function TCompilerEnvironmentProvider.GetRootDir(const compilerVersion : TCompilerVersion) : string;
var
  key : string;
begin
  case FFound[compilerVersion] of
    TStatus.found :
      begin
        result := FRootDirs[compilerVersion];
        exit;
      end;
    TStatus.notfound :
      raise EDPMError.Create(TErrorCode.E5001_CompilerNotInstalled, NotInstalledMessage(compilerVersion));
  end;
  key := Format('Software\Embarcadero\BDS\%s', [CompilerToBDSVersion(compilerVersion)]);
  FLogger.Debug('Reading RootDir from ' + key);
  //the user's own, where the IDE records it when it first runs - else the machine's, where the installer does (the
  //32 bit view - Delphi's installer is a 32 bit program), as on a build machine where no one has opened the IDE.
  result := ReadRootDir(HKEY_CURRENT_USER, KEY_READ, key);
  if result = '' then
    result := ReadRootDir(HKEY_LOCAL_MACHINE, KEY_READ or KEY_WOW64_32KEY, key);
  FLogger.Debug('RootDir : ' + result);
  if result = '' then
  begin
    FFound[compilerVersion] := TStatus.notfound;
    raise EDPMError.Create(TErrorCode.E5001_CompilerNotInstalled, NotInstalledMessage(compilerVersion));
  end;
  result := ExcludeTrailingPathDelimiter(result);
  FRootDirs[compilerVersion] := result;
  FFound[compilerVersion] := TStatus.found;
end;

function TCompilerEnvironmentProvider.GetRsVarsFilePath(const platform : TDPMPlatform;  const compilerVersion : TCompilerVersion) : string;
begin
  if TSystemUtils.Is64BitIDE then
    result := GetRootDir(compilerVersion) + '\bin64\rsvars64.bat'
  else
    result := GetRootDir(compilerVersion) + '\bin\rsvars.bat';
end;

function TCompilerEnvironmentProvider.GetMSBuildPath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;
var
  rsVarsFile : string;
  lines : TStringList;
  line : string;
  frameworkDir : string;
  i : integer;
begin
  result := FMSBuildPaths[compilerVersion];
  if result <> '' then
    exit;
  rsVarsFile := GetRsVarsFilePath(platform, compilerVersion);
  //@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319 - the folder, in every release from XE2 on.
  frameworkDir := '';
  lines := TStringList.Create;
  try
    try
      lines.LoadFromFile(rsVarsFile);
    except
      on e : Exception do
        raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to read [' + rsVarsFile + '] : ' + e.Message);
    end;
    for i := 0 to lines.Count - 1 do
    begin
      line := Trim(lines[i]);
      if (line <> '') and (line[1] = '@') then
        Delete(line, 1, 1);
      if StartsText('SET FrameworkDir=', line) then
        frameworkDir := Trim(Copy(line, Length('SET FrameworkDir=') + 1, MaxInt));
    end;
  finally
    lines.Free;
  end;
  if frameworkDir = '' then
    raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find MSBuild - [' + rsVarsFile + '] does not set FrameworkDir.');
  result := IncludeTrailingPathDelimiter(TSystemUtils.ExpandEnvironmentStrings(frameworkDir)) + 'MSBuild.exe';
  if not FileExists(result) then
    raise EDPMError.Create(TErrorCode.E5002_CompilerLaunchFailed, 'Unable to find MSBuild - [' + result + '], named by [' + rsVarsFile + '], does not exist.');
  FMSBuildPaths[compilerVersion] := result;
end;

end.


