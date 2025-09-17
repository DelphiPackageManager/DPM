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
    FRsVarFiles : array[TCompilerVersion.UnknownVersion..DelphiMax] of string;
  protected
    function FoundCompilerInfo(const compilerVersion : TCompilerVersion) : Boolean;
    function GetRsVarsFilePath(const platform : TDPMPlatform; const compilerVersion : TCompilerVersion) : string;

  public
    constructor Create(const logger : ILogger);
    destructor Destroy; override;
  end;

implementation

uses
  System.Win.Registry,
  System.SysUtils,
  WinApi.Windows,
  DPM.Core.Utils.System;


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

function TCompilerEnvironmentProvider.GetRsVarsFilePath(const platform : TDPMPlatform;  const compilerVersion : TCompilerVersion) : string;
var
  bdsVersion                  : string;
  key                         : string;
  reg                         : TRegistry;
  rootDir                     : string;
begin
  result := '';
  case FFound[compilerVersion] of
    TStatus.found :
      begin
        result := FRsVarFiles[compilerVersion];
        exit;
      end;
    TStatus.notfound :
      exit;
    TStatus.unknown :
      begin
        bdsVersion := CompilerToBDSVersion(compilerVersion);
        key := 'Software\Embarcadero\BDS\%s';
        key := Format(key, [bdsVersion]);

        reg := TRegistry.Create(KEY_READ);
        try
          reg.RootKey := HKEY_CURRENT_USER;
          FLogger.Debug('Attempting to open ' + key);
          if reg.OpenKey(key, False) then
          begin
            FLogger.Debug('Reading RootDir');
            rootDir := reg.ReadString('RootDir');
            FLogger.Debug('RootDir : ' + rootDir);
            if rootDir = '' then
            begin
              FLogger.Error('Unable to find install location for compiler [' + CompilerToString(compilerVersion) + ']');
              FFound[compilerVersion] := TStatus.notfound;
              raise Exception.Create('Unable to find install location for compiler [' + CompilerToString(compilerVersion) + ']');
//              exit;
            end;
          end
          else
          begin
            FLogger.Error('Unable to find install location for compiler [' + CompilerToString(compilerVersion) + ']');
            FFound[compilerVersion] := TStatus.notfound;
            raise Exception.Create('Unable to find install location for compiler [' + CompilerToString(compilerVersion) + ']');
  //          exit;
          end;
        finally
          reg.Free;
        end;
        if TSystemUtils.Is64BitIDE then
          FRsVarFiles[compilerVersion] := IncludeTrailingPathDelimiter(rootDir) + 'bin64\rsvars64.bat'
        else
          FRsVarFiles[compilerVersion] := IncludeTrailingPathDelimiter(rootDir) + 'bin\rsvars.bat';
        result := FRsVarFiles[compilerVersion];
        FFound[compilerVersion] := TStatus.found;
      end;
  end;
end;

end.


