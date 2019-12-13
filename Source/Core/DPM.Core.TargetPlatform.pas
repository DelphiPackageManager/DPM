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

unit DPM.Core.TargetPlatform;

interface

uses
  DPM.Core.Types;

type
  TTargetPlatform = record
  private
    FCompiler : TCompilerVersion;
    FPlatform : TDPMPlatform;

    function GetIsValid : boolean;
  public
    class operator Equal(a: TTargetPlatform; b: TTargetPlatform) : boolean;
    class operator NotEqual(a : TTargetPlatform; b : TTargetPlatform) : boolean;
    class function Parse(const value : string) : TTargetPlatform;static;
    class function TryParse(const value : string; out targetPlatform : TTargetPlatform) : boolean;static;
    class function Empty : TTargetPlatform;static;

    constructor Create(const compiler : TCompilerVersion; const platform : TDPMPlatform);

    function Clone : TTargetPlatform;
    function ToString : string;

    function IsCompatibleWith(const value : TTargetPlatform) : boolean;

    property Compiler : TCompilerVersion read FCompiler;
    property Platform : TDPMPlatform read FPlatform;
    property IsValid : boolean read GetIsValid;
  end;

function GetTargeTDPMPlatformsForCompiler(const target : TCompilerVersion) : TDPMPlatforms;

//make sure the compiler supports the platform
function ValidatePlatform(const target : TCompilerVersion; const platform : TDPMPlatform) : boolean;


implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.TypInfo,
  System.Types;



function GetTargeTDPMPlatformsForCompiler(const target : TCompilerVersion) : DPM.Core.Types.TDPMPlatforms;
begin
  //TODO : Add onther plaforms
  case target of
    TCompilerVersion.RS2009: result := [TDPMPlatform.Win32];
    TCompilerVersion.RS2010: result := [TDPMPlatform.Win32];
    TCompilerVersion.RSXE:   result := [TDPMPlatform.Win32];
    TCompilerVersion.RSXE2:  result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32];
    TCompilerVersion.RSXE3:  result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32];
    TCompilerVersion.RSXE4:  result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32];
    TCompilerVersion.RSXE5..TCompilerVersion.RS10_1 : result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32];
    TCompilerVersion.RS10_2 : result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.OSX64, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32,
                                         TDPMPlatform.LinuxIntel64];
    TCompilerVersion.RS10_3 : result := [TDPMPlatform.Win32, TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.OSX64, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32,
                                         TDPMPlatform.LinuxIntel64, TDPMPlatform.AndroidArm64];
  else
    raise EArgumentOutOfRangeException.Create('Invalid compiler version');
//    result := [Win32,Win64,OSX32,iOS32,AndroidArm32, LinuxIntel64]; //Tokyo or later.
  end;
end;

function ValidatePlatform(const target : TCompilerVersion; const platform : TDPMPlatform) : boolean;
begin
  result := platform in GetTargeTDPMPlatformsForCompiler(target);
end;


{ TTargetPlatform }


function TTargetPlatform.IsCompatibleWith(const value: TTargetPlatform): boolean;
begin
  result := (Self.Compiler = value.Compiler) and (Self.Platform = value.Platform);
    exit;
end;

function TTargetPlatform.Clone: TTargetPlatform;
begin
  result := TTargetPlatform.Create(FCompiler, FPlatform);
end;

constructor TTargetPlatform.Create(const compiler: TCompilerVersion; const platform : TDPMPlatform);
begin
  FCompiler := compiler;
  FPlatform := platform;
end;

class function TTargetPlatform.Empty: TTargetPlatform;
begin
  result := TTargetPlatform.Create(TCompilerVersion.UnknownVersion, TDPMPlatform.UnknownPlatform);
end;

class operator TTargetPlatform.Equal(a, b: TTargetPlatform): boolean;
begin
  result := (a.Compiler = b.Compiler)  and (a.Platform = b.Platform);
end;

function TTargetPlatform.GetIsValid: boolean;
begin
  result := ValidatePlatform(FCompiler, FPlatform);
end;

class operator TTargetPlatform.NotEqual(a, b: TTargetPlatform): boolean;
begin
  result := not (a = b);
end;

class function TTargetPlatform.Parse(const value: string): TTargetPlatform;
var
  parts : array[0..1] of string;
  target : TCompilerVersion;
  platform : TDPMPlatform;
  i : integer;
begin
  i := LastDelimiter('.', value);
  if i = 0  then
    raise EArgumentException.Create(value + ' is not a valid target platform, format is [Target].[Platform], e.g: RSXE7.Win32');
  parts[0] := Copy(value, 1, i -1);
  parts[1] := Copy(value, i + 1, length(value));

  if (parts[0] = '') or (parts[1] = '') then
    raise EArgumentException.Create(value + ' is not a valid target platform, format is [Target].[Platform], e.g: RSXE7.Win32');

  target := StringToCompilerVersion(parts[0]);
  if target = TCompilerVersion.UnknownVersion then
    raise EArgumentOutOfRangeException.Create('Invalid Compiler version : ' + parts[0] );

  platform := StringToDPMPlatform(parts[1]);
  if platform = TDPMPlatform.UnknownPlatform then
    raise EArgumentOutOfRangeException.Create('Invalid Platform : ' + parts[1] );
  result := TTargetPlatform.Create(target, platform);
end;


function TTargetPlatform.ToString: string;
var
  sCompiler : string;
  sPlatform : string;
begin
  sCompiler := GetEnumName(typeinfo(TCompilerVersion),ord(Compiler));
  sPlatform := GetEnumName(typeinfo(TDPMPlatform),ord(FPlatform));
  result := Format('%s.%s',[sCompiler, sPlatform]);
end;

class function TTargetPlatform.TryParse(const value: string; out targetPlatform: TTargetPlatform): boolean;
begin
  result := true;
  try
    targetPlatform :=  TTargetPlatform.Parse(value);
  except
    result := false;
  end;
end;

end.
