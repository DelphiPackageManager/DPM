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

unit DPM.Core.Types;

interface

uses
  System.Generics.Defaults,
  VSoft.SemanticVersion;

{$SCOPEDENUMS ON}

type
  TPackageVersion = VSoft.SemanticVersion.TSemanticVersion;
  TClientVersion = VSoft.SemanticVersion.TSemanticVersion;
  TVerbosity = (Quiet,Normal,Detailed, Debug);


  //TODO : Decide on min delphi version supported. Ideally go back as far as possible

  TCompilerVersion = (
    UnknownVersion,
//    RS7,        //need dof
//    RS2005,     //need bdsproj support
//    RS2006,     //need bdsproj support
    RS2009,
    RS2010,
    RSXE,
    RSXE2,
    RSXE3,
    RSXE4,
    RSXE5,
    RSXE6,
    RSXE7,
    RSXE8,
    RS10_0,
    RS10_1,
    RS10_2,
    RS10_3,
    RS10_4 //probably the next version.

  );

  TCompilerVersions = set of TCompilerVersion;

  //covering all bases here.
  TDPMPlatform = (
    UnknownPlatform,
    Win32,
    Win64,
    WinArm32,       //reserved for future use
    WinArm64,       //reserved for future use
    OSX32,
    OSX64,        //reserved for future use
    AndroidArm32,
    AndroidArm64,
    AndroidIntel32, //reserved for future use
    AndroidIntel64, //reserved for future use
    iOS32,
    iOS64,          //reserved for future use
    LinuxIntel32,   //reserved for future use
    LinuxIntel64,
    LinuxArm32,     //reserved for future use
    LinuxArm64      //reserved for future use
    );

  TDPMPlatforms = set of TDPMPlatform;


  TConstProc<T> = reference to procedure (const Arg1: T);
  TConstProc<T1,T2> = reference to procedure (const Arg1: T1; const Arg2: T2);
  TConstProc<T1,T2,T3> = reference to procedure (const Arg1: T1; const Arg2: T2; const Arg3: T3);
  TConstProc<T1,T2,T3,T4> = reference to procedure (const Arg1: T1; const Arg2: T2; const Arg3: T3; const Arg4: T4);

  TPackageVersionComparer = class(TInterfacedObject,IEqualityComparer<TPackageVersion>)
  protected
    function Equals(const Left, Right: TPackageVersion): Boolean;reintroduce;
    function GetHashCode(const Value: TPackageVersion): Integer; reintroduce;
  end;


  function DesignTimePlatform(const target : TCompilerVersion) : TDPMPlatform;
  function StringToCompilerVersion(const value : string) : TCompilerVersion;
  function StringToDPMPlatform(const value : string) : TDPMPlatform;

  function CompilerToString(const value : TCompilerVersion) : string;
  function CompilerCodeName(const value : TCompilerVersion) : string;
  function CompilerWithCodeName(const value : TCompilerVersion) : string;


  function IsValidCompilerString(const value : string) : boolean;

  function IsValidPlatformString(const value : string) : boolean;
  function DPMPlatformToString(const value : TDPMPlatform) : string;
  function DPMPlatformToBDString(const value : TDPMPlatform) : string;

  function DPMPlatformsToString(const value : TDPMPlatforms; const sep : string = ',') : string;


  function CompilerToLibSuffix(const compiler : TCompilerVersion) : string;

  function CompilerToBDSVersion(const compiler : TCompilerVersion) : string;

  //returns the delphi compiler version
  function CompilerToCompilerVersionIntStr(const compiler : TCompilerVersion) : string;

  function ProjectVersionToCompilerVersion(const value : string) : TCompilerVersion;
  function IsAmbigousProjectVersion(const value : string) : boolean;

  function ProjectPlatformToDPMPlatform(const value : string) : TDPMPlatform;

  function AllPlatforms(const compiler : TCompilerVersion ) : TDPMPlatforms;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  DPM.Core.Utils.Strings;

function DesignTimePlatform(const target : TCompilerVersion) : TDPMPlatform;
begin
  result := TDPMPlatform.Win32; //currently all delphi IDE's are 32 bit, but that might change in the future.
end;

function StringToCompilerVersion(const value : string) : TCompilerVersion;
var
  iValue : integer;
  sValue : string;
begin
  sValue := value;
  if not TStringUtils.StartsWith(sValue, 'RS') then
    sValue := 'RS' + sValue;
  sValue := StringReplace(sValue,'.','_',[rfReplaceAll]);

  iValue := GetEnumValue(typeInfo(TCompilerVersion), sValue);

  if iValue = -1 then
    result := TCompilerVersion.UnknownVersion
  else
    result := TCompilerVersion(iValue);
end;

function StringToDPMPlatform(const value : string) : TDPMPlatform;
var
  iValue : integer;
begin
  iValue := GetEnumValue(typeInfo(TDPMPlatform), value);
  if iValue = -1 then
  begin
    if value = 'Android' then
      result := TDPMPlatform.AndroidArm32
    else if value = 'Linux' then
      result := TDPMPlatform.LinuxIntel64
    else
      result := TDPMPlatform.UnknownPlatform
  end
  else
    result := TDPMPlatform(iValue);
end;

function IsValidPlatformString(const value : string) : boolean;
begin
  result := StringToDPMPlatform(value) <> TDPMPlatform.UnknownPlatform;
end;

function CompilerToString(const value : TCompilerVersion) : string;
begin
  result := GetEnumName(TypeInfo(TCompilerVersion), ord(value));
  Delete(result,1,2);// remove RS
  result := StringReplace(result, '_', '.', [rfReplaceAll]);
end;

function IsValidCompilerString(const value : string) : boolean;
begin
  result := StringToCompilerVersion(value) <> TCompilerVersion.UnknownVersion;
end;


function DPMPlatformToString(const value : TDPMPlatform) : string;
begin
  result := GetEnumName(TypeInfo(TDPMPlatform), ord(value));
end;

function DPMPlatformToBDString(const value : TDPMPlatform) : string;
begin
  case value of
    TDPMPlatform.AndroidArm32: result := 'Android';
  else
    result := GetEnumName(TypeInfo(TDPMPlatform), ord(value));
  end;
end;


function DPMPlatformsToString(const value : TDPMPlatforms; const sep : string = ',') : string;
var
  p : TDPMPlatform;
  i : integer;
begin
  result := '';
  i := 0;

  for p in value do
  begin
    if i = 0 then
      result := DPMPlatformToString(p)
    else
      result := result + sep + DPMPlatformToString(p);
    Inc(i);
  end;
end;


function AllPlatforms(const compiler : TCompilerVersion ) : TDPMPlatforms;
begin
  result := [];
  case compiler of
    TCompilerVersion.RS2009,
    TCompilerVersion.RS2010,
    TCompilerVersion.RSXE   : result := [TDPMPlatform.Win32];
    TCompilerVersion.RSXE2  : result := [TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32];
    TCompilerVersion.RSXE3,
    TCompilerVersion.RSXE4,
    TCompilerVersion.RSXE5,
    TCompilerVersion.RSXE6,
    TCompilerVersion.RSXE7,
    TCompilerVersion.RSXE8  : result := [TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32];
    TCompilerVersion.RS10_0,
    TCompilerVersion.RS10_1 : result := [TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32];
    TCompilerVersion.RS10_2 : result := [TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32, TDPMPlatform.LinuxIntel64];
    TCompilerVersion.RS10_3,
    TCompilerVersion.RS10_4 : result := [TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32, TDPMPlatform.iOS32, TDPMPlatform.AndroidArm32, TDPMPlatform.LinuxIntel64,
                                         TDPMPlatform.AndroidArm64, TDPMPlatform.OSX64];
  else
    raise Exception.Create('AllPlatforms is missing for : ' + CompilerToString(compiler));  end;
end;

function CompilerCodeName(const value : TCompilerVersion) : string;
begin
  case value of
    TCompilerVersion.RS10_0: result := 'Seattle';
    TCompilerVersion.RS10_1: result := 'Berlin';
    TCompilerVersion.RS10_2: result := 'Tokyo';
    TCompilerVersion.RS10_3: result := 'Rio';
    TCompilerVersion.RS10_4: result := 'Denali';
  else
    result := '';
  end;

end;


function CompilerWithCodeName(const value : TCompilerVersion) : string;
var
  codeName : string;
begin
  result := CompilerToString(value);
  codeName := CompilerCodeName(value);
  if codeName <> '' then
    result := result + ' ' + codeName;
end;


function CompilerToLibSuffix(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.RS2009: result := '120';
    TCompilerVersion.RS2010: result := '140';
    TCompilerVersion.RSXE  : result := '150';
    TCompilerVersion.RSXE2 : result := '160';
    TCompilerVersion.RSXE3 : result := '170';
    TCompilerVersion.RSXE4 : result := '180';
    TCompilerVersion.RSXE5 : result := '190';
    TCompilerVersion.RSXE6 : result := '200';
    TCompilerVersion.RSXE7 : result := '210';
    TCompilerVersion.RSXE8 : result := '220';
    TCompilerVersion.RS10_0: result := '230';
    TCompilerVersion.RS10_1: result := '240';
    TCompilerVersion.RS10_2: result := '250';
    TCompilerVersion.RS10_3: result := '260';
    TCompilerVersion.RS10_4: result := '270';
  else
    raise Exception.Create('LibSuffix is missing for : ' + CompilerToString(compiler));
  end;
end;


function CompilerToBDSVersion(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.RS2009: result := '6.0';
    TCompilerVersion.RS2010: result := '7.0';
    TCompilerVersion.RSXE  : result := '8.0';
    TCompilerVersion.RSXE2 : result := '9.0';
    TCompilerVersion.RSXE3 : result := '10.0';
    TCompilerVersion.RSXE4 : result := '11.0';
    TCompilerVersion.RSXE5 : result := '12.0';
    TCompilerVersion.RSXE6 : result := '14.0';
    TCompilerVersion.RSXE7 : result := '15.0';
    TCompilerVersion.RSXE8 : result := '16.0';
    TCompilerVersion.RS10_0: result := '17.0';
    TCompilerVersion.RS10_1: result := '18.0';
    TCompilerVersion.RS10_2: result := '19.0';
    TCompilerVersion.RS10_3: result := '20.0';
    TCompilerVersion.RS10_4: result := '21.0';
  else
    raise Exception.Create('BDSVersion is missing for : ' + CompilerToString(compiler));
  end;
end;


function CompilerToCompilerVersionIntStr(const compiler : TCompilerVersion) : string;
begin
  case compiler of
    TCompilerVersion.RS2009: result := '20';
    TCompilerVersion.RS2010: result := '21';
    TCompilerVersion.RSXE  : result := '22';
    TCompilerVersion.RSXE2 : result := '23';
    TCompilerVersion.RSXE3 : result := '24';
    TCompilerVersion.RSXE4 : result := '25';
    TCompilerVersion.RSXE5 : result := '26';
    TCompilerVersion.RSXE6 : result := '27';
    TCompilerVersion.RSXE7 : result := '28';
    TCompilerVersion.RSXE8 : result := '29';
    TCompilerVersion.RS10_0: result := '30';
    TCompilerVersion.RS10_1: result := '31';
    TCompilerVersion.RS10_2: result := '32';
    TCompilerVersion.RS10_3: result := '33';
    TCompilerVersion.RS10_4: result := '34';
  else
    raise Exception.Create('BDSVersion is missing for : ' + CompilerToString(compiler));
  end;

end;

function ProjectPlatformToDPMPlatform(const value : string) : TDPMPlatform;
begin
  //TODO : flesh this out! non win platforms will not works
  result := StringToDPMPlatform(value);
end;

function IsAmbigousProjectVersion(const value : string) : boolean;
var
  elements : TArray<string>;
  major : integer;
  minor : integer;
begin
  result := false;
  if value = '' then
    exit;
  elements := TStringUtils.SplitStr(Trim(value), '.');
  major := StrToIntDef(elements[0], -1);
  if major = -1 then
    exit;

  if length(elements) > 1 then
    minor := StrToIntDef(elements[1],-1)
  else
    minor := 0;
  if minor = -1 then
    exit;

  case major of
    12 :
    begin
      case minor of
        0 : result := true; //ambiguous could be Delphi 2009 or Delphi 2010
      end;
    end;
    14 :
    begin
      case minor of
         4 : result := true; //ambiguous could be xe3 update 2
      end;
    end;
    18 :
    begin
      case minor of
        1    : result := true; //ambiguous could be 10.0 Update 1 and Delphi 10.1
        2    : result := true; //ambigous could be 10.1 Update 1 and Delphi 10.2
        8    : result := true; //ambigous could be 10.3.x or Delphi 10.4
      end;
    end;
  end;
end;




//garnered from stackoverflow and the versions of delphi I have installed
//todo - need checking
function ProjectVersionToCompilerVersion(const value : string) : TCompilerVersion;
var
  elements : TArray<string>;
  major : integer;
  minor : integer;
begin
  result := TCompilerVersion.UnknownVersion;
  if value = '' then
    exit;
  elements := TStringUtils.SplitStr(Trim(value), '.');
  major := StrToIntDef(elements[0], -1);
  if major = -1 then
    exit;

  if length(elements) > 1 then
    minor := StrToIntDef(elements[1],-1)
  else
    minor := 0;
  if minor = -1 then
    exit;


  case major of
    12 :
    begin
      case minor of
        0..1 : result := TCompilerVersion.RS2010;
        2..3 : result := TCompilerVersion.RSXE;
      end;
    end;
    13 : result := TCompilerVersion.RSXE2;
    14 :
    begin
      case minor of
        0..3 : result := TCompilerVersion.RSXE3;
           4 : result := TCompilerVersion.RSXE4; //ambiguous could be xe3 update 2
      else
        result := TCompilerVersion.RSXE4;
      end;
    end;
    15 :
    begin
      case minor of
        0..1 :  result := TCompilerVersion.RSXE5;
      else
        result := TCompilerVersion.RSXE6;
      end;
    end;
    16 : result := TCompilerVersion.RSXE7;
    17 : result := TCompilerVersion.RSXE8;
    18 :
    begin
      case minor of
        0    : result := TCompilerVersion.RS10_0;
        1    : result := TCompilerVersion.RS10_1; //ambiguous could be 10.0 update 1
        2    : result := TCompilerVersion.RS10_2; //ambigous could be 10.1 update 1
        3..4 : result := TCompilerVersion.RS10_2;
        5..8 : result := TCompilerVersion.RS10_3; //18.8 for 10.3.3
      end;
    end;
  end;
end;

{ TPackageVersionComparer }

function TPackageVersionComparer.Equals(const Left, Right: TPackageVersion): Boolean;
begin
  result := Left = Right;
end;

function TPackageVersionComparer.GetHashCode(const Value: TPackageVersion): Integer;
var
  s : string;
begin
  s := Value.ToString;
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;

end.

