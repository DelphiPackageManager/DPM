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

unit DPM.Core.Dependency.Version;

(*
for delphi we need to be much more strict when
considering dependency version compatibilty - so while the syntax
here is similar to .net it is not the same.

Only the Patch part for a semantic version is allowed to float and
a min version must be specified.

1.0.12 - exact version match (ie must be a semantic version)
inclusive
[1.0.12,] -  any version >= 1.0.12 and < 1.1 is compatible
[1.0.12,1.0.57] -  any version >= 1.0.12 and <= 1.0.57 is compatible

exclusive
(1.0.12,) - exclusive - any version > 1.0.12 and < 1.1.0 is compatible
(1.0.12,1.0.57) - exclusive - any version > 1.0.12 and < 1.0.57 is compatible

mixed
[1.0.12,1.0.57) -  any version >= 1.0.12 and < 1.0.57 is compatible
(1.0.12,1.0.57] -  any version > 1.0.12 and <= 1.0.57 is compatible

invalid
(1.0.12,1.0.13) - exlusive but empty range
(1.0.12)
(,1.0.12) - no min version specified.

*)

//TODO : Pretty printing for making dependency resolution output simpler


interface

uses
  DPM.Core.Types;

type
  TVersionRange = record
  private
    FOriginal : string;
    FMaxVersion: TPackageVersion;
    FMaxVersionIsInclusive: boolean;
    FMinVersion: TPackageVersion;
    FMinVersionIsInclusive: boolean;
    FIsValid : boolean;
    constructor CreateInvalid(dummy : integer);

    //NOTE : we only allow the Patch to float for now - this is where it is enforced.
    class function GetMinFromMax(const maxVersion : TPackageVersion) : TPackageVersion;static;
    class function GetMaxFromMin(const minVersion : TPackageVersion) : TPackageVersion;static;
    class function IsValidFloat(const minVersion : TPackageVersion; const  maxVersion : TPackageVersion) : boolean;static;
  public

    constructor Create(const original : string; const minVersion : TPackageVersion; const minInclusive : boolean; const maxVersion : TPackageVersion; const maxInclusive : boolean);overload;
    constructor Create(const version : TPackageVersion);overload;
    class function Parse(const value : string)  : TVersionRange;static;

    class function TryParse(const value : string; out depVersion : TVersionRange) : boolean;static;
    class function TryParseWithError(const value : string; out depVersion : TVersionRange; out error : string) : boolean;static;
    class function Empty : TVersionRange;static;
    class operator Equal(a: TVersionRange; b: TVersionRange) : boolean;

    procedure Normalize;
    function Satisfies(const packageVersion: TPackageVersion): Boolean;

    function IsSubsetOrEqualTo(const possibleSuperset : TVersionRange) : boolean;

    function TryGetOverlappingVersion(const otherVersion : TVersionRange; out overlappingVersion : TVersionRange) : boolean;

    function ToString : string;
    function ToDisplayString : string;
    function Clone(const normalize : boolean = false) : TVersionRange;
    function IsEmpty : boolean;
    function IsFixed : boolean;
    function PatchWidth : integer;

    property MaxVersion : TPackageVersion read FMaxVersion;
    property MaxVersionIsInclusive : boolean read FMaxVersionIsInclusive;
    property MinVersion : TPackageVersion read FMinVersion;
    property MinVersionIsInclusive : boolean read FMinVersionIsInclusive;
    property IsValid : boolean read FIsValid;
  end;


implementation

uses
  System.SysUtils,
  System.Character,
  DPM.Core.Utils.Strings;

{ TVersionRange }

function TVersionRange.Clone(const normalize : boolean = false): TVersionRange;
begin
  result := TVersionRange.Create(FOriginal, FMinVersion.Clone, FMinVersionIsInclusive, FMaxVersion.Clone, FMaxVersionIsInclusive);
  if normalize then
    result.Normalize;
end;

constructor TVersionRange.Create(const original : string; const minVersion : TPackageVersion; const minInclusive : boolean; const maxVersion : TPackageVersion; const maxInclusive : boolean);
begin
  FOriginal := original;
  FMinVersion := minVersion;
  FMinVersionIsInclusive := minInclusive;

  FMaxVersion :=maxVersion;
  FMaxVersionIsInclusive := maxInclusive;;
  FIsValid := true;
end;

constructor TVersionRange.Create(const version: TPackageVersion);
begin
  FOriginal := version.ToStringNoMeta;
  FMinVersion := version;
  FMinVersionIsInclusive := true;

  FMaxVersion :=version;
  FMaxVersionIsInclusive := True;;
  FIsValid := true;

end;

constructor TVersionRange.CreateInvalid(dummy: integer);
begin
  FIsValid := false;
end;


class function TVersionRange.Empty: TVersionRange;
begin
  result := TVersionRange.Create('', TPackageVersion.Empty, true, TPackageVersion.Empty, true);
end;

class operator TVersionRange.Equal(a, b: TVersionRange): boolean;
begin
 result := (a.MinVersionIsInclusive = b.MinVersionIsInclusive) and
           (a.MaxVersionIsInclusive = b.MaxVersionIsInclusive) and
           (b.MinVersion = a.MinVersion) and
           (a.MaxVersion = b.MaxVersion);
end;

class function TVersionRange.GetMaxFromMin(const minVersion: TPackageVersion): TPackageVersion;
var
  patch : Word;
begin
  patch := High(word);
  result := TPackageVersion.Create(minVersion.Major, minVersion.Minor, patch);
end;

class function TVersionRange.GetMinFromMax(const maxVersion: TPackageVersion): TPackageVersion;
var
  patch : Word;
begin
  patch := 0;
  result := TPackageVersion.Create(maxVersion.Major, maxVersion.Minor, patch);
end;

function TVersionRange.IsEmpty: boolean;
begin
  result := FMinVersion.IsEmpty and FMaxVersion.IsEmpty;
end;

function TVersionRange.IsFixed: boolean;
begin
  result := FMinVersion = FMaxVersion;
end;

function TVersionRange.IsSubsetOrEqualTo(const possibleSuperset: TVersionRange): boolean;
begin
  result := (FMinVersion >= possibleSuperset.MinVersion) and (FMaxVersion <= possibleSuperset.MaxVersion);
end;

class function TVersionRange.IsValidFloat(const minVersion, maxVersion: TPackageVersion): boolean;
begin
  result := (minVersion.Major = maxVersion.Major) and
            (minVersion.Minor = maxVersion.Minor);
end;

procedure TVersionRange.Normalize;
begin
  if not FMinVersionIsInclusive then
  begin
    FMinVersion.Patch := FMinVersion.Patch +1;
    FMinVersionIsInclusive := true;
    FOriginal := '';
  end;
  if not FMaxVersionIsInclusive then
  begin
    FMaxVersion.Patch := FMaxVersion.Patch -1;
    FMaxVersionIsInclusive := true;
    FOriginal := ''; //we're changing it so do not display original value in tostring
  end;
end;

class function TVersionRange.Parse(const value: string): TVersionRange;
var
//  c : Char;
  sValue : string;
  i : integer;
  len : integer;
  minVersion : TPackageVersion;
  maxVersion : TPackageVersion;
  minInclusive : boolean;
  maxInclusive : boolean;
  range : TArray<string>;
  sError : string;
  minPatch : word;
  maxPatch : word;
begin
  result := TVersionRange.CreateInvalid(0);
  sValue := Trim(value);
  len := Length(sValue);

  //since we are only allowing the patch to float
  //fail early if not at least major.minor.patch
  if len < 5 then //0.0.1 = 5 chars
    raise EArgumentOutOfRangeException.Create('Value too short, min 0.0.1');

  minInclusive := false;
  {$WARN WIDECHAR_REDUCED OFF}
  if not (sValue[1] in ['(', '[']) then
  {$WARN WIDECHAR_REDUCED ON}
  begin
    //single version
    if not TPackageVersion.TryParse(sValue, minVersion) then
      raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - not a valid Package version.');
    result := TVersionRange.Create(value, minVersion, true, minVersion, true);
    exit;
  end
  else
  begin
    case sValue[1] of
      '(' : minInclusive := false;
      '[' : minInclusive := true;
    end;

    case sValue[len] of
      ')' : maxInclusive := false;
      ']' : maxInclusive := true;
    else
      raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - trailing character in range - must be ) or ]');
    end;
    //remove the brackets;
    Delete(sValue,len,1);
    Delete(sValue,1,1);
    i := pos(',',sValue);
    if i = 0 then
      raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - range must include comma after min version');

    range := TStringUtils.SplitStr(sValue, ',');
    len := Length(range);
    //
    if (len = 2) and (range[0] = '') then
    begin
      range := TArray<string>.Create(range[1]);
      len := 1;
    end;

    case len of
      1 :
      begin
        //only min or max version specified, determine which from the comma position.
        if i = 1 then //max version specified
        begin
          if not TPackageVersion.TryParseWithError(range[0], maxVersion, sError) then
            raise Exception.Create('Invalid VersionRange - ' + sError);
          minVersion := GetMinFromMax(maxVersion);
          //if exclusive range, then max must be 2 more than min
          if (not minInclusive) or (not maxInclusive) then
          begin
            if Abs(maxVersion.Patch - minVersion.Patch) < 2 then
              raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - invalid range - no valid version in range.');
          end;
          result := TVersionRange.Create(value, minVersion, minInclusive, maxVersion, maxInclusive);
          exit;
        end
        else //min version specified
        begin
          if not TPackageVersion.TryParseWithError(range[0], minVersion, sError) then
            raise Exception.Create('Invalid dependency version - ' + sError);
          maxVersion := GetMaxFromMin(minVersion);
          //if exclusive range, then max must be 2 more than min
          if (not minInclusive) or (not maxInclusive) then
          begin
            if Abs(maxVersion.Patch - minVersion.Patch) < 2 then
              raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - invalid range - no valid version in range.');
          end;
          result := TVersionRange.Create(value, minVersion,minInclusive , maxVersion, maxInclusive);
          exit;
       end;
      end;
      2 :
      begin
        //min and max specified
          if not TPackageVersion.TryParseWithError(range[0], minVersion, sError) then
            raise Exception.Create('Invalid VersionRange - ' + sError);
          if not TPackageVersion.TryParseWithError(range[1], maxVersion, sError) then
            raise Exception.Create('Invalid VersionRange - ' + sError);
          if not IsValidFloat(minVersion, maxVersion) then
            raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - invalid range - only the Patch may float.');


          minPatch := minVersion.Patch;
          maxPatch := maxVersion.Patch;
          if minInclusive and (not maxInclusive) then
            Dec(maxPatch)
          else if (not minInclusive) and maxInclusive then
            Inc(minPatch)
          else if (not minInclusive) and (not maxInclusive) then
          begin
            Inc(minPatch);
            Dec(maxPatch);
          end;

          if (maxPatch - minPatch) < 0 then
            raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - invalid range - no valid version in range.');

          result := TVersionRange.Create(value, minVersion,minInclusive , maxVersion, maxInclusive);
      end;
    else
      raise EArgumentException.Create('Invalid VersionRange "' + sValue + '" - invalid range count');

    end;
  end;
end;

function TVersionRange.PatchWidth: integer;
var
  x : TVersionRange;
begin
  x := self.Clone(true); //normalize to make calc simpler.
  result := integer(x.MaxVersion.Patch) - integer(x.MinVersion.Patch);
end;

function TVersionRange.Satisfies(const packageVersion: TPackageVersion): Boolean;
begin
  if FMinVersionIsInclusive then
    result := packageVersion >= FMinVersion
  else
    result := packageVersion > FMinVersion;

  if FMaxVersionIsInclusive then
    result := result and (packageVersion <= FMaxVersion)
  else
    result := result and (packageVersion <= FMaxVersion)
end;

function TVersionRange.ToDisplayString: string;
begin
  if (FMinVersion = FMaxVersion) and FMaxVersionIsInclusive and FMinVersionIsInclusive then
    //fixed version.
    result := FMinVersion.ToString
  else
  begin
    if FMinVersionIsInclusive then
      result := '>='
    else
      result := '>';
    result := result + FMinVersion.ToString + ', ';
    if FMaxVersionIsInclusive then
      result := result + '<='
    else
      result := result + '<';
    result := result + FMaxVersion.ToString;
  end;
end;

function TVersionRange.ToString: string;
begin
  if FOriginal <> '' then
  begin
    result := FOriginal;
    exit;
  end;

  if (FMinVersion = FMaxVersion) and FMaxVersionIsInclusive and FMinVersionIsInclusive then
    //fixed version.
    result := FMinVersion.ToString
  else
  begin
    if FMinVersionIsInclusive then
      result := '['
    else
      result := '(';
    result := result + FMinVersion.ToString + ', ' + FMaxVersion.ToString;
    if FMaxVersionIsInclusive then
      result := result + ']'
    else
      result := result + ')';
  end;
end;

function TVersionRange.TryGetOverlappingVersion(const otherVersion: TVersionRange; out overlappingVersion: TVersionRange): boolean;
var
  left : TVersionRange;
  right : TVersionRange;
  min : TPackageVersion;
  max : TPackageVersion;
begin
  result := false;
  overlappingVersion := TVersionRange.Empty;
  //normalize to make life simpler.
  left := Self.Clone(true);
  right := otherVersion.Clone(true);

  min := right.MinVersion;
  if not Self.Satisfies(min) then
  begin
    min := left.MinVersion;
    if not right.Satisfies(min) then
      exit;
  end;

  max := right.MaxVersion;
  if not self.Satisfies(max) then
  begin
    max := left.MaxVersion;
    if not right.Satisfies(max) then
      exit;
  end;
  overlappingVersion := TVersionRange.Create('', min, true, max, true);
  result := true;
end;

class function TVersionRange.TryParse(const value: string; out depVersion: TVersionRange): boolean;
begin
  result := true;
  try
    depVersion := TVersionRange.Parse(value);
  except
    result := false;
    depVersion := TVersionRange.CreateInvalid(0);
  end;
end;

class function TVersionRange.TryParseWithError(const value: string; out depVersion: TVersionRange; out error: string): boolean;
begin
  result := true;
  try
    depVersion := TVersionRange.Parse(value);
  except
    on e : Exception do
    begin
      result := false;
      depVersion := TVersionRange.CreateInvalid(0);
      error := e.Message;
    end;
  end;

end;

end.
