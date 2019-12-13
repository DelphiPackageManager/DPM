unit DPM.Core.DependencyVersion;

(*
for delphi we need to be much more strict when
considering dependency version compatibilty - so while the syntax
here is similar it is not the same.

Only the Patch part for a semantic version is allowed to float and
a min version must be specified.

1.0.12 - exact version match (ie must be a sematic version)
inclusive
[1.0.12,] -  any version >= 1.0.12 and < 1.1 is compatible
[1.0.12,1.0.57] -  any version >= 1.0.12 and <= 1.0.57 is compatible

 exclusive
(1.0.12,) - exclusive - any version > 1.0.12 and < 1.1 is compatible
(1.0.12,1.0.57) - exclusive - any version > 1.0.12 and < 1.0.57 is compatible

mixed
[1.0.12,1.0.57) -  any version >= 1.0.12 and < 1.0.57 is compatible
(1.0.12,1.0.57] -  any version > 1.0.12 and <= 1.0.57 is compatible

invalid
(1.0.12,1.0.13) - exlusive but empty range
(1.0.12)
(,1.0.12) - no min version specified.

*)


interface

uses
  DPM.Core.Types;

type
  TDependencyVersion = record
  private
    FMaxVersion: TPackageVersion;
    FMaxVersionIsInclusive: boolean;
    FMinVersion: TPackageVersion;
    FMinVersionIsInclusive: boolean;
    FIsValid : boolean;
    constructor CreateInvalid(dummy : integer);

    //NOTE : we only allow the Patch to float for now - this is where it is enforced.
    class function GetMinFromMax(const maxVersion : TPackageVersion) : TPackageVersion;static;
    class function GetMaxFromMin(const maxVersion : TPackageVersion) : TPackageVersion;static;
    class function IsValidFloat(const minVersion : TPackageVersion; const  maxVersion : TPackageVersion) : boolean;static;
  public

    constructor Create(const minVersion : TPackageVersion; const minInclusive : boolean; const maxVersion : TPackageVersion; const maxInclusive : boolean);
    class function Parse(const value : string)  : TDependencyVersion;static;

    class function TryParse(const value : string; out depVersion : TDependencyVersion) : boolean;static;
    class function TryParseWithError(const value : string; out depVersion : TDependencyVersion; out error : string) : boolean;static;

    function Satisfies(const packageVersion: TPackageVersion): Boolean;

    function ToString : string;
    function Clone : TDependencyVersion;

    property MaxVersion : TPackageVersion read FMaxVersion;
    property MaxVersionIsInclusive : boolean read FMaxVersionIsInclusive;
    property MinVersion : TPackageVersion read FMinVersion;
    property MinVersionIsInclusive : boolean read FMinVersionIsInclusive;
    property IsValid : boolean read FIsValid;
  end;


implementation

uses
  System.SysUtils,
  System.Character;

{ TDependencyVersion }

function TDependencyVersion.Clone: TDependencyVersion;
begin
  result := TDependencyVersion.Create(FMinVersion.Clone, FMinVersionIsInclusive, FMaxVersion.Clone, FMaxVersionIsInclusive);
end;

constructor TDependencyVersion.Create(const minVersion : TPackageVersion; const minInclusive : boolean; const maxVersion : TPackageVersion; const maxInclusive : boolean);
begin
  FMinVersion := minVersion;
  FMinVersionIsInclusive := minInclusive;

  FMaxVersion :=maxVersion;
  FMaxVersionIsInclusive := maxInclusive;;
  FIsValid := true;
end;

constructor TDependencyVersion.CreateInvalid(dummy: integer);
begin
  FIsValid := false;
end;


class function TDependencyVersion.GetMaxFromMin(const maxVersion: TPackageVersion): TPackageVersion;
var
  patch : Word;
begin
  patch := High(word);
  result := TPackageVersion.Create(maxVersion.Major, maxVersion.Minor, patch);
end;

class function TDependencyVersion.GetMinFromMax(const maxVersion: TPackageVersion): TPackageVersion;
var
  patch : Word;
begin
  patch := 0;
  result := TPackageVersion.Create(maxVersion.Major, maxVersion.Minor, patch);
end;

class function TDependencyVersion.IsValidFloat(const minVersion, maxVersion: TPackageVersion): boolean;
begin
  result := (minVersion.Major = maxVersion.Major) and
            (minVersion.Minor = maxVersion.Minor);
end;

class function TDependencyVersion.Parse(const value: string): TDependencyVersion;
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
begin
  result := TDependencyVersion.CreateInvalid(0);
  sValue := value.Trim;
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
      raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - not a valid Package version.');
    result := TDependencyVersion.Create(minVersion, true, minVersion, true);
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
      raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - trailing character in range - must be ) or ]');
    end;
    //remove the brackets;
    Delete(sValue,len,1);
    Delete(sValue,1,1);
    i := pos(',',sValue);
    if i = 0 then
      raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - range must include comma after min version');

    range := sValue.Split([',']);
    len := Length(range);
    //
    if (len = 2) and (range[0] = '') then
    begin
      range := [range[1]];
      len := 1;
    end;

    case len of
      1 :
      begin
        //only min or max version specified, determine which from the comma position.
        if i = 1 then //max version specified
        begin
          if not TPackageVersion.TryParseWithError(range[0], maxVersion, sError) then
            raise Exception.Create('Invalid dependency version - ' + sError);
          minVersion := GetMinFromMax(maxVersion);
          //if exclusive range, then max must be 2 more than min
          if (not minInclusive) or (not maxInclusive) then
          begin
            if Abs(maxVersion.Patch - minVersion.Patch) < 2 then
              raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - invalid range - no valid version in range.');
          end;
          result := TDependencyVersion.Create(minVersion, minInclusive, maxVersion, maxInclusive);
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
              raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - invalid range - no valid version in range.');
          end;
          result := TDependencyVersion.Create(minVersion,minInclusive , maxVersion, maxInclusive);
          exit;
       end;
      end;
      2 :
      begin
        //min and max specified
          if not TPackageVersion.TryParseWithError(range[0], minVersion, sError) then
            raise Exception.Create('Invalid dependency version - ' + sError);
          if not TPackageVersion.TryParseWithError(range[1], maxVersion, sError) then
            raise Exception.Create('Invalid dependency version - ' + sError);
          //if exclusive range, then max must be 2 more than min
          if (not minInclusive) or (not maxInclusive) then
          begin
            if Abs(maxVersion.Patch - minVersion.Patch) < 2 then
              raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - invalid range - no valid version in range.');
          end;
          if not IsValidFloat(minVersion, maxVersion) then
            raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - invalid range - only the Patch may float.');

          result := TDependencyVersion.Create(minVersion,minInclusive , maxVersion, maxInclusive);
      end;
    else
      raise EArgumentException.Create('Invalid dependencyVersion "' + sValue + '" - invalid range count');

    end;
  end;
end;

function TDependencyVersion.Satisfies(const packageVersion: TPackageVersion): Boolean;
begin
  result := false;


end;

function TDependencyVersion.ToString: string;
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

class function TDependencyVersion.TryParse(const value: string; out depVersion: TDependencyVersion): boolean;
begin
  result := true;
  try
    depVersion := TDependencyVersion.Parse(value);
  except
    result := false;
    depVersion := TDependencyVersion.CreateInvalid(0);
  end;
end;

class function TDependencyVersion.TryParseWithError(const value: string; out depVersion: TDependencyVersion; out error: string): boolean;
begin
  result := true;
  try
    depVersion := TDependencyVersion.Parse(value);
  except
    on e : Exception do
    begin
      result := false;
      depVersion := TDependencyVersion.CreateInvalid(0);
      error := e.Message;
    end;
  end;

end;

end.
