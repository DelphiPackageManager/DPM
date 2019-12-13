unit DPM.Core.Packaging.IdValidator;

interface

uses
  System.RegularExpressions,
  DPM.Core.Constants;

// Valid Id is : Org.PackageName
// must start with a letter.

type
  TPackageIdValidator = class
  private
    class var
      FRegex : TRegEx;
  public
    class procedure ValidatePackageId(const Id : string);
    class function IsValidPackageId(const Id : string) : boolean;
    class constructor Create;
  end;

implementation

uses
  System.SysUtils;

{ TPackageIdValidator }

class constructor TPackageIdValidator.Create;
begin
  FRegex := TRegex.Create('^[a-zA-Z](?:\w+)\.(?:\w+)$',[roIgnoreCase]);
end;

class function TPackageIdValidator.IsValidPackageId(const Id: string): boolean;
begin
  if Id = '' then
    result := false
  else
    result := FRegex.IsMatch(Id);
end;

class procedure TPackageIdValidator.ValidatePackageId(const Id: string);
begin
  if Id = '' then
    raise EArgumentNilException.Create('Id is empty');

  if Length(Id) > cMaxPackageIdLength then
    raise EArgumentException.Create('Length of Id [' +Id +'] exceeds max Id length [' + IntToStr(cMaxPackageIdLength) +']');

  if not IsValidPackageId(Id) then
    raise EArgumentException.Create('Invalid Package Id [' +Id +']');

end;

end.
