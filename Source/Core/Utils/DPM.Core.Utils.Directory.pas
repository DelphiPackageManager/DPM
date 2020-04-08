unit DPM.Core.Utils.Directory;

interface


type
  TDirectoryUtils = class
    class function IsDirectoryWriteable(const directory : string) : boolean;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils;

{ TDirectoryUtils }



function RandomString(const ALength: Integer): String;
var
  i: Integer;
  LCharType: Integer;
begin
  Result := '';
  for i := 1 to ALength do
  begin
    LCharType := Random(3);
    case LCharType of
      0: Result := Result + Chr(ord('a') + Random(26));
      1: Result := Result + Chr(ord('A') + Random(26));
      2: Result := Result + Chr(ord('0') + Random(10));
    end;
  end;
end;

class function TDirectoryUtils.IsDirectoryWriteable(const directory: string): boolean;
var
  fileName: String;
  H: THandle;
begin
  fileName := IncludeTrailingPathDelimiter(directory) + RandomString(5) + '.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(H);
end;

end.
