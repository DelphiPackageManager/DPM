unit DPM.Core.Utils.Masks;

interface

uses
  System.Masks,
  Spring.Collections;

type
  IFileMatcher = interface
  ['{5269C6DD-50AF-47AE-80F2-50E911B32BC9}']
    procedure AddMask(const mask : string);

    function Matches(const fileName : string) : boolean;

  end;

  TFileMatcher = class(TInterfacedObject, IFileMatcher)
  private
    FMasks : IList<TMask>;
  protected
    procedure AddMask(const mask : string);
    function Matches(const fileName : string) : boolean;
  public
    constructor Create;
  end;


implementation

uses
  System.IOUtils,
  DPM.Core.Utils.Strings;

{ TFileMatcher }

procedure TFileMatcher.AddMask(const mask: string);
var
  maskObj : TMask;
begin
  maskObj := TMask.Create(mask);
  FMasks.Add(maskObj);
end;

constructor TFileMatcher.Create;
begin
  FMasks := TCollections.CreateObjectList<TMask>;
end;

function TFileMatcher.Matches(const fileName: string): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to FMasks.Count -1 do
  begin
    if FMasks[i].Matches(fileName) then
      exit(true);
  end;
end;

end.
