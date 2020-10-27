unit DPM.Controls.AutoComplete;

interface

uses
  System.Classes,
  Winapi.Windows,
  Winapi.ShlObj,
  Winapi.ActiveX;

type
  IACList = interface(IUnknown)
    ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand : POLESTR) : HResult; stdcall;
  end;

const
  //options for IACList2
  ACLO_NONE         = 0;  // don't enumerate anything
  ACLO_CURRENTDIR   = 1;  // enumerate current directory
  ACLO_MYCOMPUTER   = 2;  // enumerate MyComputer
  ACLO_DESKTOP      = 4;  // enumerate Desktop Folder
  ACLO_FAVORITES    = 8;  // enumerate Favorites Folder
  ACLO_FILESYSONLY  = 16; // enumerate only the file system

type
  IACList2 = interface(IACList)
    ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag : DWORD) : HResult; stdcall;
    function GetOptions(var pdwFlag : DWORD) : HResult; stdcall;
  end;

  IAutoComplete = interface(IUnknown)
    ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit : HWND; const punkACL : IUnknown;
      pwszRegKeyPath, pwszQuickComplete : POLESTR) : HResult; stdcall;
    function Enable(fEnable : BOOL) : HResult; stdcall;
  end;

const
  //options for IAutoComplete2
  ACO_NONE                    = 0;
  ACO_AUTOSUGGEST             = $1;
  ACO_AUTOAPPEND              = $2;
  ACO_SEARCH                  = $4;
  ACO_FILTERPREFIXES          = $8;
  ACO_USETAB                  = $10;
  ACO_UPDOWNKEYDROPSLIST      = $20;
  ACO_RTLREADING              = $40;
  ACO_WORD_FILTER             = $080;
  ACO_NOPREFIXFILTERING       = $100;

type
  IAutoComplete2 = interface(IAutoComplete)
    ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag : DWORD) : HResult; stdcall;
    function GetOptions(out pdwFlag : DWORD) : HResult; stdcall;
  end;

  IDelphiEnumString = interface(IEnumString)
    ['{D8C2C4C1-A8B6-449E-BE32-4317D3485027}']
    function GetStrings : TStringList;
    property Strings : TStringList read GetStrings;
  end;

  TEnumString = class(TInterfacedObject, IDelphiEnumString, IEnumString)
  private
    type
      TPointerList = array[0..0] of Pointer; //avoid bug of Classes.pas declaration TPointerList = array of Pointer;
    var
      FStrings                : TStringList;
      FCurrIndex              : integer;
  protected
    //IEnumString
    function Next(celt : Longint; out elt;
      pceltFetched : PLongint) : HResult; stdcall;
    function Skip(celt : Longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone(out enm : IEnumString) : HResult; stdcall;

    //IDelphiEnumString
    function GetStrings : TStringList;

  public
    //VCL
    constructor Create;
    constructor CreateClone(const index : integer; const strings : TStrings);
    destructor Destroy; override;
  end;



implementation

{ TEnumString }

function TEnumString.Clone(out enm : IEnumString) : HResult;
begin
  Result := E_NOTIMPL; //S_OK;//
  //  enm := TEnumString.CreateClone(FCurrIndex, FStrings);

  Pointer(enm) := nil;
end;

constructor TEnumString.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FCurrIndex := 0;
end;

constructor TEnumString.CreateClone(const index : integer; const strings : TStrings);
begin
  Create;
  FStrings.Assign(strings);
  FCurrIndex := index;
end;

destructor TEnumString.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TEnumString.GetStrings : TStringList;
begin
  result := FStrings;
end;

function TEnumString.Next(celt : Integer; out elt;
  pceltFetched : PLongint) : HResult;
var
  I                           : Integer;
  wStr                        : WideString;
begin
  I := 0;
  while (I < celt) and (FCurrIndex < FStrings.Count) do
  begin
    wStr := FStrings[FCurrIndex];
    TPointerList(elt)[I] := CoTaskMemAlloc(2 * (Length(wStr) + 1));
    StringToWideChar(wStr, TPointerList(elt)[I], 2 * (Length(wStr) + 1));
    Inc(I);
    Inc(FCurrIndex);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := I;
  if I = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumString.Reset : HResult;
begin
  FCurrIndex := 0;
  Result := S_OK;
end;

function TEnumString.Skip(celt : Integer) : HResult;
begin
  if (FCurrIndex + celt) <= FStrings.Count then
  begin
    Inc(FCurrIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FCurrIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

end.



