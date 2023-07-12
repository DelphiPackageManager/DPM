unit DPM.Controls.ButtonedEdit;

interface


uses
  System.Classes,
  Vcl.ExtCtrls,
  DPM.Controls.AutoComplete;

//source : https://stackoverflow.com/questions/11615336/is-it-possible-to-add-a-history-list-dropdown-to-delphis-tbuttonededit

type
  TACOption = (acAutoAppend, acAutoSuggest, {acUseArrowKey, } acSearch, acFilterPrefixes, acUseTab, acRtlReading, acWordFilter, acNoPrefixFiltering);
  TACOptions = set of TACOption;

  TACSource = (acsList, acsHistory, acsMRU, acsShell);

  TButtonedEdit = class(Vcl.ExtCtrls.TButtonedEdit)
  private
    FACList : IDelphiEnumString;
    FAutoComplete : IAutoComplete;
    FACEnabled : boolean;
    FACOptions : TACOptions;
    FACSource : TACSource;

    //history
    function GetACStrings : TStringList;
    procedure SetACEnabled(const Value : boolean);
    procedure SetACOptions(const Value : TACOptions);
    procedure SetACSource(const Value : TACSource);
    procedure SetACStrings(const Value : TStringList);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ACEnabled : boolean read FACEnabled write SetACEnabled;
    property ACOptions : TACOptions read FACOptions write SetACOptions;
    property ACSource : TACSource read FACSource write SetACSource;
    property ACStrings : TStringList read GetACStrings write SetACStrings;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  WinApi.Windows,
  Winapi.ShlObj,
  System.Win.ComObj,
  WinApi.ActiveX;

constructor TButtonedEdit.Create(AOwner : TComponent);
begin
  inherited;

  FACList := TEnumString.Create;
  FACEnabled := True;
  FACOptions := [acAutoAppend, acAutoSuggest{, acUseArrowKey}];

end;

procedure TButtonedEdit.CreateWnd;
var
  Dummy : IUnknown;
  Strings : IEnumString;
begin
  inherited;
  if HandleAllocated then
  begin
    try
      Dummy := CreateComObject(CLSID_AutoComplete);
      if (Dummy <> nil) and (Dummy.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then
      begin
        case FACSource of
          acsHistory : Strings := CreateComObject(CLSID_ACLHistory) as IEnumString;
          acsMRU : Strings := CreateComObject(CLSID_ACLMRU) as IEnumString;
          acsShell : Strings := CreateComObject(CLSID_ACListISF) as IEnumString;
        else
          Strings := FACList as IEnumString;
        end;
        if S_OK = FAutoComplete.Init(Handle, Strings, nil, nil) then
        begin
          SetACEnabled(FACEnabled);
          SetACOptions(FACOptions);
        end;
      end;
    except
      //CLSID_IAutoComplete is not available
    end;
  end;
end;

destructor TButtonedEdit.Destroy;
begin
  FACList := nil;
  inherited;
end;

procedure TButtonedEdit.DestroyWnd;
begin
  if (FAutoComplete <> nil) then
  begin
    FAutoComplete.Enable(False);
    FAutoComplete := nil;
  end;
  inherited;
end;


function TButtonedEdit.GetACStrings : TStringList;
begin
  Result := FACList.Strings;
end;

procedure TButtonedEdit.SetACEnabled(const Value : Boolean);
begin
  if (FAutoComplete <> nil) then
  begin
    FAutoComplete.Enable(FACEnabled);
  end;
  FACEnabled := Value;
end;

procedure TButtonedEdit.SetACOptions(const Value : TACOptions);
const
  Options : array[TACOption] of integer = (ACO_AUTOAPPEND,
    ACO_AUTOSUGGEST,
    //ACO_UPDOWNKEYDROPSLIST,
    ACO_SEARCH,
    ACO_FILTERPREFIXES,
    ACO_USETAB,
    ACO_RTLREADING,
    ACO_WORD_FILTER,
    ACO_NOPREFIXFILTERING);
var
  Option : TACOption;
  Opt : DWORD;
  AC2 : IAutoComplete2;
begin
  if (FAutoComplete <> nil) then
  begin
    if S_OK = FAutoComplete.QueryInterface(IID_IAutoComplete2, AC2) then
    begin
      Opt := ACO_NONE;
      for Option := Low(Options) to High(Options) do
      begin
        if (Option in FACOptions) then
          Opt := Opt or DWORD(Options[Option]);
      end;
      AC2.SetOptions(Opt);
    end;
  end;
  FACOptions := Value;
end;

procedure TButtonedEdit.SetACSource(const Value : TACSource);
begin
  if FACSource <> Value then
  begin
    FACSource := Value;
    RecreateWnd;
  end;
end;

procedure TButtonedEdit.SetACStrings(const Value : TStringList);
begin
  if Value <> FACList.Strings then
    FACList.Strings.Assign(Value);
end;

end.

