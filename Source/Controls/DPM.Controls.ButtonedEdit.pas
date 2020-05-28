unit DPM.Controls.ButtonedEdit;

interface


uses
  System.Classes,
  Vcl.ExtCtrls,
  DPM.Controls.AutoComplete;

//source : https://stackoverflow.com/questions/11615336/is-it-possible-to-add-a-history-list-dropdown-to-delphis-tbuttonededit

type
  TACOption = (acAutoAppend, acAutoSuggest, acUseArrowKey, acSearch, acFilterPrefixes, acUseTab, acRtlReading, acWordFilter, acNoPrefixFiltering);
  TACOptions = set of TACOption;

  TACSource = (acsList, acsHistory, acsMRU, acsShell);

  TButtonedEdit = class(Vcl.ExtCtrls.TButtonedEdit)
  private
    FACList: IDelphiEnumString;
    FAutoComplete: IAutoComplete;
    FACEnabled: boolean;
    FACOptions: TACOptions;
    FACSource: TACSource;
    //debounce stuff
    FTimer: TTimer;
    FOnDebounceChanged : TNotifyEvent;
    FDebounceInterval : Cardinal;

    procedure DoOnTimer(Sender: TObject);
    procedure DoDebounce(Sender : TObject);
    procedure SetDebounceInterval(const Value: Cardinal);
    procedure DoDebounceChanged;

    //history
    function GetACStrings: TStringList;
    procedure SetACEnabled(const Value: boolean);
    procedure SetACOptions(const Value: TACOptions);
    procedure SetACSource(const Value: TACSource);
    procedure SetACStrings(const Value: TStringList);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ACEnabled: boolean read FACEnabled write SetACEnabled;
    property ACOptions: TACOptions read FACOptions write SetACOptions;
    property ACSource: TACSource read FACSource write SetACSource;
    property ACStrings: TStringList read GetACStrings write SetACStrings;
    property DebounceInterval : Cardinal read FDebounceInterval write SetDebounceInterval;
    property OnDebounceChanged : TNotifyEvent read FOnDebounceChanged write FOnDebounceChanged;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  WinApi.Windows,
  Winapi.ShlObj,
  System.Win.ComObj,
  WinApi.ActiveX;

constructor TButtonedEdit.Create(AOwner: TComponent);
begin
  inherited;
  Self.OnChange := DoDebounce;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := DoOnTimer;;
  FTimer.Enabled := False;
  FDebounceInterval := 800;
  FTimer.Interval := FDebounceInterval;


  FACList := TEnumString.Create;
  FACEnabled := True;
  FACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];

end;

procedure TButtonedEdit.CreateWnd;
var
  Dummy: IUnknown;
  Strings: IEnumString;
begin
  inherited;
  if HandleAllocated then
  begin
    try
      Dummy := CreateComObject(CLSID_AutoComplete);
      if (Dummy <> nil) and  (Dummy.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then
      begin
        case FACSource of
          acsHistory: Strings := CreateComObject(CLSID_ACLHistory) as  IEnumString;
          acsMRU: Strings := CreateComObject(CLSID_ACLMRU) as  IEnumString;
          acsShell: Strings := CreateComObject(CLSID_ACListISF) as IEnumString;
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
  FTimer.Free;
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

procedure TButtonedEdit.DoDebounce(Sender: TObject);
begin
  //nothing to do if the event isn't used.
  if not Assigned(FOnDebounceChanged) then
    exit;

  if FDebounceInterval = 0 then
  begin
    FTimer.Enabled := False;
    //we are not debouncing, just fire the event;
    DoDebounceChanged;
    exit;
  end;
  //restart the timer while we are still typing.
  FTimer.Enabled := False;
  FTimer.Enabled := true;

end;

procedure TButtonedEdit.DoDebounceChanged;
begin
   if Assigned(FOnDebounceChanged) then
    FOnDebounceChanged(Self);
end;

procedure TButtonedEdit.DoOnTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  DoDebounceChanged;
end;

function TButtonedEdit.GetACStrings: TStringList;
begin
  Result := FACList.Strings;
end;

procedure TButtonedEdit.SetACEnabled(const Value: Boolean);
begin
  if (FAutoComplete <> nil) then
  begin
    FAutoComplete.Enable(FACEnabled);
  end;
  FACEnabled := Value;
end;

procedure TButtonedEdit.SetACOptions(const Value: TACOptions);
const
  Options : array[TACOption] of integer = (ACO_AUTOAPPEND,
                                           ACO_AUTOSUGGEST,
                                           ACO_UPDOWNKEYDROPSLIST,
                                           ACO_SEARCH,
                                           ACO_FILTERPREFIXES,
                                           ACO_USETAB,
                                           ACO_RTLREADING,
                                           ACO_WORD_FILTER,
                                           ACO_NOPREFIXFILTERING);
var
  Option:TACOption;
  Opt: DWORD;
  AC2: IAutoComplete2;
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

procedure TButtonedEdit.SetACSource(const Value: TACSource);
begin
  if FACSource <> Value then
  begin
    FACSource := Value;
    RecreateWnd;
  end;
end;

procedure TButtonedEdit.SetACStrings(const Value: TStringList);
begin
  if Value <> FACList.Strings then
    FACList.Strings.Assign(Value);
end;

procedure TButtonedEdit.SetDebounceInterval(const Value: Cardinal);
begin
  if FDebounceInterval <> value then
  begin
    FDebounceInterval := Value;
    if FDebounceInterval <= 0 then
    begin
      FDebounceInterval := 0;
      FTimer.Enabled := false;
    end;
    FTimer.Interval := Value;

  end;
end;

end.
