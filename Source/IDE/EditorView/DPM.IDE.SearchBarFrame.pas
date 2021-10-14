unit DPM.IDE.SearchBarFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Themes,
  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.IDE.Types,
  DPM.IDE.Logger,
  DPM.IDE.Options,
  DPM.Controls.ButtonedEdit;

type
  TConfigChangedEvent = procedure(const configuration : IConfiguration) of object;
  TPlatformChangedEvent = procedure(const newPlatform : TDPMPlatform) of object;
  TSearchEvent = procedure(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const platform : TDPMPlatform; const refresh : boolean) of object;

  TDPMSearchBarFrame = class(TFrame)
    DPMEditorViewImages: TImageList;
    DebounceTimer: TTimer;
    Panel1: TPanel;
    btnAbout: TButton;
    btnRefresh: TButton;
    btnSettings: TButton;
    cbPlatforms: TComboBox;
    cbSources: TComboBox;
    chkIncludeCommercial: TCheckBox;
    chkIncludePrerelease: TCheckBox;
    chkIncludeTrial: TCheckBox;
    lblPlatform: TLabel;
    lblProject: TLabel;
    lblSources: TLabel;
    txtSearch: TButtonedEdit;
    procedure txtSearchChange(Sender: TObject);
    procedure txtSearchRightButtonClick(Sender: TObject);
    procedure DebounceTimerTimer(Sender: TObject);
    procedure txtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure chkIncludePrereleaseClick(Sender: TObject);
    procedure chkIncludeCommercialClick(Sender: TObject);
    procedure chkIncludeTrialClick(Sender: TObject);
    procedure cbSourcesChange(Sender: TObject);
    procedure cbPlatformsChange(Sender: TObject);
  private
    FDPMIDEOptions : IDPMIDEOptions;
    FConfigurationManager : IConfigurationManager;
    FConfiguration : IConfiguration;
    FLogger : IDPMIDELogger;
    FSearchHistFile : string;
    FConfigFile : string;
    FHasSources : boolean;
    FLoading : boolean;
    FPlatforms : TDPMPlatforms;
    FPlatform  : TDPMPlatform;
    FIsGroup   : boolean;

    //events
    FOnSearchEvent : TSearchEvent;
    FOnConfigChanged : TConfigChangedEvent;
    FOnPlatformChangedEvent : TPlatformChangedEvent;

    function GetSearchText: string;
    function GetIncludePreRelease: boolean;

  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure AddDefaultSearchHistory;

    procedure ReloadSourcesCombo;
    procedure DoSearchEvent(const refresh : boolean);
    procedure Loaded; override;

    procedure DoPlatformChangedEvent(const newPlatform : TDPMPlatform);

  public
    constructor Create(AOwner : TComponent);override;
    procedure Configure(const logger : IDPMIDELogger; const ideOptions : IDPMIDEOptions; const config : IConfiguration; const configurationManager : IConfigurationManager; const configFile : string; const platforms : TDPMPlatforms);overload;
    procedure Configure(const logger : IDPMIDELogger; const ideOptions : IDPMIDEOptions; const config : IConfiguration; const configurationManager : IConfigurationManager; const configFile : string; const platform : TDPMPlatform);overload;
    procedure ConfigureForTab(const currentTab : TDPMCurrentTab);
    procedure SetPlatform(const platform : TDPMPlatform);
    function GetPlatform : TDPMPlatform;
    procedure ThemeChanged(const ideStyleServices : TCustomStyleServices);


    property Caption : string read GetCaption write SetCaption;
    property HasSources : boolean read FHasSources;
    property SearchText : string read GetSearchText;
    property IncludePrerelease : boolean read GetIncludePreRelease;

    property Platform : TDPMPlatform read GetPlatform;

    property OnConfigChanged : TConfigChangedEvent read FOnConfigChanged write FOnConfigChanged;
    property OnSearch : TSearchEvent read FOnSearchEvent write FOnSearchEvent;
    property OnPlatformChanged : TPlatformChangedEvent read FOnPlatformChangedEvent write FOnPlatformChangedEvent;

  end;

implementation

{$R *.dfm}


uses
  DPM.Core.Utils.Config,
  DPM.IDE.AddInOptionsHostForm,
  DPM.IDE.AboutForm;

const
  cDMPSearchHistoryFile = 'packagesearch.txt';


procedure TDPMSearchBarFrame.AddDefaultSearchHistory;
begin
  //some commonly used open source libs to get people started.
  txtSearch.ACStrings.Add('Spring4D.Core');
  txtSearch.ACStrings.Add('Spring4D.Base');
  txtSearch.ACStrings.Add('VSoft');
  txtSearch.ACStrings.Add('VSoft.DUnitX');
  txtSearch.ACStrings.Add('VSoft.DelphiMocks');
  txtSearch.ACStrings.Add('Gabr42.OmniThreadLibrary');
end;

procedure TDPMSearchBarFrame.btnAboutClick(Sender: TObject);
var
  aboutForm : TDPMAboutForm;
begin
  aboutForm := TDPMAboutForm.Create(nil);
  try
    aboutForm.ShowModal;
  finally
    aboutForm.Free;
  end;
end;

procedure TDPMSearchBarFrame.btnRefreshClick(Sender: TObject);
begin
  DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.btnSettingsClick(Sender: TObject);
var
  bReload : boolean;
  optionsHost : TDPMOptionsHostForm;
begin
  optionsHost := TDPMOptionsHostForm.Create(Self, FConfigurationManager, FLogger, FDPMIDEOptions, FConfigFile);
  try
    bReload := optionsHost.ShowModal = mrOk;
  finally
    optionsHost.Free;
  end;

  if bReload then
  begin
    FConfiguration := FConfigurationManager.LoadConfig(FConfigFile);
    ReloadSourcesCombo;
    //Trigger onconfigchanged
    if Assigned(FOnConfigChanged) then
      FOnConfigChanged(FConfiguration);
//
//    PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, FProject.FileName);
//    //populate the sources combo.
  end;

end;

procedure TDPMSearchBarFrame.cbPlatformsChange(Sender: TObject);
begin
  if not FLoading then
    DoPlatformChangedEvent(StringToDPMPlatform(cbPlatforms.Items[cbPlatforms.ItemIndex]));
end;

procedure TDPMSearchBarFrame.cbSourcesChange(Sender: TObject);
begin
  if not FLoading then
    DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.chkIncludeCommercialClick(Sender: TObject);
begin
  DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.chkIncludePrereleaseClick(Sender: TObject);
begin
  DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.chkIncludeTrialClick(Sender: TObject);
begin
  DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.Configure(const logger: IDPMIDELogger; const ideOptions: IDPMIDEOptions; const config : IConfiguration; const configurationManager: IConfigurationManager; const configFile : string; const platforms : TDPMPlatforms);
var
  platform : TDPMPlatform;
begin
  FLoading := true;
  FLogger := logger;
  FDPMIDEOptions := ideOptions;
  FConfiguration := config;
  FConfigurationManager := configurationManager;
  FConfigFile := configFile;
  FIsGroup := True;
  FPlatforms := platforms;
  ReloadSourcesCombo;
  lblPlatform.Visible := true;

  for platform in FPlatforms do
  begin
    cbPlatforms.Items.Add(DPMPlatformToString(platform));
  end;
  cbPlatforms.Visible := true;
  cbPlatforms.ItemIndex := 0;
  FLoading := false;

end;

procedure TDPMSearchBarFrame.Configure(const logger: IDPMIDELogger; const ideOptions: IDPMIDEOptions; const config: IConfiguration; const configurationManager: IConfigurationManager; const configFile: string;
                                       const platform: TDPMPlatform);
begin
  Configure(logger, ideOptions, config, configurationManager, configFile, [platform]);
  FIsGroup := false;
  FPlatform := platform;
  lblPlatform.Visible := false;
  cbPlatforms.Visible := false;
end;

procedure TDPMSearchBarFrame.ConfigureForTab(const currentTab: TDPMCurrentTab);
begin
  case currentTab of
    TDPMCurrentTab.Search:
    begin
      chkIncludeCommercial.Visible := true;
      chkIncludeTrial.Visible := true;
    end;
  else
    chkIncludeCommercial.Visible := false;
    chkIncludeTrial.Visible := false;
  end;
end;

constructor TDPMSearchBarFrame.Create(AOwner: TComponent);
begin
  inherited;
  ParentColor := false;
  ParentBackground := false;

  //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont];
  chkIncludePrerelease.StyleElements := StyleElements := [seFont,seClient, seBorder];

  {$ENDIF}

  Align := alTop;

  txtSearch.ACEnabled := true;
  txtSearch.ACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
  txtSearch.ACSource := acsList;
  
  FSearchHistFile := TConfigUtils.GetDefaultDMPFolder + '\' + cDMPSearchHistoryFile;
  if FileExists(FSearchHistFile) then
    txtSearch.ACStrings.LoadFromFile(FSearchHistFile)
  else
    //some common packages to help with the search
    AddDefaultSearchHistory;


  FHasSources := false;
end;

procedure TDPMSearchBarFrame.DebounceTimerTimer(Sender: TObject);
begin
  DebounceTimer.Enabled := false;
  DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.DoPlatformChangedEvent(const newPlatform: TDPMPlatform);
begin
  FPlatform := newPlatform;

  if Assigned(FOnPlatformChangedEvent) then
    FOnPlatformChangedEvent(newPlatform);
end;

procedure TDPMSearchBarFrame.DoSearchEvent(const refresh : boolean);
var
  options : TDPMSearchOptions;
  source : string;
  platform : TDPMPlatform;
begin
  if Assigned(FOnSearchEvent) then
  begin
    options := [];
    if chkIncludePrerelease.Checked  then
      Include(options, TDPMSearchOption.IncludePrerelease);
    if chkIncludeCommercial.Checked then
      Include(options, TDPMSearchOption.IncludeCommercial);
    if chkIncludeTrial.Checked then
      Include(options, TDPMSearchOption.IncludeTrial);

    if cbSources.Items.Count > 0 then
      source := cbSources.Items[cbSources.ItemIndex]
    else
      source := 'All';

    if FIsGroup then
      platform := StringToDPMPlatform(cbPlatforms.Items[cbPlatforms.ItemIndex])
    else
      platform := FPlatform;
    FOnSearchEvent(txtSearch.Text, options, source, platform, refresh);
  end;
end;

function TDPMSearchBarFrame.GetCaption: string;
begin
  result := lblProject.Caption;
end;

function TDPMSearchBarFrame.GetIncludePreRelease: boolean;
begin
  result := chkIncludePrerelease.Checked;
end;

function TDPMSearchBarFrame.GetPlatform: TDPMPlatform;
begin
  if FPlatform = TDPMPlatform.UnknownPlatform then
  begin
    if cbPlatforms.Items.Count > 0 then
      FPlatform := StringToDPMPlatform(cbPlatforms.Items[cbPlatforms.ItemIndex]);
  end;

  result := FPlatform;
end;

function TDPMSearchBarFrame.GetSearchText: string;
begin
  result := Trim(txtSearch.Text);
end;

procedure TDPMSearchBarFrame.Loaded;
begin
  inherited;
{$IF CompilerVersion >= 34.0 }
//  ParentBackground := false;
//  ParentColor := false;
{$IFEND}
end;

procedure TDPMSearchBarFrame.ReloadSourcesCombo;
var
  sCurrent : string;
  source : ISourceConfig;
  idx : integer;
begin
  FLoading := true;
  try
    if cbSources.Items.Count > 0 then
      sCurrent := cbSources.Items[cbSources.ItemIndex];
    cbSources.Clear;

    cbSources.Items.Add('All');

    if FConfiguration.Sources.Any then
    begin
      for source in FConfiguration.Sources do
      begin
        if source.IsEnabled then
        begin
          cbSources.Items.Add(source.Name);
          FHasSources := true;
        end;
      end;
    end
    else
      FHasSources := false;

    if sCurrent <> '' then
    begin
      idx := cbSources.Items.IndexOf(sCurrent);
      if idx <> -1 then
        cbSources.ItemIndex := idx
      else
        cbSources.ItemIndex := 0;
    end;
  finally
    FLoading := false;
  end;


end;

procedure TDPMSearchBarFrame.SetCaption(const Value: string);
begin
  lblProject.Caption := value;
end;

procedure TDPMSearchBarFrame.SetPlatform(const platform: TDPMPlatform);
begin
  FPlatform := platform;
end;

procedure TDPMSearchBarFrame.ThemeChanged(const ideStyleServices : TCustomStyleServices);
begin
//{$IF CompilerVersion < 34.0 }
  Self.Color := ideStyleServices.GetSystemColor(clBtnFace);
  Self.Font.Color := ideStyleServices.GetSystemColor(clWindowText);
//{$IFEND}
end;

procedure TDPMSearchBarFrame.txtSearchChange(Sender: TObject);
begin
  txtSearch.RightButton.Visible := txtSearch.Text <> '';
end;

procedure TDPMSearchBarFrame.txtSearchKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
var
  i : integer;
begin
  DebounceTimer.Enabled := false;
  case key of
    VK_RETURN :
      begin
        i := txtSearch.ACStrings.IndexOf(txtSearch.Text);
        if i = -1 then
          txtSearch.ACStrings.Insert(0, txtSearch.Text)
        else
          txtSearch.ACStrings.Move(i, 0);
        try
          txtSearch.ACStrings.SaveToFile(FSearchHistFile);
        except
          //ignore the error, not much we can do?
          //perhaps log it?
        end;
      end;
    VK_ESCAPE :
      begin
        txtSearch.Text := '';
        DebounceTimerTimer(DebounceTimer);
      end
  else
    DebounceTimer.Enabled := true;
  end;
end;

procedure TDPMSearchBarFrame.txtSearchRightButtonClick(Sender: TObject);
begin
  txtSearch.Text := '';
  DoSearchEvent(true);
end;

end.
