unit DPM.IDE.SearchBarFrame;

interface

{$I ..\DPMIDE.inc}


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Themes,
  {$IFDEF USEIMAGECOLLECTION}
  Vcl.VirtualImageList,
  {$ENDIF}

  //VSoft.Awaitable re-exports the ICancellationToken* aliases, so
  //VSoft.CancellationToken is not needed here.
  VSoft.Awaitable,

  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Upgrade.Interfaces,
  DPM.Core.Upgrade.Cache,
  DPM.IDE.Types,
  DPM.IDE.Logger,
  DPM.IDE.Options,
  DPM.Controls.ButtonedEdit;

type
  TConfigChangedEvent = procedure(const configuration : IConfiguration) of object;
  TProjectSelectedEvent = procedure(const projectFile : string) of object;
  TSearchEvent = procedure(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const refresh : boolean) of object;

  TDPMSearchBarFrame = class(TFrame)
    DPMEditorViewImages: TImageList;
    DebounceTimer: TTimer;
    Panel1: TPanel;
    btnAbout: TButton;
    btnRefresh: TButton;
    btnSettings: TButton;
    cbSources: TComboBox;
    chkIncludeCommercial: TCheckBox;
    chkIncludePrerelease: TCheckBox;
    chkIncludeTrial: TCheckBox;
    lblSources: TLabel;
    txtSearch: TButtonedEdit;
    lblUpdateAvailable: TLinkLabel;
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
    procedure lblUpdateAvailableLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
  private
    FDPMIDEOptions : IDPMIDEOptions;
    FConfigurationManager : IConfigurationManager;
    FConfiguration : IConfiguration;
    FLogger : IDPMIDELogger;
    FSearchHistFile : string;
    FConfigFile : string;
    FHasSources : boolean;
    FLoading : boolean;
    FProjects  : TStringList;
    //events
    FOnSearchEvent : TSearchEvent;
    FOnConfigChanged : TConfigChangedEvent;
    FOnFocusList : TNotifyEvent;

    //update check. Owns its OWN token source rather than sharing the edit
    //view's - that one is Cancel/Reset cycled on every LoadPackages, which
    //would silently un-cancel this check.
    FUpgradeService : IUpgradeService;
    FUpgradeCache : IUpgradeCheckCache;
    FUpgradeInfo : IUpgradeInfo;
    FCancellationTokenSource : ICancellationTokenSource;
    FClosing : boolean;
    FRequestInFlight : boolean;

    {$IFDEF USEIMAGECOLLECTION }
    FImageList : TVirtualImageList;
    {$ELSE}
    FImageList : TImageList;
    {$ENDIF}


    function GetSearchText: string;
    function GetIncludePreRelease: boolean;

  protected
    procedure AddDefaultSearchHistory;

    procedure ReloadSourcesCombo;

    procedure DoSearchEvent(const refresh : boolean);

    procedure Loaded; override;
    procedure SetImageList(const value :  {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF});

    /// <summary>
    ///  Consults the cache and, on a miss, kicks off the async github check.
    ///  Silent when up to date; logs a warning if the check fails.
    /// </summary>
    procedure CheckForUpdate;
    procedure ShowUpdateAvailable(const upgradeInfo : IUpgradeInfo);
    function IncludePrereleaseUpdates : boolean;

  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure Configure(const logger : IDPMIDELogger; const ideOptions : IDPMIDEOptions; const config : IConfiguration;
                        const configurationManager : IConfigurationManager; const configFile : string;
                        const upgradeService : IUpgradeService; const upgradeCache : IUpgradeCheckCache);

    /// <summary>
    ///  Called by the hosting edit view when the view is closing, so any in
    ///  flight update check is cancelled before the frame goes away.
    /// </summary>
    procedure ViewClosing;


    procedure ThemeChanged(const ideStyleServices : TCustomStyleServices);

    property HasSources : boolean read FHasSources;
    property SearchText : string read GetSearchText;
    property IncludePrerelease : boolean read GetIncludePreRelease;

    property OnConfigChanged : TConfigChangedEvent read FOnConfigChanged write FOnConfigChanged;
    property OnSearch : TSearchEvent read FOnSearchEvent write FOnSearchEvent;
    property OnFocusList : TNotifyEvent read FOnFocusList write FOnFocusList;
    property ImageList : {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF} read FImageList write SetImageList;

  end;

implementation

{$R *.dfm}


uses
  WinApi.ActiveX,
  DPM.Core.Utils.Config,
  DPM.Core.Version,
  DPM.IDE.ToolsAPI,
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
  end;

end;

procedure TDPMSearchBarFrame.cbSourcesChange(Sender: TObject);
begin
  if not FLoading then
    DoSearchEvent(true);
end;

procedure TDPMSearchBarFrame.lblUpdateAvailableLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
var
  upgradeInfo : IUpgradeInfo;
  installerFile : string;
  oldCursor : TCursor;
  downloaded : boolean;
begin
  upgradeInfo := FUpgradeInfo;
  if (upgradeInfo = nil) or (FUpgradeService = nil) then
    exit;

  //Plain MessageDlg keeps this version portable - the plugin builds XE2..13.
  if MessageDlg('DPM ' + upgradeInfo.Version.ToStringNoMeta + ' is available.' + sLineBreak + sLineBreak +
                'The installer cannot update DPM while the IDE is running, so any modified files will be saved ' +
                'and the IDE will be closed.' + sLineBreak + sLineBreak +
                'Download and install it now?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;

  //Download synchronously - the user just opted in and is waiting, and this
  //way completion does not have to survive the frame being torn down.
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    FLogger.Information('DPMIDE : Downloading ' + upgradeInfo.AssetName + ' ...');
    downloaded := FUpgradeService.DownloadUpgrade(FCancellationTokenSource.Token, upgradeInfo, installerFile);
  finally
    Screen.Cursor := oldCursor;
  end;

  if not downloaded then
  begin
    //DownloadUpgrade has already logged the detail.
    MessageDlg('Unable to download the DPM installer. See the DPM message view for details.', mtError, [mbOK], 0);
    exit;
  end;

  //Save first - once the IDE starts closing the user should not be answering
  //save prompts while an installer waits behind them.
  TToolsApiUtils.SaveModifiedFiles;

  if not FUpgradeService.LaunchInstaller(installerFile) then
  begin
    MessageDlg('Unable to start the DPM installer. It was downloaded to:' + sLineBreak + installerFile,
               mtError, [mbOK], 0);
    exit;
  end;

  //Order matters : the installer refuses to run while bds.exe is up, so it must
  //already be started (sitting on its privileges prompt) as we close.
  TToolsApiUtils.CloseIDE;
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

procedure TDPMSearchBarFrame.Configure(const logger: IDPMIDELogger; const ideOptions: IDPMIDEOptions; const config : IConfiguration;
                                       const configurationManager: IConfigurationManager; const configFile : string;
                                       const upgradeService : IUpgradeService; const upgradeCache : IUpgradeCheckCache);
begin
  FLoading := true;
  FLogger := logger;
  FDPMIDEOptions := ideOptions;
  FConfiguration := config;
  FConfigurationManager := configurationManager;
  FConfigFile := configFile;
  FUpgradeService := upgradeService;
  FUpgradeCache := upgradeCache;
  ReloadSourcesCombo;
  FLoading := false;

  //Earliest safe point - FLogger and the options are only available now.
  CheckForUpdate;
end;

function TDPMSearchBarFrame.IncludePrereleaseUpdates : boolean;
begin
  //Beta is a superset : it stops prereleases being excluded, it does not
  //exclude stable releases. So a beta user still gets a newer stable.
  result := (FDPMIDEOptions <> nil) and (FDPMIDEOptions.UpdateChannel = TDPMUpdateChannel.Beta);
end;

procedure TDPMSearchBarFrame.ShowUpdateAvailable(const upgradeInfo : IUpgradeInfo);
begin
  FUpgradeInfo := upgradeInfo;
  if upgradeInfo = nil then
  begin
    lblUpdateAvailable.Visible := false;
    exit;
  end;
  //The <a> markup is what makes this clickable - without it TLinkLabel renders
  //as plain text and never fires OnLinkClick. Wrap the WHOLE caption so the
  //entire label is the click target, not just one small word.
  lblUpdateAvailable.Caption := '<a>DPM ' + upgradeInfo.Version.ToStringNoMeta + ' is available - click here to install</a>';
  lblUpdateAvailable.Visible := true;
end;

procedure TDPMSearchBarFrame.CheckForUpdate;
var
  currentVersion : TPackageVersion;
  includePrerelease : boolean;
  cachedResult : TUpgradeCheckResult;
  cachedInfo : IUpgradeInfo;
  upgradeService : IUpgradeService; //local for capture
  logger : IDPMIDELogger;            //local for capture
  cache : IUpgradeCheckCache;        //local for capture
begin
  lblUpdateAvailable.Visible := false;
  FUpgradeInfo := nil;

  if (FUpgradeService = nil) or FRequestInFlight or FClosing then
    exit;

  currentVersion := TDPMVersion.CurrentVersion;
  //An unstamped/garbage cDPMSemVer would compare as Empty and make every
  //release look like an upgrade - don't even ask.
  if currentVersion.IsEmpty then
    exit;

  includePrerelease := IncludePrereleaseUpdates;

  //Cache hit - no thread needed at all.
  if (FUpgradeCache <> nil) and FUpgradeCache.TryGet(currentVersion, includePrerelease, cachedResult, cachedInfo) then
  begin
    if cachedResult = TUpgradeCheckResult.UpgradeAvailable then
      ShowUpdateAvailable(cachedInfo);
    exit;
  end;

  upgradeService := FUpgradeService;
  logger := FLogger;
  cache := FUpgradeCache;

  TAsync.Configure<IUpgradeInfo>(
    function(const cancelToken : ICancellationToken) : IUpgradeInfo
    var
      checkResult : TUpgradeCheckResult;
      info : IUpgradeInfo;
    begin
      result := nil;
      //the http api needs this on the worker thread.
      CoInitialize(nil);
      try
        if cancelToken.IsCancelled then
          exit;
        checkResult := upgradeService.CheckForUpgrade(cancelToken, currentVersion, includePrerelease, info);
        if cancelToken.IsCancelled then
          exit;
        //Errors are deliberately not cached - an offline machine should retry
        //next time rather than go quiet for an hour.
        if (checkResult <> TUpgradeCheckResult.Error) and (cache <> nil) then
          cache.Put(currentVersion, includePrerelease, checkResult, info);
        if checkResult = TUpgradeCheckResult.UpgradeAvailable then
          result := info;
      finally
        CoUninitialize;
      end;
    end, FCancellationTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      FRequestInFlight := false;
      if FClosing then
        exit;
      //Warn rather than error - failing to reach github is not a problem with
      //the user's project.
      logger.Warning('DPMIDE : Unable to check for DPM updates : ' + e.Message);
    end)
  .OnCancellation(
    procedure
    begin
      FRequestInFlight := false;
      if FClosing then
        exit;
      logger.Debug('DPMIDE : Cancelled checking for DPM updates.');
    end)
  .Await(
    procedure(const theResult : IUpgradeInfo)
    begin
      FRequestInFlight := false;
      //The view may have closed while this was in flight - the frame is being
      //torn down, so touching controls here would AV.
      if FClosing then
        exit;
      if theResult <> nil then
      begin
        logger.Debug('DPMIDE : DPM update available : ' + theResult.Version.ToStringNoMeta);
        ShowUpdateAvailable(theResult);
      end;
    end);
  FRequestInFlight := true;
end;

procedure TDPMSearchBarFrame.ViewClosing;
begin
  FClosing := true;
  //Must return quickly - the IDE is waiting on us.
  FCancellationTokenSource.Cancel;
end;

constructor TDPMSearchBarFrame.Create(AOwner: TComponent);
begin
  inherited;
  FProjects := TStringList.Create;

  ParentColor := false;
  ParentBackground := false;

  //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont];
  chkIncludePrerelease.StyleElements :=  [seFont,seClient, seBorder];
  {$ENDIF}

  Align := alTop;

  txtSearch.ACEnabled := true;
  txtSearch.ACOptions := [acAutoAppend, acAutoSuggest{, acUseArrowKey}];
  txtSearch.ACSource := acsList;

  FSearchHistFile := TConfigUtils.GetDefaultDMPFolder + '\' + cDMPSearchHistoryFile;
  if FileExists(FSearchHistFile) then
    txtSearch.ACStrings.LoadFromFile(FSearchHistFile)
  else
    //some common packages to help with the search
    AddDefaultSearchHistory;


  FHasSources := false;

  //Must outlive any async call, so it is a field created up front rather than a
  //local at the call site.
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;
  lblUpdateAvailable.Visible := false;
end;

procedure TDPMSearchBarFrame.DebounceTimerTimer(Sender: TObject);
begin
  DebounceTimer.Enabled := false;
  DoSearchEvent(false);
end;

destructor TDPMSearchBarFrame.Destroy;
begin
  FProjects.Free;
  inherited;
end;

procedure TDPMSearchBarFrame.DoSearchEvent(const refresh : boolean);
var
  options : TDPMSearchOptions;
  source : string;
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

    FOnSearchEvent(txtSearch.Text, options, source, refresh);
  end;
end;



function TDPMSearchBarFrame.GetIncludePreRelease: boolean;
begin
  result := chkIncludePrerelease.Checked;
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


procedure TDPMSearchBarFrame.SetImageList(const value :  {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF});
begin
  FImageList := value;
  if FImageList <> nil then
  begin
//    txtSearch.RightButton.DisabledImageIndex = 0
    txtSearch.RightButton.ImageIndex := 7;
    txtSearch.RightButton.HotImageIndex := 8;
    txtSearch.Images := FImageList;
    btnRefresh.ImageIndex := 4;
    btnRefresh.Images := FImageList;
    btnSettings.ImageIndex := 6;
    btnSettings.Images := FImageList;
    btnAbout.ImageIndex := 5;
    btnAbout.Images := FImageList;

  end;
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
        DebounceTimerTimer(DebounceTimer);
      end;
    VK_ESCAPE :
      begin
        if txtSearch.Text <> '' then
        begin
          txtSearch.Text := '';
          DebounceTimerTimer(DebounceTimer);
        end;
      end;
    VK_DOWN :
    begin
      //send focus to list
      if Assigned(FOnFocusList) then
      begin
        FOnFocusList(self);
        Key := 0;
      end;
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
