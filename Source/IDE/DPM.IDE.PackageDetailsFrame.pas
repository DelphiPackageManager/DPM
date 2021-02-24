unit DPM.IDE.PackageDetailsFrame;

interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  Vcl.Themes,
  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Logging,
  DPM.IDE.Logger,
  DPM.Core.Options.Search,
  DPM.IDE.PackageDetailsPanel,
  DPM.IDE.IconCache,
  DPM.IDE.Types,
  Spring.Container,
  Spring.Collections,
  VSoft.Awaitable,
  SVGInterfaces,
  ToolsAPI;

{$I DPMIDE.inc}

type
  //implemented by the EditorViewFrame
  IPackageSearcher = interface
    ['{4FBB9E7E-886A-4B7D-89FF-FA5DBC9D93FD}']
    function GetSearchOptions : TSearchOptions;
    function SearchForPackages(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
    function GetCurrentPlatform : string;
    procedure SaveBeforeChange;
    procedure PackageInstalled(const package : IPackageSearchResultItem);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
  end;

  TPackageDetailsFrame = class(TFrame)
    sbPackageDetails : TScrollBox;
    pnlPackageId : TPanel;
    pnlInstalled : TPanel;
    lblPackageId : TLabel;
    imgPackageLogo : TImage;
    pnlVersion : TPanel;
    Label1 : TLabel;
    txtInstalledVersion : TEdit;
    btnUninstall : TButton;
    lblVersionTitle : TLabel;
    cboVersions : TComboBox;
    btnInstallOrUpdate : TButton;
    procedure cboVersionsMeasureItem(Control : TWinControl; Index : Integer; var Height : Integer);
    procedure cboVersionsDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
    procedure cboVersionsChange(Sender : TObject);
    procedure btnInstallOrUpdateClick(Sender : TObject);
    procedure btnUninstallClick(Sender : TObject);
  private
    FContainer : TContainer;
    FIconCache : TDPMIconCache;
    FPackageSearcher : IPackageSearcher;
    FPackageMetaData : IPackageSearchResultItem;
    FDetailsPanel : TPackageDetailsPanel;
    FCurrentTab : TCurrentTab;
    FCancellationTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FVersionsDelayTimer : TTimer;
    FConfiguration : IConfiguration;
    FLogger : IDPMIDELogger;
    FClosing : boolean;
    FIncludePreRelease : boolean;
    FPackageInstalledVersion : string;
    FPackageId : string;
    FProjectFile : string;
    FCurrentPlatform : TDPMPlatform;

    FIDEStyleServices : TCustomStyleServices;
  protected
    procedure SetIncludePreRelease(const Value : boolean);
    procedure VersionsDelayTimerEvent(Sender : TObject);
    procedure OnDetailsUriClick(Sender : TObject; const uri : string; const element : TDetailElement);
    procedure Loaded; override;

  public
    constructor Create(AOwner : TComponent); override;
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const packageSearcher : IPackageSearcher; const projectFile : string);
    procedure Configure(const value : TCurrentTab; const preRelease : boolean);
    procedure SetPackage(const package : IPackageSearchResultItem);
    procedure SetPlatform(const platform : TDPMPlatform);
    procedure ViewClosing;
    procedure ThemeChanged;
    property IncludePreRelease : boolean read FIncludePreRelease write SetIncludePreRelease;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  WinApi.ShellApi,
  SVGGraphic,
  DPM.Core.Utils.Strings,
  DPM.Core.Dependency.Version,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Options.Install,
  DPM.Core.Options.UnInstall;

const
  cLatestStable = 'Latest stable ';
  cLatestPrerelease = 'Latest prerelease ';

  { TPackageDetailsFrame }

procedure TPackageDetailsFrame.btnInstallOrUpdateClick(Sender : TObject);
var
  packageInstaller : IPackageInstaller;
  options : TInstallOptions;
begin
  btnInstallOrUpdate.Enabled := false;
  try
    if FRequestInFlight then
      FCancellationTokenSource.Cancel;

    while FRequestInFlight do
      Application.ProcessMessages;
    FCancellationTokenSource.Reset;
    FLogger.Clear;
    FPackageSearcher.SaveBeforeChange;
    FLogger.ShowMessageTab;
    FLogger.Information('Installing package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + ']');

    options := TInstallOptions.Create;
    options.ConfigFile := FConfiguration.FileName;
    options.PackageId := FPackageMetaData.Id;
    options.Version := TPackageVersion.Parse(FPackageMetaData.Version);
    options.ProjectPath := FProjectFile;
    options.Platforms := [ProjectPlatformToDPMPlatform(FPackageSearcher.GetCurrentPlatform)];
    options.Prerelease := FIncludePreRelease;

    //install will fail if a package is already installed, unless you specify force.
    if btnInstallOrUpdate.Caption = 'Update' then
      options.Force := true;

    packageInstaller := FContainer.Resolve<IPackageInstaller>;

    if packageInstaller.Install(FCancellationTokenSource.Token, options) then
    begin
      FPackageMetaData.InstalledVersion := FPackageMetaData.Version;
      FPackageMetaData.Installed := true;
      FPackageInstalledVersion := FPackageMetaData.InstalledVersion;
      FLogger.Information('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + '] installed.');
      FPackageSearcher.PackageInstalled(FPackageMetaData);
      SetPackage(FPackageMetaData);
    end
    else
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + '] did not install.');

  finally
    btnInstallOrUpdate.Enabled := true;
  end;



end;

procedure TPackageDetailsFrame.btnUninstallClick(Sender : TObject);
var
  packageInstaller : IPackageInstaller;
  options : TUnInstallOptions;
begin
  btnUninstall.Enabled := false;
  try
    if FRequestInFlight then
      FCancellationTokenSource.Cancel;

    while FRequestInFlight do
      Application.ProcessMessages;
    FCancellationTokenSource.Reset;
    FLogger.Clear;
    FPackageSearcher.SaveBeforeChange;
    FLogger.ShowMessageTab;
    FLogger.Information('UnInstalling package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + ']');

    options := TUnInstallOptions.Create;
    options.ConfigFile := FConfiguration.FileName;
    options.PackageId := FPackageMetaData.Id;
    options.Version := TPackageVersion.Parse(FPackageMetaData.Version);
    options.ProjectPath := FProjectFile;
    options.Platforms := [ProjectPlatformToDPMPlatform(FPackageSearcher.GetCurrentPlatform)];

    packageInstaller := FContainer.Resolve<IPackageInstaller>;

    if packageInstaller.UnInstall(FCancellationTokenSource.Token, options) then
    begin
      FPackageMetaData.InstalledVersion := FPackageMetaData.Version;
      FPackageMetaData.Installed := true;
      FPackageInstalledVersion := FPackageMetaData.InstalledVersion;
      FLogger.Information('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + '] uninstalled.');
      FPackageSearcher.PackageUninstalled(FPackageMetaData);
      SetPackage(nil);
    end
    else
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version + ' [' + FPackageSearcher.GetCurrentPlatform + '] did not uninstall.');

  finally
    btnUninstall.Enabled := true;
  end;

end;

procedure TPackageDetailsFrame.cboVersionsChange(Sender : TObject);
var
  searchOptions : TSearchOptions;
  package : IPackageSearchResultItem;
  sVersion : string;
begin
  searchOptions := FPackageSearcher.GetSearchOptions;
  searchOptions.SearchTerms := FPackageMetaData.Id;

  sVersion := cboVersions.Items[cboVersions.ItemIndex];
  if TStringUtils.StartsWith(sVersion, cLatestPrerelease, true) then
    Delete(sVersion, 1, Length(cLatestPrerelease))
  else if TStringUtils.StartsWith(sVersion, cLatestStable, true) then
    Delete(sVersion, 1, Length(cLatestStable));

  searchOptions.Version := TPackageVersion.Parse(sVersion);
  searchOptions.Prerelease := true;
  searchOptions.Commercial := true;
  searchOptions.Trial := true;

  FPackageSearcher.SearchForPackages(searchOptions)
  .OnException(
    procedure(const e : Exception)
    begin
      FRequestInFlight := false;
      if FClosing then
        exit;
      FLogger.Error(e.Message);
    end)
  .OnCancellation(
    procedure
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      FLogger.Debug('Cancelled searching for packages.');
    end)
  .Await(
    procedure(const theResult : IList<IPackageSearchResultItem>)
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      //        FLogger.Debug('Got search results.');
      package := theResult.FirstOrDefault;
      SetPackage(package);
    end);
end;

procedure TPackageDetailsFrame.cboVersionsDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
begin
  cboVersions.Canvas.FillRect(Rect);

  if (cboVersions.Items.Count = 0) or (index < 0) then
    exit;

  if odComboBoxEdit in State then
    cboVersions.Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, cboVersions.Items[Index]) //Visual state of the text in the edit control
  else
    cboVersions.Canvas.TextOut(Rect.Left + 2, Rect.Top, cboVersions.Items[Index]); //Visual state of the text(items) in the deployed list


  if odComboBoxEdit in State then
    exit;

  if Integer(cboVersions.Items.Objects[index]) <> 0 then
  begin
    cboVersions.Canvas.Pen.Color := clGray;
    cboVersions.Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    cboVersions.Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
  end;


end;

procedure TPackageDetailsFrame.cboVersionsMeasureItem(Control : TWinControl; Index : Integer; var Height : Integer);
begin
  if (cboVersions.Items.Count = 0) or (index < 0) then
    exit;
  if cboVersions.Items.Objects[index] <> nil then
    Inc(Height, 4);
end;

procedure TPackageDetailsFrame.Configure(const value : TCurrentTab; const preRelease : boolean);
begin
  if (FCurrentTab <> value) or (FIncludePreRelease <> preRelease) then
  begin
    FPackageMetaData := nil;
    FPackageId := '';
    FDetailsPanel.SetDetails(nil);
    FCurrentTab := value;
    FIncludePreRelease := preRelease;
    case FCurrentTab of
      TCurrentTab.Installed :
        begin
          btnInstallOrUpdate.Caption := 'Update';
        end;
      TCurrentTab.Updates :
        begin
          btnInstallOrUpdate.Caption := 'Update';

        end;
      TCurrentTab.Search :
        begin
          btnInstallOrUpdate.Caption := 'Install';

        end;
      TCurrentTab.Conflicts : ;
    end;
    SetPackage(nil);
  end;

end;

constructor TPackageDetailsFrame.Create(AOwner : TComponent);
begin
  inherited;
  //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont];
  {$ENDIF}

  FDetailsPanel := TPackageDetailsPanel.Create(AOwner);
  FDetailsPanel.ParentColor := false;
  FDetailsPanel.ParentBackground := false;
  FDetailsPanel.DoubleBuffered := true;
  FDetailsPanel.Align := alTop;
  FDetailsPanel.Height := 200;
  FDetailsPanel.Top := pnlVErsion.Top + pnlVErsion.Top + 20;
  FDetailsPanel.OnUriClick := Self.OnDetailsUriClick;
  FDetailsPanel.Parent := sbPackageDetails;

  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  FVersionsDelayTimer := TTimer.Create(AOwner);
  FVersionsDelayTimer.Interval := 200;
  FVersionsDelayTimer.Enabled := false;
  FVersionsDelayTimer.OnTimer := VersionsDelayTimerEvent;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;

  ThemeChanged;

end;

procedure TPackageDetailsFrame.Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const packageSearcher : IPackageSearcher; const projectFile : string);
begin
  FContainer := container;
  FIconCache := iconCache;
  FConfiguration := config;
  FLogger := FContainer.Resolve<IDPMIDELogger>;
  FPackageSearcher := packageSearcher;
  FProjectFile := projectFile;
  SetPackage(nil);
end;


procedure TPackageDetailsFrame.Loaded;
begin
  inherited;
  //  sbPackageDetails.Color := Self.Color;
  //  sbPackageDetails.ParentBackground := false;
  //
  //  FDetailsPanel.ParentColor := true;
  //  FDetailsPanel.ParentBackground := false;
  //  FDetailsPanel.Color := Self.Color;
  //  FDetailsPanel.Font.Assign(Self.Font);

end;

procedure TPackageDetailsFrame.OnDetailsUriClick(Sender : TObject; const uri : string; const element : TDetailElement);
begin
  case element of
    deNone : ;
    deLicense,
    deProjectUrl,
    deReportUrl : ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL);
    deTags : ;
  end;

end;

procedure TPackageDetailsFrame.SetIncludePreRelease(const Value : boolean);
begin
  if FIncludePreRelease <> Value then
  begin
    FIncludePreRelease := Value;
    SetPackage(FPackageMetaData);
  end;
end;

procedure TPackageDetailsFrame.SetPackage(const package : IPackageSearchResultItem);
var
  logo : IPackageIconImage;
  bFetchVersions : boolean;
  graphic : TGraphic;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;

  FPackageMetaData := package;

  if package <> nil then
  begin
    if (package.Id <> FPackageId) then
    begin
      bFetchVersions := true;
      FPackageId := package.Id;
      if package.Installed then
        FPackageInstalledVersion := package.Version
      else
        FPackageInstalledVersion := '';
    end
    else
    begin
      //same id, so we might just be displaying a new version.
      //since we're just getting the item from the feed rather than the project
      //it won't have installed or installed version set.
      package.InstalledVersion := FPackageInstalledVersion;
      if package.Installed then
        FPackageInstalledVersion := package.Version
      else
      begin
        if package.Version = FPackageInstalledVersion then
          package.Installed := true;
      end;
      bFetchVersions := false; //we already have them.
    end;

    lblPackageId.Caption := package.Id;
    logo := FIconCache.Request(package.Id);
    if logo = nil then
      logo := FIconCache.Request('missing_icon');
    if logo <> nil then
    begin
      graphic := logo.ToGraphic;
      try
        imgPackageLogo.Picture.Assign(graphic);
        imgPackageLogo.Visible := true;
      finally
        graphic.Free;
      end;
    end
    else
      imgPackageLogo.Visible := false;

    pnlInstalled.Visible := FPackageInstalledVersion <> '';

    case FCurrentTab of
      TCurrentTab.Search :
        begin
          if pnlInstalled.Visible then
          begin
            btnInstallOrUpdate.Caption := 'Update'
          end
          else
          begin
            btnInstallOrUpdate.Caption := 'Install';
          end;
        end;
      TCurrentTab.Installed :
        begin
          btnInstallOrUpdate.Caption := 'Update';
        end;
      TCurrentTab.Updates :
        begin
          btnInstallOrUpdate.Caption := 'Update';
        end;
      TCurrentTab.Conflicts : ;
    end;

    txtInstalledVersion.Text := FPackageInstalledVersion;
    btnInstallOrUpdate.Enabled := (not package.Installed) or (package.InstalledVersion <> package.Version);

    sbPackageDetails.Visible := true;

    FVersionsDelayTimer.Enabled := bFetchVersions;
  end
  else
  begin
    sbPackageDetails.Visible := false;
    //    pnlPackageId.Visible := false;
    //    pnlInstalled.Visible := false;
    //    pnlVErsion.Visible := false;
  end;

  FDetailsPanel.SetDetails(package);
end;

procedure TPackageDetailsFrame.SetPlatform(const platform : TDPMPlatform);
begin
  if platform <> FCurrentPlatform then
  begin

  end;
end;

procedure TPackageDetailsFrame.ThemeChanged;
{$IFDEF THEMESERVICES}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
begin
  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$ENDIF}

  sbPackageDetails.Color := FIDEStyleServices.GetSystemColor(clWindow);

  {$IF CompilerVersion >= 32.0}
  sbPackageDetails.ParentColor := false;
  sbPackageDetails.ParentBackground := false;
  sbPackageDetails.StyleElements := [seFont];
  sbPackageDetails.Color := FIDEStyleServices.GetSystemColor(clWindow);

  pnlPackageId.StyleElements := [seFont];
  pnlPackageId.Color := sbPackageDetails.Color;

  pnlInstalled.StyleElements := [seFont];
  pnlInstalled.Color := sbPackageDetails.Color;

  pnlVersion.StyleElements := [seFont];
  pnlVersion.Color := sbPackageDetails.Color;

  FDetailsPanel.StyleElements := [seFont];
  FDetailsPanel.Color := sbPackageDetails.Color;
  FDetailsPanel.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  {$ELSE}
  sbPackageDetails.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FDetailsPanel.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FDetailsPanel.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  {$IFEND}

end;

procedure TPackageDetailsFrame.VersionsDelayTimerEvent(Sender : TObject);
var
  versions : IList<TPackageVersion>;
  repoManager : IPackageRepositoryManager;
  options : TSearchOptions;
  config : IConfiguration;
  lStable : string;
  lPre : string;
  sVersion : string;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  while FRequestInFlight do
    Application.ProcessMessages;
  FCancellationTokenSource.Reset;

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  options := TSearchOptions.Create;
  options.CompilerVersion := IDECompilerVersion;
  options.AllVersions := true;
  options.SearchTerms := FPackageMetaData.Id;
  options.Prerelease := FIncludePreRelease;
  options.Platforms := [FCurrentPlatform];

  config := FConfiguration;

  TAsync.Configure < IList<TPackageVersion> > (
    function(const cancelToken : ICancellationToken) : IList<TPackageVersion>
    begin
      result := repoManager.GetPackageVersions(cancelToken, options, config);
    end, FCancellationTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      FRequestInFlight := false;
      if FClosing then
        exit;
      FLogger.Error(e.Message);
    end)
  .OnCancellation(
    procedure
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      FLogger.Debug('Cancelled getting package versions.');
    end)
  .Await(
    procedure(const theResult : IList<TPackageVersion>)
    var
      version : TPackageVersion;
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      versions := theResult;
      FLogger.Debug('Got package versions .');
      cboVersions.Items.BeginUpdate;
      try
        cboVersions.Clear;

        if versions.Any then
        begin
          if options.Prerelease then
          begin
            version := versions.FirstOrDefault(
              function(const value : TPackageVersion) : boolean
              begin
                result := value.IsStable = false;
              end);
            if not version.IsEmpty then
              lPre := cLatestPrerelease + version.ToStringNoMeta;
          end;
          version := versions.FirstOrDefault(
            function(const value : TPackageVersion) : boolean
            begin
              result := value.IsStable;
            end);
          if not version.IsEmpty then
            lStable := cLatestStable + version.ToStringNoMeta;
          if (lStable <> '') and (lPre <> '') then
          begin
            cboVersions.Items.Add(lPre);
            cboVersions.Items.AddObject(lStable, TObject(1));
          end
          else if lStable <> '' then
            cboVersions.Items.AddObject(lStable, TObject(1))
          else if lPre <> '' then
            cboVersions.Items.AddObject(lPre, TObject(1));
          for version in versions do
            cboVersions.Items.Add(version.ToStringNoMeta);
          cboVersions.ItemIndex := 0;

          sVersion := cboVersions.Items[0];
          if TStringUtils.StartsWith(sVersion, cLatestPrerelease, true) then
            Delete(sVersion, 1, Length(cLatestPrerelease))
          else if TStringUtils.StartsWith(sVersion, cLatestStable, true) then
            Delete(sVersion, 1, Length(cLatestStable));

          btnInstallOrUpdate.Enabled := sVersion <> FPackageInstalledVersion;
        end;
      finally
        cboVersions.Items.EndUpdate;
      end;

    end);

end;

procedure TPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  FPackageSearcher := nil;
  FCancellationTokenSource.Cancel;
end;

end.
