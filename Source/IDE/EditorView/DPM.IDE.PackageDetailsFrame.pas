{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.IDE.PackageDetailsFrame;

interface

{$I ..\DPMIDE.inc}


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Themes,
  Vcl.StdCtrls,  Vcl.ExtCtrls,
  ToolsApi,
  Spring.Container,
  Spring.Collections,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Logging,
  DPM.IDE.Logger,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Package.Cache.Receipt,
  DPM.IDE.SigningBadge,
  DPM.Core.Options.Search,
  DPM.Core.Options.Install,
  DPM.Core.Options.UnInstall,
  DPM.IDE.Details.Interfaces,
  DPM.IDE.PackageDetailsPanel,
  DPM.IDE.IconCache,
  DPM.IDE.Types,
  Vcl.ImgList,
  {$IFDEF USEIMAGECOLLECTION}
  Vcl.VirtualImageList,
  {$ENDIF}
  DPM.Controls.VersionGrid, Vcl.Buttons, Vcl.Imaging.pngimage;


type
  TPackageDetailsFrame = class(TFrame)
    sbPackageDetails: TScrollBox;
    pnlPackageId: TPanel;
    lblPackageId: TLabel;
    imgPackageLogo: TImage;
    pnlVersion: TPanel;
    lblVersionTitle: TLabel;
    cboVersions: TComboBox;
    pnlGridHost: TPanel;
    DetailsSplitter: TSplitter;
    btnInstallAll: TSpeedButton;
    btnUpgradeAll: TSpeedButton;
    btnUninstallAll: TSpeedButton;
    DebounceTimer: TTimer;
    procedure cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cboVersionsChange(Sender: TObject);
    procedure cboVersionsCloseUp(Sender: TObject);
    procedure cboVersionsDropDown(Sender: TObject);
    procedure btnInstallAllClick(Sender: TObject);
    procedure btnUpgradeAllClick(Sender: TObject);
    procedure btnUninstallAllClick(Sender: TObject);
    procedure DebounceTimerTimer(Sender: TObject);
  private
    //controls
    FProjectsGrid : TVersionGrid;
    FDetailsPanel : TPackageDetailsPanel;
    //controls

    {$IFDEF USEIMAGECOLLECTION }
    FImageList : TVirtualImageList;
    {$ELSE}
    FImageList : TImageList;
    FUpgradeBmp : TBitmap;
    FDowngradeBmp : TBitmap;
    {$ENDIF}

    FConfiguration : IConfiguration;
    FConfigurationManager : IConfigurationManager;
    FPackageCache : IPackageCache;
    FReceiptService : IReceiptService;
    // IDE-2: programmatic signing-status badge that sits above the version
    // selector. Painted via TPaintBox (not TLabel) because the IDE's
    // IOTAIDEThemingServices.ApplyTheme walks every TLabel descendant and
    // resets Font.Color to clWindowText regardless of StyleElements — so the
    // green/amber/red tint never reaches the screen on a TLabel. A paint
    // box's OnPaint isn't touched by the theming service.
    FSigningLabel : TPaintBox;
    FCurrentBadge : TSigningBadge;
    FIconCache : TDPMIconCache;
    FHost : IDetailsHost;
    FLogger : IDPMIDELogger;
    FPackageInstaller : IPackageInstaller;
    FInstallerContext :IPackageInstallerContext;
    FRespositoryManager : IPackageRepositoryManager;

    FProjectGroup : IOTAProjectGroup;
    FIDEStyleServices : TCustomStyleServices;

    FPackageMetaData : IPackageSearchResultItem;
    FInstalledVersion : TPackageVersion;
    FSelectedVersion : TPackageVersion;

    FCancellationTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FVersionsDelayTimer : TTimer;

    FClosing : boolean;
    FIncludePreRelease : boolean;
    FDropdownOpen : boolean;

    FFetchVersions : boolean;

    FVersionsCache : IDictionary<string, IList<TPackageVersion>>;
    FMetaDataCache : IDictionary<string, IPackageSearchResultItem>;
    FVersionsCacheUdate : TStopWatch;
  protected
    procedure AssignImages;

    procedure ProjectSelectionChanged(Sender : TObject);
    procedure UpdateButtonState;
    procedure SetPackageLogo(const id : string);

    function GetReferenceVersion : TPackageVersion;

    procedure VersionsDelayTimerEvent(Sender : TObject);
    procedure OnDetailsUriClick(Sender : TObject; const uri : string; const element : TDetailElement);

    procedure DoGetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion);

    procedure UpdateProjectPackageVersions(const packageId : string);
    procedure DoPackageUninstall(options : TUnInstallOptions);
    procedure DoPackageInstall(options : TInstallOptions; const isUpdate : boolean);

    function GetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion): IAwaitable<IPackageSearchResultItem>;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

    procedure VersionGridOnInstallEvent(const project : string);
    procedure VersionGridOnUnInstallEvent(const project : string);
    procedure VersionGridOnUpgradeEvent(const project : string);
    procedure VersionGridOnDowngradeEvent(const project : string);

    procedure DoUpdateVersions(const sReferenceVersion : string; const versions : IList<TPackageVersion>);
    procedure SigningLabelClick(Sender : TObject);
    procedure SigningLabelPaint(Sender : TObject);
    procedure UpdateSigningBadge(const package : IPackageSearchResultItem);
    procedure AddCurrentSignerToTrustedPublishers;

    //versions cache
    function TryGetCachedVersions(const Id : string; const includePrerelease : boolean; out versions : IList<TPackageVersion>) : boolean;
    procedure UpdateVersionsCache(const id : string; const includePrerelease : boolean; const versions : IList<TPackageVersion>);


    procedure SetImageList(const value :  {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF});

  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const host : IDetailsHost; const projectGroup : IOTAProjectGroup);
    procedure SetPackage(const package : IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
    procedure ViewClosing;
    procedure ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
    procedure ProjectReloaded;


    property ImageList : {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF} read FImageList write SetImageList;

  end;

implementation

{$R *.dfm}

uses
  WinApi.ShellApi,
  WinApi.ActiveX,
  WinApi.CommCtrl,
  {$IF CompilerVersion > 34.0 }
   BrandingAPI,
  {$IFEND}
  DPM.IDE.Constants,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Config,
  DPM.Core.Package.Classes,
  DPM.Core.Package.SearchResults,
  DPM.Core.Configuration.Classes,
  DPM.Core.Dependency.Interfaces;
{ TGroupPackageDetailsFrame }

procedure TPackageDetailsFrame.btnInstallAllClick(Sender: TObject);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  //only install in projects it's not installed in already
  options.Projects := FProjectsGrid.GetNotInstalledProjects;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;

  DoPackageInstall(options, false);
end;

procedure TPackageDetailsFrame.btnUninstallAllClick(Sender: TObject);
var
  options : TUnInstallOptions;
begin
  options := TUnInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FPackageMetaData.Version;
  //only attempt to uninstall from projects it's actually installed in.
  options.Projects := FProjectsGrid.GetInstalledProjects;
  options.Platforms := [];
  options.CompilerVersion := IDECompilerVersion;

  DoPackageUninstall(options);
end;

procedure TPackageDetailsFrame.btnUpgradeAllClick(Sender: TObject);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  //only upgrade/downgrade in projects it's actually ibstalled in.
  options.Projects := FProjectsGrid.GetInstalledProjects;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.IsUpgrade := true;

  DoPackageInstall(options, true);
end;

procedure TPackageDetailsFrame.DebounceTimerTimer(Sender: TObject);
var
  package : IPackageSearchResultItem;
begin
  DebounceTimer.Enabled := false;
  package := FPackageMetaData; //take a local reference to stop rug being pulled
  if package = nil then
    exit;

  UpdateProjectPackageVersions(package.Id);
  FProjectsGrid.Enabled := true;
  if FFetchVersions then
    FVersionsDelayTimer.Enabled := true;
  if FSelectedVersion <> package.Version then
    DoGetPackageMetaDataAsync(package.Id, FSelectedVersion.ToStringNoMeta, package.CompilerVersion)
  else
    FDetailsPanel.SetDetails(FPackageMetaData);

end;

destructor TPackageDetailsFrame.Destroy;
begin
  {$IFNDEF USEIMAGECOLLECTION}
  FUpgradeBmp.Free;
  FDowngradeBmp.Free;
  {$ENDIF}

  inherited;
end;

procedure TPackageDetailsFrame.DoGetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion);
var
  item : IPackageSearchResultItem ;
  key : string;
begin
  key := LowerCase(id) + '-' + LowerCase(version);
  //try and get it from the cache first rather than going to the repo.
  if FMetaDataCache.TryGetValue(key, item) then
  begin
    FDetailsPanel.SetDetails(item);
    FSelectedVersion := item.Version;
    UpdateButtonState;
    exit;
  end;

  //didn't find it in the cache so fire off a task to get it from the repo.
  GetPackageMetaDataAsync(id, version, compilerVersion)
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
    procedure(const theResult : IPackageSearchResultItem)
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      key := LowerCase(theResult.id) + '-' + LowerCase(theResult.version.ToStringNoMeta);
      FMetaDataCache[key] := theResult;
      FDetailsPanel.SetDetails(theResult);
      FSelectedVersion := theResult.Version;
      UpdateButtonState;
    end);

end;


procedure TPackageDetailsFrame.DoPackageInstall(options: TInstallOptions; const isUpdate : boolean);
var
  installResult : boolean;
  packageMetadata : IPackageSearchResultItem;
  projectCount : integer;
begin
  installResult := false;
  try
    if FRequestInFlight then
      FCancellationTokenSource.Cancel;

    while FRequestInFlight do
      Application.ProcessMessages;
    FCancellationTokenSource.Reset;
    FLogger.Clear;
    FLogger.StartInstall(FCancellationTokenSource);
    projectCount := Length(options.Projects);
    if projectCount = 0 then
      projectCount := 1;
    FHost.BeginInstall(projectCount);
    installResult := FPackageInstaller.Install(FCancellationTokenSource.Token, options, FInstallerContext);
    if installResult then
    begin
      packageMetadata := FPackageMetaData;
      packageMetadata.Installed := true;
      packageMetadata.IsTransitive := false;
      FHost.PackageInstalled;
      UpdateProjectPackageVersions(packageMetaData.Id);
      SetPackage(packageMetadata, FIncludePreRelease, true);
      FInstalledVersion := options.Version;
      FPackageMetaData := packageMetadata;
      FLogger.Information('Package ' + FPackageMetaData.Id + ' - ' + FInstalledVersion.ToStringNoMeta + ' installed.');
    end
    else
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FInstalledVersion.ToStringNoMeta + ' did not install.');


  finally
    FLogger.EndInstall(installResult);
    FHost.EndInstall;
  end;

end;

procedure TPackageDetailsFrame.DoPackageUninstall(options: TUnInstallOptions);
var
  uninstallResult : boolean;
  packageMetaData : IPackageSearchResultItem;
  projectCount : integer;
begin
  uninstallResult := false;
  try
    if FRequestInFlight then
      FCancellationTokenSource.Cancel;

    while FRequestInFlight do
      Application.ProcessMessages;
    FCancellationTokenSource.Reset;
    FLogger.Clear;
    FLogger.StartUnInstall(FCancellationTokenSource);
    projectCount := Length(options.Projects);
    if projectCount = 0 then
      projectCount := 1;
    FHost.BeginUninstall(projectCount);

    FLogger.Information('UnInstalling package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version.ToStringNoMeta);
    uninstallResult := FPackageInstaller.UnInstall(FCancellationTokenSource.Token, options, FInstallerContext);
    if uninstallResult then
    begin
      packageMetaData := FPackageMetaData;
      FLogger.Information('Package ' + packageMetaData.Id + ' - ' + packageMetaData.Version.ToStringNoMeta + ' uninstalled.');
      FHost.PackageUninstalled(packageMetaData.Id);
      UpdateProjectPackageVersions(packageMetaData.Id);

      if FProjectsGrid.HasAnyInstalled then
      begin
        FInstalledVersion := options.Version;
        packageMetaData.Installed := true;
        SetPackage(packageMetaData, FIncludePreRelease, false);
      end
      else
      begin
        FInstalledVersion := options.Version;
        SetPackage(nil, FIncludePreRelease);
      end;
    end
    else
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version.ToStringNoMeta + ' did not uninstall.');

  finally
    FLogger.EndUnInstall(uninstallResult);
    FHost.EndInstall;
  end;
end;

procedure TPackageDetailsFrame.cboVersionsChange(Sender: TObject);
var
  sVersion : string;
  packageVer : TPackageVersion;
begin
  sVersion := cboVersions.Items[cboVersions.ItemIndex];
  packageVer := TPackageVersion.Parse(sVersion);
  FProjectsGrid.PackageVersion := packageVer;
  DoGetPackageMetaDataAsync(FPackageMetaData.Id, sVersion, FPackageMetaData.CompilerVersion);
end;

procedure TPackageDetailsFrame.cboVersionsCloseUp(Sender: TObject);
begin
  FDropdownOpen := false;
end;

procedure TPackageDetailsFrame.cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  sInstalledVersion : string;
begin
  cboVersions.Canvas.FillRect(Rect);

  if (cboVersions.Items.Count = 0) or (index < 0) then
    exit;

  cboVersions.Canvas.Font.Style := [];

  sInstalledVersion := FInstalledVersion.ToStringNoMeta;

  if odComboBoxEdit in State then
    cboVersions.Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, cboVersions.Items[Index]) //Visual state of the text in the edit control
  else
  begin
    if FDropdownOpen {and (Index > 0)} then
    begin
      if SameText(sInstalledVersion, cboVersions.Items[index]) then
        cboVersions.Canvas.Font.Style := [TFontStyle.fsBold];
    end;
    cboVersions.Canvas.TextOut(Rect.Left + 2, Rect.Top, cboVersions.Items[Index]); //Visual state of the text(items) in the deployed list
  end;

end;

procedure TPackageDetailsFrame.cboVersionsDropDown(Sender: TObject);
begin
  FDropdownOpen := true;
end;

procedure TPackageDetailsFrame.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
begin
  inherited;
  //scaling seems to be working ok without manual intervention here
end;

constructor TPackageDetailsFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  //not published in older versions, so gets removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont];
  {$ENDIF}
  {$IF CompilerVersion > 34.0 }
  if TIDEThemeMetrics.Font.Enabled then
  begin
    Font.Assign( TIDEThemeMetrics.Font.GetFont );
    TIDEThemeMetrics.Font.AdjustDPISize( Font, TIDEThemeMetrics.Font.Size, CurrentPPI );
  end;
  {$IFEND}
  //trying to inject the grid and the splitter inside the design time controls.
  FProjectsGrid := TVersionGrid.Create(AOwner);
  FProjectsGrid.Margins.Left := 5;
  FProjectsGrid.Margins.Right := 5;
  FProjectsGrid.AlignWithMargins := true;

  FProjectsGrid.Align := alClient;
  FProjectsGrid.Top := 0;
  FProjectsGrid.Height := 300;
  FProjectsGrid.ParentColor := false;
  FProjectsGrid.ParentBackground := false;
  FProjectsGrid.ParentFont := true;
  FProjectsGrid.DoubleBuffered := true;
  FProjectsGrid.OnSelectionChanged := Self.ProjectSelectionChanged;
  FProjectsGrid.OnInstallEvent := Self.VersionGridOnInstallEvent;
  FProjectsGrid.OnUnInstallEvent := Self.VersionGridOnUnInstallEvent;
  FProjectsGrid.OnUpgradeEvent := Self.VersionGridOnUpgradeEvent;
  FProjectsGrid.OnDowngradeEvent := Self.VersionGridOnDowngradeEvent;
  FProjectsGrid.Enabled := false;
  FProjectsGrid.Parent := pnlGridHost;

  FDetailsPanel := TPackageDetailsPanel.Create(AOwner);
  FDetailsPanel.ParentColor := false;
  FDetailsPanel.ParentBackground := false;
  FDetailsPanel.DoubleBuffered := true;
  FDetailsPanel.Align := alClient;
  FDetailsPanel.Height := 200;
  FDetailsPanel.Top := DetailsSplitter.Top + DetailsSplitter.Height;
  FDetailsPanel.OnUriClick := Self.OnDetailsUriClick;
  FDetailsPanel.Parent := sbPackageDetails;

  FVersionsDelayTimer := TTimer.Create(AOwner);
  FVersionsDelayTimer.Interval := 100;
  FVersionsDelayTimer.Enabled := false;
  FVersionsDelayTimer.OnTimer := VersionsDelayTimerEvent;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;


  {$IFNDEF USEIMAGECOLLECTION } //10.4 or later
  FUpgradeBmp := TBitmap.Create;
  FDowngradeBmp := TBitmap.Create;
  {$ENDIF}


//  moved to editorviewframe
//  LoadImages;
//  AssignImages;
//  FProjectsGrid.ImageList := FImageList;

  btnInstallAll.Caption := '';
  btnUpgradeAll.Caption := '';
  btnUninstallAll.Caption := '';

  // IDE-2: signing status header. Sits in pnlPackageId next to the package
  // id so users see the trust state alongside the package identity at a
  // glance. Click opens a small details dialog. Updated by UpdateSigningBadge
  // whenever SetPackage runs.
  //
  // Built as a TPaintBox + custom OnPaint because the IDE theming service
  // (IOTAIDEThemingServices.ApplyTheme) walks every TLabel descendant and
  // forces Font.Color back to clWindowText, ignoring StyleElements. A paint
  // box paints whatever its OnPaint draws — the theming service has no hook
  // into our pen colour.
  FSigningLabel := TPaintBox.Create(Self);
  FSigningLabel.Parent := pnlPackageId;
  FSigningLabel.Left := 47;
  FSigningLabel.Top := 36;
  FSigningLabel.Width := 0;     // sized in UpdateSigningBadge once we know the caption
  FSigningLabel.Height := 16;
  FSigningLabel.Cursor := crHandPoint;
  FSigningLabel.ShowHint := true;
  FSigningLabel.Hint := 'Click for signature details';
  FSigningLabel.OnClick := SigningLabelClick;
  FSigningLabel.OnPaint := SigningLabelPaint;
  FSigningLabel.Visible := false;



  FVersionsCache := TCollections.CreateDictionary<string, IList<TPackageVersion>>;
  FVersionsCacheUdate := TStopWatch.Create;
  FMetaDataCache := TCollections.CreateDictionary<string, IPackageSearchResultItem>;
  SetPackage(nil,false);

//  ThemeChanged;




end;


procedure TPackageDetailsFrame.Init(const container: TContainer; const iconCache: TDPMIconCache; const config: IConfiguration; const host: IDetailsHost; const projectGroup : IOTAProjectGroup);
begin
  FIconCache := iconCache;
  FConfiguration := config;
  FLogger := container.Resolve<IDPMIDELogger>;
  FPackageInstaller := container.Resolve<IPackageInstaller>;
  FInstallerContext := container.Resolve<IPackageInstallerContext>;
  FRespositoryManager := container.Resolve<IPackageRepositoryManager>;
  FPackageCache := container.Resolve<IPackageCache>;
  FReceiptService := container.Resolve<IReceiptService>;
  FConfigurationManager := container.Resolve<IConfigurationManager>;
  FHost := host;
  SetPackage(nil, FIncludePreRelease);

  FProjectGroup := projectGroup;
  ProjectReloaded; //loads the project Grid;
end;

procedure TPackageDetailsFrame.AssignImages;
{$IFNDEF USEIMAGECOLLECTION}
var
  bmp : TBitmap;
{$ENDIF}
begin
  {$IFNDEF USEIMAGECOLLECTION}
  bmp := TBitmap.Create;
  try
    bmp.SetSize(16,16);
    FImageList.GetBitmap(0, bmp); //Add
    btnInstallAll.Glyph.Assign(bmp);
    bmp.SetSize(0,0);
    bmp.SetSize(16,16);
    FImageList.GetBitmap(1, bmp); //remove
    btnUninstallAll.Glyph.Assign(bmp);
    FUpgradeBmp.SetSize(16,16);
    FImageList.GetBitmap(2, FUpgradeBmp); //upgrade
    btnUpgradeAll.Glyph.Assign(FUpgradeBmp);
    FDowngradeBmp.SetSize(16,16);
    FImageList.GetBitmap(3, FDowngradeBmp); //upgrade
  finally
    bmp.Free;
  end;
  {$ELSE}
    btnInstallAll.Images := FImageList;
    btnInstallAll.ImageIndex := 0;

    btnUpgradeAll.Images := FImageList;
    btnUpgradeAll.ImageIndex := 2;

    btnUninstallAll.Images := FImageList;
    btnUninstallAll.ImageIndex := 1;

  {$ENDIF}

end;


procedure TPackageDetailsFrame.OnDetailsUriClick(Sender: TObject; const uri: string; const element: TDetailElement);
begin
  case element of
    deNone : ;
    deLicense,
    deProjectUrl,
    deRepositoryUrl,
    deRepositoryCommit,
    deReportUrl : ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL);
    deTags : ;
  end;

end;

procedure TPackageDetailsFrame.ProjectReloaded;
var
  i : integer;
begin
  FProjectsGrid.BeginUpdate;
  FProjectsGrid.Clear;
  for i := 0 to FProjectGroup.ProjectCount -1 do
    FProjectsGrid.AddProject(FProjectGroup.Projects[i].FileName, '');
  FProjectsGrid.EndUpdate;
end;

procedure TPackageDetailsFrame.ProjectSelectionChanged(Sender: TObject);
begin
  UpdateButtonState;
end;


procedure TPackageDetailsFrame.UpdateProjectPackageVersions(const packageId : string);
var
  i: Integer;
  packageRefs : IPackageReference;
  projectRefs : IPackageReference;
  projectRef : IPackageReference;
begin
  packageRefs := FHost.GetPackageReferences;
  FProjectsGrid.BeginUpdate;
  try
    FProjectsGrid.PackageVersion := FSelectedVersion;
    for i := 0 to FProjectsGrid.RowCount -1 do
    begin
      if packageRefs <> nil then
      begin
        projectRef := nil;
        projectRefs := packageRefs.FindTopLevelChild(LowerCase(FProjectGroup.Projects[i].FileName));
        if projectRefs <> nil then
          projectRef := projectRefs.FindTopLevelChild(LowerCase(packageId));
      end;

      if projectRef <> nil then
        FProjectsGrid.ProjectVersion[i] := projectRef.Version
      else
        FProjectsGrid.ProjectVersion[i] := TPackageVersion.Empty;
    end;
  finally
    FProjectsGrid.EndUpdate;
  end;
end;

function TPackageDetailsFrame.GetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion): IAwaitable<IPackageSearchResultItem>;
var
  repoManager : IPackageRepositoryManager;
begin

  //TODO: can we get from the package cache here???


  //local for capture
  repoManager := FRespositoryManager;
  repoManager.Initialize(FConfiguration);

  result := TAsync.Configure <IPackageSearchResultItem> (
    function(const cancelToken : ICancellationToken) : IPackageSearchResultItem
    begin
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      try
        result := repoManager.GetPackageMetaData(cancelToken, id, version, compilerVersion);
      finally
        CoUninitialize;
      end;

      //simulating long running.
    end, FCancellationTokenSource.Token);

end;


function TPackageDetailsFrame.GetReferenceVersion: TPackageVersion;
var
  latestVersion : TPackageVersion;
begin
  if FPackageMetaData = nil then
    exit(TPackageVersion.Empty);

  if FIncludePreRelease then
    latestVersion := FPackageMetaData.LatestVersion
  else
    latestVersion := FPackageMetaData.LatestStableVersion;

  if latestVersion > FPackageMetaData.Version then
    result := latestVersion
  else
    result := FPackageMetaData.Version;


end;

procedure TPackageDetailsFrame.SetImageList(const value :  {$IFDEF USEIMAGECOLLECTION} TVirtualImageList {$ELSE} TImageList {$ENDIF});
begin
  FImageList := value;
  FProjectsGrid.ImageList := FImageList;
  if FImageList <> nil then
    AssignImages;

end;

procedure TPackageDetailsFrame.SigningLabelClick(Sender : TObject);
begin
  if FCurrentBadge.Detail = '' then
    exit;
  // When the package is signed by an untrusted publisher and we have the
  // author SPKI, offer to add that publisher to the trusted publishers list
  // rather than just showing the details. Plain MessageDlg keeps this
  // version-portable.
  if (FCurrentBadge.State = sbsUntrustedPublisher) and (FCurrentBadge.SignerSpkiHex <> '') then
  begin
    if MessageDlg(FCurrentBadge.Caption + sLineBreak + sLineBreak +
                  FCurrentBadge.Detail + sLineBreak + sLineBreak +
                  'Add this publisher to your trusted publishers?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      AddCurrentSignerToTrustedPublishers;
    exit;
  end;
  // Plain MessageDlg keeps this version-portable. A proper dedicated form
  // is a follow-up if/when we want richer formatting.
  MessageDlg(FCurrentBadge.Caption + sLineBreak + sLineBreak +
             FCurrentBadge.Detail, mtInformation, [mbOK], 0);
end;

procedure TPackageDetailsFrame.AddCurrentSignerToTrustedPublishers;

  // Normalise an SPKI for comparison: strip a 'sha256:' prefix and lowercase,
  // mirroring NormHex in DPM.Core.Trust.Policy so an IDE-added entry dedupes
  // against a CLI entry stored without the prefix.
  function NormaliseSpki(const value : string) : string;
  begin
    result := LowerCase(Trim(value));
    if (Length(result) > 7) and (Copy(result, 1, 7) = 'sha256:') then
      result := Copy(result, 8, MaxInt);
  end;

var
  configPath : string;
  config : IConfiguration;
  signing : ISigningConfig;
  publisher : ITrustedPublisherConfig;
  publisherName : string;
  spkiToStore : string;
  target : string;
  i : integer;
  found : boolean;
  textSize : TSize;
begin
  if (FCurrentBadge.State <> sbsUntrustedPublisher) or (FCurrentBadge.SignerSpkiHex = '') then
    exit;

  publisherName := FCurrentBadge.SignerName;
  if publisherName = '' then
    publisherName := 'Unknown publisher';

  FConfigurationManager.EnsureDefaultConfig;
  configPath := TConfigUtils.GetDefaultConfigFileName;
  config := FConfigurationManager.LoadConfig(configPath);
  if config = nil then
  begin
    FLogger.Error('Could not load configuration file to add trusted publisher.');
    exit;
  end;
  signing := config.Signing;

  spkiToStore := 'sha256:' + LowerCase(Trim(FCurrentBadge.SignerSpkiHex));
  target := NormaliseSpki(spkiToStore);

  // Dedupe against any existing entry (CLI or IDE added). If found, just
  // refresh the display name; otherwise add a new entry.
  found := false;
  for i := 0 to signing.TrustedPublishers.Count - 1 do
  begin
    if NormaliseSpki(signing.TrustedPublishers[i].Spki) = target then
    begin
      signing.TrustedPublishers[i].Name := publisherName;
      found := true;
      break;
    end;
  end;
  if not found then
  begin
    publisher := TTrustedPublisherConfig.Create;
    publisher.Name := publisherName;
    publisher.Spki := spkiToStore;
    signing.TrustedPublishers.Add(publisher);
  end;

  if not FConfigurationManager.SaveConfig(config, configPath) then
  begin
    FLogger.Error('Failed to save configuration when adding trusted publisher.');
    exit;
  end;
  FLogger.Information('Added trusted publisher: ' + publisherName + '  ' + spkiToStore);

  // Keep the in-memory session config in sync so re-selecting this package
  // (which re-resolves the badge against FConfiguration) doesn't revert to the
  // amber 'not pinned' state. The on-disk default config saved above remains
  // the source of truth for the installer / CLI.
  if (FConfiguration <> nil) and (FConfiguration.Signing <> nil) then
  begin
    found := false;
    for i := 0 to FConfiguration.Signing.TrustedPublishers.Count - 1 do
    begin
      if NormaliseSpki(FConfiguration.Signing.TrustedPublishers[i].Spki) = target then
      begin
        FConfiguration.Signing.TrustedPublishers[i].Name := publisherName;
        found := true;
        break;
      end;
    end;
    if not found then
    begin
      publisher := TTrustedPublisherConfig.Create;
      publisher.Name := publisherName;
      publisher.Spki := spkiToStore;
      FConfiguration.Signing.TrustedPublishers.Add(publisher);
    end;
  end;

  // Reflect the change on the badge immediately. The publisher is now pinned,
  // so the badge goes green to match what a re-resolve against the (now
  // updated) trusted-publishers list would produce.
  FCurrentBadge.State := sbsSignedTrusted;
  if FCurrentBadge.SignerName <> '' then
    FCurrentBadge.Caption := 'Signed by ' + FCurrentBadge.SignerName
  else
    FCurrentBadge.Caption := 'Signed by trusted publisher';
  FCurrentBadge.AccentColor := $00207020;   // green
  if FSigningLabel <> nil then
  begin
    textSize := FSigningLabel.Canvas.TextExtent(FCurrentBadge.Caption);
    FSigningLabel.Width := textSize.cx + 2;
    FSigningLabel.Height := textSize.cy;
    FSigningLabel.Invalidate;
  end;

  MessageDlg('Added "' + publisherName + '" to your trusted publishers.',
             mtInformation, [mbOK], 0);
end;

procedure TPackageDetailsFrame.SigningLabelPaint(Sender : TObject);
var
  pb : TPaintBox;
  bgColor : TColor;
begin
  pb := Sender as TPaintBox;
  // Match the panel background so the badge looks like a label, not a tile.
  // FIDEStyleServices is non-nil after ThemeChanged has run; before that we
  // fall back to the parent's resolved colour.
  if FIDEStyleServices <> nil then
    bgColor := FIDEStyleServices.GetSystemColor(clWindow)
  else if pb.Parent <> nil then
    bgColor := TPanel(pb.Parent).Color
  else
    bgColor := clBtnFace;
  pb.Canvas.Brush.Color := bgColor;
  pb.Canvas.Brush.Style := bsSolid;
  pb.Canvas.FillRect(pb.ClientRect);
  pb.Canvas.Brush.Style := bsClear;
  pb.Canvas.Font.Color := TColor(FCurrentBadge.AccentColor);
  pb.Canvas.TextOut(0, 0, FCurrentBadge.Caption);
end;

procedure TPackageDetailsFrame.UpdateSigningBadge(const package : IPackageSearchResultItem);
var
  badge : TSigningBadge;
  textSize : TSize;
  trustedSpkis : TArray<string>;
  i : integer;
begin
  if (package = nil) or (FSigningLabel = nil) then
    exit;
  // Collect the user's trusted-publisher SPKIs so the badge can distinguish a
  // publisher you've pinned (green) from a valid author signature you haven't
  // (amber + the add action). In permissive mode the receipt records 'trusted'
  // for any valid author signature regardless of your trusted-publishers list,
  // so this pin check is what surfaces the add-to-trusted-publishers action.
  SetLength(trustedSpkis, 0);
  if (FConfiguration <> nil) and (FConfiguration.Signing <> nil) then
  begin
    SetLength(trustedSpkis, FConfiguration.Signing.TrustedPublishers.Count);
    for i := 0 to FConfiguration.Signing.TrustedPublishers.Count - 1 do
      trustedSpkis[i] := FConfiguration.Signing.TrustedPublishers[i].Spki;
  end;
  // Pass the gallery-reported signing fields through. When the package
  // hasn't been installed yet there's no local receipt, but the feed
  // already carries isSigned/signedBy — the badge falls back to those so
  // the user sees the signer name before they install.
  badge := TSigningBadgeResolver.Resolve(FPackageCache, FReceiptService, package.Id, package.Version, IDECompilerVersion,
                                          package.IsSigned, package.SignedBy, trustedSpkis);
  FCurrentBadge := badge;
  // Size the paint box to fit the caption. Canvas.Font inherits from the
  // paint box's font, which inherits from the parent frame via ParentFont
  // — so we measure with exactly the same font used at paint time.
  textSize := FSigningLabel.Canvas.TextExtent(badge.Caption);
  FSigningLabel.Width := textSize.cx + 2;
  FSigningLabel.Height := textSize.cy;
  FSigningLabel.Visible := true;
  FSigningLabel.Invalidate;
end;

procedure TPackageDetailsFrame.SetPackage(const package: IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
var
  i: Integer;
  wasNil : boolean;
begin
  FIncludePreRelease := preRelease;
  DebounceTimer.Enabled := false;
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  //todo - should we wait here?

  FMetaDataCache.Clear;

  wasNil := FPackageMetaData = nil;
  FPackageMetaData := package;
  FFetchVersions := fetchVersions;
  if package <> nil then
  begin
    if package.Installed then
      FInstalledVersion := package.Version
    else
      FInstalledVersion := TPackageVersion.Empty;
    FSelectedVersion := GetReferenceVersion;

    lblPackageId.Caption := package.Id;
    SetPackageLogo(package.Id);
    UpdateSigningBadge(package);
    cboVersions.Enabled := true;
    imgPackageLogo.Visible := true;
    lblPackageId.Visible := true;
    lblVersionTitle.Visible := true;
    cboVersions.Enabled := true;
    cboVersions.Visible := true;
    FProjectsGrid.Visible := true;
    sbPackageDetails.Visible := true;
    Application.ProcessMessages;
    //if we had no package before then update immediately
    if wasNil then
      DebounceTimerTimer(DebounceTimer)
    else //otherwise debounce as we may be scrolling.
      DebounceTimer.Enabled := true;

  end
  else
  begin
    FInstalledVersion := TPackageVersion.Empty;
    FSelectedVersion := TPackageVersion.Empty;
    FProjectsGrid.BeginUpdate;
    try
      FProjectsGrid.Enabled := false;
      for i := 0 to FProjectsGrid.RowCount  -1 do
        FProjectsGrid.ProjectVersion[i] := TPackageVersion.Empty;
      FProjectsGrid.PackageVersion := TPackageVersion.Empty;
    finally
      FProjectsGrid.EndUpdate;
    end;
    FDetailsPanel.SetDetails(nil);
//    SetPackageLogo('');
    cboVersions.Clear;
    lblPackageId.Caption := '';

    imgPackageLogo.Visible := false;
    lblPackageId.Visible := false;
    if FSigningLabel <> nil then
      FSigningLabel.Visible := false;
    cboVersions.Enabled := false;
    lblVersionTitle.Visible := false;
    cboVersions.Visible := false;
    FProjectsGrid.Visible := false;
    sbPackageDetails.Visible := false;
  end;
  UpdateButtonState;
end;

procedure TPackageDetailsFrame.SetPackageLogo(const id: string);
var
  logo : IPackageIconImage;
  graphic : TGraphic;
begin
  if FIconCache = nil then
    exit;

  logo := FIconCache.Request(id);
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
end;


procedure TPackageDetailsFrame.ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
begin
  {$IFDEF THEMESERVICES}
  FIDEStyleServices := StyleServices;
  ideThemeSvc.ApplyTheme(Self);
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$ENDIF}

  Self.Color := FIDEStyleServices.GetSystemColor(clWindow);


  {$IF CompilerVersion >= 32.0}
  sbPackageDetails.ParentColor := false;
  sbPackageDetails.ParentBackground := false;
  sbPackageDetails.StyleElements := [seFont];
  sbPackageDetails.Color := FIDEStyleServices.GetSystemColor(clWindow);

  pnlPackageId.StyleElements := [seFont];
  pnlPackageId.Color := sbPackageDetails.Color;


  pnlVersion.StyleElements := [seFont];
  pnlVersion.Color := sbPackageDetails.Color;

  FDetailsPanel.StyleElements := [seFont];
  FDetailsPanel.Color := sbPackageDetails.Color;
  FDetailsPanel.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FProjectsGrid.Color := sbPackageDetails.Color;
  FProjectsGrid.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  {$ELSE}
  sbPackageDetails.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FDetailsPanel.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FDetailsPanel.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FProjectsGrid.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FProjectsGrid.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  {$IFEND}
  FProjectsGrid.StyleServices := FIDEStyleServices;

  // FSigningLabel is a TPaintBox that draws via SigningLabelPaint, which
  // reads FCurrentBadge.AccentColor on every paint. A theme change just
  // needs to trigger a repaint — there is no Font.Color to re-assert.
  if (FSigningLabel <> nil) and FSigningLabel.Visible then
    FSigningLabel.Invalidate;
end;

function TPackageDetailsFrame.TryGetCachedVersions(const Id: string;  const includePrerelease : boolean; out versions: IList<TPackageVersion>): boolean;
begin
  result := false;
  if FVersionsCacheUdate.IsRunning and (FVersionsCacheUdate.Elapsed.Seconds > 60) then
  begin
    FVersionsCache.Clear;
    FVersionsCacheUdate.Reset;
    FVersionsCacheUdate.Start;
    exit;
  end
  else
  begin
    result := FVersionsCache.TryGetValue(LowerCase(id + '-' + BoolToStr(includePrerelease,true)), versions);
    FVersionsCacheUdate.Start;
  end;

end;

procedure TPackageDetailsFrame.UpdateButtonState;
var
  referenceVersion : TPackageVersion;
  isTransitive : boolean;
begin
  if FPackageMetaData = nil then
  begin
    btnInstallAll.Enabled := false;
    btnUpgradeAll.Enabled := false;
    btnUninstallAll.Enabled := false;

    btnInstallAll.Visible := false;
    btnUpgradeAll.Visible := false;
    btnUninstallAll.Visible := false;
    exit;
  end
  else
  begin
    btnUninstallAll.Visible := true;
    btnUpgradeAll.Visible := true;
    btnInstallAll.Visible := true;
  end;

  referenceVersion := GetReferenceVersion;
  //Transitive packages are brought in by another package, so uninstalling would leave the parent
  //broken and the transitive would be re-added on the next restore. Upgrades are still allowed -
  //if the new version is outside the parent's allowed range the resolver will surface that.
  isTransitive := FPackageMetaData.IsTransitive;
  btnInstallAll.Enabled := FPackageMetaData <> nil;
  btnUpgradeAll.Enabled := (FPackageMetaData <> nil) and (FInstalledVersion <> TPackageVersion.Empty) and (FSelectedVersion <> FInstalledVersion);
  btnUninstallAll.Enabled := (FPackageMetaData <> nil) and (FInstalledVersion <> TPackageVersion.Empty) and (not isTransitive);

  if isTransitive then
    btnUninstallAll.Hint := 'Cannot uninstall - this package is a dependency of another installed package'
  else
    btnUninstallAll.Hint := 'Uninstall from all projects';

  {$IFNDEF USEIMAGECOLLECTION}
    if FSelectedVersion = FInstalledVersion then
    begin
      btnUpgradeAll.Glyph.Assign(nil);
    end
    else  if FSelectedVersion > FInstalledVersion then
    begin
      btnUpgradeAll.Glyph.Assign(FUpgradeBmp);
      btnUpgradeAll.Hint := 'Upgrade package in all projects';
    end
    else
    begin
      btnUpgradeAll.Glyph.Assign(FDowngradeBmp);
      btnUpgradeAll.Hint := 'Downgrade package in all projects';
    end;
  {$ELSE}
    if FSelectedVersion = FInstalledVersion then
    begin
      btnUpgradeAll.ImageIndex := -1;
    end
    else if FSelectedVersion > FInstalledVersion then
    begin
      btnUpgradeAll.ImageIndex := 2;
      btnUpgradeAll.Hint := 'Upgrade package in all projects';
    end
    else
    begin
      btnUpgradeAll.ImageIndex := 3;
      btnUpgradeAll.Hint := 'Downgrade package in all projects';
    end;
  {$ENDIF}

end;

procedure TPackageDetailsFrame.UpdateVersionsCache(const id: string; const includePrerelease : boolean; const versions: IList<TPackageVersion>);
begin
  FVersionsCacheUdate.Stop;
  if FVersionsCacheUdate.Elapsed.Seconds > 120 then
  begin
    FVersionsCache.Clear;
    FVersionsCacheUdate.Reset;
  end;
  FVersionsCache[LowerCase(id + '-' + BoolToStr(includePrerelease,true))] := versions;
  FVersionsCacheUdate.Start;
end;

procedure TPackageDetailsFrame.VersionGridOnDowngradeEvent(const project: string);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  options.ProjectPath := project;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.Force := true;
  DoPackageInstall(options, true);
end;

procedure TPackageDetailsFrame.VersionGridOnInstallEvent(const project: string);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  options.ProjectPath := project;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.Sources := FPackageMetaData.SourceName;
  DoPackageInstall(options, false);

end;

procedure TPackageDetailsFrame.VersionGridOnUnInstallEvent(const project: string);
var
  options : TUnInstallOptions;
begin
  options := TUnInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FPackageMetaData.Version;
  options.ProjectPath := project;
  options.Platforms := [];
  options.CompilerVersion := IDECompilerVersion;

  DoPackageUninstall(options);
end;

procedure TPackageDetailsFrame.VersionGridOnUpgradeEvent(const project: string);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  options.ProjectPath := project;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.IsUpgrade := true; //changing the installed version
  DoPackageInstall(options, true);
  //call common function
end;

procedure TPackageDetailsFrame.DoUpdateVersions(const sReferenceVersion : string; const versions : IList<TPackageVersion>);
  var
    version : TPackageVersion;
begin
  cboVersions.Items.BeginUpdate;
  try
    cboVersions.Clear;

    if versions.Any then
    begin
      for version in versions do
      begin
        if FPackageMetaData.IsTransitive then
        begin
          if FPackageMetaData.VersionRange.IsSatisfiedBy(version) then
            cboVersions.Items.Add(version.ToStringNoMeta);
        end
        else
          cboVersions.Items.Add(version.ToStringNoMeta);
      end;
      cboVersions.ItemIndex := cboVersions.Items.IndexOf(sReferenceVersion);
    end
    else
    begin
      //no versions returned
      //this can happen if there is only 1 version and its prerelease and
      //prerlease not checked.
      //in this case we will just add the reference version
      if sReferenceVersion <> '' then
      begin
        cboVersions.Items.Add(sReferenceVersion);
        cboVersions.ItemIndex := 0;
      end;
      //btnInstall.Enabled := false;
    end;
  finally
    cboVersions.Items.EndUpdate;
  end;
end;


procedure TPackageDetailsFrame.VersionsDelayTimerEvent(Sender: TObject);
var
  repoManager : IPackageRepositoryManager;
  sReferenceVersion : string;
  includePreRelease : boolean;
  cachedVersions : IList<TPackageVersion>;


begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  while FRequestInFlight do
    Application.ProcessMessages;
  FCancellationTokenSource.Reset;

  cboVersions.Clear;

  sReferenceVersion := GetReferenceVersion.ToStringNoMeta;

  includePreRelease := FIncludePreRelease or (not FPackageMetaData.Version.IsStable);

  //avoid going to the repo for the info all the time, new versions are not published
  //that often so we can afford to cache the info.
  if TryGetCachedVersions(FPackageMetaData.Id, includePreRelease, cachedVersions ) then
  begin
    DoUpdateVersions(sReferenceVersion, cachedVersions);
    exit;
  end;


  repoManager := FRespositoryManager; //local for capture
  repoManager.Initialize(FConfiguration);


  TAsync.Configure<IList<TPackageVersion>> (
    function(const cancelToken : ICancellationToken) : IList<TPackageVersion>
    begin
      //need this for the http api/
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      try
        result := repoManager.GetPackageVersions(cancelToken, IDECompilerVersion, FPackageMetaData.Id, includePreRelease);
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
    procedure(const versions : IList<TPackageVersion>)
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      FLogger.Debug('Got package versions .');
      if FPackageMetaData <> nil then
        UpdateVersionsCache(FPackageMetaData.Id, includePreRelease, versions);
      DoUpdateVersions(sReferenceVersion, versions);
    end);

end;

procedure TPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  FHost := nil;
  FCancellationTokenSource.Cancel;
end;

end.
