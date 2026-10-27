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
  Vcl.Menus,
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

    //the details / versions requests for the package on show. Replaced (the old one cancelled) when the package
    //changes or an operation starts - a superseded request is delivered as cancelled on its own token, never
    //'reset' and its result applied to whatever is showing now.
    FRequestTokenSource : ICancellationTokenSource;
    //the install / uninstall running now (nil when none), for the log window's cancel button and the view closing.
    FOperationTokenSource : ICancellationTokenSource;
    //which request a callback belongs to - one for an earlier request does nothing.
    FDetailsGeneration : integer;
    FVersionsGeneration : integer;
    //whether the view still exists, for callbacks - see IViewLifetime.
    FLifetime : IViewLifetime;
    FVersionsDelayTimer : TTimer;

    FClosing : boolean;
    FIncludePreRelease : boolean;
    FDropdownOpen : boolean;

    FFetchVersions : boolean;

    //the versions of the packages looked at (key id-prerelease) - dropped a minute after the first of them was
    //got (FVersionsCacheAge), or on Refresh.
    FVersionsCache : IDictionary<string, IList<TPackageVersion>>;
    FMetaDataCache : IDictionary<string, IPackageSearchResultItem>;
    FVersionsCacheAge : TStopWatch;
    //The package id the projects grid rows were last populated for. Used by the use-source toggle
    //as a fallback - project reloads can re-enter SetPackage(nil) and null FPackageMetaData while
    //the grid is still showing (and clickable for) this package.
    FProjectsGridPackageId : string;

    //Single popup menu shown when a row's '...' button is clicked - rebuilt on each invocation
    //(never one per row). The FMenu* fields carry the clicked row's context to the item handlers.
    FPackageMenu : TPopupMenu;
    FMenuProject : string;
    FMenuUseSource : boolean;
    FMenuCachePath : string;
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
    //plans an upgrade or downgrade, confirms any companion packages with the user, then applies it. Frees options.
    procedure DoPackageUpgrade(options : TInstallOptions);
    //cancels the details / versions requests in flight (they are recognised as stale when they arrive - there is
    //nothing to wait for), for a new package or before an operation.
    procedure CancelPendingRequest;
    //a token source of its own for an install / uninstall, which ViewClosing cancels.
    function BeginOperation : ICancellationTokenSource;
    //the package is in the project now - everyPlatform is false when some of its platforms failed.
    procedure InstallSucceeded(const packageMetadata : IPackageSearchResultItem; const options : TInstallOptions;
                               const everyPlatform : boolean);

    function GetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion): IAwaitable<IPackageSearchResultItem>;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

    procedure VersionGridOnInstallEvent(const project : string);
    procedure VersionGridOnUnInstallEvent(const project : string);
    procedure VersionGridOnUpgradeEvent(const project : string);
    procedure VersionGridOnDowngradeEvent(const project : string);
    procedure VersionGridOnUseSourceChanged(const project : string; const useSource : boolean);
    procedure VersionGridOnShowPackageMenu(const project : string; const screenPos : TPoint);
    procedure UseSourceMenuItemClick(Sender : TObject);
    procedure ShowCacheLocationMenuItemClick(Sender : TObject);

    procedure DoUpdateVersions(const sReferenceVersion : string; const versions : IList<TPackageVersion>);
    procedure SigningLabelClick(Sender : TObject);
    procedure SigningLabelPaint(Sender : TObject);
    procedure UpdateSigningBadge(const package : IPackageSearchResultItem);
    procedure AddCurrentSignerToTrustedPublishers;

    //a row per project in the group, but not excluding (a project that is closing) - then the versions shown.
    procedure RebuildProjectRows(const excluding : string);

    //versions cache
    procedure ExpireVersionsCache;
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
    //projectFile is closing - it is still in the group when the IDE says so, so the rows are rebuilt without it.
    procedure ProjectClosing(const projectFile : string);
    //Refresh, or the sources changed - the versions are asked for again.
    procedure ClearVersionsCache;


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
  DPM.Core.Package.InstallPlan,
  DPM.Core.Configuration.Classes,
  DPM.Core.Dependency.Interfaces,
  DPM.IDE.ToolsAPI,
  DPM.IDE.MessageForm,
  DPM.Core.Diagnostics;

//A MessageDlg that can't open behind the log window - see TDPMMessageForm.BeginModalPrompt.
function PromptDlg(const msg : string; const dlgType : TMsgDlgType; const buttons : TMsgDlgButtons) : integer;
begin
  TDPMMessageForm.BeginModalPrompt;
  try
    result := MessageDlg(msg, dlgType, buttons, 0);
  finally
    TDPMMessageForm.EndModalPrompt;
  end;
end;

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
  //every project in the group - an upgrade only changes projects the package is installed in, but the
  //packages that must move with it (companions) may be installed in others, and the group has to stay
  //on one version of them.
  options.Projects := FProjectsGrid.GetAllProjects;
  options.Platforms := [];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.IsUpgrade := true;

  DoPackageUpgrade(options);
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
  //destroyed without ViewClosing (the IDE shutting down) - the callbacks still to come must not find the frame.
  if FLifetime <> nil then
    FLifetime.Kill;
  if FRequestTokenSource <> nil then
    FRequestTokenSource.Cancel;
  {$IFNDEF USEIMAGECOLLECTION}
  FUpgradeBmp.Free;
  FDowngradeBmp.Free;
  {$ENDIF}
  FPackageMenu.Free;

  inherited;
end;

procedure TPackageDetailsFrame.DoGetPackageMetaDataAsync(const id: string; const version: string; const compilerVersion: TCompilerVersion);
var
  item : IPackageSearchResultItem ;
  key : string;
  lifetime : IViewLifetime;
  generation : integer;
begin
  //This version's details supersede whatever was asked for before - a cached answer included, so an older request
  //still in flight can't overwrite it (or FSelectedVersion, which Install All installs) when it lands.
  Inc(FDetailsGeneration);
  generation := FDetailsGeneration;
  lifetime := FLifetime;

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
  //Each callback first checks the view still exists (only then is Self safe to touch) and that this is still the
  //latest details request.
  GetPackageMetaDataAsync(id, version, compilerVersion)
  .OnException(
    procedure(const e : Exception)
    begin
      if (not lifetime.Alive) or (generation <> FDetailsGeneration) then
        exit;
      LogException(FLogger, e);
    end)
  .OnCancellation(
    procedure
    begin
      if (not lifetime.Alive) or (generation <> FDetailsGeneration) then
        exit;
      FLogger.Debug('Cancelled getting the package details.');
    end)
  .Await(
    procedure(const theResult : IPackageSearchResultItem)
    begin
      if (not lifetime.Alive) or (generation <> FDetailsGeneration) then
        exit;
      //no source had it.
      if theResult = nil then
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
  operation : ICancellationTokenSource;
begin
  installResult := false;
  //capture the current package metadata before we pump any messages - pumping
  //(progress updates during install) can re-enter SetPackage from the editor
  //view and set FPackageMetaData to nil.
  packageMetadata := FPackageMetaData;
  try
    CancelPendingRequest;
    FLogger.Clear;
    operation := BeginOperation;
    FLogger.StartInstall(operation);
    projectCount := Length(options.Projects);
    if projectCount = 0 then
      projectCount := 1;
    FHost.BeginInstall(projectCount);
    installResult := FPackageInstaller.Install(operation.Token, options, FInstallerContext);
    //the installer reported why - this is the outcome. A platform that failed after others built still
    //left the project saved for those, so it has to be treated as installed (and reloaded).
    if installResult or FInstallerContext.AnyProjectCommitted then
      InstallSucceeded(packageMetadata, options, installResult)
    else
      FLogger.Information('Package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta + ' did not install.', true);


  finally
    FOperationTokenSource := nil;
    FLogger.EndInstall(installResult);
    //ViewClosing lets go of the host - the view may have closed while this ran.
    if FHost <> nil then
      FHost.EndInstall;
  end;

end;

procedure TPackageDetailsFrame.CancelPendingRequest;
begin
  //Cancelled on their token, so they are delivered as cancelled whenever they finish, and their generation no
  //longer matches - no waiting (pumping messages here re-entered SetPackage), and no Reset (that un-cancelled the
  //old requests, and their results were applied).
  FRequestTokenSource.Cancel;
  FRequestTokenSource := TCancellationTokenSourceFactory.Create;
  Inc(FDetailsGeneration);
  Inc(FVersionsGeneration);
end;

function TPackageDetailsFrame.BeginOperation : ICancellationTokenSource;
begin
  result := TCancellationTokenSourceFactory.Create;
  FOperationTokenSource := result;
end;

procedure TPackageDetailsFrame.InstallSucceeded(const packageMetadata : IPackageSearchResultItem; const options : TInstallOptions;
                                                const everyPlatform : boolean);
begin
  if packageMetadata <> nil then
  begin
    packageMetadata.Installed := true;
    packageMetadata.IsTransitive := false;
  end;
  if FHost <> nil then
    FHost.PackageInstalled;
  //not when the view closed while it ran (a prompt during it runs a message loop) - there is nothing to show it in.
  if (packageMetadata <> nil) and (FHost <> nil) then
  begin
    UpdateProjectPackageVersions(packageMetaData.Id);
    SetPackage(packageMetadata, FIncludePreRelease, true);
    FPackageMetaData := packageMetadata;
  end;
  FInstalledVersion := options.Version;
  if everyPlatform then
    FLogger.Information('Package ' + options.PackageId + ' - ' + FInstalledVersion.ToStringNoMeta + ' installed.')
  else
    FLogger.Information('Package ' + options.PackageId + ' - ' + FInstalledVersion.ToStringNoMeta +
      ' installed, but not for every platform - see above.', true);
end;

//Changing an installed package's version (up or down) can need other installed packages to move with it
//(companions - e.g. packages that pin the same dependency). It is planned first and the user confirms any
//companions before anything changes.
//That is asked between planning and installing, never while an install is running - the IDE must not
//pump messages mid-operation.
procedure TPackageDetailsFrame.DoPackageUpgrade(options : TInstallOptions);
var
  plan : IInstallPlan;
  installResult : boolean;
  sessionOpen : boolean;
  hostInstalling : boolean;
  packageMetadata : IPackageSearchResultItem;
  line : string;
  message : string;
  operation : ICancellationTokenSource;
begin
  installResult := false;
  sessionOpen := false;
  hostInstalling := false;
  //capture the current package metadata before we pump any messages - see DoPackageInstall.
  packageMetadata := FPackageMetaData;
  try
    try
      CancelPendingRequest;
      FLogger.Clear;
      operation := BeginOperation;
      FLogger.StartInstall(operation);
      sessionOpen := true;

      if not FPackageInstaller.PlanInstall(operation.Token, options, plan) then
      begin
        LogPlanErrors(FLogger, plan);
        FLogger.Information('Package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta + ' cannot be installed.', true);
        exit;
      end;
      for line in plan.SummaryLines do
        FLogger.Information(line, true);

      if plan.HasCompanions then
      begin
        FLogger.EndInstall(true);
        sessionOpen := false;
        message := 'Installing ' + options.PackageId + ' ' + options.Version.ToStringNoMeta + ' needs other installed packages to change with it :' + sLineBreak + sLineBreak;
        for line in plan.SummaryLines do
          message := message + line + sLineBreak;
        message := message + sLineBreak + 'Continue?';
        if PromptDlg(message, mtConfirmation, [mbYes, mbNo]) <> mrYes then
          exit;
        //the view may have closed while the question was up.
        if FHost = nil then
          exit;
        //a fresh one - the planning session's may have been cancelled from its log window.
        operation := BeginOperation;
        FLogger.Clear;
        FLogger.StartInstall(operation);
        sessionOpen := true;
      end;

      FHost.BeginInstall(plan.Projects.Count);
      hostInstalling := true;
      installResult := FPackageInstaller.ApplyInstallPlan(operation.Token, options, plan, FInstallerContext);
      //see DoPackageInstall - a project saved for the platforms that built is installed.
      if installResult or FInstallerContext.AnyProjectCommitted then
        InstallSucceeded(packageMetadata, options, installResult)
      else
        FLogger.Information('Package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta + ' did not install.', true);
    finally
      FOperationTokenSource := nil;
      if sessionOpen then
        FLogger.EndInstall(installResult);
      if hostInstalling and (FHost <> nil) then
        FHost.EndInstall;
    end;
  finally
    options.Free;
  end;
end;

procedure TPackageDetailsFrame.DoPackageUninstall(options: TUnInstallOptions);
var
  uninstallResult : boolean;
  packageMetaData : IPackageSearchResultItem;
  projectCount : integer;
  operation : ICancellationTokenSource;
begin
  uninstallResult := false;
  //capture the current package metadata before we pump any messages - pumping
  //(progress updates during uninstall) can re-enter SetPackage from the editor
  //view and set FPackageMetaData to nil.
  packageMetaData := FPackageMetaData;
  try
    CancelPendingRequest;
    FLogger.Clear;
    operation := BeginOperation;
    FLogger.StartUnInstall(operation);
    projectCount := Length(options.Projects);
    if projectCount = 0 then
      projectCount := 1;
    FHost.BeginUninstall(projectCount);

    FLogger.Information('UnInstalling package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta);
    uninstallResult := FPackageInstaller.UnInstall(operation.Token, options, FInstallerContext);
    if uninstallResult then
    begin
      FLogger.Information('Package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta + ' uninstalled.');
      if FHost <> nil then
        FHost.PackageUninstalled;
      UpdateProjectPackageVersions(options.PackageId);

      if FProjectsGrid.HasAnyInstalled then
      begin
        FInstalledVersion := options.Version;
        if packageMetaData <> nil then
        begin
          packageMetaData.Installed := true;
          SetPackage(packageMetaData, FIncludePreRelease, false);
        end;
      end
      else
      begin
        FInstalledVersion := options.Version;
        SetPackage(nil, FIncludePreRelease);
      end;
    end
    else
      FLogger.Information('Package ' + options.PackageId + ' - ' + options.Version.ToStringNoMeta + ' did not uninstall.', true);

  finally
    FOperationTokenSource := nil;
    FLogger.EndUnInstall(uninstallResult);
    //ViewClosing lets go of the host - the view may have closed while this ran.
    if FHost <> nil then
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
  FProjectsGrid.OnUseSourceChangedEvent := Self.VersionGridOnUseSourceChanged;
  FProjectsGrid.OnShowPackageMenuEvent := Self.VersionGridOnShowPackageMenu;
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
  FRequestTokenSource := TCancellationTokenSourceFactory.Create;
  FLifetime := TViewLifetime.Create;


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
  FVersionsCacheAge := TStopWatch.Create;
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


//The links come from the package's metadata, and the shell runs a file: link or a \\server\share\program.exe
//path just as readily as it opens a web page in a browser - so only web links are opened, anything else is
//shown to the user instead.
procedure OpenPackageLink(const uri : string);
begin
  if uri = '' then
    exit;
  if TStringUtils.IsWebUrl(uri) then
    ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL)
  else
    PromptDlg('DPM only opens http and https links. The link the package gives is :' + sLineBreak + sLineBreak + uri,
              mtInformation, [mbOK]);
end;

procedure TPackageDetailsFrame.OnDetailsUriClick(Sender: TObject; const uri: string; const element: TDetailElement);
begin
  case element of
    deNone : ;
    deLicense,
    deProjectUrl,
    deRepositoryUrl,
    deRepositoryCommit,
    deReportUrl : OpenPackageLink(uri);
    deTags : ;
  end;

end;

procedure TPackageDetailsFrame.ProjectReloaded;
begin
  RebuildProjectRows('');
end;

procedure TPackageDetailsFrame.ProjectClosing(const projectFile : string);
begin
  RebuildProjectRows(projectFile);
end;

procedure TPackageDetailsFrame.RebuildProjectRows(const excluding : string);
var
  i : integer;
begin
  FProjectsGrid.BeginUpdate;
  try
    FProjectsGrid.Clear;
    for i := 0 to FProjectGroup.ProjectCount -1 do
    begin
      if (excluding <> '') and SameText(FProjectGroup.Projects[i].FileName, excluding) then
        continue;
      FProjectsGrid.AddProject(FProjectGroup.Projects[i].FileName, '');
    end;
  finally
    FProjectsGrid.EndUpdate;
  end;

  //Clearing/re-adding the rows above drops each row's installed version, HasSource and UseSource.
  //Repopulate them for the package currently shown so the "Use Source" checkbox keeps its correct
  //state and stays enabled - otherwise a reload (e.g. after a search-path change, or on focus)
  //leaves the checkbox disabled and the next click is ignored. FPackageMetaData may have been
  //nulled by a re-entrant SetPackage during the reload - fall back to the id the grid was last
  //populated for.
  if FPackageMetaData <> nil then
    UpdateProjectPackageVersions(FPackageMetaData.Id)
  else if FProjectsGridPackageId <> '' then
    UpdateProjectPackageVersions(FProjectsGridPackageId);
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
  //remember which package the grid rows now represent - see FProjectsGridPackageId.
  FProjectsGridPackageId := packageId;
  //the view has closed (during an operation, from one of its prompts) - the projects are read through it.
  if FHost = nil then
    exit;
  packageRefs := FHost.GetPackageReferences;
  FProjectsGrid.BeginUpdate;
  try
    FProjectsGrid.PackageVersion := FSelectedVersion;
    for i := 0 to FProjectsGrid.RowCount -1 do
    begin
      if packageRefs <> nil then
      begin
        projectRef := nil;
        //by the row's project - the group's projects needn't be the rows, a row at a time (one is closing, say).
        projectRefs := packageRefs.FindTopLevelChild(LowerCase(FProjectsGrid.ProjectName[i]));
        if projectRefs <> nil then
          projectRef := projectRefs.FindTopLevelChild(LowerCase(packageId));
      end;

      if projectRef <> nil then
      begin
        FProjectsGrid.ProjectVersion[i] := projectRef.Version;
        //'use source' can only be offered for an installed package that actually ships source.
        //HasSource is a cache check on the installed version; UseSource is the current-session choice.
        FProjectsGrid.ProjectHasSource[i] := FPackageCache.HasSource(projectRef);
        FProjectsGrid.ProjectUseSource[i] := FInstallerContext.GetUseSource(FProjectsGrid.ProjectName[i], packageId);
      end
      else
      begin
        FProjectsGrid.ProjectVersion[i] := TPackageVersion.Empty;
        FProjectsGrid.ProjectHasSource[i] := false;
        FProjectsGrid.ProjectUseSource[i] := false;
      end;
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

    end, FRequestTokenSource.Token);

end;


function TPackageDetailsFrame.GetReferenceVersion: TPackageVersion;
var
  latestVersion : TPackageVersion;
begin
  if FPackageMetaData = nil then
    exit(TPackageVersion.Empty);

  //For a transitive (implicitly installed) package the version dropdown is filtered to the parent's
  //required range (see DoUpdateVersions), and a newer 'latest' version is normally outside that
  //range. Defaulting to latest would pre-select a version that isn't in the list, leaving the
  //dropdown blank. Use the installed version instead - it always satisfies the range and is present
  //in the list.
  if FPackageMetaData.IsTransitive then
    exit(FPackageMetaData.Version);

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
    if PromptDlg(FCurrentBadge.Caption + sLineBreak + sLineBreak +
                 FCurrentBadge.Detail + sLineBreak + sLineBreak +
                 'Add this publisher to your trusted publishers?',
                 mtConfirmation, [mbYes, mbNo]) = mrYes then
      AddCurrentSignerToTrustedPublishers;
    exit;
  end;
  // Plain MessageDlg keeps this version-portable. A proper dedicated form
  // is a follow-up if/when we want richer formatting.
  PromptDlg(FCurrentBadge.Caption + sLineBreak + sLineBreak +
            FCurrentBadge.Detail, mtInformation, [mbOK]);
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
    exit;
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
    exit;
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

  PromptDlg('Added "' + publisherName + '" to your trusted publishers.',
            mtInformation, [mbOK]);
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
  //another package - what was being fetched for the last one is no longer wanted.
  CancelPendingRequest;

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

procedure TPackageDetailsFrame.ClearVersionsCache;
begin
  FVersionsCache.Clear;
  FVersionsCacheAge.Reset;
end;

procedure TPackageDetailsFrame.ExpireVersionsCache;
begin
  //TotalSeconds - Elapsed.Seconds is the seconds part (0-59), so the versions were never dropped.
  if FVersionsCacheAge.IsRunning and (FVersionsCacheAge.Elapsed.TotalSeconds > 60) then
    ClearVersionsCache;
end;

function TPackageDetailsFrame.TryGetCachedVersions(const Id: string;  const includePrerelease : boolean; out versions: IList<TPackageVersion>): boolean;
begin
  ExpireVersionsCache;
  result := FVersionsCache.TryGetValue(LowerCase(id + '-' + BoolToStr(includePrerelease,true)), versions);
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
  ExpireVersionsCache;
  if not FVersionsCacheAge.IsRunning then
    FVersionsCacheAge.Start;
  FVersionsCache[LowerCase(id + '-' + BoolToStr(includePrerelease,true))] := versions;
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
  //changing the installed version - packages that pin the same dependencies move down with it.
  options.IsUpgrade := true;
  DoPackageUpgrade(options);
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

  //drop any current-session 'use source' choice for this package/project - it's being removed.
  FInstallerContext.ClearUseSource(project, FPackageMetaData.Id);

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
  DoPackageUpgrade(options);
end;

procedure TPackageDetailsFrame.VersionGridOnUseSourceChanged(const project: string; const useSource: boolean);
var
  packageId : string;
begin
  //The grid only fires this for the package its rows were populated for. FPackageMetaData is the
  //normal source of that id; fall back to FProjectsGridPackageId in case an earlier reload nulled it.
  if FPackageMetaData <> nil then
    packageId := FPackageMetaData.Id
  else
    packageId := FProjectsGridPackageId;
  if packageId = '' then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'Unable to determine which package the use source change was for - ignoring it.');
    exit;
  end;

  //Record the current-session choice (never persisted to the dproj) and rewrite just this project's
  //DPMSearch entries - source vs precompiled lib. No reinstall/rebuild and no design-time package
  //work: everything is already in the cache, only the search paths change.

  FInstallerContext.SetUseSource(project, packageId, useSource);
  //a token of its own - nothing else's cancelling (a package change, a request) applies to it.
  if FPackageInstaller.RefreshProjectSearchPaths(TCancellationTokenSourceFactory.Create.Token, project, IDECompilerVersion, FConfiguration.FileName, FInstallerContext) then
  begin
    FLogger.Information('Updated search paths for [' + packageId + '] in project [' + project + '] (use source = ' + BoolToStr(useSource, true) + ').');
    //Re-sync the IDE's in-memory copy of the dproj so Alt-Tab doesn't prompt to reload. This does
    //NOT refetch the package list (see TDPMEditViewFrame.SearchPathsChanged).
    if FHost <> nil then
      FHost.SearchPathsChanged;
  end
  else
    FLogger.Information('The search paths for [' + packageId + '] in [' + project + '] were not updated.', true);

  //Refresh only this grid's checkbox/version state - no package list reload.
  UpdateProjectPackageVersions(packageId);
end;

procedure TPackageDetailsFrame.VersionGridOnShowPackageMenu(const project : string; const screenPos : TPoint);
var
  packageId : string;
  i : integer;
  rowIndex : integer;
  installedVersion : TPackageVersion;
  hasSource : boolean;
  menuItem : TMenuItem;
begin
  //resolve the package id the grid rows represent - same fallback as VersionGridOnUseSourceChanged.
  if FPackageMetaData <> nil then
    packageId := FPackageMetaData.Id
  else
    packageId := FProjectsGridPackageId;
  if packageId = '' then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'Unable to determine which package the menu was for - ignoring it.');
    exit;
  end;

  //locate the grid row for this project so we can read its installed version / source state.
  rowIndex := -1;
  for i := 0 to FProjectsGrid.RowCount - 1 do
  begin
    if SameText(FProjectsGrid.ProjectName[i], project) then
    begin
      rowIndex := i;
      break;
    end;
  end;
  if rowIndex < 0 then
    exit;

  installedVersion := FProjectsGrid.ProjectVersion[rowIndex];
  //the grid only fires this for an installed row, but guard anyway.
  if installedVersion.IsEmpty then
    exit;

  hasSource := FProjectsGrid.ProjectHasSource[rowIndex];

  //capture context for the item click handlers - the menu items have no per-invocation payload.
  FMenuProject := project;
  FMenuUseSource := FProjectsGrid.ProjectUseSource[rowIndex];
  FMenuCachePath := FPackageCache.GetPackagePath(packageId, installedVersion.ToStringNoMeta, IDECompilerVersion);

  //rebuild the single popup menu on demand (never one per row).
  if Assigned(FPackageMenu) then
    FreeAndNil(FPackageMenu);
  FPackageMenu := TPopupMenu.Create(Self);
  //paRight -> the menu's right edge aligns to the button's right edge (screenPos), dropping below it.
  FPackageMenu.Alignment := paRight;

  menuItem := TMenuItem.Create(FPackageMenu);
  menuItem.Caption := 'Use Source';
  menuItem.Checked := FMenuUseSource;
  //row is installed; only offer the toggle when the package actually ships source.
  menuItem.Enabled := hasSource;
  menuItem.OnClick := UseSourceMenuItemClick;
  FPackageMenu.Items.Add(menuItem);

  menuItem := TMenuItem.Create(FPackageMenu);
  menuItem.Caption := 'Show Cache Location';
  //grey out when the package folder isn't actually present in the cache.
  menuItem.Enabled := DirectoryExists(FMenuCachePath);
  menuItem.OnClick := ShowCacheLocationMenuItemClick;
  FPackageMenu.Items.Add(menuItem);

  FPackageMenu.Popup(screenPos.X, screenPos.Y);
end;

procedure TPackageDetailsFrame.UseSourceMenuItemClick(Sender : TObject);
begin
  //toggle the current-session choice, reusing the full search-path refresh flow.
  VersionGridOnUseSourceChanged(FMenuProject, not FMenuUseSource);
end;

procedure TPackageDetailsFrame.ShowCacheLocationMenuItemClick(Sender : TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(FMenuCachePath), nil, nil, SW_SHOWNORMAL);
end;

procedure TPackageDetailsFrame.DoUpdateVersions(const sReferenceVersion : string; const versions : IList<TPackageVersion>);
  var
    version : TPackageVersion;
begin
  //the package may have been deselected (FPackageMetaData set to nil in SetPackage)
  //while an async version request was in flight - nothing to update in that case.
  if FPackageMetaData = nil then
    exit;

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
  //locals for capture - the worker runs on another thread and must not read the frame.
  packageId : string;
  lifetime : IViewLifetime;
  generation : integer;
begin
  FVersionsDelayTimer.Enabled := false;
  //a queued/already-fired tick can run after the package was deselected.
  if FPackageMetaData = nil then
    exit;
  //supersedes any versions request still in flight (a new package's cancelled it already).
  Inc(FVersionsGeneration);
  generation := FVersionsGeneration;
  lifetime := FLifetime;
  packageId := FPackageMetaData.Id;

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
        result := repoManager.GetPackageVersions(cancelToken, IDECompilerVersion, packageId, includePreRelease);
      finally
        CoUninitialize;
      end;
    end, FRequestTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      if (not lifetime.Alive) or (generation <> FVersionsGeneration) then
        exit;
      LogException(FLogger, e);
    end)
  .OnCancellation(
    procedure
    begin
      if (not lifetime.Alive) or (generation <> FVersionsGeneration) then
        exit;
      FLogger.Debug('Cancelled getting package versions.');
    end)
  .Await(
    procedure(const versions : IList<TPackageVersion>)
    begin
      //the view gone, or this request superseded (another package shown) - its versions are not this one's, and
      //caching them under the current package's id would show them for it from then on.
      if (not lifetime.Alive) or (generation <> FVersionsGeneration) then
        exit;
      FLogger.Debug('Got package versions .');
      if (versions = nil) or (FPackageMetaData = nil) then
        exit;
      UpdateVersionsCache(packageId, includePreRelease, versions);
      DoUpdateVersions(sReferenceVersion, versions);
    end);

end;

procedure TPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  //the callbacks still to come find the view dead and do nothing.
  FLifetime.Kill;
  FHost := nil;
  FRequestTokenSource.Cancel;
  //an install running now stops too, as it always has.
  if FOperationTokenSource <> nil then
    FOperationTokenSource.Cancel;
end;

end.
