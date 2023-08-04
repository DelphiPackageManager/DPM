{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Themes,
  Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
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
  Vcl.ImageCollection,
  {$ENDIF}
  DPM.Controls.VersionGrid, Vcl.Buttons;


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
    FImageCollection : TImageCollection;
    {$ELSE}
    FImageList : TImageList;
    FUpgradeBmp : TBitmap;
    FDowngradeBmp : TBitmap;
    {$ENDIF}

    FConfiguration : IConfiguration;
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
    FCurrentPlatform : TDPMPlatform;
    FDropdownOpen : boolean;

    FFetchVersions : boolean;

  protected
    procedure LoadImages;
    procedure AssignImages;

    procedure ProjectSelectionChanged(Sender : TObject);
    procedure UpdateButtonState;
    procedure SetPackageLogo(const id : string);

    function GetReferenceVersion : TPackageVersion;

    procedure VersionsDelayTimerEvent(Sender : TObject);
    procedure OnDetailsUriClick(Sender : TObject; const uri : string; const element : TDetailElement);

    procedure DoGetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string);

    procedure UpdateProjectPackageVersions(const packageId : string);
    procedure DoPackageUninstall(options : TUnInstallOptions);
    procedure DoPackageInstall(options : TInstallOptions; const isUpdate : boolean);

    function GetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string): IAwaitable<IPackageSearchResultItem>;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

    procedure VersionGridOnInstallEvent(const project : string);
    procedure VersionGridOnUnInstallEvent(const project : string);
    procedure VersionGridOnUpgradeEvent(const project : string);
    procedure VersionGridOnDowngradeEvent(const project : string);


  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const host : IDetailsHost; const projectGroup : IOTAProjectGroup);
    procedure SetPackage(const package : IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
    procedure SetPlatform(const platform : TDPMPlatform);
    procedure ViewClosing;
    procedure ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
    procedure ProjectReloaded;

  end;

implementation

{$R *.dfm}

uses
  WinApi.ShellApi,
  WinApi.ActiveX,
  WinApi.CommCtrl,
  DPM.IDE.Constants,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.System,
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
  options.Platforms := [FPackageMetaData.Platform];
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
  options.Platforms := [FPackageMetaData.Platform];
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
  options.Platforms := [FPackageMetaData.Platform];
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
    DoGetPackageMetaDataAsync(package.Id, package.CompilerVersion, package.Platform, FSelectedVersion.ToStringNoMeta)
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

procedure TPackageDetailsFrame.DoGetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string);
begin
  GetPackageMetaDataAsync(id, compilerVersion, platform, version)
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
      FDetailsPanel.SetDetails(theResult);
      FSelectedVersion := theResult.Version;
      UpdateButtonState;
    end);

end;


procedure TPackageDetailsFrame.DoPackageInstall(options: TInstallOptions; const isUpdate : boolean);
var
  installResult : boolean;
  sPlatform : string;
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
      sPlatform := DPMPlatformToString(FPackageMetaData.Platform);
      FLogger.Information('Package ' + FPackageMetaData.Id + ' - ' + FInstalledVersion.ToStringNoMeta + ' [' + sPlatform + '] installed.');
    end
    else
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FInstalledVersion.ToStringNoMeta + ' [' + sPlatform + '] did not install.');


  finally
    FLogger.EndInstall(installResult);
    FHost.EndInstall;
  end;

end;

procedure TPackageDetailsFrame.DoPackageUninstall(options: TUnInstallOptions);
var
  uninstallResult : boolean;
  sPlatform : string;
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
    FHost.BeginInstall(projectCount);

    sPlatform := DPMPlatformToString(FPackageMetaData.Platform);
    FLogger.Information('UnInstalling package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version.ToStringNoMeta + ' [' + sPlatform + ']');
    uninstallResult := FPackageInstaller.UnInstall(FCancellationTokenSource.Token, options, FInstallerContext);
    if uninstallResult then
    begin
      packageMetaData := FPackageMetaData;
      FLogger.Information('Package ' + packageMetaData.Id + ' - ' + packageMetaData.Version.ToStringNoMeta + ' [' + sPlatform + '] uninstalled.');
      FHost.PackageInstalled;
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
      FLogger.Error('Package ' + FPackageMetaData.Id + ' - ' + FPackageMetaData.Version.ToStringNoMeta + ' [' + sPlatform + '] did not uninstall.');

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
  DoGetPackageMetaDataAsync(FPackageMetaData.Id, FPackageMetaData.CompilerVersion, FPackageMetaData.Platform, sVersion);
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

constructor TPackageDetailsFrame.Create(AOwner: TComponent);
begin
  inherited;
  //not published in older versions, so gets removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont];
  {$ENDIF}

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

  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  FVersionsDelayTimer := TTimer.Create(AOwner);
  FVersionsDelayTimer.Interval := 100;
  FVersionsDelayTimer.Enabled := false;
  FVersionsDelayTimer.OnTimer := VersionsDelayTimerEvent;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;


  {$IFDEF USEIMAGECOLLECTION } //10.4 or later
  FImageList := TVirtualImageList.Create(Self);
  FImageCollection := TImageCollection.Create(Self);
  {$ELSE}
  FUpgradeBmp := TBitmap.Create;
  FDowngradeBmp := TBitmap.Create;
  FImageList := TImageList.Create(Self);
  {$ENDIF}
  FImageList.ColorDepth := cd32Bit;
  FImageList.DrawingStyle := dsTransparent; 
  FImageList.Width := 16;
  FImageList.Height := 16;

  
  LoadImages;
  AssignImages;
  FProjectsGrid.ImageList := FImageList;

  btnInstallAll.Caption := '';
  btnUpgradeAll.Caption := '';
  btnUninstallAll.Caption := '';

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

procedure TPackageDetailsFrame.LoadImages;
const
  suffixes : array[0..3] of string = ('_16', '_24', '_32','_48');

{$IF CompilerVersion = 33.0 } //10.3
//  procedure AddImageItem(AName: String; AInstance: THandle; const AResourceName: String; ASuffixes: array of string);
//  var
//    item : TImageCollectionItem;
//    sourceItem : TImageCollectionSourceItem;
//    suffix : string;
//    bmp: TBitmap;
//    png : TPngImage;
//    resStream : TResourceStream;
//  begin
//    item := FImageCollection.Images.Add;
//    item.Name := AName;
//
//    for suffix in ASuffixes do
//    begin
//      resStream := TResourceStream.Create(AInstance, AResourceName + suffix, RT_RCDATA);
//      try
//        sourceItem :=item.SourceImages.Add;
//        sourceItem.Image.Transparent := true;
//        sourceItem.Image.LoadFromStream(resStream);
//      finally
//        resStream.Free;
//      end;
//    end;
//    item.CheckSources;
//  end;
{$IFEND}

{$IF CompilerVersion < 34.0} //10.2 or earlier

  procedure AddImage(const AResourceName : string);
  var
    png : TPngImage;
    bmp: TBitmap;
  begin
    png := TPngImage.Create;
    bmp:=TBitmap.Create;
    try
      png.LoadFromResourceName(HInstance, AResourceName);
      png.AssignTo(bmp);
      bmp.AlphaFormat:=afIgnored;
      ImageList_Add(FImageList.Handle, bmp.Handle, 0);
    finally
      bmp.Free;
      png.Free;
    end;
  end;

{$IFEND}


begin
  {$IF CompilerVersion < 34.0} //10.2 or earlier
    AddImage('ADD_PACKAGE_16');
    AddImage('REMOVE_PACKAGE_16');
    AddImage('UPGRADE_PACKAGE_16');
    AddImage('DOWNGRADE_PACKAGE_16');

//  {$ELSEIF CompilerVersion = 33.0 } //10.3
//    //10.3 TImageCollection was pretty basic and hard to use.
//    AddImageItem('add',HInstance,'ADD_PACKAGE', suffixes);
//    AddImageItem('remove',HInstance,'REMOVE_PACKAGE', suffixes);
//    AddImageItem('upgrade',HInstance,'UPGRADE_PACKAGE', suffixes);
//    AddImageItem('downgrade',HInstance,'DOWNGRADE_PACKAGE', suffixes);
//    FImageList.AutoFill := true;
//    FImageList.ImageCollection := FImageCollection;
//
//  {$ELSEIF CompilerVersion > 33.0}
  {$ELSE}
    FImageCollection.Add('add',HInstance,'ADD_PACKAGE', suffixes);
    FImageCollection.Add('remove',HInstance,'REMOVE_PACKAGE', suffixes);
    FImageCollection.Add('upgrade',HInstance,'UPGRADE_PACKAGE', suffixes);
    FImageCollection.Add('downgrade',HInstance,'DOWNGRADE_PACKAGE', suffixes);
    FImageList.AutoFill := true;
    FImageList.ImageCollection := FImageCollection;
  {$IFEND}
end;

procedure TPackageDetailsFrame.OnDetailsUriClick(Sender: TObject; const uri: string; const element: TDetailElement);
begin
  case element of
    deNone : ;
    deLicense,
    deProjectUrl,
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
        projectRefs := packageRefs.FindDependency(LowerCase(FProjectGroup.Projects[i].FileName));
        if projectRefs <> nil then
          projectRef := projectRefs.FindDependency(LowerCase(packageId));
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

function TPackageDetailsFrame.GetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string): IAwaitable<IPackageSearchResultItem>;
var
  repoManager : IPackageRepositoryManager;
begin
  //local for capture
  repoManager := FRespositoryManager;
  repoManager.Initialize(FConfiguration);

  result := TAsync.Configure <IPackageSearchResultItem> (
    function(const cancelToken : ICancellationToken) : IPackageSearchResultItem
    begin
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      try
        result := repoManager.GetPackageMetaData(cancelToken, id, version, compilerVersion, platform);
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

procedure TPackageDetailsFrame.SetPlatform(const platform: TDPMPlatform);
begin
  FCurrentPlatform := platform;
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

end;

procedure TPackageDetailsFrame.UpdateButtonState;
var
  referenceVersion : TPackageVersion;
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
  btnInstallAll.Enabled := FPackageMetaData <> nil;
  btnUpgradeAll.Enabled := (FPackageMetaData <> nil) and (FInstalledVersion <> TPackageVersion.Empty) and  (FSelectedVersion <> FInstalledVersion);
  btnUninstallAll.Enabled := (FPackageMetaData <> nil) and (FInstalledVersion <> TPackageVersion.Empty);

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

procedure TPackageDetailsFrame.VersionGridOnDowngradeEvent(const project: string);
var
  options : TInstallOptions;
begin
  options := TInstallOptions.Create;
  options.ConfigFile := FConfiguration.FileName;
  options.PackageId := FPackageMetaData.Id;
  options.Version := FSelectedVersion;
  options.ProjectPath := project;
  options.Platforms := [FPackageMetaData.Platform];
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
  options.Platforms := [FPackageMetaData.Platform];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  DoPackageInstall(options, false);
  //call common function

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
  options.Platforms := [FPackageMetaData.Platform];
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
  options.Platforms := [FPackageMetaData.Platform];
  options.Prerelease := FIncludePreRelease;
  options.CompilerVersion := IDECompilerVersion;
  options.IsUpgrade := true; //changing the installed version
  DoPackageInstall(options, true);
  //call common function
end;

procedure TPackageDetailsFrame.VersionsDelayTimerEvent(Sender: TObject);
var
  repoManager : IPackageRepositoryManager;
  sReferenceVersion : string;
  includePreRelease : boolean;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  while FRequestInFlight do
    Application.ProcessMessages;
  FCancellationTokenSource.Reset;

  cboVersions.Clear;
  repoManager := FRespositoryManager; //local for capture
  repoManager.Initialize(FConfiguration);

  sReferenceVersion := GetReferenceVersion.ToStringNoMeta;

  includePreRelease := FIncludePreRelease or (not FPackageMetaData.Version.IsStable);

  TAsync.Configure<IList<TPackageVersion>> (
    function(const cancelToken : ICancellationToken) : IList<TPackageVersion>
    begin
      //need this for the http api/
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      try
        result := repoManager.GetPackageVersions(cancelToken, IDECompilerVersion, FCurrentPlatform, FPackageMetaData.Id, includePreRelease);
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
    var
      version : TPackageVersion;
    begin
      FRequestInFlight := false;
      //if the view is closing do not do anything else.
      if FClosing then
        exit;
      FLogger.Debug('Got package versions .');
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

    end);

end;

procedure TPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  FHost := nil;
  FCancellationTokenSource.Cancel;
end;

end.
