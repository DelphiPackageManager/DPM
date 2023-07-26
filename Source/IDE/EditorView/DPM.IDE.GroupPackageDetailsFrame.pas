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

unit DPM.IDE.GroupPackageDetailsFrame;

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
  DPM.Core.Logging,
  DPM.IDE.Logger,
  DPM.Core.Options.Search,
  DPM.IDE.Details.Interfaces,
  DPM.IDE.PackageDetailsPanel,
  DPM.IDE.IconCache,
  DPM.IDE.Types,
  Vcl.ImgList,
  {$IF CompilerVersion >= 33.0 }
  Vcl.VirtualImageList,
  Vcl.ImageCollection,
  {$IFEND}

  DPM.Controls.VersionGrid, Vcl.Buttons;



type
  TGroupPackageDetailsFrame = class(TFrame)
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
    procedure cboVersionsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cboVersionsChange(Sender: TObject);
    procedure cboVersionsCloseUp(Sender: TObject);
    procedure cboVersionsDropDown(Sender: TObject);
    procedure btnInstallAllClick(Sender: TObject);
    procedure btnUpgradeAllClick(Sender: TObject);
    procedure btnUninstallAllClick(Sender: TObject);
  private
    //controls
    FProjectsGrid : TVersionGrid;
    FDetailsPanel : TPackageDetailsPanel;
    //controls

    {$IF CompilerVersion > 33.0 }
    FImageList : TVirtualImageList;
    FImageCollection : TImageCollection;
    {$ELSE}
    FImageList : TImageList;
    FUpgradeBmp : TBitmap;
    FDowngradeBmp : TBitmap;
    {$IFEND}


    FConfiguration : IConfiguration;
    FContainer : TContainer;
    FIconCache : TDPMIconCache;
    FHost : IDetailsHost;
    FLogger : IDPMIDELogger;
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

    function GetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string): IAwaitable<IPackageSearchResultItem>;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

    procedure VersionGridOnInstallEvent(const project : string);
    procedure VersionGridOnUnInstallEvent(const project : string);
    procedure VersionGridOnUpgradeEvent(const project : string);
    procedure VersionGridOnDowngradeEvent(const project : string);


  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const host : IDetailsHost; const projectOrGroup : IOTAProjectGroup);
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
  DPM.Core.Repository.Interfaces,
  DPM.Core.Utils.Strings,
  DPM.Core.Dependency.Interfaces;

{ TGroupPackageDetailsFrame }

procedure TGroupPackageDetailsFrame.btnInstallAllClick(Sender: TObject);
begin
  ShowMessage('Install');
end;

procedure TGroupPackageDetailsFrame.btnUninstallAllClick(Sender: TObject);
begin
  ShowMessage('Remove');
end;

procedure TGroupPackageDetailsFrame.btnUpgradeAllClick(Sender: TObject);
begin
  ShowMessage('Upgrade');
end;

destructor TGroupPackageDetailsFrame.Destroy;
begin
  {$IF CompilerVersion < 34.0}    
  FUpgradeBmp.Free;
  FDowngradeBmp.Free;
  {$IFEND}

  inherited;
end;

procedure TGroupPackageDetailsFrame.DoGetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string);
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


procedure TGroupPackageDetailsFrame.cboVersionsChange(Sender: TObject);
var
  sVersion : string;
  packageVer : TPackageVersion;
begin
  sVersion := cboVersions.Items[cboVersions.ItemIndex];
  packageVer := TPackageVersion.Parse(sVersion);
  FProjectsGrid.PackageVersion := packageVer;
  DoGetPackageMetaDataAsync(FPackageMetaData.Id, FPackageMetaData.CompilerVersion, FPackageMetaData.Platform, sVersion);
end;

procedure TGroupPackageDetailsFrame.cboVersionsCloseUp(Sender: TObject);
begin
  FDropdownOpen := false;
end;

procedure TGroupPackageDetailsFrame.cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
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
    if FDropdownOpen and (Index > 0) then
    begin
      if SameText(sInstalledVersion, cboVersions.Items[index]) then
        cboVersions.Canvas.Font.Style := [TFontStyle.fsBold];
    end;
    cboVersions.Canvas.TextOut(Rect.Left + 2, Rect.Top, cboVersions.Items[Index]); //Visual state of the text(items) in the deployed list
  end;

  if odComboBoxEdit in State then
    exit;

  //draw underline.
  if Integer(cboVersions.Items.Objects[index]) <> 0 then
  begin
    cboVersions.Canvas.Pen.Color := clGray;
    cboVersions.Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    cboVersions.Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
  end;

end;

procedure TGroupPackageDetailsFrame.cboVersionsDropDown(Sender: TObject);
begin
  FDropdownOpen := true;
end;

procedure TGroupPackageDetailsFrame.cboVersionsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  if (cboVersions.Items.Count = 0) or (index < 0) then
    exit;
  if cboVersions.Items.Objects[index] <> nil then
    Inc(Height, 4);
end;

procedure TGroupPackageDetailsFrame.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
begin
  inherited;


end;

constructor TGroupPackageDetailsFrame.Create(AOwner: TComponent);
begin
  inherited;
  //not published in older versions, so get removed when we edit in older versions.
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
  FVersionsDelayTimer.Interval := 200;
  FVersionsDelayTimer.Enabled := false;
  FVersionsDelayTimer.OnTimer := VersionsDelayTimerEvent;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;


  {$IF CompilerVersion > 33.0 } //10.3 or later
  FImageList := TVirtualImageList.Create(Self);
  FImageCollection := TImageCollection.Create(Self);
  {$ELSE}
  FUpgradeBmp := TBitmap.Create;
  FDowngradeBmp := TBitmap.Create;
  FImageList := TImageList.Create(Self);
  {$IFEND}
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


procedure TGroupPackageDetailsFrame.Init(const container: TContainer; const iconCache: TDPMIconCache; const config: IConfiguration; const host: IDetailsHost; const projectOrGroup : IOTAProjectGroup);
var
  i: Integer;
begin
  FContainer := container;
  FIconCache := iconCache;
  FConfiguration := config;
  FLogger := FContainer.Resolve<IDPMIDELogger>;
  FHost := host;
  SetPackage(nil, FIncludePreRelease);

  FProjectGroup := projectOrGroup as IOTAProjectGroup;
  ProjectReloaded; //loadGrid;

  FProjectsGrid.BeginUpdate;
  FProjectsGrid.Clear;
  for i := 0 to FProjectGroup.ProjectCount -1 do
    FProjectsGrid.AddProject(FProjectGroup.Projects[i].FileName, '' );
  FProjectsGrid.EndUpdate;
end;

procedure TGroupPackageDetailsFrame.AssignImages;
{$IF CompilerVersion < 34.0}
var
  bmp : TBitmap;
{$IFEND}
begin
  {$IF CompilerVersion < 34.0}

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

  {$IFEND}

end;

procedure TGroupPackageDetailsFrame.LoadImages;
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

procedure TGroupPackageDetailsFrame.OnDetailsUriClick(Sender: TObject; const uri: string; const element: TDetailElement);
begin
  case element of
    deNone : ;
    deLicense,
    deProjectUrl,
    deReportUrl : ShellExecute(Application.Handle, 'open', PChar(uri), nil, nil, SW_SHOWNORMAL);
    deTags : ;
  end;

end;

procedure TGroupPackageDetailsFrame.ProjectReloaded;
var
  i : integer;
  sProject : string;
begin
  FProjectsGrid.BeginUpdate;
  FProjectsGrid.Clear;
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    sProject := ChangeFileExt(ExtractFileName(FProjectGroup.Projects[i].FileName), '');
    FProjectsGrid.AddProject(sProject, '' );
  end;
  FProjectsGrid.EndUpdate;
end;

procedure TGroupPackageDetailsFrame.ProjectSelectionChanged(Sender: TObject);
begin
  UpdateButtonState;
end;


function TGroupPackageDetailsFrame.GetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string): IAwaitable<IPackageSearchResultItem>;
var
  repoManager : IPackageRepositoryManager;
begin
  //local for capture
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
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


function TGroupPackageDetailsFrame.GetReferenceVersion: TPackageVersion;
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

procedure TGroupPackageDetailsFrame.SetPackage(const package: IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
var
  packageRefs : IPackageReference;
  projectRefs : IPackageReference;
  projectRef : IPackageReference;
  i: Integer;
begin
  FIncludePreRelease := preRelease;
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  //todo - should we wait here?

  FPackageMetaData := package;

  if package <> nil then
  begin
    if package.Installed then
      FInstalledVersion := package.Version
    else
      FInstalledVersion := TPackageVersion.Empty;

    lblPackageId.Caption := package.Id;
    SetPackageLogo(package.Id);

    sbPackageDetails.Visible := true;

    packageRefs := FHost.GetPackageReferences;

    FSelectedVersion := GetReferenceVersion;

    FProjectsGrid.BeginUpdate;
    FProjectsGrid.PackageVersion := FSelectedVersion;
    try
      for i := 0 to FProjectsGrid.RowCount -1 do
      begin
        if packageRefs <> nil then
        begin
          projectRef := nil;
          projectRefs := packageRefs.FindDependency(LowerCase(FProjectsGrid.ProjectName[i]));
          if projectRefs <> nil then
            projectRef := projectRefs.FindDependency(package.Id);
        end;

        if projectRef <> nil then
          FProjectsGrid.ProjectVersion[i] := projectRef.Version
        else
          FProjectsGrid.ProjectVersion[i] := TPackageVersion.Empty;
      end;
    finally
      FProjectsGrid.EndUpdate;
    end;

    if fetchVersions then
      FVersionsDelayTimer.Enabled := true;
    if FSelectedVersion <> FPackageMetaData.Version then
      DoGetPackageMetaDataAsync(FPackageMetaData.Id, FPackageMetaData.CompilerVersion, FPackageMetaData.Platform, FSelectedVersion.ToStringNoMeta)
    else
      FDetailsPanel.SetDetails(FPackageMetaData);
  end
  else
  begin
    FInstalledVersion := TPackageVersion.Empty;
    sbPackageDetails.Visible := false;
    cboVersions.Clear;
  end;
  UpdateButtonState;
end;

procedure TGroupPackageDetailsFrame.SetPackageLogo(const id: string);
var
  logo : IPackageIconImage;
  graphic : TGraphic;
begin
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

procedure TGroupPackageDetailsFrame.SetPlatform(const platform: TDPMPlatform);
begin
  FCurrentPlatform := platform;
end;

procedure TGroupPackageDetailsFrame.ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
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

procedure TGroupPackageDetailsFrame.UpdateButtonState;
var
  referenceVersion : TPackageVersion;
begin
  referenceVersion := GetReferenceVersion;
  btnInstallAll.Enabled := FPackageMetaData <> nil;
  btnUpgradeAll.Enabled := (FInstalledVersion <> TPackageVersion.Empty) and  (FSelectedVersion <> FInstalledVersion);
  btnUninstallAll.Enabled := FInstalledVersion <> TPackageVersion.Empty;

  {$IF CompilerVersion < 34.0}
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
  {$IFEND}

end;

procedure TGroupPackageDetailsFrame.VersionGridOnDowngradeEvent(const project: string);
begin
  ShowMessage('Downgrade : ' + project);
end;

procedure TGroupPackageDetailsFrame.VersionGridOnInstallEvent(const project: string);
begin
  ShowMessage('Install : ' + project);
end;

procedure TGroupPackageDetailsFrame.VersionGridOnUnInstallEvent(const project: string);
begin
  ShowMessage('uninstall : ' + project);
end;

procedure TGroupPackageDetailsFrame.VersionGridOnUpgradeEvent(const project: string);
begin
  ShowMessage('upgrade : ' + project);
end;

procedure TGroupPackageDetailsFrame.VersionsDelayTimerEvent(Sender: TObject);
var
  versions : IList<TPackageVersion>;
  repoManager : IPackageRepositoryManager;
  sReferenceVersion : string;
begin
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  while FRequestInFlight do
    Application.ProcessMessages;
  FCancellationTokenSource.Reset;

  cboVersions.Clear;
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  repoManager.Initialize(FConfiguration);

  sReferenceVersion := GetReferenceVersion.ToStringNoMeta;

  TAsync.Configure<IList<TPackageVersion>> (
    function(const cancelToken : ICancellationToken) : IList<TPackageVersion>
    begin
      //need this for the http api/
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      try
      result := repoManager.GetPackageVersions(cancelToken, IDECompilerVersion, FCurrentPlatform, FPackageMetaData.Id, FIncludePrerelease);
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
          for version in versions do
            cboVersions.Items.Add(version.ToStringNoMeta);
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

procedure TGroupPackageDetailsFrame.ViewClosing;
begin
  FClosing := true;
  FHost := nil;
  FCancellationTokenSource.Cancel;
end;

end.

