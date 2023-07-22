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
  DPM.Controls.VersionGrid, Vcl.Buttons;

{$I ..\DPMIDE.inc}


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
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure cboVersionsMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cboVersionsChange(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure cboVersionsCloseUp(Sender: TObject);
    procedure cboVersionsDropDown(Sender: TObject);
  private
    FContainer : TContainer;
    FIconCache : TDPMIconCache;
    FHost : IDetailsHost;

    FPackageMetaData : IPackageSearchResultItem;
    FProjectsGrid : TVersionGrid;
    FDetailsPanel : TPackageDetailsPanel;

    FCancellationTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FVersionsDelayTimer : TTimer;
    FConfiguration : IConfiguration;
    FLogger : IDPMIDELogger;
    FClosing : boolean;
    FIncludePreRelease : boolean;
    FPackageInstalledVersion : string;
    FProjectGroup : IOTAProjectGroup;
    FCurrentPlatform : TDPMPlatform;
    FDropdownOpen : boolean;

    FIDEStyleServices : TCustomStyleServices;
  protected
    procedure ProjectSelectionChanged(Sender : TObject);
    procedure UpdateButtonState;

    procedure VersionsDelayTimerEvent(Sender : TObject);
    procedure OnDetailsUriClick(Sender : TObject; const uri : string; const element : TDetailElement);

    function GetPackageMetaDataAsync(const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const version: string): IAwaitable<IPackageSearchResultItem>;
  public
    constructor Create(AOwner : TComponent); override;
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
  DPM.IDE.Constants,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Utils.Strings,
  DPM.Core.Dependency.Interfaces;

{ TGroupPackageDetailsFrame }

procedure TGroupPackageDetailsFrame.btnInstallClick(Sender: TObject);
begin
//
end;

procedure TGroupPackageDetailsFrame.btnUninstallClick(Sender: TObject);
begin
//
end;

procedure TGroupPackageDetailsFrame.cboVersionsChange(Sender: TObject);
var
  sVersion : string;
begin
  sVersion := cboVersions.Items[cboVersions.ItemIndex];
  if TStringUtils.StartsWith(sVersion, cLatestPrerelease, true) then
    Delete(sVersion, 1, Length(cLatestPrerelease))
  else if TStringUtils.StartsWith(sVersion, cLatestStable, true) then
    Delete(sVersion, 1, Length(cLatestStable));

  GetPackageMetaDataAsync(FPackageMetaData.Id, FPackageMetaData.CompilerVersion, FPackageMetaData.Platform, sVersion)
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
      //        FLogger.Debug('Got search results.');
      SetPackage(theResult, FIncludePreRelease, false);
    end);

end;

procedure TGroupPackageDetailsFrame.cboVersionsCloseUp(Sender: TObject);
begin
  FDropdownOpen := false;
end;

procedure TGroupPackageDetailsFrame.cboVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  cboVersions.Canvas.FillRect(Rect);

  if (cboVersions.Items.Count = 0) or (index < 0) then
    exit;

  cboVersions.Canvas.Font.Style := [];


  if odComboBoxEdit in State then
    cboVersions.Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, cboVersions.Items[Index]) //Visual state of the text in the edit control
  else
  begin
    if FDropdownOpen and (Index > 0) then
    begin
      if SameText(FPackageInstalledVersion, cboVersions.Items[index]) then
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

//  ThemeChanged;


end;


procedure TGroupPackageDetailsFrame.Init(const container: TContainer; const iconCache: TDPMIconCache; const config: IConfiguration; const host: IDetailsHost; const projectOrGroup : IOTAProjectGroup);
var
  i: Integer;
  sProject : string;
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
  begin
    sProject := ChangeFileExt(ExtractFileName(FProjectGroup.Projects[i].FileName), '');
    FProjectsGrid.AddProject(sProject, '' );
  end;
  FProjectsGrid.EndUpdate;
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


procedure TGroupPackageDetailsFrame.SetPackage(const package: IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
var
  logo : IPackageIconImage;
  graphic : TGraphic;
  packageRefs : IPackageReference;
  projectRefs : IPackageReference;
  projectRef : IPackageReference;
  i: Integer;
begin
  FIncludePreRelease := preRelease;
  FVersionsDelayTimer.Enabled := false;
  if FRequestInFlight then
    FCancellationTokenSource.Cancel;
  FPackageMetaData := package;

  if package <> nil then
  begin
//    if (package.Id <> FPackageId) then
//    begin
//      FPackageId := package.Id;
//      if package.Installed then
//        FPackageInstalledVersion := package.Version.ToStringNoMeta
//      else
//        FPackageInstalledVersion := '';
//    end
//    else
//    begin
//      //same id, so we might just be displaying a new version.
//      //since we're just getting the item from the feed rather than the project
//      //it won't have installed or installed version set.
//      if package.Installed then
//        FPackageInstalledVersion := package.Version.ToStringNoMeta
//      else if package.Version.ToStringNoMeta = FPackageInstalledVersion then
//          package.Installed := true;
//    end;

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

    UpdateButtonState;
    //btnInstallOrUpdate.Enabled := ((not FPackageMetaData.Installed) or (FPackageMetaData.LatestVersion <> FPackageMetaData.Version)) and FProjectsGrid.HasCheckedProjects;

    sbPackageDetails.Visible := true;

    packageRefs := FHost.GetPackageReferences;

    FProjectsGrid.BeginUpdate;
    FProjectsGrid.PackageVersion := package.Version;
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
  end
  else
  begin
    sbPackageDetails.Visible := false;
    cboVersions.Clear;
    //    pnlPackageId.Visible := false;
    //    pnlInstalled.Visible := false;
    //    pnlVErsion.Visible := false;
  end;

  FDetailsPanel.SetDetails(package);
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
begin
  //btnInstall.Enabled := (FPackageMetaData <> nil) and ((not FPackageMetaData.Installed) or (FPackageMetaData.LatestVersion <> FPackageMetaData.Version)) and FProjectsGrid.HasCheckedProjects ;
end;

procedure TGroupPackageDetailsFrame.VersionsDelayTimerEvent(Sender: TObject);
var
  versions : IList<TPackageVersion>;
  repoManager : IPackageRepositoryManager;
  sInstalledVersion : string;
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

  sInstalledVersion := FPackageInstalledVersion;

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
          cboVersions.ItemIndex := cboVersions.Items.IndexOf(sInstalledVersion)
        end
        else
        begin
          //no versions returned
          //this can happen if there is only 1 version and its prerelease and
          //prerlease not checked.
          //in this case we will just add the installed version
          if sInstalledVersion <> '' then
          begin
            cboVersions.Items.Add(sInstalledVersion);
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

