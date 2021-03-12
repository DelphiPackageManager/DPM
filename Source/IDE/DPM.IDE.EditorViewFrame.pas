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

//TODO : way too much code in this unit.. get it working then refactor!

unit DPM.IDE.EditorViewFrame;

interface

{$I DPMIDE.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, SVGInterfaces,
  Vcl.Themes,
  DPM.Controls.ButtonedEdit,
  DPM.Controls.GroupButton,
  VSoftVirtualListView,
  ToolsApi,
  Spring.Collections,
  Spring.Container,
  VSoft.Awaitable,
  DPM.IDE.Types,
  DPM.IDE.IconCache,
  DPM.IDE.Options,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.IDE.Logger,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.IDE.ProjectTreeManager,
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  {$IF CompilerVersion >= 30.0 }
  System.ImageList,
  {$IFEND}
  Vcl.ActnList, DPM.IDE.PackageDetailsFrame;

type
  TRowLayout = record
    RowWidth : integer;
    IconRect : TRect;
    TitleRect : TRect;
    VersionRect : TRect;
    InstalledVersionRect : TRect;
    DescriptionRect : TRect;
    procedure Update(const ACanvas : TCanvas; const rowRect : TRect; const showSelect : Boolean);
  end;

  TDPMEditViewFrame = class(TFrame, IPackageSearcher)
    lblProject : TLabel;
    DPMEditorViewImages : TImageList;
    pnlSearchPanel : TPanel;
    txtSearch : TButtonedEdit;
    btnRefresh : TButton;
    chkIncludePrerelease : TCheckBox;
    ContentPanel : TPanel;
    Splitter2 : TSplitter;
    chkIncludeCommercial : TCheckBox;
    btnSettings : TButton;
    btnAbout : TButton;
    lblSources : TLabel;
    cbSources : TComboBox;
    pnlButtonBar : TPanel;
    chkIncludeTrial : TCheckBox;
    searchDebounceTimer : TTimer;
    PackageListPanel : TPanel;
    PackageDetailsFrame : TPackageDetailsFrame;
    platformChangeDetectTimer : TTimer;
    procedure txtSearchKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure btnAboutClick(Sender : TObject);
    procedure btnSettingsClick(Sender : TObject);
    procedure tabMainChange(Sender : TObject);
    procedure searchDebounceTimerTimer(Sender : TObject);
    procedure btnRefreshClick(Sender : TObject);
    procedure chkIncludePrereleaseClick(Sender : TObject);
    procedure platformChangeDetectTimerTimer(Sender : TObject);
    procedure txtSearchChange(Sender : TObject);
    procedure txtSearchRightButtonClick(Sender : TObject);
  private
    //controls
    FInstalledButton : TDPMGroupButton;
    FUpdatesButton : TDPMGroupButton;
    FSearchButton : TDPMGroupButton;
    FConflictsButton : TDPMGroupButton;
    FScrollList : TVSoftVirtualListView;
    //controls

    //contains layout rects for the list view
    FRowLayout : TRowLayout;

    //dpm core stuff
    FContainer : TContainer;
    FLogger : IDPMIDELogger;
    FProjectTreeManager : IDPMProjectTreeManager;
    FConfigurationManager : IConfigurationManager;
    FConfiguration : IConfiguration;
    FConfigIsLocal : boolean;

    FDPMIDEOptions : IDPMIDEOptions;

    //RS IDE Stuff
    FProjectGroup : IOTAProjectGroup;
    FProject : IOTAProject;
    FCurrentPlatform : TDPMPlatform;

    FIDEStyleServices : TCustomStyleServices;

    //request stuff
    FCancelTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FClosing : boolean;

    FSearchHistFile : string;
    FCurrentTab : TCurrentTab;

    FIconCache : TDPMIconCache;

    //all installed packages, direct and transitive
    FAllInstalledPackages : IList<IPackageSearchResultItem>;
    //directly installed packages, might be fitered.
    FInstalledPackages : IList<IPackageSearchResultItem>;

    FSearchResultPackages : IList<IPackageSearchResultItem>;

    FConflictPackages : IList<IPackageSearchResultItem>;

    FGotConflicts : boolean;

    FPackageReferences : IGraphNode;

    FSearchOptions : TSearchOptions;
    FSearchSkip : integer;
    FSearchTake : integer;

    FUpdates : IList<IPackageSearchResultItem>;
    //true when we first load the view
    FFirstView : boolean;
    FHasSources : boolean;
    procedure FilterAndLoadInstalledPackages(const searchTxt : string);
  protected
    procedure SwitchTabs(const currentTab : TCurrentTab; const refresh : boolean);

    procedure txtSearchChanged(Sender : TObject);
    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);

    procedure RequestPackageIcon(const index : integer; const package : IPackageSearchResultItem);

    function IsProjectGroup : boolean;
    function CurrentList : IList<IPackageSearchResultItem>;

    procedure AddDefaultSearchHistory;

    procedure LoadList(const list : IList<IPackageSearchResultItem>);

    procedure SwitchedToInstalled(const refresh : boolean);
    procedure SwitchedToUpdates(const refresh : boolean);
    procedure SwitchedToSearch(const refresh : boolean);
    procedure SwitchedToConflicts(const refresh : boolean);

    function GetInstalledPackages : IAwaitable<IList<IPackageSearchResultItem>>;
    function GetUpdatedPackages : IAwaitable<IList<IPackageSearchResultItem>>;
    function GetConflictingPackages : IAwaitable<IList<IPackageSearchResultItem>>;

    function GetPackageIdsFromReferences(const platform : TDPMPlatform) : IList<IPackageId>;

    procedure ReloadSourcesCombo;

    //IPackageSearcher
    function GetSearchOptions : TSearchOptions;
    function SearchForPackages(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
    function GetCurrentPlatform : string;
    procedure PackageInstalled(const package : IPackageSearchResultItem);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);

    procedure SaveBeforeChange;

    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    //called from the EditorView
    procedure Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
    procedure ViewSelected;
    procedure ViewDeselected;
    procedure Closing;
    procedure ProjectReloaded;
    procedure ThemeChanged;
  end;

implementation

{$R *.dfm}

uses
  System.Types,
  Xml.XMLIntf,
  System.Diagnostics,
  DPM.Core.Constants,
  DPM.Core.Options.Common,
  DPM.Core.Utils.Config,
  DPM.Core.Utils.Numbers,
  DPM.Core.Utils.Strings,
  DPM.Core.Project.Editor,
  DPM.Core.Package.Icon,
  DPM.Core.Package.SearchResults,
  DPM.Core.Repository.Interfaces,
  DPM.IDE.AboutForm,
  DPM.IDE.AddInOptionsHostForm;

const
  cDMPSearchHistoryFile = 'packagesearch.txt';

  { TDPMEditViewFrame }

procedure TDPMEditViewFrame.AddDefaultSearchHistory;
begin
  //some commonly used open source libs to get people started.
  txtSearch.ACStrings.Add('Spring4D.Core');
  txtSearch.ACStrings.Add('Spring4D.Base');
  txtSearch.ACStrings.Add('VSoft');
  txtSearch.ACStrings.Add('VSoft.DUnitX');
  txtSearch.ACStrings.Add('VSoft.DelphiMocks');
  txtSearch.ACStrings.Add('Gabr42.OmniThreadLibrary');
end;

procedure TDPMEditViewFrame.btnAboutClick(Sender : TObject);
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

procedure TDPMEditViewFrame.btnRefreshClick(Sender : TObject);
begin
  if FSearchResultPackages <> nil then
    FSearchResultPackages.Clear;
  //also called by the include checkboxes.
  case FCurrentTab of
    TCurrentTab.Installed : SwitchedToInstalled(true);
    TCurrentTab.Updates : SwitchedToUpdates(true);
    TCurrentTab.Search : SwitchedToSearch(true);
    TCurrentTab.Conflicts : SwitchedToConflicts(true);
  end;
end;

procedure TDPMEditViewFrame.btnSettingsClick(Sender : TObject);
var
  bReload : boolean;
  optionsHost : TDPMOptionsHostForm;
begin
  optionsHost := TDPMOptionsHostForm.Create(Self, FConfigurationManager, FLogger, FDPMIDEOptions, FSearchOptions.ConfigFile);
  try
    bReload := optionsHost.ShowModal = mrOk;
  finally
    optionsHost.Free;
  end;

  if bReload then
  begin
    FConfiguration := FConfigurationManager.LoadConfig(FSearchOptions.ConfigFile);
    PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, FProject.FileName);
    //populate the sources combo.
    ReloadSourcesCombo;
  end;
end;

procedure TDPMEditViewFrame.chkIncludePrereleaseClick(Sender : TObject);
begin
  PackageDetailsFrame.IncludePreRelease := chkIncludePrerelease.Checked;
  btnRefreshClick(Sender);
end;

procedure TDPMEditViewFrame.Closing;
begin
  FClosing := true;
  PackageDetailsFrame.ViewClosing;
  //cancel any pending requests asap. Needs to return quickly or the IDE will hang.
  FCancelTokenSource.Cancel;
  //allow the cancellation to happen.
  //if we don't do this we will get an excepion in the await or cancellation callbacks
  while FRequestInFlight do
    Application.ProcessMessages;
end;

constructor TDPMEditViewFrame.Create(AOwner : TComponent);
{$IFDEF THEMESERVICES}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
begin
  inherited;
  FIconCache := TDPMIconCache.Create;
  //not published in older versions, so get removed when we edit in older versions.
  {$IFDEF STYLEELEMENTS}
  StyleElements := [seFont, seClient, seBorder];
  {$ENDIF}

  //todo - check when the IOTAIDEThemingServices was added
  {$IFDEF THEMESERVICES}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$ENDIF}

  CreateControls(AOwner);
  ThemeChanged;

  FFirstView := true;

  txtSearch.ACEnabled := true;
  txtSearch.ACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
  txtSearch.ACSource := acsList;

  FSearchHistFile := TConfigUtils.GetDefaultDMPFolder + '\' + cDMPSearchHistoryFile;
  if FileExists(FSearchHistFile) then
    txtSearch.ACStrings.LoadFromFile(FSearchHistFile)
  else
    //some common packages to help with the search
    AddDefaultSearchHistory;

  FSearchOptions := TSearchOptions.Create;
  //hard code our compiler version here since when we are running in the IDE we are only working with the IDE version
  FSearchOptions.CompilerVersion := IDECompilerVersion;
  //  FSearchOptions.Take := 5; //just for testing.

  FSearchSkip := 0;
  FSearchTake := 0;
  FRowLayout.RowWidth := -1;

  FCurrentTab := TCurrentTab.Installed;

  FProjectGroup := nil;
  FProject := nil;
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;

  FCancelTokenSource := TCancellationTokenSourceFactory.Create;
  FRequestInFlight := false;

end;

procedure TDPMEditViewFrame.CreateControls(AOwner : TComponent);
begin
  FInstalledButton := TDPMGroupButton.Create(Self);
  FUpdatesButton := TDPMGroupButton.Create(Self);
  FSearchButton := TDPMGroupButton.Create(Self);
  FConflictsButton := TDPMGroupButton.Create(Self);

  FSearchButton.Top := 10;
  FSearchButton.Left := 20;
  FSearchButton.Caption := 'Search';
  FSearchButton.Tag := 0;
  FSearchButton.Parent := pnlButtonBar;

  FInstalledButton.Top := 10;
  FInstalledButton.Left := FSearchButton.Left + FSearchButton.Width + 20;
  FInstalledButton.Caption := 'Installed';
  FInstalledButton.Tag := 1;
  FInstalledButton.Active := true;
  FInstalledButton.Parent := pnlButtonBar;

  FUpdatesButton.Top := 10;
  FUpdatesButton.Left := FInstalledButton.Left + FInstalledButton.Width + 20;
  FUpdatesButton.Caption := 'Updates';
  FUpdatesButton.Tag := 2;
  FUpdatesButton.Parent := pnlButtonBar;

  FConflictsButton.Top := 10;
  FConflictsButton.Left := FUpdatesButton.Left + FUpdatesButton.Width + 20;
  FConflictsButton.Caption := 'Conflicts';
  FConflictsButton.Tag := 3;
  FConflictsButton.Visible := false;
  FConflictsButton.Parent := pnlButtonBar;

  FInstalledButton.OnClick := tabMainChange;
  FUpdatesButton.OnClick := tabMainChange;
  FSearchButton.OnClick := tabMainChange;
  FConflictsButton.OnClick := tabMainChange;

  FScrollList := TVSoftVirtualListView.Create(Self);
  {$IFDEF STYLEELEMENTS}
  FScrollList.StyleElements := [seFont, seBorder];
  {$ENDIF}
  FScrollList.Align := alClient;
  FScrollList.BorderStyle := bsNone;
  FScrollList.BevelEdges := [];
  FScrollList.BevelOuter := bvNone;
  FScrollList.BevelInner := bvNone;
  FScrollList.BevelKind := bkNone;
  FScrollList.RowHeight := 75;
  FScrollList.RowCount := 0;
  FScrollList.OnPaintRow := Self.ScrollListPaintRow;
  FScrollList.OnPaintNoRows := Self.ScrollListPaintNoRows;
  FScrollList.OnRowChange := Self.ScrollListChangeRow;

  FScrollList.Constraints.MinWidth := 400;
  FScrollList.DoubleBuffered := false;
  FScrollList.ParentDoubleBuffered := false;
  FScrollList.ParentBackground := false;
  FScrollList.ParentColor := false;

  FScrollList.Parent := PackageListPanel;
end;

function TDPMEditViewFrame.CurrentList : IList<IPackageSearchResultItem>;
begin
  case FCurrentTab of
    TCurrentTab.Installed : result := FInstalledPackages;
    TCurrentTab.Updates : result := FUpdates;
    TCurrentTab.Search : result := FSearchResultPackages;
    TCurrentTab.Conflicts : result := FConflictPackages;
  end;
end;

destructor TDPMEditViewFrame.Destroy;
begin
  FSearchOptions.Free;
  FIconCache.Free;
  FLogger.Debug('DPMIDE : View Destroying');

  inherited;
end;

function TDPMEditViewFrame.GetConflictingPackages : IAwaitable<IList<IPackageSearchResultItem>>;
var
  lSearchTerm : string;
  lProjectFile : string;
begin
  //local for capture
  lSearchTerm := txtSearch.Text;
  if IsProjectGroup then
    lProjectFile := FProjectGroup.FileName
  else
    lProjectFile := FProject.FileName;

  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      filter : string;
    begin
      filter := lSearchTerm;
      result := TCollections.CreateList<IPackageSearchResultItem>;

      if not IsProjectGroup then
      begin

        exit;

      end;

      //simulating long running.
      WaitForSingleObject(cancelToken.Handle, 3000);
    end, FCancelTokenSource.Token);
end;

function FindPackageRef(const references : IGraphNode; const platform : TDPMPlatform; const searchItem : IPackageSearchResultItem) : IGraphNode;
var
  ref : IGraphNode;
begin
  result := nil;
  if (references = nil) or (not references.HasChildren) then
    exit;

  for ref in references.ChildNodes do
  begin
    if ref.Platform <> platform then
      continue;

    if SameText(ref.Id, searchItem.Id) then
      Exit(ref);
    if ref.HasChildren then
    begin
      result := FindPackageRef(ref, platform, searchItem);
      if result <> nil then
        Exit(result);
    end;
  end;

end;

function TDPMEditViewFrame.GetCurrentPlatform : string;
begin
  //todo : change this.
  result := DPMPlatformToString(FCurrentPlatform);
end;

function TDPMEditViewFrame.GetInstalledPackages : IAwaitable<IList<IPackageSearchResultItem>>;
var
  lProjectFile : string;
  repoManager : IPackageRepositoryManager;
  options : TSearchOptions;
begin
  //local for capture
  if IsProjectGroup then
    lProjectFile := FProjectGroup.FileName
  else
    lProjectFile := FProject.FileName;

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;

  options := FSearchOptions.Clone;
  //we want all packages for installed as we don't know what types we might have
  options.Prerelease := true;
  options.Commercial := true;
  options.Trial := true;
  options.SearchTerms := txtSearch.Text;

  if not IsProjectGroup then
    options.Platforms := [FCurrentPlatform];

  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      packageRef : IGraphNode;
      item : IPackageSearchResultItem;
    begin
      result := TCollections.CreateList<IPackageSearchResultItem>;

      if IsProjectGroup then
      begin
        //TODO : Figure out how to work with project groups!
        exit;

      end
      else
      begin
        FLogger.Debug('DPMIDE : Got Installed package references, fetching metadata...');
        result := repoManager.GetInstalledPackageFeed(cancelToken, options, GetPackageIdsFromReferences(ProjectPlatformToDPMPlatform(FProject.CurrentPlatform)), FConfiguration);
        for item in result do
        begin
          if FPackageReferences <> nil then
          begin
            packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item);
            if packageRef <> nil then
              item.IsTransitive := packageRef.IsTransitive;
          end;
        end;
        FLogger.Debug('DPMIDE : Got Installed package metadata.');
      end;

    end, FCancelTokenSource.Token);
end;

function TDPMEditViewFrame.GetPackageIdsFromReferences(const platform : TDPMPlatform) : IList<IPackageId>;
var
  lookup : IDictionary<string, IPackageId>;
  packageRef : IGraphNode;

  procedure AddPackageIds(const value : IGraphNode);
  var
    childRef : IGraphNode;
  begin
    if not (value.Platform = platform) then
      exit;

    if not lookup.ContainsKey(Lowercase(value.Id)) then
      lookup[Lowercase(value.Id)] := value;

    for childRef in value.ChildNodes do
      AddPackageIds(childRef);
  end;

begin
  lookup := TCollections.CreateDictionary < string, IPackageId > ;
  result := TCollections.CreateList<IPackageId>;
  if FPackageReferences <> nil then
  begin
    for packageRef in FPackageReferences.ChildNodes do
    begin
      AddPackageIds(packageRef);
    end;
    result.AddRange(lookup.Values);
  end;
end;

function TDPMEditViewFrame.GetUpdatedPackages : IAwaitable<IList<IPackageSearchResultItem>>;
begin
  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result := TCollections.CreateList<IPackageSearchResultItem>;

      //simulating long running.
      //WaitForSingleObject(cancelToken.Handle, 3000);
    end, FCancelTokenSource.Token);
end;

function TDPMEditViewFrame.IsProjectGroup : boolean;
begin
  result := FProjectGroup <> nil;
end;

procedure TDPMEditViewFrame.LoadList(const list : IList<IPackageSearchResultItem>);
begin
  if list = nil then
  begin
    FScrollList.RowCount := 0;
    exit;
  end;
  FScrollList.CurrentRow := -1;
  if FScrollList.RowCount = list.Count then
    FScrollList.Invalidate  //doing this because if the rowcount is the same it doesn't invalidate.
  else
    FScrollList.RowCount := list.Count;
  if list.Count > 0 then
    FScrollList.CurrentRow := 0;

end;

procedure TDPMEditViewFrame.PackageInstalled(const package : IPackageSearchResultItem);
var
  packageSearchResult : IPackageSearchResultItem;
begin
  package.Installed := true;
  package.InstalledVersion := package.Version;
  FAllInstalledPackages := nil;
  FInstalledPackages := nil;

  //if it's in the search results, update the installed version.
  if FSearchResultPackages <> nil then
  begin
    packageSearchResult := FSearchResultPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id,
          package.Id) and (value.Version = package.Version);
      end);

    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := true;
      packageSearchResult.InstalledVersion := package.Version;
    end;
  end;

  if FAllInstalledPackages = nil then
  begin
    FAllInstalledPackages :=
    TCollections.CreateList<IPackageSearchResultItem>;
    FAllInstalledPackages.Add(package);
  end
  else
  begin
    packageSearchResult := FAllInstalledPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id,
          package.Id) and (value.Version = package.Version);
      end);
    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := true;
      packageSearchResult.InstalledVersion := package.Version;
    end
    else
      FAllInstalledPackages.Add(packageSearchResult);
  end;
  if FCurrentTab = TCurrentTab.Installed then
    FilterAndLoadInstalledPackages(txtSearch.Text);

  //Tell the IDE to reload the project as we have just modified it on disk.
  FProject.Refresh(true);

  //force the project tree to update after installing package.
  FProjectTreeManager.NotifyEndLoading();
end;

procedure TDPMEditViewFrame.PackageUninstalled(const package : IPackageSearchResultItem);
var
  packageSearchResult : IPackageSearchResultItem;
begin
  package.Installed := false;
  package.InstalledVersion := '';

  //if it's in the search results, update the installed version.
  //might be nil if we haven't switched to the tab yet.
  if FSearchResultPackages <> nil then
  begin
    packageSearchResult := FSearchResultPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id,
          package.Id) and (value.Version = package.Version);
      end);

    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := false;
      packageSearchResult.InstalledVersion := '';
    end;
  end;

  //shouldn't be nil
  if FAllInstalledPackages <> nil then
  begin
    packageSearchResult := FAllInstalledPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id,
          package.Id) and (value.Version = package.Version);
      end);
    if packageSearchResult <> nil then
    begin
      FAllInstalledPackages.Remove(packageSearchResult);
      if FInstalledPackages <> nil then
        FInstalledPackages.Remove(packageSearchResult);
      if FCurrentTab = TCurrentTab.Installed then
        SwitchedToInstalled(False);
//        FilterAndLoadInstalledPackages(txtSearch.Text);
    end;
  end;

  //Tell the IDE to reload the project
  FProject.Refresh(true);
  //force the project tree to update after installing package.
  FProjectTreeManager.NotifyEndLoading();

end;

procedure TDPMEditViewFrame.platformChangeDetectTimerTimer(Sender : TObject);
var
  projectEditor : IProjectEditor;
  projectPlatform : TDPMPlatform;
begin
  // since the tools api provides no notifications about active platform change
  // we have to resort to this ugly hack.
  platformChangeDetectTimer.Enabled := false;
  if FProject <> nil then
  begin

    projectPlatform := ProjectPlatformToDPMPlatform(FProject.CurrentPlatform);
    if projectPlatform = TDPMPlatform.UnknownPlatform then
      raise Exception.Create('FProject.CurrentPlatform : ' + FProject.CurrentPlatform);

    if FCurrentPlatform <> projectPlatform then
    begin
      FCurrentPlatform := projectPlatform;
      projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
      projectEditor.LoadProject(FProject.FileName);
      FPackageReferences := projectEditor.GetPackageReferences(FCurrentPlatform); //NOTE : Can return nil. Will change internals to return empty root node.
      //TODO : need to do this more safely as it may interrup another operation.
      FInstalledPackages := nil;  //force refresh as we always need to update the installed packages.
      FAllInstalledPackages := nil;
      FSearchResultPackages := nil;
      FUpdates := nil;
      PackageDetailsFrame.SetPlatform(FCurrentPlatform);
      PackageDetailsFrame.SetPackage(nil);
      FScrollList.CurrentRow := -1;
      case FCurrentTab of
        TCurrentTab.Search : SwitchedToSearch(true);
        TCurrentTab.Installed : SwitchedToInstalled(true);
        TCurrentTab.Updates : SwitchedToUpdates(true);
        TCurrentTab.Conflicts : SwitchedToConflicts(True);
      end;
    end;
  end;
  platformChangeDetectTimer.Enabled := true;
end;

procedure TDPMEditViewFrame.ProjectReloaded;
begin
  FLogger.Debug('DPMIDE : EditViewReloaded');
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  platformChangeDetectTimerTimer(platformChangeDetectTimer);
end;

procedure TDPMEditViewFrame.ReloadSourcesCombo;
var
  sCurrent : string;
  source : ISourceConfig;
  idx : integer;
begin
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

end;

procedure TDPMEditViewFrame.RequestPackageIcon(const index : integer; const package : IPackageSearchResultItem);
var
  platform : TDPMPlatform;
  id : string;
  version : string;
  source : string;
  repoManager : IPackageRepositoryManager;
  config : IConfiguration;
  stopWatch : TStopWatch;
begin
  stopWatch.Start;
  //we need a platform to pass to the repo, because the directory repo needs to construct a filename
  //we just get the first plaform in the set, doesn't matter which one.
  for platform in package.Platforms do
    break;

  //local for capture for use in the anonymous methods below.
  id := package.Id;
  version := package.Version;
  source := package.SourceName;
  FLogger.Debug('DPMIDE : Requesting icon for [' + id + '.' + version + ']');

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  config := FConfiguration;

  TAsync.Configure<IPackageIcon>(
    function(const cancelToken : ICancellationToken) : IPackageIcon
    begin
      result := repoManager.GetPackageIcon(cancelToken, source, id, version,
        IDECompilerVersion, platform, FConfiguration);
    end, FCancelTokenSource.Token)
  .OnException(
    procedure(const e : Exception)
    begin
      if FClosing then
        exit;
      FLogger.Error(e.Message);
    end)
  .Await(
    procedure(const theResult : IPackageIcon)
    var
      icon : IPackageIconImage;
    begin
      if FClosing then
        exit;

      //the result can be nil if there is no icon found
      if theResult <> nil then
      try
        icon := TPackageIconImage.Create(theResult);
      except
        icon := nil;
        FLogger.Debug('DPMIDE : INVALID icon for [' + id + '.' + version + ']');
      end;

      //we will cache nil to stop future pointeless requests.
      FIconCache.Cache(id, icon);
      stopWatch.Stop;
      if icon <> nil then
        FLogger.Debug('DPMIDE : Got icon for [' + id + '.' + version + '] in ' + IntToStr(stopWatch.ElapsedMilliseconds) + 'ms');
      if icon <> nil then
        //TODO : Instead request repaint of row.
        FScrollList.Invalidate;
    end);

end;

procedure TDPMEditViewFrame.SaveBeforeChange;
begin
  (BorlandIDEServices as IOTAModuleServices).SaveAll;
end;

procedure TDPMEditViewFrame.ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);
var
  item : IPackageSearchResultItem;
  list : IList<IPackageSearchResultItem>;
begin
  list := CurrentList;
  if list = nil then
  begin
    PackageDetailsFrame.SetPackage(nil);
    exit;
  end;

  item := nil;
  if (newRowIndex >= 0) and (newRowIndex < list.Count) then
    item := list[newRowIndex];

  PackageDetailsFrame.SetPackage(item);

end;

procedure TDPMEditViewFrame.ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
begin
  ACanvas.Font.Assign(Self.Font);
  ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  if FRequestInFlight then
    ACanvas.TextOut(20, 20, 'Loading....')
  else
  begin
    if  FHasSources then
      ACanvas.TextOut(20, 20, 'No Packages found')
    else
      ACanvas.TextOut(20, 20, 'No enabled package sources, add a package source in DPM settings.')
  end;
end;

procedure TDPMEditViewFrame.ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
var
  item : IPackageSearchResultItem;
  list : IList<IPackageSearchResultItem>;
  titleRect : TRect;
  reservedRect : TRect;
  title : string;
  fontSize : integer;
  backgroundColor : TColor;
  //  foregroundColor : TColor;
  icon : IPackageIconImage;
  extent : TSize;
  oldTextAlign : UINT;
  focusRect : TRect;
begin
  list := CurrentList;
  if (list = nil) or (not list.Any) then
  begin
    FScrollList.RowCount := 0; //will force a repaint.
    //ScrollListPaintNoRows(Sender, ACanvas, FScrollList.ClientRect);
    exit;
  end;
  if (index >= 0) and (index < list.Count) then
    item := list[index]
  else
    exit;

  FRowLayout.Update(ACanvas, itemRect, false);

  fontSize := ACanvas.Font.Size;
  try
    if (state in [rsFocusedSelected, rsFocusedHot, rsHot]) then
    begin
      {$IF CompilerVersion < 32.0}
      backgroundColor := $00FFF0E9; // StyleServices.GetSystemColor(clHighlight);
      ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
      {$ELSE}
      backgroundColor := FIDEStyleServices.GetStyleColor(TStyleColor.scButtonHot);
      ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clHighlightText);
      {$IFEND}
    end
    else
    begin
      backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);
      ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
    end;

    //row background
    ACanvas.Brush.Color := backgroundColor;
    ACanvas.FillRect(itemRect);

    icon := nil;

    if item.Icon <> '' then
    begin
      //first query will add nil to avoid multiple requests
      if not FIconCache.Query(item.Id) then
        //fetch the icon async
        RequestPackageIcon(index, item)
      else
        //this might return nil if the request hasn't completed
        //or it failed to find the icon.
        icon := FIconCache.Request(item.Id);
    end;

    if icon = nil then
      icon := FIconCache.Request('missing_icon');

    icon.PaintTo(ACanvas, FRowLayout.IconRect);
    //ACanvas.StretchDraw(FRowLayout.IconRect, icon );

    //TODO : this all feels super hacky, revisit when IDE supports high dpi/scaling.

//    leaving here as it's usefull for debugging layout
//    ACanvas.Brush.Color := clRed;
//    ACanvas.FillRect(FRowLayout.TitleRect);
//    ACanvas.Brush.Color := backgroundColor;

    //make text of different font sizes align correctly.
    oldTextAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    fontSize := ACanvas.Font.Size;
    try
      //Draw the package Name.
      titleRect := FRowLayout.TitleRect;
      title := item.Id;

      ACanvas.Font.Size := fontSize + 2;
      ACanvas.Font.Style := [fsBold];

      extent := ACanvas.TextExtent(title);
      ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title), nil);
      titleRect.Left := titleRect.Left + extent.cx + 4;

      ACanvas.Font.Size := fontSize;
      ACanvas.Font.Style := [];

      //if there is room and the prefix is reserved, paint the reserved icon
      if item.IsReservedPrefix and ((FRowLayout.VersionRect.Left - titleRect.Right) > 16) then
      begin
        reservedRect.Left := titleRect.Right + 2;
        reservedRect.Top := titleRect.Top + 4;
        reservedRect.Width := 16;
        reservedRect.Height := 16;
        DPMEditorViewImages.Draw(ACanvas, reservedRect.Left, reservedRect.Top, 4, TDrawingStyle.dsTransparent, TImageType.itImage);
        titleRect.Left := reservedRect.Right + 4;
        titleRect.Right := FRowLayout.TitleRect.Right;
      end;

      title := ' by ' + item.Authors;
      if item.Downloads > 0 then
        title := title + ', ';

      extent := ACanvas.TextExtent(title);

      ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title), nil);
      titleRect.Left := titleRect.Left + extent.cx;

      if item.Downloads > 0 then
      begin
        title := TIntegerUtils.InToStringAbbr(item.Downloads);
        ACanvas.Font.Style := [fsBold];
        extent := ACanvas.TextExtent(title);
        ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title), nil);
        titleRect.Left := titleRect.Left + extent.cx + 4;

        ACanvas.Font.Style := [];
        title := ' downloads';
        extent := ACanvas.TextExtent(title);
        ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title), nil);
      end;
    finally
      //this is important
      SetTextAlign(ACanvas.Handle, oldTextAlign);
    end;
    //version
    if item.Installed and (item.Version <> item.InstalledVersion) then
      title := 'v' + item.InstalledVersion
    else
      title := 'v' + item.Version;
    ACanvas.TextRect(FRowLayout.VersionRect, title, [tfSingleLine, tfVerticalCenter, tfRight]);

    if item.Installed and (item.Version <> item.InstalledVersion) then
    begin
      title := 'v' + item.Version;
      ACanvas.TextRect(FRowLayout.InstalledVersionRect, title, [tfSingleLine, tfVerticalCenter, tfRight]);
    end;

    //description
    title := item.Description;
    ACanvas.TextRect(FRowLayout.DescriptionRect, title, [tfEndEllipsis, tfWordBreak]);

    if (state in [rsFocusedSelected, rsSelected]) then
    begin
      focusRect := itemRect;
      InflateRect(focusRect, -3, -3);
      ACanvas.DrawFocusRect(focusRect);
    end;

  finally
    ACanvas.Font.Style := [];
    ACanvas.Font.Size := fontSize;
  end;
end;

procedure TDPMEditViewFrame.searchDebounceTimerTimer(Sender : TObject);
begin
  searchDebounceTimer.Enabled := false;

  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;

  FCancelTokenSource.Reset;

  case FCurrentTab of
    TCurrentTab.Installed :
      begin
        //todo : apply filter to installed
        SwitchedToInstalled(true);
      end;
    TCurrentTab.Updates :
      begin
        SwitchedToUpdates(true);
      end;
    TCurrentTab.Search :
      begin
        SwitchedToSearch(true);
      end;
    TCurrentTab.Conflicts :
      begin

      end;
  end;

end;

function TDPMEditViewFrame.GetSearchOptions : TSearchOptions;
begin
  result := FSearchOptions.Clone;
end;

function TDPMEditViewFrame.SearchForPackages(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
var
  repoManager : IPackageRepositoryManager;
begin
  //local for capture
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;

  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result := repoManager.GetPackageFeed(cancelToken, options, FConfiguration);
      //simulating long running.
    end, FCancelTokenSource.Token);

end;

procedure TDPMEditViewFrame.SwitchedToConflicts(const refresh : boolean);
begin
  if FGotConflicts and (not refresh) then
    LoadList(FConflictPackages)
  else
  begin
    FScrollList.RowCount := 0;
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
    FLogger.Debug('DPMIDE : Searching for conflicting packages');
    GetConflictingPackages
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
        FLogger.Debug('DPMIDE : Cancelled getting conflicting packages.');
      end)
    .Await(
      procedure(const theResult : IList<IPackageSearchResultItem>)
      begin
        FRequestInFlight := false;
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got conflicting packages.');

        FConflictPackages := theResult;
        FGotConflicts := true;
        LoadList(FConflictPackages);
      end);
  end;

end;

procedure TDPMEditViewFrame.FilterAndLoadInstalledPackages(const searchTxt : string);
begin
  FInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>(FAllInstalledPackages.Where(
    function(const pkg : IPackageSearchResultItem) : boolean
    begin
      result := not pkg.IsTransitive;
      if result and (searchTxt <> '') then
      begin
        result := TStringUtils.Contains(pkg.Id, searchTxt, true);
      end;
    end));

  LoadList(FInstalledPackages);
end;

type
  TFilterProc = procedure(const searchTxt : string) of object;

procedure TDPMEditViewFrame.SwitchedToInstalled(const refresh : boolean);
var
  searchTxt : string;

  filterProc : TFilterProc;

  //filter the list to only show directly installed packages, and on the search term

begin
  FLogger.Debug('DPMIDE : SwitchToInstalled');
  searchTxt := txtSearch.Text;
  chkIncludePrerelease.Visible := true;
  chkIncludeCommercial.Visible := false;
  chkIncludeTrial.Visible := false;
  filterProc := FilterAndLoadInstalledPackages;
  if (not refresh) and (FAllInstalledPackages <> nil) and FAllInstalledPackages.Any then
    filterProc(searchTxt)
  else
  begin
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
    FAllInstalledPackages := nil;
    FInstalledPackages := nil;
    FScrollList.RowCount := 0;
    FLogger.Debug('DPMIDE : Getting Installed Packages..');
    GetInstalledPackages
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
        FLogger.Debug('DPMIDE : Cancelled getting installed packages.');
      end)
    .Await(
      procedure(const theResult : IList<IPackageSearchResultItem>)
      begin
        FRequestInFlight := false;
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got installed packages.');

        FAllInstalledPackages := theResult;

        filterProc(searchTxt);
      end);
  end;

end;

procedure TDPMEditViewFrame.SwitchedToSearch(const refresh : boolean);
begin
  chkIncludePrerelease.Visible := true;
  chkIncludeCommercial.Visible := true;
  chkIncludeTrial.Visible := true;
  if (not refresh) and (FSearchResultPackages <> nil) and FSearchResultPackages.Any then
    LoadList(FSearchResultPackages)
  else
  begin
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
    FLogger.Debug('DPMIDE : Searching for Packages..');
    FSearchOptions.SearchTerms := Trim(txtSearch.Text);
    FSearchOptions.Prerelease := chkIncludePrerelease.Checked;
    FSearchOptions.Commercial := chkIncludeCommercial.Checked;
    FSearchOptions.Trial := chkIncludeTrial.Checked;
    FSearchOptions.Platforms := [FCurrentPlatform];
    SearchForPackages(FSearchOptions)
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
        FLogger.Debug('DPMIDE : Cancelled searching for packages.');
      end)
    .Await(
      procedure(const theResult : IList<IPackageSearchResultItem>)
      var
        item : IPackageSearchResultItem;
        packageRef : IGraphNode;
      begin
        FRequestInFlight := false;
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got search results.');
        FSearchResultPackages := theResult;

        for item in FSearchResultPackages do
        begin
          packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item);
          item.Installed := (packageRef <> nil) and (not packageRef.IsTransitive);
          if item.Installed then
            item.InstalledVersion := packageRef.Version.ToStringNoMeta;
        end;

        FScrollList.RowCount := FSearchResultPackages.Count;
        LoadList(FSearchResultPackages);
      end);
  end;
end;

procedure TDPMEditViewFrame.SwitchedToUpdates(const refresh : boolean);
begin
  chkIncludePrerelease.Visible := true;
  chkIncludeCommercial.Visible := false;
  chkIncludeTrial.Visible := false;
  if (not refresh) and (FUpdates <> nil) and FUpdates.Any then
    LoadList(FUpdates)
  else
  begin
    FScrollList.RowCount := 0;
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
    FLogger.Debug('DPMIDE : Getting Updated Packages..');
    GetUpdatedPackages
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
        FLogger.Debug('DPMIDE : Cancelled getting updated packages.');
      end)
    .Await(
      procedure(const theResult : IList<IPackageSearchResultItem>)
      begin
        FRequestInFlight := false;
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got updated packages.');

        FUpdates := theResult;
        LoadList(FUpdates);
      end);
  end;

end;

procedure TDPMEditViewFrame.SwitchTabs(const currentTab : TCurrentTab; const
  refresh : boolean);
var
  doRefresh : boolean;
begin

  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;
  FCancelTokenSource.Reset;

  FCurrentTab := currentTab;

  FScrollList.RowCount := 0;
  PackageDetailsFrame.SetPackage(nil);
  PackageDetailsFrame.Configure(FCurrentTab, chkIncludePrerelease.Checked);

  doRefresh := refresh or (txtSearch.Text <> '');

  case FCurrentTab of
    TCurrentTab.Search : SwitchedToSearch(doRefresh);
    TCurrentTab.Installed : SwitchedToInstalled(doRefresh);
    TCurrentTab.Updates : SwitchedToUpdates(doRefresh);
    TCurrentTab.Conflicts : SwitchedToConflicts(doRefresh);
  end;
end;

procedure TDPMEditViewFrame.Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
var
  sConfigFile : string;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;

  if not Supports(projectOrGroup, IOTAProjectGroup, FProjectGroup) then
  begin
    lblProject.Caption := 'DPM : ' + ChangeFileExt(ExtractFileName(projectOrGroup.FileName), '');
    FProject := projectOrGroup;
    FConflictsButton.Visible := false;
  end
  else
  begin
    FConflictsButton.Visible := true;
    FProject := nil;
    lblProject.Caption := 'DPM : Project Group';
  end;

  FLogger := FContainer.Resolve<IDPMIDELogger>;
  FDPMIDEOptions := FContainer.Resolve<IDPMIDEOptions>;
  //if there is a project specific config file then that is what we should use.
  sConfigFile := IncludeTrailingPathDelimiter(ExtractFilePath(projectOrGroup.FileName)) + cDPMConfigFileName;
  if FileExists(sConfigFile) then
  begin
    FSearchOptions.ConfigFile := sConfigFile;
    FConfigIsLocal := true; //noting this so we can and tell what to do when the settings button is clicked.
  end;

  // This ensures that the default config file is uses if a project one doesn't exist.
  FSearchOptions.ApplyCommon(TCommonOptions.Default);

  //load our dpm configuration
  FConfigurationManager := FContainer.Resolve<IConfigurationManager>;
  FConfigurationManager.EnsureDefaultConfig;
  FConfiguration := FConfigurationManager.LoadConfig(FSearchOptions.ConfigFile);

  PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, FProject.FileName);
  //populate the sources combo.
  ReloadSourcesCombo;
end;

procedure TDPMEditViewFrame.tabMainChange(Sender : TObject);
var
  tab : TCurrentTab;
begin
  tab := TCurrentTab(TDPMGroupButton(Sender).Tag);
  SwitchTabs(tab, txtSearch.Text <> '');
end;

procedure TDPMEditViewFrame.ThemeChanged;
{$IF CompilerVersion >=32.0}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$IFEND}
begin
  {$IF CompilerVersion >=32.0}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  ideThemeSvc.ApplyTheme(pnlButtonBar);
  ideThemeSvc.ApplyTheme(pnlSearchPanel);
  ideThemeSvc.ApplyTheme(Splitter2);
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$IFEND}

  //
  pnlButtonBar.Color := FIDEStyleServices.GetSystemColor(clBtnFace);
  pnlButtonBar.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  pnlSearchPanel.Color := FIDEStyleServices.GetSystemColor(clBtnFace);
  pnlSearchPanel.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  Splitter2.Color := FIDEStyleServices.GetSystemColor(clBtnFace);

  FSearchButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FInstalledButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FUpdatesButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FConflictsButton.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  FScrollList.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FScrollList.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  PackageDetailsFrame.Color := FIDEStyleServices.GetSystemColor(clWindow);
  PackageDetailsFrame.ThemeChanged;
  //  Self.Invalidate;

end;

procedure TDPMEditViewFrame.txtSearchChange(Sender : TObject);
begin
  txtSearch.RightButton.Visible := txtSearch.Text <> '';
end;

procedure TDPMEditViewFrame.txtSearchChanged(Sender : TObject);
begin
end;

procedure TDPMEditViewFrame.txtSearchKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
var
  i : integer;
begin
  searchDebounceTimer.Enabled := false;
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
        searchDebounceTimerTimer(searchDebounceTimer);
      end
  else
    searchDebounceTimer.Enabled := true;
  end;
end;

procedure TDPMEditViewFrame.txtSearchRightButtonClick(Sender : TObject);
begin
  txtSearch.Text := '';
  FSearchResultPackages := nil;
  FInstalledPackages := FAllInstalledPackages;
  FUpdates := nil;
  searchDebounceTimerTimer(searchDebounceTimer);
end;

procedure TDPMEditViewFrame.ViewDeselected;
begin
  // The view tab was deselected.
  FLogger.Debug('DPMIDE : View Deselected');
  platformChangeDetectTimer.Enabled := false;
end;

procedure TDPMEditViewFrame.ViewSelected;
begin
  //For some reason this get's called twice for each time the view is selected.
  platformChangeDetectTimer.Enabled := true;
  FLogger.Debug('DPMIDE : View Selected');
  //The first time the view is opened we want to switch to the installed packages tab
  if FFirstView then
  begin
    FFirstView := false;
    platformChangeDetectTimerTimer(platformChangeDetectTimer);
    //SwitchedToInstalled(true);
  end;
end;

{ TRowLayout }

procedure TRowLayout.Update(const ACanvas : TCanvas; const rowRect : TRect; const showSelect : Boolean);
var
  bUpdateLayout : boolean;
begin
  bUpdateLayout := rowRect.Width <> RowWidth;
  if bUpdateLayout then
  begin
    IconRect.Top := rowRect.Top + 5;
    IconRect.Left := rowRect.Left + 10;
    IconRect.Width := 32;
    IconRect.Height := 32;

    VersionRect.Top := IconRect.Top;
    VersionRect.Right := rowRect.Right - 50;
    VersionRect.Height := Abs(ACanvas.Font.Height) + 10;
    //TODO : Figure out a better way to ensure this will be correct
    VersionRect.Left := rowRect.Right - ACanvas.TextExtent('1.100.100-aplha123aaaaa').Width - 10;

    InstalledVersionRect.Top := VersionRect.Bottom + 10;
    InstalledVersionRect.Right := rowRect.Right - 50;
    InstalledVersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    //TODO : Figure out a better way to ensure this will be correct
    InstalledVersionRect.Left := VersionRect.Left;

    TitleRect.Top := IconRect.Top;
    TitleRect.Height := Abs(ACanvas.Font.Height) * 2;
    TitleRect.Left := rowRect.Left + 10 + 32 + 10;
    TitleRect.Right := VersionRect.Left - 5;
    DescriptionRect.Top := TitleRect.Bottom + 4;
    DescriptionRect.Left := TitleRect.Left;
    DescriptionRect.Bottom := rowRect.Bottom - 6;
    DescriptionRect.Right := VersionRect.Left - 5;

    RowWidth := rowRect.Width;
  end
  else
  begin
    //just update top and bottom of rects.
    IconRect.Top := rowRect.Top + 5;
    IconRect.Height := 32;

    VersionRect.Top := IconRect.Top;
    VersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    InstalledVersionRect.Top := VersionRect.Bottom + 10;
    InstalledVersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    TitleRect.Top := IconRect.Top;
    TitleRect.Height := Abs(ACanvas.Font.Height) * 2;
    DescriptionRect.Top := TitleRect.Bottom + 4;
    DescriptionRect.Bottom := rowRect.Bottom - 5;
  end;

end;

end.

