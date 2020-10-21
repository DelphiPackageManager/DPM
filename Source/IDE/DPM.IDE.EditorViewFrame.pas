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

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls,   Vcl.Menus, SVGInterfaces,
  DPM.Controls.ButtonedEdit,
  DPM.Controls.GroupButton,
  VSoftVirtualListView,
  ToolsApi,
  Spring.Collections,
  Spring.Container,
  VSoft.Awaitable,
  DPM.IDE.Types,
  DPM.IDE.IconCache,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.IDE.ProjectTreeManager,
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  Vcl.ActnList, DPM.IDE.PackageDetailsFrame;

type
  TRowLayout = record
    RowWidth : integer;
    IconRect : TRect;
    TitleRect: TRect;
    VersionRect : TRect;
    InstalledVersionRect : TRect;
    DescriptionRect : TRect;
    procedure Update(const ACanvas : TCanvas; const rowRect : TRect; const showSelect : Boolean );
  end;


  TDPMEditViewFrame = class(TFrame, IPackageSearcher)
    lblProject: TLabel;
    DPMEditorViewImages: TImageList;
    SearchPanel: TPanel;
    txtSearch: TButtonedEdit;
    btnRefresh: TButton;
    chkIncludePrerelease: TCheckBox;
    ContentPanel: TPanel;
    Splitter2: TSplitter;
    chkIncludeCommercial: TCheckBox;
    btnSettings: TButton;
    btnAbout: TButton;
    lblSources: TLabel;
    cbSources: TComboBox;
    pnlButtonBar: TPanel;
    chkIncludeTrial: TCheckBox;
    searchDebounceTimer: TTimer;
    PackageListPanel: TPanel;
    PackageDetailsFrame: TPackageDetailsFrame;
    platformChangeDetectTimer: TTimer;
    procedure txtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnAboutClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure tabMainChange(Sender: TObject);
    procedure searchDebounceTimerTimer(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chkIncludePrereleaseClick(Sender: TObject);
    procedure platformChangeDetectTimerTimer(Sender: TObject);
    procedure txtSearchChange(Sender: TObject);
    procedure txtSearchRightButtonClick(Sender: TObject);
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
    FLogger : ILogger;
    FProjectTreeManager : IDPMProjectTreeManager;
    FConfigurationManager : IConfigurationManager;
    FConfiguration : IConfiguration;
    FConfigIsLocal : boolean;


    //RS IDE Stuff
    FProjectGroup : IOTAProjectGroup;
    FProject : IOTAProject;
    FCurrentPlatform : TDPMPlatform;

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


    FPackageReferences : IList<IPackageReference>;

    FSearchOptions : TSearchOptions;
    FSearchSkip : integer;
    FSearchTake : integer;

    FUpdates : IList<IPackageSearchResultItem>;
    //true when we first load the view
    FFirstView : boolean;
  protected
    procedure txtSearchChanged(Sender : TObject);
    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64 );

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
    function GetSearchOptions: TSearchOptions;
    function SearchForPackages(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
    function GetCurrentPlatform : string;
    procedure PackageInstalled(const package : IPackageSearchResultItem);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);

    procedure SaveBeforeChange;

    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;

    //called from the EditorView
    procedure Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);
    procedure ViewSelected;
    procedure ViewDeselected;
    procedure Closing;
    procedure ProjectReloaded;
  end;

implementation

{$R *.dfm}

uses
  System.Types,
  Xml.XMLIntf,
  Vcl.Themes,
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

const cDMPSearchHistoryFile = 'packagesearch.txt';

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

procedure TDPMEditViewFrame.btnAboutClick(Sender: TObject);
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

procedure TDPMEditViewFrame.btnRefreshClick(Sender: TObject);
begin
  FSearchResultPackages.Clear;
  //also called by the include checkboxes.
  case FCurrentTab of
    TCurrentTab.Installed: SwitchedToInstalled(true);
    TCurrentTab.Updates: SwitchedToUpdates(true);
    TCurrentTab.Search: SwitchedToSearch(true);
    TCurrentTab.Conflicts: SwitchedToConflicts(true);
  end;
end;

procedure TDPMEditViewFrame.btnSettingsClick(Sender: TObject);
var
  bReload : boolean;
  optionsHost : TDPMOptionsHostForm;
begin
  optionsHost := TDPMOptionsHostForm.Create(Self, FConfigurationManager, FLogger, FSearchOptions.ConfigFile);
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

procedure TDPMEditViewFrame.chkIncludePrereleaseClick(Sender: TObject);
begin
  btnRefreshClick(Sender);
  PackageDetailsFrame.IncludePreRelease := chkIncludePrerelease.Checked;
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

constructor TDPMEditViewFrame.Create(AOwner: TComponent);
begin
  inherited;
  FIconCache := TDPMIconCache.Create;

  CreateControls(AOwner);
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

procedure TDPMEditViewFrame.CreateControls(AOwner: TComponent);
begin
  FInstalledButton := TDPMGroupButton.Create(Self);
  FUpdatesButton := TDPMGroupButton.Create(Self);
  FSearchButton := TDPMGroupButton.Create(Self);
  FConflictsButton := TDPMGroupButton.Create(Self);

  FSearchButton.Top := 10;
  FSearchButton.Left := 20;
  FSearchButton.Caption := 'Search';
  FSearchButton.Tag := 0;

  FInstalledButton.Top := 10;
  FInstalledButton.Left := FInstalledButton.Left + FInstalledButton.Width + 20;
  FInstalledButton.Caption := 'Installed';
  FInstalledButton.Tag := 1;
  FInstalledButton.Active := true;

  FUpdatesButton.Top := 10;
  FUpdatesButton.Left := FInstalledButton.Left + FInstalledButton.Width + 20;
  FUpdatesButton.Caption := 'Updates';
  FUpdatesButton.Tag := 2;


  FConflictsButton.Top := 10;
  FConflictsButton.Left := FInstalledButton.Left + FInstalledButton.Width + 20;
  FConflictsButton.Caption := 'Conflicts';
  FConflictsButton.Tag := 3;
  FConflictsButton.Visible := false;


  FInstalledButton.Parent := pnlButtonBar;
  FUpdatesButton.Parent := pnlButtonBar;
  FSearchButton.Parent := pnlButtonBar;
  FConflictsButton.Parent := pnlButtonBar;

  FInstalledButton.OnClick := tabMainChange;
  FUpdatesButton.OnClick := tabMainChange;
  FSearchButton.OnClick := tabMainChange;
  FConflictsButton.OnClick := tabMainChange;

  FScrollList := TVSoftVirtualListView.Create(Self);
  FScrollList.Align := alClient;
  FScrollList.BorderStyle := bsNone;
  FScrollList.BevelOuter := bvLowered;
  FScrollList.BevelEdges := [beRight];
  FScrollList.BevelKind := bkFlat;
  FScrollList.RowHeight := 75;
  FScrollList.RowCount := 0;
  FScrollList.OnPaintRow := Self.ScrollListPaintRow;
  FScrollList.OnPaintNoRows := Self.ScrollListPaintNoRows;
  FScrollList.OnRowChange := Self.ScrollListChangeRow;

  FScrollList.Constraints.MinWidth := 400;
  FScrollList.Color := clWhite;
  FScrollList.DoubleBuffered := true;
//  FScrollList.ParentBackground := false;

  FScrollList.Parent := PackageListPanel;

end;

function TDPMEditViewFrame.CurrentList: IList<IPackageSearchResultItem>;
begin
  case FCurrentTab of
    TCurrentTab.Installed : result := FInstalledPackages;
    TCurrentTab.Updates   : result := FUpdates;
    TCurrentTab.Search    : result := FSearchResultPackages;
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


function TDPMEditViewFrame.GetConflictingPackages: IAwaitable<IList<IPackageSearchResultItem>>;
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


  result := TAsync.Configure<IList<IPackageSearchResultItem>>(
    function (const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
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
    end,FCancelTokenSource.Token);
end;

function FindPackageRef(const references : IList<IPackageReference>; const platform : TDPMPlatform; const searchItem : IPackageSearchResultItem) : IPackageReference;
var
  ref : IPackageReference;
begin
  result := nil;
  for ref in references do
  begin
    if ref.Platform <> platform then
      continue;

    if SameText(ref.Id, searchItem.Id) then
      Exit(ref);
    if ref.HasDependencies then
    begin
      result := FindPackageRef(ref.Dependencies, platform, searchItem);
      if result <> nil then
        Exit(result);
    end;
  end;

end;


function TDPMEditViewFrame.GetCurrentPlatform: string;
begin
  //todo : change this.
  result := DPMPlatformToString(FCurrentPlatform);
end;

function TDPMEditViewFrame.GetInstalledPackages: IAwaitable<IList<IPackageSearchResultItem>>;
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


  result := TAsync.Configure<IList<IPackageSearchResultItem>>(
    function (const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      packageRef : IPackageReference;
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
          packageRef := FindPackageRef(FPackageReferences,FCurrentPlatform, item);
          if packageRef <> nil then
            item.IsTransitive := packageRef.IsTransitive;
        end;
        FLogger.Debug('DPMIDE : Got Installed package metadata.');
      end;

    end,FCancelTokenSource.Token);
end;

function TDPMEditViewFrame.GetPackageIdsFromReferences(const platform : TDPMPlatform) : IList<IPackageId>;
var
  lookup : IDictionary<string, IPackageId>;
  packageRef : IPackageReference;

  procedure AddPackageIds(const value : IPackageReference);
  var
    childRef : IPackageReference;
  begin
    if not (value.Platform = platform ) then
      exit;

    if not lookup.ContainsKey(Lowercase(value.Id)) then
      lookup[Lowercase(value.Id)] := value;

    for childRef in value.Dependencies do
      AddPackageIds(childRef);
  end;


begin
  lookup := TCollections.CreateDictionary<string, IPackageId>;
  result := TCollections.CreateList<IPackageId>;

  for packageRef in FPackageReferences do
  begin
    AddPackageIds(packageRef);
  end;
  result.AddRange(lookup.Values);
end;

function TDPMEditViewFrame.GetUpdatedPackages: IAwaitable<IList<IPackageSearchResultItem>>;
begin
  result := TAsync.Configure<IList<IPackageSearchResultItem>>(
    function (const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result := TCollections.CreateList<IPackageSearchResultItem>;

      //simulating long running.
      //WaitForSingleObject(cancelToken.Handle, 3000);
    end,FCancelTokenSource.Token);
end;

function TDPMEditViewFrame.IsProjectGroup: boolean;
begin
  result := FProjectGroup <> nil;
end;

procedure TDPMEditViewFrame.LoadList(const list: IList<IPackageSearchResultItem>);
begin
  if FScrollList.RowCount = list.Count then
    FScrollList.Invalidate //doing this because if the rowcount is the same it doesn't invalidate.
  else
    FScrollList.RowCount := list.Count;
end;

procedure TDPMEditViewFrame.PackageInstalled(const package: IPackageSearchResultItem);
begin

  package.InstalledVersion := package.Version;

  if (FAllInstalledPackages <> nil) then
    FInstalledPackages.Add(package)
  else if FCurrentTab = TCurrentTab.Installed then
    SwitchedToInstalled(true);
  FProject.Refresh(true);
  //force the project tree to update after installing package.
  FProjectTreeManager.NotifyEndLoading(TProjectLoadType.plSingle);
  // Do not do this, it will overrwrite package changes.
  //FProject.MarkModified;

end;

procedure TDPMEditViewFrame.PackageUninstalled(const package: IPackageSearchResultItem);
begin
  if FCurrentTab = TCurrentTab.Installed then
    SwitchedToInstalled(true)
  else
    FInstalledPackages := nil;
  FProject.Refresh(true);
  // Do not do this, it will overrwrite package changes.
  //  FProject.MarkModified;
end;

procedure TDPMEditViewFrame.platformChangeDetectTimerTimer(Sender: TObject);
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

    if FCurrentPlatform <> projectPlatform then
    begin
      FCurrentPlatform := projectPlatform;
      projectEditor := TProjectEditor.Create(FLogger, FConfiguration);
      projectEditor.LoadProject(FProject.FileName);
      FPackageReferences := projectEditor.PackageReferences;
      //TODO : need to do this more safely as it may interrup another operation.
      FInstalledPackages := nil; //force refresh as we always need to update the installed packages.
      case FCurrentTab of
        TCurrentTab.Search: SwitchedToSearch(true) ;
        TCurrentTab.Installed: SwitchedToInstalled(true) ;
        TCurrentTab.Updates: SwitchedToUpdates(true);
        TCurrentTab.Conflicts: SwitchedToConflicts(True);
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
  source: ISourceConfig;
  idx : integer;
begin
  if cbSources.Items.Count > 0 then
    sCurrent := cbSources.Items[cbSources.ItemIndex];
  cbSources.Clear;

  cbSources.Items.Add('All');

  for source in FConfiguration.Sources do
  begin
    if source.IsEnabled then
      cbSources.Items.Add(source.Name);
  end;

  if sCurrent <> '' then
  begin
    idx := cbSources.Items.IndexOf(sCurrent);
    if idx <> -1 then
      cbSources.ItemIndex := idx
    else
      cbSources.ItemIndex := 0;
  end;

end;

procedure TDPMEditViewFrame.RequestPackageIcon(const index : integer; const package: IPackageSearchResultItem);
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
  FLogger.Debug('DPMIDE : Requesting icon for [' + id + '.' + version + ']' );

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  config := FConfiguration;

  TAsync.Configure<IPackageIcon>(
    function (const cancelToken : ICancellationToken) : IPackageIcon
    begin
      result := repoManager.GetPackageIcon(cancelToken,source,id, version, IDECompilerVersion, platform, FConfiguration);
    end,FCancelTokenSource.Token)
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
          icon := TPackageIconImage.Create(theResult);

        //we will cache nil to stop future pointeless requests.
        FIconCache.Cache(id, icon);
        stopWatch.Stop;
        if icon <> nil then
          FLogger.Debug('DPMIDE : Got icon for [' + id + '.' + version + '] in ' + IntToStr(stopWatch.ElapsedMilliseconds) + 'ms' );
        if icon <> nil then
          //TODO : Instead request repaint of row.
          FScrollList.Invalidate;
      end);

end;

procedure TDPMEditViewFrame.SaveBeforeChange;
begin
  (BorlandIDEServices as IOTAModuleServices).SaveAll;
end;

procedure TDPMEditViewFrame.ScrollListChangeRow(const Sender: TObject; const newRowIndex: Int64; const direction: TScrollDirection; const delta: Int64);
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

procedure TDPMEditViewFrame.ScrollListPaintNoRows(const Sender: TObject; const ACanvas: TCanvas; const paintRect: TRect);
begin
  ACanvas.Font.Assign(Self.Font);
  ACanvas.TextOut(20,20, 'No Packages found');
end;

procedure TDPMEditViewFrame.ScrollListPaintRow(const Sender: TObject; const ACanvas: TCanvas; const itemRect: TRect; const index: Int64; const state: TPaintRowState);
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
      backgroundColor :=  $00FFF0E9;// StyleServices.GetSystemColor(clHighlight);
      ACanvas.Font.Color := clWindowText;// StyleServices.GetSystemColor(clHighlightText);
    end
    else
    begin
      backgroundColor := FScrollList.Color;// Self.Color;// StyleServices.GetSystemColor(clWindow);
      ACanvas.Font.Color := StyleServices.GetSystemColor(clWindowText);
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
    try
      //Draw the package Name.
      titleRect := FRowLayout.TitleRect;
      title := item.Id;
      fontSize := ACanvas.Font.Size;

      ACanvas.Font.Size := fontSize + 2;
      ACanvas.Font.Style := [fsBold];

      extent := ACanvas.TextExtent(title);
      ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title),nil );
      titleRect.Left := titleRect.Left + extent.cx + 4;

      ACanvas.Font.Size := fontSize;
      ACanvas.Font.Style := [];

      //if there is room and the prefix is reserved, paint the reserved icon
      if item.IsReservedPrefix and ((FRowLayout.VersionRect.Left - titleRect.Right) > 16) then
      begin
        reservedRect.Left := titleRect.Right + 2;
        reservedRect.Top  := titleRect.Top + 4;
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

      ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title),nil );
      titleRect.Left := titleRect.Left + extent.cx;

      if item.Downloads > 0 then
      begin
        title := TIntegerUtils.InToStringAbbr(item.Downloads);
        ACanvas.Font.Style := [fsBold];
        extent := ACanvas.TextExtent(title);
        ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title),nil );
        titleRect.Left := titleRect.Left + extent.cx + 4;

        ACanvas.Font.Style := [];
        title := ' downloads';
        extent := ACanvas.TextExtent(title);
        ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title),nil );
      end;
    finally
      //this is important
      SetTextAlign(ACanvas.Handle, oldTextAlign);
    end;
    //version
    if item.Installed and (item.Version <> item.InstalledVersion)then
      title := 'v' + item.InstalledVersion
    else
      title := 'v' + item.Version;
    ACanvas.TextRect(FRowLayout.VersionRect, title , [tfSingleLine, tfVerticalCenter, tfRight]);

    if item.Installed and (item.Version <> item.InstalledVersion) then
    begin
      title := 'v' + item.Version;
      ACanvas.TextRect(FRowLayout.InstalledVersionRect, title , [tfSingleLine, tfVerticalCenter, tfRight]);
    end;

    //description
    title := item.Description;
    ACanvas.TextRect(FRowLayout.DescriptionRect, title , [tfEndEllipsis,tfWordBreak]);


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

procedure TDPMEditViewFrame.searchDebounceTimerTimer(Sender: TObject);
begin
  searchDebounceTimer.Enabled := false;

  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;

  FCancelTokenSource.Reset;

  case FCurrentTab of
    TCurrentTab.Installed:
    begin
      //todo : apply filter to installed
      SwitchedToInstalled(true);
    end;
    TCurrentTab.Updates:
    begin
      SwitchedToUpdates(true);
    end;
    TCurrentTab.Search:
    begin
      SwitchedToSearch(true);
    end;
    TCurrentTab.Conflicts:
    begin

    end;
  end;


end;


function TDPMEditViewFrame.GetSearchOptions : TSearchOptions;
begin
  result := FSearchOptions.Clone;
end;

function TDPMEditViewFrame.SearchForPackages(const options: TSearchOptions): IAwaitable<IList<IPackageSearchResultItem>>;
var
  repoManager : IPackageRepositoryManager;
begin
  //local for capture
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;

  result := TAsync.Configure<IList<IPackageSearchResultItem>>(
    function (const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result :=repoManager.GetPackageFeed(cancelToken, options, FConfiguration);
      //simulating long running.
    end,FCancelTokenSource.Token);

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

procedure TDPMEditViewFrame.SwitchedToInstalled(const refresh : boolean);
var
  searchTxt : string;
begin
  FLogger.Debug('DPMIDE : SwitchToInstalled');
  searchTxt := txtSearch.Text;
  chkIncludePrerelease.Visible := true;
  chkIncludeCommercial.Visible := false;
  chkIncludeTrial.Visible := false;
  if (not refresh) and (FAllInstalledPackages <> nil) and FAllInstalledPackages.Any then
  begin
    FInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>(FAllInstalledPackages.Where(
                function(const pkg : IPackageSearchResultItem) : boolean
                begin
                    result := not pkg.IsTransitive;
                    if result and (searchTxt <> '') then
                    begin
                      result := TStringUtils.Contains(pkg.Id, searchTxt,  true);
                    end;
                end));

    LoadList(FInstalledPackages);
  end
  else
  begin
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
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

        //filter the list to only show directly installed packages, and on the search term
        FInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>(FAllInstalledPackages.Where(
          function(const pkg : IPackageSearchResultItem) : boolean
          begin
              result := not pkg.IsTransitive;
              if result and (searchTxt <> '') then
              begin
                result := TStringUtils.Contains(pkg.Id, searchTxt,  true);
              end;
          end));
         LoadList(FInstalledPackages);
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
    FSearchOptions.Trial      := chkIncludeTrial.Checked;
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
        packageRef : IPackageReference;
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
          item.Installed := packageRef <> nil;
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

  FLogger := FContainer.Resolve<ILogger>;

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
  FConfiguration := FConfigurationManager.LoadConfig(FSearchOptions.ConfigFile);

  PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, FProject.FileName);

  //populate the sources combo.
  ReloadSourcesCombo;
  platformChangeDetectTimerTimer(platformChangeDetectTimer);
end;

procedure TDPMEditViewFrame.tabMainChange(Sender: TObject);
begin
  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;
  FCancelTokenSource.Reset;

  FCurrentTab := TCurrentTab(TDPMGroupButton(Sender).Tag);

  FScrollList.RowCount := 0;
  PackageDetailsFrame.SetPackage(nil);
  PackageDetailsFrame.Configure(FCurrentTab, chkIncludePrerelease.Checked);
  case FCurrentTab of
    TCurrentTab.Installed: SwitchedToInstalled(txtSearch.Text <> '');
    TCurrentTab.Updates: SwitchedToUpdates(txtSearch.Text <> '');
    TCurrentTab.Search: SwitchedToSearch(txtSearch.Text <> '');
    TCurrentTab.Conflicts : SwitchedToConflicts(txtSearch.Text <> '');
  end;
end;

procedure TDPMEditViewFrame.txtSearchChange(Sender: TObject);
begin
  txtSearch.RightButton.Visible := txtSearch.Text <> '';
end;

procedure TDPMEditViewFrame.txtSearchChanged(Sender: TObject);
begin
end;

procedure TDPMEditViewFrame.txtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TDPMEditViewFrame.txtSearchRightButtonClick(Sender: TObject);
begin
  txtSearch.Text := '';
  FSearchResultPackages := nil;
  FInstalledPackages := nil;
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

    SwitchedToInstalled(true);
  end;
end;

{ TRowLayout }

procedure TRowLayout.Update(const ACanvas : TCanvas; const rowRect: TRect; const showSelect: Boolean);
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
    VersionRect.Left  := rowRect.Right - ACanvas.TextExtent('1.100.100-aplha123aaaaa').Width - 10;

    InstalledVersionRect.Top := VersionRect.Bottom + 10;
    InstalledVersionRect.Right := rowRect.Right - 50;
    InstalledVersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    //TODO : Figure out a better way to ensure this will be correct
    InstalledVersionRect.Left  := VersionRect.Left;

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
