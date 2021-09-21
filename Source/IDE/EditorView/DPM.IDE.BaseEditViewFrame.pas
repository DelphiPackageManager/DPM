unit DPM.IDE.BaseEditViewFrame;

interface

uses
  System.Classes,
  System.Types,
  ToolsApi,
  Spring.Container,
  Spring.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Themes,
  VSoft.Awaitable,
  DPM.Controls.ButtonedEdit,
  DPM.Controls.ButtonBar,
  VSoftVirtualListView,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.IDE.Types,
  DPM.IDE.SearchBarFrame,
  DPM.IDE.IconCache,
  DPM.IDE.Options,
  DPM.IDE.Logger,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.Details.Interfaces,
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  {$IF CompilerVersion >= 30.0 }
  System.ImageList,
  {$IFEND}
  Vcl.ActnList;

{$I ..\DPMIDE.inc}


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


  TDPMBaseEditViewFrame = class(TFrame, IPackageSearcher)
    ContentPanel : TPanel;
    Splitter2 : TSplitter;
    PackageListPanel : TPanel;
    DetailPanel: TPanel;
    platformChangeDetectTimer: TTimer;
    procedure platformChangeDetectTimerTimer(Sender : TObject);
  private
    FIDEStyleServices : TCustomStyleServices;

    //controls
    FButtonBar : TDPMButtonBar;
    FSearchBar : TDPMSearchBarFrame;
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

    //request stuff
    FCancelTokenSource : ICancellationTokenSource;
    FRequestInFlight : boolean;
    FClosing : boolean;

    FProjectGroup : IOTAProjectGroup;
    FProject : IOTAProject;


    FIconCache : TDPMIconCache;
    FDPMIDEOptions : IDPMIDEOptions;
    FCurrentPlatform : TDPMPlatform;
    FCurrentTab : TDPMCurrentTab;

    FSearchOptions : TSearchOptions;
    FSearchSkip : integer;
    FSearchTake : integer;


    FGotConflicts : boolean;

    FPackageReferences : IGraphNode;


    //true when we first load the view
    FFirstView : boolean;

  protected
    //all installed packages, direct and transitive
    FAllInstalledPackages : IList<IPackageSearchResultItem>;
    //directly installed packages, might be fitered.
    FInstalledPackages : IList<IPackageSearchResultItem>;
    FSearchResultPackages : IList<IPackageSearchResultItem>;
    FConflictPackages : IList<IPackageSearchResultItem>;
    FUpdates : IList<IPackageSearchResultItem>;


    //IPackageSearcher
    function GetSearchOptions : TSearchOptions;
    function SearchForPackagesAsync(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;overload;
    function SearchForPackages(const options : TSearchOptions) : IList<IPackageSearchResultItem>;overload;
    function GetCurrentPlatform : string;
    procedure InstallStarting;

    procedure PackageInstalled(const package : IPackageSearchResultItem);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
    function GetPackageReferences : IGraphNode;

    //IPackageSearcher

    function IsProjectGroup : boolean;virtual;
    procedure TabChanged(const tab : TDPMCurrentTab);
    function CurrentList : IList<IPackageSearchResultItem>;
    procedure RequestPackageIcon(const index : integer; const package : IPackageSearchResultItem);
    procedure DoPlatformChange(const newPlatform : TDPMPlatform);virtual;
    function GetPackageDetailsView : IPackageDetailsView;virtual;abstract;
    procedure SetShowConflictsTab(const value : boolean);

    function GetInstalledPackages : IAwaitable<IList<IPackageSearchResultItem>>;virtual;
    function GetUpdatedPackages : IAwaitable<IList<IPackageSearchResultItem>>;virtual;
    function GetConflictingPackages : IAwaitable<IList<IPackageSearchResultItem>>;virtual;

    procedure LoadList(const list : IList<IPackageSearchResultItem>);

    function DoGetPackageReferences : IGraphNode;virtual;abstract;

    function GetPackageIdsFromReferences(const platform : TDPMPlatform) : IList<IPackageId>;

    procedure FilterAndLoadInstalledPackages(const searchTxt : string);

    procedure SwitchedToInstalled(const refresh : boolean);
    procedure SwitchedToUpdates(const refresh : boolean);
    procedure SwitchedToSearch(const refresh : boolean);
    procedure SwitchedToConflicts(const refresh : boolean);


    procedure SwitchTabs(const currentTab : TDPMCurrentTab; const refresh : boolean);

    //callbacks from the searchbar
    procedure SearchBarSettingsChanged(const configuration : IConfiguration);
    procedure SearchBarPlatformChanged(const newPlatform : TDPMPlatform);
    procedure SearchBarOnSearch(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const platform : TDPMPlatform; const refresh : boolean);


    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);


    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);virtual;

    procedure ConfigureSearchBar;virtual;

    //these are used in the descendants
    property PackageDetailsView : IPackageDetailsView read GetPackageDetailsView;
    property Project : IOTAProject read FProject;
    property ProjectGroup : IOTAProjectGroup read FProjectGroup;
    property Logger : IDPMIDELogger read FLogger;
    property SearchBar : TDPMSearchBarFrame read FSearchBar;
    property CurrentPlatform : TDPMPlatform read FCurrentPlatform write FCurrentPlatform;
    property Configuration : IConfiguration read FConfiguration;
    property ConfigurationManager : IConfigurationManager read FConfigurationManager;
    property CurrentTab : TDPMCurrentTab read FCurrentTab;
    property DPMIDEOptions : IDPMIDEOptions read FDPMIDEOptions;
    property SearchOptions : TSearchOptions read FSearchOptions;
    property CancellationTokenSource : ICancellationTokenSource read FCancelTokenSource;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Configure(const projectOrGroup : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);virtual;
    procedure ViewSelected;virtual;
    procedure ViewDeselected;virtual;
    procedure Closing;virtual;
    procedure ProjectReloaded;virtual;
    procedure ThemeChanged;virtual;
  end;

implementation
{$R *.dfm}

uses
  Winapi.Windows,
  Xml.XMLIntf,
  System.Diagnostics,
  System.SysUtils,
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
  DPM.IDE.AddInOptionsHostForm, DPM.IDE.PackageDetailsFrame;

/// called by the searchbar onsearch event.
function FindPackageRef(const references : IGraphNode; const platform : TDPMPlatform; const searchItem : IPackageSearchResultItem) : IGraphNode;
var
  ref : IGraphNode;
begin
  result := nil;
  if (references = nil) or (not references.HasChildren) then
    exit;

  //breadth first search!
  for ref in references.ChildNodes do
  begin
    if ref.Platform <> platform then
      continue;

    if SameText(ref.Id, searchItem.Id) then
      Exit(ref);
  end;

  for ref in references.ChildNodes do
  begin
    if ref.Platform <> platform then
      continue;
    //depth search
    if ref.HasChildren then
    begin
      result := FindPackageRef(ref, platform, searchItem);
      if result <> nil then
        Exit(result);
    end;
  end;


end;


{ TDPMBaseEditViewFrame }

procedure TDPMBaseEditViewFrame.Closing;
begin
  FClosing := true;
  PackageDetailsView.ViewClosing;
  //cancel any pending requests asap. Needs to return quickly or the IDE will hang.
  FCancelTokenSource.Cancel;
  //allow the cancellation to happen.
  //if we don't do this we will get an excepion in the await or cancellation callbacks
  while FRequestInFlight do
    Application.ProcessMessages;
end;

procedure TDPMBaseEditViewFrame.Configure(const projectOrGroup: IOTAProject; const container: TContainer; const projectTreeManager: IDPMProjectTreeManager);
var
  sConfigFile : string;
  sFileName : string;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;
  sFileName := projectOrGroup.FileName;
  FProject := projectOrGroup;
  if not Supports(projectOrGroup, IOTAProjectGroup, FProjectGroup) then
  begin
    FSearchBar.Caption := 'DPM : ' + ChangeFileExt(ExtractFileName(projectOrGroup.FileName), '');
    FProject := projectOrGroup;
    FProjectGroup := nil;
    SetShowConflictsTab(false);
  end
  else
  begin
    SetShowConflictsTab(true);
    FSearchBar.Caption := 'DPM : Project Group';
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

  if not IsProjectGroup then
    platformChangeDetectTimerTimer(platformChangeDetectTimer);

  //TODO : for project group configure with platforms enabled in project.

  ConfigureSearchBar;

  PackageDetailsView.Init(FContainer, FIconCache, FConfiguration, Self, projectOrGroup);
end;

procedure TDPMBaseEditViewFrame.ConfigureSearchBar;
begin
  FSearchBar.Configure(FLogger, FDPMIDEOptions, FConfiguration,FConfigurationManager, FSearchOptions.ConfigFile, FCurrentPlatform);
end;

constructor TDPMBaseEditViewFrame.Create(AOwner: TComponent);
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

  //IOTAIDEThemingServices added in 10.2
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

  FSearchOptions := TSearchOptions.Create;
  //hard code our compiler version here since when we are running in the IDE we are only working with the IDE version
  FSearchOptions.CompilerVersion := IDECompilerVersion;

  FSearchSkip := 0;
  FSearchTake := 0;
  FRowLayout.RowWidth := -1;

  FCurrentTab := TDPMCurrentTab.Installed;
  FCancelTokenSource := TCancellationTokenSourceFactory.Create;
  FRequestInFlight := false;
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;

  PackageDetailsView.Configure(FCurrentTab, FSearchBar.IncludePrerelease);

end;

procedure TDPMBaseEditViewFrame.CreateControls(AOwner: TComponent);
begin
  FButtonBar := TDPMButtonBar.Create(Self);
  FButtonBar.Top := 0;
  FButtonBar.OnTabChanged := Self.TabChanged;
  FButtonBar.ShowConflictsTab := false;
  FButtonBar.Parent := Self;
  FButtonBar.ParentBackground := false;
  FButtonBar.ParentColor := false;


  FSearchBar := TDPMSearchBarFrame.Create(Self);
  //important to make it appear below the button bar
  FSearchBar.Top := FButtonBar.Top + FButtonBar.Height;
  FSearchBar.OnSearch := Self.SearchBarOnSearch;
  FSearchBar.OnConfigChanged := Self.SearchBarSettingsChanged;
  FSearchBar.OnPlatformChanged := Self.SearchBarPlatformChanged;
  FSearchBar.Parent := Self;

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

function TDPMBaseEditViewFrame.CurrentList: IList<IPackageSearchResultItem>;
begin
  case FCurrentTab of
    TDPMCurrentTab.Installed : result := FInstalledPackages;
    TDPMCurrentTab.Updates : result := FUpdates;
    TDPMCurrentTab.Search : result := FSearchResultPackages;
    TDPMCurrentTab.Conflicts : result := FConflictPackages;
  end;
end;

destructor TDPMBaseEditViewFrame.Destroy;
begin
  FSearchOptions.Free;
  FIconCache.Free;
  if FLogger <> nil then
    FLogger.Debug('DPMIDE : View Destroying');
  inherited;
end;

procedure TDPMBaseEditViewFrame.DoPlatformChange(const newPlatform : TDPMPlatform);
begin
  if CurrentPlatform <> newPlatform then
  begin
    CurrentPlatform := newPlatform;
    FPackageReferences := DoGetPackageReferences;

    //TODO : need to do this more safely as it may interrup another operation.
    FInstalledPackages := nil;  //force refresh as we always need to update the installed packages.
    FAllInstalledPackages := nil;
    FSearchResultPackages := nil;
    FUpdates := nil;
    PackageDetailsView.SetPlatform(CurrentPlatform);
    PackageDetailsView.SetPackage(nil);
    SearchBar.SetPlatform(CurrentPlatform);
    FSearchOptions.Platforms := [CurrentPlatform];
    FScrollList.CurrentRow := -1;
    case CurrentTab of
      TDPMCurrentTab.Search : SwitchedToSearch(true);
      TDPMCurrentTab.Installed : SwitchedToInstalled(true);
      TDPMCurrentTab.Updates : SwitchedToUpdates(true);
      TDPMCurrentTab.Conflicts : SwitchedToConflicts(True);
    end;
  end;
end;

procedure TDPMBaseEditViewFrame.SearchBarOnSearch(const searchText: string; const searchOptions: TDPMSearchOptions; const source: string; const platform: TDPMPlatform; const refresh: boolean);
begin
  FCurrentPlatform := platform;

  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;

  FCancelTokenSource.Reset;

  if FSearchResultPackages <> nil then
    FSearchResultPackages.Clear;

  //this was from the edit right button click.. not sure it's still needed?
  if refresh and (searchText = '') then
  begin
    FSearchResultPackages := nil;
    FInstalledPackages := FAllInstalledPackages;
    FUpdates := nil;
  end;


  PackageDetailsView.IncludePreRelease := TDPMSearchOption.IncludePrerelease in searchOptions;

  FSearchOptions.Platforms := [platform];
  FSearchOptions.Prerelease := TDPMSearchOption.IncludePrerelease in searchOptions;
  FSearchOptions.Commercial := TDPMSearchOption.IncludeCommercial in searchOptions;
  FSearchOptions.Trial      := TDPMSearchOption.IncludeTrial in searchOptions;
  FSearchOptions.SearchTerms := Trim(searchText);
  if source <> 'All' then
    FSearchOptions.Sources := source
  else
    FSearchOptions.Sources := '';

  case FCurrentTab of
    TDPMCurrentTab.Search     : SwitchedToSearch(refresh);
    TDPMCurrentTab.Installed  : SwitchedToInstalled(refresh);
    TDPMCurrentTab.Updates    : SwitchedToUpdates(refresh);
    TDPMCurrentTab.Conflicts  : ;
  end;

end;

procedure TDPMBaseEditViewFrame.FilterAndLoadInstalledPackages(const searchTxt: string);
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

function TDPMBaseEditViewFrame.GetConflictingPackages: IAwaitable<IList<IPackageSearchResultItem>>;
var
  lSearchTerm : string;
  lProjectFile : string;
begin
  //local for capture
  lSearchTerm := FSearchOptions.SearchTerms;
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

function TDPMBaseEditViewFrame.GetCurrentPlatform: string;
begin
  //todo : change this.
  result := DPMPlatformToString(FCurrentPlatform);
end;

function TDPMBaseEditViewFrame.GetInstalledPackages: IAwaitable<IList<IPackageSearchResultItem>>;
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
//  options.Prerelease := FSearchOptions.Prerelease;
  options.Commercial := true;
  options.Trial := true;
//  options.SearchTerms := FSearchOptions.SearchTerms;

  if not IsProjectGroup then
    options.Platforms := [FCurrentPlatform];

  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      packageRef : IGraphNode;
      item : IPackageSearchResultItem;
    begin
      result := TCollections.CreateList<IPackageSearchResultItem>;
      FLogger.Debug('DPMIDE : Got Installed package references, fetching metadata...');
      result := repoManager.GetInstalledPackageFeed(cancelToken, options, GetPackageIdsFromReferences(FCurrentPlatform), FConfiguration);
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

    end, FCancelTokenSource.Token);
end;

function TDPMBaseEditViewFrame.GetPackageIdsFromReferences(const platform: TDPMPlatform): IList<IPackageId>;
var
  lookup : IDictionary<string, IPackageId>;
  packageRef : IGraphNode;
  existing : IPackageId;

  procedure AddPackageIds(const value : IGraphNode);
  var
    childRef : IGraphNode;
  begin
    if not (value.Platform = platform) then
      exit;

    if lookup.TryGetValue(Lowercase(value.Id), existing) then
    begin
      //add the highest version - with project groups there could be more than 1 version
      if existing.Version < value.Version then
        lookup[Lowercase(value.Id)] := value;
    end
    else
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

function TDPMBaseEditViewFrame.GetPackageReferences: IGraphNode;
begin
  result := FPackageReferences;
end;

function TDPMBaseEditViewFrame.GetSearchOptions: TSearchOptions;
begin
  result := FSearchOptions.Clone;
end;

function TDPMBaseEditViewFrame.GetUpdatedPackages: IAwaitable<IList<IPackageSearchResultItem>>;
begin
  result := TAsync.Configure < IList<IPackageSearchResultItem> > (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result := TCollections.CreateList<IPackageSearchResultItem>;

      //simulating long running.
      //WaitForSingleObject(cancelToken.Handle, 3000);
    end, FCancelTokenSource.Token);
end;

function TDPMBaseEditViewFrame.IsProjectGroup: boolean;
begin
  result := false;
end;

procedure TDPMBaseEditViewFrame.LoadList(const list: IList<IPackageSearchResultItem>);
begin
  if list = nil then
  begin
    FScrollList.RowCount := 0;
    exit;
  end;
  FScrollList.CurrentRow := -1;
  PackageDetailsView.SetPackage(nil);
  if FScrollList.RowCount = list.Count then
    FScrollList.Invalidate  //doing this because if the rowcount is the same it doesn't invalidate.
  else
    FScrollList.RowCount := list.Count;
  if list.Count > 0 then
    FScrollList.CurrentRow := 0;


end;

procedure TDPMBaseEditViewFrame.PackageInstalled(const package: IPackageSearchResultItem);
var
  packageSearchResult : IPackageSearchResultItem;
begin
  package.Installed := true;
  //package.InstalledVersion := package.Version;
  FAllInstalledPackages := nil;
  FInstalledPackages := nil;

  //if it's in the search results, update the installed version.
  if FSearchResultPackages <> nil then
  begin
    packageSearchResult := FSearchResultPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id,  package.Id) and (value.Version = package.Version);
      end);

    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := true;
      //packageSearchResult.InstalledVersion := package.Version;
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
        result := SameText(value.Id, package.Id) and (value.Version = package.Version);
      end);
    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := true;
      //packageSearchResult.InstalledVersion := package.Version;
    end
    else
      FAllInstalledPackages.Add(packageSearchResult);
  end;
  if FCurrentTab = TDPMCurrentTab.Installed then
    FilterAndLoadInstalledPackages(FSearchOptions.SearchTerms);

  //Tell the IDE to reload the project as we have just modified it on disk.
  FProject.Refresh(true);

  //force the project tree to update after installing package.
  FProjectTreeManager.NotifyEndLoading();
end;

procedure TDPMBaseEditViewFrame.PackageUninstalled(const package: IPackageSearchResultItem);
var
  packageSearchResult : IPackageSearchResultItem;
begin
  package.Installed := false;
  //package.InstalledVersion := '';

  //if it's in the search results, update the installed version.
  //might be nil if we haven't switched to the tab yet.
  if FSearchResultPackages <> nil then
  begin
    packageSearchResult := FSearchResultPackages.FirstOrDefault(
      function(const value : IPackageSearchResultItem) : boolean
      begin
        result := SameText(value.Id, package.Id) and (value.Version = package.Version);
      end);

    if packageSearchResult <> nil then
    begin
      packageSearchResult.Installed := false;
      //packageSearchResult.InstalledVersion := '';
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
      if FCurrentTab = TDPMCurrentTab.Installed then
        SwitchedToInstalled(False);
    end;
  end;

  //Tell the IDE to reload the project
  FProject.Refresh(true);
  //force the project tree to update after installing package.
  FProjectTreeManager.NotifyEndLoading();
end;

procedure TDPMBaseEditViewFrame.platformChangeDetectTimerTimer(Sender: TObject);
var
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
    DoPlatformChange(projectPlatform);
  end;
  platformChangeDetectTimer.Enabled := true;
end;

procedure TDPMBaseEditViewFrame.ProjectReloaded;
begin
  FLogger.Debug('DPMIDE : EditViewReloaded');
    FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  if not IsProjectGroup then
    platformChangeDetectTimerTimer(platformChangeDetectTimer)
  else
    SearchBarPlatformChanged(SearchBar.Platform);
end;

procedure TDPMBaseEditViewFrame.RequestPackageIcon(const index: integer; const package: IPackageSearchResultItem);
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
      result := repoManager.GetPackageIcon(cancelToken, source, id, version, IDECompilerVersion, platform, FConfiguration);
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

procedure TDPMBaseEditViewFrame.InstallStarting;
begin
  //save before making any changes.
  (BorlandIDEServices as IOTAModuleServices).SaveAll;
end;

procedure TDPMBaseEditViewFrame.ScrollListChangeRow(const Sender: TObject; const newRowIndex: Int64; const direction: TScrollDirection; const delta: Int64);
var
  item : IPackageSearchResultItem;
  list : IList<IPackageSearchResultItem>;
begin
  list := CurrentList;
  if list = nil then
  begin
    PackageDetailsView.SetPackage(nil);
    exit;
  end;

  item := nil;
  if (newRowIndex >= 0) and (newRowIndex < list.Count) then
    item := list[newRowIndex];

  PackageDetailsView.SetPackage(item);
end;

procedure TDPMBaseEditViewFrame.ScrollListPaintNoRows(const Sender: TObject; const ACanvas: TCanvas; const paintRect: TRect);
var
  backgroundColor : TColor;
begin
  ACanvas.Font.Assign(Self.Font);
  ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
  backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);

  ACanvas.Font.Color := backgroundColor;
  if FRequestInFlight then
    ACanvas.TextOut(20, 20, 'Loading....')
  else
  begin
    if FSearchBar.HasSources then
      ACanvas.TextOut(20, 20, 'No Packages found')
    else
      ACanvas.TextOut(20, 20, 'No enabled package sources, add a package source in DPM settings.')
  end;
end;

procedure TDPMBaseEditViewFrame.ScrollListPaintRow(const Sender: TObject; const ACanvas: TCanvas; const itemRect: TRect; const index: Int64; const state: TPaintRowState);
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
      backgroundColor := $00FFF0E9;
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

    //TODO : this all feels super hacky, revisit when IDE supports high dpi/scaling.

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
        //DPMEditorViewImages.Draw(ACanvas, reservedRect.Left, reservedRect.Top, 4, TDrawingStyle.dsTransparent, TImageType.itImage);
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
//    if item.Installed and (item.Version <> item.InstalledVersion) then
//      title := item.InstalledVersion
//    else
    title := item.Version;
    ACanvas.TextRect(FRowLayout.VersionRect, title, [tfSingleLine, tfVerticalCenter, tfRight]);

    if item.Installed and (item.Version <> item.LatestVersion) then
    begin
      title := item.LatestVersion;
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

procedure TDPMBaseEditViewFrame.SearchBarPlatformChanged(const newPlatform: TDPMPlatform);
begin
  if IsProjectGroup then
    DoPlatformChange(newPlatform);
end;

function TDPMBaseEditViewFrame.SearchForPackages(const options: TSearchOptions): IList<IPackageSearchResultItem>;
var
  repoManager : IPackageRepositoryManager;
begin
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  result := repoManager.GetPackageFeed(FCancelTokenSource.Token, options, FConfiguration);
end;

function TDPMBaseEditViewFrame.SearchForPackagesAsync(const options: TSearchOptions): IAwaitable<IList<IPackageSearchResultItem>>;
var
  repoManager : IPackageRepositoryManager;
begin
  //local for capture
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;

  result := TAsync.Configure <IList<IPackageSearchResultItem>> (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      result := repoManager.GetPackageFeed(cancelToken, options, FConfiguration);
      //simulating long running.
    end, FCancelTokenSource.Token);
end;

procedure TDPMBaseEditViewFrame.SetShowConflictsTab(const value: boolean);
begin
  FButtonBar.ShowConflictsTab := value;
end;

procedure TDPMBaseEditViewFrame.SearchBarSettingsChanged(const configuration: IConfiguration);
begin
  FConfiguration := configuration;
  PackageDetailsView.Init(FContainer, FIconCache, FConfiguration, Self, FProject);
end;

procedure TDPMBaseEditViewFrame.SwitchedToConflicts(const refresh: boolean);
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


type
  TFilterProc = procedure(const searchTxt : string) of object;

procedure TDPMBaseEditViewFrame.SwitchedToInstalled(const refresh: boolean);
var
  searchTxt : string;
  filterProc : TFilterProc;
  //filter the list to only show directly installed packages, and on the search term
begin
  FLogger.Debug('DPMIDE : SwitchToInstalled');
  searchTxt := FSearchOptions.SearchTerms;
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

procedure TDPMBaseEditViewFrame.SwitchedToSearch(const refresh: boolean);
begin
  if (not refresh) and (FSearchResultPackages <> nil) and FSearchResultPackages.Any then
    LoadList(FSearchResultPackages)
  else
  begin
    FRequestInFlight := true;
    FCancelTokenSource.Reset;
    FLogger.Debug('DPMIDE : Searching for Packages..');
    SearchForPackagesAsync(FSearchOptions)
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
          begin
            item.LatestVersion := item.Version;
            item.Version := packageRef.Version.ToStringNoMeta;
          end;
        end;

        FScrollList.RowCount := FSearchResultPackages.Count;
        LoadList(FSearchResultPackages);
      end);
  end;
end;

procedure TDPMBaseEditViewFrame.SwitchedToUpdates(const refresh: boolean);
begin
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

procedure TDPMBaseEditViewFrame.SwitchTabs(const currentTab: TDPMCurrentTab; const refresh: boolean);
var
  doRefresh : boolean;
begin
  if FRequestInFlight then
    FCancelTokenSource.Cancel;

  while FRequestInFlight do
    Application.ProcessMessages;
  FCancelTokenSource.Reset;


  FCurrentTab := currentTab;
  FSearchBar.ConfigureForTab(FCurrentTab);

  FScrollList.RowCount := 0;
  PackageDetailsView.SetPackage(nil);
  PackageDetailsView.Configure(FCurrentTab, FSearchBar.IncludePrerelease);

  doRefresh := refresh or (FSearchOptions.SearchTerms <> '');
  FSearchOptions.Platforms := [FCurrentPlatform];
  FSearchOptions.Prerelease := FSearchBar.IncludePrerelease;

  case FCurrentTab of
    TDPMCurrentTab.Search : SwitchedToSearch(doRefresh);
    TDPMCurrentTab.Installed : SwitchedToInstalled(doRefresh);
    TDPMCurrentTab.Updates : SwitchedToUpdates(doRefresh);
    TDPMCurrentTab.Conflicts : SwitchedToConflicts(doRefresh);
  end;
end;

procedure TDPMBaseEditViewFrame.TabChanged(const tab: TDPMCurrentTab);
begin
  SwitchTabs(tab, FSearchOptions.SearchTerms <> '');
end;

procedure TDPMBaseEditViewFrame.ThemeChanged;
{$IF CompilerVersion >=32.0}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$IFEND}
begin
  {$IF CompilerVersion >=32.0}
  ideThemeSvc := (BorlandIDEServices as IOTAIDEThemingServices);
  ideThemeSvc.ApplyTheme(Self);
//  ideThemeSvc.ApplyTheme(FButtonBar);
//  ideThemeSvc.ApplyTheme(FSearchBar);
//  ideThemeSvc.ApplyTheme(FSearchBar.chkIncludePrerelease);
//  ideThemeSvc.ApplyTheme(Splitter2);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$IFEND}

  Self.Color := FIDEStyleServices.GetSystemColor(clWindow);

  FButtonBar.ThemeChanged(FIDEStyleServices);
  FSearchBar.ThemeChanged(FIDEStyleServices);

  Splitter2.Color := FIDEStyleServices.GetSystemColor(clBtnFace);

  FScrollList.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FScrollList.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  PackageDetailsView.ThemeChanged(FIDEStyleServices {$IFDEF THEMESERVICES}, ideThemeSvc {$ENDIF}) ;
end;

procedure TDPMBaseEditViewFrame.ViewDeselected;
begin
  // The view tab was deselected.
  FLogger.Debug('DPMIDE : View Deselected');
  platformChangeDetectTimer.Enabled := false;
  FFirstView := true;
end;

procedure TDPMBaseEditViewFrame.ViewSelected;
begin
  FLogger.Debug('DPMIDE : View Selected');
  //For some reason this get's called twice for each time the view is selected.
  if not IsProjectGroup then
    platformChangeDetectTimer.Enabled := true;
  //The first time the view is opened we want to switch to the installed packages tab
  if FFirstView then
  begin
    FFirstView := false;
    if IsProjectGroup then
      DoPlatformChange(SearchBar.Platform)
    else
      platformChangeDetectTimerTimer(platformChangeDetectTimer);
  end;
end;

{ TRowLayout }

procedure TRowLayout.Update(const ACanvas: TCanvas; const rowRect: TRect; const showSelect: Boolean);
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
