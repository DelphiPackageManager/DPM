unit DPM.IDE.EditorViewFrame;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.SyncObjs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, SVGInterfaces,
  Vcl.Themes,
  DPM.Controls.ButtonedEdit,
  VSoftVirtualListView,
  ToolsApi,
  Spring.Collections,
  Spring.Container,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.IDE.Types,
  DPM.IDE.IconCache,
  DPM.IDE.Options,
  DPM.IDE.Logger,
  DPM.IDE.ProjectTreeManager,
  DPM.Controls.VersionGrid,
  DPM.IDE.SearchBarFrame,
  DPM.IDE.Details.Interfaces,
  DPM.IDE.ActivityIndicator,
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  {$IF CompilerVersion >= 30.0 }
  System.ImageList,
  {$IFEND}
  Vcl.ActnList,
  DPM.IDE.PackageDetailsFrame;


{$I '..\DPMIDE.inc'}


const
  bulletChar : Char = '•';


type
  TPackageRowKind = (rkInstalledHeader, rkImplicitHeader, rkAvailableHeader,rkInstalledPackage, rkImplicitPackage, rkAvailablePackage, rkUnknown);

  TRowLayout = record
    IconRect : TRect;
    TitleRect : TRect;
    LatestVersionRect : TRect;
    IconSize : integer;
    Margin : integer;
    procedure Update(const ACanvas : TCanvas; const rowRect : TRect; const rowKind : TPackageRowKind);
    constructor Create(const margin : integer; const iconSize : integer);
  end;



  TDPMEditViewFrame = class(TFrame, IDetailsHost)
    ContentPanel : TPanel;
    Splitter2 : TSplitter;
    PackageListPanel : TPanel;
    DetailPanel: TPanel;
    platformChangeDetectTimer: TTimer;
    PackageDetailsFrame: TPackageDetailsFrame;
    ActivityTimer: TTimer;
    DebounceTimer: TTimer;
    procedure platformChangeDetectTimerTimer(Sender: TObject);
    procedure ActivityTimerTimer(Sender: TObject);
    procedure DebounceTimerTimer(Sender: TObject);
  private
    FIDEStyleServices : TCustomStyleServices;

    //controls
    FSearchBar : TDPMSearchBarFrame;
    FScrollList : TVSoftVirtualListView;
    //controls

    //contains layout rects for the list view
    FRowLayout : TRowLayout;

    FInstalledHeaderRowIdx : Int64;
    FImplicitHeaderRowIdx : Int64;
    FAvailableHeaderRowIdx : Int64;


    //dpm core stuff
    FContainer : TContainer;
    FLogger : IDPMIDELogger;
    FProjectTreeManager : IDPMProjectTreeManager;
    FConfigurationManager : IConfigurationManager;
    FConfiguration : IConfiguration;
    FConfigIsLocal : boolean;

    //request stuff
    FCancelTokenSource : ICancellationTokenSource;
    FRequestsInFlight : integer;
    FClosing : boolean;

    //Project group should never be nil?
    FProjectGroup : IOTAProjectGroup;
    FProject : IOTAProject;
    FSelectedProject : string;


    FIconCache : TDPMIconCache;
    FDPMIDEOptions : IDPMIDEOptions;
    FCurrentPlatform : TDPMPlatform;

    FSearchOptions : TSearchOptions;
    FSearchSkip : integer;
    FSearchTake : integer;

    FPackageReferences : IPackageReference;

    FRowCount : Int64;
    //all installed packages, direct and transitive
    FAllInstalledPackages : IList<IPackageSearchResultItem>;
    //directly installed packages, might be fitered.
    FInstalledPackages : IList<IPackageSearchResultItem>;
    //implicitly installled
    FImplicitPackages : IList<IPackageSearchResultItem>;
    //packages found by searching
    FAvailablePackages : IList<IPackageSearchResultItem>;

    FCurrentPackage : IPackageSearchResultItem;

    FInstalledActivity : TActivityIndicator;
    FImplicitActivity  : TActivityIndicator;
    FAvailableActivity : TActivityIndicator;


    //true when we first load the view
    FFirstView : boolean;

    FLock : TCriticalSection;
  protected
    procedure CheckTimerEnabled;

    //IDetailsHost
    procedure SaveBeforeInstall;
    procedure PackageInstalled(const package : IPackageSearchResultItem; const isUpdate : boolean);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
    function GetPackageReferences : IPackageReference;
    function GetPackageIdsFromReferences(const platform: TDPMPlatform): IList<IPackageId>;
    procedure RequestPackageIcon(const index : integer; const package : IPackageSearchResultItem);

    function GetInstalledPackagesAsync: IAwaitable<IList<IPackageSearchResultItem>>;
    function SearchForPackagesAsync(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
    procedure FilterInstalledPackages(const searchTxt: string);

    function GetInstalledCount : Int64;
    function GetImplicitCount : Int64;
    function GetAvailableCount : Int64;

    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);virtual;
    procedure ConfigureSearchBar;

    function GetPlatforms : TDPMPlatforms;

    //callbacks from the searchbar
    procedure SearchBarSettingsChanged(const configuration : IConfiguration);
    procedure SearchBarPlatformChanged(const newPlatform : TDPMPlatform);
    procedure SearchBarOnSearch(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const platform : TDPMPlatform; const refresh : boolean);
    procedure SearchBarProjectSelected(const projectFile : string);
    procedure SearchBarOnFocustList(sender : TObject);

    //scrolllist evernts
    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);
    procedure ScrollListBeforeChangeRow(const Sender : TObject; const currentRowIndex : Int64; const direction : TScrollDirection; const delta : Int64; var newRowIndex : Int64);


    procedure DoPlatformChange(const newPlatform : TDPMPlatform; const refresh : boolean);

    function GetRowKind(const index : Int64) : TPackageRowKind;
    procedure CalculateIndexes;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Configure(const projectGroup : IOTAProjectGroup; const project : IOTAProject; const container : TContainer; const projectTreeManager : IDPMProjectTreeManager);virtual;
    procedure ViewSelected;
    procedure ViewDeselected;
    procedure Closing;
    procedure ProjectChanged;
    procedure ProjectClosed(const projectName : string);
    procedure ProjectLoaded(const projectName : string);
    procedure ThemeChanged;
    function CanCloseView : boolean;

  end;

implementation

{$R *.dfm}

uses
  System.Diagnostics,
  WinApi.ActiveX,
  DPM.Core.Constants,
  DPM.Core.Options.Common,
  DPM.Core.Utils.Config,
  DPM.Core.Utils.Numbers,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Package.Icon,
  DPM.Core.Package.SearchResults,
  DPM.Core.Repository.Interfaces,
  DPM.IDE.ToolsAPI,
  DPM.IDE.AboutForm,
  DPM.IDE.AddInOptionsHostForm,
  DPM.Core.Dependency.Graph;


type
  TFilterProc = procedure(const searchTxt : string) of object;



function FindPackageRef(const references : IPackageReference; const platform : TDPMPlatform; const searchItem : IPackageSearchResultItem; const topLevelOnly : boolean) : IPackageReference;
var
  ref : IPackageReference;
begin
  result := nil;
  if (references = nil) or (not references.HasDependencies) then
    exit;

  //breadth first search!
  for ref in references.Dependencies do
  begin
    if ref.Platform <> platform then
      continue;

    if SameText(ref.Id, searchItem.Id) then
      Exit(ref);
  end;
  if topLevelOnly then
    exit;

  for ref in references.Dependencies do
  begin
    if ref.Platform <> platform then
      continue;
    //depth search
    if ref.HasDependencies then
    begin
      result := FindPackageRef(ref, platform, searchItem, false);
      if result <> nil then
        Exit(result);
    end;
  end;
end;


{ TDPMEditViewFrame2 }

procedure TDPMEditViewFrame.ActivityTimerTimer(Sender: TObject);
begin
  ActivityTimer.Enabled := false;
  if FInstalledActivity.IsActive then
  begin
    FInstalledActivity.Step;
    FScrollList.InvalidateRow(FInstalledHeaderRowIdx);
  end;
  if FImplicitActivity.IsActive then
  begin
    FImplicitActivity.Step;
    FScrollList.InvalidateRow(FImplicitHeaderRowIdx);
  end;
  if FAvailableActivity.IsActive then
  begin
    FAvailableActivity.Step;
    FScrollList.InvalidateRow(FAvailableHeaderRowIdx);
  end;
  ActivityTimer.Enabled := FInstalledActivity.IsActive or FImplicitActivity.IsActive or FAvailableActivity.IsActive;
end;

procedure TDPMEditViewFrame.CalculateIndexes;
var
  installedCount : Int64;
  implicitCount : Int64;
  availableCount : Int64;
begin
  FInstalledHeaderRowIdx := 0;
  installedCount := GetInstalledCount;
  implicitCount := GetImplicitCount;
  availableCount := GetAvailableCount;

  FImplicitHeaderRowIdx := FInstalledHeaderRowIdx + installedCount + 1;
  FAvailableHeaderRowIdx := FImplicitHeaderRowIdx + implicitCount + 1;

  FRowCount := 1 + installedCount;
  FRowCount := FRowCount + 1 + implicitCount;
  FRowCount := FRowCount + 1 + availableCount;
  FScrollList.RowCount := FRowCount;
end;

function TDPMEditViewFrame.CanCloseView: boolean;
begin
  result := true; //TODO : Block closing while busy installing/removing packages.
end;

procedure TDPMEditViewFrame.ChangeScale(M, D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND});
begin
  FRowLayout.IconSize := MulDiv(FRowLayout.IconSize, M, D);
  FRowLayout.Margin := MulDiv(FRowLayout.Margin, M, D);
  inherited;
end;

procedure TDPMEditViewFrame.CheckTimerEnabled;
begin

end;

procedure TDPMEditViewFrame.Closing;
begin
  FClosing := true;
  PackageDetailsFrame.ViewClosing;
  //cancel any pending requests asap. Needs to return quickly or the IDE will hang.
  FCancelTokenSource.Cancel;
  //allow the cancellation to happen.
  //if we don't do this we will get an excepion in the await or cancellation callbacks
  while FRequestsInFlight > 0 do
    Application.ProcessMessages;

end;

procedure TDPMEditViewFrame.Configure(const projectGroup : IOTAProjectGroup; const project : IOTAProject; const container: TContainer;  const projectTreeManager: IDPMProjectTreeManager);
var
  sConfigFile : string;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;
  FProjectGroup := projectGroup;
  FProject := project;
  if project <> nil then
    FSelectedProject := project.FileName
  else
    FSelectedProject := '';

  FLogger := FContainer.Resolve<IDPMIDELogger>;
  FDPMIDEOptions := FContainer.Resolve<IDPMIDEOptions>;
  //if there is a project specific config file then that is what we should use.
  sConfigFile := IncludeTrailingPathDelimiter(ExtractFilePath(projectGroup.FileName)) + cDPMConfigFileName;
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

  ConfigureSearchBar;

  PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, projectGroup);

  //testing
  CalculateIndexes;
end;

procedure TDPMEditViewFrame.ConfigureSearchBar;
var
  platforms : TDPMPlatforms;
begin
  platforms := GetPlatforms;
  FSearchBar.Configure(FLogger, FDPMIDEOptions, FConfiguration, FConfigurationManager, FSearchOptions.ConfigFile, platforms);
end;

constructor TDPMEditViewFrame.Create(AOwner: TComponent);
{$IFDEF THEMESERVICES}
var
  ideThemeSvc : IOTAIDEThemingServices;
  {$ENDIF}
begin
  inherited;
  FIconCache := TDPMIconCache.Create;
  FRowLayout := TRowLayout.Create(10, 24);
  FLock := TCriticalSection.Create;
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

  //reset the activity indicators
  FInstalledActivity.Stop;
  FImplicitActivity.Stop;
  FAvailableActivity.Stop;

  FAllInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FImplicitPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FAvailablePackages := TCollections.CreateList<IPackageSearchResultItem>;

  CreateControls(AOwner);
  ThemeChanged;

  FFirstView := true;

  FSearchOptions := TSearchOptions.Create;
  //hard code our compiler version here since when we are running in the IDE we are only working with the IDE version
  FSearchOptions.CompilerVersion := IDECompilerVersion;

  FSearchSkip := 0;
  FSearchTake := 0;

  FCancelTokenSource := TCancellationTokenSourceFactory.Create;
  FRequestsInFlight := 0;
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;


end;

procedure TDPMEditViewFrame.CreateControls(AOwner: TComponent);
begin


  FSearchBar := TDPMSearchBarFrame.Create(Self);
  //important to make it appear below the button bar
  FSearchBar.Top := 0;
  FSearchBar.OnSearch := Self.SearchBarOnSearch;
  FSearchBar.OnConfigChanged := Self.SearchBarSettingsChanged;
  FSearchBar.OnPlatformChanged := Self.SearchBarPlatformChanged;
  FSearchBar.OnProjectSelected := Self.SearchBarProjectSelected;
  FSearchBar.OnFocusList := Self.SearchBarOnFocustList;
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
  FScrollList.RowHeight := 32; //the control will scale this for dpi
  FScrollList.RowCount := 0;
  FScrollList.OnPaintRow := Self.ScrollListPaintRow;
  FScrollList.OnPaintNoRows := Self.ScrollListPaintNoRows;
  FScrollList.OnRowChange := Self.ScrollListChangeRow;
  FScrollList.OnBeforeRowChangeEvent := Self.ScrollListBeforeChangeRow;


  FScrollList.Constraints.MinWidth := 400;
  FScrollList.DoubleBuffered := false;
  FScrollList.ParentDoubleBuffered := false;
  FScrollList.ParentBackground := false;
  FScrollList.ParentColor := false;

  FScrollList.Parent := PackageListPanel;



end;

procedure TDPMEditViewFrame.DebounceTimerTimer(Sender: TObject);
begin
  DebounceTimer.Enabled := false;
  PackageDetailsFrame.SetPackage(FCurrentPackage, FSearchOptions.Prerelease, true);
end;

destructor TDPMEditViewFrame.Destroy;
begin
  FSearchOptions.Free;
  FIconCache.Free;
  if FLogger <> nil then
    FLogger.Debug('DPMIDE : View Destroying');

  FLock.Free;
  inherited;
end;


procedure TDPMEditViewFrame.FilterInstalledPackages(const searchTxt: string);
var
  installed : IList<IPackageSearchResultItem>;
  implicit : IList<IPackageSearchResultItem>;
begin
  installed := TCollections.CreateList<IPackageSearchResultItem>(FAllInstalledPackages.Where(
    function(const pkg : IPackageSearchResultItem) : boolean
    begin
      result := not pkg.IsTransitive;
      if result and (searchTxt <> '') then
      begin
        result := TStringUtils.Contains(pkg.Id, searchTxt, true);
      end;
    end));
  implicit := TCollections.CreateList<IPackageSearchResultItem>(FAllInstalledPackages.Where(
    function(const pkg : IPackageSearchResultItem) : boolean
    begin
      result := pkg.IsTransitive;
      if result and (searchTxt <> '') then
      begin
        result := TStringUtils.Contains(pkg.Id, searchTxt, true);
      end;
    end));
    FInstalledPackages := installed;
    FImplicitPackages := implicit;

//  LoadList(FInstalledPackages);
end;



procedure TDPMEditViewFrame.DoPlatformChange(const newPlatform: TDPMPlatform; const refresh : boolean);
var
  searchTxt : string;
  filterProc : TFilterProc;
begin
  //note refresh only ever applies to the installed packages, we always fetch the available packages.
  FLogger.Debug('DPMIDE : DoPlatformChange');
  TSystemUtils.OutputDebugString('TDPMEditViewFrame2.DoPlatformChange');
  if FCurrentPlatform <> newPlatform then
  begin
    FCurrentPlatform := newPlatform;
    FPackageReferences := GetPackageReferences;
    FScrollList.CurrentRow := -1;

    PackageDetailsFrame.SetPlatform(FCurrentPlatform);
    //TODO : Can we maintain the currently selected package?
    FSearchBar.SetPlatform(FCurrentPlatform);
    FSearchOptions.Platforms := [FCurrentPlatform];
    filterProc := FilterInstalledPackages;
    searchTxt := FSearchOptions.SearchTerms;
    FLogger.Debug('DPMIDE : Getting Installed Packages..');
    FInstalledActivity.Step;
    FScrollList.InvalidateRow(FInstalledHeaderRowIdx);
    FImplicitActivity.Step;
    FScrollList.InvalidateRow(FImplicitHeaderRowIdx);
    ActivityTimer.Enabled := true;
//    if FRequestsInFlight > 0 then
//      FCancelTokenSource.Cancel;

    while FRequestsInFlight > 0 do
      Application.ProcessMessages;

    FCancelTokenSource.Reset;



    if (not refresh) and (FAllInstalledPackages <> nil) and (FAllInstalledPackages.Count > 0) then
    begin
      FLock.Acquire;
      try
        FilterInstalledPackages(searchTxt);
        Dec(FRequestsInFlight);
        CalculateIndexes;
        FInstalledActivity.Stop;
        FImplicitActivity.Stop;
        FScrollList.Invalidate;
      finally
        FLock.Release;
      end;
    end
    else
    GetInstalledPackagesAsync
    .OnException(
      procedure(const e : Exception)
      begin
        Dec(FRequestsInFlight);
        if FClosing then
          exit;
        FLogger.Error(e.Message);
      end)
    .OnCancellation(
      procedure
      begin
        Dec(FRequestsInFlight);
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Cancelled getting installed packages.');
      end)
    .Await(
      procedure(const theResult : IList<IPackageSearchResultItem>)
      begin
        Dec(FRequestsInFlight);
        FInstalledActivity.Stop;
        FImplicitActivity.Stop;

        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got installed packages.');

        FLock.Acquire;
        try
          FAllInstalledPackages := theResult;
          filterProc(searchTxt);
          CalculateIndexes;
          FScrollList.Invalidate;
        finally
          FLock.Release;
        end;
      end);
    Inc(FRequestsInFlight);

    FAvailableActivity.Step;
    FScrollList.InvalidateRow(FAvailableHeaderRowIdx);
    ActivityTimer.Enabled := true;
//    FSearchOptions.Take := 3;
    SearchForPackagesAsync(FSearchOptions)
    .OnException(
      procedure(const e : Exception)
      begin
        Dec(FRequestsInFlight);
        if FClosing then
          exit;
        FLogger.Error(e.Message);
      end)
    .OnCancellation(
      procedure
      begin
        Dec(FRequestsInFlight);
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
        toRemove : IList<IPackageSearchResultItem>;
      begin
        Dec(FRequestsInFlight);
        //if the view is closing do not do anything else.
        if FClosing then
          exit;
        FLogger.Debug('DPMIDE : Got search results.');
        FAvailableActivity.Stop;
        toRemove := TCollections.CreateList<IPackageSearchResultItem>;
        //some of the available packages may already be installed, so we need to check for that.
        for item in theResult do
        begin
          packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item, false);
          if packageRef <> nil then
            toRemove.Add(item);
        end;
        if toRemove.Any then
          theResult.RemoveRange(toRemove);

        FLock.Acquire;
        try
          FAvailablePackages := theResult;
          CalculateIndexes;
          FScrollList.Invalidate;
        finally
          FLock.Release;
        end;


      end);
      Inc(FRequestsInFlight);

  end;

end;


function TDPMEditViewFrame.GetPackageIdsFromReferences(const platform: TDPMPlatform): IList<IPackageId>;
var
  lookup : IDictionary<string, IPackageId>;
  packageRef : IPackageReference;

  procedure AddPackageIds(const value : IPackageReference);
  var
    childRef : IPackageReference;
    existing : IPackageId;
  begin
    if not (value.Platform = platform) then
      exit;

    if (value.Id <> cRootNode ) then
    begin
      if lookup.TryGetValue(Lowercase(value.Id), existing) then
      begin
        //add the highest version - with project groups there could be more than 1 version
        if existing.Version < value.Version then
          lookup[Lowercase(value.Id)] := value;
      end
      else
        lookup[Lowercase(value.Id)] := value;
    end;

    for childRef in value.Dependencies do
      AddPackageIds(childRef);
  end;

begin
  lookup := TCollections.CreateDictionary < string, IPackageId > ;
  result := TCollections.CreateList<IPackageId>;
  if FPackageReferences <> nil then
  begin
    for packageRef in FPackageReferences.Dependencies do
    begin
      AddPackageIds(packageRef);
    end;
    result.AddRange(lookup.Values);
  end;
end;


function TDPMEditViewFrame.GetPackageReferences: IPackageReference;
var
  projectEditor : IProjectEditor;
  proj : IOTAProject;
  i : integer;
  packageReference : IPackageReference;
begin
  projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
  result := TPackageReference.CreateRoot(IDECompilerVersion, FCurrentPlatform);
  Assert(FProjectGroup <> nil);
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    proj := FProjectGroup.Projects[i];
    projectEditor.LoadProject(proj.FileName);
    packageReference := projectEditor.GetPackageReferences(FCurrentPlatform); //NOTE : Can return nil. Will change internals to return empty root node.
    if packageReference <> nil then
      Result.AddExistingReference(LowerCase(proj.FileName), packageReference);
  end;
end;

function TDPMEditViewFrame.GetPlatforms: TDPMPlatforms;
var
  i : integer;
  projectEditor : IProjectEditor;
begin
  result := [];
  //it would be nice to use the open tools api to do this, but so far
  // - Project.SupportedPlatforms returns all platforms, whether they are enabled for the project or not
  // - BuildConfig.Platforms is empty.

  //doing it this way is not ideal, as it requires that the project has been saved after a platform was added.
  projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    if FProjectGroup.Projects[i].FileName <> '' then
    begin
      projectEditor.LoadProject(FProjectGroup.Projects[i].FileName, [TProjectElement.Platforms]);
      result := result + projectEditor.Platforms;
    end
    else
    begin
      //what can we do? unsaved project, will probably have a name so this shouldn't happen?
    end;
  end;

end;

function TDPMEditViewFrame.GetRowKind(const index : Int64): TPackageRowKind;
begin
  if index = 0 then
    result := rkInstalledHeader //row 0 is always installed header
  else if  (index = FImplicitHeaderRowIdx) then
    result := rkImplicitHeader
  else if (index = FAvailableHeaderRowIdx) then
    result := rkAvailableHeader
  else if (index > FInstalledHeaderRowIdx) and (index < FImplicitHeaderRowIdx) then
    result := rkInstalledPackage
  else if (index > FImplicitHeaderRowIdx) and  (index < FAvailableHeaderRowIdx) then
    result := rkImplicitPackage
  else if index > FAvailableHeaderRowIdx then
    result := rkAvailablePackage
  else
    result := rkUnknown;
end;

function TDPMEditViewFrame.GetAvailableCount: Int64;
begin
  if FAvailablePackages <> nil then
    result := FAvailablePackages.Count
  else
    result := 0;
end;

procedure TDPMEditViewFrame.PackageInstalled(const package: IPackageSearchResultItem; const isUpdate: boolean);
//var
//  platform : TDPMPlatform;
begin
  //Tell the IDE to reload the project as we have just modified it on disk.
  FProjectGroup.Refresh(false);
  //force the project tree to update after installing package.
  //FProjectTreeManager.EndLoading();
//  platform := FCurrentPlatform;
//  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  FLogger.Debug('DPMIDE : PackageInstalled');
//  DoPlatformChange(platform, true);
//  PackageDetailsFrame.ProjectReloaded;
end;

procedure TDPMEditViewFrame.PackageUninstalled(const package: IPackageSearchResultItem);
//var
//  platform : TDPMPlatform;
begin
  //Tell the IDE to reload the project as we have just modified it on disk.
  FProjectGroup.Refresh(false);
  //force the project tree to update after installing package.
  //FProjectTreeManager.EndLoading();
//  platform := FCurrentPlatform;
//  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  FLogger.Debug('DPMIDE : PackageUnInstalled');
  //DoPlatformChange(platform, true);
//  PackageDetailsFrame.ProjectReloaded;
end;

procedure TDPMEditViewFrame.platformChangeDetectTimerTimer(Sender: TObject);
var
  projectPlatform : TDPMPlatform;
  project : IOTAProject;
begin
  // since the tools api provides no notifications about active platform change
  // we have to resort to this ugly hack.
  platformChangeDetectTimer.Enabled := false;
  if FSelectedProject <> '' then
  begin
    project := TToolsApiUtils.FindProjectInGroup(FProjectGroup, FSelectedProject);
    if project <> nil then
    begin
      projectPlatform := ProjectPlatformToDPMPlatform(project.CurrentPlatform);
      if projectPlatform = TDPMPlatform.UnknownPlatform then
        raise Exception.Create('FProject.CurrentPlatform : ' + project.CurrentPlatform);
      FLogger.Debug('DPMIDE : platformChangeDetectTimerTimer');

//      DoPlatformChange(projectPlatform, true);
    end;
  end;
  //platformChangeDetectTimer.Enabled := true;
end;

procedure TDPMEditViewFrame.ProjectChanged;
var
  platforms : TDPMPlatforms;
begin
  FLogger.Debug('DPMIDE : ProjectChanged');
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  PackageDetailsFrame.ProjectReloaded;

  //need to update searchbar platforms etc.

  platforms := GetPlatforms;
  FSearchBar.UpdatePlatforms(platforms);

  DoPlatformChange(FSearchBar.Platform, true);

end;

procedure TDPMEditViewFrame.ProjectClosed(const projectName: string);
var
  platforms : TDPMPlatforms;
begin
  TSystemUtils.OutputDebugString('TDPMEditViewFrame2.ProjectClosed : ' + projectName);
  FLogger.Debug('DPMIDE : ProjectClosed');
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  PackageDetailsFrame.ProjectReloaded;

  //need to update searchbar platforms etc.

  platforms := GetPlatforms;
  FSearchBar.UpdatePlatforms(platforms);
//  DoPlatformChange(FSearchBar.Platform, true);
end;

procedure TDPMEditViewFrame.ProjectLoaded(const projectName: string);
begin
  TSystemUtils.OutputDebugString('TDPMEditViewFrame2.ProjectLoaded : ' + projectName);
end;



function TDPMEditViewFrame.GetImplicitCount: Int64;
begin
  if FImplicitPackages <> nil then
    result := FImplicitPackages.Count
  else
    result := 0;
end;

function TDPMEditViewFrame.GetInstalledCount: Int64;
begin
  if FInstalledPackages <> nil then
    result := FInstalledPackages.Count
  else
    result := 0;

end;

function TDPMEditViewFrame.GetInstalledPackagesAsync: IAwaitable<IList<IPackageSearchResultItem>>;
var
  lProjectFile : string;
  repoManager : IPackageRepositoryManager;
  options : TSearchOptions;
begin
  //local for capture
  lProjectFile := FProjectGroup.FileName;

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  repoManager.Initialize(FConfiguration);

  options := FSearchOptions.Clone;
  //we want all packages for installed as we don't know what types we might have
  options.Prerelease := true;
  options.Commercial := true;
  options.Trial := true;

  options.Platforms := [FCurrentPlatform];

  result := TAsync.Configure<IList<IPackageSearchResultItem>> (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      packageRef : IPackageReference;
      item : IPackageSearchResultItem;
      packageIds : IList<IPackageId>;
    begin
      CoInitialize(nil);
      try
        result := TCollections.CreateList<IPackageSearchResultItem>;
        FLogger.Debug('DPMIDE : Got Installed package references, fetching metadata...');
        packageIds := GetPackageIdsFromReferences(FCurrentPlatform);
        result := repoManager.GetInstalledPackageFeed(cancelToken, options, packageIds);
        for item in result do
        begin
          if FPackageReferences <> nil then
          begin
            packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item, true);
            if packageRef <> nil then //top level reference
            begin
              item.IsTransitive := false;
            end
            else //not found as a top level reference
            begin
              packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item, false);
              if packageRef <> nil then
                item.IsTransitive := packageRef.IsTransitive;
            end;
            if packageRef <> nil then
            begin
              item.Version := packageRef.Version;
              item.Installed := true;
            end;
          end;
        end;
        result.Sort(function(const Left, Right : IPackageSearchResultItem) : Integer
          begin
            result := CompareStr(Left.Id, Right.Id);
          end);


        FLogger.Debug('DPMIDE : Got Installed package metadata.');
      finally
        CoUninitialize;
      end;

    end, FCancelTokenSource.Token);
end;

function TDPMEditViewFrame.SearchForPackagesAsync(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;
var
  repoManager : IPackageRepositoryManager;
  searchOptions : TSearchOptions;
begin
  //local for capture
  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  repoManager.Initialize(FConfiguration);
  searchOptions := options.Clone;
  searchOptions.Take := 100;
  result := TAsync.Configure <IList<IPackageSearchResultItem>> (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    begin
      CoInitialize(nil);
      try
        //Sleep(200);
        if cancelToken.IsCancelled then
          exit(nil);
        result := repoManager.GetPackageFeed(cancelToken, searchOptions, IDECompilerVersion, FCurrentPlatform).Results;
      finally
        CoUninitialize;
      end;
    end, FCancelTokenSource.Token);
end;



procedure TDPMEditViewFrame.RequestPackageIcon(const index: integer; const package: IPackageSearchResultItem);
var
  platform : TDPMPlatform;
  id : string;
  version : string;
  source : string;
  repoManager : IPackageRepositoryManager;
  stopWatch : TStopWatch;
begin
  stopWatch.Start;
  //we need a platform to pass to the repo, because the directory repo needs to construct a filename
  //we just get the first plaform in the set, doesn't matter which one.
//  for platform in package.Platform do
//    break;

  platform := package.Platform;

  //local for capture for use in the anonymous methods below.
  id := package.Id;
  version := package.Version.ToStringNoMeta;
  source := package.SourceName;
  FLogger.Debug('DPMIDE : Requesting icon for [' + id + '.' + version + ']');

  repoManager := FContainer.Resolve<IPackageRepositoryManager>;
  repoManager.Initialize(FConfiguration);

  TAsync.Configure<IPackageIcon>(
    function(const cancelToken : ICancellationToken) : IPackageIcon
    begin
      result := repoManager.GetPackageIcon(cancelToken, source, id, version, IDECompilerVersion, platform);
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
//        FScrollList.InvalidateRow(index);
        //TODO : Instead request repaint of row.
        FScrollList.Invalidate;
    end);
end;

procedure TDPMEditViewFrame.SaveBeforeInstall;
begin

end;

procedure TDPMEditViewFrame.ScrollListBeforeChangeRow(const Sender: TObject; const currentRowIndex: Int64; const direction: TScrollDirection; const delta: Int64; var newRowIndex: Int64);
var
  rowKind : TPackageRowKind;
begin
  //figure out which package will be current and set the details.
  rowKind := GetRowKind(newRowIndex);
  case rowKind of
    rkInstalledHeader:
    begin
        if GetInstalledCount > 0 then
          newRowIndex := 1
        else if GetImplicitCount > 0 then
          newRowIndex  := FImplicitHeaderRowIdx + 1
        else if GetAvailableCount > 0 then
          newRowIndex := FAvailableHeaderRowIdx + 1;
    end;
    rkImplicitHeader:
    begin
       if GetImplicitCount > 0 then
       begin
        if direction = TScrollDirection.sdDown then
          newRowIndex := FImplicitHeaderRowIdx + 1
        else
          newRowIndex := FImplicitHeaderRowIdx - 1;
       end;
    end;
    rkAvailableHeader:
    begin
      if GetAvailableCount > 0 then
      begin
        if direction = TScrollDirection.sdDown then
          newRowIndex := FAvailableHeaderRowIdx + 1
        else
        begin
          if (GetInstalledCount > 0) then
            newRowIndex := FAvailableHeaderRowIdx - 1
          else
            newRowIndex := FAvailableHeaderRowIdx + 1;
        end;

      end;
    end;
    rkInstalledPackage: ;
    rkImplicitPackage: ;
    rkAvailablePackage: ;
    rkUnknown: ;
  end;


end;

procedure TDPMEditViewFrame.ScrollListChangeRow(const Sender: TObject;  const newRowIndex: Int64; const direction: TScrollDirection;  const delta: Int64);
var
  rowKind : TPackageRowKind;
  item : IPackageSearchResultItem;
begin
  //find the selected package in the list and update the package details view on the right.
  item := nil;
  DebounceTimer.Enabled := false;
  rowKind := GetRowKind(newRowIndex);
  case rowKind of
    rkInstalledHeader:;
    rkImplicitHeader: ;
    rkAvailableHeader: ;
    rkInstalledPackage:
    begin
      if FInstalledPackages <> nil then
        item := FInstalledPackages[newRowIndex -1];
    end;
    rkImplicitPackage:
    begin
      if FImplicitPackages <> nil then
        item := FImplicitPackages[newRowIndex - FImplicitHeaderRowIdx -1];
    end;
    rkAvailablePackage:
    begin
      if FAvailablePackages <> nil then
      item := FAvailablePackages[newRowIndex - FAvailableHeaderRowIdx  -1 ];
    end;
    rkUnknown: ;
  end;
  FCurrentPackage := item;
  if FCurrentPackage <> nil then
      DebounceTimer.Enabled := true
  else
    PackageDetailsFrame.SetPackage(FCurrentPackage, FSearchOptions.Prerelease, true);
end;

procedure TDPMEditViewFrame.ScrollListPaintNoRows(const Sender: TObject;  const ACanvas: TCanvas; const paintRect: TRect);
begin

end;

procedure TDPMEditViewFrame.ScrollListPaintRow(const Sender: TObject;  const ACanvas: TCanvas; const itemRect: TRect; const index: Int64;  const state: TPaintRowState);
var
  rowKind : TPackageRowKind;
  item : IPackageSearchResultItem;
  title : string;
  version : string;
  latestVersion : string;
  latestIsPrerelease : boolean;
  backgroundColor : TColor;
  borderColor : TColor;
  underlineRect : TRect;
  icon : IPackageIconImage;
  packageIdx : Int64;
  package : IPackageSearchResultItem;

begin
  if (index < 0) or (index > FRowCount -1) then
    exit;

  rowKind := GetRowKind(index);
  backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);
  borderColor := FIDEStyleServices.GetSystemColor(clBtnShadow);
  case rowKind of
    rkInstalledHeader,
    rkImplicitHeader,
    rkAvailableHeader:
    begin
      backgroundColor := FIDEStyleServices.GetSystemColor(clBtnFace);
      //havent found a good combo yet for focused/unfocused that works for all themes
//      if FScrollList.Focused then
//        borderColor := FIDEStyleServices.GetSystemColor(clBtnHighlight)
//      else
        borderColor := FIDEStyleServices.GetSystemColor(clBtnShadow);

      ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
      underLineRect := itemRect;
      underlineRect.Top := underlineRect.Bottom - 3; //todo scale for dpi.
    end;
    rkInstalledPackage,
    rkImplicitPackage,
    rkAvailablePackage,
    rkUnknown:
    begin

      if (state in [TPaintRowState.rsSelected, TPaintRowState.rsFocusedSelected]) then
      begin
        {$IF CompilerVersion < 32.0}
        backgroundColor := $00FFF0E9;
        ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
        {$ELSE}
        if state = TPaintRowState.rsFocusedSelected then
          backgroundColor := FIDEStyleServices.GetSystemColor(clBtnHighlight)// FIDEStyleServices.GetStyleColor(TStyleColor.scButtonHot)
        else
          backgroundColor := FIDEStyleServices.GetSystemColor(clBtnShadow);//FIDEStyleServices.GetStyleColor(TStyleColor.scButtonFocused);
        ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clHighlightText);
        {$IFEND}
      end
      else
      begin
        backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);
        ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
      end;
    end;
  end;

  FRowLayout.Update(ACanvas, itemRect, rowKind);

  //row background
  ACanvas.Brush.Color := backgroundColor;
  ACanvas.FillRect(itemRect);

  //header row underline/separator
  if rowKind in [rkInstalledHeader, rkImplicitHeader, rkAvailableHeader] then
  begin
    ACanvas.Brush.Color := borderColor;
    ACanvas.FillRect(underlineRect);
    ACanvas.Brush.Color := backgroundColor;
  end;


  icon := nil;

  if rowKind in [rkInstalledPackage, rkImplicitPackage, rkAvailablePackage] then
  begin
    if (item <> nil) and (item.Icon <> '') then
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
    if icon <> nil then
      icon.PaintTo(ACanvas, FRowLayout.IconRect);
  end;

  latestVersion := '';
  latestIsPrerelease := false;
  try
    case rowKind of
      rkInstalledHeader:
      begin
        title := 'Installed Packages in group : ';
        if FInstalledActivity.IsActive then
          title := title  + FInstalledActivity.CurrentFrame
        else
          title := title + IntToStr(GetInstalledCount);
      end;
      rkImplicitHeader:
      begin
        title := 'Implicitly Installed Packages in group : ';
        if FImplicitActivity.IsActive then
          title := title + FImplicitActivity.CurrentFrame
        else
          title := title +IntToStr(GetImplicitCount);
      end;
      rkAvailableHeader:
      begin
        title := 'Available Packages';
        if FSearchOptions.SearchTerms = '' then
          title := title + ' (top 100) : '
        else
          title := title + ' : ';

        if FAvailableActivity.IsActive then
          title := title + FAvailableActivity.CurrentFrame
        else
          title := title + IntToStr(GetAvailableCount);
      end;
      rkInstalledPackage:
      begin
        packageIdx := index - FInstalledHeaderRowIdx -1;
        package := FInstalledPackages[packageIdx];
        if package = nil then
          exit;
        title := package.Id;
        version := ' ' + bulletChar + ' ' + package.Version.ToStringNoMeta;
        if FSearchOptions.Prerelease then
        begin
          if package.Version <> package.LatestVersion then
          begin
            latestVersion := package.LatestVersion.ToStringNoMeta;
            latestIsPrerelease := not package.LatestVersion.IsStable;
          end;
        end
        else
        begin
          if package.Version <> package.LatestStableVersion then
          begin
            // the installed version may be a pre-release version that
            // is later than the latest stable version so check first!
            if package.LatestStableVersion > package.Version then
              latestVersion := package.LatestStableVersion.ToStringNoMeta;
          end;
        end;
      end;
      rkImplicitPackage:
      begin
        packageIdx := index - FImplicitHeaderRowIdx -1;
        package := FImplicitPackages[packageIdx];
        if package = nil then
          exit;

        title := package.Id;
        version := ' ' + bulletChar + ' (' + package.Version.ToStringNoMeta + ')';
        if FSearchOptions.Prerelease then
        begin
          if package.Version <> package.LatestVersion then
          begin
            latestVersion := package.LatestVersion.ToStringNoMeta;
            latestIsPrerelease := not package.LatestVersion.IsStable;
          end;
        end
        else
        begin
          if package.Version <> package.LatestStableVersion then
          begin
            latestVersion := package.LatestStableVersion.ToStringNoMeta;
            latestIsPrerelease := false;
          end;
        end;
      end;
      rkAvailablePackage:
      begin
        packageIdx := index - FAvailableHeaderRowIdx -1;
        if packageIdx >= 0 then
        begin
          package := FAvailablePackages[packageIdx];
          if package = nil then
            exit;

          title := package.Id;

          if FSearchOptions.Prerelease then
          begin
            latestVersion :=  package.LatestVersion.ToStringNoMeta;
            latestIsPrerelease := not package.LatestVersion.IsStable;
          end
          else
            latestVersion :=  package.LatestStableVersion.ToStringNoMeta;
        end;
      end;
      rkUnknown :
      begin

      end;
    end;
    title := title + version;

    DrawText(ACanvas.Handle, PChar(title),Length(title), FRowLayout.TitleRect, DT_SINGLELINE + DT_LEFT + DT_VCENTER );
    if latestVersion <> '' then
    begin
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
      if latestIsPrerelease then
        ACanvas.Font.Color := $006464FA// $0F2CAB
      else
        ACanvas.Font.Color := $E2A428;
      DrawText(ACanvas.Handle, PChar(latestVersion),Length(latestVersion), FRowLayout.LatestVersionRect, DT_SINGLELINE + DT_RIGHT + DT_VCENTER );
    end;


  finally
    //this is important
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
  end;




end;

procedure TDPMEditViewFrame.SearchBarOnFocustList(sender: TObject);
begin
  FScrollList.SetFocus;
  if (FInstalledPackages <> nil) and (FInstalledPackages.Count > 0) then
    FScrollList.CurrentRow := 1
  else if (FAvailablePackages <> nil) and (FAvailablePackages.Count > 0) then
    FScrollList.CurrentRow := FAvailableHeaderRowIdx + 1;

end;

procedure TDPMEditViewFrame.SearchBarOnSearch(const searchText: string;  const searchOptions: TDPMSearchOptions; const source: string; const platform: TDPMPlatform; const refresh: boolean);
begin
  //
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force DoPlatform to do something.

  FSearchOptions.Platforms := [platform];
  FSearchOptions.Prerelease := TDPMSearchOption.IncludePrerelease in searchOptions;
  FSearchOptions.Commercial := TDPMSearchOption.IncludeCommercial in searchOptions;
  FSearchOptions.Trial      := TDPMSearchOption.IncludeTrial in searchOptions;
  FSearchOptions.SearchTerms := Trim(searchText);
  if source <> 'All' then
    FSearchOptions.Sources := source
  else
    FSearchOptions.Sources := '';

  DoPlatformChange(platform, refresh);

end;

procedure TDPMEditViewFrame.SearchBarPlatformChanged(const newPlatform: TDPMPlatform);
begin
  DoPlatformChange(newPlatform, true);
end;

procedure TDPMEditViewFrame.SearchBarProjectSelected(const projectFile: string);
var
  platform : TDPMPlatform;
begin
  FSelectedProject := projectFile;
  platform := FCurrentPlatform;
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;
  DoPlatformChange(platform, true);
end;

procedure TDPMEditViewFrame.SearchBarSettingsChanged(const configuration: IConfiguration);
begin

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
//  ideThemeSvc.ApplyTheme(FButtonBar);
//  ideThemeSvc.ApplyTheme(FSearchBar);
//  ideThemeSvc.ApplyTheme(FSearchBar.chkIncludePrerelease);
//  ideThemeSvc.ApplyTheme(Splitter2);
  FIDEStyleServices := ideThemeSvc.StyleServices;
  {$ELSE}
  FIDEStyleServices := Vcl.Themes.StyleServices;
  {$IFEND}

  Self.Color := FIDEStyleServices.GetSystemColor(clWindow);

  FSearchBar.ThemeChanged(FIDEStyleServices);

  Splitter2.Color := FIDEStyleServices.GetSystemColor(clBtnFace);

  FScrollList.Color := FIDEStyleServices.GetSystemColor(clWindow);
  FScrollList.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);

  PackageDetailsFrame.ThemeChanged(FIDEStyleServices {$IFDEF THEMESERVICES}, ideThemeSvc {$ENDIF}) ;
end;

procedure TDPMEditViewFrame.ViewDeselected;
begin
  // The view tab was deselected.
  FLogger.Debug('DPMIDE : View Deselected');
//  platformChangeDetectTimer.Enabled := false;
  FFirstView := true;
end;

procedure TDPMEditViewFrame.ViewSelected;
begin
  FLogger.Debug('DPMIDE : View Selected');
  //For some reason this get's called twice for each time the view is selected.
//  platformChangeDetectTimer.Enabled := true;
  if FFirstView then
  begin
    FFirstView := false;
    DoPlatformChange(FSearchBar.Platform, true);
  end;

end;

{ TRowLayout }


constructor TRowLayout.Create(const margin : integer; const iconSize : integer);
begin
  Self.Margin := margin;
  Self.IconSize := iconSize;
end;

procedure TRowLayout.Update(const ACanvas: TCanvas; const rowRect: TRect; const rowKind : TPackageRowKind);
begin
  IconRect.Top := rowRect.Top + ((rowRect.Height - IconSize) div 2); //center vertically
  IconRect.Left := rowRect.Left + Margin;  //TODO : These margins etc need to be scaled with dpi!
  IconRect.Width := IconSize;
  IconRect.Height := IconSize;

  LatestVersionRect.Top := rowRect.Top;
  LatestVersionRect.Right := rowRect.Right - Margin;
  LatestVersionRect.Height := rowRect.Height;// Abs(ACanvas.Font.Height) + Margin;
  LatestVersionRect.Left := rowRect.Right - ACanvas.TextExtent('100.100.100-aplha123aaaaa').Width - Margin;


  TitleRect := rowRect;
  if rowKind in [rkInstalledPackage, rkImplicitPackage, rkAvailablePackage] then
    TitleRect.Left := rowRect.Left + Margin + iconRect.Width + Margin
  else
    TitleRect.Left := rowRect.Left + Margin;

  TitleRect.Right := LatestVersionRect.Left - Margin div 2;

end;



end.
