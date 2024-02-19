unit DPM.IDE.EditorViewFrame;

interface

{$I '..\DPMIDE.inc'}


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
  DPM.Core.Repository.Interfaces,
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
  System.Actions,
  {$IFDEF USEIMAGECOLLECTION}
  Vcl.VirtualImageList,
  Vcl.ImageCollection,
  {$ELSE}
  Vcl.Imaging.pngimage,
  {$ENDIF}
  System.ImageList,
  Vcl.ActnList,
  DPM.IDE.PackageDetailsFrame;


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
    procedure platformChangeDetectTimerTimer(Sender: TObject);
    procedure ActivityTimerTimer(Sender: TObject);
  private
    FIDEStyleServices : TCustomStyleServices;

    //controls
    FSearchBar : TDPMSearchBarFrame;
    FScrollList : TVSoftVirtualListView;
    //controls

    {$IFDEF USEIMAGECOLLECTION }
    FImageList : TVirtualImageList;
    FImageCollection : TImageCollection;
    {$ELSE}
    FImageList : TImageList;
    {$ENDIF}


    //contains layout rects for the list view
    FRowLayout : TRowLayout;

    FInstalledHeaderRowIdx : Int64;
    FImplicitHeaderRowIdx : Int64;
    FAvailableHeaderRowIdx : Int64;

    FInstallingProjectCount : integer;


    //dpm core stuff
    FLogger : IDPMIDELogger;
    FProjectTreeManager : IDPMProjectTreeManager;
    FConfigurationManager : IConfigurationManager;
    FRepositoryManager : IPackageRepositoryManager;
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
    procedure PackageInstalled;
    procedure BeginInstall(const projectCount : integer);
    procedure EndInstall;


    function GetPackageReferences : IPackageReference;
    function GetPackageIdsFromReferences(const platform: TDPMPlatform): IList<IPackageIdentity>;
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
    procedure SearchBarConfigChanged(const configuration : IConfiguration);
    procedure SearchBarPlatformChanged(const newPlatform : TDPMPlatform);
    procedure SearchBarOnSearch(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const platform : TDPMPlatform; const refresh : boolean);
    procedure SearchBarOnFocustList(sender : TObject);

    //scrolllist evernts
    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);
    procedure ScrollListBeforeChangeRow(const Sender : TObject; const currentRowIndex : Int64; const direction : TScrollDirection; const delta : Int64; var newRowIndex : Int64);


    procedure DoPlatformChange(const newPlatform : TDPMPlatform; const refresh : boolean; const refreshInstalled : boolean);

    function GetRowKind(const index : Int64) : TPackageRowKind;
    procedure CalculateIndexes;
    procedure ChangeScale(M: Integer; D: Integer{$IF CompilerVersion > 33}; isDpiChange: Boolean{$IFEND}); override;

    procedure LoadImages;
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
  Winapi.CommCtrl,
  DPM.Core.Constants,
  DPM.Core.Options.Common,
  DPM.Core.Utils.Config,
  DPM.Core.Utils.Numbers,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Strings,
  DPM.Core.Utils.System,
  DPM.Core.Project.Editor,
  DPM.Core.Package.Icon,
  DPM.Core.Package.SearchResults,
  DPM.IDE.ToolsAPI,
  DPM.IDE.AboutForm,
  DPM.IDE.AddInOptionsHostForm,
  DPM.Core.Dependency.Graph;


type
  TFilterProc = procedure(const searchTxt : string) of object;



function FindPackageRef(const node : IPackageReference; const platform : TDPMPlatform; const searchId : string; const topLevelOnly : boolean) : IPackageReference;
var
  reference : IPackageReference;
  dependencies : TArray<IPackageReference>;
begin
  result := nil;
  if (node = nil) or (not node.HasChildren) then
    exit;

  //when we search the project node we need to actually look at it's children, otherwise we will not find dependencies.
  //the FPackageRefences passed in will be a root, as will the project nodes, so this will recurse twice for each external call.
  if node.IsRoot then
  begin
    //1st time through, these will be the project nodes
    //2nd time through, these will be top level dependencies.
    dependencies := node.Children.ToArray; //trying to make the debugger work here
    for reference in dependencies do
    begin
      if SameText(reference.Id, searchId) then
          Exit(reference);

      result := FindPackageRef(reference, platform, searchId, topLevelOnly);
      if result <> nil then
        exit;
    end;
    exit;
  end;

  if SameText(node.Id, searchId) then
      Exit(node);

  if topLevelOnly then
    exit;

  dependencies := node.Children.ToArray;
  //breadth first search!
  for reference in dependencies do
  begin
    if reference.Platform <> platform then
      continue;

    if SameText(reference.Id, searchId) then
      Exit(reference);
  end;

  //depth

  for reference in node.Children do
  begin
    if reference.Platform <> platform then
      continue;
    //depth search
    if reference.HasChildren then
    begin
      result := FindPackageRef(reference, platform, searchId, false);
      if result <> nil then
        Exit(result);
    end;
  end;
end;


{ TDPMEditViewFrame2 }

procedure TDPMEditViewFrame.ActivityTimerTimer(Sender: TObject);
begin
  ActivityTimer.Enabled := false;
  if FClosing then
    exit;

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
  if FClosing then
    exit;
  ActivityTimer.Enabled := FInstalledActivity.IsActive or FImplicitActivity.IsActive or FAvailableActivity.IsActive;
end;

procedure TDPMEditViewFrame.BeginInstall(const projectCount : integer);
begin
  FInstallingProjectCount := projectCount;
  TSystemUtils.OutputDebugString('BeginInstall : ' + intToStr(projectCount));
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
var
  count : integer;
begin
  FClosing := true;
  PackageDetailsFrame.ViewClosing;
  //cancel any pending requests asap. Needs to return quickly or the IDE will hang.
  FCancelTokenSource.Cancel;
  //allow the cancellation to happen.
  //if we don't do this we will get an excepion in the await or cancellation callbacks
  count := 0;
  while (FRequestsInFlight > 0) and (count < 10) do
  begin
    Application.ProcessMessages;
    Inc(count);
  end;

end;

procedure TDPMEditViewFrame.Configure(const projectGroup : IOTAProjectGroup; const project : IOTAProject; const container: TContainer;  const projectTreeManager: IDPMProjectTreeManager);
var
  sConfigFile : string;
begin
  FProjectTreeManager := projectTreeManager;
  FProjectGroup := projectGroup;
  FProject := project;
  if project <> nil then
    FSelectedProject := project.FileName
  else
    FSelectedProject := '';

  FLogger := container.Resolve<IDPMIDELogger>;
  FDPMIDEOptions := container.Resolve<IDPMIDEOptions>;
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
  FConfigurationManager := container.Resolve<IConfigurationManager>;
  FConfigurationManager.EnsureDefaultConfig;
  FConfiguration := FConfigurationManager.LoadConfig(FSearchOptions.ConfigFile);

  FRepositoryManager := container.Resolve<IPackageRepositoryManager>;

  ConfigureSearchBar;

  PackageDetailsFrame.Init(container, FIconCache, FConfiguration, Self, projectGroup);

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
  FSearchOptions := TSearchOptions.Create;
  FAllInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FInstalledPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FImplicitPackages := TCollections.CreateList<IPackageSearchResultItem>;
  FAvailablePackages := TCollections.CreateList<IPackageSearchResultItem>;

  //hard code our compiler version here since when we are running in the IDE we are only working with the IDE version
  FSearchOptions.CompilerVersion := IDECompilerVersion;

  FSearchSkip := 0;
  FSearchTake := 0;
  FFirstView := true;
  FRequestsInFlight := 0;

  FCancelTokenSource := TCancellationTokenSourceFactory.Create;
  FCurrentPlatform := TDPMPlatform.UnknownPlatform;

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

  {$IFDEF USEIMAGECOLLECTION } //10.4 or later
  FImageList := TVirtualImageList.Create(Self);
  FImageCollection := TImageCollection.Create(Self);
  {$ELSE}
  FImageList := TImageList.Create(Self);
  {$ENDIF}
  FImageList.ColorDepth := cd32Bit;
  FImageList.DrawingStyle := dsTransparent;
  FImageList.Width := 16;
  FImageList.Height := 16;

  LoadImages;
  PackageDetailsFrame.ImageList := FImageList;
  CreateControls(AOwner);
  ThemeChanged;





end;

procedure TDPMEditViewFrame.CreateControls(AOwner: TComponent);
begin


  FSearchBar := TDPMSearchBarFrame.Create(Self);
  //important to make it appear below the button bar
  FSearchBar.Top := 0;
  FSearchBar.OnSearch := Self.SearchBarOnSearch;
  FSearchBar.OnConfigChanged := Self.SearchBarConfigChanged;
  FSearchBar.OnPlatformChanged := Self.SearchBarPlatformChanged;
  FSearchBar.OnFocusList := Self.SearchBarOnFocustList;
  FSearchBar.ImageList := FImageList;
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



procedure TDPMEditViewFrame.DoPlatformChange(const newPlatform: TDPMPlatform; const refresh : boolean; const refreshInstalled : boolean);
var
  searchTxt : string;
  filterProc : TFilterProc;
  count : integer;
begin
  if FClosing then
    exit;

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
    FInstalledActivity.Step;
    FScrollList.InvalidateRow(FInstalledHeaderRowIdx);
    FImplicitActivity.Step;
    FScrollList.InvalidateRow(FImplicitHeaderRowIdx);
    ActivityTimer.Enabled := true;

    if FRequestsInFlight > 0 then
      FCancelTokenSource.Cancel;

    count := 0;
    while ((FRequestsInFlight > 0) and (count < 10)) do
    begin
//      TSystemUtils.OutputDebugString('TDPMEditViewFrame2.DoPlatformChange - requests in flight : ' + IntToStr(FRequestsInFlight));
      Inc(count);
      Application.ProcessMessages;
    end;

    FCancelTokenSource.Reset;

    FAvailablePackages.Clear;
    if refreshInstalled then
    begin
      FAllInstalledPackages.Clear;
      FInstalledPackages.Clear;
      FImplicitPackages.Clear;
    end;

    if (not refreshInstalled) and (FAllInstalledPackages <> nil) and (FAllInstalledPackages.Count > 0) then
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

    SearchForPackagesAsync(FSearchOptions)
    .OnException(
      procedure(const e : Exception)
      begin
        Dec(FRequestsInFlight);
        if FClosing then
          exit;
        FAvailableActivity.Stop;
        FLogger.Error(e.Message);
      end)
    .OnCancellation(
      procedure
      begin
        Dec(FRequestsInFlight);
        FAvailableActivity.Stop;
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
        FAvailableActivity.Stop;
        //if the view is closing do not do anything else.
        if FClosing then
          exit;

        FLogger.Debug('DPMIDE : Got search results.');
        toRemove := TCollections.CreateList<IPackageSearchResultItem>;
        //some of the available packages may already be installed, so we need to check for that.
        for item in theResult do
        begin
          packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item.id, false);
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


procedure TDPMEditViewFrame.EndInstall;
begin
  TSystemUtils.OutputDebugString('TDPMEditViewFrame.EndInstall');
  if FInstallingProjectCount <> 0 then
  begin
    //just in case the change notifier doesn't fire for every project.
    FInstallingProjectCount := 0;
    ProjectChanged;
  end;
end;

function TDPMEditViewFrame.GetPackageIdsFromReferences(const platform: TDPMPlatform): IList<IPackageIdentity>;
var
  lookup : IDictionary<string, IPackageIdentity>;
  packageRef : IPackageReference;

  procedure AddPackageIds(const value : IPackageReference);
  var
    childRef : IPackageReference;
    existing : IPackageIdentity;
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

    for childRef in value.Children do
      AddPackageIds(childRef);
  end;

begin
  lookup := TCollections.CreateDictionary<string, IPackageIdentity> ;
  result := TCollections.CreateList<IPackageIdentity>;
  if FPackageReferences <> nil then
  begin
    for packageRef in FPackageReferences.Children do
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
  projectFile : string;
  basePath : string;
begin
  projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
  result := TGraphNode.CreateRoot(IDECompilerVersion, FCurrentPlatform);
  Assert(FProjectGroup <> nil);
  basePath := FProjectGroup.FileName;
  //unsaved project group still seems to have a file name.
  if FileExists(basePath) then
    basePath := ExtractFilePath(basePath)
  else
    basePath := '';
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    proj := FProjectGroup.Projects[i];
    projectFile := proj.FileName;
    if basePath <> '' then
      projectFile := TPathUtils.CompressRelativePath(basePath,projectFile);

    projectEditor.LoadProject(projectFile);

    packageReference := projectEditor.GetPackageReferences(FCurrentPlatform);
    if packageReference <> nil then
      Result.AddExistingChild(LowerCase(projectFile), packageReference);
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

procedure TDPMEditViewFrame.LoadImages;
const
  suffixes : array[0..3] of string = ('_16', '_24', '_32','_48');

{$IFNDEF USEIMAGECOLLECTION} //10.2 or earlier

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

{$ENDIF}


begin
  {$IFNDEF USEIMAGECOLLECTION} //10.2 or earlier
    AddImage('ADD_PACKAGE_16');
    AddImage('REMOVE_PACKAGE_16');
    AddImage('UPGRADE_PACKAGE_16');
    AddImage('DOWNGRADE_PACKAGE_16');
    AddImage('REFRESH_16');
    AddImage('INFO_16');
    AddImage('SETTINGS_16');
    AddImage('CANCEL_16');
    AddImage('CANCEL_HOT_16');
  {$ELSE}
    FImageCollection.Add('add',HInstance,'ADD_PACKAGE', suffixes);
    FImageCollection.Add('remove',HInstance,'REMOVE_PACKAGE', suffixes);
    FImageCollection.Add('upgrade',HInstance,'UPGRADE_PACKAGE', suffixes);
    FImageCollection.Add('downgrade',HInstance,'DOWNGRADE_PACKAGE', suffixes);
    FImageCollection.Add('refresh',HInstance,'REFRESH', suffixes);
    FImageCollection.Add('info',HInstance,'INFO', suffixes);
    FImageCollection.Add('settings',HInstance,'SETTINGS', suffixes);
    FImageCollection.Add('cancel',HInstance,'CANCEL', suffixes);
    FImageCollection.Add('cancel_hot',HInstance,'CANCEL_HOT', suffixes);
    FImageList.AutoFill := true;
    FImageList.ImageCollection := FImageCollection;
  {$ENDIF}

end;

procedure TDPMEditViewFrame.PackageInstalled;
begin
  //Tell the IDE to reload the project as we have just modified it on disk.
  FProjectGroup.Refresh(false);
end;

function TDPMEditViewFrame.GetAvailableCount: Int64;
begin
  if FAvailablePackages <> nil then
    result := FAvailablePackages.Count
  else
    result := 0;
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

  //The problem here is that the change notifier fires for each project changed
  //so if we install a package into every project in a group, it will fire for
  //every project and then we end up here.
  //the notifications are outside our control. We need to know when they are done
  //so we count down the number of times it fires. This might be problematic if
  //for some reason the notifier doesn't fire for a project. We reset it in EndInstall
  //just in case.

  if FInstallingProjectCount > 0 then
  begin
    Dec(FInstallingProjectCount);
    TSystemUtils.OutputDebugString('ProjectChanged : ' + intToStr(FInstallingProjectCount));
    if FInstallingProjectCount > 0 then
      exit;
  end;

  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  PackageDetailsFrame.ProjectReloaded;

  //need to update searchbar platforms etc.

  platforms := GetPlatforms;
  FSearchBar.UpdatePlatforms(platforms);

  DoPlatformChange(FSearchBar.Platform, true, true);

end;

procedure TDPMEditViewFrame.ProjectClosed(const projectName: string);
var
  platforms : TDPMPlatforms;
begin
  TSystemUtils.OutputDebugString('TDPMEditViewFrame2.ProjectClosed : ' + projectName);
  FLogger.Debug('DPMIDE : ProjectClosed');
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  PackageDetailsFrame.ProjectReloaded;

  platforms := GetPlatforms;
  FSearchBar.UpdatePlatforms(platforms);
//  DoPlatformChange(FSearchBar.Platform, true);
end;

procedure TDPMEditViewFrame.ProjectLoaded(const projectName: string);
begin
//  TSystemUtils.OutputDebugString('TDPMEditViewFrame2.ProjectLoaded : ' + projectName);
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
  searchResultItem : IPackageSearchResultItem;
begin
  //local for capture
  lProjectFile := FProjectGroup.FileName;

  repoManager := FRepositoryManager;
  repoManager.Initialize(FConfiguration);

  options := FSearchOptions.Clone;
  //don't filter for installed packages, we want them all
  options.Prerelease := true;
  options.Commercial := true;
  options.Trial := true;

  options.Platforms := [FCurrentPlatform];

  result := TAsync.Configure<IList<IPackageSearchResultItem>> (
    function(const cancelToken : ICancellationToken) : IList<IPackageSearchResultItem>
    var
      packageRef : IPackageReference;
      item : IPackageSearchResultItem;
      packageIds : IList<IPackageIdentity>;
      pkg : IPackageIdentity;
    I: Integer;
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
            packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item.id, true);
            if packageRef <> nil then //top level reference
            begin
              item.IsTransitive := false;
            end
            else //not found as a top level reference so check for transitive reference
            begin
              packageRef := FindPackageRef(FPackageReferences, FCurrentPlatform, item.id, false);
              if packageRef <> nil then
              begin
                item.IsTransitive := packageRef.IsTransitive;
                if item.IsTransitive then
                  item.VersionRange := packageRef.SelectedOn;
              end
              else
              begin
                item.IsTransitive := false;
              end;
            end;
            if packageRef <> nil then
            begin
              item.Version := packageRef.Version;
              item.Installed := true;
            end;

          end;
          //remove from the list so we can tell if we got them all in the end.
          pkg := packageIds.Where(
              function(const value : IPackageIdentity) : boolean
              begin
                result := SameText(value.Id, item.Id);
              end).FirstOrDefault;
          if pkg <> nil then
            packageIds.Remove(pkg);
        end;

        //if there are any left that means we didn't get the searchresultitem for it from the repos
        if packageIds.Count > 0 then
        begin
          //we just fake them so there is something for the list?
          for I := 0 to packageIds.Count -1 do
          begin
            pkg := packageIds[i];
            //Can we get these from the packagecache?


            //TODO : We should probably flag these in the list to show there is a problem!
            searchResultItem := TDPMPackageSearchResultItem.FromError(pkg.Id, pkg.Version, options.CompilerVersion, FCurrentPlatform, 'Package not found on enabled sources' );
            Result.Add(searchResultItem)
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
  repoManager := FRepositoryManager;
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

  repoManager := FRepositoryManager;
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
//  FProjectGroup.Save(false, true);
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
  if FClosing then
    exit;

  //find the selected package in the list and update the package details view on the right.
  item := nil;
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
  PackageDetailsFrame.SetPackage(FCurrentPackage, FSearchOptions.Prerelease, true);
end;

procedure TDPMEditViewFrame.ScrollListPaintNoRows(const Sender: TObject;  const ACanvas: TCanvas; const paintRect: TRect);
begin

end;

procedure TDPMEditViewFrame.ScrollListPaintRow(const Sender: TObject;  const ACanvas: TCanvas; const itemRect: TRect; const index: Int64;  const state: TPaintRowState);
var
  rowKind : TPackageRowKind;
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
        if (packageIdx >= 0) and (packageIdx < FInstalledPackages.Count) then
        begin
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
      end;
      rkImplicitPackage:
      begin
        packageIdx := index - FImplicitHeaderRowIdx -1;
        if (packageIdx >= 0) and (packageIdx < FImplicitPackages.Count) then
        begin
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
      end;
      rkAvailablePackage:
      begin
        packageIdx := index - FAvailableHeaderRowIdx -1;
        if (packageIdx >= 0) and (packageIdx < FAvailablePackages.Count) then
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

    icon := nil;

    if rowKind in [rkInstalledPackage, rkImplicitPackage, rkAvailablePackage] then
    begin
      if (package <> nil) and (package.Icon <> '') then
      begin
        //first query will add nil to avoid multiple requests
        if not FIconCache.Query(package.Id) then
          //fetch the icon async
          RequestPackageIcon(index, package)
        else
          //this might return nil if the request hasn't completed
          //or it failed to find the icon.
          icon := FIconCache.Request(package.Id);
      end;

      if icon = nil then
        icon := FIconCache.Request('missing_icon');
      if icon <> nil then
        icon.PaintTo(ACanvas, FRowLayout.IconRect);
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

  DoPlatformChange(platform, refresh, false);

end;

procedure TDPMEditViewFrame.SearchBarPlatformChanged(const newPlatform: TDPMPlatform);
begin
  DoPlatformChange(newPlatform, true, true);
end;


procedure TDPMEditViewFrame.SearchBarConfigChanged(const configuration: IConfiguration);
begin
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  FConfiguration := configuration;
  DoPlatformChange(FSearchBar.Platform, true, true);
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
    Application.ProcessMessages; //allow the frame to paint
    DoPlatformChange(FSearchBar.Platform, true, true);
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
