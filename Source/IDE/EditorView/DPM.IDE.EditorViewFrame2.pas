unit DPM.IDE.EditorViewFrame2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, SVGInterfaces,
  Vcl.Themes,
  DPM.Controls.ButtonedEdit,
  DPM.Controls.ButtonBar,
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
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  {$IF CompilerVersion >= 30.0 }
  System.ImageList,
  {$IFEND}
  Vcl.ActnList,
  DPM.IDE.GroupPackageDetailsFrame;


{$I '..\DPMIDE.inc'}

type
  TPackageRowKind = (rkInstalledHeader, rkImplicitHeader, rkAvailableHeader,rkInstalledPackage, rkImplicitPackage, rkAvailablePackage, rkUnknown);

  TRowLayout = record
    RowWidth : integer;
    IconRect : TRect;
    TitleRect : TRect;
    VersionRect : TRect;
    LatestVersionRect : TRect;
    DescriptionRect : TRect;
    procedure Update(const ACanvas : TCanvas; const rowRect : TRect; const rowKind : TPackageRowKind);
  end;



  TDPMEditViewFrame2 = class(TFrame, IDetailsHost)
    ContentPanel : TPanel;
    Splitter2 : TSplitter;
    PackageListPanel : TPanel;
    DetailPanel: TPanel;
    platformChangeDetectTimer: TTimer;
    PackageDetailsFrame: TGroupPackageDetailsFrame;
    procedure platformChangeDetectTimerTimer(Sender: TObject);
  private
    FIDEStyleServices : TCustomStyleServices;

    //controls
    FSearchBar : TDPMSearchBarFrame;
    FScrollList : TVSoftVirtualListView;
    FProjectsGrid : TVersionGrid;
    //controls

    //contains layout rects for the list view
    FRowLayout : TRowLayout;

    //temp
    FInstalledCount : Int64;
    FImplicitCount : Int64;
    FAvailableCount : Int64;
    FRowCount : Int64;

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
    FRequestInFlight : boolean;
    FClosing : boolean;

    //Project group should never be nil?
    FProjectGroup : IOTAProjectGroup;

    FProject : IOTAProject;


    FIconCache : TDPMIconCache;
    FDPMIDEOptions : IDPMIDEOptions;
    FCurrentPlatform : TDPMPlatform;
    FCurrentTab : TDPMCurrentTab;

    FSearchOptions : TSearchOptions;
    FSearchSkip : integer;
    FSearchTake : integer;

    FPackageReferences : IPackageReference;


    //true when we first load the view
    FFirstView : boolean;
  protected
    //IDetailsHost
    procedure SaveBeforeInstall;
    procedure PackageInstalled(const package : IPackageSearchResultItem; const isUpdate : boolean);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
    function GetPackageReferences : IPackageReference;

    procedure RequestPackageIcon(const index : integer; const package : IPackageSearchResultItem);


    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);virtual;
    procedure ConfigureSearchBar;


    //callbacks from the searchbar
    procedure SearchBarSettingsChanged(const configuration : IConfiguration);
    procedure SearchBarPlatformChanged(const newPlatform : TDPMPlatform);
    procedure SearchBarOnSearch(const searchText : string; const searchOptions : TDPMSearchOptions; const source : string; const platform : TDPMPlatform; const refresh : boolean);

    //scrolllist evernts
    procedure ScrollListPaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure ScrollListPaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure ScrollListChangeRow(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);
    procedure ScrollListBeforeChangeRow(const Sender : TObject; const currentRowIndex : Int64; const direction : TScrollDirection; const delta : Int64; var newRowIndex : Int64);


    procedure DoPlatformChange(const newPlatform : TDPMPlatform);

    function GetRowKind(const index : Int64) : TPackageRowKind;
    procedure CalculateIndexes;

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
    procedure FilterToGroup;
    procedure FilterToProject(const projectName : string);
    function CanCloseView : boolean;
  end;

implementation

{$R *.dfm}

uses
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
  DPM.IDE.AddInOptionsHostForm,
  DPM.Core.Dependency.Graph;


{ TDPMEditViewFrame2 }

procedure TDPMEditViewFrame2.CalculateIndexes;
begin
  FInstalledHeaderRowIdx := 0;
  if FImplicitCount > 0 then
  begin
    FImplicitHeaderRowIdx := FInstalledHeaderRowIdx + FInstalledCount + 1;
    FAvailableHeaderRowIdx := FImplicitHeaderRowIdx + FImplicitCount + 1;
  end
  else
  begin
    FImplicitHeaderRowIdx := -1;
    FAvailableHeaderRowIdx := FInstalledHeaderRowIdx + FInstalledCount + 1;
  end;

  FRowCount := 1 + FInstalledCount;
  //only show implict header if there are any packages
  if FImplicitCount > 0 then
    FRowCount := FRowCount + 1 + FImplicitCount;

  //always show available header
  FRowCount := FRowCount + 1 + FAvailableCount;

  FScrollList.RowCount := FRowCount;
end;

function TDPMEditViewFrame2.CanCloseView: boolean;
begin
  result := true; //TODO : Block closing while busy installing/removing packages.
end;

procedure TDPMEditViewFrame2.Closing;
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

procedure TDPMEditViewFrame2.Configure(const projectGroup : IOTAProjectGroup; const project : IOTAProject; const container: TContainer;  const projectTreeManager: IDPMProjectTreeManager);
var
  sConfigFile : string;
begin
  FContainer := container;
  FProjectTreeManager := projectTreeManager;
  FProjectGroup := projectGroup;
  FProject := project;

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

//  if not IsProjectGroup then
//    platformChangeDetectTimerTimer(platformChangeDetectTimer);

  //TODO : for project group configure with platforms enabled in project.

  ConfigureSearchBar;

  PackageDetailsFrame.Init(FContainer, FIconCache, FConfiguration, Self, projectGroup);

  //testing
  FInstalledCount := 1;
  FImplicitCount := 3;
  FAvailableCount := 50;
  CalculateIndexes;
end;

procedure TDPMEditViewFrame2.ConfigureSearchBar;
var
  platforms : TDPMPlatforms;
  i : integer;
  projectEditor : IProjectEditor;
begin
  //it would be nice to use the open tools api to do this, but so far
  // - Project.SupportedPlatforms returns all platforms, whether they are enabled for the project or not
  // - BuildConfig.Platforms is empty.

  //doing it this way is not ideal, as it requires that the project has been saved after a platform was added.
  projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
  platforms := [];//TDPMPlatform.Win32,TDPMPlatform.Win64, TDPMPlatform.OSX32];
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    if FProjectGroup.Projects[i].FileName <> '' then
    begin
      projectEditor.LoadProject(FProjectGroup.Projects[i].FileName, [TProjectElement.Platforms]);
      platforms := platforms + projectEditor.Platforms;
    end
    else
    begin
      //what can we do? unsaved project, will probably have a name so this shouldn't happen?


    end;
  end;

  FSearchBar.Configure(FLogger, FDPMIDEOptions, FConfiguration, FConfigurationManager, FSearchOptions.ConfigFile, platforms);

end;

constructor TDPMEditViewFrame2.Create(AOwner: TComponent);
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

  PackageDetailsFrame.Configure(FCurrentTab, FSearchBar.IncludePrerelease);

end;

procedure TDPMEditViewFrame2.CreateControls(AOwner: TComponent);
begin


  FSearchBar := TDPMSearchBarFrame.Create(Self);
  //important to make it appear below the button bar
  FSearchBar.Top := 0;
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
  FScrollList.RowHeight := 32;
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

destructor TDPMEditViewFrame2.Destroy;
begin
  FSearchOptions.Free;
  FIconCache.Free;
  if FLogger <> nil then
    FLogger.Debug('DPMIDE : View Destroying');

  inherited;
end;

procedure TDPMEditViewFrame2.DoPlatformChange(const newPlatform: TDPMPlatform);
begin
  if FCurrentPlatform <> newPlatform then
  begin
    FCurrentPlatform := newPlatform;
    FPackageReferences := GetPackageReferences;

    PackageDetailsFrame.SetPlatform(FCurrentPlatform);
    PackageDetailsFrame.SetPackage(nil, FSearchOptions.Prerelease);
    FSearchBar.SetPlatform(FCurrentPlatform);
    FSearchOptions.Platforms := [FCurrentPlatform];
    FScrollList.CurrentRow := -1;
    //clear scroll list and fetch again.

  end;

end;

procedure TDPMEditViewFrame2.FilterToGroup;
begin

end;

procedure TDPMEditViewFrame2.FilterToProject(const projectName: string);
begin

end;

function TDPMEditViewFrame2.GetPackageReferences: IPackageReference;
var
  projectEditor : IProjectEditor;
  proj : IOTAProject;
  i : integer;
  packageReference : IPackageReference;
  sProjectId : string;
begin
  projectEditor := TProjectEditor.Create(FLogger, FConfiguration, IDECompilerVersion);
  result := TPackageReference.CreateRoot(IDECompilerVersion, FCurrentPlatform);

  if FProject <> nil then
  begin
    projectEditor.LoadProject(FProject.FileName);
    result := projectEditor.GetPackageReferences(FCurrentPlatform); //NOTE : Can return nil. Will change internals to return empty root node.
  end
  else
  begin

    for i := 0 to FProjectGroup.ProjectCount -1 do
    begin
      proj := FProjectGroup.Projects[i];
      sProjectId := proj.FileName;
      projectEditor.LoadProject(sProjectId);
      packageReference := projectEditor.GetPackageReferences(FCurrentPlatform); //NOTE : Can return nil. Will change internals to return empty root node.
      if packageReference <> nil then
      begin
        sProjectId := ChangeFileExt(ExtractFileName(sProjectId), '');
        Result.AddExistingReference(LowerCase(sProjectId), packageReference);
      end;
    end;
  end;

end;

function TDPMEditViewFrame2.GetRowKind(const index : Int64): TPackageRowKind;
begin
  if index = 0 then
    exit(rkInstalledHeader)
  else if (FImplicitHeaderRowIdx > 0) and (index < FImplicitHeaderRowIdx) then
    result := rkInstalledPackage
  else if (index > FImplicitHeaderRowIdx) and (index < FAvailableHeaderRowIdx) then
    result := rkInstalledPackage
  else if (index = FImplicitHeaderRowIdx) and (FImplicitHeaderRowIdx > 0)  then
    result := rkImplicitHeader
  else if (index < FAvailableHeaderRowIdx) and (FImplicitHeaderRowIdx > 0) then
    result := rkImplicitPackage
  else if index = FAvailableHeaderRowIdx then
    result := rkAvailableHeader
  else if index > FAvailableHeaderRowIdx then
    result := rkAvailablePackage
  else
    result := rkUnknown;
end;

procedure TDPMEditViewFrame2.PackageInstalled(const package: IPackageSearchResultItem; const isUpdate: boolean);
begin

end;

procedure TDPMEditViewFrame2.PackageUninstalled(const package: IPackageSearchResultItem);
begin

end;

procedure TDPMEditViewFrame2.platformChangeDetectTimerTimer(Sender: TObject);
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

procedure TDPMEditViewFrame2.ProjectChanged;
var
  i : integer;
  sProject : string;
begin
  FLogger.Debug('DPMIDE : EditViewReloaded');
  FCurrentPlatform := TDPMPlatform.UnknownPlatform; //force a reload
  PackageDetailsFrame.ProjectReloaded;

  FProjectsGrid.BeginUpdate;
  FProjectsGrid.Clear;
  for i := 0 to FProjectGroup.ProjectCount -1 do
  begin
    sProject := ChangeFileExt(ExtractFileName(FProjectGroup.Projects[i].FileName), '');
    FProjectsGrid.AddProject(sProject, '' );
  end;
  FProjectsGrid.EndUpdate;
//  if not IsProjectGroup then
//    platformChangeDetectTimerTimer(platformChangeDetectTimer)
//  else
  DoPlatformChange(FSearchBar.Platform);

end;

procedure TDPMEditViewFrame2.ProjectClosed(const projectName: string);
begin


end;

procedure TDPMEditViewFrame2.ProjectLoaded(const projectName: string);
begin

end;

procedure TDPMEditViewFrame2.RequestPackageIcon(const index: integer; const package: IPackageSearchResultItem);
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
        //TODO : Instead request repaint of row.
        FScrollList.Invalidate;
    end);
end;

procedure TDPMEditViewFrame2.SaveBeforeInstall;
begin

end;

procedure TDPMEditViewFrame2.ScrollListBeforeChangeRow(const Sender: TObject; const currentRowIndex: Int64; const direction: TScrollDirection; const delta: Int64; var newRowIndex: Int64);
var
  rowKind : TPackageRowKind;
begin
  //figure out which package will be current and set the details.
  rowKind := GetRowKind(newRowIndex);
  case rowKind of
    rkInstalledHeader:
    begin
        if FInstalledCount > 0 then
          newRowIndex := 1
        else if FAvailableCount > 0 then
          newRowIndex := FAvailableHeaderRowIdx + 1;
    end;
    rkImplicitHeader:
    begin
       if FImplicitCount > 0 then
       begin
        if direction = TScrollDirection.sdDown then
          newRowIndex := FImplicitHeaderRowIdx + 1
        else
          newRowIndex := FImplicitHeaderRowIdx - 1;
       end;
    end;
    rkAvailableHeader:
    begin
      if FAvailableCount > 0 then
      begin
        if direction = TScrollDirection.sdDown then
          newRowIndex := FAvailableHeaderRowIdx + 1
        else
        begin
          if (FInstalledCount > 0) then
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

procedure TDPMEditViewFrame2.ScrollListChangeRow(const Sender: TObject;  const newRowIndex: Int64; const direction: TScrollDirection;  const delta: Int64);
var
  rowKind : TPackageRowKind;
begin
  //figure out which package will be current and set the details.
  rowKind := GetRowKind(newRowIndex);
  case rowKind of
    rkInstalledHeader: ;
    rkImplicitHeader: ;
    rkAvailableHeader: ;
    rkInstalledPackage: ;
    rkImplicitPackage: ;
    rkAvailablePackage: ;
    rkUnknown: ;
  end;
end;

procedure TDPMEditViewFrame2.ScrollListPaintNoRows(const Sender: TObject;  const ACanvas: TCanvas; const paintRect: TRect);
begin

end;

procedure TDPMEditViewFrame2.ScrollListPaintRow(const Sender: TObject;  const ACanvas: TCanvas; const itemRect: TRect; const index: Int64;  const state: TPaintRowState);
var
  rowKind : TPackageRowKind;
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
  rowKind := GetRowKind(index);
  backgroundColor := FIDEStyleServices.GetSystemColor(clWindow);

  case rowKind of
    rkInstalledHeader,
    rkImplicitHeader,
    rkAvailableHeader:
    begin
      backgroundColor := FIDEStyleServices.GetSystemColor(clBtnFace);
      ACanvas.Font.Color := FIDEStyleServices.GetSystemColor(clWindowText);
      ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
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




  //make text of different font sizes align correctly.
  oldTextAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
  fontSize := ACanvas.Font.Size;
  titleRect := FRowLayout.TitleRect;
  try
    case rowKind of
      rkInstalledHeader:
      begin
        title := 'Installed Packages in group : ' + IntToStr(FInstalledCount);

      end;
      rkImplicitHeader:
      begin
        title := 'Implicitly Installed Packages in group : ' + IntToStr(FImplicitCount);

      end;
      rkAvailableHeader:
      begin
        title := 'Available Packages : ' + IntToStr(FAvailableCount);
      end;
      rkInstalledPackage:
      begin
        title := 'Installed Package ' + IntToStr(index - FInstalledHeaderRowIdx);

      end;
      rkImplicitPackage:
      begin
        title := 'Implicit Package ' + IntToStr(index - FImplicitHeaderRowIdx);

      end;
      rkAvailablePackage:
      begin
        title := 'Available Package ' + IntToStr(index - FAvailableHeaderRowIdx);
      end;
      rkUnknown :
      begin

      end;
    end;
    extent := ACanvas.TextExtent(title);
    ExtTextOut(ACanvas.Handle, titleRect.Left, titleRect.Bottom - 6, ETO_CLIPPED, titleRect, title, Length(title), nil);



  finally
    //this is important
    SetTextAlign(ACanvas.Handle, oldTextAlign);
    ACanvas.Font.Size := fontSize;
    ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];

  end;




end;

procedure TDPMEditViewFrame2.SearchBarOnSearch(const searchText: string;  const searchOptions: TDPMSearchOptions; const source: string; const platform: TDPMPlatform; const refresh: boolean);
begin

end;

procedure TDPMEditViewFrame2.SearchBarPlatformChanged(const newPlatform: TDPMPlatform);
begin

end;

procedure TDPMEditViewFrame2.SearchBarSettingsChanged(const configuration: IConfiguration);
begin

end;

procedure TDPMEditViewFrame2.ThemeChanged;
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

procedure TDPMEditViewFrame2.ViewDeselected;
begin
  // The view tab was deselected.
  FLogger.Debug('DPMIDE : View Deselected');
  platformChangeDetectTimer.Enabled := false;
  FFirstView := true;
end;

procedure TDPMEditViewFrame2.ViewSelected;
begin
  FLogger.Debug('DPMIDE : View Selected');
  //For some reason this get's called twice for each time the view is selected.
//  if not IsProjectGroup then
    platformChangeDetectTimer.Enabled := true;
  //The first time the view is opened we want to switch to the installed packages tab
  if FFirstView then
  begin
    FFirstView := false;
//    if IsProjectGroup then
      DoPlatformChange(FSearchBar.Platform);
  end;

end;

{ TRowLayout }

procedure TRowLayout.Update(const ACanvas: TCanvas; const rowRect: TRect; const rowKind : TPackageRowKind);
var
  bUpdateLayout : boolean;
begin
  bUpdateLayout := rowRect.Width <> RowWidth;
  if bUpdateLayout then
  begin
    IconRect.Top := rowRect.Top + ((rowRect.Height - 24) div 2); //center vertically
    IconRect.Left := rowRect.Left + 10;  //TODO : These margins etc need to be scaled with dpi!
    IconRect.Width := 24;
    IconRect.Height := 24;

    VersionRect.Top := IconRect.Top;
    VersionRect.Right := rowRect.Right - 50;
    VersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    //TODO : Figure out a better way to ensure this will be correct
    VersionRect.Left := rowRect.Right - ACanvas.TextExtent('1.100.100-aplha123aaaaa').Width - 10;

    LatestVersionRect.Top := VersionRect.Bottom + 10;
    LatestVersionRect.Right := rowRect.Right - 50;
    LatestVersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    //TODO : Figure out a better way to ensure this will be correct
    LatestVersionRect.Left := VersionRect.Left;

    TitleRect.Top := IconRect.Top;
    TitleRect.Height := Abs(ACanvas.Font.Height) * 2;
    if rowKind in [rkInstalledPackage, rkImplicitPackage, rkAvailablePackage] then
      TitleRect.Left := rowRect.Left + 10 + iconRect.Width + 10
    else
      TitleRect.Left := rowRect.Left + 10;
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
    IconRect.Top := rowRect.Top + 2;
    IconRect.Height := 24;

    if rowKind in [rkInstalledPackage, rkImplicitPackage, rkAvailablePackage] then
      TitleRect.Left := rowRect.Left + 10 + iconRect.Width + 10
    else
      TitleRect.Left := rowRect.Left + 10;

    VersionRect.Top := IconRect.Top;
    VersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    LatestVersionRect.Top := VersionRect.Bottom + 10;
    LatestVersionRect.Height := Abs(ACanvas.Font.Height) + 10;

    TitleRect.Top := IconRect.Top;
    TitleRect.Height := Abs(ACanvas.Font.Height) * 2;
    DescriptionRect.Top := TitleRect.Bottom + 4;
    DescriptionRect.Bottom := rowRect.Bottom - 5;
  end;

end;

end.
