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

unit DPM.IDE.Wizard;

interface

uses
  System.Classes,
  ToolsApi,
  Vcl.ActnList,
  Vcl.Controls,
  Vcl.Menus,
  Spring.Container,
  DPM.Core.Cache.Interfaces,
  DPM.IDE.Logger,
  DPM.IDE.MessageService,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.EditorViewManager;

type
  TDPMWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard)
  private
    FStorageNotifierID : integer;
    FIDENotifier : integer;
    FProjectMenuNoftifierId : integer;
    FSBOMMenuNotifierId : integer;
    FThemeChangeNotifierId : integer;
    //One-shot IOTAIDENotifier that on the first FileNotification scans IOTAPackageServices
    //for design BPLs loaded from under our cache root and unloads them. The registry scrub
    //in the constructor only takes effect on the NEXT session - this catches orphans the
    //IDE has already re-loaded into the current session.
    FOrphanUnloaderNotifierId : integer;
    FEditorViewManager : IDPMEditorViewManager;
    FDPMIDEMessageService : IDPMIDEMessageService;
    FProjectTreeManager : IDPMProjectTreeManager;
    FLogger : IDPMIDELogger;
    FContainer : TContainer;
    //Held so we can call UnregisterAddInOptions on shutdown. Without the unregister, the
    //environment-options page entry sticks around with stale container refs if the plugin is
    //ever reloaded.
    FAddInOptions : INTAAddInOptions;
    // IDE-6: held so the DPM > Verify Package Cache menu has the
    // cache singleton on hand at click time.
    FPackageCacheForVerify : IPackageCache;
    //Top-level "DPM" menu inserted into the IDE main menu bar (after Tools). Held so
    //it can be freed on shutdown - freeing it frees its child items too. The backing
    //TActions are owned by INTAServices.ActionList and must NOT be freed here.
    FDPMTopMenu : TMenuItem;
    procedure InitContainer;
    procedure RegisterTopLevelMenu(const menuServices : INTAServices;
                                   const packageCache : IPackageCache; const imageIdx : integer);
    procedure DPMVerifyCacheClick(Sender : TObject);
    procedure DPMOptionsClick(Sender : TObject);
    procedure DPMGenerateSBOMProjectClick(Sender : TObject);
    procedure DPMGenerateSBOMProjectUpdate(Sender : TObject);
    procedure DPMGenerateSBOMGroupClick(Sender : TObject);
    procedure DPMGenerateSBOMGroupUpdate(Sender : TObject);
  protected

    //IOTAWizard
    procedure Execute;
    function GetIDString : string;
    function GetName : string;
    function GetState : TWizardState;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;


    procedure DPMMenuClick(Sender : TObject);

    procedure DPMActionUpdate(Sender : TObject);

    //Scrubs Known-Packages registry entries whose BPL path sits under our cache root.
    //Anything in there pointing into our cache can only be a leftover from a prior (crashed)
    //IDE session - we go straight to the registry, no IOTAPackageServices needed, so this is
    //safe to call synchronously from the wizard constructor.
    procedure CleanupOrphanedKnownPackages(const cacheRoot : string);

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  System.StrUtils,
  System.UITypes,
  System.Win.Registry,
  Winapi.Windows,
  Vcl.Dialogs,
  Vcl.Graphics,
  VSoft.CancellationToken,
  Vcl.Imaging.pngimage,
  Spring.Container.Registration,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Init,
  DPM.Core.Trust.Prompt,
  DPM.Core.Utils.Config,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.IDE.Trust.Prompt,
  DPM.IDE.ProjectController,
  DPM.IDE.ProjectStorageNotifier,
  DPM.IDE.IDENotifier,
  DPM.IDE.ProjectMenu,
  DPM.IDE.SBOM.Menu,
  DPM.IDE.AddInOptions,
  DPM.IDE.AddInOptionsHostForm,
  DPM.IDE.Options,
  DPM.IDE.InstallerContext,
  DPM.IDE.PathManager,
  DPM.IDE.EnvironmentVariableManager,
  DPM.IDE.ToolsAPI;

{$R DPM.IDE.Resources.res}

type
  //One-shot IOTAIDENotifier that runs the IOTAPackageServices-based orphan unload on the
  //first FileNotification - by which point Delphi has finished processing Known Packages and
  //the loaded BPLs are visible. 
  TDPMOrphanPackageUnloader = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    FDone : boolean;
    FCacheRoot : string;
    FLogger : IDPMIDELogger;
    procedure UnloadOrphans;
  protected
    procedure FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
    procedure AfterCompile(Succeeded : Boolean);
    procedure BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
  public
    constructor Create(const cacheRoot : string; const logger : IDPMIDELogger);
  end;

constructor TDPMOrphanPackageUnloader.Create(const cacheRoot : string; const logger : IDPMIDELogger);
begin
  inherited Create;
  FCacheRoot := cacheRoot;
  FLogger := logger;
  FDone := false;
end;

procedure TDPMOrphanPackageUnloader.AfterCompile(Succeeded : Boolean);
begin
end;

procedure TDPMOrphanPackageUnloader.BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
begin
end;

procedure TDPMOrphanPackageUnloader.FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
begin
  if FDone then
    exit;
  FDone := true;
  try
    UnloadOrphans;
  except
    on e : Exception do
      FLogger.Error('Exception during orphan BPL cleanup : ' + e.Message);
  end;
end;

procedure TDPMOrphanPackageUnloader.UnloadOrphans;
var
  svc : IOTAPackageServices;
  cacheRootWithDelim : string;
  info : IOTAPackageInfo;
  i : integer;
  candidate : string;
  found : TStringList;
  uninstallOk : boolean;
  cleanedCount : integer;
begin
  if FCacheRoot = '' then
    exit;
  cacheRootWithDelim := IncludeTrailingPathDelimiter(FCacheRoot);

  if not Supports(BorlandIDEServices, IOTAPackageServices, svc) then
    exit;

  //First pass: collect matching paths. Don't uninstall inside the loop - UninstallPackage
  //mutates PackageCount and invalidates the index-based walk.
  found := TStringList.Create;
  try
    for i := 0 to svc.PackageCount - 1 do
    begin
      info := svc.Package[i];
      if info = nil then
        continue;
      //IDE packages are loaded by the IDE itself and cannot be uninstalled by the user - skip
      //defensively even though they won't match our cache path anyway.
      if info.IDEPackage then
        continue;
      candidate := info.FileName;
      if candidate = '' then
        continue;
      if StartsText(cacheRootWithDelim, candidate) then
        found.Add(candidate);
    end;

    if found.Count = 0 then
      exit;

    cleanedCount := 0;
    for i := 0 to found.Count - 1 do
    begin
      uninstallOk := false;
      try
        uninstallOk := svc.UninstallPackage(found[i]);
      except
        on e : Exception do
          FLogger.Error('Exception unloading orphaned design BPL [' + found[i] + '] : ' + e.Message);
      end;
      if uninstallOk then
      begin
        FLogger.Information('Unloaded orphaned design BPL [' + ExtractFileName(found[i]) + ']');
        Inc(cleanedCount);
      end
      else
        FLogger.Warning('IDE reported failure unloading orphaned design BPL [' + ExtractFileName(found[i]) + ']');
    end;

    if cleanedCount > 0 then
      FLogger.Success('Unloaded ' + IntToStr(cleanedCount) + ' orphaned design BPL(s) from previous session');
  finally
    found.Free;
  end;
end;

{ TDPMWizard }

procedure TDPMWizard.CleanupOrphanedKnownPackages(const cacheRoot : string);
var
  baseKey : string;
  knownPackagesKey : string;
  cacheRootWithDelim : string;
  reg : TRegistry;
  valueNames : TStringList;
  i : integer;
  cleanedCount : integer;
  svc : IOTAServices;
  bplPath : string;
begin
  if cacheRoot = '' then
    exit;
  cacheRootWithDelim := IncludeTrailingPathDelimiter(cacheRoot);

  if not Supports(BorlandIDEServices, IOTAServices, svc) then
    exit;
  //GetBaseRegistryKey returns the IDE's HKCU subpath, e.g. 'Software\Embarcadero\BDS\23.0'.
  baseKey := svc.GetBaseRegistryKey;
  if baseKey = '' then
    exit;
  knownPackagesKey := baseKey + '\Known Packages';

  cleanedCount := 0;
  reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(knownPackagesKey, false) then
      exit;
    valueNames := TStringList.Create;
    try
      reg.GetValueNames(valueNames);
      //Each value name is the full BPL path; the value's data is the human-readable description.
      for i := 0 to valueNames.Count - 1 do
      begin
        bplPath := valueNames[i];
        if not StartsText(cacheRootWithDelim, bplPath) then
          continue;
        try
          if reg.DeleteValue(bplPath) then
          begin
            FLogger.Information('Removed orphaned Known-Packages entry [' + ExtractFileName(bplPath) + ']');
            Inc(cleanedCount);
          end
          else
            FLogger.Warning('Failed to remove Known-Packages registry value [' + bplPath + ']');
        except
          on e : Exception do
            FLogger.Error('Exception removing Known-Packages registry value [' + bplPath + '] : ' + e.Message);
        end;
      end;
    finally
      valueNames.Free;
    end;
    reg.CloseKey;
  finally
    reg.Free;
  end;

  if cleanedCount > 0 then
    FLogger.Success('Removed ' + IntToStr(cleanedCount) + ' orphaned Known-Packages registry entry/entries from previous session');
end;

procedure TDPMWizard.InitContainer;
begin
  try
    FContainer := TContainer.Create;
    FContainer.RegisterType<IDPMIDEOptions, TDPMIDEOptions>.AsSingleton();
    FContainer.RegisterType<IDPMIDEMessageService,TDPMIDEMessageService>.AsSingleton();
    FContainer.RegisterType<TDPMIDELogger>.Implements<IDPMIDELogger>.Implements<ILogger>.AsSingleton();
    FContainer.RegisterType<IDPMProjectTreeManager, TDPMProjectTreeManager>.AsSingleton();
    FContainer.RegisterType<IDPMEditorViewManager, TDPMEditorViewManager>.AsSingleton();
    FContainer.RegisterType<IDPMIDEProjectController,TDPMIDEProjectController>.AsSingleton();
    FContainer.RegisterType<IDPMIDEPathManager,TDPMIDEPathManager>.AsSingleton();
    FContainer.RegisterType<IDPMIDEEnvironmentVariableManager,TDPMIDEEnvironmentVariableManager>.AsSingleton();

    DPM.Core.Init.InitCore(FContainer,
      //replaces core registration of the IPackageInstallerContext implementation,
      //and swaps the non-interactive trust-prompt default for the IDE modal form.
      procedure(const container : TContainer)
      begin
        container.RegisterType<IPackageInstallerContext, TDPMIDEPackageInstallerContext>().AsSingleton();
        container.RegisterType<ITrustPromptStrategy, TIdeTrustPromptStrategy>;
      end);
    FContainer.Build;
  except
    on e : Exception do
    begin
     FLogger.Error('Error setting up the container : ' + e.Message);
    end;
  end;
end;


procedure TDPMWizard.AfterSave;
begin

end;

procedure TDPMWizard.BeforeSave;
begin

end;

constructor TDPMWizard.Create;
var
  storageNotifier : IOTAProjectFileStorageNotifier;
  ideNotifier : IOTAIDENotifier;
  orphanUnloader : IOTAIDENotifier;
  projMenuNotifier : IOTAProjectMenuItemCreatorNotifier;
  sbomMenuNotifier : IOTAProjectMenuItemCreatorNotifier;
  options : INTAAddInOptions;
  projectController : IDPMIDEProjectController;
  dpmIDEOptions : IDPMIDEOptions;
  bmp : TBitmap;
  png : TPngImage;
  idx : integer;
  menuServices : INTAServices;
  configManager : IConfigurationManager;
  dpmConfig : IConfiguration;
  packageCache : IPackageCache;
begin
  InitContainer;
  FLogger := FContainer.Resolve<IDPMIDELogger>;
  dpmIDEOptions := FContainer.Resolve<IDPMIDEOptions>;
  if FileExists(dpmIDEOptions.FileName) then
    dpmIDEOptions.LoadFromFile()
  else
  begin
    TConfigUtils.EnsureDefaultConfigDir;
    dpmIDEOptions.SaveToFile(); //create the file
  end;

  FLogger.Verbosity := dpmIDEOptions.LogVerbosity;

  FEditorViewManager := FContainer.Resolve<IDPMEditorViewManager>;
  FDPMIDEMessageService := FContainer.Resolve<IDPMIDEMessageService>;
  FProjectTreeManager := FContainer.Resolve<IDPMProjectTreeManager>;

  {$IF CompilerVersion >= 32.0}
  FThemeChangeNotifierId := (BorlandIDEServices as IOTAIDEThemingServices).AddNotifier(FEditorViewManager as INTAIDEThemingServicesNotifier);
  {$ELSE}
  FThemeChangeNotifierId := -1;
  {$IFEND}

  //If a previous IDE session crashed while DPM design BPLs were loaded, the Known Packages
  //registry under HKCU\<IDE base>\Known Packages still lists them. The IDE has already
  //read that key by the time we get here, so we can't stop it loading the stale BPLs THIS
  //session - but scrubbing the registry entries that point into our cache stops the next
  //IDE start from picking them up again.
  //
  //The package cache's Location is normally wired by TPackageInstaller.Initialize which only
  //runs when a project is installed/restored - so we have to load the config and set it here
  //or the cleanup can't resolve the cache root to match against.
  FOrphanUnloaderNotifierId := -1;
  configManager := FContainer.Resolve<IConfigurationManager>;
  configManager.EnsureDefaultConfig;
  dpmConfig := configManager.LoadConfig(TConfigUtils.GetDefaultConfigFileName);
  if dpmConfig <> nil then
  begin
    packageCache := FContainer.Resolve<IPackageCache>;
    packageCache.Location := dpmConfig.PackageCacheLocation;
    CleanupOrphanedKnownPackages(packageCache.PackagesFolder);
    //The IDE has already cached the Known-Packages list and may have loaded stale BPLs from
    //our cache into this session - the registry scrub above won't undo that. Register a
    //one-shot notifier that on the first FileNotification (by which point IOTAPackageServices
    //has been populated) walks the loaded package list and unloads anything pointing into
    //our cache. UninstallPackage also removes the Known-Packages registry entry, so this
    //path is both more thorough and self-healing.
    if packageCache.PackagesFolder <> '' then
    begin
      orphanUnloader := TDPMOrphanPackageUnloader.Create(packageCache.PackagesFolder, FLogger);
      FOrphanUnloaderNotifierId := (BorlandIDEServices as IOTAServices).AddNotifier(orphanUnloader);
    end;
  end;

  projectController := FContainer.Resolve<IDPMIDEProjectController>;

  ideNotifier := TDPMIDENotifier.Create(FLogger, projectController);
  FIDENotifier := (BorlandIDEServices as IOTAServices).AddNotifier(ideNotifier);

  projMenuNotifier := TDPMProjectMenuNotifier.Create(FEditorViewManager);
  FProjectMenuNoftifierId := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(projMenuNotifier);

  //Phase 4: separate notifier for the Generate SBOM... menu so we don't widen
  //TDPMProjectMenuNotifier's constructor. The SBOM menu needs the container to
  //resolve ISbomGenerator at click time; the editor-view manager doesn't.
  sbomMenuNotifier := TDPMSBOMProjectMenuNotifier.Create(FContainer, FLogger);
  FSBOMMenuNotifierId := (BorlandIDEServices as IOTAProjectManager).AddMenuItemCreatorNotifier(sbomMenuNotifier);

  options := TDPMAddinOptions.Create(FContainer);
  FAddInOptions := options;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(FAddInOptions);

  storageNotifier := TDPMProjectStorageNotifier.Create(FLogger, projectController);
  FStorageNotifierID := (BorlandIDEServices as IOTAProjectFileStorage).AddNotifier(storageNotifier);

  menuServices := (BorlandIDEServices as INTAServices);


  bmp := TBitmap.Create;
  png := TPngImage.Create;
  try
    png.LoadFromResourceName(HInstance, 'DPMLOGO_16');
    png.AssignTo(bmp);
    bmp.AlphaFormat:=afIgnored;
    idx := menuServices.AddMasked(bmp, clNone, 'DPM');
  finally
    bmp.Free;
    png.Free;
  end;

  //Build the top-level "DPM" menu (after Tools) with all DPM commands, plus the
  //toolbar button. Replaces the old per-command AddActionMenu('ProjectOptionsItem')
  //entries that buried these under the Project menu.
  RegisterTopLevelMenu(menuServices, packageCache, idx);
end;

procedure TDPMWizard.RegisterTopLevelMenu(const menuServices : INTAServices;
                                          const packageCache : IPackageCache; const imageIdx : integer);
var
  toolsItem : TMenuItem;
  manageAction : TAction;
  sbomGroupAction : TAction;
  sbomProjectAction : TAction;
  optionsAction : TAction;
  verifyAction : TAction;
  optionsSeparatorItem : TMenuItem;
  separatorItem : TMenuItem;

  function CreateAction(const aName, aCaption, aHint : string;
                        const aExecute, aUpdate : TNotifyEvent) : TAction;
  begin
    result := TAction.Create(menuServices.ActionList);
    result.Name := aName;
    result.Caption := aCaption;
    result.Category := 'DPM';
    result.Hint := aHint;
    result.OnExecute := aExecute;
    if Assigned(aUpdate) then
      result.OnUpdate := aUpdate;
  end;

  procedure AddChild(const aAction : TAction);
  var
    item : TMenuItem;
  begin
    item := TMenuItem.Create(FDPMTopMenu);
    item.Action := aAction;
    FDPMTopMenu.Add(item);
  end;

begin
  FPackageCacheForVerify := packageCache;

  //If the design BPL is reloaded without restarting the IDE, the constructor runs
  //again - bail if we've already inserted the menu so we don't end up with two.
  if TToolsApiUtils.FindTopLevelMenu('DPMTopLevelMenu') <> nil then
    exit;

  FDPMTopMenu := TMenuItem.Create(menuServices.MainMenu);
  FDPMTopMenu.Name := 'DPMTopLevelMenu';
  FDPMTopMenu.Caption := 'DPM';

  manageAction := CreateAction('actDPMPackageManager', 'Manage DPM Packages',
    'Manage DPM Packages...', Self.DPMMenuClick, Self.DPMActionUpdate);
  AddChild(manageAction);

  //Hidden until a saved .groupproj is open - DPMGenerateSBOMGroupUpdate flips it.
  //Start invisible so it doesn't flash before the first OnUpdate fires.
  sbomGroupAction := CreateAction('actDPMGenerateSBOMGroup', 'Generate SBOM for Project Group',
    'Generate an aggregated SBOM for the open project group.',
    Self.DPMGenerateSBOMGroupClick, Self.DPMGenerateSBOMGroupUpdate);
  sbomGroupAction.Visible := false;
  AddChild(sbomGroupAction);

  sbomProjectAction := CreateAction('actDPMGenerateSBOMProject', 'Generate SBOM for Project',
    'Generate an SBOM for the active project.',
    Self.DPMGenerateSBOMProjectClick, Self.DPMGenerateSBOMProjectUpdate);
  AddChild(sbomProjectAction);

  optionsSeparatorItem := TMenuItem.Create(FDPMTopMenu);
  optionsSeparatorItem.Caption := '-';
  FDPMTopMenu.Add(optionsSeparatorItem);

  optionsAction := CreateAction('actDPMOptions', 'DPM Options...',
    'Configure DPM package sources and IDE options.',
    Self.DPMOptionsClick, nil);
  AddChild(optionsAction);

  separatorItem := TMenuItem.Create(FDPMTopMenu);
  separatorItem.Caption := '-';
  FDPMTopMenu.Add(separatorItem);

  verifyAction := CreateAction('actDPMVerifyCache', 'Verify DPM Package Cache',
    'Re-hash and re-verify every package in the DPM cache.',
    Self.DPMVerifyCacheClick, nil);
  AddChild(verifyAction);

  //Image index must be assigned after the item is added or the image never shows.
  manageAction.ImageIndex := imageIdx;

  //Insert immediately after the Tools menu. 'ToolsMenu' is the usual Name but it isn't
  //guaranteed across IDE versions / localisations, so fall back to a Caption match and
  //finally to appending at the end rather than failing.
  toolsItem := TToolsApiUtils.FindTopLevelMenu('ToolsMenu');
  if toolsItem = nil then
    toolsItem := TToolsApiUtils.FindTopLevelMenu('Tools');
  if toolsItem <> nil then
    menuServices.MainMenu.Items.Insert(toolsItem.MenuIndex + 1, FDPMTopMenu)
  else
    menuServices.MainMenu.Items.Add(FDPMTopMenu);

  menuServices.NewToolbar('DPMPackageManager', 'DPM Package Manager');
  menuServices.AddToolButton('DPMPackageManager', 'ShowDPMButton', manageAction);
end;

procedure TDPMWizard.DPMGenerateSBOMProjectClick(Sender : TObject);
var
  activeProject : IOTAProject;
begin
  activeProject := TToolsApiUtils.GetActiveProject;
  if activeProject = nil then
    exit;
  TDPMSBOMProjectMenu.RunSBOM(FContainer, FLogger, activeProject.FileName);
end;

procedure TDPMWizard.DPMGenerateSBOMProjectUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := TToolsApiUtils.IsProjectAvailable;
end;

procedure TDPMWizard.DPMGenerateSBOMGroupClick(Sender : TObject);
var
  projectGroup : IOTAProjectGroup;
begin
  projectGroup := TToolsApiUtils.GetMainProjectGroup;
  if projectGroup = nil then
    exit;
  TDPMSBOMProjectMenu.RunSBOM(FContainer, FLogger, projectGroup.FileName);
end;

procedure TDPMWizard.DPMGenerateSBOMGroupUpdate(Sender : TObject);
var
  projectGroup : IOTAProjectGroup;
  visible : boolean;
begin
  //Only meaningful when a real, saved .groupproj is open. MainProjectGroup returns an
  //implicit group even for a single loose .dproj, so group<>nil alone isn't enough -
  //the extension + FileExists check excludes the unsaved/implicit case.
  projectGroup := TToolsApiUtils.GetMainProjectGroup;
  visible := (projectGroup <> nil) and (projectGroup.FileName <> '') and
             SameText(ExtractFileExt(projectGroup.FileName), '.groupproj') and
             FileExists(projectGroup.FileName);
  TAction(Sender).Visible := visible;
  TAction(Sender).Enabled := visible;
end;

procedure TDPMWizard.DPMOptionsClick(Sender : TObject);
var
  optionsHost : TDPMOptionsHostForm;
  configManager : IConfigurationManager;
  ideOptions : IDPMIDEOptions;
begin
  //Reuse the same options host the search bar's settings button shows - it wraps the
  //TDPMOptionsFrame (package sources + IDE options) with OK/Cancel and persists on OK.
  configManager := FContainer.Resolve<IConfigurationManager>;
  ideOptions := FContainer.Resolve<IDPMIDEOptions>;
  optionsHost := TDPMOptionsHostForm.Create(nil, configManager, FLogger, ideOptions,
    TConfigUtils.GetDefaultConfigFileName);
  try
    if optionsHost.ShowModal = mrOk then
      //Pick up any verbosity change immediately rather than waiting for the next session.
      FLogger.Verbosity := ideOptions.LogVerbosity;
  finally
    optionsHost.Free;
  end;
end;

procedure TDPMWizard.DPMVerifyCacheClick(Sender : TObject);
var
  cancellationTokenSource : ICancellationTokenSource;
  failures : integer;
begin
  if FPackageCacheForVerify = nil then
    exit;

  //Drive the verify through the DPM message window: StartVerifyCache shows it and wires
  //the Cancel button to this token source. The verify runs on the main thread (the message
  //service/form are main-thread only) but FullReVerify logs per-package progress, and each
  //log line pumps the message loop - so the window stays live, the cancel click is seen,
  //and FullReVerify breaks at the next package boundary. Replaces the old blocking MessageDlg
  //that made a large cache look like the IDE had hung.
  cancellationTokenSource := TCancellationTokenSourceFactory.Create;
  FLogger.StartVerifyCache(cancellationTokenSource);
  failures := 0;
  try
    failures := FPackageCacheForVerify.FullReVerify(cancellationTokenSource.Token);
  finally
    //Cancelled or failed -> keep the window open so the user sees why; clean success ->
    //let it auto-close per the user's log options.
    FLogger.EndVerifyCache((failures = 0) and (not cancellationTokenSource.Token.IsCancelled));
  end;
end;

destructor TDPMWizard.Destroy;
begin
  inherited;
end;

procedure TDPMWizard.Destroyed;
begin
  //Free the top-level DPM menu - this frees its child items too. The backing TActions
  //are owned by INTAServices.ActionList and freed by the IDE, so we must NOT free them.
  if FDPMTopMenu <> nil then
    FreeAndNil(FDPMTopMenu);

  if FStorageNotifierId > -1 then
    (BorlandIDEServices as IOTAProjectFileStorage).RemoveNotifier(FStorageNotifierId);
  if FIDENotifier > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FIDENotifier);
  if FOrphanUnloaderNotifierId > -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FOrphanUnloaderNotifierId);

  //Both menu notifiers were registered via IOTAProjectManager.AddMenuItemCreatorNotifier - they
  //MUST be removed via the same service. Calling IOTAServices.RemoveNotifier with an ID issued
  //by a different registry either silently fails (leaving them attached to IOTAProjectManager
  //until IDE shutdown) or - worse - removes a different notifier whose IOTAServices index
  //happens to collide numerically.
  if FProjectMenuNoftifierId > -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuItemCreatorNotifier(FProjectMenuNoftifierId);

  if FSBOMMenuNotifierId > -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuItemCreatorNotifier(FSBOMMenuNotifierId);

  {$IF CompilerVersion >= 32.0}
  if FThemeChangeNotifierId > -1 then
    (BorlandIDEServices as IOTAIDEThemingServices).RemoveNotifier(FThemeChangeNotifierId);
  {$IFEND}

  if FAddInOptions <> nil then
  begin
    (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(FAddInOptions);
    FAddInOptions := nil;
  end;

  if FProjectTreeManager <> nil then
  begin
    FProjectTreeManager.Shutdown;
    FProjectTreeManager := nil;
  end;

  FDPMIDEMessageService.ShutDown;
  FEditorViewManager.Destroyed; //don't try to resolve this here, errors in the rtl on 10.2
  FEditorViewManager := nil;

end;

procedure TDPMWizard.DPMActionUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := TToolsApiUtils.IsProjectAvailable;
end;

procedure TDPMWizard.DPMMenuClick(Sender: TObject);
begin
  FEditorViewManager.ShowViewForProject(TToolsApiUtils.GetMainProjectGroup,TToolsApiUtils.GetActiveProject);
end;

procedure TDPMWizard.Execute;
begin
end;

function TDPMWizard.GetIDString : string;
begin
  result := 'DPM.IDE';
end;

function TDPMWizard.GetName : string;
begin
  result := 'DPM';
end;

function TDPMWizard.GetState : TWizardState;
begin
  result := [wsEnabled];
end;

procedure TDPMWizard.Modified;
begin

end;

end.

