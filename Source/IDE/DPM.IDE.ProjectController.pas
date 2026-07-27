unit DPM.IDE.ProjectController;

interface

uses
  System.Classes,
  ToolsApi,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.IDE.Logger,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.CopyLocal,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.EditorViewManager,
  DPM.Core.Package.Installer.Interfaces;

type
  TProjectMode = (pmNone, pmSingle, pmGroup);

  IDPMIDEProjectController = interface
  ['{860448A2-1015-44A7-B051-0D29692B9986}']
    //IDENotifier
    procedure ProjectOpening(const fileName : string); //does restore!
    procedure ProjectClosed(const fileName : string);

    procedure ProjectGroupClosed;

    //StorageNotifier
    procedure ProjectCreating(const fileName : string);
    procedure ProjectLoaded(const fileName : string);
    procedure ProjectClosing(const fileName : string);
    procedure ProjectSaving(const fileName : string);

    procedure BeginLoading(const mode : TProjectMode);
    procedure EndLoading(const mode : TProjectMode);

    procedure ActivePlatformChanged(const platform : string);
    procedure ActiveProjectChanged(const project : IOTAProject);

    //Called after a successful IDE compile/build (the IDE doesn't run the DPM.CopyLocal.targets
    //AfterBuild target, so we trigger copylocal here for in-IDE builds).
    procedure ProjectBuilt(const project : IOTAProject);
  end;

  // The Project controller is used to receive notifications from the IDE and funnel
  // them to the various parts of the IDE integration - views, project tree etc.

  TDPMIDEProjectController = class(TInterfacedObject, IDPMIDEProjectController)
  private
    FLogger : IDPMIDELogger;
    FEditorViewManager : IDPMEditorViewManager;
    FProjectTreeManager : IDPMProjectTreeManager;
    FProjectMode : TProjectMode;
    FPackageInstaller : IPackageInstaller;
    FInstallerContext : IPackageInstallerContext;
    FCopyLocalService : ICopyLocalService;
    FCancellationTokenSource : ICancellationTokenSource;
    FLastResult : boolean;
    //True once StartRestore has actually been called for the current load. EndLoading used to
    //call EndRestore unconditionally, which meant TaskDone ran - and could force the log window
    //open - for loads where no restore was ever started.
    FRestoreStarted : boolean;
    //Projects the IDE told us it is CREATING (IOTAProjectFileStorageNotifier.CreatingProject).
    //A brand new project must never be restored, even if its default name happens to collide
    //with a real .dproj left on disk (eg a stale Project1.dproj in the default projects folder).
    //Keyed on LowerCase(fileName).
    FCreatingProjects : ISet<string>;
  protected
    //from IDENotifier
    procedure ProjectOpening(const fileName : string);
    procedure ProjectClosed(const fileName : string);
    procedure ProjectGroupClosed;

    procedure BeginLoading(const mode : TProjectMode);
    procedure EndLoading(const mode : TProjectMode);

    //from StorageNotifier
    procedure ProjectLoaded(const fileName : string);
    procedure ProjectClosing(const fileName : string);
    procedure ProjectCreating(const fileName : string);
    procedure ProjectSaving(const fileName : string);

    procedure RestoreProject(const fileName : string);
    procedure ActivePlatformChanged(const platform : string);
    procedure ActiveProjectChanged(const project : IOTAProject);
    procedure ProjectBuilt(const project : IOTAProject);

  public
    constructor Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager; const context : IPackageInstallerContext;
                       const copyLocalService : ICopyLocalService);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  DPM.Core.Types,
  DPM.Core.Options.Common,
  DPM.Core.Options.Restore,
  DPM.Core.Options.CopyLocal,
  DPM.Core.Utils.System,
  DPM.IDE.Types;

{ TDPMIDEProjectController }

procedure TDPMIDEProjectController.ActivePlatformChanged(const platform: string);
begin
  FEditorViewManager.ActivePlatformChanged(platform);
end;

procedure TDPMIDEProjectController.ActiveProjectChanged(const project: IOTAProject);
begin
  FEditorViewManager.ActiveProjectChanged(project);
end;

procedure TDPMIDEProjectController.ProjectBuilt(const project: IOTAProject);
var
  projectOptions : IOTAProjectOptions;
  configs : IOTAProjectOptionsConfigurations;
  config : IOTABuildConfiguration;
  platformName : string;
  outputDir : string;
  options : TCopyLocalOptions;
begin
  if project = nil then
    exit;
  if not SameText(ExtractFileExt(project.FileName), '.dproj') then
    exit;
  //Cheap guard - skip projects that don't use dpm so a normal compile costs nothing (same check
  //the restore path uses).
  if not FPackageInstaller.ProjectHasPackageReferences(project.FileName, IDECompilerVersion) then
    exit;

  projectOptions := project.ProjectOptions;
  if not Supports(projectOptions, IOTAProjectOptionsConfigurations, configs) then
    exit;
  config := configs.ActiveConfiguration;
  if config = nil then
    exit;

  //Honour the same opt-out the msbuild target uses.
  if SameText(Trim(config.GetValue('DPMCopyLocalDisable')), 'true') then
    exit;

  platformName := project.CurrentPlatform;

  //DCC_ExeOutput is the output folder, commonly carrying $(Platform)/$(Config) macros. The
  //copylocal service resolves relative paths but not msbuild macros, so expand the two we know.
  outputDir := config.GetValue('DCC_ExeOutput');
  outputDir := StringReplace(outputDir, '$(Platform)', platformName, [rfReplaceAll, rfIgnoreCase]);
  outputDir := StringReplace(outputDir, '$(Config)', config.Name, [rfReplaceAll, rfIgnoreCase]);

  options := TCopyLocalOptions.Create;
  try
    options.ApplyCommon(TCommonOptions.Default);
    options.ProjectPath := project.FileName;
    options.Platform := ProjectPlatformToDPMPlatform(platformName);
    options.Config := config.Name;
    options.OutputDir := outputDir;
    options.CompilerVersion := IDECompilerVersion;
    options.UsePackages := SameText(Trim(config.GetValue('UsePackages')), 'true');
    options.RuntimePackages := config.GetValue('DCC_UsePackage');
    if not options.Validate(FLogger) then
      exit;
    try
      FCopyLocalService.CopyLocal(FCancellationTokenSource.Token, options);
    except
      on e : Exception do
        FLogger.Error('Error during copylocal for [' + ExtractFileName(project.FileName) + '] : ' + e.Message);
    end;
  finally
    options.Free;
  end;
end;

procedure TDPMIDEProjectController.BeginLoading(const mode: TProjectMode);
begin
  FProjectMode := mode;
  FLogger.Debug('ProjectController.BeginLoading : ' + GetEnumName(TypeInfo(TProjectMode),Ord(mode)));
  FCancellationTokenSource.Reset;
  FLastResult := true;
  FRestoreStarted := false;
//  FInstallerContext.Clear; //should really only do this on closing
end;

constructor TDPMIDEProjectController.Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager; const context : IPackageInstallerContext;
                       const copyLocalService : ICopyLocalService);
begin
  inherited Create;
  FProjectMode := TProjectMode.pmNone;
  FLogger := logger;
  FPackageInstaller := packageInstaller;
  FEditorViewManager := editorViewManager;
  FProjectTreeManager := projectTreeManager;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;
  FInstallerContext := context;
  FCopyLocalService := copyLocalService;
  FCreatingProjects := TCollections.CreateSet<string>;
end;

destructor TDPMIDEProjectController.Destroy;
begin
  FEditorViewManager := nil;
  FPackageInstaller := nil;
  FProjectTreeManager := nil;
  inherited;
end;

procedure TDPMIDEProjectController.EndLoading(const mode: TProjectMode);
begin
  FLogger.Debug('ProjectController.EndLoading : ' + GetEnumName(TypeInfo(TProjectMode),Ord(mode)));
  FProjectMode := pmNone;
  //Only close off a restore we actually started - otherwise TaskDone runs for loads that had
  //nothing to do (a new project, or a project with no package references) and can surface the
  //log window carrying a stale FLastResult from an earlier project in the same group.
  if FRestoreStarted then
  begin
    FRestoreStarted := false;
    FLogger.EndRestore(FLastResult);
  end;
  FProjectTreeManager.EndLoading;
end;

procedure TDPMIDEProjectController.ProjectClosed(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectClosed : ' + fileName);
  FCreatingProjects.Remove(LowerCase(fileName));
  FProjectTreeManager.ProjectClosed(FileName);
  FEditorViewManager.ProjectClosed(FileName);
  FInstallerContext.RemoveProject(fileName);
end;


procedure TDPMIDEProjectController.ProjectOpening(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectOpening : ' + fileName);
  RestoreProject(fileName);
end;

procedure TDPMIDEProjectController.ProjectClosing(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectClosing : ' + fileName);
// using FileClosing as it fires earlier
end;

procedure TDPMIDEProjectController.ProjectCreating(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectCreating : ' + fileName);
  //The IDE's authoritative "this is a new project" signal - remember it so RestoreProject
  //skips it no matter what happens to be on disk at that path.
  FCreatingProjects.Add(LowerCase(fileName));
  ProjectLoaded(fileName);
end;


procedure TDPMIDEProjectController.ProjectGroupClosed;
begin
  //Belt and braces - a stale entry must never suppress restore for a project genuinely opened
  //later in the same IDE session.
  FCreatingProjects.Clear;
  FProjectTreeManager.ProjectGroupClosed;
  FEditorViewManager.ProjectGroupClosed;
  FInstallerContext.Clear;
end;

procedure TDPMIDEProjectController.ProjectLoaded(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectLoaded : ' + fileName);
  //queue the project for loading in the tree.
  FProjectTreeManager.ProjectLoaded(fileName);

  //this will be pmNone when reloading after external edit
  if FProjectMode = pmNone then
  begin
    FProjectTreeManager.EndLoading; //force tree update
    FEditorViewManager.ProjectLoaded(fileName);
  end;
end;

procedure TDPMIDEProjectController.ProjectSaving(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectSaving : ' + fileName);
  //Clear the lot, not just this key : a new project saved under a different name (Project1.dproj
  //-> Foo.dproj) arrives here as the NEW name, so removing by key would leave the old one behind
  //to wrongly suppress restore if a real Project1.dproj is opened later in the session. Any other
  //still-unsaved new project is not on disk, so the FileExists guard in RestoreProject covers it.
  FCreatingProjects.Clear;
end;

procedure TDPMIDEProjectController.RestoreProject(const fileName: string);
var
  options : TRestoreOptions;
begin
  FLogger.Debug('ProjectController.RestoreProject : ' + fileName);

  if FCancellationTokenSource.Token.IsCancelled then
    exit;

  // A project the IDE is in the middle of creating is not on disk and has no packages - never
  // restore it, whatever happens to be at that path.
  if FCreatingProjects.Contains(LowerCase(fileName)) then
  begin
    TSystemUtils.OutputDebugString('ProjectController.RestoreProject : new project, skipping : ' + fileName);
    FLogger.Debug('ProjectController.RestoreProject : new project, skipping : ' + fileName);
    exit;
  end;

  // Don't trigger a restore (which shows the log window) for projects that don't use dpm.
  // Keeps the log window hidden when loading projects/groups with no PackageReferences.
  if not FPackageInstaller.ProjectHasPackageReferences(fileName, IDECompilerVersion) then
  begin
    FLogger.Debug('ProjectController.RestoreProject : no package references, skipping : ' + fileName);
    exit;
  end;

  // Last check before we commit - StartRestore below is what shows the log window, and the core
  // has nothing useful to say about a file that isn't there. Narrows the window between the
  // check above and the restore to nothing we can act on.
  if not FileExists(fileName) then
  begin
    TSystemUtils.OutputDebugString('ProjectController.RestoreProject : project file missing, skipping : ' + fileName);
    FLogger.Debug('ProjectController.RestoreProject : project file missing, skipping : ' + fileName);
    exit;
  end;

  options := TRestoreOptions.Create;
  try
    options.ApplyCommon(TCommonOptions.Default);
    options.ProjectPath := fileName;
    options.Validate(FLogger);
    options.CompilerVersion := IDECompilerVersion;
    FLogger.StartRestore(FCancellationTokenSource);
    FRestoreStarted := true;
    FLastResult := FLastResult and FPackageInstaller.Restore(FCancellationTokenSource.Token, options, FInstallerContext);
  finally
    options.Free;
  end;
end;


end.
