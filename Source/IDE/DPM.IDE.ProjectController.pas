unit DPM.IDE.ProjectController;

interface

uses
  System.Classes,
  VSoft.CancellationToken,
  DPM.IDE.Logger,
  DPM.Core.Package.Interfaces,
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
    FCancellationTokenSource : ICancellationTokenSource;
    FLastResult : boolean;
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
  public
    constructor Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager; const context : IPackageInstallerContext);
    destructor Destroy;override;
  end;

implementation

uses
  System.TypInfo,
  DPM.Core.Options.Common,
  DPM.Core.Options.Restore,
  DPM.IDE.Types;

{ TDPMIDEProjectController }

procedure TDPMIDEProjectController.BeginLoading(const mode: TProjectMode);
begin
  FProjectMode := mode;
  FLogger.Debug('ProjectController.BeginLoading : ' + GetEnumName(TypeInfo(TProjectMode),Ord(mode)));
  FCancellationTokenSource.Reset;
  FLastResult := true;
  FInstallerContext.Clear;
end;

constructor TDPMIDEProjectController.Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager; const context : IPackageInstallerContext);
begin
  inherited Create;
  FProjectMode := TProjectMode.pmNone;
  FLogger := logger;
  FPackageInstaller := packageInstaller;
  FEditorViewManager := editorViewManager;
  FProjectTreeManager := projectTreeManager;
  FCancellationTokenSource := TCancellationTokenSourceFactory.Create;
  FInstallerContext := context;
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
  FLogger.EndRestore(FLastResult);
  FProjectTreeManager.EndLoading;
end;

procedure TDPMIDEProjectController.ProjectClosed(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectClosed : ' + fileName);
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
  ProjectLoaded(fileName);
end;


procedure TDPMIDEProjectController.ProjectGroupClosed;
begin
  FProjectTreeManager.ProjectGroupClosed;
  FEditorViewManager.ProjectGroupClosed;
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
// not sure we need to do anything.
end;

procedure TDPMIDEProjectController.RestoreProject(const fileName: string);
var
  options : TRestoreOptions;
begin
  FLogger.Debug('ProjectController.RestoreProject : ' + fileName);

  if FCancellationTokenSource.Token.IsCancelled then
    exit;
  options := TRestoreOptions.Create;
  options.ApplyCommon(TCommonOptions.Default);
  options.ProjectPath := fileName;
  options.Validate(FLogger);
  options.CompilerVersion := IDECompilerVersion;
  FLogger.StartRestore(FCancellationTokenSource);
  FLastResult := FLastResult and FPackageInstaller.Restore(FCancellationTokenSource.Token, options, FInstallerContext);
end;


end.
