unit DPM.IDE.ProjectController;

interface

uses
  System.Classes,
  DPM.IDE.Logger,
  DPM.Core.Package.Interfaces,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.EditorViewManager;

type
  TProjectMode = (pmNone, pmSingle, pmGroup);

  IDPMIDEProjectController = interface
  ['{860448A2-1015-44A7-B051-0D29692B9986}']
    //IDENotifier
    procedure FileOpening(const fileName : string);
    procedure FileOpened(const fileName : string);
    procedure FileClosed(const fileName : string);

    //StorageNotifier
    procedure ProjectCreating(const fileName : string);
    procedure ProjectLoaded(const fileName : string);
    procedure ProjectClosing(const fileName : string);
    procedure ProjectSaving(const fileName : string);

    procedure BeginLoading(const mode : TProjectMode);
    procedure EndLoading(const mode : TProjectMode);

  end;


  TDPMIDEProjectController = class(TInterfacedObject, IDPMIDEProjectController)
  private
    FLogger : IDPMIDELogger;
    FEditorViewManager : IDPMEditorViewManager;
    FProjectTreeManager : IDPMProjectTreeManager;
    FProjectMode : TProjectMode;
    FPackageInstaller : IPackageInstaller;
  protected
    //from IDENotifier
    procedure FileOpening(const fileName : string);
    procedure FileOpened(const fileName : string);
    procedure FileClosed(const fileName : string);

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
                       const projectTreeManager : IDPMProjectTreeManager);
    destructor Destroy;override;
  end;

implementation

uses
  System.TypInfo,
  VSoft.Awaitable,
  DPM.Core.Options.Common,
  DPM.Core.Options.Restore,
  DPM.IDE.Types;

{ TDPMIDEProjectController }

procedure TDPMIDEProjectController.BeginLoading(const mode: TProjectMode);
begin
  FProjectMode := mode;
  FLogger.Debug('ProjectController.BeginLoading : ' + GetEnumName(TypeInfo(TProjectMode),Ord(mode)));

end;

constructor TDPMIDEProjectController.Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager);
begin
  inherited Create;
  FProjectMode := TProjectMode.pmNone;
  FLogger := logger;
  FPackageInstaller := packageInstaller;
  FEditorViewManager := editorViewManager;
  FProjectTreeManager := projectTreeManager;
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
//  FLogger.Debug('ProjectController.EndLoading : ' + GetEnumName(TypeInfo(TProjectMode),Ord(mode)));
  FProjectMode := pmNone;
  FProjectTreeManager.NotifyEndLoading;
end;

procedure TDPMIDEProjectController.FileClosed(const fileName: string);
begin
//  FLogger.Debug('ProjectController.FileClosed : ' + fileName);
  FProjectTreeManager.NotifyProjectClosed(FileName);
  FEditorViewManager.ProjectClosed(FileName);
end;

procedure TDPMIDEProjectController.FileOpened(const fileName: string);
begin
  //FLogger.Debug('ProjectController.FileOpened : ' + fileName);
  //using projectloaded as it fires later.
end;

procedure TDPMIDEProjectController.FileOpening(const fileName: string);
begin
//  FLogger.Debug('ProjectController.FileOpening : ' + fileName);
  RestoreProject(fileName);
end;

procedure TDPMIDEProjectController.ProjectClosing(const fileName: string);
begin
//  FLogger.Debug('ProjectController.ProjectClosing : ' + fileName);
// using FileClosing as it fires earlier
end;

procedure TDPMIDEProjectController.ProjectCreating(const fileName: string);
begin
//  FLogger.Debug('ProjectController.ProjectCreating : ' + fileName);
end;


procedure TDPMIDEProjectController.ProjectLoaded(const fileName: string);
begin
  FLogger.Debug('ProjectController.ProjectLoaded : ' + fileName);
  //queue the project for loading in the tree.
  FProjectTreeManager.NotifyProjectLoaded(fileName);

  //this will be pmNone when reloading after external edit
  if FProjectMode = pmNone then
  begin
    FProjectTreeManager.NotifyEndLoading;
    FEditorViewManager.ProjectLoaded(fileName);
  end;
end;

procedure TDPMIDEProjectController.ProjectSaving(const fileName: string);
begin
//  FLogger.Debug('ProjectController.ProjectSaving : ' + fileName);
// not sure we need to do anything.
end;

procedure TDPMIDEProjectController.RestoreProject(const fileName: string);
var
  options : TRestoreOptions;
  cancellationTokenSource : ICancellationTokenSource;

begin
  options := TRestoreOptions.Create;
  options.ApplyCommon(TCommonOptions.Default);
  options.ProjectPath := fileName;
  options.Validate(FLogger);
  options.CompilerVersion := IDECompilerVersion;


  cancellationTokenSource := TCancellationTokenSourceFactory.Create;
  FLogger.ShowMessageTab;
  FLogger.StartRestore;
  FLogger.StartProject(FileName);

  FPackageInstaller.Restore(cancellationTokenSource.Token, options);
  FLogger.EndRestore;


end;

end.
