unit DPM.IDE.Notifier;

interface

uses
  ToolsApi,
  Spring.Collections,
  DPM.IDE.Logger,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces;

type
  TDPMIDENotifier = class(TInterfacedObject, IOTANotifier, IOTAIDENotifier)
  private
    FLogger : IDPMIDELogger;
    FLoadingGroup : boolean;
    FPackageInstaller : IPackageInstaller;
    FGroupProjects : IList<string>;
  protected
    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    //IOTAIDENotifier
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);

    function CreateOptions(const fileName : string) : TRestoreOptions;

    function LoadProjectGroup(const fileName : string) : boolean;
  public
    constructor Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.GroupProjReader,
  DPM.Core.Options.Common;

{ TDPMIDENotifier }

procedure TDPMIDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TDPMIDENotifier.AfterSave;
begin

end;

procedure TDPMIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TDPMIDENotifier.BeforeSave;
begin

end;

constructor TDPMIDENotifier.Create(const logger: IDPMIDELogger; const packageInstaller : IPackageInstaller);
begin
  FLogger := logger;
  FPackageInstaller := packageInstaller;
  FGroupProjects := TCollections.CreateList<string>;
end;

function TDPMIDENotifier.CreateOptions(const fileName : string) : TRestoreOptions;
begin
  result := TRestoreOptions.Create;
  result.ApplyCommon(TCommonOptions.Default);
  result.ProjectPath := fileName;


  result.Validate(FLogger);
end;

destructor TDPMIDENotifier.Destroy;
begin

  inherited;
end;

procedure TDPMIDENotifier.Destroyed;
begin

end;


function TDPMIDENotifier.LoadProjectGroup(const fileName: string): boolean;
var
  i : integer;
  groupReader : IGroupProjectReader;
  projectRoot : string;
begin
  result := false;
  FLogger.Clear;
  FLogger.StartRestore;
  FGroupProjects.Clear;
  groupReader := TGroupProjectReader.Create(FLogger);
  if groupReader.LoadGroupProj(fileName) then
  begin
    groupReader.ExtractProjects(FGroupProjects);
    projectRoot := ExtractFilePath(fileName);
    //projects likely to be relative, so make them full paths
    for i := 0 to FGroupProjects.Count -1 do
    begin
      //sysutils.IsRelativePath returns false with paths starting with .\
      if TPathUtils.IsRelativePath(FGroupProjects[i]) then
        //TPath.Combine really should do this but it doesn't
        FGroupProjects[i] := TPathUtils.CompressRelativePath(projectRoot, FGroupProjects[i])
    end;
    result := true;
  end;
end;

procedure TDPMIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
var
  restoreOptions : TRestoreOptions;
  ext : string;
begin
  if not (NotifyCode in  [ofnFileOpening, ofnFileClosing])  then
    exit;

  ext := ExtractFileExt(FileName);
  if (ext <> '.groupproj') and (ext <> '.dproj') then
    exit;

  if NotifyCode = ofnFileClosing then
  begin
    FLogger.Clear;
    exit;
  end;


  //
  if FLoadingGroup then
  begin
    FGroupProjects.Remove(FileName);
    if FGroupProjects.Count = 0 then
      FLoadingGroup := false;
    exit;
  end;


  if (not FLoadingGroup) and (ExtractFileExt(FileName) = '.groupproj') then
  begin
    FLoadingGroup := true;
    //need this to determine when we are done loading the project group.
    if not LoadProjectGroup(FileName) then
    begin
      //log error
      exit;
    end;
  end
  else
    FLoadingGroup := false;


  FLogger.Clear;
  FLogger.ShowMessageTab;
  FLogger.StartRestore;
  FLogger.StartProject(FileName);
  restoreOptions := CreateOptions(fileName);

  FPackageInstaller.Restore(restoreOptions);

  FLogger.EndRestore;

end;

procedure TDPMIDENotifier.Modified;
begin

end;

end.
