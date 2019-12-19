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

  public
    constructor Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Path,
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


  if FLoadingGroup then
    exit;

  if (not FLoadingGroup) and (ExtractFileExt(FileName) = '.groupproj') then
  begin
    FLoadingGroup := true;
//    if not LoadProjectGroup(FileName) then
//    begin
//      //log error
//      exit;
//    end;
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
