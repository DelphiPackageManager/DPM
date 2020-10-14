{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.IDE.IDENotifier;

interface

uses
  ToolsApi,
  Spring.Collections,
  DPM.IDE.Logger,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.EditorViewManager;

type
  TDPMIDENotifier = class(TInterfacedObject, IOTANotifier, IOTAIDENotifier)
  private
    FLogger : IDPMIDELogger;
    FLoadingGroup : boolean;
    FPackageInstaller : IPackageInstaller;
    FGroupProjects : IList<string>;

    FEditorViewManager : IDPMEditorViewManager;
    FProjectTreeManager : IDPMProjectTreeManager;
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
    constructor Create(const logger : IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager;
                       const projectTreeManager : IDPMProjectTreeManager);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  VSoft.Awaitable,
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

constructor TDPMIDENotifier.Create(const logger: IDPMIDELogger; const packageInstaller : IPackageInstaller; const editorViewManager : IDPMEditorViewManager; const projectTreeManager : IDPMProjectTreeManager);
begin
  FLogger := logger;
  FPackageInstaller := packageInstaller;
  FGroupProjects := TCollections.CreateList<string>;
  FEditorViewManager := editorViewManager;
  FProjectTreeManager := projectTreeManager;
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
  FEditorViewManager.Destroyed;
  FEditorViewManager := nil;
end;


function TDPMIDENotifier.LoadProjectGroup(const fileName: string): boolean;
var
  i : integer;
  groupReader : IGroupProjectReader;
  projectRoot : string;
  ext : string;
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
      ext := ExtractFileExt(FGroupProjects[i]);
      //TODO : Allow cbproj when cbuilder supported.
      if ext = '.dproj' then
      begin
        //sysutils.IsRelativePath returns false with paths starting with .\
        if TPathUtils.IsRelativePath(FGroupProjects[i]) then
          //TPath.Combine really should do this but it doesn't
          FGroupProjects[i] := TPathUtils.CompressRelativePath(projectRoot, FGroupProjects[i])
      end;
    end;
    result := true;
  end;
end;

procedure TDPMIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
var
  restoreOptions : TRestoreOptions;
  cancellationTokenSource : ICancellationTokenSource;
  ext : string;
begin
  ext := ExtractFileExt(FileName);
  if not (SameText(ext, '.dproj') or SameText(ext, '.groupproj')) then
    exit;

  case NotifyCode of
    ofnFileOpening:
    begin
      FLogger.Debug('IDE File Opening ' + FileName);

      {
        Since there's no built in way to determine when the project group has finished
        loading, we have to cheat here.

        We load up the list of projects in the group, and then as each one is loaded
        (we get notified here) we remove them from the list. when the list is empty
        we are done and can continue. This has the added benefit of running restore
        when the grouproj is loading but before the project is loaded, so no reload loop
        to deal with!
      }

      //
      if FLoadingGroup then
      begin
        FGroupProjects.Remove(FileName);
        if FGroupProjects.Count = 0 then
          FLoadingGroup := false;
        exit;
      end
      else if (not FLoadingGroup) and (ext = '.groupproj')  then
      begin
        //if the groupproj doesn't exist, it's a placeholder and we are about to load a single project
        if not FileExists(FileName) then
        begin
          FProjectTreeManager.NotifyStartLoading(plSingle, nil);
          exit;
        end;
        FLoadingGroup := true;
        //need this to determine when we are done loading the project group.
        if not LoadProjectGroup(FileName) then
          exit;
        FProjectTreeManager.NotifyStartLoading(plGroup, FGroupProjects);
      end
      else
        FLoadingGroup := false;

      cancellationTokenSource := TCancellationTokenSourceFactory.Create;
//      FLogger.Clear;
      FLogger.ShowMessageTab;
      FLogger.StartRestore;
      FLogger.StartProject(FileName);
      restoreOptions := CreateOptions(fileName);

      FPackageInstaller.Restore(cancellationTokenSource.Token, restoreOptions);

      FLogger.EndRestore;


    end;
    ofnFileOpened:
    begin
       FLogger.Debug('File Opened ' + FileName);

    end;
    ofnFileClosing:
    begin
      FLogger.Clear;
      FEditorViewManager.ProjectClosed(FileName);
      exit;
    end;
  else
    exit;
  end;


end;

procedure TDPMIDENotifier.Modified;
begin

end;

end.
