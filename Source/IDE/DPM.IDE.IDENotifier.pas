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

unit DPM.IDE.IDENotifier;

interface

uses
  ToolsApi,
  Spring.Collections,
  DPM.IDE.Logger,
  DPM.IDE.ProjectController;

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}

// The IDE notifier is used to receive notifications from the IDE and pass them
// to the ProjectController. Note that the file notifications are not fired when
// the IDE reloads due to external modifications (including by us) - so we still'
// need the StorageNotifier

type
  TDPMIDENotifier = class(TInterfacedObject, IOTANotifier, IOTAIDENotifier)
  private
    FLogger : IDPMIDELogger;
    FLoadingGroup : boolean;
    FGroupProjects : IList<string>;
    FProjectController : IDPMIDEProjectController;

  protected
    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    //IOTAIDENotifier
    procedure AfterCompile(Succeeded : Boolean);
    procedure BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);

    procedure FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);

    function LoadProjectGroup(const fileName : string) : boolean;
  public
    constructor Create(const logger : IDPMIDELogger; const projectController : IDPMIDEProjectController);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.System,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.GroupProjReader;

{ TDPMIDENotifier }

procedure TDPMIDENotifier.AfterCompile(Succeeded : Boolean);
begin
end;

procedure TDPMIDENotifier.AfterSave;
begin
end;

procedure TDPMIDENotifier.BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
begin
end;

procedure TDPMIDENotifier.BeforeSave;
begin
end;

constructor TDPMIDENotifier.Create(const logger : IDPMIDELogger; const projectController : IDPMIDEProjectController);
begin
  FLogger := logger;
  FGroupProjects := TCollections.CreateList < string > ;
  FProjectController := projectController;

end;

destructor TDPMIDENotifier.Destroy;
begin

  inherited;
end;

procedure TDPMIDENotifier.Destroyed;
begin
end;


function TDPMIDENotifier.LoadProjectGroup(const fileName : string) : boolean;
var
  i : integer;
  groupReader : IGroupProjectReader;
  projectRoot : string;
  ext : string;
begin
  result := false;
  FGroupProjects.Clear;
  groupReader := TGroupProjectReader.Create(FLogger);
  if groupReader.LoadGroupProj(fileName) then
  begin
    groupReader.ExtractProjects(FGroupProjects);
    projectRoot := ExtractFilePath(fileName);
    //projects likely to be relative, so make them full paths
    for i := 0 to FGroupProjects.Count - 1 do
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

procedure TDPMIDENotifier.FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
var
  ext : string;
begin
  ext := ExtractFileExt(FileName);
  //we only care about the project and project group files, ignoring all other notifications.
  if not (SameText(ext, '.dproj') or SameText(ext, '.groupproj')) then
    exit;

  case NotifyCode of
    ofnFileOpening :
      begin
        FLogger.Debug('TDPMIDENotifier ofnFileOpening : ' + FileName);
        TSystemUtils.OutputDebugString('TDPMIDENotifier ofnFileOpening : ' + FileName);

        {$IF CompilerVersion < 34.0}
        {
          Since there's no built in way to determine when the project group has finished
          loading, we have to cheat here.

          We load up the list of projects in the group, and then as each one is loaded
          (we get notified here) we remove them from the list. when the list is empty
          we are done and can continue. This has the added benefit of running restore
          when the grouproj is loading but before the project is loaded, so no reload loop
          to deal with!
        }
        if ext = '.groupproj' then
        begin
          if not FLoadingGroup then
          begin
            //if the groupproj doesn't exist, it's a placeholder and we are about to load a single project
            //this seems a bit iffy.. doesn't always happen.
            if not FileExists(FileName) then
            begin
              FProjectController.BeginLoading(TProjectMode.pmSingle);
              exit;
            end;
            //the group file exists, so we are definitely loading a group
            FLoadingGroup := true;

            //need this to determine when we are done loading the project group.
            if not LoadProjectGroup(FileName) then
            begin
              //Cancel := true; //stop loading as the group file is messed up?
              //not sure this is the right thing to do, we might have a bug that is our fault.
              exit;
            end;
            FProjectController.BeginLoading(TProjectMode.pmGroup);
          end;
          exit;
        end
        else if not FLoadingGroup then //we are loading a single project that is not part of a project group.
          FProjectController.BeginLoading(TProjectMode.pmSingle);
        {$ELSE}
        //10.4 adds ofnBeginProjectGroupOpen, ofnEndProjectGroupOpen, ofnBeginProjectGroupClose, ofnEndProjectGroupClose
        //so we will use those for the project group.
        if (ext = '.groupproj') then
          exit;
        if not FLoadingGroup then
          FProjectController.BeginLoading(TProjectMode.pmSingle);
        {$IFEND}

        if FileExists(FileName) then
          FProjectController.ProjectOpening(FileName);

      end;
    ofnFileOpened :
      begin
        //make sire we ignore the groupproj here.
        if ext <> '.dproj' then
          exit;
        FLogger.Debug('TDPMIDENotifier ofnFileOpened ' + FileName);
        TSystemUtils.OutputDebugString('TDPMIDENotifier ofnFileOpened : ' + FileName);
        {$IF CompilerVersion < 34.0}
        if FLoadingGroup then
        begin
          FGroupProjects.Remove(FileName);
          if FGroupProjects.Count = 0 then
          begin
            FLoadingGroup := false;
            FProjectController.EndLoading(TProjectMode.pmGroup);
          end;
        end
        else
          FProjectController.EndLoading(TProjectMode.pmSingle);
        {$ELSE}
          if not FLoadingGroup then
            FProjectController.EndLoading(TProjectMode.pmSingle);
        {$IFEND}

      end;
    ofnFileClosing :
      begin
        FLogger.Debug('TDPMIDENotifier ofnFileClosing ' + FileName);
        TSystemUtils.OutputDebugString('TDPMIDENotifier ofnFileClosing : ' + FileName);
        if (ext = '.dproj') then
          FProjectController.ProjectClosed(FileName)
        else
          FProjectController.ProjectGroupClosed;
      end;
    {$IF CompilerVersion >= 34.0 }
    //10.4 or later
    ofnBeginProjectGroupOpen :
    begin
      FLogger.Debug('TDPMIDENotifier ofnBeginProjectGroupOpen ' + FileName);
      TSystemUtils.OutputDebugString('TDPMIDENotifier ofnBeginProjectGroupOpen : ' + FileName);

      //if the project file doesn't exist, it's a temporary file and a single project is about to be opened.
      if not FileExists(FileName) then
        exit;

      FLoadingGroup := true;
      FProjectController.BeginLoading(TProjectMode.pmGroup);
    end;
    ofnEndProjectGroupOpen :
    begin
      FLogger.Debug('TDPMIDENotifier ofnEndProjectGroupOpen ' + FileName);
      TSystemUtils.OutputDebugString('TDPMIDENotifier ofnEndProjectGroupOpen : ' + FileName);
      if not FileExists(FileName) then
        exit;

      FProjectController.EndLoading(TProjectMode.pmGroup);
      FLoadingGroup := true;
    end;
    ofnBeginProjectGroupClose :
    begin
      FLogger.Debug('TDPMIDENotifier ofnBeginProjectGroupClose ' + FileName);
      TSystemUtils.OutputDebugString('TDPMIDENotifier ofnBeginProjectGroupClose : ' + FileName);

      FProjectController.ProjectGroupClosed;
//      FLogger.Clear;
    end;
    ofnEndProjectGroupClose :
    begin
      TSystemUtils.OutputDebugString('TDPMIDENotifier ofnEndProjectGroupClose : ' + FileName);
      FLogger.Debug('TDPMIDENotifier ofnEndProjectGroupClose ' + FileName);
    end;
    {$IFEND}

  else
    exit;
  end;


end;

procedure TDPMIDENotifier.Modified;
begin
  FLogger.Debug('TDPMIDENotifier.Modified');
end;

end.

