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

unit DPM.IDE.ProjectStorageNotifier;

interface

uses
  ToolsApi,
  XML.XMLIntf,
  System.Classes,
  Spring.Collections,
  DPM.IDE.Logger;

type
  TDPMProjectStorageNotifier = class(TNotifierObject, IOTAProjectFileStorageNotifier)
  private
    FLogger : IDPMIDELogger;
    FIgnoreReload : TStringList;

    FGroupProjects : IList<string>;
    FProjectGroup : IOTAProjectGroup;
    FModuleServices : IOTAModuleServices;

  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    procedure LoadProjectGroup;

  //IOTAProjectFileStorageNotifier
    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    function GetName: string;
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
  public
    constructor Create(const logger : IDPMIDELogger);
    destructor Destroy;override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DPM.Core.Utils.Path,
  DPM.Core.Project.Interfaces,
  DPM.Core.Project.GroupProjReader;

{ TDPMProjectStorageNotifier }

procedure TDPMProjectStorageNotifier.AfterSave;
begin

end;

procedure TDPMProjectStorageNotifier.BeforeSave;
begin

end;

constructor TDPMProjectStorageNotifier.Create(const logger : IDPMIDELogger);
begin
  FLogger := logger;
  FGroupProjects := TCollections.CreateList<string>;
  FIgnoreReload := TStringList.Create;
  FModuleServices := BorlandIDEServices as IOTAModuleServices;
end;

procedure TDPMProjectStorageNotifier.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
  //don't need to do anything here, a new project will not be using any dpm packages.
end;

destructor TDPMProjectStorageNotifier.Destroy;
begin
  FIgnoreReload.Free;
  FGroupProjects := nil;
  inherited;
end;

procedure TDPMProjectStorageNotifier.Destroyed;
begin
  FProjectGroup := nil;
end;

function TDPMProjectStorageNotifier.GetName: string;
begin
  //this has to be a node under BorlandProject
  //we don't really care what this is, we are not going to modify it,
  //we just want to make the notifications work!

  result := 'Platforms';
end;

procedure TDPMProjectStorageNotifier.LoadProjectGroup;
var
  i : integer;
  projectGroup : IOTAProjectGroup;
  groupReader : IGroupProjectReader;
  projectRoot : string;
begin
  //WARNING - MAJOR HACKERY AHEAD!!!
  //tools api doesn't give us notications on begin/end project loading. This method is called as each project is loaded
  //We get the projectgroup on the first call, if it exists the get the list of projects from it
  //and then on subsequent calls we remove each project from the list.. when it's empty we're done.
  if FProjectGroup = nil then
  begin
    projectGroup := FModuleServices.MainProjectGroup;
    if projectGroup <> FProjectGroup then
    begin
      FLogger.Clear;
      FLogger.StartRestore;
      FGroupProjects.Clear;
      FProjectGroup := projectGroup;
      //doesn't work, project group only has 1 file.. grrr
//      for i := 0 to FProjectGroup.ProjectCount -1 do
//        FGroupProjects.Add(FProjectGroup.Projects[i].FileName);

      if FileExists(FProjectGroup.FileName) then
      begin
        groupReader := TGroupProjectReader.Create(FLogger);
        if groupReader.LoadGroupProj(FProjectGroup.FileName) then
        begin
          groupReader.ExtractProjects(FGroupProjects);
          projectRoot := ExtractFilePath(FProjectGroup.FileName);
          //projects likely to be relative, so make them full paths
          for i := 0 to FGroupProjects.Count -1 do
          begin
            //sysutils.IsRelativePath returns false with paths starting with .\
            if TPathUtils.IsRelativePath(FGroupProjects[i]) then
              //TPath.Combine really should do this but it doesn't
              FGroupProjects[i] := TPathUtils.CompressRelativePath(projectRoot, FGroupProjects[i])
          end;

        end;
      end;
    end;
  end;
end;

procedure TDPMProjectStorageNotifier.Modified;
begin

end;

procedure TDPMProjectStorageNotifier.ProjectClosing(const ProjectOrGroup: IOTAModule);
begin
  FLogger.Clear;
  FProjectGroup := nil;
end;

procedure TDPMProjectStorageNotifier.ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
var
  i : integer;
begin
  //Note : We cannot use the Node here, as it's ownerdocument is not the full dproj, it appears to be
  //a fragment from BorlandProject.. we need to work outside of this node..


  //modifying a project during load will cause a reload, so we need to deal with that here.
   i := FIgnoreReload.IndexOf(ProjectOrGroup.FileName);
   if i <> -1 then
      exit;
  FIgnoreReload.Add(ProjectOrGroup.FileName);

  FLogger.ShowMessageTab;

  //hacky attempting to figure out when we are done.
  LoadProjectGroup;


  try
    FLogger.StartProject(ProjectOrGroup.FileName, ProjectOrGroup.FileSystem);

    //if we modified the project then force a refresh.
    //ProjectOrGroup.Refresh(true);

  finally
    FLogger.EndProject(ProjectOrGroup.FileName);
    i := FGroupProjects.IndexOf(ProjectOrGroup.FileName);
    if i <> -1 then
        FGroupProjects.Delete(i);
    if FGroupProjects.Count = 0 then
    begin
      FLogger.EndRestore;
      FProjectGroup := nil;
    end;
    i := FIgnoreReload.IndexOf(ProjectOrGroup.FileName);
     if i <> -1 then
        FIgnoreReload.Delete(i);
  end;

end;

procedure TDPMProjectStorageNotifier.ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin
end;

end.
