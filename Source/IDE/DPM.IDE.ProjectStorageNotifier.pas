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
  DPM.IDE.Logger,
  DPM.IDE.ProjectController;

type
  TDPMProjectStorageNotifier = class(TModuleNotifierObject, IOTAProjectFileStorageNotifier)
  private
    FLogger : IDPMIDELogger;
    FProjectController : IDPMIDEProjectController;
  protected
    //IOTAProjectFileStorageNotifier
    procedure CreatingProject(const ProjectOrGroup : IOTAModule);
    function GetName : string;
    procedure ProjectClosing(const ProjectOrGroup : IOTAModule);
    procedure ProjectLoaded(const ProjectOrGroup : IOTAModule; const Node : IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup : IOTAModule; const Node : IXMLNode);
  public
    constructor Create(const logger : IDPMIDELogger; const projectController : IDPMIDEProjectController);
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Forms,
  System.SysUtils,
  DPM.Core.Utils.Path;

{ TDPMProjectStorageNotifier }


constructor TDPMProjectStorageNotifier.Create(const logger : IDPMIDELogger; const projectController : IDPMIDEProjectController );
begin
  FLogger := logger;
  FProjectController := projectController
end;

procedure TDPMProjectStorageNotifier.CreatingProject(const ProjectOrGroup : IOTAModule);
begin
  if not (ExtractFileExt(ProjectOrGroup.FileName) = '.dproj') then
    exit;
  FProjectController.ProjectLoaded(ProjectOrGroup.FileName);
end;

destructor TDPMProjectStorageNotifier.Destroy;
begin
  FLogger := nil;
  FProjectController := nil;
  inherited;
end;


function TDPMProjectStorageNotifier.GetName : string;
begin
  //this has to be a node under BorlandProject
  //we don't really care what this is, we are not going to modify it,
  //we just want to make the notifications work!

  result := 'Platforms';
end;



procedure TDPMProjectStorageNotifier.ProjectClosing(const ProjectOrGroup : IOTAModule);
begin
  FLogger.Debug('Storage Project Closing : ' + ProjectOrGroup.FileName);
  //doesn't seem to get called with the project group?
  if not (ExtractFileExt(ProjectOrGroup.FileName) = '.dproj') then
    exit;
  FProjectController.ProjectClosing(ProjectOrGroup.FileName);
end;

procedure TDPMProjectStorageNotifier.ProjectLoaded(const ProjectOrGroup : IOTAModule; const Node : IXMLNode);
begin
  FLogger.Debug('Storage Project Loaded : ' + ProjectOrGroup.FileName);
//doesn't seem to get called with the project group?
  if not (ExtractFileExt(ProjectOrGroup.FileName) = '.dproj') then
    exit;
  FProjectController.ProjectLoaded(ProjectOrGroup.FileName);
end;

procedure TDPMProjectStorageNotifier.ProjectSaving(const ProjectOrGroup : IOTAModule; const Node : IXMLNode);
begin
  if not (ExtractFileExt(ProjectOrGroup.FileName) = '.dproj') then
    exit;
  FProjectController.ProjectSaving(ProjectOrGroup.FileName);

end;

end.

