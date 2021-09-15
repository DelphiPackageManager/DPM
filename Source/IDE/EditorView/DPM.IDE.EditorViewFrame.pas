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

//TODO : way too much code in this unit.. get it working then refactor!

unit DPM.IDE.EditorViewFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, SVGInterfaces,
  Vcl.Themes,
  DPM.Controls.ButtonedEdit,
  DPM.Controls.ButtonBar,
  VSoftVirtualListView,
  ToolsApi,
  Spring.Collections,
  Spring.Container,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Project.Interfaces,
  DPM.IDE.Types,
  DPM.IDE.IconCache,
  DPM.IDE.Options,
  DPM.IDE.Logger,
  DPM.IDE.ProjectTreeManager,
  DPM.IDE.BaseEditViewFrame,
  DPM.IDE.SearchBarFrame,
  DPM.IDE.Details.Interfaces,
  {$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
  System.Actions,
  {$IFEND}
  {$IF CompilerVersion >= 30.0 }
  System.ImageList,
  {$IFEND}
  Vcl.ActnList, DPM.IDE.PackageDetailsFrame;


{$I '..\DPMIDE.inc'}


type
  TDPMEditViewFrame = class(TDPMBaseEditViewFrame)
    PackageDetailsFrame : TPackageDetailsFrame;
  private
  protected

    //Create Ui elements at runtime - uses controls that are not installed, saves dev needing
    //to install controls before they can work in this.
    procedure CreateControls(AOwner : TComponent);override;
    function GetPackageDetailsView : IPackageDetailsView;override;
    function GetPackageReferences : IGraphNode;override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  System.Types,
  Xml.XMLIntf,
  System.Diagnostics,
  DPM.Core.Constants,
  DPM.Core.Options.Common,
  DPM.Core.Utils.Config,
  DPM.Core.Utils.Numbers,
  DPM.Core.Utils.Strings,
  DPM.Core.Project.Editor,
  DPM.Core.Package.Icon,
  DPM.Core.Package.SearchResults,
  DPM.Core.Repository.Interfaces,
  DPM.IDE.AboutForm,
  DPM.IDE.AddInOptionsHostForm;

const
  cDMPSearchHistoryFile = 'packagesearch.txt';

  { TDPMEditViewFrame }



constructor TDPMEditViewFrame.Create(AOwner : TComponent);
begin
  inherited;

end;

procedure TDPMEditViewFrame.CreateControls(AOwner : TComponent);
begin
  inherited;

end;


destructor TDPMEditViewFrame.Destroy;
begin
  inherited;
end;




function TDPMEditViewFrame.GetPackageDetailsView: IPackageDetailsView;
begin
  result := PackageDetailsFrame as IPackageDetailsView;
end;






function TDPMEditViewFrame.GetPackageReferences: IGraphNode;
var
  projectEditor : IProjectEditor;
begin
  projectEditor := TProjectEditor.Create(Logger, Configuration, IDECompilerVersion);
  projectEditor.LoadProject(Project.FileName);
  result := projectEditor.GetPackageReferences(CurrentPlatform); //NOTE : Can return nil. Will change internals to return empty root node.
end;

end.

