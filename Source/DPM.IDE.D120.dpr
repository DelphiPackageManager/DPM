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

library DPM.IDE.D120;



uses
  System.SysUtils,
  System.Classes,
  SVGParse in 'SVG\SVGParse.pas',
  SVG in 'SVG\SVG.pas',
  SVGBase64 in 'SVG\SVGBase64.pas',
  SVGStyle in 'SVG\SVGStyle.pas',
  SVGCommon in 'SVG\SVGCommon.pas',
  SVGTypes in 'SVG\SVGTypes.pas',
  SVGPath in 'SVG\SVGPath.pas',
  SVGInterfaces in 'SVG\SVGInterfaces.pas',
  XmlLite in 'SVG\XmlLite.pas',
  SVGPaint in 'SVG\SVGPaint.pas',
  PasSVGFactory in 'SVG\PasSVGFactory.pas',
  GDIPKerning in 'SVG\GDIPKerning.pas',
  GDIPOBJ2 in 'SVG\GDIPOBJ2.pas',
  GDIPPathText in 'SVG\GDIPPathText.pas',
  SVGColor in 'SVG\SVGColor.pas',
  SVGGraphic in 'SVG\SVGGraphic.pas',
  DPM.Core.Cache.Interfaces in 'Core\Cache\DPM.Core.Cache.Interfaces.pas',
  DPM.Core.Cache in 'Core\Cache\DPM.Core.Cache.pas',
  DPM.Core.Compiler.EnvironmentProvider in 'Core\Compiler\DPM.Core.Compiler.EnvironmentProvider.pas',
  DPM.Core.Compiler.Factory in 'Core\Compiler\DPM.Core.Compiler.Factory.pas',
  DPM.Core.Compiler.Interfaces in 'Core\Compiler\DPM.Core.Compiler.Interfaces.pas',
  DPM.Core.Compiler.MSBuild in 'Core\Compiler\DPM.Core.Compiler.MSBuild.pas',
  DPM.Core.Configuration.Classes in 'Core\Configuration\DPM.Core.Configuration.Classes.pas',
  DPM.Core.Configuration.Interfaces in 'Core\Configuration\DPM.Core.Configuration.Interfaces.pas',
  DPM.Core.Configuration.Manager in 'Core\Configuration\DPM.Core.Configuration.Manager.pas',
  DPM.Core.Dependency.Context in 'Core\Dependency\DPM.Core.Dependency.Context.pas',
  DPM.Core.Dependency.Graph in 'Core\Dependency\DPM.Core.Dependency.Graph.pas',
  DPM.Core.Dependency.Interfaces in 'Core\Dependency\DPM.Core.Dependency.Interfaces.pas',
  DPM.Core.Dependency.Resolution in 'Core\Dependency\DPM.Core.Dependency.Resolution.pas',
  DPM.Core.Dependency.Resolver in 'Core\Dependency\DPM.Core.Dependency.Resolver.pas',
  DPM.Core.Dependency.Version in 'Core\Dependency\DPM.Core.Dependency.Version.pas',
  DPM.Core.Logging in 'Core\Logging\DPM.Core.Logging.pas',
  DPM.Core.Options.Base in 'Core\Options\DPM.Core.Options.Base.pas',
  DPM.Core.Options.Cache in 'Core\Options\DPM.Core.Options.Cache.pas',
  DPM.Core.Options.Common in 'Core\Options\DPM.Core.Options.Common.pas',
  DPM.Core.Options.Config in 'Core\Options\DPM.Core.Options.Config.pas',
  DPM.Core.Options.Install in 'Core\Options\DPM.Core.Options.Install.pas',
  DPM.Core.Options.List in 'Core\Options\DPM.Core.Options.List.pas',
  DPM.Core.Options.Pack in 'Core\Options\DPM.Core.Options.Pack.pas',
  DPM.Core.Options.Push in 'Core\Options\DPM.Core.Options.Push.pas',
  DPM.Core.Options.Restore in 'Core\Options\DPM.Core.Options.Restore.pas',
  DPM.Core.Options.Search in 'Core\Options\DPM.Core.Options.Search.pas',
  DPM.Core.Options.Sources in 'Core\Options\DPM.Core.Options.Sources.pas',
  DPM.Core.Options.Spec in 'Core\Options\DPM.Core.Options.Spec.pas',
  DPM.Core.Options.UnInstall in 'Core\Options\DPM.Core.Options.UnInstall.pas',
  DPM.Core.Package.Dependency in 'Core\Package\DPM.Core.Package.Dependency.pas',
  DPM.Core.Package.Installer in 'Core\Package\DPM.Core.Package.Installer.pas',
  DPM.Core.Package.InstallerContext in 'Core\Package\DPM.Core.Package.InstallerContext.pas',
  DPM.Core.Package.Interfaces in 'Core\Package\DPM.Core.Package.Interfaces.pas',
  DPM.Core.Package.Metadata in 'Core\Package\DPM.Core.Package.Metadata.pas',
  DPM.Core.Package.SearchResults in 'Core\Package\DPM.Core.Package.SearchResults.pas',
  DPM.Core.Packaging.Archive in 'Core\Packaging\DPM.Core.Packaging.Archive.pas',
  DPM.Core.Packaging.Archive.Reader in 'Core\Packaging\DPM.Core.Packaging.Archive.Reader.pas',
  DPM.Core.Packaging.Archive.Writer in 'Core\Packaging\DPM.Core.Packaging.Archive.Writer.pas',
  DPM.Core.Packaging.IdValidator in 'Core\Packaging\DPM.Core.Packaging.IdValidator.pas',
  DPM.Core.Packaging in 'Core\Packaging\DPM.Core.Packaging.pas',
  DPM.Core.Packaging.Writer in 'Core\Packaging\DPM.Core.Packaging.Writer.pas',
  DPM.Core.Project.Configuration in 'Core\Project\DPM.Core.Project.Configuration.pas',
  DPM.Core.Project.Editor in 'Core\Project\DPM.Core.Project.Editor.pas',
  DPM.Core.Project.GroupProjReader in 'Core\Project\DPM.Core.Project.GroupProjReader.pas',
  DPM.Core.Project.Interfaces in 'Core\Project\DPM.Core.Project.Interfaces.pas',
  DPM.Core.Repository.Base in 'Core\Repository\DPM.Core.Repository.Base.pas',
  DPM.Core.Repository.Directory in 'Core\Repository\DPM.Core.Repository.Directory.pas',
  DPM.Core.Repository.Factory in 'Core\Repository\DPM.Core.Repository.Factory.pas',
  DPM.Core.Repository.Http in 'Core\Repository\DPM.Core.Repository.Http.pas',
  DPM.Core.Repository.Interfaces in 'Core\Repository\DPM.Core.Repository.Interfaces.pas',
  DPM.Core.Repository.Manager in 'Core\Repository\DPM.Core.Repository.Manager.pas',
  DPM.Core.Sources.Interfaces in 'Core\Sources\DPM.Core.Sources.Interfaces.pas',
  DPM.Core.Sources.Manager in 'Core\Sources\DPM.Core.Sources.Manager.pas',
  DPM.Core.Sources.Types in 'Core\Sources\DPM.Core.Sources.Types.pas',
  DPM.Core.Spec.BPLEntry in 'Core\Spec\DPM.Core.Spec.BPLEntry.pas',
  DPM.Core.Spec.BuildEntry in 'Core\Spec\DPM.Core.Spec.BuildEntry.pas',
  DPM.Core.Spec.Dependency in 'Core\Spec\DPM.Core.Spec.Dependency.pas',
  DPM.Core.Spec.DependencyGroup in 'Core\Spec\DPM.Core.Spec.DependencyGroup.pas',
  DPM.Core.Spec.FileEntry in 'Core\Spec\DPM.Core.Spec.FileEntry.pas',
  DPM.Core.Spec.Interfaces in 'Core\Spec\DPM.Core.Spec.Interfaces.pas',
  DPM.Core.Spec.MetaData in 'Core\Spec\DPM.Core.Spec.MetaData.pas',
  DPM.Core.Spec.Node in 'Core\Spec\DPM.Core.Spec.Node.pas',
  DPM.Core.Spec in 'Core\Spec\DPM.Core.Spec.pas',
  DPM.Core.Spec.Reader in 'Core\Spec\DPM.Core.Spec.Reader.pas',
  DPM.Core.Spec.SearchPath in 'Core\Spec\DPM.Core.Spec.SearchPath.pas',
  DPM.Core.Spec.SearchPathGroup in 'Core\Spec\DPM.Core.Spec.SearchPathGroup.pas',
  DPM.Core.Spec.TargetPlatform in 'Core\Spec\DPM.Core.Spec.TargetPlatform.pas',
  DPM.Core.Spec.Template in 'Core\Spec\DPM.Core.Spec.Template.pas',
  DPM.Core.Spec.TemplateBase in 'Core\Spec\DPM.Core.Spec.TemplateBase.pas',
  DPM.Core.Spec.Writer in 'Core\Spec\DPM.Core.Spec.Writer.pas',
  DPM.Core.Utils.Config in 'Core\Utils\DPM.Core.Utils.Config.pas',
  DPM.Core.Utils.Directory in 'Core\Utils\DPM.Core.Utils.Directory.pas',
  DPM.Core.Utils.Enum in 'Core\Utils\DPM.Core.Utils.Enum.pas',
  DPM.Core.Utils.Numbers in 'Core\Utils\DPM.Core.Utils.Numbers.pas',
  DPM.Core.Utils.Path in 'Core\Utils\DPM.Core.Utils.Path.pas',
  DPM.Core.Utils.Process in 'Core\Utils\DPM.Core.Utils.Process.pas',
  DPM.Core.Utils.Strings in 'Core\Utils\DPM.Core.Utils.Strings.pas',
  DPM.Core.Utils.System in 'Core\Utils\DPM.Core.Utils.System.pas',
  DPM.Core.Utils.XML in 'Core\Utils\DPM.Core.Utils.XML.pas',
  DPM.Core.MSXML in 'Core\Xml\DPM.Core.MSXML.pas',
  DPM.Core.XML.NodeBase in 'Core\Xml\DPM.Core.XML.NodeBase.pas',
  DPM.Core.Constants in 'Core\DPM.Core.Constants.pas',
  DPM.Core.Init in 'Core\DPM.Core.Init.pas',
  DPM.Core.TargetPlatform in 'Core\DPM.Core.TargetPlatform.pas',
  DPM.Core.Types in 'Core\DPM.Core.Types.pas',
  DPM.Core.Package.Icon in 'Core\Package\DPM.Core.Package.Icon.pas',
  DPM.Core.Package.Installer.Interfaces in 'Core\Package\DPM.Core.Package.Installer.Interfaces.pas',
  DPM.Core.Compiler.BOM in 'Core\Compiler\DPM.Core.Compiler.BOM.pas',
  DPM.Core.Compiler.ProjectSettings in 'Core\Compiler\DPM.Core.Compiler.ProjectSettings.pas',
  DPM.Controls.VersionGrid in 'Controls\DPM.Controls.VersionGrid.pas',
  DPM.Core.Utils.Files in 'Core\Utils\DPM.Core.Utils.Files.pas',
  DPM.Controls.LogMemo in 'Controls\DPM.Controls.LogMemo.pas',
  DPM.IDE.Main in 'IDE\DPM.IDE.Main.pas',
  DPM.IDE.Wizard in 'IDE\DPM.IDE.Wizard.pas',
  DPM.IDE.ProjectStorageNotifier in 'IDE\DPM.IDE.ProjectStorageNotifier.pas',
  DPM.IDE.IDENotifier in 'IDE\DPM.IDE.IDENotifier.pas',
  DPM.IDE.ProjectMenu in 'IDE\DPM.IDE.ProjectMenu.pas',
  DPM.IDE.Constants in 'IDE\DPM.IDE.Constants.pas',
  DPM.IDE.AddInOptions in 'IDE\DPM.IDE.AddInOptions.pas',
  DPM.IDE.AddInOptionsFrame in 'IDE\DPM.IDE.AddInOptionsFrame.pas' {DPMOptionsFrame: TFrame},
  DPM.Controls.ButtonedEdit in 'Controls\DPM.Controls.ButtonedEdit.pas',
  DPM.Controls.AutoComplete in 'Controls\DPM.Controls.AutoComplete.pas',
  DPM.IDE.AboutForm in 'IDE\DPM.IDE.AboutForm.pas' {DPMAboutForm},
  DPM.IDE.Types in 'IDE\DPM.IDE.Types.pas',
  DPM.IDE.AddInOptionsHostForm in 'IDE\DPM.IDE.AddInOptionsHostForm.pas' {DPMOptionsHostForm},
  DPM.IDE.IconCache in 'IDE\DPM.IDE.IconCache.pas',
  DPM.IDE.VSTProxy in 'IDE\DPM.IDE.VSTProxy.pas',
  DPM.IDE.ProjectTreeManager in 'IDE\DPM.IDE.ProjectTreeManager.pas',
  DPM.IDE.Utils in 'IDE\DPM.IDE.Utils.pas',
  DPM.IDE.ProjectTree.Containers in 'IDE\DPM.IDE.ProjectTree.Containers.pas',
  DPM.IDE.ProjectController in 'IDE\DPM.IDE.ProjectController.pas',
  DPM.IDE.Options in 'IDE\Options\DPM.IDE.Options.pas',
  DPM.IDE.Logger in 'IDE\Logging\DPM.IDE.Logger.pas',
  DPM.IDE.InstallerContext in 'IDE\DPM.IDE.InstallerContext.pas',
  DPM.IDE.EditorView in 'IDE\EditorView\DPM.IDE.EditorView.pas',
  DPM.IDE.EditorViewManager in 'IDE\EditorView\DPM.IDE.EditorViewManager.pas',
  DPM.IDE.PackageDetailsPanel in 'IDE\EditorView\DPM.IDE.PackageDetailsPanel.pas',
  DPM.IDE.SearchBarFrame in 'IDE\EditorView\DPM.IDE.SearchBarFrame.pas' {DPMSearchBarFrame: TFrame},
  DPM.IDE.MessageForm in 'IDE\Logging\DPM.IDE.MessageForm.pas' {DPMMessageForm},
  DPM.IDE.MessageService in 'IDE\Logging\DPM.IDE.MessageService.pas',
  DPM.IDE.ToolsAPI in 'IDE\DPM.IDE.ToolsAPI.pas',
  DPM.IDE.Details.Interfaces in 'IDE\EditorView\DPM.IDE.Details.Interfaces.pas',
  DPM.IDE.PackageDetailsFrame in 'IDE\EditorView\DPM.IDE.PackageDetailsFrame.pas' {PackageDetailsFrame: TFrame},
  DPM.Core.Sources.ServiceIndex in 'Core\Sources\DPM.Core.Sources.ServiceIndex.pas',
  DPM.Core.Package.ListItem in 'Core\Package\DPM.Core.Package.ListItem.pas',
  DPM.IDE.EditorViewFrame in 'IDE\EditorView\DPM.IDE.EditorViewFrame.pas' {DPMEditViewFrame: TFrame},
  DPM.IDE.ProjectNotifier in 'IDE\DPM.IDE.ProjectNotifier.pas',
  DPM.IDE.ActivityIndicator in 'IDE\EditorView\DPM.IDE.ActivityIndicator.pas',
  DPM.Core.Package.PackageLatestVersionInfo in 'Core\Package\DPM.Core.Package.PackageLatestVersionInfo.pas',
  DPM.IDE.PathManager in 'IDE\DPM.IDE.PathManager.pas';

{$R *.res}

begin
end.
