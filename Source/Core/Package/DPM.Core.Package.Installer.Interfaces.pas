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

unit DPM.Core.Package.Installer.Interfaces;

interface

uses
  Spring.Collections,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces;


type
  IPackageInstallerContext = interface;

  //does the work of installing/restoring packages.
  IPackageInstaller = interface
    ['{554A0842-6C83-42BD-882C-B49FE4619DE0}']
    function Install(const cancellationToken : ICancellationToken; const options : TInstallOptions; const context : IPackageInstallerContext) : boolean;
    function UnInstall(const cancellationToken : ICancellationToken; const options : TUnInstallOptions; const context : IPackageInstallerContext) : boolean;
    function Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const context : IPackageInstallerContext) : boolean;
    function Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;
    //function Remove(const cancellationToken : ICancellationToken; const options : TUninstallOptions) : boolean;
    function Context : IPackageInstallerContext;
  end;

  //used to collect and detect package conflicts when working with multiple projects.
  //will also be used to collect build instructions and
  //design-time packages to install etc.
  IPackageInstallerContext = interface
    ['{8FD229A2-FE7B-4315-84B2-FF18B78C76DC}']
    //called from the project controller in the IDE
    procedure Clear;
    //This is need to clear the context data when a project is closed.
    procedure RemoveProject(const projectFile : string);

    //register a bpl for install into the IDE.
    function RegisterDesignPackage(const platform : TDPMPlatform; const packageFile : string; const dependsOn : IList<string>; out errorMessage : string) : boolean;

    function IsDesignPackageInstalled(const packageName : string; out platform : TDPMPlatform; out project : string) : boolean;

    procedure RecordGraph(const projectFile : string; const platform : TDPMPlatform; const graph : IPackageReference; const resolutions : TArray<IResolution>);
    function FindPackageResolution(const currentProjectFile: string; const packageId : string; const platform : TDPMPlatform) : IResolution;

  end;


implementation

end.
