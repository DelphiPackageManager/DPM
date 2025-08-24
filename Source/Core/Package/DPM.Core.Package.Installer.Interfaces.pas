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
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Manifest.Interfaces;


type
  IPackageInstallerContext = interface;

  //
  /// <summary>
  ///  does the work of installing/restoring/unstalling packages.
  ///  shared by the command line and the IDE.
  /// </summary>
  IPackageInstaller = interface
    ['{554A0842-6C83-42BD-882C-B49FE4619DE0}']
    function Install(const cancellationToken : ICancellationToken; const options : TInstallOptions; const context : IPackageInstallerContext) : boolean;
    function UnInstall(const cancellationToken : ICancellationToken; const options : TUnInstallOptions; const context : IPackageInstallerContext) : boolean;
    function Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const context : IPackageInstallerContext) : boolean;
    function Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;
  end;

  ///<summary> The installer context is use to collect package resolutions and detect
  ///  package conflicts across projects in a project group.
  ///  It is alos used to manage installing/uninstalling design time packages in the IDE.
  ///  The IDE plugin provides it's own implementation of this interface so the core
  ///  version can avoid doing design time stuff (methods do nothing).
  ///</summary>
  IPackageInstallerContext = interface
    ['{8FD229A2-FE7B-4315-84B2-FF18B78C76DC}']
    /// <summary> called from the project controller in the IDE when starting loading. This is probably wrong! </summary>
    procedure Clear;

    ///<summary>called from the ProjectController when a project is closed.</summary>
    procedure RemoveProject(const projectFile : string);

    ///<summary>called from the package installer during install/restore</summary>
    procedure RecordGraph(const projectFile : string; const platform : TDPMPlatform; const graph : IPackageReference);

    ///<summary> called from the package installer during install/restore - to install design time packages. See IDE implementation</summary>
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const platform: TDPMPlatform; const packageManifests : IDictionary<string, IPackageManifest>) : boolean;


    ///<summary> Called from the dependency resolver to record package resolutions, so we can detect conflicts in other projects in the project group. </summary>
    procedure RecordResolutions(const projectFile: string; const platform : TDPMPlatform; const resolutions : TArray<IResolvedPackage>);

    ///<summary> Check for an existing package resolution in already loaded projects in the group.</summary>
    function FindPackageResolution(const projectFile: string; const platform : TDPMPlatform; const packageId : string ) : IResolvedPackage;

    /// <summary>remove an existing resolution - need to do this when upgrading a package.</summary>
    procedure RemoveResolution(const platform : TDPMPlatform; const packageId : string);

  end;


implementation

end.
