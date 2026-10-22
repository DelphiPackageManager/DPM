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
  DPM.Core.Diagnostics,
  DPM.Core.TargetPlatform,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore,
  DPM.Core.Package.Interfaces,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Spec.Interfaces;


type
  IPackageInstallerContext = interface;

  ///<summary> A package an install will apply, and where. </summary>
  IInstallPlanItem = interface
    ['{4B0B8D5E-1D52-4E7B-9C1B-6A7E3B2F9D10}']
    function GetPackageInfo : IPackageInfo;
    function GetIsCompanion : boolean;
    function GetCurrentVersion : TPackageVersion;
    function GetReason : string;
    function GetProjects : IList<string>;
    //'<installed versions> -> <new version>' for the summary - every installed version that changes, lowest
    //first, since the projects needn't agree. Just the new version when no project changes version.
    function VersionChangeText : string;
    property PackageInfo : IPackageInfo read GetPackageInfo;
    //true when the package was not asked for, but is upgraded because it blocks the packages that were.
    property IsCompanion : boolean read GetIsCompanion;
    //the highest version currently referenced at the top level of the plan's projects - empty if none.
    property CurrentVersion : TPackageVersion read GetCurrentVersion;
    //why a companion is included.
    property Reason : string read GetReason;
    //the projects the package is applied to.
    property Projects : IList<string> read GetProjects;
  end;

  ///<summary> Why a plan can't go ahead - reported (once) by LogPlanErrors in DPM.Core.Package.InstallPlan. </summary>
  TPlanError = record
    Code : TErrorCode;
    Message : string;
  end;

  ///<summary> What an install will do : which packages, at which versions, in which projects. Built by
  ///  IPackageInstaller.PlanInstall without changing anything, so it can be shown (and confirmed) before
  ///  ApplyInstallPlan applies exactly that. </summary>
  IInstallPlan = interface
    ['{9E3F2C71-5A4D-4C8E-B0F6-2D7A1E4B8C93}']
    function GetSucceeded : boolean;
    function GetItems : IList<IInstallPlanItem>;
    //why the plan can't go ahead. A plan can fail with no errors : when what went wrong was already
    //reported to the log, or the planning was cancelled.
    function GetErrors : IList<TPlanError>;
    function GetProjects : IList<string>;
    function HasCompanions : boolean;
    //the packages to apply to a project, requested ones first.
    function PackagesForProject(const projectFile : string) : IList<IPackageInfo>;
    //a human readable description, shared by the command line log and the IDE's confirmation.
    function SummaryLines : IList<string>;
    property Succeeded : boolean read GetSucceeded;
    property Items : IList<IInstallPlanItem> read GetItems;
    property Errors : IList<TPlanError> read GetErrors;
    //the projects with something to apply, in the order they were given.
    property Projects : IList<string> read GetProjects;
  end;

  ///<summary> A project an install plan is built for, with its current (unmodified) package graph. </summary>
  TInstallPlanProject = record
    ProjectFile : string;
    Graph : IPackageReference;
  end;

  IInstallPlanner = interface
    ['{C7A51E26-0F3B-4D98-8E2A-5B6C9D1F3A47}']
    ///<summary> requested : the packages asked for, at concrete versions.
    ///  upgrade = false : every requested package is applied to every project.
    ///  upgrade = true (changing installed versions, up or down) : a package is only applied to projects where
    ///  it is a top level reference, and other top level packages that would block the change (they pin a
    ///  dependency the requested packages need at another version) are added as companions, at their newest
    ///  compatible version - moving the same way as the requested packages, never the other way. </summary>
    function Plan(const cancellationToken : ICancellationToken; const config : IConfiguration; const options : TSearchOptions;
                  const requested : IList<IPackageInfo>; const projects : IList<TInstallPlanProject>; const upgrade : boolean) : IInstallPlan;
  end;

  //
  /// <summary>
  ///  does the work of installing/restoring/unstalling packages.
  ///  shared by the command line and the IDE.
  /// </summary>
  IPackageInstaller = interface
    ['{554A0842-6C83-42BD-882C-B49FE4619DE0}']
    function Install(const cancellationToken : ICancellationToken; const options : TInstallOptions; const context : IPackageInstallerContext) : boolean;

    ///<summary> Works out what Install would do with these options - including, for an upgrade, the companion
    ///  packages that must move with the requested ones - without changing anything. Returns false (with
    ///  plan.Errors explaining why) when the install could not go ahead. </summary>
    function PlanInstall(const cancellationToken : ICancellationToken; const options : TInstallOptions; out plan : IInstallPlan) : boolean;

    ///<summary> Applies a plan from PlanInstall : every project is resolved before anything is changed, and
    ///  no project is changed unless all of them resolve. </summary>
    function ApplyInstallPlan(const cancellationToken : ICancellationToken; const options : TInstallOptions; const plan : IInstallPlan; const context : IPackageInstallerContext) : boolean;
    function UnInstall(const cancellationToken : ICancellationToken; const options : TUnInstallOptions; const context : IPackageInstallerContext) : boolean;
    function Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions; const context : IPackageInstallerContext) : boolean;
    function Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;

    /// <summary> Cheap check used by the IDE to decide whether opening this project should
    ///  trigger a restore (and therefore show the log window). Loads only the PackageReferences
    ///  from the dproj. Returns False only when the project loads cleanly and has no dpm package
    ///  references; returns True if unsure (e.g. load failure) so restore still proceeds and any
    ///  real problem is surfaced through the normal path. </summary>
    function ProjectHasPackageReferences(const projectFile : string; const compilerVersion : TCompilerVersion) : boolean;

    /// <summary> Lightweight reconfigure used by the IDE's 'use source' toggle. Loads the project's
    ///  existing package graph, applies the current-session use-source choices from the context,
    ///  collects the search paths (source vs precompiled lib) and rewrites the dproj's DPMSearch.
    ///  It does NOT resolve, download, compile, or load/unload design-time packages - everything is
    ///  already in the cache, only the search paths change. </summary>
    function RefreshProjectSearchPaths(const cancellationToken : ICancellationToken; const projectFile : string;
                                       const compilerVersion : TCompilerVersion; const configFile : string;
                                       const context : IPackageInstallerContext) : boolean;
  end;

  ///<summary> The installer context is use to collect package resolutions and detect
  ///  package conflicts across projects in a project group.
  ///  It is alos used to manage installing/uninstalling design time packages in the IDE.
  ///  The IDE plugin provides it's own implementation of this interface so the core
  ///  version can avoid doing design time stuff (methods do nothing).
  ///</summary>
  IPackageInstallerContext = interface
    ['{8FD229A2-FE7B-4315-84B2-FF18B78C76DC}']
    /// <summary> Wipes all recorded per-project graphs and resolutions. Called by the IDE
    /// project controller in ProjectGroupClosed - the right trigger, since the next group's projects
    /// will record their own state from scratch. </summary>
    procedure Clear;

    ///<summary>called from the ProjectController when a project is closed.</summary>
    procedure RemoveProject(const projectFile : string);

    ///<summary>called from the package installer during install/restore</summary>
    procedure RecordGraph(const projectFile : string; const graph : IPackageReference);

    ///<summary> called from the package installer during install/restore - to install design time packages. See IDE implementation</summary>
    function InstallDesignPackages(const cancellationToken: ICancellationToken; const projectFile : string; const packageSpecs : IDictionary<string, IPackageSpec>) : boolean;

    ///<summary> called from the package installer during uninstall - to unload design time packages whose dpm packages are no longer referenced. See IDE implementation</summary>
    function UninstallDesignPackages(const cancellationToken : ICancellationToken; const projectFile : string; const orphanedPackageIds : IList<string>) : boolean;


    ///<summary> Called from the dependency resolver to record package resolutions, so we can detect conflicts in other projects in the project group. </summary>
    procedure RecordResolutions(const projectFile: string; const resolutions : TArray<IResolvedPackage>);

    ///<summary> Check for an existing package resolution in already loaded projects in the group.</summary>
    function FindPackageResolution(const projectFile: string; const packageId : string ) : IResolvedPackage;

    ///<summary> Starts an install that changes several projects at once. The resolutions already recorded
    ///  for these projects are hidden from FindPackageResolution (they are about to be replaced, so a
    ///  project not yet processed must not block one that is), and RecordResolutions/RecordGraph for
    ///  them are held back until CommitProject. Operations do not nest. </summary>
    procedure BeginOperation(const projectFiles : IList<string>);

    ///<summary> Makes a project's held back resolutions and graph the recorded ones - called once its
    ///  dproj has been saved. </summary>
    procedure CommitProject(const projectFile : string);

    ///<summary> Ends the operation, discarding anything not committed - so a failed or cancelled
    ///  install leaves the recorded state as it was. </summary>
    procedure EndOperation;

    ///<summary> Records the IDE's current-session 'use source' choice for a package in a project.
    ///  This is a debugging-only, session-lifetime toggle - it is never persisted to the dproj.
    ///  The installer consults it when collecting search paths, so a source choice flows down to
    ///  the package's dependencies. The CLI never sets this, so command line installs always use
    ///  the precompiled lib folder. </summary>
    procedure SetUseSource(const projectFile, packageId : string; const value : boolean);

    ///<summary> Returns the current-session 'use source' choice for a package in a project, or
    ///  false when none has been set (the default - use the precompiled lib folder). </summary>
    function GetUseSource(const projectFile, packageId : string) : boolean;

    ///<summary> Clears any current-session 'use source' choice for a package in a project - called
    ///  when the package is uninstalled from the project. </summary>
    procedure ClearUseSource(const projectFile, packageId : string);

  end;


implementation

end.
