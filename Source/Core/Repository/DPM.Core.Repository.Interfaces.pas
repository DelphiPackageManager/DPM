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

unit DPM.Core.Repository.Interfaces;

interface

uses
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Sources.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces;


type
  IPackageRepository = interface
    ['{0B495C12-4BDF-4C1C-9BD6-B008F0BA7F18}']
    function GetRepositoryType : TSourceType;
    function GetName : string;
    function GetSource : string;
    procedure Configure(const source : ISourceConfig);
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    //above implemented in base class


    ///  Used by the PackageInstaller
    ///
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion;  const includePrerelease : boolean) : IPackageInfo;


    function DownloadPackage(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const localFolder : string; var fileName : string) : boolean;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const preRelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;


    //used by the IDE
    function GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;
    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageIdentity>; const compilerVersion : TCompilerVersion) :  IPackageSearchResult;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;


    property Enabled : boolean read GetEnabled write SetEnabled;
    property Name : string read GetName;
    property Source : string read GetSource;
    property RepositoryType : TSourceType read GetRepositoryType;

  end;

  //Implemented only by the git registry repository. The package installer uses
  //Supports() to detect a git source and install it in place (clone + build)
  //rather than via a downloaded .dpkg.
  IGitRegistryRepository = interface
    ['{2C9E7A48-3D5B-4F1A-9E6C-8B0D1A2F3E4D}']
    function InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;
    //minutes between auto-pulls of the git-URL registry mirror (from app config).
    procedure SetRefreshIntervalMinutes(const value : integer);
  end;

  IPackageRepositoryFactory = interface
    ['{67014BE3-AA4C-45ED-A043-68262E57B89A}']
    function CreateRepository(const repoType : TSourceType) : IPackageRepository;
  end;


  IPackageRepositoryManager = interface
    ['{86DEB23D-7229-4F1C-949C-0A5CFB421152}']
    function Initialize(const configuration : IConfiguration) : boolean;

    function HasSources : boolean;

    //commands
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;


    //used by the package installer when no version specified - also for the cache command
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const includePrerelease : boolean; const sources : string) : IPackageInfo;

    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageInfo; const localFolder : string; var fileName : string) : boolean;

    //true (and sets sourceType) if a source with this name exists.
    function TryGetSourceType(const sourceName : string; out sourceType : TSourceType) : boolean;
    //installs a git registry package in place (clone + dspec) into targetDir.
    function InstallPackageInPlace(const cancellationToken : ICancellationToken; const packageInfo : IPackageInfo; const targetDir : string) : boolean;

    //downloads the dspec
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion;
                                                const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean) : IList<IPackageInfo>;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>; overload;


    //ui specific stuff
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion) : IPackageSearchResult;
    function GetPackageIcon(const cancellationToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion) : IPackageIcon;

    function GetInstalledPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageIdentity>) : IList<IPackageSearchResultItem>;
  end;


implementation

end.

