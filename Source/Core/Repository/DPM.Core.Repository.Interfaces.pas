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

unit DPM.Core.Repository.Interfaces;

interface

uses
  Spring.Collections,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Sources.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Options.Push,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces;


type
  //  IPackageSearchResult = interface
  //  ['{39B253DC-4BC5-4E72-918A-2FACC3EB5AC5}']
  //    function GetPackages : IList<IPackageIdentity>;
  //    property Packages : IList<IPackageIdentity> read GetPackages;
  //  end;

    //bear in mind that there will be a remote repository implemented with http
    //so need to keep that in mind with these interfaces.

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
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean) : IPackageIdentity;


    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const preRelease : boolean) : IList<TPackageVersion>; overload;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;

    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResultItem;

    function GetPackageIcon(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;

    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) :  IPackageSearchResult;

    function GetPackageFeedByIds(const cancellationToken : ICancellationToken;  const ids : IList<IPackageId>; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) :  IPackageSearchResult;

    //commands
    function Push(const cancellationToken : ICancellationToken; const pushOptions : TPushOptions) : Boolean;
    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IList<IPackageListItem>;


    property Enabled : boolean read GetEnabled write SetEnabled;
    property Name : string read GetName;
    property Source : string read GetSource;
    property RepositoryType : TSourceType read GetRepositoryType;

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
    function FindLatestVersion(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const version : TPackageVersion; const platform : TDPMPlatform; const includePrerelease : boolean; const sources : string) : IPackageIdentity;

    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string) : boolean;
    //downloads the dspec
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageId) : IPackageInfo;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform;
                                                const packageId : string; const versionRange : TVersionRange; const includePrerelease : boolean) : IList<IPackageInfo>;

    function GetPackageVersions(const cancellationToken : ICancellationToken; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const packageId : string; const includePrerelease : boolean) : IList<TPackageVersion>; overload;


    //ui specific stuff
    function GetPackageMetaData(const cancellationToken : ICancellationToken; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResultItem;
    function GetPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageSearchResult;
    function GetPackageIcon(const cancellationToken : ICancellationToken; const source : string; const packageId : string; const packageVersion : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform) : IPackageIcon;

    function GetInstalledPackageFeed(const cancellationToken : ICancellationToken; const options : TSearchOptions; const installedPackages : IList<IPackageId>) : IList<IPackageSearchResultItem>;
  end;


implementation

end.

