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
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Search,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces;


type
  IPackageSearchResult = interface
  ['{39B253DC-4BC5-4E72-918A-2FACC3EB5AC5}']
    function GetPackages : IList<IPackageIdentity>;
    property Packages : IList<IPackageIdentity> read GetPackages;
  end;

  //bear in mind that there will be a remote repository implemented with http
  //so need to keep that in mind with these interfaces.

  IPackageRepository = interface
  ['{0B495C12-4BDF-4C1C-9BD6-B008F0BA7F18}']
    function GetName : string;
    function GetSource : string;
    function GetIsLocal : boolean;
    function GetIsRemote : boolean;
    procedure Init(const source : ISourceConfig; const isRemote : boolean);

    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string ) : boolean;
    function Search(const cancellationToken : ICancellationToken; const options : TSearchOptions ) : IList<IPackageIdentity>;overload;
    function Search(const cancellationToken : ICancellationToken; const id : string; const range : TVersionRange)  : IList<IPackageIdentity>;overload;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity) : IPackageInfo;


    function GetPackageVersions(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : boolean) : IList<TPackageVersion>;

    function GetPackageVersionsWithDependencies(const cancellationToken : ICancellationToken; const id : string; const compilerVersion : TCompilerVersion; const platform : TDPMPlatform; const versionRange : TVersionRange; const preRelease : boolean) : IList<IPackageInfo>;



    property Name   : string read GetName;
    property Source : string read GetSource;
  end;

  IPackageRepositoryFactory = interface
  ['{67014BE3-AA4C-45ED-A043-68262E57B89A}']
    function CreateRepository(const repoType : string) : IPackageRepository;
  end;


  IPackageRepositoryManager = interface
  ['{86DEB23D-7229-4F1C-949C-0A5CFB421152}']
    function Initialize( const configuration : IConfiguration) : boolean;
    function Search(const cancellationToken : ICancellationToken; const options : TSearchOptions) : IPackageSearchResult;overload;
    function Search(const cancellationToken : ICancellationToken; const id : string; const range : TVersionRange) : IPackageSearchResult;overload;
    function DownloadPackage(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity; const localFolder : string; var fileName : string ) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageIdentity : IPackageIdentity) : IPackageInfo;
    function GetPackageVersions(const cancellationToken : ICancellationToken; const options : TSearchOptions; const platform : TDPMPlatform;const versionRange : TVersionRange) : IList<IPackageInfo>;

    //ui specific stuff
    function GetPackageFeed(const cancelToken : ICancellationToken; const options : TSearchOptions; const configuration : IConfiguration = nil) : IList<IPackageSearchResultItem>;

  end;


implementation

end.
