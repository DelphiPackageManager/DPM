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

unit DPM.Core.Package.Interfaces;

interface

uses
  Spring.Collections,
  System.Generics.Defaults,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Restore;

type
  //represents the core package identity, ie id, version, compiler, platform
  //Note this only has info we can get from the package filename!
  IPackageIdentity = interface
  ['{E9E49A25-3ECA-4380-BB75-AC9E29725BEE}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetSourceName : string;
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;
    function ToString : string;
    function ToIdVersionString : string;
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion;
    property SourceName : string read GetSourceName;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Platform : TDPMPlatform read GetPlatform;
  end;

  IPackageDependency = interface
  ['{E3576B9F-2CD5-415F-81D7-9E01AA74C9DB}']
    function GetId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function ToString : string;
    property Id : string read GetId;
    property Version : TVersionRange read GetVersionRange write SetVersionRange;
  end;

  //identity plus dependencies. used when resolving.
  IPackageInfo = interface(IPackageIdentity)
  ['{5672DB4A-40BC-45E0-857C-39117D03C322}']
    function GetDependencies : IList<IPackageDependency>;

    property Dependencies : IList<IPackageDependency> read GetDependencies;
  end;

  IPackageSearchPath = interface
  ['{55B09D0C-01E4-4FF3-977F-98A1A57B62B1}']
    function GetPath : string;
    function GetSourceOnly : boolean;
    function GetBinariesOnly : boolean;

    property Path : string read GetPath;
    property BinariesOnly : boolean read GetBinariesOnly;
    property SourceOnly : boolean read GetSourceOnly;
  end;


  //full package metadata.
  IPackageMetadata = interface(IPackageInfo)
  ['{0C39A81D-63FF-4939-A74A-4BFE29724168}']
    function GetDescription   : string;
    function GetAuthors       : string;
    function GetProjectUrl    : string;
    function GetLicense       : string;
    function GetIcon          : string;
    function GetCopyright     : string;
    function GetTags          : string;
    function GetIsTrial       : boolean;
    function GetIsCommercial  : boolean;
    function GetSearchPaths   : IList<IPackageSearchPath>;

    property Description  : string  read GetDescription;
    property Authors      : string  read GetAuthors;
    property ProjectUrl   : string  read GetProjectUrl;
    property License      : string  read GetLicense;
    property Icon         : string  read GetIcon;
    property Copyright    : string  read GetCopyright;
    property Tags         : string  read GetTags;
    property IsTrial      : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    property SearchPaths  : IList<IPackageSearchPath> read GetSearchPaths;
  end;

  //dependencies list for a single platform
  IPackagePlatformDependencies = interface
  ['{0C274B9B-ACD5-4355-8EDD-DA2E51247075}']
    function GetPlatform : TDPMPlatform;
    function GetDependencies : IList<IPackageDependency>;
    property Dependencies : IList<IPackageDependency> read GetDependencies;
  end;

  //The available platforms and dependencies for a package version.
  IPackageVersionResult = interface
  ['{45329FED-210A-42E1-B2A2-243C7DB0A645}']
    function GetVersion : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDependencies : IList<IPackagePlatformDependencies> ;

    property Version: string read GetVersion;
    property Platforms : TDPMPlatforms read GetPlatforms;
    property Dependencies : IList<IPackagePlatformDependencies> read GetDependencies;
  end;

  IPackageVersionsResults = interface
  ['{273329F2-3996-454F-9E93-BFB898C97F05}']
    function GetId : string;
    function GetResults : IList<IPackageVersionResult>;

    property Id           : string  read GetId;
    property Results : IList<IPackageVersionResult> read GetResults;
  end;


  //this is what is returned from a package feed for the UI.
  //note for version we are using strings to improve performance
  IPackageSearchResultItem = interface
  ['{8EB6EA16-3708-41F7-93A2-FE56EB75510B}']
    function GetId : string;
    function GetVersion : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDependencies : IList<IPackagePlatformDependencies>;

    function GetDescription   : string;
    function GetAuthors       : string;
    function GetProjectUrl    : string;
    function GetLicense       : string;
    function GetIcon          : string;
    function GetCopyright     : string;
    function GetTags          : string;
    function GetIsTrial       : boolean;
    function GetIsCommercial  : boolean;

    property Id           : string  read GetId;
    property Version      : string  read GetVersion;
    property Description  : string  read GetDescription;
    property Authors      : string  read GetAuthors;
    property ProjectUrl   : string  read GetProjectUrl;
    property License      : string  read GetLicense;
    property Icon         : string  read GetIcon;
    property Copyright    : string  read GetCopyright;
    property Tags         : string  read GetTags;
    property IsTrial      : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    property Dependencies : IList<IPackagePlatformDependencies> read GetDependencies;
  end;


  //does the work of installing/restoring packages.
  IPackageInstaller = interface
  ['{554A0842-6C83-42BD-882C-B49FE4619DE0}']
    function Install(const cancellationToken : ICancellationToken; const options : TInstallOptions) : boolean;
    function Restore(const cancellationToken : ICancellationToken; const options : TRestoreOptions) : boolean;
    function Cache(const cancellationToken : ICancellationToken; const options : TCacheOptions) : boolean;
  end;

  //used to collect and detect package conflicts when working with multiple projects.
  //will also be used to collect build instructions and
  //design-time packages to install etc.
  IPackageInstallerContext = interface
  ['{8FD229A2-FE7B-4315-84B2-FF18B78C76DC}']
    procedure Reset;
//    //provides context for build and runtime package copying.
//    procedure StartProject(const projectFile : string);
//
//    procedure EndProject(const projectFile : string);
//
//    //register a bpl for install into the IDE.
//    procedure RegisterDesignPackage(const packageFile : string; const dependsOn : IList<string>);
//

  end;


  TPackageIdentityComparer = class(TInterfacedObject,IEqualityComparer<IPackageIdentity>)
  protected
    function Equals(const Left, Right: IPackageIdentity): Boolean;reintroduce;
    function GetHashCode(const Value: IPackageIdentity): Integer; reintroduce;
  end;

  TPackageInfoComparer = class(TInterfacedObject,IEqualityComparer<IPackageInfo>)
  protected
    function Equals(const Left, Right: IPackageInfo): Boolean;reintroduce;
    function GetHashCode(const Value: IPackageInfo): Integer; reintroduce;
  end;



implementation

uses
  System.SysUtils;

{ TPackageIdentityComparer }

function TPackageIdentityComparer.Equals(const Left, Right: IPackageIdentity): Boolean;
begin
  result := SameText(Left.ToString, right.ToString);
end;

function TPackageIdentityComparer.GetHashCode(const Value: IPackageIdentity): Integer;
var
  s : string;
begin
  s := Value.ToString;
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;

{ TPackageInfoComparer }

function TPackageInfoComparer.Equals(const Left, Right: IPackageInfo): Boolean;
begin
  result := SameText(Left.ToString, right.ToString);
end;

function TPackageInfoComparer.GetHashCode(const Value: IPackageInfo): Integer;
var
  s : string;
begin
  s := Value.ToString;
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
end;

end.
