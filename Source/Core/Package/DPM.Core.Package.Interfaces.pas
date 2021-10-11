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
  System.Classes,
  System.Generics.Defaults,
  Spring.Collections,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore;

type
  //minimum info needed to identify package
  //need to make more of the api use this rather than the derived interfaces.
  //Note this only has info we can get from the package filename!
  //represents the core package identity - id, version, compiler, platform
  IPackageId = interface
    ['{35FABD79-3880-4F46-9D70-AA19AAE44565}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;
    function ToString : string;
    function ToIdVersionString : string;
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Platform : TDPMPlatform read GetPlatform;
  end;

  //packageid plus sourcename
  IPackageIdentity = interface(IPackageId)
    ['{E9E49A25-3ECA-4380-BB75-AC9E29725BEE}']
    function GetSourceName : string;
    property SourceName : string read GetSourceName;
  end;

  IPackageDependency = interface
    ['{E3576B9F-2CD5-415F-81D7-9E01AA74C9DB}']
    function GetId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function ToString : string;
    property Id : string read GetId;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
  end;

  //identity plus dependencies. used when resolving.
  IPackageInfo = interface(IPackageIdentity)
    ['{5672DB4A-40BC-45E0-857C-39117D03C322}']
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);


    property Dependencies : IList<IPackageDependency>read GetDependencies;
    property UseSource : boolean read GetUseSource write SetUseSource;
  end;

  IPackageSearchPath = interface
    ['{55B09D0C-01E4-4FF3-977F-98A1A57B62B1}']
    function GetPath : string;

    property Path : string read GetPath;
  end;


  //full package metadata.
  IPackageMetadata = interface(IPackageInfo)
    ['{0C39A81D-63FF-4939-A74A-4BFE29724168}']
    function GetDescription : string;
    function GetAuthors : string;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : string;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetProjectUrl : string;
    function GetSearchPaths : IList<IPackageSearchPath>;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    property Description : string read GetDescription;
    property Authors : string read GetAuthors;
    property License : string read GetLicense;
    property Icon : string read GetIcon;
    property Copyright : string read GetCopyright;
    property Tags : string read GetTags;
    property IsTrial : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    property ProjectUrl : string read GetProjectUrl;
    property RepositoryUrl : string read GetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit;

    //TODO : We may be able t remove this as the only place it's use will probably be getting the full spec file.
    property SearchPaths : IList<IPackageSearchPath>read GetSearchPaths;
  end;

  //dependencies list for a single platform
  IPackagePlatformDependencies = interface
    ['{0C274B9B-ACD5-4355-8EDD-DA2E51247075}']
    function GetPlatform : TDPMPlatform;
    function GetDependencies : IList<IPackageDependency>;
    property Dependencies : IList<IPackageDependency>read GetDependencies;
  end;

  //The available platforms and dependencies for a package version.
  IPackageVersionResult = interface
    ['{45329FED-210A-42E1-B2A2-243C7DB0A645}']
    function GetVersion : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDependencies : IList<IPackagePlatformDependencies>;

    property Version : string read GetVersion;
    property Platforms : TDPMPlatforms read GetPlatforms;
    property Dependencies : IList<IPackagePlatformDependencies>read GetDependencies;
  end;

  IPackageVersionsResults = interface
    ['{273329F2-3996-454F-9E93-BFB898C97F05}']
    function GetId : string;
    function GetResults : IList<IPackageVersionResult>;

    property Id : string read GetId;
    property Results : IList<IPackageVersionResult>read GetResults;
  end;


  //this is what is returned from a package feed for the UI.
  //note for version we are using strings to improve performance
  IPackageSearchResultItem = interface
    ['{8EB6EA16-3708-41F7-93A2-FE56EB75510B}']
    function GetSourceName : string;
    function GetId : string;
    function GetVersion : string;
    function GetPlatform : TDPMPlatform;
    function GetCompilerVersion : TCompilerVersion;
    function GetDependencies : IList<IPackagePlatformDependencies>;

    function GetDescription : string;
    function GetAuthors : string;
    function GetOwners : string;
    function GetProjectUrl : string;
    function GetReportUrl : string;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetPublishedDate : string;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : string;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetDownloadCount : Int64;
    function GetInstalled : boolean;
    function GetLatestVersion : string;
    function GetIsError : boolean;
    function GetIsReservedPrefix : boolean;
    function GetIsTransitive : boolean;

    procedure SetVersion(const value : string);
    procedure SetInstalled(const value : boolean);
    procedure SetLatestVersion(const value : string);
    procedure SetReportUrl(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetPublishedDate(const value : string);
    procedure SetIsTransitive(const value : boolean);

    property Id : string read GetId;
    property Version : string read GetVersion write SetVersion;
    property Description : string read GetDescription;
    property Authors : string read GetAuthors;
    property Owners : string read GetOwners;
    property ProjectUrl : string read GetProjectUrl;
    property RepositoryUrl : string read GetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType write SetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch write SetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit write SetRepositoryCommit;
    property License : string read GetLicense;
    property Icon : string read GetIcon;
    property Copyright : string read GetCopyright;
    property Tags : string read GetTags;

    property Platform : TDPMPlatform read GetPlatform;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Dependencies : IList<IPackagePlatformDependencies>read GetDependencies;

    //only returned from server feeds.
    property IsReservedPrefix : boolean read GetIsReservedPrefix;
    property IsTrial : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    //returns -1 if not set.
    property Downloads : Int64 read GetDownloadCount;

    //these are for use by the UI, it's not returned.
    property Installed : boolean read GetInstalled write SetInstalled;
    property LatestVersion : string read GetLatestVersion write SetLatestVersion;
    property IsTransitive : boolean read GetIsTransitive write SetIsTransitive;
    property ReportUrl : string read GetProjectUrl write SetReportUrl;
    property PublishedDate : string read GetPublishedDate write SetPublishedDate; //TODO : what format should this be - see repos
    property IsError : boolean read GetIsError;

    property SourceName : string read GetSourceName;
  end;



  TPackageIconKind = (ikSvg, ikPng);

  IPackageIcon = interface
    ['{FB87A9AD-B114-4D1D-9AF5-1BD50FE17842}']
    function GetKind : TPackageIconKind;
    function GetStream : TStream;
    procedure SetStream(const value : TStream);
    property Kind : TPackageIconKind read GetKind;
    property Stream : TStream read GetStream write SetStream;
  end;


  TPackageInfoComparer = class(TInterfacedObject, IEqualityComparer<IPackageInfo>)
  protected
    function Equals(const Left, Right : IPackageInfo) : Boolean; reintroduce;
    function GetHashCode(const Value : IPackageInfo) : Integer; reintroduce;
  end;

  TPackageSearchResultItemComparer = class(TInterfacedObject, IEqualityComparer<IPackageSearchResultItem>)
  protected
    function Equals(const Left, Right : IPackageSearchResultItem) : Boolean; reintroduce;
    function GetHashCode(const Value : IPackageSearchResultItem) : Integer; reintroduce;
  end;

implementation

// For Delphi XE3 and up:
{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}


uses
  {$IF CompilerVersion >= 29.0}
  System.Hash,
  {$IFEND}
  System.SysUtils;


{ TPackageInfoComparer }

function TPackageInfoComparer.Equals(const Left, Right : IPackageInfo) : Boolean;
begin
  result := SameText(Left.ToString, right.ToString);
end;

function TPackageInfoComparer.GetHashCode(const Value : IPackageInfo) : Integer;
var
  s : string;
begin
  s := Value.ToString;
  {$IF CompilerVersion >= 29.0}
  Result := System.Hash.THashBobJenkins.GetHashValue(s);
  {$ELSE}
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
  {$IFEND}
end;

{ TPackageSearchResultItemComparer }

function TPackageSearchResultItemComparer.Equals(const Left, Right : IPackageSearchResultItem) : Boolean;
begin
  result := SameText(Left.Id, right.Id);
end;

function TPackageSearchResultItemComparer.GetHashCode(const Value : IPackageSearchResultItem) : Integer;
var
  s : string;
begin
  s := Value.Id;
  {$IF CompilerVersion >= 29.0}
  Result := System.Hash.THashBobJenkins.GetHashValue(s);
  {$ELSE}
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
  {$IFEND}

end;

end.

