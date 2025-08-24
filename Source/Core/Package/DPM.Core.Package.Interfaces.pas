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

unit DPM.Core.Package.Interfaces;

interface

uses
  System.Classes,
  System.Generics.Defaults,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.TargetPlatform,
  DPM.Core.Options.Cache,
  DPM.Core.Options.Install,
  DPM.Core.Options.Uninstall,
  DPM.Core.Options.Restore;

type
  ///<summary>IPackageIdentity has minimum info needed to identify package
  /// Note this only has info we can get from the package filename!
  /// represents the core package identity - id, version, compiler, platform
  ///  + sourceName (optional, not part of the filename).
  /// </summary>
  IPackageIdentity = interface
    ['{35FABD79-3880-4F46-9D70-AA19AAE44565}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetCompilerVersion : TCompilerVersion;
    function GetPlatform : TDPMPlatform;
    function ToString : string;
    function ToIdVersionString : string;
    function GetSourceName : string;
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Platform : TDPMPlatform read GetPlatform;
    property SourceName : string read GetSourceName;
  end;

  ///<summary>A package dependency and the versionrange it was selected on.</summary>
  IPackageDependency = interface
    ['{E3576B9F-2CD5-415F-81D7-9E01AA74C9DB}']
    function GetId : string;
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function ToString : string;
    property Id : string read GetId;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
  end;

  ///<summary> PackageIdentity plus dependencies. used when resolving.</summary>
  IPackageInfo = interface(IPackageIdentity)
    ['{5672DB4A-40BC-45E0-857C-39117D03C322}']
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);
    function GetHash : string;
    function GetHashAlgorithm : string;

    property Dependencies : IList<IPackageDependency>read GetDependencies;
    property UseSource : boolean read GetUseSource write SetUseSource;
    property Hash : string read GetHash;
    property HashAlgorithm : string read GetHashAlgorithm;
  end;

  ///<summary>Package Info plus metadata</summary>
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
    function GetSearchPaths : IList<string>;
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
    property SearchPaths : IList<string> read GetSearchPaths;
  end;

  ///<summary>This is what is returned from a package repository for the ui </summary>
  IPackageSearchResultItem = interface(IPackageIdentity)
    ['{8EB6EA16-3708-41F7-93A2-FE56EB75510B}']
    function GetSourceName : string;
    function GetDependencies : IList<IPackageDependency>;

    function GetDescription : string;
    function GetAuthors : string;
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
    function GetHashAlgorithm : string;
    function GetFileHash : string;
    function GetInstalled : boolean;
    function GetLatestVersion : TPackageVersion;
    function GetLatestStableVersion : TPackageVersion;
    function GetIsError : boolean;
    function GetIsReservedPrefix : boolean;
    function GetIsTransitive : boolean;
    function GetIsLatestVersion : boolean;
    function GetIsLatestStableVersion : boolean;
    function GetVersionRange : TVersionRange;

    procedure SetVersion(const value : TPackageVersion);
    procedure SetInstalled(const value : boolean);
    procedure SetLatestVersion(const value : TPackageVersion);
    procedure SetLatestStableVersion(const value : TPackageVersion);
    procedure SetReportUrl(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetPublishedDate(const value : string);
    procedure SetIsTransitive(const value : boolean);
    procedure SetVersionRange(const value : TVersionRange);

    //reintroducing here to make it settable.
    property Version : TPackageVersion read GetVersion write SetVersion;

    property Description : string read GetDescription;
    property Authors : string read GetAuthors;
    property ProjectUrl : string read GetProjectUrl;
    property RepositoryUrl : string read GetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType write SetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch write SetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit write SetRepositoryCommit;
    property License : string read GetLicense;
    property Icon : string read GetIcon;
    property Copyright : string read GetCopyright;
    property Tags : string read GetTags;

    property Dependencies : IList<IPackageDependency>read GetDependencies;

    //only returned from server feeds.
    property IsReservedPrefix : boolean read GetIsReservedPrefix;
    property IsTrial : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    //returns -1 if not set.
    property Downloads : Int64 read GetDownloadCount;
    property HashAlgorithm : string read GetHashAlgorithm;
    property FileHash : string read GetFileHash;

    //these are for use by the UI, not returned.
    property Installed : boolean read GetInstalled write SetInstalled;
    property LatestVersion : TPackageVersion read GetLatestVersion write SetLatestVersion;
    property LatestStableVersion : TPackageVersion read GetLatestStableVersion write SetLatestStableVersion;
    property IsLatestVersion : boolean read GetIsLatestVersion;
    property IsLatestStableVersion : boolean read GetIsLatestStableVersion;
    property IsTransitive : boolean read GetIsTransitive write SetIsTransitive;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;
    property ReportUrl : string read GetProjectUrl write SetReportUrl;
    property PublishedDate : string read GetPublishedDate write SetPublishedDate; //TODO : what format should this be - see repos
    property IsError : boolean read GetIsError;

    property SourceName : string read GetSourceName;
  end;

  //List of search result items
  IPackageSearchResult = interface
  ['{547DDC6A-4A5F-429C-8A00-1B8FA4BA6D69}']
    function GetTotalCount : Int64;
    function GetSkip : Int64;
    function GetResults : IList<IPackageSearchResultItem>;

    procedure SetSkip(const value : Int64);
    procedure SetTotalCount(const value : Int64);


    property Skip : Int64 read GetSkip write SetSkip;
    property TotalCount : Int64 read GetTotalCount write SetTotalCount;
    property Results : IList<IPackageSearchResultItem> read GetResults;
  end;


  TPackageIconKind = (ikSvg, ikPng);

  /// <summary>Container for an icon returned from a repository
  ///
  IPackageIcon = interface
    ['{FB87A9AD-B114-4D1D-9AF5-1BD50FE17842}']
    function GetKind : TPackageIconKind;
    function GetStream : TStream;
    procedure SetStream(const value : TStream);
    property Kind : TPackageIconKind read GetKind;
    property Stream : TStream read GetStream write SetStream;
  end;

  IPackageListItem = interface
  ['{649F91AF-95F9-47A2-99A3-30BF68844E6B}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetPlatforms : string;
    function GetCompilerVersion : TCompilerVersion;
    procedure SetPlatforms(const value : string);
    function IsSamePackageVersion(const item : IPackageListItem) : Boolean;
    function IsSamePackageId(const item : IPackageListItem) : boolean;
    function MergeWith(const item : IPackageListItem) : IPackageListItem;

    property Id : string read GetId;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Version : TPackageVersion read GetVersion;
    property Platforms : string read GetPlatforms write SetPlatforms;
  end;

  IPackageLatestVersionInfo = interface
  ['{F157D248-248E-42C2-82E6-931423A5D1B0}']
    function GetId : string;
    function GetLatestStableVersion : TPackageVersion;
    function GetLatestVersion : TPackageVersion;
    procedure SetLatestStableVersion(const value : TPackageVersion);
    procedure SetLatestVersion(const value : TPackageVersion);

    property Id : string read GetId;
    property LatestStableVersion : TPackageVersion read GetLatestStableVersion write SetLatestStableVersion;
    property LatestVersion : TPackageVersion read GetLatestVersion write SetLatestVersion;
  end;

  //note : only compares Id

  TPackageInfoComparer = class(TInterfacedObject, IEqualityComparer<IPackageInfo>)
  protected
    function Equals(const Left, Right : IPackageInfo) : Boolean; reintroduce;
    function GetHashCode(const Value : IPackageInfo) : Integer; reintroduce;
  end;

  //note : only compares Id
  TPackageSearchResultItemComparer = class(TInterfacedObject, IEqualityComparer<IPackageSearchResultItem>)
  protected
    function Equals(const Left, Right : IPackageSearchResultItem) : Boolean; reintroduce;
    function GetHashCode(const Value : IPackageSearchResultItem) : Integer; reintroduce;
  end;

  TPackageListItemEqualityComparer = class(TInterfacedObject, IEqualityComparer<IPackageListItem>)
  protected
    function Equals(const Left, Right : IPackageListItem) : Boolean; reintroduce;
    function GetHashCode(const Value : IPackageListItem) : Integer; reintroduce;
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

{ TPackageListItemComparer }


{ TPackageListItemComparer }


function TPackageListItemEqualityComparer.Equals(const Left, Right: IPackageListItem): Boolean;
begin
  result := (Left.Id = Right.Id) and (left.CompilerVersion = right.CompilerVersion) and (left.Version = right.Version);// and (left.Platforms = right.Platforms);
end;

function TPackageListItemEqualityComparer.GetHashCode(const Value: IPackageListItem): Integer;
var
  s : string;
begin
  s := Value.Id + CompilerToString(value.CompilerVersion) + Value.Version.ToStringNoMeta + Value.Platforms;
  {$IF CompilerVersion >= 29.0}
  Result := System.Hash.THashBobJenkins.GetHashValue(s);
  {$ELSE}
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
  {$IFEND}
end;

end.

