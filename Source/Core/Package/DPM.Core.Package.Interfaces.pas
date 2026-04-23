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
    function ToString : string;
    function ToIdVersionString : string;
    function GetSourceName : string;
    property Id : string read GetId;
    property Version : TPackageVersion read GetVersion;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
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

  ///<summary> PackageIdentity plus dependencies. used when resolving.
  /// SupportedPlatforms is the set of platforms the package supports (per-compiler in the
  /// new single-package-per-compiler model). The IDE uses this to filter the feed by the
  /// platforms enabled on the current project. Populated from the spec's TargetPlatform.Platforms
  /// when reading a manifest, from the `platforms` JSON field when coming off the HTTP feed,
  /// and from the filename's platform segment when parsing from a .dpkg filename.
  /// </summary>
  IPackageInfo = interface(IPackageIdentity)
    ['{5672DB4A-40BC-45E0-857C-39117D03C322}']
    function GetDependencies : IList<IPackageDependency>;
    function GetUseSource : boolean;
    procedure SetUseSource(const value : boolean);
    function GetHash : string;
    function GetHashAlgorithm : string;
    function GetSupportedPlatforms : TDPMPlatforms;
    procedure SetSupportedPlatforms(const value : TDPMPlatforms);

    property Dependencies : IList<IPackageDependency>read GetDependencies;
    property UseSource : boolean read GetUseSource write SetUseSource;
    property Hash : string read GetHash;
    property HashAlgorithm : string read GetHashAlgorithm;
    property SupportedPlatforms : TDPMPlatforms read GetSupportedPlatforms write SetSupportedPlatforms;
  end;

  ///<summary>Package Info plus metadata. Mirrors ISpecMetaData's shape so everything the author wrote
  /// in the .dspec survives the round trip through a packed .dpkg — including ReleaseNotes, ReadMe
  /// and Frameworks (planned future use: filter the IDE feed by project type).</summary>
  IPackageMetadata = interface(IPackageInfo)
    ['{0C39A81D-63FF-4939-A74A-4BFE29724168}']
    function GetDescription : string;
    function GetAuthors : IList<string>;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : TStrings;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetProjectUrl : string;
    function GetSearchPaths : IList<string>;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetReleaseNotes : string;
    function GetReadMe : string;
    function GetFrameworks : TArray<TDPMUIFrameworkType>;

    property Description : string read GetDescription;
    //Authors matches the spec shape (IList<string>) — callers that want a display string
    //should join at the presentation layer instead of relying on a CSV-joined field here.
    property Authors : IList<string> read GetAuthors;
    property License : string read GetLicense;
    property Icon : string read GetIcon;
    property Copyright : string read GetCopyright;
    property Tags : TStrings read GetTags;
    property IsTrial : boolean read GetIsTrial;
    property IsCommercial : boolean read GetIsCommercial;
    property ProjectUrl : string read GetProjectUrl;
    property RepositoryUrl : string read GetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit;
    property SearchPaths : IList<string> read GetSearchPaths;
    property ReleaseNotes : string read GetReleaseNotes;
    property ReadMe : string read GetReadMe;
    property Frameworks : TArray<TDPMUIFrameworkType> read GetFrameworks;
  end;

  ///<summary>What a package repository returns for the UI.
  ///  Extends IPackageMetadata with server-only fields (download count, file hash,
  ///  published date, reserved-prefix flag, report url) and IDE-mutable UI state
  ///  (installed flag, latest-version cache, transitive flag, selected version range).
  ///</summary>
  IPackageSearchResultItem = interface(IPackageMetadata)
    ['{8EB6EA16-3708-41F7-93A2-FE56EB75510B}']

    //server-only fields
    function GetReportUrl : string;
    function GetPublishedDate : string;
    function GetDownloadCount : Int64;
    function GetFileHash : string;
    function GetIsReservedPrefix : boolean;

    //UI-mutable state
    function GetInstalled : boolean;
    function GetLatestVersion : TPackageVersion;
    function GetLatestStableVersion : TPackageVersion;
    function GetIsLatestVersion : boolean;
    function GetIsLatestStableVersion : boolean;
    function GetIsTransitive : boolean;
    function GetVersionRange : TVersionRange;

    function GetIsError : boolean;

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

    //Version is re-declared writable here (IPackageIdentity exposes it read-only)
    property Version : TPackageVersion read GetVersion write SetVersion;

    //Repository* re-declared writable (IPackageMetadata exposes them read-only)
    property RepositoryUrl : string read GetRepositoryUrl write SetRepositoryUrl;
    property RepositoryType : string read GetRepositoryType write SetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch write SetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit write SetRepositoryCommit;

    //only returned from server feeds.
    property IsReservedPrefix : boolean read GetIsReservedPrefix;
    //returns -1 if not set.
    property Downloads : Int64 read GetDownloadCount;
    property FileHash : string read GetFileHash;
    property ReportUrl : string read GetReportUrl write SetReportUrl;
    property PublishedDate : string read GetPublishedDate write SetPublishedDate;

    //these are for use by the UI, not returned.
    property Installed : boolean read GetInstalled write SetInstalled;
    property LatestVersion : TPackageVersion read GetLatestVersion write SetLatestVersion;
    property LatestStableVersion : TPackageVersion read GetLatestStableVersion write SetLatestStableVersion;
    property IsLatestVersion : boolean read GetIsLatestVersion;
    property IsLatestStableVersion : boolean read GetIsLatestStableVersion;
    property IsTransitive : boolean read GetIsTransitive write SetIsTransitive;
    property VersionRange : TVersionRange read GetVersionRange write SetVersionRange;

    property IsError : boolean read GetIsError;
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
    function GetPlatforms : TDPMPlatforms;
    function GetCompilerVersion : TCompilerVersion;
    procedure SetPlatforms(const value : TDPMPlatforms);
    function IsSamePackageVersion(const item : IPackageListItem) : Boolean;
    function IsSamePackageId(const item : IPackageListItem) : boolean;
    function MergeWith(const item : IPackageListItem) : IPackageListItem;

    property Id : string read GetId;
    property CompilerVersion : TCompilerVersion read GetCompilerVersion;
    property Version : TPackageVersion read GetVersion;
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
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
  s := Value.Id + CompilerToString(value.CompilerVersion) + Value.Version.ToStringNoMeta + DPMPlatformsToString(Value.Platforms);
  {$IF CompilerVersion >= 29.0}
  Result := System.Hash.THashBobJenkins.GetHashValue(s);
  {$ELSE}
  Result := BobJenkinsHash(PChar(s)^, SizeOf(Char) * Length(s), 0);
  {$IFEND}
end;

end.

