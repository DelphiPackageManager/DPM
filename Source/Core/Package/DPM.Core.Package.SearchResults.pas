unit DPM.Core.Package.SearchResults;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Package.Interfaces;

type
  TDPMPackageSearchResultItem = class(TInterfacedObject, IPackageSearchResultItem)
  private
    FIsError : boolean;
    FAuthors : string;
    FCopyright : string;
    FDependencies : IList<IPackageDependency>;
    FDescription : string;
    FIcon : string;
    FId : string;
    FIsCommercial : Boolean;
    FIsTrial : Boolean;
    FIsTransitive : boolean;
    FLicense : string;
    FPlatform : TDPMPlatform;
    FCompilerVersion : TCompilerVersion;
    FProjectUrl : string;
    FRepositoryUrl : string;
    FRepositoryType : string;
    FRepositoryBranch : string;
    FRepositoryCommit : string;
    FReportUrl : string;
    FTags : string;

    FVersion : TPackageVersion;
    FLatestVersion : TPackageVersion;
    FLatestStableVersion : TPackageVersion;
    FVersionRange : TVersionRange;

    FDownloadCount : Int64;
    FFileHash : string;
    FHashAlgorithm : string;

    FInstalled : boolean;
    FIsReservedPrefix : boolean;
    FSourceName : string;
    FPublishedDate : string;
  protected
    function GetAuthors : string;
    function GetCopyright : string;
    function GetDependencies : IList<IPackageDependency>;
    function GetDescription : string;
    function GetIcon : string;
    function GetId : string;

    function GetPublishedDate : string;
    function GetReportUrl : string;
    function GetInstalled : Boolean;
    function GetLatestVersion : TPackageVersion;
    function GetLatestStableVersion : TPackageVersion;
    function GetIsReservedPrefix : Boolean;

    function GetIsCommercial : Boolean;
    function GetIsTrial : Boolean;
    function GetLicense : string;
    function GetPlatform : TDPMPlatform;
    function GetCompilerVersion : TCompilerVersion;
    function GetProjectUrl : string;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetTags : string;
    function GetVersion : TPackageVersion;
    function GetDownloadCount : Int64;
    function GetHashAlgorithm : string;
    function GetFileHash : string;
    function GetSourceName : string;
    function GetIsError : boolean;
    function GetIsTransitive : Boolean;
    function GetIsLatestVersion : boolean;
    function GetIsLatestStableVersion : boolean;
    function GetIsStableVersion : boolean;
    function GetVersionRange : TVersionRange;

    function ToIdVersionString : string;

    procedure SetVersion(const value : TPackageVersion);
    procedure SetPublishedDate(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetReportUrl(const value : string);
    procedure SetInstalled(const value : Boolean);
    procedure SetLatestVersion(const value : TPackageVersion);
    procedure SetLatestStableVersion(const value : TPackageVersion);
    procedure SetIsTransitive(const value : Boolean);
    procedure SetVersionRange(const value : TVersionRange);
    constructor CreateFromJson(const sourceName : string; const jsonObject : TJsonObject);
    constructor CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata; fileHash : string; hasHalgorithm : string);
    constructor CreateFromError(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion; const platform : TDPMPlatform; const errorDescription : string);

  public
    destructor Destroy;override;
    class function FromJson(const sourceName : string; const jsonObject : TJsonObject) : IPackageSearchResultItem;
    class function FromMetaData(const sourceName : string; const metaData : IPackageMetadata; fileHash : string; hasHalgorithm : string) : IPackageSearchResultItem;
    class function FromError(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion; const platform : TDPMPlatform; const errorDescription : string) : IPackageSearchResultItem;
  end;

  TDPMPackageSearchResult = class(TInterfacedObject, IPackageSearchResult)
  private
    FSkip : Int64;
    FTotalCount : Int64;
    FResults : IList<IPackageSearchResultItem>;
  protected
    function GetResults: IList<IPackageSearchResultItem>;
    function GetTotalCount: Int64;
    function GetSkip: Int64;
    procedure SetSkip(const value : Int64);
    procedure SetTotalCount(const value : Int64);
  public
    constructor Create(const skip : Int64; const total : Int64);
  end;


implementation

uses
  System.SysUtils,
  DPM.Core.Package.Dependency;

{ TDPMPackageSearchResultItem }

constructor TDPMPackageSearchResultItem.CreateFromError(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion; const platform : TDPMPlatform; const errorDescription : string);
begin
  FIsError := true;
  FId := id;
  FVersion := version;
  FDescription := errorDescription;
  FCompilerVersion := compiler;
  FPlatform := platform;
  FLatestVersion := version;
  FLatestStableVersion := version;
  FVersionRange := TVersionRange.Empty;
end;

constructor TDPMPackageSearchResultItem.CreateFromJson(const sourceName : string; const jsonObject : TJsonObject);
var
  depArr : TJsonArray;
  depId : string;
  depVersion : string;
  i: Integer;
  range : TVersionRange;
  dependency : IPackageDependency;
begin
  FSourceName := sourceName;
  FCompilerVersion := StringToCompilerVersion(jsonObject.S['compiler']);
  if FCompilerVersion = TCompilerVersion.UnknownVersion then
    raise EArgumentOutOfRangeException.Create('Invalid compiler version returned from server : ' + jsonObject.S['compiler']);

  FPlatform := StringToDPMPlatform(jsonObject.S['platform']);
  if FPlatform = TDPMPlatform.UnknownPlatform then
    raise EArgumentOutOfRangeException.Create('Invalid platform returned from server : ' + jsonObject.S['platform']);

  FDependencies := TCollections.CreateList<IPackageDependency>;

  FId               := jsonObject.S['id'];
  FVersion          := TPackageVersion.Parse(jsonObject.S['version']);

  FAuthors          := jsonObject.S['authors'];
  FCopyright        := jsonObject.S['copyright'];
  FDescription      := jsonObject.S['description'];
  FIcon             := jsonObject.S['icon'];
  FIsCommercial     := jsonObject.B['isCommercial'];
  FIsTrial          := jsonObject.B['isTrial'];
  FLicense          := jsonObject.S['license'];
  FProjectUrl       := jsonObject.S['projectUrl'];
  FRepositoryUrl    := jsonObject.S['repositoryUrl'];
  FRepositoryType   := jsonObject.S['repositoryType'];
  FRepositoryBranch := jsonObject.S['repositoryBranch'];
  FRepositoryCommit := jsonObject.S['repositoryCommit'];
  FReportUrl        := jsonObject.S['reportUrl'];
  FTags             := jsonObject.S['tags'];

  FDownloadCount    := jsonObject.L['totalDownloads'];
  FHashAlgorithm    := jsonObject.S['hashAlgorithm'];
  FFileHash         := jsonObject.S['hash'];

  FLatestVersion    := TPackageVersion.Parse(jsonObject.S['latestVersion']);

  if jsonObject.Contains('latestStableVersion') and ( not jsonObject.IsNull('latestStableVersion')  )then
    FLatestStableVersion := TPackageVersion.Parse(jsonObject.S['latestStableVersion'])
  else
    FLatestStableVersion := TPackageVersion.Empty;
  FIsReservedPrefix := jsonObject.B['isReservedPrefix'];
  FVersionRange := TVersionRange.Empty;

  if jsonObject.Contains('dependencies') and (not jsonObject.IsNull('dependencies')) then
  begin
    depArr := jsonObject.A['dependencies'];
    for i := 0 to depArr.Count -1 do
    begin
      depId := depArr.O[i].S['packageId'];
      depVersion := depArr.O[i].S['versionRange'];

      if TVersionRange.TryParse(depVersion, range) then
      begin
        dependency := TPackageDependency.Create(depId, range, FPlatform);
        FDependencies.Add(dependency);
      end;
    end;
  end;


end;

constructor TDPMPackageSearchResultItem.CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata; fileHash : string; hasHalgorithm : string);
begin
  FSourceName := sourceName;
  FPlatform := metaData.Platform;
  FCompilerVersion := metaData.CompilerVersion;
  FDependencies := TCollections.CreateList<IPackageDependency>;
  if metaData.Dependencies <> nil then
    FDependencies.AddRange(metaData.Dependencies);
  FAuthors := metaData.Authors;
  FCopyright := metaData.Copyright;
  FDescription := metaData.Description;
  FIcon := metaData.Icon;
  FId := metaData.Id;
  FIsCommercial := metaData.IsCommercial;
  FIsTrial := metaData.IsTrial;
  FLicense := metaData.License;
  FProjectUrl := metaData.ProjectUrl;
  FRepositoryUrl := metaData.RepositoryUrl;
  FRepositoryType := metaData.RepositoryType;
  FRepositoryBranch := metaData.RepositoryBranch;
  FRepositoryCommit := metaData.RepositoryCommit;
  FTags := metaData.Tags;
  FVersion := metaData.Version;
  FDownloadCount := -1; //indicates not set;
  FIsReservedPrefix := false;
  FVersionRange := TVersionRange.Empty;
  FFileHash := fileHash;
  FHashAlgorithm := hashAlgorithm;
end;

destructor TDPMPackageSearchResultItem.Destroy;
begin
  FDependencies := nil;
  inherited;
end;

class function TDPMPackageSearchResultItem.FromError(const id : string; const version : TPackageVersion; const compiler : TCompilerVersion; const platform : TDPMPlatform; const errorDescription : string) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromError(id, version, compiler, platform, errorDescription);
end;

class function TDPMPackageSearchResultItem.FromJson(const sourceName : string; const jsonObject : TJsonObject) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromJson(sourceName, jsonObject);
end;

class function TDPMPackageSearchResultItem.FromMetaData(const sourceName : string; const metaData : IPackageMetadata; fileHash : string; hasHalgorithm : string) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromMetaData(sourceName, metaData, fileHash, hasHalgorithm);
end;

function TDPMPackageSearchResultItem.GetAuthors : string;
begin
  result := FAuthors;
end;

function TDPMPackageSearchResultItem.GetCompilerVersion: TCompilerVersion;
begin
  result := FCompilerVersion;
end;

function TDPMPackageSearchResultItem.GetCopyright : string;
begin
  result := FCopyright;
end;

function TDPMPackageSearchResultItem.GetDependencies : IList<IPackageDependency>;
begin
  result := FDependencies;
end;

function TDPMPackageSearchResultItem.GetDescription : string;
begin
  result := FDescription;
end;

function TDPMPackageSearchResultItem.GetDownloadCount : Int64;
begin
  result := FDownloadCount;
end;

function TDPMPackageSearchResultItem.GetFileHash: string;
begin
  result := FFileHash;
end;

function TDPMPackageSearchResultItem.GetHashAlgorithm: string;
begin
  result := FHashAlgorithm;
end;

function TDPMPackageSearchResultItem.GetIcon : string;
begin
  result := FIcon;
end;

function TDPMPackageSearchResultItem.GetId : string;
begin
  result := FId;
end;

function TDPMPackageSearchResultItem.GetInstalled : Boolean;
begin
  result := FInstalled;
end;


function TDPMPackageSearchResultItem.GetIsCommercial : Boolean;
begin
  result := FIsCommercial;
end;

function TDPMPackageSearchResultItem.GetIsError : boolean;
begin
  result := FIsError;
end;

function TDPMPackageSearchResultItem.GetIsLatestStableVersion: boolean;
begin
  result := FVersion = FLatestStableVersion;
end;

function TDPMPackageSearchResultItem.GetIsLatestVersion: boolean;
begin
  result := FVersion = FLatestVersion;
end;

function TDPMPackageSearchResultItem.GetIsReservedPrefix : Boolean;
begin
  result := FIsReservedPrefix;
end;

function TDPMPackageSearchResultItem.GetIsStableVersion: boolean;
begin
  result := FVersion.IsStable;
end;

function TDPMPackageSearchResultItem.GetIsTransitive : Boolean;
begin
  result := FIsTransitive;
end;

function TDPMPackageSearchResultItem.GetIsTrial : Boolean;
begin
  result := FIsTrial;
end;

function TDPMPackageSearchResultItem.GetLatestStableVersion: TPackageVersion;
begin
  result := FLatestStableVersion;
end;

function TDPMPackageSearchResultItem.GetLatestVersion: TPackageVersion;
begin
  result := FLatestVersion;
end;

function TDPMPackageSearchResultItem.GetLicense : string;
begin
  result := FLicense;
end;


function TDPMPackageSearchResultItem.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

function TDPMPackageSearchResultItem.GetProjectUrl : string;
begin
  result := FProjectUrl;
end;

function TDPMPackageSearchResultItem.GetPublishedDate : string;
begin
  result := FPublishedDate;
end;

function TDPMPackageSearchResultItem.GetReportUrl : string;
begin
  result := FReportUrl;
end;

function TDPMPackageSearchResultItem.GetRepositoryBranch: string;
begin
  result := FRepositoryBranch;
end;

function TDPMPackageSearchResultItem.GetRepositoryCommit: string;
begin
  result := FRepositoryCommit;
end;

function TDPMPackageSearchResultItem.GetRepositoryType: string;
begin
  result := FRepositoryType;
end;

function TDPMPackageSearchResultItem.GetRepositoryUrl: string;
begin
  result := FRepositoryUrl;
end;

function TDPMPackageSearchResultItem.GetSourceName : string;
begin
  result := FSourceName;
end;

function TDPMPackageSearchResultItem.GetTags : string;
begin
  result := FTags;
end;

function TDPMPackageSearchResultItem.GetVersion : TPackageVersion;
begin
  result := FVersion;
end;


function TDPMPackageSearchResultItem.GetVersionRange: TVersionRange;
begin
  result := FVersionRange;
end;

procedure TDPMPackageSearchResultItem.SetInstalled(const value : Boolean);
begin
  FInstalled := value;
end;


procedure TDPMPackageSearchResultItem.SetIsTransitive(const value : Boolean);
begin
  FIsTransitive := value;
end;

procedure TDPMPackageSearchResultItem.SetLatestStableVersion(const value: TPackageVersion);
begin
  FLatestStableVersion := value;
end;

procedure TDPMPackageSearchResultItem.SetLatestVersion(const value: TPackageVersion);
begin
  FLatestVersion := value;
end;

procedure TDPMPackageSearchResultItem.SetPublishedDate(const value : string);
begin
  FPublishedDate := value;
end;

procedure TDPMPackageSearchResultItem.SetReportUrl(const value : string);
begin
  FReportUrl := value;
end;

procedure TDPMPackageSearchResultItem.SetRepositoryBranch(const value: string);
begin
  FRepositoryBranch := value;
end;

procedure TDPMPackageSearchResultItem.SetRepositoryCommit(const value: string);
begin
  FRepositoryCommit := value;
end;

procedure TDPMPackageSearchResultItem.SetRepositoryType(const value: string);
begin
  FRepositoryType := value;
end;

procedure TDPMPackageSearchResultItem.SetRepositoryUrl(const value: string);
begin
  FRepositoryUrl := value;
end;

procedure TDPMPackageSearchResultItem.SetVersion(const value: TPackageVersion);
begin
  FVersion := value;
end;

procedure TDPMPackageSearchResultItem.SetVersionRange(const value: TVersionRange);
begin
  FVersionRange := value;
end;

function TDPMPackageSearchResultItem.ToIdVersionString: string;
begin
  result := FId + ' [' + FVersion.ToStringNoMeta + ']';
end;

{ TDPMPackageSearchResult }

constructor TDPMPackageSearchResult.Create(const skip : Int64; const total: Int64);
begin
  FSkip := skip;
  FTotalCount := total;
  FResults := TCollections.CreateList<IPackageSearchResultItem>;
end;

function TDPMPackageSearchResult.GetResults: IList<IPackageSearchResultItem>;
begin
  result := FResults;
end;

function TDPMPackageSearchResult.GetSkip: Int64;
begin
  Result := FSkip;
end;

function TDPMPackageSearchResult.GetTotalCount: Int64;
begin
  result := FTotalCount;
end;

procedure TDPMPackageSearchResult.SetSkip(const value: Int64);
begin
  FSkip := value;
end;

procedure TDPMPackageSearchResult.SetTotalCount(const value: Int64);
begin
  FTotalCount := value;
end;


initialization
 JsonSerializationConfig.NullConvertsToValueTypes := true;
end.

