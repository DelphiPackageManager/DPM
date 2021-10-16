unit DPM.Core.Package.SearchResults;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces;

type
  TDPMPackageSearchResultItem = class(TInterfacedObject, IPackageSearchResultItem)
  private
    FIsError : boolean;
    FAuthors : string;
    FOwners : string;
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
    FVersion : string;

    FDownloadCount : Int64;
    FInstalled : boolean;
    FLatestVersion : string;
    FLatestStableVersion : string;
    FIsReservedPrefix : boolean;
    FSourceName : string;
    FPublishedDate : string;
  protected
    function GetAuthors : string;
    function GetOwners : string;
    function GetCopyright : string;
    function GetDependencies : IList<IPackageDependency>;
    function GetDescription : string;
    function GetIcon : string;
    function GetId : string;

    function GetPublishedDate : string;
    function GetReportUrl : string;
    function GetInstalled : Boolean;
    function GetLatestVersion : string;
    function GetLatestStableVersion : string;
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
    function GetVersion : string;
    function GetDownloadCount : Int64;
    function GetSourceName : string;
    function GetIsError : boolean;
    function GetIsTransitive : Boolean;

    procedure SetVersion(const value : string);
    procedure SetPublishedDate(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetReportUrl(const value : string);
    procedure SetInstalled(const value : Boolean);
    procedure SetLatestVersion(const value : string);
    procedure SetLatestStableVersion(const value : string);
    procedure SetIsTransitive(const value : Boolean);



    constructor CreateFromJson(const sourceName : string; const jsonObject : TJsonObject);
    constructor CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata);
    constructor CreateFromError(const id : string; const version : TPackageVersion; const errorDescription : string);
  public
    class function FromJson(const sourceName : string; const jsonObject : TJsonObject) : IPackageSearchResultItem;
    class function FromMetaData(const sourceName : string; const metaData : IPackageMetadata) : IPackageSearchResultItem;
    class function FromError(const id : string; const version : TPackageVersion; const errorDescription : string) : IPackageSearchResultItem;
  end;


implementation


{ TDPMPackageSearchResultItem }

constructor TDPMPackageSearchResultItem.CreateFromError(const id : string; const version : TPackageVersion; const errorDescription : string);
begin
  FIsError := true;
  FId := id;
  FVersion := version.ToString;
  FDescription := errorDescription;
end;

constructor TDPMPackageSearchResultItem.CreateFromJson(const sourceName : string; const jsonObject : TJsonObject);
begin
  FDependencies := TCollections.CreateList<IPackageDependency>;

  //TODO : implement this;
end;

constructor TDPMPackageSearchResultItem.CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata);
begin
  FSourceName := sourceName;
  FPlatform := metaData.Platform;
  FCompilerVersion := metaData.CompilerVersion;
  FDependencies := TCollections.CreateList<IPackageDependency>;
  FDependencies.AddRange(metaData.Dependencies);
  FAuthors := metaData.Authors;
  FOwners := metaData.Authors;
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
  FVersion := metaData.Version.ToStringNoMeta;
  FDownloadCount := 123456; //-1; //indicates not set;
  FIsReservedPrefix := false;
end;

class function TDPMPackageSearchResultItem.FromError(const id : string; const version : TPackageVersion; const errorDescription : string) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromError(id, version, errorDescription);
end;

class function TDPMPackageSearchResultItem.FromJson(const sourceName : string; const jsonObject : TJsonObject) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromJson(sourceName, jsonObject);
end;

class function TDPMPackageSearchResultItem.FromMetaData(const sourceName : string; const metaData : IPackageMetadata) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromMetaData(sourceName, metaData);
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

function TDPMPackageSearchResultItem.GetIsReservedPrefix : Boolean;
begin
  result := FIsReservedPrefix;
end;

function TDPMPackageSearchResultItem.GetIsTransitive : Boolean;
begin
  result := FIsTransitive;
end;

function TDPMPackageSearchResultItem.GetIsTrial : Boolean;
begin
  result := FIsTrial;
end;

function TDPMPackageSearchResultItem.GetLatestStableVersion: string;
begin
  result := FLatestStableVersion;
end;

function TDPMPackageSearchResultItem.GetLatestVersion: string;
begin
  result := FLatestVersion;
end;

function TDPMPackageSearchResultItem.GetLicense : string;
begin
  result := FLicense;
end;

function TDPMPackageSearchResultItem.GetOwners : string;
begin
  result := FOwners;
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

function TDPMPackageSearchResultItem.GetVersion : string;
begin
  result := FVersion;
end;


procedure TDPMPackageSearchResultItem.SetInstalled(const value : Boolean);
begin
  FInstalled := value;
end;


procedure TDPMPackageSearchResultItem.SetIsTransitive(const value : Boolean);
begin
  FIsTransitive := value;
end;

procedure TDPMPackageSearchResultItem.SetLatestStableVersion(const value: string);
begin
  FLatestStableVersion := value;
end;

procedure TDPMPackageSearchResultItem.SetLatestVersion(const value: string);
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

procedure TDPMPackageSearchResultItem.SetVersion(const value: string);
begin
  FVersion := value;
end;

end.

