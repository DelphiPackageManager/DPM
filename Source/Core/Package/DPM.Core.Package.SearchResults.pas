unit DPM.Core.Package.SearchResults;

interface

uses
  Spring.Collections,
  JsonDataObjects,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces;

type
  TDPMPackagePlatformDependencies = class(TInterfacedObject, IPackagePlatformDependencies)
  private
    FDependencies : IList<IPackageDependency>;
    FPlatform : TDPMPlatform;
  protected
    function GetDependencies : IList<IPackageDependency>;
    function GetPlatform : TDPMPlatform;
  public
    constructor Create(const platform : TDPMPlatform; const dependencies : IList<IPackageDependency>);
  end;


  TDPMPackageVersionResult = class(TInterfacedObject, IPackageVersionResult)
  private
    FDependencies : IList<IPackagePlatformDependencies>;
    FPlatforms : TDPMPlatforms;
    FVersion : string;
  protected
    function GetDependencies : IList<IPackagePlatformDependencies>;
    function GetPlatforms : TDPMPlatforms;
    function GetVersion : string;
  public
    constructor Create(const version : string; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>);
  end;

  TDPMPackageVersionsResults = class(TInterfacedObject, IPackageVersionsResults)
  private
    FId : string;
    FResults : IList<IPackageVersionResult>;
  protected
    function GetId : string;
    function GetResults : IList<IPackageVersionResult>;
  public
    constructor Create(const id : string; const results : IList<IPackageVersionResult>);
  end;


  TDPMPackageSearchResultItem = class(TInterfacedObject, IPackageSearchResultItem)
  private
    FIsError : boolean;
    FAuthors : string;
    FOwners : string;
    FCopyright : string;
    FDependencies : IList<IPackagePlatformDependencies>;
    FDescription : string;
    FIcon : string;
    FId : string;
    FIsCommercial : Boolean;
    FIsTrial : Boolean;
    FIsTransitive : boolean;
    FLicense : string;
    FPlatforms : TDPMPlatforms;
    FProjectUrl : string;
    FReportUrl : string;
    FTags : string;
    FVersion : string;

    FDownloadCount : Int64;
    FInstalled : boolean;
    FLatestVersion : string;
    FIsReservedPrefix : boolean;
    FSourceName : string;
    FPublishedDate : string;
  protected
    function GetAuthors : string;
    function GetOwners : string;
    function GetCopyright : string;
    function GetDependencies : IList<IPackagePlatformDependencies>;
    function GetDescription : string;
    function GetIcon : string;
    function GetId : string;

    function GetPublishedDate : string;
    function GetReportUrl : string;
    function GetInstalled : Boolean;
    function GetLatestVersion : string;
    function GetIsReservedPrefix : Boolean;

    function GetIsCommercial : Boolean;
    function GetIsTrial : Boolean;
    function GetLicense : string;
    function GetPlatforms : TDPMPlatforms;
    function GetProjectUrl : string;
    function GetTags : string;
    function GetVersion : string;
    function GetDownloadCount : Int64;
    function GetSourceName : string;
    function GetIsError : boolean;
    function GetIsTransitive : Boolean;

    procedure SetVersion(const value : string);
    procedure SetPublishedDate(const value : string);
    procedure SetReportUrl(const value : string);
    procedure SetInstalled(const value : Boolean);
    procedure SetLatestVersion(const value : string);
    procedure SetIsTransitive(const value : Boolean);



    constructor CreateFromJson(const sourceName : string; const jsonObject : TJsonObject);
    constructor CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>);
    constructor CreateFromError(const id : string; const version : TPackageVersion; const errorDescription : string);
  public
    class function FromJson(const sourceName : string; const jsonObject : TJsonObject) : IPackageSearchResultItem;
    class function FromMetaData(const sourceName : string; const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>) : IPackageSearchResultItem;
    class function FromError(const id : string; const version : TPackageVersion; const errorDescription : string) : IPackageSearchResultItem;
  end;


implementation

{ TDPMPackagePlatformDependencies }

constructor TDPMPackagePlatformDependencies.Create(const platform : TDPMPlatform; const dependencies : IList<IPackageDependency>);
begin
  FPlatform := platform;
  FDependencies := TCollections.CreateList<IPackageDependency>;
  FDependencies.AddRange(dependencies);
end;

function TDPMPackagePlatformDependencies.GetDependencies : IList<IPackageDependency>;
begin
  result := FDependencies;
end;

function TDPMPackagePlatformDependencies.GetPlatform : TDPMPlatform;
begin
  result := FPlatform;
end;

{ TDPMPackageVersionResult }

constructor TDPMPackageVersionResult.Create(const version : string; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>);
begin
  FVersion := version;
  FPlatforms := platforms;
  FDependencies := dependencies;
end;

function TDPMPackageVersionResult.GetDependencies : IList<IPackagePlatformDependencies>;
begin
  result := FDependencies;
end;

function TDPMPackageVersionResult.GetPlatforms : TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TDPMPackageVersionResult.GetVersion : string;
begin
  result := FVersion;
end;

{ TDPMPackageVersionsResults }

constructor TDPMPackageVersionsResults.Create(const id : string; const results : IList<IPackageVersionResult>);
begin
  FId := id;
  FResults := results;
end;

function TDPMPackageVersionsResults.GetId : string;
begin
  result := FId;
end;

function TDPMPackageVersionsResults.GetResults : IList<IPackageVersionResult>;
begin
  result := FResults;
end;

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
  FDependencies := TCollections.CreateList<IPackagePlatformDependencies>;

  //TODO : implement this;
end;

constructor TDPMPackageSearchResultItem.CreateFromMetaData(const sourceName : string; const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>);
begin
  FSourceName := sourceName;
  FPlatforms := platforms;
  FDependencies := TCollections.CreateList<IPackagePlatformDependencies>;
  FDependencies.AddRange(dependencies);
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

class function TDPMPackageSearchResultItem.FromMetaData(const sourceName : string; const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>) : IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromMetaData(sourceName, metaData, platforms, dependencies);
end;

function TDPMPackageSearchResultItem.GetAuthors : string;
begin
  result := FAuthors;
end;

function TDPMPackageSearchResultItem.GetCopyright : string;
begin
  result := FCopyright;
end;

function TDPMPackageSearchResultItem.GetDependencies : IList<IPackagePlatformDependencies>;
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

function TDPMPackageSearchResultItem.GetPlatforms : TDPMPlatforms;
begin
  result := FPlatforms;
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

procedure TDPMPackageSearchResultItem.SetVersion(const value: string);
begin
  FVersion := value;
end;

end.

