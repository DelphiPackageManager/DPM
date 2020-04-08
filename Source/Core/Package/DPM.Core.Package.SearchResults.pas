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
    FDependencies: IList<IPackageDependency>;
    FPlatform: TDPMPlatform;

  protected
    function GetDependencies: IList<IPackageDependency>;
    function GetPlatform: TDPMPlatform;
  public
    constructor Create(const platform : TDPMPlatform; const dependencies : IList<IPackageDependency>);
  end;


  TDPMPackageVersionResult = class(TInterfacedObject, IPackageVersionResult)
  private
    FDependencies: IList<IPackagePlatformDependencies>;
    FPlatforms: TDPMPlatforms;
    FVersion: string;
  protected
    function GetDependencies: IList<IPackagePlatformDependencies>;
    function GetPlatforms: TDPMPlatforms;
    function GetVersion: string;
  public
    constructor Create(const version : string; const platforms : TDPMPlatforms; const dependencies : IList<IPackagePlatformDependencies>);
  end;

  TDPMPackageVersionsResults = class(TInterfacedObject,  IPackageVersionsResults)
  private
    FId: string;
    FResults: IList<IPackageVersionResult>;
  protected
    function GetId: string;
    function GetResults: IList<IPackageVersionResult>;
  public
    constructor Create(const id : string; const results : IList<IPackageVersionResult>);
  end;


  TDPMPackageSearchResultItem = class(TInterfacedObject, IPackageSearchResultItem)
  private
    FAuthors: string;
    FCopyright: string;
    FDependencies: IList<IPackagePlatformDependencies>;
    FDescription: string;
    FIcon: string;
    FId: string;
    FIsCommercial: Boolean;
    FIsTrial: Boolean;
    FLicense: string;
    FPlatforms: TDPMPlatforms;
    FProjectUrl: string;
    FTags: string;
    FVersion: string;
  protected
    function GetAuthors: string;
    function GetCopyright: string;
    function GetDependencies: IList<IPackagePlatformDependencies>;
    function GetDescription: string;
    function GetIcon: string;
    function GetId: string;
    function GetIsCommercial: Boolean;
    function GetIsTrial: Boolean;
    function GetLicense: string;
    function GetPlatforms: TDPMPlatforms;
    function GetProjectUrl: string;
    function GetTags: string;
    function GetVersion: string;

    constructor CreateFromJson(const jsonObject : TJsonObject);
//    constructor Create(const id: string; const authors: string; const copyright: string; const dependencies: IList<IPackagePlatformDependencies>;
//                       const description: string; const icon: string; const isCommercial: Boolean; const isTrial: Boolean; const license: string;
//                       const platforms: TDPMPlatforms; const projectUrl: string; const tags: string; const version: string);
    constructor CreateFromMetaData(const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies: IList<IPackagePlatformDependencies>);
  public
    class function FromJson(const jsonObject : TJsonObject) : IPackageSearchResultItem;
    class function FromMetaData(const metaData : IPackageMetadata; const platforms : TDPMPlatforms; const dependencies: IList<IPackagePlatformDependencies>) : IPackageSearchResultItem;

  end;


implementation

{ TDPMPackagePlatformDependencies }

constructor TDPMPackagePlatformDependencies.Create(const platform: TDPMPlatform; const dependencies: IList<IPackageDependency>);
begin
  FPlatform := platform;
  FDependencies := dependencies;
end;

function TDPMPackagePlatformDependencies.GetDependencies: IList<IPackageDependency>;
begin
  result := FDependencies;
end;

function TDPMPackagePlatformDependencies.GetPlatform: TDPMPlatform;
begin
  result := FPlatform;
end;

{ TDPMPackageVersionResult }

constructor TDPMPackageVersionResult.Create(const version: string; const platforms: TDPMPlatforms; const dependencies: IList<IPackagePlatformDependencies>);
begin
  FVersion := version;
  FPlatforms := platforms;
  FDependencies := dependencies;
end;

function TDPMPackageVersionResult.GetDependencies: IList<IPackagePlatformDependencies>;
begin
  result := FDependencies;
end;

function TDPMPackageVersionResult.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TDPMPackageVersionResult.GetVersion: string;
begin
  result := FVersion;
end;

{ TDPMPackageVersionsResults }

constructor TDPMPackageVersionsResults.Create(const id: string; const results: IList<IPackageVersionResult>);
begin
  FId := id;
  FResults := results;
end;

function TDPMPackageVersionsResults.GetId: string;
begin
  result := FId;
end;

function TDPMPackageVersionsResults.GetResults: IList<IPackageVersionResult>;
begin
  result := FResults;
end;

{ TDPMPackageSearchResultItem }

constructor TDPMPackageSearchResultItem.CreateFromJson(const jsonObject: TJsonObject);
begin
  FDependencies := TCollections.CreateList<IPackagePlatformDependencies>;

  //TODO : implement this;
end;

constructor TDPMPackageSearchResultItem.CreateFromMetaData(const metaData: IPackageMetadata; const platforms: TDPMPlatforms; const dependencies: IList<IPackagePlatformDependencies>);
begin
  FPlatforms := platforms;
  FDependencies := dependencies;

  FAuthors := metaData.Authors;
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
end;

class function TDPMPackageSearchResultItem.FromJson(const jsonObject: TJsonObject): IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromJson(jsonObject);
end;

class function TDPMPackageSearchResultItem.FromMetaData(const metaData: IPackageMetadata; const platforms: TDPMPlatforms; const dependencies: IList<IPackagePlatformDependencies>): IPackageSearchResultItem;
begin
  result := TDPMPackageSearchResultItem.CreateFromMetaData(metaData, platforms, dependencies);
end;

function TDPMPackageSearchResultItem.GetAuthors: string;
begin
  result := FAuthors;
end;

function TDPMPackageSearchResultItem.GetCopyright: string;
begin
  result := FCopyright;
end;

function TDPMPackageSearchResultItem.GetDependencies: IList<IPackagePlatformDependencies>;
begin
  result := FDependencies;
end;

function TDPMPackageSearchResultItem.GetDescription: string;
begin
  result := FDescription;
end;

function TDPMPackageSearchResultItem.GetIcon: string;
begin
  result := FIcon;
end;

function TDPMPackageSearchResultItem.GetId: string;
begin
  result := FId;
end;

function TDPMPackageSearchResultItem.GetIsCommercial: Boolean;
begin
  result := FIsCommercial;
end;

function TDPMPackageSearchResultItem.GetIsTrial: Boolean;
begin
  result := FIsTrial;
end;

function TDPMPackageSearchResultItem.GetLicense: string;
begin
  result := FLicense;
end;

function TDPMPackageSearchResultItem.GetPlatforms: TDPMPlatforms;
begin
  result := FPlatforms;
end;

function TDPMPackageSearchResultItem.GetProjectUrl: string;
begin
  result := FProjectUrl;
end;

function TDPMPackageSearchResultItem.GetTags: string;
begin
  result := FTags;
end;

function TDPMPackageSearchResultItem.GetVersion: string;
begin
  result := FVersion;
end;

end.
