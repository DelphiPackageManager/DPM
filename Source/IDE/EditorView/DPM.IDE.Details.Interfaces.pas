unit DPM.IDE.Details.Interfaces;

interface

uses
  Vcl.Themes,
  ToolsApi,
  Spring.Container,
  Spring.Collections,
  VSoft.Awaitable,
  DPM.Core.Types,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Package.Interfaces,
  DPM.Core.Options.Search,
  DPM.Core.Dependency.Interfaces,
  DPM.IDE.IconCache,
  DPM.IDE.Types;

{$I ..\DPMIDE.inc}


type
  //implemented by the EditorViewFrame
  IPackageSearcher = interface
    ['{4FBB9E7E-886A-4B7D-89FF-FA5DBC9D93FD}']
    function GetSearchOptions : TSearchOptions;
    function SearchForPackagesAsync(const options : TSearchOptions) : IAwaitable<IList<IPackageSearchResultItem>>;overload;
    function SearchForPackages(const options : TSearchOptions) : IList<IPackageSearchResultItem>;overload;
    function GetCurrentPlatform : string;
    function GetPackageReferences : IGraphNode;
    procedure InstallStarting;
    procedure PackageInstalled(const package : IPackageSearchResultItem);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
  end;

  IPackageDetailsView = interface
  ['{B4B48A9A-D04A-4316-B3FB-B03E4BD763F3}']
    function GetIncludePreRelease : boolean;
    procedure SetIncludePreRelease(const value : boolean);

    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const packageSearcher : IPackageSearcher; const projectOrGroup : IOTAProject);
    procedure Configure(const value : TDPMCurrentTab; const preRelease : boolean);
    procedure SetPackage(const package : IPackageSearchResultItem);
    procedure SetPlatform(const platform : TDPMPlatform);
    procedure ViewClosing;
    procedure ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
    property IncludePreRelease : boolean read GetIncludePreRelease write SetIncludePreRelease;
  end;

implementation

end.
