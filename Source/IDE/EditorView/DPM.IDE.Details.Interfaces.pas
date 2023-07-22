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
  //TODO : This is far to convoluted - we should not need to use options here.
  // we never actually want more than 1 searchresult so this needs to change to
  // just get the get the package metadata - which might actually come from the
  // package cache!
  IDetailsHost = interface
    ['{4FBB9E7E-886A-4B7D-89FF-FA5DBC9D93FD}']

    function GetPackageReferences : IPackageReference;
    //tell the IDE to saveall before installing or updating packages.
    procedure SaveBeforeInstall;
    procedure PackageInstalled(const package : IPackageSearchResultItem; const isUpdate : boolean);
    procedure PackageUninstalled(const package : IPackageSearchResultItem);
  end;

  IPackageDetailsView = interface
  ['{B4B48A9A-D04A-4316-B3FB-B03E4BD763F3}']
    procedure Init(const container : TContainer; const iconCache : TDPMIconCache; const config : IConfiguration; const host : IDetailsHost; const projectOrGroup : IOTAProject);
    procedure SetPackage(const package : IPackageSearchResultItem; const preRelease : boolean; const fetchVersions : boolean = true);
    procedure SetPlatform(const platform : TDPMPlatform);
    procedure ViewClosing;
    procedure ProjectReloaded;
    procedure ThemeChanged(const StyleServices : TCustomStyleServices {$IFDEF THEMESERVICES}; const ideThemeSvc : IOTAIDEThemingServices{$ENDIF});
  end;

implementation

end.
