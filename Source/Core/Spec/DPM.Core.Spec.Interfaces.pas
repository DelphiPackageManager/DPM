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

unit DPM.Core.Spec.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.TargetPlatform,
  DPM.Core.Dependency.Version,
  JsonDataObjects,
  DPM.Core.Options.Spec;


type
  ISpecNode = interface
    ['{AD47A3ED-591B-4E47-94F2-7EC136182202}']
    function LoadFromJson(const jsonObject : TJsonObject) : boolean;
    function LoadObjectList(const list: IList<ISpecNode>): TJsonArray;
    function ToJSON: string;
  end;

  ISpecDependency = interface(ISpecNode)
    ['{6CE14888-54A8-459C-865E-E4B4628DB8C6}']
    function GetId : string;
    procedure SetId(const id: string);
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function GetVersionString : string;
    function IsGroup : boolean;
    function Clone : ISpecDependency;
    property Id : string read GetId write SetId;
    property Version : TVersionRange read GetVersionRange write SetVersionRange;
    property VersionString : string read GetVersionString;
  end;


  ISpecDependencyGroup = interface(ISpecDependency)
    ['{98666E8B-0C15-4CEF-9C1C-18E90D86E378}']
    function GetTargetPlatform : TTargetPlatform;
    function GetDependencies : IList<ISpecDependency>;

    property TargetPlatform : TTargetPlatform read GetTargetPlatform;
    property Dependencies : IList<ISpecDependency>read GetDependencies;
  end;

  ISpecMetaData = interface(ISpecNode)
    ['{9972F2EA-4180-4D12-9193-13A55B927B81}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetDescription : string;
    function GetAuthors : string;
    function GetProjectUrl : string;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetReleaseNotes : string;
    function GetLicense : string;
    function GetLicenseType : TDPMLicenseType;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : string;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetReadMe : string;
    function GetUIFrameworkType : TDPMUIFrameworkType;


    procedure SetVersion(const value : TPackageVersion);
    procedure SetId(const value : string);
    procedure SetDescription(const value : string);
    procedure SetAuthors(const value : string);
    procedure SetProjectUrl(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetReleaseNotes(const value : string);
    procedure SetLicense(const value : string);
    procedure SetLicenseType(const value : TDPMLicenseType);
    procedure SetIcon(const value : string);
    procedure SetCopyright(const value : string);
    procedure SetTags(const value : string);
    procedure SetIsTrial(const value : boolean);
    procedure SetIsCommercial(const value : boolean);
    procedure SetReadMe(const value : string);
    procedure SetUIFrameworkType(const value : TDPMUIFrameworkType);


    property Id : string read GetId write SetId;
    property Version : TPackageVersion read GetVersion write SetVersion;
    property Description : string read GetDescription write SetDescription;
    property Authors : string read GetAuthors write SetAuthors;
    property ProjectUrl       : string read GetProjectUrl write SetProjectUrl;
    property RepositoryUrl    : string read GetRepositoryUrl write SetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType write SetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch write SetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit write SetRepositoryCommit;
    property ReleaseNotes     : string read GetReleaseNotes write SetReleaseNotes;
    property License : string read GetLicense write SetLicense;
    property LicenseType : TDPMLicenseType read GetLicenseType write SetLicenseType;
    property Icon : string read GetIcon write SetIcon;
    property Copyright : string read GetCopyright write SetCopyright;
    property Tags : string read GetTags write SetTags;
    property IsTrial : boolean read GetIsTrial write SetIsTrial;
    property IsCommercial : boolean read GetIsCommercial write SetIsCommercial;
    property ReadMe : string read GetReadMe write SetReadMe;
    property UIFrameworkType : TDPMUIFrameworkType read  GetUIFrameworkType write SetUIFrameworkType;

  end;

  ISpecFileEntry = interface(ISpecNode)
    ['{2BE821AA-94C7-439C-B236-85D8901FFA81}']
    function GetSource : string;
    function GetDestination : string;
    function GetExclude : IList<string>;
    function GetFlatten : boolean;
    procedure SetFlatten(value: Boolean);
    procedure SetSource(const value : string);
    procedure SetDestination(const value : string);
    function GetIgnore : boolean;

    function Clone : ISpecFileEntry;

    property Source : string read GetSource write SetSource;
    property Destination : string read GetDestination write SetDestination;
    property Exclude : IList<string>read GetExclude;
    property Flatten : boolean read GetFlatten write SetFlatten;
    property Ignore : boolean read GetIgnore;
  end;


  ISpecBPLEntry = interface(ISpecNode)
    ['{13723048-E2AA-45BE-A0F1-C446848F3936}']
    function GetSource : string;
    function GetCopyLocal : boolean;
    function GetInstall : boolean;
    function GetBuildId : string;

    procedure SetSource(const value : string);
    procedure SetBuildId(const value : string);
    procedure SetCopyLocal(const value : Boolean);
    procedure SetInstall(const value : Boolean);

    function Clone : ISpecBPLEntry;
    property Source : string read GetSource write SetSource;
    property CopyLocal : boolean read GetCopyLocal write SetCopyLocal; //ignored for design
    property Install : boolean read GetInstall write SetInstall; //ignored for runtime
    property BuildId : string read GetBuildId write SetBuildId;
  end;

  ISpecSearchPath = interface(ISpecNode)
    ['{493371C5-CD82-49EF-9D2A-BA7C4CFA2550}']
    function GetPath : string;
    procedure SetPath(const value : string);

    function IsGroup : boolean;
    function Clone : ISpecSearchPath;

    property Path : string read GetPath write SetPath;
  end;


  ISpecSearchPathGroup = interface(ISpecSearchPath)
    ['{B558E9C4-5B01-409F-AB59-5D8B71F0DCB1}']
    function GetTargetPlatform : TTargetPlatform;
    function GetSearchPaths : IList<ISpecSearchPath>;

    property TargetPlatform : TTargetPlatform read GetTargetPlatform;
    property SearchPaths : IList<ISpecSearchPath>read GetSearchPaths;
  end;

  //post build copy operations for res, dfm etc
  ISpecCopyEntry = interface(ISpecNode)
  ['{F36D7156-0537-4BF4-9D51-E873B797FA27}']
    function GetSource : string;
    function GetFlatten : boolean;

    function Clone : ISpecCopyEntry;

    property Source : string read GetSource;
    property Flatten : boolean read GetFlatten;
  end;

  ISpecBuildEntry = interface(ISpecNode)
    ['{9E1850EB-40C4-421F-A47F-03FDD6286573}']
    function GetId : string;
    function GetProject : string;
    function GetConfig : string;
    function GetBplOutputDir : string;
    function GetLibOutputDir : string;
    function GetDesignOnly : boolean;
    function GetBuildForDesign : boolean;
    function GetCopyFiles : IList<ISpecCopyEntry>;


    procedure SetId(const value : string);
    procedure SetProject(const value : string);
    procedure SetConfig(const value: string);
    procedure SetBplOutputDir(const value : string);
    procedure SetLibOutputDir(const value : string);
    procedure SetDesignOnly(const value : boolean);
    procedure SetBuildForDesign(const value : boolean);

    function Clone : ISpecBuildEntry;
    property Id : string read GetId write SetId;
    property Project : string read GetProject write SetProject;
    property Config : string read GetConfig write SetConfig;
    property LibOutputDir : string read GetLibOutputDir write SetLibOutputDir;
    property BplOutputDir : string read GetBplOutputDir write SetBplOutputDir;

    property DesignOnly   : boolean read GetDesignOnly write SetDesignOnly;
    property BuildForDesign : boolean read GetBuildForDesign write SetBuildForDesign;
    property CopyFiles : IList<ISpecCopyEntry> read GetCopyFiles;
  end;


  ISpecTemplateBase = interface(ISpecNode)
    ['{B4DE32F7-AE58-4519-B69D-2389F12EC63F}']
    function GetLibFiles : IList<ISpecFileEntry>;
    function GetSourceFiles : IList<ISpecFileEntry>;
    function GetFiles : IList<ISpecFileEntry>;
    function GetRuntimeFiles : IList<ISpecBPLEntry>;
    function GetDesignFiles : IList<ISpecBPLEntry>;
    function GetDependencies : IList<ISpecDependency>;
    function GetSearchPaths : IList<ISpecSearchPath>;
    function GetBuildEntries : IList<ISpecBuildEntry>;

    function NewSource(const src: string): ISpecFileEntry;
    function NewLib(const src: string): ISpecFileEntry;
    function NewFiles(const src: string): ISpecFileEntry;
    function NewSearchPath(const path : string) : ISpecSearchPath;
    function NewRuntimeBplBySrc(const src : string) : ISpecBPLEntry;
    function NewDesignBplBySrc(const src : string) : ISpecBPLEntry;
    function NewBuildEntryById(const id : string) : ISpecBuildEntry;
    function NewDependencyById(const id : string) : ISpecDependency;

    procedure DeleteSource(const src: string);
    procedure DeleteLib(const src: string);
    procedure DeleteFiles(const src: string);
    procedure DeleteSearchPath(const path : string);
    procedure DeleteRuntimeBplBySrc(const src : string);
    procedure DeleteDesignBplBySrc(const src : string);
    procedure DeleteBuildEntryById(const src : string);
    procedure DeleteDependencyById(const id : string);

    function FindDependencyById(const id : string) : ISpecDependency;
    function FindDependencyGroupByTargetPlatform(const targetPlatform : TTargetPlatform) : ISpecDependencyGroup;
    function FindSearchPathByPath(const path : string) : ISpecSearchPath;
    function FindRuntimeBplBySrc(const src : string) : ISpecBPLEntry;
    function FindDesignBplBySrc(const src : string) : ISpecBPLEntry;
    function FindLibFileBySrc(const src : string) : ISpecFileEntry;
    function FindSourceFileBySrc(const src : string) : ISpecFileEntry;
    function FindOtherFileBySrc(const src : string) : ISpecFileEntry;
    function FindBuildEntryById(const id : string) : ISpecBuildEntry;

    property LibFiles : IList<ISpecFileEntry>read GetLibFiles;
    property SourceFiles : IList<ISpecFileEntry>read GetSourceFiles;
    property Files : IList<ISpecFileEntry>read GetFiles;
    property RuntimeFiles : IList<ISpecBPLEntry>read GetRuntimeFiles;
    property DesignFiles : IList<ISpecBPLEntry> read GetDesignFiles;
    property Dependencies : IList<ISpecDependency>read GetDependencies;
    property SearchPaths : IList<ISpecSearchPath>read GetSearchPaths;
    property BuildEntries : IList<ISpecBuildEntry>read GetBuildEntries;
  end;

  ISpecTemplate = interface(ISpecTemplateBase)
    ['{FB9EE9B8-E77B-4E45-A838-E1C9C9947CFB}']
    function GetName : string;
    procedure SetName(const templateName: string);

    function Clone : ISpecTemplate;

    property Name : string read GetName write SetName;
  end;


  ISpecTargetPlatform = interface(ISpecTemplateBase)
    ['{43BE69CA-0C29-4147-806B-460FFF402A68}']
    function GetPlatforms : TArray<TDPMPlatform>;
    procedure SetPlatforms(const platforms: TArray<TDPMPlatform>);
    function GetTemplateName : string;
    procedure SetTemplateName(const name: string);
    function GetCompiler : TCompilerVersion;
    procedure SetCompiler(compiler: TCompilerVersion);
    function GetVariables : TStrings;

    function CloneForPlatform(const platform : TDPMPlatform) : ISpecTargetPlatform;
    function PlatformContains(const platformName:string): Boolean;
    function ToString : string;

    property Compiler : TCompilerVersion read GetCompiler write SetCompiler;
    property Platforms : TArray<TDPMPlatform> read GetPlatforms write SetPlatforms;
    property TemplateName : string read GetTemplateName write SetTemplateName;
    property Variables : TStrings read GetVariables;
  end;


  IPackageSpec = interface(ISpecNode)
    ['{9F2BE15D-40DD-4263-925C-01E255D7BE03}']
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatforms : IList<ISpecTargetPlatform>;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetTemplates : IList<ISpecTemplate>;
    function GetIsValid : boolean;
    function GetFileName : string;
    //builds out the full spec
    function PreProcess(const version : TPackageVersion; const properties : TStringList) : boolean;
    function GenerateManifestJson(const version : TPackageVersion; const targetPlatform : ISpecTargetPlatform) : string;
    function FindTemplate(const name : string) : ISpecTemplate;
    function NewTemplate(const name: string): ISpecTemplate;
    procedure RenameTemplate(const currentTemplateName: string; const NewTemplateName:string);
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;

    property MetaData : ISpecMetaData read GetMetaData;
    property TargetPlatforms : IList<ISpecTargetPlatform>read GetTargetPlatforms;
    property TargetPlatform : ISpecTargetPlatform read GetTargetPlatform;
    property Templates : IList<ISpecTemplate>read GetTemplates;
    property IsValid : boolean read GetIsValid;
    property FileName : string read GetFileName;
  end;

  IPackageSpecReader = interface
    ['{8A20F825-8DCA-4784-BDBD-8F91A651BA72}']
    function ReadSpec(const fileName : string) : IPackageSpec;
  end;

  IPackageSpecWriter = interface
    ['{F3370E25-2E9D-4353-9985-95C75D35D68E}']
    procedure SaveToFile(const filename: string);
  end;





implementation

end.

