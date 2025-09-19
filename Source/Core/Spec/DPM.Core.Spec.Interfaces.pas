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

unit DPM.Core.Spec.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.TargetPlatform,
  DPM.Core.Dependency.Version,
  DPM.Core.Options.Spec;


type
  IVariables = IOrderedDictionary<string,string>;

  ISpecNode = interface
    ['{AD47A3ED-591B-4E47-94F2-7EC136182202}']
    function LoadFromYAML(const yamlObject : IYAMLMapping) : boolean;
    procedure ToYAML(const parent : IYAMLValue; const packageKind : TDPMPackageKind);
    function GetComments : TStrings;
    function HasComments : boolean;
    procedure SetComments(const value : TStrings);

    property Comments : TStrings read GetComments write SetComments;
  end;


  ISpecDependency = interface(ISpecNode)
    ['{6CE14888-54A8-459C-865E-E4B4628DB8C6}']
    function GetId : string;
    procedure SetId(const id: string);
    function GetVersionRange : TVersionRange;
    procedure SetVersionRange(const value : TVersionRange);
    function Clone : ISpecDependency;
    property Id : string read GetId write SetId;
    property Version : TVersionRange read GetVersionRange write SetVersionRange;
  end;



    //used for git-registry style manifests
  ISpecVersion = interface
  ['{56ED6A29-D5CE-4E93-AAE5-84C9C0E2ADCB}']
    function GetVersion : TPackageVersion;
    procedure SetVersion(version : TPackageVersion);
    function GetCommit  : string;
    procedure SetCommit(const value : string);
    property Version : TPackageVersion read GetVersion write SetVersion;
    property Commit : string read GetCommit write SetCommit;
  end;


  ISpecMetaData = interface(ISpecNode)
    ['{9972F2EA-4180-4D12-9193-13A55B927B81}']
    function GetId : string;
    function GetVersion : TPackageVersion;
    function GetDescription : string;
    function GetAuthors : IList<string>;
    function GetProjectUrl : string;
    function GetRepositoryUrl : string;
    function GetRepositoryType : string;
    function GetRepositoryBranch : string;
    function GetRepositoryCommit : string;
    function GetReleaseNotes : string;
    function GetLicense : string;
    function GetIcon : string;
    function GetCopyright : string;
    function GetTags : TStrings;
    function GetIsTrial : boolean;
    function GetIsCommercial : boolean;
    function GetReadMe : string;
    function GetFrameworks : TArray<TDPMUIFrameworkType>;
    function GetPackageKind : TDPMPackageKind;

    procedure SetPackageKind(const value : TDPMPackageKind);
    procedure SetVersion(const value : TPackageVersion);
    procedure SetId(const value : string);
    procedure SetDescription(const value : string);
    procedure SetProjectUrl(const value : string);
    procedure SetRepositoryUrl(const value : string);
    procedure SetRepositoryType(const value : string);
    procedure SetRepositoryBranch(const value : string);
    procedure SetRepositoryCommit(const value : string);
    procedure SetReleaseNotes(const value : string);
    procedure SetLicense(const value : string);
    procedure SetIcon(const value : string);
    procedure SetCopyright(const value : string);
    procedure SetTags(const value : TStrings);
    procedure SetIsTrial(const value : boolean);
    procedure SetIsCommercial(const value : boolean);
    procedure SetReadMe(const value : string);
    procedure SetFrameworks(const value : TArray<TDPMUIFrameworkType>);

    function GetVersions : IList<ISpecVersion>;
    function HasVersions : boolean;
    function Clone : ISpecMetaData;

    property Id : string read GetId write SetId;
    property Version : TPackageVersion read GetVersion write SetVersion;
    property Versions : IList<ISpecVersion> read GetVersions;
    property Description : string read GetDescription write SetDescription;
    property Authors : IList<string> read GetAuthors;
    property ProjectUrl       : string read GetProjectUrl write SetProjectUrl;
    property RepositoryUrl    : string read GetRepositoryUrl write SetRepositoryUrl;
    property RepositoryType   : string read GetRepositoryType write SetRepositoryType;
    property RepositoryBranch : string read GetRepositoryBranch write SetRepositoryBranch;
    property RepositoryCommit : string read GetRepositoryCommit write SetRepositoryCommit;
    property ReleaseNotes     : string read GetReleaseNotes write SetReleaseNotes;
    property License : string read GetLicense write SetLicense;
    property Icon : string read GetIcon write SetIcon;
    property Copyright : string read GetCopyright write SetCopyright;
    property Tags : TStrings read GetTags write SetTags;
    property IsTrial : boolean read GetIsTrial write SetIsTrial;
    property IsCommercial : boolean read GetIsCommercial write SetIsCommercial;
    property ReadMe : string read GetReadMe write SetReadMe;

    property Frameworks : TArray<TDPMUIFrameworkType> read GetFrameworks write SetFrameworks;
    property PackageKind : TDPMPackageKind read GetPackageKind write SetPackageKind;
  end;

  ISpecSourceEntry = interface(ISpecNode)
    ['{2BE821AA-94C7-439C-B236-85D8901FFA81}']
    function GetSource : string;
    function GetDestination : string;
    function GetExclude : IList<string>;
    procedure SetSource(const value : string);
    procedure SetDestination(const value : string);

    function Clone : ISpecSourceEntry;

    property Source : string read GetSource write SetSource;
    property Destination : string read GetDestination write SetDestination;
    property Exclude : IList<string>read GetExclude;
  end;

  ISpecBuildEntry = interface(ISpecNode)
    ['{9E1850EB-40C4-421F-A47F-03FDD6286573}']
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;

    procedure SetProject(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetDefines(const value : string);

    function Clone : ISpecBuildEntry;

    /// <summary> The dproj for the package file to build </summary>
    property Project : string read GetProject write SetProject;
    /// <summary> Allows you to override the platforms configured on the targetPlatform </summary>
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    /// <summary> Semicolon seprated list of compiler defines</summary>
    property Defines : string read GetDefines write SetDefines;
  end;

  /// <summary>
  /// Design time package projects.
  /// </summary>
  ISpecDesignEntry = interface(ISpecNode)
  ['{7879DE88-0612-45F1-AAE1-9D9CE50748EC}']
    function GetProject : string;
    function GetDefines : string;

    function GetLibSuffix : string;
    function GetLibPrefix : string;
    function GetLibVersion : string;

    procedure SetProject(const value : string);
    procedure SetDefines(const value : string);
    procedure SetLibSuffix(const value : string);
    procedure SetLibPrefix(const value : string);
    procedure SetLibVersion(const value : string);

    function Clone : ISpecDesignEntry;

    /// <summary> The dproj for the package file to build </summary>
    property Project : string read GetProject write SetProject;
    /// <summary> Semicolon seprated list of compiler defines</summary>
    property Defines : string read GetDefines write SetDefines;

    property LibSuffix : string read GetLibSuffix write SetLibSuffix;
    property LibPrefix : string read GetLibPrefix write SetLibPrefix;
    property LibVersion : string read GetLibVersion write SetLibVersion;
  end;


  ISpecTemplate = interface(ISpecNode)
    ['{B4DE32F7-AE58-4519-B69D-2389F12EC63F}']
    function GetSourceFiles : IList<ISpecSourceEntry>;
    function GetDesignFiles : IList<ISpecDesignEntry>;
    function GetDependencies : IList<ISpecDependency>;
    function GetBuildEntries : IList<ISpecBuildEntry>;
    function GetName : string;
    procedure SetName(const templateName: string);

    // RoundTripping comments
    function GetSourceComments : TStrings;
    function GetDependenciesComments : TStrings;
    function GetBuildComments : TStrings;
    function GetDesignComments : TStrings;
    function GetPackageDefComments : TStrings;


    function NewDependency(const id : string) : ISpecDependency;
    function NewSource(const src: string): ISpecSourceEntry;
    function NewBuildEntry(const project : string) : ISpecBuildEntry;
    function NewDesignEntry(const project : string) : ISpecDesignEntry;

    procedure DeleteDependency(const id : string);
    procedure DeleteSource(const src: string);
    procedure DeleteBuildEntry(const project : string);
    procedure DeleteDesignEntry(const project : string);

    function FindDependency(const id : string) : ISpecDependency;
    function FindSourceEntry(const src : string) : ISpecSourceEntry;
    function FindBuildEntry(const project : string) : ISpecBuildEntry;
    function FindDesignEntry(const project : string) : ISpecDesignEntry;


    function Clone : ISpecTemplate;

    property Name : string read GetName write SetName;
    property Dependencies : IList<ISpecDependency>read GetDependencies;
    property SourceEntries : IList<ISpecSourceEntry>read GetSourceFiles;
    property BuildEntries : IList<ISpecBuildEntry>read GetBuildEntries;
    property DesignEntries: IList<ISpecDesignEntry> read GetDesignFiles;

    property SourceComments : TStrings read GetSourceComments;
    property DependenciesComments : TStrings read GetDependenciesComments;
    property BuildComments : TStrings read GetBuildComments;
    property DesignComments : TStrings read GetDesignComments;
    property PackageDefComments : TStrings read GetPackageDefComments;
  end;


  ISpecTargetPlatform = interface(ISpecNode)
    ['{43BE69CA-0C29-4147-806B-460FFF402A68}']
    function GetPlatforms : TArray<TDPMPlatform>;
    procedure SetPlatforms(const platforms: TArray<TDPMPlatform>);
    function GetTemplateName : string;
    procedure SetTemplateName(const name: string);


    function GetCompiler : TCompilerVersion;
    procedure SetCompiler(compiler: TCompilerVersion);
    function GetCompilers : TArray<TCompilerVersion>;
    procedure SetCompilers(compiler: TArray<TCompilerVersion>);

    function GetMinCompiler : TCompilerVersion;
    procedure SetMinCompiler(value: TCompilerVersion);
    function GetMaxCompiler : TCompilerVersion;
    procedure SetMaxCompiler(value : TCompilerVersion);

    function GetVariables : IVariables;

    function Clone : ISpecTargetPlatform;

    function IsForCompiler(compilerVersion : TCompilerVersion) : boolean;

    function ToString : string;

    property Compiler : TCompilerVersion read GetCompiler write SetCompiler;
    property Compilers : TArray<TCompilerVersion> read GetCompilers write SetCompilers;
    property MinCompiler : TCompilerVersion read GetMinCompiler write SetMinCompiler;
    property MaxCompiler : TCompilerVersion read GetMaxCompiler write SetMaxCompiler;

    property Platforms : TArray<TDPMPlatform> read GetPlatforms write SetPlatforms;
    property TemplateName : string read GetTemplateName write SetTemplateName;
    property Variables : IVariables read GetVariables;



  end;


  IPackageSpec = interface(ISpecNode)
    ['{9F2BE15D-40DD-4263-925C-01E255D7BE03}']
    function GetMetaData : ISpecMetaData;
    function GetTargetPlatforms : IList<ISpecTargetPlatform>;
    function GetTargetPlatform : ISpecTargetPlatform;
    function GetTemplates : IList<ISpecTemplate>;
    function GetIsValid : boolean;
    function GetFileName : string;
    function GetPackageKind : TDPMPackageKind;
    procedure SetPackageKind(const value : TDPMPackageKind);
    function GetVariables : IVariables;

    // targetPlatforms, templates and variabls collections are not backed with a node
    // so we are collecting the comments here
    function GetTargetPlatformsComments : TStrings;
    function GetTemplatesComments : TStrings;
    function GetVariablesComments : TStrings;


    function GenerateManifestYAML(const version : TPackageVersion) : string;

    /// <summary>
    ///  Generates a preprocessed spec with most variables (apart from platform) expanded.
    ///  Called by the package Writer
    /// </summary>
//    function GenerateManifest(const version : TPackageVersion; const targetPlatform : ISpecTargetPlatform) : IPackageSpec;


    function FindTemplate(const name : string) : ISpecTemplate;
    function NewTemplate(const name: string): ISpecTemplate;
    procedure RenameTemplate(const currentTemplateName: string; const NewTemplateName:string);
    procedure DeleteTemplate(const templateName: string);
    function DuplicateTemplate(const sourceTemplate: ISpecTemplate; const newTemplateName: string): ISpecTemplate;

    function Clone : IPackageSpec;
    procedure ToYAMLFile(const fileName : string);

    property PackageKind : TDPMPackageKind read GetPackageKind write SetPackageKind;
    property MetaData : ISpecMetaData read GetMetaData;
    property TargetPlatforms : IList<ISpecTargetPlatform>read GetTargetPlatforms;
    property TargetPlatform : ISpecTargetPlatform read GetTargetPlatform;
    property Templates : IList<ISpecTemplate>read GetTemplates;
    property IsValid : boolean read GetIsValid;
    property FileName : string read GetFileName;
    property Variables : IVariables read GetVariables;

    //these are for attempting to round tripping comments
    property TargetPlatformsComments : TStrings read GetTargetPlatformsComments;
    property VariablesComments : TStrings read GetVariablesComments;
    property TemplatesVariables : TStrings read GetTemplatesComments;
  end;

  IPackageSpecReader = interface
    ['{8A20F825-8DCA-4784-BDBD-8F91A651BA72}']
    function ReadSpec(const fileName : string) : IPackageSpec;
  end;



implementation

end.

