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
    function GetVersionString : string;
    procedure SetVersionString(const value : string);
    /// <summary> If this dependency's version was authored as the $version$ token, resolve it to a
    /// fixed range on the supplied package version. No-op for any other (already concrete) version. </summary>
    procedure ResolveVersionToken(const version : TPackageVersion);
    function Clone : ISpecDependency;
    property Id : string read GetId write SetId;
    property Version : TVersionRange read GetVersionRange write SetVersionRange;
    /// <summary> The version as authored text: the literal $version$ token when this dependency uses
    /// it (still unresolved), an empty string when no version is set, otherwise the range's string
    /// form. Setting it accepts the $version$ token or any parseable range. Use this (not Version) for
    /// UI binding so the token survives editing instead of being flattened to an empty range. </summary>
    property VersionString : string read GetVersionString write SetVersionString;
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

    function Clone : ISpecMetaData;

    property Id : string read GetId write SetId;
    property Version : TPackageVersion read GetVersion write SetVersion;
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
    function GetCopyToLib : boolean;
    function GetCopyToBin : TDPMPlatform;
    procedure SetSource(const value : string);
    procedure SetDestination(const value : string);
    procedure SetCopyToLib(const value : boolean);
    procedure SetCopyToBin(const value : TDPMPlatform);

    function Clone : ISpecSourceEntry;

    property Source : string read GetSource write SetSource;
    property Destination : string read GetDestination write SetDestination;
    property Exclude : IList<string>read GetExclude;
    //When true, the files matched by this source entry are also copied into lib\{platform}
    //at install time (flattened). Authored on the source entry; carried into the packed spec
    //as the template-level copyToLib list since source entries are stripped during pack.
    property CopyToLib : boolean read GetCopyToLib write SetCopyToLib;
    //When set to a real platform (not UnknownPlatform), the files matched by this source entry
    //are copied into bpl\{platform} at install time (flattened), but only when installing for
    //that platform - used for native DLLs a runtime/design BPL needs to load. Authored on the
    //source entry; carried into the packed spec as the template-level copyToBin list since
    //source entries are stripped during pack.
    property CopyToBin : TDPMPlatform read GetCopyToBin write SetCopyToBin;
  end;

  //A copyLocal entry: a glob (src) matched against the package cache at 'dpm copylocal' time
  //and copied, flattened, into the consuming project's build output folder. Unlike copyToLib/
  //copyToBin (authored as source-entry flags, so the file must exist at pack time), copyLocal
  //entries are authored directly and can target artifacts built during install (e.g. a runtime
  //.bpl in bpl\{platform}). The src may contain the $platform$ token, substituted with the build
  //platform's folder name before expansion.
  ISpecCopyLocalEntry = interface(ISpecNode)
    ['{4D8A6F23-7B1E-4C90-9A2D-5E6F0B3C8A14}']
    function GetSource : string;
    function GetPlatforms : TDPMPlatforms;
    function GetMode : TCopyLocalMode;
    procedure SetSource(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetMode(const value : TCopyLocalMode);

    function Clone : ISpecCopyLocalEntry;

    /// <summary> The glob (relative to the extracted package root) of files to copy. May contain $platform$. </summary>
    property Source : string read GetSource write SetSource;
    /// <summary> Platforms this entry applies to. Empty = all platforms. </summary>
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    /// <summary> always (default) or runtimeOnly (only when the project links this package's runtime bpl). </summary>
    property Mode : TCopyLocalMode read GetMode write SetMode;
  end;

  ISpecBuildEntry = interface(ISpecNode)
    ['{9E1850EB-40C4-421F-A47F-03FDD6286573}']
    function GetProject : string;
    function GetPlatforms : TDPMPlatforms;
    function GetDefines : string;
    function GetReferences : IList<string>;

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
    /// <summary> Package names this build package requires beyond rtl. Emitted by `dpm prepare` as `requires` entries in the dpk and `<DCCReference>` elements in the dproj. Not consumed at pack/install time. </summary>
    property References : IList<string> read GetReferences;

  end;

  /// <summary>
  /// Design time package projects.
  /// </summary>
  ISpecDesignEntry = interface(ISpecNode)
  ['{7879DE88-0612-45F1-AAE1-9D9CE50748EC}']
    function GetProject : string;
    function GetDefines : string;
    function GetPlatforms : TDPMPlatforms;
    function GetReferences : IList<string>;

    function GetLibSuffix : string;
    function GetLibPrefix : string;
    function GetLibVersion : string;

    procedure SetProject(const value : string);
    procedure SetDefines(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetLibSuffix(const value : string);
    procedure SetLibPrefix(const value : string);
    procedure SetLibVersion(const value : string);

    function Clone : ISpecDesignEntry;

    /// <summary> The dproj for the package file to build </summary>
    property Project : string read GetProject write SetProject;
    /// <summary> Semicolon seprated list of compiler defines</summary>
    property Defines : string read GetDefines write SetDefines;
    /// <summary> Design-host platforms this entry supports. Empty = defer to the design dproj's enabled platforms. </summary>
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    /// <summary> Package names this design package requires beyond rtl/designide. Emitted by `dpm prepare` as `requires` entries in the dpk and `<DCCReference>` elements in the dproj. Not consumed at pack/install time. </summary>
    property References : IList<string> read GetReferences;

    property LibSuffix : string read GetLibSuffix write SetLibSuffix;
    property LibPrefix : string read GetLibPrefix write SetLibPrefix;
    property LibVersion : string read GetLibVersion write SetLibVersion;
  end;


  /// <summary>
  /// Declares a package project (dpk/dproj) that DPM should generate for a source-only
  /// library that doesn't ship its own package projects. The project is generated into the
  /// package cache version folder at install time, then compiled via the matching build/design
  /// entry. The Project path must match the corresponding build/design entry's project path.
  /// </summary>
  ISpecPackageDefinition = interface(ISpecNode)
    ['{E6A1B3C4-5D7E-4F12-9A3B-8C2D4E6F1A09}']
    function GetProject : string;
    function GetFiles : IList<string>;
    function GetExclude : IList<string>;
    function GetRequires : IList<string>;
    function GetPlatforms : TDPMPlatforms;
    function GetKind : string;

    procedure SetProject(const value : string);
    procedure SetPlatforms(const value : TDPMPlatforms);
    procedure SetKind(const value : string);

    function Clone : ISpecPackageDefinition;

    /// <summary> The dproj for the package project to generate (path + name, relative to the package root). </summary>
    property Project : string read GetProject write SetProject;
    /// <summary> Source file glob patterns to include, same syntax as a source entry's src. </summary>
    property Files : IList<string> read GetFiles;
    /// <summary> Glob patterns to exclude from the matched files, same semantics as a source entry's exclude. </summary>
    property Exclude : IList<string> read GetExclude;
    /// <summary> Package names to add to the dpk requires clause (beyond the implicit rtl / designide). </summary>
    property Requires : IList<string> read GetRequires;
    /// <summary> Optional platform override. Empty = use the targetPlatform's platforms. </summary>
    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
    /// <summary> Optional explicit kind: 'runtime' or 'design'. Empty = infer (design when requires contains designide, else runtime). </summary>
    property Kind : string read GetKind write SetKind;
  end;


  ISpecTemplate = interface(ISpecNode)
    ['{B4DE32F7-AE58-4519-B69D-2389F12EC63F}']
    function GetSourceFiles : IList<ISpecSourceEntry>;
    function GetDesignFiles : IList<ISpecDesignEntry>;
    function GetDependencies : IList<ISpecDependency>;
    function GetBuildEntries : IList<ISpecBuildEntry>;
    function GetCopyToLibEntries : IList<ISpecSourceEntry>;
    function GetCopyToBinEntries : IList<ISpecSourceEntry>;
    function GetCopyLocalEntries : IList<ISpecCopyLocalEntry>;
    function GetPackageDefinitions : IList<ISpecPackageDefinition>;
    function GetPrecompiledBinaries : IList<string>;
    function GetEnvironmentVariables : IVariables;
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
    function NewPackageDefinition(const project : string) : ISpecPackageDefinition;
    function NewCopyLocalEntry(const src : string) : ISpecCopyLocalEntry;

    procedure DeleteDependency(const id : string);
    procedure DeleteSource(const src: string);
    procedure DeleteBuildEntry(const project : string);
    procedure DeleteDesignEntry(const project : string);
    procedure DeletePackageDefinition(const project : string);
    procedure DeleteCopyLocalEntry(const src : string);

    function FindDependency(const id : string) : ISpecDependency;
    function FindSourceEntry(const src : string) : ISpecSourceEntry;
    function FindBuildEntry(const project : string) : ISpecBuildEntry;
    function FindDesignEntry(const project : string) : ISpecDesignEntry;
    function FindPackageDefinition(const project : string) : ISpecPackageDefinition;


    function Clone : ISpecTemplate;

    property Name : string read GetName write SetName;
    property Dependencies : IList<ISpecDependency>read GetDependencies;
    property SourceEntries : IList<ISpecSourceEntry>read GetSourceFiles;
    property BuildEntries : IList<ISpecBuildEntry>read GetBuildEntries;
    property DesignEntries: IList<ISpecDesignEntry> read GetDesignFiles;
    /// <summary> Source globs (archive-relative) whose matched files should also be copied, flattened,
    /// into lib\{platform} at install time. Populated at pack time from source entries flagged
    /// copyToLib, since the source entries themselves are stripped from the packed spec. </summary>
    property CopyToLibEntries : IList<ISpecSourceEntry> read GetCopyToLibEntries;
    /// <summary> Source globs (archive-relative) whose matched files should be copied, flattened,
    /// into bpl\{platform} at install time - but only when installing for the entry's platform.
    /// Populated at pack time from source entries flagged copyToBin, since the source entries
    /// themselves are stripped from the packed spec. </summary>
    property CopyToBinEntries : IList<ISpecSourceEntry> read GetCopyToBinEntries;
    /// <summary> First-class copyLocal entries: globs (relative to the extracted package root, may
    /// contain $platform$) whose matched files 'dpm copylocal' copies, flattened, into the consuming
    /// project's build output folder at build time. Each entry carries a platform list (empty = all)
    /// and a mode (always/runtimeOnly). Authored directly, so they can target artifacts built during
    /// install (e.g. a runtime .bpl in bpl\{platform}). </summary>
    property CopyLocalEntries : IList<ISpecCopyLocalEntry> read GetCopyLocalEntries;
    property PackageDefinitions : IList<ISpecPackageDefinition> read GetPackageDefinitions;
    /// <summary> Precompiled Windows PE binaries (.bpl/.dll/.exe) this package ships, declared as
    /// archive-relative paths (forward slashes) exactly as they appear inside the .dpkg. Auto-derived
    /// at pack time from the files actually added to the archive. The gallery cross-checks this against
    /// archive contents: an author-signed package whose archive contains an undeclared PE is rejected. </summary>
    property PrecompiledBinaries : IList<string> read GetPrecompiledBinaries;
    /// <summary> IDE environment variables to set (in the IDE process) while this package's design
    /// components are loaded, and clear/restore when unloaded. Key = variable name (PATH is
    /// special-cased as append-only); value may use the install-time $packageDir$ token. </summary>
    property EnvironmentVariables : IVariables read GetEnvironmentVariables;

    property SourceComments : TStrings read GetSourceComments;
    property DependenciesComments : TStrings read GetDependenciesComments;
    property BuildComments : TStrings read GetBuildComments;
    property DesignComments : TStrings read GetDesignComments;
    property PackageDefComments : TStrings read GetPackageDefComments;
  end;


  ISpecTargetPlatform = interface(ISpecNode)
    ['{43BE69CA-0C29-4147-806B-460FFF402A68}']
    function GetPlatforms : TDPMPlatforms;
    procedure SetPlatforms(const platforms: TDPMPlatforms);
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

    property Platforms : TDPMPlatforms read GetPlatforms write SetPlatforms;
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


    function GenerateDspecYAML(const version : TPackageVersion) : string;

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
    function ReadSpecString(const value : string; const fileName : string = '') : IPackageSpec;
  end;



implementation

end.

