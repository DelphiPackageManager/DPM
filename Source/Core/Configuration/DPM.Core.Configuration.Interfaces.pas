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

unit DPM.Core.Configuration.Interfaces;

interface

uses
  JsonDataObjects,
  VSoft.YAML,
  VSoft.Uri,
  Spring.Collections,
  DPM.Core.Types;

type
  IConfigNode = interface
    ['{18443C57-FA60-4DAB-BB67-10ACCBB7EC3B}']
    function LoadFromYAML(const yamlObj : IYAMLValue) : boolean;
    function SaveToYAML(const parentObj : IYAMLValue) : boolean;

  end;

  // Signing-policy fields. Sub-record of IConfiguration. The trust policy
  // service projects this onto TTrustPolicy + a fingerprint. Layout matches
  // the architecture doc §Trust Policies (cConfiguration example).
  ITrustedPublisherConfig = interface(IConfigNode)
    ['{0B7E2A48-2D9F-4A3D-9E5E-7C5D77FA1A18}']
    function GetName : string;
    function GetSpki : string;
    procedure SetName(const value : string);
    procedure SetSpki(const value : string);
    property Name : string read GetName write SetName;
    property Spki : string read GetSpki write SetSpki;
  end;

  ITrustedRepositoryConfig = interface(IConfigNode)
    ['{1F4B0D6F-D2C7-4D0A-AAE9-8B6D71D6D8A2}']
    function GetUrl : string;
    function GetSpki : string;
    procedure SetUrl(const value : string);
    procedure SetSpki(const value : string);
    property Url : string read GetUrl write SetUrl;
    property Spki : string read GetSpki write SetSpki;
  end;

  ISigningConfig = interface(IConfigNode)
    ['{7E4C9A66-9C7C-46C8-B98D-2BCBD4471B91}']
    // 'permissive' | 'require' | 'repository-required' | 'author-and-repository'
    // The mode is the single switch — `permissive` already accepts unsigned
    // packages; the three stricter modes reject them.
    function GetValidationMode : string;
    procedure SetValidationMode(const value : string);

    // 'prompt' | 'block' | 'allow'
    function GetAuthorDowngradePolicy : string;
    procedure SetAuthorDowngradePolicy(const value : string);

    function GetAllowKeyCompromiseOverride : boolean;
    procedure SetAllowKeyCompromiseOverride(const value : boolean);

    function GetTrustedPublishers : IList<ITrustedPublisherConfig>;
    function GetTrustedRepositories : IList<ITrustedRepositoryConfig>;

    property ValidationMode : string read GetValidationMode write SetValidationMode;
    property AuthorDowngradePolicy : string read GetAuthorDowngradePolicy write SetAuthorDowngradePolicy;
    property AllowKeyCompromiseOverride : boolean read GetAllowKeyCompromiseOverride write SetAllowKeyCompromiseOverride;
    property TrustedPublishers : IList<ITrustedPublisherConfig> read GetTrustedPublishers;
    property TrustedRepositories : IList<ITrustedRepositoryConfig> read GetTrustedRepositories;
  end;

  ISourceConfig = interface(IConfigNode)
    ['{88D98629-8276-4782-B46B-004E7B9934E4}']
    function GetName : string;
    function GetSource : string;
    function GetUserName : string;
    function GetPassword : string;
    function GetIsEnabled : boolean;
    function GetFileName : string;
    function GetSourceType : TSourceType;

    procedure SetName(const value : string);
    procedure SetSource(const value : string);
    procedure SetUserName(const value : string);
    procedure SetPassword(const value : string);
    procedure SetIsEnabled(const value : boolean);
    procedure SetSourceType(const value : TSourceType);


    property Name : string read GetName write SetName;
    property Source : string read GetSource write SetSource;
    property UserName : string read GetUserName write SetUserName;
    property Password : string read GetPassword write SetPassword;
    property FileName : string read GetFileName;
    property IsEnabled : boolean read GetIsEnabled write SetIsEnabled;
    property SourceType : TSourceType read GetSourceType write SetSourceType;
  end;


  IConfiguration = interface(IConfigNode)
    ['{C5B88059-C5C8-4207-BF5A-503FFE31863D}']
    function GetPackageCacheLocation : string;
    procedure SetPackageCacheLocation(const value : string);
    function GetIsDefaultPackageCacheLocation : boolean;
    procedure AddDefaultSources;

    function GetSourceByName(const name : string) : ISourceConfig;
    function GetSources : IList<ISourceConfig>;
    function GetFileName : string;
    procedure SetFileName(const value : string);

    function GetAuthor : string;
    procedure SetAuthor(const value : string);

    function GetSigning : ISigningConfig;

    property FileName : string read GetFileName write SetFileName;
    //defaults to %userprofile%\.dpm\packages - can override with env var DPM_PACKAGES
    property PackageCacheLocation : string read GetPackageCacheLocation write SetPackageCacheLocation;
    property IsDefaultPackageCacheLocation : boolean read GetIsDefaultPackageCacheLocation;

    property Sources : IList<ISourceConfig>read GetSources;

    //cached default author used by the spec scaffolder - optional
    property Author : string read GetAuthor write SetAuthor;

    // Signing policy block (Phase 1+). Always non-nil; freshly created
    // configs have default values per the architecture doc.
    property Signing : ISigningConfig read GetSigning;

  end;

  IConfigurationLoadSave = interface(IConfiguration)
    ['{17DDD5AB-5262-4E51-918E-170BC25BFE8A}']
    function LoadFromFile(const fileName : string) : boolean;
    function SaveToFile(const fileName : string) : boolean;
  end;

  //use DI to inject this where needed
  IConfigurationManager = interface
    ['{7A2A8F5E-A241-459C-BD62-AB265AA5935F}']
    function LoadConfig(const configFile : string) : IConfiguration;
    function NewConfig : IConfiguration;
    function EnsureDefaultConfig : boolean;
    function SaveConfig(const configuration : IConfiguration; const fileName : string = '') : boolean;
  end;


implementation

end.

