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

unit DPM.Core.Init;

interface

uses
  Spring.Container,
  DPM.Core.Types;

//register types with the DI container.
procedure InitCore(const container : TContainer; const overrideProc : TConstProc<TContainer> = nil);

implementation

uses
  System.Rtti,
  System.TypInfo,
  Spring.Container.Registration,
  Spring.Container.Common,
  DPM.Core.Utils.Enum,
  DPM.Core.Packaging,
  DPM.Core.Packaging.Writer,
  DPM.Core.Packaging.Archive,
  DPM.Core.Packaging.Archive.Reader,
  DPM.Core.Packaging.Archive.Writer,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec,
  DPM.Core.Spec.Reader,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Installer.Interfaces,
  DPM.Core.Package.Installer,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Sources.Manager,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Manager,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Manager,
  DPM.Core.Repository.Factory,
  DPM.Core.Repository.Directory,
  DPM.Core.Repository.Http,
  DPM.Core.Repository.GitRegistry,
  DPM.Core.Git.Interfaces,
  DPM.Core.Git.Client,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Cache,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.Factory,
  DPM.Core.Compiler.EnvironmentProvider,
  DPM.Core.Project.MapFile,
  DPM.Core.Project.Transformer,
  DPM.Core.Project.Prepare,
  DPM.Core.Project.PackageGenerator,
  DPM.Core.SBOM.Interfaces,
  DPM.Core.SBOM.Writers,
  DPM.Core.SBOM.Writers.Reports,
  DPM.Core.SBOM.Reader,
  DPM.Core.SBOM.Generator,

  // Signing — Phase 1
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Crypto.Hashing,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Crypto.X509,
  DPM.Core.Crypto.Cms.Interfaces,
  DPM.Core.Crypto.Cms,
  DPM.Core.Crypto.Timestamping,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Trust.TrustSet,
  DPM.Core.Trust.Policy,
  DPM.Core.Trust.State,
  DPM.Core.Trust.Prompt,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Manifest,
  DPM.Core.Package.Archive,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Package.Signing.Interfaces,
  DPM.Core.Package.Signing;


procedure InitCore(const container : TContainer; const overrideProc : TConstProc<TContainer>);
begin

//  Container.RegisterType<IPackageArchiveReader, TZipFileArchiveReader>('file.archive');
//  Container.RegisterType<IPackageArchiveReader, TFolderArchiveReader>('folder.archive');

  Container.RegisterType<IPackageArchiveWriter, TPackageArchiveWriter>;


  Container.RegisterType<IPackageWriter, TPackageWriter>;

  Container.RegisterType<IPackageSpecReader, TPackageSpecReader>;

  //Prepare command services — used by `dpm prepare` and reusable from the IDE.
  Container.RegisterType<IProjectTransformer, TProjectTransformer>;
  Container.RegisterType<IDpkTransformer, TDpkTransformer>;
  Container.RegisterType<IPreparePackageFolders, TPreparePackageFolders>;

  //Install-time package-project generation for source-only libraries (`package definitions`).
  Container.RegisterType<IPackageProjectGenerator, TPackageProjectGenerator>;

  Container.RegisterType<ICompilerEnvironmentProvider, TCompilerEnvironmentProvider>;
  Container.RegisterType<ICompilerFactory, TCompilerFactory>().AsSingleton();

  // Register the core default. The IDE plugin replaces this via overrideProc
  // (called at the end) with TDPMIDEPackageInstallerContext. Spring4D's default
  // registration is last-write-wins, so the override takes precedence.
  Container.RegisterType<IPackageInstallerContext, TCorePackageInstallerContext>;

  Container.RegisterType<IPackageInstaller, TPackageInstaller>;


  Container.RegisterType<IConfigurationManager, TConfigurationManager>;

  Container.RegisterType<ISourcesManager, TSourcesManager>;

  Container.RegisterType<IPackageRepositoryFactory, TPackageRepositoryFactory>;
  Container.RegisterType<IPackageRepositoryManager, TPackageRepositoryManager>().AsSingleton();

  Container.RegisterType<IGitClient, TGitClient>;

  Container.RegisterType<IPackageRepository, TDirectoryPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.Folder));
  Container.RegisterType<IPackageRepository, TDPMServerPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.DPMServer));
  Container.RegisterType<IPackageRepository, TGitRegistryPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.GitRegistry));

  Container.RegisterType<IPackageCache, TPackageCache>.AsSingleton();

  Container.RegisterType<IDependencyResolver, TDependencyResolver>;

  Container.RegisterType<IMapFileReader, TMapFileReader>;
  Container.RegisterType<ISbomWriter, TCycloneDXWriter>(cSBOMWriterCycloneDX);
  Container.RegisterType<ISbomWriter, TSPDXWriter>(cSBOMWriterSPDX);
  Container.RegisterType<ISbomWriter, THTMLReportWriter>(cSBOMWriterHTML);
  Container.RegisterType<ISbomWriter, TMarkdownReportWriter>(cSBOMWriterMarkdown);
  Container.RegisterType<ISBOMReader, TCycloneDXReader>;
  Container.RegisterType<ISbomGenerator, TSBOMGenerator>;

  // Signing — Phase 1 registrations
  Container.RegisterType<IHashingService, TBCryptHashingService>.AsSingleton;
  Container.RegisterType<IX509Service, TX509Service>;
  Container.RegisterType<ICmsService, TCmsService>;
  Container.RegisterType<ITimestamper, TWindowsTimestamper>;
  Container.RegisterType<ITrustSet, TBuiltInTrustSet>.AsSingleton;
  Container.RegisterType<ITrustPolicyService, TTrustPolicyService>;
  Container.RegisterType<IManifestService, TManifestService>;
  Container.RegisterType<IArchiveValidator, TArchiveValidator>;
  Container.RegisterType<IReceiptService, TYamlReceiptService>;
  Container.RegisterType<IPackageSigningService, TPackageSigningService>;
  // TYamlTrustStateService has a parameterless ctor that uses
  // %APPDATA%\.dpm\trust-state.yaml.
  Container.RegisterType<ITrustStateService, TYamlTrustStateService>.AsSingleton;
  // Non-interactive default — fails closed when no UI is wired. The CLI
  // and IDE register their own ITrustPromptStrategy on top of this default.
  Container.RegisterType<ITrustPromptStrategy, TNonInteractiveTrustPromptStrategy>;

  Container.RegisterInstance<TContainer>(Container);

  // Run overrideProc LAST so callers (e.g. the IDE plugin) can replace any of
  // the core defaults above — Spring4D's default-registration semantics give
  // last-write-wins for unnamed registrations of the same interface.
  if Assigned(overrideProc) then
    overrideProc(container);

end;


end.

