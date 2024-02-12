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

unit DPM.Core.Init;

interface

uses
  Spring.Container,
  DPM.Core.Types;

//register types with the DI container.
procedure InitCore(const container : TContainer; const overrideProc : TConstProc<TContainer> = nil);

implementation

uses
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
  DPM.Core.Manifest.Interfaces,
  DPM.Core.Manifest,
  DPM.Core.Manifest.Reader,
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
  DPM.Core.Cache.Interfaces,
  DPM.Core.Cache,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolver,
  DPM.Core.Compiler.Interfaces,
  DPM.Core.Compiler.Factory,
  DPM.Core.Compiler.EnvironmentProvider;


procedure InitCore(const container : TContainer; const overrideProc : TConstProc<TContainer>);
begin

//  Container.RegisterType<IPackageArchiveReader, TZipFileArchiveReader>('file.archive');
//  Container.RegisterType<IPackageArchiveReader, TFolderArchiveReader>('folder.archive');

  Container.RegisterType<IPackageArchiveWriter, TPackageArchiveWriter>;


  Container.RegisterType<IPackageWriter, TPackageWriter>;

  Container.RegisterType<IPackageSpecReader, TPackageSpecReader>;

  Container.RegisterType<IPackageManifestReader, TPackageManifestReader>;

  Container.RegisterType<ICompilerEnvironmentProvider, TCompilerEnvironmentProvider>;
  Container.RegisterType<ICompilerFactory, TCompilerFactory>().AsSingleton();


  if Assigned(overrideProc) then
    //allow IDE plugin to register it's own implementations.
    overrideProc(container)
  else
  begin
    Container.RegisterType<IPackageInstallerContext, TCorePackageInstallerContext>;
  end;

  Container.RegisterType<IPackageInstaller, TPackageInstaller>;


  Container.RegisterType<IConfigurationManager, TConfigurationManager>;

  Container.RegisterType<ISourcesManager, TSourcesManager>;

  Container.RegisterType<IPackageRepositoryFactory, TPackageRepositoryFactory>;
  Container.RegisterType<IPackageRepositoryManager, TPackageRepositoryManager>().AsSingleton();

  Container.RegisterType<IPackageRepository, TDirectoryPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.Folder));
  Container.RegisterType<IPackageRepository, TDPMServerPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.DPMServer));

  Container.RegisterType<IPackageCache, TPackageCache>.AsSingleton();

  Container.RegisterType<IDependencyResolver, TDependencyResolver>;

  Container.RegisterInstance<TContainer>(Container);

end;


end.

