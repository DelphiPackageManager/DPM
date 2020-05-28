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
  Spring.Container;

//register types with the DI container.
procedure InitCore(const container : TContainer);

implementation

uses
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
  DPM.Core.Package.Installer,
  DPM.Core.Package.InstallerContext,
  DPM.Core.Types,
  DPM.Core.Sources.Interfaces,
  DPM.Core.Sources.Manager,
  DPM.Core.Sources.ClientFactory,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Configuration.Manager,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Manager,
  DPM.Core.Repository.Factory,
  DPM.Core.Repository.Directory,
  DPM.Core.Repository.Http,
  DPM.Core.Repository.DPMGithub,
  DPM.Core.Repository.DNGithub,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Cache,
  DPM.Core.Dependency.Interfaces,
  DPM.Core.Dependency.Resolver;


procedure InitCore(const container : TContainer);
begin

  Container.RegisterType<IPackageArchiveReader,TZipFileArchiveReader>('file.archive');
  Container.RegisterType<IPackageArchiveReader,TFolderArchiveReader>('folder.archive');

  Container.RegisterType<IPackageArchiveWriter, TPackageArchiveWriter>;


  Container.RegisterType<IPackageWriter,TPackageWriter>;

  Container.RegisterType<IPackageSpecReader,TPackageSpecReader>;

//  Container.RegisterType<IProjectEditor,TProjectEditor>;

  Container.RegisterType<IPackageInstallerContext,TPackageInstallerContext>;
  Container.RegisterType<IPackageInstaller,TPackageInstaller>;


  Container.RegisterType<IConfigurationManager,TConfigurationManager>;

  Container.RegisterType<ISourcesManager,TSourcesManager>;
  Container.RegisterType<ISourceClientFactory,TClientFactory>;

  Container.RegisterType<IPackageRepositoryFactory,TPackageRepositoryFactory>;
  Container.RegisterType<IPackageRepositoryManager,TPackageRepositoryManager>;

  Container.RegisterType<IPackageRepository,TDirectoryPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.Folder) );
  Container.RegisterType<IPackageRepository,TDPMServerPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.DPMServer) );
  Container.RegisterType<IPackageRepository,TDPMGithubPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.DPMGithub) );
  Container.RegisterType<IPackageRepository,TDNGithubPackageRepository>(TEnumUtils.EnumToString<TSourceType>(TSourceType.DNGithub) );

  Container.RegisterType<IPackageCache,TPackageCache>;

  Container.RegisterType<IDependencyResolver,TDependencyResolver>;

  Container.RegisterInstance<TContainer>(Container);

end;


end.
