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

unit DPM.Core.Repository.Http;

interface

uses
  Generics.Defaults,
  VSoft.Awaitable,
  Spring.Collections,
  SVGInterfaces,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base;


type
  TDPMServerPackageRepository = class(TBaseRepository,IPackageRepository)
  private
  protected
    function DownloadPackage(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity; const localFolder: string; var fileName: string): Boolean;
    function GetPackageInfo(const cancellationToken: ICancellationToken; const packageId : IPackageId): IPackageInfo;
    function GetPackageVersions(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion): IList<TPackageVersion>;
    function GetPackageVersionsWithDependencies(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): IList<IPackageInfo>;

    function List(const cancellationToken : ICancellationToken; const options : TSearchOptions ) : IList<IPackageIdentity>;overload;
    function GetPackageFeed(const cancelToken: ICancellationToken; const options: TSearchOptions; const configuration: IConfiguration): IList<IPackageSearchResultItem>;
    function GetPackageIcon(const cancelToken : ICancellationToken; const packageId: string; const packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): ISVG;

  public
    constructor Create(const logger : ILogger);override;

  end;

implementation

{ TDPMServerPackageRepository }

constructor TDPMServerPackageRepository.Create(const logger: ILogger);
begin
  inherited Create(logger);

end;

function TDPMServerPackageRepository.DownloadPackage(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity; const localFolder: string; var fileName: string): Boolean;
begin
  result := false;
end;


function TDPMServerPackageRepository.GetPackageFeed(const cancelToken: ICancellationToken; const options: TSearchOptions; const configuration: IConfiguration): IList<IPackageSearchResultItem>;
begin
  result := TCollections.CreateList<IPackageSearchResultItem>;
end;

function TDPMServerPackageRepository.GetPackageIcon(const cancelToken : ICancellationToken; const packageId, packageVersion: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform): ISVG;
begin
  result := nil;
end;

function TDPMServerPackageRepository.GetPackageInfo(const cancellationToken: ICancellationToken; const packageId : IPackageId): IPackageInfo;
begin
  result := nil;
end;

function TDPMServerPackageRepository.GetPackageVersions(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion): IList<TPackageVersion>;
begin
  result := TCollections.CreateList<TPackageVersion>;
end;

function TDPMServerPackageRepository.GetPackageVersionsWithDependencies(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;


function TDPMServerPackageRepository.List(const cancellationToken: ICancellationToken; const options: TSearchOptions): IList<IPackageIdentity>;
begin
  result := TCollections.CreateList<IPackageIdentity>;
end;


end.
