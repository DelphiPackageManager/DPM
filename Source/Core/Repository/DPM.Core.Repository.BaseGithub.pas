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

unit DPM.Core.Repository.BaseGithub;

interface

uses
  Generics.Defaults,
  VSoft.Awaitable,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces,
  DPM.Core.Repository.Base,
  VSoft.CancellationToken,
  VSoft.HttpClient;

type
  TGithubBasePackageRepository = class(TBaseRepository)
  private
    FHttpClient : IHttpClient;

  protected
//    function DownloadPackage(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity; const localFolder: string; var fileName: string): Boolean;
//    function GetIsLocal: Boolean;
//    function GetIsRemote: Boolean;
//    function GetName: string;
//    function GetPackageInfo(const cancellationToken: ICancellationToken; const packageIdentity: IPackageIdentity): IPackageInfo;
//    function GetPackageVersions(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): Spring.Collections.IList<VSoft.SemanticVersion.TSemanticVersion>;
//    function GetPackageVersionsWithDependencies(const cancellationToken: ICancellationToken; const id: string; const compilerVersion: TCompilerVersion; const platform: TDPMPlatform; const versionRange: TVersionRange; const preRelease: Boolean): Spring.Collections.IList<DPM.Core.Package.Interfaces.IPackageInfo>;
//    function GetSource: string;
      procedure Configure(const source: ISourceConfig);override;
//    function Search(const cancellationToken: ICancellationToken; const id: string; const range: TVersionRange): Spring.Collections.IList<DPM.Core.Package.Interfaces.IPackageIdentity>;

    property HttpClient : IHttpClient read FHttpClient;
  public
    constructor Create(const logger : ILogger);override;

  end;

const
  //needed for now because repo search on topics is in preview
  cTopicSearchAccept = 'application/vnd.github.mercy-preview+json';

  cGithubv3Accept = 'application/vnd.github.v3+json';
  cGithubRawAccept = 'application/vnd.github.v3.raw';

  cGithubApiUrl = 'https://api.github.com';
  cGithubReleases = 'repos/%s/releases'; //repo
  cGithubReleaseDownloadUrl = 'repos/%s/zipball/%s'; //repo release

implementation

{ TGithubBasePackageRepository }

procedure TGithubBasePackageRepository.Configure(const source: ISourceConfig);
begin
  inherited Configure(source);
  FHttpClient := THttpClientFactory.CreateClient(Self.Source);
  FHttpClient.UserName := Self.UserName;
  FHttpClient.Password := Self.Password;

  if Self.Password <> '' then
    FHttpClient.AuthType := THttpAuthType.GitHubToken
  else
    FHttpClient.AuthType := THttpAuthType.None;
end;

constructor TGithubBasePackageRepository.Create(const logger: ILogger);
begin
  inherited Create(logger)

end;


end.
