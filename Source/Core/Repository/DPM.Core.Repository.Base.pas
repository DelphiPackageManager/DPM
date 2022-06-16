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

unit DPM.Core.Repository.Base;

interface

uses
  Generics.Defaults,
  VSoft.Awaitable,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Sources.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TBaseRepository = class(TInterfacedObject)
  private
    FLogger : ILogger;
    FName : string;
    FSourceUri : string;
    FUserName : string;
    FPassword : string;
    FRepositoryType : TSourceType;
    FEnabled : boolean;
  protected
    //IPackageRepository;
    function GetRepositoryType : TSourceType;
    function GetName : string;
    function GetSource : string;
    procedure Configure(const source : ISourceConfig); virtual;
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);

    //exposing these for descendants
    property Logger : ILogger read FLogger;

    property Name : string read FName;
    property SourceUri : string read FSourceUri;
    property UserName : string read FUserName;
    property Password : string read FPassword;

    property RepositoryType : TSourceType read GetRepositoryType;
  public
    constructor Create(const logger : ILogger); virtual;

  end;

implementation

{ TBaseRepository }

constructor TBaseRepository.Create(const logger : ILogger);
begin
  FLogger := logger;
end;

function TBaseRepository.GetEnabled: boolean;
begin
  result := FEnabled;
end;

function TBaseRepository.GetName : string;
begin
  result := FName;
end;

function TBaseRepository.GetRepositoryType : TSourceType;
begin
  result := FRepositoryType;
end;

function TBaseRepository.GetSource : string;
begin
  result := FSourceUri;
end;

procedure TBaseRepository.SetEnabled(const value: boolean);
begin
  FEnabled := value;
end;

procedure TBaseRepository.Configure(const source : ISourceConfig);
begin
  FName := source.Name;
  FSourceUri := source.Source;
  FUserName := source.UserName;
  FPassword := source.Password;
  FRepositoryType := source.SourceType;
  FEnabled := source.IsEnabled;
end;

end.

