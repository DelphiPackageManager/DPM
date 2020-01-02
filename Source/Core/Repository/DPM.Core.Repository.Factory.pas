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

unit DPM.Core.Repository.Factory;

interface

uses
  Spring.Container,
  DPM.Core.Logging,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TPackageRepositoryFactory = class(TInterfacedObject, IPackageRepositoryFactory)
  private
    FContainer : TContainer;
    FLogger   : ILogger;
  protected
      function CreateRepository(const repoType : string) : IPackageRepository;
  public
    constructor Create(const container : TContainer; const logger : ILogger);
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  VSoft.Uri;

{ TPackageRepositoryFactory }

constructor TPackageRepositoryFactory.Create(const container: TContainer; const logger : ILogger);
begin
  FContainer := container;
  FLogger := logger;
end;

function TPackageRepositoryFactory.CreateRepository(const repoType : string): IPackageRepository;
begin
  result := FContainer.Resolve<IPackageRepository>(repoType);
end;

end.
