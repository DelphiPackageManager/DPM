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

unit DPM.Core.Repository.Base;

interface

uses
  Generics.Defaults,
  System.SyncObjs,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Types,
  DPM.Core.Diagnostics,
  DPM.Core.Sources.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Logging,
  DPM.Core.Options.Search,
  DPM.Core.Package.Interfaces,
  DPM.Core.Configuration.Interfaces,
  DPM.Core.Repository.Interfaces;

type
  TRepositoryError = record
    Reason : string;
    Code : TErrorCode;
  end;

  TBaseRepository = class(TInterfacedObject)
  private
    FLogger : ILogger;
    FName : string;
    FSourceUri : string;
    FUserName : string;
    FPassword : string;
    FRepositoryType : TSourceType;
    FEnabled : boolean;
    FLock : TCriticalSection;
    //A request's error belongs to the thread that made it (keyed by thread id) : the IDE asks one repository
    //from many threads at once, and one query's ClearLastError must not wipe another's failure.
    FLastErrors : IDictionary<Cardinal, TRepositoryError>;
  protected
    //IPackageRepository;
    function GetRepositoryType : TSourceType;
    function GetName : string;
    function GetSource : string;
    procedure Configure(const source : ISourceConfig); virtual;
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetLastError : string;
    function GetLastErrorCode : TErrorCode;
    procedure ClearLastError;
    //a request failed - remembered for the repository manager to report (see IPackageRepository.LastError).
    //reason reads on from 'Source [name] ', eg 'could not be reached : ...'.
    procedure RequestFailed(const reason : string; const code : TErrorCode = TErrorCode.E1201_PackageDownloadFailed);
    //for state a descendant sets up lazily (a service index, a catalog) - several threads may get there first.
    procedure EnterLock;
    procedure LeaveLock;

    //exposing these for descendants
    property Logger : ILogger read FLogger;

    property Name : string read FName;
    property SourceUri : string read FSourceUri;
    property UserName : string read FUserName;
    property Password : string read FPassword;

    property RepositoryType : TSourceType read GetRepositoryType;
  public
    constructor Create(const logger : ILogger); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  WinApi.Windows;

{ TBaseRepository }

constructor TBaseRepository.Create(const logger : ILogger);
begin
  FLogger := logger;
  FLock := TCriticalSection.Create;
  FLastErrors := TCollections.CreateDictionary<Cardinal, TRepositoryError>;
end;

destructor TBaseRepository.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TBaseRepository.EnterLock;
begin
  FLock.Enter;
end;

procedure TBaseRepository.LeaveLock;
begin
  FLock.Leave;
end;

procedure TBaseRepository.ClearLastError;
begin
  FLock.Enter;
  try
    FLastErrors.Remove(GetCurrentThreadId);
  finally
    FLock.Leave;
  end;
end;

function TBaseRepository.GetLastError : string;
var
  error : TRepositoryError;
begin
  result := '';
  FLock.Enter;
  try
    if FLastErrors.TryGetValue(GetCurrentThreadId, error) then
      result := error.Reason;
  finally
    FLock.Leave;
  end;
end;

function TBaseRepository.GetLastErrorCode : TErrorCode;
var
  error : TRepositoryError;
begin
  result := TErrorCode.E1201_PackageDownloadFailed;
  FLock.Enter;
  try
    if FLastErrors.TryGetValue(GetCurrentThreadId, error) then
      result := error.Code;
  finally
    FLock.Leave;
  end;
end;

procedure TBaseRepository.RequestFailed(const reason : string; const code : TErrorCode);
var
  error : TRepositoryError;
begin
  error.Reason := reason;
  error.Code := code;
  FLock.Enter;
  try
    FLastErrors[GetCurrentThreadId] := error;
  finally
    FLock.Leave;
  end;
  FLogger.Verbose('Source [' + FName + '] ' + reason);
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

