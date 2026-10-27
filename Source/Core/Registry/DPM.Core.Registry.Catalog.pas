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

unit DPM.Core.Registry.Catalog;

interface

uses
  System.SyncObjs,
  System.Diagnostics,
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Git.Interfaces,
  DPM.Core.Registry.Interfaces;

type
  //Thread safe - the IDE queries a source from several threads, and the first to need it clones it while the
  //others wait.
  TRegistryCatalog = class(TInterfacedObject, IRegistryCatalog)
  private
    class var
      FCloneRetrySeconds : integer;
    class constructor Create;
  private
    FLock : TCriticalSection;
    FLogger : ILogger;
    FGitClient : IGitClient;
    FSpecReader : IPackageSpecReader;
    FName : string;
    FSource : string;            //a local folder path or a git url
    FRegistriesFolder : string;  //base mirror folder for git-url registries
    FTTLMinutes : integer;
    FLoaded : boolean;
    FWorkingDir : string;
    FSpecs : IDictionary<string, IPackageSpec>; //key = lowercase id
    //since the clone of a git url registry failed (not running when it hasn't), and what git said.
    FCloneFailed : TStopwatch;
    FCloneError : string;
    function IsFolderSource : boolean;
    function MirrorDir : string;
    function MirrorIsCloneOfSource(const cancellationToken : ICancellationToken; const mirror : string) : boolean;
    function CloneMirror(const cancellationToken : ICancellationToken; const mirror : string; const forceRefresh : boolean) : boolean;
    function FetchStampFile(const mirrorDir : string) : string;
    function MirrorNeedsRefresh(const mirrorDir : string; const forceRefresh : boolean) : boolean;
    procedure TouchFetchStamp(const mirrorDir : string);
    function ResolveWorkingDir(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;
    procedure LoadCatalog;
  protected
    function EnsureUpdated(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;
    function GetPackageSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;
    function GetPackageIds(const cancellationToken : ICancellationToken) : IList<string>;
  public
    constructor Create(const logger : ILogger; const gitClient : IGitClient; const name : string; const source : string;
                       const registriesFolder : string; const ttlMinutes : integer = 60);
    destructor Destroy; override;
    //exposed for diagnostics/testing
    property WorkingDir : string read FWorkingDir;
    //how long a query waits after a failed clone before it tries again (an explicit refresh always tries).
    class property CloneRetrySeconds : integer read FCloneRetrySeconds write FCloneRetrySeconds;
  end;

  //true if source looks like a git url rather than a local folder path.
  function IsLikelyGitUrl(const source : string) : boolean;

implementation

uses
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Diagnostics,
  DPM.Core.Constants,
  DPM.Core.Spec.Reader,
  DPM.Core.Utils.Path,
  DPM.Core.Utils.Hash,
  DPM.Core.Utils.System;

const
  cFetchStampFileName = '.dpm-fetched';

function IsLikelyGitUrl(const source : string) : boolean;
begin
  result := StartsText('http://', source) or
            StartsText('https://', source) or
            StartsText('git://', source) or
            StartsText('ssh://', source) or
            StartsText('git@', source) or
            EndsText('.git', source);
end;

function SanitizeForFolderName(const value : string) : string;
var
  i : integer;
  ch : Char;
begin
  result := '';
  for i := 1 to Length(value) do
  begin
    ch := value[i];
    if CharInSet(ch, ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) then
      result := result + ch
    else
      result := result + '_';
  end;
  if result = '' then
    result := 'registry';
end;

{ TRegistryCatalog }

class constructor TRegistryCatalog.Create;
begin
  FCloneRetrySeconds := 60;
end;

constructor TRegistryCatalog.Create(const logger : ILogger; const gitClient : IGitClient; const name : string; const source : string;
                                    const registriesFolder : string; const ttlMinutes : integer);
begin
  inherited Create;
  FLogger := logger;
  FGitClient := gitClient;
  FSpecReader := TPackageSpecReader.Create(logger);
  FName := name;
  FSource := source;
  FRegistriesFolder := TSystemUtils.ExpandEnvironmentStrings(registriesFolder);
  FTTLMinutes := ttlMinutes;
  FLoaded := false;
  FSpecs := TCollections.CreateDictionary<string, IPackageSpec>;
  FLock := TCriticalSection.Create;
end;

destructor TRegistryCatalog.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TRegistryCatalog.IsFolderSource : boolean;
begin
  //An existing local folder is treated as a folder registry; otherwise if it
  //looks like a git url we mirror it. A non-existent folder that is not a url
  //will fail later in ResolveWorkingDir with a clear error.
  if DirectoryExists(FSource) then
    result := true
  else
    result := not IsLikelyGitUrl(FSource);
end;

function TRegistryCatalog.MirrorDir : string;
begin
  //the name keeps the folder recognisable, the url hash keeps two sources that share a name (one renamed to
  //what the other was called) out of each other's mirror.
  result := IncludeTrailingPathDelimiter(FRegistriesFolder) + SanitizeForFolderName(FName) + '-' +
            Copy(THashSHA256.GetHashStringFromText(LowerCase(Trim(FSource))), 1, 12);
end;

function TRegistryCatalog.MirrorIsCloneOfSource(const cancellationToken : ICancellationToken; const mirror : string) : boolean;
var
  origin : string;
begin
  //when git can't say, the mirror is used - git not working is no reason to throw it away, and the refresh
  //that follows will say git isn't working.
  if not FGitClient.GetRemoteUrl(cancellationToken, mirror, origin) then
    exit(true);
  result := SameText(origin, Trim(FSource));
  if not result then
    FLogger.Information('The registry folder [' + mirror + '] is a clone of ' + origin + ', not ' + FSource + ' - cloning it again.');
end;

function TRegistryCatalog.CloneMirror(const cancellationToken : ICancellationToken; const mirror : string; const forceRefresh : boolean) : boolean;
var
  waitSeconds : Int64;
begin
  //a failed clone is not retried by every query that follows it - in the IDE that is every search and
  //selection, each running git against a url that just failed. Each still says why it couldn't use the source.
  if (not forceRefresh) and FCloneFailed.IsRunning and (FCloneFailed.Elapsed.TotalSeconds < FCloneRetrySeconds) then
  begin
    waitSeconds := FCloneRetrySeconds - Trunc(FCloneFailed.Elapsed.TotalSeconds);
    FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Source [' + FName + '] could not be used - unable to clone the registry : ' + FCloneError +
                    sLineBreak + '  (not tried again for ' + IntToStr(waitSeconds) + ' seconds)');
    exit(false);
  end;

  ForceDirectories(FRegistriesFolder);
  //a stale/partial folder would make clone fail - clear it.
  if DirectoryExists(mirror) then
  begin
    try
      TDirectory.Delete(mirror, true);
    except
      //best effort
    end;
  end;
  FLogger.Information('Cloning package registry [' + FName + '] from ' + FSource);
  result := FGitClient.Clone(cancellationToken, FSource, mirror);
  if result then
  begin
    FCloneFailed.Reset;
    FCloneError := '';
    TouchFetchStamp(mirror);
  end
  else if not cancellationToken.IsCancelled then
  begin
    FCloneError := FGitClient.LastError;
    FCloneFailed := TStopwatch.StartNew;
    if forceRefresh then
      FLogger.Error(TErrorCode.E1203_GitCommandFailed, 'Unable to clone the registry [' + FName + '] from ' + FSource + ' : ' + FCloneError)
    else
      FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Source [' + FName + '] could not be used - unable to clone the registry : ' + FCloneError);
  end;
end;

function TRegistryCatalog.FetchStampFile(const mirrorDir : string) : string;
begin
  result := IncludeTrailingPathDelimiter(mirrorDir) + cFetchStampFileName;
end;

function TRegistryCatalog.MirrorNeedsRefresh(const mirrorDir : string; const forceRefresh : boolean) : boolean;
var
  stampFile : string;
  lastWrite : TDateTime;
  ageMinutes : double;
begin
  if forceRefresh then
    exit(true);
  //negative TTL = never auto-pull (only an explicit refresh, handled above).
  if FTTLMinutes < 0 then
    exit(false);
  stampFile := FetchStampFile(mirrorDir);
  if not FileExists(stampFile) then
    exit(true);
  try
    lastWrite := TFile.GetLastWriteTime(stampFile);
  except
    exit(true);
  end;
  ageMinutes := (Now - lastWrite) * 24 * 60;
  //zero TTL = always pull.
  result := ageMinutes >= FTTLMinutes;
end;

procedure TRegistryCatalog.TouchFetchStamp(const mirrorDir : string);
begin
  try
    TFile.WriteAllText(FetchStampFile(mirrorDir), '');
  except
    on e : Exception do
      FLogger.Debug('Could not write registry fetch stamp : ' + e.Message);
  end;
end;

function TRegistryCatalog.ResolveWorkingDir(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;
var
  mirror : string;
begin
  if IsFolderSource then
  begin
    FWorkingDir := FSource;
    result := DirectoryExists(FWorkingDir);
    if not result then
      FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Source [' + FName + '] could not be used - the registry folder [' + FSource + '] does not exist.');
    exit;
  end;

  //git-url registry - clone on first use, refresh per TTL.
  mirror := MirrorDir;
  FWorkingDir := mirror;
  if (not DirectoryExists(IncludeTrailingPathDelimiter(mirror) + '.git')) or (not MirrorIsCloneOfSource(cancellationToken, mirror)) then
  begin
    result := CloneMirror(cancellationToken, mirror, forceRefresh);
    exit;
  end;
  if cancellationToken.IsCancelled then
    exit(false);

  if MirrorNeedsRefresh(mirror, forceRefresh) then
  begin
    FLogger.Information('Refreshing package registry [' + FName + ']');
    if FGitClient.Pull(cancellationToken, mirror) then
      TouchFetchStamp(mirror)
    else if cancellationToken.IsCancelled then
      exit(false)
    else if forceRefresh then
    begin
      FLogger.Error(TErrorCode.E1203_GitCommandFailed, 'Unable to refresh the registry [' + FName + '] from ' + FSource + ' : ' + FGitClient.LastError);
      exit(false);
    end
    else
      FLogger.Warning(TWarningCode.W1004_SourceUnavailable, 'Source [' + FName + '] could not be refreshed - using the cached copy : ' + FGitClient.LastError);
  end;
  result := true;
end;

procedure TRegistryCatalog.LoadCatalog;
var
  subDirs : TStringDynArray;
  subDir : string;
  dspecFiles : TStringDynArray;
  dspecFile : string;
  expected : string;
  spec : IPackageSpec;
  id : string;
begin
  FSpecs.Clear;
  if (FWorkingDir = '') or (not DirectoryExists(FWorkingDir)) then
    exit;

  subDirs := TDirectory.GetDirectories(FWorkingDir);
  for subDir in subDirs do
  begin
    //skip the git metadata folder
    if SameText(ExtractFileName(ExcludeTrailingPathDelimiter(subDir)), '.git') then
      continue;

    //prefer <folderName>.dspec, then <folderName>.dspec.yaml, else the first dspec in the folder.
    expected := IncludeTrailingPathDelimiter(subDir) + ExtractFileName(ExcludeTrailingPathDelimiter(subDir)) + cPackageSpecExt;
    if FileExists(expected) then
      dspecFile := expected
    else
    begin
      expected := IncludeTrailingPathDelimiter(subDir) + ExtractFileName(ExcludeTrailingPathDelimiter(subDir)) + cLegacyPackageSpecExt;
      if FileExists(expected) then
        dspecFile := expected
      else
      begin
        dspecFiles := TPathUtils.FindDspecFiles(subDir);
        if Length(dspecFiles) = 0 then
          continue;
        dspecFile := dspecFiles[0];
      end;
    end;

    spec := FSpecReader.ReadSpec(dspecFile);
    if spec = nil then
    begin
      FLogger.Warning(TWarningCode.W1208_RegistryEntryInvalid, 'Unable to read the registry dspec [' + dspecFile + '] in [' + FName + '] - skipping it.');
      continue;
    end;
    id := spec.MetaData.Id;
    if id = '' then
    begin
      FLogger.Warning(TWarningCode.W1208_RegistryEntryInvalid, 'The registry dspec [' + dspecFile + '] in [' + FName + '] has no id - skipping it.');
      continue;
    end;
    if FSpecs.ContainsKey(LowerCase(id)) then
      FLogger.Warning(TWarningCode.W1208_RegistryEntryInvalid, 'The package id [' + id + '] appears more than once in the registry [' + FName + '] - using the first.')
    else
      FSpecs[LowerCase(id)] := spec;
  end;
end;

function TRegistryCatalog.EnsureUpdated(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;
begin
  //held across the clone/pull - a second thread waits for the first one's result rather than cloning into the
  //same folder at the same time.
  FLock.Enter;
  try
    if FLoaded and (not forceRefresh) then
      exit(true);

    result := ResolveWorkingDir(cancellationToken, forceRefresh);
    if not result then
      exit;

    LoadCatalog;
    FLoaded := true;
  finally
    FLock.Leave;
  end;
end;

function TRegistryCatalog.GetPackageSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;
begin
  result := nil;
  if not EnsureUpdated(cancellationToken, false) then
    exit;
  FLock.Enter;
  try
    FSpecs.TryGetValue(LowerCase(id), result);
  finally
    FLock.Leave;
  end;
end;

function TRegistryCatalog.GetPackageIds(const cancellationToken : ICancellationToken) : IList<string>;
var
  spec : IPackageSpec;
begin
  result := TCollections.CreateList<string>;
  if not EnsureUpdated(cancellationToken, false) then
    exit;
  FLock.Enter;
  try
    for spec in FSpecs.Values do
      result.Add(spec.MetaData.Id);
  finally
    FLock.Leave;
  end;
end;

end.
