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
  VSoft.CancellationToken,
  Spring.Collections,
  DPM.Core.Logging,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Git.Interfaces,
  DPM.Core.Registry.Interfaces;

type
  TRegistryCatalog = class(TInterfacedObject, IRegistryCatalog)
  private
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
    function IsFolderSource : boolean;
    function MirrorDir : string;
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
    //exposed for diagnostics/testing
    property WorkingDir : string read FWorkingDir;
  end;

  //true if source looks like a git url rather than a local folder path.
  function IsLikelyGitUrl(const source : string) : boolean;

implementation

uses
  System.SysUtils,
  System.Types,
  System.IOUtils,
  System.StrUtils,
  DPM.Core.Spec.Reader,
  DPM.Core.Utils.System;

const
  cFetchStampFileName = '.dpm-fetched';
  cDspecExt = '.dspec.yaml';

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
  result := IncludeTrailingPathDelimiter(FRegistriesFolder) + SanitizeForFolderName(FName);
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
      FLogger.Error('Registry folder [' + FSource + '] does not exist.');
    exit;
  end;

  //git-url registry - clone on first use, refresh per TTL.
  mirror := MirrorDir;
  FWorkingDir := mirror;
  if not DirectoryExists(IncludeTrailingPathDelimiter(mirror) + '.git') then
  begin
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
      TouchFetchStamp(mirror);
    exit;
  end;

  if MirrorNeedsRefresh(mirror, forceRefresh) then
  begin
    FLogger.Information('Refreshing package registry [' + FName + ']');
    if FGitClient.Pull(cancellationToken, mirror) then
      TouchFetchStamp(mirror)
    else
      FLogger.Warning('Could not refresh registry [' + FName + '] - using cached copy.');
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

    //prefer <folderName>.dspec.yaml, else the first *.dspec.yaml in the folder.
    expected := IncludeTrailingPathDelimiter(subDir) + ExtractFileName(ExcludeTrailingPathDelimiter(subDir)) + cDspecExt;
    if FileExists(expected) then
      dspecFile := expected
    else
    begin
      dspecFiles := TDirectory.GetFiles(subDir, '*' + cDspecExt);
      if Length(dspecFiles) = 0 then
        continue;
      dspecFile := dspecFiles[0];
    end;

    spec := FSpecReader.ReadSpec(dspecFile);
    if spec = nil then
    begin
      FLogger.Warning('Could not read registry dspec [' + dspecFile + '] - skipping.');
      continue;
    end;
    id := spec.MetaData.Id;
    if id = '' then
    begin
      FLogger.Warning('Registry dspec [' + dspecFile + '] has no id - skipping.');
      continue;
    end;
    if FSpecs.ContainsKey(LowerCase(id)) then
      FLogger.Warning('Duplicate package id [' + id + '] in registry [' + FName + '] - using first.')
    else
      FSpecs[LowerCase(id)] := spec;
  end;
end;

function TRegistryCatalog.EnsureUpdated(const cancellationToken : ICancellationToken; const forceRefresh : boolean) : boolean;
begin
  if FLoaded and (not forceRefresh) then
    exit(true);

  result := ResolveWorkingDir(cancellationToken, forceRefresh);
  if not result then
    exit;

  LoadCatalog;
  FLoaded := true;
end;

function TRegistryCatalog.GetPackageSpec(const cancellationToken : ICancellationToken; const id : string) : IPackageSpec;
begin
  result := nil;
  if not EnsureUpdated(cancellationToken, false) then
    exit;
  FSpecs.TryGetValue(LowerCase(id), result);
end;

function TRegistryCatalog.GetPackageIds(const cancellationToken : ICancellationToken) : IList<string>;
var
  spec : IPackageSpec;
begin
  result := TCollections.CreateList<string>;
  if not EnsureUpdated(cancellationToken, false) then
    exit;
  for spec in FSpecs.Values do
    result.Add(spec.MetaData.Id);
end;

end.
