{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2026 Vincent Parrett and contributors               }
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

unit DPM.Core.Trust.State;

// Per-user sticky trust state ratchet (TOFU). Records, per package id, the
// highest assurance ever observed for the author dimension (Phase 1) — the
// repository dimension is added in Phase 2.
//
// File: %APPDATA%\.dpm\trust-state.yaml. The CLI, the IDE and parallel builds share it, so every
// change is made under a named mutex to a fresh read of the file - two processes recording
// different packages both keep their records - and written atomically (temp + rename), so a crash
// never leaves a half-written file. A file that can't be parsed is set aside, never overwritten.

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Logging,
  DPM.Core.Trust.Interfaces;

type
  //a change to the state - true when it changed anything, so it must be saved.
  TTrustStateChange = reference to function : boolean;

  TYamlTrustStateService = class(TInterfacedObject, ITrustStateService)
  private
    FLogger : ILogger;
    FFilePath : string;
    FLock : TCriticalSection;
    //held while the file is read or written - across processes.
    FMutex : THandle;
    FEntries : IDictionary<string, TAuthorTrustEntry>;
    FRepoEntries : IDictionary<string, TRepositoryTrustEntry>;
    FLoaded : boolean;
    //the file as it was when last read or written - a different one means another process wrote it.
    FLoadedExists : boolean;
    FLoadedWriteTime : Int64;
    FLoadedSize : Int64;
    function ReadStamp(out writeTime : Int64; out size : Int64) : boolean;
    function IsCurrent : boolean;
    procedure RememberStamp;
    procedure AcquireFileLock;
    procedure ReleaseFileLock;
    procedure EnsureCurrent;
    procedure Change(const apply : TTrustStateChange);
    procedure LoadLocked;
    procedure SetAsideCorruptFile(const reason : string);
    procedure SaveLocked;
  protected
    function TryGetAuthor(const packageId : string; out entry : TAuthorTrustEntry) : boolean;
    procedure RecordAuthor(const packageId : string; const entry : TAuthorTrustEntry);
    procedure AcknowledgeAuthorDowngrade(const packageId : string);
    procedure BlockPermanently(const packageId : string);
    procedure RemoveAuthor(const packageId : string);
    function TryGetRepository(const packageId : string; out entry : TRepositoryTrustEntry) : boolean;
    procedure RecordRepository(const packageId : string; const entry : TRepositoryTrustEntry);
    procedure RemoveRepository(const packageId : string);
  public
    // Default location: %APPDATA%\.dpm\trust-state.yaml.
    constructor Create(const logger : ILogger); overload;
    constructor Create(const logger : ILogger; const filePath : string); overload;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils,
  System.IOUtils,
  System.Generics.Defaults,
  DPM.Core.Diagnostics,
  DPM.Core.Utils.DateTime,
  DPM.Core.Utils.Files,
  DPM.Core.Utils.Hash,
  DPM.Core.Utils.Strings;

const
  cMutexWaitMs = 30000;
  cReadAttempts = 10;
  cReadRetryDelayMs = 25;

//one mutex per state file - a test's file, or a redirected APPDATA, doesn't wait on the real one.
function MutexNameFor(const filePath : string) : string;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.Create(TEncoding.UTF8.GetBytes(LowerCase(ExpandFileName(filePath))));
  try
    result := 'Local\DPM.TrustState.' + Copy(THashSHA256.GetHashString(stream), 1, 16);
  finally
    stream.Free;
  end;
end;

//An AV scanner or the search indexer briefly holding the file is not a reason to call it corrupt.
function ReadStateText(const filePath : string) : string;
var
  attempt : integer;
begin
  attempt := 0;
  while true do
  begin
    try
      result := TFile.ReadAllText(filePath);
      exit;
    except
      on e : Exception do
      begin
        Inc(attempt);
        if attempt >= cReadAttempts then
          raise ETrust.CreateFmt('Unable to read the trust state [%s] : %s', [filePath, e.Message]);
        Sleep(cReadRetryDelayMs);
      end;
    end;
  end;
end;

constructor TYamlTrustStateService.Create(const logger : ILogger);
var
  appData : string;
begin
  appData := GetEnvironmentVariable('APPDATA');
  if appData = '' then
    raise ETrust.Create('APPDATA environment variable is not set');
  Create(logger, IncludeTrailingPathDelimiter(appData) + '.dpm' + PathDelim + 'trust-state.yaml');
end;

constructor TYamlTrustStateService.Create(const logger : ILogger; const filePath : string);
begin
  inherited Create;
  FLogger := logger;
  FFilePath := filePath;
  FLock := TCriticalSection.Create;
  FMutex := CreateMutex(nil, false, PChar(MutexNameFor(filePath)));
  if FMutex = 0 then
    raise ETrust.CreateFmt('Unable to create the trust state lock : %s', [SysErrorMessage(GetLastError)]);
  // Case-insensitive keys: package ids are treated case-insensitively everywhere
  // else (TPackageCache.MakeCacheKey lowercases the id). The trust entry is
  // recorded under the canonical id parsed from the .dpkg filename on install,
  // but RemovePackage removes under the id the user typed on the command line.
  // With an ordinal (case-sensitive) dictionary a casing difference makes the
  // Remove silently no-op, so the package folders get deleted while the trust
  // entry survives - tripping the author/repository ratchet on the next install.
  FEntries := TCollections.CreateDictionary<string, TAuthorTrustEntry>(TIStringComparer.Ordinal);
  FRepoEntries := TCollections.CreateDictionary<string, TRepositoryTrustEntry>(TIStringComparer.Ordinal);
end;

destructor TYamlTrustStateService.Destroy;
begin
  if FMutex <> 0 then
    CloseHandle(FMutex);
  FLock.Free;
  inherited;
end;

function TYamlTrustStateService.ReadStamp(out writeTime : Int64; out size : Int64) : boolean;
var
  fad : TWin32FileAttributeData;
begin
  writeTime := 0;
  size := 0;
  result := GetFileAttributesEx(PChar(FFilePath), GetFileExInfoStandard, @fad);
  if result then
  begin
    writeTime := Int64(fad.ftLastWriteTime.dwHighDateTime) shl 32 or fad.ftLastWriteTime.dwLowDateTime;
    size := Int64(fad.nFileSizeHigh) shl 32 or fad.nFileSizeLow;
  end;
end;

function TYamlTrustStateService.IsCurrent : boolean;
var
  writeTime : Int64;
  size : Int64;
  exists : boolean;
begin
  result := false;
  if not FLoaded then
    exit;
  exists := ReadStamp(writeTime, size);
  if exists <> FLoadedExists then
    exit;
  result := (not exists) or ((writeTime = FLoadedWriteTime) and (size = FLoadedSize));
end;

procedure TYamlTrustStateService.RememberStamp;
begin
  FLoadedExists := ReadStamp(FLoadedWriteTime, FLoadedSize);
end;

procedure TYamlTrustStateService.AcquireFileLock;
begin
  case WaitForSingleObject(FMutex, cMutexWaitMs) of
    //abandoned : the process that held it died. The file is still whole - it is only ever replaced.
    WAIT_OBJECT_0, WAIT_ABANDONED : ;
  else
    raise ETrust.CreateFmt('Timed out waiting for another DPM process to finish with the trust state [%s]', [FFilePath]);
  end;
end;

procedure TYamlTrustStateService.ReleaseFileLock;
begin
  ReleaseMutex(FMutex);
end;

//FLock is held.
procedure TYamlTrustStateService.EnsureCurrent;
begin
  if IsCurrent then
    exit;
  AcquireFileLock;
  try
    LoadLocked;
  finally
    ReleaseFileLock;
  end;
end;

procedure TYamlTrustStateService.Change(const apply : TTrustStateChange);
begin
  FLock.Enter;
  try
    AcquireFileLock;
    try
      //another process may have written since this one last read - change a fresh copy, so its records survive.
      if not IsCurrent then
        LoadLocked;
      if apply() then
        SaveLocked;
    finally
      ReleaseFileLock;
    end;
  finally
    FLock.Leave;
  end;
end;

//FLock and the file lock are held.
procedure TYamlTrustStateService.LoadLocked;
var
  text : string;
  doc : IYAMLDocument;
  root : IYAMLValue;
  mapping : IYAMLMapping;
  i : integer;
  key : string;
  packageNode : IYAMLValue;
  entry : TAuthorTrustEntry;
  repoEntry : TRepositoryTrustEntry;
  iso : string;
  hasRepoFields : boolean;
begin
  FEntries.Clear;
  FRepoEntries.Clear;
  FLoaded := true;
  RememberStamp;
  if not FLoadedExists then
    exit;
  text := ReadStateText(FFilePath);
  try
    doc := TYAML.LoadFromString(text);
  except
    on e : Exception do
    begin
      SetAsideCorruptFile(e.Message);
      exit;
    end;
  end;
  //nothing recorded yet.
  if (doc = nil) or (doc.Root = nil) or doc.Root.IsNull then
    exit;
  if not doc.Root.IsMapping then
  begin
    SetAsideCorruptFile('it does not hold a mapping of package ids');
    exit;
  end;

  root := doc.Root;
  mapping := root.AsMapping;
  for i := 0 to mapping.Count - 1 do
  begin
    key := mapping.GetKey(i);
    packageNode := mapping.Values[key];
    if (packageNode = nil) or not packageNode.IsMapping then
      Continue;
    entry.LastAuthorSpkiHex := '';
    entry.LastSeenAuthorSigned := false;
    entry.DowngradeAcknowledged := false;
    entry.BlockedPermanently := false;
    entry.LastSeenAt := 0;
    if packageNode.AsMapping.Values['lastAuthorSpki'].IsString then
      entry.LastAuthorSpkiHex := packageNode.AsMapping.Values['lastAuthorSpki'].AsString;
    if packageNode.AsMapping.Values['lastSeenAuthorSigned'].IsBoolean then
      entry.LastSeenAuthorSigned := packageNode.AsMapping.Values['lastSeenAuthorSigned'].AsBoolean;
    if packageNode.AsMapping.Values['downgradeAcknowledged'].IsBoolean then
      entry.DowngradeAcknowledged := packageNode.AsMapping.Values['downgradeAcknowledged'].AsBoolean;
    if packageNode.AsMapping.Values['blockedPermanently'].IsBoolean then
      entry.BlockedPermanently := packageNode.AsMapping.Values['blockedPermanently'].AsBoolean;
    if packageNode.AsMapping.Values['lastSeenAt'].IsString then
    begin
      iso := packageNode.AsMapping.Values['lastSeenAt'].AsString;
      try
        entry.LastSeenAt := TDPMDateTimeUtils.ISO8601ToDate(iso, True);
      except
        on Exception do
          entry.LastSeenAt := 0;
      end;
    end;
    FEntries[key] := entry;

    // Phase 2 — pick up the repository ratchet fields if present.
    repoEntry.TrustedRepoSpkiHex := '';
    repoEntry.Namespace := '';
    repoEntry.FirstSeenAt := 0;
    repoEntry.LastSeenAt := 0;
    hasRepoFields := false;
    if packageNode.AsMapping.Values['repoTrustedSpki'].IsString then
    begin
      repoEntry.TrustedRepoSpkiHex := packageNode.AsMapping.Values['repoTrustedSpki'].AsString;
      hasRepoFields := repoEntry.TrustedRepoSpkiHex <> '';
    end;
    if packageNode.AsMapping.Values['repoNamespace'].IsString then
      repoEntry.Namespace := packageNode.AsMapping.Values['repoNamespace'].AsString;
    if packageNode.AsMapping.Values['repoFirstSeenAt'].IsString then
    begin
      iso := packageNode.AsMapping.Values['repoFirstSeenAt'].AsString;
      try
        repoEntry.FirstSeenAt := TDPMDateTimeUtils.ISO8601ToDate(iso, True);
      except
        on Exception do
          repoEntry.FirstSeenAt := 0;
      end;
    end;
    if packageNode.AsMapping.Values['repoLastSeenAt'].IsString then
    begin
      iso := packageNode.AsMapping.Values['repoLastSeenAt'].AsString;
      try
        repoEntry.LastSeenAt := TDPMDateTimeUtils.ISO8601ToDate(iso, True);
      except
        on Exception do
          repoEntry.LastSeenAt := 0;
      end;
    end;
    if hasRepoFields then
      FRepoEntries[key] := repoEntry;
  end;
end;

//The ratchets lose their history if the file goes - keep it for whoever wants to look, and start again
//rather than write over it. FLock and the file lock are held.
procedure TYamlTrustStateService.SetAsideCorruptFile(const reason : string);
var
  backup : string;
  stamp : string;
  n : integer;
begin
  stamp := FormatDateTime('yyyymmdd"T"hhnnss"Z"', TTimeZone.Local.ToUniversalTime(Now));
  backup := FFilePath + '.corrupt-' + stamp;
  n := 1;
  while FileExists(backup) do
  begin
    Inc(n);
    backup := FFilePath + '.corrupt-' + stamp + '-' + IntToStr(n);
  end;
  if not MoveFileEx(PChar(FFilePath), PChar(backup), MOVEFILE_WRITE_THROUGH) then
    raise ETrust.CreateFmt('The trust state [%s] can''t be read (%s), and setting it aside as [%s] failed : %s',
      [FFilePath, reason, backup, SysErrorMessage(GetLastError)]);
  FLogger.Warning(TWarningCode.W7013_TrustStateCorrupt, Format('The trust state [%s] can''t be read (%s). It has been kept as [%s] and DPM ' +
    'starts again with no record of the signatures it has seen - each package''s signer is trusted on first use again.',
    [FFilePath, reason, backup]));
  RememberStamp;
end;

//FLock and the file lock are held.
procedure TYamlTrustStateService.SaveLocked;
var
  sb : TStringBuilder;
  pair : TPair<string, TAuthorTrustEntry>;
  repoPair : TPair<string, TRepositoryTrustEntry>;
  dir : string;
  allKeys : IDictionary<string, byte>;
  key : string;
  authorEntry : TAuthorTrustEntry;
  repoEntry : TRepositoryTrustEntry;
  hasAuthor, hasRepo : boolean;
begin
  dir := ExtractFilePath(FFilePath);
  if (dir <> '') and not DirectoryExists(dir) then
    ForceDirectories(dir);

  // Union of both keyspaces — a package id may have only repo state
  // (never seen an author signature) or only author state (no trusted
  // repo has yet signed it).
  allKeys := TCollections.CreateDictionary<string, byte>(TIStringComparer.Ordinal);
  for pair in FEntries do
    allKeys[pair.Key] := 0;
  for repoPair in FRepoEntries do
    allKeys[repoPair.Key] := 0;

  sb := TStringBuilder.Create;
  try
    sb.AppendLine('---');
    for key in allKeys.Keys do
    begin
      hasAuthor := FEntries.TryGetValue(key, authorEntry);
      hasRepo := FRepoEntries.TryGetValue(key, repoEntry);
      sb.Append(TStringUtils.YamlSingleQuote(key)); sb.AppendLine(':');
      if hasAuthor then
      begin
        sb.Append('  lastAuthorSpki: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(authorEntry.LastAuthorSpkiHex));
        sb.Append('  lastSeenAuthorSigned: ');
        sb.AppendLine(BoolToStr(authorEntry.LastSeenAuthorSigned, True));
        sb.Append('  downgradeAcknowledged: ');
        sb.AppendLine(BoolToStr(authorEntry.DowngradeAcknowledged, True));
        sb.Append('  blockedPermanently: ');
        sb.AppendLine(BoolToStr(authorEntry.BlockedPermanently, True));
        sb.Append('  lastSeenAt: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(TDPMDateTimeUtils.DateToISO8601(authorEntry.LastSeenAt, True)));
      end;
      if hasRepo then
      begin
        sb.Append('  repoTrustedSpki: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(repoEntry.TrustedRepoSpkiHex));
        sb.Append('  repoNamespace: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(repoEntry.Namespace));
        sb.Append('  repoFirstSeenAt: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(TDPMDateTimeUtils.DateToISO8601(repoEntry.FirstSeenAt, True)));
        sb.Append('  repoLastSeenAt: ');
        sb.AppendLine(TStringUtils.YamlSingleQuote(TDPMDateTimeUtils.DateToISO8601(repoEntry.LastSeenAt, True)));
      end;
    end;

    // An AV scanner or the search indexer routinely holds a brief handle on a just-written file,
    // and FullReVerify rewrites this file once per package - the replace retries for that.
    try
      TFileUtils.AtomicWriteAllText(FFilePath, sb.ToString, TEncoding.UTF8);
    except
      on e : Exception do
        raise ETrust.CreateFmt('Unable to save the trust state [%s] : %s', [FFilePath, e.Message]);
    end;
    RememberStamp;
  finally
    sb.Free;
  end;
end;

function TYamlTrustStateService.TryGetAuthor(const packageId : string; out entry : TAuthorTrustEntry) : boolean;
begin
  FLock.Enter;
  try
    EnsureCurrent;
    result := FEntries.TryGetValue(packageId, entry);
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RecordAuthor(const packageId : string; const entry : TAuthorTrustEntry);
var
  id : string;
  value : TAuthorTrustEntry;
begin
  id := packageId;
  value := entry;
  Change(
    function : boolean
    begin
      FEntries[id] := value;
      result := true;
    end);
end;

procedure TYamlTrustStateService.AcknowledgeAuthorDowngrade(const packageId : string);
var
  id : string;
begin
  id := packageId;
  Change(
    function : boolean
    var
      entry : TAuthorTrustEntry;
    begin
      result := FEntries.TryGetValue(id, entry);
      if result then
      begin
        entry.DowngradeAcknowledged := true;
        FEntries[id] := entry;
      end;
    end);
end;

procedure TYamlTrustStateService.BlockPermanently(const packageId : string);
var
  id : string;
begin
  id := packageId;
  Change(
    function : boolean
    var
      entry : TAuthorTrustEntry;
    begin
      if not FEntries.TryGetValue(id, entry) then
      begin
        entry.LastAuthorSpkiHex := '';
        entry.LastSeenAuthorSigned := false;
        entry.DowngradeAcknowledged := false;
        entry.LastSeenAt := TTimeZone.Local.ToUniversalTime(Now);
      end;
      entry.BlockedPermanently := true;
      FEntries[id] := entry;
      result := true;
    end);
end;

procedure TYamlTrustStateService.RemoveAuthor(const packageId : string);
var
  id : string;
begin
  id := packageId;
  Change(
    function : boolean
    begin
      result := FEntries.ContainsKey(id);
      if result then
        FEntries.Remove(id);
    end);
end;

function TYamlTrustStateService.TryGetRepository(const packageId : string;
                                                  out entry : TRepositoryTrustEntry) : boolean;
begin
  FLock.Enter;
  try
    EnsureCurrent;
    result := FRepoEntries.TryGetValue(packageId, entry);
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RecordRepository(const packageId : string;
                                                   const entry : TRepositoryTrustEntry);
var
  id : string;
  value : TRepositoryTrustEntry;
begin
  id := packageId;
  value := entry;
  Change(
    function : boolean
    begin
      FRepoEntries[id] := value;
      result := true;
    end);
end;

procedure TYamlTrustStateService.RemoveRepository(const packageId : string);
var
  id : string;
begin
  id := packageId;
  Change(
    function : boolean
    begin
      result := FRepoEntries.ContainsKey(id);
      if result then
        FRepoEntries.Remove(id);
    end);
end;

end.
