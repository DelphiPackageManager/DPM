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
// File: %APPDATA%\.dpm\trust-state.yaml. Atomic writes (temp + rename) so
// a crash during update never leaves a half-written state file.

interface

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Trust.Interfaces;

type
  TYamlTrustStateService = class(TInterfacedObject, ITrustStateService)
  private
    FFilePath : string;
    FLock : TCriticalSection;
    FEntries : IDictionary<string, TAuthorTrustEntry>;
    FRepoEntries : IDictionary<string, TRepositoryTrustEntry>;
    FLoaded : boolean;
    procedure EnsureLoaded;
    procedure SaveLocked;
    procedure LoadLocked;
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
    constructor Create; overload;
    constructor Create(const filePath : string); overload;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils,
  System.IOUtils,
  DPM.Core.Utils.DateTime;

constructor TYamlTrustStateService.Create;
var
  appData : string;
begin
  appData := GetEnvironmentVariable('APPDATA');
  if appData = '' then
    raise ETrust.Create('APPDATA environment variable is not set');
  Create(IncludeTrailingPathDelimiter(appData) + '.dpm' + PathDelim + 'trust-state.yaml');
end;

constructor TYamlTrustStateService.Create(const filePath : string);
begin
  inherited Create;
  FFilePath := filePath;
  FLock := TCriticalSection.Create;
  FEntries := TCollections.CreateDictionary<string, TAuthorTrustEntry>;
  FRepoEntries := TCollections.CreateDictionary<string, TRepositoryTrustEntry>;
end;

destructor TYamlTrustStateService.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TYamlTrustStateService.EnsureLoaded;
begin
  if FLoaded then
    exit;
  LoadLocked;
  FLoaded := true;
end;

procedure TYamlTrustStateService.LoadLocked;
var
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
  if not FileExists(FFilePath) then
    exit;
  try
    doc := TYAML.LoadFromFile(FFilePath);
  except
    on Exception do
      exit;   // tolerate corrupt state; rebuild on next write
  end;
  if (doc = nil) or (doc.Root = nil) or not doc.Root.IsMapping then
    exit;

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

procedure TYamlTrustStateService.SaveLocked;
var
  sb : TStringBuilder;
  pair : TPair<string, TAuthorTrustEntry>;
  repoPair : TPair<string, TRepositoryTrustEntry>;
  dir : string;
  tempPath : string;
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
  allKeys := TCollections.CreateDictionary<string, byte>;
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
      sb.Append(key); sb.AppendLine(':');
      if hasAuthor then
      begin
        sb.Append('  lastAuthorSpki: ''');
        sb.Append(authorEntry.LastAuthorSpkiHex);
        sb.AppendLine('''');
        sb.Append('  lastSeenAuthorSigned: ');
        sb.AppendLine(BoolToStr(authorEntry.LastSeenAuthorSigned, True));
        sb.Append('  downgradeAcknowledged: ');
        sb.AppendLine(BoolToStr(authorEntry.DowngradeAcknowledged, True));
        sb.Append('  blockedPermanently: ');
        sb.AppendLine(BoolToStr(authorEntry.BlockedPermanently, True));
        sb.Append('  lastSeenAt: ''');
        sb.Append(TDPMDateTimeUtils.DateToISO8601(authorEntry.LastSeenAt, True));
        sb.AppendLine('''');
      end;
      if hasRepo then
      begin
        sb.Append('  repoTrustedSpki: ''');
        sb.Append(repoEntry.TrustedRepoSpkiHex);
        sb.AppendLine('''');
        sb.Append('  repoNamespace: ''');
        sb.Append(repoEntry.Namespace);
        sb.AppendLine('''');
        sb.Append('  repoFirstSeenAt: ''');
        sb.Append(TDPMDateTimeUtils.DateToISO8601(repoEntry.FirstSeenAt, True));
        sb.AppendLine('''');
        sb.Append('  repoLastSeenAt: ''');
        sb.Append(TDPMDateTimeUtils.DateToISO8601(repoEntry.LastSeenAt, True));
        sb.AppendLine('''');
      end;
    end;

    tempPath := FFilePath + '.tmp';
    TFile.WriteAllText(tempPath, sb.ToString, TEncoding.UTF8);
    if FileExists(FFilePath) then
      DeleteFile(FFilePath);
    if not RenameFile(tempPath, FFilePath) then
      raise ETrust.CreateFmt('Failed to atomically replace %s', [FFilePath]);
  finally
    sb.Free;
  end;
end;

function TYamlTrustStateService.TryGetAuthor(const packageId : string; out entry : TAuthorTrustEntry) : boolean;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    result := FEntries.TryGetValue(packageId, entry);
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RecordAuthor(const packageId : string; const entry : TAuthorTrustEntry);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    FEntries[packageId] := entry;
    SaveLocked;
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.AcknowledgeAuthorDowngrade(const packageId : string);
var
  entry : TAuthorTrustEntry;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    if FEntries.TryGetValue(packageId, entry) then
    begin
      entry.DowngradeAcknowledged := true;
      FEntries[packageId] := entry;
      SaveLocked;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.BlockPermanently(const packageId : string);
var
  entry : TAuthorTrustEntry;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    if not FEntries.TryGetValue(packageId, entry) then
    begin
      entry.LastAuthorSpkiHex := '';
      entry.LastSeenAuthorSigned := false;
      entry.DowngradeAcknowledged := false;
      entry.LastSeenAt := TTimeZone.Local.ToUniversalTime(Now);
    end;
    entry.BlockedPermanently := true;
    FEntries[packageId] := entry;
    SaveLocked;
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RemoveAuthor(const packageId : string);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    if FEntries.ContainsKey(packageId) then
    begin
      FEntries.Remove(packageId);
      SaveLocked;
    end;
  finally
    FLock.Leave;
  end;
end;

function TYamlTrustStateService.TryGetRepository(const packageId : string;
                                                  out entry : TRepositoryTrustEntry) : boolean;
begin
  FLock.Enter;
  try
    EnsureLoaded;
    result := FRepoEntries.TryGetValue(packageId, entry);
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RecordRepository(const packageId : string;
                                                   const entry : TRepositoryTrustEntry);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    FRepoEntries[packageId] := entry;
    SaveLocked;
  finally
    FLock.Leave;
  end;
end;

procedure TYamlTrustStateService.RemoveRepository(const packageId : string);
begin
  FLock.Enter;
  try
    EnsureLoaded;
    if FRepoEntries.ContainsKey(packageId) then
    begin
      FRepoEntries.Remove(packageId);
      SaveLocked;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
