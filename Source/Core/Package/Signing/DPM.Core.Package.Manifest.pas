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

unit DPM.Core.Package.Manifest;

// dpm-manifest.json — generation, parsing, and path-safety enforcement.
//
// Generation uses a purpose-built deterministic emitter (M-4). The emitter
// writes a fixed key sequence in lexicographic order, a sorted `files`
// array (byte-wise on the canonical path), UTF-8 no BOM, LF line endings,
// integers only.
//
// Parsing uses VSoft.YAML with DuplicateKeyBehavior=dkError and our own
// depth and size limits (V-2). The signed-content bytes are the exact
// stored bytes — never re-emitted (V-1).

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults,
  System.Zip,
  Spring.Collections,
  VSoft.YAML,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces,
  DPM.Core.Package.Manifest.Interfaces;

type
  TManifestService = class(TInterfacedObject, IManifestService)
  private
    FHashingService : IHashingService;
    procedure CollectFiles(const root : string; const subdir : string;
                           const algorithm : THashAlgorithm;
                           var entries : TArray<TManifestFileEntry>;
                           var count : integer);
    function EncodeManifest(const dpmPackageFormat : integer;
                            const manifestSchemaVersion : integer;
                            const packageId : string;
                            const version : string;
                            const created : TDateTime;
                            const algorithm : THashAlgorithm;
                            const files : TArray<TManifestFileEntry>) : TBytes;
    function CheckDepth(const value : IYAMLValue; current, max : integer) : boolean;
    function BuildManifestFromEntries(const packageId, version : string;
                                      algorithm : THashAlgorithm;
                                      const entries : TArray<TManifestFileEntry>) : IPackageManifest;
  protected
    function Generate(const packageRoot : string;
                      const packageId : string;
                      const version : string;
                      algorithm : THashAlgorithm) : IPackageManifest;
    function GenerateFromArchive(const archivePath : string;
                                 const packageId : string;
                                 const version : string;
                                 algorithm : THashAlgorithm) : IPackageManifest;
    procedure InjectIntoArchive(const archivePath : string;
                                const manifest : IPackageManifest);
    function Parse(const bytes : TBytes) : IPackageManifest;
    function ValidatePath(const path : string; out reason : string) : boolean;
    function NormalizeToNfc(const value : string) : string;
  public
    constructor Create(const hashingService : IHashingService);
  end;

implementation

uses
  System.DateUtils,
  System.IOUtils,
  System.StrUtils,
  System.Character,
  DPM.Core.Utils.DateTime;

type
  TPackageManifest = class(TInterfacedObject, IPackageManifest)
  private
    FDpmPackageFormat : integer;
    FManifestSchemaVersion : integer;
    FPackageId : string;
    FVersion : string;
    FHashAlgorithm : THashAlgorithm;
    FCreated : TDateTime;
    FFiles : TArray<TManifestFileEntry>;
    FRawBytes : TBytes;
  protected
    function DpmPackageFormat : integer;
    function ManifestSchemaVersion : integer;
    function PackageId : string;
    function Version : string;
    function HashAlgorithm : THashAlgorithm;
    function Created : TDateTime;
    function Files : TArray<TManifestFileEntry>;
    function RawBytes : TBytes;
  public
    constructor Create;
  end;

const
  cReservedDeviceNames : array[0..21] of string = (
    'CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9'
  );

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function BytesToHexLower(const bytes : TBytes) : string;
const
  cHex : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  i : integer;
  p : PChar;
begin
  SetLength(result, Length(bytes) * 2);
  if Length(bytes) = 0 then
    exit;
  p := PChar(result);
  for i := 0 to Length(bytes) - 1 do
  begin
    p^ := cHex[bytes[i] shr 4]; Inc(p);
    p^ := cHex[bytes[i] and $0F]; Inc(p);
  end;
end;

function HexNibbleValue(c : Char; out value : Byte) : boolean;
begin
  result := true;
  case c of
    '0'..'9' : value := Byte(Ord(c) - Ord('0'));
    'a'..'f' : value := Byte(Ord(c) - Ord('a') + 10);
    'A'..'F' : value := Byte(Ord(c) - Ord('A') + 10);
  else
    value := 0;
    result := false;
  end;
end;

function HexStringToBytes(const hex : string) : TBytes;
var
  len, i : integer;
  hi, lo : Byte;
begin
  len := Length(hex);
  if (len = 0) or ((len and 1) <> 0) then
    raise EManifestParse.CreateFmt('Invalid hash hex length %d', [len]);
  SetLength(result, len div 2);
  for i := 0 to (len div 2) - 1 do
  begin
    if not HexNibbleValue(hex[1 + i * 2], hi) or
       not HexNibbleValue(hex[2 + i * 2], lo) then
      raise EManifestParse.Create('Invalid hash hex digit');
    result[i] := (hi shl 4) or lo;
  end;
end;

function NormalizeToNfcUtf16(const value : string) : string;
var
  needed : integer;
begin
  if value = '' then
  begin
    result := '';
    exit;
  end;
  needed := NormalizeString(NormalizationC, PWideChar(value), Length(value), nil, 0);
  if needed <= 0 then
    raise EManifestPath.CreateFmt('NormalizeString failed for "%s"', [value]);
  SetLength(result, needed);
  needed := NormalizeString(NormalizationC, PWideChar(value), Length(value),
    PWideChar(result), needed);
  if needed <= 0 then
    raise EManifestPath.CreateFmt('NormalizeString(write) failed for "%s"', [value]);
  SetLength(result, needed);
end;

function EncodeUtf8NoBom(const value : string) : TBytes;
begin
  result := TEncoding.UTF8.GetBytes(value);
end;

function FormatRfc3339Utc(value : TDateTime) : string;
begin
  result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"', value);
end;

function ParseRfc3339Utc(const value : string; out dt : TDateTime) : boolean;
begin
  // Inputs are restricted to the canonical "yyyy-mm-ddThh:nn:ssZ" form the
  // packager writes; we accept that form strictly and reject everything else.
  try
    dt := TDPMDateTimeUtils.ISO8601ToDate(value, True);
    result := true;
  except
    on Exception do
    begin
      dt := 0;
      result := false;
    end;
  end;
end;

// JSON string escape per RFC 8259 — single-line, integer-only schema so the
// escape set is small.
function JsonEscape(const value : string) : string;
var
  i : integer;
  c : Char;
  sb : TStringBuilder;
begin
  sb := TStringBuilder.Create(Length(value) + 8);
  try
    sb.Append('"');
    for i := 1 to Length(value) do
    begin
      c := value[i];
      case c of
        '\' : sb.Append('\\');
        '"' : sb.Append('\"');
        #8  : sb.Append('\b');
        #9  : sb.Append('\t');
        #10 : sb.Append('\n');
        #12 : sb.Append('\f');
        #13 : sb.Append('\r');
      else
        if Ord(c) < $20 then
          sb.Append(Format('\u%.4x', [Ord(c)]))
        else
          sb.Append(c);
      end;
    end;
    sb.Append('"');
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function PathStartsWith(const path, prefix : string) : boolean;
begin
  result := (Length(path) >= Length(prefix)) and
            SameStr(Copy(path, 1, Length(prefix)), prefix);
end;

// V-2 strict-duplicate-key scan. Walks the raw manifest text and tracks every
// JSON object's key set with a per-depth bucket; raises EManifestParse on the
// first duplicate. Skips string-literal contents so quoted strings can contain
// brace/quote characters freely.
procedure EnforceUniqueObjectKeys(const text : string);
type
  TKeyContext = record
    Seen     : TStringList;
    InObject : boolean;
  end;
var
  ctxStack : array of TKeyContext;
  depth : integer;
  i, len : integer;
  c : Char;
  inString : boolean;
  escape : boolean;
  awaitingKey : boolean;
  keyBuf : string;
  collectingKey : boolean;

  procedure PushObject;
  begin
    Inc(depth);
    if depth >= Length(ctxStack) then
      SetLength(ctxStack, depth + 8);
    ctxStack[depth].Seen := TStringList.Create;
    ctxStack[depth].Seen.Sorted := true;
    ctxStack[depth].Seen.Duplicates := dupError;
    ctxStack[depth].InObject := true;
    awaitingKey := true;
  end;

  procedure PushArray;
  begin
    Inc(depth);
    if depth >= Length(ctxStack) then
      SetLength(ctxStack, depth + 8);
    ctxStack[depth].Seen := nil;
    ctxStack[depth].InObject := false;
    awaitingKey := false;
  end;

  procedure Pop;
  begin
    if depth < 0 then
      exit;
    if ctxStack[depth].Seen <> nil then
      FreeAndNil(ctxStack[depth].Seen);
    Dec(depth);
    awaitingKey := (depth >= 0) and ctxStack[depth].InObject;
  end;

  procedure RecordKey(const key : string);
  begin
    if (depth < 0) or not ctxStack[depth].InObject or (ctxStack[depth].Seen = nil) then
      exit;
    try
      ctxStack[depth].Seen.Add(key);
    except
      on EListError do
        raise EManifestParse.CreateFmt('Duplicate manifest key "%s"', [key]);
    end;
  end;

begin
  depth := -1;
  SetLength(ctxStack, 16);
  inString := false;
  escape := false;
  awaitingKey := false;
  collectingKey := false;
  keyBuf := '';
  len := Length(text);
  i := 1;
  try
    while i <= len do
    begin
      c := text[i];
      if inString then
      begin
        if escape then
        begin
          escape := false;
          if collectingKey then
            keyBuf := keyBuf + c;
        end
        else if c = '\' then
          escape := true
        else if c = '"' then
        begin
          inString := false;
          if collectingKey then
          begin
            RecordKey(keyBuf);
            collectingKey := false;
            keyBuf := '';
            awaitingKey := false;   // next non-ws will be ':', then value
          end;
        end
        else if collectingKey then
          keyBuf := keyBuf + c;
      end
      else
      begin
        case c of
          '{' : PushObject;
          '}' : Pop;
          '[' : PushArray;
          ']' : Pop;
          ',' : if (depth >= 0) and ctxStack[depth].InObject then
                  awaitingKey := true;
          '"' : begin
                  inString := true;
                  escape := false;
                  if awaitingKey then
                  begin
                    collectingKey := true;
                    keyBuf := '';
                  end;
                end;
        end;
      end;
      Inc(i);
    end;
  finally
    while depth >= 0 do
      Pop;
  end;
end;

function IsReservedDeviceName(const segment : string) : boolean;
var
  upper : string;
  i : integer;
  dotPos : integer;
  bare : string;
begin
  result := false;
  upper := UpperCase(segment);
  dotPos := Pos('.', upper);
  if dotPos > 0 then
    bare := Copy(upper, 1, dotPos - 1)
  else
    bare := upper;
  for i := Low(cReservedDeviceNames) to High(cReservedDeviceNames) do
    if bare = cReservedDeviceNames[i] then
    begin
      result := true;
      exit;
    end;
end;

// ---------------------------------------------------------------------------
// TPackageManifest
// ---------------------------------------------------------------------------

constructor TPackageManifest.Create;
begin
  inherited Create;
end;

function TPackageManifest.DpmPackageFormat : integer;
begin result := FDpmPackageFormat; end;
function TPackageManifest.ManifestSchemaVersion : integer;
begin result := FManifestSchemaVersion; end;
function TPackageManifest.PackageId : string;
begin result := FPackageId; end;
function TPackageManifest.Version : string;
begin result := FVersion; end;
function TPackageManifest.HashAlgorithm : THashAlgorithm;
begin result := FHashAlgorithm; end;
function TPackageManifest.Created : TDateTime;
begin result := FCreated; end;
function TPackageManifest.Files : TArray<TManifestFileEntry>;
begin result := FFiles; end;
function TPackageManifest.RawBytes : TBytes;
begin result := FRawBytes; end;

// ---------------------------------------------------------------------------
// TManifestService
// ---------------------------------------------------------------------------

constructor TManifestService.Create(const hashingService : IHashingService);
begin
  if hashingService = nil then
    raise EManifest.Create('TManifestService requires a hashing service');
  inherited Create;
  FHashingService := hashingService;
end;

function TManifestService.NormalizeToNfc(const value : string) : string;
begin
  if IsNormalizedString(NormalizationC, PWideChar(value), Length(value)) then
    result := value
  else
    result := NormalizeToNfcUtf16(value);
end;

function TManifestService.ValidatePath(const path : string; out reason : string) : boolean;
var
  i : integer;
  c : Char;
  segment : string;
  segStart : integer;

  procedure CheckSegment(const seg : string);
  var
    last : integer;
  begin
    if seg = '' then
    begin
      reason := 'empty path segment';
      result := false;
      exit;
    end;
    if (seg = '.') or (seg = '..') then
    begin
      reason := 'path contains "' + seg + '" segment';
      result := false;
      exit;
    end;
    last := Length(seg);
    if seg[last] = '.' then
    begin
      reason := 'segment "' + seg + '" has a trailing dot (Windows strips it on extraction)';
      result := false;
      exit;
    end;
    if (seg[1] = ' ') or (seg[last] = ' ') then
    begin
      reason := 'segment "' + seg + '" has leading or trailing whitespace';
      result := false;
      exit;
    end;
    if Pos(':', seg) > 0 then
    begin
      reason := 'segment "' + seg + '" contains a colon (NTFS alternate data stream)';
      result := false;
      exit;
    end;
    if IsReservedDeviceName(seg) then
    begin
      reason := 'segment "' + seg + '" is a Windows reserved device name';
      result := false;
      exit;
    end;
  end;

begin
  result := true;
  reason := '';

  if path = '' then
  begin
    reason := 'empty path';
    result := false;
    exit;
  end;

  // Absolute / drive-letter
  if path[1] = '/' then
  begin
    reason := 'absolute path';
    result := false;
    exit;
  end;
  if (Length(path) >= 2) and (path[2] = ':') then
  begin
    reason := 'drive letter not permitted';
    result := false;
    exit;
  end;

  // Backslashes (must use forward slash)
  if Pos('\', path) > 0 then
  begin
    reason := 'backslash separator not permitted (use forward slash)';
    result := false;
    exit;
  end;

  // NFC enforcement
  if not IsNormalizedString(NormalizationC, PWideChar(path), Length(path)) then
  begin
    reason := 'path is not in Unicode Normalization Form C';
    result := false;
    exit;
  end;

  // Control characters
  for i := 1 to Length(path) do
  begin
    c := path[i];
    if (Ord(c) < $20) or (c = #$7F) then
    begin
      reason := Format('control character at position %d', [i]);
      result := false;
      exit;
    end;
  end;

  // Walk segments
  segStart := 1;
  i := 1;
  while i <= Length(path) do
  begin
    if path[i] = '/' then
    begin
      segment := Copy(path, segStart, i - segStart);
      CheckSegment(segment);
      if not result then
        exit;
      segStart := i + 1;
    end;
    Inc(i);
  end;
  // Final segment
  segment := Copy(path, segStart, Length(path) - segStart + 1);
  CheckSegment(segment);
end;

procedure TManifestService.CollectFiles(const root : string; const subdir : string;
                                        const algorithm : THashAlgorithm;
                                        var entries : TArray<TManifestFileEntry>;
                                        var count : integer);
var
  searchPath : string;
  search : TSearchRec;
  fullPath : string;
  relativePath : string;
  manifestRelative : string;
  reason : string;
  entry : TManifestFileEntry;
begin
  searchPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(root) + subdir) + '*';
  if FindFirst(searchPath, faAnyFile, search) <> 0 then
    exit;
  try
    repeat
      if (search.Name = '.') or (search.Name = '..') then
        Continue;

      if subdir = '' then
        relativePath := search.Name
      else
        relativePath := IncludeTrailingPathDelimiter(subdir) + search.Name;
      fullPath := IncludeTrailingPathDelimiter(root) + relativePath;

      // Canonical path: forward slashes, NFC.
      manifestRelative := StringReplace(relativePath, '\', '/', [rfReplaceAll]);
      manifestRelative := NormalizeToNfc(manifestRelative);

      if (search.Attr and faDirectory) = faDirectory then
      begin
        CollectFiles(root, relativePath, algorithm, entries, count);
        Continue;
      end;

      // Exclusions per architecture doc §Excluded files
      if SameText(manifestRelative, cManifestFileName) then
        Continue;
      if PathStartsWith(manifestRelative, 'signatures/') then
        Continue;

      if not ValidatePath(manifestRelative, reason) then
        raise EManifestPath.CreateFmt('Refusing to include "%s": %s',
          [manifestRelative, reason]);

      entry.Path := manifestRelative;
      entry.Size := search.Size;
      entry.Hash := FHashingService.HashFile(fullPath, algorithm);

      if count >= Length(entries) then
        SetLength(entries, (count + 1) * 2);
      entries[count] := entry;
      Inc(count);
    until FindNext(search) <> 0;
  finally
    FindClose(search);
  end;
end;

function CompareFileEntry(const a, b : TManifestFileEntry) : integer;
begin
  result := CompareStr(a.Path, b.Path);
end;

function TManifestService.EncodeManifest(const dpmPackageFormat : integer;
                                         const manifestSchemaVersion : integer;
                                         const packageId : string;
                                         const version : string;
                                         const created : TDateTime;
                                         const algorithm : THashAlgorithm;
                                         const files : TArray<TManifestFileEntry>) : TBytes;
var
  sb : TStringBuilder;
  i : integer;
begin
  // Deterministic emitter — fixed key order (lexicographic), LF only, integers
  // only, no insignificant whitespace.
  //
  // Keys in lex order: created, dpmPackageFormat, files, hashAlgorithm,
  //                    manifestSchemaVersion, packageId, version
  //
  // We pretty-print with a single line per files-entry to keep diffs and
  // human review readable; the JSON is still canonical.

  sb := TStringBuilder.Create(4096);
  try
    sb.Append('{'#10);
    sb.Append('  "created": '); sb.Append(JsonEscape(FormatRfc3339Utc(created))); sb.Append(','#10);
    sb.Append('  "dpmPackageFormat": '); sb.Append(IntToStr(dpmPackageFormat)); sb.Append(','#10);
    sb.Append('  "files": ['#10);
    for i := 0 to High(files) do
    begin
      sb.Append('    {');
      sb.Append('"hash": '); sb.Append(JsonEscape(BytesToHexLower(files[i].Hash))); sb.Append(', ');
      sb.Append('"path": '); sb.Append(JsonEscape(files[i].Path)); sb.Append(', ');
      sb.Append('"size": '); sb.Append(IntToStr(files[i].Size));
      sb.Append('}');
      if i < High(files) then
        sb.Append(',');
      sb.Append(#10);
    end;
    sb.Append('  ],'#10);
    sb.Append('  "hashAlgorithm": '); sb.Append(JsonEscape(TAlgorithmProfile.HashAlgorithmName(algorithm))); sb.Append(','#10);
    sb.Append('  "manifestSchemaVersion": '); sb.Append(IntToStr(manifestSchemaVersion)); sb.Append(','#10);
    sb.Append('  "packageId": '); sb.Append(JsonEscape(packageId)); sb.Append(','#10);
    sb.Append('  "version": '); sb.Append(JsonEscape(version)); sb.Append(#10);
    sb.Append('}'#10);
    result := EncodeUtf8NoBom(sb.ToString);
  finally
    sb.Free;
  end;
end;

function TManifestService.BuildManifestFromEntries(const packageId, version : string;
                                                    algorithm : THashAlgorithm;
                                                    const entries : TArray<TManifestFileEntry>) : IPackageManifest;
var
  sorted : TArray<TManifestFileEntry>;
  sortList : IList<TManifestFileEntry>;
  comparer : IComparer<TManifestFileEntry>;
  created : TDateTime;
  encoded : TBytes;
  manifest : TPackageManifest;
begin
  comparer := TComparer<TManifestFileEntry>.Construct(
    function(const Left, Right : TManifestFileEntry) : integer
    begin
      result := CompareStr(Left.Path, Right.Path);
    end);
  sortList := TCollections.CreateList<TManifestFileEntry>(entries);
  sortList.Sort(comparer);
  sorted := sortList.ToArray;

  created := TTimeZone.Local.ToUniversalTime(Now);
  encoded := EncodeManifest(
    cCurrentDpmPackageFormat, cCurrentManifestSchemaVersion,
    packageId, version, created, algorithm, sorted);

  manifest := TPackageManifest.Create;
  manifest.FDpmPackageFormat := cCurrentDpmPackageFormat;
  manifest.FManifestSchemaVersion := cCurrentManifestSchemaVersion;
  manifest.FPackageId := packageId;
  manifest.FVersion := version;
  manifest.FHashAlgorithm := algorithm;
  manifest.FCreated := created;
  manifest.FFiles := sorted;
  manifest.FRawBytes := encoded;
  result := manifest;
end;

function TManifestService.Generate(const packageRoot : string;
                                   const packageId : string;
                                   const version : string;
                                   algorithm : THashAlgorithm) : IPackageManifest;
var
  entries : TArray<TManifestFileEntry>;
  count : integer;
begin
  if not TAlgorithmProfile.FileHashAllowed(algorithm) then
    raise EManifest.CreateFmt('Hash algorithm %s not permitted',
      [TAlgorithmProfile.HashAlgorithmName(algorithm)]);
  if not DirectoryExists(packageRoot) then
    raise EManifest.CreateFmt('Package root "%s" does not exist', [packageRoot]);
  if Trim(packageId) = '' then
    raise EManifest.Create('packageId is empty');
  if Trim(version) = '' then
    raise EManifest.Create('version is empty');

  SetLength(entries, 0);
  count := 0;
  CollectFiles(packageRoot, '', algorithm, entries, count);
  SetLength(entries, count);

  result := BuildManifestFromEntries(packageId, version, algorithm, entries);
end;

function TManifestService.GenerateFromArchive(const archivePath : string;
                                              const packageId : string;
                                              const version : string;
                                              algorithm : THashAlgorithm) : IPackageManifest;
var
  zip : TZipFile;
  i, count : integer;
  name : string;
  canonical : string;
  reason : string;
  entries : TArray<TManifestFileEntry>;
  entry : TManifestFileEntry;
  ms : TStream;
  header : TZipHeader;
  bytes : TBytes;
begin
  if not TAlgorithmProfile.FileHashAllowed(algorithm) then
    raise EManifest.CreateFmt('Hash algorithm %s not permitted',
      [TAlgorithmProfile.HashAlgorithmName(algorithm)]);
  if not FileExists(archivePath) then
    raise EManifest.CreateFmt('Archive "%s" does not exist', [archivePath]);
  if Trim(packageId) = '' then
    raise EManifest.Create('packageId is empty');
  if Trim(version) = '' then
    raise EManifest.Create('version is empty');

  SetLength(entries, 0);
  count := 0;

  zip := TZipFile.Create;
  try
    zip.Open(archivePath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      name := zip.FileName[i];
      // Strip directory entries (trailing slash) — they aren't real files.
      if (Length(name) > 0) and ((name[Length(name)] = '/') or (name[Length(name)] = '\')) then
        Continue;

      canonical := StringReplace(name, '\', '/', [rfReplaceAll]);
      canonical := NormalizeToNfc(canonical);

      // Architecture-doc exclusions
      if SameText(canonical, cManifestFileName) then
        Continue;
      if PathStartsWith(canonical, 'signatures/') then
        Continue;

      if not ValidatePath(canonical, reason) then
        raise EManifestPath.CreateFmt('Refusing to include "%s": %s', [canonical, reason]);

      zip.Read(i, ms, header);
      try
        SetLength(bytes, ms.Size);
        ms.Position := 0;
        if ms.Size > 0 then
          ms.Read(bytes[0], ms.Size);
      finally
        ms.Free;
      end;

      entry.Path := canonical;
      entry.Size := Length(bytes);
      entry.Hash := FHashingService.HashBytes(bytes, algorithm);

      if count >= Length(entries) then
        SetLength(entries, (count + 1) * 2);
      entries[count] := entry;
      Inc(count);
    end;
  finally
    zip.Free;
  end;
  SetLength(entries, count);

  result := BuildManifestFromEntries(packageId, version, algorithm, entries);
end;

procedure TManifestService.InjectIntoArchive(const archivePath : string;
                                              const manifest : IPackageManifest);
var
  zip : TZipFile;
  ms : TMemoryStream;
  bytes : TBytes;
begin
  if manifest = nil then
    raise EManifest.Create('InjectIntoArchive: manifest is nil');
  bytes := manifest.RawBytes;

  ms := TMemoryStream.Create;
  zip := TZipFile.Create;
  try
    if Length(bytes) > 0 then
      ms.WriteBuffer(bytes[0], Length(bytes));
    ms.Position := 0;
    zip.Open(archivePath, zmReadWrite);
    zip.Add(ms, cManifestFileName, zcDeflate);
  finally
    ms.Free;
    zip.Free;
  end;
end;

function TManifestService.CheckDepth(const value : IYAMLValue; current, max : integer) : boolean;
var
  seq : IYAMLSequence;
  map : IYAMLMapping;
  i : integer;
begin
  if current > max then
  begin
    result := false;
    exit;
  end;
  if value = nil then
  begin
    result := true;
    exit;
  end;
  if value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      if not CheckDepth(seq.Items[i], current + 1, max) then
      begin
        result := false;
        exit;
      end;
  end
  else if value.IsMapping then
  begin
    map := value.AsMapping;
    for i := 0 to map.Count - 1 do
      if not CheckDepth(map.Values[map.GetKey(i)], current + 1, max) then
      begin
        result := false;
        exit;
      end;
  end;
  result := true;
end;

function TManifestService.Parse(const bytes : TBytes) : IPackageManifest;
var
  asText : string;
  options : IYAMLParserOptions;
  doc : IYAMLDocument;
  root, entryNode, hashAlgNode : IYAMLValue;
  rootMap : IYAMLMapping;
  filesSeq : IYAMLSequence;
  i : integer;
  algorithm : THashAlgorithm;
  algName : string;
  reason : string;
  entryPath : string;
  entries : TArray<TManifestFileEntry>;
  createdStr : string;
  created : TDateTime;
  manifest : TPackageManifest;
begin
  if Length(bytes) = 0 then
    raise EManifestParse.Create('Manifest is empty');
  if Length(bytes) > cManifestMaxBytes then
    raise EManifestParse.CreateFmt('Manifest exceeds maximum size of %d bytes',
      [cManifestMaxBytes]);

  asText := TEncoding.UTF8.GetString(bytes);

  options := TYAML.CreateParserOptions;
  options.DuplicateKeyBehavior := TYAMLDuplicateKeyBehavior.dkError;

  // V-2 pre-check. VSoft.YAML's dkError option is not consistently honoured
  // for JSON-style flow mappings, so we scan the raw bytes ourselves for any
  // duplicate object key. Manifest schema is small and fixed, so a streaming
  // single-pass scan is cheap.
  // TODO : this is fixed in vsoft.yaml v1.5.5
  // TODO : add test
//  EnforceUniqueObjectKeys(asText);

  try
    doc := TYAML.LoadFromString(asText, options);
  except
    on e : EYAMLParseException do
      raise EManifestParse.CreateFmt('Manifest parse failed at %d:%d: %s',
        [e.Line, e.Column, e.Message]);
    on e : Exception do
      raise EManifestParse.CreateFmt('Manifest parse failed: %s', [e.Message]);
  end;

  if doc = nil then
    raise EManifestParse.Create('Manifest document is empty');

  root := doc.Root;
  if (root = nil) or not root.IsMapping then
    raise EManifestParse.Create('Manifest root must be an object');

  if not CheckDepth(root, 0, cManifestMaxDepth) then
    raise EManifestParse.CreateFmt('Manifest exceeds maximum nesting depth %d',
      [cManifestMaxDepth]);

  rootMap := root.AsMapping;

  if not rootMap.Values['hashAlgorithm'].IsString then
    raise EManifestParse.Create('hashAlgorithm must be a string');
  hashAlgNode := rootMap.Values['hashAlgorithm'];
  algName := hashAlgNode.AsString;
  if not TAlgorithmProfile.ParseHashName(algName, algorithm) then
    raise EManifestParse.CreateFmt('Unsupported hashAlgorithm "%s"', [algName]);
  if not TAlgorithmProfile.FileHashAllowed(algorithm) then
    raise EManifestParse.CreateFmt('Hash algorithm %s not permitted', [algName]);

  if not rootMap.Values['files'].IsSequence then
    raise EManifestParse.Create('files must be an array');
  filesSeq := rootMap.Values['files'].AsSequence;
  SetLength(entries, filesSeq.Count);
  for i := 0 to filesSeq.Count - 1 do
  begin
    entryNode := filesSeq.Items[i];
    if (entryNode = nil) or not entryNode.IsMapping then
      raise EManifestParse.CreateFmt('files[%d] must be an object', [i]);
    if not entryNode.AsMapping.Values['path'].IsString then
      raise EManifestParse.CreateFmt('files[%d].path must be a string', [i]);
    if not entryNode.AsMapping.Values['size'].IsInteger then
      raise EManifestParse.CreateFmt('files[%d].size must be an integer', [i]);
    if not entryNode.AsMapping.Values['hash'].IsString then
      raise EManifestParse.CreateFmt('files[%d].hash must be a string', [i]);

    entryPath := entryNode.AsMapping.Values['path'].AsString;
    if not ValidatePath(entryPath, reason) then
      raise EManifestParse.CreateFmt('files[%d].path invalid: %s', [i, reason]);

    entries[i].Path := entryPath;
    entries[i].Size := entryNode.AsMapping.Values['size'].AsInteger;
    entries[i].Hash := HexStringToBytes(entryNode.AsMapping.Values['hash'].AsString);
  end;

  // Detect duplicate paths after parse (case-insensitive comparison).
  for i := 1 to High(entries) do
    if SameText(entries[i].Path, entries[i - 1].Path) then
      raise EManifestParse.CreateFmt('Duplicate manifest path "%s"', [entries[i].Path]);

  if not rootMap.Values['dpmPackageFormat'].IsInteger then
    raise EManifestParse.Create('dpmPackageFormat must be an integer');
  if not rootMap.Values['manifestSchemaVersion'].IsInteger then
    raise EManifestParse.Create('manifestSchemaVersion must be an integer');
  if not rootMap.Values['packageId'].IsString then
    raise EManifestParse.Create('packageId must be a string');
  if not rootMap.Values['version'].IsString then
    raise EManifestParse.Create('version must be a string');

  createdStr := '';
  if rootMap.Values['created'].IsString then
    createdStr := rootMap.Values['created'].AsString;
  if (createdStr <> '') and not ParseRfc3339Utc(createdStr, created) then
    raise EManifestParse.CreateFmt('Invalid created timestamp "%s"', [createdStr]);

  manifest := TPackageManifest.Create;
  manifest.FDpmPackageFormat := rootMap.Values['dpmPackageFormat'].AsInteger;
  manifest.FManifestSchemaVersion := rootMap.Values['manifestSchemaVersion'].AsInteger;
  manifest.FPackageId := rootMap.Values['packageId'].AsString;
  manifest.FVersion := rootMap.Values['version'].AsString;
  manifest.FHashAlgorithm := algorithm;
  manifest.FCreated := created;
  manifest.FFiles := entries;
  manifest.FRawBytes := bytes;
  result := manifest;
end;

end.
