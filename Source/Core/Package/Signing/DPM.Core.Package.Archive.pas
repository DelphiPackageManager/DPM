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

unit DPM.Core.Package.Archive;

// Archive-format rule enforcement (V-9 through V-13). Walks every entry in
// a .dpkg before extraction and rejects:
//
//   V-9  duplicate entry names under NFC + case-insensitive comparison
//   V-10 names not in NFC (UTF-8 LEF bit checked when System.Zip exposes it)
//   V-11 non-regular-file/directory entries (symlinks, devices)
//   V-12 NTFS ADS (`:` in path) or Windows reserved device names
//   V-13 compression methods other than Store or Deflate; encrypted entries
//
// and, because the validated names must be the names that get extracted:
//
//   - a local header whose name differs from its central directory entry
//   - the Info-ZIP Unicode Path extra field (0x7075). From 10.1 Berlin the RTL's
//     TZipFile.Extract takes the file name from it rather than from the validated
//     header name, and only checks it against a CRC of the (attacker controlled)
//     local name - so it can rename an entry to ..\..\anything at extract time.
//   - files under signatures/ other than author-N.p7s / repository-N.p7s. That
//     folder is excluded from the manifest, so anything else there is unhashed.
//   - the .dpm-git-commit marker, which only DPM writes for git registry installs.
//
// TPackageArchiveExtractor is the extraction counterpart : it never asks the RTL to
// pick a file name, so a name can't escape the target folder even if validation is
// skipped.

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  Spring.Collections,
  DPM.Core.Package.Manifest.Interfaces;

type
  TArchiveValidationResult = record
    Ok      : boolean;
    Reason  : string;
    Entry   : string;
  end;

  IArchiveValidator = interface
    ['{52E1A28A-1C97-4E70-A4B0-37BF8B2A6E0D}']
    function Validate(const archivePath : string) : TArchiveValidationResult;
  end;

  TArchiveValidator = class(TInterfacedObject, IArchiveValidator)
  private
    FManifestService : IManifestService;
  protected
    function Validate(const archivePath : string) : TArchiveValidationResult;
  public
    constructor Create(const manifestService : IManifestService);
  end;

  EArchiveExtract = class(Exception);

  TPackageArchiveExtractor = class
  private
    class procedure DoExtract(const archivePath : string; const folder : string; const onlyEntries : ISet<string>);
  public
    /// <summary>
    /// Extracts every file entry of archivePath into folder as a regular file, using the
    /// central directory name only. Directory entries and the signatures/ folder are
    /// skipped (signatures are read from the .dpkg, never from the extraction). Raises
    /// EArchiveExtract if a name would resolve outside folder.
    /// </summary>
    class procedure Extract(const archivePath : string; const folder : string);
    /// <summary>
    /// As Extract, but only the named entries (archive paths, case-insensitive) - restores
    /// individual files without disturbing the rest of the folder.
    /// </summary>
    class procedure ExtractFiles(const archivePath : string; const folder : string; const entryNames : array of string);
  end;

/// True for the only names allowed under signatures/ - author-N.p7s and repository-N.p7s.
function IsCanonicalSignatureEntry(const name : string) : boolean;

implementation

uses
  System.StrUtils,
  System.Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Utils.Path;

{$IF CompilerVersion >= 24.0 }
  {$LEGACYIFEND ON}
{$IFEND}

const
  // ZIP local-file-header general-purpose bit-flag bits we test
  cZipFlagEncrypted = $0001;
  cZipFlagUtf8Name  = $0800;

  // ZIP made-by-version: host high byte
  cZipHostMsDos = 0;
  cZipHostUnix = 3;

  // Unix file-mode S_IFMT bits (high half of ExternalAttributes for Unix host)
  cSIfReg = $8000;
  cSIfDir = $4000;
  cSIfLnk = $A000;

  // Link attributes in the low half of ExternalAttributes for an MS-DOS host. From 11
  // Alexandria the RTL's TZipFile.Extract creates a symbolic link for an entry with
  // FILE_ATTRIBUTE_DEVICE - TFile maps that bit to faSymLink, and only the low byte is
  // read. FILE_ATTRIBUTE_REPARSE_POINT is what a real link would carry. No file in a
  // package has either.
  cDosAttrDevice = $0040;
  cDosAttrReparsePoint = $0400;

  // Info-ZIP Unicode Path extra field
  cExtraFieldUnicodePath = $7075;

  cSignatureFolderPrefix = 'signatures/';

function IsCanonicalSignatureEntry(const name : string) : boolean;

  function HasCounter(const prefix : string) : boolean;
  var
    i : integer;
    counter : string;
  begin
    result := false;
    if not (StartsStr(prefix, name) and EndsStr('.p7s', name)) then
      exit;
    counter := Copy(name, Length(prefix) + 1, Length(name) - Length(prefix) - Length('.p7s'));
    if (counter = '') or (counter[1] = '0') then
      exit;
    for i := 1 to Length(counter) do
      if not CharInSet(counter[i], ['0'..'9']) then
        exit;
    result := true;
  end;

begin
  result := HasCounter(cSignatureFolderPrefix + 'author-') or HasCounter(cSignatureFolderPrefix + 'repository-');
end;

// True when the extra field block holds the given tag. Each field is
// tag(2) + size(2) + data(size), little endian.
function HasExtraField(const extra : TBytes; const tag : Word) : boolean;
var
  i : integer;
  fieldTag : Word;
  fieldSize : Word;
begin
  result := false;
  i := 0;
  while i + 4 <= Length(extra) do
  begin
    fieldTag := extra[i] or (extra[i + 1] shl 8);
    fieldSize := extra[i + 2] or (extra[i + 3] shl 8);
    if fieldTag = tag then
      exit(true);
    Inc(i, 4 + fieldSize);
  end;
end;

//XE2's TZipHeader.FileName is a RawByteString, XE3 and later a TBytes - compare the raw bytes.
{$IF CompilerVersion >= 24.0 }
function SameHeaderName(const a, b : TBytes) : boolean;
var
  i : integer;
begin
  result := Length(a) = Length(b);
  if not result then
    exit;
  for i := 0 to Length(a) - 1 do
    if a[i] <> b[i] then
      exit(false);
end;
{$ELSE}
function SameHeaderName(const a, b : RawByteString) : boolean;
begin
  result := a = b;
end;
{$IFEND}

// True when every character is in the 7-bit ASCII range. A ZIP entry name made
// of pure ASCII bytes decodes identically under UTF-8 and any OEM/ANSI codepage,
// so it is unambiguous even when the language-encoding flag (bit 11) is not set.
// (Delphi's TZipFile decodes a non-UTF-8 name via the system codepage, but bytes
// < $80 always map to the same char < $80, so an all-ASCII decoded string proves
// the original bytes were pure ASCII.)
function IsAsciiName(const value : string) : boolean;
var
  i : integer;
begin
  for i := 1 to Length(value) do
    if Ord(value[i]) > 127 then
      exit(false);
  result := true;
end;

{ TArchiveValidator }

constructor TArchiveValidator.Create(const manifestService : IManifestService);
begin
  if manifestService = nil then
    raise EManifest.Create('TArchiveValidator requires a manifest service');
  inherited Create;
  FManifestService := manifestService;
end;

function TArchiveValidator.Validate(const archivePath : string) : TArchiveValidationResult;
var
  zip : TZipFile;
  i : integer;
  name : string;
  candidatePath : string;
  normalised : string;
  lowered : string;
  seen : IDictionary<string, integer>;
  header : TZipHeader;
  localHeader : TZipHeader;
  entryStream : TStream;
  reason : string;
  mode : Word;
begin
  result.Ok := false;
  result.Reason := '';
  result.Entry := '';

  zip := TZipFile.Create;
  seen := TCollections.CreateDictionary<string, integer>;
  try
    try
      zip.Open(archivePath, zmRead);
    except
      on e : Exception do
      begin
        result.Reason := 'Archive open failed: ' + e.Message;
        exit;
      end;
    end;

    for i := 0 to zip.FileCount - 1 do
    begin
      name := zip.FileName[i];
      result.Entry := name;
      header := zip.FileInfo[i];

      // V-10: the ZIP language-encoding flag (general-purpose bit 11) SHOULD be
      // set so the name's codepage is unambiguous, but per the package spec it is
      // only a hint - the byte-level UTF-8 + NFC check below is authoritative.
      // This matches the gallery verifier (dpm-verifier.ts decodeAndCheckName),
      // which treats bit 11 as a SHOULD. A name written without the flag is still
      // unambiguous when it is pure ASCII (identical under UTF-8 and any codepage),
      // so we accept those - third-party zip tools (7-Zip, .NET, Compress-Archive)
      // routinely omit the flag for ASCII names. Only a NON-ASCII name that fails
      // to declare UTF-8 is genuinely ambiguous, so reject just that case.
      if ((header.Flag and cZipFlagUtf8Name) = 0) and (not IsAsciiName(name)) then
      begin
        result.Reason := 'entry name does not declare UTF-8 (general-purpose bit 11 unset) and contains non-ASCII characters';
        exit;
      end;

      // Validate the *original* name so M-8's backslash check fires here.
      // Strip a trailing slash before validation (directory marker — the
      // path-safety rule treats trailing slashes on otherwise valid names
      // as a no-op).
      candidatePath := name;
      if (candidatePath <> '') and (candidatePath[Length(candidatePath)] = '/') then
        candidatePath := Copy(candidatePath, 1, Length(candidatePath) - 1);

      if not FManifestService.ValidatePath(candidatePath, reason) then
      begin
        result.Reason := 'invalid path: ' + reason;
        exit;
      end;

      normalised := FManifestService.NormalizeToNfc(name);
      if normalised <> name then
      begin
        result.Reason := 'entry name is not in Unicode Normalization Form C';
        exit;
      end;

      // V-9: case-insensitive duplicate check on NFC name (full original name
      // including any trailing slash — Windows / NTFS treat the two
      // representations as the same file).
      lowered := LowerCase(normalised);
      if seen.ContainsKey(lowered) then
      begin
        result.Reason := Format('duplicate entry name "%s" (case-insensitive match to "%s")',
          [name, zip.FileName[seen[lowered]]]);
        exit;
      end;
      seen[lowered] := i;

      // V-13: reject encryption + non-allowlisted compression methods
      if (header.Flag and cZipFlagEncrypted) <> 0 then
      begin
        result.Reason := 'encrypted entries are not permitted';
        exit;
      end;
      if (header.CompressionMethod <> 0) and (header.CompressionMethod <> 8) then
      begin
        result.Reason := Format('compression method %d not in {Store=0, Deflate=8}',
          [header.CompressionMethod]);
        exit;
      end;

      // The name validated above has to be the name that is extracted. The RTL
      // renames an entry from a Unicode Path extra field, and the local header
      // carries its own copy of the name - reject both ways of disagreeing.
      if HasExtraField(header.ExtraField, cExtraFieldUnicodePath) then
      begin
        result.Reason := 'entry carries an Info-ZIP Unicode Path extra field (0x7075)';
        exit;
      end;
      try
        zip.Read(i, entryStream, localHeader);
        entryStream.Free;
      except
        on e : Exception do
        begin
          result.Reason := 'unable to read the local header : ' + e.Message;
          exit;
        end;
      end;
      if not SameHeaderName(localHeader.FileName, header.FileName) then
      begin
        result.Reason := 'local header name differs from the central directory name';
        exit;
      end;
      if HasExtraField(localHeader.ExtraField, cExtraFieldUnicodePath) then
      begin
        result.Reason := 'local header carries an Info-ZIP Unicode Path extra field (0x7075)';
        exit;
      end;

      // signatures/ is excluded from the manifest, so only the signature blobs
      // themselves may live there - anything else would be extracted unhashed.
      if StartsText(cSignatureFolderPrefix, name) and (not SameText(name, cSignatureFolderPrefix)) and
         (not IsCanonicalSignatureEntry(name)) then
      begin
        result.Reason := 'only author-N.p7s and repository-N.p7s may appear under signatures/';
        exit;
      end;

      // The git registry marker turns off the cache-hit re-checks; only DPM writes it.
      if SameText(name, cGitPackageMarkerFile) then
      begin
        result.Reason := Format('"%s" is reserved for git registry installs', [cGitPackageMarkerFile]);
        exit;
      end;

      if (((header.MadeByVersion shr 8) and $FF) = cZipHostMsDos) and
         ((header.ExternalAttributes and (cDosAttrDevice or cDosAttrReparsePoint)) <> 0) then
      begin
        result.Reason := 'device or reparse point (symbolic link) entries are not permitted';
        exit;
      end;

      // V-11: when the archive was produced by a Unix host (so external
      // attributes carry Unix file mode), reject non-regular/non-directory
      // entries. Windows-host archives can't represent symlinks via local
      // header attrs, but the path-safety rule above also catches them
      // indirectly via `:` and reserved-name rejection.
      if ((header.MadeByVersion shr 8) and $FF) = cZipHostUnix then
      begin
        mode := (header.ExternalAttributes shr 16) and $F000;
        if mode = cSIfLnk then
        begin
          result.Reason := 'symbolic link entries are not permitted';
          exit;
        end;
        if (mode <> 0) and (mode <> cSIfReg) and (mode <> cSIfDir) then
        begin
          result.Reason := Format('non-regular-file entry type 0x%.4x', [mode]);
          exit;
        end;
      end;
    end;

    result.Entry := '';
    result.Ok := true;
  finally
    zip.Free;
  end;
end;

{ TPackageArchiveExtractor }

class procedure TPackageArchiveExtractor.Extract(const archivePath : string; const folder : string);
begin
  DoExtract(archivePath, folder, nil);
end;

class procedure TPackageArchiveExtractor.ExtractFiles(const archivePath : string; const folder : string; const entryNames : array of string);
var
  names : ISet<string>;
  i : integer;
begin
  names := TCollections.CreateSet<string>(TIStringComparer.Ordinal);
  for i := Low(entryNames) to High(entryNames) do
    names.Add(entryNames[i]);
  DoExtract(archivePath, folder, names);
end;

class procedure TPackageArchiveExtractor.DoExtract(const archivePath : string; const folder : string; const onlyEntries : ISet<string>);
var
  zip : TZipFile;
  i : integer;
  name : string;
  targetFile : string;
  bytes : TBytes;
  fileStream : TFileStream;
begin
  zip := TZipFile.Create;
  try
    zip.Open(archivePath, zmRead);
    for i := 0 to zip.FileCount - 1 do
    begin
      name := zip.FileName[i];
      //directory entries - the folders get created for the files they contain.
      if (name = '') or (name[Length(name)] = '/') then
        continue;
      if StartsText(cSignatureFolderPrefix, name) then
        continue;
      if (onlyEntries <> nil) and (not onlyEntries.Contains(name)) then
        continue;

      //the target comes from the central directory name and nothing else - never the RTL's
      //Extract, which honours a Unicode Path extra field (and creates symbolic links).
      if not TPathUtils.TryResolveInFolder(folder, name, targetFile) then
        raise EArchiveExtract.CreateFmt('The archive entry [%s] would extract outside [%s].', [name, folder]);

      ForceDirectories(ExtractFilePath(targetFile));
      //the TBytes overload - the stream overload is unusable for stored entries on 10.2 (see
      //TPackageSigningService.ReadSignatureBlobs).
      zip.Read(i, bytes);
      fileStream := TFileStream.Create(targetFile, fmCreate);
      try
        if Length(bytes) > 0 then
          fileStream.WriteBuffer(bytes[0], Length(bytes));
      finally
        fileStream.Free;
      end;
      FileSetDate(targetFile, Integer(zip.FileInfo[i].ModifiedDateTime));
    end;
  finally
    zip.Free;
  end;
end;

end.
