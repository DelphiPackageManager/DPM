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

interface

uses
  System.Classes, System.SysUtils, System.Zip, System.Generics.Collections,
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

implementation

const
  // ZIP local-file-header general-purpose bit-flag bits we test
  cZipFlagEncrypted = $0001;
  cZipFlagUtf8Name  = $0800;

  // ZIP made-by-version: host high byte
  cZipHostUnix = 3;

  // Unix file-mode S_IFMT bits (high half of ExternalAttributes for Unix host)
  cSIfReg = $8000;
  cSIfDir = $4000;
  cSIfLnk = $A000;

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
  seen : TDictionary<string, integer>;
  header : TZipHeader;
  reason : string;
  mode : Word;
begin
  result.Ok := false;
  result.Reason := '';
  result.Entry := '';

  zip := TZipFile.Create;
  seen := TDictionary<string, integer>.Create;
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

      // V-10: ZIP language-encoding flag must indicate UTF-8 so the name's
      // codepage is unambiguous.
      if (header.Flag and cZipFlagUtf8Name) = 0 then
      begin
        result.Reason := 'entry name is not declared UTF-8 (general-purpose bit 11 unset)';
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
      seen.AddOrSetValue(lowered, i);

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
    seen.Free;
    zip.Free;
  end;
end;

end.
