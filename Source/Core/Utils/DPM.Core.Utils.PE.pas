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

unit DPM.Core.Utils.PE;

// Pure byte-level inspection of Windows PE binaries (.exe/.dll/.bpl). No Windows API is used so this
// unit is safe in Core (which must run in Docker Windows Server Core and not depend on VCL/GDI).
//
//  - IsPE detects whether the bytes are a real PE image (MZ + PE signature), regardless of extension.
//  - HasAuthenticodeSignature detects the *presence* of an embedded Authenticode signature by reading
//    the PE security data directory (IMAGE_DIRECTORY_ENTRY_SECURITY). It does NOT validate the
//    signature or its certificate chain - it answers "is this binary signed at all?", which is enough
//    to fail fast on an unsigned binary before an author-signed package is rejected by the gallery.
//    The two are deliberately separate so a Windows WinVerifyTrust validator can be layered on later
//    without changing call sites.

interface

uses
  System.Classes,
  System.SysUtils;

type
  TPEUtils = class
  private
    class function ReadWordAt(const stream : TStream; const pos : Int64; out value : Word) : boolean; static;
    class function ReadCardinalAt(const stream : TStream; const pos : Int64; out value : Cardinal) : boolean; static;
    class function FindPEHeaderOffset(const stream : TStream; out peOffset : Cardinal) : boolean; static;
  public
    class function IsPE(const stream : TStream) : boolean; overload; static;
    class function IsPE(const bytes : TBytes) : boolean; overload; static;
    class function IsPE(const fileName : string) : boolean; overload; static;

    class function HasAuthenticodeSignature(const stream : TStream) : boolean; overload; static;
    class function HasAuthenticodeSignature(const bytes : TBytes) : boolean; overload; static;
    class function HasAuthenticodeSignature(const fileName : string) : boolean; overload; static;
  end;

implementation

const
  cMZSignature = $5A4D;          // 'MZ' as a little-endian word at offset 0
  cPESignature = $00004550;      // 'PE'#0#0 as a little-endian cardinal at e_lfanew
  cELfanewOffset = $3C;          // offset of the 4-byte e_lfanew pointer in the DOS header
  cOptMagicPE32 = $010B;         // 32 bit optional header
  cOptMagicPE32Plus = $020B;     // 64 bit optional header
  cSecurityDirIndex = 4;         // IMAGE_DIRECTORY_ENTRY_SECURITY
  cDataDirEntrySize = 8;         // each data directory entry: VirtualAddress(4) + Size(4)
  // offsets within the optional header to NumberOfRvaAndSizes and the start of the data directory
  cPE32NumRvaOffset = 92;
  cPE32DataDirOffset = 96;
  cPE32PlusNumRvaOffset = 108;
  cPE32PlusDataDirOffset = 112;

{ TPEUtils }

class function TPEUtils.ReadWordAt(const stream : TStream; const pos : Int64; out value : Word) : boolean;
begin
  value := 0;
  result := false;
  if pos < 0 then
    exit;
  if (pos + SizeOf(Word)) > stream.Size then
    exit;
  stream.Position := pos;
  result := stream.Read(value, SizeOf(Word)) = SizeOf(Word);
end;

class function TPEUtils.ReadCardinalAt(const stream : TStream; const pos : Int64; out value : Cardinal) : boolean;
begin
  value := 0;
  result := false;
  if pos < 0 then
    exit;
  if (pos + SizeOf(Cardinal)) > stream.Size then
    exit;
  stream.Position := pos;
  result := stream.Read(value, SizeOf(Cardinal)) = SizeOf(Cardinal);
end;

class function TPEUtils.FindPEHeaderOffset(const stream : TStream; out peOffset : Cardinal) : boolean;
var
  mz : Word;
  sig : Cardinal;
begin
  peOffset := 0;
  result := false;
  if not ReadWordAt(stream, 0, mz) then
    exit;
  if mz <> cMZSignature then
    exit;
  if not ReadCardinalAt(stream, cELfanewOffset, peOffset) then
    exit;
  if not ReadCardinalAt(stream, peOffset, sig) then
    exit;
  result := sig = cPESignature;
end;

class function TPEUtils.IsPE(const stream : TStream) : boolean;
var
  peOffset : Cardinal;
begin
  result := FindPEHeaderOffset(stream, peOffset);
end;

class function TPEUtils.IsPE(const bytes : TBytes) : boolean;
var
  ms : TBytesStream;
begin
  ms := TBytesStream.Create(bytes);
  try
    result := IsPE(TStream(ms));
  finally
    ms.Free;
  end;
end;

class function TPEUtils.IsPE(const fileName : string) : boolean;
var
  fs : TFileStream;
begin
  result := false;
  if not FileExists(fileName) then
    exit;
  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    result := IsPE(TStream(fs));
  finally
    fs.Free;
  end;
end;

class function TPEUtils.HasAuthenticodeSignature(const stream : TStream) : boolean;
var
  peOffset : Cardinal;
  optHeaderStart : Int64;
  magic : Word;
  numRvaPos : Int64;
  dataDirStart : Int64;
  numRva : Cardinal;
  secDirPos : Int64;
  virtualAddress : Cardinal;
  size : Cardinal;
begin
  result := false;
  if not FindPEHeaderOffset(stream, peOffset) then
    exit;

  //optional header starts after the PE signature (4 bytes) and the COFF file header (20 bytes)
  optHeaderStart := Int64(peOffset) + 4 + 20;
  if not ReadWordAt(stream, optHeaderStart, magic) then
    exit;

  case magic of
    cOptMagicPE32 :
      begin
        numRvaPos := optHeaderStart + cPE32NumRvaOffset;
        dataDirStart := optHeaderStart + cPE32DataDirOffset;
      end;
    cOptMagicPE32Plus :
      begin
        numRvaPos := optHeaderStart + cPE32PlusNumRvaOffset;
        dataDirStart := optHeaderStart + cPE32PlusDataDirOffset;
      end;
  else
    //unknown optional header magic (e.g. a ROM image) - we can't locate the data directory, so
    //treat it as not signed.
    exit;
  end;

  if not ReadCardinalAt(stream, numRvaPos, numRva) then
    exit;
  //the security directory must actually exist in the data directory array
  if numRva <= cSecurityDirIndex then
    exit;

  secDirPos := dataDirStart + (cSecurityDirIndex * cDataDirEntrySize);
  if not ReadCardinalAt(stream, secDirPos, virtualAddress) then
    exit;
  if not ReadCardinalAt(stream, secDirPos + SizeOf(Cardinal), size) then
    exit;

  //a non-zero offset and size means an embedded WIN_CERTIFICATE (Authenticode) blob is present.
  result := (virtualAddress <> 0) and (size <> 0);
end;

class function TPEUtils.HasAuthenticodeSignature(const bytes : TBytes) : boolean;
var
  ms : TBytesStream;
begin
  ms := TBytesStream.Create(bytes);
  try
    result := HasAuthenticodeSignature(TStream(ms));
  finally
    ms.Free;
  end;
end;

class function TPEUtils.HasAuthenticodeSignature(const fileName : string) : boolean;
var
  fs : TFileStream;
begin
  result := false;
  if not FileExists(fileName) then
    exit;
  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    result := HasAuthenticodeSignature(TStream(fs));
  finally
    fs.Free;
  end;
end;

end.
