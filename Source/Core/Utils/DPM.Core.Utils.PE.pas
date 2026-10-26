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

// Inspection of Windows PE binaries (.exe/.dll/.bpl). Safe in Core (which must run in Docker Windows
// Server Core and not depend on VCL/GDI) - wintrust.dll is part of Server Core.
//
//  - IsPE detects whether the bytes are a real PE image (MZ + PE signature), regardless of extension.
//  - HasAuthenticodeSignature detects the *presence* of an embedded Authenticode signature by reading
//    the PE security data directory (IMAGE_DIRECTORY_ENTRY_SECURITY). It does NOT validate the
//    signature or its certificate chain - it answers "is this binary signed at all?", which is enough
//    to fail fast on an unsigned binary before an author-signed package is rejected by the gallery.
//    Pure byte-level, no Windows API.
//  - VerifyAuthenticode asks Windows (WinVerifyTrust) whether the embedded signature is valid and
//    chains to a trusted root - the check before dpm runs an executable it downloaded.

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

    //True when fileName carries an embedded Authenticode signature that Windows reports valid : the file is
    //unchanged since it was signed and the signing certificate chains to a trusted root, was valid when it
    //signed (or when the signature was timestamped) and is allowed to sign code. Catalog signatures don't
    //count - an installer we downloaded has to carry its own. checkRevocation checks every certificate
    //in the chain online and fails when that can't be done. Otherwise reason says what was wrong, in a
    //form that follows "the file ...".
    class function VerifyAuthenticode(const fileName : string; const checkRevocation : boolean; out reason : string) : boolean; static;
  end;

implementation

uses
  WinApi.Windows,
  DPM.Core.Crypto.Win32;

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

//what a WinVerifyTrust result means for the file, worded to follow "the file ...".
function AuthenticodeFailureReason(const status : LongInt) : string;
begin
  case status of
    TRUST_E_NOSIGNATURE :
      result := 'is not signed';
    TRUST_E_SUBJECT_FORM_UNKNOWN :
      result := 'is not a file Windows can check a signature on (not a Windows executable)';
    TRUST_E_BAD_DIGEST :
      result := 'has been changed since it was signed';
    CERT_E_UNTRUSTEDROOT, CERT_E_UNTRUSTEDTESTROOT :
      result := 'is signed with a certificate that does not chain to a trusted root';
    CERT_E_CHAINING :
      result := 'is signed with a certificate whose chain could not be built';
    CERT_E_EXPIRED :
      result := 'is signed with a certificate that had expired when it signed, and the signature is not timestamped';
    CERT_E_REVOKED, CRYPT_E_REVOKED :
      result := 'is signed with a certificate that has been revoked';
    CERT_E_REVOCATION_FAILURE, CRYPT_E_REVOCATION_OFFLINE, CRYPT_E_NO_REVOCATION_CHECK :
      result := 'is signed, but the revocation status of its certificates could not be checked (is something blocking ' +
                'the certificate authority''s revocation servers?)';
    CERT_E_WRONG_USAGE :
      result := 'is signed with a certificate that is not valid for signing code';
    TRUST_E_EXPLICIT_DISTRUST :
      result := 'is signed with a certificate that this machine is set to distrust';
    CRYPT_E_SECURITY_SETTINGS :
      result := 'can not be run under this machine''s security settings';
    TRUST_E_SUBJECT_NOT_TRUSTED :
      result := 'is not trusted';
  else
    result := 'failed its signature check : ' + SysErrorMessage(DWORD(status)) + ' (0x' + IntToHex(DWORD(status), 8) + ')';
  end;
end;

class function TPEUtils.VerifyAuthenticode(const fileName : string; const checkRevocation : boolean; out reason : string) : boolean;
var
  fileInfo : WINTRUST_FILE_INFO;
  trustData : WINTRUST_DATA;
  actionId : TGUID;
  status : LongInt;
begin
  result := false;
  reason := '';
  if not FileExists(fileName) then
  begin
    reason := 'does not exist';
    exit;
  end;

  FillChar(fileInfo, SizeOf(fileInfo), 0);
  fileInfo.cbStruct := SizeOf(fileInfo);
  fileInfo.pcwszFilePath := PWideChar(fileName);

  FillChar(trustData, SizeOf(trustData), 0);
  trustData.cbStruct := SizeOf(trustData);
  trustData.dwUIChoice := WTD_UI_NONE;
  trustData.dwUnionChoice := WTD_CHOICE_FILE;
  trustData.pFile := @fileInfo;
  trustData.dwStateAction := WTD_STATEACTION_VERIFY;
  if checkRevocation then
  begin
    trustData.fdwRevocationChecks := WTD_REVOKE_WHOLECHAIN;
    //a root is trusted because it is in the store - there is nothing to revoke it.
    trustData.dwProvFlags := WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT;
  end
  else
  begin
    trustData.fdwRevocationChecks := WTD_REVOKE_NONE;
    trustData.dwProvFlags := WTD_REVOCATION_CHECK_NONE;
  end;

  //a typed constant can't be passed as a var parameter.
  actionId := WINTRUST_ACTION_GENERIC_VERIFY_V2;
  status := WinVerifyTrust(INVALID_HANDLE_VALUE, actionId, @trustData);
  try
    result := status = 0;
    if not result then
      reason := AuthenticodeFailureReason(status);
  finally
    //the verify call keeps state (the chain, the file open) until it is closed - whatever it returned.
    trustData.dwStateAction := WTD_STATEACTION_CLOSE;
    WinVerifyTrust(INVALID_HANDLE_VALUE, actionId, @trustData);
  end;
end;

end.
