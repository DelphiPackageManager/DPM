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

unit DPM.Core.Crypto.Hashing.Interfaces;

interface

uses
  System.Classes, System.SysUtils,
  DPM.Core.Crypto.Algorithms;

type
  // Streaming hash. One instance hashes one message. Update is called as
  // many times as desired, Finish exactly once.
  IHasher = interface
    ['{4B5A87A1-7C9B-43F4-B5E2-1E1A48AD7B36}']
    procedure Update(const buffer; size : NativeUInt); overload;
    procedure Update(const bytes : TBytes); overload;
    procedure Update(const bytes : TBytes; offset, count : NativeInt); overload;
    function Finish : TBytes;
    function Algorithm : THashAlgorithm;
  end;

  IHashingService = interface
    ['{2D7F90C0-2A0A-4F8B-9D45-9D2E84D77EAF}']
    function CreateHasher(algorithm : THashAlgorithm) : IHasher;
    function HashFile(const filename : string; algorithm : THashAlgorithm) : TBytes;
    function HashStream(const stream : TStream; algorithm : THashAlgorithm) : TBytes;
    function HashBytes(const data : TBytes; algorithm : THashAlgorithm) : TBytes;
    function HashString(const value : string; algorithm : THashAlgorithm) : TBytes;
  end;

  ECryptoHashing = class(Exception);

// Hex / Base64 helpers used everywhere we render hashes.
function BytesToHex(const bytes : TBytes) : string;
function HexToBytes(const hex : string) : TBytes;
function BytesEqual(const a, b : TBytes) : boolean;

implementation

uses
  System.Character;

function BytesToHex(const bytes : TBytes) : string;
const
  cHex : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  i : integer;
  len : integer;
  p : PChar;
begin
  len := Length(bytes);
  SetLength(result, len * 2);
  if len = 0 then
    exit;
  p := PChar(result);
  for i := 0 to len - 1 do
  begin
    p^ := cHex[bytes[i] shr 4]; Inc(p);
    p^ := cHex[bytes[i] and $0F]; Inc(p);
  end;
end;

function HexNibble(c : Char; out value : Byte) : boolean;
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

function HexToBytes(const hex : string) : TBytes;
var
  len : integer;
  i : integer;
  hi, lo : Byte;
begin
  len := Length(hex);
  if (len = 0) or ((len and 1) <> 0) then
    raise ECryptoHashing.CreateFmt('Invalid hex string length (%d)', [len]);
  SetLength(result, len div 2);
  for i := 0 to (len div 2) - 1 do
  begin
    if not HexNibble(hex[1 + i * 2], hi) or
       not HexNibble(hex[2 + i * 2], lo) then
      raise ECryptoHashing.CreateFmt('Invalid hex digit at position %d', [i * 2]);
    result[i] := (hi shl 4) or lo;
  end;
end;

function BytesEqual(const a, b : TBytes) : boolean;
var
  i : integer;
  len : integer;
  diff : Byte;
begin
  len := Length(a);
  if len <> Length(b) then
  begin
    result := false;
    exit;
  end;
  if len = 0 then
  begin
    result := true;
    exit;
  end;
  // Constant-time comparison — used for hash and signature digests.
  diff := 0;
  for i := 0 to len - 1 do
    diff := diff or (a[i] xor b[i]);
  result := diff = 0;
end;

end.
