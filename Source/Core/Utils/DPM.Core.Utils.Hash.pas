{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright © 2019 Vincent Parrett and contributors               }
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

unit DPM.Core.Utils.Hash;

//works in XE2 or higher - around 10% faster than rtl in release mode.
//rtl didn't add SHA256 until 10.0

interface

uses
  System.Classes, System.SysUtils;

type
  THashSHA256 = record
    class function GetHashString(const AStream: TStream): string;static;
    class function GetHashBytes(const Stream: TStream): TBytes;static;
    class function GetHashBytesFromFile(const fileName : string) : TBytes;static;
    class function GetHashStringFromFile(const fileName : string) : string;static;
    class function DigestAsString(const Digest: TBytes): string;static;
  end;


implementation

{$Q-} // Disable overflow checking for modular arithmetic


const
  K: array[0..63] of UInt32 = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3,
    $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC,
    $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7,
    $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13,
    $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3,
    $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5,
    $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );


type
  TSHA256Ctx = record
    State: array[0..7] of UInt32;
    Buffer: array[0..63] of Byte; // holds partial block
    BufLen: Integer;               // bytes currently in Buffer
    TotalBits: UInt64;             // message length in bits (original data only)
  end;

function RRot32(x: UInt32; n: Integer): UInt32; inline;
begin
  Result := (x shr n) or (x shl (32 - n));
end;

function Ch(x, y, z: UInt32): UInt32; inline;
begin
  Result := (x and y) xor (not x and z);
end;

function Maj(x, y, z: UInt32): UInt32; inline;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

function BigSigma0(x: UInt32): UInt32; inline;
begin
  Result := RRot32(x, 2) xor RRot32(x, 13) xor RRot32(x, 22);
end;

function BigSigma1(x: UInt32): UInt32; inline;
begin
  Result := RRot32(x, 6) xor RRot32(x, 11) xor RRot32(x, 25);
end;

function SmallSigma0(x: UInt32): UInt32; inline;
begin
  Result := RRot32(x, 7) xor RRot32(x, 18) xor (x shr 3);
end;

function SmallSigma1(x: UInt32): UInt32; inline;
begin
  Result := RRot32(x, 17) xor RRot32(x, 19) xor (x shr 10);
end;

procedure SHA256_Init(var Ctx: TSHA256Ctx);inline;
begin
  Ctx.State[0] := $6A09E667;
  Ctx.State[1] := $BB67AE85;
  Ctx.State[2] := $3C6EF372;
  Ctx.State[3] := $A54FF53A;
  Ctx.State[4] := $510E527F;
  Ctx.State[5] := $9B05688C;
  Ctx.State[6] := $1F83D9AB;
  Ctx.State[7] := $5BE0CD19;
  Ctx.BufLen := 0;
  Ctx.TotalBits := 0;
end;


function BSwap32(x: UInt32): UInt32; inline;
begin
  Result :=
    ((x and $FF000000) shr 24) or
    ((x and $00FF0000) shr 8)  or
    ((x and $0000FF00) shl 8)  or
    ((x and $000000FF) shl 24);
end;


type
  TBlock = array[0..15] of UInt32;
  PBlock = ^TBlock;

procedure SHA256_ProcessBlock(var Ctx: TSHA256Ctx; Data: PByte);
var
  W: array[0..63] of UInt32;
  a, b, c, d, e, f, g, h, t1, t2: UInt32;
  i: Integer;
  P : PBlock;
begin
  P := @Data[0];
  // Load 16 big-endian words
  for i := 0 to 15 do
  begin
    W[i] := BSwap32(P[i]);
  end;

  // Expand to 64 words
  for i := 16 to 63 do
    W[i] := SmallSigma1(W[i-2]) + W[i-7] + SmallSigma0(W[i-15]) + W[i-16];

  // Initialize working variables
  a := Ctx.State[0];
  b := Ctx.State[1];
  c := Ctx.State[2];
  d := Ctx.State[3];
  e := Ctx.State[4];
  f := Ctx.State[5];
  g := Ctx.State[6];
  h := Ctx.State[7];

  // Main loop
  for i := 0 to 63 do
  begin
    t1 := h + BigSigma1(e) + Ch(e, f, g) + K[i] + W[i];
    t2 := BigSigma0(a) + Maj(a, b, c);
    h := g; g := f; f := e;
    e := d + t1; d := c; c := b; b := a;
    a := t1 + t2;
  end;

  Ctx.State[0] := Ctx.State[0] + a;
  Ctx.State[1] := Ctx.State[1] + b;
  Ctx.State[2] := Ctx.State[2] + c;
  Ctx.State[3] := Ctx.State[3] + d;
  Ctx.State[4] := Ctx.State[4] + e;
  Ctx.State[5] := Ctx.State[5] + f;
  Ctx.State[6] := Ctx.State[6] + g;
  Ctx.State[7] := Ctx.State[7] + h;
end;

procedure SHA256_Update(var Ctx: TSHA256Ctx; Data: PByte; Len: NativeUInt);
var
  ToCopy: Integer;
begin
  // Track original message length ONLY
  Inc(Ctx.TotalBits, UInt64(Len) * 8);

  // If there's data buffered, fill it to a full block first
  if (Ctx.BufLen > 0) then
  begin
    ToCopy := 64 - Ctx.BufLen;
    if ToCopy > Integer(Len) then
      ToCopy := Integer(Len);
    Move(Data^, Ctx.Buffer[Ctx.BufLen], ToCopy);
    Inc(Ctx.BufLen, ToCopy);
    Inc(Data, ToCopy);
    Dec(Len, ToCopy);
    if Ctx.BufLen = 64 then
    begin
      SHA256_ProcessBlock(Ctx, @Ctx.Buffer[0]);
      Ctx.BufLen := 0;
    end;
  end;

  // Process full blocks directly from the caller's buffer (no copy)
  while Len >= 64 do
  begin
    SHA256_ProcessBlock(Ctx, Data);
    Inc(Data, 64);
    Dec(Len, 64);
  end;

  // Buffer any tail
  if Len > 0 then
  begin
    Move(Data^, Ctx.Buffer[0], Len);
    Ctx.BufLen := Integer(Len);
  end;
end;

type
  PUint32 = ^UInt32;

function SHA256_Final(var Ctx: TSHA256Ctx): TBytes;
var
  Pad: array[0..63] of Byte;
  LenBytes: array[0..7] of Byte;
  i: Integer;
  BitLen: UInt64; // snapshot BEFORE padding
  Need: Integer;
  p : PUint32;
begin
  // Save original message length in bits (must not include padding)
  BitLen := Ctx.TotalBits;

  // Append the '1' bit (0x80) followed by zero padding to reach 56 mod 64
  Pad[0] := $80;

  if Ctx.BufLen < 56 then
  begin
    // one block is enough
    FillChar(Pad[1], 55 - Ctx.BufLen, 0);
    Need := 56 - Ctx.BufLen;
    SHA256_Update(Ctx, @Pad[0], Need);
  end
  else
  begin
    // need two blocks
    FillChar(Pad[1], 63 - Ctx.BufLen, 0);
    Need := 64 - Ctx.BufLen;
    SHA256_Update(Ctx, @Pad[0], Need);
    FillChar(Pad[0], 56, 0);
    SHA256_Update(Ctx, @Pad[0], 56);
  end;

  // Append 64-bit big-endian original length
  for i := 0 to 7 do
    LenBytes[7 - i] := Byte(BitLen shr (i * 8));
  SHA256_Update(Ctx, @LenBytes[0], 8);

  // Produce the digest
  SetLength(Result, 32);
  p := PUInt32(@Result[0]);
  for i := 0 to 7 do
  begin
    p^ := BSwap32(Ctx.State[i]); // write big-endian word
    Inc(p);                         // advance by 4 bytes
  end;

end;

function SHA256FromStream(const Stream: TStream): TBytes;
const
  BUFFER_SIZE = 8 * 1024; // buffer size > 8K doesn't seem to make much difference to performance
var
  Buf: array of Byte;
  Read: Integer;
  Ctx: TSHA256Ctx;
begin
  SHA256_Init(Ctx);
  SetLength(Buf, BUFFER_SIZE);
  repeat
    Read := Stream.Read(Buf[0], BUFFER_SIZE);
    if Read > 0 then
      SHA256_Update(Ctx, @Buf[0], Read);
  until Read = 0;
  Result := SHA256_Final(Ctx);
end;

class function THashSHA256.DigestAsString(const Digest: TBytes): string;
const
  Hex: array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  i: Integer;
  l : integer;
  p: PChar;
begin
  l := Length(Digest);
  SetLength(Result, l * 2);
  p := PChar(Result);
  for i := 0 to l-1 do
  begin
    p^ := Hex[Digest[i] shr 4]; Inc(p);
    p^ := Hex[Digest[i] and $0F]; Inc(p);
  end;
end;

class function THashSHA256.GetHashString(const AStream: TStream): string;
begin
  Result := DigestAsString(SHA256FromStream(AStream));
end;

class function THashSHA256.GetHashBytes(const Stream: TStream): TBytes;
begin
  Result := SHA256FromStream(Stream);
end;

class function THashSHA256.GetHashBytesFromFile(const fileName : string) : TBytes;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fileName,fmOpenRead + fmShareDenyNone);
  try
    Result := SHA256FromStream(fs);
  finally
    fs.Free;
  end;
end;

class function THashSHA256.GetHashStringFromFile(const fileName : string) : string;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fileName,fmOpenRead + fmShareDenyNone);
  try
    Result := DigestAsString(SHA256FromStream(fs));
  finally
    fs.Free;
  end;
end;

end.
