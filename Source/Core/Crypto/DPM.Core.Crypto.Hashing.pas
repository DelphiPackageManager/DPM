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

unit DPM.Core.Crypto.Hashing;

// SHA-256/384/512 hashing over Windows CNG (BCrypt). One algorithm provider
// per algorithm is held in a singleton, reused across hashers — opening a
// new provider per hash is measurably slow.
//
// Files are streamed in 64KB chunks, never loaded whole.

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  DPM.Core.Crypto.Win32,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Hashing.Interfaces;

type
  TBCryptHashingService = class(TInterfacedObject, IHashingService)
  private
    FLock : TCriticalSection;
    FAlgorithms : array[THashAlgorithm] of BCRYPT_ALG_HANDLE;
    function GetAlgorithmProvider(algorithm : THashAlgorithm) : BCRYPT_ALG_HANDLE;
  protected
    function CreateHasher(algorithm : THashAlgorithm) : IHasher;
    function HashFile(const filename : string; algorithm : THashAlgorithm) : TBytes;
    function HashStream(const stream : TStream; algorithm : THashAlgorithm) : TBytes;
    function HashBytes(const data : TBytes; algorithm : THashAlgorithm) : TBytes;
    function HashString(const value : string; algorithm : THashAlgorithm) : TBytes;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  WinApi.Windows;

const
  cStreamBufferSize = 64 * 1024;

type
  TBCryptHasher = class(TInterfacedObject, IHasher)
  private
    FHash       : BCRYPT_HASH_HANDLE;
    FHashObject : TBytes;
    FAlgorithm  : THashAlgorithm;
    FOutputSize : integer;
    FFinished   : boolean;
  protected
    procedure Update(const buffer; size : NativeUInt); overload;
    procedure Update(const bytes : TBytes); overload;
    procedure Update(const bytes : TBytes; offset, count : NativeInt); overload;
    function Finish : TBytes;
    function Algorithm : THashAlgorithm;
  public
    constructor Create(provider : BCRYPT_ALG_HANDLE; algorithm : THashAlgorithm; objectSize : integer);
    destructor Destroy; override;
  end;

procedure CheckNTStatus(status : NTSTATUS; const apiName : string);
begin
  if status <> STATUS_SUCCESS then
    raise ECryptoHashing.CreateFmt('%s failed with NTSTATUS 0x%.8x', [apiName, status]);
end;

{ TBCryptHasher }

constructor TBCryptHasher.Create(provider : BCRYPT_ALG_HANDLE; algorithm : THashAlgorithm; objectSize : integer);
var
  status : NTSTATUS;
begin
  inherited Create;
  FAlgorithm := algorithm;
  FOutputSize := TAlgorithmProfile.HashOutputSize(algorithm);
  if FOutputSize = 0 then
    raise ECryptoHashing.Create('Unsupported hash algorithm');

  if objectSize > 0 then
    SetLength(FHashObject, objectSize);

  status := BCryptCreateHash(provider, FHash,
    PByte(FHashObject), Length(FHashObject), nil, 0, 0);
  CheckNTStatus(status, 'BCryptCreateHash');
end;

destructor TBCryptHasher.Destroy;
begin
  if FHash <> 0 then
    BCryptDestroyHash(FHash);
  inherited;
end;

procedure TBCryptHasher.Update(const buffer; size : NativeUInt);
var
  status : NTSTATUS;
begin
  if FFinished then
    raise ECryptoHashing.Create('Hasher already finished');
  if size = 0 then
    exit;
  status := BCryptHashData(FHash, @buffer, ULONG(size), 0);
  CheckNTStatus(status, 'BCryptHashData');
end;

procedure TBCryptHasher.Update(const bytes : TBytes);
begin
  if Length(bytes) > 0 then
    Update(bytes[0], Length(bytes));
end;

procedure TBCryptHasher.Update(const bytes : TBytes; offset, count : NativeInt);
begin
  if (count < 0) or (offset < 0) or (offset + count > Length(bytes)) then
    raise ECryptoHashing.CreateFmt('Update range out of bounds (offset=%d count=%d len=%d)',
      [offset, count, Length(bytes)]);
  if count = 0 then
    exit;
  Update(bytes[offset], count);
end;

function TBCryptHasher.Finish : TBytes;
var
  status : NTSTATUS;
begin
  if FFinished then
    raise ECryptoHashing.Create('Hasher already finished');
  SetLength(result, FOutputSize);
  status := BCryptFinishHash(FHash, PByte(result), FOutputSize, 0);
  CheckNTStatus(status, 'BCryptFinishHash');
  FFinished := true;
end;

function TBCryptHasher.Algorithm : THashAlgorithm;
begin
  result := FAlgorithm;
end;

{ TBCryptHashingService }

constructor TBCryptHashingService.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TBCryptHashingService.Destroy;
var
  alg : THashAlgorithm;
begin
  for alg := Low(THashAlgorithm) to High(THashAlgorithm) do
  begin
    if FAlgorithms[alg] <> 0 then
    begin
      BCryptCloseAlgorithmProvider(FAlgorithms[alg], 0);
      FAlgorithms[alg] := 0;
    end;
  end;
  FLock.Free;
  inherited;
end;

function TBCryptHashingService.GetAlgorithmProvider(algorithm : THashAlgorithm) : BCRYPT_ALG_HANDLE;
var
  status : NTSTATUS;
  algName : PWideChar;
begin
  if not TAlgorithmProfile.FileHashAllowed(algorithm) then
    raise ECryptoHashing.CreateFmt('Hash algorithm %s is not permitted by the algorithm profile',
      [TAlgorithmProfile.HashAlgorithmName(algorithm)]);

  FLock.Enter;
  try
    if FAlgorithms[algorithm] = 0 then
    begin
      algName := TAlgorithmProfile.HashBCryptAlgorithm(algorithm);
      status := BCryptOpenAlgorithmProvider(FAlgorithms[algorithm], algName, nil, 0);
      CheckNTStatus(status, 'BCryptOpenAlgorithmProvider');
    end;
    result := FAlgorithms[algorithm];
  finally
    FLock.Leave;
  end;
end;

function TBCryptHashingService.CreateHasher(algorithm : THashAlgorithm) : IHasher;
var
  provider : BCRYPT_ALG_HANDLE;
  objectSize : DWORD;
  pcb : ULONG;
  status : NTSTATUS;
begin
  provider := GetAlgorithmProvider(algorithm);

  // BCryptCreateHash needs a caller-allocated working buffer of the size
  // BCrypt reports via BCRYPT_OBJECT_LENGTH. Querying once per hasher
  // creation matches the documented usage.
  pcb := 0;
  status := BCryptGetProperty(provider, BCRYPT_OBJECT_LENGTH,
    PByte(@objectSize), SizeOf(objectSize), pcb, 0);
  CheckNTStatus(status, 'BCryptGetProperty(BCRYPT_OBJECT_LENGTH)');

  result := TBCryptHasher.Create(provider, algorithm, objectSize);
end;

function TBCryptHashingService.HashStream(const stream : TStream; algorithm : THashAlgorithm) : TBytes;
var
  hasher : IHasher;
  buf : TBytes;
  bytesRead : integer;
begin
  hasher := CreateHasher(algorithm);
  SetLength(buf, cStreamBufferSize);
  repeat
    bytesRead := stream.Read(buf[0], cStreamBufferSize);
    if bytesRead > 0 then
      hasher.Update(buf[0], bytesRead);
  until bytesRead = 0;
  result := hasher.Finish;
end;

function TBCryptHashingService.HashFile(const filename : string; algorithm : THashAlgorithm) : TBytes;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    result := HashStream(fs, algorithm);
  finally
    fs.Free;
  end;
end;

function TBCryptHashingService.HashBytes(const data : TBytes; algorithm : THashAlgorithm) : TBytes;
var
  hasher : IHasher;
begin
  hasher := CreateHasher(algorithm);
  if Length(data) > 0 then
    hasher.Update(data[0], Length(data));
  result := hasher.Finish;
end;

function TBCryptHashingService.HashString(const value : string; algorithm : THashAlgorithm) : TBytes;
var
  utf8 : TBytes;
begin
  utf8 := TEncoding.UTF8.GetBytes(value);
  result := HashBytes(utf8, algorithm);
end;

end.
