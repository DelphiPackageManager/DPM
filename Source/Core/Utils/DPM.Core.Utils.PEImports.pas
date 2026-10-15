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

unit DPM.Core.Utils.PEImports;

{ Reads the modules a Windows PE image (exe, dll, bpl) imports - the runtime packages and DLLs it
  loads as soon as it starts. Plain file reading, no Windows image APIs, so it works on any image
  regardless of the bitness of the process reading it.

  Only the static import table and the delay-load table are read. A module loaded by name at
  runtime (LoadLibrary, LoadPackage) is not in either. }

interface

type
  TPEImport = record
    ModuleName : string;    // as the import table spells it - 'rtl370.bpl', 'KERNEL32.dll'
    Delayed : boolean;      // only in the delay-load table
  end;

///<summary>The modules fileName imports, in table order, static imports before delay-loaded ones.
/// A module is listed once however many descriptors name it (compared case insensitively), and
/// counts as Delayed only when no static descriptor names it. Returns false with errorMessage set
/// when the file is not a PE image or its import tables can't be read.</summary>
function TryReadPEImports(const fileName : string; out imports : TArray<TPEImport>; out errorMessage : string) : boolean;

implementation

uses
  System.SysUtils,
  System.Classes;

type
  EPEImportError = class(Exception);

  TSection = record
    VirtualAddress : Cardinal;
    VirtualSize : Cardinal;
    RawSize : Cardinal;
    RawPointer : Cardinal;
  end;

const
  cImportDirectory = 1;
  cDelayImportDirectory = 13;
  //sanity limits - a damaged image must not send us round in circles
  cMaxDescriptors = 4096;
  cMaxNameLength = 512;

function TryReadPEImports(const fileName : string; out imports : TArray<TPEImport>; out errorMessage : string) : boolean;
var
  stream : TFileStream;
  sections : array of TSection;
  imageBase : UInt64;
  seen : TStringList;

  procedure Fail(const reason : string);
  begin
    raise EPEImportError.Create(reason);
  end;

  procedure ReadAt(const offset : Int64; var buffer; const count : integer);
  begin
    if (offset < 0) or (offset + count > stream.Size) then
      Fail('truncated image - offset ' + IntToStr(offset) + ' is past the end of the file');
    stream.Position := offset;
    stream.ReadBuffer(buffer, count);
  end;

  function ReadWord(const offset : Int64) : Word;
  begin
    ReadAt(offset, result, SizeOf(result));
  end;

  function ReadDWord(const offset : Int64) : Cardinal;
  begin
    ReadAt(offset, result, SizeOf(result));
  end;

  function RvaToOffset(const rva : Cardinal) : Int64;
  var
    i : integer;
    span : Cardinal;
  begin
    for i := 0 to Length(sections) - 1 do
    begin
      span := sections[i].VirtualSize;
      if sections[i].RawSize > span then
        span := sections[i].RawSize;
      if (rva >= sections[i].VirtualAddress) and (rva - sections[i].VirtualAddress < span) then
      begin
        //inside the section but past its raw data means zero filled, not in the file
        if rva - sections[i].VirtualAddress >= sections[i].RawSize then
          Fail('RVA $' + IntToHex(rva, 8) + ' has no data in the file');
        exit(Int64(sections[i].RawPointer) + (rva - sections[i].VirtualAddress));
      end;
    end;
    Fail('RVA $' + IntToHex(rva, 8) + ' is not in any section');
    result := -1;
  end;

  function ReadAnsiName(const rva : Cardinal) : string;
  var
    offset : Int64;
    buffer : array[0..cMaxNameLength - 1] of AnsiChar;
    count : integer;
    i : integer;
    name : AnsiString;
  begin
    offset := RvaToOffset(rva);
    count := cMaxNameLength;
    if offset + count > stream.Size then
      count := stream.Size - offset;
    if count <= 0 then
      Fail('truncated image - module name past the end of the file');
    ReadAt(offset, buffer[0], count);
    for i := 0 to count - 1 do
    begin
      if buffer[i] = #0 then
      begin
        SetString(name, PAnsiChar(@buffer[0]), i);
        result := string(name);
        exit;
      end;
    end;
    Fail('unterminated module name');
  end;

  procedure Add(const moduleName : string; const delayed : boolean);
  var
    entry : TPEImport;
  begin
    if (moduleName = '') or (seen.IndexOf(moduleName) >= 0) then
      exit;
    seen.Add(moduleName);
    entry.ModuleName := moduleName;
    entry.Delayed := delayed;
    SetLength(imports, Length(imports) + 1);
    imports[Length(imports) - 1] := entry;
  end;

  //IMAGE_IMPORT_DESCRIPTOR - 20 bytes, Name RVA at +12, an all zero entry ends the table
  procedure ReadImportTable(const rva : Cardinal);
  var
    offset : Int64;
    descriptor : array[0..4] of Cardinal;
    i : integer;
  begin
    offset := RvaToOffset(rva);
    for i := 0 to cMaxDescriptors - 1 do
    begin
      ReadAt(offset + i * 20, descriptor[0], 20);
      if descriptor[3] = 0 then
        exit;
      Add(ReadAnsiName(descriptor[3]), false);
    end;
  end;

  //IMAGE_DELAYLOAD_DESCRIPTOR - 32 bytes, Attributes at +0, DllNameRVA at +4. With Attributes bit 0
  //clear (Visual C++ 6 era) the name is a virtual address, not an RVA.
  procedure ReadDelayImportTable(const rva : Cardinal);
  var
    offset : Int64;
    descriptor : array[0..7] of Cardinal;
    nameRva : Cardinal;
    i : integer;
  begin
    offset := RvaToOffset(rva);
    for i := 0 to cMaxDescriptors - 1 do
    begin
      ReadAt(offset + i * 32, descriptor[0], 32);
      if descriptor[1] = 0 then
        exit;
      nameRva := descriptor[1];
      if (descriptor[0] and 1) = 0 then
        nameRva := Cardinal(UInt64(nameRva) - imageBase);
      Add(ReadAnsiName(nameRva), true);
    end;
  end;

var
  peOffset : Int64;
  sectionCount : Word;
  optionalSize : Word;
  optionalStart : Int64;
  magic : Word;
  rvaCount : Cardinal;
  dataDirectories : Int64;
  importRva : Cardinal;
  delayRva : Cardinal;
  sectionTable : Int64;
  i : integer;
begin
  imports := nil;
  errorMessage := '';
  result := false;
  seen := nil;
  stream := nil;
  try
    try
      stream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
      seen := TStringList.Create;
      seen.CaseSensitive := false;

      if ReadWord(0) <> $5A4D then //'MZ'
        Fail('no MZ header');
      peOffset := ReadDWord($3C);
      if ReadDWord(peOffset) <> $00004550 then //'PE'#0#0
        Fail('no PE signature');
      sectionCount := ReadWord(peOffset + 4 + 2);
      optionalSize := ReadWord(peOffset + 4 + 16);
      optionalStart := peOffset + 4 + 20;

      magic := ReadWord(optionalStart);
      case magic of
        $10B : //PE32
        begin
          imageBase := ReadDWord(optionalStart + 28);
          rvaCount := ReadDWord(optionalStart + 92);
          dataDirectories := optionalStart + 96;
        end;
        $20B : //PE32+
        begin
          ReadAt(optionalStart + 24, imageBase, SizeOf(imageBase));
          rvaCount := ReadDWord(optionalStart + 108);
          dataDirectories := optionalStart + 112;
        end;
      else
        Fail('unknown optional header magic $' + IntToHex(magic, 4));
        exit;
      end;

      sectionTable := optionalStart + optionalSize;
      SetLength(sections, sectionCount);
      for i := 0 to sectionCount - 1 do
      begin
        sections[i].VirtualSize := ReadDWord(sectionTable + i * 40 + 8);
        sections[i].VirtualAddress := ReadDWord(sectionTable + i * 40 + 12);
        sections[i].RawSize := ReadDWord(sectionTable + i * 40 + 16);
        sections[i].RawPointer := ReadDWord(sectionTable + i * 40 + 20);
      end;

      importRva := 0;
      delayRva := 0;
      if rvaCount > cImportDirectory then
        importRva := ReadDWord(dataDirectories + cImportDirectory * 8);
      if rvaCount > cDelayImportDirectory then
        delayRva := ReadDWord(dataDirectories + cDelayImportDirectory * 8);

      if importRva <> 0 then
        ReadImportTable(importRva);
      if delayRva <> 0 then
        ReadDelayImportTable(delayRva);
      result := true;
    except
      on e : Exception do
      begin
        imports := nil;
        errorMessage := e.Message;
      end;
    end;
  finally
    seen.Free;
    stream.Free;
  end;
end;

end.
