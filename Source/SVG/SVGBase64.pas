unit SVGBase64;

interface

uses
  classes;

//stripped out from TNetEncoding - just the bits we need for svg.

function Base64DecodeStream(const InputStream : TStream; const OutputStream: TStream) : integer;

implementation

{$IF CompilerVersion > 23.0}
{$LEGACYIFEND ON}
{$IFEND}

uses
{$IF CompilerVersion > 27}
  System.NetEncoding,
{$IFEND}
  System.SysUtils,
  System.Types;

const
   DecodeTable: array[0..79] of Int8 = (
      62,  -1,  -1,  -1,  63,  52,  53,  54,  55,  56,  57, 58, 59, 60, 61, -1,
      -1,  -1,  -2,  -1,  -1,  -1,   0,   1,   2,   3,   4,  5,  6,  7,  8,  9,
      10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20, 21, 22, 23, 24, 25,
      -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,  29,  30, 31, 32, 33, 34, 35,
      36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46, 47, 48, 49, 50, 51);

 Base64Values : array [0..255] of byte =
     (
     $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FE, $FE, $FF, $FF, $FE, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $FF, $FF, $3F,
     $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FD, $FF, $FF,
     $FF,  $0,  $1,  $2,  $3,  $4,  $5,  $6,  $7,  $8,  $9,  $A,  $B,  $C,  $D,  $E,
      $F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $FF,
     $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
     $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
     ) ;


type
  TDecodeGroup = (groupA, groupB, groupC, groupD);

function DecodeValue(const Code: Byte): Integer;
var
  LCode: Integer;
begin
  LCode := Code - 43;
  if (LCode < Low(DecodeTable)) or (LCode > High(DecodeTable)) then
    Result := -1
  else
    Result := DecodeTable[LCode];
end;

function DecodeBytes(Input, Output: PByte; InputLen: Integer; var group : TDecodeGroup ) : integer;
var
  POutput : PByte;
  PInput  : PByte;
  PEnd    : PByte;
  fragment: Integer;
begin
  POutput := Output;
  PInput := Input;
  PEnd := PInput + InputLen;
  POutput^ := 0;
  while True do
  begin
    case group of
      TDecodeGroup.groupA:
      begin
        repeat
          if PInput = PEnd then
            Exit(POutput - Output);
          fragment := DecodeValue(Ord(PInput^));
          Inc(PInput);
        until (Fragment >= 0) ;
        POutput^ := (fragment and $03F) shl 2;
        group := TDecodeGroup.groupB;
      end;

      TDecodeGroup.groupB:
      begin
        repeat
          if PInput = PEnd then
            Exit(POutput - Output);
          fragment := DecodeValue(Ord(PInput^));
          Inc(PInput);
        until (Fragment >= 0) ;
        POutput^ := (POutput^ or ((fragment and $030) shr 4));
        Inc(POutput);
        POutput^ :=           ((fragment and $00F) shl 4);
        group := TDecodeGroup.groupC;
      end;

      TDecodeGroup.groupC:
      begin
        repeat
          if PInput = PEnd then
            Exit(POutput - Output);
          fragment := DecodeValue(Ord(PInput^));
          Inc(PInput);
        until (Fragment >= 0) ;
        POutput^ := (POutput^ or ((fragment and $03C) shr 2));
        Inc(POutput);
        POutput^ := ((fragment and $003) shl 6);
        group := TDecodeGroup.groupD;
      end;

      TDecodeGroup.groupD:
      begin
        repeat
          if PInput = PEnd then
            Exit(POutput - Output);
          fragment := DecodeValue(Ord(PInput^));
          Inc(PInput);
        until (fragment >= 0) ;
        POutput^ := (POutput^ or (fragment and $03F));
        Inc(POutput);
        group := TDecodeGroup.groupA;
      end;
    end;
  end;
end;


{$IF CompilerVersion > 27}
function Base64DecodeStream(const InputStream : TStream; const OutputStream: TStream) : integer;
begin
  result := TNetEncoding.Base64.Decode(InputStream, OutputStream);
end;
{$ELSE}
function Base64DecodeStream(const InputStream : TStream; const OutputStream: TStream) : integer;
var
  inputBuffer: array[0..1023] of byte;
  outputBuffer: array[0..767] of byte;
  bytesRead, bytesToWrite: Integer;
  group : TDecodeGroup;
begin
  Result := 0;
  group := TDecodeGroup.groupA;
  repeat
    bytesRead := InputStream.Read(inputBuffer[0], Length(inputBuffer));
    bytesToWrite := DecodeBytes(@inputBuffer[0], @outputBuffer[0], bytesRead, group);
    OutputStream.Write(Outputbuffer, bytesToWrite);
    Result := Result + bytesToWrite;
  until bytesRead = 0;
end;
{$IFEND}
end.
