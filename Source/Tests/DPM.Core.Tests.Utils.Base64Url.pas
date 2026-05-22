unit DPM.Core.Tests.Utils.Base64Url;

// RFC 4648 §5 base64url codec tests. The test vectors below are the standard
// ones from §10 of the RFC, adjusted for base64url (substitute - for +, _
// for /, strip = padding).

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBase64UrlTests = class
  public
    [Test] procedure Encode_EmptyInput_ReturnsEmptyString;
    [Test] procedure Encode_OneByte_NoPadding;
    [Test] procedure Encode_TwoBytes_NoPadding;
    [Test] procedure Encode_ThreeBytes_NoPadding;
    [Test] procedure Encode_KnownVector_f;
    [Test] procedure Encode_KnownVector_fo;
    [Test] procedure Encode_KnownVector_foo;
    [Test] procedure Encode_KnownVector_foobar;
    [Test] procedure Encode_UsesUrlSafeAlphabet;

    [Test] procedure Decode_EmptyInput_ReturnsEmptyBytes;
    [Test] procedure Decode_KnownVector_foobar;
    [Test] procedure Decode_AcceptsMissingPadding;
    [Test] procedure Decode_AcceptsUrlSafeChars;
    [Test] procedure Decode_RejectsInvalidLength;

    [Test] procedure RoundTrip_RandomBinary_Preserves;
    [Test] procedure RoundTrip_AllBytesZeroToFifteen;
    [Test] procedure RoundTrip_BinaryWithPlusAndSlashEquivalent;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Utils.Base64Url;

function Bytes(const s : string) : TBytes;
begin
  result := TEncoding.UTF8.GetBytes(s);
end;

function BytesFromHex(const hex : string) : TBytes;
var
  i : integer;
begin
  SetLength(result, Length(hex) div 2);
  for i := 0 to High(result) do
    result[i] := StrToInt('$' + Copy(hex, (i * 2) + 1, 2));
end;

procedure TBase64UrlTests.Encode_EmptyInput_ReturnsEmptyString;
begin
  Assert.AreEqual('', TBase64Url.Encode(nil));
end;

procedure TBase64UrlTests.Encode_OneByte_NoPadding;
begin
  // 1 input byte → 2 base64 chars (plus 2 '=' in standard base64); base64url
  // strips the padding entirely.
  Assert.AreEqual('Zg', TBase64Url.Encode(Bytes('f')));
end;

procedure TBase64UrlTests.Encode_TwoBytes_NoPadding;
begin
  Assert.AreEqual('Zm8', TBase64Url.Encode(Bytes('fo')));
end;

procedure TBase64UrlTests.Encode_ThreeBytes_NoPadding;
begin
  // 3 input bytes → exactly 4 base64 chars, no padding even in standard.
  Assert.AreEqual('Zm9v', TBase64Url.Encode(Bytes('foo')));
end;

procedure TBase64UrlTests.Encode_KnownVector_f;
begin
  Assert.AreEqual('Zg', TBase64Url.Encode(Bytes('f')));
end;

procedure TBase64UrlTests.Encode_KnownVector_fo;
begin
  Assert.AreEqual('Zm8', TBase64Url.Encode(Bytes('fo')));
end;

procedure TBase64UrlTests.Encode_KnownVector_foo;
begin
  Assert.AreEqual('Zm9v', TBase64Url.Encode(Bytes('foo')));
end;

procedure TBase64UrlTests.Encode_KnownVector_foobar;
begin
  Assert.AreEqual('Zm9vYmFy', TBase64Url.Encode(Bytes('foobar')));
end;

procedure TBase64UrlTests.Encode_UsesUrlSafeAlphabet;
var
  raw : TBytes;
  encoded : string;
  i : integer;
begin
  // FF FF FF in standard base64 is "////"; base64url substitutes to "____".
  // Pick a byte pattern that produces '+' and '/' in standard base64 so we
  // confirm both substitutions kick in.
  raw := BytesFromHex('FBFFBF');   // → standard base64 "+/+/" — wait, check:
  // 0xFB = 11111011
  // 0xFF = 11111111
  // 0xBF = 10111111
  // Concatenated 24 bits: 111110111111111110111111
  // 6-bit groups: 111110 111111 111110 111111 = 62 63 62 63 = +, /, +, /
  // Standard base64: "+/+/"; base64url: "-_-_"
  encoded := TBase64Url.Encode(raw);
  Assert.AreEqual('-_-_', encoded);
  for i := 1 to Length(encoded) do
    Assert.IsFalse((encoded[i] = '+') or (encoded[i] = '/') or (encoded[i] = '='),
      'base64url output contains a non-url-safe char at position ' + IntToStr(i));
end;

procedure TBase64UrlTests.Decode_EmptyInput_ReturnsEmptyBytes;
begin
  Assert.AreEqual<integer>(0, Length(TBase64Url.Decode('')));
end;

procedure TBase64UrlTests.Decode_KnownVector_foobar;
var
  decoded : TBytes;
begin
  decoded := TBase64Url.Decode('Zm9vYmFy');
  Assert.AreEqual('foobar', TEncoding.UTF8.GetString(decoded));
end;

procedure TBase64UrlTests.Decode_AcceptsMissingPadding;
var
  decoded : TBytes;
begin
  // "Zg" → "f" (1 byte; standard would require "Zg==")
  decoded := TBase64Url.Decode('Zg');
  Assert.AreEqual('f', TEncoding.UTF8.GetString(decoded));

  // "Zm8" → "fo" (2 bytes; standard would require "Zm8=")
  decoded := TBase64Url.Decode('Zm8');
  Assert.AreEqual('fo', TEncoding.UTF8.GetString(decoded));
end;

procedure TBase64UrlTests.Decode_AcceptsUrlSafeChars;
var
  decoded : TBytes;
begin
  // "-_-_" → bytes FB FF BF (the inverse of Encode_UsesUrlSafeAlphabet).
  decoded := TBase64Url.Decode('-_-_');
  Assert.AreEqual<integer>(3, Length(decoded));
  Assert.AreEqual<byte>($FB, decoded[0]);
  Assert.AreEqual<byte>($FF, decoded[1]);
  Assert.AreEqual<byte>($BF, decoded[2]);
end;

procedure TBase64UrlTests.Decode_RejectsInvalidLength;
begin
  // 5 chars after padding restoration would be invalid — base64 has only
  // 0, 2, or 3 leftover chars after multiples of 4. We catch this and raise
  // rather than feed garbage to the inner decoder.
  Assert.WillRaise(
    procedure begin TBase64Url.Decode('Zg9v0') end,
    EBase64Url);
end;

procedure TBase64UrlTests.RoundTrip_RandomBinary_Preserves;
var
  original, decoded : TBytes;
  i : integer;
begin
  // A 256-byte all-values payload like a SHA-256 digest. Anything KV sends
  // us as a signature is similar shape (256 bytes for RSA-2048), so this
  // covers the typical case.
  SetLength(original, 256);
  for i := 0 to 255 do
    original[i] := i;
  decoded := TBase64Url.Decode(TBase64Url.Encode(original));
  Assert.AreEqual<integer>(256, Length(decoded));
  for i := 0 to 255 do
    Assert.AreEqual<byte>(byte(i), decoded[i],
      'mismatch at offset ' + IntToStr(i));
end;

procedure TBase64UrlTests.RoundTrip_AllBytesZeroToFifteen;
var
  original, decoded : TBytes;
  i : integer;
begin
  // Short inputs of every length 0..15 — catches off-by-one in padding
  // handling for any boundary.
  for i := 0 to 15 do
  begin
    SetLength(original, i);
    if i > 0 then
      original[i - 1] := byte(i);
    decoded := TBase64Url.Decode(TBase64Url.Encode(original));
    Assert.AreEqual<integer>(i, Length(decoded),
      'wrong length at i=' + IntToStr(i));
    if i > 0 then
      Assert.AreEqual<byte>(byte(i), decoded[i - 1],
      'wrong last byte at i=' + IntToStr(i));
  end;
end;

procedure TBase64UrlTests.RoundTrip_BinaryWithPlusAndSlashEquivalent;
var
  original, decoded : TBytes;
  encoded : string;
begin
  // Bytes that would produce + and / in standard base64. Verify they
  // round-trip cleanly through the url-safe codec.
  original := BytesFromHex('FBFFBF');
  encoded := TBase64Url.Encode(original);
  decoded := TBase64Url.Decode(encoded);
  Assert.AreEqual<integer>(3, Length(decoded));
  Assert.AreEqual<byte>($FB, decoded[0]);
  Assert.AreEqual<byte>($FF, decoded[1]);
  Assert.AreEqual<byte>($BF, decoded[2]);
end;

initialization
  TDUnitX.RegisterTestFixture(TBase64UrlTests);

end.
