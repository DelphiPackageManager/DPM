unit DPM.Core.Tests.Crypto.Algorithms;

// Direct tests for TAlgorithmProfile — the V-16 / S-5 allowlist. Every hash
// digest / signature decision in the verifier funnels through this profile;
// these tests pin the answers so a future "fix" to broaden the allowlist
// can't slip through without a test going red.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAlgorithmProfileTests = class
  public
    // FileHashAllowed / CmsDigestAllowed
    [Test] procedure FileHash_AllowsSHA256;
    [Test] procedure FileHash_AllowsSHA384;
    [Test] procedure FileHash_AllowsSHA512;
    [Test] procedure FileHash_RejectsUnknown;
    [Test] procedure CmsDigest_AllowsSHA256;
    [Test] procedure CmsDigest_AllowsSHA384;
    [Test] procedure CmsDigest_AllowsSHA512;
    [Test] procedure CmsDigest_RejectsUnknown;

    // TimestampDigestAllowed — Phase 1: SHA-256 only
    [Test] procedure TimestampDigest_AllowsSHA256_Only;

    // SignatureAlgorithmAllowed by OID
    [Test] [TestCase('RSA-SHA256', '1.2.840.113549.1.1.11')]
    [TestCase('RSA-SHA384', '1.2.840.113549.1.1.12')]
    [TestCase('RSA-SHA512', '1.2.840.113549.1.1.13')]
    [TestCase('ECDSA-SHA256', '1.2.840.10045.4.3.2')]
    [TestCase('ECDSA-SHA384', '1.2.840.10045.4.3.3')]
    [TestCase('Bare-RSA', '1.2.840.113549.1.1.1')]
    procedure SignatureAlg_Accepts(const oid : string);

    [Test] [TestCase('RSA-SHA1',  '1.2.840.113549.1.1.5')]
    [TestCase('RSA-MD5',          '1.2.840.113549.1.1.4')]
    [TestCase('DSA-SHA1',         '1.2.840.10040.4.3')]
    [TestCase('Empty',            ' ')]
    [TestCase('Garbage',          'not.an.oid')]
    procedure SignatureAlg_Rejects(const oid : string);

    // MinRsaKeyBits is 2048 in Phase 1
    [Test] procedure MinRsaKeyBits_Is2048;

    // ParseHashName edge cases
    [Test] [TestCase('Lowercase',    'sha256')]
    [TestCase('Mixed',               'Sha-384')]
    [TestCase('Hyphen',              'SHA-512')]
    [TestCase('TrailingSpace',       'SHA256 ')]
    [TestCase('LeadingSpace',        ' SHA256')]
    procedure ParseHashName_AcceptsValidForms(const input : string);

    [Test] [TestCase('SHA1',  'SHA1')]
    [TestCase('MD5',          'MD5')]
    [TestCase('Empty',        ' ')]
    [TestCase('Garbage',      'xxx')]
    [TestCase('SHA-1Hyphen',  'SHA-1')]
    procedure ParseHashName_RejectsBadForms(const input : string);

    // HashAlgorithmName / HashOid / HashOutputSize / HashBCryptAlgorithm
    [Test] procedure HashName_MatchesAlgorithm;
    [Test] procedure HashOutputSize_MatchesAlgorithm;
    [Test] procedure HashOid_RaisesForUnknown;
    [Test] procedure HashBCrypt_RaisesForUnknown;
  end;

implementation

uses
  System.SysUtils,
  DPM.Core.Crypto.Algorithms;

procedure TAlgorithmProfileTests.FileHash_AllowsSHA256;
begin Assert.IsTrue(TAlgorithmProfile.FileHashAllowed(haSha256)); end;
procedure TAlgorithmProfileTests.FileHash_AllowsSHA384;
begin Assert.IsTrue(TAlgorithmProfile.FileHashAllowed(haSha384)); end;
procedure TAlgorithmProfileTests.FileHash_AllowsSHA512;
begin Assert.IsTrue(TAlgorithmProfile.FileHashAllowed(haSha512)); end;
procedure TAlgorithmProfileTests.FileHash_RejectsUnknown;
begin Assert.IsFalse(TAlgorithmProfile.FileHashAllowed(haUnknown)); end;

procedure TAlgorithmProfileTests.CmsDigest_AllowsSHA256;
begin Assert.IsTrue(TAlgorithmProfile.CmsDigestAllowed(haSha256)); end;
procedure TAlgorithmProfileTests.CmsDigest_AllowsSHA384;
begin Assert.IsTrue(TAlgorithmProfile.CmsDigestAllowed(haSha384)); end;
procedure TAlgorithmProfileTests.CmsDigest_AllowsSHA512;
begin Assert.IsTrue(TAlgorithmProfile.CmsDigestAllowed(haSha512)); end;
procedure TAlgorithmProfileTests.CmsDigest_RejectsUnknown;
begin Assert.IsFalse(TAlgorithmProfile.CmsDigestAllowed(haUnknown)); end;

procedure TAlgorithmProfileTests.TimestampDigest_AllowsSHA256_Only;
begin
  Assert.IsTrue (TAlgorithmProfile.TimestampDigestAllowed(haSha256), 'SHA-256 must be allowed');
  Assert.IsFalse(TAlgorithmProfile.TimestampDigestAllowed(haSha384), 'SHA-384 not in Phase 1 timestamp profile');
  Assert.IsFalse(TAlgorithmProfile.TimestampDigestAllowed(haSha512), 'SHA-512 not in Phase 1 timestamp profile');
  Assert.IsFalse(TAlgorithmProfile.TimestampDigestAllowed(haUnknown));
end;

procedure TAlgorithmProfileTests.SignatureAlg_Accepts(const oid : string);
begin
  Assert.IsTrue(TAlgorithmProfile.SignatureAlgorithmAllowed(oid),
    'expected to accept ' + oid);
end;

procedure TAlgorithmProfileTests.SignatureAlg_Rejects(const oid : string);
begin
  Assert.IsFalse(TAlgorithmProfile.SignatureAlgorithmAllowed(Trim(oid)),
    'expected to reject ' + oid);
end;

procedure TAlgorithmProfileTests.MinRsaKeyBits_Is2048;
begin
  Assert.AreEqual(2048, TAlgorithmProfile.MinRsaKeyBits);
end;

procedure TAlgorithmProfileTests.ParseHashName_AcceptsValidForms(const input : string);
var
  alg : THashAlgorithm;
begin
  Assert.IsTrue(TAlgorithmProfile.ParseHashName(input, alg),
    'expected to parse: "' + input + '"');
  Assert.AreNotEqual(Ord(haUnknown), Ord(alg));
end;

procedure TAlgorithmProfileTests.ParseHashName_RejectsBadForms(const input : string);
var
  alg : THashAlgorithm;
begin
  Assert.IsFalse(TAlgorithmProfile.ParseHashName(Trim(input), alg),
    'expected to reject: "' + input + '"');
  Assert.AreEqual(Ord(haUnknown), Ord(alg));
end;

procedure TAlgorithmProfileTests.HashName_MatchesAlgorithm;
begin
  Assert.AreEqual('SHA256', TAlgorithmProfile.HashAlgorithmName(haSha256));
  Assert.AreEqual('SHA384', TAlgorithmProfile.HashAlgorithmName(haSha384));
  Assert.AreEqual('SHA512', TAlgorithmProfile.HashAlgorithmName(haSha512));
  Assert.AreEqual('UNKNOWN', TAlgorithmProfile.HashAlgorithmName(haUnknown));
end;

procedure TAlgorithmProfileTests.HashOutputSize_MatchesAlgorithm;
begin
  Assert.AreEqual(32, TAlgorithmProfile.HashOutputSize(haSha256));
  Assert.AreEqual(48, TAlgorithmProfile.HashOutputSize(haSha384));
  Assert.AreEqual(64, TAlgorithmProfile.HashOutputSize(haSha512));
  Assert.AreEqual(0,  TAlgorithmProfile.HashOutputSize(haUnknown));
end;

procedure TAlgorithmProfileTests.HashOid_RaisesForUnknown;
begin
  Assert.WillRaise(
    procedure begin TAlgorithmProfile.HashOid(haUnknown); end,
    ECryptoAlgorithm);
end;

procedure TAlgorithmProfileTests.HashBCrypt_RaisesForUnknown;
begin
  Assert.WillRaise(
    procedure begin TAlgorithmProfile.HashBCryptAlgorithm(haUnknown); end,
    ECryptoAlgorithm);
end;

initialization
  TDUnitX.RegisterTestFixture(TAlgorithmProfileTests);

end.
