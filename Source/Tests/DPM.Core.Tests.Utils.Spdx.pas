unit DPM.Core.Tests.Utils.Spdx;

// Tests for the shared SPDX license lookup. These also confirm that the
// DPM_SPDX_LICENSES resource (compiled from Source\spdx-licenses.txt via
// Source\DPM.Spdx.rc) embeds correctly into the test binary.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSpdxLicensesTests = class
  public
    [Test] procedure IsValidLicenseId_KnownId_ReturnsTrue;
    [Test] procedure IsValidLicenseId_IsCaseInsensitive;
    [Test] procedure IsValidLicenseId_NonSpdxValueWithSpace_ReturnsFalse;
    [Test] procedure IsValidLicenseId_Empty_ReturnsFalse;
    [Test] procedure GetLicenseUrl_KnownId_ReturnsSpdxUrl;
    [Test] procedure GetLicenseUrl_UnknownId_ReturnsEmpty;
    [Test] procedure GetLicenseName_KnownId_ReturnsName;
    [Test] procedure GetLicenseIds_PopulatesList;
    [Test] procedure TryGetCanonicalLicenseId_ReturnsCanonicalCasing;
    [Test] procedure TryGetCanonicalLicenseId_UnknownId_ReturnsFalse;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  DPM.Core.Utils.Spdx;

procedure TSpdxLicensesTests.IsValidLicenseId_KnownId_ReturnsTrue;
begin
  Assert.IsTrue(TSpdxLicenses.IsValidLicenseId('MIT'));
  Assert.IsTrue(TSpdxLicenses.IsValidLicenseId('Apache-2.0'));
end;

procedure TSpdxLicensesTests.IsValidLicenseId_IsCaseInsensitive;
begin
  // ids are matched case-insensitively
  Assert.IsTrue(TSpdxLicenses.IsValidLicenseId('mit'));
  Assert.IsTrue(TSpdxLicenses.IsValidLicenseId('apache-2.0'));
end;

procedure TSpdxLicensesTests.IsValidLicenseId_NonSpdxValueWithSpace_ReturnsFalse;
begin
  // 'MPL 1.1' is not a valid SPDX id (the real id is 'MPL-1.1'); this is the
  // case that used to produce a broken, space-containing spdx.org link.
  Assert.IsFalse(TSpdxLicenses.IsValidLicenseId('MPL 1.1'));
end;

procedure TSpdxLicensesTests.IsValidLicenseId_Empty_ReturnsFalse;
begin
  Assert.IsFalse(TSpdxLicenses.IsValidLicenseId(''));
  Assert.IsFalse(TSpdxLicenses.IsValidLicenseId('   '));
end;

procedure TSpdxLicensesTests.GetLicenseUrl_KnownId_ReturnsSpdxUrl;
begin
  Assert.AreEqual('https://spdx.org/licenses/MIT.html', TSpdxLicenses.GetLicenseUrl('MIT'));
end;

procedure TSpdxLicensesTests.GetLicenseUrl_UnknownId_ReturnsEmpty;
begin
  Assert.AreEqual('', TSpdxLicenses.GetLicenseUrl('MPL 1.1'));
  Assert.AreEqual('', TSpdxLicenses.GetLicenseUrl('not-a-real-license'));
end;

procedure TSpdxLicensesTests.GetLicenseName_KnownId_ReturnsName;
begin
  Assert.AreEqual('MIT License', TSpdxLicenses.GetLicenseName('MIT'));
end;

procedure TSpdxLicensesTests.TryGetCanonicalLicenseId_ReturnsCanonicalCasing;
var
  canonical : string;
begin
  //CycloneDX licenses[].license.id is a case sensitive enum, so callers need the
  //canonical spelling back, not whatever casing the .dspec happened to use.
  //note the explicit ignoreCase=false - DUnitX string comparisons ignore case by default,
  //which would make these assertions vacuous.
  Assert.IsTrue(TSpdxLicenses.TryGetCanonicalLicenseId('apache-2.0', canonical));
  Assert.AreEqual('Apache-2.0', canonical, false);
  Assert.IsTrue(TSpdxLicenses.TryGetCanonicalLicenseId('mit', canonical));
  Assert.AreEqual('MIT', canonical, false);
  Assert.IsTrue(TSpdxLicenses.TryGetCanonicalLicenseId('  MIT  ', canonical));
  Assert.AreEqual('MIT', canonical, false);
end;

procedure TSpdxLicensesTests.TryGetCanonicalLicenseId_UnknownId_ReturnsFalse;
var
  canonical : string;
begin
  Assert.IsFalse(TSpdxLicenses.TryGetCanonicalLicenseId('Apache 2.0', canonical));
  Assert.AreEqual('', canonical);
  Assert.IsFalse(TSpdxLicenses.TryGetCanonicalLicenseId('', canonical));
  Assert.AreEqual('', canonical);
end;

procedure TSpdxLicensesTests.GetLicenseIds_PopulatesList;
var
  ids : TStringList;
begin
  ids := TStringList.Create;
  try
    TSpdxLicenses.GetLicenseIds(ids);
    Assert.IsTrue(ids.Count > 100, 'expected the full SPDX list to load');
    Assert.IsTrue(ids.IndexOf('MIT') <> -1, 'MIT should be present in the id list');
  finally
    ids.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSpdxLicensesTests);

end.
