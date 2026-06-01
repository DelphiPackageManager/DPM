unit DPM.Core.Tests.SigningBadge;

// IDE-2: SigningBadgeResolver projects a verification receipt onto a small
// display badge. The resolver itself is pure logic — no VCL dependencies —
// so these tests can live in the Core test project.

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TSigningBadgeTests = class
  private
    function MakeTempFolder : string;
  public
    [Test] procedure Unverified_When_NoReceipt;
    [Test] procedure IntegrityUnsigned_When_Decision_IsUnsigned;
    [Test] procedure UntrustedPublisher_When_Decision_IsUntrusted;
    [Test] procedure SignedTrusted_When_Decision_IsTrusted_NamesSigner;
    [Test] procedure Invalid_When_Decision_IsInvalid;
    [Test] procedure Detail_Includes_ManifestHash_AndSignatures;
    [Test] procedure UntrustedPublisher_Exposes_AuthorSpkiAndName;
    [Test] procedure Trusted_Exposes_AuthorSpkiAndName;
    [Test] procedure Unsigned_HasEmpty_SignerFields;
    [Test] procedure Invalid_HasEmpty_SignerFields;
    [Test] procedure AuthorSigner_PreferredOver_RepositorySigner;
    [Test] procedure Trusted_NotPinned_Downgrades_WhenPinChecked;
    [Test] procedure Trusted_Pinned_StaysTrusted_WhenPinChecked;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Types,
  DPM.Core.Dependency.Version,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Package.Interfaces,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Spec.Interfaces,
  DPM.IDE.SigningBadge;

type
  // Cache fake — only the GetPackagePath overload the resolver uses needs
  // a real return value.
  TStubCache = class(TInterfacedObject, IPackageCache)
  private
    FFolder : string;
  protected
    function GetLocation : string;
    procedure SetLocation(const value : string);
    function GetPackagesFolder : string;
    function Clean : boolean;
    function CreatePackagePath(const packageId : IPackageIdentity) : string;
    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string;
                            const compilerVersion : TCompilerVersion) : string; overload;
    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;
    function EnsurePackage(const packageId : IPackageIdentity) : boolean;
    function InstallPackageFromFile(const packageFileName : string) : boolean;
    function GetPackageInfo(const cancellationToken : ICancellationToken;
                            const packageId : IPackageIdentity) : IPackageInfo;
    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;
    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;
    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;
    function TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
    function GetPackageHash(const packageId : IPackageIdentity) : string;
    function FullReVerify : integer;
    function GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
    function RemovePackage(const packageId : IPackageIdentity) : boolean;
  public
    constructor Create(const folder : string);
  end;

constructor TStubCache.Create(const folder : string);
begin
  inherited Create;
  FFolder := folder;
end;

function TStubCache.GetLocation : string; begin result := ''; end;
procedure TStubCache.SetLocation(const value : string); begin end;
function TStubCache.GetPackagesFolder : string; begin result := ''; end;
function TStubCache.Clean : boolean; begin result := false; end;
function TStubCache.CreatePackagePath(const packageId : IPackageIdentity) : string; begin result := ''; end;
function TStubCache.GetPackagePath(const packageId : IPackageIdentity) : string; begin result := FFolder; end;
function TStubCache.GetPackagePath(const id : string; const version : string;
                                    const compilerVersion : TCompilerVersion) : string;
begin
  result := FFolder;
end;
function TStubCache.GetPackageFileFolder(const packageId : IPackageIdentity) : string; begin result := ''; end;
function TStubCache.EnsurePackage(const packageId : IPackageIdentity) : boolean; begin result := false; end;
function TStubCache.InstallPackageFromFile(const packageFileName : string) : boolean; begin result := false; end;
function TStubCache.GetPackageInfo(const cancellationToken : ICancellationToken;
                                    const packageId : IPackageIdentity) : IPackageInfo; begin result := nil; end;
function TStubCache.GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata; begin result := nil; end;
function TStubCache.GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec; begin result := nil; end;
function TStubCache.GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms; begin result := []; end;
function TStubCache.GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                              const id : string;
                                                              const compilerVersion : TCompilerVersion;
                                                              const versionRange : TVersionRange;
                                                              const preRelease : boolean) : IList<IPackageInfo>;
begin
  result := TCollections.CreateList<IPackageInfo>;
end;
function TStubCache.TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
begin
  result := false;
  icon := nil;
end;
function TStubCache.GetPackageHash(const packageId : IPackageIdentity) : string; begin result := ''; end;
function TStubCache.FullReVerify : integer; begin result := 0; end;
function TStubCache.GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
begin
  result := TCollections.CreateList<IPackageIdentity>;
end;
function TStubCache.RemovePackage(const packageId : IPackageIdentity) : boolean; begin result := false; end;

function TSigningBadgeTests.MakeTempFolder : string;
var
  g : TGUID;
begin
  CreateGUID(g);
  result := TPath.Combine(TPath.GetTempPath,
    'dpm-badge-test-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(result);
end;

function MakeReceipt(const decision : string;
                     const signerSubject : string = '';
                     const signerSpki : string = '') : TVerificationReceipt;
var
  sig : TReceiptSignature;
begin
  result.ReceiptVersion := 1;
  result.PackageId := 'Test.Pkg';
  result.Version := '1.0.0';
  result.Compiler := 'DelphiXE2';
  result.ManifestHashAlgorithm := haSha256;
  result.ManifestHashHex := 'deadbeef';
  result.TrustDecision := decision;
  result.TrustPolicyFingerprint := 'fp1';
  result.VerifiedAt := Now;
  result.DpmVersion := '0.6.0';
  if (signerSubject <> '') or (signerSpki <> '') then
  begin
    sig.Role := 'author';
    sig.SignerSpkiHex := signerSpki;
    sig.SignerSubject := signerSubject;
    sig.Thumbprint := '';
    sig.EffectiveSigningTime := Now;
    sig.TimestampAuthority := '';
    sig.RevocationStatus := 'notChecked';
    SetLength(result.Signatures, 1);
    result.Signatures[0] := sig;
  end
  else
    SetLength(result.Signatures, 0);
end;

procedure TSigningBadgeTests.Unverified_When_NoReceipt;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual(Ord(sbsUnverified), Ord(badge.State));
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.IntegrityUnsigned_When_Decision_IsUnsigned;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('unsigned');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual(Ord(sbsIntegrityUnsigned), Ord(badge.State));
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.UntrustedPublisher_When_Decision_IsUntrusted;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('untrusted-publisher', 'CN=Joe', 'ab12');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual(Ord(sbsUntrustedPublisher), Ord(badge.State));
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.SignedTrusted_When_Decision_IsTrusted_NamesSigner;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('trusted', 'CN=VSoft Technologies', 'ab12');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual(Ord(sbsSignedTrusted), Ord(badge.State));
    Assert.IsTrue(Pos('VSoft Technologies', badge.Caption) > 0,
      'caption should name the signer, got: ' + badge.Caption);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Invalid_When_Decision_IsInvalid;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('invalid');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual(Ord(sbsInvalid), Ord(badge.State));
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Detail_Includes_ManifestHash_AndSignatures;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('trusted', 'CN=Foo', 'cafe');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.IsTrue(Pos('deadbeef', badge.Detail) > 0,
      'detail should include the manifest hash');
    Assert.IsTrue(Pos('cafe', badge.Detail) > 0,
      'detail should include the signer SPKI');
    Assert.IsTrue(Pos('CN=Foo', badge.Detail) > 0,
      'detail should include the signer subject');
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.UntrustedPublisher_Exposes_AuthorSpkiAndName;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('untrusted-publisher', 'CN=Joe', 'ab12');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual('ab12', badge.SignerSpkiHex);
    Assert.AreEqual('Joe', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Trusted_Exposes_AuthorSpkiAndName;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('trusted', 'CN=VSoft Technologies', 'cafe');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual('cafe', badge.SignerSpkiHex);
    Assert.AreEqual('VSoft Technologies', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Unsigned_HasEmpty_SignerFields;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('unsigned');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual('', badge.SignerSpkiHex);
    Assert.AreEqual('', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Invalid_HasEmpty_SignerFields;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('invalid', 'CN=Joe', 'ab12');
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual('', badge.SignerSpkiHex);
    Assert.AreEqual('', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.AuthorSigner_PreferredOver_RepositorySigner;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
  repoSig : TReceiptSignature;
  authorSig : TReceiptSignature;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    // Build a receipt with a repository signature *before* the author one to
    // prove the resolver selects the author signature regardless of order.
    r := MakeReceipt('untrusted-publisher');
    repoSig.Role := 'repository';
    repoSig.SignerSpkiHex := 'deadbeef';
    repoSig.SignerSubject := 'CN=Some Gallery';
    repoSig.Thumbprint := '';
    repoSig.EffectiveSigningTime := Now;
    repoSig.TimestampAuthority := '';
    repoSig.RevocationStatus := 'notChecked';
    authorSig.Role := 'author';
    authorSig.SignerSpkiHex := 'ab12';
    authorSig.SignerSubject := 'CN=Joe';
    authorSig.Thumbprint := '';
    authorSig.EffectiveSigningTime := Now;
    authorSig.TimestampAuthority := '';
    authorSig.RevocationStatus := 'notChecked';
    SetLength(r.Signatures, 2);
    r.Signatures[0] := repoSig;
    r.Signatures[1] := authorSig;
    receipt.Write(folder, r);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2);
    Assert.AreEqual('ab12', badge.SignerSpkiHex);
    Assert.AreEqual('Joe', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Trusted_NotPinned_Downgrades_WhenPinChecked;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
  emptyList : TArray<string>;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    // Permissive-mode receipt: 'trusted' even though the signer isn't pinned.
    r := MakeReceipt('trusted', 'CN=Joe', 'ab12');
    receipt.Write(folder, r);
    SetLength(emptyList, 0);
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2,
      false, '', emptyList);
    Assert.AreEqual(Ord(sbsUntrustedPublisher), Ord(badge.State),
      'an unpinned author should downgrade to untrusted-publisher when pin-checked');
    Assert.AreEqual('ab12', badge.SignerSpkiHex);
    Assert.AreEqual('Joe', badge.SignerName);
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

procedure TSigningBadgeTests.Trusted_Pinned_StaysTrusted_WhenPinChecked;
var
  folder : string;
  cache : IPackageCache;
  receipt : IReceiptService;
  badge : TSigningBadge;
  r : TVerificationReceipt;
  pinned : TArray<string>;
begin
  folder := MakeTempFolder;
  try
    cache := TStubCache.Create(folder);
    receipt := TYamlReceiptService.Create;
    r := MakeReceipt('trusted', 'CN=Joe', 'ab12');
    receipt.Write(folder, r);
    // Pinned with a 'sha256:'-prefixed entry to prove normalisation matches a
    // raw-hex receipt SPKI.
    SetLength(pinned, 1);
    pinned[0] := 'sha256:AB12';
    badge := TSigningBadgeResolver.Resolve(cache, receipt,
      'Test.Pkg', TPackageVersion.Parse('1.0.0'), TCompilerVersion.DelphiXE2,
      false, '', pinned);
    Assert.AreEqual(Ord(sbsSignedTrusted), Ord(badge.State),
      'a pinned author should remain trusted');
  finally
    if DirectoryExists(folder) then
      TDirectory.Delete(folder, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSigningBadgeTests);

end.
