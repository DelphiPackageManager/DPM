unit DPM.Core.Tests.Cache.PackageHash;

interface

uses
  System.SysUtils,
  DPM.Core.Cache.Interfaces,
  DUnitX.TestFramework;

type
  //Exercises TPackageCache.GetPackageHash - the self-heal that mirrors the
  //directory repo's hash handling so any consumer (SBOM, scan, verify) sees
  //a consistent hash regardless of whether the .sha256 sidecar was written
  //at download / push time.
  [TestFixture]
  TPackageCacheHashTests = class
  private
    function MakeTempCacheRoot : string;
    procedure WriteBytes(const path : string; const bytes : TBytes);
    procedure WriteText(const path : string; const text : string);
    function NewIdentity(const id, version : string) : Pointer; //dodge generic IPackageIdentity in DUnitX RTTI scan
    function MakeCache : IPackageCache;
  public
    [Test]
    procedure SidecarPresentIsReturnedAsIs;
    [Test]
    procedure SidecarMissingIsComputedAndPersisted;
    [Test]
    procedure NoDpkgOrSidecarReturnsEmpty;
    [Test]
    procedure SidecarComputedHashMatchesDirectHash;
  end;

implementation

uses
  System.IOUtils,
  System.Classes,
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Types,
  DPM.Core.Constants,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Cache,
  DPM.Core.Utils.Hash,
  DPM.Core.Dependency.Version;

function TPackageCacheHashTests.MakeTempCacheRoot : string;
var
  g : TGUID;
  guidStr : string;
begin
  CreateGUID(g);
  guidStr := LowerCase(Copy(GUIDToString(g), 2, 36));
  result := TPath.Combine(TPath.GetTempPath, 'dpm-cache-test-' + guidStr);
  ForceDirectories(result);
end;

procedure TPackageCacheHashTests.WriteBytes(const path : string; const bytes : TBytes);
var
  fs : TFileStream;
begin
  ForceDirectories(ExtractFilePath(path));
  fs := TFileStream.Create(path, fmCreate);
  try
    if Length(bytes) > 0 then
      fs.WriteBuffer(bytes[0], Length(bytes));
  finally
    fs.Free;
  end;
end;

procedure TPackageCacheHashTests.WriteText(const path : string; const text : string);
begin
  ForceDirectories(ExtractFilePath(path));
  TFile.WriteAllText(path, text);
end;

function TPackageCacheHashTests.NewIdentity(const id, version : string) : Pointer;
var
  ver : TPackageVersion;
  identity : IPackageIdentity;
begin
  ver := TPackageVersion.Parse(version);
  identity := TPackageIdentity.Create('', id, ver, TCompilerVersion.Delphi12_0);
  result := Pointer(identity);
  identity._AddRef;  //caller is responsible for releasing via IInterface(result)._Release
end;

function TPackageCacheHashTests.MakeCache : IPackageCache;
var
  logger : ILogger;
  specReader : IPackageSpecReader;
begin
  // GetPackageHash predates signing — pass nil for the signing/trust/receipt
  // deps. The cache code guards each with `if FSigningService <> nil` etc.
  // Hold the constructed deps in interface locals first: creating an interfaced
  // object directly in a parameter position can leak in Delphi.
  logger := TTestLogger.Create;
  specReader := TPackageSpecReader.Create(logger);
  result := TPackageCache.Create(logger, specReader, nil, nil, nil, nil, nil);
end;

procedure TPackageCacheHashTests.SidecarPresentIsReturnedAsIs;
var
  cacheRoot : string;
  cache : IPackageCache;
  identity : IPackageIdentity;
  packageFolder : string;
  dpkgPath : string;
  sidecarPath : string;
  hash : string;
begin
  cacheRoot := MakeTempCacheRoot;
  try
    cache := MakeCache;
    cache.Location := cacheRoot;
    identity := IPackageIdentity(NewIdentity('Sample.Pkg', '1.2.3'));
    try
      //{cache}/delphi12.0/Sample.Pkg/  <- where the .dpkg + sidecar live
      packageFolder := IncludeTrailingPathDelimiter(cacheRoot) +
                       'delphi12.0' + PathDelim + 'Sample.Pkg' + PathDelim;
      dpkgPath := packageFolder + 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg';
      sidecarPath := dpkgPath + cPackageHashAlgorithmExt;
      WriteBytes(dpkgPath, TEncoding.UTF8.GetBytes('not-the-real-content'));
      WriteText(sidecarPath, 'deadbeefcafebabe');

      hash := cache.GetPackageHash(identity);
      Assert.AreEqual('deadbeefcafebabe', hash, 'sidecar contents should be returned verbatim');
    finally
      IInterface(identity)._Release;
    end;
  finally
    if DirectoryExists(cacheRoot) then
      TDirectory.Delete(cacheRoot, true);
  end;
end;

procedure TPackageCacheHashTests.SidecarMissingIsComputedAndPersisted;
var
  cacheRoot : string;
  cache : IPackageCache;
  identity : IPackageIdentity;
  packageFolder : string;
  dpkgPath : string;
  sidecarPath : string;
  contentBytes : TBytes;
  expectedHash : string;
  firstCallHash : string;
  secondCallHash : string;
begin
  cacheRoot := MakeTempCacheRoot;
  try
    cache := MakeCache;
    cache.Location := cacheRoot;
    identity := IPackageIdentity(NewIdentity('Sample.Pkg', '1.2.3'));
    try
      packageFolder := IncludeTrailingPathDelimiter(cacheRoot) +
                       'delphi12.0' + PathDelim + 'Sample.Pkg' + PathDelim;
      dpkgPath := packageFolder + 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg';
      sidecarPath := dpkgPath + cPackageHashAlgorithmExt;
      contentBytes := TEncoding.UTF8.GetBytes('the actual file content for hashing');
      WriteBytes(dpkgPath, contentBytes);
      //Deliberately do NOT write the sidecar - that's what we're testing.
      Assert.IsFalse(FileExists(sidecarPath), 'precondition: sidecar should not exist');

      expectedHash := THashSHA256.GetHashStringFromFile(dpkgPath);
      firstCallHash := cache.GetPackageHash(identity);
      Assert.AreEqual(expectedHash, firstCallHash,
                      'first call should compute and return the hash');
      Assert.IsTrue(FileExists(sidecarPath),
                    'first call should have persisted the sidecar');

      //Touch the sidecar file with a known marker, then call again - we expect
      //the second call to short-circuit to the sidecar (proving we don't
      //recompute on every call).
      WriteText(sidecarPath, 'previously-written-marker');
      secondCallHash := cache.GetPackageHash(identity);
      Assert.AreEqual('previously-written-marker', secondCallHash,
                      'second call should read the persisted sidecar, not recompute');
    finally
      IInterface(identity)._Release;
    end;
  finally
    if DirectoryExists(cacheRoot) then
      TDirectory.Delete(cacheRoot, true);
  end;
end;

procedure TPackageCacheHashTests.NoDpkgOrSidecarReturnsEmpty;
var
  cacheRoot : string;
  cache : IPackageCache;
  identity : IPackageIdentity;
  hash : string;
begin
  cacheRoot := MakeTempCacheRoot;
  try
    cache := MakeCache;
    cache.Location := cacheRoot;
    identity := IPackageIdentity(NewIdentity('No.Such.Pkg', '9.9.9'));
    try
      hash := cache.GetPackageHash(identity);
      Assert.AreEqual('', hash, 'missing package should yield empty hash, not raise');
    finally
      IInterface(identity)._Release;
    end;
  finally
    if DirectoryExists(cacheRoot) then
      TDirectory.Delete(cacheRoot, true);
  end;
end;

procedure TPackageCacheHashTests.SidecarComputedHashMatchesDirectHash;
//Belt-and-braces: prove the self-heal computes the same hash THashSHA256 does
//directly. Catches future regressions where the cache might switch to a
//different file (e.g. inner manifest) without updating the contract.
var
  cacheRoot : string;
  cache : IPackageCache;
  identity : IPackageIdentity;
  packageFolder : string;
  dpkgPath : string;
  contentBytes : TBytes;
  expected : string;
  actual : string;
begin
  cacheRoot := MakeTempCacheRoot;
  try
    cache := MakeCache;
    cache.Location := cacheRoot;
    identity := IPackageIdentity(NewIdentity('Pkg.With.Content', '0.0.1'));
    try
      packageFolder := IncludeTrailingPathDelimiter(cacheRoot) +
                       'delphi12.0' + PathDelim + 'Pkg.With.Content' + PathDelim;
      dpkgPath := packageFolder + 'Pkg.With.Content-delphi12.0-0000000000000011-0.0.1.dpkg';
      SetLength(contentBytes, 4096);
      FillChar(contentBytes[0], Length(contentBytes), $5A);
      WriteBytes(dpkgPath, contentBytes);

      expected := THashSHA256.GetHashStringFromFile(dpkgPath);
      actual := cache.GetPackageHash(identity);
      Assert.AreEqual(expected, actual);
    finally
      IInterface(identity)._Release;
    end;
  finally
    if DirectoryExists(cacheRoot) then
      TDirectory.Delete(cacheRoot, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheHashTests);

end.
