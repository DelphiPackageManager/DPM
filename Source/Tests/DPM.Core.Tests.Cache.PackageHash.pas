unit DPM.Core.Tests.Cache.PackageHash;

interface

uses
  System.SysUtils,
  DPM.Core.Cache.Interfaces,
  DUnitX.TestFramework;

type
  //Exercises TPackageCache.GetPackageHash - the .sha256 sidecar written when the package was checked, or else
  //the hash of the .dpkg as it is - and the hash a package info read from the cache carries.
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
    procedure SidecarMissingIsComputedButNotWritten;
    [Test]
    procedure NoDpkgOrSidecarReturnsEmpty;
    [Test]
    procedure SidecarComputedHashMatchesDirectHash;
    [Test]
    procedure CachedPackageInfoCarriesTheSidecarHash;
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
  DPM.Core.Dependency.Version,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl;

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

procedure TPackageCacheHashTests.SidecarMissingIsComputedButNotWritten;
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
      //a sidecar is only written for a package that has been checked - one made from whatever file is there
      //(an error page a failed download saved, say) would vouch for it.
      Assert.IsFalse(FileExists(sidecarPath),
                     'the hash of an unchecked .dpkg must not be written as its sidecar');
      secondCallHash := cache.GetPackageHash(identity);
      Assert.AreEqual(expectedHash, secondCallHash);
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

procedure TPackageCacheHashTests.CachedPackageInfoCarriesTheSidecarHash;
const
  cHash = '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef';
var
  cacheRoot : string;
  cache : IPackageCache;
  identity : IPackageIdentity;
  idFolder : string;
  info : IPackageInfo;
begin
  //the .sha256 file holds it in hex - it was read as if it were base64, which made a different 96 character hash.
  cacheRoot := MakeTempCacheRoot;
  try
    cache := MakeCache;
    cache.Location := cacheRoot;
    identity := TPackageIdentity.Create('', 'Sample.Pkg', TPackageVersion.Parse('1.2.3'), TCompilerVersion.Delphi12_0);
    idFolder := IncludeTrailingPathDelimiter(cacheRoot) + 'delphi12.0' + PathDelim + 'Sample.Pkg' + PathDelim;
    WriteText(idFolder + 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg', 'package');
    WriteText(idFolder + 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg' + cPackageHashAlgorithmExt, cHash);
    WriteText(idFolder + '1.2.3' + PathDelim + cPackageDspecFile,
      'metadata:' + sLineBreak +
      '  id: Sample.Pkg' + sLineBreak +
      '  version: 1.2.3' + sLineBreak +
      '  description: cache hash test package' + sLineBreak +
      '  authors:' + sLineBreak +
      '    - DPM' + sLineBreak +
      '  license: Apache-2.0' + sLineBreak +
      'targetPlatforms:' + sLineBreak +
      '  - compiler: 12.0' + sLineBreak +
      '    platforms: [Win32]' + sLineBreak +
      '    template: default' + sLineBreak +
      'templates:' + sLineBreak +
      '  - name: default' + sLineBreak +
      '    source:' + sLineBreak +
      '      - src: ''Source\*.pas''' + sLineBreak);
    info := cache.GetPackageInfo(TCancellationTokenSourceFactory.Create.Token, identity);
    Assert.IsNotNull(info, 'the cached package info');
    Assert.AreEqual(cHash, info.Hash, false);
  finally
    if DirectoryExists(cacheRoot) then
      TDirectory.Delete(cacheRoot, true);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheHashTests);

end.
