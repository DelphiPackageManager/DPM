unit DPM.Core.Tests.Cache.Verify;

interface

uses
  System.SysUtils,
  DPM.Core.Cache.Interfaces,
  DUnitX.TestFramework;

type
  //Exercises TPackageCache.FullReVerify (the IDE's "Verify DPM Package Cache" and dpm cache verify).
  //The installer builds into the cached package folder - lib\{platform}, bpl\{platform} and the
  //package.{platform}.bom compile-skip marker all live next to the extracted package. Verifying a
  //package whose extraction is intact must leave them alone; only a damaged extraction is replaced.
  [TestFixture]
  TPackageCacheVerifyTests = class
  private
    FCacheRoot : string;
    FPackageFileName : string;
    FPackageFolder : string;
    function MakeCache(const fingerprint : string) : IPackageCache;
    procedure WriteTestPackage(const fileName : string);
    procedure WriteText(const path : string; const text : string);
    procedure WriteBuildOutput;
    procedure AssertBuildOutputKept;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FullReVerify_KeepsBuildOutputOfIntactExtraction;
    [Test]
    procedure FullReVerify_KeepsBuildOutputWhenTrustPolicyChanged;
    [Test]
    procedure FullReVerify_ReExtractsWhenExtractedManifestAltered;
    [Test]
    procedure FullReVerify_ReExtractsWhenReceiptMissing;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Zip,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  TestLogger,
  DPM.Core.Logging,
  DPM.Core.Constants,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Spec.Reader,
  DPM.Core.Cache,
  DPM.Core.Utils.Hash,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Crypto.Algorithms,
  DPM.Core.Crypto.Provider.Interfaces,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Package.Signing.Interfaces;

const
  cTestPackageFile = 'Sample.Pkg-delphi12.0-0000000000000011-1.2.3.dpkg';
  cTestSourceFile = 'Source' + PathDelim + 'Sample.Unit.pas';

type
  //Trust policy whose fingerprint the test controls - a changed fingerprint is how a trust policy
  //change shows up to the cache.
  TFakeTrustPolicy = class(TInterfacedObject, ITrustPolicyService)
  private
    FFingerprint : string;
  protected
    function GetEffectivePolicy : TTrustPolicy;
    function PolicyFingerprint(const policy : TTrustPolicy) : string;
    function PublisherTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
    function RepositoryTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
  public
    constructor Create(const fingerprint : string);
  end;

  //Stands in for the real verifier: every package verifies as unsigned, carrying the hash of the
  //manifest inside the .dpkg - the same value the real service puts in the result and the receipt.
  TFakeSigningService = class(TInterfacedObject, IPackageSigningService)
  private
    FTrustPolicy : ITrustPolicyService;
  protected
    procedure SignPackage(const packageFilePath : string;
                          const provider : ISigningProvider;
                          const options : ISignOptions);
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy) : TVerificationResult; overload;
    function VerifyPackage(const packageFilePath : string;
                           const policy : TTrustPolicy;
                           const flags : TVerifyFlags) : TVerificationResult; overload;
    function QuickRecheck(const cacheFolder : string;
                          const policy : TTrustPolicy) : boolean;
  public
    constructor Create(const trustPolicy : ITrustPolicyService);
  end;

{ TFakeTrustPolicy }

constructor TFakeTrustPolicy.Create(const fingerprint : string);
begin
  inherited Create;
  FFingerprint := fingerprint;
end;

function TFakeTrustPolicy.GetEffectivePolicy : TTrustPolicy;
begin
  result.ValidationMode := vmPermissive;
  result.AuthorDowngradePolicy := adpAllow;
  result.AllowKeyCompromiseOverride := false;
  result.TrustSetVersion := 1;
  SetLength(result.TrustedPublishers, 0);
  SetLength(result.TrustedRepositories, 0);
end;

function TFakeTrustPolicy.PolicyFingerprint(const policy : TTrustPolicy) : string;
begin
  result := FFingerprint;
end;

function TFakeTrustPolicy.PublisherTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
begin
  result := false;
end;

function TFakeTrustPolicy.RepositoryTrusted(const policy : TTrustPolicy; const spkiHex : string) : boolean;
begin
  result := false;
end;

{ TFakeSigningService }

constructor TFakeSigningService.Create(const trustPolicy : ITrustPolicyService);
begin
  inherited Create;
  FTrustPolicy := trustPolicy;
end;

procedure TFakeSigningService.SignPackage(const packageFilePath : string;
                                          const provider : ISigningProvider;
                                          const options : ISignOptions);
begin
  raise ENotImplemented.Create('TFakeSigningService.SignPackage');
end;

function TFakeSigningService.VerifyPackage(const packageFilePath : string;
                                           const policy : TTrustPolicy) : TVerificationResult;
var
  zip : TZipFile;
  manifestBytes : TBytes;
  stream : TBytesStream;
begin
  zip := TZipFile.Create;
  try
    zip.Open(packageFilePath, zmRead);
    zip.Read(cManifestFileName, manifestBytes);
  finally
    zip.Free;
  end;
  stream := TBytesStream.Create(manifestBytes);
  try
    result.ManifestHashHex := THashSHA256.GetHashString(stream);
  finally
    stream.Free;
  end;
  result.Outcome := voUnsigned;
  result.ManifestHashAlgorithm := haSha256;
  SetLength(result.Signatures, 0);
  result.Reason := '';
  result.Mode := policy.ValidationMode;
  result.PolicyFingerprint := FTrustPolicy.PolicyFingerprint(policy);
end;

function TFakeSigningService.VerifyPackage(const packageFilePath : string;
                                           const policy : TTrustPolicy;
                                           const flags : TVerifyFlags) : TVerificationResult;
begin
  result := VerifyPackage(packageFilePath, policy);
end;

function TFakeSigningService.QuickRecheck(const cacheFolder : string;
                                          const policy : TTrustPolicy) : boolean;
begin
  result := false;
end;

{ TPackageCacheVerifyTests }

procedure TPackageCacheVerifyTests.Setup;
var
  g : TGUID;
begin
  CreateGUID(g);
  FCacheRoot := TPath.Combine(TPath.GetTempPath, 'dpm-cache-verify-' + LowerCase(Copy(GUIDToString(g), 2, 36)));
  ForceDirectories(FCacheRoot);
  //{cache}\delphi12.0\Sample.Pkg\ holds the .dpkg, 1.2.3\ beside it is the extraction.
  FPackageFileName := TPath.Combine(FCacheRoot, 'delphi12.0' + PathDelim + 'Sample.Pkg' + PathDelim + cTestPackageFile);
  FPackageFolder := TPath.Combine(FCacheRoot, 'delphi12.0' + PathDelim + 'Sample.Pkg' + PathDelim + '1.2.3');
  WriteTestPackage(FPackageFileName);
end;

procedure TPackageCacheVerifyTests.TearDown;
begin
  if DirectoryExists(FCacheRoot) then
    TDirectory.Delete(FCacheRoot, true);
end;

function TPackageCacheVerifyTests.MakeCache(const fingerprint : string) : IPackageCache;
var
  logger : ILogger;
  specReader : IPackageSpecReader;
  trustPolicy : ITrustPolicyService;
  signingService : IPackageSigningService;
  receiptService : IReceiptService;
begin
  //hold each dep in an interface local first: constructing an interfaced object directly in a
  //parameter position can leak in Delphi. No trust state - the TOFU ratchets aren't under test.
  logger := TTestLogger.Create;
  specReader := TPackageSpecReader.Create(logger);
  trustPolicy := TFakeTrustPolicy.Create(fingerprint);
  signingService := TFakeSigningService.Create(trustPolicy);
  receiptService := TYamlReceiptService.Create;
  result := TPackageCache.Create(logger, specReader, signingService, trustPolicy, receiptService, nil, nil);
  result.Location := FCacheRoot;
end;

procedure TPackageCacheVerifyTests.WriteTestPackage(const fileName : string);
var
  zip : TZipFile;
begin
  ForceDirectories(ExtractFilePath(fileName));
  zip := TZipFile.Create;
  try
    zip.Open(fileName, zmWrite);
    zip.Add(TEncoding.UTF8.GetBytes('{"files":[]}'), cManifestFileName);
    zip.Add(TEncoding.UTF8.GetBytes('metadata:' + sLineBreak + '  id: Sample.Pkg' + sLineBreak), cPackageDspecFile);
    zip.Add(TEncoding.UTF8.GetBytes('unit Sample.Unit;' + sLineBreak + 'interface' + sLineBreak + 'implementation' + sLineBreak + 'end.'),
            'Source/Sample.Unit.pas');
    zip.Close;
  finally
    zip.Free;
  end;
end;

procedure TPackageCacheVerifyTests.WriteText(const path : string; const text : string);
begin
  ForceDirectories(ExtractFilePath(path));
  TFile.WriteAllText(path, text);
end;

procedure TPackageCacheVerifyTests.WriteBuildOutput;
begin
  //what TPackageInstaller.CompilePackage leaves in the package folder.
  WriteText(TPath.Combine(FPackageFolder, 'lib' + PathDelim + 'Win32' + PathDelim + 'Sample.Unit.dcu'), 'dcu');
  WriteText(TPath.Combine(FPackageFolder, 'bpl' + PathDelim + 'Win32' + PathDelim + 'Sample.bpl'), 'bpl');
  WriteText(TPath.Combine(FPackageFolder, 'package.Win32.bom'), 'bom');
end;

procedure TPackageCacheVerifyTests.AssertBuildOutputKept;
begin
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, 'lib' + PathDelim + 'Win32' + PathDelim + 'Sample.Unit.dcu')),
                'the compiled lib output should survive verifying an intact extraction');
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, 'bpl' + PathDelim + 'Win32' + PathDelim + 'Sample.bpl')),
                'the compiled bpl output should survive verifying an intact extraction');
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, 'package.Win32.bom')),
                'the compile-skip bom should survive verifying an intact extraction - without it every package recompiles');
end;

procedure TPackageCacheVerifyTests.FullReVerify_KeepsBuildOutputOfIntactExtraction;
var
  cache : IPackageCache;
  failures : integer;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  WriteBuildOutput;

  failures := cache.FullReVerify(TCancellationTokenSourceFactory.Create.Token);

  Assert.AreEqual(0, failures, 'the package is valid');
  AssertBuildOutputKept;
end;

procedure TPackageCacheVerifyTests.FullReVerify_KeepsBuildOutputWhenTrustPolicyChanged;
var
  cache : IPackageCache;
  receiptService : IReceiptService;
  receipt : TVerificationReceipt;
  failures : integer;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  WriteBuildOutput;

  //same cache folder, new policy - the receipt on disk now carries a stale fingerprint.
  cache := MakeCache('policy-b');
  failures := cache.FullReVerify(TCancellationTokenSourceFactory.Create.Token);

  Assert.AreEqual(0, failures, 'the package is valid under the new policy');
  AssertBuildOutputKept;
  receiptService := TYamlReceiptService.Create;
  Assert.IsTrue(receiptService.TryRead(FPackageFolder, receipt), 'the receipt should still be there');
  Assert.AreEqual('policy-b', receipt.TrustPolicyFingerprint, false,
                  'verifying under the new policy should refresh the receipt''s fingerprint');
end;

procedure TPackageCacheVerifyTests.FullReVerify_ReExtractsWhenExtractedManifestAltered;
var
  cache : IPackageCache;
  failures : integer;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  WriteText(TPath.Combine(FPackageFolder, cManifestFileName), '{"files":["tampered"]}');
  DeleteFile(TPath.Combine(FPackageFolder, cTestSourceFile));

  failures := cache.FullReVerify(TCancellationTokenSourceFactory.Create.Token);

  Assert.AreEqual(0, failures, 'the .dpkg itself is valid');
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, cTestSourceFile)),
                'an extraction that no longer matches the package should be replaced from the .dpkg');
end;

procedure TPackageCacheVerifyTests.FullReVerify_ReExtractsWhenReceiptMissing;
var
  cache : IPackageCache;
  failures : integer;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  //no receipt = the extraction never completed (it is written last).
  DeleteFile(TPath.Combine(FPackageFolder, cReceiptFileName));
  DeleteFile(TPath.Combine(FPackageFolder, cTestSourceFile));

  failures := cache.FullReVerify(TCancellationTokenSourceFactory.Create.Token);

  Assert.AreEqual(0, failures, 'the .dpkg itself is valid');
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, cTestSourceFile)),
                'an extraction without a receipt is incomplete and should be replaced from the .dpkg');
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheVerifyTests);

end.
