unit DPM.Core.Tests.Cache.Verify;

interface

uses
  System.SysUtils,
  DPM.Core.Logging,
  DPM.Core.Cache.Interfaces,
  TestLogger,
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
    //the last cache's logger - FLoggerRef keeps it alive.
    FLogger : TTestLogger;
    FLoggerRef : ILogger;
    //what the fake verifier says the signed manifest declares.
    FManifestPackageId : string;
    FManifestVersion : string;
    FFormatNotSupported : boolean;
    function MakeCache(const fingerprint : string) : IPackageCache;
    procedure WriteTestPackage(const fileName : string; const specVersion : string = '1.2.3'; const specCompiler : string = '12.0');
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
    [Test]
    procedure FullReVerify_RestoresModifiedFileAndKeepsBuildOutput;
    [Test]
    procedure InstallPackageFromFile_OnlyFullReVerifyChecksEveryFile;
    [Test]
    procedure GitMarker_CountsOnlyWithoutAReceipt;
    [Test]
    procedure InstallPackageFromFile_ManifestOfAnotherVersion_FailsAndRemovesFile;
    [Test]
    procedure InstallPackageFromFile_ManifestOfAnotherPackage_Fails;
    [Test]
    procedure InstallPackageFromFile_ManifestVersionMetadataIsIgnored;
    [Test]
    procedure InstallPackageFromFile_DspecOfAnotherVersion_FailsAndRemovesExtraction;
    [Test]
    procedure InstallPackageFromFile_DspecForAnotherCompiler_Fails;
    [Test]
    procedure InstallPackageFromFile_NewerFormat_IsE7109;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Zip,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  DPM.Core.Diagnostics,
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
  cTestSourceEntry = 'Source/Sample.Unit.pas';
  cTestSourceContent = 'unit Sample.Unit;' + sLineBreak + 'interface' + sLineBreak + 'implementation' + sLineBreak + 'end.';

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
  //manifest inside the .dpkg - the same value the real service puts in the result and the receipt -
  //and the identity the test says the manifest declares.
  TFakeSigningService = class(TInterfacedObject, IPackageSigningService)
  private
    FTrustPolicy : ITrustPolicyService;
    FManifestPackageId : string;
    FManifestVersion : string;
    FFormatNotSupported : boolean;
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
    //the test package's only source file must be there with its original content.
    function VerifyExtractedFiles(const packageFolder : string;
                                  out mismatched : TArray<string>) : boolean;
  public
    constructor Create(const trustPolicy : ITrustPolicyService; const manifestPackageId : string; const manifestVersion : string;
                       const formatNotSupported : boolean);
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

constructor TFakeSigningService.Create(const trustPolicy : ITrustPolicyService; const manifestPackageId : string; const manifestVersion : string;
                                       const formatNotSupported : boolean);
begin
  inherited Create;
  FTrustPolicy := trustPolicy;
  FManifestPackageId := manifestPackageId;
  FManifestVersion := manifestVersion;
  FFormatNotSupported := formatNotSupported;
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
  result.ManifestPackageId := FManifestPackageId;
  result.ManifestVersion := FManifestVersion;
  result.FormatNotSupported := FFormatNotSupported;
  if FFormatNotSupported then
  begin
    result.Outcome := voInvalid;
    result.Reason := 'The package is in a newer format than this version of DPM supports.';
  end;
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

function TFakeSigningService.VerifyExtractedFiles(const packageFolder : string;
                                                  out mismatched : TArray<string>) : boolean;
var
  fileName : string;
begin
  SetLength(mismatched, 0);
  fileName := TPath.Combine(packageFolder, cTestSourceFile);
  result := FileExists(fileName) and (TFile.ReadAllText(fileName) = cTestSourceContent);
  if not result then
  begin
    SetLength(mismatched, 1);
    mismatched[0] := cTestSourceEntry;
  end;
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
  FManifestPackageId := 'Sample.Pkg';
  FManifestVersion := '1.2.3';
  FFormatNotSupported := false;
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
  FLogger := TTestLogger.Create;
  FLoggerRef := FLogger;
  logger := FLoggerRef;
  specReader := TPackageSpecReader.Create(logger);
  trustPolicy := TFakeTrustPolicy.Create(fingerprint);
  signingService := TFakeSigningService.Create(trustPolicy, FManifestPackageId, FManifestVersion, FFormatNotSupported);
  receiptService := TYamlReceiptService.Create;
  result := TPackageCache.Create(logger, specReader, signingService, trustPolicy, receiptService, nil, nil);
  result.Location := FCacheRoot;
end;

procedure TPackageCacheVerifyTests.WriteTestPackage(const fileName : string; const specVersion : string; const specCompiler : string);
var
  zip : TZipFile;
  spec : string;
begin
  ForceDirectories(ExtractFilePath(fileName));
  spec :=
    'metadata:' + sLineBreak +
    '  id: Sample.Pkg' + sLineBreak +
    '  version: ' + specVersion + sLineBreak +
    '  description: cache verify test package' + sLineBreak +
    '  authors:' + sLineBreak +
    '    - DPM' + sLineBreak +
    '  license: Apache-2.0' + sLineBreak +
    'targetPlatforms:' + sLineBreak +
    '  - compiler: ' + specCompiler + sLineBreak +
    '    platforms: [Win32]' + sLineBreak +
    '    template: default' + sLineBreak +
    'templates:' + sLineBreak +
    '  - name: default' + sLineBreak +
    '    source:' + sLineBreak +
    '      - src: Source\*.pas' + sLineBreak;
  zip := TZipFile.Create;
  try
    zip.Open(fileName, zmWrite);
    zip.Add(TEncoding.UTF8.GetBytes('{"files":[]}'), cManifestFileName);
    zip.Add(TEncoding.UTF8.GetBytes(spec), cPackageDspecFile);
    zip.Add(TEncoding.UTF8.GetBytes(cTestSourceContent), cTestSourceEntry);
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

procedure TPackageCacheVerifyTests.FullReVerify_RestoresModifiedFileAndKeepsBuildOutput;
var
  cache : IPackageCache;
  failures : integer;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  WriteBuildOutput;
  //edited while debugging with "use source", say.
  WriteText(TPath.Combine(FPackageFolder, cTestSourceFile), 'unit Sample.Unit; //edited');

  failures := cache.FullReVerify(TCancellationTokenSourceFactory.Create.Token);

  Assert.AreEqual(0, failures, 'the package is valid once its file is restored');
  Assert.AreEqual(cTestSourceContent, TFile.ReadAllText(TPath.Combine(FPackageFolder, cTestSourceFile)),
                  'the modified file should be restored from the .dpkg');
  AssertBuildOutputKept;
  Assert.IsTrue(FLogger.HasWarning(TWarningCode.W3018_CacheExtractionRepaired), 'restoring files should be reported');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_OnlyFullReVerifyChecksEveryFile;
var
  cache : IPackageCache;
begin
  //hashing every file on every cache hit would make each restore pay for it - that is cache verify's job.
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  WriteText(TPath.Combine(FPackageFolder, cTestSourceFile), 'unit Sample.Unit; //edited');

  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName));

  Assert.AreEqual('unit Sample.Unit; //edited', TFile.ReadAllText(TPath.Combine(FPackageFolder, cTestSourceFile)));
  Assert.IsFalse(FLogger.HasWarning(TWarningCode.W3018_CacheExtractionRepaired));
end;

procedure TPackageCacheVerifyTests.GitMarker_CountsOnlyWithoutAReceipt;
var
  cache : IPackageCache;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), 'precondition: the package installs into the cache');
  Assert.IsFalse(IsGitRegistryPackageFolder(FPackageFolder), 'no marker');

  //a marker in an extraction from a .dpkg (which always has a receipt) must not turn off its re-checks.
  WriteText(TPath.Combine(FPackageFolder, cGitPackageMarkerFile), 'abc');
  Assert.IsFalse(IsGitRegistryPackageFolder(FPackageFolder), 'marker beside a receipt');

  DeleteFile(TPath.Combine(FPackageFolder, cReceiptFileName));
  Assert.IsTrue(IsGitRegistryPackageFolder(FPackageFolder), 'marker without a receipt - a git registry install');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_ManifestOfAnotherVersion_FailsAndRemovesFile;
var
  cache : IPackageCache;
begin
  //a validly signed 1.2.2 renamed to stand in for 1.2.3 - the signatures vouch for the manifest, not the name.
  FManifestVersion := '1.2.2';
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7012_PackageIdentityMismatch), FLogger.Messages.Text);
  Assert.AreEqual(1, FLoggerRef.ErrorCount, FLogger.Messages.Text);
  Assert.IsFalse(FileExists(FPackageFileName), 'the misnamed package was left in the cache');
  Assert.IsFalse(FileExists(TPath.Combine(FPackageFolder, cPackageDspecFile)), 'the misnamed package was extracted');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_ManifestOfAnotherPackage_Fails;
var
  cache : IPackageCache;
begin
  FManifestPackageId := 'Other.Pkg';
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7012_PackageIdentityMismatch), FLogger.Messages.Text);
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_ManifestVersionMetadataIsIgnored;
var
  cache : IPackageCache;
begin
  //build metadata isn't part of a version's identity; neither is the id's case.
  FManifestPackageId := 'sample.pkg';
  FManifestVersion := '1.2.3+build.7';
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(FPackageFileName), FLogger.Messages.Text);
  Assert.AreEqual(0, FLoggerRef.ErrorCount, FLogger.Messages.Text);
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_DspecOfAnotherVersion_FailsAndRemovesExtraction;
var
  cache : IPackageCache;
begin
  //the dspec is what the rest of DPM reads - its dependencies above all.
  WriteTestPackage(FPackageFileName, '9.9.9');
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7012_PackageIdentityMismatch), FLogger.Messages.Text);
  Assert.AreEqual(1, FLoggerRef.ErrorCount, FLogger.Messages.Text);
  Assert.IsFalse(FileExists(FPackageFileName), 'the misnamed package was left in the cache');
  Assert.IsFalse(DirectoryExists(FPackageFolder), 'the misnamed package''s extraction was left in the cache');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_DspecForAnotherCompiler_Fails;
var
  cache : IPackageCache;
begin
  WriteTestPackage(FPackageFileName, '1.2.3', '11.0');
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7012_PackageIdentityMismatch), FLogger.Messages.Text);
  Assert.IsFalse(DirectoryExists(FPackageFolder), 'the misnamed package''s extraction was left in the cache');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_NewerFormat_IsE7109;
var
  cache : IPackageCache;
begin
  //the cause is an old client, not a bad package - say so, once.
  FFormatNotSupported := true;
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7109_PackageFormatNotSupported), FLogger.Messages.Text);
  Assert.AreEqual(1, FLoggerRef.ErrorCount, FLogger.Messages.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheVerifyTests);

end.
