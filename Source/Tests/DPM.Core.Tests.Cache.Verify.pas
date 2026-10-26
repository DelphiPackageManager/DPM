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
    function NewCache(const fingerprint : string; const logger : ILogger) : IPackageCache;
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

    //A .dpkg left from an earlier session is only used if it is still a package matching its .sha256 file.
    [Test]
    procedure EnsurePackage_RemovesALeftoverThatIsNotAPackage;
    [Test]
    procedure EnsurePackage_RemovesALeftoverNotMatchingItsHashFile;
    [Test]
    procedure EnsurePackage_UsesALeftoverMatchingItsHashFile;
    [Test]
    procedure InstallPackageFromFile_AFileThatFailsDoesNotReplaceTheCachedOne;
    [Test]
    procedure InstallPackageFromFile_RecordsTheHashOfThePackage;
    //the IDE and the CLI, or parallel builds, installing the same package into one cache.
    [Test]
    procedure InstallPackageFromFile_ConcurrentInstallsOfOnePackage_AllSucceed;
    //waiting for another process to finish with the package stops when the operation is cancelled.
    [Test]
    procedure InstallPackageFromFile_WaitingForAnotherProcess_StopsWhenCancelled;
    //what the IDE shows from the cache : only a finished install, never repaired, never waited for.
    [Test]
    procedure GetPackageMetadata_ReadsAFinishedInstall;
    [Test]
    procedure GetPackageMetadata_IgnoresAnInstallInProgress;
    [Test]
    procedure GetPackageMetadata_NeverRepairsTheCache;
    [Test]
    procedure GetPackageMetadata_DoesNotWaitForAnotherProcess;
  end;

implementation

uses
  WinApi.Windows,
  System.Classes,
  System.IOUtils,
  System.Types,
  System.SyncObjs,
  System.Diagnostics,
  System.Zip,
  VSoft.CancellationToken,
  VSoft.CancellationToken.Impl,
  DPM.Core.Diagnostics,
  DPM.Core.Constants,
  DPM.Core.Types,
  DPM.Core.Package.Interfaces,
  DPM.Core.Package.Classes,
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
begin
  FLogger := TTestLogger.Create;
  FLoggerRef := FLogger;
  result := NewCache(fingerprint, FLoggerRef);
end;

function TPackageCacheVerifyTests.NewCache(const fingerprint : string; const logger : ILogger) : IPackageCache;
var
  specReader : IPackageSpecReader;
  trustPolicy : ITrustPolicyService;
  signingService : IPackageSigningService;
  receiptService : IReceiptService;
begin
  //hold each dep in an interface local first: constructing an interfaced object directly in a
  //parameter position can leak in Delphi. No trust state - the TOFU ratchets aren't under test.
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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
  WriteText(TPath.Combine(FPackageFolder, cManifestFileName), '{"files":["tampered"]}');
  System.SysUtils.DeleteFile(TPath.Combine(FPackageFolder, cTestSourceFile));

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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
  //no receipt = the extraction never completed (it is written last).
  System.SysUtils.DeleteFile(TPath.Combine(FPackageFolder, cReceiptFileName));
  System.SysUtils.DeleteFile(TPath.Combine(FPackageFolder, cTestSourceFile));

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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
  WriteText(TPath.Combine(FPackageFolder, cTestSourceFile), 'unit Sample.Unit; //edited');

  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));

  Assert.AreEqual('unit Sample.Unit; //edited', TFile.ReadAllText(TPath.Combine(FPackageFolder, cTestSourceFile)));
  Assert.IsFalse(FLogger.HasWarning(TWarningCode.W3018_CacheExtractionRepaired));
end;

procedure TPackageCacheVerifyTests.GitMarker_CountsOnlyWithoutAReceipt;
var
  cache : IPackageCache;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
  Assert.IsFalse(IsGitRegistryPackageFolder(FPackageFolder), 'no marker');

  //a marker in an extraction from a .dpkg (which always has a receipt) must not turn off its re-checks.
  WriteText(TPath.Combine(FPackageFolder, cGitPackageMarkerFile), 'abc');
  Assert.IsFalse(IsGitRegistryPackageFolder(FPackageFolder), 'marker beside a receipt');

  System.SysUtils.DeleteFile(TPath.Combine(FPackageFolder, cReceiptFileName));
  Assert.IsTrue(IsGitRegistryPackageFolder(FPackageFolder), 'marker without a receipt - a git registry install');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_ManifestOfAnotherVersion_FailsAndRemovesFile;
var
  cache : IPackageCache;
begin
  //a validly signed 1.2.2 renamed to stand in for 1.2.3 - the signatures vouch for the manifest, not the name.
  FManifestVersion := '1.2.2';
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));
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
  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));
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
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), FLogger.Messages.Text);
  Assert.AreEqual(0, FLoggerRef.ErrorCount, FLogger.Messages.Text);
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_DspecOfAnotherVersion_FailsAndRemovesExtraction;
var
  cache : IPackageCache;
begin
  //the dspec is what the rest of DPM reads - its dependencies above all.
  WriteTestPackage(FPackageFileName, '9.9.9');
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));
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
  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));
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
  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName));
  Assert.IsTrue(FLogger.HasError(TErrorCode.E7109_PackageFormatNotSupported), FLogger.Messages.Text);
  Assert.AreEqual(1, FLoggerRef.ErrorCount, FLogger.Messages.Text);
end;

type
  //installs a package, as another process would.
  TInstallThread = class(TThread)
  private
    FCache : IPackageCache;
    FFileName : string;
    FLog : TTestLogger;
  protected
    procedure Execute; override;
  public
    constructor Create(const cache : IPackageCache; const fileName : string; const log : TTestLogger);
  end;

constructor TInstallThread.Create(const cache : IPackageCache; const fileName : string; const log : TTestLogger);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FCache := cache;
  FFileName := fileName;
  FLog := log;
end;

procedure TInstallThread.Execute;
begin
  try
    if FCache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FFileName) then
      FLog.Messages.Add('installed');
  except
    on e : Exception do
      FLog.Messages.Add('raised : ' + e.Message);
  end;
end;

type
  //holds the lock on a package folder, as another DPM process installing it would.
  THoldLockThread = class(TThread)
  private
    FLockName : string;
    FHeld : TEvent;
    FRelease : TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const lockName : string);
    destructor Destroy; override;
    property Held : TEvent read FHeld;
    property Release : TEvent read FRelease;
  end;

constructor THoldLockThread.Create(const lockName : string);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FLockName := lockName;
  FHeld := TEvent.Create(nil, true, false, '');
  FRelease := TEvent.Create(nil, true, false, '');
end;

destructor THoldLockThread.Destroy;
begin
  FHeld.Free;
  FRelease.Free;
  inherited;
end;

procedure THoldLockThread.Execute;
var
  lock : THandle;
begin
  lock := CreateMutex(nil, false, PChar(FLockName));
  if lock = 0 then
    exit;
  try
    if WaitForSingleObject(lock, 10000) <> WAIT_OBJECT_0 then
      exit;
    FHeld.SetEvent;
    FRelease.WaitFor(60000);
    ReleaseMutex(lock);
  finally
    CloseHandle(lock);
  end;
end;

//the name TPackageCache locks a package folder under - one mutex per folder, shared by every DPM process.
function PackageLockName(const packageFolder : string) : string;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.Create(TEncoding.UTF8.GetBytes(LowerCase(ExcludeTrailingPathDelimiter(ExpandFileName(packageFolder)))));
  try
    result := 'Local\DPM.Cache.' + Copy(THashSHA256.GetHashString(stream), 1, 16);
  finally
    stream.Free;
  end;
end;

function TestIdentity : IPackageIdentity;
begin
  result := TPackageIdentity.Create('', 'Sample.Pkg', TPackageVersion.Parse('1.2.3'), TCompilerVersion.Delphi12_0);
end;

//copies of a package left part way - none may stay behind.
function TempCopiesIn(const folder : string) : integer;
begin
  result := Length(TDirectory.GetFiles(folder, '*.tmp-*'));
end;

procedure TPackageCacheVerifyTests.EnsurePackage_RemovesALeftoverThatIsNotAPackage;
var
  cache : IPackageCache;
begin
  //what a failed download saved under the package's name.
  TFile.WriteAllText(FPackageFileName, '<html><body>502 Bad Gateway</body></html>');
  TFile.WriteAllText(FPackageFileName + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(FPackageFileName));
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.EnsurePackage(TCancellationTokenSourceFactory.Create.Token, TestIdentity), 'an error page is not a package');
  Assert.AreEqual(0, FLoggerRef.ErrorCount, 'the package is downloaded again - not an error : ' + FLogger.Messages.Text);
  Assert.IsFalse(FileExists(FPackageFileName), 'the error page was left in the cache');
  Assert.IsFalse(FileExists(FPackageFileName + cPackageHashAlgorithmExt), 'its hash file was left in the cache');
end;

procedure TPackageCacheVerifyTests.EnsurePackage_RemovesALeftoverNotMatchingItsHashFile;
var
  cache : IPackageCache;
begin
  TFile.WriteAllText(FPackageFileName + cPackageHashAlgorithmExt, StringOfChar('a', 64));
  cache := MakeCache('policy-a');
  Assert.IsFalse(cache.EnsurePackage(TCancellationTokenSourceFactory.Create.Token, TestIdentity));
  Assert.AreEqual(0, FLoggerRef.ErrorCount, FLogger.Messages.Text);
  Assert.IsFalse(FileExists(FPackageFileName), 'a package that is not what was recorded was left in the cache');
  Assert.IsFalse(DirectoryExists(FPackageFolder), 'it was extracted');
end;

procedure TPackageCacheVerifyTests.EnsurePackage_UsesALeftoverMatchingItsHashFile;
var
  cache : IPackageCache;
begin
  TFile.WriteAllText(FPackageFileName + cPackageHashAlgorithmExt, THashSHA256.GetHashStringFromFile(FPackageFileName) + sLineBreak);
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.EnsurePackage(TCancellationTokenSourceFactory.Create.Token, TestIdentity), FLogger.Messages.Text);
  Assert.IsTrue(FileExists(TPath.Combine(FPackageFolder, cPackageDspecFile)), 'it was not extracted');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_AFileThatFailsDoesNotReplaceTheCachedOne;
var
  cache : IPackageCache;
  goodHash : string;
  otherFile : string;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), 'precondition: the package installs into the cache');
  goodHash := THashSHA256.GetHashStringFromFile(FPackageFileName);
  //the same name, from somewhere else, and broken.
  otherFile := TPath.Combine(TPath.Combine(FCacheRoot, 'elsewhere'), cTestPackageFile);
  ForceDirectories(ExtractFilePath(otherFile));
  TFile.WriteAllText(otherFile, 'not a package');

  Assert.IsFalse(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, otherFile));
  Assert.AreEqual(goodHash, THashSHA256.GetHashStringFromFile(FPackageFileName), 'the cached package was overwritten by one that failed');
  Assert.AreEqual(0, TempCopiesIn(ExtractFilePath(FPackageFileName)), 'the copy that failed was left in the cache');
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_RecordsTheHashOfThePackage;
var
  cache : IPackageCache;
begin
  //left from an older copy of the package - the SBOM reads it.
  TFile.WriteAllText(FPackageFileName + cPackageHashAlgorithmExt, StringOfChar('a', 64));
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), FLogger.Messages.Text);
  Assert.AreEqual(THashSHA256.GetHashStringFromFile(FPackageFileName), Trim(TFile.ReadAllText(FPackageFileName + cPackageHashAlgorithmExt)));
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_ConcurrentInstallsOfOnePackage_AllSucceed;
const
  cRounds = 6;
var
  loggers : array[0..1] of TTestLogger;
  loggerRefs : array[0..1] of ILogger;
  caches : array[0..1] of IPackageCache;
  threads : array[0..1] of TInstallThread;
  round, i : integer;
begin
  for i := 0 to 1 do
  begin
    loggers[i] := TTestLogger.Create;
    loggerRefs[i] := loggers[i];
    caches[i] := NewCache('policy-a', loggerRefs[i]);
  end;
  for round := 1 to cRounds do
  begin
    //nothing extracted - both have to extract it.
    if DirectoryExists(FPackageFolder) then
      TDirectory.Delete(FPackageFolder, true);
    for i := 0 to 1 do
      threads[i] := TInstallThread.Create(caches[i], FPackageFileName, loggers[i]);
    try
      for i := 0 to 1 do
        threads[i].Start;
      for i := 0 to 1 do
        threads[i].WaitFor;
    finally
      for i := 0 to 1 do
        threads[i].Free;
    end;
    for i := 0 to 1 do
    begin
      Assert.IsTrue(loggers[i].Messages.IndexOf('installed') >= 0, Format('round %d, install %d : %s', [round, i, loggers[i].Messages.Text]));
      loggers[i].Messages.Clear;
      Assert.AreEqual(0, loggerRefs[i].ErrorCount, loggers[i].Messages.Text);
    end;
    Assert.AreEqual(cTestSourceContent, TFile.ReadAllText(TPath.Combine(FPackageFolder, cTestSourceFile)), 'the extraction is not whole');
  end;
end;

procedure TPackageCacheVerifyTests.InstallPackageFromFile_WaitingForAnotherProcess_StopsWhenCancelled;
var
  holder : THoldLockThread;
  cache : IPackageCache;
  cancellation : ICancellationTokenSource;
  stopwatch : TStopwatch;
  installed : boolean;
begin
  cache := MakeCache('policy-a');
  cancellation := TCancellationTokenSourceFactory.Create;
  holder := THoldLockThread.Create(PackageLockName(FPackageFolder));
  try
    holder.Start;
    Assert.AreEqual(Ord(wrSignaled), Ord(holder.Held.WaitFor(10000)), 'the other process did not take the lock');
    //Ctrl+C a moment after the install starts waiting.
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(300);
        cancellation.Cancel;
      end).Start;
    stopwatch := TStopwatch.StartNew;
    installed := cache.InstallPackageFromFile(cancellation.Token, FPackageFileName);
    stopwatch.Stop;
    Assert.IsFalse(installed, 'it installed while another process held the package');
    Assert.IsTrue(stopwatch.ElapsedMilliseconds < 5000, Format('cancelling took %d ms to stop the wait', [stopwatch.ElapsedMilliseconds]));
    Assert.AreEqual(0, FLoggerRef.ErrorCount, 'a cancelled wait reports nothing - whoever cancelled says so : ' + FLogger.Messages.Text);
  finally
    holder.Release.SetEvent;
    holder.WaitFor;
    holder.Free;
  end;
  //and once the other process is done with it, it installs.
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), FLogger.Messages.Text);
end;

procedure TPackageCacheVerifyTests.GetPackageMetadata_ReadsAFinishedInstall;
var
  cache : IPackageCache;
  metadata : IPackageMetadata;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), FLogger.Messages.Text);
  metadata := MakeCache('policy-a').GetPackageMetadata(TestIdentity);
  Assert.IsNotNull(metadata, 'a finished install should be shown');
  Assert.AreEqual('Sample.Pkg', metadata.Id);
end;

procedure TPackageCacheVerifyTests.GetPackageMetadata_IgnoresAnInstallInProgress;
begin
  //what another process has extracted so far - its receipt is written last.
  ForceDirectories(FPackageFolder);
  TFile.WriteAllText(TPath.Combine(FPackageFolder, cPackageDspecFile), 'metadata:' + sLineBreak + '  id: Sample.Pkg');
  Assert.IsNull(MakeCache('policy-a').GetPackageMetadata(TestIdentity), 'an install in progress is not something to show');
  Assert.AreEqual(0, FLoggerRef.ErrorCount, FLogger.Messages.Text);
end;

procedure TPackageCacheVerifyTests.GetPackageMetadata_NeverRepairsTheCache;
begin
  //only the .dpkg is there - extracting and verifying it is an install's job, not a display's.
  Assert.IsNull(MakeCache('policy-a').GetPackageMetadata(TestIdentity));
  Assert.IsFalse(DirectoryExists(FPackageFolder), 'reading for display extracted the package');
end;

procedure TPackageCacheVerifyTests.GetPackageMetadata_DoesNotWaitForAnotherProcess;
var
  cache : IPackageCache;
  holder : THoldLockThread;
  stopwatch : TStopwatch;
begin
  cache := MakeCache('policy-a');
  Assert.IsTrue(cache.InstallPackageFromFile(TCancellationTokenSourceFactory.Create.Token, FPackageFileName), FLogger.Messages.Text);
  cache := MakeCache('policy-a');
  holder := THoldLockThread.Create(PackageLockName(FPackageFolder));
  try
    holder.Start;
    Assert.AreEqual(Ord(wrSignaled), Ord(holder.Held.WaitFor(10000)), 'the other process did not take the lock');
    stopwatch := TStopwatch.StartNew;
    Assert.IsNotNull(cache.GetPackageMetadata(TestIdentity));
    stopwatch.Stop;
    Assert.IsTrue(stopwatch.ElapsedMilliseconds < 1000, Format('the display read waited %d ms', [stopwatch.ElapsedMilliseconds]));
  finally
    holder.Release.SetEvent;
    holder.WaitFor;
    holder.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPackageCacheVerifyTests);

end.
