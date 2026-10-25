{***************************************************************************}
{                                                                           }
{           Delphi Package Manager - DPM                                    }
{                                                                           }
{           Copyright � 2019 Vincent Parrett and contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DPM.Core.Cache;

interface

uses
  System.SyncObjs,
  Spring.Collections,
  VSoft.CancellationToken,
  DPM.Core.Types,
  DPM.Core.Logging,
  DPM.Core.Dependency.Version,
  DPM.Core.Package.Interfaces,
  DPM.Core.Cache.Interfaces,
  DPM.Core.Spec.Interfaces,
  DPM.Core.Trust.Interfaces,
  DPM.Core.Trust.Prompt,
  DPM.Core.Crypto.X509.Interfaces,
  DPM.Core.Package.Cache.Receipt,
  DPM.Core.Package.Signing.Interfaces;

type
  TPackageCache = class(TInterfacedObject, IPackageCache)
  private
    FLogger : ILogger;
    FSpecReader : IPackageSpecReader;
    FSigningService : IPackageSigningService;
    FTrustPolicy : ITrustPolicyService;
    FReceiptService : IReceiptService;
    FTrustState : ITrustStateService;
    FTrustPrompt : ITrustPromptStrategy;
    FLocation : string;
    // When set, every TOFU ratchet evaluation is skipped (test cache builds).
    FSkipTrustRatchets : boolean;
    //Process-level memo. Safe because the cached spec is write-once per (id, compiler, version).
    //TPackageCache is registered AsSingleton in DPM.Core.Init.pas so these survive the whole command
    //and dedupe across projects in a group restore as well as IDE operations.
    FInfoCache : IDictionary<string, IPackageInfo>;
    FSpecCache : IDictionary<string, IPackageSpec>;
    FExtractionVerified : ISet<string>;
    FCacheLock : TCriticalSection;
    //set while FullReVerify runs - an extraction that is kept is then checked file by file.
    FDeepVerify : boolean;
    procedure WriteReceipt(const packageFolder : string;
                           const packageId : IPackageIdentity;
                           const verifyResult : TVerificationResult);
    //true when packageFolder already holds a completed extraction of the package verifyResult describes.
    function ExtractionMatchesPackage(const packageFolder : string; const verifyResult : TVerificationResult) : boolean;
    function ExtractedFilesIntact(const logger : ILogger; const packageFilePath : string; const packageFolder : string) : boolean;
    // Returns true if the install may proceed. When false, the package must
    // not be extracted. Records the new author state on a successful pass.
    function EvaluateAuthorDowngrade(const packageId : IPackageIdentity;
                                      const verifyResult : TVerificationResult;
                                      const policy : TTrustPolicy) : boolean;
    // P2 §2.3 (V-24): repository assurance ratchet. Returns true if the
    // install may proceed. Hard-fails when a previously seen trusted-repo
    // signature is now missing or the namespace has changed.
    // `hadPriorAuthor`/`priorAuthor` must be captured by the caller *before*
    // EvaluateAuthorDowngrade runs — see TAuthorRebuildExemption.
    function EvaluateRepositoryRatchet(const packageId : IPackageIdentity;
                                        const verifyResult : TVerificationResult;
                                        hadPriorAuthor : boolean;
                                        const priorAuthor : TAuthorTrustEntry) : boolean;
    // Materialise a TVerificationResult from a cached receipt so the trust-
    // state ratchets can be re-evaluated on a cache hit without re-extracting.
    // Trust-set membership (PublisherTrusted/RepositoryTrusted) is re-checked
    // against the *current* policy — the receipt is only the signature roster.
    function BuildResultFromReceipt(const receipt : TVerificationReceipt) : TVerificationResult;
    //InstallPackageFromFile, reporting through logger - a TDemotingLogger when trying a .dpkg left on disk,
    //whose problems are not the operation's because the package is downloaded again.
    function DoInstallPackageFromFile(const logger : ILogger; const packageFileName : string; const skipTrustRatchets : boolean) : boolean;
  protected
    procedure SetLocation(const value : string);
    function GetLocation : string;
    function GetPackagesFolder : string;

    function CachePackage(const packageId : IPackageIdentity; const saveFile : Boolean) : Boolean;
    function Clean : Boolean;
    function CreatePackagePath(const packageId : IPackageIdentity) : string;

    function GetPackagePath(const packageId : IPackageIdentity) : string; overload;
    function GetPackagePath(const id : string; const version : string; const compilerVersion : TCompilerVersion) : string;overload;

    function GetPackageFileFolder(const packageId : IPackageIdentity) : string;

    function EnsurePackage(const packageId : IPackageIdentity) : Boolean;

    function GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;

    function GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;

    function GetPackageSpec(const packageId : IPackageIdentity) : IPackageSpec;

    function GetPackagePlatforms(const packageId : IPackageIdentity) : TDPMPlatforms;

    function HasSource(const packageId : IPackageIdentity) : boolean;

    function GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                      const id : string;
                                                      const compilerVersion : TCompilerVersion;
                                                      const versionRange : TVersionRange;
                                                      const preRelease : boolean) : IList<IPackageInfo>;

    function GetPackageHash(const packageId : IPackageIdentity) : string;

    function TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;

//    function InstallPackage(const packageId : IPackageIdentity; const saveFile : boolean; const source : string = '') : boolean;

    function InstallPackageFromFile(const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;

    procedure SetSkipTrustRatchets(const value : boolean);

    function FullReVerify(const cancellationToken : ICancellationToken) : integer;

    function GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
    function RemovePackage(const packageId : IPackageIdentity) : boolean;

  public
    constructor Create(const logger : ILogger;
                       const specReader : IPackageSpecReader;
                       const signingService : IPackageSigningService;
                       const trustPolicy : ITrustPolicyService;
                       const receiptService : IReceiptService;
                       const trustState : ITrustStateService;
                       const trustPrompt : ITrustPromptStrategy);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.DateUtils,
  System.RegularExpressions,
  System.Generics.Defaults,
  DPM.Core.Constants,
  DPM.Core.Diagnostics,
  DPM.Core.Package.Archive,
  DPM.Core.Package.Classes,
  DPM.Core.Package.Icon,
  DPM.Core.Package.Manifest.Interfaces,
  DPM.Core.Utils.Hash,
  DPM.Core.Utils.System,
  DPM.Core.Utils.Strings;

//A package id is a folder name in the cache, and it comes from wherever the package did - a feed's json,
//a package's own spec. Anything but a package id's shape - words separated by dots, so never a separator,
//drive or '..' ('..\..\x', 'c:\x') - would put the package, and everything DPM later reads and builds
//from it, outside the cache.
function PackageFolderName(const id : string) : string;
begin
  if not TRegEx.IsMatch(id, cPackageIdRegex) then
    raise EDPMError.Create(TErrorCode.E3017_InvalidPackageId, 'The package id [' + id + '] is not a valid package id - it can''t be used in the package cache.');
  result := id;
end;

//'' when the identity a package declares - in its manifest or its dspec - is the one it is installed as,
//the one in its file name. Otherwise what differs.
function DeclaredIdentityProblem(const packageIdentity : IPackageIdentity; const declaredIn : string;
                                 const id : string; const version : TPackageVersion) : string;
begin
  result := '';
  if not SameText(id, packageIdentity.Id) then
    result := Format('its %s says it is [%s]', [declaredIn, id])
  else if version <> packageIdentity.Version then
    result := Format('its %s says it is version [%s]', [declaredIn, version.ToStringNoMeta]);
end;

//A package that isn't what its name says is never installed - a validly signed old version renamed to
//stand in for the current one, say. The file is removed so the next restore fetches the package again.
procedure RejectMisnamedPackage(const logger : ILogger; const packageFilePath : string; const packageIdentity : IPackageIdentity;
                                const problem : string);
begin
  logger.Error(TErrorCode.E7012_PackageIdentityMismatch, Format('The package file [%s] is not %s %s - %s.',
    [ExtractFileName(packageFilePath), packageIdentity.Id, packageIdentity.Version.ToStringNoMeta, problem]));
  if FileExists(packageFilePath) and (not DeleteFile(packageFilePath)) then
    logger.Verbose('Unable to remove [' + packageFilePath + '] from the package cache : ' + SysErrorMessage(GetLastError));
end;

//an extraction that can't be used - leave nothing behind for the cache-hit checks to find.
procedure RemoveExtraction(const logger : ILogger; const packageFolder : string);
begin
  try
    TDirectory.Delete(packageFolder, true);
  except
    on e : Exception do
      logger.Verbose('Unable to remove the extracted package folder [' + packageFolder + '] : ' + e.Message);
  end;
end;

{ TPackageCache }

function TPackageCache.FullReVerify(const cancellationToken : ICancellationToken) : integer;
var
  packagesRoot : string;
  compilerFolders : TStringDynArray;
  idFolders : TStringDynArray;
  dpkgFiles : TStringDynArray;
  allPackages : IList<string>;
  i, j, k : integer;
  total : integer;
  failures : integer;
  cancelled : boolean;
begin
  failures := 0;
  if FSigningService = nil then
  begin
    FLogger.Error(TErrorCode.E0003_InternalError, 'No signing service is available to verify the package cache with.');
    result := 0;
    exit;
  end;

  packagesRoot := GetPackagesFolder;
  if not DirectoryExists(packagesRoot) then
  begin
    FLogger.Information('[PackageCache] Cache folder does not exist - nothing to verify.', true);
    result := 0;
    exit;
  end;

  FLogger.Information('[PackageCache] Re-verifying every package in ' + packagesRoot, true);

  // Invalidate the in-memory "already verified this session" set so the
  // re-verify path actually re-runs the workflow rather than short-circuiting.
  FCacheLock.Enter;
  try
    FExtractionVerified.Clear;
  finally
    FCacheLock.Leave;
  end;

  // Enumerate every .dpkg up front. Knowing the total lets us log "[i/total]"
  // progress so a UI host can show how far through the (potentially slow) verify
  // we are, and report how many were done if the user cancels partway.
  allPackages := TCollections.CreateList<string>;
  compilerFolders := TDirectory.GetDirectories(packagesRoot);
  for i := 0 to High(compilerFolders) do
  begin
    idFolders := TDirectory.GetDirectories(compilerFolders[i]);
    for j := 0 to High(idFolders) do
    begin
      // Every cached id folder holds its .dpkg files sibling to the per-version
      // extraction folders. Re-installing the .dpkg re-runs the full
      // verification workflow inside InstallPackageFromFile.
      dpkgFiles := TDirectory.GetFiles(idFolders[j], '*' + cPackageFileExt,
        TSearchOption.soTopDirectoryOnly);
      for k := 0 to High(dpkgFiles) do
        allPackages.Add(dpkgFiles[k]);
    end;
  end;

  total := allPackages.Count;
  if total = 0 then
  begin
    FLogger.Information('[PackageCache] No cached packages found to verify.', true);
    result := 0;
    exit;
  end;

  cancelled := false;
  //re-hash the files of every extraction that is kept, not just its manifest.
  FDeepVerify := true;
  try
    for i := 0 to total - 1 do
    begin
      // Cooperative cancellation - checked between packages. The per-package work in
      // InstallPackageFromFile is not itself interruptible, so the cancel takes effect
      // at the next package boundary rather than instantly.
      if cancellationToken.IsCancelled then
      begin
        cancelled := true;
        break;
      end;
      // Important so it shows regardless of verbosity, and (in the IDE host) pumps the
      // message window so the log stays live and the Cancel button can be clicked.
      FLogger.Information(Format('[PackageCache] Verifying [%d/%d] %s',
        [i + 1, total, ExtractFileName(allPackages[i])]), true);
      try
        //what is wrong with it was reported by InstallPackageFromFile.
        if not InstallPackageFromFile(allPackages[i]) then
          Inc(failures);
      except
        on e : Exception do
        begin
          Inc(failures);
          FLogger.Error(ErrorCodeOf(e, TErrorCode.E7001_PackageVerificationFailed), 'Unable to re-verify [' +
            allPackages[i] + '] : ' + e.Message);
        end;
      end;
    end;
  finally
    FDeepVerify := false;
  end;

  if cancelled then
    FLogger.Information(Format('[PackageCache] FullReVerify cancelled after %d of %d package(s); %d failure(s) so far.',
      [i, total, failures]), true)
  else if failures = 0 then
    FLogger.Success(Format('[PackageCache] FullReVerify completed: all %d package(s) valid.', [total]), true)
  else
    FLogger.Information(Format('[PackageCache] FullReVerify completed with %d failure(s) of %d package(s).',
      [failures, total]), true);
  result := failures;
end;

function TPackageCache.EvaluateRepositoryRatchet(const packageId : IPackageIdentity;
                                                  const verifyResult : TVerificationResult;
                                                  hadPriorAuthor : boolean;
                                                  const priorAuthor : TAuthorTrustEntry) : boolean;
var
  i : integer;
  prior : TRepositoryTrustEntry;
  hadPrior : boolean;
  currentTrustedRepoSpki : string;
  currentAttestedNamespace : string;
  hasCurrentTrustedRepo : boolean;
  newEntry : TRepositoryTrustEntry;
  context : TRepoTrustPromptContext;
  decision : TTrustPromptDecision;
  ratchetFailure : string;
  missingRepoSignature : boolean;
begin
  result := true;
  if (FTrustState = nil) or (packageId = nil) then
    exit;

  hasCurrentTrustedRepo := false;
  currentTrustedRepoSpki := '';
  currentAttestedNamespace := '';

  // First valid trusted repository signature on this build (its attestation
  // — if present — sets the namespace we ratchet against).
  for i := 0 to High(verifyResult.Signatures) do
  begin
    if (verifyResult.Signatures[i].Role = srRepository) and
       verifyResult.Signatures[i].Valid and
       verifyResult.Signatures[i].RepositoryTrusted then
    begin
      hasCurrentTrustedRepo := true;
      currentTrustedRepoSpki := verifyResult.Signatures[i].SignerSpkiHex;
      if verifyResult.Signatures[i].Attestation.Present then
        currentAttestedNamespace := verifyResult.Signatures[i].Attestation.Namespace;
      break;
    end;
  end;

  hadPrior := FTrustState.TryGetRepository(packageId.Id, prior);

  if not hadPrior then
  begin
    // First time seeing this id — if it carries a trusted repo signature,
    // record the high-water mark. If not, nothing to do.
    if hasCurrentTrustedRepo then
    begin
      newEntry.TrustedRepoSpkiHex := currentTrustedRepoSpki;
      newEntry.Namespace := currentAttestedNamespace;
      newEntry.FirstSeenAt := TTimeZone.Local.ToUniversalTime(Now);
      newEntry.LastSeenAt := newEntry.FirstSeenAt;
      FTrustState.RecordRepository(packageId.Id, newEntry);
    end;
    exit;
  end;

  // V-24: once seen carrying a trusted-repo signature, any later build
  // lacking one or attesting a different namespace would normally be a hard
  // fail. We now prompt the user (when a UI is wired) so dev workflows can
  // recover without hand-editing trust-state.yaml — the non-interactive
  // strategy still fails closed for CI.
  ratchetFailure := '';
  missingRepoSignature := not hasCurrentTrustedRepo;
  if missingRepoSignature then
    ratchetFailure := Format(
      'The package [%s] previously carried a trusted repository ' +
      'signature (sha256:%s) but this build does not.',
      [packageId.Id, prior.TrustedRepoSpkiHex])
  else if (prior.Namespace <> '') and (currentAttestedNamespace <> '') and
          not SameText(prior.Namespace, currentAttestedNamespace) then
    ratchetFailure := Format(
      'The repository attestation namespace of the package [%s] ' +
      'changed from "%s" to "%s".',
      [packageId.Id, prior.Namespace, currentAttestedNamespace]);

  if ratchetFailure <> '' then
  begin
    // Author-rebuild exemption. The author who published this id is entitled to
    // install their own locally built copy, which carries no gallery
    // countersignature until it is pushed. Scoped to the missing-signature case
    // only: a *different* trusted repository attesting a different namespace is
    // a distinct situation and still prompts.
    if missingRepoSignature and
       TAuthorRebuildExemption.Applies(hadPriorAuthor, priorAuthor, verifyResult.Signatures) then
    begin
      FLogger.Warning(TWarningCode.W7007_AuthorRebuildAccepted, Format(
        'The package [%s] carries no trusted repository signature, but is ' +
        'author-signed by the same key previously recorded for it (sha256:%s). Treating ' +
        'this as an author rebuild and allowing the install.',
        [packageId.Id, priorAuthor.LastAuthorSpkiHex]));
      // Deliberately do NOT ratchet forward: TrustedRepoSpkiHex and Namespace must
      // survive untouched so the next build from a repository is still required to
      // carry the signature. Only the freshness stamp moves.
      newEntry := prior;
      newEntry.LastSeenAt := TTimeZone.Local.ToUniversalTime(Now);
      FTrustState.RecordRepository(packageId.Id, newEntry);
      result := true;
      exit;
    end;

    FLogger.Warning(TWarningCode.W7005_RepositorySignatureChanged, ratchetFailure);

    if FTrustPrompt = nil then
    begin
      FLogger.Error(TErrorCode.E7004_RepositorySignatureBlocked, 'The install of [' + packageId.Id + '] was blocked - it no longer carries the repository signature ' +
                    'previously seen for it, and there is no way to ask whether to continue. Edit %APPDATA%\.dpm\trust-state.yaml to override.');
      result := false;
      exit;
    end;

    context.PackageId           := packageId.Id;
    context.Version             := packageId.Version.ToStringNoMeta;
    context.PreviousRepoSpkiHex := prior.TrustedRepoSpkiHex;
    context.PreviousNamespace   := prior.Namespace;
    context.NewHasTrustedRepo   := hasCurrentTrustedRepo;
    context.NewRepoSpkiHex      := currentTrustedRepoSpki;
    context.NewNamespace        := currentAttestedNamespace;
    FTrustPrompt.PromptRepositoryRatchet(context, decision);
    case decision of
      tpdOverride :
        begin
          FTrustState.RemoveRepository(packageId.Id);
          FLogger.Information('The repository signature change of [' +
            packageId.Id + '] was accepted - its previous repository trust entry was removed.', true);
          result := true;
          exit;
        end;
      tpdBlockOnce :
        begin
          // Reached either when the user explicitly declined an interactive prompt
          // or - more commonly in headless/IDE contexts - when the non-interactive
          // strategy fails closed. "User cancelled" is misleading in the latter
          // case, so state the actual outcome.
          FLogger.Error(TErrorCode.E7004_RepositorySignatureBlocked, 'The install of [' +
            packageId.Id + '] was blocked - this build no longer carries the ' +
            'previously seen trusted repository signature.');
          result := false;
          exit;
        end;
      tpdBlockAlways :
        begin
          FLogger.Error(TErrorCode.E7002_PackageBlocked, 'The install of [' +
            packageId.Id + '] was blocked, and the package is now permanently blocked - it no longer carries its repository signature.');
          FTrustState.BlockPermanently(packageId.Id);
          result := false;
          exit;
        end;
    end;
  end;

  // No regression — update the high-water mark and ratchet forward.
  newEntry := prior;
  newEntry.TrustedRepoSpkiHex := currentTrustedRepoSpki;
  if currentAttestedNamespace <> '' then
    newEntry.Namespace := currentAttestedNamespace;
  newEntry.LastSeenAt := TTimeZone.Local.ToUniversalTime(Now);
  FTrustState.RecordRepository(packageId.Id, newEntry);
end;

function TPackageCache.BuildResultFromReceipt(const receipt : TVerificationReceipt) : TVerificationResult;
var
  i : integer;
  effectivePolicy : TTrustPolicy;
begin
  effectivePolicy := FTrustPolicy.GetEffectivePolicy;

  if SameText(receipt.TrustDecision, 'trusted') then
    result.Outcome := voTrusted
  else if SameText(receipt.TrustDecision, 'unsigned') then
    result.Outcome := voUnsigned
  else if SameText(receipt.TrustDecision, 'untrusted-publisher') then
    result.Outcome := voUntrustedPublisher
  else
    result.Outcome := voInvalid;

  result.ManifestHashAlgorithm := receipt.ManifestHashAlgorithm;
  result.ManifestHashHex := receipt.ManifestHashHex;
  result.PolicyFingerprint := receipt.TrustPolicyFingerprint;
  result.Reason := '';

  SetLength(result.Signatures, Length(receipt.Signatures));
  for i := 0 to High(receipt.Signatures) do
  begin
    if SameText(receipt.Signatures[i].Role, 'repository') then
      result.Signatures[i].Role := srRepository
    else
      result.Signatures[i].Role := srAuthor;

    result.Signatures[i].SignerSpkiHex := receipt.Signatures[i].SignerSpkiHex;
    result.Signatures[i].SignerSubject := receipt.Signatures[i].SignerSubject;
    result.Signatures[i].Thumbprint := receipt.Signatures[i].Thumbprint;
    result.Signatures[i].EffectiveSigningTime := receipt.Signatures[i].EffectiveSigningTime;
    result.Signatures[i].TimestampAuthority := receipt.Signatures[i].TimestampAuthority;
    // The receipt records every signature, and why one failed. A receipt that records a
    // failure never reaches here (QuickRecheck sends the package back for a full verify), but
    // the ratchets pick the first *valid* signature - never let a failed one count.
    result.Signatures[i].Valid := receipt.Signatures[i].FailureReason = '';
    result.Signatures[i].FailureReason := receipt.Signatures[i].FailureReason;

    // Re-check trust set against the CURRENT policy. If the user has since
    // removed a SPKI from trustedRepositories, the repo ratchet now sees
    // it as untrusted and reacts accordingly.
    if result.Signatures[i].Role = srAuthor then
      result.Signatures[i].PublisherTrusted := FTrustPolicy.PublisherTrusted(effectivePolicy, result.Signatures[i].SignerSpkiHex)
    else
      result.Signatures[i].RepositoryTrusted := FTrustPolicy.RepositoryTrusted(effectivePolicy, result.Signatures[i].SignerSpkiHex);

    result.Signatures[i].Attestation.Present := receipt.Signatures[i].AttestationNamespace <> '';
    result.Signatures[i].Attestation.Namespace := receipt.Signatures[i].AttestationNamespace;
    result.Signatures[i].Attestation.AuthorSpkiHex := receipt.Signatures[i].AttestationAuthorSpkiHex;
  end;
end;

procedure TPackageCache.WriteReceipt(const packageFolder : string;
                                      const packageId : IPackageIdentity;
                                      const verifyResult : TVerificationResult);
var
  receipt : TVerificationReceipt;
  i : integer;
  outcomeStr : string;
begin
  receipt.ReceiptVersion := cCurrentReceiptVersion;
  receipt.PackageId := packageId.Id;
  receipt.Version := packageId.Version.ToStringNoMeta;
  receipt.Compiler := CompilerToString(packageId.CompilerVersion);
  receipt.ManifestHashAlgorithm := verifyResult.ManifestHashAlgorithm;
  receipt.ManifestHashHex := verifyResult.ManifestHashHex;
  receipt.TrustPolicyFingerprint := verifyResult.PolicyFingerprint;
  receipt.VerifiedAt := TTimeZone.Local.ToUniversalTime(Now);
  receipt.DpmVersion := TSystemUtils.GetVersionString;

  case verifyResult.Outcome of
    voTrusted             : outcomeStr := 'trusted';
    voUnsigned            : outcomeStr := 'unsigned';
    voUntrustedPublisher  : outcomeStr := 'untrusted-publisher';
  else
    outcomeStr := 'invalid';
  end;
  receipt.TrustDecision := outcomeStr;

  SetLength(receipt.Signatures, Length(verifyResult.Signatures));
  for i := 0 to High(verifyResult.Signatures) do
  begin
    if verifyResult.Signatures[i].Role = srRepository then
      receipt.Signatures[i].Role := 'repository'
    else
      receipt.Signatures[i].Role := 'author';
    receipt.Signatures[i].SignerSpkiHex := verifyResult.Signatures[i].SignerSpkiHex;
    receipt.Signatures[i].SignerSubject := verifyResult.Signatures[i].SignerSubject;
    // DIAGNOSTIC (signature-verify investigation): persist why a signature
    // failed to verify so the receipt on disk explains a degraded result.
    receipt.Signatures[i].FailureReason := verifyResult.Signatures[i].FailureReason;
    receipt.Signatures[i].Thumbprint := verifyResult.Signatures[i].Thumbprint;
    receipt.Signatures[i].EffectiveSigningTime := verifyResult.Signatures[i].EffectiveSigningTime;
    receipt.Signatures[i].TimestampAuthority := verifyResult.Signatures[i].TimestampAuthority;
    // V-26 revocation outcome captured during chain build at signing time.
    case verifyResult.Signatures[i].Revocation of
      rsGood       : receipt.Signatures[i].RevocationStatus := 'good';
      rsRevoked    : receipt.Signatures[i].RevocationStatus := 'revoked';
      rsUnknown    : receipt.Signatures[i].RevocationStatus := 'unknown';
    else
      receipt.Signatures[i].RevocationStatus := 'notChecked';
    end;
    // P2 §2.2 — attestation block is only set when present on this signature
    // (the verifier only populates it for trusted-repo sigs).
    if verifyResult.Signatures[i].Attestation.Present then
    begin
      receipt.Signatures[i].AttestationNamespace := verifyResult.Signatures[i].Attestation.Namespace;
      receipt.Signatures[i].AttestationAuthorSpkiHex := verifyResult.Signatures[i].Attestation.AuthorSpkiHex;
      case verifyResult.Signatures[i].Attestation.UnsignedReason of
        urAttestNeverSigned          : receipt.Signatures[i].AttestationUnsignedReason := 'neverSigned';
        urAttestAuthorCeasedSigning  : receipt.Signatures[i].AttestationUnsignedReason := 'authorCeasedSigning';
      else
        receipt.Signatures[i].AttestationUnsignedReason := '';
      end;
    end;
  end;

  try
    FReceiptService.Write(packageFolder, receipt);
  except
    on e : Exception do
      FLogger.Warning(TWarningCode.W3015_CacheReceiptWriteFailed, 'Unable to write the verification receipt for [' + packageId.ToString + '] : ' + e.Message);
  end;
end;

//The receipt is written last, so its presence means the extraction finished, and its manifest hash says
//which package it came from. The manifest on disk is re-hashed too (as QuickRecheck does), so an extraction
//whose manifest was altered is not taken as intact.
function TPackageCache.ExtractionMatchesPackage(const packageFolder : string; const verifyResult : TVerificationResult) : boolean;
var
  receipt : TVerificationReceipt;
  manifestFile : string;
begin
  result := false;
  if (FReceiptService = nil) or (verifyResult.ManifestHashHex = '') then
    exit;
  if not FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile) then
    exit;
  if not FReceiptService.TryRead(packageFolder, receipt) then
    exit;
  if not SameText(receipt.ManifestHashHex, verifyResult.ManifestHashHex) then
    exit;
  manifestFile := IncludeTrailingPathDelimiter(packageFolder) + cManifestFileName;
  if not FileExists(manifestFile) then
    exit;
  result := SameText(THashSHA256.GetHashStringFromFile(manifestFile), verifyResult.ManifestHashHex);
end;

//Only while re-verifying the whole cache : the extraction being kept is checked file by file against its
//manifest, and missing or modified files are restored from the package file that was just verified. That
//leaves the compiled output (lib\, bpl\) alone, where re-extracting the package would throw it away. False
//when the extraction can't be checked or put right - the caller then re-extracts the package.
function TPackageCache.ExtractedFilesIntact(const logger : ILogger; const packageFilePath : string; const packageFolder : string) : boolean;
const
  cMaxNamesShown = 10;
var
  mismatched : TArray<string>;
  stillMismatched : TArray<string>;
  names : string;
  i : integer;
begin
  result := true;
  if (not FDeepVerify) or (FSigningService = nil) then
    exit;
  if FSigningService.VerifyExtractedFiles(packageFolder, mismatched) then
    exit;
  //the manifest itself couldn't be read.
  if Length(mismatched) = 0 then
    exit(false);

  TPackageArchiveExtractor.ExtractFiles(packageFilePath, packageFolder, mismatched);
  if not FSigningService.VerifyExtractedFiles(packageFolder, stillMismatched) then
    exit(false);

  names := '';
  for i := 0 to High(mismatched) do
  begin
    if i = cMaxNamesShown then
    begin
      names := names + Format(' and %d more', [Length(mismatched) - cMaxNamesShown]);
      break;
    end;
    if names <> '' then
      names := names + ', ';
    names := names + mismatched[i];
  end;
  logger.Warning(TWarningCode.W3018_CacheExtractionRepaired, Format('%d file(s) in the cached package [%s] were missing or modified and have been restored : %s',
    [Length(mismatched), packageFolder, names]));
end;

function TPackageCache.CachePackage(const packageId : IPackageIdentity; const saveFile : Boolean) : Boolean;
begin
  result := false;
end;

function TPackageCache.Clean : Boolean;
begin
  result := false;
end;

constructor TPackageCache.Create(const logger : ILogger;
                                  const specReader : IPackageSpecReader;
                                  const signingService : IPackageSigningService;
                                  const trustPolicy : ITrustPolicyService;
                                  const receiptService : IReceiptService;
                                  const trustState : ITrustStateService;
                                  const trustPrompt : ITrustPromptStrategy);
begin
  FLogger := logger;
  FSpecReader := specReader;
  FSigningService := signingService;
  FTrustPolicy := trustPolicy;
  FReceiptService := receiptService;
  FTrustState := trustState;
  FTrustPrompt := trustPrompt;
  FInfoCache := TCollections.CreateDictionary<string, IPackageInfo>;
  FSpecCache := TCollections.CreateDictionary<string, IPackageSpec>;
  FExtractionVerified := TCollections.CreateSet<string>;
  FCacheLock := TCriticalSection.Create;
end;

function TPackageCache.EvaluateAuthorDowngrade(const packageId : IPackageIdentity;
                                                const verifyResult : TVerificationResult;
                                                const policy : TTrustPolicy) : boolean;
var
  i : integer;
  prior : TAuthorTrustEntry;
  hadPrior : boolean;
  currentSigned : boolean;
  currentSpki : string;
  currentSubject : string;
  isDowngrade : boolean;
  isKeyChange : boolean;
  context : TTrustPromptContext;
  decision : TTrustPromptDecision;
  newEntry : TAuthorTrustEntry;
begin
  result := true;
  if (FTrustState = nil) or (packageId = nil) then
    exit;

  // Identify the first valid author signature on the current build. We only
  // record valid signers — a broken signature does not feed the ratchet.
  currentSigned := false;
  currentSpki := '';
  currentSubject := '';
  for i := 0 to High(verifyResult.Signatures) do
  begin
    if (verifyResult.Signatures[i].Role = srAuthor) and verifyResult.Signatures[i].Valid then
    begin
      currentSigned := true;
      currentSpki := verifyResult.Signatures[i].SignerSpkiHex;
      currentSubject := verifyResult.Signatures[i].SignerSubject;
      break;
    end;
  end;

  hadPrior := FTrustState.TryGetAuthor(packageId.Id, prior);

  // Permanent block trumps everything else.
  if hadPrior and prior.BlockedPermanently then
  begin
    FLogger.Error(TErrorCode.E7002_PackageBlocked, Format(
      'The package [%s] is permanently blocked by user choice. ' +
      'Edit %%APPDATA%%\.dpm\trust-state.yaml or remove the entry to lift the block.',
      [packageId.Id]));
    result := false;
    exit;
  end;

  // First-ever install for this id — nothing to compare against. Record and
  // proceed (TOFU).
  if not hadPrior then
  begin
    newEntry.LastAuthorSpkiHex    := currentSpki;
    newEntry.LastSeenAuthorSigned := currentSigned;
    newEntry.LastSeenAt           := TTimeZone.Local.ToUniversalTime(Now);
    newEntry.DowngradeAcknowledged := false;
    newEntry.BlockedPermanently   := false;
    FTrustState.RecordAuthor(packageId.Id, newEntry);
    exit;
  end;

  // Was there a real downgrade vs. the previous high-water mark?
  // 1) previously signed, now unsigned: downgrade
  // 2) previously signed by X, now signed by Y (Y <> X): key change (also a downgrade)
  // 3) previously unsigned, now signed: upgrade — silently accept and ratchet
  // 4) previously unsigned, now unsigned: no change
  // 5) same SPKI as last time: no change
  isDowngrade := prior.LastSeenAuthorSigned and not currentSigned;
  isKeyChange := prior.LastSeenAuthorSigned and currentSigned and
                 (not SameText(prior.LastAuthorSpkiHex, currentSpki));

  if not (isDowngrade or isKeyChange) then
  begin
    // No-downgrade — update last-seen if anything changed.
    if (prior.LastAuthorSpkiHex <> currentSpki) or
       (prior.LastSeenAuthorSigned <> currentSigned) then
    begin
      newEntry := prior;
      newEntry.LastAuthorSpkiHex    := currentSpki;
      newEntry.LastSeenAuthorSigned := currentSigned;
      newEntry.LastSeenAt           := TTimeZone.Local.ToUniversalTime(Now);
      // Once author has signed, lock that in — even an unsigned re-install
      // would have already failed the downgrade check above.
      if currentSigned then
        newEntry.DowngradeAcknowledged := false;
      FTrustState.RecordAuthor(packageId.Id, newEntry);
    end;
    exit;
  end;

  // Honour an earlier acknowledgement so we don't re-prompt for the same SPKI.
  if prior.DowngradeAcknowledged and
     SameText(prior.LastAuthorSpkiHex, currentSpki) and
     (prior.LastSeenAuthorSigned = currentSigned) then
    exit;

  // Apply the downgrade policy.
  case policy.AuthorDowngradePolicy of
    adpAllow :
      begin
        FLogger.Warning(TWarningCode.W7006_AuthorSignatureChanged, Format(
          'The author signature of [%s] changed - allowed by authorDowngradePolicy=allow.',
          [packageId.Id]));
        newEntry := prior;
        newEntry.LastAuthorSpkiHex    := currentSpki;
        newEntry.LastSeenAuthorSigned := currentSigned;
        newEntry.LastSeenAt           := TTimeZone.Local.ToUniversalTime(Now);
        newEntry.DowngradeAcknowledged := true;
        FTrustState.RecordAuthor(packageId.Id, newEntry);
      end;
    adpBlock :
      begin
        FLogger.Error(TErrorCode.E7003_AuthorDowngradeBlocked, Format(
          'The install of [%s] was blocked - its author signature changed, and authorDowngradePolicy is block. ' +
          'Previous signer sha256:%s.',
          [packageId.Id, prior.LastAuthorSpkiHex]));
        result := false;
      end;
    adpPrompt :
      begin
        if FTrustPrompt = nil then
        begin
          FLogger.Error(TErrorCode.E7003_AuthorDowngradeBlocked, Format(
            'The install of [%s] was blocked - its author signature changed and there is no way to ask ' +
            'whether to continue. Set authorDowngradePolicy=allow to override.',
            [packageId.Id]));
          result := false;
          exit;
        end;
        context.PackageId        := packageId.Id;
        context.Version          := packageId.Version.ToStringNoMeta;
        context.PreviousSpkiHex  := prior.LastAuthorSpkiHex;
        context.PreviousSubject  := '';
        context.NewSigned        := currentSigned;
        context.NewSpkiHex       := currentSpki;
        context.NewSubject       := currentSubject;
        if currentSigned then
          FLogger.Warning(TWarningCode.W7006_AuthorSignatureChanged, Format(
            'The author signature of [%s] changed - previously sha256:%s, now sha256:%s.',
            [packageId.Id, prior.LastAuthorSpkiHex, currentSpki]))
        else
          FLogger.Warning(TWarningCode.W7006_AuthorSignatureChanged, Format(
            'The package [%s] is no longer author-signed - previously sha256:%s.',
            [packageId.Id, prior.LastAuthorSpkiHex]));
        result := FTrustPrompt.PromptAuthorDowngrade(context, decision);
        case decision of
          tpdOverride :
            begin
              // Drop the trust entry so the new build installs and the next
              // install is treated as fresh TOFU rather than another downgrade.
              FTrustState.RemoveAuthor(packageId.Id);
              FLogger.Information('The author signature change of [' +
                packageId.Id + '] was accepted - its previous author trust entry was removed.', true);
              result := true;
            end;
          tpdBlockOnce :
            begin
              FLogger.Error(TErrorCode.E7003_AuthorDowngradeBlocked, 'The install of [' +
                packageId.Id + '] was blocked - its author signature changed.');
              result := false;
            end;
          tpdBlockAlways :
            begin
              FLogger.Error(TErrorCode.E7002_PackageBlocked, 'The install of [' +
                packageId.Id + '] was blocked, and the package is now permanently blocked - its author signature changed.');
              FTrustState.BlockPermanently(packageId.Id);
              result := false;
            end;
        end;
      end;
  end;
end;

destructor TPackageCache.Destroy;
begin
  FCacheLock.Free;
  inherited;
end;

function MakeCacheKey(const packageId : IPackageIdentity) : string;
begin
  result := LowerCase(packageId.Id) + '|' +
            CompilerToString(packageId.CompilerVersion) + '|' +
            packageId.Version.ToStringNoMeta;
end;

function TPackageCache.GetCachedPackagesMatching(const id : string; const compilerVersion : TCompilerVersion; const version : string) : IList<IPackageIdentity>;
var
  packagesRoot : string;
  compilerFolders : TStringDynArray;
  compilerName : string;
  thisCompiler : TCompilerVersion;
  idFolder : string;
  versionFolders : TStringDynArray;
  dpkgFiles : TStringDynArray;
  i, j : integer;
  versionName : string;
  candidateVersion : TPackageVersion;
  dpkgIdentity : IPackageIdentity;
  identity : IPackageIdentity;
  seen : ISet<string>;
  key : string;
begin
  result := TCollections.CreateList<IPackageIdentity>;
  seen := TCollections.CreateSet<string>;

  packagesRoot := GetPackagesFolder;
  if not DirectoryExists(packagesRoot) then
    exit;

  // Which compiler folders to scan - UnknownVersion means "all of them".
  if compilerVersion <> TCompilerVersion.UnknownVersion then
  begin
    SetLength(compilerFolders, 1);
    compilerFolders[0] := packagesRoot + PathDelim + CompilerToString(compilerVersion);
  end
  else
    compilerFolders := TDirectory.GetDirectories(packagesRoot, '*', TSearchOption.soTopDirectoryOnly);

  for i := 0 to High(compilerFolders) do
  begin
    if not DirectoryExists(compilerFolders[i]) then
      continue;
    compilerName := ExtractFileName(ExcludeTrailingPathDelimiter(compilerFolders[i]));
    thisCompiler := StringToCompilerVersion(compilerName);
    if thisCompiler = TCompilerVersion.UnknownVersion then
      continue;

    idFolder := compilerFolders[i] + PathDelim + id;
    if not DirectoryExists(idFolder) then
      continue;

    // Versions present as extracted folders.
    versionFolders := TDirectory.GetDirectories(idFolder, '*', TSearchOption.soTopDirectoryOnly);
    for j := 0 to High(versionFolders) do
    begin
      versionName := ExtractFileName(ExcludeTrailingPathDelimiter(versionFolders[j]));
      if not TPackageVersion.TryParse(versionName, candidateVersion) then
        continue;
      if (version <> '') and (not SameText(candidateVersion.ToStringNoMeta, version)) then
        continue;
      key := CompilerToString(thisCompiler) + '|' + candidateVersion.ToStringNoMeta;
      if seen.Contains(key) then
        continue;
      seen.Add(key);
      identity := TPackageIdentity.Create('', id, candidateVersion, thisCompiler);
      result.Add(identity);
    end;

    // Versions present only as raw .dpkg files (downloaded, never extracted).
    dpkgFiles := TDirectory.GetFiles(idFolder, '*' + cPackageFileExt, TSearchOption.soTopDirectoryOnly);
    for j := 0 to High(dpkgFiles) do
    begin
      versionName := ChangeFileExt(ExtractFileName(dpkgFiles[j]), '');
      if not TPackageIdentity.TryCreateFromString(FLogger, versionName, '', dpkgIdentity) then
        continue;
      if (not SameText(dpkgIdentity.Id, id)) or (dpkgIdentity.CompilerVersion <> thisCompiler) then
        continue;
      candidateVersion := dpkgIdentity.Version;
      if (version <> '') and (not SameText(candidateVersion.ToStringNoMeta, version)) then
        continue;
      key := CompilerToString(thisCompiler) + '|' + candidateVersion.ToStringNoMeta;
      if seen.Contains(key) then
        continue;
      seen.Add(key);
      identity := TPackageIdentity.Create('', id, candidateVersion, thisCompiler);
      result.Add(identity);
    end;
  end;
end;

function TPackageCache.RemovePackage(const packageId : IPackageIdentity) : boolean;
var
  packageFolder : string;
  packageFileFolder : string;
  searchPattern : string;
  matchingFiles : TStringDynArray;
  i : integer;
  key : string;
  removedAnything : boolean;
begin
  result := false;
  if packageId = nil then
    exit;

  removedAnything := false;

  // 1) the extracted per-version folder.
  packageFolder := GetPackagePath(packageId);
  if DirectoryExists(packageFolder) then
  begin
    try
      TDirectory.Delete(packageFolder, true);
      removedAnything := true;
    except
      on e : Exception do
        FLogger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to remove the cached folder [' + packageFolder + '] : ' + e.Message);
    end;
  end;

  // 2) the raw .dpkg files + .sha256 sidecars for this version. On-disk name is
  // {id}-{compiler}-{binPlatforms}-{version}.dpkg - wildcard the platforms segment.
  packageFileFolder := GetPackageFileFolder(packageId);
  if DirectoryExists(packageFileFolder) then
  begin
    searchPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) +
      '-*-' + packageId.Version.ToStringNoMeta + cPackageFileExt;
    try
      matchingFiles := TDirectory.GetFiles(packageFileFolder, searchPattern, TSearchOption.soTopDirectoryOnly);
    except
      SetLength(matchingFiles, 0);
    end;

    for i := 0 to High(matchingFiles) do
    begin
      try
        TFile.Delete(matchingFiles[i]);
        removedAnything := true;
      except
        on e : Exception do
          FLogger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to remove the cached file [' + matchingFiles[i] + '] : ' + e.Message);
      end;
      if FileExists(matchingFiles[i] + cPackageHashAlgorithmExt) then
      begin
        try
          TFile.Delete(matchingFiles[i] + cPackageHashAlgorithmExt);
        except
          on e : Exception do
            FLogger.Debug('[PackageCache] Unable to remove hash sidecar [' + matchingFiles[i] + cPackageHashAlgorithmExt + '] : ' + e.Message);
        end;
      end;
    end;

    // tidy up the now-empty id folder so the cache doesn't accumulate stubs.
    try
      if Length(TDirectory.GetFileSystemEntries(packageFileFolder)) = 0 then
        TDirectory.Delete(packageFileFolder, false);
    except
      //best effort - leaving an empty folder is harmless.
    end;
  end;

  // 3) drop the in-memory memo so a later re-add / re-install re-reads from disk.
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    FInfoCache.Remove(key);
    FSpecCache.Remove(key);
    FExtractionVerified.Remove(key);
  finally
    FCacheLock.Leave;
  end;

  // 4) drop the per-user TOFU trust state for this package id. The trust state
  // is keyed by id only (not version/compiler), so removing a cached package
  // resets author/repository ratchets for the id. Without this, repacking and
  // re-testing the same id with a freshly built (differently signed) .dpkg
  // would trip the author-downgrade / repository ratchet and fail the install.
  if FTrustState <> nil then
  begin
    FTrustState.RemoveAuthor(packageId.Id);
    FTrustState.RemoveRepository(packageId.Id);
  end;

  result := removedAnything;
end;

function TPackageCache.CreatePackagePath(const packageId : IPackageIdentity) : string;
begin
  result := GetPackagePath(packageId);
  if not ForceDirectories(result) then
  begin
    FLogger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to create the package folder [' + result + '].');
    exit;
  end;
end;

function TPackageCache.GetLocation : string;
begin
  result := FLocation;
end;

function TPackageCache.GetPackageInfo(const cancellationToken : ICancellationToken; const packageId : IPackageIdentity) : IPackageInfo;
var
  packageFolder : string;
  metaDataFile : string;
  spec : IPackageSpec;
  key : string;
  packageHash : string;
begin
  result := nil;
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FInfoCache.TryGetValue(key, result) then
      exit;
  finally
    FCacheLock.Leave;
  end;

  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package dspec file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  spec := FSpecReader.ReadSpec(metaDataFile);
  if spec = nil then
    exit;
  //Carry the package hash through from the .sha256 sidecar next to the cached .dpkg
  //(GetPackageHash self-heals from the .dpkg when the sidecar is missing). The dspec
  //itself has no hash, so building the info without one produced an IPackageInfo with
  //Hash = ''. Both TPackageInstaller.GetPackageInfo and the resolver are cache-first,
  //so that empty hash then reached TDPMServerPackageRepository.DownloadPackage, which
  //verifies the downloaded file against it - failing with "does not match server hash []".
  packageHash := GetPackageHash(packageId);
  if packageHash <> '' then
    result := TPackageInfo.CreateFromManifest('', spec, packageHash, cPackageHashAlgorithm)
  else
    result := TPackageInfo.CreateFromManifest('', spec, '', '');

  FCacheLock.Enter;
  try
    FInfoCache[key] := result;
  finally
    FCacheLock.Leave;
  end;
end;

function TPackageCache.GetPackageMetadata(const packageId : IPackageIdentity) : IPackageMetadata;
var
  spec : IPackageSpec;
begin
  spec := GetPackageSpec(packageId);
  if spec = nil then
    exit;
  result := TPackageMetadata.CreateFromManifest('', spec);
end;

function TPackageCache.GetPackagePath(const id: string; const version: string; const compilerVersion : TCompilerVersion): string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + PackageFolderName(Id) + PathDelim + Version;
end;

function TPackageCache.GetPackagePath(const packageId : IPackageIdentity) : string;
begin
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim +  PackageFolderName(packageId.Id) + PathDelim + packageId.Version.ToStringNoMeta;
end;

function TPackageCache.GetPackageFileFolder(const packageId : IPackageIdentity) : string;
begin
  //{cache}/{compiler}/{id} - parent of the per-version extraction folders. Raw .dpkg files
  //for all versions of this package id live here side-by-side with the versioned subfolders.
  result := GetPackagesFolder + PathDelim + CompilerToString(packageId.CompilerVersion) + PathDelim + PackageFolderName(packageId.Id);
end;

function TPackageCache.GetPackagesFolder : string;
begin
  //  result := IncludeTrailingPathDelimiter(FLocation);
  result := TPath.GetFullPath(FLocation)
end;

function TPackageCache.GetPackageSpec(const packageId: IPackageIdentity): IPackageSpec;
var
  packageFolder : string;
  metaDataFile : string;
  key : string;
begin
  result := nil;
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FSpecCache.TryGetValue(key, result) then
      exit;
  finally
    FCacheLock.Leave;
  end;

  if not EnsurePackage(packageId) then
  begin
    FLogger.Debug('Package dspec file [' + packageId.ToString + '] not found in cache.');
    exit;
  end;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;
  metaDataFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;
  if not FileExists(metaDataFile) then
  begin
    FLogger.Debug('Package dspec file [' + metaDataFile + '] not found in cache.');
    exit;
  end;
  result := FSpecReader.ReadSpec(metaDataFile);
  if result = nil then
    exit;

  FCacheLock.Enter;
  try
    FSpecCache[key] := result;
  finally
    FCacheLock.Leave;
  end;
end;

function TPackageCache.GetPackagePlatforms(const packageId: IPackageIdentity): TDPMPlatforms;
var
  spec: IPackageSpec;
begin
  result := [];
  spec := GetPackageSpec(packageId);
  if (spec <> nil) and (spec.TargetPlatform <> nil) then
    result := spec.TargetPlatform.Platforms;
end;

function TPackageCache.HasSource(const packageId: IPackageIdentity): boolean;
var
  packageFolder : string;
  subDir : string;
begin
  result := false;
  packageFolder := GetPackagePath(packageId);
  if not DirectoryExists(packageFolder) then
    exit;

  //git-registry packages are cloned in place - always consumed as source.
  if IsGitRegistryPackageFolder(packageFolder) then
    exit(true);

  //Mirror the installer's source-path collection (CollectSearchPaths/AddPasSubdirs): a packed
  //package ships source when some subfolder (outside .git) contains .pas files. The lib\ and bpl\
  //compiled-output folders hold dcu/dcp/bpl and no .pas, so they are naturally excluded.
  for subDir in TDirectory.GetDirectories(packageFolder, '*', TSearchOption.soAllDirectories) do
  begin
    if SameText(ExtractFileName(subDir), '.git') then
      continue;
    if Pos(PathDelim + '.git' + PathDelim, subDir + PathDelim) > 0 then
      continue;
    if Length(TDirectory.GetFiles(subDir, '*.pas')) > 0 then
      exit(true);
  end;
end;

function TPackageCache.GetCachedPackageVersionsWithDependencies(const cancellationToken : ICancellationToken;
                                                                const id : string;
                                                                const compilerVersion : TCompilerVersion;
                                                                const versionRange : TVersionRange;
                                                                const preRelease : boolean) : IList<IPackageInfo>;
var
  packageFileFolder : string;
  versionFolders : TStringDynArray;
  versionFolder : string;
  versionName : string;
  packageVersion : TPackageVersion;
  packageIdentity : IPackageIdentity;
  packageInfo : IPackageInfo;
begin
  result := TCollections.CreateList<IPackageInfo>;

  //{cache}/{compiler}/{id} - reusing GetPackageFileFolder which returns this path
  //without requiring a version. Sibling folders are the per-version extraction folders.
  packageFileFolder := GetPackagesFolder + PathDelim + CompilerToString(compilerVersion) + PathDelim + id;
  if not DirectoryExists(packageFileFolder) then
    exit;

  versionFolders := TDirectory.GetDirectories(packageFileFolder, '*', TSearchOption.soTopDirectoryOnly);
  for versionFolder in versionFolders do
  begin
    versionName := ExtractFileName(ExcludeTrailingPathDelimiter(versionFolder));
    if not TPackageVersion.TryParse(versionName, packageVersion) then
      continue;
    if not versionRange.IsSatisfiedBy(packageVersion) then
      continue;
    if (not preRelease) and (not packageVersion.IsStable) then
      continue;

    packageIdentity := TPackageIdentity.Create('', id, packageVersion, compilerVersion);
    packageInfo := GetPackageInfo(cancellationToken, packageIdentity);
    if packageInfo <> nil then
      result.Add(packageInfo);
  end;

  //sort descending by version - matches the order produced by TPackageRepositoryManager
  result.Sort(TComparer<IPackageInfo>.Construct(
    function(const Left, Right : IPackageInfo) : Integer
    begin
      result := Right.Version.CompareTo(Left.Version);
    end));
end;

function TPackageCache.GetPackageHash(const packageId : IPackageIdentity) : string;
var
  folder : string;
  sidecarPattern : string;
  dpkgPattern : string;
  matches : TStringDynArray;
  sidecarPath : string;
  dpkgPath : string;
  lines : TStringList;
begin
  result := '';
  if packageId = nil then
    exit;
  folder := GetPackageFileFolder(packageId);
  if not DirectoryExists(folder) then
    exit;

  //.dpkg filenames follow {id}-{compiler}-{binPlatforms}-{version}.dpkg - the
  //binPlatforms segment is opaque from this side so we wildcard it. The .sha256
  //sidecar shares the .dpkg basename plus the .sha256 extension.
  sidecarPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) + '-*-'
                  + packageId.Version.ToStringNoMeta + cPackageFileExt + cPackageHashAlgorithmExt;
  try
    matches := TDirectory.GetFiles(folder, sidecarPattern, TSearchOption.soTopDirectoryOnly);
  except
    SetLength(matches, 0);
  end;

  if Length(matches) > 0 then
  begin
    sidecarPath := matches[0];
    lines := TStringList.Create;
    try
      try
        lines.LoadFromFile(sidecarPath);
        if lines.Count > 0 then
          result := Trim(lines[0]);
      except
        on e : Exception do
          FLogger.Debug('[Cache] could not read hash sidecar [' + sidecarPath + '] : ' + e.Message);
      end;
    finally
      lines.Free;
    end;
    if result <> '' then
      exit;
  end;

  //Sidecar missing or unreadable - compute from the .dpkg and persist for
  //next time (mirrors TDirectoryPackageRepository.DoGetPackageMetaData's
  //self-heal). Other tooling - SBOM, vulnerability scanners, signature
  //verification - can then assume the sidecar is present.
  dpkgPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) + '-*-'
              + packageId.Version.ToStringNoMeta + cPackageFileExt;
  try
    matches := TDirectory.GetFiles(folder, dpkgPattern, TSearchOption.soTopDirectoryOnly);
  except
    SetLength(matches, 0);
  end;
  if Length(matches) = 0 then
    exit;
  dpkgPath := matches[0];

  try
    result := THashSHA256.GetHashStringFromFile(dpkgPath);
  except
    on e : Exception do
    begin
      FLogger.Debug('[Cache] could not hash .dpkg [' + dpkgPath + '] : ' + e.Message);
      result := '';
    end;
  end;
  if result = '' then
    exit;

  //Persist the sidecar. Best-effort: if the cache directory is somehow
  //read-only we still return the computed hash, we just won't have cached
  //it for the next call.
  try
    TFile.WriteAllText(dpkgPath + cPackageHashAlgorithmExt, result);
  except
    on e : Exception do
      FLogger.Debug('[Cache] could not write hash sidecar [' + dpkgPath + cPackageHashAlgorithmExt + '] : ' + e.Message);
  end;
end;

function TPackageCache.TryGetPackageIcon(const packageId : IPackageIdentity; out icon : IPackageIcon) : boolean;
var
  packageFolder : string;
  svgPath : string;
  pngPath : string;
  fileStream : TFileStream;
  stream : TMemoryStream;
begin
  result := false;
  icon := nil;
  if packageId = nil then
    exit;
  //EnsurePackage extracts a stray .dpkg if the folder is missing - cheap no-op once a package
  //has been installed by either the CLI or the IDE.
  if not EnsurePackage(packageId) then
    exit;
  packageFolder := GetPackagePath(packageId);
  svgPath := IncludeTrailingPathDelimiter(packageFolder) + cIconFileSVG;
  pngPath := IncludeTrailingPathDelimiter(packageFolder) + cIconFilePNG;

  if FileExists(svgPath) then
  begin
    fileStream := TFileStream.Create(svgPath, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    //icon now owns the stream.
    icon := CreatePackageIcon(TPackageIconKind.ikSvg, stream);
    result := true;
    exit;
  end;

  if FileExists(pngPath) then
  begin
    fileStream := TFileStream.Create(pngPath, fmOpenRead);
    stream := TMemoryStream.Create;
    try
      stream.CopyFrom(fileStream, fileStream.Size);
    finally
      fileStream.Free;
    end;
    icon := CreatePackageIcon(TPackageIconKind.ikPng, stream);
    result := true;
  end;
end;

function TPackageCache.EnsurePackage(const packageId : IPackageIdentity) : Boolean;
var
  packageFolder : string;
  packageFileFolder : string;
  dspecFile : string;
  searchPattern : string;
  matchingFiles : TStringDynArray;
  key : string;
  receipt : TVerificationReceipt;
  cachedVerifyResult : TVerificationResult;
  ratchetBlocked : boolean;
  isGitPackage : boolean;
  priorAuthor : TAuthorTrustEntry;
  hadPriorAuthor : boolean;
begin
  key := MakeCacheKey(packageId);
  FCacheLock.Enter;
  try
    if FExtractionVerified.Contains(key) then
    begin
      //The cache is a session-long singleton in the IDE, so this memo can outlive the
      //extraction on disk if the user deletes the package folder by hand (e.g. while
      //testing). Confirm the folder + dspec are still present before trusting it - if
      //they're gone, drop the stale key and fall through to re-extract / re-download.
      packageFolder := GetPackagePath(packageId);
      if DirectoryExists(packageFolder) and
         FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile) then
      begin
        result := true;
        exit;
      end;
      FExtractionVerified.Remove(key);
    end;
  finally
    FCacheLock.Leave;
  end;

  ratchetBlocked := false;

  //check if we have a package folder and dspec.
  packageFolder := GetPackagePath(packageId);
  result := DirectoryExists(packageFolder);

  dspecFile := IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile;

  result := result and FileExists(dspecFile);

  // git registry packages are cloned + built from source in place. They are unsigned
  // by design (no .dpkg / manifest / receipt), so the signing gate and the .dpkg
  // fallback below do not apply - presence of the marker + dspec is sufficient.
  isGitPackage := result and IsGitRegistryPackageFolder(packageFolder);

  // Cheap re-check on cache hit (plan §1.7, V-33). Confirms the receipt
  // exists and matches the active trust-policy fingerprint, and re-hashes
  // only the manifest. If the policy has changed since extraction, fall
  // through to InstallPackageFromFile which does the full re-verify.
  if result and (not isGitPackage) and (FSigningService <> nil) and (FTrustPolicy <> nil) then
  begin
    if not FSigningService.QuickRecheck(packageFolder, FTrustPolicy.GetEffectivePolicy) then
    begin
      FLogger.Information(
        '[PackageCache] Receipt is missing, policy has changed, or manifest has been altered. ' +
        'Re-verifying [' + packageId.Id + '].');
      result := false;
    end
    else if (not FSkipTrustRatchets) and (FReceiptService <> nil) and (FTrustState <> nil) and
            FReceiptService.TryRead(packageFolder, receipt) then
    begin
      // QuickRecheck only verifies the cached extraction against the trust
      // *policy*. The trust *state* (per-package TOFU history) can change
      // independently — manual edits to trust-state.yaml or an intervening
      // install that ratcheted a different signer. Re-run both ratchets
      // against the signatures recorded in the receipt so cache hits don't
      // bypass the author / V-24 protections. Skipped entirely while the cache
      // is in test mode (FSkipTrustRatchets) - the same build that just installed
      // a ratchet-exempt local package re-checks it here, and would otherwise
      // block its own freshly-cached entry.
      cachedVerifyResult := BuildResultFromReceipt(receipt);
      // Read the author high-water mark before EvaluateAuthorDowngrade ratchets
      // it forward — the repository ratchet's author-rebuild exemption compares
      // against the *prior* identity, not the one about to be recorded.
      hadPriorAuthor := FTrustState.TryGetAuthor(packageId.Id, priorAuthor);
      if not EvaluateAuthorDowngrade(packageId, cachedVerifyResult, FTrustPolicy.GetEffectivePolicy) then
      begin
        result := false;
        ratchetBlocked := true;
      end
      else if not EvaluateRepositoryRatchet(packageId, cachedVerifyResult, hadPriorAuthor, priorAuthor) then
      begin
        result := false;
        ratchetBlocked := true;
      end;
    end;
  end;

  // Only fall back to the .dpkg when the cached extraction is missing or
  // stale. A ratchet block is the user's explicit decision — re-installing
  // from the .dpkg would just trigger the same prompt again.
  if (not result) and (not ratchetBlocked) then
  begin
    //fallback: the extracted folder is missing or has no dspec, but the .dpkg may still be
    //on disk from a previous session. On-disk filename is
    //{Id}-{Compiler}-{BinPlatforms}-{Version}.dpkg - BinPlatforms is a DPMPlatformsToBinString
    //bitmask encoding the platforms this .dpkg was packed for. packageId only tells us
    //Id/Compiler/Version so we wildcard the platforms segment.
    packageFileFolder := GetPackageFileFolder(packageId);
    if DirectoryExists(packageFileFolder) then
    begin
      searchPattern := packageId.Id + '-' + CompilerToString(packageId.CompilerVersion) +
        '-*-' + packageId.Version.ToStringNoMeta + cPackageFileExt;
      matchingFiles := TDirectory.GetFiles(packageFileFolder, searchPattern, TSearchOption.soTopDirectoryOnly);
      if Length(matchingFiles) > 0 then
      begin
        //speculative - if the .dpkg left on disk is no good, the caller downloads the package again, so
        //what is wrong with it is not the operation's problem.
        try
          result := DoInstallPackageFromFile(TDemotingLogger.Create(FLogger), matchingFiles[0], false);
        except
          on e : Exception do
          begin
            FLogger.Debug('[PackageCache] Unable to use [' + matchingFiles[0] + '] : ' + e.Message);
            result := false;
          end;
        end;
      end;
    end;
  end;

  if result then
  begin
    FCacheLock.Enter;
    try
      FExtractionVerified.Add(key);
    finally
      FCacheLock.Leave;
    end;
  end;
end;


procedure TPackageCache.SetSkipTrustRatchets(const value : boolean);
begin
  FSkipTrustRatchets := value;
end;

function TPackageCache.InstallPackageFromFile(const packageFileName : string; const skipTrustRatchets : boolean = false) : boolean;
begin
  result := DoInstallPackageFromFile(FLogger, packageFileName, skipTrustRatchets);
end;

function TPackageCache.DoInstallPackageFromFile(const logger : ILogger; const packageFileName : string; const skipTrustRatchets : boolean) : boolean;
var
  packageFilePath : string;
  fileName : string;
  packageIndentity : IPackageIdentity;
  packageFolder : string;
  packageFileFolder : string;
  key : string;
  verifyResult : TVerificationResult;
  priorAuthor : TAuthorTrustEntry;
  hadPriorAuthor : boolean;
  i : integer;
  declaredVersion : TPackageVersion;
  identityProblem : string;
  spec : IPackageSpec;
begin
  result := false;
  logger.Debug('[PackageCache] installing from file : ' + packageFileName);
  if not FileExists(packageFileName) then
  begin
    logger.Error(TErrorCode.E3004_PackageFileNotFound, 'The package file [' + packageFileName + '] does not exist.');
    exit;
  end;
  if ExtractFileExt(packageFileName) <> '.dpkg' then
  begin
    logger.Error(TErrorCode.E3005_InvalidPackageFile, 'The file [' + packageFileName + '] is not a package file (.dpkg).');
    exit;
  end;

  fileName := ChangeFileExt(ExtractFileName(packageFileName), '');

  if not TPackageIdentity.TryCreateFromString(logger, fileName, '', packageIndentity) then
    exit;

  //defensive: about to re-extract this id-compiler-version, invalidate any memoised state
  //so the next read picks up the fresh dspec. Contents should be identical for the same
  //(id, compiler, version), but this future-proofs against replacing a corrupt extraction.
  key := MakeCacheKey(packageIndentity);
  FCacheLock.Enter;
  try
    FInfoCache.Remove(key);
    FSpecCache.Remove(key);
    FExtractionVerified.Remove(key);
  finally
    FCacheLock.Leave;
  end;

  //creates the per-version extraction folder
  packageFolder := CreatePackagePath(packageIndentity);
  logger.Debug('[PackageCache] PackageFolder  : ' + packageFolder);

  //.dpkg lives in the package-id folder (sibling of the per-version folders) so multiple versions
  //of the same id share a folder rather than cluttering the cache root.
  packageFileFolder := GetPackageFileFolder(packageIndentity);
  if not ForceDirectories(packageFileFolder) then
  begin
    logger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to create the package folder [' + packageFileFolder + '].');
    exit;
  end;

  //if the source file isn't already in its canonical cache location, copy it there.
  packageFilePath := IncludeTrailingPathDelimiter(packageFileFolder) + ExtractFileName(packageFileName);
  if not SameText(packageFileName, packageFilePath) then
  begin
    logger.Debug('[PackageCache] Copying Package file to   : ' + packageFilePath);
    try
      TFile.Copy(packageFileName, packageFilePath, true);
    except
      on e : Exception do
      begin
        logger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to copy [' + packageFileName + '] into the package cache : ' + e.Message);
        exit;
      end;
    end;
  end;

  //work with packageFilePath now.

  // Signing — the central verification gate (architecture doc §Verification
  // Workflow, plan §1.7). Verify the package *before* extraction so a hostile
  // archive never gets written to disk. The signing service is optional:
  // tests/legacy harnesses that don't wire DI can skip it.
  if FSigningService <> nil then
  begin
    try
      verifyResult := FSigningService.VerifyPackage(packageFilePath,
        FTrustPolicy.GetEffectivePolicy);
    except
      on e : Exception do
      begin
        logger.Error(TErrorCode.E7001_PackageVerificationFailed, 'Unable to verify the package [' +
          packageFilePath + '] : ' + e.Message);
        exit;
      end;
    end;

    if verifyResult.Outcome = voInvalid then
    begin
      if verifyResult.FormatNotSupported then
        logger.Error(TErrorCode.E7109_PackageFormatNotSupported, 'The package [' + packageFilePath + '] can''t be installed : ' + verifyResult.Reason)
      else
        logger.Error(TErrorCode.E7001_PackageVerificationFailed, 'The package [' + packageFilePath + '] failed verification : ' + verifyResult.Reason);
      exit;
    end;

    //the signatures vouch for the identity in the manifest, not for the file name.
    if not TPackageVersion.TryParse(verifyResult.ManifestVersion, declaredVersion) then
      identityProblem := Format('its manifest version [%s] is not a valid version', [verifyResult.ManifestVersion])
    else
      identityProblem := DeclaredIdentityProblem(packageIndentity, 'manifest', verifyResult.ManifestPackageId, declaredVersion);
    if identityProblem <> '' then
    begin
      RejectMisnamedPackage(logger, packageFilePath, packageIndentity, identityProblem);
      exit;
    end;

    case verifyResult.Outcome of
      voTrusted :
        logger.Verbose('[PackageCache] Package signature verified (' + IntToStr(Length(verifyResult.Signatures)) + ' signatures).');
      voUnsigned :
        logger.Information('[PackageCache] Installing unsigned package ' + '[' + packageIndentity.Id + '] under permissive mode.');
      voUntrustedPublisher :
        logger.Warning(TWarningCode.W7008_PublisherNotTrusted, 'The package [' + packageIndentity.Id + '] is signed, but its author certificate does not chain to a trusted root and is ' +
          'not in trustedPublishers - permissive mode allows installing it.');
    end;
    for i := 0 to High(verifyResult.Signatures) do
      if verifyResult.Signatures[i].TimestampAuthorityUntrusted then
        logger.Warning(TWarningCode.W7011_TimestampAuthorityUntrusted, Format('The timestamp authority [%s] on a signature of the package [%s] does not chain to a trusted root - the ' +
          'signing certificate was checked as of now instead. Updating this machine''s root certificates fixes this.',
          [verifyResult.Signatures[i].TimestampAuthority, packageIndentity.Id]));

    // The TOFU ratchets compare this build against the high-water mark from the
    // last install and can block (or mutate trust state). The test workflow
    // installs a freshly built local .dpkg that legitimately lacks the repository
    // signature its published counterpart carries, so it asks us to skip them
    // (via the per-call argument or the cache-wide FSkipTrustRatchets mode).
    // Signature verification above still applies; we just don't ratchet.
    if skipTrustRatchets or FSkipTrustRatchets then
      logger.Information('[PackageCache] Skipping author/repository trust ratchets for [' +
        packageIndentity.Id + '] (test install).')
    else
    begin
      // Read the author high-water mark before EvaluateAuthorDowngrade ratchets
      // it forward — the repository ratchet's author-rebuild exemption compares
      // against the *prior* identity, not the one about to be recorded.
      hadPriorAuthor := (FTrustState <> nil) and
                        FTrustState.TryGetAuthor(packageIndentity.Id, priorAuthor);

      // TOFU author no-downgrade ratchet (plan §1.10). Compares the current
      // signer against the high-water mark from the last successful install
      // of this package id and either accepts, blocks, or prompts the user.
      if not EvaluateAuthorDowngrade(packageIndentity, verifyResult, FTrustPolicy.GetEffectivePolicy) then
        exit;

      // P2 §2.3 (V-24): repository assurance ratchet. Once seen carrying a
      // trusted-repo signature, future builds must continue to — unless this is
      // the author rebuilding their own package (TAuthorRebuildExemption).
      if not EvaluateRepositoryRatchet(packageIndentity, verifyResult, hadPriorAuthor, priorAuthor) then
        exit;
    end;

    //Already extracted from this very package - keep it. The installer builds into the package folder
    //(lib\, bpl\ and the package.{platform}.bom compile-skip marker), so emptying it would throw that away
    //and force a recompile - re-verifying the cache did that to every package. Only the receipt is
    //rewritten, so it carries the fingerprint of the policy the package was just verified under.
    if ExtractionMatchesPackage(packageFolder, verifyResult) and ExtractedFilesIntact(logger, packageFilePath, packageFolder) then
    begin
      logger.Debug('[PackageCache] The package folder already holds this package, keeping it : ' + packageFolder);
      WriteReceipt(packageFolder, packageIndentity, verifyResult);
      result := true;
      exit;
    end;
  end;

  //Ensure a clean extraction target. Stale files from a previous (possibly partial, or a
  //different-layout) extraction collide with the archive contents - TZipFile overwrites in
  //place and fails if any existing file is locked or read-only. Empty the folder first so we
  //always extract into a known-empty directory.
  if TDirectory.Exists(packageFolder) and (not TDirectory.IsEmpty(packageFolder)) then
  begin
    logger.Debug('[PackageCache] Cleaning existing package folder before extraction : ' + packageFolder);
    try
      TDirectory.Delete(packageFolder, true);
    except
      on e : Exception do
        raise EDPMError.Create(TErrorCode.E3008_CacheWriteFailed, 'Unable to clear the package folder [' + packageFolder + '] before extracting into it : ' + e.Message);
    end;
    if not ForceDirectories(packageFolder) then
    begin
      logger.Error(TErrorCode.E3008_CacheWriteFailed, 'Unable to recreate the package folder [' + packageFolder + '].');
      exit;
    end;
  end;

  try
    logger.Debug('[PackageCache] Extracting Package file [' + packageFilePath + '] to : ' + packageFolder);
    //not TZipFile.ExtractZipFile - from 10.1 Berlin the RTL takes file names from a Unicode Path
    //extra field and creates symbolic links, neither of which the archive validation covers.
    TPackageArchiveExtractor.Extract(packageFilePath, packageFolder);
    result := FileExists(IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile);
    if not result then
      logger.Error(TErrorCode.E3005_InvalidPackageFile, 'The package file [' + packageFilePath + '] does not contain a ' + cPackageDspecFile + ' - it is not a valid package.')
    else if FSpecReader <> nil then
    begin
      //what the rest of DPM reads about the package - its dependencies above all - comes from its dspec.
      spec := FSpecReader.ReadSpec(IncludeTrailingPathDelimiter(packageFolder) + cPackageDspecFile);
      if spec = nil then
      begin
        //the reader said why.
        logger.Verbose('The package [' + packageFilePath + '] has a dspec that can''t be read.');
        result := false;
      end
      else
      begin
        identityProblem := DeclaredIdentityProblem(packageIndentity, cPackageDspecFile, spec.MetaData.Id, spec.MetaData.Version);
        if (identityProblem = '') and (spec.TargetPlatform <> nil) and (spec.TargetPlatform.Compiler <> packageIndentity.CompilerVersion) then
          identityProblem := Format('its %s says it is for %s', [cPackageDspecFile, CompilerToString(spec.TargetPlatform.Compiler)]);
        if identityProblem <> '' then
        begin
          RejectMisnamedPackage(logger, packageFilePath, packageIndentity, identityProblem);
          result := false;
        end;
      end;
      if not result then
        RemoveExtraction(logger, packageFolder);
    end;
    if result then
      logger.Verbose('Package  [' + packageFilePath + '] added to cache.');

    // Write the verification receipt as the FINAL step of a successful
    // verify-and-extract — V-31. Its presence is the signal that extraction
    // completed; a crash leaves no receipt and we re-fetch on next use.
    if result and (FReceiptService <> nil) and (FSigningService <> nil) then
      WriteReceipt(packageFolder, packageIndentity, verifyResult);

  except
    on e : exception do
    begin
       logger.Debug('[PackageCache] Error Extracting Package file : ' + e.Message);
       TDirectory.Delete(packageFolder, true); //just empties it but doesn't delete?
       //the extraction failure below is what matters.
       if not RemoveDir(packageFolder) then
         logger.Verbose('Unable to remove the partly extracted folder [' + packageFolder + '] : ' + SysErrorMessage(GetLastError));
      //raising here, if we don't we end up here again!
      raise EDPMError.Create(TErrorCode.E3010_PackageExtractFailed, 'Unable to extract [' + packageFilePath + '] into the package cache : ' + e.Message);
    end;
  end;
end;

procedure TPackageCache.SetLocation(const value : string);
begin
  FLocation := value;
end;

end.

