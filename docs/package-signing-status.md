# DPM Package Signing — Implementation Status

Companion to [package-signing-client-plan.md](package-signing-client-plan.md).
Records what's landed, what's outstanding, and what's been intentionally
deferred. Reference the plan for the *what and why*; this file is purely the
*where are we*.

Last updated: 2026-05-23

---

## Quick scorecard

| Phase | Class items | Status |
|-------|-------------|--------|
| **Phase 1** — Core Signing MVP | M-1..M-12, S-1..S-8, V-1..V-19, V-22 (`permissive`/`require`), V-23, V-25, V-27, V-30..V-34 | ✅ **Complete** — verified end-to-end against a real hardware token + DigiCert TSA |
| **Phase 2** — Production Signing | + V-20, V-21, V-22 (`repository-required`/`author-and-repository`), V-24 | ✅ **Complete** — code + tests; no live trusted repo to test against yet |
| **Phase 3** — Enterprise Features | + V-26 + remote providers | ⚠️ **Mostly complete** — see §Phase 3 below; one runtime risk flagged |
| **Phase 4** — Advanced Supply-Chain | — | ⏸️ **Not started** (future work per plan) |

**Test count**: 348 tests passing as of 2026-05-23, plus 18 base64url
codec tests and the §3.4 revoke / §3.3 v2 round-trip suite still landing.

---

## Phase 1 — Core Signing MVP

All items below verified end-to-end against a real hardware token signing a
real `.dpkg`, timestamped by DigiCert, then verified clean.

### Core crypto layer
- `DPM.Core.Crypto.Win32` — Win32 imports for crypt32 / bcrypt / ncrypt.
- `DPM.Core.Crypto.Hashing` — SHA-256/384/512 via BCrypt, SHA-1/MD5 hard-rejected.
- `DPM.Core.Crypto.X509` — `ICertificate` / `ICertificateChain` wrappers, SPKI extraction.
- `DPM.Core.Crypto.Cms` — Detached CMS sign + verify + unsigned-attribute attach.
- `DPM.Core.Crypto.Timestamping` — RFC3161 request + verify via `CryptRetrieveTimeStamp`.
- `DPM.Core.Crypto.Provider` — `TCertStoreSigningProvider`, `TPfxSigningProvider`.
- `DPM.Core.Crypto.Algorithms` — V-16 mandatory algorithm profile (single source of truth).

### Manifest + archive
- `DPM.Core.Package.Manifest` — deterministic emitter + hardened JSON parser (M-1..M-12, V-1, V-2).
- `DPM.Core.Package.Archive` — V-9..V-13 archive-format rules.
- Pack command emits `dpm-manifest.json` post-archive-close.
- Pack writer normalises archive entry names to forward-slash (V-12) — was
  the root cause of the first round-trip verify failure.

### Trust set + policy
- `DPM.Core.Trust.TrustSet` — built-in YAML trust set (loaded from RC resource, falls back to safe defaults).
- `DPM.Core.Trust.Policy` — `TTrustPolicy` record + `TTrustPolicyService` with
  policy-fingerprint hash for receipt invalidation (V-32, V-33).
- `DPM.Core.Trust.State` — TOFU author no-downgrade ratchet (V-25) with
  YAML state at `%APPDATA%\.dpm\trust-state.yaml`, atomic write-then-rename.
- `DPM.Core.Trust.Prompt` — `ITrustPromptStrategy` abstraction for author downgrade prompts.
  - `TConsoleTrustPromptStrategy` — CLI `[t]rust/[b]lock/[a]lways-block` on stdin.
  - `TIdeTrustPromptStrategy` — IDE wraps `TTrustPromptForm`.
  - `TNonInteractiveTrustPromptStrategy` — safe-default fallback for headless contexts.
- Validation mode dropped `allowUnsigned` after the user flagged it as
  redundant — the mode alone determines unsigned acceptance.

### Receipts
- `DPM.Core.Package.Cache.Receipt` — YAML `dpm-verification-receipt.yaml`
  written as the *final* step of successful verify-and-extract (V-31).
- Quick-recheck path uses receipt + manifest re-hash on cache hits (V-33).
- Receipt invalidation on trust-policy fingerprint mismatch — forces re-verify
  whenever the user touches their trusted publishers / repos / mode (V-32).

### Cache verification gate
- `TPackageCache.InstallPackageFromFile` is the single integration point —
  verification runs *before* extraction so a hostile archive never gets
  written to disk.
- `EvaluateAuthorDowngrade` walks the policy + state ratchet and either
  accepts, hard-fails, or prompts via the strategy.

### CLI commands
- `dpm sign` — signs a `.dpkg` (cert store or PFX).
- `dpm verify` — full verification workflow, human or `--json-output`
  shape, `--offline` for cached revocation only.
- `dpm trust list|add|remove|show` — manages trusted publishers/repos.
- `dpm cache verify` — re-verifies every cached package (V-34).

### IDE
- Splash + about box show trust-set version.
- `TPackageDetailsFrame` shows a signing-status badge above the version selector.
- `TTrustPromptForm` modal for author-downgrade decisions.
- Tools > DPM > Verify Package Cache.
- All units wired into 16 IDE plugin projects.

### Conformance covered
M-1..M-12, S-1..S-8, V-1..V-19, V-22 (`permissive`/`require`), V-23, V-25,
V-27, V-30..V-34.

---

## Phase 2 — Production Signing

### Repository signature verification (V-20)
- Verifier reads `signatures/*.p7s` and routes each per role (author/repository).
- Untrusted repo SPKI → signature silently skipped (V-20).
- Trusted repo SPKI → fully verified.

### Repository attestation reading (V-21)
- `dpmRepositoryAttestation` signed attribute parsed only from
  trusted-repository signatures.
- Attestation surfaced on `TSignatureInfo.Attestation`, in receipt YAML,
  and in `dpm verify` output (namespace, author SPKI, unsigned-reason).

### Repository no-downgrade ratchet (V-24)
- `TRepositoryTrustEntry` added to `ITrustStateService`.
- `EvaluateRepositoryRatchet` in cache: once seen carrying a trusted-repo
  signature, any later build lacking one — or attesting a different
  namespace — is a hard fail regardless of mode.

### Validation modes (V-22 full set)
- `TTrustModeEvaluator.Evaluate` — single truth-table function covering
  all four modes × `(hasAnySig, hasValidAuthor, hasValidTrustedRepo)`.
- 18 exhaustive truth-table tests in `DPM.Core.Tests.Trust.Modes`.

### Project-level lock — `PackageReference.manifestHash` (§2.6)
- Round-trip plumbed through `DPM.Core.Project.Editor`:
  - Read: `<PackageReference manifestHash="sha256:..."/>` populates `IPackageReference.ManifestHash`.
  - Write: `UpdatePackageReferences` emits the attribute when set.
- Installer wiring:
  - After successful install → `PopulateManifestHashes(graph)` reads each
    package's receipt and stamps the hash onto the reference.
  - During restore → `ValidateLockedManifestHashes(graph)` compares the
    reference's stored hash against the receipt; mismatch is a hard fail.
- `IReceiptService` injected into `TPackageInstaller`.

### Conformance added
V-20, V-21, V-22 (`repository-required`, `author-and-repository`), V-24.

---

## Phase 3 — Enterprise Features

### §3.1 — Timestamp-aware revocation (V-26)  ✅
- `ICertificateChain.BuildAtTime(cert, additional, asOfTime, checkRevocation)`
  passes `pTime = signingTime` to the Win32 chain engine so revocations that
  post-date the signature don't fail the chain.
- `CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT` enables online CRL/OCSP
  via Windows' built-in caching.
- `ICertificateChain.RevocationStatus : TRevocationStatus` exposes
  `rsGood`/`rsRevoked`/`rsUnknown`/`rsNotChecked`. Surfaced on
  `TSignatureInfo.Revocation`, receipt's `revocationStatus`, and verify output.

### §3.1 follow-up — keyCompromise reason  ✅
- `CERT_REVOCATION_INFO` / `CERT_REVOCATION_CRL_INFO` / `CRL_ENTRY` struct
  imports + `szOID_CRL_REASON_CODE`.
- `CaptureLeafRevocationDetail` walks the leaf chain element's
  `pRevocationInfo` and parses the CRL reason extension.
- Second-pass build at "now" detects post-signing revocations; if the reason
  is `keyCompromise` AND `policy.AllowKeyCompromiseOverride` is false, the
  signature is retroactively invalidated.
- `TSignatureInfo.CurrentRevocationReason` carried through to verify output.

### §3.2 — `--offline` for `dpm verify`  ✅
- `TVerifyFlags.Offline` flag plumbed through the service overload.
- When set, chain build skips network CRL/OCSP fetches but still consults
  Windows' on-disk cache. Unreachable revocation surfaces as `rsUnknown`.

### §3.2 follow-up — DPM-managed revocation cache  ⏸️ Deferred
- Low marginal value over the Windows chain-engine cache.
- Useful mainly for stricter `--offline` semantics; can wait.

### §3.3 — Remote signing providers  ⚠️ Code complete, runtime-untested-against-real-services

#### `ISigningProvider` extended
- `IsLocal : boolean` — branch flag.
- `SignDigest(digest, algorithm) : TBytes` — remote-friendly primitive that
  returns the raw signature value (RSA/ECDSA) over a pre-computed digest.
- Local providers (cert-store, PFX) keep the existing `AcquirePrivateKey`
  path; their `SignDigest` raises `ENotImplementedSignDigest`.

#### Azure Key Vault — `DPM.Core.Crypto.Provider.Azure`
- `TAzureAccessTokenService` — AAD client-credentials flow with on-disk
  token cache at `%APPDATA%\.dpm\azure-token-cache\<tenant>-<client>.json`.
- `TKeyVaultClient` — REST wrapper for `/certificates/{name}` (cert download)
  and `/keys/{name}/sign` (digest-sign).
- `TKeyVaultSigningProvider` — full `ISigningProvider` implementation.
- Routes through `DPM.Core.Utils.Base64Url` (extracted to its own unit) +
  `VSoft.Base64` polyfill so it compiles back to XE2.
- CLI: `--provider keyvault --vault-url ... --cert-name ... --tenant-id ...
  --client-id ... --client-secret-env ...`.

#### Signotaur — `DPM.Core.Crypto.Provider.Signotaur`
- Full REST implementation matching the gRPC proto shapes the user supplied
  (`GetCertRequest`/`Response`, `SignRequest`/`Response`).
- Endpoints (assumed pending the real REST surface):
  - `POST {endpoint}/api/v1/cert/get`
  - `POST {endpoint}/api/v1/sign`
- JSON field names mirror proto PascalCase verbatim; binary fields are
  standard base64 (not base64url — different from Azure KV).
- Uses the server-returned `Thumbprint` for the sign call (audit-trail accuracy
  even when the caller selected by Subject/Label).
- CLI: `--provider signotaur --endpoint ... --api-key-env ... [--thumbprint
  <hex> | --subject "CN=..." | --label "MyCert"]`.

#### Manual CMS assembly — `TCmsService.SignRemote`
- Branches in `TCmsService.Sign` when `provider.IsLocal = false`.
- Flow:
  1. Hash the content (manifest) → `messageDigest`.
  2. Build `contentType=id-data` + `messageDigest` + caller attrs into a `CRYPT_ATTRIBUTE` array.
  3. Encode each via `CryptEncodeObjectEx(PKCS_ATTRIBUTE)`, byte-sort per DER, wrap in SET — these are the to-be-signed bytes.
  4. Hash the SET → digest.
  5. **Single remote call**: `provider.SignDigest(digest, alg)` → raw signature bytes.
  6. Stuff signature + cert into `CMSG_SIGNER_INFO`, encode via `CryptEncodeObjectEx(PKCS7_SIGNER_INFO)`.
  7. Hand-encode the outer `SignedData SEQUENCE`.
  8. Wrap in `ContentInfo` via `CryptEncodeObjectEx(PKCS_CONTENT_INFO)`.
- New Win32 imports: `CMSG_SIGNER_INFO`, `CRYPT_CONTENT_INFO`, `PKCS_CONTENT_INFO`,
  `PKCS7_SIGNER_INFO`, `X509_ALGORITHM_IDENTIFIER`, `szOID_RSA_RSA`.

#### Risk to verify against a real service
The SET-of-Attributes hashing assumes Windows' `PKCS7_SIGNER_INFO` encoder
uses the same DER byte-sort the assembly code does for the `[0] IMPLICIT
signedAttrs` field. Per DER spec both must sort byte-wise ascending; if they
diverge, end-to-end verification will reject with a signature mismatch.
Easy to diagnose with `dpm verify -v=debug` once the first KV-signed package
round-trips.

#### Known gaps in §3.3
- **ECDSA support**: `TKeyVaultSigningProvider.MapAlgToJwa` defaults to RSA.
  Stub branch for ECDSA exists but is disabled — needs `ICertificate` to
  surface the public-key algorithm OID before it can light up.
- **PSS signatures**: not currently supported. RSA PKCS#1 v1.5 only.
- **Round-trip unit test**: deferred. Would require self-signed cert + key
  generation infrastructure. KV/Signotaur end-to-end is the realistic
  validation path.

### §3.4 — Repository key rotation  Partial (data layer only)
- `ITrustSet.RevokedRepositorySpkis : TArray<string>` — emergency revocation
  channel. A repository SPKI listed here is treated as untrusted regardless
  of any other configuration.
- `TTrustPolicyService.RepositoryTrusted` consults the revoked list first and
  returns false on match, overriding both the user's `trustedRepositories`
  pin list and the trust set's own `repositorySpki` pins.
- YAML loader tolerates both quoted and unquoted forms (reassembles the
  YAML "colon → mapping" quirk for unquoted `sha256:xxxx` entries).
- 4 trust-set parse tests + 4 policy-level revoke override tests.
- **Not implemented**: signed-rollover-metadata client (fetch
  `/trustset/rollover.json` from a trusted repo). Needs gallery
  infrastructure to be testable end-to-end.

### §3.5 — CI/CD ergonomics  ✅
- `dpm verify --json-output` — single JSON object on stdout (chatter routes
  to stderr via the logger). CI pipelines can pipe directly into `jq`.
- `--offline` flag (covered in §3.2).
- Sign command emits structured per-step log lines (signer, digest,
  timestamp request/response, blob write).
- `CheckBool` helper now appends `SysErrorMessage(code)` so every Crypto API
  failure produces a readable error (e.g. `CryptMsgControl(...) failed:
  0x80091008 — Cannot find object or property.`).

---

## Phase 4 — Advanced Supply-Chain Features

Not started. Per the plan, this is mostly future work that depends on
ecosystem pieces (transparency log servers, TUF metadata distribution,
attestation tooling). Architectural hooks are noted but no code yet.

---

## Cross-cutting items still outstanding

1. **IANA PEN.** Resolved as far as it can be pre-assignment. The interim
   arc is `1.3.6.1.4.1.95860.x` — distinctively above the current IANA
   issuance frontier so collision risk is low. When the IANA assignment
   lands, migration is dual-emit + always-accept rather than a flag-day
   swap; full plan in [package-signing-oid-migration.md](package-signing-oid-migration.md).
   The interim arc constants in
   [DPM.Core.Package.Signing.pas](../Source/Core/Package/Signing/DPM.Core.Package.Signing.pas)
   are load-bearing forever — old packages embed them and must keep verifying.

2. **Built-in trust-set RC resource.** The trust set loader expects a
   `DPM_TRUST_SET` YAML resource compiled into the binary. Currently
   missing — the loader tolerates the absence and falls back to safe
   defaults. Production release needs the resource populated with the
   actual DPM gallery's SPKI and the trust-set version number.

3. **§2.5 default-mode auto-tightening.** The trust set may carry
   `defaultValidationMode` and the loader reads it, but the gallery isn't
   yet repository-signed so it ships as `permissive`. Auto-tightening to
   `repository-required` lands when the gallery is ready.

4. **Negative-corpus `.dpkg` fixtures** under `tests/fixtures/signing/`
   for every conformance requirement. The verification logic is covered
   by unit tests today; pre-built fixture packages would add an extra layer
   of confidence and catch regressions in archive validation rules.

5. **Round-trip test for `TCmsService.SignRemote`.** See §3.3 known gaps.

6. **IDE per-version smoke checklists.** Per the plan §IDE-11, every Delphi
   version we ship the IDE plugin for needs a one-time UI smoke check
   (status badge renders, trust prompt fires, About box version shown, etc.).
   Not blocking a release but should happen before public Phase 1 announcement.

---

## How to validate the work

For each major capability, the smoke test sequence:

```text
# Sign with hardware token
dpm.exe sign Foo.dpkg --thumbprint <hash> --timestamper http://timestamp.digicert.com

# Verify it
dpm.exe verify Foo.dpkg               # human output
dpm.exe verify Foo.dpkg --json-output # CI shape
dpm.exe verify Foo.dpkg --offline     # skip network revocation

# Trust the publisher
dpm.exe trust add publisher --spki <hex> --name "VSoft Technologies"

# Install (drives the cache verification gate)
dpm.exe install Foo  # in a project dir

# Re-verify entire cache
dpm.exe cache verify
```

For Key Vault end-to-end (the §3.3 v2 risk-verification):

```text
set DPM_AAD_SECRET=<your-secret>
dpm.exe sign Foo.dpkg --provider keyvault ^
  --vault-url https://my-vault.vault.azure.net ^
  --cert-name CodeSigning ^
  --tenant-id <tenant> ^
  --client-id <client> ^
  --client-secret-env DPM_AAD_SECRET ^
  -v=debug
dpm.exe verify Foo.dpkg -v=debug
```

If the verify step reports `RFC3161 timestamp verification failed` or
`signature does not verify`, the most likely cause is DER byte-sort
divergence between the manual assembly code and Windows' `PKCS7_SIGNER_INFO`
encoder. Inspect `signatures/author-1.p7s` with `signtool verify /v /pa`
to cross-check.
