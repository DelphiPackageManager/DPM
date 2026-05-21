# DPM Client — Package Signing Implementation Plan

## Context

DPM is gaining a package signing system whose architecture, conformance
requirements, and threat model are defined in three companion documents:

* [`package-signing.md`](package-signing.md) — architecture and rationale.
* [`package-signing-confirmance.md`](package-signing-confirmance.md) —
  numbered, testable requirements grouped by conformance class.
* [`package-signing-threat-model.md`](package-signing-threat-model.md) —
  threats, mitigations, and accepted residual risk.

Those documents are deliberately implementation-agnostic. This plan is the
**client-side implementation plan**: what code we build, where it slots into
the existing DPM codebase, in what phase, and how we verify it.

The conformance document defines four classes (M, S, V, R). The DPM **client**
implements three of them — Manifest Producer, Signer, Verifier — and the IDE
plug-in surfaces the same Core functionality in the Delphi IDE. Repository (R)
is server-side and is the subject of a separate plan.

Phasing follows the architecture document's P1–P4. This plan covers all four
phases at comparable depth so the architecture, the data structures, and the
unit boundaries are right from day one. Per-phase deliverables are scoped so
that no later phase requires reshaping a Phase 1 abstraction.

---

## Scope

**In scope:**

* Conformance classes **M, S, V** in [`dpm.exe`](../Source/Cmdline/) and the
  shared [`Source/Core/`](../Source/Core/) library.
* IDE plug-in changes across every supported Delphi version (XE2 through 13.0
  — ten parallel `.dproj` files under [`Source/`](../Source/)).
* New CLI commands: `dpm sign`, `dpm verify`, `dpm cache verify`,
  `dpm trust ...`.
* Integration of verification into the existing install/restore pipeline
  (the central change — see [Phase 1 → Install/Restore integration](#17-installrestore-integration-v-class--the-central-change)).

**Out of scope:**

* Conformance class **R** — gallery server, publisher accounts, repository
  signing infrastructure, publish-time policy. Covered separately.
* Package *content* security review. As the threat model states, DPM signs code
  but does not audit it (T-15).
* Cross-platform crypto. DPM is Windows-only by design; the implementation
  uses CryptoAPI / CNG directly.

**Conformance target by phase:**

| Phase | Classes claimed | Headline scope |
| ----- | --------------- | -------------- |
| P1    | M-1…M-12, S-1…S-8, V-1…V-19, V-22 (`permissive`/`require`), V-23, V-25, V-27, V-30…V-34 | MVP: manifest, author signing, integrity verification, cache receipts, TOFU author no-downgrade |
| P2    | + V-20, V-21, V-22 (`repository-required`/`author-and-repository`), V-24 | Repository signatures, attestation, full trust policy, project-level lock |
| P3    | + V-26 | Revocation (CRL+OCSP, timestamp-aware), remote signing providers, repository key rotation handling |
| P4    | (architectural hooks only) | Transparency-log client hooks, SBOM/attestation reading |

The full list of which requirement IDs are met by which phase appears in the
per-phase sections below.

---

## Architectural principles (recap)

These constrain every decision in the plan and are restated here so they don't
need rationalising in each section:

1. **No custom cryptography, no custom ASN.1.** Every crypto operation goes
   through CryptoAPI / CNG. The verifier uses
   `CryptVerifyDetachedMessageSignature`, `CertGetCertificateChain`,
   `CertVerifyCertificateChainPolicy`, `CryptRetrieveTimeStamp`,
   `BCryptHashData`, and the timestamp verification API. RFC3161 tokens are
   round-tripped through the OS, never parsed by hand.
2. **`ISigningProvider` is the only place a private key is touched.** The CMS
   layer accepts a provider and never opens stores or files itself. Local
   store, PFX, and (Phase 3) remote providers (Azure Key Vault, Signotaur)
   are interchangeable implementations.
3. **The cryptography layer knows nothing about packages.** It operates on
   byte arrays, certificates, hashes, and CMS blobs. Package-specific logic
   lives one layer up, in `DPM.Package.Signing`.
4. **Verification never re-emits the manifest.** Every signature is verified
   against the *exact stored bytes* of `dpm-manifest.json`. The verifier
   parses the manifest only to check the file set; it never regenerates it
   for comparison (architecture doc §Manifest Design and conformance V-1).
5. **Trust pins are SPKI hashes, never Common Names** (conformance V-23).
6. **Selection is by signed attribute + SPKI, never by file name**
   (conformance V-18).

---

## High-level module layout

New units are prefixed `DPM.Crypto.*`, `DPM.Package.Signing*`, and `DPM.Trust.*`.
Where existing units gain responsibility, they are noted with **(extended)**.

| Unit | Layer | Role |
| ---- | ----- | ---- |
| `DPM.Crypto.Win32` | Core/Crypto | Flat Win32 imports for `crypt32.dll`, `bcrypt.dll`, `ncrypt.dll` and constants. Single API surface for the whole crypto layer. |
| `DPM.Crypto.Hashing` | Core/Crypto | `IHasher` interface + SHA-256/384/512 implementations over BCrypt. Streaming hash of files and byte arrays. Algorithm allowlist enforced here. |
| `DPM.Crypto.X509` | Core/Crypto | `ICertificate`, `ICertificateStore`, chain building and policy validation. Wraps `CERT_CONTEXT`/`HCERTSTORE` lifetime. |
| `DPM.Crypto.Cms` | Core/Crypto | `ICmsSignedData` build and decode. Custom signed-attribute read/write via `CryptMsgGetParam` / `CryptMsgControl`. Detached-mode only. |
| `DPM.Crypto.Timestamping` | Core/Crypto | RFC3161 token request and verification. |
| `DPM.Crypto.Provider` | Core/Crypto | `ISigningProvider` abstraction + `TCertStoreProvider`, `TPfxProvider`. Phase 3 adds `TKeyVaultProvider`, `TSignotaurProvider`. |
| `DPM.Crypto.Algorithms` | Core/Crypto | The mandatory algorithm profile (V-16). Single source of truth used by Hashing, CMS, and Timestamping. |
| `DPM.Package.Manifest` | Core/Package | Canonical manifest generation (purpose-built deterministic emitter) and hardened JSON parser for verification. |
| `DPM.Package.Archive` **(extended)** | Core/Packaging | Archive-format rule enforcement (V-9…V-13). Built on existing `System.Zip` code in [`Source/Core/Packaging/`](../Source/Core/Packaging/). |
| `DPM.Package.Signing` | Core/Package | Orchestration of `dpm sign` and `dpm verify`. The only unit that knows about both packages and crypto. |
| `DPM.Trust.Set` | Core/Trust | Built-in versioned trust set (publisher/repository SPKI pins, accepted roots). Loaded from embedded resource. |
| `DPM.Trust.Policy` | Core/Trust | Effective trust policy: validation mode + `trustedPublishers` + `trustedRepositories` + `authorDowngradePolicy`. Used by Verifier. |
| `DPM.Trust.State` | Core/Trust | Per-user sticky trust state file (no-downgrade ratchet, TOFU pins). |
| `DPM.Package.Cache.Receipt` | Core/Cache | Verification receipt file format and read/write. |
| `DPM.Console.Command.Sign` | Cmdline | `dpm sign` CLI command. |
| `DPM.Console.Command.Verify` | Cmdline | `dpm verify` CLI command. |
| `DPM.Console.Command.Trust` | Cmdline | `dpm trust list/add/remove/show` CLI command. |
| `DPM.Console.Command.Cache` **(extended)** | Cmdline | `dpm cache verify` subcommand added. |
| `DPM.Console.Command.Pack` **(extended)** | Cmdline | Emits canonical manifest as part of pack. |
| `DPM.Core.Package.Installer` **(extended)** | Core/Package | Calls Verifier before extraction; honours validation mode. |
| `DPM.Core.Cache` **(extended)** | Core/Cache | Writes receipts on ingest; fast-path receipt re-check on cache hit. |
| `DPM.IDE.SigningStatusFrame` | IDE | UI for signature status / signer identity in PackageDetails. |
| `DPM.IDE.TrustOptionsFrame` | IDE | New tab in Tools > Options > DPM for trust policy. |
| `DPM.IDE.TrustPromptForm` | IDE | Modal dialog for `authorDowngradePolicy=prompt` decisions. |

These new Units will be placed in appropriate folders under the `Source\Core`, `Source\IDE` or `Source\CmdLine` folders.

---

## Phase 1 — Core Signing MVP

**Conformance covered:** M-1…M-12, S-1…S-8, V-1…V-19, V-22 (`permissive`,
`require`), V-23, V-25, V-27, V-30…V-34.

**Threats addressed:** T-1, T-2, T-3 (partial — pinning works against
attackers, not yet repository attacker), T-4, T-5, T-6, T-7 (TOFU-limited),
T-8 (TOFU-limited), T-9, T-12, T-23, T-25.

**Out of Phase 1:** repository signing, repository-based trust modes,
revocation, remote providers, multi-repository handling.

### 1.1 New Core units — public surface

#### `DPM.Crypto.Win32`

Flat import unit for `crypt32.dll`, `bcrypt.dll`, `ncrypt.dll`, plus the
necessary structures (`CMSG_SIGNER_ENCODE_INFO`, `CRYPT_SIGN_MESSAGE_PARA`,
`CERT_CHAIN_PARA`, `CERT_CHAIN_POLICY_PARA`, `CRYPT_TIMESTAMP_PARA`, etc.) and
OIDs. Single namespace for every external declaration in the crypto layer.
Naming follows the project's existing convention (one unit, `external 'dll'`
declarations near the top), since the codebase has no precedent for either a
single API unit *or* per-unit imports for Win32 crypto (none exist today —
hashing currently uses a hand-rolled implementation in
[`DPM.Core.Utils.Hash`](../Source/Core/Utils/DPM.Core.Utils.Hash.pas)).

#### `DPM.Crypto.Hashing`

```pascal
type
  THashAlgorithm = (haSha256, haSha384, haSha512);

  IHasher = interface
    procedure Update(const buffer; size: NativeUInt);
    function Finish: TBytes;
  end;

  IHashingService = interface
    function CreateHasher(algorithm: THashAlgorithm): IHasher;
    function HashFile(const filename: string; algorithm: THashAlgorithm): TBytes;
    function HashBytes(const data: TBytes; algorithm: THashAlgorithm): TBytes;
    class function ParseAlgorithm(const name: string; out algorithm: THashAlgorithm): boolean; static;
    class function AlgorithmName(algorithm: THashAlgorithm): string; static;
  end;
```

* Streams files in 64KB chunks via `BCryptHashData`.
* `ParseAlgorithm` rejects `sha1`, `md5`, and unknown names — this is one of
  the two places (with `DPM.Crypto.Algorithms`) where the V-16 allowlist is
  enforced.
* The existing [`THashSHA256`](../Source/Core/Utils/DPM.Core.Utils.Hash.pas) is
  replaced for new code paths but kept temporarily to avoid breaking the
  existing `.sha256` sidecar mechanism in
  [`TPackageCache.GetPackageHash`](../Source/Core/Cache/DPM.Core.Cache.pas).
  The sidecar mechanism is itself superseded by the receipt file in this
  phase (see [Cache changes](#18-cache-changes--verification-receipts)).

#### `DPM.Crypto.X509`

```pascal
type
  ICertificate = interface
    function SubjectCommonName: string;     // display only — never a trust input
    function Thumbprint: string;             // SHA-1, display only
    function SpkiHash(algorithm: THashAlgorithm): TBytes;
    function NotBefore: TDateTime;
    function NotAfter: TDateTime;
    function HasCodeSigningEku: boolean;
    function RawHandle: PCCERT_CONTEXT;
  end;

  ICertificateStore = interface
    function FindByThumbprint(const thumbprint: string): ICertificate;
    function FindBySpki(const spkiHash: TBytes): ICertificate;
  end;

  TChainResult = (crValid, crUntrustedRoot, crExpired, crRevoked,
                  crWrongUsage, crIncomplete, crUnknownError);

  ICertificateChain = interface
    function Build(const cert: ICertificate; const additionalCerts: array of ICertificate): TChainResult;
    function VerifyForCodeSigning(asOfTime: TDateTime): TChainResult;
    function Roots: TArray<ICertificate>;
  end;
```

* Wraps `CERT_CONTEXT`/`HCERTSTORE` in interfaces so handles release in
  destructors (XE2 — no managed records, per [`CLAUDE.md`](../CLAUDE.md)).
* `SpkiHash` is the trust primitive. Computed by extracting the
  `SubjectPublicKeyInfo` field via `CryptEncodeObjectEx` (no ASN.1 by hand) and
  hashing the DER bytes.
* `VerifyForCodeSigning(asOfTime)` is what enables timestamp-aware chain
  validation: in Phase 1 `asOfTime` is the RFC3161 timestamp, in Phase 3 the
  same parameter feeds the timestamp-aware revocation check.

#### `DPM.Crypto.Cms`

```pascal
type
  ICmsSignedAttribute = interface
    function Oid: string;
    function RawValue: TBytes;
  end;

  ICmsSignedData = interface
    function ContentDigest: TBytes;
    function SignerCertificate: ICertificate;
    function EmbeddedCertificates: TArray<ICertificate>;
    function SignedAttributes: TArray<ICmsSignedAttribute>;
    function UnsignedAttributes: TArray<ICmsSignedAttribute>;
    function FindSignedAttribute(const oid: string): ICmsSignedAttribute;
    function FindUnsignedAttribute(const oid: string): ICmsSignedAttribute;
    function ToDer: TBytes;
  end;

  ICmsService = interface
    function Sign(const content: TBytes;
                  provider: ISigningProvider;
                  signedAttributes: TArray<TPair<string, TBytes>>;
                  digest: THashAlgorithm): ICmsSignedData;

    function Decode(const der: TBytes): ICmsSignedData;

    function VerifyDetached(const der: TBytes; const content: TBytes;
                            out signerCert: ICertificate): boolean;

    procedure AddUnsignedAttribute(var der: TBytes;
                                   const oid: string; const value: TBytes);
  end;
```

* `VerifyDetached` is a thin wrapper over
  `CryptVerifyDetachedMessageSignature`. This is the V-15 implementation.
* `AddUnsignedAttribute` uses `CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR` via
  `CryptMsgControl`; called by `DPM.Crypto.Timestamping` to attach the RFC3161
  token after signing.
* Signed attributes are written with `CRYPT_SIGN_MESSAGE_PARA.rgAuthAttr`.
  Attribute values are DER-encoded by the OS using `CryptEncodeObjectEx` —
  the *DPM* layer hands in plain UTF-8 strings or hash bytes and never builds
  ASN.1 itself.

#### `DPM.Crypto.Timestamping`

```pascal
type
  ITimestampToken = interface
    function SigningTime: TDateTime;       // UTC
    function TsaCertificate: ICertificate;
    function RawToken: TBytes;
  end;

  ITimestamper = interface
    function RequestTimestamp(const hash: TBytes;
                              const url: string;
                              digest: THashAlgorithm): ITimestampToken;
    function Verify(const token: TBytes; const dataHash: TBytes): boolean;
  end;
```

* `RequestTimestamp` calls `CryptRetrieveTimeStamp` with a hash of the CMS
  signature, then `DPM.Crypto.Cms.AddUnsignedAttribute` writes it back into
  the signature blob.
* `Verify` calls the corresponding timestamp verification API and returns the
  signing time, used by `ICertificateChain.VerifyForCodeSigning(asOfTime)`.

#### `DPM.Crypto.Provider`

```pascal
type
  ISigningProvider = interface
    function Certificate: ICertificate;
    function SignDigest(const digest: TBytes; algorithm: THashAlgorithm): TBytes;
  end;

  TCertStoreProvider = class(TInterfacedObject, ISigningProvider)
    // wraps CryptAcquireCertificatePrivateKey — covers smart cards/HSM transparently
  end;

  TPfxProvider = class(TInterfacedObject, ISigningProvider)
    // PFX file + password (password supplied via env var or interactive prompt)
  end;
```

* The provider returns a *signature over a digest*, not over content bytes.
  This is what makes Phase 3 remote providers (Key Vault) drop-in: they
  compute the digest locally and call the remote API with just the digest.
* `CmsService.Sign` calls `provider.SignDigest` exactly once per signature;
  every other part of the CMS structure is assembled locally.

#### `DPM.Crypto.Algorithms`

```pascal
type
  TAlgorithmProfile = record
  public
    class function FileHashAllowed(algorithm: THashAlgorithm): boolean; static;
    class function CmsDigestAllowed(algorithm: THashAlgorithm): boolean; static;
    class function TimestampDigestAllowed(algorithm: THashAlgorithm): boolean; static;
    class function SignatureAlgorithmAllowed(const oid: string): boolean; static;
    class function MinRsaKeyBits: integer; static;        // 2048
  end;
```

* Phase 1 allowlist: SHA-256/384/512 for hashes and digests, SHA-256 for
  timestamps, RSA PKCS#1 v1.5 + ECDSA P-256/P-384 for signatures.
* SHA-1 and MD5 are rejected here regardless of what the manifest declares
  (V-16). Adding an algorithm in a future phase is one new line in this
  record; weakening it requires a phase bump.

### 1.2 `DPM.Package.Manifest`

```pascal
type
  TManifestFileEntry = record
    Path: string;
    Size: Int64;
    Hash: TBytes;
  end;

  IPackageManifest = interface
    function DpmPackageFormat: integer;
    function ManifestSchemaVersion: integer;
    function PackageId: string;
    function Version: string;
    function HashAlgorithm: THashAlgorithm;
    function Created: TDateTime;          // UTC, informational only
    function Files: TArray<TManifestFileEntry>;
    function RawBytes: TBytes;            // the exact signed bytes
  end;

  IManifestService = interface
    function Generate(const packageRoot: string;
                      const id, version: string;
                      algorithm: THashAlgorithm): IPackageManifest;
    function Parse(const bytes: TBytes): IPackageManifest;
    function ValidatePath(const path: string): boolean;
  end;
```

**Generation** uses a purpose-built deterministic emitter, *not* a general
JSON library, because [`CLAUDE.md`](../CLAUDE.md) constrains us to XE2 and
`System.JSON` does not guarantee ordering on early versions (architecture doc
§Manifest Design). The emitter writes:

* a fixed key sequence (lexicographic) for the top-level object,
* a sorted `files` array (byte-wise on the canonical NFC path string),
* integers only (no floats — the schema has none),
* UTF-8, no BOM, LF line endings only.

For NFC normalisation we use `NormalizeString` from `normaliz.dll` (already
shipped on every supported Windows version). Inputs that cannot be normalised
are rejected with a clear error (M-7).

**Parsing** uses `VSoft.YAML` (already a dependency, per
[`DPM.Core.Configuration.Interfaces`](../Source/Core/Configuration/DPM.Core.Configuration.Interfaces.pas)),
configured to enforce V-2:

* Reject duplicate object keys
* Reject documents exceeding a configured maximum depth (default 8) and
  maximum size (default 8 MiB — generous; real manifests are tens of KB).
* No silent type coercion: a value declared `int` must be an integer in JSON.

`ValidatePath` enforces every M-8 path-safety rule and is reused by the
producer (pre-emit) and the verifier (post-parse).

### 1.3 Pack command changes (M class)

The existing pack command in
[`DPM.Console.Command.Pack`](../Source/Cmdline/Commands/) currently produces
a `.dpkg` containing `package.dspec.yaml` + content files. After this change
it additionally:

1. Calls `IManifestService.Generate` over the staged package root *before*
   the archive is sealed.
2. Writes `dpm-manifest.json` into the staged root.
3. Optionally invokes `IPackageSigningService.Sign` (see §1.4) if a
   `--sign` argument is present, so a single pack command can produce a
   signed package.
4. Asserts the archive satisfies V-9…V-13 before writing — duplicate-name
   detection, UTF-8 + NFC entry names, only Store/Deflate compression, no
   ADS, no Windows reserved names, no encrypted entries (M-12).

The pack tool also enforces M-8 path safety on each file it adds; this is the
canonical place where an author would discover an unacceptable path during
package authoring.

### 1.4 New CLI command: `dpm sign` (S class)

```text
dpm sign Foo.dpkg --thumbprint AB12CD34EF56
                  [--store CurrentUser|LocalMachine]
                  [--pfx cert.pfx]
                  [--pfx-password-env DPM_PFX_PWD]
                  [--timestamper http://timestamp.digicert.com]
                  [--digest sha256|sha384|sha512]
```

Implementation in `DPM.Console.Command.Sign`:

* Follows the same pattern as
  [`TInstallCommand`](../Source/Cmdline/Commands/DPM.Console.Command.Install.pas):
  extends `TBaseCommand`, holds an `ISigningOrchestrator` injected by DI,
  parses options into a record, returns `TExitCode`.
* PFX password is read from the named env var or via interactive prompt — never
  accepted as a literal command-line value (architecture doc §CLI Design).

`ISigningOrchestrator.Sign` (in `DPM.Package.Signing`):

1. Opens the `.dpkg` and locates `dpm-manifest.json`.
2. Reads its raw bytes.
3. Calls `ICmsService.Sign(rawBytes, provider, signedAttributes, digest)`
   where `signedAttributes` includes the `dpmSignatureRole = author`
   attribute (S-2).
4. Calls `ITimestamper.RequestTimestamp` and attaches the token.
5. Writes the result as `signatures/author-<n>.p7s`, where `<n>` is the
   lowest integer such that no collision exists (the name is non-semantic
   per S-6).
6. Re-seals the `.dpkg` without modifying any existing entry (S-7).

### 1.5 New CLI command: `dpm verify` (V class — manual)

```text
dpm verify Foo.dpkg [--config dpm.config.yaml] [--offline]
```

* Performs the full Verification Workflow (architecture doc §Verification
  Workflow) against the given file using the current trust set + policy.
* `--offline` skips network operations (timestamp authority revocation,
  Phase 3 OCSP). Cached responses are used; absent ones cause graceful
  degradation per active policy.
* Output format follows the example in the architecture doc, identifying
  *who* signed (V-27).

### 1.6 New CLI command: `dpm trust`

```text
dpm trust list
dpm trust add publisher --spki sha256:AB12... --name "VSoft Technologies"
dpm trust add repository --spki sha256:EF56... --url https://packages.delphi.dev
dpm trust remove <publisher|repository> --spki sha256:...
dpm trust show
```

* Read/write the trust policy in `dpm.config.yaml` via the existing
  [`IConfigurationManager`](../Source/Core/Configuration/DPM.Core.Configuration.Interfaces.pas).
* `add` calls verify the SPKI format and refuse plainly malformed input;
  beyond that the user is trusted to know what they're pinning.

### 1.7 Install/Restore integration (V class) — **the central change**

The single most important integration point is
[`TPackageCache.InstallPackageFromFile`](../Source/Core/Cache/DPM.Core.Cache.pas)
at approximately line 529. Today it calls
`TZipFile.ExtractZipFile(packageFilePath, packageFolder)` directly. After
this change the sequence becomes:

```pascal
// (existing) compute target folder
// (NEW) IPackageSigningService.VerifyForIngest(packageFilePath, policy) -> TVerificationResult
//       - applies the entire Verification Workflow before extraction
//       - on failure: log, surface error, return False — no extraction occurs
//       - on success: returns the manifest hash, signer SPKIs, trust decision
// (NEW) policy decision:
//       - permissive + unsigned: integrity-checked install allowed
//       - require + missing/invalid signature: hard fail
//       - no-downgrade ratchet consulted (see §1.10)
// (existing) ExtractZipFile
// (existing) verify dspec
// (NEW) write receipt file as the final step (§1.5)
```

`TVerificationResult` is the record persisted into the receipt and returned to
callers; see §1.8.

A separate, smaller change in
[`TPackageCache.EnsurePackage`](../Source/Core/Cache/DPM.Core.Cache.pas) at
approximately line 471 (the cache-hit fast path) calls
`IPackageSigningService.QuickRecheck(packageFolder, policy)`:

* Confirms the receipt exists and is well-formed.
* Confirms the receipt's policy fingerprint matches the active policy
  (V-33); if not, falls back to the full re-verify path.
* Recomputes only the `dpm-manifest.json` hash and compares it to the
  receipt — no CMS, no chain build, no content rehash. If the manifest hash
  matches, the recorded verification result still holds.

The existing `FExtractionVerified` set on `TPackageCache` (line 53) is
extended into a `Dictionary<TKey, TVerificationResult>` so that downstream
consumers can read the trust decision without touching disk.

`TPackageInstaller` itself needs no behavioural change beyond
forwarding the verification failure through its existing logger. The
verification gate lives entirely inside the cache layer, so install,
restore, and direct CLI install all benefit without separate plumbing.

**Reference: existing flow** —
[`TPackageInstaller.DoInstallPackageForPlatform`](../Source/Core/Package/DPM.Core.Package.Installer.pas)
line ~740,
[`TPackageInstaller.DoRestoreProjectForPlatform`](../Source/Core/Package/DPM.Core.Package.Installer.pas)
line ~865, and
[`TPackageInstaller.DownloadPackages`](../Source/Core/Package/DPM.Core.Package.Installer.pas)
line ~980 all route through the cache, so a single intercept covers both
paths.

### 1.8 Cache changes — verification receipts

New file `dpm-verification-receipt.yaml` is written into each cache folder
under `%APPDATA%\.dpm\packages\{compiler}\{id}\{version}\` as the *final* step
of a successful ingest (V-31). Format:

```yaml
---
dpmReceiptVersion: 1
packageId: VSoft.CommandLine
version: 1.0.0
compiler: DelphiXE2
manifestHashAlgorithm: SHA256
manifestHash: base64...
signatures:
- role: author
  signerSpki: base64...
  signerSubject: CN=VSoft Technologies
  thumbprint: AB12CD34...
  effectiveSigningTime: '2026-05-19T10:00:03Z'
  timestampAuthority: DigiCert
  revocationStatus: notChecked
trustDecision: trusted
trustPolicyFingerprint: sha256:...
verifiedAt: '2026-05-19T10:00:05Z'
dpmVersion: 0.6.0
```

* Written via `VSoft.YAML` (no canonicalisation needed — this file is
  not signed; it is consumed only by DPM itself).
* The `trustPolicyFingerprint` is SHA-256 over a canonical projection of
  `{validationMode, allowUnsigned, trustedPublishers[], trustedRepositories[],
  authorDowngradePolicy, trustSetVersion}`. Any change forces a full
  re-verify on next cache hit.
* Extraction runs under a file lock (existing semantics in `TPackageCache`
  preserved) so concurrent installs of the same package don't race (V-31).

The legacy `.sha256` sidecar mechanism in `TPackageCache.GetPackageHash`
remains for one release as a compatibility shim, then is removed in Phase 2.

### 1.9 Trust set — built-in versioned trust roots

The DPM client ships with a `dpm-trust-set.yaml` resource compiled into the
binary as an RC resource (one of the few places the project uses resources
today — see `DPM.IDE.Main` for the splash bitmap precedent). Schema:

```yaml
dpmTrustSetVersion: 1
issued: '2026-01-15T00:00:00Z'
repositorySpki:
- name: DPM Official Gallery
  spki: sha256:...
codeSigningRoots: useWindowsRootStore
timestampingRoots: useWindowsRootStore
```

In Phase 1 the code-signing and timestamping roots delegate to the Windows
Root certificate store — the practical equivalent of NuGet's behaviour and
sufficient for the public CA roots required for publicly trusted code-signing
certs.

The trust-set resource is loaded by `DPM.Trust.Set` at startup and cached
in memory. Its version number contributes to the
`trustPolicyFingerprint` so a DPM upgrade with a new trust set invalidates
cache decisions.

Phase 2 will introduce signed rollover metadata for the repository SPKI pins
(architecture doc §Trust Bootstrapping); Phase 1 ships the built-in pins as
the only source.

### 1.10 Author-side no-downgrade rule (TOFU)

`DPM.Trust.State` maintains `%APPDATA%\.dpm\trust-state.yaml` recording, per
package id and per namespace prefix:

```yaml
VSoft.CommandLine:
  lastAuthorSpki: sha256:...
  lastSeenAuthorSigned: true
  lastSeenAt: '2026-05-19T10:00:05Z'
```

Updated atomically (write-temp, rename) at the end of every successful
verify-and-extract.

On the next install/restore of the same id:

* If `lastSeenAuthorSigned = true` and this build is **unsigned**, the
  `authorDowngradePolicy` (default `prompt`) determines the action:
  * `prompt` (default) — CLI: interactive `[y/N]` confirmation; IDE: modal
    `TTrustPromptForm` (see [IDE](#ide-integration)). Decision is recorded
    in state so it isn't repeated.
  * `block` — hard fail. Strict environments / CI.
  * `allow` — accept silently.
* If `lastAuthorSpki` is set and this build is signed by a *different* key,
  the same `authorDowngradePolicy` governs the decision (same options).

The state file is governed by file ACLs (per-user folder); the threat model
explicitly accepts local-attacker tampering as out of scope (T-20…T-22).

### 1.11 Configuration schema additions (Phase 1)

`dpm.config.yaml` gains:

```yaml
signing:
  validationMode: permissive          # permissive | require
  allowUnsigned: true
  authorDowngradePolicy: prompt       # prompt | block | allow
  trustedPublishers: []               # populated in Phase 2 usage; readable in Phase 1
  trustedRepositories: []             # ditto
```

Loaded via the existing
[`IConfigurationManager`](../Source/Core/Configuration/DPM.Core.Configuration.Interfaces.pas).
The schema is forward-compatible — Phase 2 adds `repository-required` and
`author-and-repository` modes without breaking the file format.

### 1.12 Phase 1 test plan

**Unit tests** (DUnit project under
[`Source/Tests/`](../Source/Tests/) — extend, mirror existing layout):

| Suite | Cases |
| ----- | ----- |
| `DPM.Crypto.Hashing.Tests` | Known-answer SHA-256/384/512 vectors; rejection of SHA-1/MD5. |
| `DPM.Crypto.Cms.Tests` | Round-trip sign/verify with a self-signed test cert from the test PFX; verify detection of one-byte content corruption; reject malformed DER. |
| `DPM.Crypto.X509.Tests` | SPKI hash for a known cert matches an externally computed reference; chain build to a trusted self-signed root; EKU rejection. |
| `DPM.Crypto.Timestamping.Tests` | Mock TSA — verify timestamp embedded as unsigned attribute, re-decoded correctly. |
| `DPM.Package.Manifest.Tests` | M-1…M-12: byte-stable output across two emit cycles; lexicographic ordering; integer-only; NFC normalisation; reject every M-8 path violation. |
| `DPM.Package.Manifest.Parser.Tests` | V-2: reject duplicate keys, oversize input, deep nesting, float values, unknown extensions tolerated. |
| `DPM.Package.Archive.Tests` | V-9…V-13: duplicate names, case-colliding NFC variants, symbolic-link entries, ADS paths, reserved device names, Bzip2 entries, encrypted entries — each rejected. |
| `DPM.Trust.State.Tests` | TOFU author no-downgrade: first install records SPKI; second unsigned install consults policy. |

**Integration tests** (new project, signed-package round trip):

* `pack → sign → verify` with a generated self-signed cert produces a package
  whose CMS signature verifies against the manifest bytes and whose receipt
  records the correct SPKI and decision.
* `verify` rejects a package whose manifest has been edited.
* `verify` rejects a package with an unlisted file (V-3).
* `verify` rejects a package missing a listed file (V-3).
* `verify` rejects a package with `Foo.pas` and `foo.pas` (V-9).
* `dpm install` against a `require`-mode config with an unsigned package
  fails; same install in `permissive` succeeds with an integrity-only
  receipt.

**Negative-corpus tests** — small set of pre-built `.dpkg` fixtures committed
under `tests/fixtures/signing/`, each named after the requirement it
exercises. These are golden files; they shouldn't change unless the rule
changes. CI runs verify against each and asserts the expected rejection
reason.

**Manual verification** — `dpm verify` against a real publicly-signed
test package and inspecting the output format matches the architecture doc.

---

## Phase 2 — Production Signing

**Conformance added:** V-20, V-21, V-22 (`repository-required` and
`author-and-repository`), V-24.

**Threats moved from partial to fully mitigated:** T-3, T-7 (when a trusted
repository signature is present), T-8 (ditto), T-10, T-11.

This phase is the moment the client's trust model becomes *full* — repository
signatures, attestations, multi-repository handling, and the lock-file
integration. Most of the heavy lifting is server-side (separate plan); the
client work is consumption-side, but it is the difference between a
ratcheting TOFU model and a properly anchored trust model.

### 2.1 Repository signature handling (V-20)

`DPM.Trust.Policy` is extended to know about *trusted repositories* keyed by
SPKI. The signature-selection logic in `DPM.Package.Signing.Verify` (existing
from Phase 1) gains a second match arm:

* Repository signatures from a trusted repository SPKI are fully verified.
* Repository signatures from an *untrusted* repository SPKI are skipped
  entirely — neither verified, nor an error, nor a warning (V-20).
* Author signatures continue to match against `trustedPublishers`.

The `(role, signerSpki)` index built in step 4 of the Verification Workflow
already exists from Phase 1 — Phase 2 just adds the additional consumer.

### 2.2 Repository attestation reading (V-21)

The `dpmRepositoryAttestation` signed attribute carries the publisher
namespace identifier plus either the verified author SPKI or an unsigned
marker. `DPM.Package.Signing` reads it via the existing
`ICmsSignedData.FindSignedAttribute` API, and exposes it on
`TVerificationResult`:

```pascal
TRepositoryAttestation = record
  RepositorySpki: TBytes;
  Namespace: string;
  AuthorSpki: TBytes;                  // empty if unsigned
  UnsignedReason: TUnsignedReason;     // urNeverSigned | urAuthorCeasedSigning | urNotApplicable
end;
```

Critically, the verifier reads the attestation **only** from a repository
signature whose SPKI is in `trustedRepositories` (V-21). An attestation on
an untrusted signature is ignored.

The attestation is recorded in the verification receipt so the IDE / `dpm
verify` output can render "author key registered to VSoft Technologies"
without re-parsing the signature.

### 2.3 Repository no-downgrade rule (V-24)

`DPM.Trust.State` gains a second dimension keyed by package id (and namespace
prefix):

```yaml
VSoft.CommandLine:
  highestRepositoryAssurance:
    trustedRepoSpki: 'sha256:...'
    namespace: VSoft.*
    firstSeenAt: ...
```

Once a package id has been seen carrying a valid repository signature from a
*trusted* repository attesting namespace *N*, any later build lacking such
a signature, or attesting a different namespace, is a **hard failure**
regardless of the global mode (V-24).

This ratchet is keyed strictly to *trusted* repositories — an attacker
operating their own repository never satisfies it, addressing T-10.

### 2.4 Validation modes `repository-required` and `author-and-repository`

`DPM.Trust.Policy.EvaluateRequirements` is the central decision function;
it takes the active mode + the list of `(role, signerSpki, verifyResult)`
tuples and returns satisfied / not-satisfied + a reason. The Phase 1
implementation handles `permissive` and `require`; Phase 2 adds the two
repository-anchored modes. The function is a single match expression — small
and easy to test exhaustively.

Per the architecture doc note: `repository-required` and
`author-and-repository` apply only to HTTP-based repositories; directory-
based repositories have no way to add a repository signature, so the policy
engine carries a per-source override flag forcing `permissive`-equivalent
behaviour for local folder sources.

### 2.5 Default-mode auto-tightening

The default `validationMode` continues to be `permissive` until the gallery
is fully repository-signed (architecture doc §Validation modes). The trust
set carries a field:

```json
"defaultValidationMode": "permissive"   // becomes "repository-required" when ready
```

Clients applying a newer trust set automatically pick up the tightened
default. `dpm.config.yaml` overrides this if explicitly set by the user.

### 2.6 Project-level lock — `PackageReference` extension

Per the architecture doc §Project-level integrity, lock data lives in the
existing `PackageReference` elements in `.dproj`, not in a separate file.
Each `PackageReference` gains:

```xml
<PackageReference Include="VSoft.CommandLine" Version="1.0.0"
                  ManifestHash="sha256:..." />
```

On restore, [`DPM.Core.Project`](../Source/Core/Project/) reads the attribute
and the cache layer compares it against the receipt's `manifestHash`. A
mismatch fails the restore with a clear error (analogous to NuGet's
`packages.lock.json` content-hash validation).

Generation of the attribute is part of the install/upgrade write-back.
Existing references without the attribute are tolerated and updated on first
restore (since the receipt contains the manifest hash, this is non-trivial
state but cheap to fill in).

### 2.7 Trust-policy-aware cache invalidation

Already implemented in Phase 1 via `trustPolicyFingerprint`; Phase 2's
additions (`trustedPublishers`, `trustedRepositories`,
`authorDowngradePolicy` value changes) automatically flow into the
fingerprint because the projection includes them. No new code path; new
trust-policy fields just need to be added to the canonical projection
function.

### 2.8 CLI / config changes (Phase 2)

```yaml
signing:
  validationMode: repository-required
  allowUnsigned: false
  authorDowngradePolicy: prompt
  trustedPublishers:
    - name: VSoft Technologies
      spki: sha256:AB12CD34...
  trustedRepositories:
    - url: https://packages.delphi.dev
      spki: sha256:EF56AB78...
```

`dpm verify` output extends to render the attestation line ("Verified: author
key registered to VSoft Technologies") when a trusted repository signature is
present (matches the example output in the architecture doc).

### 2.9 Phase 2 test plan

| Suite | Cases |
| ----- | ----- |
| `DPM.Crypto.Cms.Repository.Tests` | Parse and verify a repository signature with `dpmRepositoryAttestation`. |
| `DPM.Trust.Policy.Tests` | Exhaustive truth table for `EvaluateRequirements` across the four modes × (author-signed?, author-trusted?, repo-signed?, repo-trusted?). |
| `DPM.Trust.State.Repository.Tests` | Repository no-downgrade ratchet: once seen trusted, an untrusted-only build of the same id is rejected. |
| `DPM.Trust.Policy.LocalFeed.Tests` | A directory-based source under `repository-required` mode degrades to `permissive`-equivalent. |
| `DPM.Core.Project.PackageReference.Tests` | Round-trip `ManifestHash` attribute; mismatch fails restore. |
| Integration | Multi-repository fixture (one trusted, one untrusted) — the untrusted signature is silently ignored. |
| Integration | Attestation rendered correctly in `dpm verify` output for a fixture that has both signatures. |

---

## Phase 3 — Enterprise Features

**Conformance added:** V-26.

**Threats moved from partial:** T-13 (closes the compromise-to-revocation
window for online clients), T-14 (key rotation handling on the client),
T-17 (timestamp-aware revocation reduces the attack window).

### 3.1 Revocation checking — CRL + OCSP, timestamp-aware (V-26)

`DPM.Crypto.X509` is extended with revocation. Implementation uses Win32
chain engine flags (`CERT_CHAIN_REVOCATION_CHECK_END_CERT`,
`CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT`) and `CertGetCertificateChain`
+ `CertVerifyCertificateChainPolicy` with `CERT_CHAIN_POLICY_BASE` —
delegating both CRL and OCSP fetching to the OS. No HTTP code in DPM for
revocation.

**Timestamp-awareness is the non-obvious part.** The policy parameter passes
`CERT_CHAIN_POLICY_PARA.dwFlags` together with a custom evaluation:

* The chain is built `asOfTime = signatureTimestamp`.
* If the chain's revocation result is `CRYPT_E_REVOKED`, DPM reads the
  revocation *reason* from the CRL entry (one call to
  `CertVerifyRevocation`) and:
  * If reason is `keyCompromise` (CRL reason code 1), the signature is
    treated as retroactively invalid (V-26 default), unless an explicit
    administrator override exists in `dpm.config.yaml`:
    `signing.allowKeyCompromiseOverride: true`. The override is logged and
    surfaced in `dpm verify` output.
  * Otherwise the signature is accepted, because the revocation post-dates a
    valid timestamped signing.
* The naive "is this certificate revoked now?" path would reject every
  legitimately signed package whose certificate has since been rotated;
  V-26 explicitly prohibits this, and the implementation must match.

### 3.2 Cached / offline revocation responses

OCSP responders are flaky. The Win32 chain engine has a cache, but DPM adds
its own cache for `dpm verify --offline` use cases:

* OCSP responses keyed by `(issuerKeyHash, serialNumber)` cached in
  `%APPDATA%\.dpm\revocation-cache/` with `nextUpdate` from the response.
* Cache is consulted before the OS cache if `--offline` is set.
* The cache is updated opportunistically on every online verify.

This is the only place DPM holds CRL/OCSP bytes itself; it is not a
re-implementation of CRL parsing — the stored bytes are raw OCSP responses,
fed back to the OS via `CertVerifyTimeValidity` and equivalent APIs.

### 3.3 Remote signing providers (Azure Key Vault, Signotaur)

Both fit the existing `ISigningProvider` shape from Phase 1. The key
insight (architecture doc Certificate Support) is that the provider's
`SignDigest(digest, algorithm)` returns the signature over the digest —
nothing about the surrounding CMS structure crosses the network.

#### `TKeyVaultProvider`

* Uses an Azure REST client via `VSoft.HttpClient` (existing dependency).
* AAD authentication via either a service principal (CI scenario) or device
  code (interactive). Token cache in `%APPDATA%\.dpm\azure-token-cache/`.
* The leaf certificate is downloaded from Key Vault once and cached; the
  rest of the chain is supplied via the Windows store / additional cert
  parameter to `Sign`.
* Key Vault rate limits inform retry/backoff (HTTP 429 → exponential backoff
  with jitter, three attempts).

#### `TSignotaurProvider`

* Per the architecture doc, Signotaur will support DPM packages natively in
  a future update; in parallel, DPM can integrate Signotaur as a remote
  provider that performs the sign-digest operation and assembles CMS
  locally. Same shape as the Key Vault provider, different REST API.

Both providers are independently DI-registered and selected by CLI flag:

```text
dpm sign Foo.dpkg --provider keyvault --vault-url https://vsoft.vault.azure.net --cert-name codesigning
dpm sign Foo.dpkg --provider signotaur --endpoint https://signotaur.vsoft.com.au --cert-id 12345
```

### 3.4 Repository key rotation handling (client side)

Already designed-in by Phase 2 (multiple repository SPKIs allowed via the
trust set). Phase 3 adds:

* Signed-rollover-metadata client: when fetching from a trusted repository,
  DPM checks for a `/trustset/rollover.json` endpoint signed by a *currently
  trusted* repository key. New keys learned from rollover metadata are added
  to the trust set in memory and persisted to the user's local trust state
  cache for offline use.
* Emergency revocation channel: the trust set may carry a
  `revokedRepositorySpki` array. A repository SPKI present here is treated
  as untrusted regardless of any other configuration. DPM checks this list
  on every verification.

### 3.5 CI/CD ergonomics

Specific to Phase 3 because remote providers are the main CI-driver:

* `dpm sign` returns non-zero on any failure with structured stderr (one line
  per failure category) suitable for shell parsing.
* `--quiet` and `--json-output` flags on `dpm verify` for build pipelines.
* Documented exit codes appended to `--help` output.

### 3.6 Phase 3 test plan

* CRL revocation rejected for `keyCompromise`, accepted for `superseded` if
  signing timestamp predates revocation.
* OCSP responder offline → cached response used; cache expiry honoured.
* Key Vault provider — integration test against a real Azure tenant in CI
  (gated, optional). Unit test with a mock REST endpoint.
* Signed rollover metadata — fixture: new key signed by old key, new key
  trusted; new key signed by *another new key* (chain broken), rejected.
* Revoked repository SPKI — fixture: present in trust set as revoked,
  signature from that key rejected even if validly chained.

---

## Phase 4 — Advanced Supply-Chain Features

Phase 4 is largely a *future-work* phase per the architecture doc. The
**client** work in this phase is mostly hook-points and reading additional
attestations — the heavy lifting (running the log, building the SBOM
pipeline, distributing TUF metadata) is server / ecosystem work.

### 4.1 Transparency log client hooks

* A new optional unsigned attribute on signatures: `dpmTransparencyLogEntry`,
  carrying the log-server URL and the inclusion proof for that entry.
* On verify, if the active policy has `signing.requireTransparencyLog: true`,
  DPM contacts the log server (online only) and verifies the inclusion
  proof. Otherwise the attribute is informational (rendered in `dpm verify`
  output).
* No log server is run by DPM; the client only validates proofs against the
  log's signed tree head.

### 4.2 SBOM and vulnerability attestations

* New manifest field `extensions.sbom`: a URL or inline reference to an SBOM
  document. Read-only on the client: `dpm sbom` (separate plan,
  [`sbom-command.md`](sbom-command.md)) will consume these.
* Attestation files alongside signatures: `attestations/*.attestation.json`,
  signed by the same author key. Verifier extends to validate them but does
  not act on their content (downstream tooling does — `dpm scan` per
  [`scan-command.md`](scan-command.md)).

### 4.3 Reproducible-build attestations

Optional `dpmReproducibleBuild` signed attribute referencing a build manifest
+ hash. Client renders in `dpm verify` output; verification is read-only.

### 4.4 TUF metadata

If pursued, TUF affects how the trust set itself is delivered rather than
the per-package verification flow. The Phase 4 client work would replace
`DPM.Trust.Set` with a TUF-aware loader (`DPM.Trust.Set.Tuf`) that fetches
top-level metadata, snapshot, timestamp, and targets files from the
gallery. The per-package verifier is unchanged.

### 4.5 Phase 4 test plan

* Transparency-log fixture: inclusion proof verifies; tampered proof rejected.
* Attestation parsing: fixture with valid + invalid signatures; verifier
  marks each accordingly without affecting the overall trust decision.
* TUF integration (if scope extends to this phase): fixture repository with
  metadata, snapshot rollover, key rotation per TUF spec.

---

## IDE Integration

The IDE plug-in lives under [`Source/IDE/`](../Source/IDE/) with ten parallel
`.dproj` files (`DPM.IDE.XE2.dproj` through `DPM.IDE.D130.dproj`). All
IDE-layer additions and modifications must be reflected in every project file;
the new units below go into the shared source set, so the change to each
`.dproj` is the same line repeated ten times.

### IDE-1 — Splash and About (Phase 1)

[`DPM.IDE.Main`](../Source/IDE/DPM.IDE.Main.pas) registers the splash bitmap
and About-box entry today. Extend the About-form string to include:

```text
DPM 0.6.0
Signing trust set: v1 (issued 2026-01-15)
```

The trust-set version is read from `DPM.Trust.Set.GetActiveVersion`.
[`DPM.IDE.AboutForm`](../Source/IDE/DPM.IDE.AboutForm.pas) at line ~79 is the
single change site.

### IDE-2 — Signing status in PackageDetails frame (Phase 1)

[`DPM.IDE.PackageDetailsFrame`](../Source/IDE/EditorView/DPM.IDE.PackageDetailsFrame.pas)
gains a signing-status header above the existing version/dependency list.
States (Phase 1):

* Unverified — package not yet in cache. (Most common pre-install state.)
* Integrity-checked, unsigned — `permissive` mode, no signatures present.
* Signed, untrusted publisher — author signature valid but signer SPKI not
  in `trustedPublishers`.
* Signed by *Name* — both valid and trusted.
* Invalid signature — verification failed; install is blocked in `require`
  mode.

Each state has a distinct icon and one-line text. Clicking the header opens
a small modal showing the full `dpm verify` output (the same text the CLI
emits, rendered in a fixed-width font).

The frame's `DoPackageInstall` at line ~352 already calls
`FPackageInstaller.Install`; verification happens inside that call now, so
no orchestration changes are needed in the frame — only the state display.

### IDE-3 — Messages window updates (Phase 1)

[`DPM.IDE.MessageForm`](../Source/IDE/Logging/DPM.IDE.MessageForm.pas) and
[`DPM.IDE.MessageService`](../Source/IDE/Logging/DPM.IDE.MessageService.pas)
already render `FLogger.Warning/Error/Success`. The verifier emits structured
log lines that follow the existing pattern (e.g.
`Logger.Information('VSoft.CommandLine 1.0.0: signature valid, chains to trusted root')`),
so they appear in the existing window with no new code — only new strings.

### IDE-4 — Trust prompt dialog (Phase 1)

New unit `DPM.IDE.TrustPromptForm`. Triggered by the `authorDowngradePolicy
= prompt` decision point in `DPM.Trust.State`. Modal, shown on the main
thread, displays:

* Package id and version.
* The previously-seen signer (name + SPKI thumbprint) and the new state
  (unsigned, or signed by a different key).
* Three buttons: `Trust this build`, `Block this build`, `Always block this
  package from now on`.

The decision is recorded in trust state. For non-interactive contexts (CI
inside the IDE — rare), the dialog respects an existing
`Tools > Options > DPM > Non-interactive mode` flag and falls back to
`block`.

### IDE-5 — Options page: Trust tab (Phase 1)

New `DPM.IDE.TrustOptionsFrame` registered as a tab on the existing
[`DPM.IDE.AddInOptionsHostForm`](../Source/IDE/DPM.IDE.AddInOptionsHostForm.pas).
Surfaces:

* Validation mode (combo, four options — Phases 2+ enable the latter two).
* `Allow unsigned packages` (checkbox).
* `Author downgrade policy` (combo: prompt/block/allow).
* `Trusted publishers` (grid: name + SPKI, add/remove).
* `Trusted repositories` (grid: URL + SPKI, add/remove).
* `Trust set version` (read-only, with link to "What is this?").

Writes back via the same `IConfigurationManager` the existing tab uses.

### IDE-6 — Cache-verify menu item (Phase 1)

Tools menu gains `Tools > DPM > Verify Package Cache`, registered in
[`DPM.IDE.Wizard`](../Source/IDE/DPM.IDE.Wizard.pas). It invokes the same
`IPackageCache.FullReVerify` API the `dpm cache verify` CLI uses, with a
progress dialog and a results panel.

### IDE-7 — Attestation rendering (Phase 2)

When a repository signature with attestation is present, the
PackageDetails header line "Signed by VSoft Technologies" gains a tooltip:
"Repository `packages.delphi.dev` verified author key against namespace
`VSoft.*`". Pure UI; data comes from the receipt.

### IDE-8 — Lock-file write-back (Phase 2)

`PackageReference` elements gain `ManifestHash` when the IDE installs
or upgrades a package. The existing `DPM.IDE.ProjectController.pas`
(`ProjectOpened`, `ProjectSaved` event handlers) is the natural place; the
change is in [`Source/Core/Project/`](../Source/Core/Project/) and merely
needs to be triggered from the IDE save path.

### IDE-9 — Remote-provider authentication UI (Phase 3)

Sign-from-IDE is not a primary scenario (signing typically happens in CI),
but for completeness the Tools menu gains `Tools > DPM > Sign Package...`
opening a dialog that wraps `dpm sign`. The dialog supports cert-store,
PFX, and Key Vault providers; Key Vault requires AAD authentication via a
device-code flow. This is a low-priority IDE feature and can land late in
Phase 3 or be omitted.

### IDE-10 — Per-Delphi-version impact

All IDE-layer units listed above are added to **every** `.dproj` from XE2
through 13.0. The signing-status icons are bundled into the existing IDE
resource compilation. There are no version-conditional defines required;
the units are pure VCL using APIs available since XE2.

### IDE-11 — IDE test plan (manual smoke per phase)

Per [`CLAUDE.md`](../CLAUDE.md), UI cannot be reliably automated. Each phase
has a per-Delphi-version smoke checklist:

* Install a signed package — status badge shows "Signed by ...".
* Install an unsigned package in `permissive` mode — status badge shows
  "Integrity-checked, unsigned"; package installs.
* Install an unsigned package in `require` mode — install is blocked; the
  Messages window shows the reason.
* Author-downgrade prompt — uninstall the previously seen signed build, then
  install an unsigned build of the same id; verify the prompt dialog
  appears and the decision is recorded.
* Tools > DPM > Verify Package Cache — runs to completion, reports per-
  package status.
* About box shows the trust-set version.

This is repeated for every Delphi version we ship the IDE plug-in for. The
core verifier behaviour is exercised by the headless integration tests; the
per-version smoke is for VCL surface only.

---

## Cross-cutting: compatibility and migration

### Existing unsigned packages

Existing unsigned packages will be repacked - we are still in development so backwards compatibility is not a concern.

### Existing `.sha256` sidecars

The current
[`TPackageCache.GetPackageHash`](../Source/Core/Cache/DPM.Core.Cache.pas)
caches a SHA-256 of the package file in a `.sha256` sidecar. After Phase 1
the manifest hash recorded in the receipt makes this redundant. To avoid
churn we keep both for one release; the sidecar is removed in Phase 2.

### Configuration migration

The `signing:` block is added with defaults if absent — no existing config
fails to load. Adding a `validationMode` other than `permissive` is the
user's choice; the install never silently tightens for an existing user.

---

## Cross-cutting: open questions

These are recorded so they aren't lost in the implementation phase. None
blocks Phase 1 design, but each will need an answer before the Phase 1
release.

1. **IANA PEN.** The architecture doc earmarks `1.3.6.1.4.1.<PEN>.1.x` for
   DPM's signed-attribute arc; the application is "applied, awaiting
   approval." Until then the implementation uses placeholder UUID-form OIDs
   under `2.25.<uuid>` (per the doc) — fine for development; must swap for
   real PEN before release. Constant lives in `DPM.Crypto.Cms` as
   `cDpmOidArc`.
2. **Trust set delivery channel.** Phase 1 ships the trust set in the DPM
   binary. Phase 2 introduces signed rollover metadata; the *transport* for
   that metadata is unspecified — gallery endpoint, separate download, or
   delivered via the existing DPM update channel. Worth deciding before
   Phase 2 design starts.
3. **CI prompt behaviour.** `authorDowngradePolicy: prompt` is the
   sensible default for interactive use but blocks unattended CI. The
   current plan falls back to `block` when stdin is not a tty for the CLI,
   and to `block` when an IDE non-interactive flag is set. Confirm this is
   the intended behaviour vs. requiring users to pick `block` explicitly.
4. **`PackageReference.ManifestHash` storage.** XML attribute on the
   existing element is the simplest; an alternative is a sibling
   `<ManifestHash>` element. Attribute is preferred — terser and easier to
   diff — but if attribute order is unstable in the .dproj writer we use
   today, a child element is the fallback.
5. **Receipt file format.** YAML
6. **NFC dependency.** The plan uses `NormalizeString` from `normaliz.dll`.
   Available on every supported Windows version (Vista+), but if a target
   environment ever restricts it, a fallback ICU dependency would be
   required. Worth recording as a constraint.
7. **Repository signing on directory feeds.** The architecture doc states
   `repository-required` doesn't apply to directory-based repositories;
   the implementation needs an unambiguous source-type flag on each
   configured source. Verify that
   [`IPackageSource`](../Source/Core/Sources/) already carries this.

---

## Critical files and integration points (summary)

A scannable index for the implementer. **Bold** rows are new units; the rest
are extensions to existing units.

| Touch point | File | Phase | Conformance |
| ----------- | ---- | ----- | ----------- |
| Cache ingest | [`DPM.Core.Cache.pas`](../Source/Core/Cache/DPM.Core.Cache.pas) `TPackageCache.InstallPackageFromFile` (~line 529) | P1 | V-1, V-3, V-15, V-30, V-31 |
| Cache hit fast path | [`DPM.Core.Cache.pas`](../Source/Core/Cache/DPM.Core.Cache.pas) `TPackageCache.EnsurePackage` (~line 471) | P1 | V-33 |
| Pack command | [`DPM.Console.Command.Pack`](../Source/Cmdline/Commands/) | P1 | M-1…M-12, V-9…V-13 (producer side) |
| Install/restore orchestration | [`DPM.Core.Package.Installer.pas`](../Source/Core/Package/DPM.Core.Package.Installer.pas) | P1 | wiring only |
| Configuration | [`DPM.Core.Configuration.Classes.pas`](../Source/Core/Configuration/DPM.Core.Configuration.Classes.pas) | P1 (signing block); P2 (trust pins) | V-22, V-23 |
| Sources / source type flag | [`Source/Core/Sources/`](../Source/Core/Sources/) | P2 | V-22 directory-source carve-out |
| Project lock | [`Source/Core/Project/`](../Source/Core/Project/) `PackageReference` writer/reader | P2 | architecture §Project-level integrity |
| IDE Options host | [`DPM.IDE.AddInOptionsHostForm.pas`](../Source/IDE/DPM.IDE.AddInOptionsHostForm.pas) | P1 | IDE-5 |
| IDE PackageDetails | [`DPM.IDE.PackageDetailsFrame.pas`](../Source/IDE/EditorView/DPM.IDE.PackageDetailsFrame.pas) | P1 / P2 | IDE-2 / IDE-7 |
| IDE About | [`DPM.IDE.AboutForm.pas`](../Source/IDE/DPM.IDE.AboutForm.pas) | P1 | IDE-1 |
| IDE Wizard menu | [`DPM.IDE.Wizard.pas`](../Source/IDE/DPM.IDE.Wizard.pas) | P1 | IDE-6 |
| **Crypto Win32 imports** | `DPM.Crypto.Win32.pas` | P1 | – |
| **Hashing** | `DPM.Crypto.Hashing.pas` | P1 | V-16 |
| **X509 / chain** | `DPM.Crypto.X509.pas` | P1; revocation P3 | V-15, V-26 |
| **CMS** | `DPM.Crypto.Cms.pas` | P1 | S-1…S-3, V-15 |
| **Timestamping** | `DPM.Crypto.Timestamping.pas` | P1 | S-4, V-17 |
| **Signing provider** | `DPM.Crypto.Provider.pas` | P1 store+PFX, P3 remote | S-8 |
| **Algorithm profile** | `DPM.Crypto.Algorithms.pas` | P1 | V-16, S-5 |
| **Manifest** | `DPM.Package.Manifest.pas` | P1 | M-1…M-12, V-2 |
| **Archive rules** | `DPM.Package.Archive.pas` | P1 | V-9…V-13 |
| **Package signing orchestration** | `DPM.Package.Signing.pas` | P1; attestation P2 | S-1…S-7, V-1…V-21 |
| **Trust set** | `DPM.Trust.Set.pas` | P1; signed rollover P3 | architecture §Trust Bootstrapping |
| **Trust policy** | `DPM.Trust.Policy.pas` | P1; full modes P2 | V-19, V-22 |
| **Trust state (TOFU)** | `DPM.Trust.State.pas` | P1 author; P2 repo | V-24, V-25 |
| **Receipt** | `DPM.Package.Cache.Receipt.pas` | P1 | V-31, V-32, V-33 |
| **`dpm sign`** | `DPM.Console.Command.Sign.pas` | P1; remote P3 | S-1…S-8 |
| **`dpm verify`** | `DPM.Console.Command.Verify.pas` | P1; attestation P2 | V-27 |
| **`dpm trust`** | `DPM.Console.Command.Trust.pas` | P1 read / P2 write | – |
| **`dpm cache verify`** | extend `DPM.Console.Command.Cache.pas` | P1 | V-34 |
| **IDE Signing status frame** | `DPM.IDE.SigningStatusFrame.pas` | P1 | IDE-2 |
| **IDE Trust options frame** | `DPM.IDE.TrustOptionsFrame.pas` | P1 | IDE-5 |
| **IDE Trust prompt** | `DPM.IDE.TrustPromptForm.pas` | P1 | IDE-4 |

---

## End-to-end verification (manual acceptance test for Phase 1 release)

Before declaring Phase 1 done, the following round-trip must pass on a clean
Windows machine with no DPM state:

1. **Generate a self-signed test cert** in `Cert:\CurrentUser\My`.
2. `dpm pack VSoft.CommandLine.dspec --output VSoft.CommandLine-1.0.0.dpkg` —
   verify the archive contains `dpm-manifest.json` and no `signatures/`
   directory.
3. `dpm sign VSoft.CommandLine-1.0.0.dpkg --thumbprint <test cert>` — verify
   archive now contains `signatures/author-1.p7s` and a valid RFC3161
   timestamp inside it (use `signtool verify /v /pa` cross-check).
4. `dpm verify VSoft.CommandLine-1.0.0.dpkg` — output identifies signer,
   timestamp, and reports `valid, chains to trusted root` (after adding the
   test root to the user's trust store).
5. Modify one byte in a content file → repack (without re-signing) →
   `dpm verify` reports the failed file hash and refuses.
6. Add a stray file directly to the archive → `dpm verify` reports the
   unlisted file and refuses (V-3).
7. `dpm install VSoft.CommandLine` from a local source in `permissive` mode
   — succeeds; receipt file is written; second install completes in <50ms
   from cache.
8. Switch to `require` mode; remove the signature file from the cached
   package; `dpm install` is rejected.
9. In the IDE: open a project, install the same signed package, verify the
   PackageDetails frame shows "Signed by ..." and the About box shows the
   trust-set version.
10. Trigger the no-downgrade prompt by uninstalling and then attempting to
    install an unsigned re-pack of the same id — modal appears with three
    correct options; chosen action recorded in `trust-state.json`.

Equivalent checklists for Phase 2, 3, and 4 are built incrementally on top of
this baseline and are detailed in the per-phase test-plan subsections above.

---

## Summary

Phase 1 lands a manifest format, an author signature with timestamp, an
integrity-enforcing verifier wired into the existing cache ingest path, a
cache receipt that makes cache hits fast, and a TOFU author no-downgrade
rule — enough to make signing meaningful for the first set of producers
without requiring server-side repository infrastructure. Phase 2 adds the
repository signature, attestation, and full trust policy that turn the
verifier from TOFU-anchored to properly-anchored. Phase 3 adds enterprise
features (revocation, remote signing providers, key rotation). Phase 4 is
optional supply-chain hardening on top of an already-complete signing
system.

Every change slots into a clearly named existing integration point —
[`TPackageCache.InstallPackageFromFile`](../Source/Core/Cache/DPM.Core.Cache.pas)
is the central one — and every new unit is owned by either Core or the IDE
plug-in with no cross-cutting refactors. The unit boundaries set in Phase 1
(`ISigningProvider`, the `DPM.Crypto.*` layer, `DPM.Trust.*`) are sized to
accommodate the later phases without restructuring.
