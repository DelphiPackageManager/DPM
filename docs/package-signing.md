# DPM Package Signing Architecture

## Overview

This document specifies a package signing architecture for the DPM (Delphi
Package Manager) ecosystem. It supersedes the earlier draft and incorporates
fixes for several correctness and security gaps identified in review.

Design goals:

* Use industry-standard cryptographic formats and trust models.
* Implement no custom cryptography and no custom ASN.1 parsing.
* Build entirely on native Windows cryptographic infrastructure.
* Support enterprise and CI/CD workflows, including smart cards and HSMs.
* Provide long-term signature validity through timestamping.
* Support author signing (optional) and repository signing.
* Keep the implementation substantially simpler than NuGet signing without
  sacrificing the integrity guarantees that make signing meaningful.
* Avoid dependencies on commercial or cross-platform cryptography libraries.

The design draws on NuGet package signing, Authenticode, CMS/PKCS#7, and Linux
repository signing models. Where it deliberately diverges from NuGet, the
divergence and its consequences are stated explicitly.

---

## Goals and Non-Goals

### Functional goals

* Package integrity verification (contents cannot be altered undetected).
* Package author identity verification.
* Repository trust verification.
* Offline signature verification.
* RFC3161 timestamp validation for long-term validity.
* Enterprise certificate infrastructure, smart cards, and HSM-backed keys.
* Deterministic, reproducible verification.
* Support for multiple signatures per package (e.g. author plus repository).

### Non-goals for the initial implementation

The following are explicitly out of scope and may be revisited later:

* Transparency logs.
* TUF-style repository metadata.
* SBOM and vulnerability attestations.
* Reproducible build verification.
* Cross-platform cryptography abstraction (DPM is Windows-only).
* Package encryption.

Note that custom ASN.1 parsing and custom CMS implementations are *not* listed
as future work — they are never required, because Windows provides the
necessary primitives, including for RFC3161. The earlier draft listed a "native
RFC3161 implementation" as a later phase; that has been removed because it
would have contradicted this non-goal.

---

## Core Design Principles

### 1. Use Windows native cryptography

DPM is Windows-only and relies entirely on the platform crypto stack:

* CryptoAPI message functions (`Crypt32.dll`) for CMS/PKCS#7.
* CNG / BCrypt for hashing.
* The Windows certificate stores and chain engine.
* `CryptRetrieveTimeStamp` / timestamp verification for RFC3161.

This removes any need for OpenSSL, commercial libraries, or hand-written ASN.1.

### 2. Use detached CMS signatures

Signatures are detached CMS/PKCS#7 `SignedData` structures, DER-encoded. The
signed content (the manifest, see below) is *not* embedded in the signature
blob; it travels alongside it inside the package. This gives standard tooling
compatibility, embedded certificate chains, and RFC3161 timestamp support.

### 3. Sign a canonical manifest, not the archive

DPM does **not** sign raw ZIP bytes, archive metadata, or central-directory
structures. Instead it:

1. Generates a canonical manifest that enumerates every content file with its
   size and hash.
2. Signs the exact byte content of that manifest.

This avoids ZIP instability — compression differences, timestamp variance, and
entry-ordering differences across machines and tools.

**Critical consequence — the manifest must commit to the complete file set.**
Because the signature covers only the manifest, signing the manifest is
worthless unless verification also proves that the package contains *exactly*
the files the manifest describes — no missing files and, equally important, no
extra files. See *Verification Workflow*, step 3. This is the single most
important rule in this document; NuGet signs the whole package specifically to
close this hole, and DPM's simpler approach is only safe if verification
enforces set-equality.

### 4. Separate cryptography from package logic

The cryptography layer knows nothing about packages, feeds, repositories, or
dependency resolution. It operates only on byte arrays, certificates, and CMS
blobs. The actual signing primitive is reached through a provider interface
(`ISigningProvider`), so that local certificate-store signing, PFX signing, and
remote signing (e.g. Azure Key Vault) are interchangeable implementations
rather than special cases threaded through the package code.

---

## Package Structure

### Unsigned package

```text
Foo.1.0.0.dpkg
+- dpm-manifest.json
+- package.dspec.yaml
+- lib/
+- source/
+- tools/
```

### Signed package

```text
Foo.1.0.0.dpkg
+- dpm-manifest.json
+- package.dspec.yaml
+- lib/
+- source/
+- tools/
+- signatures/
   +- author-1.p7s
   +- repository-1.p7s   (added when the package is repository-signed)
```

Each signature is a separate `.p7s` file. **File names are non-semantic**: the
verifier enumerates `signatures/*.p7s` and never parses the names. A signature's
role (`author` or `repository`) and signer identity come entirely from the CMS
signed attributes and the embedded certificate (see *Signature Roles*). Any
collision-free naming scheme is therefore acceptable; DPM uses readable
sequential names such as `author-1.p7s` and `repository-1.p7s`. In Phase 1 there
will normally be exactly one file in `signatures/` (the author's); the directory
layout is defined this way from the outset so repository signing in Phase 2
requires no structural change.

`dpm-manifest.json` and everything under `signatures/` are the only mutable
surface of a published package; see *Immutability*.

### Archive format rules

A `.dpkg` is a ZIP archive. Signing the manifest rather than the archive avoids
ZIP *instability*, but extraction and verification still parse the ZIP, and ZIP
parsers disagree with one another in ways that enable differential attacks. DPM
therefore enforces a strict archive profile and rejects any package that
violates it:

* **Duplicate entry names are invalid.** Two entries resolving to the same path
  make the package ambiguous: parsers disagree on which entry wins, so the file
  the manifest check sees can differ from the file extraction produces. Paths
  are compared *case-insensitively* — matching Windows case folding — over their
  NFC form; this catches `Foo.pas` versus `foo.pas`, as well as the trailing-dot
  and trailing-space variants rejected under *Path safety*. Any duplicate fails
  verification outright. Case folding is deliberately kept separate from
  normalization folding: NTFS treats different Unicode normalizations as
  distinct files, so DPM mandates one normalization form (NFC) rather than
  folding forms together at comparison time.
* **Entry names are UTF-8.** The ZIP language-encoding flag must indicate
  UTF-8; names are decoded as UTF-8 and must match their NFC manifest path
  exactly.
* **Only regular files and directories are permitted.** Symbolic links, device
  entries, and other special entry types are rejected.
* **Alternate Data Streams and reserved names are rejected.** Paths containing
  `:` (NTFS ADS) or matching Windows reserved device names (`CON`, `PRN`,
  `AUX`, `NUL`, `COM1`–`COM9`, `LPT1`–`LPT9`) are rejected.
* **Compression method allowlist.** Only Store and Deflate are accepted;
  encrypted entries and exotic compression methods are rejected.

These rules complement the per-path checks in *Manifest Design → Path safety*;
together they close the zip-slip and parser-differential classes of attack.

---

## Manifest Design

The manifest is the canonical description of the package contents and is the
only object that is signed.

### Requirements

The manifest must be deterministic, UTF-8 encoded, and byte-stable across
machines and across time for a given set of inputs.

### Canonical serialization

Canonicalization rules apply **only to manifest generation**. They exist so
that producing a manifest is reproducible. They are *not* applied during
verification — see the note below.

* UTF-8, no BOM.
* LF line endings only.
* Object properties sorted lexicographically by key.
* No insignificant whitespace.
* `files` array sorted by `path`, byte-wise on the canonical path string.
* Paths use forward-slash separators, NFC-normalised, relative to the package
  root.
* Numeric fields are integers only — no floating-point values anywhere in the
  schema.

These rules are close to RFC 8785 (JSON Canonicalization Scheme), but DPM does
**not** depend on a general JSON library to produce the manifest. Delphi's
`System.JSON`, especially across the range of supported compiler versions, does
not guarantee lexicographic key ordering or deterministic serialization.
Manifest generation instead uses a small purpose-built deterministic emitter
for the fixed schema, writing the canonical bytes directly. Because the schema
is fixed and contains no floating-point numbers, this emitter is trivial and
its output is byte-identical on every supported Delphi version — the hardest
part of JCS, canonical number formatting, is avoided entirely by the
integer-only rule. Verification still parses the manifest, with a hardened
parser (see *Verification Workflow*), but never re-emits it.

**Verification never re-serializes or regenerates the manifest.** The signed
content is the exact byte sequence of `dpm-manifest.json` as stored in the
package. The verifier reads those bytes verbatim and verifies the CMS signature
over them. It does not parse-and-re-emit the manifest, and in particular it
does not regenerate a manifest from the package contents and byte-compare it
against the signed one. Regenerate-and-compare looks equivalent but is not: it
cannot reproduce non-deterministic fields such as `created`, it breaks
`manifestSchemaVersion` forward compatibility (an older verifier cannot
regenerate fields it does not understand), and it re-couples verification to
byte-identical re-serialization across every future version of DPM — reintroducing the exact
"signed on machine A, fails on machine B" fragility that signing raw bytes was
chosen to eliminate. The package is instead checked against the *parsed*
manifest, entry by entry; see *Verification Workflow*, step 3.

### Path safety

Every `path` in the manifest must be validated before extraction. Reject:

* absolute paths and drive letters,
* `..` segments or any path that escapes the package root,
* backslashes or other non-canonical separators,
* any path component with a trailing dot or with leading or trailing
  whitespace — Windows silently strips these on extraction, so `Foo.pas` and
  `Foo.pas ` would collide on disk while appearing distinct in the manifest,
* control and other non-printable characters,
* any path not in Unicode Normalization Form C (see below),
* duplicate paths.

All manifest paths must be in **Unicode Normalization Form C (NFC)**. The
packaging tool normalises to NFC at generation time — rejecting, with a clear
error, any input it cannot — and the verifier *requires* NFC and rejects any
path that is not. Mandating a single form, rather than normalising on the fly
during comparison, keeps path comparison well-defined and removes the risk of
two visually identical paths in different normalization forms being treated as
distinct files. NFC is chosen because it is the standard interchange form and
the form source files on disk almost always already use.

This is a security control (zip-slip and path-traversal hardening), independent
of canonicalization. As defence-in-depth, extraction may additionally resolve
each destination with the Windows path APIs and confirm it still lies within
the target directory — the containment check being made on a path-segment
boundary, so that `base` does not match `base-evil`.

### Excluded files

Two things are present in a signed package but never listed in `files`:

* `dpm-manifest.json` itself (it cannot list its own hash).
* Everything under `signatures/` (signatures are added after, and to, the
  manifest).

Although `signatures/` is excluded from the manifest, it is not unmanaged
space: verification requires every file under it to be a well-formed CMS
signature (see *Verification Workflow*), so no arbitrary data can be hidden
there.

### Example manifest

```json
{
  "dpmPackageFormat": 1,
  "manifestSchemaVersion": 1,
  "packageId": "VSoft.CommandLine",
  "version": "1.0.0",
  "created": "2026-05-19T10:00:00Z",
  "hashAlgorithm": "SHA256",
  "files": [
    {
      "path": "package.dspec.yaml",
      "size": 512,
      "hash": "D4E5F6..."
    },
    {
      "path": "source/CommandLine.pas",
      "size": 18432,
      "hash": "A1B2C3..."
    }
  ],
  "extensions": {}
}
```

`packageId` and `version` are inside the signed manifest, which binds a
signature to a specific package identity and prevents a signature being lifted
onto a differently named package. `created` is informational only; the
authoritative signing time comes from the CMS signing-time attribute and the
RFC3161 timestamp.

Two version fields are kept deliberately separate. `dpmPackageFormat` is the
version of the on-disk package *layout* — the directory structure, the
`signatures/` convention, the archive rules. `manifestSchemaVersion` is the
version of the manifest *schema* itself. The two evolve independently: the
layout can change without touching the manifest schema, and vice versa.

`extensions` is a reserved object for additive, forward-compatible metadata.
Because it is inside the signed manifest it is covered by the signature and
cannot be tampered with, and verifiers ignore keys they do not recognise. It is
for *non-security* metadata only: anything a verifier must understand to make a
correct trust decision requires a `manifestSchemaVersion` bump instead, so that
an older client refuses or warns rather than silently ignoring a field that
matters.

---

## Signature Roles

A signature's role is recorded as a CMS **signed attribute** with a
DPM-assigned OID, with value `author` or `repository`. This is the same
technique NuGet uses for commitment type. It is an attribute, not custom
cryptography or custom ASN.1, and is read with the standard `CryptMsg*` APIs.

### Author signature

Created by the package publisher. Establishes package origin, publisher
identity, and integrity.

### Repository signature

Created by the package feed when it accepts a package. Establishes that the
package was admitted by a trusted repository, that the repository verified the
author against a registered key (see *Publisher Registration and Repository
Verification*), and supports enterprise trust policies. A repository signature
carries an additional signed attribute recording the publisher namespace and
the author certificate it verified, so that attestation is portable and
offline-verifiable.

**Design choice — independent signatures, not countersignatures.** When both
an author and a repository signature are present, NuGet nests the repository
signature as a countersignature on the author signature, binding the two. DPM
deliberately keeps them as two independent detached signatures over the same
manifest bytes. This is simpler, and for DPM's threat model it is sufficient:
content authenticity is pinned by the manifest signature and author identity by
the repository's SPKI attribute, so substituting one valid author signature for
another by the same registered key over the same manifest delivers identical
content from the identical author — it is not an attack. The one thing
independent signatures do not provide is a cryptographic record of *which exact*
author CMS blob the repository saw; the optional `dpmVerifiedAuthorSignatureHash`
attribute (see *CMS / PKCS#7 Format*) closes that gap for audit and
transparency-log purposes without resorting to CMS nesting.

---

## CMS / PKCS#7 Format

All signatures are DER-encoded CMS `SignedData`, detached.

Each signature embeds:

* the signing certificate and its intermediate certificates,
* the signing-time signed attribute,
* the message digest of the manifest,
* the signature and digest algorithm identifiers,
* the DPM signature-role signed attribute (`author` or `repository`),
* for repository signatures, a signed attribute recording the publisher
  namespace identifier and the author certificate verified at publish time
  (its SPKI hash), or — when the package is unsigned — a marker that
  distinguishes a package that was never author-signed from one whose author
  has deliberately ceased signing,
* an RFC3161 timestamp token, carried as an unsigned attribute on the signer
  info (added after the signature is produced).

**Optional — author-signature binding.** A repository signature may also carry
a `dpmVerifiedAuthorSignatureHash` signed attribute holding the SHA-256 of the
exact DER bytes of the author CMS blob it verified. This is a *provenance and
audit* feature, not a security control. It is not required to prevent any
attack: content authenticity is already pinned by the manifest signature and
author identity by the SPKI attribute, so substituting one valid author
signature by the registered key over the same manifest delivers identical
content from the identical author. Its value is precision for audit trails and
future transparency-log entries — the repository signature then names the exact
author blob it saw. It carries one cost: it freezes the author signature blob,
so that signature can no longer be independently re-timestamped for long-term
validity without re-issuing the repository attestation. DPM treats this
attribute as optional and off by default.

### Object identifiers

The DPM-specific CMS signed attributes need real, registered OIDs; an
implementation cannot ship with placeholders. DPM should obtain an IANA Private
Enterprise Number (PEN) — these are free — and define its arc beneath it, of
the form `1.3.6.1.4.1.<PEN>.1.x`. The registration-free UUID OID arc,
`2.25.<uuid-as-integer>`, is a valid fallback but produces unwieldy identifiers
and is less conventional.

| Attribute                        | Carries                                     | OID (DPM arc) |
| --------------------------------- | ------------------------------------------- | ------------- |
| `dpmSignatureRole`                | `author` or `repository`                    | `…1.1`        |
| `dpmRepositoryAttestation`        | publisher namespace + verified author SPKI  | `…1.2`        |
| `dpmVerifiedAuthorSignatureHash`  | optional author-blob hash (provenance)      | `…1.3`        |

The concrete arc is fixed once the PEN is assigned; until then the table uses
placeholders, and obtaining the PEN is tracked as a prerequisite for the
Phase 1 release. During development it is acceptable to use UUID OID's while we wait for a PEN (applied, awaiting approval);

---

## Cryptographic Algorithms

Algorithm *agility* is supported — the manifest names its hash algorithm
explicitly, and the verifier can be extended to accept new algorithms — but
agility must not mean accepting anything. To prevent implementation drift and
silent downgrades, DPM defines a mandatory algorithm profile. A Phase 1
implementation MUST support exactly this profile, and the verifier MUST reject
anything weaker even when a signed manifest or signature specifies it.

| Purpose              | Phase 1 requirement                          |
| -------------------- | -------------------------------------------- |
| Manifest file hashes | SHA-256 (SHA-384 / SHA-512 also accepted)    |
| CMS digest           | SHA-256 (SHA-384 / SHA-512 also accepted)    |
| Signature algorithm  | RSA PKCS#1 v1.5, or ECDSA P-256 / P-384      |
| Minimum RSA key size | 2048 bits                                    |
| Timestamp digest     | SHA-256                                      |

SHA-1 and MD5 are rejected everywhere — for file hashes, CMS digests, and
timestamps — regardless of what a manifest or signature declares. Because the
`hashAlgorithm` field is inside the signed manifest, an attacker cannot
downgrade it without breaking the signature; the verifier's allowlist exists to
refuse a *signer* who legitimately chose a now-unacceptable algorithm. New
algorithms are added by extending the allowlist in a future phase, never by
weakening it.

---

## Trust Model

Verification is a sequence of independent checks. A package is trusted only if
all checks required by the active validation mode pass.

### Cryptographic validity

* CMS structure is well-formed.
* The signature verifies against the manifest bytes.
* The manifest's recorded file hashes match the package contents, and the file
  set matches exactly (see *Verification Workflow*).

### Certificate trust

* The certificate chain builds to a trusted root.
* The chain satisfies code-signing usage requirements.
* The certificate was valid at the effective signing time (see timestamping).

### Timestamp validity

* The RFC3161 timestamp token verifies.
* The timestamping authority chains to a trusted root.
* The signing time falls within the signing certificate's validity period.

### Repository trust policy

* The repository certificate is trusted and explicitly allowed.

### Identifying trusted parties

Trusted publishers and repositories are pinned by the **SHA-256 hash of the
certificate's Subject Public Key Info (SPKI)**, never by Common Name. A CN
string is neither unique nor difficult to obtain, so CN-based matching is not an
acceptable trust anchor. SPKI is the primary pinning mechanism throughout DPM —
it is what publisher registration records — and it survives routine certificate
renewal as long as the key pair is unchanged, so a renewal does not silently
break trust. Certificate thumbprints still appear in output and logs for human
identification and debugging, but they are not the pinning mechanism.

---

## Trust Policies

### Configuration example

```yaml
---
signatureValidationMode: require
allowUnsigned: false
authorDowngradePolicy: prompt
trustedPublishers:
- name: VSoft Technologies
  spki: sha256:AB12CD34...
trustedRepositories:
- url: https://packages.delphi.dev
  spki: sha256:EF56AB78...
```

`spki` is the SHA-256 hash of the certificate's Subject Public Key Info — see
*Trust Model → Identifying trusted parties*. `authorDowngradePolicy` governs how
the client reacts when a previously author-signed package is now unsigned — see
*The no-downgrade rule*.

### Validation modes

* **permissive** — unsigned packages are allowed; invalid signature fails installation.
* **require** — packages must carry a valid author signature; an invalid or
  missing signature fails installation.
* **repository-required** — a valid repository signature is mandatory.
* **author-and-repository** — both a valid author and a valid repository
  signature are mandatory.

Phase 1 implements `permissive` and `require`. The repository modes arrive with
repository signing in Phase 2.

The *default* mode tightens over time, tied to an ecosystem fact rather than to
a version number. While the gallery still contains unsigned packages the
default is `permissive`. Once the gallery guarantees that every package it
serves carries a repository signature — which Phase 2 delivers — the default
becomes `repository-required`: a valid repository signature is mandatory, while
an author signature stays optional (see *Author Signing and Certificate
Requirements*). The default is `repository-required` rather than `require`
precisely because author signing is optional — `require` would reject every
package whose author chose not to sign. `allowUnsigned` then survives only for
local and development folder sources, which are explicitly an insecure
convenience.

**Note** :  for repository-required and author-and-repository - this only applies to http based repositories - directory based repositories have no ability to ad repository signatures.

### The no-downgrade rule

Validation modes set a floor for *unknown* packages. They are not the whole
story, because a global `permissive` default would otherwise allow a serious
downgrade attack: an attacker able to substitute a package — a malicious
mirror, a compromised intermediate — could serve an *unsigned* build of a
popular package, and a `permissive` client would install it with only a
warning.

DPM closes this with a sticky, no-downgrade rule. The client maintains local
trust state recording, per package id and per namespace prefix, the highest
assurance level ever observed and the publisher identity seen with it. The rule
ratchets along two dimensions:

* **Repository assurance.** Once a package id has been seen carrying a valid
  repository signature from a **trusted** repository attesting namespace *N*, a
  later build that no longer carries a valid signature from a trusted
  repository, or attests a different namespace, is a **hard failure** regardless
  of the global mode. The ratchet is keyed to trusted repositories specifically
  — a signature from some other, untrusted repository can never satisfy it, or
  an attacker running their own instance could. Because a trusted repository
  repository-signs every package it serves, this dimension never legitimately
  downgrades — losing it means the package is no longer coming from a repository
  the client trusts.
* **Author assurance.** A package that was previously author-signed and is now
  unsigned is handled primarily at the server, at publish time: the gallery
  detects the downgrade and will not accept it until the author has
  acknowledged it on their account (see *Publisher Registration → Author
  signing downgrades*). On the client, the downgrade is governed by a
  configurable setting, `authorDowngradePolicy`, which defaults to `prompt` —
  the first time a client encounters a build whose author-signing status has
  weakened, it prompts the user and remembers the decision. The setting can be
  raised to `block` (treat the downgrade as a hard failure, for strict
  environments) or lowered to `allow` (accept silently). The default ensures a
  previously-signed dependency never *quietly* becomes unsigned: the consumer
  is told once, and decides.

In Phase 1, before repository signing exists, only the author-assurance
dimension applies — trust-on-first-use on the author SPKI. In Phase 2 the
repository-assurance dimension is added and becomes the strong anchor;
legitimate author key rotation is then handled cleanly, because the repository
attestation vouches for the newly registered key, so the client trusts the
*namespace* rather than a frozen key.

The effect is that the global default can stay `permissive` through the
ecosystem transition without exposing already-signed packages to silent
downgrade: a package's effective policy only ever ratchets upward.

---

## Author Signing and Certificate Requirements

### Author signing is optional

Author signing is **optional**, by deliberate design. Requiring every author to
sign would exclude a large part of the ecosystem: open-source maintainers
frequently cannot justify the cost of a publicly trusted code-signing
certificate, and since mid-2023 such certificates additionally require a
hardware token or HSM, raising both the price and the friction. A package
manager that made signing mandatory would simply have far fewer packages.

DPM therefore treats the **repository signature as the primary trust
mechanism** and the **author signature as an additional, optional proof**:

* A package may be uploaded to the gallery unsigned by its author. The gallery
  still repository-signs it, and the repository signature's attestation records
  that the package was uploaded unsigned (see *Publisher Registration and
  Repository Verification*).
* A consumer verifying such a package still gets integrity, and the gallery's
  vouching for namespace ownership, from the repository signature alone. What
  they do not get is an independent author-identity proof — but they are not
  blocked.
* An author who *does* sign gives consumers a second, independent proof and
  enables the stricter `author-and-repository` mode for those who want it.

This mirrors the Linux repository model and NuGet.org, where the repository
signs every package whether or not the author did.

### Certificates for the public gallery

When an author *chooses* to sign a package destined for the **public gallery**,
the signing certificate must be a **publicly trusted code-signing
certificate** — one that chains to a public certificate-authority root, carries
the code-signing extended key usage, and, per the CA/Browser Forum requirement
in force since June 2023, has a hardware-protected, non-exportable private key.
Self-issued and private-CA certificates are not accepted for author signatures
on the public gallery, the same position NuGet.org takes.

This is not in tension with optional signing: an author who cannot obtain such
a certificate simply does not author-sign, and the package is still accepted
and repository-signed. The requirement only governs what counts as a *valid*
author signature on the public gallery.

A publicly trusted certificate is not, by itself, an authority claim. Public
CAs issue code-signing certificates to many thousands of organisations, so
chaining to a public root establishes identity vetting and revocation
infrastructure — not "this package is from VSoft." Authorship and namespace
authority continue to come from the registered-key and namespace-attestation
model, not from the mere fact of public trust.

### Certificates for enterprise-internal use

Packages written for use **inside a single organisation**, never published to
the public gallery, are a different case. Here an author may sign with a
certificate issued by the organisation's **internal CA** — such as Active
Directory Certificate Services. The organisation configures its DPM clients to
trust its internal CA root through the enterprise-supplied trust set (see
*Trust Bootstrapping and Key Rotation*), and those internal packages live on
enterprise-internal feeds, not on the public gallery.

The trust-root set is therefore **contextual**. A client consuming from the
public gallery trusts the public code-signing roots for author signatures, plus
the gallery's repository key. A client inside an enterprise additionally — or
instead — trusts that organisation's internal CA roots and its internal feeds.
The public-gallery rule and the enterprise-internal rule do not conflict,
because they apply to different feeds.

### Local and development feeds

A local folder feed used during development accepts whatever the developer
produces, including unsigned or self-signed packages, under `permissive` mode.
This is explicitly an insecure convenience for the inner development loop, and
is never how the public gallery or a shared enterprise feed is configured.

---

## Publisher Registration and Repository Verification

Repository signing is only meaningful if the repository establishes *who*
published a package before vouching for it. DPM does this through publisher
accounts: an account owns a package-id namespace, and — when the author chooses
to sign — registers the signing keys it will use. Authorship rests on account
ownership of the namespace; an author signature, when present, is an additional
cryptographic proof on top of it.

### Key registration

A publisher that signs its packages registers one or more signing certificates
with its server account; a publisher that does not sign registers none. An
account that does register keys holds a *set* of them, not a single key,
because publishers legitimately need several: overlapping keys during rotation,
separate CI and release-signing keys, and distinct keys per maintainer of a
shared package. Keys can be added and removed without locking the account out.

Registration pins the **SHA-256 hash of the certificate's Subject Public Key
Info (SPKI)** rather than the leaf-certificate thumbprint. The SPKI hash is
stable across certificate renewal as long as the publisher keeps the same key
pair, so a routine renewal does not force a re-registration; rotating to a new
key pair is an explicit, deliberate account action.

### Publish-time verification

The steps below describe the **official gallery's** publish policy. The DPM
server is open source, so other instances might exist (see *Multiple Repositories and
Re-Ingest*); each instance configures its own publish policy and need not apply
every step — a private instance, for example, typically has no record of an
upstream package's original author.

When a publisher uploads a package, the official gallery — before adding any
repository signature:

1. Runs the **full verification workflow** against the package: the same
   file-set equality and integrity check defined below, not a lighter one. The
   server never repository-signs a package it has not itself completely
   verified.
2. Confirms package identity: `manifest.packageId` and `manifest.version` MUST
   match the identity declared for the upload and the namespace of the URL it
   was published to. A mismatch is rejected. This prevents a package from
   claiming an identity it does not own — a confused-deputy risk if left
   unchecked.
3. If the package is author-signed, verifies the author signature and confirms
   that its signing key is one of the keys registered to the account that owns
   the package's namespace. If the package is unsigned, this step is skipped —
   author signing is optional (see *Author Signing and Certificate
   Requirements*).
4. Checks signing history. If the account has previously published this package
   id author-signed and this upload is unsigned, the server raises a downgrade
   flag and rejects the upload until the author acknowledges the change on
   their account (see below).
5. Only then signs the manifest with the repository key, recording the
   publisher namespace and — when the package was author-signed — the verified
   author certificate in the repository signature.

The public gallery accepts unsigned uploads as a matter of design, recording
the unsigned marker in the repository signature. A private or enterprise
repository may, by its own policy, choose instead to reject unsigned uploads.

### Author signing downgrades

A package that was previously author-signed and is now uploaded unsigned is a
red flag — but a *recoverable* one, because in the open-source world an author
legitimately losing the ability to sign — cost, time, or simply no longer being
able to obtain a publicly trusted certificate — is common.

The flag is resolved by the author, once, at the server, rather than reactively
by every consumer. When the downgrade is detected at publish time (step 4
above) the upload is rejected until the author updates an account setting
acknowledging that the package — or the namespace — is no longer author-signed.
That setting change is itself a sensitive account action: re-authenticated,
logged, and notified to the account's verified email. As a result, even an
attacker who has taken over a publisher account cannot *silently* downgrade a
popular package; the downgrade becomes a deliberate, visible, attributable
event.

Once acknowledged, the server records the change and subsequent unsigned
uploads of that package id are accepted normally. The repository signature's
attestation then distinguishes the two unsigned cases — a package that was
never author-signed, and one whose author has deliberately ceased signing — so
that a client can present an accurate message rather than an alarming one.

### Account security is the root of publisher trust

This scheme reduces publisher identity to account control — whoever controls a
publisher account decides which keys are registered. A compromised account lets
an attacker register their own key and publish a validly signed package.
Publisher accounts must therefore enforce strong authentication (2FA), and key
registration must be a sensitive operation: re-authenticated and confirmed out
of band (for example by email). This is the same trust model as npm and NuGet,
and the same caveat applies — see *Security Considerations*.

### What the repository signature attests

Because the server performs the registered-key check at publish time, the
repository signature means more than "the repository accepted this package." To
make that stronger statement portable, the repository signature carries a
signed attribute recording both the **publisher namespace identifier** — the
account or namespace that owns the package's id prefix — and the **author
certificate verified** against it (its SPKI hash), or a marker that the package
was uploaded unsigned.

Including the namespace identifier, not just the key hash, is what makes
verification fully offline. A consumer that trusts the repository key can read
straight from the package that the gallery verified the author against a key
registered to a named namespace, render that identity, and confirm it is
consistent with the package's `packageId` — all without contacting the
server's account database. A consumer verifying a package obtained from a
mirror, or with no network connection at all, gets the same assurance as one
talking to the live gallery.

The practical consequence is that most consumers need to pin only one key — the
repository's — and transitively gain publisher-identity assurance, instead of
pinning every publisher individually. Security-conscious consumers and
enterprises can still additionally pin specific publishers and verify the
author signature directly; both checks coexist under `author-and-repository`
mode.

---

## Multiple Repositories and Re-Ingest

The DPM server is open source, so the official gallery is not the only
repository that will exist: organisations may run their own instances, often
for internal use. Such an instance may serve packages that originated on the
official gallery and therefore already carry an author signature and the
official gallery's repository signature. When that instance ingests such a
package it adds *its own* repository signature, and the package ends up
carrying two — or more — repository signatures.

The architecture handles this without change, because repository signatures are
detached, independent CMS blobs in the append-only `signatures/` directory: the
official gallery is simply one repository among potentially many, and a package
accumulates one repository signature per repository that has served it. The
rules below make that explicit.

### Signatures are additive

A repository that ingests an already-signed package **appends** its signature
and MUST preserve every existing signature — author and repository alike. It
never strips or rewrites them; `signatures/` is append-only. A package may
therefore hold any number of `repository`-role signatures (`repository-1.p7s`,
`repository-2.p7s`, …), each an independent detached signature by a different
repository over the same manifest bytes.

### Trust is per-client and per-repository

Each client's `trustedRepositories` lists the repository keys *it* trusts. A
client validates the repository signatures from repositories it trusts and
ignores the rest. Two repository signatures never need to be reconciled,
because no single client depends on more than the ones it has chosen to trust.
A repository signature from an untrusted repository is inert — neither an error
nor a warning, just additional vouching the client has no reason to act on.

This is also what keeps the model safe: as established in *Verification
Workflow*, step 9, a mode requirement is satisfied only by a signature from a
*trusted* repository. The mere presence of a repository signature means
nothing — otherwise anyone running their own instance could satisfy
`repository-required` for everyone.

### Attestations are per-repository

Each repository signature carries that repository's *own* `dpmRepositoryAttestation`
— the namespace and author verification *as that repository's account database
records them*. Different repositories have independent account databases, so
their attestations may legitimately differ: the official gallery may attest a
verified, registered author key, while a private instance that imported the
same package — and has no record of the upstream author — attests the package
as unsigned, or under a namespace its own admins own. Neither is wrong; each is
a claim by a different party. A client therefore reads an attestation **only**
from a repository signature it trusts, and never acts on an attestation from an
untrusted one.

### Re-ingest publish behaviour

When a repository ingests an already-signed package, its publish-time policy
(*Publisher Registration → Publish-time verification*) applies to *its* decision
to admit and sign the package, not to the upstream signatures, which it leaves
intact. A private instance generally cannot run the registered-key check — it
has no record of the original author — and will configure its own policy:
commonly, repository-sign whatever its administrators choose to import. That is
a legitimate use of a repository signature, which has always meant "this
repository admitted and served this package," not "this repository wrote it."

---

## Trust Bootstrapping and Key Rotation

Every check in this design ultimately resolves to a small number of trust
anchors. How a client comes to trust those anchors, and how they are rotated,
must be defined explicitly — otherwise first-install trust is ambiguous and
future migrations are painful.

### Built-in trust roots

The DPM client ships with a built-in set of trust anchors, the way a browser
ships a root store or a Linux distribution ships a keyring package:

* the SPKI pin(s) of the official package repository's signing key(s),
* the public certificate-authority roots DPM accepts for code signing and for
  RFC3161 timestamping — the same publicly trusted root set browsers and NuGet
  rely on. These might depend on the Windows Certificate store (what does nuget do?).

This built-in trust set is *versioned*: the client records which version it
carries, and a newer set can be delivered by a DPM update. A fresh install
therefore has an unambiguous, offline-checkable answer to "should I trust this
repository signature" without any first-use prompt.

Enterprises can supply their own trust set — internal repository SPKI pins and
internal CA roots, such as Active Directory Certificate Services — overriding or
supplementing the built-in one through configuration. This is what lets
enterprise-internal packages be signed with an internally issued certificate
(see *Author Signing and Certificate Requirements*).

### Repository key rotation

The repository signing key is the single most important trust anchor, so
rotating it must be possible without invalidating every client. The design
requires:

* **Overlapping keys.** The repository may have more than one valid signing key
  at once. The built-in trust set can list several repository SPKI pins, and a
  consumer accepts a repository signature made by any currently trusted key.
* **Signed rollover metadata.** A new repository key is introduced by metadata
  signed with the *current* key, so clients learn the new key through a chain
  of trust they already have rather than out of band.
* **Grace periods.** An old key remains trusted for a defined overlap window
  after a new key is introduced, so packages signed just before rotation
  continue to verify.
* **Emergency revocation.** A compromised repository key must be revocable
  promptly — distributed through a signed revocation in the trust-set update
  channel, and, for online clients, through standard certificate revocation.
  This path is intentionally separate from routine rotation.

Publisher key rotation is already handled by the per-account key *set* in
*Publisher Registration*; the repository simply accepts author signatures from
any key currently registered to the namespace.

---

## Signing Workflow

### Author signing

1. Generate the canonical manifest from the package contents.
2. Sign the manifest bytes, producing a detached CMS `SignedData` with the
   signing certificate and chain, the signing-time attribute, and the
   `author` role attribute.
3. Request an RFC3161 timestamp for the signature and add the returned token
   as an unsigned attribute on the signer info.
4. Write the result to a `signatures/*.p7s` file inside the package (for
   example `signatures/author-1.p7s`; the name is non-semantic).

Timestamping is part of the author signing flow from Phase 1 onward. Without a
timestamp a signature can only ever be checked as "valid now," so it becomes
unverifiable the moment the signing certificate expires. Timestamping is a
single Windows API call and is not deferred.

### Repository signing

1. The repository receives the package (author-signed or unsigned).
2. It runs the full verification workflow against the package, verifies any
   author signature, and confirms the author's signing key is registered to
   the account that owns the package's namespace (see *Publisher Registration
   and Repository Verification*). It never signs a package it has not itself
   fully verified.
3. It signs the *same* manifest bytes, producing a CMS `SignedData` with the
   `repository` role attribute and the author-certificate-verified attribute,
   and timestamps it.
4. It writes an additional `signatures/*.p7s` file (for example
   `signatures/repository-1.p7s`).

The content files and the manifest are not modified — only a new file is added
under `signatures/`.

---

## Verification Workflow

1. Open the package and enumerate every entry.
2. Read the exact bytes of `dpm-manifest.json`, then parse them with a hardened
   JSON parser — rejecting duplicate keys, enforcing depth and size limits, and
   performing no surprising type coercions, since the manifest is
   attacker-controlled input.
3. **Enforce file-set equality and integrity.** Working from the *parsed*
   manifest (never a regenerated one), check the package against
   `manifest.files` entry by entry: every listed entry must exist in the
   package with a matching size and hash, and the package must contain no
   entries *other than* `manifest.files ∪ {dpm-manifest.json} ∪ signatures/**`.
   This detects every form of tampering — a modified file fails its hash, an
   added file is an unlisted entry, a removed file is a missing entry, a rename
   is both. Reject any path that fails the path-safety rules. A package with an
   extra, unlisted file fails verification even if every listed file is
   correct. This check is what gives the signature meaning: a signature over
   the manifest is worth nothing until the package is proven to *be* what the
   manifest says it is.
4. Load every file under `signatures/`. Every such file MUST be a well-formed
   CMS `SignedData` blob; any file under `signatures/` that is not — stray data,
   unknown file types — fails verification. Together with step 3 this leaves the
   package with no unmanaged space: every byte is either manifest-committed
   content, the manifest itself, or a valid signature. Parse each blob's
   structure enough to read its `dpmSignatureRole` attribute and signer
   certificate, and index the signatures by `(role, signer SPKI)`; file names
   play no part — see *Signature selection*.
5. For each signature the active mode requires (see *Signature selection*),
   verify the detached CMS signature against the manifest bytes using
   `CryptVerifyDetachedMessageSignature`.
6. Build and validate the certificate chain with `CertGetCertificateChain`,
   then apply chain policy with `CertVerifyCertificateChainPolicy`. The chain
   must reach a trusted root.
7. Verify the RFC3161 timestamp, establish the effective signing time, and
   confirm the certificate was valid at that time.
8. Apply the active trust policy: validation mode, `allowUnsigned`, and the
   publisher/repository allowlists (Phase 2+).
9. Classify each signature by its role attribute and confirm the mode's
   requirements are satisfied. A requirement is met only by a signature that is
   valid *and* from a party the client trusts: `repository-required` needs at
   least one valid repository signature from a trusted repository, and
   `author-and-repository` additionally needs a valid author signature. A
   package may carry several repository signatures (see *Multiple Repositories
   and Re-Ingest*); signatures from repositories the client does not trust are
   ignored — they are neither errors nor warnings. "A repository signature is
   present" is never sufficient; it must be a *trusted* one.
10. Report results.

An unsigned package can still have steps 1–3 performed against it: integrity
verification is independent of authenticity. In `permissive` mode this yields
an integrity-checked but unauthenticated install.

### Signature selection

Steps 5–7 are run only on the signatures the active mode actually requires, not
on every blob in `signatures/`. The verifier selects them as follows:

* **File names are never used.** A file name is unsigned, attacker-controllable
  metadata; relying on it could mislead, and it would have to be re-checked
  against the signature content anyway. Selection works entirely from what step
  4 parsed out of each CMS blob — the `dpmSignatureRole` attribute and the
  signer certificate's SPKI. This is also why signature files are not named
  after repositories or domains: a domain is not a repository's identity (its
  signing-key SPKI is), it is not stable across key rotation or multiple
  hostnames, and a name unrelated to the signature content could never be
  trusted regardless.
* **Match by SPKI.** Repository signatures are matched against the client's
  `trustedRepositories` SPKI pin set, and author signatures against
  `trustedPublishers`, by computing the SHA-256 of each signer certificate's
  Subject Public Key Info and looking it up in the pinned set. A package holds
  only a handful of signatures, so the index built in step 4 makes this an
  in-memory lookup; the directory scan carries no significant cost.
* **Verify only what is required.** In `repository-required` mode the verifier
  fully verifies a trusted repository signature and need not run the
  cryptographic checks on the author signature at all; `author-and-repository`
  additionally verifies the author signature. A repository signature whose
  signer SPKI is not in the trusted set is skipped entirely — not verified, and
  not an error.

The cost worth optimising is the cryptography — signature verification, chain
building, timestamp validation — not locating a file. Selecting by the signed
role attribute and the signer SPKI optimises exactly that, and does so without
trusting any unauthenticated metadata.

### Verification output

Output identifies *who* signed, not merely that a signature exists:

```text
Package: VSoft.CommandLine 1.0.0

Author signature
  Signer:     CN=VSoft Technologies
  Thumbprint: AB12CD34EF56...
  Timestamp:  2026-05-19 10:00:03 UTC (DigiCert)
  Result:     valid, chains to trusted root

Repository signature
  Signer:     CN=delphi.dev package service
  Thumbprint: EF56AB7890...
  Timestamp:  2026-05-19 10:04:11 UTC (DigiCert)
  Verified:   author key registered to VSoft Technologies
  Result:     valid, repository trusted

Package integrity: verified (14 files, no unlisted entries)
```

---

## Package Cache Integrity

DPM relies on a package cache (`%APPDATA%\.dpm\packages`) holding downloaded
`.dpkg` files and their extracted contents. Fully verifying a package — CMS
signature cryptography, certificate chain building, timestamp validation, and
hashing every content file — on every cache access would be prohibitively slow;
a restore that touches dozens or hundreds of packages would pay that cost on
every build. The cache therefore needs a way to verify once and then trust
cheaply, without weakening the guarantee.

### The model: verify on ingest, trust on read

A package is fully verified exactly once — when it is downloaded and extracted
into the cache. Subsequent cache reads do not re-run signature cryptography or
re-hash all content. This matches NuGet, which validates a package signature
when the package is downloaded into the global-packages folder but not when it
is already present there, and which treats the extracted global-packages folder
as a trusted location.

### Verification receipt

The final step of a successful verify-and-extract is to write a small receipt
file into the package's cache folder. Like NuGet's `.nupkg.metadata`, the
receipt is written *last*, so its presence is the signal that extraction
completed; a crash or interrupted download leaves no receipt and the entry is
re-fetched on next use. Extraction runs under a file lock so concurrent installs
of the same package do not collide.

The receipt records more than completion — it records the verification
*result*:

* package id, version, and target compiler,
* the manifest hash (SHA-256 of the signed `dpm-manifest.json` bytes),
* the signing certificate SPKI hashes and roles, and the repository namespace
  attestation,
* the effective signing time established from each signature's RFC3161
  timestamp,
* the revocation status of each certificate as of the verification time,
* the trust decision and a fingerprint of the trust-policy inputs that produced
  it,
* the verification time and the DPM version.

### Cheap re-check on a cache hit

On a cache hit DPM does not re-verify in full. It instead:

1. confirms the receipt exists and is well-formed;
2. confirms the current effective trust policy still matches the policy
   fingerprint in the receipt — if the user has since tightened the policy
   (revoked trust in a publisher, switched `permissive` to `require`), the
   receipt is stale and the package is fully re-verified;
3. re-hashes only the signed manifest — a small, fast operation — and confirms
   it matches the hash recorded in the receipt.

If the manifest is unchanged and the policy is unchanged, the recorded
verification result still holds, because the signed manifest already commits to
every content file's hash. No CMS cryptography and no full content rehash occur
on this path.

### What this protects, and what it does not

This mechanism is corruption-aware and policy-aware; it is not a defense against
a local attacker. The fast path does not re-hash extracted content files, so a
modification made to an extracted file *after* verification is not caught by a
normal cache hit. This is a deliberate trade-off: the cache is
a per-user, ACL-protected directory, and an attacker able to write to it already
has the privileges to modify `dpm.exe`, `dpm.config.yaml`, or the consuming project
directly — cache-integrity checks cannot defend against that and do not try to.
What the receipt *does* guarantee is detection of accidental corruption and
partial extraction, and correct re-verification whenever the trust policy
changes.

For environments that need more — CI, or shared and restored caches of
uncertain provenance — `dpm cache verify` re-hashes all content files in the
cache against their manifests and re-runs signature verification. The original
signed manifest and `.p7s` files are retained in the cache so this full
re-verification is always possible offline, without re-downloading.

### Project-level integrity

The receipt protects a cache entry against itself. To detect a cache entry that
does not match what a *project* expects, a project-level lock file should record
the manifest hash of each resolved dependency. On restore, DPM compares the
cache entry's manifest hash against the lock file; a mismatch — a substituted
package, a wrong resolved version, a tampered manifest — fails the restore. This
is the analogue of NuGet's `packages.lock.json` content-hash validation. Since DPM does not use a separate lock file, this should be recorded in the `PackageReference` entries in the `projectname.dproj` file

### Optional: content-addressed cache layout

A stronger structural option is to store extracted content in a directory named
by the manifest hash. The manifest hash then *is* the lookup path, so a
substituted manifest lands at a different path and a lookup by the expected hash
simply misses rather than returning tampered content. This also yields natural
deduplication. It is a larger change to the current id/version/compiler-keyed
layout and is noted here as an option, not a requirement.

---

## Windows API Usage

### Hashing

`BCryptOpenAlgorithmProvider`, `BCryptHashData`, `BCryptFinishHash`.

### CMS signing

`CryptSignMessage` for the detached signature in the simple case;
`CryptMsgOpenToEncode` when finer control is needed. Adding the timestamp
unsigned attribute is done with `CryptMsgControl`
(`CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR`) after the signature is produced.

### CMS verification and decoding

`CryptVerifyDetachedMessageSignature` (detached content supplied separately —
note this is *not* `CryptVerifyMessageSignature`, which expects embedded
content). `CryptMsgOpenToDecode` and `CryptMsgGetParam` for reading signed
attributes such as the role attribute.

### Certificates

`CertOpenStore`, `CertFindCertificateInStore`,
`CryptAcquireCertificatePrivateKey` (which resolves CNG key storage providers,
covering smart cards and HSMs transparently), `CertGetCertificateChain`,
`CertVerifyCertificateChainPolicy`.

### Timestamping

`CryptRetrieveTimeStamp` to obtain an RFC3161 token and the corresponding
timestamp verification API to validate one. No ASN.1 is written by hand.

---

## Delphi Architecture

### Supported compiler range

DPM supports Delphi XE2 and later — roughly fifteen years of compiler
evolution. That floor rules out several modern conveniences and shapes the
implementation:

* **No managed records.** Record `Initialize` / `Finalize` operators arrived in
  10.3, so handle wrappers must be interface-based (see *Handle lifetime*).
* **No reliance on `System.JSON` for manifest generation.** Early `System.JSON`
  is immature and non-deterministic; manifest bytes come from the purpose-built
  emitter described in *Manifest Design*. The verification-side parser must in
  turn be hardened against hostile input (see *Verification Workflow*). We will use JSONDataObjects or VSoft.YAML for this - TBD.
* **Flat Win32 API imports.** Cryptographic functionality is reached through
  direct `crypt32.dll` / `bcrypt.dll` imports rather than version-specific RTL
  wrappers, keeping the code stable across the whole compiler range.

### Proposed units

* **DPM.Crypto.Hashing** — SHA-256 hashing of files and streams.
* **DPM.Crypto.Cms** — CMS signing, verification, and attribute decoding.
* **DPM.Crypto.X509** — certificate loading, selection, chain building, trust
  validation.
* **DPM.Crypto.Timestamping** — RFC3161 requests and timestamp validation.
* **DPM.Crypto.Provider** — the `ISigningProvider` abstraction and its
  implementations (local certificate store, PFX, and later remote providers).
* **DPM.Package.Manifest** — canonical manifest generation and parsing.
* **DPM.Package.Signing** — orchestration of package signing and verification;
  the only unit that knows about both packages and crypto.

`DPM.Crypto.Cms` depends on `ISigningProvider` for the actual signing
operation, so it never embeds knowledge of where a private key lives.

### Handle lifetime

All Windows crypto handles (`CERT_CONTEXT`, `HCERTSTORE`, `HCRYPTMSG`,
`BCRYPT_*` handles) are wrapped. Because the supported compiler range starts at
XE2, managed records are not available; wrappers are interface-based
reference-counted objects — a `TInterfacedObject` implementing a small wrapper
interface — since Delphi objects are not destroyed at scope exit. Example
wrapper interfaces:

```pascal
ICertificateContext
ICertificateStore
ICmsMessage
```

Each wrapper releases its underlying handle in its implementation's destructor,
so consumers never write explicit cleanup.

---

## Certificate Support

* **Windows certificate store** — the recommended default.
* **PFX files** — a software keystore, suitable for enterprise-internal and
  development certificates. Note that a publicly trusted code-signing
  certificate can no longer be held as a bare PFX — its key must be
  hardware-protected since June 2023 — so for public-gallery signing the CI/CD
  path is a hardware token on the build agent or a signing service / Key Vault.
* **Smart cards and hardware tokens** — work transparently, because Windows
  exposes them through a CNG key storage provider that
  `CryptAcquireCertificatePrivateKey` resolves automatically.
* **Enterprise PKI** — Active Directory Certificate Services, internal roots,
  and private trust anchors are supported through the standard chain engine.

**Signotaur** - Signotaur will support signing dpm packages natively in a future update (once package signing is settled) - and can also be integrated into the `dpm sign` command some point via a dedicated remote `ISigningProvider` implementation (Phase 3) that performs the sign-digest operation remotely and assembles the CMS locally. Treating it as
just another certificate source would not work.

**Azure Key Vault is a special case.** Unlike a smart card or local HSM, Key
Vault does not present itself as a local CNG provider — the private key never
reaches the machine and signing happens over a REST API. It therefore does not
slot into the certificate-store model "for free." Key Vault is supported via a
dedicated remote `ISigningProvider` implementation (Phase 3) that performs the
sign-digest operation remotely and assembles the CMS locally. Treating it as
just another certificate source would not work.

---

## CLI Design

```text
dpm sign Foo.dpkg --thumbprint AB12CD34EF56
dpm sign Foo.dpkg --pfx cert.pfx
dpm sign Foo.dpkg --timestamper http://timestamp.digicert.com
dpm verify Foo.dpkg
dpm verify Foo.dpkg --config dpm.config
dpm verify Foo.dpkg --offline
dpm cache verify
```

PFX passwords should be supplied via environment variable or interactive
prompt rather than a plain command-line argument, so they do not appear in
process listings or shell history.

---

## Security Considerations

### Package immutability

Once a package is published, the manifest and all content files
(`package.dspec.yaml`, `lib/`, `source/`, `tools/`) are immutable. Any change to
them invalidates every signature. The one exception is the `signatures/`
directory, which is append-only: a repository legitimately adds its signature
after the author published. `signatures/` is excluded from the manifest
precisely so that adding a signature does not invalidate existing ones.
Changing manifest content or content files requires repackaging, resigning,
and republishing under a new version.

### Complete file set

As stated in principle 3, verification must reject packages containing files
not listed in the manifest. This is restated here because it is a security
control, not merely a correctness detail: omitting it makes signing
cosmetic.

### Path traversal

Manifest paths are validated against absolute paths, drive letters, `..`
escapes, and non-canonical separators and normalised before any file is written to disk.

### Hash algorithm agility and downgrade resistance

The manifest names its hash algorithm explicitly, and because `hashAlgorithm`
is inside the signed manifest it cannot be downgraded without breaking the
signature. The verifier nonetheless enforces the mandatory profile in
*Cryptographic Algorithms* and refuses weak algorithms (SHA-1, MD5) even when a
signed manifest specifies them.

### Timestamping and revocation

These two interact and must be handled together. With a valid RFC3161
timestamp, a signature remains valid after the signing certificate expires,
because the timestamp proves the signature predates expiry. The same logic
applies to revocation: a certificate revoked *after* a legitimate timestamped
signing should generally still be trusted — **except** when the revocation
reason is `keyCompromise`. DPM policy SHOULD treat a `keyCompromise` revocation
as retroactively invalidating every signature by that certificate, regardless
of timestamp, unless an administrator has set an explicit recovery policy; any
such override is a deliberate, logged action, not a default. Naive "is this
certificate revoked now?" logic would instead incorrectly reject every
legitimately signed package the moment a certificate rotates. Revocation
checking (Phase 3) must be timestamp-aware.

### Signature stripping

An attacker who removes a signature file is defeated by validation mode: under
`require`, `repository-required`, or `author-and-repository`, a missing
required signature fails verification.

### Publisher account compromise

Repository-mediated publisher identity (see *Publisher Registration and
Repository Verification*) is only as strong as publisher account security. An
attacker who takes over a publisher account can register their own signing key
and publish a validly signed, repository-signed malicious package. Signing
cannot defend against this; it is mitigated by enforcing 2FA on publisher
accounts, treating sensitive account actions — key registration, and
acknowledging an author-signing downgrade — as re-authenticated, logged, and
out-of-band-notified operations, and, for detection after the fact, by
transparency logging (Phase 4). Making these actions notified and attributable
means an account takeover that attempts a silent downgrade instead produces a
visible, logged event the real owner can react to.

### Replay and dependency confusion

Future repository metadata may add publish timestamps and repository serial
numbers to resist replay, and repository pinning, source mapping, and namespace
ownership to resist dependency-confusion attacks. These are out of scope for
the phases below.

---

## Implementation Phases

### Phase 1 — Core signing MVP

Included:

* Canonical manifest generation.
* SHA-256 file and manifest hashing.
* Detached CMS author signatures.
* RFC3161 timestamping (moved into the MVP — see *Signing Workflow*).
* Verification: cryptographic validity, the mandatory algorithm profile,
  certificate chain to a trusted root, complete-file-set enforcement, and
  path-safety and archive-format rules.
* Built-in code-signing and timestamping trust roots (versioned).
* Cache verification receipts: verify on ingest, trust on read.
* Windows certificate store and PFX signing.
* `permissive` and `require` validation modes, with the sticky no-downgrade
  rule (trust-on-first-use on the author key).

Excluded: repository signing, publisher/repository allowlists, the repository
validation modes, revocation checking, and remote signing providers.

### Phase 2 — Production signing

* Repository signing and the role attribute end to end.
* Publisher key registration on the server, and publish-time registered-key
  and package-identity verification.
* The repository attestation attribute: publisher namespace plus the verified
  author certificate, enabling fully offline verification.
* Built-in repository trust roots, with signed rollover metadata and key
  rotation.
* Full trust policies: `trustedPublishers` and `trustedRepositories` pinned by
  SPKI.
* Trust-policy-aware cache invalidation, and a project lock file recording
  per-dependency manifest hashes.
* `repository-required` and `author-and-repository` modes; the default mode
  tightens once the gallery is fully repository-signed.
* Improved CLI output and ergonomics.

### Phase 3 — Enterprise features

* Revocation checking (CRL and OCSP), timestamp-aware as described above.
* Cached and offline revocation responses.
* Remote `ISigningProvider` implementations, including Signaotaur and Azure Key Vault.
* Enterprise repository trust and offline trust policies.
* CI/CD integration improvements.

### Phase 4 — Advanced supply-chain features

Possible future work: transparency logs, SBOM attestations, vulnerability
attestations, reproducible build attestations, Sigstore compatibility, and TUF
metadata.

---

## Comparison to NuGet

### Adopted from NuGet

Author signing, repository signing, X509 trust, RFC3161 timestamping, CMS
signatures, and a signed attribute to record signature role.

### Deliberate simplifications

* No OPC package transforms or XML canonicalization — DPM signs a JSON
  manifest's raw bytes.
* No nested countersignatures — author and repository signatures are
  independent over the same manifest; an optional signed attribute records the
  exact author blob for audit purposes (see *Signature Roles*).
* No mutable package semantics — published content is immutable; only the
  append-only `signatures/` directory may grow.

---

## Conclusion

This architecture provides strong package integrity guarantees,
industry-standard cryptographic trust, enterprise compatibility, and long-term
signature validity, while implementing no custom cryptography and no custom
ASN.1. The two changes most important to get right are enforcing complete
file-set equality during verification — without which signing is cosmetic — and
including timestamping from the first release, without which signatures expire
with their certificates. With those in place, DPM can offer a modern, secure
signing ecosystem at significantly lower complexity than NuGet signing.