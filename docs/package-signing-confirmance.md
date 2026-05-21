# DPM Package Signing — Conformance Requirements

## Status and scope

This document defines conformance requirements for implementations of the DPM
package signing system. It is a companion to *DPM Package Signing Architecture*
(the "architecture document"), which gives the design and its rationale, and to
*DPM Package Signing — Threat Model*. Together the three form the DPM package
signing specification set.

The architecture document is written as prose. This document restates its
normative content as a flat, numbered, testable checklist, so that an
implementation can be assessed and so that each requirement can be mapped to a
test. Where this document and the architecture document appear to differ, the
architecture document is the source of intent and this document should be
corrected.

## Terminology

The key words MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY, and OPTIONAL in this document are to be interpreted as
described in BCP 14 (RFC 2119, RFC 8174) when, and only when, they appear in
all capitals.

All other terms — manifest, author signature, repository signature, SPKI pin,
attestation, namespace, trust set, and so on — are used as defined in the
architecture document.

## Conformance classes

An implementation conforms as one or more of the following classes. A single
program — for example the `dpm` command-line tool — MAY implement several.

* **Manifest Producer (M)** — generates `dpm-manifest.json` and assembles the
  `.dpkg` archive.
* **Signer (S)** — produces author signatures.
* **Verifier (V)** — the client side: verifies packages before use and manages
  the package cache. This is the security-critical class.
* **Repository (R)** — the server side: ingests, verifies, repository-signs,
  and serves packages, and manages publisher accounts.

## Phases

Requirements are tagged with the implementation phase (P1–P4) defined in the
architecture document. An implementation claiming conformance at phase *N* MUST
satisfy every MUST and MUST NOT of its class(es) tagged phase *N* or earlier,
and SHOULD satisfy the SHOULD-level items. Requirements tagged with a later
phase do not apply to a claim at an earlier phase.

A conformance claim MUST state the class(es) claimed and the phase.

## Traceability

Each requirement group cites the architecture-document section(s) it derives
from. Every requirement SHOULD have at least one corresponding test.

---

## Manifest Producer (M)

*Derives from: Manifest Design; Package Structure.*

* **M-1 (P1)** The producer MUST emit `dpm-manifest.json` as UTF-8 with no byte
  order mark and LF line endings only.
* **M-2 (P1)** The producer MUST serialise object properties in lexicographic
  key order, and the `files` array in byte-wise ascending order of `path`.
* **M-3 (P1)** Numeric fields MUST be integers; the manifest MUST NOT contain
  floating-point values.
* **M-4 (P1)** The producer MUST generate the manifest with a dedicated
  deterministic emitter. It MUST NOT depend on a general-purpose JSON library
  whose key ordering or number formatting is not guaranteed.
* **M-5 (P1)** For identical input, manifest output MUST be byte-identical
  across all supported Delphi compiler versions.
* **M-6 (P1)** The `files` array MUST enumerate every content file in the
  package. `dpm-manifest.json` itself, and every entry under `signatures/`,
  MUST NOT appear in `files`. No other file may be absent from it.
* **M-7 (P1)** Every `path` MUST be relative, use forward-slash separators, and
  be in Unicode Normalization Form C. The producer MUST reject input it cannot
  bring into NFC, rather than silently rewriting it.
* **M-8 (P1)** The producer MUST NOT emit a `path` that is absolute, contains a
  drive letter, contains a `..` segment, contains a backslash, has any
  component with a trailing dot or with leading or trailing whitespace,
  contains control characters, or duplicates another `path` under
  case-insensitive comparison.
* **M-9 (P1)** The manifest MUST contain `dpmPackageFormat`,
  `manifestSchemaVersion`, `packageId`, `version`, and `hashAlgorithm`.
* **M-10 (P1)** Each `files` entry MUST record `path`, `size`, and `hash`, the
  hash computed with the algorithm named by `hashAlgorithm`.
* **M-11 (P1)** The producer MUST place any security-significant information in
  a schema field governed by `manifestSchemaVersion`. It MUST NOT place such
  information in `extensions`, which is reserved for additive non-security
  metadata.
* **M-12 (P1)** The `.dpkg` archive MUST satisfy the archive-format rules
  V-9 through V-13; the producer MUST NOT create an archive that a conforming
  Verifier would reject.

---

## Signer (S)

*Derives from: Signature Roles; CMS / PKCS#7 Format; Cryptographic Algorithms;
Signing Workflow.*

* **S-1 (P1)** An author signature MUST be a detached, DER-encoded CMS
  `SignedData` structure computed over the exact byte content of
  `dpm-manifest.json`.
* **S-2 (P1)** The signature MUST carry the `dpmSignatureRole` signed attribute
  with value `author`.
* **S-3 (P1)** The signature MUST embed the signing certificate, its
  intermediate certificates, and a signing-time signed attribute.
* **S-4 (P1)** The Signer MUST obtain an RFC3161 timestamp and embed the
  timestamp token as an unsigned attribute on the signer info.
* **S-5 (P1)** The Signer MUST use only algorithms permitted by the mandatory
  algorithm profile (V-16). It MUST NOT produce a signature using SHA-1, MD5,
  or an RSA key shorter than 2048 bits.
* **S-6 (P1)** The Signer MUST write each signature as a separate file under
  `signatures/`. The file name carries no meaning; the Signer MUST NOT encode
  trust-relevant information (such as a repository domain) in it.
* **S-7 (P1)** When signing, the Signer MUST NOT modify `dpm-manifest.json` or
  any content file, and MUST NOT remove or alter any existing signature. It
  MUST only add a file under `signatures/`.
* **S-8 (P1)** A Signer MAY use any code-signing certificate. Whether a given
  certificate is acceptable is determined by the target Repository's acceptance
  policy and by each consuming Verifier's trust policy, not by the Signer.

---

## Verifier (V)

*Derives from: Verification Workflow; Trust Model; Trust Policies; Author
Signing and Certificate Requirements; Cryptographic Algorithms; Multiple
Repositories and Re-Ingest; Package Cache Integrity.*

### Manifest and integrity

* **V-1 (P1)** The Verifier MUST verify every signature over the exact stored
  bytes of `dpm-manifest.json`. It MUST NOT re-serialise or regenerate the
  manifest for any verification purpose.
* **V-2 (P1)** The Verifier MUST parse the manifest with a hardened parser that
  rejects duplicate object keys and enforces input depth and size limits.
* **V-3 (P1)** The Verifier MUST enforce file-set equality: every entry in
  `manifest.files` MUST be present in the package with matching `size` and
  `hash`, and the package MUST contain no file other than `dpm-manifest.json`,
  the entries in `manifest.files`, and files under `signatures/`. A package
  with any unlisted or missing file MUST be rejected.
* **V-4 (P1)** The Verifier MUST reject any manifest `path` that violates the
  path-safety rules (M-8).
* **V-5 (P1)** The Verifier MUST NOT silently accept a package whose
  `dpmPackageFormat` or `manifestSchemaVersion` is newer than the version it
  implements; it MUST refuse the package or fail in a clearly reported way.
* **V-6 (P1)** The Verifier MUST ignore unrecognised keys in `extensions`
  without failing, and MUST NOT base a trust decision on them.

### Archive format

* **V-9 (P1)** The Verifier MUST reject an archive containing two entries whose
  paths are equal under case-insensitive comparison of their NFC form.
* **V-10 (P1)** The Verifier MUST decode entry names as UTF-8 and MUST reject
  the package if a name is not in NFC or does not exactly match a manifest
  `path`.
* **V-11 (P1)** The Verifier MUST reject an archive entry that is not a regular
  file or directory (for example a symbolic link or device entry).
* **V-12 (P1)** The Verifier MUST reject any path containing an NTFS alternate
  data stream (`:`) or matching a Windows reserved device name.
* **V-13 (P1)** The Verifier MUST reject an archive entry that is encrypted or
  uses a compression method other than Store or Deflate.

### Signatures

* **V-14 (P1)** Every file under `signatures/` MUST be a well-formed CMS
  `SignedData` blob; the Verifier MUST reject a package containing any other
  kind of file there.
* **V-15 (P1)** For each signature it relies on, the Verifier MUST verify the
  detached CMS signature against the manifest bytes, MUST build the certificate
  chain to a trusted root, and MUST confirm the certificate satisfies
  code-signing usage requirements.
* **V-16 (P1)** The Verifier MUST enforce the mandatory algorithm profile and
  MUST reject SHA-1, MD5, and RSA keys shorter than 2048 bits for file hashes,
  CMS digests, and timestamps, even when a signed manifest or signature
  specifies them.
* **V-17 (P1)** The Verifier MUST verify the RFC3161 timestamp, establish the
  effective signing time from it, and confirm the signing certificate was valid
  at that time.
* **V-18 (P1)** The Verifier MUST select signatures by the `dpmSignatureRole`
  attribute and the signer certificate's SPKI. It MUST NOT use signature file
  names for selection or for any trust decision.
* **V-19 (P1)** A validation-mode requirement MUST be treated as satisfied only
  by a signature that is both cryptographically valid and produced by a party
  the Verifier trusts. The presence of a repository signature MUST NOT by
  itself be treated as satisfying `repository-required`.
* **V-20 (P2)** The Verifier MUST accept a package carrying more than one
  repository signature. A repository signature whose signer SPKI is not in the
  client's trusted set MUST be ignored — neither verified, nor treated as an
  error or a warning.
* **V-21 (P2)** The Verifier MUST read a `dpmRepositoryAttestation` only from a
  repository signature it trusts, and MUST NOT act on an attestation carried by
  an untrusted signature.

### Trust policy and modes

* **V-22 (P1)** The Verifier MUST implement the `permissive` and `require`
  validation modes. **(P2)** It MUST additionally implement
  `repository-required` and `author-and-repository`.
* **V-23 (P1)** The Verifier MUST pin trusted publishers and repositories by
  the SHA-256 hash of the certificate's Subject Public Key Info. It MUST NOT
  use the certificate Common Name as a trust anchor.
* **V-24 (P2)** The Verifier MUST implement the no-downgrade rule for the
  repository dimension: once a package id has been seen carrying a valid
  repository signature from a trusted repository, a later build lacking a valid
  signature from a trusted repository, or attesting a different namespace, MUST
  fail regardless of the global mode. This ratchet MUST be keyed to trusted
  repositories.
* **V-25 (P1)** The Verifier MUST implement the no-downgrade rule for the
  author dimension, governed by the `authorDowngradePolicy` setting, which MUST
  default to `prompt`. The Verifier MUST NOT allow a previously author-signed
  package to become unsigned silently.
* **V-26 (P3)** When performing revocation checking, the Verifier MUST be
  timestamp-aware: a certificate revoked after a valid timestamped signing
  SHOULD still be trusted, except that a `keyCompromise` revocation SHOULD be
  treated as retroactively invalidating every signature by that certificate
  unless an explicit, logged administrator override is in effect.
* **V-27 (P1)** The Verifier SHOULD report the signer identity, trust basis,
  and timestamp for each signature, not merely a pass or fail.

### Cache

* **V-30 (P1)** The Verifier MUST fully verify a package on ingest into the
  cache, before it is first used.
* **V-31 (P1)** The Verifier MUST write the verification receipt as the final
  step of a successful verify-and-extract, and MUST perform extraction under a
  lock that prevents concurrent installs of the same package from colliding.
* **V-32 (P1)** The receipt MUST record at least: package id, version, and
  compiler; the manifest hash; the signing certificate SPKI hashes and roles;
  the effective signing time; the trust decision and a fingerprint of the
  trust-policy inputs that produced it; and the verification time.
* **V-33 (P1)** On a cache hit, the Verifier MUST re-verify the package in full
  if the current trust policy does not match the policy fingerprint recorded in
  the receipt. It MUST NOT serve a cached package whose recorded trust policy no
  longer matches the active one.
* **V-34 (P1)** The Verifier MUST provide a command that re-hashes all cached
  content against its manifests and re-runs signature verification.

---

## Repository (R)

*Derives from: Publisher Registration and Repository Verification; Multiple
Repositories and Re-Ingest; Trust Bootstrapping and Key Rotation; Author
Signing and Certificate Requirements.*

The requirements below define a conforming Repository. R-5, R-6, and R-7
describe the **official gallery's** publish policy; a private or
enterprise-internal Repository instance MAY adopt a different publish policy,
but MUST still satisfy every other requirement in this class.

* **R-1 (P2)** A repository signature MUST be a detached, DER-encoded CMS
  `SignedData` structure over the manifest bytes, carrying the
  `dpmSignatureRole` attribute with value `repository`.
* **R-2 (P2)** Before adding its signature, the Repository MUST run the full
  Verifier workflow (V-1 through V-17) against the package. It MUST NOT
  repository-sign a package it has not itself completely verified.
* **R-3 (P2)** On ingest, the Repository MUST preserve every existing signature
  — author and repository — and MUST only append its own. It MUST NOT strip or
  rewrite existing signatures.
* **R-4 (P2)** A repository signature MUST carry a `dpmRepositoryAttestation`
  signed attribute recording the publisher namespace and, when the package is
  author-signed, the verified author certificate SPKI. When the package is
  unsigned, the attestation MUST distinguish a package that was never
  author-signed from one whose author has acknowledged ceasing to sign.
* **R-5 (P2, gallery policy)** The Repository MUST confirm at publish time that
  `manifest.packageId` and `manifest.version` match the declared upload
  identity and the namespace of the publishing URL, and MUST reject a mismatch.
* **R-6 (P2, gallery policy)** When an uploaded package is author-signed, the
  Repository MUST verify the author signature and confirm the signing key is
  registered to the account that owns the package's namespace.
* **R-7 (P2, gallery policy)** The Repository MUST accept unsigned uploads,
  recording the appropriate unsigned marker in its attestation; author signing
  is optional. A private Repository MAY instead reject unsigned uploads by
  policy.
* **R-8 (P2)** If an account has previously published a package id
  author-signed and a later upload of that id is unsigned, the Repository MUST
  reject the upload until the author acknowledges the change through an account
  setting.
* **R-9 (P2)** The Repository MUST treat key registration and downgrade
  acknowledgement as sensitive account actions: re-authenticated, logged, and
  notified to the account's verified contact address.
* **R-10 (P2)** The Repository MUST support strong authentication (2FA) on
  publisher accounts.
* **R-11 (P2)** Key registration MUST pin the SHA-256 SPKI hash. An account
  MUST be able to hold a set of registered keys and to add and remove keys
  without losing account access.
* **R-12 (P2)** The Repository MUST timestamp each repository signature it
  produces (S-4 applies equally).
* **R-13 (P3)** The Repository MUST support repository key rotation: more than
  one valid signing key at once, introduction of a new key via rollover
  metadata signed by the current key, a defined grace period for the old key,
  and a separate emergency-revocation path.
* **R-14 (P2)** The Repository MUST publish its trust set — repository SPKI
  pins and accepted roots — in a versioned form that clients can update.

---

## Conformance claims

An implementation claiming DPM package-signing conformance MUST state:

1. the class or classes claimed (M, S, V, R);
2. the phase (P1–P4);
3. for any SHOULD-level requirement not met, a rationale.

A claim is valid only if every applicable MUST and MUST NOT for the stated
class(es) at the stated phase is satisfied. Meeting the requirements of a later
phase is not required for a claim at an earlier phase, but an implementation
MUST NOT claim a phase whose requirements it does not fully meet.