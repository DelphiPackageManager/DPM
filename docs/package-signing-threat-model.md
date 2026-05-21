# DPM Package Signing — Threat Model

## Purpose and scope

This document is the threat model for the DPM package signing system. It is a
companion to *DPM Package Signing Architecture* (the "architecture document")
and *DPM Package Signing — Conformance Requirements*.

It is intended as the basis for security review. It states what is being
protected, who the adversaries are, what is assumed, which threats the design
addresses and how — and, explicitly, which risks remain. A candid account of
residual risk is part of the model, not an omission from it.

The model covers the signing system: the manifest, signatures, the trust
model, repository interaction, and the client cache. It does not cover the
security of package *content* — DPM signs code, it does not audit it — nor the
general operational security or availability of the gallery service.

---

## Assets

| ID  | Asset | Description |
| --- | ----- | ----------- |
| A-1 | Package integrity | The guarantee that a package's content is exactly what its publisher produced. |
| A-2 | Author authenticity | The binding between a package and the identity that produced it. |
| A-3 | Repository vouching | The assurance that a package was admitted and served by a named, trusted repository. |
| A-4 | Namespace ownership | The binding between a package-id namespace and the account entitled to publish under it. |
| A-5 | Signing keys | Author signing keys, and especially repository signing keys and the timestamp / CA roots. |
| A-6 | Publisher accounts | Server-side accounts that own namespaces and register signing keys. |
| A-7 | Client trust state | The local sticky no-downgrade state and the cache verification receipts. |

---

## Trust boundaries

* **Author / build environment** — where packages are produced and, optionally,
  author-signed.
* **Transport** — the network between any repository and any client.
* **Repository / gallery server** — ingest, verification, repository signing,
  and account management.
* **Client machine** — where DPM verifies and installs packages, including the
  package cache.
* **Public PKI** — certificate authorities, their roots, and timestamp
  authorities; external to DPM.

A package crosses every one of these boundaries between creation and use, which
is why integrity and authenticity must travel *with the package* rather than
being properties of any single channel.

---

## Adversaries

| ID | Adversary | Capability |
| --- | --------- | ---------- |
| NET | Network attacker | Intercepts or modifies network traffic. |
| MIRROR | Malicious mirror | Operates a compromised mirror, CDN, or intermediate package source. |
| REPO | Compromised repository | Controls a repository's infrastructure or operators. |
| ROGUE-REPO | Rogue repository | Operates their own DPM server instance. |
| AUTHOR | Malicious author | A malicious publisher, or an attacker who has compromised a publisher account. |
| KEY-THIEF | Key thief | Has obtained a legitimate signing private key. |
| LOCAL | Local process | Runs on the client machine with the user's own privileges. |
| SUPPLY | Supply-chain attacker | Can place or alter a package somewhere between author and client. |

---

## Assumptions

* **AS-1** The operating-system cryptographic stack (CryptoAPI / CNG) is
  correct.
* **AS-2** The DPM client binary, its configuration, and its built-in trust set
  are intact when DPM runs. A client machine that is already fully compromised
  is out of scope.
* **AS-3** The public code-signing PKI performs identity validation,
  key-protection enforcement, and revocation broadly as specified. DPM does not
  re-audit certificate authorities.
* **AS-4** TLS protects transport integrity and authenticates repository
  servers.
* **AS-5** Repository and publisher operators protect their private keys in
  hardware, per the CA/Browser Forum requirements in force since June 2023.
* **AS-6** The cryptographic primitives in the mandatory algorithm profile
  (SHA-256, RSA-2048+, ECDSA P-256+) are sound for the lifetime of a signature.
  Future weakening is addressed by algorithm agility, not by this model.

---

## Threat summary

| ID | Threat | Adversary | Asset | Status |
| --- | ------ | --------- | ----- | ------ |
| T-1 | Package content tampering | SUPPLY, MIRROR, NET | A-1 | Mitigated |
| T-2 | Unlisted-file injection | SUPPLY | A-1 | Mitigated |
| T-3 | Manifest substitution with attacker re-signing | SUPPLY, ROGUE-REPO | A-1, A-2 | Mitigated |
| T-4 | ZIP parser-differential / zip-slip variants | SUPPLY | A-1 | Mitigated |
| T-5 | Algorithm downgrade | SUPPLY | A-1, A-2 | Mitigated |
| T-6 | Signature lift to another package | SUPPLY | A-2 | Mitigated |
| T-7 | Downgrade to unsigned | MIRROR, SUPPLY | A-2, A-3 | Mitigated (TOFU limit) |
| T-8 | Signature stripping | SUPPLY | A-2, A-3 | Mitigated (TOFU limit) |
| T-9 | First-install trust bootstrapping | NET, MIRROR | A-3 | Mitigated |
| T-10 | Rogue repository serving malware | ROGUE-REPO | A-3 | Mitigated |
| T-11 | Confused multi-repository verification | ROGUE-REPO | A-3 | Mitigated |
| T-12 | Signing certificate expiry | — | A-2 | Mitigated |
| T-13 | Signing key compromise | KEY-THIEF | A-2, A-5 | Partial |
| T-14 | Repository key compromise | KEY-THIEF, REPO | A-3, A-5 | Partial |
| T-15 | Validly-signed malware from a genuine author | AUTHOR | A-1 | Out of scope |
| T-16 | Publisher account takeover | AUTHOR | A-2, A-6 | Partial |
| T-17 | Compromised repository substitutes packages | REPO | A-1, A-3 | Partial |
| T-18 | Dependency confusion / namespace squatting | AUTHOR, ROGUE-REPO | A-4 | Partial |
| T-19 | Rollback / replay of an old signed version | MIRROR | A-1 | Partial |
| T-20 | Package cache tampering | LOCAL | A-1, A-7 | Out of scope |
| T-21 | Verification receipt tampering | LOCAL | A-7 | Out of scope |
| T-22 | Trust-config or client-binary tampering | LOCAL | A-3, A-7 | Out of scope |
| T-23 | Hostile manifest parsing | SUPPLY | A-1 | Mitigated |
| T-24 | Timestamp authority compromise | KEY-THIEF | A-2 | Partial |
| T-25 | Author-signature substitution | SUPPLY | — | Not a threat |

---

## Threats in detail

### Package integrity

**T-1 — Package content tampering.** An attacker modifies a file inside a
`.dpkg` in transit or at rest. *Mitigation:* the signed manifest commits the
hash of every content file, and verification enforces file-set equality, so any
modified file fails its hash. *Residual:* none for detection, provided the
client requires a signature from a trusted party.

**T-2 — Unlisted-file injection.** An attacker adds a file not listed in the
manifest. *Mitigation:* file-set equality rejects any package containing a file
absent from `manifest.files` (other than the manifest and `signatures/`).
*Residual:* none. This is the failure mode that makes manifest signing
meaningful rather than cosmetic.

**T-3 — Manifest substitution with attacker re-signing.** An attacker replaces
the manifest with one describing malicious content and signs it with their own
key. *Mitigation:* trust is anchored on pinned SPKIs and built-in trusted
roots; a signature is honoured only if it is from a party the client trusts. A
"valid signature" is never sufficient. *Residual:* a client that has been
induced to trust the attacker's key is not protected — see T-10 and T-16.

**T-4 — ZIP parser-differential and zip-slip variants.** An attacker crafts an
archive with duplicate entries, case-colliding names, trailing dots or spaces,
alternate data streams, `..` traversal, or exotic compression, so that the
file extracted differs from the file verified. *Mitigation:* the archive-format
and path-safety rules reject all of these. *Residual:* depends on the
implementation applying the rules correctly — a conformance concern (V-9…V-13).

**T-5 — Algorithm downgrade.** An attacker signs or hashes with a weak
algorithm. *Mitigation:* the mandatory algorithm profile and the verifier
allowlist reject SHA-1, MD5, and undersized keys; the `hashAlgorithm` field is
inside the signed manifest and so cannot be downgraded without breaking the
signature. *Residual:* none while the profile is enforced.

**T-6 — Signature lift.** An attacker moves a valid signature onto a different
package. *Mitigation:* `packageId` and `version` are inside the signed
manifest, binding the signature to a specific identity. *Residual:* none.

**T-23 — Hostile manifest parsing.** An attacker supplies malformed JSON to
crash or exploit the verifier's parser. *Mitigation:* the manifest is parsed
with a hardened parser — duplicate-key rejection, depth and size limits, no
type-coercion surprises. *Residual:* implementation-quality dependent.

### Identity and trust

**T-7 — Downgrade to unsigned.** An attacker serves an unsigned build of a
package that is normally signed. *Mitigation:* the no-downgrade rule — the
repository dimension is a hard failure; the author dimension is governed by
`authorDowngradePolicy`, defaulting to prompt — and server-side downgrade
detection at publish time. *Residual:* a package never previously seen signed
has no history to ratchet against, so its first encounter is unprotected. This
is inherent to trust-on-first-use.

**T-8 — Signature stripping.** An attacker removes signature files.
*Mitigation:* validation modes and the no-downgrade rule. *Residual:* as T-7,
the first encounter of a package is unprotected.

**T-9 — First-install trust bootstrapping.** A fresh client has no prior basis
to trust the gallery. *Mitigation:* the client ships with a versioned built-in
trust set — repository SPKI pins and accepted roots. *Residual:* a tampered
client binary, which is out of scope (AS-2).

**T-10 — Rogue repository.** An attacker runs their own DPM server and signs
malware with it. *Mitigation:* trust is per-client; a mode requirement is
satisfied only by a signature from a repository in the client's trusted set, so
the rogue's signature is inert to clients that do not trust it. *Residual:* a
client that explicitly adds the rogue repository to its trusted set is, by
definition, trusting it — equivalent to T-16.

**T-11 — Confused multi-repository verification.** An attacker adds a rogue
repository signature alongside a genuine one on the same package.
*Mitigation:* the verifier honours only signatures from trusted repositories
and ignores untrusted extras; "a repository signature is present" is never
sufficient. *Residual:* none.

### Keys and certificates

**T-12 — Signing certificate expiry.** A signature becomes unverifiable once
its signing certificate expires. *Mitigation:* RFC3161 timestamping, mandatory
from Phase 1, keeps a signature verifiable past certificate expiry. *Residual:*
none for a timestamped signature.

**T-13 — Signing key compromise.** An attacker obtains a legitimate signing key
and signs malware. *Mitigation:* revocation checking (Phase 3), with
`keyCompromise` revocation invalidating signatures retroactively; mandatory
hardware key storage reduces the likelihood of theft. *Residual:* the window
between compromise and revocation; and, before Phase 3, no revocation checking
at all.

**T-14 — Repository key compromise.** An attacker obtains a repository signing
key. *Mitigation:* key rotation with overlapping keys, signed rollover
metadata, and a separate emergency-revocation path distributed through the
trust-set channel. *Residual:* the repository key is a high-value single point;
a compromise is severe until revocation propagates to clients.

**T-24 — Timestamp authority compromise.** An attacker forges or backdates
timestamps. *Mitigation:* the timestamp authority must chain to a trusted root
and the RFC3161 token is verified. *Residual:* a compromised but still-trusted
TSA could backdate; the likelihood is low and the exposure is shared with the
whole Authenticode ecosystem.

### Repository and ecosystem

**T-15 — Validly-signed malware from a genuine author.** A malicious author, or
an attacker fully in control of a genuine author's identity, publishes malware
that is correctly signed. *Mitigation:* essentially none from signing — this is
out of the design's scope. A signature proves origin, not benevolence. The
ecosystem-level responses are admission control, reputation, revocation, and
transparency logging (Phase 4) for after-the-fact detection. *Residual:*
significant, and explicitly accepted: this is a governance problem, not a
signing problem.

**T-16 — Publisher account takeover.** An attacker takes over a publisher
account, registers their own key, and publishes. *Mitigation:* 2FA on publisher
accounts; key registration and downgrade acknowledgement are re-authenticated,
logged, and notified; transparency logging (Phase 4) aids detection.
*Residual:* real — an attacker with full account control can publish.
The mitigations make the attack noisy and attributable, not impossible.

**T-17 — Compromised repository substitutes packages.** A repository's
infrastructure is compromised and serves altered packages. *Mitigation:* an
independent author signature, under `author-and-repository` mode, means a
repository compromise alone cannot forge a package without also breaking the
author signature; transparency logging aids detection. *Residual:* if the
repository signing key is also compromised, and the package was never
author-signed, the repository can substitute freely for clients in
`repository-required` mode.

**T-18 — Dependency confusion / namespace squatting.** An attacker publishes a
package under a name that shadows or mimics a legitimate one. *Mitigation:*
namespace ownership through publisher accounts; the repository attestation
binds a package to its namespace; per-client source trust. *Residual:*
cross-repository confusion and source-to-package pinning are only partially
addressed; further hardening is noted as future work.

**T-19 — Rollback / replay.** An attacker serves an old, validly signed, but
vulnerable version of a package. *Mitigation:* the no-downgrade rule resists
*signing-status* rollback; it does not resist *version* rollback. *Residual:*
real — version-level rollback resistance (publish timestamps, repository serial
numbers) is noted as future work and is not provided in the phases specified.

### Client and local

**T-20 — Package cache tampering.** A local process modifies an extracted file
in the package cache after it was verified. *Mitigation:* limited by design —
the cache is a per-user, ACL-protected directory; the verification receipt
detects accidental corruption and trust-policy changes, not a same-privilege
attacker. *Residual:* explicitly accepted and out of scope.

**T-21 — Verification receipt tampering.** A local process forges a cache
verification receipt. *Mitigation:* as T-20. DPAPI protection was considered
and rejected, because it provides no protection against a process running as
the same user. *Residual:* accepted, out of scope.

**T-22 — Trust-config or client-binary tampering.** A local process modifies
`dpm.config` or the `dpm` binary itself. *Mitigation:* none — out of scope
(AS-2). *Residual:* total, but the precondition is a machine the attacker
already controls.

### Analysed and dismissed

**T-25 — Author-signature substitution.** An attacker swaps one valid author
signature for another valid signature by the *same registered key* over the
*same manifest*. *Analysis:* this delivers identical content from the identical
author; no security-relevant property changes. It is not a threat. The optional
`dpmVerifiedAuthorSignatureHash` attribute addresses only audit precision, not
a vulnerability.

---

## Residual risk summary

The following are explicitly accepted as out of scope or only partially
addressed, and should be the focus of review attention:

* A fully compromised client machine, or a same-user-privilege local attacker
  (T-20, T-21, T-22).
* Validly-signed malware from a genuine, uncompromised author (T-15).
* Vulnerabilities in package *content* itself — DPM signs code, it does not
  audit it.
* Version-level rollback resistance, which is incomplete before the
  supply-chain features of Phase 4 (T-19).
* Revocation checking, which is absent until Phase 3, leaving the
  compromise-to-revocation window unaddressed before then (T-13).
* The repository signing key as a high-value single point of failure (T-14,
  T-17).
* Availability and denial-of-service against the gallery, which this model does
  not cover.
* A break in an underlying cryptographic primitive, addressed only by algorithm
  agility over time.

---

## Notes for the reviewer

Particular attention is invited on:

1. The trust-on-first-use gap (T-7, T-8): whether the no-downgrade rule's
   first-encounter exposure is acceptable, and whether the `permissive` default
   during the ecosystem transition is the right call.
2. Publisher account compromise (T-16): whether the sensitive-action controls
   (2FA, re-authentication, logging, notification) are sufficient, and what
   else the server side should enforce.
3. The repository key as a single point of failure (T-14, T-17): whether the
   rotation and emergency-revocation design propagates fast enough.
4. The multi-repository trust model (T-10, T-11): whether "satisfied only by a
   trusted signature" is stated and enforced consistently everywhere.
5. The honesty of the out-of-scope list: whether anything on it should instead
   be brought into scope.