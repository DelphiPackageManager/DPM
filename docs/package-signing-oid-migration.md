# DPM Signing — OID Migration Plan

This document describes the **dual-OID** strategy DPM uses to migrate from
its interim private OID arc to a permanent IANA-assigned arc without
breaking any package already in the wild. It's intended as a high-level
prompt for the future engineering work; the cross-references to the main
[package-signing.md](package-signing.md) architecture doc carry the
detailed rules.

---

## Why we need a migration plan at all

Every CMS signature embeds its attribute OIDs literally in the signed
bytes. Once a `.dpkg` ships with `dpmSignatureRole = 1.3.6.1.4.1.95860.1`,
that OID is permanent for that package — drop the OID from the verifier
and the package fails to verify.

DPM is shipping its first signing release under the **interim arc
`1.3.6.1.4.1.95860`**. IANA's assignment of VSoft's own Private Enterprise
Number is pending. When the IANA-assigned arc lands, we cannot simply
"switch over" — every package signed under the interim arc must keep
verifying forever.

---

## The strategy

**Dual-emit, always-accept.**

* **Signer (client `dpm sign` + server publish path).** Once the canonical
  arc is known, signers emit *both* arcs as signed attributes on every new
  signature — `1.3.6.1.4.1.95860.x` (legacy) and `1.3.6.1.4.1.<PEN>.x`
  (canonical). The attribute values are byte-identical between the two —
  same role string, same attestation payload, same hash.

* **Verifier (client + server).** Verifiers accept either arc. When
  walking the signer info's signed-attribute set, the lookup for "the
  signature role" matches an OID set `{legacy, canonical}` rather than a
  single OID; same for attestation and verified-author-hash.

* **Receipts / transparency log / SBOM rendering.** Display the canonical
  arc when present, fall back to the legacy arc when not. The receipt
  records which one was actually parsed so a forensic audit can tell.

* **No flag day.** Old packages keep verifying via the legacy arc
  indefinitely. New packages carry both so non-DPM verifiers (e.g.
  generic CMS dumpers, Authenticode-style readers, future tooling) can
  see the canonical IANA-anchored arc.

* **Eventual stop-emit (optional, far future).** Once the legacy arc
  has been deprecated for, say, 5+ years AND telemetry shows new packages
  are all dual-arc'd, we *may* stop emitting the legacy arc on new
  signatures. We **never** stop accepting it on verify — old packages live
  forever.

---

## Client (Delphi / DPM CLI + IDE)

Touch points in [DPM.Core.Package.Signing.pas](../Source/Core/Package/Signing/DPM.Core.Package.Signing.pas):

* **Constants.** Add `cDpmOidArcCanonical : AnsiString = '1.3.6.1.4.1.<PEN>';`
  alongside the existing interim `cDpmOidArc`. Derive
  `cOidDpmSignatureRoleCanonical`, `cOidDpmRepositoryAttestationCanonical`,
  `cOidDpmVerifiedAuthorSigHashCanonical` from it.

* **Sign path (`SignPackage` / `SignRemote`).** Where the signed-attributes
  array is built, append a second copy of each DPM attribute under the
  canonical OID with the same value bytes. The CMS encoder sorts and SET-
  wraps automatically; nothing else changes.

* **Verify path (`VerifyOneSignature`, `ReadRepositoryAttestation`).** Replace
  the single-OID lookups with helpers that check the legacy OID, then the
  canonical OID:
  ```pascal
  function FindDpmSignedAttribute(const cms : ICmsSignedData;
      const legacyOid, canonicalOid : AnsiString;
      out value : TBytes) : boolean;
  ```
  Use this for role, attestation, and verified-author-hash.

* **Verify output / JSON output.** Render the canonical OID when found, the
  legacy OID when only the legacy is present. The receipt's signature
  block can add an `oidArc` field with values `'legacy' | 'canonical' | 'both'`.

* **Tests.** Add a fixture pair: one `.dpkg` signed under legacy-only
  (representing the corpus of packages already shipped), one signed under
  legacy+canonical. Both must verify clean under the new code. Bonus: a
  canonical-only fixture should also verify, anticipating a far-future
  stop-emit world.

---

## Server (Node / Bun / TanStack Start)

The gallery server signs packages with the repository key and verifies
author signatures on upload. It needs the same dual-OID handling.

Touch points (rough — paths to be filled in when wiring):

* **OID constants module.** Single source of truth for both arcs; consumed
  by signer + verifier + attestation reader + transparency log writer.

  ```ts
  export const DPM_OID_LEGACY_ARC    = '1.3.6.1.4.1.95860';
  export const DPM_OID_CANONICAL_ARC = '1.3.6.1.4.1.<PEN>';

  export const dpmAttrOids = {
    signatureRole:        ['1.3.6.1.4.1.95860.1', '1.3.6.1.4.1.<PEN>.1'],
    repositoryAttestation:['1.3.6.1.4.1.95860.2', '1.3.6.1.4.1.<PEN>.2'],
    verifiedAuthorSigHash:['1.3.6.1.4.1.95860.3', '1.3.6.1.4.1.<PEN>.3'],
  } as const;
  ```

* **Repository signer (publish path).** When building the
  `dpmRepositoryAttestation` signed attribute, write it twice — once under
  each OID, same encoded value. Use the project's CMS library
  (`@peculiar/asn1-schema` + `@peculiar/cms` or `node-forge`, whichever
  the server has settled on).

* **Author-signature verifier (upload path).** When parsing the uploaded
  package's author signature to read `dpmSignatureRole` and decide that
  it really is an author signature, match against the OID set, not a
  single OID.

* **Re-ingest path.** Same dual-emit when adding the gallery's own
  repository signature to a package that originated elsewhere.

* **API responses.** When the server returns signing metadata to clients
  (e.g. a package details endpoint that exposes the attestation
  namespace), render the canonical arc when present.

* **Database / event log.** Any persisted record of "this package was
  signed with attribute X" should record both OIDs when both are present,
  with an explicit `oidArcSource: 'legacy' | 'canonical' | 'both'`
  discriminator for audit queries.

---

## Sequencing

1. **Now (Phase 1/2 release).** Ship with the interim arc
   `1.3.6.1.4.1.95860` everywhere. Client + server only know one arc.
   This is the current state.

2. **When IANA assigns the PEN.**
   1. Define the canonical arc and constants in **both** client and server.
   2. Land the dual-accept change in the verifier first — client + server —
      and ship it for several weeks before any signer starts emitting the
      canonical arc. (This way, when canonical-arc'd packages start
      appearing, every consumer is already prepared.)
   3. Land the dual-emit change in the signer — client `dpm sign` + server
      publish path. Both signers emit both arcs from this point on.
   4. Bump the trust set's `dpmTrustSetVersion` and rebuild — receipts
      pinned to the old fingerprint re-verify and pick up the new
      canonical OID. The fingerprint covers the trust-set version, not
      the OID directly, so this is the natural cache invalidation
      trigger.

3. **Forever after.** Legacy arc continues to be accepted. Canonical arc
   is the displayed identity. Old packages keep verifying. New packages
   carry both.

4. **Optional, 5+ years out.** Once dual-emit corpus is overwhelming and
   any new tooling expects the canonical arc, signers may stop emitting
   the legacy arc on new signatures. Verifiers **never** stop accepting
   it.

---

## Things that intentionally do NOT change

* **The interim arc constants stay in the codebase.** Never remove them.
  They're load-bearing for every package signed before the canonical
  arc was added.

* **The on-disk `.dpkg` layout, `dpm-manifest.json` schema, and trust
  set YAML format don't change.** This is purely a CMS signed-attribute
  identity change.

* **Trust pinning (publisher / repository SPKI hashes) doesn't change.**
  Trust still flows through the cert key, not the attribute OID.

---

## Open questions for the future

* **Single attribute carrying both OID values vs. two separate attributes
  with the same value?** Current plan above is two separate attributes
  (simpler, no schema invention). If a CMS spec we care about gains an
  "alias OID" container we could collapse. Likely not worth chasing.

* **Cache fingerprint sensitivity.** The policy fingerprint includes the
  trust-set version; should it also include an "OID set generation"? Only
  if we ever expect to flip dual-accept off, which we shouldn't.

* **Cross-tool interop.** If anyone else implements DPM verification
  (third-party security scanners, etc.), they'll consume whichever arc
  we've documented as canonical. The legacy arc is for backward
  compatibility, not for re-implementation. The architecture doc should
  state the canonical arc once it exists; the legacy arc should remain
  documented only in this migration doc.
