# `dpm scan` — Vulnerability Scanning

Takes an SBOM (or a `.dproj` / `.groupproj`) and produces a vulnerability report by querying the [Open Source Vulnerabilities (OSV)](https://osv.dev) database. Designed for CI gating: pair with `-fail-on=high` to make the build red when something serious is found.

## Table of Contents

- [Quick Start](#quick-start)
- [What `dpm scan` Does](#what-dpm-scan-does)
- [Terminology Primer](#terminology-primer)
- [Command Syntax](#command-syntax)
- [Options](#options)
- [Examples](#examples)
- [Output Format (CycloneDX VEX)](#output-format-cyclonedx-vex)
- [Caching](#caching)
- [CI Gating with `-fail-on`](#ci-gating-with--fail-on)
- [Exit Codes](#exit-codes)
- [How It Works](#how-it-works)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
# Scan an existing SBOM
dpm scan MyProject-Win64.cdx.json

# Or skip the intermediate file: generate the SBOM then scan in one shot
dpm scan MyProject.dproj
```

Either way produces `MyProject-<platform>.vex.json` next to the input — a CycloneDX 1.5 VEX (Vulnerability Exploitability eXchange) document carrying every found vulnerability and which components are affected.

```bash
# Gate a CI build on findings: exit 1 if anything High or Critical is reported
dpm scan MyProject.dproj -fail-on=high
```

---

## What `dpm scan` Does

For each component in the SBOM with a `purl` (package URL):

1. Look it up in OSV — a free public REST API at `https://api.osv.dev` aggregating GHSA, NVD, ecosystem-specific feeds (Python, Rust, Go, npm, etc.) into a uniform schema.
2. For each matching vulnerability, fetch the full advisory (severity, fixed-in version, references).
3. Record every (component, vulnerability) pair.
4. Write a CycloneDX VEX file with `vulnerabilities[].affects[].ref` pointing back into the SBOM's `components[].bom-ref`.
5. Optionally fail with exit code 1 if the worst severity found meets the `-fail-on` threshold.

OSV responses are cached on disk for 24 hours, so repeat scans of the same SBOM during a CI run don't re-hit the network.

---

## Terminology Primer

If you're new to vulnerability scanning, these are the terms you'll see in the output.

| Term | What it is | Why it matters |
|---|---|---|
| **CVE** (Common Vulnerabilities and Exposures) | Global ID for a published security hole — e.g. `CVE-2021-44228` (Log4Shell). Maintained by MITRE. | This is the ID the report cites for each finding. |
| **CVSS** (Common Vulnerability Scoring System) | 0.0–10.0 score on a CVE. ≥9.0 = critical, 7.0–8.9 = high, 4.0–6.9 = medium, ≤3.9 = low. | Drives `-fail-on`. |
| **GHSA** (GitHub Security Advisory) | GitHub's structured advisory database. IDs look like `GHSA-jfh8-c2jp-5v3q`. Carries *exactly which versions* are affected and fixed. | Most GHSAs have a matching CVE. |
| **OSV** (Open Source Vulnerabilities) | Free public REST API at `https://api.osv.dev` run by Google. Aggregates GHSA + many ecosystem-specific feeds. | The single source `dpm scan` queries in v1. |
| **purl** (Package URL) | The identifier the SBOM already carries: `pkg:generic/dpm/VSoft.SemanticVersion@0.2.7`. | OSV understands purls — that's how it knows which package + version you mean. |
| **VEX** (Vulnerability Exploitability eXchange) | Output format for "here are the vulns in this SBOM". DPM emits CycloneDX 1.5 VEX in v1. | Standard format → CI dashboards, SCA tools, audit pipelines can ingest the output automatically. |
| **SCA** (Software Composition Analysis) | The umbrella term for "scan my dependencies for vulns." | `dpm scan` is a small Delphi-aware SCA tool. |

---

## Command Syntax

```
dpm scan <sbom-or-project> [options]
```

`<sbom-or-project>` can be:

- A CycloneDX `.json` (typically `.cdx.json` from `dpm sbom`, or any other CycloneDX 1.5 SBOM)
- A `.dproj` — DPM runs the SBOM generator in a temp folder, then scans each produced platform SBOM
- A `.groupproj` — same, with aggregated cross-project SBOMs

---

## Options

| Option | Default | Description |
|---|---|---|
| `-output=<path>` | `<input>.vex.json` next to input (file input) or project folder (project input) | Output file (for SBOM input) or directory (for project input). |
| `-source=osv` | `osv` | Vulnerability database. Only `osv` is supported in v1. |
| `-fail-on=<sev>` | `none` | Exit 1 if any vuln of this severity or higher is found. Accepted: `none` \| `low` \| `medium` \| `high` \| `critical`. |
| `-no-cache` | (cache on) | Bypass the 24h response cache for reads. Still writes fresh responses, so the next run repopulates the cache. |
| `-platforms=<csv>` | all enabled | Only used when input is a `.dproj` / `.groupproj`. Passed through to the SBOM generator. |
| `-config=<file>` | default DPM config | DPM config file path. Same semantics as other commands. |

---

## Examples

```bash
# Scan an existing CycloneDX SBOM, write next to it
dpm scan MyProject-Win64.cdx.json

# Same but force fresh OSV queries (skip the cache)
dpm scan MyProject-Win64.cdx.json -no-cache

# Generate + scan a project in one shot - produces one .vex.json per platform
dpm scan MyProject.dproj

# Limit to specific platforms when scanning from a project
dpm scan MyProject.dproj -platforms=Win32,Win64

# CI gating: succeed quietly unless we find High or Critical
dpm scan MyProject.dproj -fail-on=high

# CI gating: only fail on Critical (typical for "phase-in" rollouts)
dpm scan MyProject.dproj -fail-on=critical

# Scan an entire solution, write all per-platform .vex.json files to c:\reports
dpm scan MySolution.groupproj -output=c:\reports
```

---

## Output Format (CycloneDX VEX)

The output is a CycloneDX 1.5 JSON document whose `vulnerabilities[]` array holds every finding. Each entry carries:

| Field | What it is |
|---|---|
| `id` | The canonical advisory id (e.g. `GHSA-jfh8-c2jp-5v3q`, `CVE-2021-44228`). |
| `source.name` / `source.url` | Where the advisory was sourced from. |
| `description` | Short summary. |
| `detail` | Longer prose description, if the advisory has one. |
| `ratings[]` | One entry with `severity` (`none`/`low`/`medium`/`high`/`critical`/`unknown`), `score` (CVSS 0.0–10.0), `method` (`CVSSv3` / `CVSSv2`), `vector` (the CVSS vector string). |
| `published` / `updated` | ISO-8601 timestamps. |
| `advisories[]` | External advisory URLs. |
| `references[]` | Alias ids (e.g. CVE for a GHSA-primary record, or vice versa). |
| `affects[]` | One entry per affected source-SBOM component. `ref` is the source SBOM's `bom-ref`, which lets downstream tooling cross-reference the SBOM and the VEX without ambiguity. |

The metadata section also carries `sourceSbomSerialNumber` so a tool can match the VEX back to the SBOM it scanned.

### Example slice

```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.5",
  "version": 1,
  "metadata": {
    "timestamp": "2026-05-20T10:00:00Z",
    "tools": [{"vendor": "DPM", "name": "dpm-scan", "version": "1.0.0"}],
    "component": {"type": "application", "bom-ref": "app-1", "name": "MyApp"}
  },
  "vulnerabilities": [
    {
      "id": "GHSA-jfh8-c2jp-5v3q",
      "source": {
        "name": "GitHub Advisory Database",
        "url": "https://github.com/advisories/GHSA-jfh8-c2jp-5v3q"
      },
      "description": "Apache Log4j2 JNDI features do not protect against attacker controlled LDAP and other JNDI related endpoints.",
      "ratings": [{
        "source": {"name": "GitHub Advisory Database"},
        "score": 10.0,
        "severity": "critical",
        "method": "CVSSv3",
        "vector": "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:C/C:H/I:H/A:H"
      }],
      "affects": [{"ref": "pkg-log4j-core-2.14.0"}]
    }
  ]
}
```

---

## Caching

OSV responses are cached on disk at:

```
%APPDATA%\.dpm\vuln-cache\<source>\<sha256-of-purl>.json
```

Cache entries carry a `cachedAt` timestamp. Entries older than 24 hours are treated as a miss and re-fetched.

Two layers are cached:

1. **Per-purl batch lookup** — the list of vulnerability IDs OSV returns for one purl.
2. **Per-vulnerability detail** — the full advisory record for one ID. Cached more aggressively because advisory details rarely change after publication.

Pass `-no-cache` to force fresh reads for this run. Even with `-no-cache`, fresh responses are still **written** to the cache — the flag only suppresses reads.

To wipe the cache entirely, delete `%APPDATA%\.dpm\vuln-cache\`.

---

## CI Gating with `-fail-on`

The default `-fail-on=none` means `dpm scan` always exits 0, regardless of what it finds. This matches `npm audit` / `pip-audit` default behaviour: a casual local scan should never surprise you with a non-zero exit.

For CI:

```yaml
- run: dpm scan MyProject.dproj -fail-on=high
```

The threshold is inclusive — `-fail-on=high` fails on both High AND Critical findings.

Severities:

- `none` (default) — never fail
- `low` — fail on Low, Medium, High, Critical
- `medium` — fail on Medium, High, Critical
- `high` — fail on High, Critical *(typical CI choice)*
- `critical` — fail on Critical only

`Unknown` severity (advisory exists but no CVSS / textual severity rating) never trips the gate — there's no actionable threshold to compare against, so failing on it would be noisy. The vuln is still listed in the report; you just won't fail CI on it.

---

## Exit Codes

| Code | Meaning |
|---|---|
| 0 | OK (no vulns, or vulns found but below `-fail-on` threshold). |
| 1 | A vuln of severity `>= -fail-on` was found, or a write/network error occurred. |
| 101 | Invalid arguments (missing input file, unknown source, etc.). |

For the full list of DPM exit codes, run `dpm exitcodes`.

---

## How It Works

```
your .dproj  ──► dpm sbom  ──► SBOM (TSBOMReport: list of components with purls)
                                         │
                                         ▼
                                  dpm scan
                                         │
                            ┌────────────┴────────────┐
                            ▼                         ▼
                       OSV REST API             local cache
                       (POST batch query)       %APPDATA%\.dpm\vuln-cache\
                                                (24h TTL, keyed by SHA(purl))
                            │                         │
                            └────────────┬────────────┘
                                         ▼
                                  TVulnReport
                                         │
                                         ▼
                          CycloneDX VEX JSON file
                                         │
                                         ▼
                       exit 0  /  exit 1 (if -fail-on tripped)
```

- **Project input** runs the SBOM generator into a temp folder (`%TEMP%\dpm-scan-XXXXXXXX\`), then scans each generated `.cdx.json` and cleans up.
- **SBOM file input** parses the CycloneDX 1.5 JSON directly.
- The OSV client uses `POST /v1/querybatch` to discover vulnerable IDs (one request per scan), then `POST /v1/vulns/<id>` for each unique ID to fetch full details. Both layers go through the disk cache.
- Same advisory reported against multiple components (common — one CVE often hits many package versions) collapses into ONE `vulnerabilities[]` entry with multiple `affects[]` rows.

---

## Troubleshooting

**Zero vulnerabilities even though I expect findings**
Check the SBOM has `purl` entries on the components you care about — open it in a JSON viewer and look for `components[].purl`. No purl → nothing to look up. Most-commonly missing for "third-party" units discovered via the MAP file with no DPM provenance. Generate a fresh SBOM with `dpm sbom -strict` to see warnings about unattributed units.

**Network errors**
`dpm scan` requires network access to `api.osv.dev`. If you're behind a proxy, set `HTTP_PROXY` / `HTTPS_PROXY` env vars; `VSoft.HttpClient` honours them. Cached entries within 24h will still work offline.

**"Unknown" severity in the output**
The advisory exists in OSV but no CVSS score and no GHSA-style textual severity could be derived. The vuln is listed but doesn't trip `-fail-on`. Click through the advisory URL to make a manual judgement.

**`-fail-on=high` doesn't fail on Critical**
It does — the threshold is inclusive *and* uses ordinal comparison. `high < critical`, so a Critical finding triggers `-fail-on=high` too. If you're seeing exit 0 despite a Critical, check the actual severity in the VEX (`vulnerabilities[].ratings[0].severity`) — it may be `unknown`.

**Scanning a project but only one platform's results appear**
Each platform produces its own `.vex.json` file. Look in the project folder (or your `-output=` directory) for `<projectName>-<platform>.vex.json` per platform. The CLI prints the file path for each scan.

**Cache appears stale**
24h TTL is the default. Use `-no-cache` to force fresh fetches for this run, or delete `%APPDATA%\.dpm\vuln-cache\` to wipe it entirely.

---

## What's *Not* Yet Supported

These are designed-in extension points; v1 doesn't ship them.

- **GHSA-direct / NVD / Sonatype** sources beyond what OSV aggregates. `IVulnDatabase` is the extension point.
- **OpenVEX 0.2** output format. `IVulnWriter` is the extension point.
- **Reachability analysis** — "is this vulnerable function actually called from my code." Needs static analysis, separate project.
- **Auto-remediation** — "upgrade these packages to fix N vulns." Needs solver integration with the dependency resolver.
- **VEX suppression input** — telling `scan` "we've reviewed CVE-X and it doesn't apply to us." Useful but adds a workflow we don't need in v1.

---

## See Also

- [`dpm sbom`](sbom-command.md) — generating the SBOM input
- [CycloneDX 1.5 Vulnerabilities spec](https://cyclonedx.org/docs/1.5/json/#vulnerabilities)
- [OSV API reference](https://google.github.io/osv.dev/api/)
- [CVSS v3.1 specification](https://www.first.org/cvss/v3.1/specification-document)
