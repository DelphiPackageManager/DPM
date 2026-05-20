# `dpm sbom` — Software Bill of Materials

Generates an SBOM (Software Bill of Materials) for a Delphi project. An SBOM is a structured inventory of every third-party library, runtime component, and unidentified unit that ends up inside the built binary. It is the input to vulnerability scanning, licence audits, and supply-chain attestations.

## Table of Contents

- [Quick Start](#quick-start)
- [What's in an SBOM](#whats-in-an-sbom)
- [Output Formats](#output-formats)
- [Command Syntax](#command-syntax)
- [Options](#options)
- [Examples](#examples)
- [MAP Files & Evidence Attribution](#map-files--evidence-attribution)
- [Component Classification](#component-classification)
- [`.groupproj` Aggregation](#groupproj-aggregation)
- [IDE Integration](#ide-integration)
- [Output Filenames](#output-filenames)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
dpm sbom MyProject.dproj
```

Produces `MyProject-<platform>.cdx.json` (CycloneDX 1.5) and `MyProject-<platform>.spdx.json` (SPDX 2.3) next to the project, one of each per platform enabled in the project.

For a richer audit-friendly view:

```bash
dpm sbom MyProject.dproj -format=all
```

Also produces HTML and Markdown reports.

---

## What's in an SBOM

Each SBOM lists components grouped by *kind*:

| Kind | What it covers |
|---|---|
| **Application** | The project being analysed itself (one entry, the root component). |
| **DPM package** | Anything installed via DPM (`dpm install`). Carries the package id, version, hash, source repo metadata, and a `pkg:generic/dpm/<id>@<version>` purl. |
| **Delphi runtime** | The RTL/VCL/FMX framework. One entry per platform. Version comes from `dcc32.exe`'s file version (e.g. `29.0.55362.2017` for Delphi 12.x). |
| **Third-party** | Discovered via the linker MAP file — units linked into the binary that aren't DPM packages and don't live under the Delphi installation. Keyed by the parent folder of the discovered source/dcu file. |
| **Unidentified** | A unit appeared in the MAP file but DPM couldn't classify it. Usually means the MAP file lacks a source path *and* the search index couldn't locate the unit. |

Every component carries — where available — name, version, licence, supplier, SHA-256 hash, repository URL, and an `evidence.occurrences[]` array showing the on-disk files that placed it in the binary.

---

## Output Formats

Four writers are available; pick any combination via `-format=`.

| Format | `-format=` token | Extension | Purpose |
|---|---|---|---|
| **CycloneDX 1.5 JSON** (default) | `cyclonedx` or `cdx` | `.cdx.json` | Industry-standard, supported by most SCA tooling (Grype, Trivy, Dependency-Track). Carries full evidence and dependency graph. Used as input to `dpm scan`. |
| **SPDX 2.3 JSON** (default) | `spdx` | `.spdx.json` | Industry-standard, often required for legal / licence-compliance audits. |
| **HTML** | `html` or `htm` | `.html` | Self-contained dark-themed report. Open in a browser. |
| **Markdown** | `markdown` or `md` | `.md` | GitHub-flavoured markdown report. Preview in VS Code or render with `gh`. |

Multi-format syntax: `-format=cyclonedx,html` (comma-separated; whitespace is tolerated; tokens are case-insensitive).

Aliases:

| Alias | Expands to |
|---|---|
| `both` | `cyclonedx,spdx` (the default) |
| `json` | `cyclonedx,spdx` (synonym for `both`) |
| `all` | every format |

Default when `-format=` is omitted: `cyclonedx,spdx`.

---

## Command Syntax

```
dpm sbom <project> [options]
```

`<project>` is a `.dproj`, `.groupproj`, or a directory containing exactly one of either. Defaults to the current directory.

---

## Options

| Option | Alias | Default | Description |
|---|---|---|---|
| `-outdir=<dir>` | `-o` | project's directory | Output directory for SBOM files. |
| `-format=<csv>` | `-f` | `cyclonedx,spdx` | Comma-separated formats: `cyclonedx` (`cdx`) \| `spdx` \| `html` (`htm`) \| `markdown` (`md`). Aliases: `both`, `all`. |
| `-platforms=<csv>` | `-p` | all enabled in project | Comma-separated DPM platform names: `Win32`, `Win64`, `OSX64`, `Linux64`, etc. |
| `-config=<name>` | `-c` | `Release`, else `Debug`, else first | Build configuration to use for locating the MAP file. |
| `-map=<path>` | `-m` | auto-detected from config | Override MAP file path. Single-platform invocations only. |
| `-no-runtime` | | (off, runtime is included) | Exclude the Delphi RTL/VCL/FMX component from the SBOM. |
| `-strict` | | (off, warn on missing) | Fail with non-zero exit code if a MAP file is missing. Default: warn and emit a partial SBOM (packages + runtime only). |
| `-per-project` | | (off, aggregated) | When the input is a `.groupproj`, emit one SBOM per `.dproj` per platform (legacy CI workflow). Default: one aggregated SBOM per platform spanning the whole group. |

---

## Examples

```bash
# Default behaviour - CycloneDX + SPDX next to the project, all enabled platforms
dpm sbom .\MyProject.dproj

# Pick a different output directory and just CycloneDX
dpm sbom .\MyProject.dproj -outdir=c:\reports -format=cyclonedx

# Add HTML + Markdown reports for human consumption
dpm sbom .\MyProject.dproj -format=html,markdown

# Everything
dpm sbom .\MyProject.dproj -format=all

# Specific platforms + a specific build config
dpm sbom .\MyProject.dproj -platforms=Win32,Win64 -config=Release

# Aggregate a whole solution into one SBOM per platform
dpm sbom .\MySolution.groupproj

# Legacy per-dproj output (e.g. for a per-binary attestation pipeline)
dpm sbom .\MySolution.groupproj -per-project

# Fail the build if the MAP file is missing (strict CI mode)
dpm sbom .\MyProject.dproj -strict
```

---

## MAP Files & Evidence Attribution

The Delphi linker's MAP file is what tells DPM *which units actually ended up in the binary*. Without it, an SBOM can only list "every package the project depends on" — useful, but doesn't tell you which non-DPM code was actually linked in.

### Enabling MAP file generation

In each build configuration you intend to scan:

- **Project Options → Linking → Map file = Detailed**

This produces both the segment summary and the `Line numbers for ...` sections DPM needs. If the option is set to `Off` or `Publics`, DPM warns and emits a packages-only SBOM.

### Where DPM looks

MAP file path is auto-detected from the project's `DCC_MapFile` and the active config's output folder. Override with `-map=<path>` for single-platform runs (e.g. when scanning a CI artefact in a non-default location).

### Tiered unit resolution

For each unit named in the MAP file, DPM applies these checks in order:

1. **Source path in MAP** — if the linker recorded the source file path, use it. File under the Delphi install dir → runtime; under a DPM package's cache folder → that package; under the project folder (or DCCReference) → project source (skipped); elsewhere → third-party.
2. **Per-package unit-ownership map** — DPM enumerates every `.pas` (source mode) or `.dcu` (compile-on-install mode) shipped by each DPM package and builds a `unitName → bom-ref` map. Unattributed MAP entries are looked up in this map.
3. **Embarcadero namespace prefix** — units beginning with `System.`, `Vcl.`, `Fmx.`, `Winapi.`, `Data.`, `Datasnap.`, `Soap.`, `Web.`, `Xml.`, `Bde.`, `IBX.`, `REST.`, `Posix.`, `Macapi.`, `IOSapi.`, `Androidapi.`, `Bluetooth.` plus the bare names `System` / `SysInit`. Prefix alone is not enough — DPM also checks that the resolved file lives under the Delphi RootDir (a third-party can legitimately ship a unit named `System.MyStuff`).
4. **Search-path index** — DPM scans the project's `DCC_UnitSearchPath` plus the IDE's registry Library Path (`HKCU\Software\Embarcadero\BDS\<ver>\Library\<Platform>\Search Path`) and indexes file locations. A MAP-named unit found via the index is attributed to a third-party component keyed by its parent folder.

The index is **resolver-only**, not an enumerator — DPM never walks it to populate the SBOM. The SBOM only ever contains units the MAP file proves were linked.

---

## Component Classification

The classification rules in plain English:

- **DPM package**: ships through `dpm install`, lives under `%APPDATA%\.dpm\packages\…`, carries a purl.
- **Delphi runtime**: a single component (`type: framework` in CycloneDX) per platform, identified by `dcc32.exe`'s file version. Aggregates all RTL/VCL/FMX units rather than listing them individually.
- **Third-party**: a non-DPM, non-runtime unit found by the search index. Grouped by parent folder — if you have ten units in `c:\libs\jcl\`, they collapse into one "jcl" component with ten evidence occurrences.
- **Unidentified**: in the MAP file but couldn't be placed. Usually a sign of a custom unit search path the index didn't see, or a unit whose source file moved after the build. Investigate by checking the component's `evidence.occurrences[]`.

---

## `.groupproj` Aggregation

When the input is a `.groupproj`, the default is to emit **one aggregated SBOM per platform** spanning every `.dproj` in the group:

- Root component = the group (id = `.groupproj` filename).
- Each contained `.dproj` becomes an `application`-typed sub-component.
- DPM packages / runtime / third-party / unidentified components are unioned across all dprojs and deduplicated by `(id, version)`.
- Dependency edges show `group → dprojApp → packages`.

For CI pipelines that need a per-binary attestation, pass `-per-project` to revert to legacy behaviour (one SBOM per dproj per platform).

---

## IDE Integration

Right-click a `.dproj` or `.groupproj` in the Delphi Project Manager and pick **Generate SBOM…**. The IDE plugin wraps the same generator and writes the output to the project folder. Available in every supported IDE version (XE2 through 13).

When right-clicking a `.groupproj`, the menu reads "Generate SBOM… for Project Group" and produces the aggregated output; right-clicking a `.dproj` produces per-project output.

---

## Output Filenames

- Single `.dproj`: `<projectName>-<platform>.<ext>`
- `.groupproj` (default aggregated): `<groupName>-<platform>.<ext>`
- `.groupproj` with `-per-project`: `<projectName>-<platform>.<ext>` per dproj

Examples for a project called `MyApp.dproj` enabled for Win32 + Win64:

```
MyApp-Win32.cdx.json
MyApp-Win32.spdx.json
MyApp-Win64.cdx.json
MyApp-Win64.spdx.json
```

---

## Troubleshooting

**"MAP file not found at …"**
Set Project Options → Linking → Map file = Detailed for the build config you're scanning. Pass `-config=Release` (or whichever config you actually build with) so DPM looks in the right output folder.

**Components landing in "unidentified"**
The MAP file references a unit DPM can't place. Build with `verbosity=Detailed` to see which units are missing, then either:
- Add the unit's source folder to the project's `DCC_UnitSearchPath`, or
- Confirm the unit ships with a DPM package that has been installed (run `dpm restore`), or
- Check the IDE Library Path registry key contains the unit's folder.

**Wrong runtime version reported**
DPM reads `dcc32.exe`'s file version. If the version looks stale, the Delphi installation registry entry may point at a different `dcc32.exe` than the one you're using. Check `HKCU\Software\Embarcadero\BDS\<ver>\RootDir`.

**Aggregated `.groupproj` SBOM looks wrong**
Deduplication is by `(id, version)`. If two dprojs in the group reference *different versions* of the same package, both appear as separate components. That's intentional — it surfaces real version drift.

---

## See Also

- [`dpm scan`](scan-command.md) — vulnerability scanning of an SBOM
- [CycloneDX 1.5 spec](https://cyclonedx.org/docs/1.5/json/)
- [SPDX 2.3 spec](https://spdx.github.io/spdx-spec/v2.3/)
- [Package URL (purl) spec](https://github.com/package-url/purl-spec)
