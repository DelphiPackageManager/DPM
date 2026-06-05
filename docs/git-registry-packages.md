# Git Registry Packages

DPM can consume packages whose definitions live in a **git registry** instead of
requiring pre-built `.dpkg` files pushed to a DPM server. 

A registry is a collection of `.dspec.yaml` files (one folder per package id). Each
dspec points at the package's own git repository, using git tags for versions.

On install, DPM clones that
repository at the resolved commit directly into the package cache and builds it (or,
for source-only packages, adds its source folders to the project search paths). No
`.dpkg` is produced and nothing is pushed to a server.

---

## For consumers

### Adding a registry source

A registry source can be a **git URL** or a **local folder**:

```
dpm sources add -name myregistry -source https://github.com/acme/dpm-registry.git
dpm sources add -name localreg   -source C:\work\dpm-registry
```

DPM auto-detects the `GitDirectory` source type when the source is a git URL
(`*.git`, `git@…`, `ssh://…`) or a folder already laid out as a registry (a
sub-folder containing an `<id>.dspec.yaml`). If auto-detection can't tell (e.g. an
empty folder, or an `https` URL without a `.git` suffix), set it explicitly:

```
dpm sources add -name myregistry -source https://example.com/registry -type GitDirectory
```

`dpm sources` shows the type so you can confirm:

```
  1.  myregistry  [Enabled]  (Git Registry)
      https://github.com/acme/dpm-registry.git
```

### Installing

Git packages install like any other:

```
dpm install Acme.Widgets -projectPath MyApp.dproj
dpm restore MyApp.dproj
dpm cache add Acme.Widgets -compiler=13
```

- A package repo with **semver tags** (`v1.2.3`, `1.2.3`, `2.0.0-beta1`) exposes
  those as versions. Prerelease tags require `-prerelease`, as usual.
- A package repo with **no tags** is published as a stable **`0.0.1`** that tracks
  the default branch HEAD. It's visible/installable without `-prerelease`, and DPM
  re-clones it whenever the remote HEAD moves (on the next restore/install).

### Refreshing a registry

For **git-URL** registries DPM keeps a local mirror under
`%APPDATA%\.dpm\registries\<name>\`, refreshed automatically when it's older than a
TTL at the start of a `list`/`install`/`restore`. To force an immediate pull:

```
dpm sources refresh                # all git registries
dpm sources refresh -name myreg    # just one
```

The TTL is a **global** setting in your DPM config file
(`%APPDATA%\.dpm\dpm.config.yaml`), applied to every git-URL registry:

```yaml
registryRefreshIntervalMinutes: 60   # default
#   0  -> always pull on every catalog operation
#  -1  -> never auto-pull (only `dpm sources refresh`)
```

**Folder** registries are read in place — there is no mirror and nothing to refresh.

### Trust / signing

Git packages are **built from source on your machine, so they are unsigned**. They
install under DPM's permissive trust policy (the default). Under a strict policy,
unsigned packages — from any source — are rejected by design. Cloned packages carry
no `.dpkg`, manifest, or signing receipt; DPM identifies them in the cache by a
`.dpm-git-commit` marker and skips signing verification for them only.

You cannot `dpm push` to a git registry — versions come from the package repo's git
history, not from uploaded artifacts.

---

## For registry authors

### Layout

One folder per package id, each containing `<id>.dspec.yaml`:

```
dpm-registry/
  Acme.Widgets/
    Acme.Widgets.dspec.yaml
  Acme.Core/
    Acme.Core.dspec.yaml
```

### The registry dspec

The registry dspec is the **full build spec**. It must declare `packageKind: git`
and include `repositoryUrl` and the `targetPlatforms` / `templates` (with `source`,
and `build`/`design` if the package compiles to BPLs). A minimal source-only
example:

```yaml
packageKind: git
metadata:
  id: Acme.Widgets
  description: Reusable VCL widgets
  authors: [ Acme ]
  license: MIT
  repositoryUrl: https://github.com/acme/widgets.git
targetPlatforms:
  - compiler: delphi13.0
    platforms: [ Win32, Win64 ]
templates:
  - name: default
    source:
      - src: ./src/*.pas
```

Notes:

- **`packageKind: git` is required.** It's a top-level field (a sibling of
  `metadata` / `targetPlatforms`). It's what lets you omit `version`: a normal
  (`dpm`) dspec requires `metadata.version`, but for a git registry the versions
  come from the package repo's git tags instead.
- **Don't set `metadata.version`** — it's ignored for git packages; versions are the
  repo's tags (or `0.0.1` for an untagged HEAD).
- **`repositoryUrl` is required** — it's the repo DPM clones.
- If a package compiles, add `build`/`design` entries as in a normal dspec; their
  `project:` paths are repo-relative. With no build/design entries the package is
  source-only and DPM just adds its source folders to the search paths.

### Search paths come from `src`, not `dest`

Because git packages are cloned in place (no `src → dest` copy step), DPM derives
search paths from each source entry's **`src`** glob, ignoring `dest`. Author your
`src` patterns to match the repo layout:

| `src` pattern        | search path(s) added                               |
|----------------------|----------------------------------------------------|
| `./*.pas`            | the repo root                                      |
| `./source/*.pas`     | `source`                                           |
| `./source/**/*.pas`  | `source` **and** every subfolder under it with `.pas` files |

(Delphi search paths aren't recursive, so a `**` pattern expands to each real
subfolder that contains source.)

### Versioning

- Tag releases with semver tags (`v1.2.3` or `1.2.3`; a leading `v` is fine).
  Prerelease tags (`2.0.0-beta1`) are honored under `-prerelease`.
- An **untagged** repo is published as stable `0.0.1`, tracking the default branch
  HEAD. Add tags as soon as you have releases so consumers can pin versions.

### Per-version build / the package's own dspec

The registry dspec applies to all versions, **but if the cloned repo contains its
own `*.dspec.yaml` at the resolved commit, that one wins** for the build. The
registry dspec remains the fallback and is always the source DPM uses for **version
discovery and dependency resolution** (it can't clone every candidate while
resolving). Keep the registry dspec's `dependencies` accurate even if the repo
provides its own build spec.

---

## How it works (summary)

| Step | What DPM does |
|---|---|
| Discover | Reads the registry catalog; lists versions from `git ls-remote --tags <repositoryUrl>` (no tags → `0.0.1`). |
| Resolve | Builds dependency info from the registry dspec. |
| Install | Clones the repo at the resolved tag/HEAD into `%APPDATA%\.dpm\package_cache\<compiler>\<id>\<version>\`, writes `package.dspec.yaml`, drops a `.dpm-git-commit` marker. |
| Build | Compiles `build`/`design` projects if present; otherwise adds the `src` folders to the project search paths. |
