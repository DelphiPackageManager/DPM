# Bundled Dependencies

Some libraries ship **bundled with the Delphi IDE** rather than as DPM packages. The
classic example is **Indy** — every Delphi install already has a copy of Indy on its
library path, but there is (currently) no DPM package for it.

This creates a problem for package authors. If your package uses Indy, you would like to
*declare* that dependency so it is visible and documented. But a normal DPM dependency
must point at a real package and a version range:

```yaml
dependencies:
  - id: Indy.System
    version: "[10.6.2,]"     # <-- there is no Indy.System package to satisfy this!
```

With no `Indy.System` package on any feed, the resolver fails: it cannot satisfy the
dependency, so the install errors out.

**Bundled dependencies** solve this. You declare the dependency using the special
version `bundled`, and DPM does the right thing:

- If **no** package for that id exists, the dependency is satisfied by the copy that
  ships with the IDE — no download, nothing added to your `.dproj` search paths.
- If a **real** package for that id *is* present (because the user installed one, or
  another package pulled it in), that real package is used automatically — at whatever
  version it happens to be.

You declare the intent once; DPM picks the best available option at install time.

---

## Declaring a bundled dependency

Use `bundled` as the version:

```yaml
dependencies:
  - id: Indy.System
    version: bundled
```

That's it. There is no separate "kind" or flag — the version string is the whole signal.

> Under the hood `bundled` is an alias for the sentinel version `999.999.999`. You can
> write either form; `bundled` is just easier to read.

---

## A worked example: Indy

Imagine the Indy source is eventually published to DPM as three packages that build on
each other:

| Package | Purpose |
|---|---|
| `Indy.System` | Low-level system units (sockets, streams, globals) |
| `Indy.Core` | Core components, built on `Indy.System` |
| `Indy.Protocols` | HTTP / SMTP / FTP etc., built on `Indy.Core` and `Indy.System` |

Today none of these exist as DPM packages — Indy comes from the IDE. So a package that
needs Indy's protocols declares the whole chain as **bundled**:

```yaml
# MyHttpHelper.dspec
metadata:
  id: Acme.MyHttpHelper
  version: 1.0.0
  description: Helpers built on Indy's HTTP client
  authors:
    - Acme
  license: Apache-2.0
targetPlatforms:
  - compiler: 12.0
    platforms: [Win32, Win64]
    template: default
templates:
  - name: default
    dependencies:
      - id: Indy.System
        version: bundled
      - id: Indy.Core
        version: bundled
      - id: Indy.Protocols
        version: bundled
    source:
      - src: .\src\*.pas
        dest: src
```

### What happens when a user installs `Acme.MyHttpHelper`

**Case 1 — no Indy packages exist (the situation today).**
The three bundled dependencies resolve to no-ops. Nothing is downloaded for them and no
Indy search paths are added — the compiler finds Indy on the IDE's default library path,
exactly as it would without DPM. The install succeeds.

**Case 2 — the user installs a real Indy package.**
Later, real Indy packages become available and the user adds one to their project as a
top-level package:

```
dpm install Indy.Protocols
```

Now `Indy.Protocols` (and whatever it depends on — `Indy.Core`, `Indy.System`) is a real
resolved package. The next time `Acme.MyHttpHelper` is resolved, its `bundled`
dependencies see the real packages already in the graph and **defer to them
automatically**. The real Indy source is used and its search paths are added — no change
needed to `Acme.MyHttpHelper`.

The author never had to ship two versions of their package or guess at version numbers.

---

## Rules and behaviour

- **Any real version wins.** A `bundled` dependency is satisfied by *any* version of a
  real package with the same id. DPM does not apply a version range to it — that is the
  user's choice when they install the real package.
- **DPM never auto-fetches the real package.** A `bundled` dependency only honours what is
  already in the dependency graph. It will not reach out to a feed and pull in the latest
  Indy on its own — the user opts in by installing a real package themselves. This keeps
  resolution predictable and works offline.
- **Nothing is written to your project.** A bundled no-op contributes no `PackageReference`
  and no search path to the `.dproj`. Restores re-evaluate it each time, so there is never
  a stale `999.999.999` entry left behind.
- **It is satisfied per id, once.** If several of your packages declare `bundled` on the
  same id, they all share a single resolution.

---

## When to use it

Use a bundled dependency when:

- The library ships with the Delphi IDE (Indy, and similar built-in libraries), **and**
- You want to declare the dependency so it is documented and ready to be replaced by a
  real package later.

Do **not** use it for ordinary libraries that have a normal DPM package — declare those
with a real version range (see [Version Ranges](dspec-format.md#version-ranges)).

---

## See also

- [DPM Package Specification (.dspec) Format](dspec-format.md) — the full spec format,
  including the [Dependencies](dspec-format.md#dependencies) and
  [Version Ranges](dspec-format.md#version-ranges) sections.
