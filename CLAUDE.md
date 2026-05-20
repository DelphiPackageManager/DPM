# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

DPM (Delphi Package Manager) is an open-source package manager for Delphi XE2 and later, influenced by NuGet. It consists of three main components:
- **Command-line tool** (`dpm.exe`) - Cross-platform CLI for package operations
- **IDE plugins** - Integrated package management for Delphi XE2 through 13.0
- **Core library** - Shared functionality used by both CLI and IDE


## Architecture

### Core Module Constraint
**IMPORTANT:** Units in `Source/Core/` must NOT reference VCL or Windows GDI APIs. This enables the CLI to run in Docker containers on Windows Server Core.

### Key Interfaces and Their Hierarchy

```
IPackageIdentity (id, version, compiler)
    └── IPackageInfo (adds dependencies, hash, SupportedPlatforms)
            └── IPackageMetadata (adds description, authors, tags, release notes, frameworks)
                    └── IPackageSearchResultItem (adds UI-mutable state + server-only fields)

IPackageReference - DAG node for dependency resolution
IPackageInstaller - Install/Uninstall/Restore operations
IPackageRepository - Abstract package source (HTTP or directory)
ISpec* - Package specification (YAML-based .dspec files)
```

### Resolution / installation data flow

Three types carry "a resolved package" through the pipeline. They overlap on purpose — each
serves a different phase and has a different shape:

| Type | Phase | Shape | Mutability | Used for |
|---|---|---|---|---|
| `IPackageReference` | Resolution | DAG node with parent/children | Mutable (resolver attaches children, sets PackageInfo) | The project's live dependency graph. Walked DFS by the installer to compile packages and collect search paths in dependency order. |
| `IResolvedPackage` | Post-resolution | Flat record keyed by (Project, Package.Id) | Effectively immutable (only VersionRange is settable) | Cross-project / project-group conflict detection. Recorded in the installer context so other projects in the same group see the resolutions already made. |
| `IList<IPackageInfo>` | Post-resolution → install | Flat list, topologically sorted | Filled once by the resolver | The flattened work queue fed into download + compile. |

Clone semantics:
- `IPackageReference.Clone` produces a bare standalone reference with no children. Used by the
  restore slow path (`BuildTopLevelOnlyGraph`) to seed a fresh resolver pass from the project's
  top-level references only, stripping stale transients.
- `IResolvedPackage.Clone(parentId)` produces a copy with a new `ParentId`. Used when a sibling
  project in a group inherits a resolution from another project under its own top-level parent.

### Two restore paths in `TPackageInstaller`

`DoRestoreProjectForPlatform` picks one of two paths depending on whether the project's existing
graph is still consistent with each package's declared dependencies:

1. **Fast path** (`TryValidateRestoreGraph`) — walks the dproj's recorded graph and, for every
   declared dependency, verifies a child exists at a version within the declared range. If every
   transient is present and in range, the graph is good and we skip the resolver entirely. The
   resolved-packages list is populated in DFS order (deps before dependers) from the graph itself.

2. **Slow path** (`RestoreUsingInstallResolver`) — entered when the fast path detects a missing
   transient or out-of-range version. Builds a fresh root carrying only the top-level references
   (`BuildTopLevelOnlyGraph` — transients stripped) and calls `ResolveForInstall` for each top-level
   in turn, accumulating the output so shared transients are reused across iterations rather than
   being double-resolved.

There is only one "resolve" entry point on `IDependencyResolver` — `ResolveForInstall`. A previous
`ResolveForRestore` method existed but was never working (the context filtered out its requirements)
and has been removed; both paths above now go through `ResolveForInstall`.

### Core Subsystems (`Source/Core/`)

| Folder | Purpose |
|--------|---------|
| `Cache/` | Package caching in `%APPDATA%\.dpm\packages` |
| `Compiler/` | Delphi installation detection, MSBuild integration |
| `Configuration/` | App config management (YAML format) |
| `Dependency/` | Dependency resolution engine |
| `Package/` | Package representation and installation |
| `Packaging/` | ZIP archive reading/writing |
| `Project/` | .dproj/.groupproj file manipulation |
| `Repository/` | HTTP and directory-based package sources |
| `Sources/` | Package source configuration |
| `Spec/` | Package specification parsing (YAML .dspec files) |

### CLI Commands (`Source/Cmdline/Commands/`)

Commands follow `TConsoleCommandBase` pattern. Key commands: `Install`, `Restore`, `Pack`, `Push`, `Sources`.

### IDE Integration (`Source/IDE/`)

Each supported Delphi version has its own plugin project (`DPM.IDE.{version}.dproj`). IDE code handles project tree integration, context menus, and the package manager UI.

## Package Format

- **Package files**: `.dpkg` (ZIP archives with manifest)
- **Spec files**: `.dspec` (YAML format defining package metadata, dependencies, and build artifacts)
- **Naming convention**: `{PackageId}-{Compiler}-{Platform}-{Version}.dpkg`

## Output Directories

- `Output/` - Win32 binaries
- `Output64/` - Win64 binaries

## External Dependencies

Managed via DPM itself. Key packages: Spring4D (collections), VSoft.HttpClient, VSoft.SemanticVersion, vsoft.yaml.

## Current status

We are in the process of changing how dpm packages work. Previously, in the master branch, there would be a separate file per packageversion-compiler-platform combination. Whilst this worked well for command line installation - installing in the IDE proved problematic, since when installing design time components in the delphi IDE, design time components are typically shared - so if a package supports Win32, Win64 and MacOS for example, only one lot of design time components (a bpl file) can be installed (rather than separate design time components for each platform). 

So in the f-single-package-per-compiler branch, we are changing this, so that the package file includes all the platforms that the package supports. We have already refactored the code for the Pack command - the status of the install command is unclear since it's a while since we worked on it.