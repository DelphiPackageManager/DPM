# DPM Package Specification (.dspec) Format

This document describes the YAML-based `.dspec` file format used by DPM (Delphi Package Manager) to define package metadata, dependencies, and build configurations.

## Table of Contents

- [Overview](#overview)
- [Root Structure](#root-structure)
- [Metadata Section](#metadata-section)
- [Target Platforms Section](#target-platforms-section)
- [Templates Section](#templates-section)
- [Variables Section](#variables-section)
- [Package Kind](#package-kind)
- [Version Ranges](#version-ranges)
- [Variable Expansion](#variable-expansion)
- [Complete Example](#complete-example)

---

## Overview

A `.dspec` file is a YAML document that defines everything needed to build, package, and distribute a Delphi package. The format supports:

- Multiple compiler versions (XE2 through 13.0)
- Multiple target platforms (Win32, Win64, macOS, iOS, Android, Linux)
- Runtime and design-time packages
- Dependencies with version ranges
- Source file mapping
- Variables and templates for flexible configurations

---

## Root Structure

```yaml
metadata:                    # REQUIRED - Package metadata
targetPlatforms:             # REQUIRED - Target compiler/platform configurations
templates:                   # REQUIRED - Build templates
variables:                   # OPTIONAL - Package-level variables
packageKind:                 # OPTIONAL - Package type (default: "dpm")
min dpm client version:      # OPTIONAL - Minimum DPM client version required
```

---

## Metadata Section

The `metadata` section contains information about the package itself.

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique package identifier (e.g., "VSoft.CommandLine") |
| `version` | string | Semantic version (required when `packageKind` is "dpm") |
| `description` | string | Brief description of the package |
| `authors` | string or array | Package author(s) |

### Optional Fields

| Field | Type | Description |
|-------|------|-------------|
| `projectUrl` | string | URL to project homepage |
| `repositoryUrl` | string | URL to source repository |
| `repositoryType` | string | Repository type (e.g., "git") |
| `repositoryBranch` | string | Default branch name |
| `repositoryCommit` | string | Specific commit hash or `#HASH#` placeholder |
| `license` | string | License identifier (e.g., "Apache-2.0", "MIT") |
| `copyright` | string | Copyright notice |
| `icon` | string | Path or URL to package icon |
| `readme` | string | Path to README file |
| `releaseNotes` | string | Release notes content or path |
| `tags` | string or array | Searchable tags for the package |
| `isTrial` | boolean | Indicates trial version (default: false) |
| `isCommercial` | boolean | Indicates commercial package (default: false) |
| `frameworks` | array | UI frameworks supported: `VCL`, `FMX`, or both |

### Example

```yaml
metadata:
  id: "VSoft.CommandLine"
  version: "1.0.0"
  description: "Command line argument parser for Delphi"
  authors: "Vincent Parrett"
  projectUrl: "https://github.com/VSoftTechnologies/VSoft.CommandLine"
  repositoryUrl: "https://github.com/VSoftTechnologies/VSoft.CommandLine.git"
  repositoryType: "git"
  license: "Apache-2.0"
  copyright: "Copyright 2024 Vincent Parrett"
  tags:
    - "command-line"
    - "arguments"
    - "parser"
  frameworks:
    - VCL
    - FMX
```

---

## Target Platforms Section

The `targetPlatforms` section defines which compiler versions and platforms the package supports.

### Structure

```yaml
targetPlatforms:
  - compiler: "XE2"              # Single compiler version
    platforms: "Win32, Win64"    # Comma-separated platforms
    template: "default"          # Template to use (defaults to "default")
    variables:                   # Platform-specific variable overrides
      customVar: "value"
```

### Compiler Specification Options

You must use exactly ONE of these approaches per target platform entry:

**Single Compiler:**
```yaml
- compiler: "12.0"
  platforms: "Win32, Win64"
```

**Compiler Range (inclusive):**
```yaml
- compiler from: "XE2"
  compiler to: "12.0"
  platforms: "Win32, Win64"
```

**Multiple Discrete Compilers:**
```yaml
- compilers:
    - "XE2"
    - "XE7"
    - "12.0"
  platforms: "Win32, Win64"
```

### Supported Compiler Versions

| Version | Delphi Version |
|---------|----------------|
| `XE2` | Delphi XE2 |
| `XE3` | Delphi XE3 |
| `XE4` | Delphi XE4 |
| `XE5` | Delphi XE5 |
| `XE6` | Delphi XE6 |
| `XE7` | Delphi XE7 |
| `XE8` | Delphi XE8 |
| `10.0` | Delphi 10 Seattle |
| `10.1` | Delphi 10.1 Berlin |
| `10.2` | Delphi 10.2 Tokyo |
| `10.3` | Delphi 10.3 Rio |
| `10.4` | Delphi 10.4 Sydney |
| `11.0` | Delphi 11 Alexandria |
| `12.0` | Delphi 12 Athens |
| `13.0` | Delphi 13 |

### Supported Platforms

| Platform | Description |
|----------|-------------|
| `Win32` | Windows 32-bit |
| `Win64` | Windows 64-bit |
| `MacOS32` | macOS 32-bit (legacy) |
| `MacOS64` | macOS 64-bit Intel |
| `MacOSARM64` | macOS ARM64 (Apple Silicon) |
| `Android` | Android 32-bit |
| `Android64` | Android 64-bit |
| `iOS32` | iOS 32-bit (legacy) |
| `iOS64` | iOS 64-bit |
| `iOSSimulator` | iOS Simulator Intel |
| `iOSSimARM64` | iOS Simulator ARM64 |
| `Linux64` | Linux 64-bit |

> **Note:** Platform availability varies by compiler version. Not all platforms are available for all compilers.

### Example

```yaml
targetPlatforms:
  # XE2 only supports Win32/Win64
  - compiler: "XE2"
    platforms: "Win32, Win64"
    template: "default"

  # Range of compilers with more platforms
  - compiler from: "10.2"
    compiler to: "12.0"
    platforms: "Win32, Win64, Android, Android64"
    template: "default"
    variables:
      extraDefines: "MOBILE_SUPPORT"
```

---

## Templates Section

Templates define what gets included in the package and how it's built.

### Structure

```yaml
templates:
  - name: "default"          # REQUIRED - Template identifier
    dependencies:            # Package dependencies
    source:                  # Source files to include
    build:                   # Runtime packages to build
    design:                  # Design-time packages to build
```

### Dependencies

Dependencies specify other DPM packages that this package requires.

```yaml
dependencies:
  - id: "Spring4D.Core"
    version: "[2.0.0,)"          # Version 2.0.0 or higher
  - id: "VSoft.SemanticVersion"
    version: "[1.0.0,2.0.0]"     # Version 1.0.0 to 2.0.0 inclusive
  - id: "MyCompany.Shared"
    version: "$version$"          # Same version as this package
```

### Source Entries

Source entries define which files to include in the package.

```yaml
source:
  - src: ".\\src\\*.pas"         # Source path (relative, supports wildcards)
    dest: "src"                   # Destination folder in package
    exclude:                      # Patterns to exclude
      - "*.dcupkg"
      - "Test*.pas"
      - "*.dcu"
```

| Field | Required | Description |
|-------|----------|-------------|
| `src` | Yes | Relative path to source files (supports wildcards) |
| `dest` | No | Destination folder within the package |
| `exclude` | No | Array of patterns to exclude |

### Build Entries (Runtime Packages)

Build entries define runtime packages (.bpl) to compile.

```yaml
build:
  - project: "packages\\RuntimePkg.dpk"
    platforms: "Win32, Win64"     # Can override targetPlatform
    defines: "RELEASE;DPM"        # Semicolon-separated compiler defines
```

| Field | Required | Description |
|-------|----------|-------------|
| `project` | Yes | Path to .dpk or .dproj file |
| `platforms` | No | Override platforms for this build |
| `defines` | No | Additional compiler defines |

### Design Entries (Design-time Packages)

Design entries define design-time packages for IDE integration.

```yaml
design:
  - project: "packages\\DesignPkg.dpk"
    platforms: "Win32, Win64"     # Design packages only support Win32/Win64
    defines: "DESIGNTIME"
    libSuffix: "280"              # Library suffix (e.g., "280" for Delphi 12)
    libPrefix: "dcl"              # Library prefix
    libVersion: "28.0"            # Library version string
```

| Field | Required | Description |
|-------|----------|-------------|
| `project` | Yes | Path to design-time .dpk file |
| `platforms` | No | Limited to Win32 and Win64 |
| `defines` | No | Compiler defines for design-time build |
| `libSuffix` | No | Override library suffix |
| `libPrefix` | No | Override library prefix (default: "dcl") |
| `libVersion` | No | Override library version string |

> **Note:** Design-time packages are only supported on Win32 and Win64 platforms.

### Complete Template Example

```yaml
templates:
  - name: "default"
    dependencies:
      - id: "Spring4D.Core"
        version: "[2.0.0,)"

    source:
      - src: ".\\src\\*.pas"
        dest: "src"
        exclude:
          - "*.dcu"
          - "*.dcupkg"
      - src: ".\\inc\\*.inc"
        dest: "inc"

    build:
      - project: "packages\\MyPackage.dpk"
        platforms: "Win32, Win64"
        defines: "RELEASE"

    design:
      - project: "packages\\MyPackageDesign.dpk"
        platforms: "Win32, Win64"
        defines: "DESIGNTIME"
```

---

## Variables Section

Variables allow you to define reusable values that can be referenced throughout the spec.

### Package-Level Variables

```yaml
variables:
  outputDir: ".\\output"
  srcPattern: "src\\*.pas"
  companyName: "MyCompany"
```

### Platform-Specific Variables

Variables can be overridden per target platform:

```yaml
targetPlatforms:
  - compiler: "12.0"
    platforms: "Win32, Win64"
    template: "default"
    variables:
      outputDir: ".\\output\\delphi12"
```

---

## Variable Expansion

Variables can be referenced using the `$variableName$` syntax:

```yaml
variables:
  baseDir: ".\\src"

templates:
  - name: "default"
    source:
      - src: "$baseDir$\\*.pas"
        dest: "src"
```

### Special Variables

| Variable | Description |
|----------|-------------|
| `$version$` | The package version from metadata (useful in dependencies) |

### Example with Special Variables

```yaml
metadata:
  id: "MyCompany.Core"
  version: "1.2.0"

templates:
  - name: "default"
    dependencies:
      # Depend on same version of related package
      - id: "MyCompany.Shared"
        version: "$version$"
```

---

## Package Kind

The `packageKind` field specifies the type of package.

### Standard DPM Package (Default)

```yaml
packageKind: "dpm"

metadata:
  id: "MyPackage"
  version: "1.0.0"      # Single version
```

### Git Package

Git packages reference a Git repository and can define multiple versions:

```yaml
packageKind: "git"

metadata:
  id: "MyPackage"
  versions:                     # Array of versions (instead of single version)
    - version: "1.0.0"
      commit: "abc123def456789..."
    - version: "0.9.0"
      commit: "xyz789abc123456..."
```

---

## Version Ranges

DPM uses NuGet-style version ranges:

| Notation | Description |
|----------|-------------|
| `1.0.0` | Exact version 1.0.0 |
| `[1.0.0]` | Exact version 1.0.0 |
| `[1.0.0,)` | Version 1.0.0 or higher |
| `(,2.0.0]` | Any version up to and including 2.0.0 |
| `[1.0.0,2.0.0]` | Version 1.0.0 to 2.0.0 (inclusive) |
| `(1.0.0,2.0.0)` | Greater than 1.0.0 and less than 2.0.0 (exclusive) |
| `[1.0.0,2.0.0)` | Version 1.0.0 (inclusive) to 2.0.0 (exclusive) |

### Version Format

Versions follow semantic versioning:

```
MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
```

Examples:
- `1.0.0`
- `2.1.3`
- `1.0.0-beta`
- `1.0.0-rc.1`
- `1.0.0+build.123`

---

## Complete Example

```yaml
# DPM package spec file
# Used to build packages

min client version: 1.0
# metadata describes the package to the user
metadata:
  id: Gabr42.OmniThreadLibrary
  authors: [Primož Gabrijelčič]
  description: OmniThreadLibrary is a powerful threading library for Delphi
  projectUrl: https://github.com/gabr42/OmniThreadLibrary
  repositoryUrl: https://github.com/gabr42/OmniThreadLibrary
  license: BSD
  readme: README.md
  copyright: Primož Gabrijelčič
  tags: [threading, async]
  version: 3.7.12
  repositoryCommit: f5a3b982084b6b54f2ac0a501130799b594202e0

# define variable defaults - can be overriden in targetPlatforms
variables:
  packageSource: "Delphi $compilernoprefix$ $compilerCodeName$"

# define the compiler/platform sup
targetPlatforms:
  - compiler from: delphixe2
    compiler to: delphixe8
    platforms: [win32, win64]
  - compiler: delphi10
    platforms: [win32, win64]
    # delphi 10.0 uses 10 for folder name
    variables:
      packageSource: "Delphi 10 $compilerCodeName$"
  # defining compilers as a range
  - compiler from: delphi10.1
    compiler to: delphi13
    platforms: [win32, win64]

# defines how the packages will be built
templates:
  - name: default
    # omni has the source in the root of the repo, moving everything into src folder
    source:
      - src: ./*.dcr
        dest: src
      - src: ./LICENSE.txt
      - src: ./**/*.pas
        exclude:
          - ./tests/**
          - ./unittests/**
          - ./examples/**
          - ./bag of stuff/**
        dest: src
      - src: ./*.inc
        dest: src
      - src: ./packages/$packageSource$/**
        dest: /src/packages/$packageSource$
    build:
      - project: ./src/packages/Delphi $packageSource$/OmniThreadLibraryRuntime.dproj
    design:
      - project: ./src/packages/Delphi $packageSource$/OmniThreadLibraryDesigntime.dproj

```

---

## File Extensions

DPM recognizes these file extensions:

| Extension | Description |
|-----------|-------------|
| `.dspec` | Package specification file |
| `.dspec.yaml` | Explicit YAML variant |
| `.dpkg` | Compiled package archive (ZIP format) |
| `.manifest.yaml` | Package manifest (generated) |

---

## Validation Rules

When creating a `.dspec` file, ensure:

1. **Required sections exist:** `metadata`, `targetPlatforms`, and `templates`
2. **Metadata is complete:** `id`, `version` (for dpm), `description`, and `authors` are required
3. **At least one target platform** is defined
4. **At least one template** is defined with a `name`
5. **Compiler specification is valid:** Use exactly one of `compiler`, `compiler from/to`, or `compilers`
6. **Platforms are valid** for the specified compiler versions
7. **Design packages** only target Win32 or Win64

---

## Tips and Best Practices

1. **Use semantic versioning** consistently for your package versions
2. **Define version ranges** for dependencies to allow flexibility
3. **Exclude generated files** (*.dcu, *.dcupkg) from source entries
4. **Test with multiple compiler versions** if you support a range
5. **Use variables** to avoid repetition in paths
6. **Keep templates simple** - use one template unless you need different configurations
7. **Document your package** with a good description and tags for discoverability
