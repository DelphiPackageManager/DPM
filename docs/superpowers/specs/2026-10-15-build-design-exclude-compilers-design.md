# Exclude build/design entries for specific compilers

## Problem

A template is shared by every compiler whose targetPlatform names it. When one of those compilers
lacks a project the others have, the entry cannot be expressed today. Example:
`EtheaDev.StyledComponents.dspec` uses one `default` template for XE6..13.0, but there is no
`StyledAnimatedComponents` / `dclStyledAnimatedComponents` package for XE6. Packing XE6 fails
because the build entry's dproj does not exist in `packages/DXE6`.

## dspec format

Build and design entries gain an optional `excludeCompilers` sequence of compiler names, parsed
with `StringToCompilerVersion` (same spellings as `targetPlatforms.compiler`):

```yaml
build:
  - project: $packageSource$/StyledAnimatedComponents.dproj
    excludeCompilers:
      - delphixe6
design:
  - project: $packageSource$/dclStyledAnimatedComponents.dproj
    excludeCompilers:
      - delphixe6
```

- Absent or empty means the entry applies to every compiler using the template (current behaviour).
- A single scalar (`excludeCompilers: delphixe6`) is accepted as a one-item list.
- An unknown compiler name is a load error, logged with the offending value. A typo must not
  silently leave the entry included. `TSpecNode.LoadYAMLCollection` previously ignored each item's
  `LoadFromYAML` result, so entry-level errors (missing `src`/`project`/`files`) were logged but the
  spec stayed valid; it now propagates them, so those specs also fail `pack`/`prepare`. DSpecCreator
  does not check `IsValid` and still opens them.
- `excludeCompilers` is a dspec authoring concept. It is never written into a packed package's
  spec, since a packed spec covers exactly one compiler and the exclusion has already been applied.
  The packed and authoring specs share one writer, so this is done by clearing the list on the
  entries that remain (an empty list is not written).

## Core model

- `ISpecBuildEntry` and `ISpecDesignEntry` gain
  `property ExcludeCompilers : TCompilerVersions read/write`.
- `TSpecBuildEntry` / `TSpecDesignEntry`: field `FExcludeCompilers`, loaded in `LoadFromYAML`,
  written in `ToYAML` (sorted by enum order, via `CompilerToString`), carried through `Clone` /
  `CreateClone`.
- A small helper on each entry, `function AppliesTo(const compiler : TCompilerVersion) : boolean`,
  returns `not (compiler in FExcludeCompilers)`. Callers use it instead of testing the set
  directly.
- YAML read/write of a compiler list lives on `TSpecNode` (`LoadCompilersFromYAML`,
  `CompilersToYAML`), shared by both entry types.
- `ISpecTemplate.ApplyCompilerExclusions(compiler)` narrows a template to one compiler: removes the
  excluded build/design entries (logged at verbose level) and clears `ExcludeCompilers` on the rest.

## Pack (`TPackageWriter.InternalWritePackage`)

Immediately after the template is located on the reduced spec clone, and before `ReplaceTokens`,
`ComputeEffectiveBasePath` and `ValidateBuildEntries`, call
`template.ApplyCompilerExclusions(targetPlatform.Compiler)`.

Because removal happens first, an excluded entry's project:
- is not validated as covered by a source entry,
- is not scanned for build hooks,
- does not influence the effective base path,
- does not appear in the packed manifest.

## Install / IDE

No change. Both consume the packed spec, which is already filtered.

## Git registry packages

A git package is never packed - `TGitRegistryPackageRepository.ReduceSpec` reduces the repo's dspec
to the compiler being installed and writes that as the package spec. It calls
`ApplyCompilerExclusions(compilerVersion)` on every template of the reduced spec.

## `dpm prepare` (`TPreparePackageFolders`)

`GatherScaffoldItems` takes the compiler being scaffolded and skips entries that do not apply to it,
so the one-shot scaffold does not generate a dpk/dproj for an excluded entry. The propagate path
(copying an existing compiler folder's pairs to the others) is unchanged. It copies whatever pairs
exist on disk and has no mapping from pair back to spec entry.

## DSpecCreator

On both the Build card and the Design card, add a label "Exclude:" and a `TCheckListBox`
(`clbBuildExclude`, `clbDesignExclude`) beside the Platforms list.

- Populated on node selection (`tvTemplatesChange`) with the compilers whose targetPlatform uses
  the selected entry's template, plus any compiler already in the entry's `ExcludeCompilers` (so a
  stale exclusion stays visible and can be unticked). Items are in compiler enum order, displayed
  with `CompilerToString`.
- Items use the same short names as the Compilers list on the Platforms tab (`XE6`, `12.0`).
- Checked state reflects `ExcludeCompilers`.
- `OnClickCheck` rebuilds the set from checked items and assigns it to the entry, then marks the
  file modified, following the existing `clbBuildPlatformsClickCheck` pattern and honouring
  `FLoadingCard`.
- Layout: the Build card's References block and the Design card's lib/search-path controls shift
  right/down as needed to make room. The existing anchoring is kept.

## Pre-existing bugs fixed alongside

- `TSpecDesignEntry.SetPlatforms` never assigns `FPlatforms`, reads an uninitialised local, and
  tests `Win32` twice. Platform edits on DSpecCreator's Design card are currently discarded. Fix:
  assign `FPlatforms := value * [Win32, Win64]`. DSpecCreator disables the non-IDE items in the
  Design card's Platforms list and re-syncs the list after each click.
- The design entry's missing-project error said "Build Entry".
- `TSpec.DuplicateTemplate` does not copy build entries (nor copyLocal entries, package defs,
  environment variables). Fix by cloning the full template via the existing template `Clone` path
  and renaming, so excludes, and everything else, carry over.

## Documentation

`docs/dspec-format.md`: add `excludeCompilers` to the Build Entries and Design Entries tables, with
a short example and a note that exclusion is applied at pack time. `Schema/dspec-yaml-schema.json`:
add `excludeCompilers` to the build and design item schemas.

## Testing (DUnitX, `Source/Tests`)

- Build entry and design entry: load `excludeCompilers` from YAML. Write back and get the same set.
  Absent key gives the empty set. Unknown compiler name fails the load.
- Clone preserves `ExcludeCompilers`.
- `ToYAML` for a packed spec omits `excludeCompilers`.
- Pack writer: with a template shared by two compilers and one entry excluding one of them, the
  excluded compiler's packed spec lacks the entry, and the other compiler's packed spec has it.
  Where the existing writer tests make this practical, also cover an excluded entry whose dproj is
  not covered by any source entry: it must not fail validation.
- `TSpecDesignEntry.SetPlatforms` regression test.
- `ApplyCompilerExclusions` narrows a template correctly for each compiler.
- Prepare: a dry run does not scaffold an entry for a compiler that excludes it.

All of these are in `Source/Tests/DPM.Core.Tests.Spec.ExcludeCompilers.pas`.
