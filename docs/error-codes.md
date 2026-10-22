# DPM error and warning codes

Every error and warning DPM reports carries a code, shown in front of the message:

```
error DPME1101: The package [Spring4D.Core] was not found for compiler 12.0.
warning DPMW1004: Source [delphi.dev] could not be reached : 12029 Cannot connect
```

- `DPME` codes are errors. An error means the operation failed.
- `DPMW` codes are warnings. The operation carried on, but something needs your attention.

This is the same layout MSBuild uses, so CI systems that recognise MSBuild errors pick DPM's up too.

When an operation fails, DPM reports the error that caused it once, where it was found. The lines that
follow it ("Install failed for ...", "Restore failed in ...") say what happened as a result and carry no
code. Search this page for the code to find out what it means and what to do about it. Messages without a
code (`error: ...`) come from parts of DPM that haven't been given codes yet.

To see more detail about what DPM was doing when something went wrong, run the command again with
`--verbosity=Detailed`, or `--verbosity=Debug` for everything.

## How the numbers are organised

Errors and warnings share one set of numbers, so a number is used by only one code: an error and a
warning never have the same number. The first digit says which part of DPM reported the problem.

| Range | Area |
|---|---|
| 0xxx | General: the command line, options and configuration |
| 1xxx | Package sources and repositories |
| 2xxx | Dependency resolution |
| 3xxx | Install, restore, uninstall and the package cache |
| 4xxx | Project files (.dproj, .groupproj) |
| 5xxx | Compiling packages |
| 6xxx | Package specs (.dspec), pack and push |
| 7xxx | Signing, trust and verification |
| 8xxx | SBOM and vulnerability scanning |
| 9xxx | The IDE plugin, DSpec Creator and the MCP server |

Once a code is released it is never renumbered or reused.

## General (0xxx)

### DPME0001

**An unexpected exception occurred.**

DPM hit an error that nothing in it was expecting. The message holds the exception's text, and its class
name when that's more specific than `Exception`.

**What to do:** If the message doesn't point to something you can fix, such as a file that is locked or a
disk that is full, please report it as a DPM bug and include the full output.

### DPME0002

**The operation failed without reporting why.**

Something failed but didn't report an error explaining why. DPM adds this code so that a failure is never
silent.

**What to do:** This is always a bug in DPM. Please report it with the full output, run with
`--verbosity=Debug` if you can.

### DPME0003

**Internal error.**

DPM found itself in a state that should not be possible.

**What to do:** Please report it as a DPM bug and include the full output.

### DPMW0004

**The operation was cancelled.**

You cancelled the operation (Ctrl+C on the command line, or Cancel in the IDE) before it finished. Any
changes it hadn't finished making were not saved.

## Package sources and repositories (1xxx)

### DPME1001

**No package sources are configured.**

DPM needs at least one package source to find packages, and the configuration file has none.

**What to do:** Add a source with `dpm sources add`, or in the IDE's DPM options.

### DPME1002

**A package source is not valid.**

A source in the configuration has a uri DPM can't parse, or one whose scheme isn't `file`, `http` or
`https`. The message names the source.

**What to do:** Fix or remove the source with `dpm sources update` / `dpm sources remove`, or in the IDE's
DPM options.

### DPME1003

**The package source is not configured.**

An operation names a source (for example a package recorded as coming from it, or `--source` on push) that
isn't in the configuration.

**What to do:** Add the source, or use one that is configured. `dpm sources list` shows them.

### DPMW1004

**A package source could not be used.**

A request to a source failed: it couldn't be reached, it returned an error, it isn't a DPM server, or it
sent something DPM couldn't read. The message says which. DPM carries on with the other sources - this only
becomes a problem if none of them has what was asked for, and then an error (such as DPME1102) says so.

**What to do:** Check the source's url and your network or proxy settings. A source that is down only for a
while can be disabled with `dpm sources disable` until it's back.

### DPME1101

**Package not found.**

None of the sources (and not the package cache) has the package, or not that version of it, for the
compiler being used.

**What to do:** Check the package id and version. `dpm search <id> --compiler=<version>` shows what the
sources have. If you use more than one source, check the package is on one that is enabled.

### DPME1102

**Package not found - a package source could not be used.**

The package wasn't found, but at least one source couldn't be used (see the DPMW1004 warnings before this
error), so it may be on that source.

**What to do:** Fix whatever the DPMW1004 warnings report, then try again.

### DPME1201

**Unable to download the package.**

Every source that was tried failed to send the package, or none of them had it. The message lists each
source and why.

**What to do:** Check the sources named in the message and your network or proxy settings, then try again.

### DPME1202

**The package does not match its recorded hash.**

The package file a source sent is not the file it should be. A folder source has a package file that
doesn't match its `.sha256` file, or a server sent a file whose hash differs from the one it recorded.
DPM won't use it.

**What to do:** For a folder source, push the package again, or delete the `.sha256` file if the change was
intentional. For a server, try again later and report it to whoever runs the server if it persists.

### DPME1203

**A git command failed.**

Installing a package from a git registry ran a git command that failed. The message includes what git said.

**What to do:** Check that git is installed and on the PATH, that the repository url in the registry is
right, and that you can reach it (for example with `git ls-remote <url>`).

### DPMW1204

**No hash is recorded for the package, so the download was not verified.**

The source doesn't publish a hash for this package, so DPM couldn't check the downloaded file.

**What to do:** Nothing, unless you need every download verified - then use a source that publishes hashes.

### DPMW1205

**Unable to write the package hash file.**

DPM downloaded and verified the package, but couldn't save its `.sha256` file next to it in the package
cache.

**What to do:** Check you can write to the package cache folder.

### DPMW1206

**More than one package file matched in a folder source.**

A folder source holds more than one file for the same package, compiler and version (for example packed for
different platforms). DPM used the first one.

**What to do:** Remove the extra files from the folder source.

### DPMW1207

**A package from a source could not be read.**

One package from a source - a file in a folder source, or an entry a server sent - couldn't be read. DPM
skips it and carries on with the rest.

**What to do:** For a folder source, replace or remove the file the message names. For a server, report it
to whoever runs the server.

### DPMW1208

**An entry in a package registry is not valid.**

A git package registry has an entry DPM can't use: its dspec can't be read, it has no id, the id appears
twice, or it has no `repositoryUrl`. DPM skips it.

**What to do:** Fix the entry in the registry.

### DPME1209

**Unable to push the package.**

The push request couldn't be made: the server couldn't be reached, or it doesn't support publishing.

**What to do:** Check the source's url and your network, then push again.

## Dependency resolution (2xxx)

### DPME2001

**No available version of a dependency satisfies the required range.**

A package needs a version of another package that none of the sources (or the package cache) has. The
message shows which package needs what.

**What to do:** Check whether a newer version of the package that needs it is available, or whether the
dependency is on a source you haven't added.

### DPME2002

**A top level package version does not satisfy a dependency.**

The project references a package directly at one version, and another package needs a different version of
it. A version you reference directly is never changed for you.

**What to do:** Install a version of the directly referenced package that satisfies the range, or upgrade
the package that needs it (in the IDE, use Upgrade; on the command line, `dpm install --upgrade`).

### DPME2003

**A version used by another project in the group does not satisfy a dependency.**

Projects in a project group share their package versions. Another project in the group uses a version of a
package that this install needs to be different.

**What to do:** Bring the projects in the group onto versions that work together - install or upgrade the
package in the other project first.

### DPME2004

**Dependency resolution did not finish.**

DPM tried too many combinations of versions without settling. This is most likely a cycle in the packages'
dependencies.

**What to do:** Report it, with the packages being installed, to the package authors or as a DPM bug.

### DPME2005

**A selected package version does not satisfy a dependency.**

The versions chosen for this install don't fit together: one package requires a range that the version
chosen for another doesn't satisfy.

**What to do:** Choose versions that work together, or install with `--upgrade` so the packages that need to
move can move together.

### DPME2006

**A top level package version is not compatible with a package that uses it.**

During a restore, a package the project references directly is at a version that another package in the
project can't use - an older version, or a different major version, than the one it needs.

**What to do:** Change the version the project references (install it again at a compatible version), or
upgrade the package that needs it.

### DPME2007

**The packages to install cannot be resolved together.**

The packages being installed (with the ones that would have to move with them) need versions of their
dependencies that conflict. The message lists the conflicts.

**What to do:** Install different versions of the packages, or install them separately.

### DPME2008

**No compatible version of a package that has to move with the install.**

The install needs another installed package to move to a version that works with it, but no version of
that package does.

**What to do:** Wait for a compatible version of the other package, or choose a different version of the one
you're installing.

### DPME2009

**The install plan did not settle.**

Working out which other packages have to move with the install kept finding more. DPM gave up.

**What to do:** Install the packages one at a time, or report it with the packages involved.

### DPMW2010

**Projects in the group use different versions of a package.**

Another project in the group uses a different version of a package that this project references. The
install carries on - it doesn't change that package - but the group is inconsistent.

**What to do:** Bring the projects onto the same version of the package.

### DPMW2011

**A top level package is newer than the range a package that uses it declares.**

The project references a package directly at a version newer than another package's declared range, but on
the same major version, so it should work. DPM uses the referenced version.

**What to do:** Nothing, unless the package that uses it has problems with that version.

### DPMW2012

**Other installed packages have to move with this install.**

The install failed because other packages installed in the projects need to move to new versions with it.
The warning lists them.

**What to do:** Use Upgrade in the IDE, or `--upgrade` on the command line, to install them together.

## Install, restore, uninstall and the package cache (3xxx)

### DPME3001

**None of the projects need the package(s).**

None of the projects the install was given can take the package - for example, none of them target the
platforms that were asked for.

**What to do:** Check the projects and the `--platforms` option.

### DPME3002

**The package is not installed in any of the projects.**

An upgrade was asked for a package that isn't installed in any of the projects.

**What to do:** Install the package instead of upgrading it.

### DPME3003

**No packages were given to install.**

The install wasn't told which package(s) to install.

**What to do:** Give a package id (or a package file) to install.

### DPME3004

**The package file does not exist.**

The package file (`.dpkg`) given to install or add to the cache doesn't exist.

**What to do:** Check the path.

### DPME3005

**The file is not a valid package file.**

The file isn't a DPM package: it doesn't have the `.dpkg` extension, its name isn't
`{Id}-{Compiler}-{Platforms}-{Version}.dpkg`, or it doesn't contain a `package.dspec`.

**What to do:** Use a package file produced by `dpm pack`, with its name unchanged.

### DPME3006

**The package does not declare any supported platforms.**

The package's spec has no platforms for the compiler being used, so there is nothing to build.

**What to do:** Use a version of the package that supports the compiler, or report it to the package author.

### DPME3007

**A cached package does not match the hash the project recorded for it.**

The project records a hash for each package it uses, and the package in the cache doesn't match it - the
cached package has changed since it was installed. DPM won't load its design-time packages, and leaves the
project unchanged.

**What to do:** Reinstall the package, or delete it from the package cache and restore. If the change is
expected, restore with `--ignoreHashLocks` to record the new hashes.

### DPME3008

**Unable to write to the package cache.**

DPM couldn't create, copy, clear or remove something in the package cache folder. The message names the
file or folder.

**What to do:** Check you can write to the package cache folder, and that nothing (such as the IDE, or
another dpm) has the file open.

### DPME3009

**Unable to read a package spec from the cache.**

A package that should be in the package cache has no readable `package.dspec`.

**What to do:** Delete the package from the package cache and try again, so it is downloaded fresh.

### DPME3010

**Unable to extract the package into the cache.**

The package file couldn't be extracted - it may be damaged, or a file in the cache folder may be locked.

**What to do:** Check nothing has the package's cache folder open, delete the package's `.dpkg` from the
cache, and try again.

### DPMW3011

**Some packages do not support one of the project platforms.**

The project targets a platform that some of its packages don't support. Those packages are skipped for that
platform.

**What to do:** Remove the packages from the project, or use versions that support the platform.

### DPMW3012

**A package spec has no target platform or template for the compiler.**

The package's spec doesn't describe what to add to the search path for the compiler being used, so the
package adds nothing to it.

**What to do:** Report it to the package author.

### DPMW3013

**The package spec could not be read.**

Installing from a package file, DPM couldn't read its spec to find which platforms it supports, so it
installs for all the project's platforms.

**What to do:** Check the package file is a complete package produced by `dpm pack`.

### DPMW3014

**Unable to unload design packages.**

Uninstalling a package couldn't unload its design-time packages from the IDE.

**What to do:** Restart the IDE.

### DPMW3015

**Unable to write the package verification receipt.**

DPM verified and extracted the package, but couldn't save the record of that verification, so it verifies
the package again next time.

**What to do:** Check you can write to the package cache folder.

### DPMW3016

**Unable to write the copy-local targets file.**

DPM couldn't write the msbuild targets file that copies packages' runtime files next to your executable when
you build.

**What to do:** Check you can write to the package cache folder.

## Project files (4xxx)

### DPME4001

**The project file does not exist.**

The project (or project path) given doesn't exist.

**What to do:** Check the path.

### DPME4002

**No project files were found.**

The folder or group project given contains no projects, or none of the projects listed exist.

**What to do:** Check the path, or give the project files explicitly.

### DPME4003

**Unable to load the project file.**

The project or group project isn't valid XML, or couldn't be read. The message says why.

**What to do:** Fix the file (or restore it from version control). Opening and saving it in the IDE often
fixes small problems.

### DPME4004

**The project contains an invalid package reference.**

A package reference DPM recorded in the project is damaged: it's missing its id, version or range, a value
isn't valid, or the same package is listed twice.

**What to do:** Remove the reference from the project's `ProjectExtensions` section and restore (or install
the package again).

### DPME4005

**The project file is missing something DPM needs.**

The project has no `ProjectVersion`, `AppType`, `MainSource`, build configurations, `ProjectExtensions` or
base configuration - or one of them is empty or has a value DPM doesn't recognise.

**What to do:** Open and save the project in the IDE, which writes these elements.

### DPME4006

**Unable to save the project file.**

DPM couldn't write the project file.

**What to do:** Check the file isn't read-only or locked (for example checked out by version control, or
open in another program).

### DPMW4008

**The project does not target the requested platforms.**

The project doesn't target any of the platforms asked for, so it is skipped.

**What to do:** Nothing, if that's what you meant. Otherwise check the `--platforms` option, or add the
platform to the project.

### DPMW4009

**The project version does not identify a single compiler version.**

The project's `ProjectVersion` is used by more than one Delphi version, so DPM can't tell which compiler the
project is for.

**What to do:** Give the compiler version on the command line (`--compiler=<version>`).

### DPMW4010

**The project version does not match the compiler version.**

The project was last saved by a different Delphi version than the one being used.

**What to do:** Nothing, if you're moving the project to that compiler. Otherwise check the `--compiler`
option.

### DPMW4011

**A listed project file does not exist.**

One of the projects given doesn't exist and is skipped.

**What to do:** Check the path.

### DPMW4012

**The project targets a platform DPM does not support.**

The project targets a platform this version of DPM doesn't know about. DPM ignores that platform.

**What to do:** Nothing, unless you need packages for that platform - then check for a newer DPM.

### DPMW4013

**The project DPMCompiler value is not a known compiler version.**

The `DPMCompiler` value DPM recorded in the project isn't a compiler version it recognises. DPM uses the
project's `ProjectVersion` instead.

**What to do:** Nothing - the next restore or install records the right value.

## Compiling packages (5xxx)

### DPME5001

**The Delphi compiler is not installed.**

The package needs to be compiled with a Delphi version that isn't installed on this machine - DPM couldn't
find its install location in the registry.

**What to do:** Install that Delphi version, or use the compiler you have (`--compiler=<version>`).

### DPME5002

**Unable to run msbuild.**

DPM couldn't start msbuild to compile a package.

**What to do:** Check the Delphi installation is complete and that `rsvars.bat` exists in its `bin` folder.

### DPME5003

**A package failed to compile.**

msbuild couldn't compile one of the package's projects. The compiler output is shown just before the error.

**What to do:** Look at the compiler output for the cause. A package that doesn't compile for a compiler or
platform is usually something to report to the package author.

### DPME5004

**A package project could not be inspected before building it.**

DPM checks every package project before building it (see DPME5005), and couldn't read this one, so it won't
build it.

**What to do:** Report it to the package author.

### DPME5005

**A package project would run commands during the build.**

The package's project contains msbuild targets, tasks or imports that would run commands when it's built.
DPM compiles packages, it doesn't run them, so it won't build this package.

**What to do:** Report it to the package author.

### DPME5006

**A prebuilt design package entry is not valid.**

The package's spec points a design entry at a prebuilt `.bpl` in a way DPM can't use. The message says why.

**What to do:** Report it to the package author.

### DPME5007

**A prebuilt design package is missing from the package.**

The package's spec names a prebuilt design `.bpl` that isn't in the package.

**What to do:** Report it to the package author.

### DPME5008

**Unable to generate a package project from its package definition.**

The package describes its package projects with package definitions, and DPM couldn't generate one: its
project path isn't valid, it matches no source files, or the files couldn't be written.

**What to do:** Report it to the package author.

### DPMW5009

**Unable to save the package build record.**

DPM built the package but couldn't save the record that lets it skip the build next time, so it builds the
package again then.

**What to do:** Check you can write to the package cache folder.

### DPMW5010

**Unable to compile design-time packages for the IDE platform.**

The design-time packages are compiled for the platform the IDE runs on as well as the project's platforms,
and that build failed. The project's own platforms are fine, but the design components may not load in the
IDE. The message includes the first problem.

**What to do:** Check the problem the message gives. Run with `--verbosity=Debug` (or set the IDE's DPM log
level to Debug) to see the compiler output.

### DPMW5011

**Unable to add the build configuration to a package project.**

DPM adds the configuration and platform being built to a package's project when it lacks them, and couldn't
for this one. It builds the project anyway.

**What to do:** Nothing, unless the build then fails - then report it to the package author.

### DPMW5012

**A package path resolves outside the package folder.**

A search path or project in the package's spec points outside the package's own folder, so DPM ignores it.

**What to do:** Report it to the package author.

### DPMW5013

**A package project has a build event that DPM does not run.**

The package's project has a build event (such as a post-build command). DPM never runs them, so that step
doesn't happen.

**What to do:** Nothing, unless the package needs that step - then report it to the package author.

### DPMW5014

**Unable to copy a package file.**

DPM couldn't copy one of the package's files (such as a `.dfm`, `.res` or `.dll`) to where it's needed after
the build.

**What to do:** Check nothing has the file open, and that you can write to the package cache folder.

### DPMW5015

**A design project declares no platforms DPM supports.**

A design-time project in the package targets no platforms DPM supports, so no design package is built from
it.

**What to do:** Report it to the package author.

### DPMW5016

**A design project could not be inspected.**

DPM couldn't read a design-time project to find its platforms, so it builds it for Win32 only.

**What to do:** Report it to the package author.

### DPMW5017

**A package definition has no platforms.**

One of the package's package definitions has no platforms in common with the package's target platforms, so
it's skipped.

**What to do:** Report it to the package author.

### DPMW5018

**A package project had no configuration for the platform being built.**

The package's project had no settings for the platform being built. DPM added the configuration, but there
are no platform specific settings, so the build may still fail.

**What to do:** Nothing, unless the build then fails - then report it to the package author.

## Signing, trust and verification (7xxx)

### DPME7001

**The package failed verification.**

The package's signature or contents didn't verify under your trust policy. DPM won't install it. The message
says why.

**What to do:** Check where the package came from. If you trust it, review your trust policy (the `signing`
section of the configuration).

### DPME7002

**The package is blocked.**

You chose to block this package permanently, during an earlier install or this one.

**What to do:** To lift the block, remove the package's entry from `%APPDATA%\.dpm\trust-state.yaml`.

### DPME7003

**The package author signature changed and the install was blocked.**

The package is signed by a different author key than before, or is no longer signed by its author, and the
install was blocked - by your `authorDowngradePolicy`, or because you declined it when asked.

**What to do:** Check the change is genuine. To allow it, set `authorDowngradePolicy` to `allow`, or accept
the change when asked.

### DPME7004

**The package no longer carries its repository signature and the install was blocked.**

Earlier versions of this package carried a trusted repository's signature and this one doesn't (or the
repository vouches for a different namespace), and the install was blocked.

**What to do:** Check where the package came from. If the change is genuine, accept it when asked, or edit
`%APPDATA%\.dpm\trust-state.yaml` to remove the package's repository entry.
