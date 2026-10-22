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

### DPME0005

**Unable to create a folder.**

DPM couldn't create a folder it needed, such as an output folder or a temporary working folder. The message names the folder and says why.

**What to do:** Check the path is valid and that you can write to its parent folder.

### DPME0101

**A required argument is missing.**

The command needs something it wasn't given, such as the spec file to pack or the api key to push with. The
message says what.

**What to do:** Pass the missing argument. `dpm help <command>` lists a command's arguments.

### DPME0102

**An argument is not valid.**

A value given on the command line isn't one the command accepts. The message says which and why.

**What to do:** Correct the value. `dpm help <command>` lists what each argument accepts.

### DPME0103

**Arguments that cannot be used together were given.**

Two of the arguments contradict each other, for example `--thumbprint` and `--pfx`, or a version given both with `--version` and as `id@version`. The message says which.

**What to do:** Remove one of them.

### DPME0104

**The command line could not be parsed.**

An option on the command line isn't one DPM knows, or it is missing its value. Each problem is reported separately, followed by the help for the command.

**What to do:** Correct the option. `dpm help <command>` lists a command's options.

### DPME0105

**The command is not known.**

The first word on the command line isn't a DPM command.

**What to do:** Check the spelling. `dpm help` lists the commands.

### DPME0106

**The command is not implemented yet.**

The command is reserved for a future version of DPM and does nothing yet.

**What to do:** Use another way to do what you need - `dpm help` lists the commands that work.

### DPME0107

**A confirmation is needed but there is no console to ask.**

The command asks before it does something that can't be undone, but its input is redirected (a script, a CI job or another program running DPM), so nobody can answer.

**What to do:** Run it in a console, or pass the option the message names (eg `--force`) to go ahead without asking.

### DPME0201

**Unable to load the configuration file.**

The DPM configuration file couldn't be read - it may not be valid YAML, or it can't be opened. The message
names the file and says why.

**What to do:** Fix the file, or move it aside so DPM creates a new default one.

### DPME0202

**Unable to save the configuration file.**

DPM couldn't write the configuration file.

**What to do:** Check you can write to the file and its folder (by default `%APPDATA%\.dpm`).

### DPME0203

**The configuration file does not exist.**

The command needs a configuration file, and the one it was given (or the default one) doesn't exist.

**What to do:** Check the `--configFile` path, or add a source with `dpm sources add`, which creates the file.

### DPME0301

**Unable to check for a newer version of DPM.**

`dpm upgrade` couldn't get the list of DPM releases from GitHub, or the newest release has no installer attached. The message says why - GitHub limits how often a computer can ask, so `rate limited` usually just means trying again later.

**What to do:** Check you can reach https://api.github.com, then try again.

### DPMW0302

**The check for a newer version of DPM could not be made.**

The IDE plugin checks for newer DPM versions in the background and couldn't reach GitHub. Nothing else is affected.

**What to do:** Nothing, unless it keeps happening while you are online - then check you can reach https://api.github.com.

### DPME0303

**Unable to download the DPM installer.**

The installer for the new version couldn't be downloaded or saved. The message says why.

**What to do:** Check you can reach github.com and write to your temp folder, then try again. You can also download the installer from the DPM releases page.

### DPME0304

**Unable to start the DPM installer.**

The installer was downloaded but couldn't be started.

**What to do:** Run the installer from the path in the message yourself.

### DPMW0305

**Unable to write the update check cache.**

DPM remembers the result of its last update check for a while so it doesn't ask GitHub every time. It couldn't save that, so it will just check again next time.

**What to do:** Check you can write to `%APPDATA%\.dpm`.

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

### DPME1005

**A source with that name already exists.**

`dpm sources add` was given a name that is already used by a configured source.

**What to do:** Pick another name, or change the existing source with `dpm sources update`.

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

### DPME4007

**The compiler version of the project could not be determined.**

The project has no `DPMCompiler` property and its `ProjectVersion` doesn't identify a single Delphi version.

**What to do:** Open and save the project in the Delphi version it is for, or install a package into it, which records the compiler.

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

One of the projects given, or listed in a .groupproj, doesn't exist and is skipped.

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

### DPME4014

**The project has no enabled platforms.**

The project file doesn't enable any platform, so there is nothing to do for it.

**What to do:** Enable a platform in the project (Project Manager > Target Platforms) and save it.

### DPME4015

**The project does not target any of the requested platforms.**

None of the platforms given with `--platforms` is enabled in the project.

**What to do:** Pass platforms the project targets, or leave `--platforms` out to use all of them.

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

## Package specs, pack, push and prepare (6xxx)

### DPME6001

**The spec file does not exist.**

The `.dspec` file given (or the one found) doesn't exist.

**What to do:** Check the path.

### DPME6002

**The spec file could not be read.**

The spec isn't valid YAML, doesn't have the `.dspec` or `.dspec.yaml` extension, or has no `metadata` - so it
probably isn't a spec at all. The message says why.

**What to do:** Fix the file. DSpec Creator can open and save a spec to tidy it.

### DPME6003

**The spec is missing something it needs.**

A required element or value isn't in the spec - for example `metadata`, `version`, `description`, `authors`,
`targetPlatforms`, `platforms`, `templates`, a template's `name`, or an entry's `project`, `src` or `files`.
The message says which.

**What to do:** Add it to the spec. `docs\dspec-format.md` describes every element.

### DPME6004

**A value in the spec is not valid.**

A value in the spec isn't one DPM accepts - an invalid package id or version, an unknown compiler or platform,
a dependency range that isn't valid, or overlapping target platforms. The message says which.

**What to do:** Correct the value. `docs\dspec-format.md` describes what each element accepts.

### DPME6005

**The spec needs a newer version of DPM.**

The spec's `min dpm client version` is later than the version of DPM being used.

**What to do:** Update DPM (`dpm upgrade`).

### DPMW6006

**A platform in the spec is not valid for its compiler.**

Writing the spec out, a target platform lists a platform its compiler doesn't support, so it's left out.

**What to do:** Remove the platform from that target platform.

### DPME6101

**The spec has no template with that name.**

A target platform names a template that isn't in the spec's `templates`.

**What to do:** Correct the target platform's `template`, or add the template.

### DPME6102

**A template sets an environment variable DPM does not allow.**

A template's environment variables include one DPM reserves (one the IDE itself sets, for example).

**What to do:** Rename or remove the variable.

### DPME6103

**Unable to replace the variables in the spec.**

Replacing `$variable$` tokens in the spec failed. The message says why.

**What to do:** Check the spec's `variables` and the `--variables` given to pack.

### DPMW6104

**A file pattern in the spec matched no files.**

A source pattern in the spec (or a folder it names) matched nothing, so nothing is added for it.

**What to do:** Check the pattern and the base path. A pattern that should match nothing can be removed.

### DPME6105

**A file the spec refers to does not exist.**

The spec's `icon` or `readme` file doesn't exist.

**What to do:** Correct the path in the spec, or add the file.

### DPME6106

**Unable to write the package file.**

DPM couldn't create the `.dpkg` file in the output folder.

**What to do:** Check you can write to the output folder and that nothing has the file open.

### DPME6107

**The package written is not valid.**

The package was written, but failed DPM's checks on what a package may contain, so it was deleted. The
message names the entry.

**What to do:** Change the spec so the package doesn't include that entry, or report it as a DPM bug if the
entry looks fine.

### DPME6108

**Unable to write the package manifest.**

DPM couldn't add the manifest, which lists every file and its hash, to the package. The package was deleted.

**What to do:** Check there is space in the output folder and try again.

### DPME6109

**Git registry packages cannot be packed.**

The spec is for a git registry package (`packageKind: git`). Those are installed from their repository and
aren't packed.

**What to do:** Pack a `dpm` package instead, or publish the git package through a registry.

### DPMW6110

**A source pattern in the spec uses a variable that is not defined.**

A source pattern still has a `$variable$` token after the spec's variables were applied, so it's skipped.

**What to do:** Define the variable in the spec's `variables`, or correct the pattern.

### DPME6201

**The server rejected the package.**

The package server refused the push - the version may already exist, the api key may not allow it, or the
package may not be valid for the server. The message has what the server said.

**What to do:** Act on the server's message. To push a version that already exists without an error, use
`--skipDuplicate`.

### DPME6202

**The source does not accept pushed packages.**

The source is a git registry, which packages can't be pushed to.

**What to do:** Push to a folder source or a DPM server.

### DPME6203

**The source to push to is disabled.**

The source named for the push is disabled in the configuration.

**What to do:** Enable it with `dpm sources enable`, or push to another source.

### DPMW6301

**A file prepare would write already exists.**

`dpm prepare` didn't overwrite a package project that already exists.

**What to do:** Nothing, if you meant to keep it. Pass `--force` to replace it.

### DPMW6302

**The spec declares no platforms.**

`dpm prepare` found no platforms for a compiler in the spec, so it uses Win32 only.

**What to do:** Add the platforms to the spec's target platform.

### DPME6401

**A spec file already exists.**

`dpm spec` would replace the .dspec file that is already in the folder, and it was run non-interactively so it can't ask.

**What to do:** Pass `--overwrite` to replace it, or run it in a console to be asked.

### DPME6402

**No source folder was found to create the spec from.**

`dpm spec` looks for a `Source` folder, or .pas files in the current folder, and found neither. It was run non-interactively so it can't ask where the source is.

**What to do:** Run it from the folder that holds your library, or run it in a console to be asked.

### DPMW6403

**The packages folder does not exist.**

The packages folder you gave `dpm spec` doesn't exist, so the spec gets no build or design entries.

**What to do:** Run it again with the right folder, or add the entries to the spec yourself.

### DPMW6404

**No compiler folders were found in the packages folder.**

`dpm spec` expects one folder per Delphi version in the packages folder (eg `Rad Studio 12.0`), and didn't recognise any, so the spec gets no build or design entries.

**What to do:** Add the entries to the spec yourself, or rename the folders after the Delphi versions they are for.

### DPMW6405

**No author is set in the configuration.**

`dpm spec` fills in the author from the DPM configuration, and none is set there. It was run non-interactively so it can't ask.

**What to do:** Fill in `authors` in the spec, or run it once in a console - it asks and saves the answer.

### DPME6406

**Unable to write the spec file.**

The .dspec file couldn't be written. The message names the file and says why.

**What to do:** Check you can write to the folder.

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

### DPMW7005

**The package repository signature changed.**

Earlier versions of this package carried a trusted repository's signature, and this one doesn't, or the
repository vouches for a different namespace. You are asked whether to continue (where DPM can ask).

**What to do:** Check where the package came from before accepting the change.

### DPMW7006

**The package author signature changed.**

The package is signed by a different author key than before, or is no longer signed by its author. You are
asked whether to continue, or your `authorDowngradePolicy` decides.

**What to do:** Check the change is genuine before accepting it - an author changing keys normally says so.

### DPMW7007

**A rebuild by the package author, without a repository signature, was allowed.**

The package has no trusted repository signature, but is signed by the same author key as before, so DPM
treats it as the author's own rebuild and installs it.

**What to do:** Nothing, if you built the package yourself. Otherwise check where it came from.

### DPMW7008

**The package signer is not a trusted publisher.**

The package is signed, but by a publisher that isn't in your `trustedPublishers`. Permissive mode allows
installing it.

**What to do:** If you trust the publisher, add them with `dpm trust add publisher`.

### DPMW7009

**A signature self-check failed.**

While signing with a remote provider, DPM's check of the signature it assembled found a mismatch. The
signature may not verify. The message has the details.

**What to do:** Verify the signed package with `dpm verify`, and report the warning with its details.

### DPMW7010

**The package is not signed.**

`dpm verify` found no signatures on the package. Permissive mode allows installing it.

**What to do:** Nothing, unless you require signed packages - then sign it, or use a signed version.

### DPME7101

**Unable to set up the signing provider.**

DPM couldn't create the signing provider: the certificate wasn't found, the PFX file has no certificate, or a
secret or api key it needs isn't set. The message says which.

**What to do:** Check the signing options (thumbprint, PFX file and password, Azure Key Vault or Signotaur
settings) and the environment variables they name.

### DPME7102

**Unable to sign the package.**

Signing a package failed - the provider returned an error, or the package can't be author-signed (for
example, it bundles binaries that aren't Authenticode-signed). The message says why.

**What to do:** Act on the message. For a provider error, check the provider's credentials and that it can be
reached.

### DPME7103

**A signature could not be built or read.**

Building or reading a CMS signature failed. The message says why.

**What to do:** For a package you're signing, try again and report it if it persists. For a package you're
installing, the signature is damaged - get the package again.

### DPME7104

**A certificate could not be used.**

A certificate couldn't be read, or doesn't meet DPM's requirements for signing or verification.

**What to do:** Check the certificate (and its private key, for signing).

### DPME7105

**Unable to get a timestamp for the signature.**

The timestamp authority couldn't be reached, or didn't return a timestamp.

**What to do:** Check the `--timestamper` url and your network, then sign again.

### DPME7106

**A cryptographic operation failed.**

Hashing, or another cryptographic operation, failed - often because the algorithm isn't supported.

**What to do:** Use SHA256, SHA384 or SHA512. Report it if it persists.

### DPME7107

**The package manifest is not valid.**

The package's manifest, which lists its files and their hashes, is damaged or doesn't match the package.

**What to do:** Get the package again. If you produced it, pack it again.

### DPME7108

**Unable to read or write the trust state.**

DPM couldn't read or save its record of the signatures it has seen (`%APPDATA%\.dpm\trust-state.yaml`).

**What to do:** Check the `APPDATA` environment variable is set, and that you can write to the file.
## SBOM and vulnerability scanning (8xxx)

### DPME8001

**Vulnerabilities at or above the --fail-on severity were found.**

`dpm scan` found a known vulnerability in a component, at or above the severity given with `--fail-on`. The VEX report lists what was found and the versions that fix it.

**What to do:** Update the affected packages to a version that fixes the vulnerability, or record why it doesn't affect you and raise `--fail-on`.

### DPME8002

**Unable to read the SBOM.**

The SBOM given to `dpm scan` couldn't be read - it doesn't exist, isn't valid JSON, or isn't a CycloneDX 1.x SBOM.

**What to do:** Check the file, or let `dpm scan` generate the SBOM by passing the .dproj or .groupproj instead.

### DPME8003

**Unable to query the vulnerability database.**

The vulnerability database (osv.dev) couldn't be reached, or its answer couldn't be read. DPM never reports a scan as clean when it couldn't ask.

**What to do:** Check you can reach https://api.osv.dev, then try again.

### DPME8004

**Unable to write the vulnerability report.**

The VEX report couldn't be written. The message names the file and says why.

**What to do:** Check you can write to the output folder.

### DPMW8005

**Unable to use the vulnerability cache.**

DPM keeps vulnerability database answers for a while to avoid asking again. A cached answer couldn't be read or saved, so DPM asked the database instead.

**What to do:** Nothing, unless it keeps happening - then delete the cache folder named in the message.

### DPME8101

**The SBOM product information is not valid.**

The product information for the SBOM - from the options or the project's `.sbom.yaml` file - has a value that can't be used, such as an SBOM creator that is neither an email address nor an http(s) URL, or a `.sbom.yaml` that isn't a list of `key: value` pairs.

**What to do:** Correct the value the message names.

### DPME8102

**The SBOM product information file could not be read.**

The project's `.sbom.yaml` file exists but isn't valid YAML, or can't be opened.

**What to do:** Fix the file, or delete it.

### DPMW8103

**An entry in the SBOM product information was ignored.**

The `.sbom.yaml` file has a key DPM doesn't know, or a value of the wrong kind (a list where a single value belongs). The rest of the file is used.

**What to do:** Correct or remove the entry the message names.

### DPMW8104

**An SBOM component rule is not valid.**

A component rule in the `.sbom.yaml` file has no name (it is skipped), an unknown type (library is used), or a unit prefix so short it may claim other libraries' units.

**What to do:** Correct the rule the message names.

### DPMW8105

**An SBOM component rule matched no unit.**

A component rule in the `.sbom.yaml` file didn't match any unit in the MAP file. The library may no longer be used, or the rule's units or unit prefixes are wrong.

**What to do:** Remove the rule, or correct its `units` / `unitPrefixes`.

### DPMW8106

**The SBOM is missing product information CRA/BSI ask for.**

The SBOM was written, but it has no supplier, licence or version for the product, which the EU Cyber Resilience Act and BSI TR-03183-2 expect. The message names the `.sbom.yaml` file to fill in.

**What to do:** Add the missing values to the `.sbom.yaml` file, or pass them as options.

### DPMW8107

**The built binary was not found.**

The SBOM was written, but the file the project builds wasn't found, so the SBOM has no hash for it and no import evidence.

**What to do:** Build the configuration and platform the SBOM is for first, or check `--config` and `--platforms`.

### DPMW8108

**A binary could not be read.**

A built binary, or a package binary it ships, couldn't be hashed or its imports read. The SBOM is written without that detail.

**What to do:** Check the file isn't locked or damaged, then try again.

### DPMW8109

**Project settings the SBOM uses could not be read.**

The project's version information or search path couldn't be read, so the SBOM goes without it.

**What to do:** Check the project opens in the IDE. Run with `--verbosity=Debug` for more detail.

### DPMW8110

**The requested build configuration is not in the project.**

The configuration given with `--config` isn't defined in the project, so DPM used Release, then Debug, then the first one there is.

**What to do:** Pass a configuration the project defines.

### DPME8112

**Unable to list the packages for the SBOM.**

Something went wrong while DPM worked out which packages the project uses, so no SBOM was written for that platform - one without its packages would be wrong.

**What to do:** Restore the project (`dpm restore`) and try again. If it persists, report it as a DPM bug.

### DPME8113

**The MAP file is required and could not be read.**

`--strict` was given, and the linker MAP file - the evidence for components that aren't DPM packages - is missing or couldn't be read.

**What to do:** Enable the linker option "Map file = Detailed", build the project, and try again. Or pass the MAP file with `--map`.

### DPMW8114

**The MAP file was not found.**

The linker MAP file wasn't found, so the SBOM lists the DPM packages and the Delphi runtime only.

**What to do:** Enable the linker option "Map file = Detailed" and build the project to include other components, or pass the MAP file with `--map`.

### DPME8115

**Unable to write the SBOM.**

An SBOM file couldn't be written. The message names the file and says why.

**What to do:** Check you can write to the output folder.

### DPMW8116

**The MAP file could not be read.**

The linker MAP file was found but couldn't be read, so the SBOM lists the DPM packages and the Delphi runtime only.

**What to do:** Build the project again to regenerate the MAP file.

## The IDE plugin, DSpec Creator and the MCP server (9xxx)

### DPMW9101

**A design package could not be loaded into the IDE.**

The package was installed, but the IDE refused to load its design package (.bpl), so its components and editors aren't available. The message names the file and says why when the IDE did.

**What to do:** Check the design package was built for this Delphi version and IDE bitness, and that the packages it needs can be found. Restore the project to try again.

### DPMW9102

**The design package to load into the IDE was not found.**

The package declares a design package, but the .bpl wasn't where DPM expected it after the build, so it wasn't loaded.

**What to do:** If you publish the package, set `libPrefix` / `libSuffix` on the design entry to match the name the compiled .bpl gets, or check the path of a prebuilt one.

### DPMW9103

**More than one file could be the design package to load.**

Several .bpl files match the package's design project, and DPM won't guess - loading the wrong one into the IDE is worse than loading none.

**What to do:** If you publish the package, set `libPrefix` / `libSuffix` on the design entry so it names one file.

### DPMW9104

**A design package with the same name is already loaded from another location.**

The IDE already has a design package with this file name loaded from somewhere other than the DPM package cache - usually the same library installed by hand. DPM leaves it alone.

**What to do:** Remove the hand-installed copy (Component > Install Packages) if you want DPM to manage it.

### DPMW9105

**Packages set an IDE environment variable to different values.**

Two or more packages declare the same IDE environment variable with different values. The value from the package loaded last is used.

**What to do:** Check which value the packages need - this usually means two versions of one library are in use.

### DPMW9106

**Design packages left by a previous IDE session could not be cleaned up.**

At startup DPM unloads design packages, and removes Known Packages registry entries, that a previous session left pointing into the package cache. One of them couldn't be removed.

**What to do:** Remove it in Component > Install Packages. It does no harm unless the file is gone.

### DPMW9201

**The SPDX licence list could not be loaded.**

DSpec Creator couldn't read its list of SPDX licence ids, so the licence list is empty. You can still type a licence id.

**What to do:** Report it - the list is built into DSpec Creator.

### DPMW9202

**The tested package could not be removed from the package cache.**

After a test install DSpec Creator removes the package it tested from the package cache, so the next test uses the newly packed one. That failed.

**What to do:** Remove it with `dpm cache remove <id> --version=<version> --force`.

### DPMW9501

**The MCP server stopped because its connection failed.**

DPM couldn't write to the MCP client - usually because the client went away without closing the connection.

**What to do:** Nothing - restart the server from the client.

### DPMW9502

**Unable to write the MCP frame log.**

The file given with `--logfile` couldn't be written, so frame logging is off for the session. The server itself is unaffected.

**What to do:** Check the path, and that you can write to it.
