# list command (DP<> CLI)

Displays a list of packages from a given source. If no sources are specified, all sources defined in the global configuration file, `%AppData%\.dpm\dpm.config` are used. If `dpm.config` specifies no sources, then `list` uses the default feed (tbd).

## Usage

```cli
dpm list [search terms] [options]
```

where the optional search terms will filter the displayed list. Search terms are applied to the names of packages (folder), tags and package descriptions (HTTP feeds).

## Options

| Option          | Description                                                                                                                                  |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| allVersions     | List all versions of a package. By default, only the latest package version is displayed.                                                    |
| configFile      | The dpm configuration file to apply. If not specified, `%AppData%\.dpm\dpm.Config` is used.                                                  |
| compiler        | Compiler version. When not specified, all compiler versions found are listed.                                                                |
| exact           | Search for an exact packageId match.                                                                                                         |
| help            | Displays help information for the command.                                                                                                   |
| includeDelisted | Display unlisted packages.                                                                                                                   |
| preRelease      | Includes prerelease packages in the list.                                                                                                    |
| source          | Specifies a sources to search, option may be specified multiple times to add extra sources, if ommitted all registered sources are searched. |
| skip            | Skip over x results before listing.                                                                                                          |
| take            | Take max x results.                                                                                                                          |
| Verbosity       | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.                                                       |

## Examples

```cli
dpm list "commandline"

dpm list "semantic" -prerelease -skip=10 -take=10'

dpm list "commandline" -compiler=10.2 -platforms=Win32,Win63,OSX32 -source=VSoftInternal -prerelease
```
