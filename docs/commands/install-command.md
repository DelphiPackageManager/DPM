# install command (DPM CLI)

Downloads and installs a package using the specified sources. If no sources are specified, all sources defined in the DPM configuration file are used. If the configuration file specifies no sources, uses the default DPM feed.
If no sources are specified, those listed in the global configuration file, `%appdata%\.dpm\dpm.Config`

## Usage

```cli
dpm install <packageID> [options]
```

where `<packageID>` names the package to install (using the latest version). You can indicate a specific version with the `-Version` option. Alternatively, use the packageFile option to specify the package.

## Options

| Option      | Description                                                                                                                                              |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| compiler    | The Delphi compiler version to target. If not specified, the version is detected from the .dproj file. See [compiler-versions](../compiler-versions.md). |
| configFile  | The dpm configuration file to apply. If not specified, `%AppData%\.dpm\dpm.Config` is used.                                                              |
| force       | Force install of package (acts as restore).                                                                                                              |
| help        | Displays help information for the command.                                                                                                               |
| packageFile | The path to a package file (.dpkg).                                                                                                                      |
| platforms   | The platforms to install for (comma-separated). Default is to install for all platforms the project targets. See [platforms](../platforms.md).           |
| projectPath | The path to a dproj or groupproj, or a folder containing 1 or more dproj files. Defaults to current directory.                                           |
| preRelease  | Allows prerelease packages to be installed.                                                                                                              |
| sources     | Specifies a comma-separated list of package sources to use. If omitted, the command uses the sources provided in configuration files.                    |
| verbosity   | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.                                                                   |
| version     | Specifies the version of the package to install.                                                                                                         |

## Examples

```cli
dpm install VSoft.CommandLine

dpm install VSoft.CommandLine -version=1.0.1 -projectPath=c:\myprojects\project1.dproj

dpm install Spring.Base -projectPath=c:\myprojects -target=10.3

dpm install DUnitX.Framework -projectPath=c:\myproject\tests\mytest.dproj -compiler=10.3 -platforms=Win32,Win64,OSX32
```
