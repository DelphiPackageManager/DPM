# cache command (DPM CLI)

Downloads a package to the local package cache using the specified sources. If no sources are specified, those listed in the global configuration file, `%appdata%\.dpm\dpm.Config`

## Usage

```cli
dpm cache <packageID> [options]
```

where `<packageID>` names the package to download (using the latest version). You can indicate a specific version with the `-Version` option. Alternatively, uset the packageFile option to specify the package.

## Options

| Option     | Description                                                                                                                           |
| ---------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| compiler   | The delphi compiler version to target. Required. See [compiler-versions](../compiler-versions.md).                                    |
| configFile | The dpm configuration file to apply. If not specified, `%AppData%\.dpm\dpm.Config` is used.                                           |
| help       | Displays help information for the command.                                                                                            |
| platforms  | The platforms to download (comma separated). Default is to download for all platforms available. See [platforms](../platforms.md).    |
| preRelease | Allows prerelease packages to be downloaded.                                                                                          |
| sources    | Specifies a comma-separated list of package sources to use. If omitted, the command uses the sources provided in configuration files. |
| verbosity  | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.                                                |
| version    | Specifies the version of the package to download.                                                                                     |

## Examples

```cli
dpm cache VSoft.CommandLine -compiler=10.3

dpm cache VSoft.CommandLine -compiler=10.3 -version=1.0.1

dpm cache Spring.Base -compiler=10.3

dpm cache DUnitX.Framework -compiler=10.3 -platforms=Win32,Win64,OSX32
```
