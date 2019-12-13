# restore command (DPM CLI)

Downloads and installs and package(s) missing from the project (.dproj) using the specified sources. If no sources are specified, all sources defined in the DPM configuration file are used. If the configuration file specifies no sources, uses the default DPM feed.

If a project group is specified, this command downloads and installs the missing DPM packages that are listed in each project in the project group.

If a folder is specified, then this command applies to all .dproj files in the folder.

## Usage

```cli
dpm restore <project | grouproj | folder> [options]
```

## Options

| Option     | Description                                                                                 |
| ---------- | ------------------------------------------------------------------------------------------- |
| configFile | The DPM configuration file to apply. If not specified, `%AppData%\.dpm\dpm.Config` is used. |
| help       | Displays help information for the command.                                                  |
| Source     | Optional source(s), if not specified all sources will be used.                              |
| Verbosity  | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.      |

Also see [Environment variables](cli-ref-environment-variables.md)

## Examples

```cli
  dpm resore .\MyProject.dproj

  dpm resore c:\Projects\MyProject.dproj -source=local

  dpm resore c:\Projects -source=local

  dpm resore c:\Projects\AllProjects.groupproj -source=local

  dpm resore c:\Projects\AllProjects.groupproj -configFile=.\dev.dpm.config
```
