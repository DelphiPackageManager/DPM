# Pack command (DPM CLI)

Creates a DPM package based on the specified [.dspec](../dpec.md).

## Usage

```cli
dpm pack <.dspec file> [options]
```

## Options

| Option                  | Description                                                                                                                                                                  |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| basePath                | Sets the base path of the files defined in the [.dspec](../dspec.md) file.                                                                                                   |
| excludeEmptyDirectories | Prevents inclusion of empty directories when building the package.                                                                                                           |
| configFile              | Specify the configuration file for the pack command.                                                                                                                         |
| help                    | Displays help information for the command.                                                                                                                                   |
| minClientVersion        | Set the _minClientVersion_ attribute for the created package. This value will override the value of the existing _minClientVersion_ attribute (if any) in the `.dspec` file. |
| outputFolder            | Specifies the folder in which the created package is stored. If no folder is specified, the current folder is used.                                                          |
| properties              | Provides the ability to specify a semicolon ";" delimited list of properties (name=value) when creating a package.                                                           |
| verbosity               | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.                                                                                       |
| version                 | Overrides the version number from the `.dspec` file.                                                                                                                         |

## Examples

```cli
dpm pack

dpm pack foo.dspec

dpm pack foo.dspec -Properties Configuration=Release

dpm pack foo.dspec -Version 2.1.0
```
