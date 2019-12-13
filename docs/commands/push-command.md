# push command (DPM CLI)

Pushes a package to a package source and publishes it.

## Usage

```cli
dpm push <packagePath> [options]
```

where `<packagePath>` identifies the package to push to the server.

## Options

| Option        | Description                                                                                                                                        |
| ------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| apiKey        | The API key for the target repository. If not present, the one specified in the config file is used.                                               |
| configFile    | The DPM configuration file to apply. If not specified, `%AppData%\.dpm\dpm.config` is used.                                                        |
| help          | Displays help information for the command.                                                                                                         |
| source        | Specifies the server URL. DPM identifies a UNC or local folder source and simply copies the file there instead of pushing it using HTTP. Required. |
| skipDuplicate | If a package and version already exists, skip it .                                                                                                 |
| timeout       | Specifies the timeout, in seconds, for pushing to a server. The default is 300 seconds (5 minutes).                                                |
| verbosity     | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.                                                             |

## Examples

```cli
dpm push .\VSoft.SemanticVersion.1.0.1.dpkg -source=local

dpm push .\VSoft.SemanticVersion.1.0.1.dpkg -source=corporate -apiKey=abcdef
```
