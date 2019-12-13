# sources command (DPM CLI)

Manages the list of sources located in the user scope configuration file or a specified configuration file. The user scope configuration file is located at `%appdata%\.dpm\dpm.Config`.

## Usage

```cli
dpm  sources <operation> -Name <name> -Source <source>
```

where `<operation>` is one of _List, Add, Remove, Enable, Disable,_ or _Update_, `<name>` is the name of the source, and `<source>` is the source's URL. You can operate on only one source at a time.

## Options

| Option     | Description                                                                                 |
| ---------- | ------------------------------------------------------------------------------------------- |
| configFile | The DPM configuration file to apply. If not specified, `%AppData%\.dpm\dpm.Config` is used. |
| format     | Applies to the `list` action and can be `Detailed` (the default) or `Short`.                |
| help       | Displays help information for the command.                                                  |
| password   | Specifies the password for authenticating with the source.                                  |
| userName   | Specifies the user name for authenticating with the source.                                 |
| verbosity  | Specifies the amount of detail displayed in the output: _normal_, _quiet_, _detailed_.      |

> [!Note]
> Make sure to add the sources' password under the same user context as the dpm.exe is later used to access the package source. The password will be stored encrypted in the config file and can only be decrypted in the same user context as it was encrypted. So for example when you use a build server to restore DPM packages the password must be encrypted with the same Windows user under which the build server task will run.

## Examples

```cli
dpm  sources Add -Name "MyServer" -Source \\myserver\packages

dpm  sources Disable -Name "MyServer"

dpm  sources Enable -Name "dpm.org"

dpm  sources add -name foo.bar -source C:\dpm\local -username foo -password bar -configfile %AppData%\DPM\my.config
```
