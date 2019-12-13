# DPM CLI Commands

This list is the implemented and planned cli commands. Subject to change.

Note you can get help for each command from the command line by invoking

```cli
dpm help <command>
```

| Command                                      | Description                                                                                                                                                                                                          | Implemented |
| -------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- |
| [cache](./commands/cache-command.md)         | Downloads a package to the local package cache using the specified sources. If no sources are specified, those listed in the global configuration file, `%appdata%\DPM\dpm.Config`                                   | yes         |
| [config](./commands/config-command.md)       | Gets or sets DPM config values.                                                                                                                                                                                      | no          |
| [delete](./commands/delete-command.md)       | Deletes a package from the server.                                                                                                                                                                                   | no          |
| help                                         | Get help                                                                                                                                                                                                             | yes         |
| [install](./commands/install-command.md)     | Installs a package using the specified sources. If no sources are specified, all sources defined in the DPM configuration file are used. If the configuration file specifies no sources, uses the default DPM feed.  | yes         |
| [list](./commands/list-command.md)           | Displays a list of packages from a given source. If no sources are specified, all sources defined in %AppData%\DPM\DPM.config are used. If DPM.config specifies no sources, uses the default DPM feed.               | yes         |
| [pack](./commands/pack-command.md)           | Creates a DPM package based on the specified .dspec file.                                                                                                                                                            | yes         |
| [push](./commands/push-command.md)           | Pushes a package to a package source and publishes it.                                                                                                                                                               | yes         |
| [remove](./commands/remove-command.md)       | Removes a package from a project.                                                                                                                                                                                    | no          |
| [restore](./commands/restore-command.md)     | Restores package(s) using the specified sources. If no sources are specified, all sources defined in the DPM configuration file are used. If the configuration file specifies no sources, uses the default DPM feed. | yes         |
| [setapikey](./commands/setapikey-command.md) | Saves an API key for a given server URL. When no URL is provided API key is saved for the DPM gallery.                                                                                                               | no          |
| [sign](./commands/sign-command.md)           | Signs all the packages matching the first argument with a certificate.                                                                                                                                               | no          |
| [sources](./commands/sources-command.md)     | Provides the ability to manage list of sources located in %AppData%\DPM\dpm.config                                                                                                                                   | yes         |
| [spec](./commands/spec-command.md)           | Generates a package.dspec for a new package. If this command is run in the same folder as a project file (.dproj), it will create a tokenized dspec file.                                                            | no          |
| [update](./commands/update-command.md)       | Updates a package using the specified sources. If no sources are specified, all sources defined in the DPM configuration file are used. If the configuration file specifies no sources, uses the default DPM feed.   | no          |
| [verify](./commands/verify-command.md)       | Verifies the signature of the specified package files.                                                                                                                                                               | no          |
| [why](./commands/why-command.md)             | Explains why a package is referenced.                                                                                                                                                                                | no          |
