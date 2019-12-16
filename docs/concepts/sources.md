# Package Sources

A Package Source is either a folder or an HTTP service which contains package files, and allows for searching, downloading or uploading of package files. Currently, only folder-based package sources are implemented.

Multiple package sources can be defined, and they will be searched in the order they are defined. So when installing a package, if I don't specify a package version, each registered package source will be searched, and the highest version chosen.

## Managing Package Sources

Package sources are listed in your [dpm.config](./config-file.md). It is recommended you manage them via the dpm cli [sources command](../commands/sources-command.md).

**Adding a Package Source**

```cli
dpm sources add -name=local -source=\\myserver\dpmsource
```
