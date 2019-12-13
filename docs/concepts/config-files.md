# DPM Config files

dpm.config file are json format files which store the location of the package cache, along with the registered sources.

The default config file lives in `%APPDATA%\.dpm\dpm.config` - it will be created when first accessed.

Most [dpm cli commmands](../commands.md) have a -configFile option. If that is not specified, then dpm will look for a dpm.config file in the current folder. If that is not found, then the default config file will be used.

**Note** : DPM is still in development, and the config file format is subject to change. Where possible, use the dpm cli to interact with and modify the config files.

| Property             | Value                                                                                        |
| -------------------- | -------------------------------------------------------------------------------------------- |
| packageCacheLocation | The location of the default global packages folder. The default is `%APPDATA%\.dpm\packages` |
| packagesSources      | An array of packageSource objects                                                            |

**packageSource**

| Property | Value                                                            |
| -------- | ---------------------------------------------------------------- |
| name     | The name of the source                                           |
| source   | The uri of the packageSource                                     |
| enabled  | Whether the source is enabled or not (boolean)                   |
| apiKey   | The push apiKey for a http packageSource                         |
| userName | The userName for an authenticated http packageSource             |
| password | The password (encrypted) for an authenticated http packageSource |

**Example**

Note - do not read too much into the domains below, they are just for illustration purposes.

```json
{
  "packageCacheLocation": "C:\\Users\\vincent\\.dpm\\packages",
  "packageSources": [
    {
      "name": "local",
      "source": "I:\\DPMFeed",
      "enabled": true
    },
    {
      "name": "tdpm.org",
      "source": "https://dpm.org/api/v1",
      "enabled": false,
      "apiKey": "6E29A27D0870B988C9FB2B89D4A0C3E7"
    },
    {
      "name": "testServer",
      "source": "https://testServer/api/v1",
      "enabled": false,
      "userName": "JoeUser",
      "password": "6E29A27D0870B988C9FB2B89D4A0C3E7"
    },
    {
      "name": "corporate",
      "source": "\\colossus\\dpmfeed",
      "enabled": true
    }
  ]
}
```
