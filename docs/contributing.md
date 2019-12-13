# Contributing

We welcome contributions, however before submitting any changes, create an issue on github so we can discuss your ideas, bugs etc. That way we all avoid wasting effort.

We make extensive use of Spring4D for Dependency Injection and collections. Make sure you are up to speed with DI and use it where possible.

- Use extremely defensive coding style, with useful/detailed error messages.
- Pass an ILogger in everywhere. There is one available through Dependency Injection.
- Start with high level classes for functionality, that take an options object for the command.
- Do not use the RTL generic collections, we're using spring4d ones (interfaced).
- Use interfaces where possible and only expose what is needed.
- Use unit namespaces, but avoid prefixs or namespacing of types where possible (makes for simpler renaming).
- Since we're targeting multiple compiler versions, avoid new language features or rtl features.
- Do not add/use new external libraries before checking with the team. Whilst using libraries is unavaiodable, we would like to keep the dependencies as low as possible. Many of the libraries listed below were created for this project.

## Building DPM

DPM uses the following external libs

<https://github.com/VSoftTechnologies/VSoft.AntPatterns>

<https://github.com/VSoftTechnologies/VSoft.Uri>

<https://github.com/VSoftTechnologies/VSoft.CommandLineParser>

<https://github.com/VSoftTechnologies/VSoft.SemanticVersion>

<https://github.com/ahausladen/JsonDataObjects>

<https://bitbucket.org/sglienke/spring4d>

Clone these repos to your machine, then add Symlinks in the Source\Libs folder for each one

e.g (adjust paths accordingly)

```cli
mklink /D Spring4D i:\bitbucket\spring4d
mklink /D VSoft.AntPatterns i:\github\VSoft.AntPatterns
....
```

These will be eventually pulled in as packages, but for now till we are stable we're going old school!

There are some test packages in the TestPackages Folder and a batch file to generate them - need to specify an output folder (ie your feed folder) as a parameter, e.g

`GeneratePackages C:\DPMFeed`

Then you need to add the feed as a source

`dpm sources add -name=local -source=c:\DPMFeed`
