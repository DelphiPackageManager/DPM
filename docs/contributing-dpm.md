# Contributing

We welcome contributions, however before submitting any changes, create an issue on GitHub so we can discuss your ideas, bugs etc. That way we all avoid wasting effort.

We make extensive use of Spring4D for Dependency Injection and collections. Make sure you are up to speed with DI and use it where possible.

- Use an extremely defensive coding style, with useful/detailed error messages.
- Pass an ILogger in everywhere. There is one available through Dependency Injection.
- Start with high-level classes for functionality, that take an options object for the command.
- Do not use the RTL generic collections, we're using spring4d ones (interfaced).
- Use interfaces where possible and only expose what is needed.
- Use unit namespaces, but avoid prefixes or name-spacing of types where possible (makes for simpler renaming).
- Since we're targeting multiple compiler versions, avoid new language features or RTL features.
- Do not add/use new external libraries before checking with the team. Whilst using libraries is unavoidable, we would like to keep the dependencies as low as possible. Many of the libraries listed below were created specifically for this project.

## Building DPM

DPM uses the following external libs

[Spring4D.Base](https://github.com/VSoftTechnologies/Spring4DMirror/releases)

[Spring4D.Core](https://github.com/VSoftTechnologies/Spring4DMirror/releases)

[Spring4D.Extensions](https://github.com/VSoftTechnologies/Spring4DMirror/releases)

[VSoft.AntPatterns](https://github.com/VSoftTechnologies/VSoft.AntPatterns/releases)

[VSoft.Awaitable](https://github.com/VSoftTechnologies/VSoft.Awaitable/releases)

[VSoft.CancellationToken](https://github.com/VSoftTechnologies/VSoft.CancellationToken/releases)

[VSoft.CommandLineParser](https://github.com/VSoftTechnologies/VSoft.CommandLineParser/releases)

[VSoft.DUnitX](https://github.com/VSoftTechnologies/DUnitX/releases)

[VSoft.HttpClient](https://github.com/VSoftTechnologies/VSoft.HttpClient/releases)

[VSoft.JsonDataObjects](https://github.com/ahausladen/JsonDataObjects/releases)

[VSoft.SemanticVersion](https://github.com/VSoftTechnologies/VSoft.SemanticVersion/releases)

[VSoft.VirtualListView](https://github.com/VSoftTechnologies/VSoft.VirtualListView/releases)

[VSoft.Uri](https://github.com/VSoftTechnologies/VSoft.Uri/releases)

[Gabr42.OmniThreadLibrary](https://github.com/VSoftTechnologies/OmniThreadLibrary/releases)

Download the dpkg files from the releases tab on the above projects, into a single folder.

DPM uses dpm itself to build (dogfood). The simplest way to get up and running is install a build from the releases tab

Then add the folder you downloaded the packages to as a package source

`dpm sources add -name=local -source=c:\DPMFeed`

You should now be able to load the dpm projects - there are IDE plugin projects for XE2-11.3, the unit tests and the command line are developed with 11.3.

The dpm IDE plugin will restore the packages automatically when you load the project and you should be able to build without issue.

For debugging the IDE plugin dll, set the host application to the Delphi IDE version you are using (eg. E:\Emb\Studio\22.0\bin\bds.exe) and in the parameters to -rDPMTesting

This will start an new IDE instance, with a clean registry. Do this once, then open regedit to

`HKEY_CURRENT_USER\Software\Embarcadero\DPMTesting\22.0\Experts`

and add a string value pointing to the dll you just build

eg. `I:\Github\DelphiPackageManager\DPMMaster\Output\DPM.IDE.D110.dll`

Then try debugging again - this time the new IDE instanced should load the plugin (you will see the logo on the splash screen).
