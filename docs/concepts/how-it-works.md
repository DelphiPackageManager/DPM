# How does DPM work

Many people have expressed a concern that using DPM would force them to give up using other techniques for working with third party libraries. This is not the case, and this page will explain just how it all hangs together. Lets start with the basics:

## Packages

DPM packages are a zip file that contains source files and/or compiled dcu/dcp/bpl files, and a manifest file that describes the package, how it should be used, which paths to add to the project search path (more on this below), which design time packages should be install, which runtime files should be included etc. The manifest also describes the other packages this package depends on. When this package is installed, any dependendcies are also install, and their dependencies are also installed.

### So how are packages created

See [creating packages](../concepts/creating-packages.md).

### What does installing a package actually do

Lets walk through the installation process, starting with the command line. This is a simplification but covers the most important aspects.

`dpm install VSoft.CommandLine -projectPath=.\MyProject.dproj`

The first step is to determine the Delphi compiler version, which is done by reading the dproj file ([see known issues](../known-issues.md)).

The next step is find our package (VSoft.CommandLine). In the above example, we didn't specify a package version, so dpm searches the registered [sources](./sources.md) to determine the latest version (if any) available.

DPM then checks it's [package cache](./package-cache.md) to see if it already has the package version downloaded.

If the package is not already cached, dpm downloads it from the registred [source](./sources.md) and extracts the package file into the package cache. This provides us with access to the package manifest.

Using the package manifest, we then start the package [dependency resolution](./dependencies.md) process, which walks the dependency tree to ensure that all dependencies can be resolved. The dependent packages are also downloaded to the package cache during this process (as we need the manifest).

When all the dependencies are met, the dproj is updated with the package reference and the collected search paths are added to the base configuration for each platform. Note that the default is to attempt to install the package for all supported platforms that are enabled in the project. You can install different packages for different platforms by specifying the platform on the command line.

A [lock file](./lock-file.md) will be created in the project folder which can checked into source control, this file provides reliable restore process, which means that the exact package versions you are using in dev will be used on the build machine/ci server. The lock file is a serialized version of the dependency tree, and can be used to speed up package installation. This feature is currently under development. This is something that Nuget lacks (although they have it planned), the yarn package manager is the inspiration for this feature.

### So do I need to install the package on every dev machine

No. DPM has a [restore command](../commands/restore-command.md) which will look at the package references in the dproj, and go through the process of downloading and installing the packages and dependencies. If a lock file is present it will use that rather than use the dependency resolution process.
