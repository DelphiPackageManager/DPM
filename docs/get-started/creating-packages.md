# Creating Packages

DPM packages are a zip file that contains source files and/or compiled dcu/dcp/bpl files, and a manifest file that describes the package, how it should be used, which paths to add to the project search path (more on this below), which design time packages should be install, which runtime files should be included etc. The manifest also describes the other packages this package depends on. When this package is installed, any dependendcies are also install, and their dependencies are also installed.

### So how are packages created

The first step is to create a [package.dspec file](../dspec.md). The dspec is a json file which descirbes the package (the manifest file mentioned above is derived from it), which files are to be included, which compiler versions and platforms are supported, which search paths it needs, which other packages it depends on etc. To aid in this process, the [dpm spec command](../commands/spec-command.md) can be run against a project file to produce a dspec file. This will give you a starting point but some editing will still be required (the dproj lacks information we need).

The [dpm pack command](../commands/pack-command.md) is then used with the dspec file to generate the package files. One package file is produced per compiler/platform combination.

### Ok, so I have a package file, what do I do with it

You need to define a package source. See here for detailed information on [package sources](../concepts/sources.md).

Use the [dpm push command](../commands/push-command.md) to push the package to a package source. For non http sources (ie folders) the package file is just copied to the folder. You can skip this step if you specify your package source folder as the output folder when using the pack command (only for folder based package sources).

Then you can use the [dpm install command](../commands/install-command.md) to install the package into your project.
