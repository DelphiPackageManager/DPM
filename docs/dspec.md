# Package Spec (dspec)

The Package Spec file (packagename.dspec) is a json file that defines the package metadata, which compiler/platforms the package supports, and which files are to be included in the package.

## metadata (required)

The metadata object provides static information the package consumer can inspect.

### Required Properties

| Property    | Description                                    |
| ----------- | ---------------------------------------------- |
| id          | The package id, eg. Spring.Base                |
| version     | The package Semantic Version, eg. 0.1.2-alpha  |
| description | A text description of a package.               |
| authors     | Who created this package.                      |

Note that the version property can be overridden from the [pack command](./commands/pack-command.md).

### Optional Properties

| Property         | Description                                                                                                                                                                                                                                                                                               |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| minClientVersion | Specifies the minimum version of dpm that can install this package.                                                                                                                                                                                                                                       |
| owners           | A comma-separated list of package creator profilenames on dpm website.                                                                                                                                                                                                                                    |
| projectUrl       | URL for the packages home page. Will be displayed in the UI.                                                                                                                                                                                                                                              |
| license          | The package license, will be displayed in the UI. See the [SPDX License List](https://spdx.org/licenses/)                                                                                                                                                                                                 |
| icon             | File path in the package for a 64x64 image with a transparent background (png), for display in the UI.                                                                                                                                                                                                    |
| releaseNotes     | A description of the changes made in this release of the package, often used in UI like the Updates tab Package Manager in place of the package description.                                                                                                                                              |
| copyright        | Copyright details for the package.                                                                                                                                                                                                                                                                        |
| tags             | A comma-delimited list of tags and keywords that describe the package and aid discoverability of packages through search and filtering.                                                                                                                                                                   |
| repository       | Repository metadata, consisting of four optional attributes: type and url, and branch and commit. These attributes allow you to map the .dpkg to the repository that built it, with the potential to get as detailed as the individual branch or commit that built the package. Not currently implemented |

e.g

```json
{
  "metadata": {
    "id": "VSoft.A",
    "version": "0.0.1",
    "description": "Test package A",
    "authors": "Vincent Parrett",
    "projectUrl": "https://github.com/org/project",
    "license": "Apache-2.0",
    "icon": "images\\logo.png",
    "copyright": "2019 Joe User",
    "tags": "Test"
  }
}
```

## targetPlatforms (required)

An array of 1 or more objects, which specify the compiler/platform combinations this package supports.

## targetPlatform

### Required Properties

| Property  | Description                                               |
| --------- | --------------------------------------------------------- |
| compiler  | the [compiler version](./compiler-versions.md) (required) |
| platforms | A comma-separated list of [platforms](./platforms.md)     |

A targetPlatform object must also define either :

**template** : The name of a template object in this dspec

e.g

```json
{
  "targetPlatforms": [
    {
      "compiler": "XE2",
      "platforms": "Win32, Win64",
      "template": "default"
    }
  ]
}
```

or it must fully define the dependencies, source files etc. targetPlatforms and templates share the same properties (except for the name property on the template, and the template property on the targetPlatform).

Where the package includes the same files for each targetPlatform, it is recommended to use a template.

## templates

A collection of zero or more template objects

## template

### Required Properties

| Property | Description       |
| -------- | ----------------- |
| name     | The template name |

### Optional Properties

#### dependencies

An array of 1 or more dependency objects.

#### dependency

A dependency object must specify either (a) :

| Property | Description                                    |
| -------- | ---------------------------------------------- |
| id       | dependency package id                          |
| version  | dependency [Version Range](./version-range.md) |

or (b)

| Property       | Description                                                   |
| -------------- | ------------------------------------------------------------- |
| targetPlatform | Compiler.Platform (e.g XE2.Win32 )                            |
| dependencies   | an array of 1 or more dependency objects as defined above (a) |

**Note** option (b) is only valid when used in a template.

#### lib

An array or 1 or more fileEntry objects which define which compiled/binary (dcu/dcp/obj etc) files to include in the package.

#### src

An array or 1 or more fileEntry objects which define which source files to include in the package.

#### files

An array or 1 or more fileEntry objects which define which non source or binaries to include in the package (eg, images).

#### fileEntry Objects

| Property | Description                                                                                                                                                 |
| -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| src      | an Ant Pattern which describes which files to include                                                                                                       |
| dest     | the destination folder in the package file                                                                                                                  |
| flatten  | boolean (default false), determines whether the files are put directly in the dest folder, or in relative subfolders which mirror original folder structure |

#### runtime

An array of 1 or more runtimePackage objects, which define which compiled runtime packages (.bpl or .dll) should be included in the package.

| Property  | Description                                                                                                                       |
| --------- | --------------------------------------------------------------------------------------------------------------------------------- |
| src       | an Ant Pattern which describes which files to include                                                                             |
| dest      | the destination folder in the package file                                                                                        |
| copyLocal | boolean (default false), determines whether the files should be copied to the project output folder when the package is installed |

e.g

```json
runtime : [
        {
          "src" : "output\\$targetPlatform$\\rtl\\DPM.Core$libSuffix$.bpl",
          "dest" : "rtl",
          "copylocal" : true,
          "flatten" : true
        }
      ]
```

#### design

An array of 1 or more designPackage objects, which define which compiled design packages (.bpl) should be included in the package.

| Property | Description                                                                                                      |
| -------- | ---------------------------------------------------------------------------------------------------------------- |
| src      | an Ant Pattern which describes which files to include                                                            |
| dest     | the destination folder in the package file                                                                       |
| install  | boolean (default false), determines whether the bpl should be installed in the IDE when the package is installed |

### Replacement Tokens

When creating a package using the dpm pack command, \$ delimited tokens in the dspec file's `metadata` node will be replaced with values that come from either the project file, or the -properties argument of the pack command.

To use the values from the project file, use the pack command with the dproj file rather than the dspec file. The dspec file and dproj file must live in the same folder, with the same name.

Values provided on the command line will override any values taken from the dproj.

TODO: Define actual tokens when implemented.

| **Token**         | **Value source** | **Value**                          |
| ----------------- | ---------------- | ---------------------------------- |
| **$id$**          | Project file     | Name (title) from the project file |
| **$version$**     | VersionInfo      | FileVersion                        |
| **$author$**      | VersionInfo      | Company                            |
| **$title$**       | VersionInfo      | Title                              |
| **$description$** | VersionInfo      | Description                        |
| **$copyright$**   | VersionInfo      | Copyright                          |
