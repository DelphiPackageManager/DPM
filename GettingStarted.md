## Getting Started

Note that DPM is still very much alpha software, things are subject to change.

### Installing

Download the latest installer from github (it is codesigned, as are the dll's and command line exe) :

https://github.com/DelphiPackageManager/DPM/releases

The installer will prompt to install for the "only me" (recommended) or everyone (needs admin access for everyone). I recommended you leave the "add to path" option checked, it makes running the command line easier.

Create a folder somewhere on your machine or a network share for the packages.

Open a command prompt and run (assuming you left the add to path option checked in the installer).

`dpm sources add -name=local -source=path to the folder you created`

This will add a package source to your config. At this time only folders are supported, http sources will be added later.

When you install packages, they will be downloaded from the package source to the package cache folder. By default the package cache folder will be in %APPDATA%\\.dpm\packages

You can change that by editing %APPDATA%\\.dpm\dpm.config and changing the packageCacheLocation setting.

To use DPM, you need some packages. I have published packages for all of my open source delphi projects, you will find the dpkg files under releases for each project. Packages files are named in a particular manner (eg : VSoft.CommandLine-XE2-Win64-0.0.2.dpkg) , do not rename them. Download the files to the folder you created earlier.

https://github.com/VSoftTechnologies/DUnitX/releases

https://github.com/VSoftTechnologies/VSoft.SemanticVersion/releases

https://github.com/VSoftTechnologies/VSoft.Awaitable/releases - note that for this one you will need to create a package for omnithreadlibrary - see the ThirdParty folder for a dspec file

https://github.com/VSoftTechnologies/VSoft.VirtualListView/releases

https://github.com/VSoftTechnologies/VSoft.AntPatterns/releases

https://github.com/VSoftTechnologies/Delphi-Mocks/releases

https://github.com/VSoftTechnologies/VSoft.Messaging/releases

https://github.com/VSoftTechnologies/VSoft.CommandLineParser/releases

https://github.com/VSoftTechnologies/VSoft.HttpClient/releases

https://github.com/VSoftTechnologies/VSoft.Uri/releases

https://github.com/VSoftTechnologies/VSoft.CancellationToken/releases

https://github.com/VSoftTechnologies/VSoft.WeakReferences/releases

https://github.com/VSoftTechnologies/Spring4DMirror

Once you have some packages, you can either install packages from the command line, or using the IDE plugin. The IDE plugin adds a DPM node to each project in the project tree,
and a right click menu to "Manage DPM Packages". The DPM package view will open docked in the IDE code window, and show the list of installed packages, plus the first 100 packages
from the package sources. When you click on a package, the details on the right side of the view will show the package information and the projects which have the package installed.

Note that your dproj will be modified by installing packages.

DPM will :

a) Add some msbuild properties, and add a $(DPMSearch) variable to the base configuration project search path for each platform that you install for.
b) Add a DPM section at the bottom of the file with PackageReference elements, which record which packages are installed in the project.

Note that installing design time packages is not currently supported, I'm working on that still.

The rest of this post is for people that want to compile or contribute or create dpm packages.

### Creating packages

Create a dspec file - see the dspec files in the github projects linked above for examples. The docs are out a bit of date so use the source if you need a reference (Source\Core\Spec). If you get stuck just create a github issue.

To generate the packages you need to run

`dpm pack My.Package.dspec -o=c:\mypackagefolder`

That will generate 1 file per compiler/platform combination.

### Building DPM

See [Contributing](/contributing-dpm.md)
