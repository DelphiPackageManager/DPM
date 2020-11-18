
## Getting Started

Note that DPM is still very much alpha software, things are subject to change. 

### Installing

Download the latest installer from github (it is codesigned, as are the dll's and command line exe) :

https://github.com/DelphiPackageManager/DPM/releases

The installer will prompt to install for the "only me" (recommended) or everyone (needs admin access for everyone). I recommended you leave the  "add to path" option checked, it makes running the command line easier.

Create a folder somewhere on your machine or a network share for the packages.

Open a command prompt and run (assuming you left the add to path option checked in the installer).

```dpm sources add -name=local -source=path to the folder you created```

This will add a package source to your config. At this time only folders are supported, http sources will be added later.

When you install packages, they will be downloaded from the package source to the package cache folder.  By default the package cache folder will be in %APPDATA%\.dpm\packages

You can change that by editing %APPDATA%\.dpm\dpm.config and changing the packageCacheLocation setting.

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

Once you have some packages, you can either install packages from the command line, or using the IDE plugin. The IDE plugin adds a DPM node to each project in the project tree, 
and a right click menu to "Manage DPM Packages". The DPM package view will open docked in the IDE code window, opened on the Installed tab. To add packages, switch to the search 
tab, search or click on the package you want to install, and in the package details panel on the right, click on install. If there are multiple package versions, you can chose 
which version to install. If you think a package is missing, it may be a pre-release package, in which case check the "Include Prerelease" checkbox under the search box - the list 
will refresh. Some of the above packages are still prerelease. 

Note that your dproj will be modified by installing packages. 

DPM will :

a) Add some msbuild properties, and add a $(DPMSearch) variable to the base configuration project search path for each platform that you install for.
b) Add a DPM section at the bottom of the file with PackageReference elements, which record which packages are installed in the project.

Note that installing design time packages is not currently supported, I'm working on that still. 

The rest of this post is for people that want to compile or contribute or create dpm packages.

### Creating packages

Create a dspec file - see the dspec files in the ThirdParty folder or the github projects linked above for examples. The docs are out a bit of date so use the source if you need a reference (Source\Core\Spec). I will endeavor to update the docs over the next week or two. If you get stuck just email me or create a github issue.
To generate the packages you need to run 

```dpm pack My.Package.dspec -o=c:\mypackagefolder```

That will generate 1 file per compiler/platform combination. 

### Building DPM
DPM is bootstrapped, so you need DPM to build dpm!

dpm.dproj requires these packages :

Ahausladen.JsonDataObjects
Spring4D.Core
Spring4D.Base
Spring4D.Extensions
VSoft.AntPatterns
VSoft.Awaitable
Gabr42.OmniThreadLibrary
VSoft.CancellationToken
VSoft.CommandLine
VSoft.HttpClient
VSoft.SemanticVersion
VSoft.Uri

DPM.IDE.XXX.dproj requires these packages :

Ahausladen.JsonDataObjects
Spring4D.Core
Spring4D.Base
Spring4D.Extensions
VSoft.AntPatterns
VSoft.Awaitable
Gabr42.OmniThreadLibrary
VSoft.CancellationToken
VSoft.HttpClient
VSoft.SemanticVersion
VSoft.Uri
VSoft.VirtualListView

If you have a package source configured and the IDE plugin installed, it should restore the packages when you load the project. Note that errors in the IDE plugin are not obvious, on some versions of the IDE it does not show the messages view when it should.

Debugging the IDE plugin should be done using runtime parameters : -rDPM
Host executable should be the same version of bds.exe that you are using to compile with. If you try use a different bds.exe version to debug, it won't be pretty!

Note the -r parameter creates a new registry hive for the IDE, so you may find not all your settings etc are quite there.

Debugging an IDE plugin is painful, waiting for the IDE to load etc. If you are working on a core feature debugging is best done using the command line projects.

I do not recommend doing dev work on this project using anything earlier than 10.1 Berlin, due to the compiler performance issues with generics (XE2/3/4 struggle with spring4d).

### Contributing

If you think you can contribute, create an issue on github and describe the area or feature you would like to work on. Don't just go and create a massive pull request and expect it to be merged! After some discussion to ensure we are all on the same page, we'll invite a pull request. Keep PR's as small and isolated as you can, that will make it easier to merge later.

If you have any ideas to contribute, create an issue on github.

