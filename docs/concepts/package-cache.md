# Package Cache

The package cache is a folder where packages are downloaded and extracted to, so they can be referenced in your projects.

## Where is it

The default package cache location is `%APPDATA%\.dpm\packages` - it can be changed by modifying the **packageCacheLocation** setting in the `%APPDATA%\.dpm\dpm.config` file.

If a dpm.config file exists in the same folder as a project (dproj), then that config file will be used, and this file may have a different package cache location. Most commands also allow you to specify a configfile option.

## What's inside the cache

The package cache is a folder structure that makes it easy to find and extract packages.

In the package cache root folder, you will find the downloaded .dpkg files

There will be a folder for each compiler version you have used with dpm

e.g

```cmd
packages\XE7
```

and inside each compiler version folder, you will find platform folders

```cmd
packages\XE7\Win32
packages\XE7\Win64
```

Inside each platform folder you will find package folders, with version subfolders under them

```cmd
packages\XE7\Win32\VSoft.CommandLine\0.0.1
packages\XE7\Win32\VSoft.CommandLine\0.0.2
```

The structure inside the version folders will be dependent on the settings in the package dspec file used when the package was created. There will be a package.dspec file in the root of the version folder, this is the package manifest. You should not modifiy this file!

## What if I delete the folder

No problem, dpm will recreate it and download missing packages during the install or restore process.

## What if I am no longer using a package

It's difficult to know which packages are in use or not without doing some sort of package tracking/reference counting etc. We did tinker with that early on, but it's too easy to break. To clean up you can just delete the package cache folder or delete individual package folders. We will continue to look for a better way to manage this.
